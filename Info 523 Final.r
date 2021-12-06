#####################################
#Load the data and necessary initial libraries 
###################################
library(heplots)
library(car)
library(carData)
library(psych)
library(tidyverse)
library(ggplot2)

data("schooldata")
####################################
#General description about the data and Visualizations
####################################
?schooldata
str(schooldata)
head(schooldata)
describe(schooldata)
summary(schooldata)
View(schooldata)

# Explore and visualize data
###############################
plot(schooldata) # it seems like many variables have linear correlations with each other so I will continue to explore a linear model.
library(corrgram)
corrgram(schooldata, lower.panel=panel.ellipse, upper.panel=panel.pts) # Run correlation lines for each variable

#Check for Normality and remove outliers
############################
require(DescTools)
require(semTools)
mardiaSkew(schooldata) # check for skewness, Teh data is skewed. There might be outliers.
mardiaKurtosis(schooldata) # check for kurtosis

any(schooldata$education>=100)
schooldata %>% filter(education > 100) # Education cannot be over 100 percent per data description
schooldata <- schooldata[-59, ]; schooldata # remove the outlier.

# plot the data again after removing the outlier.
pairs.panels(x = schooldata, 
             ellipses = TRUE,	
             lm=FALSE, 	
             smooth = TRUE,
             show.points = TRUE, 	
             density = TRUE,	
             hist.col = "lightblue",
             breaks = 10)
emean<-mean(schooldata$education)
omean<-mean(schooldata$occupation)
vmean<-mean(schooldata$visit)
cmean<-mean(schooldata$counseling)
tmean<-mean(schooldata$teacher)
rmean<-mean(schooldata$reading)
mmean<-mean(schooldata$mathematics)
smean<-mean(schooldata$selfesteem)
cov(schooldata)

#check how the data is spread out from each other through mahalanobis distance method.
schooldatameans<-c(emean,omean,vmean,cmean,tmean,rmean,mmean,smean)
mahalanobis(schooldatameans,center = c(0,0,0,0,0,0,0,0),cov(schooldata), inverted = FALSE )

# From prior knowledge we know that Self esteem and Math/Reading scores are highly correlated.
#We picked self esteem as the initial dependent variable and visualized it to compare it to other independent variables.
ggplot(schooldata, aes(visit, selfesteem)) + geom_smooth(method=lm) + geom_jitter() 
ggplot(schooldata, aes(visit)) + geom_histogram()
ggplot(schooldata, aes(selfesteem, counseling)) +geom_smooth(method = lm) +geom_jitter()
ggplot(schooldata, aes(teacher)) + geom_histogram()
ggplot(schooldata, aes(occupation, selfesteem)) + geom_jitter()+ geom_smooth(method = lm)
#####################################
#Canonical Correlation Analysis  
#(Partially, this code was used as apart of another assignment. 
# We built our own analysis further down after this portion of the code 
#for this class as a continuation of this code.)
#################################
library(candisc)
cca.schooldata <- candisc::cancor(x=schooldata[,1:4], 
                                  y=schooldata[,6:8])

cca.schooldata
summary(cca.schooldata)
str(cca.schooldata)

#Getting raw and structure of the coefficient
cca.schooldata$coef
#The first matrix contains the weights for 
# locus and concept on each of the two weighted
# sums of the psych variables.
# (Each column is a different weighted sum)

# The second matrix contains the weights for 
# read, write, and math on each of the two 
# weighted sums of the academic variables.
# (Each column is a different weighted sum)

cca.schooldata$structure

CanCorCompScores <- data.frame(Data1=cca.schooldata$scores$X[,1],
                               Data2=cca.schooldata$scores$X[,2],
                               Data3=cca.schooldata$scores$X[,3],
                               Output1=cca.schooldata$scores$Y[,1],
                               Output2=cca.schooldata$scores$Y[,2],
                               Output3=cca.schooldata$scores$Y[,3]
)

pairs.panels(CanCorCompScores)

# Now we have weighted sums of 
# variables that are as highly correlated 
# as possible. AND these weighted sums
# are all UNCORRELATED with each other.
# ONLY the pairs of weighted sums that
# were created to go together are 
# correlated. Everything else has 
# correlation of approximately 0.

describe(CanCorCompScores)


require(yacca)

yacca.schooldata <- yacca::cca(x = schooldata[, 1:4], y = schooldata[,6:8], ycenter=FALSE, xcenter = FALSE)
yacca.schooldata
summary(yacca.schooldata)
str(yacca.schooldata)

F.test.cca(yacca.schooldata)
# Raw coefficients
yacca.schooldata$xcoef
yacca.schooldata$ycoef

####################################
# Run Linear Regression
###################################

# Create training and test data sets

set.seed(3) # so that results are reproducibledt <-  sort(sample(nrow(schooldata), nrow(schooldata)*.7))
train<-schooldata[dt,]
test<-schooldata[-dt,]

# I picked self esteem as the initial dependent variable and run linear regression to look for relationships.
lm.selfesteem <- lm(train$selfesteem ~ .,data=schooldata[,1:5])
summary(lm.selfesteem) # there is a high R^2 number indicating high correlation 

# check to see if there is any variables that does not have much effect.
anova(lm.selfesteem) 
#remove teacher variable as it has a high P value making it insignificant and run linear regression again. 
lm.selfesteem_V2 <- update(lm.selfesteem, . ~ . - teacher)
summary(lm.selfesteem_V2)

# Compare both models
anova(lm.selfesteem, lm.selfesteem_V2)
lm.selfesteem_V3 <- update(lm.selfesteem_V2, . ~ . - counseling)
summary(lm.selfesteem_V3) # best model for linerar regression so far. 

# Check to see how significant are the remaining independent variables. 
anova(lm.selfesteem, lm.selfesteem_V3) #p value dropped dramatically. it is a good progress.
anova(lm.selfesteem_V3) # all remaining values have significant difference (p<0.001).

# Create a "Stepwise backward approach regression"  model to check if another option is available.
lm.stepwise <- step(lm.selfesteem)
summary(lm.stepwise) # stepwise analysis suggested the LmselfesteemV2 from our linear regression. It is a very strong linear correlation between education, occupation, visit and counselling.
# we will use this model as our prediction model. 


# Create visuals for presentation
library(patchwork)
library(ggpubr)
library(car)

avPlots(lm.stepwise) # plot the linear model

# Plot the individual correlations

a <- ggplot(schooldata, aes(education, selfesteem)) +geom_smooth(method = lm) +geom_jitter()+ theme_bw()
b <- ggplot(schooldata, aes(occupation, selfesteem)) + geom_smooth(method = lm) + geom_jitter()+ theme_bw()
c <- ggplot(schooldata, aes(visit, selfesteem)) +geom_smooth(method = lm) +geom_jitter()+ theme_bw()
D <- ggplot(schooldata, aes(counseling, selfesteem)) +geom_smooth(method = lm) +geom_jitter()+ theme_bw()
Patchwork <- a + b + c + D
Patchwork <- Patchwork + plot_annotation(tag_levels = "a") 
  
Patchwork 

predict(lm.stepwise, newdata = test) # our model clearly predicts the self esteem in a very close range.

# There is no reason to try regression trees as the linear model is already highly correlated. 

###################################
# Create a clusters to see if there are any other trends
##################################
library(cluster)
library(ClusterR)
library(factoextra)

# Investigate the number of clusters with "kmeans" clustering method as all the values are all continues numbers. 
#wss method
k1 <- fviz_nbclust(schooldata.scaled, kmeans, method = "wss") + labs(title = "WSS Method")

# Silhouette Method
k2 <- fviz_nbclust(schooldata.scaled, kmeans, method = "silhouette") + labs(title = "Silhouette Method")

# Gap Stat Method
schooldata.scaled <- scale(schooldata) # scale the data 
gap_stat <- clusGap(schooldata.scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 10)
print(gap_stat, method = "firstmax")
k3 <- fviz_gap_stat(gap_stat) + labs(title = "Gap Stat Method")

# Prepare visuals for presentation
k <- k1 + k2 + k3 + plot_annotation(tag_levels = "a")
k

# Run Kmeans cluster as all the values are all continues numbers. 
Kmeans2 <- kmeans(schooldata, centers = 2, nstart = 25, iter.max = 100)
summary(Kmeans2)
Kmeans2$centers
Kmeans2$size


# Plot the clusters.
d <- fviz_cluster(Kmeans2, geom= "point", data = train)
d

# Run PCA analysis
fit <- prcomp(x = schooldata, 
              center = TRUE, 
              scale = TRUE)

# plot the results with the corresponding varibale vectors 
e <- fviz_pca_biplot(fit, repel = TRUE, labelsize = 3)
e
  # Prepare visuals for presentation
  
Patchwork_cluster <- d + e + plot_annotation(tag_levels = "a")
Patchwork_cluster

   
