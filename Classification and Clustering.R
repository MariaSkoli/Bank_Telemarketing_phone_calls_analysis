##################################################################################
# IMPORT THE DATA 
#################################################################################3

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

#install.packages("knitr")
#library(knitr)
install.packages("sjmisc")
library("sjmisc")
install.packages("kableExtra")
install.packages("ggpubr")
library('pgmm')
library('heplots')
library('mclust')

install.packages(c("corrgram","nnet", "class", "tree", "pgmm",
                   "penalizedLDA", "klaR","dplyr","ggplot2", "e1071", "randomForest", "scoring"))

library(corrgram)
library(nnet)
library(class)
library(tree)
library(MASS)
library(pgmm)
library(penalizedLDA)
library(klaR)
library(dplyr)
library(ggplot2)
library(scoring)
library(e1071)
library(randomForest)



#####################################################
#------------------ Data cleansing -----------------#
#####################################################


# loading in the data 

setwd("C:\\Users\\Maria Skoli\\Documents\\1st Semester\\R")  
getwd()
data <- read.csv('project.csv', header = TRUE, sep=",")
View(data)

# removing all missing values
data <- data[complete.cases(data), ]


# getting a quick view of the data
str(data)

# renaming the Age 
data <- data %>% rename(Age = names(data[1]))

#Transformation of pdays variable 
data <- data %>% 
  mutate(pdays = ifelse(pdays %in% c(0, 999), 0,1))

data$pdays<-as.factor(data$pdays)


#############################################
# CONVERT DATA TYPES 
#############################################


library("Hmisc")
data$Age <- cut2(data$Age, g=3 )
#data$Age <- as.numeric(data$Age)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$job <- as.factor(data$job)
data$marital <- as.factor(data$marital)
data$education <- as.factor(data$education)
data$contact <- as.factor(data$contact)
data$day_of_week <- as.factor(data$day_of_week)
data$campaign <- as.numeric(data$campaign)
data$poutcome<- as.factor(data$poutcome)
data$SUBSCRIBED <-as.factor(data$SUBSCRIBED)
data$previous <-as.numeric(data$previous)


#Drop the variables month, duration and nr.employed
data <- subset(data,select=-c(9,11,19,20))
data


###############################################################################################
# VARIABLE SELECTION -AIC
###############################################################################################

##################################################################
# ---------------------- Logistic Model -------------------------#
##################################################################

mylogit <- glm(SUBSCRIBED ~., data = data, family = "binomial")
mylogit2 <-glm(SUBSCRIBED ~.-marital-education-housing-loan-previous-day_of_week-default-pdays, data = data, family = "binomial")
summary(mylogit2)

#ROC CURVE for Full logit (my logit)

threshold<-0.5


res2<-NULL
for (threshold in seq(0.01,0.99,by=0.01)){
  clas<- mylogit$fitted > threshold
  conftable<- table(clas,data$SUBSCRIBED)
  sens<- conftable[1,1]/apply(conftable,2,sum)[1]
  spec<- conftable[2,2]/apply(conftable,2,sum)[2]
  res2<-rbind(res2, c(sens,1-spec, threshold) )
}

res2<-res2[order(res2[,1]),]

#ROC Curve For eliminated logit (logit 2)

threshold<-0.5


res3<-NULL
for (threshold in seq(0.01,0.99,by=0.01)){
  clas2<- mylogit2$fitted > threshold
  conftable2<- table(clas2,data$SUBSCRIBED)
  sens2<- conftable2[1,1]/apply(conftable2,2,sum)[1]
  spec2<- conftable2[2,2]/apply(conftable2,2,sum)[2]
  res3<-rbind(res3, c(sens2,1-spec2, threshold) )
}

res3<-res3[order(res3[,1]),]

plot(res2[,2],res2[,1], xlab="FN", ylab="TP", type="l",col=2, xlim=c(0,1), ylim=c(0,1), main="ROC Curves Comparison")
abline(0,1)

lines(res3[,2],res3[,1], xlab="FN", ylab="TP", type="l",col=3, xlim=c(0,1), ylim=c(0,1))

legend("topleft",
       c("full logit","eliminated logit"),
       fill=c(2,3))

###############################################
# SELECT THE FINAL DATA
###############################################

modeldata=subset(data, select=-c(3,4,5,6,7,9,11,12) )


##############################################
# CLASSIFICATION
##############################################

###############################
# LOGISTIC REGRESSION
###############################

model_logit <- glm(SUBSCRIBED~., data=modeldata, family="binomial")
pr_logit<-predict(model_logit, type="response", newdata = modeldata)
n2=length(pr_logit)
pr<-rep("no",n2)
pr[pr_logit > 0.5]="yes"

par(mfrow=c(3,1))

plot(model_logit$fitted.values, model_logit$y,
     col = rgb(0,0,1,alpha = 0.01),
     pch = 14)
table_logit<-table(modeldata$SUBSCRIBED, pr)

library("kableExtra")
kbl((table_logit), caption="Confusion Matrix - Logistic Regression (threshold=0.09)", valine="t", alligh="H") %>%
  kable_styling()


Accuracy_logit<-mean(pr==modeldata$SUBSCRIBED)

####################################
# FINDING THE BEST THRESHOLD
####################################

threshold<-0.5


res2<-NULL
for (threshold in seq(0.01,0.99,by=0.01)){
  clas<- model_logit$fitted > threshold
  conftable<- table(clas,modeldata$SUBSCRIBED)
  sens<- conftable[1,1]/apply(conftable,2,sum)[1]
  spec<- conftable[2,2]/apply(conftable,2,sum)[2]
  res2<-rbind(res2, c(sens,1-spec, threshold) )
}

res2<-res2[order(res2[,1]),]

plot(res2[,2],res2[,1], xlab="FN", ylab="TP", type="l",col=2, xlim=c(0,1), ylim=c(0,1))
abline(0,1)

dist<-(res2[,2]-0)^2+(res2[,1]-1)^2
min(dist)

res3 <-cbind(res2,dist)
res4 <- res3[order(res3[,4]),]
head(res4)


##################################
# NAIVE BAYES 
##################################

library('e1071')
nbm <- naiveBayes(y=modeldata$SUBSCRIBED, x= modeldata[,-9])
nbclass <- predict(nbm , newdata = modeldata[,-9])
table_nb <- table(modeldata$SUBSCRIBED, nbclass)
#f1<-(modeldata$SUBSCRIBED[te], nbclass , positive="yes")
accuracy <- mean(modeldata[,'SUBSCRIBED'] == nbclass)

library("kableExtra")
kbl((table_nb), caption="Confusion Matrix - Naive Bayes", valine="t", alligh="H") %>%
  kable_styling()

nbm$apriori

plot(nbm)


plot(as.numeric(nbclass) , as.numeric(modeldata$SUBSCRIBED)

   ,  col = rgb(0,0,1,alpha = 0.01),
     pch = 14)

library(tm)

#############################################
# DECISION TREES 
#############################################

treedata <- modeldata
treedata$campaign <- as.factor(treedata$campaign)
treedata$emp.var.rate <- as.factor(treedata$emp.var.rate)
treedata$cons.price.idx <- as.factor(treedata$cons.price.idx)
treedata$cons.conf.idx <- as.factor(treedata$cons.conf.idx)

-emp.var.rate - cons.conf.idx - cons.price.idx
library('tree')
fit1 <- tree(as.factor(SUBSCRIBED)~.-emp.var.rate ,data=modeldata)
tr <- predict(fit1, type='class', newdata=modeldata)
table_tree <-table(modeldata[,9],tr)
#f1<-(modeldata$SUBSCRIBED[te], nbclass, positive="yes")
accuracy_tree <- mean(modeldata[,'SUBSCRIBED'] == tr)


plot(fit1)
text(fit1, pretty=0)

library("kableExtra")
kbl((table_tree), caption="Confusion Matrix - Decision Tree", valine="t", alligh="H") %>%
  kable_styling()


###########################################
# LDA 
###########################################








##############################################################
# CROSS VALIDATION
#######################################################


# TRAINING AND TEST ALL METHODS


## add the variable names
colnames(modeldata)<-c(
                   "job","marital", "education", "default",
                    "contact", "day_of_week",
                   "campaign", "pdays", "poutcome",
                   "emp.var.rate","cons.price.idx", "cons.conf.idx", "euribor3m", "SUBSCRIBED")

n <- dim(modeldata)[1]
# k=6-fold cross-validation
k <- 6
set.seed(1)
deiktes<-sample(1:n)	#random permutation of the rows
methods <- c('Logistic','LDA', 'naiveBayes','tree')
accuracy <- matrix(data=NA, ncol= k, nrow = length(methods))
ari <- matrix(data=NA, ncol= k, nrow = length(methods))
rownames(accuracy) <- rownames(ari) <- methods

for (i in 1:k){
  print(i)
  te <- deiktes[ ((i-1)*(n/k)+1):(i*(n/k))]	
  train <- modeldata[-te, ]
  train[,'SUBSCRIBED'] <- as.factor(train[,'SUBSCRIBED'])
  test <- modeldata[te,]
  print("gothere")
  
  #Logistic Regression
  z_logit<- glm(SUBSCRIBED~., family=binomial, data=train)
  pr_logit<-predict(z_logit, type="response", newdata = test)
  n2=length(pr_logit)
  pr<-rep("no",n2)
  pr[pr_logit > 0.4]="yes"
  accuracy['Logistic',i] <- sum(modeldata[te,'SUBSCRIBED'] == pr)/dim(test)[1]
  ari['Logistic',i] <- adjustedRandIndex(pr, modeldata[te,'SUBSCRIBED'])

  
  #	LDA
  z_LDA <- lda(SUBSCRIBED ~ ., data = train)
  pr_LDA <- predict(z_LDA, test)$class
  accuracy['LDA',i] <- sum(modeldata[te,'SUBSCRIBED'] == pr_LDA)/dim(test)[1]
  ari['LDA',i] <- adjustedRandIndex(pr_LDA, modeldata[te,'SUBSCRIBED'])
  print("gothere2")
}
  #	svm
  #z_svm <- svm(SUBSCRIBED~., data=train)
  #pr_svm <-predict(z_svm, test)
  #table(modeldata$SUBSCRIBED[te] , pr_svm)
  #accuracy['svm',i] <- sum(modeldata[te,'SUBSCRIBED'] == pr_svm)/dim(test)[1]
  #ari['svm',i] <- adjustedRandIndex(pr_svm, modeldata[te,'SUBSCRIBED'])
  #print("gothere3")
  
  #Naive Bayes
  z_nb <- naiveBayes(SUBSCRIBED ~ ., data = train)
  pr_nb <- predict(z_nb, test)
  accuracy['naiveBayes',i] <- sum(modeldata[te,'SUBSCRIBED'] == pr_nb)/dim(test)[1]
  ari['naiveBayes',i] <- adjustedRandIndex(pr_nb, modeldata[te,'SUBSCRIBED'])
  print("gothere5")
  
  #	tree
  z_tree <- tree(SUBSCRIBED ~ .-emp.var.rate, data = train)
  pr_tree <- predict(z_tree,newdata=test,type='class')
  accuracy['tree',i] <- sum(modeldata[te,'SUBSCRIBED'] == pr_tree)/dim(test)[1]	
  ari['tree',i] <- adjustedRandIndex(pr_tree, modeldata[te,'SUBSCRIBED'])
  print("gothere6")
}
pdf(file = 'data1_accuracy.pdf', width=12,height=6)
boxplot(t(accuracy), ylab='predictive accuracy', xlab='method')
abline(h=0.9,col='red', ylab="0,9")
dev.off()  
pdf(file = 'data_ari.pdf', width=12,height=6)
boxplot(t(ari), ylab='Adjusted Rand Index', xlab='method')
dev.off()



T1<-table(modeldata$SUBSCRIBED[te],pr_LDA)



library("kableExtra")
kbl((T1), caption="Confusion Matrix - LDA", valine="t", alligh="H") %>%
  kable_styling()

accuracy

5975/(5975+672)
table(pr)
table(modeldata$SUBSCRIBED[te])


gc()

##################################################3
# TRIALS 
###################################################
n=nrow(modeldata)
te<- sample(1:n, round(n*0.3), replace=FALSE)
variab<-1:14
train <- modeldata[-te,variab]
test <-   modeldata[te,variab]

###########################################
# LDA
###########################################

# LDA 

LDA_model = lda(SUBSCRIBED~., data=train)
LDA_pred =predict(LDA_model, newdata = test)
t<-table(modeldata[te,14], LDA_pred$class)
sum(diag(t))/sum(t)

boxplot(LDA_pred$x~as.factor(modeldata[te,14]))
#kalo precision alla mikro pososto twn pragmatikwn 

sum(brierscore(modeldata$SUBSCRIBED[te]~LDA_pred$posterior))

plot(LDA_model)
#LDA_model

#LDA_model2 = lda(SUBSCRIBED~., data=train, CV=TRUE)
#t<-table(modeldata[-te,14], LDA_model2$class)
#sum(diag(t))/sum(t)
ARI_LDA <- adjustedRandIndex(LDA_pred$class, modeldata[te,'SUBSCRIBED'])
ARI_LDA



par(mfrow=c(3,1))
plot(LDA_model$posterior[,1], col=modeldata[te,1])
plot(LDA_model$posterior[,2], col=modeldata[te,1])
plot(LDA_model$posterior[,3], col=modeldata[te,1])

plot(m2$x,col=winedata[,1])
plot(m1)



# Checking LDA assumptions 

library('heplots')
boxM(Y= modeldata, group=modeldata$SUBSCRIBED)
#Homogeneity of covariance matrices is rejected so QDA should be preferred here. 

###########################################################
# LOGIT 
###########################################################

logit_model<- glm(SUBSCRIBED~.-marital-education-euribor3m, family=binomial, data=train)
summary(logit_model)

#mbin2<-predict(mbin)
mbin<-predict(logit_model, type="response", newdata = test)
table(modeldata$SUBSCRIBED[-te],round(logit_model$fitted))

#mbin2[1:10]
mbin[1:20]
#contrasts(data1$SUBSCRIBED)

#class1<- (mbin > 0.5)*1
#table(class1)
n2=length(mbin)
mbin4<-rep("no",n2)
mbin4[mbin > 0.09]="yes"
mbin4[1:10]
table(modeldata$SUBSCRIBED[te],mbin4)

mean(mbin4==modeldata$SUBSCRIBED[te])
#indicate correct predictions

model_logit$fitted.values



fit <- glm(am~hp+wt,data=mtcars,family='binomial')
pred.prob <- predict(fit,type='response')
brierScore <- mean((mbin4-modeldata$SUBSCRIBED[-te])^2)


#sum(brierscore(modeldata$SUBSCRIBED[te]~ model_logit$posterior))
#sum(brierscore(data2$SUBSCRIBED~m2$posterior))
threshold<-0.5

############## ROC curve for logistiv regression
###### on diabetes data



#pregnant glucose blood.press triceps

res2<-NULL
for (threshold in seq(0.01,0.99,by=0.01)){
  clas<- logit_model$fitted > threshold
  conftable<- table(clas,modeldata$SUBSCRIBED[-te])
  sens<- conftable[1,1]/apply(conftable,2,sum)[1]
  spec<- conftable[2,2]/apply(conftable,2,sum)[2]
  res2<-rbind(res2, c(sens,1-spec, threshold) )
}

res2<-res2[order(res2[,1]),]

plot(res2[,2],res2[,1], xlab="FN", ylab="TP", type="l",col=2, xlim=c(0,1), ylim=c(0,1))
abline(0,1)

dist<-(res2[,2]-0)^2+(res2[,1]-1)^2
min(dist)

res3 <-cbind(res2,dist)
res4 <- res3[order(res3[,4]),]
head(res4)
#sort(res3)

prop.table(table(data1$SUBSCRIBED))

ari <- adjustedRandIndex(mbin4, modeldata[te,'SUBSCRIBED'])
ari

###############################

z <- multinom(SUBSCRIBED ~ .-marital-education-euribor3m, data = train)
pr <- z$ypred - 1 
accuracy <- sum(modeldata[te,'SUBSCRIBED'] == pr)/dim(test)[1]
ari <- adjustedRandIndex(pr, modeldata[te,'SUBSCRIBED'])


################################################################################
#---------------------------- TIME FOR CLUSTERING ------------------------------
################################################################################



set.seed(1680) # for reproducibility
library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library('daisy')

cluster_data=subset(data,select=-c(8,9,14,15,16,17))

cluster_data=subset(cluster_data, select=-c(5,10,11))

#mono ta numeric
x<- scale(cluster_data)

n=nrow(cluster_data)
te<- sample(1:n, round(n*0.3), replace=FALSE)
variab<-1:8
sample_data <- cluster_data[te,variab]



gc()
gower_dist <- daisy(sample_data, metric = "gower")


gower_dist <- daisy(sample_data,
                    metric = "gower",
                    type = list(logratio = 3))



summary(gower_dist)


gower_mat <- as.matrix(gower_dist)
# Output most similar pair
sample_data[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


# Output most dissimilar pair
sample_data[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]




############# 
# k - means 
#############

set.seed(123)
mixedClusters<-kmeans(gower_dist, centers=4)

table(mixedClusters$cluster)


pairs(sample_data, col = sample_data$Age)


# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:6){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)
plot(1:6, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:6, sil_width)


pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
pam_results <- sample_data %>%
  dplyr::select(-name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary


k<-3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- sample_data %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
table1 <-pam_results$the_summary[[1]]
table2 <- pam_results$the_summary[[2]]
table3 <-pam_results$the_summary[[3]]

library("kableExtra")
kbl((table1), caption="Cluster 1 ", valine="t", alligh="H") %>%
  kable_styling()


library("kableExtra")
kbl((table2), caption="Cluster 2 ", valine="t", alligh="H") %>%
  kable_styling()

library("kableExtra")
kbl((table2), caption="Cluster 3 ", valine="t", alligh="H") %>%
  kable_styling()


k <- 3
agnes_clust <- agnes(x = diss_mat)
ag_clust <- cutree(agnes_clust, k)




sample_data[pam_fit$medoids, ]
gc()

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = sample_data$name)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


gc()


hc3 <- hclust(dist(sample_data),method="ward")
plot(hc3)
rect.hclust(hc3, k=3, border="red")
adjustedRandIndex(cutree(gower_dist, k = 3))


hc4 <- hclust(dist(sample_data),method="single")
plot(hc4)
rect.hclust(hc4, k=3, border="red")
adjustedRandIndex(cutree(hc4, k = 3),wine[,1])

hc5 <- hclust(dist(x),method="centroid")
plot(hc5)
rect.hclust(hc5, k=3, border="red")
adjustedRandIndex(cutree(hc5, k = 3),wine[,1])

ari <- adjustedRandIndex(gower_dist$cluster, cluster_data[,1])







