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

# making sure that all jobs belong to the following categories
sum(!(data$job) %in% c("admin.","blue-collar","entrepreneur", "housemaid", "management","retired","self-employed", "services","student","technician","unemployed","unknown"))

#converting the Job variable into a factor
data$job <- as.factor(data$job)

# making sure that all marital status belong to the following categories 
sum(!(data$marital) %in% c("divorced","married", "single", "unknown"))

data$marital <- as.factor(data$marital)

# making sure that all education levels belong to the following categories 
sum(!(data$education) %in% c("basic.4y","basic.6y", "basic.9y", "high.school", "illiterate", "professional.course","university.degree","unknown"))

#data <- data %>% 
  #mutate(education = ifelse(education =="basic.4y", "",
   #                      ifelse(education =="basic.6y","secondary",
    #                            ifelse(education=="basic.9y","high.school",
     #                                  3))))

data$education<- as.factor(data$education)


# making sure that all data belongs to the categories "no", "yes" and "unknown" 
sum(!(data$default) %in% c("no","yes", "unknown"))
sum(!(data$housing) %in% c("no","yes", "unknown"))
sum(!(data$loan) %in% c("no","yes", "unknown"))

data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)

# making sure that all contact data belong to the categories cellular, telephone 
sum(!(data$contact) %in% c("cellular", "telephone"))

data$contact <- as.factor(data$contact)

# making sure that all months belong to the following categories
sum(!(data$month) %in% c("apr" ,"aug", "dec" ,"jul", "jun" ,"mar" ,"may", "nov", "oct" ,"sep"))
data$month<- as.factor(data$month)

# making sure that all week days belong to the following categories
sum(!(data$day_of_week) %in% c("fri", "mon", "thu", "tue", "wed"))
data$day_of_week <- as.factor(data$day_of_week)

# making sure that all previous outcomes belong to the following categories
sum(!(data$poutcome) %in% c("success", "nonexistent", "failure"))
data$poutcome <- as.factor(data$poutcome)

# making sure that the response variable has two levels "yes" and "no"
sum(!(data$SUBSCRIBED) %in% c("no", "yes"))
data$SUBSCRIBED <- as.factor(data$SUBSCRIBED)

#converting the following variables into numeric 
data$Age <- as.numeric(data$Age)
data$duration <- as.numeric(data$duration)
data$campaign <- as.numeric(data$campaign)
data$pdays <- as.numeric(data$pdays)
data$previous <- as.numeric(data$previous)

#making sure that all clients have a reasonable age 
sum(!(data$Age) %in% c(17:100))

#drop the variable number of employees 
data <- data[,-20]


#####################################################################################################################
#-------------------------------------------------- Descriptive Analysis -------------------------------------------#
#####################################################################################################################


#######################################################
#--------------- Numeric Variables -------------------#
#######################################################

#descriptive statistics for numeric variables 
require(psych)
index <- sapply(data, class) == "numeric"
datanum <- data[,index] 
head(datanum)
table1<-round(t(describe(datanum)),2)

library("kableExtra")
kbl((table1), caption="Statistics for Numeric Variables", valine="t", alligh="H") %>%
kable_styling()

library("kableExtra")
kbl(t(table1),digits = 18, caption = "Statistics for Numeric Variables", valine="t",allign="H") %>%
  kable_paper(full_width = F) %>%
  kableExtra::kable_styling(latex_options = "hold_position")

#Histograms

par(mfrow=c(2,2))

#Histogram for Age 
par(mfrow=c(2,2))

h1<-ggplot(data = data, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw()

#Histogram for duration
h2<-ggplot(data = data, aes(x = duration)) +
  geom_histogram(fill = "skyblue", color = "black", binwidth = 40) +
  theme_bw() 


#Histogram for campaign
h3<-ggplot(data = data, aes(x = campaign)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw() 

library("ggpubr")
ggarrange(h1,h2,h3, ncol=3, nrow=1)


#Histogram for pdays
ggplot(data = data, aes(x = pdays)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw() 

#Histogram for previous
ggplot(data = data, aes(x = previous)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw() 

#Histogram for emp variation
h4<-ggplot(data = data, aes(x = emp.var.rate)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw() 

#Histogram for cons.price.idx
h5<-ggplot(data = data, aes(x = cons.price.idx)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw() 

#Histogram for cons.conf.idx
h6<-ggplot(data = data, aes(x = cons.conf.idx)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw() 

#Histogram for euribor3m
h7<-ggplot(data = data, aes(x = euribor3m)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw()

ggarrange(h4,h5,h6,h7, nrow=1,ncol = 4)

#Histogram for number of employees
ggplot(data = data, aes(x = nr.employed)) +
  geom_histogram(fill = "skyblue", color = "black") +
  theme_bw()

#transformation of the variable pdays into discrete 
table(data$pdays)

data <- data %>% 
  mutate(pdays = ifelse(pdays %in% c(0, 999), 0,1))

data$pdays<-as.factor(data$pdays)

####################################################################
#-------------- ggplots with SUBSCRIBED variable--------------------
####################################################################


n<-nrow(data)
ggplot(data = data , aes( fill = SUBSCRIBED , x = pdays)) + geom_bar()

round(table(data$pdays)/n,2)


table2<-round(prop.table(table(data$SUBSCRIBED, data$pdays ),2) , 2)

library("kableExtra")
kbl((table2), caption="Percentages of Subscribed depending on a previous contact", valine="t", alligh="H") %>%
  kable_styling()


#Variable duration 
#However, we will not be able to know the duration of the call 

ggplot(data=data, aes(x = SUBSCRIBED, y = duration)) +
  geom_boxplot(fill="lightblue") +
  theme_bw()



##############################################
#- Boxplot between Age and Subscribed -------#
##############################################


ggplot(data=data, aes(x = SUBSCRIBED, y = Age)) +
  geom_boxplot(fill="lightblue") +
  theme_bw()


#Gia to job σε σχέση με το Subscribed 
#megalutero pososto twn retired kai ton students deixnei na apantaei yes pio syxna 
ggplot(data=data, aes(job, fill = SUBSCRIBED)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

table4<-round(prop.table(table(data$SUBSCRIBED, data$job ),2) , 2)

library("kableExtra")
kbl((table4), caption="Percentages of Subscribed depending on the Job Description", valine="t", alligh="H") %>%
  kable_styling()




################################################
#----------- Subscribed AND Age ---------------#
################################################

ggplot(data = data , aes( fill = SUBSCRIBED , x = job)) + geom_bar()  

install.packages("Hmisc")
library("Hmisc")

Agecut = cut2(data$Age, g=3 )
ggplot(data = data , aes( fill = SUBSCRIBED , x = Agecut)) + 
  geom_bar(position="dodge",color="black") +
  scale_fill_brewer(palette="Pastel1")+
  theme(axis.text.x = element_text(angle = 45, hjust =1))+
  theme_bw()

table3<-round(prop.table(table(Agecut, data$SUBSCRIBED)),2)


library("kableExtra")
kbl((table3), caption="Percentages of Subscribed per Age group", valine="t", alligh="H") %>%
  kable_styling()



#10% of the total phonecalls were successful during this period
n<-nrow(data)
round(table(data$SUBSCRIBED)/n,4)

###################################################
#---Subscribed per month and per day of week -----#
###################################################

h8<-ggplot(data = data , aes( fill = SUBSCRIBED , x = month)) + 
  geom_bar(position="dodge",color="black") +
  scale_fill_brewer(palette="Pastel1")+
  theme(axis.text.x = element_text(angle = 45, hjust =1))+
  theme_bw()

h9<-ggplot(data = data , aes( fill = SUBSCRIBED , x = day_of_week)) + 
  geom_bar(position="dodge",color="black") +
  scale_fill_brewer(palette="Pastel1")+
  theme(axis.text.x = element_text(angle = 45, hjust =1))+
  theme_bw()

ggarrange(h8,h9)

###################################################
#-------- Subscribed per previous outcome --------#
###################################################

ggplot(data = data , aes( fill = SUBSCRIBED , x = poutcome)) + 
  geom_bar(position="dodge",color="black") +
  scale_fill_brewer(palette="Pastel1")+
  theme(axis.text.x = element_text(angle = 45, hjust =1))+
  theme_bw()

table5<-round(prop.table(table(data$SUBSCRIBED,data$poutcome)),2)

library("kableExtra")
kbl((table5), caption="Percentages of Subscribed per Previous Outcome", valine="t", alligh="H") %>%
  kable_styling()


#############################################
#--Visual Analysis for numerical variables--#
#############################################

n <- nrow(datanum)

#histograms
par(mfrow=c(3,3))
for (i in 1:9){
  hist(datanum[,i], main=names(datanum)[i], xlab=names(datanum)[i], col='light blue')
}

#QQ plots
par(mfrow=c(3,3))
for (i in 1:9){
  qqnorm(datanum[,i], main=names(datanum)[i],col='blue')
  qqline(datanum[,i])
}

#Boxplots 
par(mfrow=c(3,3))
c=c(1,2,6,7,8,9)
for (i in c){
  boxplot(datanum[,i], main=names(datanum)[i],col='light blue', horizontal=TRUE)
}



#######################################
#---- Visual Analysis for factors ----#
#######################################

#extracting categorical variables
require(psych)
datafac <- data[,!index] 
n <- nrow(datafac)

############################################################
#---- Visualize lOAN / DEFAULT AND HOUSING VARIABLES ------#
############################################################

par(mfrow=c(1,1))
fac <-datafac[,c(4,5,6)]
barplot(sapply(fac,table)/n, horiz=T, las=1, col=2:4, ylim=c(0,8), cex.names=0.9)
legend('center', fil=2:4, legend=c('No','Unknown','Yes'), ncol=3, bty='n',cex=1.2)

table(data$default)
table(data$loan)

####################################################
# Frequency tables for months and Weekdays! -------#
####################################################

par(mfrow=c(1,2))
for (i in c(9,10)){
  barplot(table(data[,i]), col="light blue", xlab=names(data)[i], ylab='Relative frequency')
}

###################################################
# ---- Pie charts for Marital and Education ----- #
###################################################

install.packages("plotrix")
library("plotrix")
par(mfrow=c(1,2))
pie3D(table(data$marital),
      col = hcl.colors(4, "Spectral"),
      labels = levels(data$marital),
      labelcex=0.80,
      main="Marital Status")


pie3D(table(data$education), explode=0.1,
      col = hcl.colors(8, "Spectral"),
      labels = levels(data$education), 
      labelcex= 0.80,
      main = "Education Levels")

####################################
# --- Pie Chart for SUBSCRIBED --- # 
####################################

pie3D(table(data$SUBSCRIBED),
      col = hcl.colors(2, "Pastel1"),
      labels = levels(data$SUBSCRIBED), 
      labelcex= 0.80,
      main="SUBSCRIBED")

round(prop.table(table(data$SUBSCRIBED)),2)

# And this plot maybe
library(plotrix)
x<- table(data$education)
barp(x,col=2, cylindrical=T,
     shadow=T, names.arg=names(x))



##################################
#----- Pairwise comparisons -----#
##################################

#Pairs of numerical variables
pairs(datanum)

#better visualization with corrplot
install.packages("corrplot")
require(corrplot)
corrplot(cor(datanum))





############################
#------ Correlations ------#
############################

#Correlations valid on the numerical variables
round(cor(datanum), 2)  

require(corrplot)
par(mfrow = c(1,1))
corrplot(cor(datanum), method = "number") 

###################################################################################################################################
#------------------------------------------ Descriptive Models - Model Selection -------------------------------------------------# 
###################################################################################################################################

library("Hmisc")

#Creating a copy dataset
data1 <- data

# Transform the Age variable into AgeGroup with 3 levels 

data1$Age <- cut2(data1$Age, g=3 )





########################################################
########################LASSOO##########################
########################################################

mylogit <- glm(SUBSCRIBED ~. -default, data = data1, family = "binomial")
mylogit2 <- glm(SUBSCRIBED ~. -default -emp.var.rate -cons.price.idx , data = data1, family = "binomial")

mylogit4 <- glm(SUBSCRIBED ~. -default -emp.var.rate -cons.price.idx -poutcome, data = data1, family = "binomial")

summary(mylogit)
summary(mylogit2)
summary(mylogit3)

vif(mylogit)
vif(mylogit2)

####### AIC method #########
m1 <- step(mylogit, direction='both')

m2 <- step(mylogit2, direction='both')

n<-nrow(data1)
############ BIC METHOD ##############
b1 <- step(mylogit, direction='both', k = log(n))

vif(m1)
modelm1<-glm(SUBSCRIBED ~. -default-marital-education-housing-loan-previous-poutcome-emp.var.rate-month-cons.price.idx-day_of_week, data = data1, family = "binomial")

modelm2<-glm(SUBSCRIBED ~. -default-marital-education-housing-loan-previous-emp.var.rate-month-cons.price.idx-campaign, data = data1, family = "binomial")

vif(modelm1)
summary(modelm1)


###### loG RATIO TEST ###############

timi<-2*(logLik(modelm1)-logLik(modelm2))
timi

q<-qchisq(0.95,1)
q
# an timi>q tote aporiptw Ho
#Ho b1=b2=b3=0
#H1 oti ena apo ayta diaforetiko


round(exp(coef(modelb1)),3)

summary(modelm1)

########### BIC #############
vif(b1)
modelb1<-glm(SUBSCRIBED ~. -default-job-marital-education-housing-loan-day_of_week-campaign-pdays-previous-emp.var.rate-month-cons.price.idx, data = data1, family = "binomial")

vif(modelb1)

modelb2<-glm(SUBSCRIBED ~. -default-marital-education-housing-loan-previous-emp.var.rate-month-poutcome-cons.price.idx-day_of_week, data = data1, family = "binomial")

summary(modelb2)

logLik(modelm1)
logLik(modelb1)


summary(modelb1)$coefficients %>% as.data.frame() %>% round(3) %>% HTML(file = "coefs.html")



plot(modelb1$fitted.values, modelb1$y,
     col = rgb(0,0,1,alpha = 0.01),
     pch = 14)



summary(glm(step_formula, data =dt, family = "binomial"))




install.packages("R2HTML")
library("R2HTML")
coef(modelb1) %>% as.data.frame() %>% round(2) %>% HTML(file = "coefs.html")
getwd()

table78<-summary(modelb1)


install.packages('sjPlot')
# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

b2<-step(mylogit2,direction = "both", k = log(n))

b3 <-step(mylogit4,direction = "both", k = log(n))
vif(b2)

modelb1 <- glm(SUBSCRIBED ~. -default-emp.var.rate -job-marital-education-housing-loan-day_of_week-campaign-pdays-previous-month-cons.price.idx, data = data1, family = "binomial")

modelb2 <- glm(SUBSCRIBED ~. -default-emp.var.rate -job-marital-education-housing-loan-day_of_week-previous-cons.price.idx -poutcome-month, data = data1, family = "binomial")
vif(b2)
vif(modelb2)

summary(modelb2)
summary(modelb1)

library("car")
vif(b1)
summary(m1)
summary(m2)
summary(b1)
summary(b2)

vif(modelb1)
AIC(modelb1)
AIC(mylogit)
logLik(mylogit)
logLik(modelb1)
summary(modelb1)


vif(b2)
vif(b3)
summary(b3)

summary(b3)
logLik(b3)
logLik(b2)
logLik(b1)

vif(b3)

logLik(b3)

logLik(b2)

install.packages("car")
library("car")
vif(b1)
vif(m2)


#####################################
################ LASSO ##############
#####################################

library(glmnet)
lambdas <- 10 ^ seq(8,-4,length=250)
x_matrix <- model.matrix(mylogit)[,-1]
x_matrix2 <- model.matrix(mylogit2)[,-1]

#η εντολή είναι αυτή και θα δώσουμε τον πίνακα σχεδιασμού του μοντέλου χωρίς ρην στήλη των άσσων. για alpha =1 exeis lasso gia 0 ridge kai gia endiamesa exeis to mixed
# ισως χρειαστεί να πάρουμε πάνω απο 250 τιμές 

fit_lasso <- glmnet(x_matrix, data1$SUBSCRIBED, alpha=1, lambda=lambdas, family="binomial")
fit_lasso1 <- glmnet(x_matrix2, data1$SUBSCRIBED, alpha=1, lambda=lambdas, family="binomial")


plot(fit_lasso, xvar = "lambda", label = TRUE)
plot(fit_lasso1, xvar = "lambda", label = TRUE)

# Cross Validation 
lasso.cv <- cv.glmnet(x_matrix,data1$SUBSCRIBED, alpha=1, lambda=lambdas, family="binomial", type.measure='class')
plot(lasso.cv)
#coef(lasso.cv, s = lasso.cv$lambda.min)	# min is somewhat sensitive to specific run
coef(lasso.cv, s = lasso.cv$lambda.1se)	# the most regularized model such that error is within one standard error of the minimum

vif(lasso.cv)

lasso.cv <- cv.glmnet(x_matrix2,data1$SUBSCRIBED, alpha=1, lambda=lambdas, family="binomial", type.measure='class')
plot(lasso.cv)
#coef(lasso.cv, s = lasso.cv$lambda.min)	# min is somewhat sensitive to specific run
coef(lasso.cv, s = lasso.cv$lambda.1se)	# the most regularized model such that error is within one standard error of the minimum

modellasso <- glm(SUBSCRIBED ~. -default -emp.var.rate -cons.price.idx -marital -education -housing -loan-contact- day_of_week-campaign-previous, data = data1, family = "binomial")
summary(modellasso)
vif(modellasso)

vif(fit_lasso)

n<-nrow(data1)
b1 <- step(modellasso, direction='both', k = log(n))
summary(b1)
vif(b1)
modelBIC <-glm(SUBSCRIBED ~. -default-job-poutcome-month -emp.var.rate -cons.price.idx -marital -education -housing -loan-contact- day_of_week-campaign-previous, data = data1, family = "binomial")
vif(modelBIC)
summary(modelBIC)


mylogit3 <- glm(SUBSCRIBED ~. -default -emp.var.rate -cons.price.idx -housing -loan-contact- day_of_week-campaign-previous-poutcome-month, data = data1, family = "binomial")

summary(mylogit3)
vif(mylogit3)

b3<-step(mylogit3,direction = "both", k = log(n))
summary(b3)
vif(b3)


facta1 <-table(data1$education,data1$marital)
chisq.test(facta1)


##########################
# Likelihood Ratio Test  #
##########################


# 2(LLa -LLo) 

mylogit2 <- glm(SUBSCRIBED ~. -default -emp.var.rate -cons.price.idx , data = data1, family = "binomial")

mylogit6 <- glm(SUBSCRIBED ~. -default -emp.var.rate -cons.price.idx -month, data = data1, family = "binomial")



logitnull <- glm(SUBSCRIBED ~1 , data = data1, family = "binomial")

summary(logitnull)
summary(mylogit2)

timi<-2*(logLik(mylogit2)-logLik(mylogit6))
timi

q<-qchisq(0.95,9)
q
# an timi>q tote aporiptw Ho
#Ho b1=b2=b3=0
#H1 oti ena apo ayta diaforetiko

wald.test(b=coef(mylogit2), Sigma = vcov(mylogit2), Terms = c(1:12))


plot(b3$fitted, data1$SUBSCRIBED, col=data1$SUBSCRIBED+1, pch=19)


# Independent
# standardized residuals plot
plot(rstandard(modelb1), main = "Standardized Residuals Plot", ylab = "Residuals")

#Linear
plot(fitted(mylogit2), rstandard(mylogit2), main = "Fitted Values vs Standardized Residuals", ylab = "Residual")
abline(h = 0, col = 'red', lty = 2)


####################
# AIC Critirion    #
####################

AIC(modelb1)
























