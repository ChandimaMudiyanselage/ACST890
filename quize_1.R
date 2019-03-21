##Question01
y=c(0.01,0.02, 0.03, 0.04) #a vector of effective half yearly interest rates
Coupay=function(C,F){
  p=seq(1, length(y), by=1)
  out=0
  for (k in 1:length(y)) {
    out=out+C*exp(-y[k]*p[k])
  }
  return(out+(F*exp(-y[length(y)]*p[length(y)])))
}
Coupay(6,100)

##-	Here we can give any value for C and F with a given vector of effective half yearly interest
#rates and calculate the price of the bond.  Above I've used C,F as 6, 100 respectively. 

##Question03
#A
dataset <- read.csv("singapore.economy.csv", header=TRUE, sep=",", stringsAsFactors = F)

#B
dataset <- na.omit(dataset)

head(dataset)

#C
plot(dataset$time, dataset$gdp, xlab="Time", ylab="GDP (%)", main ="GDP vs Time")

#D
library('dplyr')
dataset %>% group_by(period) %>% summarize(mean=mean(gdp), std=sd(gdp))
#or
##Means
mean_1<-mean(dataset$gdp[dataset$period==1])
mean_2<-mean(dataset$gdp[dataset$period==2])
mean_3<-mean(dataset$gdp[dataset$period==3])
##Variances
sd_1<-sd(dataset$gdp[dataset$period==1])
sd_2<-sd(dataset$gdp[dataset$period==2])
sd_3<-sd(dataset$gdp[dataset$period==3])

#using a matrix
MeanAndStd <- matrix(c(mean_1,mean_2,mean_3,sd_1,sd_2,sd_3),ncol=3,byrow=T)
colnames(MeanAndStd) <- c("1","2","3")
rownames(MeanAndStd) <- c("mean","sd")
MeanAndStd <- as.table(MeanAndStd)

#using daraframe
MeanAndStd <- data.frame(mean=c(mean_1,mean_2,mean_3), sd=c(sd_1,sd_2,sd_3), row.names=c(1,2,3))

#E
pairs(select(dataset, -c(time, period)))

#F
lr = lm(gdp~exp, dataset)
summary(lr)

##Comments: 
#P-value of "exp" is very small, which indicates that predictor is significant. 
#Thus, there's a relationship between "gdp" and "ëxp". 
#Adjusted R-squared value is small, i.e. the model fit isn't good. 

#G
mlr = lm(gdp~exp+epg+hpr+oil+gdpus+crd, dataset)
summary(mlr)
#Comments;
#Only "exp",  "epg" and "hpr" are significant at 5% level of significance out 
#of all predictors. As others, p-values are greater than 0.05. 
#Here also, R-squared suggests the model fit isn't good enough. 


#H
quantile(dataset$gdp, probs = 0.05 )
dataset=data.frame(dataset)
dataset$state = ifelse(dataset$gdp > quantile(dataset$gdp, 0.05) ,1,0) #1-Normal, 0-Crisis
logit <- glm(state ~ bci, data = dataset[dataset$time<=2007,], family = "binomial")
summary(logit)
train<-dataset[(dataset$time)<2008,]
test<-dataset[(dataset$time)>=2008,]
logit<-glm(state~bci,data=train, family=binomial)
logit_predict<-predict(logit,test,type="response")
logit.pred<-rep("crisis", nrow(test))
logit.pred[logit_predict>0.5]<-"normal"
table(test$state, logit.pred)



