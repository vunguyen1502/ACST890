#.	Github username: vunguyen1502
#.	Public repository: ACST890
#.	file name: 44284381NguyenLeAnhVuTHQ1.R

#Question 1
bondprice <- function (C,F,y,n) {#Name the function "bondprice" with C, F, y, n as input
  P = (sum(C*exp(-y*c(1:n))))+(F*exp(-y[n]*n)) 
  # This is the code for the formula given in question 1
  # In detail:
  # - The first part "(sum(C*exp(-y*c(1:n))))" give you the present value of the coupon payment
  #  + y will be input as a vector with length(y) = input n. Otherwise, it will give an error because it is a vector*vector
  #  + (C*exp(-y*c(1:n))) is the vector contain all the present value of all coupon payments
  #  + The sum() function presents the summation (uppercase sigma) in the formula. It will sum all the value in the vector.
  
  # - The second part "(F*exp(-y[n]*n))" give you the present value of the face value
  
  
  # The code below give a warning if the number of interest rate input is not equal to the number of coupon payment
  if( any(is.na(P))) warning('Number of interest rate must equal to number of coupon payment')
  return (P)
}
#To get the bond price, use function bondprice(C, F, y, n). In which:
#C is coupon payment
#F is the face value
#y is the interest rate for maturity tj (must input as vector [c(yt1,yt2,....,ytn)])
#n is the number of coupon payments
#below is an example code

bondprice(5000,7000,c(0.01,0.05,0.08),3)


#Question 3a
library(readr)
dataset <- read_csv("D:/Work/ACST890/Quiz 1/singapore.economy.csv") #set your directory here
View(dataset) #change the name to dataset


#Question 3b
omdataset<-na.omit(dataset) #this code will omit the NA record
omdataset #this is to show the result

#Question 3c
GDP.time<-omdataset[,c(1,3)] #set GDP.time is the omitted dataset with 2 columm (Time, GDP)
plot(GDP.time,type="l",xlab="Time",ylab="GDP (%)",main="Singapore GDP growth")

#Question 3d
#I set a ,b ,c as the data of GDP for period 1, 2, 3.
GDP.period<-omdataset[,c(2,3)]
a <- subset(GDP.period,period == 1)
b <- subset(GDP.period,period == 2)
c <- subset(GDP.period,period == 3)
#The code below is to create a table and put my answers in it.
#To check individual answers. Type mean(a[[2]]) and sd(a[[2]])[change a to b, c for period 2, 3]
mean.sd <- matrix(c(1,mean(a[[2]]), sd(a[[2]]),2,mean(b[[2]]), sd(b[[2]]),3,mean(c[[2]]), sd(c[[2]])),ncol=3, byrow=TRUE)
colnames(mean.sd) <- c("period","Mean","Standard deviation")
mean.sd

#Question 3e
pairs(~gdp+exp+epg+hpr+gdpus+oil+crd+bci,data=omdataset,main="Scatterplot")

#Question 3f
gdp.exp<-lm(formula=gdp~exp,data=omdataset)
summary(gdp.exp)
#The F-statistic and p-value provide concrete proof against null hypothesis.Therefore, we conclude exp is a useful predictor to predict gdp

#question 3g
gdp.multi<-lm(formula=gdp~exp+epg+hpr+oil+gdpus+crd,data=omdataset)
summary(gdp.multi)
#The result of p-value shows that hpr, oil, gdpus, crd have minor effect on gdp
#While exp and epg have significant effect on gdp. Therefore, we conclude exp and epg are useful predictor to predict gdp

#question 3h
quan.gdp <-quantile(omdataset$gdp, probs = 0.05) #This code calculate the 5% quantile of gdp
quan.gdp

#This code creates a vector of True False (true if GDP > 5% quantile)
statecheck <- with(omdataset, gdp>quan.gdp)

#The code below changes False value to crisis and true value to normal
state <- replace(statecheck,statecheck==FALSE,"CRISIS")
state <- replace(state,state==TRUE,"NORMAL")

#The code below adds the column state (Normal, Crisis) and statecheck(True, False) to the data
statedata <- data.frame(omdataset,state,statecheck)

# Filter the data from the begin to 2007
train.data <- subset(statedata,time < 2008)# Filter the data from the begin to 2007

# Fit the logistic regression
state.fit=glm(formula = statecheck~bci, data=train.data, family = binomial)#Create logistic regression model
summary(state.fit)

#The code below here is to calculate the confusion matrix
state.probs=predict(state.fit,type="response")
contrasts(statecheck)
state.pred=rep("FALSE",110)
state.pred[state.probs>0.5]="TRUE"
table(state.pred,statecheck)#This is the confusion matrix
mean(state.pred==statecheck)#This is the confusion matrix








#Please refer to the next page for question 2