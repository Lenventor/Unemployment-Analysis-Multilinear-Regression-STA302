Combined = read.csv(
  file = "./Combined.csv")
X1 = Combined$GDP
X2 = Combined$Immigrant
X3 = Combined$Investment
X4 = Combined$Rate
X5 = Combined$CPI
Y = Combined$Unemployment
#EDA by plotting each variable individually
plot(X1, Y)
plot(X2, Y)
plot(X3, Y)
plot(X4, Y)
plot(X5, Y)
#Identifying outlier variable-wise 
#(basically getting the index of each outlier
#note that 82,83 are always problematic
fitX1 = lm(Y~X1, data = Combined)
summary(fitX1)
plot(fitX1)
fitX2 = lm(Y~X2, data = Combined)
summary(fitX2)
plot(fitX2)
fitX3 = lm(Y~X3, data = Combined)
summary(fitX3)
plot(fitX3)
fitX4 = lm(Y~X4, data = Combined)
summary(fitX4)
plot(fitX4)
fitX5 = lm(Y~X5, data = Combined)
summary(fitX5)
plot(fitX5)
#scatterplot matrix
Combined.subset = subset(
  Combined, select = c(
    Unemployment,GDP,Immigrant, Investment, Rate, CPI))
pairs(Combined.subset, lower.panel = NULL)

#Remove 82, 83
Combined1 = Combined[-c(82,83),]
Combined1.subset = subset(
  Combined1, select = c(
    Unemployment,GDP,Immigrant, Investment, Rate, CPI))
#plot again
pairs(Combined1.subset, lower.panel = NULL)
#Boxplots and histograms to identify skews
par(mfrow=c(2,3))
for(i in c("GDP", "Immigrant", "Investment", "Rate", "CPI")){
  boxplot(Combined1[,i], main=paste0("Boxplot of ", i), xlab=i
          , horizontal=T)
}
par(mfrow=c(2,3))
for(i in c("GDP", "Immigrant", "Investment", "Rate", "CPI")){
  hist(Combined1[,i], main=paste0("Hist of ", i), xlab=i
  )
}

#Divide the data into Training and Testing sets
library(MASS)
library(caTools)
set.seed(101)
Sample = sample.split(Combined1$TIME, SplitRatio = 0.5)
train  <- subset(Combined1, Sample == TRUE)
test   <- subset(Combined1, Sample == FALSE)

#Fit a preliminary model to the train set with all variables
Y2 = train$Unemployment
X12 = train$GDP
X22 = train$Immigrant
X32 = train$Investment
X42 = train$Rate
X52 = train$CPI

fit3 = lm(Y2~X12+X22+X32+X42+X52, data = train)
summary(fit3)
vif(fit3)
#R^2 = 0.4656
par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(3,2))
r1 = fit3$residuals
#diagnostics
plot(X12[-c(64)], r1, xlab = "GDP", ylab = "Residuals")
plot(X22[-c(64)], r1, xlab = "Immigrants", ylab = "Residuals")
plot(X32[-c(64)], r1, xlab = "Investments", ylab = "Residuals")
plot(X42[-c(64)], r1, xlab = "Prime Rate", ylab = "Residuals")
plot(X52[-c(64)], r1, xlab = "Inflation", ylab = "Residuals")
AIC(fit3)
vic(fit3) #to check multicollinearity
#Eliminate all insignificant variables
#setup a new model fit6 that removes insignificant terms
fit6 = lm(Y2~X22+X42, data = train)
summary(fit6)
AIC(fit6)
BIC(fit6)
#perform a F-test
anova(fit6)
anova(fit6, fit3)
fit4 = lm(Y2~X12+X22+X42, data=train)
AIC(fit4)
BIC(fit4)
anova(fit6, fit4)
#p-value = 0.57, we fail to reject the null hypothesis
#Add a predictor to the reduced model, then check again
fit5 = lm(Y2~X22+X32+X42, data=train)
AIC(fit5)
BIC(fit5)
anova(fit6, fit5)
#p = 0.2212
fit7 = lm(Y2~X22+X42+X52, data=train)
AIC(fit7)
BIC(fit7)
anova(fit6, fit7)
#p = 0.1248
#Since the loss of R^2adj value is not significant, select
#the model with less parameters, fit6.
#perform diagnostics
par(mfrow=c(2,2))
plot(fit6)
r2 = fit6$residuals
plot(X22[-c(64)], r2, xlab = "Immigrants", ylab = "Residuals")
plot(X42[-c(64)], r2, xlab = "Prime Rate", ylab = "Residuals")
#bad predictor vs. residuals, qq-plot
#perform transformations of X2, X4, Y
summary(powerTransform(cbind(train[3],train[5],train[7])))
#for X2, X4, a value of -.6
#reciprocal sqrt transform or reciprocal (-1/-.5)
#two models are generated one with each method
recipsqrtX22 = 1/sqrt(X22)
recipsqrtX42 = 1/sqrt(X42)
recipX22 = 1/X22
recipX42 = 1/X42
#a new fit is generated from the new transformation
fit8 = lm(Y2~recipsqrtX22+recipsqrtX42, data=train)
summary(fit8)
r3 = fit8$residuals
par(mfrow=c(2,2))
plot(fit8)
#Now with the simpler transform
fit9 = lm(Y2~recipX22+recipX42, data=train)
r4 = fit9$residuals
plot(recipX22, r4, xlab = "1/Immigrants", ylab = "Residuals")
plot(recipX42, r4, xlab = "1/Prime Rate", ylab = "Residuals")
#behavior pretty similar to fit8, use the simpler model

#Apply model to testing set
Y_test = test$Unemployment
X2_test = test$Immigrant
X4_test = test$Rate

recipsqrtX2 = 1/sqrt(X2_test)
recipX2 = 1/X2_test
recipX4 = 1/X4_test
recipsqrtX4 = 1/sqrt(X4_test)

#Test linear model
test_model_1 = lm(Y_test~X2_test+X4_test, data = test)
summary(test_model_1)
qqnorm(test_model_1$residuals)
qqline(test_model_1$residuals)
plot(X2_test, test_model_2$residuals, 
     xlab = "Immigration", ylab = "Residuals")
plot(X4_test, test_model_2$residuals, 
     xlab = "Rate", ylab = "Residuals")
#Conclusion: severe deviation of normality 
#Test recip sqrt model
test_model_2 = lm(Y_test~recipsqrtX2+recipsqrtX4, data = test)
summary(test_model_2)
par(mfrow = c(2,2))
plot(test_model_2)
plot(recipsqrtX2, test_model_2$residuals, 
     xlab = "1/sqrt(Immigration)", ylab = "Residuals")
plot(recipsqrtX4, test_model_2$residuals, 
     xlab = "1/sqrt(Rate)", ylab = "Residuals")
#Test reciprocal model
test_model_3 = lm(Y_test~recipX2+ recipX4, data = test)
summary(test_model_3)
#multicolllinearity
vif(test_model_3)
vif(fit9)
#testing assumptions
par(mfrow = c(2,2))
plot(test_model_3)
par(mfrow=c(1,2))
plot(
  recipX2, test_model_3$residuals, 
  xlab = "1/Immigration", ylab = "Residuals")
#Not ideal, OK
plot(
  recipX4, test_model_3$residuals, 
  xlab = "1/Rate", ylab = "Residuals")
#Conclusion: assumptions reasonably satisfied