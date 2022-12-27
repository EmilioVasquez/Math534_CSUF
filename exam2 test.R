#Emilio Vasquez
#Professor Nichols

setwd("C:/Users/Emilio/Downloads")
data = read.csv("midterm1.csv", h = T)
attach(data)
head(data)

model = lm(SuggestedRetailPrice~ Hybrid + EngineSize + Cylinders + Horsepower + HighwayMPG + Weight + WheelBase ) 
model
summary(model)

par(mfrow=c(2,2))
plot(model)
pairs(SuggestedRetailPrice~ Hybrid + EngineSize + Cylinders + Horsepower + HighwayMPG + Weight + WheelBase)
pairs(~EngineSize + Cylinders + Horsepower + HighwayMPG + Weight + WheelBase)
hist(data$SuggestedRetailPrice)

y.hat=model$fitted.values
par(mfrow=c(1,1))
plot(y.hat, SuggestedRetailPrice)
abline(lm(SuggestedRetailPrice~y.hat))

##Have issues with residual diagnostics. Non constant variance from residual diagnostics.
##Plotting y.hat vs actual y, we have a linearity issue.

new.y = log(SuggestedRetailPrice)
log.y.model = lm(new.y~ Hybrid + EngineSize + Cylinders + Horsepower + HighwayMPG + Weight + WheelBase ) 
summary(log.y.model)

par(mfrow=c(2,2))
plot(log.y.model)

y.hat=log.y.model$fitted.values
par(mfrow=c(1,1))
plot(y.hat, xlab="Fitted Values", new.y, ylab="Log(SuggestedRetailPrice)")
abline(lm(new.y~y.hat))



pairs(new.y~ Hybrid + EngineSize + Cylinders + Horsepower + HighwayMPG + Weight + WheelBase)
pairs(~EngineSize + Cylinders + Horsepower + HighwayMPG + Weight + WheelBase)



##We've improved some linearity issues with our fitted values, as well as
##constant variance. But, plotting our predictors against each other
##they do not show that they have a linear relationship amongst each other
##and we do not have a valid linear model quite yet. Transformations will
##need to be made

log.engine=log(EngineSize)
log.cylinder=log(Cylinders)
log.HP = log(Horsepower)
log.MPG=log(HighwayMPG)
log.wt= log(Weight)
log.wb=log(WheelBase)


inv.MPG= (1/HighwayMPG)


model3 = lm(new.y~ Hybrid + EngineSize + Cylinders + Horsepower + inv.MPG + Weight + WheelBase ) 
summary(log.y.model)
par(mfrow=c(2,2))
plot(model3)

pairs(~ Hybrid + EngineSize + Cylinders + Horsepower + inv.MPG + Weight + WheelBase)

##Taking the reciprocal of MPG did wonders on gettting all the plots
##in the same positive direction, we linearity is still an issue. 
##More transformations must be made.


model4 = lm(new.y~ Hybrid+ log.engine + log.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb ) 
summary(model4)
par(mfrow=c(2,2))
plot(model4)

pairs(~log.engine + log.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb)


y.hat=model4$fitted.values
par(mfrow=c(1,1))
plot(y.hat, xlab="Fitted Values", new.y, ylab="Log(SuggestedRetailPrice)")
abline(lm(new.y~y.hat))

##Smoewhat closer, but in all the plots related to the engine, we have some curvature.
##So, the next transform on teh list is a sqrt. 
##Closer linear relationship amongst the predictors. 

sqrt.engine=sqrt(EngineSize)

model5= lm(new.y~ Hybrid+ sqrt.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb ) 
summary(model5)
pairs(~Hybrid+ sqrt.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb )

##Sort of like a puzzle. Trying to create linear relationships between all the predictors.
##Now have lineaity betweeen enginesize and horsepower, but all the other graphs
##involving EngineSize  are still not linear. 

sqrt.engine=sqrt(EngineSize)
model6= lm(new.y~ Hybrid+ sqrt.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb ) 
summary(model6)
pairs(~Hybrid+ sqrt.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb )


par(mfrow=c(2,2))
plot(model6)

StanRes1 <- rstandard(model6)
par(mfrow=c(3,2))

plot(Hybrid,StanRes1, ylab="Standardized Residuals")
plot(sqrt.engine,StanRes1, ylab="Standardized Residuals")
plot(log.cylinder,StanRes1, ylab="Standardized Residuals")
plot(log.HP,StanRes1, ylab="Standardized Residuals")
plot(inv.MPG,StanRes1, ylab="Standardized Residuals")
plot(log.wt,StanRes1, ylab="Standardized Residuals")

par(mfrow=c(2,2))
plot(model6)

##So the model has imrpoved a lot. But we need to address the outliers. 

data1= data[-67,]
attach(data1)
detach(data)


model6= lm(new.y~ Hybrid+ sqrt.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb ) 
summary(model6)
pairs(~Hybrid+ sqrt.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb )


y.hat=model6$fitted.values
par(mfrow=c(1,1))
plot(y.hat, log(SuggestedRetailPrice))

StanRes1 <- rstandard(model6)
par(mfrow=c(3,2))

plot(Hybrid,StanRes1, ylab="Standardized Residuals")
plot(sqrt.engine,StanRes1, ylab="Standardized Residuals")
plot(log.cylinder,StanRes1, ylab="Standardized Residuals")
plot(log.HP,StanRes1, ylab="Standardized Residuals")
plot(inv.MPG,StanRes1, ylab="Standardized Residuals")
plot(log.wt,StanRes1, ylab="Standardized Residuals")

par(mfrow=c(2,2))l;/
plot(model6)


EngineSize^(1/3) = 3rdEngine

model7= lm(new.y~ Hybrid+ I(arcsin(EngineSize)) + log.cylinder + log.HP + inv.MPG + log.wt + log.wb ) 
summary(model7)
pairs(~Hybrid+ I(cos(EngineSize)) + log.cylinder + log.HP + inv.MPG + log.wt + log.wb )


StanRes1 <- rstandard(model6)
par(mfrow=c(3,2))


##Need to investigage teh outlier. I believe that if we investigate why it's an outlier
##and take it out, our model could potentially be improve and 


rm(list=ls())

setwd("C:/Users/Emilio/Downloads")

data = read.csv("midterm1.csv", h = T)
attach(data)
head(data)

data2= data[-88,]

new.y = log(SuggestedRetailPrice)
log.engine=log(EngineSize)
log.cylinder=log(Cylinders)
log.HP = log(Horsepower)
log.MPG=log(HighwayMPG)
log.wt= log(Weight)
log.wb=log(WheelBase)
inv.MPG= (1/HighwayMPG)
sqrt.engine=sqrt(EngineSize)


model7=lm(new.y~ Hybrid+ sqrt.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb ) 
summary(model7)
pairs(~Hybrid+ sqrt.engine + log.cylinder + log.HP + inv.MPG + log.wt + log.wb )

par(mfrow=c(2,2))
plot(model7)

model8=lm(new.y~ Hybrid+ sqrt.engine + log.cylinder + log.HP + log.wt ) 
summary(model8)
AIC(model8)
AIC(model7)

##Our finalized model is model8

par(mfrow=c(2,2))

y.hat=model$fitted.values
par(mfrow=c(1,1))
plot(y.hat, xlab="Residuals", SuggestedRetailPrice)
abline(lm(SuggestedRetailPrice~y.hat))


transy.hat=model8$fitted.values
par(mfrow=c(1,1))
plot(transy.hat, xlab="Transformed Residuals", new.y, ylab="Log(SuggestedRetailPrice)")
abline(lm(new.y~transy.hat))

par(mfrow=c(2,2))


summary(model8)
plot(model8)
pairs(~Hybrid+ sqrt.engine + log.cylinder + log.HP + log.wt)

Cyli

####Have to set the values to each predictor
hankengine= data.frame(sqrt.engine=sqrt(3.5))
sixcyl= data.frame(log.cylinder=log(6))
hankhp = data.frame(log.HP=log(210))
wt= data.frame(log.wt=log(4210))
gas=data.frame(Hybrid=0)

newdata=data.frame(sixcyl, hankengine , hankhp , wt , gas)

##When we find the average, use confidence. 

exp(predict(model8, newdata , interval= "confidence"))

par(mfrow=c(1,1))
hist(data$SuggestedRetailPrice, xlab="Suggested Retail Price")


