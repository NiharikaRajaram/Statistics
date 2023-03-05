data<-read.csv("C:/Users/nihar/OneDrive/Documents/Niharika_UTD/Fall_2022/Stats_for_DS/MiniProject6/prostate_cancer.csv") 

boxplot(data$psa, main="Distribution of PSA levels")

boxplot(log(data$psa), main="Distribution of PSA levels(log transform)")

# cancervol
data.log<-log(data$psa)

plot(data$cancervol, data.log, main="Transformed PSA level VS cancervol") 

test1 <- lm(data.log ~ cancervol, data = data)

abline(test1)

summary(test1)

# weight
plot(data$weight, data.log, main="Transformed PSA level VS weight") 

test2 <- lm(data.log ~ weight, data = data)

abline(test2)

summary(test2)

# Age
plot(data$age, data.log, main="Transformed PSA level VS age") 

test3 <- lm(data.log ~ age, data = data)

abline(test3)

summary(test3)


# Benpros
plot(data$benpros, data.log, main="Transformed PSA level VS benpros") 

test4 <- lm(data.log ~ benpros, data = data)

abline(test4)

summary(test4)

# Vesinv
plot(data$vesinv, data.log, main="Transformed PSA level VS vesinv") 

test5 <- lm(data.log ~ vesinv, data = data)

abline(test5)

summary(test5)

# Capspen
plot(data$capspen, data.log, main="Transformed PSA level VS Capspen") 

test6 <- lm(data.log ~ capspen, data = data)

abline(test6)

summary(test6)

# gleason
plot(data$gleason, data.log, main="Transformed PSA level VS Gleason") 

test7 <- lm(data.log ~ gleason, data = data)

abline(test7)

summary(test7)


# Fit 1

fit1 <- lm(data.log ~ cancervol + factor(vesinv) + capspen + gleason + benpros, data=data)
summary(fit1)

# Fit 2
fit2 <- lm(data.log ~ cancervol + factor(vesinv) + gleason+benpros, data=data)
summary(fit2)

# Anova - Fit1, Fit2
anova(fit1, fit2)

# Fit 3
fit3 <- lm(data.log ~ cancervol + factor(vesinv) + weight + gleason+benpros, data=data)
summary(fit3)

# Fit 4
fit4 <- lm(data.log ~ cancervol + factor(vesinv) + age + gleason+benpros, data=data)
summary(fit4)

bestfit<-fit2 
(bestfit)

#Analysing our model 
plot(fitted(bestfit), resid(bestfit), main="Residual Plot") 
abline(h=0)

#Normal Q-Q plot of our model 
qqnorm(resid(bestfit)) 
qqline(resid(bestfit))

#Time series plot of out=r model 
plot(resid(bestfit), type="l", main="Time Series Plot",ylim = c(-2,2)) 
abline(h=0)

#Backward AIC 
bestfit.backward <- step(lm(data.log ~ cancervol + weight + age + benpros + factor(vesinv) + capspen + gleason, data=data), scope = list(lower = ~-1), direction = "backward")

#Forward AIC 
bestfit.forward <- step(lm(data.log ~ 1, data=data), scope = list(upper = ~cancervol + weight + age + benpros + factor(vesinv) + capspen + gleason), direction = "forward")

#Both AIC 
bestfit.both <- step(lm(data.log ~ cancervol + weight + age + benpros + factor(vesinv) + capspen + gleason, data=data), scope = list(upper = ~cancervol + weight + age + benpros + factor(vesinv) + capspen + gleason), direction ="both")

#Predicting cancervol 
cancervol <- mean(data$cancervol) 
cancervol

#Predicting Benpros 
benpros <- mean(data$benpros) 
benpros

#Predicting vesinv 
vesinv.t <- table(factor(data$vesinv)) 
vesinv <- names(which.max(vesinv.t)) 
vesinv

#Predicting gleason 
gleason <- mean(data$gleason) 
gleason

#Predicting response of model
arguments <- data.frame(cancervol: cancervol, benpros: benpros, vesinv: vesinv, gleason: gleason) 
PSAresponse <- predict(bestfit, arguments) 
exp(PSAresponse)
