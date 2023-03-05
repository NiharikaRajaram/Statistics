data<-read.csv("C:/Users/nihar/OneDrive/Documents/Niharika_UTD/Fall_2022/Stats_for_DS/MiniProject5/bodytemp-heartrate.csv")

boxplot(heartrate_male,heartrate_female,names=c('Male','Female'),main='Heart Rate(male
vs female)',ylab='Heart Rate')

par(mfrow=c(1,2))
qqnorm(heartrate_male,main='heart rate-Male')
qqline(heartrate_male)
qqnorm(heartrate_female,main='heart rate-Female')
qqline(heartrate_female)

summary(heartrate_male)
summary(heartrate_female)

t.test(heartrate_male,heartrate_female,alternative='two.sided',var.equal = F)

# 1c
#correlation
par(mfrow=c(1,2))
plot(heartrate_male, temp_male,pch=1,main='Scatter plot-Male')
abline(lm(temp_male~heartrate_male))
plot(heartrate_female, temp_female,pch=1,main='Scatter plot-Female')
abline(lm(temp_female~heartrate_female))
cor(temp_male,heartrate_male)
cor(temp_female,heartrate_female)