data<-read.csv("C:/Users/nihar/OneDrive/Documents/Niharika_UTD/Fall_2022/Stats_for_DS/MiniProject5/bodytemp-heartrate.csv")
temp_male=c(data$body_temperature[which(data$gender==1)])
temp_female=c(data$body_temperature[which(data$gender==2)])

heartrate_male=c(data$heart_rate[which(data$gender==1)])
heartrate_female=c(data$heart_rate[which(data$gender==2)])

boxplot(temp_male,temp_female,names=c('Male','Female'),main='Body Temperatures(male vs
female)',ylab='Temperatures')

par(mfrow=c(1,2))
qqnorm(temp_male,main='Body Temp-Male')
qqline(temp_male)
qqnorm(temp_female,main='Body Temp-Female')
qqline(temp_female)
#summary of body temperatures-Male and Female
summary(temp_male)
summary(temp_female)
#Confidence Interval
t.test(temp_male,temp_female,alternative='two.sided',var.equal = F)
