vapor_data = read.csv('C:/Users/nihar/OneDrive/Documents/Niharika_UTD/Fall_2022/Stats_for_DS/MiniProject4/VAPOR.csv')

par(mfrow=c(1,2))
qqnorm(vapor_data$theoretical, main = 'Theoretical data')
qqline(vapor_data$theoretical)
qqnorm(vapor_data$experimental, main = 'Experimental data')
qqline(vapor_data$experimental)

boxplot(vapor_data$theoretical, vapor_data$experimental, names=c('Theoretical data', 'Experimental Data'), main = 'Boxplot')

print(summary(vapor_data$theoretical))

print(summary(vapor_data$experimental))


vapor_diff = vapor_data$theoretical - vapor_data$experimental
mean_vapor_diff = mean(vapor_diff)
print(mean_vapor_diff)

sd_vapor_diff = sd(vapor_diff)
print(sd_vapor_diff)

CI = mean_vapor_diff + c(-1,1)*qt(0.975,15)*sd_vapor_diff/sqrt(nrow(vapor_data))
print(CI)

t.test(vapor_data$theoretical, vapor_data$experimental, alternative ='two.sided', paired=TRUE, var.equal = FALSE, conf.level = 0.95)
