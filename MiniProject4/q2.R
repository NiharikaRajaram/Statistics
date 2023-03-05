voltage_data = read.csv("C:/Users/nihar/OneDrive/Documents/Niharika_UTD/Fall_2022/Stats_for_DS/MiniProject4/VOLTAGE.csv")

remote_voltage = voltage_data$voltage[voltage_data$location == 0]
local_voltage = voltage_data$voltage[voltage_data$location == 1]

print(summary(remote_voltage))
print(summary(local_voltage))

par(mfrow=c(1,2))
hist(remote_voltage, main = "Histogram of Remote Voltage")
hist(local_voltage, main = "Histogram of Local Voltage")

par(mfrow=c(1,2))
qqnorm(remote_voltage, ylab = "voltage", main = "Remote voltage distribution - QQ plot")
qqline(remote_voltage)
qqnorm(local_voltage, ylab = "voltage", main = "Local voltage distribution - QQ plot")
qqline(local_voltage)

boxplot(remote_voltage, local_voltage, ylab = "Voltage", main = "Boxplot for the Voltage Distribution", names = c('Remote', 'Local'))

mean_remote_voltage = mean(remote_voltage)
mean_local_voltage = mean(local_voltage)

var_remote_voltage = var(remote_voltage)
var_local_voltage = var(local_voltage)

standard_error = sqrt(var_remote_voltage/length(remote_voltage) + var_local_voltage/length(local_voltage))

confidence_interval = (mean_remote_voltage - mean_local_voltage) + c(-1,1)*qnorm(0.975)*standard_error
print(confidence_interval)

t.test(remote_voltage, local_voltage, alternative = "two.sided", paired=F, var.equal = F, conf.level = 0.95)
