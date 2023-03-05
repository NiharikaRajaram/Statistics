gpa_data= read.csv("C:/Users/nihar/OneDrive/Documents/Niharika_UTD/Fall_2022/Stats_for_DS/MiniProject4/gpa.csv")
gpa = gpa_data$gpa
act = gpa_data$act

plot(gpa, act, xlab = 'GPA', ylab = 'ACT', main = 'Scatterplot between GPA and ACT data columns')
abline(lm(act~gpa))

cor(gpa, act)

bootstrap_covar_npar = function(data, index) {
  gpa_value = data$gpa[index]
  act_value = data$act[index]
  npar <- cor(gpa_value, act_value)
  return (npar)
}

bootstrap_output = boot(gpa_data, bootstrap_covar_npar, R = 999, sim = 'ordinary', stype = 'i')
print(bootstrap_output)

mean(bootstrap_output$t)

boot.ci(bootstrap_output)

sort(bootstrap_output$t)[c(25, 975)]