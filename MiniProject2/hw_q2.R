input = read.csv("C:/Users/nihar/OneDrive/Documents/Niharika_UTD/Fall_2022/Stats_for_DS/MiniProject2/motorcycle.csv")
print(input)

Accidents = input$Fatal.Motorcycle.Accidents
boxplot(Accidents, data=data, xlab="Fatal Accidents", ylab="Number of Accidents")

lowerbound = quantile(Accidents, prob=0.25) - 1.5 * IQR(Accidents)
print("Lower Bound")
print(lowerbound)
upperbound = quantile(Accidents, prob=0.75) + 1.5 * IQR(Accidents)
print("Upper Bound")
print(upperbound)

fatalcounty = input$County[which(input$Fatal.Motorcycle.Accidents<lowerbound|input$Fatal.Motorcycle.Accidents>upperbound )]
print(fatalcounty)