library(arules)
#Market basket using Groceries dataset
library(arulesViz)

groceries = read.transactions(choose.files())
str(groceries)

itemFrequencyPlot(groceries, topN = 10, type = "relative")
rules = apriori(groceries, parameter = list(supp = 0.001, conf = 0.8))

options(digits = 10)
inspect(rules[1:6])

rules = sort(rules, by="confidence", decreasing = TRUE)
rules


plot(rules, method="graph")