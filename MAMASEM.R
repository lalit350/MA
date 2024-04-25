#SEM using Political Democracy
library(lavaan)
?PoliticalDemocracy
data("PoliticalDemocracy")

model = ' #Measurement model
          ind60 =~ x1 + x2 + x3
          dem60 =~ y1 + y2 + y3 + y4
          dem65 =~ y5 + y6 + y7 + y8
          #Regression
          dem60 ~ ind60
          dem65 ~ ind60 + dem60
          #residual correlation
          y1 ~~ y5
          y2 ~~ y4+y6
          y3 ~~ y7
          y4 ~~ y8
          y6 ~~ y8
'

fit = sem(model, data = PoliticalDemocracy)
summary(fit)

library(semPlot)
# Plot the standardized factor loadings
semPaths(fit, "std", layout = "tree2")
semPaths(fit, whatLabels = "est", style = "lisrel", main = "SEM Diagram")











#SEM using Holzinger Swine Ford
library(lavaan)
data("HolzingerSwineford1939")

model = 'visual =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed =~ x7 + x8 + x9
 
          x1 ~~ x2 + x3
          x4 ~~ x6
          x5 ~~ x6
          x7 ~~ x8
          x8 ~~ x9
'

fit = sem(model, data = HolzingerSwineford1939)
summary(fit)

library(semPlot)
# Plot the standardized factor loadings
semPaths(fit, "std", layout = "tree2")
semPaths(fit, whatLabels = "est", style = "lisrel", main = "SEM Diagram")


