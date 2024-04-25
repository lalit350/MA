library(lavaan)
data("HolzingerSwineford1939")

model = 'visual =~ x1 + x2 + x3
          textual =~ x4 + x5 + x6
          speed =~ x7 + x8 + x9
'

fit = cfa(model, data = HolzingerSwineford1939)
summary(fit)

library(semPlot)
# Plot the standardized factor loadings
semPaths(fit, whatLabels = "est", style = "lisrel", main = "cfa diagram")

