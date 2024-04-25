library(lavaan)
data(HolzingerSwineford1939)
# specify the model
HS.model <- ' visual  =~ x1 + x2 + x3      
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

# fit the model
fit <- cfa(HS.model, data = HolzingerSwineford1939)

# display summary output
summary(fit, fit.measures=TRUE)

library(semPlot)
semPaths(fit, what = "std", layout = "tree2")
