#Logistic Regression using Airplane dataset
# Read data
data <- read.csv(file.choose())

# Check structure of data
str(data)

# Convert categorical variables to factors
data$Gender <- as.factor(data$Gender)
data$Customer.Type <- as.factor(data$Customer.Type)
data$Type.of.Travel <- as.factor(data$Type.of.Travel)
data$Class <- as.factor(data$Class)
data$satisfaction <- as.factor(data$satisfaction)

# Check structure again
str(data)

# Checking missing values
colSums(is.na(data))

# Remove rows with missing values
data <- na.omit(data)

# Install & Load library
install.packages("DescTools")
library(DescTools)

# Apply Winsorization
data$X <- Winsorize(data$X, probs = c(0.05, 0.95))
data$id <- Winsorize(data$id, probs = c(0.05, 0.95))

# Fit logistic regression model
model <- glm(satisfaction ~ ., family = binomial, data = data)
summary(model)

# Choose only significant variables for building model
Fmodel <- glm(satisfaction ~ Type.of.Travel + Flight.Distance + Inflight.wifi.service +
                Departure.Arrival.time.convenient + Ease.of.Online.booking + Food.and.drink +
                Seat.comfort + Inflight.entertainment + On.board.service + Leg.room.service +
                Checkin.service + Inflight.service + Cleanliness + Departure.Delay.in.Minutes +
                Arrival.Delay.in.Minutes, family = binomial, data = data)
summary(Fmodel)

# Accuracy
dp <- predict(model, type = "response")
p <- ifelse(dp > 0.5, 1, 0)
m <- table(data$satisfaction, p)
Accuracy <- sum(diag(m)) / sum(m)
Accuracy

# Correlation plot
numeric_data <- data[, sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data)
correlation_matrix
install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method = "circle")








#Logistic Regression & Factor Analysis Airplane dataset
library(corrplot)
library(DescTools)
data = read.csv(file.choose(), header = TRUE)
dim(data)
str(data)
data$Gender = as.factor(data$Gender)
data$Customer.Type = as.factor(data$Customer.Type)
data$Type.of.Travel = as.factor(data$Type.of.Travel)
data$Class = as.factor(data$Class)
data$satisfaction = as.factor(data$satisfaction)
str(data)

new_data = subset(data , select=-c(1:8,23:25))
dim(new_data)
colSums(is.na(new_data))
View(new_data)
new_data = na.omit(new_data)
colSums(is.na(new_data))
c = cor(new_data)
corrplot(c,method="circle")

library(psych)
# Kmo value is high inf=dicates the factor anlysis is good
KMO(new_data)

cortest.bartlett(new_data)
v=eigen(c)
plot(v$values,type="b")

#model
factanal(new_data,5)
f=factanal(new_data,5,rotation = "varimax")
f

#Diagramatic representation of factors
loads<-f$loadings
fa.diagram(loads)

#Factor score
out<-factanal(x=new_data,factors = 5,scores = "regression")
scores = out$scores
dim(scores)
data_frame = cbind(data,scores)
dim(data_frame)
View(data_frame)

boxplot(data_frame)

#Logistic Regression for the Model
model=glm(data_frame$satisfaction~data_frame$Factor1+data_frame$Gender+data_frame$Customer.Type
          +data_frame$Age+data_frame$Type.of.Travel+data_frame$Class+data_frame$Departure.Delay.in.Minutes
          +data_frame$Arrival.Delay.in.Minutes+data_frame$Factor2+data_frame$Factor3+data_frame$Factor4+
            data_frame$Factor5,family = "binomial", data = data_frame)
summary(model)

Accuracydp=predict(model,type="response")
p = ifelse(Accuracydp > 1,2,1)
m = table(data_frame$satisfaction,p)
Accuracy=sum(diag(m)/sum(m))
print(Accuracy)

