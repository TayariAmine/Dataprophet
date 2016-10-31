library(dplyr)
library(data.table)
library(ade4)
library(boot)

# Reading DATA

A <- read.csv("C:/Users/Tayari/Desktop/Tayari/Projects/DataProphet/dataset_A.csv")
B <- read.csv("C:/Users/Tayari/Desktop/Tayari/Projects/DataProphet/dataset_B.csv")


names(A)
names(B)

summary(A)

# Eliminating unsignificant variables
AT = A[,-c(1,3,6,7,8,9)]

# Eliminating NAs
ATN = na.omit(AT)

names(ATN)
str(ATN)

ATN$Sale <- as.numeric(ATN$Sale)
ATN$Gender <- as.numeric(ATN$Gender)
ATN$Area_Code <- as.numeric(ATN$Area_Code)

str(ATN)


cov(ATN)
cor(ATN)

hist(ATN$Age, main = "Histogram of Age", xlab = "Age")
hist(ATN$List_ID, main = "Histogram of List_ID", xlab = "List_ID")
hist(ATN$Area_Code, main = "Histogram of Area_Code", xlab = "Area_Code")
hist(ATN$Gender, main = "Histogram of Gender", xlab = "Gender")
hist(ATN$Call_Count, main = "Histogram of Call_Count", xlab = "Call_Count")



plot(ATN$Age,ATN$Sale,main = "Sale = f(Age)",xlab = "Age",ylab = "Sale")
plot(ATN$Area_Code,ATN$Sale,main = "Sale = f(Area_Code)",xlab = "Area_Code",ylab = "Sale")
plot(ATN$Call_Count,ATN$Sale,main = "Sale = f(Call_Count)",xlab = "Call_Count",ylab = "Sale")

plot(ATN[,c(2,3,5)])


model <- glm(data = ATN, Sale~ ., family = "binomial" (link = "logit"))
summary(model)
predicted <- predict(model, ATN) %>% inv.logit
predicted %>% hist
summary(predicted)
df1 <- data.frame(predicted)
df2 <- df1 %>% mutate(binary_prediction = ifelse (predicted > 0.08577, 1, 0))
table(ATN$Sale, df2$binary_prediction)



model2 <- glm(data = ATN, Sale~ Age, family = "binomial" (link = "logit"))
summary(model2)
predicted2 <- predict(model2, ATN) %>% inv.logit
predicted2 %>% hist
summary(predicted2)
df12 <- data.frame(predicted2)
df22 <- df12 %>% mutate(binary_prediction = ifelse (predicted2 > 0.08703, 1, 0))
table(ATN$Sale, df22$binary_prediction)



model3 <- glm(data = ATN, Sale~ Age + Gender, family = "binomial" (link = "logit"))
summary(model3)
predicted3 <- predict(model3, ATN) %>% inv.logit
predicted3 %>% hist
summary(predicted3)
df13 <- data.frame(predicted3)
df23 <- df12 %>% mutate(binary_prediction = ifelse (predicted3 > 0.08703, 1, 0))
table(ATN$Sale, df23$binary_prediction)


model4 <- glm(data = ATN, Sale~ Age + Gender + Call_Count, family = "binomial" (link = "logit"))
summary(model4)
predicted4 <- predict(model4, ATN) %>% inv.logit
predicted4 %>% hist
summary(predicted4)
df14 <- data.frame(predicted4)
df24 <- df14 %>% mutate(binary_prediction = ifelse (predicted4 > 0.08703, 1, 0))
table(ATN$Sale, df24$binary_prediction)
