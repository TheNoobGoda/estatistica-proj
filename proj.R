install.packages("psych")
library(psych)
install.packages("AER")
library(AER)
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("nortest")
library(nortest)

#load_data
data("CASchools", package = "AER")
df <- subset(CASchools,select= -school)
df <- subset(df,select= -math)

#description of the data
numerical_vars <- df[sapply(df, is.numeric)]

#dependent varaible
summary(df)
describe(df)
mean(df$read)
sd(df$read)
min(df$read)
max(df$read)
summary(df$read)

#independent varaibles
means <- colMeans(numerical_vars)
std_dev <- apply(numerical_vars, 2, sd)
minimum <- apply(numerical_vars, 2, min)
maximum <- apply(numerical_vars, 2, max)

print("Means:")
print(means)
print("Standard Deviation:")
print(std_dev)
print("Minimum:")
print(minimum)
print("Maximum:")
print(maximum)

#barplots
par(mfrow = c(1, 1))
for (col in names(numerical_vars)) {
  if (is.factor(numerical_vars[[col]])) {
    barplot(table(numerical_vars[[col]]), main = col, xlab = col)
  }
}

#histograms
par(mfrow = c(1, 1))  # Adjust the layout as needed
for (col in names(numerical_vars)) {
  if (is.numeric(numerical_vars[[col]])) {
    hist(numerical_vars[[col]], main = col, xlab = col)
  }
}

#boxplots
par(mfrow = c(1, 1))  # Adjust the layout as needed
for (col in names(numerical_vars)) {
  if (is.numeric(numerical_vars[[col]])) {
    boxplot(numerical_vars[[col]], main = col, xlab = col)
  }
}

#correlation
cor_matrix <- cor(numerical_vars)
ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(cor_matrix[,])

#scatterplots
pairs(numerical_vars)

#pca analysis
pca_result <- prcomp(numerical_vars, scale. = TRUE)
summary(pca_result)

#normality
for (col in names(numerical_vars)) {
  print(paste("Shapiro-Wilk test for", col, ":"))
  print(shapiro.test(numerical_vars[[col]]))
}
for (col in names(numerical_vars)) {
  print(paste("Anderson-Darling test for", col, ":"))
  print(ad.test(numerical_vars[[col]]))
}


#chossing the model
# Fit models with different variable combinations
model1 <- lm(read ~ lunch, data = df)
model2 <- lm(read ~ income, data = df)
model3 <- lm(read ~ english, data = df)
model4 <- lm(read ~ calworks, data = df)
model5 <- lm(read ~ lunch+income, data = df)
model6 <- lm(read ~ lunch+english, data = df)
model7 <- lm(read ~ lunch+calworks, data = df)
model8 <- lm(read ~ students+teachers+calworks+lunch+computer+expenditure+income+english+read, data = df)
# ...

# Compare models using AIC
AIC_values <- AIC(model1, model2,model3,model4,model5,model6,model7,model8)
print(AIC_values)

summary(model8)

#model fitting and preditcions
model <- lm(read ~ ., data = df)
AIC(model)
summary(model)

predictions <- predict(model, newdata = df)
plot(predictions, df$read, xlab = "Predicted Read", ylab = "Observed Read")
