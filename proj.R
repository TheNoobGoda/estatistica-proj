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

#description of the data
numerical_vars <- CASchools[sapply(CASchools, is.numeric)]

summary(CASchools)
describe(CASchools)

#barplots
par(mfrow = c(1, 2))
for (col in names(CASchools)) {
  if (is.factor(CASchools[[col]])) {
    barplot(table(CASchools[[col]]), main = col, xlab = col)
  }
}

#histograms
par(mfrow = c(2, 2))  # Adjust the layout as needed
for (col in names(CASchools)) {
  if (is.numeric(CASchools[[col]])) {
    hist(CASchools[[col]], main = col, xlab = col)
  }
}

#boxplots
par(mfrow = c(3, 2))  # Adjust the layout as needed
for (col in names(CASchools)) {
  if (is.numeric(CASchools[[col]])) {
    boxplot(CASchools[[col]], main = col, xlab = col)
  }
}

#correlation
cor_matrix <- cor(numerical_vars)
ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(cor_matrix[, "read"])

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


#ftting the model
# Fit a linear model using stepwise regression
# Forward selection
forward_model <- step(lm(read ~ 1, data = CASchools),
                      scope = list(lower = ~1, upper = ~.))

# Fit models with different variable combinations
model1 <- lm(read ~ expenditure, data = CASchools)
model2 <- lm(read ~ income, data = CASchools)
# ...

# Compare models using AIC
AIC_values <- AIC(model1, model2)
print(AIC_values)

#model fitting and preditcions
model <- lm(read ~ ., data = CASchools)
summary(model)

predictions <- predict(model, newdata = CASchools)
plot(predictions, CASchools$read, xlab = "Predicted Read", ylab = "Observed Read")
