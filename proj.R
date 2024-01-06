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
library(dplyr)
install.packages("lmtest")
library(lmtest)
install.packages('glmnet')
library(glmnet)
install.packages('gridExtra')
library(gridExtra)

#load_data
data("CASchools", package = "AER")
df <- subset(CASchools,select= -school)
df <- subset(df,select= -math)
df <- subset(df,select= -district)



#description of the data
numerical_vars <- df[sapply(df, is.numeric)]

#dependent variable
summary(df)
describe(df)

mean(df$read)
sd(df$read)
min(df$read)
max(df$read)
summary(df$read)

#independent variables
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
par(mfrow = c(2, 4))  # Adjust the layout as needed
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

plot1 <- ggplot(df, aes(x = county, y = read)) +
  geom_boxplot() +
  stat_summary(fun = "median", geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Boxplot with Median Values by Category", x = "county", y = "read")
plot2 <- ggplot(df, aes(x = grades, y = read)) +
  geom_boxplot() +
  stat_summary(fun = "median", geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Boxplot with Median Values by Category", x = "grades", y = "read")

grid.arrange(plot1, plot2, nrow = 2)

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


#normality remove?
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
model1 <- lm(read ~ lunch+income+english, data = df)
model2 <- lm(read ~ lunch+income+english+calworks, data = df)
model3 <- lm(read ~ lunch+income+english+expenditure, data = df)
model4 <- lm(read ~ lunch+income+english+expenditure+students, data = df)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
anova(model1,model2)
anova(model1,model3)
anova(model3,model4)

model5 <- lm(read ~ lunch+income+english+expenditure+county, data = df)
model6 <- lm(read ~ lunch+income+english+expenditure+county+grades, data = df)

summary(model5)
anova(model3,model5)
anova(model5,model6)

final_model = model5

#varaibles X1 and X2
#X1 = lunch X2 = county

modelx1 <- lm(read ~lunch, data = df)
modelx2 <- lm(read ~ county, data = df)

coef(final_model)['lunch']
coef(modelx1)['lunch']

coef(final_model)['countyButte']
coef(modelx2)['countyButte']
coef(final_model)['countyCalaveras']
coef(modelx2)['countyCalaveras']
coef(final_model)['countyContra Costa']
coef(modelx2)['countyContra Costa']
coef(final_model)['countyEl Dorado']
coef(modelx2)['countyEl Dorado']

#x2 3 = calaveras x2 2 = butte

coefButte <- coef(final_model)['countyButte']
coefCalaveras <- coef(final_model)['countyCalaveras']
coefButte
coefCalaveras

coefCalaveras -coefButte

confint(final_model, level = 0.95)["countyCalaveras",] - confint(final_model, level = 0.95)["countyButte",]
confint(final_model, level = 0.90)["countyCalaveras",] - confint(final_model, level = 0.90)["countyButte",]

#interaction

model_interaction <- lm(read ~ lunch * county, data = df)
summary(model_interaction)

model_no_interaction <- lm(read ~ lunch + county, data = df)

anova(model_no_interaction,model_interaction)



#logistic regression part
#change to binary value
median(df$read)
df <- df %>%
  mutate(read_cat = ifelse(read >= median(read), 1, 0))

#data analysis

#model
model1 <- glm(read_cat ~ lunch+income+english, data = df, family = "binomial")
model2 <- glm(read_cat ~ lunch+income+english+calworks, data = df, family = "binomial")
model3 <- glm(read_cat ~ lunch+income+english+expenditure, data = df, family = "binomial")
model4 <- glm(read_cat ~ lunch+income+english+students, data = df, family = "binomial")

summary(model1)
summary(model2)
summary(model3)
summary(model4)
anova(model1,model2, test = "Chisq")
anova(model1,model3, test = "Chisq")
anova(model1,model4, test = "Chisq")

model5 <- glm(read_cat ~ lunch+income+english+county, data = df)
model6 <- glm(read_cat ~ lunch+income+english+county+grades, data = df)

summary(model5)
anova(model1,model5, test = "Chisq")
anova(model5,model6, test = "Chisq")

final_model = model5

predictions <- predict(final_model, newdata = df, type = "response")
predicted_labels <- ifelse(predictions > 0.5, 1, 0)
confusion_matrix <- table(predicted_labels, df$read_cat)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
confusion_matrix


#analyse variables
modelx1 <- glm(read_cat ~lunch, data = df, family = "binomial")
modelx2 <- glm(read_cat ~ county, data = df, family = "binomial")

coef(final_model)['lunch']
coef(modelx1)['lunch']

exp(coef(final_model)['lunch'])
exp(coef(modelx1)['lunch'])


coef(final_model)['countyButte']
coef(modelx2)['countyButte']

exp(coef(final_model)['countyButte'])
exp(coef(modelx2)['countyButte'])


coef(final_model)['countyCalaveras']
coef(modelx2)['countyCalaveras']

exp(coef(final_model)['countyCalaveras'])
exp(coef(modelx2)['countyCalaveras'])


coef(final_model)['countyContra Costa']
coef(modelx2)['countyContra Costa']

exp(coef(final_model)['countyContra Costa'])
exp(coef(modelx2)['countyContra Costa'])


coef(final_model)['countyEl Dorado']
coef(modelx2)['countyEl Dorado']

exp(coef(final_model)['countyEl Dorado'])
exp(coef(modelx2)['countyEl Dorado'])

#x2 3 = calaveras x2 2 = butte

coefButte <- coef(final_model)['countyButte']
coefCalaveras <- coef(final_model)['countyCalaveras']
coefButte
coefCalaveras

coefCalaveras -coefButte
or_ceof_diff <- exp(coefCalaveras -coefButte)

confint(final_model, level = 0.95)["countyCalaveras",] - confint(final_model, level = 0.95)["countyButte",]
confint(final_model, level = 0.90)["countyCalaveras",] - confint(final_model, level = 0.90)["countyButte",]


#interaction
df$county_numeric <- as.numeric(df$county)
model_interaction <- glm(read_cat ~ lunch + county_numeric + lunch * county_numeric, data = df, family = "binomial",maxit = 1000)
summary(model_interaction)

