# Load the data - use your own path for canoladiesel.txt
blackcherry <- read.table("./blackcherry.txt", header = TRUE)
head(blackcherry)

model1 <- glm(Volume ~ Girth+Height, family=gaussian("identity"),
              data = blackcherry)
summary(model1)

ci95 <- confint(model1)
print(ci95)

model2 <- glm(Volume ~ Girth * Height, family=gaussian("identity"),
              data = blackcherry)
summary(model2)
ci95 <- confint(model2)
print(ci95)


anova(model5, model6, test="Chisq")

model7 <- glm(Yield ~ I(Time - 15) + I(scale(Temp)) + I(Methanol - 1) + I(Time - 15) : I(scale(Temp)), family=Gamma("log"),
              data = canola_data)
summary(model7)

# The best model according to AIC is the one with log(ab)  as predictors
beta <- coef(model5)
ci95 <- confint(model5)

# Odds Ratios (OR)
OR_beta <- exp(beta)
OR_ci95 <- exp(ci95)
print(OR_beta)
print(OR_ci95)


