# Load the data - use your own path for canoladiesel.txt
insurance <- read.table("./insurance_claims.txt", header = TRUE)
head(insurance)

claims <- insurance$claims
age <- insurance$age
income <- insurance$income
urban <- insurance$urban

library(MASS)

poisreg.fit <- glm(claims ~ age + income + urban, 
                family=poisson(link="log"),
                data=insurance)
pois.res <- summary(poisreg.fit)
print(coef(pois.res))
print(AIC(pois.fit))

negbin.fit <- glm.nb(claims ~ age + income + urban,
                  data=insurance)
print(summary(negbin.fit))
