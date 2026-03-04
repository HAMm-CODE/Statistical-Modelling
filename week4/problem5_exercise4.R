# Load the data
baby_food <- read.table("./babyfood.txt", header = TRUE)
head(baby_food)

model1 <- glm(mal ~ ab, family = binomial(link = "logit"), data = baby_food)
summary(model1)
