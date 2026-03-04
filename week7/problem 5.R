library(survival)
# For pretty plots
library(survminer)
df <- read.csv("./survival_exercise.csv", header = TRUE, sep=",")
head(df)


# Fit KM curves
fit_km <- survfit(Surv(time, status) ~ treatment, data = df)
# Plot
ggsurvplot(
  fit_km,
  data = df,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
)

# Cox model
fit_cox <- coxph(Surv(time, status) ~ treatment + age, data = df)
summary(fit_cox)
# Test proportional hazards assumption
cox.zph(fit_cox)
