# Reading the file into an R dataframe
lc <- read.csv("LungCancer.csv")

install.packages("survival")
install.packages("survminer")
library(survminer)
library(survival)
library(dplyr)

# Using dplyr to filter rows based on a column value
#treat <- lc %>% 
  #filter(Treatment == '1')

#test.treat <- lc %>%
  #filter(Treatment == "2")

km1 <- survfit(Surv(lc$Survival_in_days, lc$Status) ~ lc$Treatment)      
summary(km1)
plot(km1, xlab="Survival_in_days", ylab="Survival Probability", col=c("#F8766D","#00BFC4"), lty=1:2, main="Standard treatment (Red) vs. Test (Blue)")

#Survival probability given specific time
time_value <- 183
summary_km <- summary(km1, times = time_value)
specific_time_survival <- summary_km$surv[summary_km$time == time_value]
print(specific_time_survival)

#Mean survival time
mean_survival_times <- tapply(lc$Survival_in_days, lc$Treatment, function(x) mean(x, na.rm = TRUE))
print(mean_survival_times)

attach(lc)
# Cox proportional hazard model - coefficients and hazard rates semi-parametric
cox <- coxph(Surv(lc$Survival_in_days, lc$Status) ~ Prior_chemo + Age + Treatment, method="breslow")
summary(cox)

cox2 <- coxph(Surv(lc$Survival_in_days, lc$Status) ~ Months_from_diagnosis + Karnofsky_score + Cell_type, method="breslow")
summary(cox2)

den <- density(Age)                           # Density function
hist(Age, breaks=20, prob=T, main="Histogram of Months")
lines(den, col="red")

cox3 <- coxph(Surv(lc$Survival_in_days, lc$Status) ~ log(Months_from_diagnosis) + Karnofsky_score + Cell_type + Prior_chemo + Age + Treatment, method="breslow")
summary(cox3)



#Parametric survival models
exp <- survreg(Surv(lc$Survival_in_days, lc$Status) ~ Age + Months_from_diagnosis + Treatment, dist="exponential")
summary(exp)

weibull <- survreg(Surv(lc$Survival_in_days, lc$Status) ~ Age + Months_from_diagnosis + Treatment, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(lc$Survival_in_days, lc$Status) ~ Age + Months_from_diagnosis + Treatment, dist="loglogistic")
summary(loglogistic)
