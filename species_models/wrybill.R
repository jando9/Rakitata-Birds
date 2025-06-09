#source("read_organise_data.R")
library(lme4)

# Random effect selection ------------------------------------------------------
Wrybill_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                              offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                              data = Wrybill)

Wrybill_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                             offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                             data = Wrybill)

# Check overdispersion
summary(Wrybill_random_slope)
random_df <- 15
deviance(Wrybill_random_slope) / random_df #good

summary(Wrybill_fixed_slope)
fixed_df <- 17
deviance(Wrybill_fixed_slope) / resid_df #ok

AIC(Wrybill_random_slope, Wrybill_fixed_slope)
BIC(Wrybill_random_slope, Wrybill_fixed_slope)
# pretty close, maybe stick to simpler model?

# Fixed effects selection ------------------------------------------------------


