#source("read_organise_data.R")
library(lme4)

ggplot(data = Black_billed_gull,
       aes(x = Year, y = (Number/(mean_daily_surveyors * Hectares)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
Black_billed_gull_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                                      offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                                      data = Black_billed_gull)

Black_billed_gull_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                                     offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                                     data = Black_billed_gull)

# Check overdispersion
summary(Black_billed_gull_random_slope)
random_df <- 15
deviance(Black_billed_gull_random_slope) / random_df #over dispersed
summary(Black_billed_gull_fixed_slope)
fixed_df <- 17
deviance(Black_billed_gull_fixed_slope) / fixed_df #over dispersed

# refit NB models
Black_billed_gull_random_slope_NB <- glmer.nb(Number ~ centeredYear + (centeredYear | section_number),
                                            offset = log(mean_daily_surveyors*Hectares),
                                            data = Black_billed_gull)
Black_billed_gull_fixed_slope_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                           offset = log(mean_daily_surveyors*Hectares),
                                           data = Black_billed_gull)
summary(Black_billed_gull_random_slope_NB)
random_NB_df <- 14
deviance(Black_billed_gull_random_slope_NB) / random_df # good
summary(Black_billed_gull_fixed_slope_NB)
fixed_NB_df <- 16
deviance(Black_billed_gull_fixed_slope_NB) / fixed_NB_df # good, use NB

AIC(Black_billed_gull_random_slope_NB, Black_billed_gull_fixed_slope_NB)
BIC(Black_billed_gull_random_slope_NB, Black_billed_gull_fixed_slope_NB)
# Use fixed slope

# Fixed effects selection ------------------------------------------------------

Black_billed_gull_max <- glmer.nb(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                                offset = log(mean_daily_surveyors*Hectares),
                                data = Black_billed_gull)
Black_billed_gull_years_only <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                       offset = log(mean_daily_surveyors*Hectares),
                                       data = Black_billed_gull)
AIC(Black_billed_gull_max, Black_billed_gull_years_only)
BIC(Black_billed_gull_max, Black_billed_gull_years_only) # use simpler model

summary(Black_billed_gull_years_only)

Black_billed_gull_testMod <- Black_billed_gull_years_only

# Visualisation of Model -------------------------------------------------------

Black_billed_gull_pred <- expand.grid(centeredYear = seq(min(Black_billed_gull$centeredYear), max(Black_billed_gull$centeredYear)),
                                    Year = seq(min(Black_billed_gull$Year), max(Black_billed_gull$Year)),
                                    section_number = factor(seq(1,5))) 
Black_billed_gull_pred <- Black_billed_gull_pred %>%  mutate(Conditional = predict(Black_billed_gull_testMod, type = "response", newdata = Black_billed_gull_pred, re.form = NULL),
                                                         Marginal = predict(Black_billed_gull_testMod, type = "response", newdata = Black_billed_gull_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(Black_billed_gull, aes(x = Year, y = (Number/(mean_daily_surveyors*Hectares)))) +
  ylab("Count per Surveyor*Hectare") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = Black_billed_gull_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                             size = type,
                                             color = ifelse(type == "Conditional",
                                                            section_number,
                                                            type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.5), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model")

