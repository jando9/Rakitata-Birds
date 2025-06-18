#source("read_organise_data.R")
library(lme4)


ggplot(data = Spur_winged_plover,
       aes(x = Year, y = (Number/(mean_daily_surveyors * Hectares)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
Spur_winged_plover_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                           offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                           data = Spur_winged_plover)

Spur_winged_plover_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                          offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                          data = Spur_winged_plover)

# Check overdispersion
summary(Spur_winged_plover_random_slope)
random_df <- 15
deviance(Spur_winged_plover_random_slope) / random_df #over dispersed
summary(Spur_winged_plover_fixed_slope)
fixed_df <- 17
deviance(Spur_winged_plover_fixed_slope) / fixed_df #over dispersed

# refit NB models
Spur_winged_plover_random_slope_NB <- glmer.nb(Number ~ centeredYear + (centeredYear | section_number),
                                 offset = log(mean_daily_surveyors*Hectares),
                                 data = Spur_winged_plover)
Spur_winged_plover_fixed_slope_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                offset = log(mean_daily_surveyors*Hectares),
                                data = Spur_winged_plover)
summary(Spur_winged_plover_random_slope_NB)
random_NB_df <- 14
deviance(Spur_winged_plover_random_slope_NB) / random_df # okay
summary(Spur_winged_plover_fixed_slope_NB)
fixed_NB_df <- 16
deviance(Spur_winged_plover_fixed_slope_NB) / fixed_NB_df # okay

AIC(Spur_winged_plover_random_slope_NB, Spur_winged_plover_fixed_slope_NB)
BIC(Spur_winged_plover_random_slope_NB, Spur_winged_plover_fixed_slope_NB)
# use fixed slope

# Fixed effects selection ------------------------------------------------------

Spur_winged_plover_max <- glmer.nb(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                     offset = log(mean_daily_surveyors*Hectares),
                     data = Spur_winged_plover)
Spur_winged_plover_years_only <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                            offset = log(mean_daily_surveyors*Hectares),
                            data = Spur_winged_plover)
AIC(Spur_winged_plover_max, Spur_winged_plover_years_only)
BIC(Spur_winged_plover_max, Spur_winged_plover_years_only) # use years only

summary(Spur_winged_plover_years_only)

Spur_winged_plover_testMod <- Spur_winged_plover_years_only

# Visualisation of Model -------------------------------------------------------

Spur_winged_plover_pred <- expand.grid(centeredYear = seq(min(Spur_winged_plover$centeredYear),
                                            max(Spur_winged_plover$centeredYear)),
                         Year = seq(min(Spur_winged_plover$Year),
                                    max(Spur_winged_plover$Year)),
                         section_number = factor(seq(1,5))) 
Spur_winged_plover_pred <- Spur_winged_plover_pred %>%  mutate(Conditional = predict(Spur_winged_plover_testMod, type = "response", newdata = Spur_winged_plover_pred, re.form = NULL),
                                   Marginal = predict(Spur_winged_plover_testMod, type = "response", newdata = Spur_winged_plover_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(Spur_winged_plover, aes(x = Year, y = (Number/(mean_daily_surveyors*Hectares)))) +
  ylab("Count per Surveyor*Hectare") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = Spur_winged_plover_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                  size = type,
                                  color = ifelse(type == "Conditional",
                                                 section_number,
                                                 type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.5), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model")

