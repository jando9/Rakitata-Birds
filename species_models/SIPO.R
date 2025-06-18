#source("read_organise_data.R")
library(lme4)


ggplot(data = South_Island_pied_oystercatcher,
       aes(x = Year, y = (Number/(mean_daily_surveyors * Hectares)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
SIPO_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                                 offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                                 data = South_Island_pied_oystercatcher)

SIPO_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                                offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                                data = South_Island_pied_oystercatcher)

# Check overdispersion
summary(SIPO_random_slope)
random_df <- 15
deviance(SIPO_random_slope) / random_df #over dispersed
summary(SIPO_fixed_slope)
fixed_df <- 17
deviance(SIPO_fixed_slope) / fixed_df #over dispersed

# refit NB models
SIPO_random_slope_NB <- glmer.nb(Number ~ centeredYear + (centeredYear | section_number),
                                       offset = log(mean_daily_surveyors*Hectares),
                                       data = South_Island_pied_oystercatcher)
SIPO_fixed_slope_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                      offset = log(mean_daily_surveyors*Hectares),
                                      data = South_Island_pied_oystercatcher)
summary(SIPO_random_slope_NB)
random_NB_df <- 14
deviance(SIPO_random_slope_NB) / random_df # good
summary(SIPO_fixed_slope_NB)
fixed_NB_df <- 16
deviance(SIPO_fixed_slope_NB) / fixed_NB_df # good

AIC(SIPO_random_slope_NB, SIPO_fixed_slope_NB)
BIC(SIPO_random_slope_NB, SIPO_fixed_slope_NB)
# use fixed slope

# Fixed effects selection ------------------------------------------------------

SIPO_max <- glmer.nb(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                           offset = log(mean_daily_surveyors*Hectares),
                           data = South_Island_pied_oystercatcher)
SIPO_years_only <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                  offset = log(mean_daily_surveyors*Hectares),
                                  data = South_Island_pied_oystercatcher)
AIC(SIPO_max, SIPO_years_only)
BIC(SIPO_max, SIPO_years_only) # use years only

summary(SIPO_years_only)

SIPO_testMod <- SIPO_years_only

# Visualisation of Model -------------------------------------------------------

SIPO_pred <- expand.grid(centeredYear = seq(min(South_Island_pied_oystercatcher$centeredYear),
                                            max(South_Island_pied_oystercatcher$centeredYear)),
                               Year = seq(min(South_Island_pied_oystercatcher$Year),
                                          max(South_Island_pied_oystercatcher$Year)),
                               section_number = factor(seq(1,5))) 
SIPO_pred <- SIPO_pred %>%  mutate(Conditional = predict(SIPO_testMod, type = "response", newdata = SIPO_pred, re.form = NULL),
                                               Marginal = predict(SIPO_testMod, type = "response", newdata = SIPO_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(South_Island_pied_oystercatcher, aes(x = Year, y = (Number/(mean_daily_surveyors*Hectares)))) +
  ylab("Count per Surveyor*Hectare") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = SIPO_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                        size = type,
                                        color = ifelse(type == "Conditional",
                                                       section_number,
                                                       type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.5), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model")

