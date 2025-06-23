library(lme4)
library(ggplot2)

#source("prep_older_data.R")

historic_South_Island_pied_oystercatcher <- historic_South_Island_pied_oystercatcher %>%
  mutate(mean_daily_surveyors = case_when(is.na(mean_daily_surveyors) ~ mean(mean_daily_surveyors, na.rm = T),
                                          .default = mean_daily_surveyors))

ggplot(data = historic_South_Island_pied_oystercatcher,
       aes(x = Year, y = Number/mean_daily_surveyors, groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()


# Random effect selection ------------------------------------------------------
hist_SIPO_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                                   offset = log(mean_daily_surveyors), family = "poisson",
                                   data = historic_South_Island_pied_oystercatcher)

hist_SIPO_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                                  offset = log(mean_daily_surveyors), family = "poisson",
                                  data = historic_South_Island_pied_oystercatcher)

# Check overdispersion
summary(hist_SIPO_random_slope)
random_df <- 43
deviance(hist_SIPO_random_slope) / random_df #over

summary(hist_SIPO_fixed_slope)
fixed_df <- 45
deviance(hist_SIPO_fixed_slope) / fixed_df #over

hist_SIPO_random_slope_nb <- glmer.nb(Number ~ centeredYear + (centeredYear | section_number),
                                         offset = log(mean_daily_surveyors),
                                         data = historic_South_Island_pied_oystercatcher)

hist_SIPO_fixed_slope_nb <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                        offset = log(mean_daily_surveyors),
                                        data = historic_South_Island_pied_oystercatcher)

# Check overdispersion for nb
summary(hist_SIPO_random_slope_nb)
random_df <- 42
deviance(hist_SIPO_random_slope_nb) / random_df #good but failed to converge

summary(hist_SIPO_fixed_slope_nb)
fixed_df <- 44
deviance(hist_SIPO_fixed_slope_nb) / fixed_df #good


AIC(hist_SIPO_random_slope_nb, hist_SIPO_fixed_slope_nb)
BIC(hist_SIPO_random_slope_nb, hist_SIPO_fixed_slope_nb)
# use fixed

# Fixed effects selection ------------------------------------------------------

historic_SIPO_max <- glmer.nb(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                                 offset = log(mean_daily_surveyors),
                                 data = historic_South_Island_pied_oystercatcher)
historic_SIPO_years_only <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                        offset = log(mean_daily_surveyors),
                                        data = historic_South_Island_pied_oystercatcher)
AIC(historic_SIPO_max, historic_SIPO_years_only)
BIC(historic_SIPO_max, historic_SIPO_years_only) 

summary(historic_SIPO_years_only)

historic_SIPO_testMod <- historic_SIPO_years_only

# Visualisation of Model -------------------------------------------------------

historic_SIPO_pred <- expand.grid(centeredYear = seq(min(historic_South_Island_pied_oystercatcher$centeredYear), max(historic_South_Island_pied_oystercatcher$centeredYear)),
                                     Year = seq(min(historic_South_Island_pied_oystercatcher$Year), max(historic_South_Island_pied_oystercatcher$Year)),
                                     section_number = factor(seq(1,5)))
historic_SIPO_pred <- historic_SIPO_pred %>%
  mutate(Conditional = predict(historic_SIPO_testMod, type = "response", newdata = historic_SIPO_pred, re.form = NULL),
         Marginal = predict(historic_SIPO_testMod, type = "response", newdata = historic_SIPO_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(historic_South_Island_pied_oystercatcher, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Count per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = historic_SIPO_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                              size = type,
                                              color = ifelse(type == "Conditional",
                                                             section_number,
                                                             type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.5), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model")
