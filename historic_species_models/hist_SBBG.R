library(lme4)
library(ggplot2)

#source("prep_older_data.R")

historic_Southern_black_backed_gull <- historic_Southern_black_backed_gull %>%
  mutate(mean_daily_surveyors = case_when(is.na(mean_daily_surveyors) ~ mean(mean_daily_surveyors, na.rm = T),
                                          .default = mean_daily_surveyors))

ggplot(data = historic_Southern_black_backed_gull,
       aes(x = Year, y = Number/mean_daily_surveyors, groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()


# Random effect selection ------------------------------------------------------
hist_SBBG_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                                   offset = log(mean_daily_surveyors), family = "poisson",
                                   data = historic_Southern_black_backed_gull)

hist_SBBG_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                                  offset = log(mean_daily_surveyors), family = "poisson",
                                  data = historic_Southern_black_backed_gull)

# Check overdispersion
summary(hist_SBBG_random_slope)
random_df <- 43
deviance(hist_SBBG_random_slope) / random_df #over

summary(hist_SBBG_fixed_slope)
fixed_df <- 45
deviance(hist_SBBG_fixed_slope) / fixed_df #over

hist_SBBG_random_slope_nb <- glmer.nb(Number ~ centeredYear + (centeredYear | section_number),
                                         offset = log(mean_daily_surveyors),
                                         data = historic_Southern_black_backed_gull)

hist_SBBG_fixed_slope_nb <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                        offset = log(mean_daily_surveyors),
                                        data = historic_Southern_black_backed_gull)

# Check overdispersion for nb
summary(hist_SBBG_random_slope_nb)
random_df <- 42
deviance(hist_SBBG_random_slope_nb) / random_df #good

summary(hist_SBBG_fixed_slope_nb)
fixed_df <- 44
deviance(hist_SBBG_fixed_slope_nb) / fixed_df #good


AIC(hist_SBBG_random_slope_nb, hist_SBBG_fixed_slope_nb)
BIC(hist_SBBG_random_slope_nb, hist_SBBG_fixed_slope_nb)
# use fixed model

# Fixed effects selection ------------------------------------------------------

historic_SBBG_max <- glmer.nb(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                                 offset = log(mean_daily_surveyors),
                                 data = historic_Southern_black_backed_gull)
historic_SBBG_years_only <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                        offset = log(mean_daily_surveyors),
                                        data = historic_Southern_black_backed_gull)
AIC(historic_SBBG_max, historic_SBBG_years_only)
BIC(historic_SBBG_max, historic_SBBG_years_only) 

summary(historic_SBBG_years_only)

historic_SBBG_testMod <- historic_SBBG_years_only

# Visualisation of Model -------------------------------------------------------

historic_SBBG_pred <- expand.grid(centeredYear = seq(min(historic_Southern_black_backed_gull$centeredYear), max(historic_Southern_black_backed_gull$centeredYear)),
                                     Year = seq(min(historic_Southern_black_backed_gull$Year), max(historic_Southern_black_backed_gull$Year)),
                                     section_number = factor(seq(1,5)))
historic_SBBG_pred <- historic_SBBG_pred %>%
  mutate(Conditional = predict(historic_SBBG_testMod, type = "response", newdata = historic_SBBG_pred, re.form = NULL),
         Marginal = predict(historic_SBBG_testMod, type = "response", newdata = historic_SBBG_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(historic_Southern_black_backed_gull, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Count per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = historic_SBBG_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                              size = type,
                                              color = ifelse(type == "Conditional",
                                                             section_number,
                                                             type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.5), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model")
