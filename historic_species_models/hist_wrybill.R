library(lme4)
library(ggplot2)
library(ggeffects)

#source("prep_older_data.R")

historic_Wrybill <- historic_Wrybill %>%
  mutate(mean_daily_surveyors = case_when(is.na(mean_daily_surveyors) ~ mean(mean_daily_surveyors, na.rm = T),
                                          .default = mean_daily_surveyors))

ggplot(data = historic_Wrybill,
       aes(x = Year, y = Number/mean_daily_surveyors, groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()


# Random effect selection ------------------------------------------------------
hist_wrybill_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                              offset = log(mean_daily_surveyors), family = "poisson",
                              data = historic_Wrybill)

hist_wrybill_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                             offset = log(mean_daily_surveyors), family = "poisson",
                             data = historic_Wrybill)

# Check overdispersion
summary(hist_wrybill_random_slope)
random_df <- 43
deviance(hist_wrybill_random_slope) / random_df #over

summary(hist_wrybill_fixed_slope)
fixed_df <- 45
deviance(hist_wrybill_fixed_slope) / fixed_df #over

hist_wrybill_random_slope_nb <- glmer.nb(Number ~ centeredYear + (centeredYear | section_number),
                                   offset = log(mean_daily_surveyors),
                                   data = historic_Wrybill)

hist_wrybill_fixed_slope_nb <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                  offset = log(mean_daily_surveyors),
                                  data = historic_Wrybill)

# Check overdispersion for nb
summary(hist_wrybill_random_slope_nb)
random_df <- 42
deviance(hist_wrybill_random_slope_nb) / random_df #good but failed to converge

summary(hist_wrybill_fixed_slope_nb)
fixed_df <- 44
deviance(hist_wrybill_fixed_slope_nb) / fixed_df #good


AIC(hist_wrybill_random_slope_nb, hist_wrybill_fixed_slope_nb)
BIC(hist_wrybill_random_slope_nb, hist_wrybill_fixed_slope_nb)
# stick to simpler model

# Fixed effects selection ------------------------------------------------------

historic_wrybill_max <- glmer.nb(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                             offset = log(mean_daily_surveyors),
                             data = historic_Wrybill)
historic_wrybill_years_only <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                     offset = log(mean_daily_surveyors),
                     data = historic_Wrybill)
AIC(historic_wrybill_max, historic_wrybill_years_only)
BIC(historic_wrybill_max, historic_wrybill_years_only) 

summary(historic_wrybill_years_only)

historic_wrybill_testMod <- historic_wrybill_years_only

# Visualisation of Model -------------------------------------------------------

historic_wrybill_pred <- expand.grid(centeredYear = seq(min(historic_Wrybill$centeredYear), max(historic_Wrybill$centeredYear)),
                                     Year = seq(min(historic_Wrybill$Year), max(historic_Wrybill$Year)),
                                     section_number = factor(seq(1,5)))
historic_wrybill_pred <- historic_wrybill_pred %>%
  mutate(Conditional = predict(historic_wrybill_testMod, type = "response", newdata = historic_wrybill_pred, re.form = NULL),
         Marginal = predict(historic_wrybill_testMod, type = "response", newdata = historic_wrybill_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(historic_Wrybill, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Count per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = historic_wrybill_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                              size = type,
                                              color = ifelse(type == "Conditional",
                                                             section_number,
                                                             type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model")

# ggeffects
hist_wry_resp <- predict_response(historic_wrybill_testMod, terms = "centeredYear")

ggplot(hist_wry_resp, aes(x = (x + 1986), y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme(plot.title = element_blank()) +
  labs(x = "Year", y = "Predicted Wrybill per Observer")
  