#source("read_organise_data.R")
library(lme4)
library(ggplot2)

ggplot(data = Wrybill,
       aes(x = Year, y = (Number/(mean_daily_surveyors * Hectares)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

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
deviance(Wrybill_fixed_slope) / fixed_df #ok

AIC(Wrybill_random_slope, Wrybill_fixed_slope)
BIC(Wrybill_random_slope, Wrybill_fixed_slope)
# pretty close, maybe stick to simpler model?

# Fixed effects selection ------------------------------------------------------

Wrybill_max <- glmer(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                             offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                             data = Wrybill)
Wrybill_years_only <- glmer(Number ~ centeredYear + (1 | section_number),
                     offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                     data = Wrybill)
AIC(Wrybill_max, Wrybill_years_only)
BIC(Wrybill_max, Wrybill_years_only) # both show year only model is better

summary(Wrybill_years_only)

wrybill_testMod <- Wrybill_years_only

# Visualisation of Model -------------------------------------------------------

wrybill_pred <- expand.grid(centeredYear = seq(min(Wrybill$centeredYear), max(Wrybill$centeredYear)),
                                     Year = seq(min(Wrybill$Year), max(Wrybill$Year)),
                                     section_number = factor(seq(1,5))) 
wrybill_pred <- wrybill_pred %>%  mutate(Conditional = predict(wrybill_testMod, type = "response", newdata = wrybill_pred, re.form = NULL),
         Marginal = predict(wrybill_testMod, type = "response", newdata = wrybill_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(Wrybill, aes(x = Year, y = (Number/(mean_daily_surveyors*Hectares)))) +
  ylab("Wrybill per Surveyor*Hectare") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = wrybill_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                              size = type,
                                              color = ifelse(type == "Conditional",
                                                             section_number,
                                                             type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)))
