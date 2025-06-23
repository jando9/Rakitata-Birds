#source("read_organise_data.R")
library(lme4)

ggplot(data = Black_fronted_tern,
       aes(x = Year, y = (Number/(mean_daily_surveyors * Hectares)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
BFT_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                              offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                              data = Black_fronted_tern)

BFT_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                             offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                             data = Black_fronted_tern)

# Check overdispersion
summary(BFT_random_slope)
random_df <- 15
deviance(BFT_random_slope) / random_df #over dispersed
summary(BFT_fixed_slope)
fixed_df <- 17
deviance(BFT_fixed_slope) / fixed_df #over dispersed

# refit NB models
BFT_random_slope_NB <- glmer.nb(Number ~ centeredYear + (centeredYear | section_number),
                          offset = log(mean_daily_surveyors*Hectares),
                          data = Black_fronted_tern)
BFT_fixed_slope_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                offset = log(mean_daily_surveyors*Hectares),
                                data = Black_fronted_tern)
summary(BFT_random_slope_NB)
random_NB_df <- 14
deviance(BFT_random_slope_NB) / random_df # good
summary(BFT_fixed_slope_NB)
fixed_NB_df <- 16
deviance(BFT_fixed_slope_NB) / fixed_NB_df # good, use NB

AIC(BFT_random_slope_NB, BFT_fixed_slope_NB)
BIC(BFT_random_slope_NB, BFT_fixed_slope_NB)
# Use simpler model

# Fixed effects selection ------------------------------------------------------

BFT_max <- glmer.nb(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                     offset = log(mean_daily_surveyors*Hectares),
                     data = Black_fronted_tern)
BFT_years_only <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                            offset = log(mean_daily_surveyors*Hectares),
                            data = Black_fronted_tern)
AIC(BFT_max, BFT_years_only)
BIC(BFT_max, BFT_years_only) # both show year only model is better

summary(BFT_years_only)

BFT_testMod <- BFT_years_only

# Visualisation of Model -------------------------------------------------------

BFT_pred <- expand.grid(centeredYear = seq(min(Black_fronted_tern$centeredYear), max(Black_fronted_tern$centeredYear)),
                            Year = seq(min(Black_fronted_tern$Year), max(Black_fronted_tern$Year)),
                            section_number = factor(seq(1,5))) 
BFT_pred <- BFT_pred %>%  mutate(Conditional = predict(BFT_testMod, type = "response", newdata = BFT_pred, re.form = NULL),
                                         Marginal = predict(BFT_testMod, type = "response", newdata = BFT_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(Black_fronted_tern, aes(x = Year, y = (Number/(mean_daily_surveyors*Hectares)))) +
  ylab("Black Fronted Tern per Surveyor*Hectare") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = BFT_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
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

            