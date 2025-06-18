#source("read_organise_data.R")
library(lme4)

ggplot(data = Banded_dotterel,
       aes(x = Year, y = (Number/(mean_daily_surveyors * Hectares)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
Banded_dotterel_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number),
                                      offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                                      data = Banded_dotterel)

Banded_dotterel_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number),
                                     offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                                     data = Banded_dotterel)

# Check overdispersion
summary(Banded_dotterel_random_slope)
random_df <- 15
deviance(Banded_dotterel_random_slope) / random_df #over dispersed
summary(Banded_dotterel_fixed_slope)
fixed_df <- 17
deviance(Banded_dotterel_fixed_slope) / fixed_df #over dispersed

# refit NB models
Banded_dotterel_random_slope_NB <- glmer.nb(Number ~ centeredYear + (centeredYear | section_number),
                                            offset = log(mean_daily_surveyors*Hectares),
                                            data = Banded_dotterel)
Banded_dotterel_fixed_slope_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                           offset = log(mean_daily_surveyors*Hectares),
                                           data = Banded_dotterel)
summary(Banded_dotterel_random_slope_NB)
random_NB_df <- 14
deviance(Banded_dotterel_random_slope_NB) / random_df # good
summary(Banded_dotterel_fixed_slope_NB)
fixed_NB_df <- 16
deviance(Banded_dotterel_fixed_slope_NB) / fixed_NB_df # good, use NB

AIC(Banded_dotterel_random_slope_NB, Banded_dotterel_fixed_slope_NB)
BIC(Banded_dotterel_random_slope_NB, Banded_dotterel_fixed_slope_NB)
# Use fixed slope

# Fixed effects selection ------------------------------------------------------

Banded_dotterel_max <- glmer.nb(Number ~ centeredYear + log(mean_days_since_flood) + (1 | section_number),
                                offset = log(mean_daily_surveyors*Hectares),
                                data = Banded_dotterel)
Banded_dotterel_years_only <- glmer.nb(Number ~ centeredYear + (1 | section_number),
                                       offset = log(mean_daily_surveyors*Hectares),
                                       data = Banded_dotterel)
AIC(Banded_dotterel_max, Banded_dotterel_years_only)
BIC(Banded_dotterel_max, Banded_dotterel_years_only) # close- maybe use simpler model

summary(Banded_dotterel_years_only)

Banded_dotterel_testMod <- Banded_dotterel_years_only

# Visualisation of Model -------------------------------------------------------

Banded_dotterel_pred <- expand.grid(centeredYear = seq(min(Banded_dotterel$centeredYear), max(Banded_dotterel$centeredYear)),
                        Year = seq(min(Banded_dotterel$Year), max(Banded_dotterel$Year)),
                        section_number = factor(seq(1,5))) 
Banded_dotterel_pred <- Banded_dotterel_pred %>%  mutate(Conditional = predict(Banded_dotterel_testMod, type = "response", newdata = Banded_dotterel_pred, re.form = NULL),
                                 Marginal = predict(Banded_dotterel_testMod, type = "response", newdata = Banded_dotterel_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(Banded_dotterel, aes(x = Year, y = (Number/(mean_daily_surveyors*Hectares)))) +
  ylab("Count per Surveyor*Hectare") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = Banded_dotterel_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                 size = type,
                                 color = ifelse(type == "Conditional",
                                                section_number,
                                                type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.5), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model")

