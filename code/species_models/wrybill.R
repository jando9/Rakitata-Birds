#source("code/read_organise_data.R")
library(lme4)
library(ggplot2)
library(ggeffects)

ggplot(data = Wrybill,
       aes(x = Year, y = (Number/(mean_daily_surveyors)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
Wrybill_random_slope <- glmer(Number ~ scaledYear + (scaledYear | section_number),
                              offset = log(mean_daily_surveyors), family = "poisson",
                              data = Wrybill)

Wrybill_fixed_slope <- glmer(Number ~ scaledYear + (1 | section_number),
                             offset = log(mean_daily_surveyors), family = "poisson",
                             data = Wrybill)

# Check overdispersion
summary(Wrybill_random_slope)
random_df <- 15
deviance(Wrybill_random_slope) / random_df #good

summary(Wrybill_fixed_slope)
fixed_df <- 17
deviance(Wrybill_fixed_slope) / fixed_df #ok

AIC(Wrybill_random_slope, Wrybill_fixed_slope)
AICc(Wrybill_random_slope, Wrybill_fixed_slope)
BIC(Wrybill_random_slope, Wrybill_fixed_slope)
# pretty close, maybe stick to simpler model?

# Fixed effects selection ------------------------------------------------------

Wrybill_year_flood_int <- glmer(Number ~ scaledYear * scaledFlood + (1 | section_number),
                             offset = log(mean_daily_surveyors), family = "poisson",
                             data = Wrybill)
Wrybill_year_flow_int <- glmer(Number ~ scaledYear * scaledMeanFlow + (1 | section_number),
                     offset = log(mean_daily_surveyors), family = "poisson",
                     data = Wrybill)
Wrybill_year_flood <- glmer(Number ~ scaledYear + scaledFlood + (1 | section_number),
                            offset = log(mean_daily_surveyors), family = "poisson",
                            data = Wrybill)
Wrybill_year_flow <- glmer(Number ~ scaledYear + scaledMeanFlow + (1 | section_number),
                            offset = log(mean_daily_surveyors), family = "poisson",
                            data = Wrybill)
Wrybill_years_only <- glmer(Number ~ scaledYear + (1 | section_number),
                     offset = log(mean_daily_surveyors), family = "poisson",
                     data = Wrybill)
Wrybill_no_years <- glmer(Number ~ (1 | section_number),
                            offset = log(mean_daily_surveyors), family = "poisson",
                            data = Wrybill)

AIC(Wrybill_year_flood_int, Wrybill_year_flow_int, Wrybill_year_flood, Wrybill_year_flow, Wrybill_years_only)
AICc(Wrybill_year_flood_int, Wrybill_year_flow_int, Wrybill_year_flood, Wrybill_year_flow, Wrybill_years_only)
BIC(Wrybill_year_flood_int, Wrybill_year_flow_int, Wrybill_year_flood, Wrybill_year_flow, Wrybill_years_only) 

wrybill_lrt <- lrtest(Wrybill_year_flood_int, Wrybill_year_flow_int, Wrybill_year_flood, Wrybill_year_flow, Wrybill_years_only)

r.squaredGLMM(Wrybill_year_flow)

summary(Wrybill_years_only)

Wrybill_testMod <- Wrybill_years_only
r2_Wrybill <- data.frame(R2M = r.squaredGLMM(Wrybill_testMod)[3,1],R2C = r.squaredGLMM(Wrybill_testMod)[3,2], species = "Wrybill")


# Visualisation of Model -------------------------------------------------------

Wrybill_pred <- expand.grid(scaledYear = unique(Wrybill$scaledYear),
                                     Year = unique(Wrybill$Year),
                                     section_number = factor(seq(1,5))) %>%
  left_join(mean_days_since_flood_key) %>%
  mutate(scaledMeanFlow = mean(meanFlow))

Wrybill_pred <- Wrybill_pred %>%mutate(Conditional = predict(Wrybill_testMod, type = "response", newdata = Wrybill_pred, re.form = NULL),
         Marginal = predict(Wrybill_testMod, type = "response", newdata = Wrybill_pred, re.form = NA)) %>%
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds") %>%
  mutate(species = "Wrybill")

wrybill_plot <- ggplot(Wrybill, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Wrybill per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = wrybill_pred, aes(x = (scaledYear * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1]),
                                     y = preds, linetype = type,
                                              size = type,
                                              color = ifelse(type == "Conditional",
                                                             section_number,
                                                             type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model",
       title = bquote("Marginal"~ R^2== .(round(r2_Wrybill[1], 3))~
                     "\nConditional"~ R^2== .(round(r2_Wrybill[2],3)))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)),
        plot.title = element_text(size = 11))

# ggeffect

wry_resp <- predict_response(wrybill_testMod, terms = "scaledYear")

ggplot(wry_resp, aes(x = (x + 2021), y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme(plot.title = element_blank()) +
  labs(x = "Year", y = "Predicted Wrybill per Surveyor")
