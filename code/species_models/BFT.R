#source("code/read_organise_data.R")
library(lme4)

ggplot(data = Black_fronted_tern,
       aes(x = Year, y = (Number/(mean_daily_surveyors)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
BFT_random_slope <- glmer(Number ~ scaledYear + (scaledYear | section_number),
                              offset = log(mean_daily_surveyors), family = "poisson",
                              data = Black_fronted_tern)

BFT_fixed_slope <- glmer(Number ~ scaledYear + (1 | section_number),
                             offset = log(mean_daily_surveyors), family = "poisson",
                             data = Black_fronted_tern)

# Check overdispersion
summary(BFT_random_slope)
random_df <- 15
deviance(BFT_random_slope) / random_df #over dispersed
summary(BFT_fixed_slope)
fixed_df <- 17
deviance(BFT_fixed_slope) / fixed_df #over dispersed

# refit NB models
BFT_random_slope_NB <- glmer.nb(Number ~ scaledYear + (scaledYear | section_number),
                          offset = log(mean_daily_surveyors),
                          data = Black_fronted_tern) # likely over parameterized with random slope
BFT_fixed_slope_NB <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                                offset = log(mean_daily_surveyors),
                                data = Black_fronted_tern)
summary(BFT_random_slope_NB)
random_NB_df <- 14
deviance(BFT_random_slope_NB) / random_df # good
summary(BFT_fixed_slope_NB)
fixed_NB_df <- 16
deviance(BFT_fixed_slope_NB) / fixed_NB_df # good, use NB

AIC(BFT_random_slope_NB, BFT_fixed_slope_NB)
AICc(BFT_random_slope_NB, BFT_fixed_slope_NB)
BIC(BFT_random_slope_NB, BFT_fixed_slope_NB)
# Use simpler model

# Fixed effects selection ------------------------------------------------------

BFT_year_flood_int <- glmer.nb(Number ~ scaledYear * scaledFlood + (1 | section_number),
                                offset = log(mean_daily_surveyors), 
                                data = Black_fronted_tern)
BFT_year_flow_int <- glmer.nb(Number ~ scaledYear * scaledMeanFlow + (1 | section_number),
                               offset = log(mean_daily_surveyors), 
                               data = Black_fronted_tern)
BFT_year_flood <- glmer.nb(Number ~ scaledYear + scaledFlood + (1 | section_number),
                            offset = log(mean_daily_surveyors),
                            data = Black_fronted_tern)
BFT_year_flow <- glmer.nb(Number ~ scaledYear + scaledMeanFlow + (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Black_fronted_tern)
BFT_years_only <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                            offset = log(mean_daily_surveyors),
                            data = Black_fronted_tern)
BFT_no_years <- glmer.nb(Number ~ (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Black_fronted_tern)

AIC(BFT_year_flood_int, BFT_year_flow_int, BFT_year_flood, BFT_year_flow, BFT_years_only)
AICc(BFT_year_flood_int, BFT_year_flow_int, BFT_year_flood, BFT_year_flow, BFT_years_only)
BIC(BFT_year_flood_int, BFT_year_flow_int, BFT_year_flood, BFT_year_flow, BFT_years_only) # use years only

BFT_lrt <- lrtest(BFT_year_flood_int, BFT_year_flow_int, BFT_year_flood, BFT_year_flow, BFT_years_only)

r.squaredGLMM(BFT_year_flow)

summary(BFT_years_only)

BFT_testMod <- BFT_years_only
r2_BFT <- data.frame(R2M = r.squaredGLMM(BFT_testMod)[3,1],R2C = r.squaredGLMM(BFT_testMod)[3,2], species = "Black-fronted tern")


# Visualisation of Model -------------------------------------------------------

BFT_pred <- expand.grid(scaledYear = unique(Black_fronted_tern$scaledYear),
                            Year = unique(Black_fronted_tern$Year),
                            section_number = factor(seq(1,5))) %>%
  left_join(mean_days_since_flood_key)%>%
  mutate(scaledMeanFlow = mean(meanFlow))
BFT_pred <- BFT_pred %>%  mutate(Conditional = predict(BFT_testMod, type = "response", newdata = BFT_pred, re.form = NULL),
                                         Marginal = predict(BFT_testMod, type = "response", newdata = BFT_pred, re.form = NA)) %>%
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds") %>%
  mutate(species = "Black-fronted tern")

BFT_plot <- ggplot(Black_fronted_tern, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Black Fronted Tern per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = BFT_pred, aes(x = (scaledYear * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1]),
                                     y = preds, linetype = type,
                                     size = type,
                                     color = ifelse(type == "Conditional",
                                                    section_number,
                                                    type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model",
       title = bquote("Marginal"~ R^2== .(round(r2_BFT[1], 3))~
                        "\nConditional"~ R^2== .(round(r2_BFT[2],3)))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)),
        plot.title = element_text(size = 11))

# ggeffects

BFT_resp <- predict_response(BFT_testMod, terms = c("scaledYear", "mean_days_since_flood"))

ggplot(BFT_resp, aes(x = (x + 2021), y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  theme(plot.title = element_blank()) +
  labs(x = "Year", y = "Predicted BFT per Surveyor")

            