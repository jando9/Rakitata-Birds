#source("code/read_organise_data.R")
library(lme4)

ggplot(data = Black_billed_gull,
       aes(x = Year, y = (Number/(mean_daily_surveyors)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
Black_billed_gull_random_slope <- glmer(Number ~ scaledYear + (scaledYear | section_number),
                                      offset = log(mean_daily_surveyors), family = "poisson",
                                      data = Black_billed_gull)

Black_billed_gull_fixed_slope <- glmer(Number ~ scaledYear + (1 | section_number),
                                     offset = log(mean_daily_surveyors), family = "poisson",
                                     data = Black_billed_gull)

# Check overdispersion
summary(Black_billed_gull_random_slope)
random_df <- 15
deviance(Black_billed_gull_random_slope) / random_df #over dispersed
summary(Black_billed_gull_fixed_slope)
fixed_df <- 17
deviance(Black_billed_gull_fixed_slope) / fixed_df #over dispersed

# refit NB models
Black_billed_gull_random_slope_NB <- glmer.nb(Number ~ scaledYear + (scaledYear | section_number),
                                            offset = log(mean_daily_surveyors),
                                            data = Black_billed_gull) #singularity
Black_billed_gull_fixed_slope_NB <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                                           offset = log(mean_daily_surveyors),
                                           data = Black_billed_gull)
summary(Black_billed_gull_random_slope_NB)
random_NB_df <- 14
deviance(Black_billed_gull_random_slope_NB) / random_df # good
summary(Black_billed_gull_fixed_slope_NB)
fixed_NB_df <- 16
deviance(Black_billed_gull_fixed_slope_NB) / fixed_NB_df # good, use NB

AIC(Black_billed_gull_random_slope_NB, Black_billed_gull_fixed_slope_NB)
BIC(Black_billed_gull_random_slope_NB, Black_billed_gull_fixed_slope_NB)
# Use fixed slope

# Fixed effects selection ------------------------------------------------------

Black_billed_gull_year_flood_int <- glmer.nb(Number ~ scaledYear * scaledFlood + (1 | section_number),
                               offset = log(mean_daily_surveyors), 
                               data = Black_billed_gull)
Black_billed_gull_year_flow_int <- glmer.nb(Number ~ scaledYear * scaledMeanFlow + (1 | section_number),
                              offset = log(mean_daily_surveyors), 
                              data = Black_billed_gull)
Black_billed_gull_year_flood <- glmer.nb(Number ~ scaledYear + scaledFlood + (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Black_billed_gull)
Black_billed_gull_year_flow <- glmer.nb(Number ~ scaledYear + scaledMeanFlow + (1 | section_number),
                          offset = log(mean_daily_surveyors),
                          data = Black_billed_gull)
Black_billed_gull_years_only <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Black_billed_gull)
Black_billed_gull_no_years <- glmer.nb(Number ~ (1 | section_number),
                                         offset = log(mean_daily_surveyors),
                                         data = Black_billed_gull)

AIC(Black_billed_gull_year_flood_int, Black_billed_gull_year_flow_int, Black_billed_gull_year_flood, Black_billed_gull_year_flow, Black_billed_gull_years_only)
BIC(Black_billed_gull_year_flood_int, Black_billed_gull_year_flow_int, Black_billed_gull_year_flood, Black_billed_gull_year_flow, Black_billed_gull_years_only) # model better with interaction

BBG_lrt <- lrtest(Black_billed_gull_year_flood_int, Black_billed_gull_year_flow_int, Black_billed_gull_year_flood, Black_billed_gull_year_flow, Black_billed_gull_years_only)


r.squaredGLMM(Black_billed_gull_year_flow)

summary(Black_billed_gull_year_flood_int)
summary(Black_billed_gull_year_flow_int)


Black_billed_gull_testMod <- Black_billed_gull_year_flow

r2_Black_billed_gull <- data.frame(R2M = r.squaredGLMM(Black_billed_gull_testMod)[3,1],R2C = r.squaredGLMM(Black_billed_gull_testMod)[3,2], species = "Black-billed gull")


# Visualisation of Model -------------------------------------------------------

Black_billed_gull_pred <- expand.grid(scaledYear = unique(Black_billed_gull$scaledYear),
                                    Year = seq(min(Black_billed_gull$Year), max(Black_billed_gull$Year)),
                                    section_number = factor(seq(1,5))) %>%
  left_join(mean_days_since_flood_key) %>%
  mutate(scaledMeanFlow = mean(meanFlow))
Black_billed_gull_pred <- Black_billed_gull_pred %>% mutate(Conditional = predict(Black_billed_gull_testMod, type = "response", newdata = Black_billed_gull_pred, re.form = NULL),
                                                         Marginal = predict(Black_billed_gull_testMod, type = "response", newdata = Black_billed_gull_pred, re.form = NA)) %>%
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds") %>%
  mutate(species = "Black-billed gull")

Black_billed_gull_plot <- ggplot(Black_billed_gull, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Black Billed Gull per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = Black_billed_gull_pred, aes(x = scaledYear * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1],
                                               y = preds, linetype = type,
                                             size = type,
                                             color = ifelse(type == "Conditional",
                                                            section_number,
                                                            type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model",
       title = bquote("Marginal"~ R^2== .(round(r2_Black_billed_gull[1], 3))~
                        "\nConditional"~ R^2== .(round(r2_Black_billed_gull[2],3)))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)),
        plot.title = element_text(size = 11))

x <- predict_response(Black_billed_gull_int, terms = c("scaledYear", "scaledFlood"))
plot(x)
performance(Black_billed_gull_int)
