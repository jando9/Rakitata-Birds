#source("code/read_organise_data.R")
library(lme4)


ggplot(data = Spur_winged_plover,
       aes(x = Year, y = (Number/(mean_daily_surveyors)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
Spur_winged_plover_random_slope <- glmer(Number ~ scaledYear + (scaledYear | section_number),
                           offset = log(mean_daily_surveyors), family = "poisson",
                           data = Spur_winged_plover) #singularity

Spur_winged_plover_fixed_slope <- glmer(Number ~ scaledYear + (1 | section_number),
                          offset = log(mean_daily_surveyors), family = "poisson",
                          data = Spur_winged_plover)

# Check overdispersion
summary(Spur_winged_plover_random_slope)
random_df <- 15
deviance(Spur_winged_plover_random_slope) / random_df #over dispersed
summary(Spur_winged_plover_fixed_slope)
fixed_df <- 17
deviance(Spur_winged_plover_fixed_slope) / fixed_df #over dispersed

# refit NB models
Spur_winged_plover_random_slope_NB <- glmer.nb(Number ~ scaledYear + (scaledYear | section_number),
                                 offset = log(mean_daily_surveyors),
                                 data = Spur_winged_plover) #singularity
Spur_winged_plover_fixed_slope_NB <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                                offset = log(mean_daily_surveyors),
                                data = Spur_winged_plover)
summary(Spur_winged_plover_random_slope_NB)
random_NB_df <- 14
deviance(Spur_winged_plover_random_slope_NB) / random_df # okay
summary(Spur_winged_plover_fixed_slope_NB)
fixed_NB_df <- 16
deviance(Spur_winged_plover_fixed_slope_NB) / fixed_NB_df # okay

AIC(Spur_winged_plover_random_slope_NB, Spur_winged_plover_fixed_slope_NB)
AICc(Spur_winged_plover_random_slope_NB, Spur_winged_plover_fixed_slope_NB)
BIC(Spur_winged_plover_random_slope_NB, Spur_winged_plover_fixed_slope_NB)
# use fixed slope

# Fixed effects selection ------------------------------------------------------

Spur_winged_plover_year_flood_int <- glmer.nb(Number ~ scaledYear * scaledFlood + (1 | section_number),
                               offset = log(mean_daily_surveyors), 
                               data = Spur_winged_plover)
Spur_winged_plover_year_flow_int <- glmer.nb(Number ~ scaledYear * scaledMeanFlow + (1 | section_number),
                              offset = log(mean_daily_surveyors), 
                              data = Spur_winged_plover)
Spur_winged_plover_year_flood <- glmer.nb(Number ~ scaledYear + scaledFlood + (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Spur_winged_plover)
Spur_winged_plover_year_flow <- glmer.nb(Number ~ scaledYear + scaledMeanFlow + (1 | section_number),
                          offset = log(mean_daily_surveyors),
                          data = Spur_winged_plover)
Spur_winged_plover_years_only <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Spur_winged_plover)
Spur_winged_plover_no_years <- glmer.nb(Number ~ (1 | section_number),
                                          offset = log(mean_daily_surveyors),
                                          data = Spur_winged_plover)

AIC(Spur_winged_plover_year_flood_int, Spur_winged_plover_year_flow_int, Spur_winged_plover_year_flood, Spur_winged_plover_year_flow, Spur_winged_plover_years_only)
AICc(Spur_winged_plover_year_flood_int, Spur_winged_plover_year_flow_int, Spur_winged_plover_year_flood, Spur_winged_plover_year_flow, Spur_winged_plover_years_only)
BIC(Spur_winged_plover_year_flood_int, Spur_winged_plover_year_flow_int, Spur_winged_plover_year_flood, Spur_winged_plover_year_flow, Spur_winged_plover_years_only) # close but not int better

SpurWing_lrt <- lrtest(Spur_winged_plover_year_flood_int, Spur_winged_plover_year_flow_int, Spur_winged_plover_year_flood, Spur_winged_plover_year_flow, Spur_winged_plover_years_only)

r.squaredGLMM(Spur_winged_plover_year_flow)

summary(Spur_winged_plover_years_only)

Spur_winged_plover_testMod <- Spur_winged_plover_years_only

r2_Spur_winged_plover <- data.frame(R2M = r.squaredGLMM(Spur_winged_plover_testMod)[3,1],R2C = r.squaredGLMM(Spur_winged_plover_testMod)[3,2], species = "Spur-winged plover")

# Visualisation ----------------------------------------------------------------

Spur_winged_plover_pred <- expand.grid(scaledYear = unique(Spur_winged_plover$scaledYear),
                                      Year = seq(min(Spur_winged_plover$Year), max(Spur_winged_plover$Year)),
                                      section_number = factor(seq(1,5))) %>%
  left_join(mean_days_since_flood_key) %>%
  mutate(scaledMeanFlow = mean(meanFlow))

Spur_winged_plover_pred <- Spur_winged_plover_pred %>% mutate(Conditional = predict(Spur_winged_plover_testMod, type = "response", newdata = Spur_winged_plover_pred, re.form = NULL),
                                                            Marginal = predict(Spur_winged_plover_testMod, type = "response", newdata = Spur_winged_plover_pred, re.form = NA)) %>%
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds") %>%
  mutate(species = "Spur-winged plover")

Spur_winged_plover_plot <- ggplot(Spur_winged_plover, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Spur Winged Plover per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = Spur_winged_plover_pred, aes(x = scaledYear * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1],
                                               y = preds, linetype = type,
                                               size = type,
                                               color = ifelse(type == "Conditional",
                                                              section_number,
                                                              type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model",
       title = bquote("Marginal"~ R^2== .(round(r2_Spur_winged_plover[1], 3))~
                        "\nConditional"~ R^2== .(round(r2_Spur_winged_plover[2],3)))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)),
        plot.title = element_text(size = 11))

