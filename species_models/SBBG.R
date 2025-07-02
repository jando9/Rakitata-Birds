#source("code/read_organise_data.R")
library(lme4)


ggplot(data = Southern_black_backed_gull,
       aes(x = Year, y = (Number/(mean_daily_surveyors)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
SBBG_random_slope <- glmer(Number ~ scaledYear + (scaledYear | section_number),
                           offset = log(mean_daily_surveyors), family = "poisson",
                           data = Southern_black_backed_gull)

SBBG_fixed_slope <- glmer(Number ~ scaledYear + (1 | section_number),
                          offset = log(mean_daily_surveyors), family = "poisson",
                          data = Southern_black_backed_gull)

# Check overdispersion
summary(SBBG_random_slope)
random_df <- 15
deviance(SBBG_random_slope) / random_df #over dispersed
summary(SBBG_fixed_slope)
fixed_df <- 17
deviance(SBBG_fixed_slope) / fixed_df #over dispersed

# refit NB models
SBBG_random_slope_NB <- glmer.nb(Number ~ scaledYear + (scaledYear | section_number),
                                 offset = log(mean_daily_surveyors),
                                 data = Southern_black_backed_gull) #singularity
SBBG_fixed_slope_NB <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                                offset = log(mean_daily_surveyors),
                                data = Southern_black_backed_gull)
summary(SBBG_random_slope_NB)
random_NB_df <- 14
deviance(SBBG_random_slope_NB) / random_df # good
summary(SBBG_fixed_slope_NB)
fixed_NB_df <- 16
deviance(SBBG_fixed_slope_NB) / fixed_NB_df # good

AIC(SBBG_random_slope_NB, SBBG_fixed_slope_NB)
BIC(SBBG_random_slope_NB, SBBG_fixed_slope_NB)
# use fixed slope

# Fixed effects selection ------------------------------------------------------

SBBG_year_flood_int <- glmer.nb(Number ~ scaledYear * scaledFlood + (1 | section_number),
                               offset = log(mean_daily_surveyors), 
                               data = Southern_black_backed_gull)
SBBG_year_flow_int <- glmer.nb(Number ~ scaledYear * scaledMeanFlow + (1 | section_number),
                              offset = log(mean_daily_surveyors), 
                              data = Southern_black_backed_gull)
SBBG_year_flood <- glmer.nb(Number ~ scaledYear + scaledFlood + (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Southern_black_backed_gull)
SBBG_year_flow <- glmer.nb(Number ~ scaledYear + scaledMeanFlow + (1 | section_number),
                          offset = log(mean_daily_surveyors),
                          data = Southern_black_backed_gull)
SBBG_years_only <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Southern_black_backed_gull)

AIC(SBBG_year_flood_int, SBBG_year_flow_int, SBBG_year_flood, SBBG_year_flow, SBBG_years_only)
BIC(SBBG_year_flood_int, SBBG_year_flow_int, SBBG_year_flood, SBBG_year_flow, SBBG_years_only)

r.squaredGLMM(SBBG_year_flood_int)

summary(SBBG_years_only)

SBBG_testMod <- SBBG_years_only

r2_SBBG <- data.frame(R2M = r.squaredGLMM(SBBG_testMod)[3,1],R2C = r.squaredGLMM(SBBG_testMod)[3,2], species = "Southern black-backed gull")


# Visualisation of Model -------------------------------------------------------

SBBG_pred <- expand.grid(scaledYear = unique(Southern_black_backed_gull$scaledYear),
                                       Year = seq(min(Southern_black_backed_gull$Year), max(Southern_black_backed_gull$Year)),
                                       section_number = factor(seq(1,5)))  %>%
  left_join(mean_days_since_flood_key) %>%
  mutate(scaledMeanFlow = mean(meanFlow))

SBBG_pred <- SBBG_pred %>% mutate(Conditional = predict(SBBG_testMod, type = "response", newdata = SBBG_pred, re.form = NULL),
                                                              Marginal = predict(SBBG_testMod, type = "response", newdata = SBBG_pred, re.form = NA)) %>%
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds") %>%
  mutate(species = "Southern black-backed gull")

SBBG_plot <- ggplot(Southern_black_backed_gull, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Spur Winged Plover per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = SBBG_pred, aes(x = scaledYear * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1],
                                                y = preds, linetype = type,
                                                size = type,
                                                color = ifelse(type == "Conditional",
                                                               section_number,
                                                               type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model",
       title = bquote("Marginal"~ R^2== .(round(r2_SBBG[1], 3))~
                        "\nConditional"~ R^2== .(round(r2_SBBG[2],3)))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)),
        plot.title = element_text(size = 11))

