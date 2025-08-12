source("code/read_organise_data.R")
library(lme4)

ggplot(data = Banded_dotterel,
       aes(x = Year, y = (Number/(mean_daily_surveyors)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
Banded_dotterel_random_slope <- glmer(Number ~ scaledYear + (scaledYear | section_number),
                                      offset = log(mean_daily_surveyors), family = "poisson",
                                      data = Banded_dotterel)

Banded_dotterel_fixed_slope <- glmer(Number ~ scaledYear + (1 | section_number),
                                     offset = log(mean_daily_surveyors), family = "poisson",
                                     data = Banded_dotterel)

# Check overdispersion
summary(Banded_dotterel_random_slope)
random_df <- 15
deviance(Banded_dotterel_random_slope) / random_df #over dispersed
summary(Banded_dotterel_fixed_slope)
fixed_df <- 17
deviance(Banded_dotterel_fixed_slope) / fixed_df #over dispersed

# refit NB models
Banded_dotterel_random_slope_NB <- glmer.nb(Number ~ scaledYear + (scaledYear | section_number),
                                            offset = log(mean_daily_surveyors),
                                            data = Banded_dotterel) # over parameterized
Banded_dotterel_fixed_slope_NB <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                                           offset = log(mean_daily_surveyors),
                                           data = Banded_dotterel)
summary(Banded_dotterel_random_slope_NB)
random_NB_df <- 14
deviance(Banded_dotterel_random_slope_NB) / random_df # good

summary(Banded_dotterel_fixed_slope_NB)
fixed_NB_df <- 16
deviance(Banded_dotterel_fixed_slope_NB) / fixed_NB_df # good, use NB

AIC(Banded_dotterel_random_slope_NB, Banded_dotterel_fixed_slope_NB)
AICc(Banded_dotterel_random_slope_NB, Banded_dotterel_fixed_slope_NB)
BIC(Banded_dotterel_random_slope_NB, Banded_dotterel_fixed_slope_NB)
# Use fixed slope

# Fixed effects selection ------------------------------------------------------

Banded_dotterel_year_flood_int <- glmer.nb(Number ~ scaledYear * scaledFlood + (1 | section_number),
                                offset = log(mean_daily_surveyors), 
                                data = Banded_dotterel)
Banded_dotterel_year_flow_int <- glmer.nb(Number ~ scaledYear * scaledMeanFlow + (1 | section_number),
                               offset = log(mean_daily_surveyors), 
                               data = Banded_dotterel)
Banded_dotterel_year_flood <- glmer.nb(Number ~ scaledYear + scaledFlood + (1 | section_number),
                            offset = log(mean_daily_surveyors),
                            data = Banded_dotterel)
Banded_dotterel_year_flow <- glmer.nb(Number ~ scaledYear + scaledMeanFlow + (1 | section_number),
                           offset = log(mean_daily_surveyors),
                           data = Banded_dotterel)
Banded_dotterel_years_only <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                            offset = log(mean_daily_surveyors),
                            data = Banded_dotterel)
Banded_dotterel_no_years <- glmer.nb(Number ~ (1 | section_number),
                                       offset = log(mean_daily_surveyors),
                                       data = Banded_dotterel)

AIC(Banded_dotterel_year_flood_int, Banded_dotterel_year_flow_int, Banded_dotterel_year_flood, Banded_dotterel_year_flow, Banded_dotterel_years_only)
AICc(Banded_dotterel_year_flood_int, Banded_dotterel_year_flow_int, Banded_dotterel_year_flood, Banded_dotterel_year_flow, Banded_dotterel_years_only)
BIC(Banded_dotterel_year_flood_int, Banded_dotterel_year_flow_int, Banded_dotterel_year_flood, Banded_dotterel_year_flow, Banded_dotterel_years_only) # model better without interaction

Banded_dotterel_lrt <- lrtest(Banded_dotterel_year_flood_int, Banded_dotterel_year_flow_int, Banded_dotterel_year_flood, Banded_dotterel_year_flow, Banded_dotterel_years_only)

r.squaredGLMM(Banded_dotterel_year_flow, Banded_dotterel_year_flood_int, Banded_dotterel_year_flow_int, Banded_dotterel_year_flood, Banded_dotterel_year_flow, Banded_dotterel_years_only)

summary(Banded_dotterel_years_only)


Banded_dotterel_testMod <- Banded_dotterel_years_only
r2_Banded_dotterel <- data.frame(R2M = r.squaredGLMM(Banded_dotterel_testMod)[3,1],R2C = r.squaredGLMM(Banded_dotterel_testMod)[3,2], species = "Banded dotterel")

# Visualisation of Model -------------------------------------------------------

banded_dotterel_pred <- expand.grid(scaledYear = unique(Banded_dotterel$scaledYear),
                        Year = unique(Banded_dotterel$Year),
                        section_number = factor(seq(1,5))) %>%
  left_join(mean_days_since_flood_key) %>%
  mutate(scaledMeanFlow = mean(meanFlow))
banded_dotterel_pred <- banded_dotterel_pred %>%  mutate(Conditional = predict(Banded_dotterel_testMod, type = "response", newdata = banded_dotterel_pred, re.form = NULL),
                                 Marginal = predict(Banded_dotterel_testMod, type = "response", newdata = banded_dotterel_pred, re.form = NA)) %>%
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds") %>%
  mutate(species = "Banded dotterel")

banded_dotterel_plot <- ggplot(Banded_dotterel, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Banded Dotterel per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = banded_dotterel_pred, aes(x = (scaledYear * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1]),
                                 y = preds, linetype = type,
                                 size = type,
                                 color = ifelse(type == "Conditional",
                                                section_number,
                                                type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model",
       title = bquote("Marginal"~ R^2== .(round(r2_Banded_dotterel[1,1], 3))~
                        "\nConditional"~ R^2== .(round(r2_Banded_dotterel[1,2],3)))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)),
        plot.title = element_text(size = 11))

