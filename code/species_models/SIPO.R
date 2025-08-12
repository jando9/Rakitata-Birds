#source("code/read_organise_data.R")
library(lme4)


ggplot(data = South_Island_pied_oystercatcher,
       aes(x = Year, y = (Number/(mean_daily_surveyors)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

# Random effect selection ------------------------------------------------------
SIPO_random_slope <- glmer(Number ~ scaledYear + (scaledYear | section_number),
                                 offset = log(mean_daily_surveyors), family = "poisson",
                                 data = South_Island_pied_oystercatcher)

SIPO_fixed_slope <- glmer(Number ~ scaledYear + (1 | section_number),
                                offset = log(mean_daily_surveyors), family = "poisson",
                                data = South_Island_pied_oystercatcher)

# Check overdispersion
summary(SIPO_random_slope)
random_df <- 15
deviance(SIPO_random_slope) / random_df #over dispersed
summary(SIPO_fixed_slope)
fixed_df <- 17
deviance(SIPO_fixed_slope) / fixed_df #over dispersed

# refit NB models
SIPO_random_slope_NB <- glmer.nb(Number ~ scaledYear + (scaledYear | section_number),
                                       offset = log(mean_daily_surveyors),
                                       data = South_Island_pied_oystercatcher) # singularity warning
SIPO_fixed_slope_NB <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                                      offset = log(mean_daily_surveyors),
                                      data = South_Island_pied_oystercatcher)
summary(SIPO_random_slope_NB)
random_NB_df <- 14
deviance(SIPO_random_slope_NB) / random_df # good
summary(SIPO_fixed_slope_NB)
fixed_NB_df <- 16
deviance(SIPO_fixed_slope_NB) / fixed_NB_df # good

AIC(SIPO_random_slope_NB, SIPO_fixed_slope_NB)
BIC(SIPO_random_slope_NB, SIPO_fixed_slope_NB)
# use fixed slope

# Fixed effects selection ------------------------------------------------------

SIPO_year_flood_int <- glmer.nb(Number ~ scaledYear * scaledFlood + (1 | section_number),
                                offset = log(mean_daily_surveyors), 
                                data = South_Island_pied_oystercatcher)
SIPO_year_flow_int <- glmer.nb(Number ~ scaledYear * scaledMeanFlow + (1 | section_number),
                                offset = log(mean_daily_surveyors), 
                                data = South_Island_pied_oystercatcher)
SIPO_year_flood <- glmer.nb(Number ~ scaledYear + scaledFlood + (1 | section_number),
                                       offset = log(mean_daily_surveyors),
                                       data = South_Island_pied_oystercatcher)
SIPO_year_flow <- glmer.nb(Number ~ scaledYear + scaledMeanFlow + (1 | section_number),
                            offset = log(mean_daily_surveyors),
                            data = South_Island_pied_oystercatcher)
SIPO_years_only <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                                       offset = log(mean_daily_surveyors),
                                       data = South_Island_pied_oystercatcher)
SIPO_no_years <- glmer.nb(Number ~ (1 | section_number),
                            offset = log(mean_daily_surveyors),
                            data = South_Island_pied_oystercatcher)

AIC(SIPO_year_flood_int, SIPO_year_flow_int, SIPO_year_flood, SIPO_year_flow, SIPO_years_only)
BIC(SIPO_year_flood_int, SIPO_year_flow_int, SIPO_year_flood, SIPO_year_flow, SIPO_years_only) # model better without interaction

SIPO_lrt <- lrtest(SIPO_year_flood_int, SIPO_year_flow_int, SIPO_year_flood, SIPO_year_flow, SIPO_years_only)

r.squaredGLMM(SIPO_year_flow)

summary(SIPO_int)

SIPO_testMod <- SIPO_years_only
r2_SIPO <- data.frame(R2M = r.squaredGLMM(SIPO_testMod)[3,1],R2C = r.squaredGLMM(SIPO_testMod)[3,2], species = "South Island pied oystercatcher")


# Visualisation of Model -------------------------------------------------------

SIPO_pred <- expand.grid(scaledYear = unique(South_Island_pied_oystercatcher$scaledYear),
                        Year = unique(South_Island_pied_oystercatcher$Year),
                        section_number = factor(seq(1,5))) %>%
  left_join(mean_days_since_flood_key)%>%
  mutate(scaledMeanFlow = mean(meanFlow))

SIPO_pred <- SIPO_pred %>%  mutate(Conditional = predict(SIPO_testMod, type = "response", newdata = SIPO_pred, re.form = NULL),
                                 Marginal = predict(SIPO_testMod, type = "response", newdata = SIPO_pred, re.form = NA)) %>%
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds") %>%
  mutate(species = "South Island pied oystercatcher")

SIPO_plot <- ggplot(South_Island_pied_oystercatcher, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("South Island Pied Oystercatcher per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = SIPO_pred, aes(x = (scaledYear * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1]),
                                 y = preds, linetype = type,
                                 size = type,
                                 color = ifelse(type == "Conditional",
                                                section_number,
                                                type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model",
       title = bquote("Marginal"~ R^2== .(round(r2_SIPO[1], 3))~
                        "\nConditional"~ R^2== .(round(r2_SIPO[2],3)))) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)),
        plot.title = element_text(size = 11))


