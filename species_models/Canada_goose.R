#source("code/read_organise_data.R")
library(lme4)
library(MASS)
library(ggeffects)


ggplot(data = Canada_goose,
       aes(x = Year, y = (Number/(mean_daily_surveyors)), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

Canada_goose$logSurveyors <- log(Canada_goose$mean_daily_surveyors)

surveyorKey <- unique(Canada_goose[,c("Year", "logSurveyors")])

# Random effect selection ------------------------------------------------------
Canada_goose_random_slope <- glmer(Number ~ scaledYear + (scaledYear | section_number),
                                         offset = log(mean_daily_surveyors), family = "poisson",
                                         data = Canada_goose) #singularity

Canada_goose_fixed_slope <- glmer(Number ~ scaledYear + (1 | section_number),
                                        offset = log(mean_daily_surveyors), family = "poisson",
                                        data = Canada_goose)

# Check overdispersion
summary(Canada_goose_random_slope)
random_df <- 15
deviance(Canada_goose_random_slope) / random_df #over dispersed
summary(Canada_goose_fixed_slope)
fixed_df <- 17
deviance(Canada_goose_fixed_slope) / fixed_df #over dispersed

# refit NB models
Canada_goose_random_slope_NB <- glmer.nb(Number ~ scaledYear + (scaledYear | section_number),
                                               offset = log(mean_daily_surveyors),
                                               data = Canada_goose) # singularity
Canada_goose_fixed_slope_NB <- glmer.nb(Number ~ scaledYear + (1 | section_number),
                                              offset = log(mean_daily_surveyors),
                                              data = Canada_goose) #singularity
Canada_goose_glm <- glm(Number ~ scaledYear, offset = log(mean_daily_surveyors),
                        family = "poisson",
                        data = Canada_goose)
summary(Canada_goose_glm)
glm_df <- 18
deviance(Canada_goose_glm)/glm_df # over dispersed

Canada_goose_glm.nb <- glm.nb(Number ~ scaledYear + offset(log(mean_daily_surveyors)),
                           data = Canada_goose)
summary(Canada_goose_glm.nb) 


# use glm 

# Fixed effects selection ------------------------------------------------------

Canada_goose_int <- glm.nb(Number ~ scaledYear * scaledLogFlood + offset(logSurveyors),
                           data = Canada_goose)
Canada_goose_year_flood <- glm.nb(Number ~ scaledYear + scaledLogFlood + offset(logSurveyors),
                                  data = Canada_goose)
Canada_goose_years_only <- glm.nb(Number ~ scaledYear + offset(logSurveyors),
                                  data = Canada_goose)

AIC(Canada_goose_int, Canada_goose_year_flood)
BIC(Canada_goose_int, Canada_goose_year_flood) # with interaction

AIC(Canada_goose_year_flood, Canada_goose_years_only)
BIC(Canada_goose_year_flood, Canada_goose_years_only) 

r.squaredGLMM(Canada_goose_int)

summary(Canada_goose_int)

Canada_goose_testMod <- Canada_goose_int

r2_Canada_goose <- r.squaredGLMM(Canada_goose_testMod)

# Visualisation of Model -------------------------------------------------------

CG_resp <- predict_response(Canada_goose_testMod, terms = "scaledYear")

Canada_goose_plot <- ggplot() +
  ylab("Canada Geese per Surveyor") + xlab("Year") +
  geom_point(data = Canada_goose, aes(x = Year, y = Number)) +
  geom_line(data = CG_resp, aes(x = x * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1],
                                  y = predicted)) +
  labs(title = bquote(R^2== .(round(r2_Canada_goose[1], 3)))) +
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 11))

