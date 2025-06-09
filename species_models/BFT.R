#source("read_organise_data.R")
library(lme4)

Black_fronted_tern_poisson <- glmer(Number ~ centeredYear + (1 | section_number), offset = log(mean_daily_surveyors), family = "poisson",
                                    data = Black_fronted_tern)
summary(Black_fronted_tern_poisson)
resid_df <- 17
deviance(Black_fronted_tern_poisson) / resid_df # overdispersed

Black_fronted_tern_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = log(mean_daily_surveyors),
                                  data = Black_fronted_tern)
summary(Black_fronted_tern_NB) #very little variation by section when hectare included- area explains most of section variation?
resid_df <- 16
deviance(Black_fronted_tern_NB) / resid_df # dispersion sorted

AIC()
BIC()