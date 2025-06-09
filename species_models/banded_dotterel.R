#source("read_organise_data.R")
library(lme4)

Banded_dotterel_poisson <- glmer(Number ~ centeredYear + (1 | section_number), offset = log(mean_daily_surveyors), family = "poisson",
                                 data = Banded_dotterel)
summary(Banded_dotterel_poisson)
resid_df <- 17
deviance(Banded_dotterel_poisson) / resid_df # overdispersed

Banded_dotterel_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = log(mean_daily_surveyors),
                               data = Banded_dotterel)
summary(Banded_dotterel_NB)
resid_df <- 16
deviance(Banded_dotterel_NB) / resid_df # dispersion sorted

AIC()
BIC()