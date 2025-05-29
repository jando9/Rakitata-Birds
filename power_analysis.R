library(lme4)
library(ggplot2)
library(simr) #power analysis
library(rptR) #repeatability
library(MASS) #for glm.nb

source("initial_look.R")

# set up data ------------------------------------------------------------------
subset_of_interest <- list(species = c("Wrybill","Black-fronted tern", "Banded dotterel"),
                           start_year = 2000, end_year = 2025)
specified_years <- subset(UR_data, Year > subset_of_interest$start_year - 1 &
                                   Year < subset_of_interest$end_year + 1)

for (i in 1:length(subset_of_interest$species)) {
  subset_species <- subset(specified_years, Species == subset_of_interest$species[i]) %>%
           group_by(section_number, Year, Hectares) %>% summarise(Number = sum(Number))
  subset_species$section_number <- factor(subset_species$section_number)
  subset_species$centeredYear <- subset_species$Year - min(subset_species$Year)
  subset_species$logHectare <- log(subset_species$Hectares)
  subset_species$factorYear <- as.factor(subset_species$centeredYear)
  assign(paste0(gsub(" |-", "_", subset_of_interest$species[i])), subset_species)
}

ggplot(data = Banded_dotterel,
              aes(x = Year, y = Number, groups = section_number, colour = section_number)) +
         geom_line() +
         geom_point()

# Pilot Models ----------------------------------------------------------------
# wrybill
Wrybill_mod <- glmer(Number ~ centeredYear + (1 | section_number), offset = logHectare, family = "poisson",
                      data = Wrybill)
summary(Wrybill_mod)
resid_df <- 17
deviance(Wrybill_mod) / resid_df #dispersion okay

# BFT 
Black_fronted_tern_poisson <- glmer(Number ~ centeredYear + (1 | section_number), offset = logHectare, family = "poisson",
                     data = Black_fronted_tern)
summary(Black_fronted_tern_poisson)
resid_df <- 17
deviance(Black_fronted_tern_poisson) / resid_df # overdispersed

Black_fronted_tern_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = logHectare,
                                  data = Black_fronted_tern)
summary(Black_fronted_tern_NB) #very little variation by section when hectare included- area explains most of section variation?
resid_df <- 16
deviance(Black_fronted_tern_NB) / resid_df # dispersion sorted

# Banded dotterel
Banded_dotterel_poisson <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = logHectare, family = "poisson",
                     data = Banded_dotterel)
summary(Banded_dotterel_poisson)
resid_df <- 17
deviance(Banded_dotterel_poisson) / resid_df # overdispersed

Banded_dotterel_NB <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = logHectare,
                                data = Banded_dotterel)
summary(Banded_dotterel_NB)
resid_df <- 16
deviance(Banded_dotterel_NB) / resid_df # dispersion sorted

# Repeatability analysis -------------------------------------------------------
wrybill_repeat <- rpt(Number ~ (1|section_number), grname = "section_number",
                      data = wrybill, datatype = "Poisson", nboot = 1000, npermut = 0)
summary(wrybill_repeat)

# VC analysis ------------------------------------------------------------------
wrybill_vca <- fitVCA(Number ~ centeredYear/section_number, Data = wrybill)
summary(wrybill_vca)

# power analysis ---------------------------------------------------------------
effect_sizes <- seq(-0.1, 0.1, by = 0.05)
year_range <- 5:20

mod_list <- list(Wrybill = Wrybill_mod, BFT = Black_fronted_tern_NB, Banded_dotterel = Banded_dotterel_NB)
power_analyses <- list(NA)
# loop year*effects 
for (i in 2:length(mod_list)) {
  results <- expand.grid(effect = effect_sizes, years = year_range)
  results <- results %>% mutate(perc_change = (exp(effect)-1)*100, power = NA, lower = NA, upper = NA)
  for (j in 1:nrow(results)) {
    ef <- results$effect[j]
    n_years <- results$years[j]
  
    sim_model <- extend(mod_list[[i]], along = "centeredYear", n = n_years)
    
    fixef(sim_model)["centeredYear"] <- ef
  
    power_out <- try(powerSim(sim_model, fixed("centeredYear", "z"), nsim = 100), silent = TRUE)
    CI <- confint(power_out)
    #check class of power_out in case model fails to converge and gives try-error
    if (!inherits(power_out, "try-error")) {
      results$power[j] <- power_out$x
      results$lower[j] <- CI[1]*100
      results$upper[j] <- CI[2]*100
    } else {
      results$power[j] <- NA
      results$lower[j] <- NA
      results$upper[j] <- NA
    }
    
    cat("Finished effect =", ef, ", years =", n_years, "\n", "Model=", names(mod_list)[i])
  }
  power_analyses[[i]] <- results
  names(power_analyses)[i] <- names(mod_list)[i]
}
#-------------------------------------------------------------------------------
effect_colours <- c("#050e71", "#3540bd",  "grey", "#a5f7a1", "#0d8007")
power_analyses[]
for (i in 2:length(power_analyses)) {
  ggplot(power_analyses[["Banded_dotterel"]], aes(x = years, y = power, colour = perc_change, group = perc_change)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
    scale_color_gradientn(colours=effect_colours) +
    geom_hline(yintercept=80, linetype='dashed', col = 'grey')+
    labs(x = "Number of Years", y = "Power (%)", color = "% Change") +
    theme_minimal()
}
#------------------------------------------------------------------------------

