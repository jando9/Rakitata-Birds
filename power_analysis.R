library(lme4)
library(ggplot2)
library(simr) #power analysis
library(rptR) #repeatability
library(MASS) #for glm.nb


source("read_organise_data.R")

## set up data ------------------------------------------------------------------
subset_of_interest <- list(species = c("Wrybill","Black-fronted tern", "Banded dotterel", "Black Stilt/Kakī"),
                           start_year = 2021, end_year = 2025)
specified_years <- subset(filled_counts_noDups, Year > subset_of_interest$start_year - 1 &
                                   Year < subset_of_interest$end_year + 1) 

for (i in 1:length(subset_of_interest$species)) {
  subset_species <- subset(specified_years, Species == subset_of_interest$species[i]) %>%
           group_by(section_number, Year, Hectares) %>% summarise(Number = sum(Number))
  subset_species$section_number <- factor(subset_species$section_number)
  subset_species$centeredYear <- subset_species$Year - min(subset_species$Year)
  subset_species$factorYear <- as.factor(subset_species$centeredYear)
  with_flow <- inner_join(subset_species, flow_and_observers, by = "Year")
  assign(paste0(gsub(" |-", "_", subset_of_interest$species[i])), with_flow)
}

ggplot(data = Wrybill,
              aes(x = Year, y = Number, groups = section_number, colour = section_number)) +
         geom_line() +
         geom_point()

section_plot <- UR_data %>% distinct(Year, section_number) %>%
  mutate(one = case_when(grepl("1", section_number) ~ 1,
                         !grepl("1", section_number) ~ 0),
         two = case_when(grepl("2", section_number) ~ 1,
                         !grepl("2", section_number) ~ 0),
         three = case_when(grepl("3", section_number) ~ 1,
                         !grepl("3", section_number) ~ 0),
         four = case_when(grepl("4", section_number) ~ 1,
                         !grepl("4", section_number) ~ 0),
         five = case_when(grepl("5", section_number) ~ 1,
                         !grepl("5", section_number) ~ 0)) %>%
  pivot_longer(one:five, names_to = "current_sections", values_to = "presence") %>% 
  subset(presence == 1) %>% mutate(current_sections = factor(current_sections, levels = c("five", "four", "three", "two", "one")))
  
  ggplot(data = section_plot,
         aes(x = Year, y = current_sections, fill = section_number)) +
  geom_tile(colour = "white", linewidth = 1.2,) 


## Pilot Models ----------------------------------------------------------------
# wrybill
Wrybill_random_slope <- glmer(Number ~ centeredYear + (centeredYear | section_number), offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                      data = Wrybill)

Wrybill_fixed_slope <- glmer(Number ~ centeredYear + (1 | section_number), offset = log(mean_daily_surveyors*Hectares), family = "poisson",
                             data = Wrybill)

summary(Wrybill_random_slope)
resid_df <- 15
deviance(Wrybill_random_slope) / resid_df 

AIC(Wrybill_random_slope, Wrybill_fixed_slope)
BIC(Wrybill_random_slope, Wrybill_fixed_slope)


Wrybill_NB <- glmer.(Number ~ centeredYear + (1 | section_number), offset = log(Hectares),
         data = Wrybill)

# BFT 
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

# Banded dotterel
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

mod_list <- list(Wrybill = Wrybill_poisson, BFT = Black_fronted_tern_NB)
power_analyses <- list(NA)
# loop year*effects 
for (i in 1:length(mod_list)) {
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
effect_colours <- c("#050e71", "#3540bd",  "grey", "#75ff6e", "#0d8007")

ggplot(power_analyses[["Wrybill"]], aes(x = years, y = power, colour = perc_change, group = perc_change)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
  scale_color_gradientn(colours=effect_colours) +
  geom_hline(yintercept=80, linetype='dashed', col = 'darkgrey')+
  labs(x = "Number of Years", y = "Power (%)", color = "% Change") +
  theme_minimal()

#------------------------------------------------------------------------------

