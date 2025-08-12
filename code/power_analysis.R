library(ggplot2)
library(simr) #power analysis
library(rptR) #repeatability
library(MASS) #for glm.nb
library(simglm) # for glm power analysis

# Source Data
source("code/read_organise_data.R")

# Source Test Models
model_files <- list.files(path = "code/species_models", full.names = T)
sapply(model_files, source)

# Power analysis ---------------------------------------------------------------
effect_sizes <- seq(-0.1, 0.1, by = 0.05)
year_range <- 5:20

mod_list <- list(wrybill = Wrybill_testMod, BFT = BFT_testMod, Banded_dotterel = Banded_dotterel_testMod,
                 Black_billed_gull = Black_billed_gull_testMod, SBBG = SBBG_testMod, SIPO = SIPO_testMod,
                 Spur_winged_plover = Spur_winged_plover_testMod)
power_analyses <- list(NA)
# loop year*effects 
for (i in 1:length(mod_list)) {
  results <- expand.grid(effect = effect_sizes, years = year_range)
  results <- results %>% mutate(perc_change = (exp(effect)-1)*100, power = NA, lower = NA, upper = NA)
  for (j in 1:nrow(results)) {
    ef <- results$effect[j]
    n_years <- results$years[j]
    sim_model <- extend(mod_list[[i]], along = "scaledYear", n = n_years)
    fixef(sim_model)["scaledYear"] <- ef
    power_out <- try(powerSim(sim_model, fixed("scaledYear", "z"), nsim = 100), silent = TRUE)
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

for(i in 1:length(power_analyses)){
  write.csv(power_analyses[[i]], file = paste0("power_analyses/both/", names(mod_list)[i], "_power.csv"))
}

# Visualize
file_names <- list.files("power_analyses/observer_only")
file_dir <- paste0("power_analyses/observer_only/", file_names)
power_analyses <- lapply(file_dir, read.csv)
names(power_analyses) <- file_names
names(file_names) <- c("Banded Dotterel ~ Years + (1 | Section Number), offset = log(Hectares), Family = Negative Binomial",
                       "Banded Dotterel ~ Years + (1 | Section Number), offset = log(Hectares * Mean Daily Observers), Family = Negative Binomial",
                       "Black Fronted Tern ~ Years + (1 | Section Number), offset = log(Hectares), Family = Negative Binomial",
                       "Black Fronted Tern ~ Years + (1 | Section Number), offset = log(Hectares * Mean Daily Observers), Family = Negative Binomial",
                       "Black Billed Gull ~ Years + (1 | Section Number), offset = log(Hectares), Family = Negative Binomial",
                       "Black Billed Gull ~ Years + (1 | Section Number), offset = log(Hectares * Mean Daily Observers), Family = Negative Binomial",
                       "Southern Black Backed Gull ~ Years + (1 | Section Number), offset = log(Hectares), Family = Negative Binomial",
                       "Southern Black Backed Gull ~ Years + (1 | Section Number), offset = log(Hectares * Mean Daily Observers), Family = Negative Binomial",
                       "South Island Pied Oystercatcher ~ Years + (1 | Section Number), offset = log(Hectares), Family = Negative Binomial",
                       "South Island Pied Oystercatcher ~ Years + (1 | Section Number), offset = log(Hectares * Mean Daily Observers), Family = Negative Binomial",
                       "Spur Winged Plover ~ Years + (1 | Section Number), offset = log(Hectares), Family = Negative Binomial",
                       "Spur Winged Plover ~ Years + (1 | Section Number), offset = log(Hectares * Mean Daily Observers), Family = Negative Binomial",
                       "Wrybill ~ Years + (1 | Section Number), offset = log(Hectares), Family = Poisson",
                       "Wrybill ~ Years + (1 | Section Number), offset = log(Hectares * Mean Daily Observers), Family = Poisson")

column_names <- colnames(power_analyses[[1]])
power_analyses <- lapply(power_analyses, setNames, column_names)

effect_colours <- c("#7b3294", "#ba8bcf",  "grey90", "#89da80", "#008837")
for(i in 1:length(power_analyses)){
  assign(paste0(gsub(".csv", "",names(power_analyses)[i]), "_plot"),
         ggplot(power_analyses[[i]], aes(x = years, y = power, colour = perc_change, group = perc_change)) +
               geom_line() +
               geom_point() +
               geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
               scale_color_gradientn(colours=effect_colours) +
               geom_hline(yintercept=80, linetype='dashed', col = 'grey20')+
               labs(x = "Years of Data", y = "Power (%)", color = "% Change",
                    title = paste0(names(power_analyses[i]), " ~ Years + (1 | Section Number), offset = log(Mean Daily Observers), Family = Negative Binomial")) +
               theme(plot.title = element_text(size = 11),
                     panel.background = element_rect(fill = "white"),
                     panel.grid = element_line(colour = alpha("grey", 0.4)))
  )
}

# Repeatability analysis -------------------------------------------------------
wrybill_repeat <- rpt(Number ~ (1|section_number), grname = "section_number",
                      data = wrybill, datatype = "Poisson", nboot = 1000, npermut = 0)
summary(wrybill_repeat)

# VC analysis ------------------------------------------------------------------
wrybill_vca <- fitVCA(Number ~ centeredYear/section_number, Data = wrybill)
summary(wrybill_vca)
