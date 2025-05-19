library(simr)
library(lme4)
library(ggplot2)

source("initial_look.R")
# set up data ------------------------------------------------------------------
post_2020 <- subset(UR_data, Year > 2020)
wrybill <- subset(post_2020, Species == "Wrybill") %>% group_by(section_number, Year, Hectares) %>%
  summarise(Number=sum(Number))
wrybill$section_number <- factor(wrybill$section_number)
wrybill$centeredYear <- wrybill$Year - min(wrybill$Year)
wrybill$logHectare <- log(wrybill$Hectares)

# Initial Model ----------------------------------------------------------------
wrybill_mod <- glmer(Number ~ centeredYear + (1 | section_number), offset = logHectare, family = "poisson",
                      data = wrybill)
summary(wrybill_mod)
effect_sizes <- seq(log(0.5), log(2), by = (log(2)/5))
year_range <- 5:20

results <- expand.grid(effect=effect_sizes, years=year_range)
results <- results %>% mutate(perc_change = (exp(effect)-1)*100)

results$power <- NA

# loop year*effects ------------------------------------------------------------
for (i in 1:nrow(results)) {
  ef <- results$effect[i]
  n_years <- results$years[i]

  sim_model <- extend(wrybill_mod, along = "centeredYear", n = n_years)
  
  fixef(sim_model)["centeredYear"] <- ef

  power_out <- try(powerSim(sim_model, fixed("centeredYear", "z"), nsim = 100), silent = TRUE)
  
  if (!inherits(power_out, "try-error")) {
    results$power[i] <- power_out$x
  } else {
    results$power[i] <- NA  
  }
  
  cat("Finished effect =", ef, ", years =", n_years, "\n")
}

#-------------------------------------------------------------------------------
ggplot(results, aes(x = years, y = power, color = as.factor(perc_change), group = perc_change)) +
  geom_line() +
  geom_point() +
  geom_jitter() +
  labs(x = "Number of Years", y = "Power (%)", color = "Percentage Change (%)") +
  theme_minimal()




