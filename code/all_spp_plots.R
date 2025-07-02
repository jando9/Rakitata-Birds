library(ggplot2)
library(dplyr)


# Models -----------------------------------------------------------------------
all_spp_pred <- rbind(Wrybill_pred,
                      BFT_pred,
                      banded_dotterel_pred,
                      SIPO_pred,
                      Spur_winged_plover_pred,
                      SBBG_pred,
                      Black_billed_gull_pred
)

all_spp_data <- rbind(Wrybill,
                      Black_fronted_tern,
                      Banded_dotterel,
                      South_Island_pied_oystercatcher,
                      Spur_winged_plover,
                      Southern_black_backed_gull,
                      Black_billed_gull
)

r2_all_species <- as.data.frame(rbind(r2_Banded_dotterel,
                                      r2_BFT,
                                      r2_Black_billed_gull,
                                      r2_SBBG,
                                      r2_SIPO,
                                      r2_Spur_winged_plover,
                                      r2_Wrybill)
)

  
labels <- data.frame(
  label = c(paste("Marginal", "R^2 =", round(r2_all_species[1,1], 3),
                     "\nConditional", "R^2 =", round(r2_all_species[1,2],3)),
            paste("Marginal", "R^2 =", round(r2_all_species[2,1], 3),
                     "\nConditional", "R^2 =", (round(r2_all_species[2,2],3))),
            paste("Marginal", "R^2 =", round(r2_all_species[3,1], 3),
                     "\nConditional", "R^2 =", round(r2_all_species[3,2],3)),
            paste("Marginal", "R^2 =", round(r2_all_species[4,1], 3),
                     "\nConditional", "R^2 =", round(r2_all_species[4,2],3)),
            paste("Marginal", "R^2 =", round(r2_all_species[5,1], 5),
                     "\nConditional", "R^2 =", round(r2_all_species[5,2],3)),
            paste("Marginal", "R^2 =", round(r2_all_species[6,1], 3),
                     "\nConditional", "R^2 =", round(r2_all_species[6,2],3)),
            paste("Marginal", "R^2 =", round(r2_all_species[7,1], 3),
                     "\nConditional", "R^2 =", round(r2_all_species[7,2],3))
  ),
  label = "Marginal R^2 < 0.0001 \nConditional R^2 < 0.0001",
  species = c(r2_all_species[1,3],
              r2_all_species[2,3],
              r2_all_species[3,3],
              r2_all_species[4,3],
              r2_all_species[5,3],
              r2_all_species[6,3],
              r2_all_species[7,3]),
  x = rep(2023,7),
  y = rep(60,7)
)


full_plot <- ggplot(all_spp_data, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Count per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = all_spp_pred, aes(x = (scaledYear * scaling_attributes$`scaled:scale`[1] + scaling_attributes$`scaled:center`[1]),
                                     y = preds, linetype = type,
                                     size = type,
                                     color = ifelse(type == "Conditional",
                                                    section_number,
                                                    type))) +
  facet_wrap(~species, nrow = 2) +
  geom_text(data = labels,
            mapping = aes(x = x, y = y, label = label),
            size = 3) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.3), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)),
        plot.title = element_text(size = 11))

# power analyses ---------------------------------------------------------------
spp_names <- unique(all_spp_data$species)

for(i in 1:length(power_analyses)){
  power_analyses[[i]] <- mutate(power_analyses[[i]],
                                species = gsub("_observer_only.csv", "", spp_names[i]))
}

all_species_pwr <- bind_rows(power_analyses[-8])

effect_colours <- c("#7b3294", "#ba8bcf",  "grey75", "#89da80", "#008837")
ggplot(all_species_pwr, aes(x = years, y = power, colour = perc_change, group = perc_change)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
  scale_color_gradientn(colours=effect_colours) +
  geom_hline(yintercept=80, linetype='dashed', col = 'grey20') +
  facet_wrap( ~ species, ncol = 2) +
  labs(x = "Years of Data", y = "Power (%)", color = "% Change") +
  theme(plot.title = element_text(size = 11),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = alpha("grey", 0.4)))
