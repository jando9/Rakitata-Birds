library(lme4)
library(MASS) 
library(ggplot2)

source("initial_look.R")

# set up data ------------------------------------------------------------------
subset_of_interest <- list(species = c("Wrybill","Black-fronted tern"), start_year = 0, end_year = 2025)
specified_years <- subset(UR_data, Year > subset_of_interest$start_year - 1 &
                            Year < subset_of_interest$end_year + 1)

for (i in 1:length(subset_of_interest$species)) {
  subset_species <- subset(specified_years, Species == subset_of_interest$species[i]) %>%
    group_by(section_number, Year, Hectares) %>% summarise(Number = sum(Number))
  subset_species$section_number <- factor(subset_species$section_number)
  subset_species$centeredYear <- subset_species$Year - min(subset_species$Year)
  subset_species$logHectare <- log(subset_species$Hectares)
  subset_species$factorYear <- as.factor(subset_species$centeredYear)
  assign(paste0(gsub(" |-", "_", subset_of_interest$species[i]),"_old_dat"), subset_species)
}

ggplot(data = Wrybill,
       aes(x = Year, y = (Number/Hectares), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

Wrybill_old_data <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = logHectare,
                     data = Wrybill_old_dat)
summary(Wrybill_old_data)
resid_df <- 37
deviance(Wrybill_old_data)/resid_df
