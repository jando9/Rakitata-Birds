library(lme4)
library(MASS) 
library(ggplot2)

source("read_organise_data.R")

# set up data ------------------------------------------------------------------
subset_of_interest <- list(species = c("Wrybill","Black-fronted tern", "Banded dotterel"),
                           start_year = 1980, end_year = 2025)
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
       aes(x = Year, y = (Number/Hectares), groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

## Try splitting categories by proportion of area
after2021_sections <- subset(section_key, year(Date) > 2021)
full_area <- sum(unique(after2021_sections$Hectares))
sections_to_fix <- c("Part of 1", "Part of  1, part of  2", "2,3,4", "1,2", "2,3", "Part of  1, 2")

part1 <- 1
part1_part2 <- unique(c(one=section_key$Hectares[section_key$section_number=="1"]/
                  section_key$Hectares[section_key$section_number=="Part of  1, part of  2"],
                  two=section_key$Hectares[section_key$section_number=="2"]/
                  section_key$Hectares[section_key$section_number=="Part of  1, part of  2"]))
sect_234 <- unique(c(two=section_key$Hectares[section_key$section_number=="2"]/
              section_key$Hectares[section_key$section_number=="2,3,4"],
              three=section_key$Hectares[section_key$section_number=="3"]/
              section_key$Hectares[section_key$section_number=="2,3,4"],
              four=section_key$Hectares[section_key$section_number=="4"]/
              section_key$Hectares[section_key$section_number=="2,3,4"]))
sect_12 <- unique(c(one=section_key$Hectares[section_key$section_number=="1"]/
             section_key$Hectares[section_key$section_number=="1,2"],
             two=section_key$Hectares[section_key$section_number=="2"]/
             section_key$Hectares[section_key$section_number=="1,2"]))
sect_23 <- unique(c(two=section_key$Hectares[section_key$section_number=="2"]/
                    unique(section_key$Hectares[section_key$section_number=="2,3"]),
                    three=section_key$Hectares[section_key$section_number=="3"]/
                    unique(section_key$Hectares[section_key$section_number=="2,3"])))
part1_2 <- unique(c(one=section_key$Hectares[section_key$section_number=="1"]/
                      section_key$Hectares[section_key$section_number=="Part of  1, 2"],
                    two=section_key$Hectares[section_key$section_number=="2"]/
                      section_key$Hectares[section_key$section_number=="Part of  1, 2"]))

# need to split sections up and multiply by these proportions above
                                                 



Wrybill_old_data <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = log(mean_daily_surveyors),
                     data = Wrybill_old_dat)
summary(Wrybill_old_data)
resid_df <- 37
deviance(Wrybill_old_data)/resid_df
