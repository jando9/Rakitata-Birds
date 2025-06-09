library(lme4)
library(MASS) 
library(ggplot2)

source("read_organise_data.R")

# set up data ------------------------------------------------------------------
subset_of_interest <- list(species = spp_of_interest,
                           start_year = 1986, end_year = 2025)
specified_years <- subset(counts_filled_zeros, Year > subset_of_interest$start_year - 1 &
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

# Try splitting categories by proportion of area -------------------------------
# Calculate proportions
after2021_sections <- subset(section_key, year(Date) > 2021)
full_area <- sum(unique(after2021_sections$Hectares))
sections_to_fix <- c("Part of  1", "Part of  1, part of  2", "2,3,4", "1,2", "2,3", "Part of  1, 2")

proportions <- list(
  1,
  unique(c(one=section_key$Hectares[section_key$section_number=="1"]/
                    section_key$Hectares[section_key$section_number=="Part of  1, part of  2"],
                    two=section_key$Hectares[section_key$section_number=="2"]/
                    section_key$Hectares[section_key$section_number=="Part of  1, part of  2"])),
  unique(c(two=section_key$Hectares[section_key$section_number=="2"]/
                section_key$Hectares[section_key$section_number=="2,3,4"],
                three=section_key$Hectares[section_key$section_number=="3"]/
                section_key$Hectares[section_key$section_number=="2,3,4"],
                four=section_key$Hectares[section_key$section_number=="4"]/
                section_key$Hectares[section_key$section_number=="2,3,4"])),
  unique(c(one=section_key$Hectares[section_key$section_number=="1"]/
               section_key$Hectares[section_key$section_number=="1,2"],
               two=section_key$Hectares[section_key$section_number=="2"]/
               section_key$Hectares[section_key$section_number=="1,2"])),
  unique(c(two=section_key$Hectares[section_key$section_number=="2"]/
                      unique(section_key$Hectares[section_key$section_number=="2,3"]),
                      three=section_key$Hectares[section_key$section_number=="3"]/
                      unique(section_key$Hectares[section_key$section_number=="2,3"]))),
  unique(c(one=section_key$Hectares[section_key$section_number=="1"]/
                        section_key$Hectares[section_key$section_number=="Part of  1, 2"],
                      two=section_key$Hectares[section_key$section_number=="2"]/
                        section_key$Hectares[section_key$section_number=="Part of  1, 2"]))
)
names(proportions) <- sections_to_fix

# multiply numbers by proportions
# need to generalise to run for all species
fixed_sections <- data.frame(NULL)
for (i in 1:length(proportions)) {
  
  fixing_section <- Wrybill %>% subset(section_number == names(proportions[i]))
  fixing_section <- fixing_section[rep(row.names(fixing_section), times = length(proportions[[i]])), ] %>%
    mutate(Number = Number * proportions[[i]],
           section_number = seq(1,length(proportions[[i]]))) %>% 
    mutate(section_number = case_when(names(proportions[i]) == "2,3,4" |
                                      names(proportions[i]) == "2,3" ~ section_number + 1,
                                      names(proportions[i]) == "Part  of 1" ~ 1,
                                      .default = section_number))
  fixed_sections <- rbind(fixed_sections, fixing_section)
}
fixed_sections$section_number <- as.factor(fixed_sections$section_number)
full_data_wrybill <- rbind(Wrybill %>% subset(!section_number %in% names(proportions)), fixed_sections)




Wrybill_old_data <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = log(mean_daily_surveyors),
                     data = Wrybill_old_dat)
summary(Wrybill_old_data)
resid_df <- 37
deviance(Wrybill_old_data)/resid_df
