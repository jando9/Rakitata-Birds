library(lme4)
library(MASS) 
library(ggplot2)

source("code/read_organise_data.R")

# set up data ------------------------------------------------------------------
subset_of_interest <- list(species = spp_of_interest,
                           start_year = 1986, end_year = 2025)
specified_years <- subset(counts_filled_zeros, Year > subset_of_interest$start_year - 1 &
                            Year < subset_of_interest$end_year + 1)

historic_data <- vector("list", length = length(spp_of_interest))
for (i in 1:length(subset_of_interest$species)) {
  subset_species <- subset(specified_years, Species == subset_of_interest$species[i]) %>%
    group_by(section_number, Year, Hectares) %>% summarise(Number = sum(Number))
  subset_species$section_number <- factor(subset_species$section_number)
  subset_species$centeredYear <- subset_species$Year - min(subset_species$Year)
  subset_species$factorYear <- as.factor(subset_species$centeredYear)
  with_flow_hist <- inner_join(subset_species, flow_and_observers, by = "Year")
  historic_data[[i]] <- with_flow_hist
}
names(historic_data) <- spp_of_interest

ggplot(data = historic_data$Wrybill,
       aes(x = Year, y = Number/mean_daily_surveyors, groups = section_number, colour = section_number)) +
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
observer_na_filler <- mean(flow_and_observers$mean_daily_surveyors, na.rm = T)
# multiply numbers by proportions
for (j in 1:length(spp_of_interest)) {
  fixed_sections <- data.frame(NULL)
  for (i in 1:length(proportions)) {
    fixing_section <- historic_data[[j]] %>% subset(section_number == names(proportions[i]) &
                                                      section_number != "Part of  1")
    fixing_section <- fixing_section[rep(row.names(fixing_section), times = length(proportions[[i]])), ] %>%
      mutate(Number = round(Number * proportions[[i]]),
             section_number = seq(1,length(proportions[[i]]))) %>% 
      mutate(section_number = case_when(names(proportions[i]) == "2,3,4" |
                                        names(proportions[i]) == "2,3" ~ section_number + 1,
                                        .default = section_number))
    fixed_sections <- rbind(fixed_sections, fixing_section)
  }
  fixed_sections$section_number <- as.factor(fixed_sections$section_number)
  full_data <- rbind(historic_data[[j]] %>% subset(!section_number %in% names(proportions)), fixed_sections) %>% 
    select(-Hectares) %>% left_join(distinct(section_key[,c("section_number", "Hectares")]), by = "section_number") %>% 
    mutate(mean_daily_surveyors = case_when(is.na(mean_daily_surveyors) ~ observer_na_filler,
                                            .default = mean_daily_surveyors))
  if(median(full_data$Number) >= 5){
    assign(paste0("historic_", gsub(" |-", "_", names(historic_data[j]))), full_data)
  }
}
