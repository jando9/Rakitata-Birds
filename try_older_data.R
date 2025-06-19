library(lme4)
library(MASS) 
library(ggplot2)

source("read_organise_data.R")

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
  with_flow <- inner_join(subset_species, flow_and_observers, by = "Year")
  historic_data[[i]] <- with_flow
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
  assign(paste0("historic_", gsub(" |-", "_", names(historic_data[j]))), full_data)
}

historic_Wrybill <- historic_Wrybill %>%
  mutate(mean_daily_surveyors = case_when(is.na(mean_daily_surveyors) ~ mean(mean_daily_surveyors, na.rm = T),
                                          .default = mean_daily_surveyors))

ggplot(data = historic_Wrybill,
       aes(x = Year, y = Number/mean_daily_surveyors, groups = section_number, colour = section_number)) +
  geom_line() +
  geom_point()

historic_wrybill_mod <- glmer.nb(Number ~ centeredYear + (1 | section_number), offset = log(mean_daily_surveyors),
                     data = historic_Wrybill)
summary(historic_wrybill_mod)
resid_df <- 44
deviance(historic_wrybill_mod)/resid_df


# Visualisation of Model -------------------------------------------------------

historic_wrybill_pred <- expand.grid(centeredYear = seq(min(historic_Wrybill$centeredYear), max(historic_Wrybill$centeredYear)),
                            Year = seq(min(historic_Wrybill$Year), max(historic_Wrybill$Year)),
                            section_number = factor(seq(1,5)))
historic_wrybill_pred <- historic_wrybill_pred %>%
  mutate(Conditional = predict(historic_wrybill_mod, type = "response", newdata = historic_wrybill_pred, re.form = NULL),
         Marginal = predict(historic_wrybill_mod, type = "response", newdata = historic_wrybill_pred, re.form = NA)) %>% 
  pivot_longer(cols = c(Conditional, Marginal), names_to = "type", values_to = "preds")

ggplot(historic_Wrybill, aes(x = Year, y = (Number/(mean_daily_surveyors)))) +
  ylab("Count per Surveyor") + xlab("Year") +
  geom_point(aes(colour = section_number)) +
  geom_line(data = historic_wrybill_pred, aes(x = centeredYear + min(Year), y = preds, linetype = type,
                                              size = type,
                                              color = ifelse(type == "Conditional",
                                                             section_number,
                                                             type))) +
  scale_color_discrete(limits = c("1","2","3","4","5"), na.value = "black") +
  scale_size_manual("type", values = c(0.8,1.5), guide = "none") +
  scale_linetype_manual(values = c(2,1)) +
  labs(colour = "Section Number", linetype = "Model")
