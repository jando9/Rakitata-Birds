library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)
library(afex) #checking convergence issues

source("code/functions.R")


# Reading in, cleaning and wrangling data --------------------------------------
#count data
UR_data <- read_excel("data/RangitataUpperCounts.xlsx", sheet = "BirdCountData")
UR_data$Species <- as.factor(UR_data$Species)
UR_data$Year <- as.numeric(UR_data$Year)
head(UR_data)
UR_data$section_number<- as.factor(UR_data$`section number`)

spp_of_interest <- c("Wrybill","Black-fronted tern", "Banded dotterel", "Black-billed gull",
                     "Black shag", "Caspian tern", "Little shag", "South Island pied oystercatcher",
                     "Southern black-backed gull", "Black Stilt/Kakī", "Shag species",
                     "Hybrid black stilt", "Spur-winged plover", "Swamp harrier", "Pied shag")

section_key <- UR_data %>%  select(Date, section_number, Hectares) %>% distinct()

counts_filled_zeros <- UR_data %>% select(c(Species:Year, Km_code, Hectares:section_number)) %>%
  complete(Species, Date, fill = list(Number = 0)) %>% 
  subset(Species %in% spp_of_interest) %>% left_join(section_key, by = c("Date"),
                                                     relationship = "many-to-many") %>%
  mutate(Number = case_when(section_number.x == section_number.y ~ Number,
                            section_number.x != section_number.y ~ 0),
         Year = year(Date)) %>%
  replace_na(list(Number = 0)) %>% 
  rename(Hectares = Hectares.y, section_number = section_number.y) %>% select(-c(section_number.x, Hectares.x, Km_code)) %>% 
  distinct()

# zeros <- subset(counts_filled_zeros, is.na(section_number)) %>% select(-c(Hectares,section_number)) %>%
#   rename(Hectares = Hectares.y, section_number = section_number.y)
# counts_filled_zeros <- counts_filled_zeros %>% select(-c(Hectares.x, section_number.x)) %>%
#   subset(!is.na(section_number))
# filled_counts_noDups <- rbind(counts_filled_zeros,zeros) %>% mutate(Year = year(Date)) %>% distinct()
  
mean_annual_counts <- UR_data %>% group_by(Species, year = Year) %>% summarize(sum = sum(Number)) %>%
  group_by(Species) %>% summarize(mean_annual = mean(sum))

by_section <- UR_data %>%  subset(Year >2001) %>%
  group_by(Species, year = Year, section_number = `section number`) %>% summarize(sum = sum(Number))
#meta data
meta_data <- read_excel("data/RangitataUpperCounts.xlsx", sheet = "Survey Metadataa")
#flow data
raw_flow <- rbind(read.csv("data/2000.01.01_2015.04.08_flowdata.csv", skip = 2),
                   read.csv("data/2015.04.08_2025.03.27_flowdata.csv", skip = 2))
flow_data <- raw_flow %>% separate(Date, c("date", "time"), sep = " ")
flow_data <- raw_flow %>% reframe(date_time = as.POSIXct(Date, format="%d/%m/%Y %H:%M"),
                                  flow = Inst, qual = Qual, Date = Date) %>% 
  mutate(flooding = case_when(flow >= 250 ~ 1,
                              flow < 250 ~ 0,
                              qual == 255 ~ 0)) %>% 
  separate(Date, c("date", "time"), sep = " ") %>% mutate(Date = as.Date(date, format = "%d/%m/%Y")) %>%
  group_by(Date) %>% 
  mutate(day_flooded = any(flooding == 1)) %>% ungroup() %>% 
  mutate(last_flood_date = if_else(day_flooded == 1, Date, as.Date(NA))) %>%
  fill(last_flood_date, .direction = "down") %>%
  mutate(days_since_flood = as.integer(Date - last_flood_date))

days_since_flood <- flow_data[,c("Date", "days_since_flood")]
observers <- meta_data[,c("Year","Total People", "Total Days Surveyed", "Mean Daily Surveyors")]

flow_and_observers <- left_join(
  left_join(UR_data[,c("Date", "Year")],
            days_since_flood, by = "Date", relationship = "many-to-many"),
  observers, by = "Year") %>% select(!Date) %>%  distinct() 

flow_and_observers <- apply(flow_and_observers, 2, function(x) gsub("Unknown", NA, x))
flow_and_observers <- as.data.frame(apply(flow_and_observers, 2, as.numeric))
flow_and_observers <- flow_and_observers %>% group_by(Year) %>%
  summarise(mean_days_since_flood = mean(days_since_flood), total_surveyors = mean(`Total People`),
            total_days_surveyed = mean(`Total Days Surveyed`), mean_daily_surveyors = mean(`Mean Daily Surveyors`))

# Set up data ------------------------------------------------------------------
subset_of_interest <- list(species = spp_of_interest,
                           start_year = 2021, end_year = 2025)
specified_years <- subset(counts_filled_zeros, Year > subset_of_interest$start_year - 1 &
                            Year < subset_of_interest$end_year + 1) 

for (i in 1:length(subset_of_interest$species)) {
  subset_species <- subset(specified_years, Species == subset_of_interest$species[i]) %>% 
    group_by(section_number, Year, Hectares) %>% summarise(Number = sum(Number))
  subset_species$section_number <- factor(subset_species$section_number)
  subset_species$centeredYear <- subset_species$Year - min(subset_species$Year)
  subset_species$factorYear <- as.factor(subset_species$centeredYear)
  with_flow <- inner_join(subset_species, flow_and_observers, by = "Year")
  if(median(with_flow$Number) >= 5) {
     assign(paste0(gsub(" |-|/", "_", subset_of_interest$species[i])), with_flow)
   }
}

# Visualisation ----------------------------------------------------------------

# sections sampled
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
  geom_tile(colour = "white", linewidth = 1.2) +
  labs(fill = "Old Sections", y = "Current Sections")
  

# all species over time
ggplot(data = subset(by_section, Species %in% spp_of_interest), aes(x = year, y = sum, group=section_number, colour = section_number)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Species, scales = "free_y")

# flow
ggplot(data = flow_data, aes(x = date, y = flow, colour= flooding)) +
  geom_line()






