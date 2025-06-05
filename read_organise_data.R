library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)

## Reading in, cleaning and wrangling data -------------------------------------
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
  subset(Species %in% spp_of_interest) %>% left_join(section_key, by = c("Date")) %>% 
  rename(Hectares = Hectares.x, section_number = section_number.x)
zeros <- subset(counts_filled_zeros, is.na(section_number)) %>% select(-c(Hectares,section_number)) %>%
  rename(Hectares = Hectares.y, section_number = section_number.y)
counts_filled_zeros <- counts_filled_zeros %>% select(-c(Hectares.y, section_number.y)) %>%
  subset(!is.na(section_number))
filled_counts_noDups <- rbind(counts_filled_zeros,zeros) %>% mutate(Year = year(Date)) %>% distinct()
  
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
            days_since_flood, by = "Date"),
  observers, by = "Year") %>% select(!Date) %>%  distinct() 

flow_and_observers <- apply(flow_and_observers, 2, function(x) gsub("Unknown", NA, x))
flow_and_observers <- as.data.frame(apply(flow_and_observers, 2, as.numeric))
flow_and_observers <- flow_and_observers %>% group_by(Year) %>%
  summarise(mean_days_since_flood = mean(days_since_flood), total_surveyors = mean(`Total People`),
            total_days_surveyed = mean(`Total Days Surveyed`), mean_daily_surveyors = mean(`Mean Daily Surveyors`))

## Visualisation ---------------------------------------------------------------
ggplot(data = flow_data, aes(x = date, y = flow, colour= flooding)) +
  geom_line()


# wide_date_by_spp <- UR_data %>% group_by(Species, Date) %>% reframe(count= Number, Date=as.Date(as.character(Date))) %>%
#   pivot_wider(names_from = Species, values_from = count, values_fn = sum) %>% replace(is.na(.),0)


# BFT_wrybill <- left_join(subset(sum_counts_by_year, Species == "Black-fronted tern" | Species == "Wrybill"),
#                          subset(mean_counts_by_year, Species == "Black-fronted tern" | Species == "Wrybill"))
#                      

# ggplot(data = BFT_wrybill,
#        aes(x = year, y = mean, colour = Species)) +
#   geom_line() +
#   geom_point()
# 
# ggplot(data = BFT_wrybill,
#        aes(x = year, y = sum, groups = Species, colour = Species)) +
#   geom_line() +
#   geom_point()
# 
# # all species ------------------------------------------------------------------
ggplot(data = subset(by_section, Species %in% spp_of_interest), aes(x = year, y = sum, group=section_number, colour = section_number)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Species, scales = "free_y")

# 
# ggplot(data = subset(by_section, year > 2020), aes(x = year, y = sum, group=section_number, colour = section_number)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~Species, scales = "free_y")






