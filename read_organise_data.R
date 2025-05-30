library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)

#count data
UR_data <- read_excel("data/RangitataUpperCounts.xlsx", sheet = "BirdCountData")
UR_data$Species <- as.factor(UR_data$Species)
UR_data$Year <- as.numeric(UR_data$Year)
head(UR_data)
UR_data$section_number<- as.factor(UR_data$`section number`)

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
flow_data <- raw_flow %>% reframe(date_time = as.POSIXct(Date, format="%d/%m/%Y %H:%M"), flow = Inst, qual = Qual, Date = Date) %>% 
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

counts_and_flow <- left_join(UR_data, days_since_flood, by = "Date")

ggplot(data = flow_data, aes(x = date, y = flow, colour= day_flooded)) +
  geom_line()


# wide_date_by_spp <- UR_data %>% group_by(Species, Date) %>% reframe(count= Number, Date=as.Date(as.character(Date))) %>%
#   pivot_wider(names_from = Species, values_from = count, values_fn = sum) %>% replace(is.na(.),0)


# BFT_wrybill <- left_join(subset(sum_counts_by_year, Species == "Black-fronted tern" | Species == "Wrybill"),
#                          subset(mean_counts_by_year, Species == "Black-fronted tern" | Species == "Wrybill"))
#                      
# # BFT and wrybill --------------------------------------------------------------
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
# ggplot(data = sum_counts_by_year, aes(x = year, y = sum, group = Species)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~Species, scales = "free_y")
# 
# ggplot(data = subset(by_section, year > 2020), aes(x = year, y = sum, group=section_number, colour = section_number)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~Species, scales = "free_y")






