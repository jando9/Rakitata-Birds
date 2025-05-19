library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)

UR_data <- read_excel("Data/RangitataUpperData.xlsx", sheet = "BirdCountData")
meta_data <- read_excel("Data/RangitataUpperData.xlsx", sheet = "Survey Metadataa")
UR_data$Species <- as.factor(UR_data$Species)
UR_data$Year <- as.numeric(UR_data$Year)
head(UR_data)
UR_data$section_number<- as.factor(UR_data$`section number`)

by_section <- UR_data %>%  subset(Year >2001) %>%
  group_by(Species, year = Year, section_number = `section number`) %>% summarize(sum = sum(Number))

# wide_date_by_spp <- UR_data %>% group_by(Species, Date) %>% reframe(count= Number, Date=as.Date(as.character(Date))) %>% 
#   pivot_wider(names_from = Species, values_from = count, values_fn = sum) %>% replace(is.na(.),0)

mean_counts_by_year <- UR_data %>% group_by(Species, year = year(Date)) %>% reframe(mean = mean(Number)) 

sum_counts_by_year <- UR_data %>% group_by(Species, year = Year) %>% summarize(sum = sum(Number))
BFT_wrybill <- left_join(subset(sum_counts_by_year, Species == "Black-fronted tern" | Species == "Wrybill"),
                         subset(mean_counts_by_year, Species == "Black-fronted tern" | Species == "Wrybill"))
                     
# BFT and wrybill --------------------------------------------------------------
ggplot(data = BFT_wrybill,
       aes(x = year, y = mean, colour = Species)) +
  geom_line() +
  geom_point()

ggplot(data = BFT_wrybill,
       aes(x = year, y = sum, groups = Species, colour = Species)) +
  geom_line() +
  geom_point()

# all species ------------------------------------------------------------------
ggplot(data = sum_counts_by_year, aes(x = year, y = sum, group = Species)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Species, scales = "free_y")

ggplot(data = subset(by_section, year > 2020), aes(x = year, y = sum, group=section_number, colour = section_number)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Species, scales = "free_y")






