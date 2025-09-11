# This script creates plots based on file sizes, including total repo size

# Load libraries and set theme
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Load pre-defined quarters and SOLR query
source("~/arctic-data/reporting/R/query_objects.R")
source("~/arctic-data/reporting/R/theme_ADC.R")

adc_objects <- query_objects() %>%
  mutate(source = "actual")

options(scipen=999)

# Get current large data sizes, must be run on datateam
large_file_cmd <- paste0("cd /var/data/10.18739 && find . -maxdepth 1 -type d | grep A2 | xargs -n 1 basename | xargs -I @ -n 1 sh -c \"stat --printf='%n,%Y,' @ && getfattr --only-values -n ceph.dir.rbytes @ && echo ''\" > ", getwd(), "/adc-large-data.csv")
processx::run("bash", c("-c", large_file_cmd))
large_data <- readr::read_csv("adc-large-data.csv", col_names = c("id","dateUploaded","size")) %>%
  mutate(source = "actual",
         dateUploaded = as.POSIXct(dateUploaded),
         formatType = "DATA",
         size = size*1) %>% 
  arrange(dateUploaded)

adc_objects <- bind_rows(adc_objects, large_data)

# add in projections for 2020-2021
projected <- tribble(~dateUploaded, ~size, # size in bytes
                     # "2020-11-01", 27e12, # mosaic
                     # "2021-09-01", 400e12, # drones
                     #"2021-09-01", 20e12, # model output
                     # "2020-09-01", 25e12, # reusch 
                     # "2020-08-01", 7e12,# IARC
                     # "2020-08-01", 0.008e12, #AIS
                     "2025-08-25", 5,
                     "2026-01-30", 3e13,
                     "2026-02-27", 2.25e14) %>%  
  mutate(source = "actual",
         dateUploaded = as.POSIXct(dateUploaded),
         formatType = "DATA") 

adc_objects <- bind_rows(projected, adc_objects)


adc_sizes <- adc_objects %>% 
  filter(is.na(obsoletedBy)) %>% 
  mutate(dateUploaded = as.Date(dateUploaded),
         size_kb = as.numeric(size)/1024) %>% 
  group_by(dateUploaded, formatType, source) %>% 
  summarise(size_kb = sum(as.numeric(size_kb))) %>% 
  ungroup() %>% 
  arrange(dateUploaded) %>% 
  mutate(cumsize = cumsum(size_kb))

last_row <- tail(adc_sizes, 2)
# Delete the last row
last_row <- last_row[-nrow(last_row), ]

compounded_sizes <- data.frame(
  dateUploaded = seq(from = last_row$dateUploaded + years(1),
                     by = "1 year",
                     length.out = 5),
  cumsize = last_row$cumsize * (1 + 0.25)^(1:5)
)
compounded_sizes <- compounded_sizes %>% 
  mutate(source = "compounded",
         formatType = "DATA")

compounded_sizes <- bind_rows(compounded_sizes, tibble(dateUploaded = last_row$dateUploaded, 
                                                       formatType = last_row$formatType,
                                                       source = "compounded",
                                                       size_kb = last_row$size_kb,
                                                       cumsize = last_row$cumsize))
compounded_binded <- bind_rows(adc_sizes, compounded_sizes)
# compounded_binded <- compounded_binded[-nrow(compounded_binded), ]


# Plot total repository size over time
ggplot(compounded_binded, aes(x = dateUploaded,
                              y = cumsize/1e9,
                              linetype = source)) +
  scale_linetype_manual(values = c("actual" = "solid", "projected" = "22", "compounded" = "11")) +
  geom_line(size = 1.1, color="#1D244F") +
  geom_vline(xintercept = as.numeric(as.Date(ymd("20160405", tz = "America/Los_Angeles"))), color = "#146660") +
  annotate(geom = "text",
           x =  as.Date(ymd("20160405", tz = "America/Los_Angeles")),
           y = 0,#min(adc_sizes$cumsize) + 5.5,
           angle = 90,
           hjust = -0.15,#-0.075,
           vjust = 1.9,
           label = "ADC Launch (April 5, 2016)",
           color = "#146660",
           size = 3) +
  scale_x_date(breaks = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01", "2016-01-01", "2018-01-01", "2020-01-01", "2022-01-01", "2024-01-01", "2026-01-01", "2028-01-01", "2030-01-01", "2032-01-01")),
               labels = c("2010", "2012", "2014", "2016", "2018", "2020", "2022", "2024", "2026", "2028", "2030", "2032")) +
  labs(x = "",
       y = "Repository Size (TB)") +
  # ggtitle("Cumulative Repository Size, ACADIS and Arctic Data Center") +
  theme_ADC +
  theme(legend.title = element_blank(),
        legend.position = "none")

wd <- getwd()
ggsave(paste0(wd, "report/proposal-2025/output-data/repo-sizes-projected-compounded-proposal-2025.png"), width = 3, height = 3)
