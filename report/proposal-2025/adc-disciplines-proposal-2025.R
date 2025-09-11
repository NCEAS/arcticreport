devtools::load_all()
library(EML)
library(dataone)
library(readr)
library(purrr)
library(DT)
library(dplyr)
library(tidyr)
library(jsonlite)
library(rt)
library(lubridate)
library(stringr)
library(data.table)
library(ggplot2)

### TODO: SET TOKEN
token = Sys.getenv("TOKEN")
options(dataone_token = token)

### TODO: Change date ranges
quarter_start_date <- as.Date("2000-01-01")
quarter_end_date <- as.Date("2025-07-31")

objs <- query_objects(cache_tolerance = 1000000)

# Get list of filesystem data package sizes and counts
# Runs a system bash command, so assumes running on `datateam`
datasets_add <- query_filesys_objects()

# Adds rows of data to objs table
objs <- bind_rows(objs, datasets_add)

mn <- getMNode(CNode("PROD"), "urn:node:ARCTIC")

print("Done getting member node")
# filter down the list of metadata docs during the time period
m_q <- objs %>% 
  filter(formatType == "METADATA") %>% 
  filter(!grepl("*.dataone.org/portals|*.dataone.org/collections", formatId)) %>%
  filter(is.na(obsoletedBy)) %>%
  filter(grepl("doi:", id)) %>%
  filter(dateUploaded >= quarter_start_date & dateUploaded <= quarter_end_date)

res <- list()
for (i in 1:nrow(m_q)){
  q <- dataone::query(mn, list(q = paste0('id:"', m_q$id[i], '"'),
                               fl = 'id,sem_annotation',
                               sort = 'dateUploaded+desc',
                               rows = 1000),
                      as = "data.frame") 
  
  if (nrow(q) > 0){
    # q <- q %>% 
    #   rename(latest = id)
  } else {
    q <- data.frame(id = m_q$id[i], sem_annotation = NA)
  }
  
  
  res[[i]] <- left_join(q, m_q[i, ])
  
}

res <- do.call(bind_rows, res) 

adc_disc <- read.csv("https://raw.githubusercontent.com/NCEAS/adc-disciplines/main/adc-disciplines.csv") %>% 
  mutate(an_uri = paste0("https://purl.dataone.org/odo/ADCAD_", stringr::str_pad(id, 5, "left", pad = "0")))

res$category <- map(res$sem_annotation, function(x){
  t <- grep("*ADCAD*", x, value = TRUE)
  cats <- c()
  for (i in 1:length(t)){
    z <- which(adc_disc$an_uri == t[i])
    cats[i] <- adc_disc$discipline[z]
    
  }
  return(cats)
})

res_summ <- res %>% 
  unnest_wider(category, names_sep = "") %>% 
  select(-sem_annotation) %>% 
  pivot_longer(cols = starts_with("category"), names_to = "cat", values_to = "disc") %>% 
  filter(!is.na(disc)) %>% 
  group_by(disc) %>% 
  summarise(n = n())


res1 <- res_summ %>% 
  arrange(disc)

print("Writing dataset categorization counts to CSV")
wd <- getwd()
write.csv(x = res1, file = paste0(wd, "/report/proposal-2025/output-data/disciplinesProposal.csv"), row.names = FALSE)
csv_data <- read_csv(paste0(wd, "/report/proposal-2025/output-data/disciplinesProposal.csv"))

################# Adding old categorizations
old_disciplines_df <- read_csv("/home/jeakadi/arcticreport/report/proposal-2025/input-data/dataset_categorization_old.csv")
disciplines_map_df <- read_csv("/home/jeakadi/arcticreport/report/proposal-2025/input-data/disciplinesMAP.csv")

old <- old_disciplines_df %>% 
  select(id, dateUploaded, theme1, theme2, theme3, theme4, theme5) %>% 
  pivot_longer(cols=starts_with("theme"), names_to = "h", values_to = "disc") %>% 
  select(-h) %>% 
  filter(!is.na(disc) & disc != "")

new_table <- old %>% 
  left_join(disciplines_map_df, join_by(disc == old_discipline)) %>% 
  select(-disc) %>% 
  filter(!is.na(new_discipline)) %>% 
  group_by(new_discipline) %>% 
  summarise(n = n())

combined_df <- csv_data %>% 
  full_join(new_table, join_by(disc == new_discipline)) %>% 
  mutate(n = coalesce(n.x, 0) + coalesce(n.y, 0)) %>% 
  select(disc, n)

write.csv(x = combined_df, file = paste0(wd, "/report/proposal-2025/output-data/combined_disciplines_mapped_proposal.csv"))

# FINISHED
# creates combined data with currennt dataset annotations and previous
