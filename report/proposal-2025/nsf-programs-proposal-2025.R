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

# filter down the list of metadata docs during the time period
m_q <- objs %>% 
  filter(formatType == "METADATA") %>% 
  filter(!grepl("*.dataone.org/portals|*.dataone.org/collections", formatId)) %>%
  filter(is.na(obsoletedBy)) %>%
  filter(grepl("doi:", id)) %>%
  filter(dateUploaded >= quarter_start_date & dateUploaded <= quarter_end_date)

m_q$funding <- NA

print("get most recent versions")
counter <- 1
for (i in seq_along(m_q$id)){
  print(counter)
  counter <- counter + 1
  doc <- read_eml(getObject(mn, m_q$id[i]))
  print("done getting doc")
  if (!is.null(doc$dataset$project$award)){
    print("award section exists")
    m_q$funding[i] <- paste(arcticdatautils::eml_get_simple(doc, "awardNumber"), collapse = ";")
    if (!is.null(doc$dataset$project$funding)) {
      print("skipping funding section bc already got NSF award")
      next
    }
    next
  } 
  else if (!is.null(doc$dataset$project$funding)) {
    print("only funding section exists")
    nsf_awards <- c()
    for (funding in unlist(doc$dataset$project$funding)) {
      matches <- str_extract_all(funding, "(?<!\\d)\\d{7}(?!\\d)")[[1]] #looking for 7-digit codes in text
      if (length(matches) > 0) {
        nsf_awards <- append(nsf_awards, matches)
      }
    }
    if (length(nsf_awards) > 0) {
      m_q$funding[i] <- paste(nsf_awards, collapse = ";")
    }
  } else {
    print("project section doesn't exist")
  }
}

wd <- getwd()
write.csv(x = m_q, file = paste0(wd, "/report/proposal-2025/output-data/m_q_v2.csv"), row.names = FALSE)

print("DONE WITH FIRST LOOP")
# clean up awards
funding <- m_q %>% 
  select(id, dateUploaded, funding) %>% 
  separate(funding, paste("funding", 1:10, sep="_"), sep=";", extra="drop") %>% 
  pivot_longer(cols = starts_with("funding"), names_to = "h", values_to = "funding") %>% 
  select(-h) %>% 
  filter(!is.na(funding) & funding != "") %>% 
  filter(nchar(funding) == 7)
# extract program names
print("EXTRACTING PROGRAM NAMES")
print("number of rows")
print(nrow(funding))
counter <- 1
for (i in 1:nrow(funding)){
  print(counter)
  print("^ counter")
  counter <- counter + 1
  url <- paste0("https://www.research.gov/awardapi-service/v1/awards.json?id=", funding$funding[i] ,"&printFields=fundProgramName")
  
  t <- fromJSON(url)
  if (!is.null(t$response$award$fundProgramName)){
    funding$programName[i] <- t$response$award$fundProgramName
  }
  else {funding$programName[i] <- "unknown"}
}    

# Writing funding report to CSV
write.csv(x = funding, file = paste0(wd, "/report/proposal-2025/output-data/funding_checkProposal.csv"), row.names = FALSE)

print("FINISHED")
