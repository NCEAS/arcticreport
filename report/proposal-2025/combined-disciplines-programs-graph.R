library(dplyr)
library(tidyr)
library(cowplot)
library(ggplot2)

### TODO: Run `adc-disciplines-proposal-2025.R` and `nsf-programs-proposal-2025.R` first to 
### create `combined_disciplines_mapped_proposal.csv` and `funding_checkProposal.csv`

wd <- getwd()
disciplinesTable <- read.csv(paste0(wd, "/report/proposal-2025/output-data/combined_disciplines_mapped_proposal.csv"))
programsTable <- read.csv(paste0(wd, "/report/proposal-2025/output-data/funding_checkProposal.csv"))

source(paste0(wd, "/report/theme_ADC.R"))

disciplinesTable <- disciplinesTable %>% 
  filter(disc != "Academic Discipline") %>% 
  filter(disc != "Engineering") %>%
  filter(disc != "Formal Science") %>%
  filter(disc != "Natural Science") %>%
  filter(disc != "Life Science") %>%
  filter(disc != "Physical Science") %>%
  filter(disc != "Geoscience") %>% 
  filter(disc != "Social Science") %>% 
  filter(n >= 20)

disc_plot <- ggplot(data=disciplinesTable, aes(x=reorder(disc, n), y=n)) +
  geom_col(fill="#1D244F", color="#1D244F", width=0.8) +
  coord_flip() +
  labs(y = "Number of Datasets", x = "Discipline") +
  theme_ADC

#######

rm <- programsTable

aggr_tbl <- rm %>% group_by(programName) %>% summarise(total_count=n(), .groups = "drop") %>% as.data.frame()
aggr_tbl_drop <- subset(aggr_tbl, total_count>=10)

program_plot <- ggplot(data=aggr_tbl_drop, aes(x=reorder(programName, total_count), y=total_count)) +
  geom_col(fill="#1D244F", color="#1D244F", width=0.8) +
  coord_flip() +
  labs(y = "Number of Datasets", x = "Program Name") +
  theme_ADC

plot_grid(disc_plot, program_plot, nrow = 1, labels = c("a)", "b)"), label_size = 8)

ggsave(paste0(wd, "/report/proposal-2025/output-data/combined_disciplines_program_graph.png"), width = 8, height = 5.5)
