
library(tidyverse)

go_data <- bind_rows(list(
  readxl::read_xlsx("data-raw/go_data_modified/GO_term_summary_20190330_173856.xlsx") %>%
    mutate(Process = "Citric Acid Cycle"),
  readxl::read_xlsx("data-raw/go_data_modified/GO_term_summary_20190330_174038.xlsx") %>%
    mutate(Process = "Neuronal Apoptosis"),
  readxl::read_xlsx("data-raw/go_data_modified/GO_term_summary_20190330_174122.xlsx") %>%
    mutate(Process = "Meiotic Cell Cycle"),
  readxl::read_xlsx("data-raw/go_data_modified/GO_term_summary_20190330_174300.xlsx") %>%
    mutate(Process = "Chemokine Secretion"),
  readxl::read_xlsx("data-raw/go_data_modified/GO_term_summary_20190330_174345.xlsx") %>%
    mutate(Process = "Actin dependent Cell Motility"),
  readxl::read_xlsx("data-raw/go_data_modified/GO_term_summary_20190330_174437.xlsx") %>%
    mutate(Process = "Mammalian Oogenesis")
)) %>%
  filter(Symbol != "4933405O20Rik")

# go_data %>%
#   select(Symbol, Process) %>%
#   distinct() %>%
#   count(Symbol) %>%
#   arrange(-n)

gene_pathway_membership <- go_data %>%
  select(Symbol, Process) %>%
  distinct() %>%
  mutate(member = TRUE) %>%
  spread(Symbol, member, fill=FALSE) %>%
  as.data.frame() %>%
  column_to_rownames("Process") %>%
  as.matrix()

usethis::use_data(gene_pathway_membership, overwrite = TRUE)

