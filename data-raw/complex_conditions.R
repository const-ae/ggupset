library(tidyverse)

set.seed(1)

df_complex_conditions <- tribble(
  ~KO,     ~DrugA,     ~Timepoint,
  TRUE,    "Yes",      8,
  TRUE,    "Yes",      24,
  TRUE,    "Yes",      48,

  TRUE,    "No",       8,
  TRUE,    "No",       24,
  TRUE,    "No",       48,

  FALSE,   "Yes",      8,
  FALSE,   "Yes",      24,
  FALSE,   "Yes",      48,

  FALSE,   "No",       8,
  FALSE,   "No",       24,
  FALSE,   "No",       48
)  %>%
  mutate(response = pmap(., function(KO, DrugA, Timepoint, n=30){
    if(!KO){
      if(DrugA == "Yes"){
        rnorm(n, mean=100 + (Timepoint - 8) * 3, sd=15)
      }else{
        rnorm(n, mean=100 + (Timepoint - 8) * 1.2, sd=15)
      }
    }else{
      rnorm(n, mean=100, sd=25)
    }
  })) %>%
  unnest()


usethis::use_data(df_complex_conditions, overwrite = TRUE)


