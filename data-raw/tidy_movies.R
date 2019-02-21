library(ggplot2movies)
library(tidyverse)

set.seed(1)
samp_movies <- movies %>%
  sample_n(size = 5000)

tidy_movies <- samp_movies %>%
  mutate(id = 1:n()) %>%
  gather(Genre, GenreMember, Action:Short) %>%
  group_by_at(vars(-c(Genre, GenreMember))) %>%
  nest() %>%
  mutate(Genres =  map(data, ~ if(all(.x$GenreMember == 0)){ character(0) }else{ .x$Genre[.x$GenreMember == 1]} )) %>%
  select(-data, -id) %>%
  gather(stars, percent_rating, tidyselect::matches("r\\d{1,2}")) %>%
  mutate(stars = as.numeric(str_sub(stars, start=2)))

usethis::use_data(tidy_movies, overwrite = TRUE)
