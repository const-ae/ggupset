


context("ggcombaxis")

test_that("Plotting works as expected", {
  mtcars$comb <- paste0("gears: ", mtcars$gear, "-cyl: ", mtcars$cyl)
  mtcars$list_col <- purrr::map2(mtcars$gear, mtcars$cyl, ~ c(paste0("gear: ", .x), paste0("cyl: ", .y)))

  expect_silent(
    ggplot(mtcars, aes(x=comb)) +
      geom_bar() +
      axis_combmatrix(sep="-")
  )

  ggplot(mtcars, aes(x=comb)) +
    geom_bar() +
    axis_combmatrix(sep="-")

  ggplot(mtcars, aes(x=comb)) +
    geom_bar() +
    scale_x_discrete(position = "top") +
    axis_combmatrix(sep="-")

  ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_mergelist() +
    axis_combmatrix(sep = "-")

  ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset()

  ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(n_sets = 6)


  ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(intersections = list(c("gear: 3", "cyl: 8"), c("gear: 4", "cyl: 4"), c("cyl: 4", "gear: 3")))


  ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(n_intersections = 5)

  ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(n_intersections = 5, levels=c(paste0("gear: ", c(3,4,5)), paste0("cyl: ", c(4,6,8,9))))

})


# test_that("Test for skipping", {
#   skip("Ignore")
#   library(tidyverse)
#
#   starwars %>%
#     ggplot(aes(x=films)) +
#     geom_bar() +
#     scale_x_upset() +
#     axis_combmatrix(sep = "-_-_-")
#
#   starwars %>%
#     mutate(films_col = map_chr(films, paste0, collapse="-")) %>%
#     ggplot(aes(x=films)) +
#     geom_bar() +
#     axis_combmatrix(sep = "-")
#
#
#   starwars %>%
#     mutate(films_col = map_chr(films, paste0, collapse="-")) %>%
#     ggplot(aes(x=films_col, y=height)) +
#     geom_boxplot() +
#     axis_combmatrix(sep = "-") +
#     # theme(axis.text.x = element_text(angle = 90))
#     NULL
#
# })
#
#
#
# test_that("Test for skipping 2", {
#   skip("Ignore")
#
#   library(ggplot2movies)
#   library(tidyverse)
#   tidy_movies <- movies %>%
#     mutate(id = 1:n()) %>%
#     gather(Genre, GenreMember, Action:Short) %>%
#     filter(GenreMember == 1) %>%
#     select(- GenreMember) %>%
#     group_by_at(vars(-Genre)) %>%
#     summarize(Genres = list(Genre)) %>%
#     ungroup()
#
#
#   tidy_movies %>%
#     ggplot(aes(x=Genres)) +
#       geom_bar() +
#       scale_x_upset(n_intersections = 10)
#
#   tidy_movies %>%
#     sample_n(1000) %>%
#     ggplot(aes(x=Genres)) +
#     geom_bar() +
#     scale_x_upset(n_intersections = 10)
#
#   tidy_movies %>%
#     sample_n(1000) %>%
#     ggplot(aes(x=Genres, y=year)) +
#     # geom_violin() +
#     geom_boxplot() +
#     scale_x_upset()
#
#   tidy_movies %>%
#     # sample_n(1000) %>%
#     mutate(Genres_col = map_chr(Genres, ~ paste0(sort(.x), collapse="-"))) %>%
#     mutate(Genres_col = fct_infreq(as.factor(Genres_col))) %>%
#     mutate(Genres_col = fct_lump(Genres_col, n=12)) %>%
#     ggplot(aes(x=Genres_col, y=year)) +
#       geom_violin() +
#       # geom_jitter(width = 0.1, height=0, size=0.01) +
#       axis_combmatrix(sep="-", levels = c("Drama", "Comedy", "Short", "Documentary", "Action", "Romance"))
#       NULL
#
# })
