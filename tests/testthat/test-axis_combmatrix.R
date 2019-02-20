


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


test_that("Test for skipping", {
  skip("Ignore")
  library(tidyverse)

  starwars %>%
    ggplot(aes(x=films)) +
    geom_bar() +
    scale_x_upset() +
    axis_combmatrix(sep = "-_-_-")

  starwars %>%
    mutate(films_col = map_chr(films, paste0, collapse="-")) %>%
    ggplot(aes(x=films)) +
    geom_bar() +
    axis_combmatrix(sep = "-")


  starwars %>%
    mutate(films_col = map_chr(films, paste0, collapse="-")) %>%
    ggplot(aes(x=films_col, y=height)) +
    geom_boxplot() +
    axis_combmatrix(sep = "-") +
    # theme(axis.text.x = element_text(angle = 90))
    NULL

})
