


context("ggcombaxis")

test_that("Plotting works as expected", {
  mtcars$comb <- paste0("gears: ", mtcars$gear, "-cyl: ", mtcars$cyl)
  expect_silent(
    ggplot(mtcars, aes(x=comb)) +
      geom_bar() +
      axis_combmatrix(sep="-")
  )
})


test_that("Test for skipping", {
  skip("Ignore")
  library(tidyverse)

  starwars %>%
    mutate(films_col = map_chr(films, paste0, collapse="-")) %>%
    ggplot(aes(x=films_col)) +
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
