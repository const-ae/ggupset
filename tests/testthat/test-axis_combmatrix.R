


context("ggupset")

test_that("Plotting works as expected", {
  library(ggplot2)
  mtcars$comb <- paste0("gears: ", mtcars$gear, "-cyl: ", mtcars$cyl)
  mtcars$list_col <- lapply(seq_len(nrow(mtcars)), function(idx) c(paste0("gear: ", mtcars$gear[idx]), paste0("cyl: ", mtcars$cyl[idx])))

  p <- ggplot(mtcars, aes(x=comb)) +
    geom_bar() +
    axis_combmatrix(sep="-")
  expect_silent(ggplot_build(p))


  p <- ggplot(mtcars, aes(x=comb)) +
    geom_bar() +
    axis_combmatrix(sep="-")
  expect_silent(ggplot_build(p))

  p <- ggplot(mtcars, aes(x=comb)) +
    geom_bar() +
    scale_x_discrete(position = "top") +
    axis_combmatrix(sep="-")
  expect_silent(ggplot_build(p))

  p <- ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_mergelist() +
    axis_combmatrix(sep = "-")
  expect_silent(ggplot_build(p))

  p <- ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset()
  expect_silent(ggplot_build(p))

  p <- ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(n_sets = 6)
  expect_silent(ggplot_build(p))


  p <- ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(intersections = list(c("gear: 3", "cyl: 8"), c("gear: 4", "cyl: 4"), c("cyl: 4", "gear: 3")))
  expect_warning(ggplot_build(p))


  p <- ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(n_intersections = 5)
  expect_warning(ggplot_build(p))

  p <- ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(n_intersections = 5, sets=c(paste0("gear: ", c(3,4,5)), paste0("cyl: ", c(4,6,8,9))))
  expect_warning(ggplot_build(p))

  p <- ggplot(mtcars, aes(x=list_col)) +
    geom_bar() +
    scale_x_upset(n_intersections = 5) +
    theme_combmatrix(combmatrix.panel.line.size = 0)
  expect_warning(ggplot_build(p))
})



test_that("Plotting with tidy_movies works as expected 2", {
  p <- ggplot(tidy_movies[1:100, ], aes(x=Genres)) +
    geom_bar() +
    scale_x_upset(n_intersections = 10)
  expect_warning(ggplot_build(p))

  p <- ggplot(tidy_movies[1:100, ], aes(x=Genres)) +
    geom_bar() +
    scale_x_upset(n_intersections = 10)
  expect_warning(ggplot_build(p))

  p <- ggplot(tidy_movies[1:100, ], aes(x=Genres, y=year)) +
    geom_violin(drop=FALSE) +
    scale_x_upset()
  expect_warning(ggplot_build(p))

  p <- ggplot(tidy_movies[1:100, ], aes(x=sapply(Genres, paste0, collapse="-"), y=year)) +
    geom_boxplot() +
    geom_jitter(width = 0.1, height=0, size=1) +
    axis_combmatrix(sep="-", levels = c("Drama", "Comedy", "Short", "Documentary", "Action", "Romance"))
  expect_silent(ggplot_build(p))
})
