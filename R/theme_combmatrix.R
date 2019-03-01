#' Theme for the combination matrix
#'
#' This theme sets the default styling for the combination matrix axis
#' by extending the default ggplot2 \code{theme()}.
#'
#' @param combmatrix.label.make_space Boolean indicator if the y-axis label
#'   is moved so far to the left to make enough space for the combination
#'   matrix labels. Default: TRUE
#' @param combmatrix.label.width A unit that specifies how much space to make
#'   for the labels of the combination matrix. Default: NULL, which means the
#'   width of the label text is used
#' @param combmatrix.label.height A unit that specifies how high the combination
#'   matrix should be. Default: NULL, which means that the height of the label
#'   text + \code{combmatrix.label.total_extra_spacing} + #rows *
#'   \code{combmatrix.label.extra_spacing} is used. Default: 3
#' @param combmatrix.label.extra_spacing A single number for the additional
#'   height per row. Default: \code{unit(10, "pt")}
#' @param combmatrix.label.total_extra_spacing A unit that specifies the total
#'   offset for the height of the combination matrix
#' @param combmatrix.label.text A \code{element_text()} to style the label
#'   text of the combination matrix. Default NULL, which means the style of
#'   \code{axis.text.y} is used.
#' @param combmatrix.panel.margin A two element unit vector to specify top
#'   and bottom margin around the combination matrix. Default:
#'   \code{unit(c(1.5, 1.5), "pt")}
#' @param combmatrix.panel.striped_background Boolean to indicate if the
#'   background of the plot is striped. Default: TRUE
#' @param combmatrix.panel.striped_background.color.one Color of the first
#'   kind of stripes. Default: "white"
#' @param combmatrix.panel.striped_background.color.two Color of the second
#'   kind of stripes. Default: "#F7F7F7"
#' @param combmatrix.panel.point.size Number to specify the size of the points
#'   in the combination matrix. Default: 3
#' @param combmatrix.panel.line.size Number to specify the size of the lines
#'   connecting the points. Default: 1.2
#' @param combmatrix.panel.point.color.fill Color of the filled points.
#'   Default: "black"
#' @param combmatrix.panel.point.color.empty Color of the empty points.
#'   Default: "#E0E0E0"
#' @param ... additional arguments that are passed to \code{theme()}
#'
#'
#' @examples
#' library(ggplot2)
#' # Ensure that the y-axis label is next to the axis by setting
#' # combmatrix.label.make_space to FALSE
#' ggplot(tidy_movies[1:100, ], aes(x=Genres)) +
#'   geom_bar() +
#'   scale_x_upset() +
#'   theme_combmatrix(combmatrix.label.text = element_text(color = "black", size=15),
#'                    combmatrix.label.make_space = FALSE,
#'                    plot.margin = unit(c(1.5, 1.5, 1.5, 65), "pt"))
#'
#'  # Change the color of the background stripes
#'  ggplot(tidy_movies[1:100, ], aes(x=Genres)) +
#'    geom_bar() +
#'    scale_x_upset() +
#'    theme_combmatrix(combmatrix.panel.striped_background = TRUE,
#'                     combmatrix.panel.striped_background.color.one = "grey")
#'
#' @export
theme_combmatrix <- function(
  combmatrix.label.make_space = TRUE,
  combmatrix.label.width = NULL,
  combmatrix.label.height = NULL,
  combmatrix.label.extra_spacing = 3,
  combmatrix.label.total_extra_spacing = unit(10, "pt"),
  combmatrix.label.text = NULL,
  combmatrix.panel.margin = unit(c(1.5, 1.5), "pt"),
  combmatrix.panel.striped_background = TRUE,
  combmatrix.panel.striped_background.color.one = "white",
  combmatrix.panel.striped_background.color.two = "#F7F7F7",
  combmatrix.panel.point.size = 3,
  combmatrix.panel.line.size = 1.2,
  combmatrix.panel.point.color.fill = "black",
  combmatrix.panel.point.color.empty = "#E0E0E0",
  ...
){
  theme(
    combmatrix.label.make_space = combmatrix.label.make_space,
    combmatrix.label.width = combmatrix.label.width,
    combmatrix.label.height = combmatrix.label.height,
    combmatrix.label.extra_spacing = combmatrix.label.extra_spacing,
    combmatrix.label.total_extra_spacing = combmatrix.label.total_extra_spacing,
    combmatrix.label.text = combmatrix.label.text,
    combmatrix.panel.margin = combmatrix.panel.margin,
    combmatrix.panel.striped_background = combmatrix.panel.striped_background,
    combmatrix.panel.striped_background.color.one = combmatrix.panel.striped_background.color.one,
    combmatrix.panel.striped_background.color.two = combmatrix.panel.striped_background.color.two,
    combmatrix.panel.point.size = combmatrix.panel.point.size,
    combmatrix.panel.line.size = combmatrix.panel.line.size,
    combmatrix.panel.point.color.fill = combmatrix.panel.point.color.fill,
    combmatrix.panel.point.color.empty = combmatrix.panel.point.color.empty,
    ...,
    validate = FALSE
  )
}
