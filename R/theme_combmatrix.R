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
  combmatrix.panel.point.color.empty = "#E0E0E0"
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
    validate = FALSE
  )
}
