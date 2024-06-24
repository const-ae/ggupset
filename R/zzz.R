


.onLoad <- function(libname, pkgname){
  ggplot2::register_theme_elements(
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
    combmatrix.panel.line.color = "black",
    combmatrix.panel.point.color.fill = "black",
    combmatrix.panel.point.color.empty = "#E0E0E0",
    element_tree = list(
      combmatrix.label.make_space = ggplot2::el_def("logical"),
      combmatrix.label.width = ggplot2::el_def("unit"),
      combmatrix.label.height =  ggplot2::el_def("unit"),
      combmatrix.label.extra_spacing = ggplot2::el_def("numeric"),
      combmatrix.label.total_extra_spacing =ggplot2::el_def("unit"),
      combmatrix.label.text = ggplot2::el_def("element_text", "axis.text.y"),
      combmatrix.panel.margin = ggplot2::el_def("unit"),
      combmatrix.panel.striped_background = ggplot2::el_def("logical"),
      combmatrix.panel.striped_background.color.one = ggplot2::el_def("character"),
      combmatrix.panel.striped_background.color.two = ggplot2::el_def("character"),
      combmatrix.panel.point.size = ggplot2::el_def("numeric"),
      combmatrix.panel.line.size = ggplot2::el_def("numeric"),
      combmatrix.panel.line.color = ggplot2::el_def("character"),
      combmatrix.panel.point.color.fill = ggplot2::el_def("character"),
      combmatrix.panel.point.color.empty = ggplot2::el_def("character")
    )
  )
}
