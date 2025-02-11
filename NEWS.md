
# Development version (v0.4.1.900x)

# v0.4.1

* Use `calc_element` internally instead of accessing values from the `theme` directly
(thanks to @teunbrand, #44)

# v0.4.0

* Fixed incompatibility with ggplot2 version 3.5.1 which meant that `combmatrix` theme
elements were ignored (#43)
* New parameter `override_plotting_function` for `axis_combmatrix` that gives maximum
flexibility to modify the combination matrix plot in any desired way.
* Fix bug when remobing y-axis (`theme(axis.text.y = element_blank())`)

# v0.3.0

* Remove deprecated `as.tibble()` call (thanks to @abichat)
* Removed lines that remained in pdf output with `combmatrix.panel.line.size = 0`
(issue #12). Thanks to @karldw.

# v0.2.1

* Better error messages (#9)
* Bug fix in scale_x_mergelist() to work with ggplot2 v3.3.0

# v0.2.0

* Fix issues with non-default themes


# v0.1.0 (Initial release)

* Implement combination matrix for x axis
* Provide convenience functions to create UpSet plots and handle
list columns
