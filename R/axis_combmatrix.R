

#' @import ggplot2
NULL

#' @import grid
NULL

#' @import gtable
NULL

#' @import rlang
NULL

#' @import tibble
NULL

#' @import scales
NULL



#' Convert delimited text labels into a combination matrix axis
#'
#' The function splits the text based on the \code{sep} argument and
#' views each occurring element as potential set.
#'
#' Technically the function appends a \code{coord} system to the ggplot object.
#' To maintain compatibility additional arguments like \code{ytrans},
#' \code{ylim}, and \code{clip} are forwarded to \code{coord_trans()}.
#'
#' \emph{Note:} make sure that the argument to the 'x' aesthetic is
#'   character vector that contains the \code{sep} sequence. The only
#'   exception is if \code{axis_combmatrix()} is combined with a
#'   \code{scale_x_mergelist()}. This pattern works because in the
#'   first step \code{scale_x_mergelist()} turns a list argument
#'   to 'x' into a character vector that \code{axis_combmatrix()}
#'   can work with.
#'
#' @param sep The separator that is used to split the string labels. Can be a
#'   regex. Default: \code{"[^[:alnum:]]+"}
#' @param levels The selection of string elements that are displayed in the
#'   combination matrix axis. Default: NULL, which means simply all elements
#'   in the text labels are used
#' @param override_plotting_function to achieve maximum flexibility, you can
#'   provide a custom plotting function. For more information, see details.
#'   Default: \code{NULL}
#' @param xlim,ylim The limits fort the x and y axes
#' @param expand Boolean with the same effect as in
#'   \code{ggplot2::coord_cartesian()}. Default: TRUE
#' @param clip String with the same effect as in
#'   \code{ggplot2::coord_cartesian()}. Default: "on"
#' @param ytrans transformers for y axis. For more information see
#'   \code{ggplot2::coord_trans()}. Default: "identity"
#'
#' @details
#'  For maximum flexibility, you can use the `override_plotting_function` parameter
#'  which returns a ggplot and is called with a \code{tibble}
#'  with one entry per point of the combination matrix. Specifically, it contains
#'   \describe{
#'     \item{labels}{the collapsed label string}
#'     \item{single_label}{an ordered factor with the labels on the left of the plot}
#'     \item{id}{consecutive numbering of the points}
#'     \item{labels_split}{a list column that contains the splitted labels}
#'     \item{at}{the x-position of the point}
#'     \item{observed}{boolean to indicate if this element is active in the intersection}
#'     \item{index}{the row of the point}
#'   }
#' See the examples how the \code{override_plotting_function} looks that recreates
#' the default combination matrix
#'
#' @examples
#'   library(ggplot2)
#'   mtcars$combined <- paste0("Cyl: ", mtcars$cyl, "_Gears: ", mtcars$gear)
#'   head(mtcars)
#'   ggplot(mtcars, aes(x=combined)) +
#'     geom_bar() +
#'     axis_combmatrix(sep = "_")
#'
#' # Example of 'override_plotting_function'
#'
#' ggplot(mtcars, aes(x=combined)) +
#'   geom_bar() +
#'     axis_combmatrix(sep = "_", override_plotting_function = function(df){
#'       ggplot(df, aes(x= at, y= single_label)) +
#'         geom_rect(aes(fill= index %% 2 == 0), ymin=df$index-0.5,
#'                   ymax=df$index+0.5, xmin=0, xmax=1) +
#'         geom_point(aes(color= observed), size = 3) +
#'         geom_line(data= function(dat) dat[dat$observed, ,drop=FALSE],
#'                   aes(group = labels), size= 1.2) +
#'         ylab("") + xlab("") +
#'         scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
#'         scale_fill_manual(values= c(`TRUE` = "white", `FALSE` = "#F7F7F7")) +
#'         scale_color_manual(values= c(`TRUE` = "black", `FALSE` = "#E0E0E0")) +
#'         guides(color="none", fill="none") +
#'         theme(
#'           panel.background = element_blank(),
#'           axis.text.x = element_blank(),
#'           axis.ticks.y = element_blank(),
#'           axis.ticks.length = unit(0, "pt"),
#'           axis.title.y = element_blank(),
#'           axis.title.x = element_blank(),
#'           axis.line = element_blank(),
#'           panel.border = element_blank()
#'         )
#'     })
#'
#' @export
axis_combmatrix <- function(sep="[^[:alnum:]]+", levels=NULL, override_plotting_function = NULL,
                            xlim = NULL, ylim = NULL, expand = TRUE, clip = "on",
                            ytrans="identity") {
  # Copied from coord-transform.R
  if (is.character(ytrans)) ytrans <- as.trans(ytrans)

  res <- ggproto(NULL, CoordCombMatrix,
                 trans = list(x=as.trans("identity"), y=ytrans),
                 limits = list(x = xlim, y = ylim),
                 expand = expand,
                 clip = clip,
                 sep = sep,
                 levels = levels,
                 override_plotting_function = override_plotting_function
  )

  list(res, theme_combmatrix())
}










CoordCombMatrix <- ggproto("CoordCombMatrix", CoordTrans,

  setup_params = function(self, data){
    if(is.function(self$levels)){
      self$levels <- self$levels(data)
    }
    ggproto_parent(CoordTrans, self)$setup_params(data)
  },

  setup_panel_params = function(self, scales_x, scales_y, params=list()){
    if(inherits(scales_x, "ScaleUpset")){
      if(is.null(self$levels)){
        self$levels <- factor(scales_x$sets, levels=rev(levels(scales_x$sets)), ordered=TRUE)
      }else{
        self$levels <- factor(union(self$levels, as.character(scales_x$sets)),
               levels=c(levels(scales_x$sets), setdiff(self$levels, as.character(scales_x$sets))),
               ordered = TRUE)
      }

      self$sep <- scales_x$internal_text_separator
    }
    details <- ggproto_parent(CoordTrans, self)$setup_panel_params(scales_x, scales_y, params)
    details <- c(details, list(
      x.arrange = scales_x$axis_order(),  y.arrange = scales_y$axis_order()
    ))
    details
  },


  render_axis_h = function(self, panel_params, theme) {
    arrange <- panel_params$x.arrange %||% c("secondary", "primary")
    list(
      top = render_comb_axis(self,  panel_params, arrange[1], "top", theme),
      bottom = render_comb_axis(self, panel_params, arrange[2], "bottom", theme)
    )
  },

  render_axis_v = function(self, panel_params, theme) {
    vert_axes <- ggproto_parent(CoordTrans, self)$render_axis_v(panel_params, theme)
    if(isTRUE(theme$combmatrix.label.make_space)){
      if(inherits(vert_axes[["left"]], "zeroGrob")){
        # The axis is on the right so I will just create a new axis
        vert_axes[["left"]] <- absoluteGrob(gList(vert_axes[["left"]]), width=self$comb_axis_label_width)
      }else{
        vert_axes[["left"]]$width <- max(vert_axes[["left"]]$width, self$comb_axis_label_width)
      }
    }
    vert_axes
  },


  # Parameter used to communicate width between
  # horizontal and vertical axis render
  comb_axis_label_width = unit(0, "pt")
)



render_comb_axis <- function(self, panel_params, axis=c("primary", "secondary"),
                             position=c("bottom", "top"), theme){
  # This block is originally copied from the guide_axis function
  at <- unit(panel_params[["x.major"]], "native")

  # Easy way out
  if (length(at) == 0){
    return(zeroGrob())
  }

  position <- match.arg(position, c("top", "bottom"))
  axis <- match.arg(axis, c("primary", "secondary"))
  if(axis == "secondary"){
    # Secondary axis not yet implemented
    return(zeroGrob())
  }

  zero <- unit(0, "npc")
  one <- unit(1, "npc")
  line <- switch(position,
     top =    element_render(theme, "axis.line.x.top", c(0, 1), c(0, 0), id.lengths = 2),
     bottom = element_render(theme, "axis.line.x.bottom", c(0, 1), c(1, 1), id.lengths = 2)
  )

  nticks <- length(at)
  ticks_length <- calc_element(paste0("axis.ticks.length.x.", position), theme)
  ticks <- switch(position,
     top = element_render(theme, "axis.ticks.x.top", x = rep(at, each = 2),
                          y = rep(unit.c(zero, ticks_length), nticks),
                          id.lengths = rep(2, nticks)),
     bottom = element_render(theme, "axis.ticks.x.bottom", x = rep(at, each = 2),
                             y = rep(unit.c(one - ticks_length, one), nticks),
                             id.lengths = rep(2, nticks))
  )

  labels <- factor(panel_params$x.labels, levels = panel_params$x.labels, ordered=TRUE)
  labels_split <- strsplit(panel_params$x.labels, self$sep)
  if(!is.null(self$levels)){
    label_set_levels <- rev(self$levels)
  }else{
    label_set_levels <- sort(unique(unlist(labels_split)), decreasing = TRUE)
  }
  label_set <- factor(label_set_levels, levels=label_set_levels, ordered=TRUE)
  # Add missing values to theme
  default <- theme_combmatrix()
  for(item in setdiff(names(default), names(theme))){
    theme[[item]] <- default[[item]]
  }
  ggpl <- make_combination_matrix_plot(labels=labels,
                                labels_split = labels_split,
                                label_set= label_set,
                                range=panel_params$x.range,
                                at = at,
                                theme=theme,
                                override_plotting_function = self$override_plotting_function)

  label_width <- gtable_width(gtable_filter(ggplotGrob(ggpl), "axis-l"))
  if(is.null(theme$combmatrix.label.height)){
    if(length(calc_element("axis.text.y", theme)) > 0){
      axis_text_size <- calc_element("axis.text.y", theme)$size
    }else{
      axis_text_size <- 0
    }
    label_height <- unit((axis_text_size + theme$combmatrix.label.extra_spacing) * length(label_set), "pt") +
      theme$combmatrix.label.total_extra_spacing
  }else{
    label_height <- theme$combmatrix.label.height
  }

  if(is.null(theme$combmatrix.label.width)){
    self$comb_axis_label_width <- label_width
  }else{
    self$comb_axis_label_width <- theme$combmatrix.label.width
  }

  ggpl <- ggpl + theme(plot.margin = unit.c(theme$combmatrix.panel.margin[1], zero,
                                            theme$combmatrix.panel.margin[2], label_width * -1))
  axis_repl <- ggplotGrob(ggpl)

  if(position == "bottom"){
    gt <- gtable_col("axis", grobs = list(ticks, axis_repl),
                     width = one, heights = unit.c(ticks_length, label_height))
    justvp <- viewport(y = 1, just = "top", height = gtable_height(gt))
  }else{
    gt <- gtable_col("axis", grobs = list(axis_repl,ticks),
                     width = one, heights = unit.c(label_height, ticks_length))
    justvp <-  viewport(y = 0, just = "bottom",    height = gtable_height(gt))
  }



  absoluteGrob(gList(line, gt), width = gtable_width(gt),
               height = gtable_height(gt), vp = justvp)



}



make_combination_matrix_plot <- function(labels, labels_split, label_set, range, at, theme, override_plotting_function){

  df <- tibble(labels, labels_split, at=c(at))
  df2 <- as_tibble(expand.grid(labels=labels, single_label=label_set, stringsAsFactors = FALSE))
  df2$id <- seq_len(nrow(df2))
  df2 <- as_tibble(merge(df2, df, sort=FALSE, by="labels"))
  df2 <- df2[order(df2$id), ]
  df2$observed <- mapply(FUN=function(labs, row) {
    row %in% labs
  }, df2$labels_split, df2$single_label)
  df2$index <- as.numeric(as.factor(df2$single_label))

  if(is.null(override_plotting_function)){
    plt <- ggplot(df2, aes(x= .data$at, y= .data$single_label))
    if(isTRUE(theme$combmatrix.panel.striped_background)){
      plt <- plt + geom_rect(aes(fill= .data$index %% 2 == 0), ymin=df2$index-0.5, ymax=df2$index+0.5, xmin=0, xmax=1)
    }

    plt <- plt +
      geom_point(aes(color= .data$observed), size=theme$combmatrix.panel.point.size)
    if (!isTRUE(theme$combmatrix.panel.line.size == 0)) {
      # If combmatrix.panel.line.size is not a single number equal to 0, add the
      # lines. (ifFALSE is available starting in v3.5)
      plt <- plt + geom_line(
        data=function(dat) dat[dat$observed, ,drop=FALSE],
        aes(group = .data$labels),
        size=theme$combmatrix.panel.line.size,
        color=theme$combmatrix.panel.line.color
      )
    }
    plt <- plt +
      ylab("") + xlab("") +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      scale_y_discrete(breaks=label_set, labels=if(is.null(names(label_set))) waiver() else names(label_set)) +
      scale_fill_manual(values= c(`TRUE` = theme$combmatrix.panel.striped_background.color.one,
                                  `FALSE` = theme$combmatrix.panel.striped_background.color.two)) +
      scale_color_manual(values= c(`TRUE` = theme$combmatrix.panel.point.color.fill,
                                   `FALSE` = theme$combmatrix.panel.point.color.empty)) +
      guides(color="none", fill="none") +
      theme(
        panel.background = element_blank(),
        axis.text.y = theme$combmatrix.label.text %||% theme$axis.text.y,
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank()
      )
  }else{
    plt <- override_plotting_function(df2)
  }

  plt
}

#' @export
merge_element.unit.list <- function(new, old){
  new
}




absoluteGrob <- function (grob, width = NULL, height = NULL, xmin = NULL, ymin = NULL,
          vp = NULL){
  gTree(children = grob, width = width, height = height, xmin = xmin,
        ymin = ymin, vp = vp, cl = "absoluteGrob")
}

element_render <- function (theme, element, ..., name = NULL) {
  el <- calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }
  grob <- element_grob(el, ...)
  grob$name <- grobName(grob, paste(element, name, sep = "."))
  grob
}
