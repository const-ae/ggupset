
#' @export
scale_x_mergelist <- function(sep="-", ..., expand = waiver(), position = "bottom"){
  sc <- discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
                       expand = expand, guide = "none", position = position, super = ScaleMergeList)
  sc$range_c <- ggplot2:::continuous_range()
  sc$internal_text_separator <- sep
  sc
}

#' @export
scale_x_upset <- function(order_by = c("freq", "degree"), n_sets = Inf, n_intersections = Inf,
                          sets = NULL, intersections = NULL, reverse=FALSE,
                          levels=NULL, ytrans="identity",
                          ..., expand = waiver(), position = "bottom"){
  sc <- discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
               expand = expand, guide = "none", position = position, super = ScaleUpset)
  order_by <- match.arg(order_by,  c("freq", "degree"))
  sc$order_by <- order_by
  sc$n_sets <- n_sets
  sc$n_intersections <- n_intersections
  sc$sets <- sets
  sc$intersections <- intersections
  sc$range_c <- ggplot2:::continuous_range()
  # Some unique separator that does not randomly appear in text data
  sc$internal_text_separator <- "-__-__-_-"


  list(sc, axis_combmatrix(sep = sc$internal_text_separator, levels=levels, ytrans=ytrans))
}


ScaleMergeList <- ggproto("ScaleMergeList", ScaleDiscretePosition,

  internal_text_separator = "-",

  train = function(self, x) {
    if(is.list(x)){
      x <- lapply(x, sort)
      x <- vapply(x, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
    }
    ggproto_parent(ScaleDiscretePosition, self)$train(x)
  },


  map = function(self, x, limits = self$get_limits()) {
    if(is.list(x)){
      x <- lapply(x, sort)
      x <- vapply(x, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
    }
    ggproto_parent(ScaleDiscretePosition, self)$map(x, limits)
  }


)



ScaleUpset <- ggproto("ScaleUpset", ScaleMergeList,
   order_by = "freq",
   n_sets = Inf,
   n_intersections = Inf,
   sets = NULL,
   intersections = NULL,
   reverse=FALSE,

   train = function(self, x) {
     if(is.list(x)){
       x <- lapply(x, sort)

       if(self$n_intersections < length(unique(x))){
         x_tmp <- vapply(x, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
         intersections_tmp <- names(sort(table(x_tmp), decreasing=TRUE))[seq_len(self$n_intersections)]
         self$intersections <- strsplit(intersections_tmp, self$internal_text_separator)
       }
       if(! is.null(self$intersections)){
         x <- lapply(x, function(elem){
           keep <- any(vapply(self$intersections, function(inter){
             length(elem) == length(inter) && all(sort(elem) == sort(inter))
           }, FUN.VALUE= FALSE))
           if(keep){
             elem
           }else{
             NA
           }
         })
       }


       if(is.null(self$sets)){
         sets <- unique(unlist(x))
         sets <- sets[! is.na(sets)]
       }else{
         sets <- self$sets
       }
       sets <- names(sort(table(unlist(x))[sets], decreasing = TRUE))[seq_len(min(length(sets), self$n_sets))]
       sets <- factor(sets, levels=sets, ordered=TRUE)

       self$sets <- sets

       to_delete <- vapply(x, function(elem) length(elem) > 0 && !any(elem %in% levels(sets)), FUN.VALUE = FALSE)
       x <- lapply(x, function(elem) elem[elem %in% levels(sets)])
       x_string <- vapply(x, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
       x_string[to_delete] <- NA

       if(self$order_by == "freq"){
         levels <- names(sort(table(x_string), decreasing = !self$reverse))
       }else if(self$order_by == "degree"){
         levels <- unique(x)
         degree <- vapply(levels, length, FUN.VALUE = 0)
         levels <- vapply(levels, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
         levels <- levels[degree != 0]
         x_string <- vapply(x, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
         levels <- levels[order(degree[degree > 0], sort(-table(x_string))[levels])]
         if(self$reverse){
           levels <- rev(levels)
         }
       }
       x_string <- factor(x_string, levels = levels, ordered=TRUE)
     }else{
       x_string <- x
     }
     ggproto_parent(ScaleMergeList, self)$train(x_string)
   },


   map = function(self, x, limits = self$get_limits()) {
     if(is.list(x)){
       x <- lapply(x, sort)
       to_delete <- vapply(x, function(elem) length(elem) > 0 && !any(elem %in% levels(self$sets)), FUN.VALUE = FALSE)
       x <- lapply(x, function(elem) elem[elem %in% levels(self$sets)])
       x_string <- vapply(x, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
       x_string[to_delete] <- NA
     }else{
       x_string <- x
     }
     ggproto_parent(ScaleDiscretePosition, self)$map(x_string, limits)
   },

   get_limits = function(self){
     limits <- ggproto_parent(ScaleMergeList, self)$get_limits()
     limits <- limits[! is.na(limits)]
     limits
   }

)
