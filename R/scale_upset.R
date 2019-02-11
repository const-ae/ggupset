
scale_x_combmatrix <- function(..., expand = waiver(), position = "bottom"){
  sc <- discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
                       expand = expand, guide = "none", position = position, super = ScaleCombMatrix)
  sc$range_c <- ggplot2:::continuous_range()
  sc
}


scale_x_upset <- function(order_by = c("freq", "degree"), n_sets = Inf, n_intersections = Inf,
                          sets = NULL, intersections = NULL, reverse=FALSE,
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
  sc
}


ScaleCombMatrix <- ggproto("ScaleCombMatrix", ScaleDiscretePosition,

  internal_text_separator = "-_-_-",

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



ScaleUpset <- ggproto("ScaleUpset", ScaleCombMatrix,
   # The
   order_by = "freq",
   n_sets = Inf,
   n_intersections = Inf,
   sets = NULL,
   intersections = NULL,
   reverse=FALSE,

   train = function(self, x) {
     if(is.list(x)){
       x <- lapply(x, sort)
       if(is.null(self$sets)){
         sets <- unique(unlist(x))
       }
       sets <- names(sort(table(unlist(x))[sets], decreasing = TRUE))[seq_len(min(length(sets), self$n_sets))]
       sets <- factor(sets, levels=sets, ordered=TRUE)

       self$sets <- sets

       to_delete <- vapply(x, function(elem) !any(elem %in% levels(sets)), FUN.VALUE = FALSE)
       x <- lapply(x, function(elem) elem[elem %in% levels(sets)])
       x_string <- vapply(x, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
       x_string[to_delete] <- NA
       # browser()

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
     ggproto_parent(ScaleCombMatrix, self)$train(x_string)
   },


   map = function(self, x, limits = self$get_limits()) {
     if(is.list(x)){
       x <- lapply(x, sort)
       to_delete <- vapply(x, function(elem) !any(elem %in% levels(self$sets)), FUN.VALUE = FALSE)
       x <- lapply(x, function(elem) elem[elem %in% levels(self$sets)])
       x_string <- vapply(x, paste0, collapse=self$internal_text_separator, FUN.VALUE = "")
       x_string[to_delete] <- NA
     }else{
       x_string <- x
     }
     ggproto_parent(ScaleDiscretePosition, self)$map(x_string, limits)
   },

   get_limits = function(self){
     limits <- ggproto_parent(ScaleDiscretePosition, self)$get_limits()
     limits <- limits[! is.na(limits)]
     limits
   }

)
