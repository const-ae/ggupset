#' Tidy version of the movies dataset from the ggplot2 package
#'
#' The original \code{ggplot2movies::movies} dataset has 7 columns that
#' contain indicators if a movies belongs to a certain genre. In this version
#' the 7 columns are collapsed to a single list column to create a tidy
#' dataset. It also has information on only 5,000 movies to reduce the size
#' of the dataset. Furthermore each star rating is in its on row.
#'
#' @format a data frame with 50,000 rows and 10 columns
#' \itemize{
#'   \item title. The title of the movie.
#'   \item year. Year of release.
#'   \item budget. Total budget (if known) in US dollars.
#'   \item length. Length in minutes.
#'   \item rating. Average IMDB user rating.
#'   \item votes. Number of IMDB user who rated this movie.
#'   \item mpaa. MPAA rating
#'   \item Genres. List column with all genres the movie belongs to
#'   \item stars, percent_rating. The number of stars and the corresponding
#'     percentage of people rating the movie with this many stars.
#' }
#' @examples
#' dim(tidy_movies)
#' head(tidy_movies)
#'
"tidy_movies"




#' A fictional biological dataset with a complex experimental design
#'
#' @format a data frame with 360 rows and 4 variables
#' \itemize{
#'   \item KO. Boolean value if the sample had a knock out.
#'   \item DrugA. character vector with "Yes" and "No" elements
#'     indicating if the sample was treated with drug A.
#'   \item Timepoint. Numeric vector with elements 8, 24, and 48
#'     indicating the time of measurement since the beginning of
#'     the experiment.
#'   \item response. Numeric vector with the response of the sample
#'     to the treatment conditions. Could for example be the concentration
#'     of a metabolite.
#' }
#' @examples
#' dim(df_complex_conditions)
#' head(df_complex_conditions)
#'
"df_complex_conditions"







#' A fictional dataset describing which genes belong to certain pathways
#'
#' @format a matrix with 6 rows and 37 columns. Each row is one pathway, with
#'   its name given as `rownames` and each column is a gene. The values
#'   in the matrix are Boolean indicators if the gene is a member of the pathway.
#'
#'
#' @examples
#' dim(gene_pathway_membership)
#' gene_pathway_membership[, 1:15]
"gene_pathway_membership"

