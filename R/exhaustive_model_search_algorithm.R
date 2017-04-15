# TODO(taranti) enforce Google's R style
# TODO(taranti) documentar


#' Title
#'
#' @include model_search_algorithm.R
#'
#' @return
#' @export
#'
#' @examples
setClass("ExhaustiveModelSearchAlgorithm",
         #    representation(score = "numeric", cluster = "list"),
         contains = "ModelSearchAlgorithm"
)
