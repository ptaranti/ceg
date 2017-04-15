# TODO(taranti) documentar
# TODO(taranti) enforce Google's R style

#' Title
#'
#' @include model_search_algorithm.R
#'
#'
#' @return
#' @export
#'
#' @examples
setClass("HeuristicModelSearchAlgorithm",
         #    representation(score = "numeric", cluster = "list"),
         contains = "ModelSearchAlgorithm"
)
