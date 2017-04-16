# TODO(taranti) documentar


#' Title
#'
#' @include model_search_algorithm.R
#'
#' @return
#' @export
#'
#' @examples
setClass("Exhaustive.model.search.algorithm",
         #    representation(score = "numeric", cluster = "list"),
         contains = "Model.search.algorithm"
)
