# TODO(taranti) documentar


#' Title
#' @include distribution_of_probability.R
#'
#' @slot score numeric.
#' @slot cluster list.
#'
#' @return
#' @export
#'
#' @examples
setClass("Dirchlet.distribution",
         #    representation(score = "numeric", cluster = "list"),
         contains = "Distribution.of.probability"
)
