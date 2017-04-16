
#' Dirchlet.distribution
#'
#'
#' @include distribution_of_probability.R
#'
#' @slot score numeric.
#' @slot cluster list.
#'
setClass("Dirchlet.distribution",
         #    representation(score = "numeric", cluster = "list"),
         contains = "Distribution.of.probability"
)
