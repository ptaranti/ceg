# TODO(taranti) enforce Google's R style
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
setClass("DirchletDistribution",
         #    representation(score = "numeric", cluster = "list"),
         contains = "DistributionOfProbability"
)
