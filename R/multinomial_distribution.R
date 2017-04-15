# TODO(taranti) documentar
# TODO(taranti) enforce Google's R style

#' Title
#'
#' @include  distribution_of_probability.R
#'
#' @slot score numeric.
#' @slot cluster list.
#'
#' @return
#' @export
#'
#' @examples
setClass("MultinomialDistribution",
     #    representation(score = "numeric", cluster = "list"),
         contains = "DistributionOfProbability"
)
