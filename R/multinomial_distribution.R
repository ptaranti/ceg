
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
setClass("Multinomial.distribution",
     #    representation(score = "numeric", cluster = "list"),
         contains = "Distribution.of.probability"
)
