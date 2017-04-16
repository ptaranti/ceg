
#' Multinomial.distribution
#'
#' @include  distribution_of_probability.R
#'
#' @slot score numeric.
#' @slot cluster list.
#'
setClass("Multinomial.distribution",
     #    representation(score = "numeric", cluster = "list"),
         contains = "Distribution.of.probability"
)
