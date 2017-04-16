
#' Dirchlet.MPNL.distribution
#'
#' @include dirchlet_distribution.R
#'
#' @slot score numeric.
#' @slot cluster list.
#'
#'
setClass("Dirchlet.MPNL.distribution",
         #    representation(score = "numeric", cluster = "list"),
         contains = "Dirchlet.distribution"
)
