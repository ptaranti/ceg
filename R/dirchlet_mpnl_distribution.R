# TODO(taranti) documentar


#' Title
#' @include dirchlet_distribution.R
#'
#' @slot score numeric.
#' @slot cluster list.
#'
#' @return
#' @export
#'
#' @examples
setClass("Dirchlet.MPNL.distribution",
         #    representation(score = "numeric", cluster = "list"),
         contains = "Dirchlet.distribution"
)
