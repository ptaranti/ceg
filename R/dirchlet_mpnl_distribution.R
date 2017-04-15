# TODO(taranti) enforce Google's R style
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
setClass("DirchletMPNLDistribution",
         #    representation(score = "numeric", cluster = "list"),
         contains = "DirchletDistribution"
)
