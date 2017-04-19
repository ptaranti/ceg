# Class defined for future use


#' Staged.tree
#'
#' @slot event.tree Event.tree.
#'
#'@include event_tree.R

setClass(
  "Staged.tree",
  representation(
    event.tree = "Event.tree"
  )
)
