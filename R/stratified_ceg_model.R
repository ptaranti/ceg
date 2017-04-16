# TODO(Collazo) permanece confusao com position e staged.tree

#' The \code{Stratified.ceg.model} is a S4 class thar extends \code{\link{Ceg.model}}. Its
#'  objects represents a CEG model derived from a Stratified.staged.tree.
#'
#' @include ceg_model.R
#' @export
#'
setClass("Stratified.ceg.model",
         representation( ),
          contains = "Ceg.model"
)


setMethod(
  f = "initialize",
  signature = "Stratified.ceg.model",
  definition = function(.Object,
                        stratified.staged.tree,
                        position) {
    cat("~~~ CEGStretified: initializator ~~~ \n")
    # Assignment of the slots
    .Object@staged.tree <- stratified.staged.tree
    .Object@position <- position
    return(.Object)
    # return of the object
  }
)


#' @title StratifiedCegModel constructor.
#'
#' @description S3 function to friendly construct S4  StratifiedCegModel.
#'
#' @details The \code{stratified.ceg.model} returns a S4 StratifiedCegModel.
#'
#' @seealso
#' \code{\link{StratifiedCegModel}}
#'
#' @param StratifiedStagedTree    S4 object    TODO(Collazo)  describe
#' @param stages                  list         TODO(Collazo)  describe
#'
#' @return a StratifiedCegModel S4 object.
#'
#' @examples
#' TODO(taranti) depois de resolvida a confusao entre stage.structure e position
#'
#' @export
#'
#' @include stratified_ceg_model.R
#'
Stratified.ceg.model <- function(Stratified.staged.tree, stages)
{
  return(new("Stratified.ceg.model", Stratified.staged.tree, stages))
}





