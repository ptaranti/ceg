# TODO(taranti) enforce Google's R style
# TODO(taranti) documentar



#' The \code{Stratified.ceg.model} is a S4 class thar extends \code{\link{Ceg.model}}. Its
#'  objects represents a CEG model derived from a Stratified.staged.tree.
#'
#' @include ceg_model.R
#' @return a "Stratified.ceg.model" S4 object.
#' @export
#'
#' @examples
setClass("Stratified.ceg.model",
         representation( ),
          contains = "Ceg.model"
)

#' A simple "initialize" method. The real construction is performed by a
#' specific constructor. # TODO(taranti)  O CONSTRUTOR NAO FOI IMPLEMENTADO
#'
#' @param Ceg.model
#'
#' @return
#' @export
#'
#' @examples
#' A simple "initialize" method. The real construction is performed by a
#' specific constructor. # TODO(taranti)  O CONSTRUTOR FOI IMPLEMENTADO NA FUNCAO CEG
#'
#' @param stratified.staged.tree
#' @param position
#'
#' @return Stratified.ceg.model
#  @export
#'
#' @examples  \code{new("Stratified.ceg.model", stratified.stagedTree, position)}, however we recomend using
#' a specific function to construct the object # TODO(taranti)   LISTAR A FUNCAO

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
#' @param StratifiedStagedTree    S4 object    TODO(taranti)  describe
#' @param stages                  list         TODO(taranti)  describe
#'
#' @return a StratifiedCegModel S4 object.
#'
#' @examples
#' TODO(taranti)
#'
#' @export
#'
#' @include stratified_ceg_model.R
#'
Stratified.ceg.model <- function(Stratified.staged.tree, stages)
{
  return(new("Stratified.ceg.model", Stratified.staged.tree, stages))
}





