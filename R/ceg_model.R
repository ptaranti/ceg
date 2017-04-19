
# TODO(Taranti) Trasferir todo codigo para stratified
# TODO(collazo)  DEFINIR O QUE EH position (slot "list") e o que a difere da estrutura de estagios. Se é igual, o objeto CEG seria descartavel - ele é igual ao staged, os dados são os mesmos. O que muda é o plot.
# TODO(collazo)  VRF documentação das funçoes
# TODO(Taranti)  mudar função plot para usar positon slot

#' Ceg.model S4 class
#'
#' \code{Ceg.model} is a S4 class which objects represents a Chain-Event Graph
#' (CEG) model, which is composed by a Staged Tree and its stages structure.

#'
#' @slot staged.tree "Staged.tree"
#' @slot position list  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#'
#'
#'@include staged_tree.R
setClass("Ceg.model",
         representation(staged.tree = "Staged.tree",
                        position = "list")
)



setMethod(
  f = "initialize",
  signature = "Ceg.model",
  definition = function(.Object,
                        staged.tree,
                        postion) {
    cat("~~~ CegModel: initializator ~~~ \n")
    # Assignment of the slots
    .Object@staged.tree <- staged.tree
    .Object@postion <- postion
    return(.Object)
    # return of the object
  }
)



