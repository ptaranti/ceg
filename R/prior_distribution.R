# TODO(Collazo) a classe Prior.distribution nao esta sendo usada : A funcao PriorDistribution retorna um vetor e nao um objeto.

#' Prior.distribution
#'
#'
setClass("Prior.distribution"
         #    representation(score = "numeric", cluster = "list"),
          # contains = "Model.search.algorithm"
)





#' PriorDistribution
#'
#' \code{PriorDistribution} initialises the prior distributions under the
#' conservative and uniform assumptions for the hyperparameter 'alpha' over
#' the event tree.
#'
#' @param tree  "EventTree" a S4 object that represents an event tree.
#' @param alpha  # TODO(Collazo)  -- see function DCEG.AHC
#'
#' @return  prior is a list of matrices. Each matrix is a collection of
#' vectors that correspond to a prior for each situation associated with
#' a particular variable.
#
#'
#' @seealso \code{\link{PriorVariable}}
#'
# @examples
PriorDistribution <- function(tree, alpha) {
  alpha.edge <- lapply(1:(tree@num.variable), function(x)
    AlphaEdgeInternal(x, tree, alpha))
  prior <- lapply(1:(tree@num.variable),
                  function(x) PriorVariable(tree@num.situation[x],
                                             alpha.edge[[x]]))
  return(prior)
}



#'   PriorVariable
#'
#'   The function \code{PriorVariable}  yields the prior distribution for each
#'   situation in the event tree associated with a particular variable.
#'
#' @param ref         "numeric"  - variable order
#' @param alpha.edge  "vector"   - the values of hyperparameters associated with
#' each edge of a situation corresponding to a particular variable.
#'
#' @return   a matrix. The number of rows is equal to the number of situations
#' associated with a particular variable.
#'
# @export
#'
# @examples
#' @seealso  \code{\link{Prior.distribution}} and
#'          \code{\link{alpha.edge.internal}}
#'
PriorVariable <- function(ref, alpha.edge) {
  if (ref < 1)
    return(c())
  return(rbind(PriorVariable(ref - 1, alpha.edge),
               alpha.edge,
               deparse.level = 0))
}




#' AlphaEdgeInternal
#'
#' \code{AlphaEdgeInternal} yields the prior distribution for each situation
#' in the event tree associated with a particular variable.
#'
#' @param level "numeric" controls the number of descendent situations
#' @param tree  "EventTree" a S4 object that represents an event tree.
#' @param alpha  # TODO(Collazo)
#'
#' @return # TODO(Collazo) descrever retorno, se necessario.
# @export
#'
# @examples
#'
AlphaEdgeInternal <- function(level, tree, alpha) {
  if (level <= tree@num.variable) {
    variable <- level
    result <- rep(alpha / (tree@num.category[variable] * tree@num.situation[level]),
                  tree@num.category[variable])
  } else {
    variable <- level - tree@num.variable
    result <- rep(alpha / (tree@num.category[variable] * tree@num.situation[level]),
                  tree@num.category[variable]) * (tree@num.slice - 1)
  }
  return(result)
}

