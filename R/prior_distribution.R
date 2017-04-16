# TODO(taranti) enforce Google's R style
# TODO(taranti) documentar
# TODO(Collazo) a classe prior distribution nao esta sendo usada.
# A funcao PriorDistribution retorna um vetor e nao um objeto.

#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
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
#' @param alpha  # TODO(taranti)  -- see function DCEG.AHC
#'
#' @return  prior  It is a list of matrices. Each matrix is a collection of
#' vectors that correspond to a prior for each situation associated with
#' a particular variable.
# @export
#'
#' @seealso \code{\link{prior.variable}}
#'
#' @examples
PriorDistribution <- function(tree, alpha) {
  alpha.edge <- lapply(1:(tree@num.variable), function(x)
    alpha.edge.internal(x, tree, alpha))
  prior <- lapply(1:(tree@num.variable),
                  function(x) prior.variable(tree@num.situation[x],
                                             alpha.edge[[x]]))
  return(prior)
}



#'   The function \code{prior.variable}  yields the prior distribution for each
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
#' @examples
#' @seealso  \code{\link{Prior.distribution}} and
#'          \code{\link{alpha.edge.internal}}
#'
prior.variable <- function(ref, alpha.edge) {
  if (ref < 1)
    return(c())
  return(rbind(prior.variable(ref - 1, alpha.edge),
               alpha.edge,
               deparse.level = 0))
}




#' \code{alpha.edge.internal} yields the prior distribution for each situation
#' in the event tree associated with a particular variable.
#'
#' @param level "numeric" controls the number of descendent situations
#' @param tree  "EventTree" a S4 object that represents an event tree.
#' @param alpha  # TODO(taranti)
#'
#' @return # TODO(taranti)
# @export
#'
#' @examples
#'
alpha.edge.internal <- function(level, tree, alpha) {
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

