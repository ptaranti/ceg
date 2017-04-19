

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
#' @param stratified.event.tree  "Stratified.event.tree" a S4 object that represents an event tree.
#' @param alpha  numeric this represents a phantom sample used to initialize the
#'  learning process
#'
#' @return  prior is a list of matrices. Each matrix is a collection of
#' vectors that correspond to a prior for each situation associated with
#' a particular variable.
#
#'
#' @seealso \code{\link{PriorVariable}}
#'
PriorDistribution <- function(stratified.event.tree, alpha) {
  alpha.edge <- lapply(1:(stratified.event.tree@num.variable), function(x)
    AlphaEdgeInternal(x, stratified.event.tree, alpha))
  prior <- lapply(1:(stratified.event.tree@num.variable),
                  function(x) PriorVariable(stratified.event.tree@num.situation[x],
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
#' @seealso  \code{Prior.distribution} and
#'          \code{AlphaEdgeInternal}
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
#' @param stratified.event.tree  "Stratified.event.tree" a S4 object that
#' represents an event tree.
#' @param alpha  numeric this represents a phantom sample used to initialize the
#'  learning process
#'
#' @return numeric It is the hyperparameter of the Dirichlet prior distribution
#'  for each situation in the event tree associated with a particular variable.
#'
AlphaEdgeInternal <- function(level, stratified.event.tree, alpha) {
  if (level <= stratified.event.tree@num.variable) {
    variable <- level
    result <- rep(alpha / (stratified.event.tree@num.category[variable] * stratified.event.tree@num.situation[level]),
                  stratified.event.tree@num.category[variable])
  } else {
    variable <- level - stratified.event.tree@num.variable
    result <- rep(alpha / (stratified.event.tree@num.category[variable] * stratified.event.tree@num.situation[level]),
                  stratified.event.tree@num.category[variable]) * (stratified.event.tree@num.slice - 1)
  }
  return(result)
}

