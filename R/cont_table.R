
#' ContingencyTable
#'
#' This function calculates the contigency tables associated with each variable
#' in time-slices t_0 or t_k, k>=1.
#' #TODO(Collazo) definir melhor a funçao  ContingencyTable/ContingencyTableLevel - qual objetivo ou output
#' @param data (see function DCEG.AHC) # TODO(Collazo) ????
#' @param event.tree an object of type "Event.tree"
#'
#' @return a list of matrices. Each matrix is the contigency table associate
#'         with a particular variable.
#
#'
#'
#'
#' @seealso \code{\link{ContingencyTableLevel}} and \code{\link{ContingencyTableVariable}}
#'
ContingencyTable <- function(data, event.tree) {
  contingency.table <-
    lapply(1:(event.tree@num.variable), function(x)
      ContingencyTableLevel(x, data, event.tree))
  return(contingency.table)
}


#' ContingencyTableLevel
#'
#' This function calculates the contigency tables associated with a specific
#' variable in time-slices t_0 or t_k, k>=1.
#'
#' @param level numeric - identifies the level in the infinite event tree.
#' @param data data #TODO(Collazo) definir o que é data (see function DCEG.AHC)
#' @param tree  an "Event.tree" S4 object
#'
#' @return  a matrix. Each row represents the contigency table of a situation
#'       k associate with a particular variable in time-slice t_0 or t_k, k>=1.
#  @export
#'
# @examples

ContingencyTableLevel <- function(level, data, event.tree) {
  if (level <= event.tree@num.variable) {
    variable <- level
    result <- ContingencyTableVariable(variable, data, event.tree)
  } else {
    variable <- level - event.tree@num.variable
    aux <- variable + event.tree@num.variable
    result <-
      sapply(1:(event.tree@num.slice - 1), function(x)
        ContingencyTableVariable(aux,
                            data[, (1 + (x - 1) * event.tree@num.variable):(variable + x * event.tree@num.variable)], event.tree))
    result <- apply(result, 1, sum)
    dim(result) <-
      c(event.tree@num.situation[aux], event.tree@num.category[variable])
  }
  return(result)
}




#' ContingencyTableVariable
#'
#' This function calculates the contigency tables associated with a specific
#' variable according to a time-slice.
#'
#' @param variable numeric
#' @param data  #TODO(Collazo) definir o que é data
#' @param tree an object of type 'event.tree' (see function event.tree)
#'
#' @return  a matrix. Each row represents the contigency table of a situation
#'          k associate with a particular variable.
# TODO (Collazo) VRF texto: It is the same function of CEG_IP_ModelSearch and CEG_IP_ModelSearch_HS+AHC.
#
#
ContingencyTableVariable <- function(variable, data, tree) {
  contingency.table.var <- ftable(data[, 1:variable])
  contingency.table.var <-
    sapply(seq_len(tree@num.situation[variable]), function(x)
      contingency.table.var[x,])
  # contingency_table_var<-lapply(seq_len(tree@num_situation[variable]),function(x)
  #   contingency_table_var[x,]) To return a list.
  return(t(contingency.table.var))
}
