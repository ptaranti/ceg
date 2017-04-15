# TODO(taranti) enforce Google's R style
# TODO(taranti) documentar


#' This function calculates the contigency tables associated with each variable
#' in time-slices t_0 or t_k, k>=1.
#' #TODO(Collazo) definir melhor a funçao  cont.table.level - qual objetivo ou output
#' @param data (see function DCEG.AHC)
#' @param tree an object of type "EventTree"
#'
#' @return a list of matrices. Each matrix is the contigency table associate
#'         with a particular variable.
# @export
#'
#' @examples
#'
#' @seealso \code{\link{cont.table.level}} and \code{\link{cont.table.variable}}
cont.table <- function(data, tree) {
  contingency.table <-
    lapply(1:(tree@num.variable), function(x)
      cont.table.level(x, data, tree))
  return(contingency.table)
}


#' This function calculates the contigency tables associated with a specific
#' variable in time-slices t_0 or t_k, k>=1.
#'
#' @param level numeric - identifies the level in the infinite event tree.
#' @param data data #TODO(Collazo) definir o que é data (see function DCEG.AHC)
#' @param tree  an object of type "EventTree"
#'
#' @return  a matrix. Each row represents the contigency table of a situation
#'       k associate with a particular variable in time-slice t_0 or t_k, k>=1.
#  @export
#'
#' @examples

cont.table.level <- function(level, data, tree) {
  if (level <= tree@num.variable) {
    variable <- level
    result <- cont.table.variable(variable, data, tree)
  } else {
    variable <- level - tree@num.variable
    aux <- variable + tree@num.variable
    result <-
      sapply(1:(tree@num.slice - 1), function(x)
        cont.table.variable(aux,
                            data[, (1 + (x - 1) * tree@num.variable):
                                   (variable + x * tree@num.variable)], tree))
    result <- apply(result, 1, sum)
    dim(result) <-
      c(tree@num.situation[aux], tree@num.category[variable])
  }
  return(result)
}




#' This function calculates the contigency tables associated with a specific
#' variable according to a time-slice.
#'
#' @param variable numeric
#' @param data  #TODO(Collazo) definir o que é data
#' @param tree an object of type 'event.tree' (see function event.tree)
#'
#' @return  a matrix. Each row represents the contigency table of a situation
#'          k associate with a particular variable.
# @export
#'
#' @examples

# It is the same function of CEG_IP_ModelSearch and CEG_IP_ModelSearch_HS+AHC.
cont.table.variable <- function(variable, data, tree) {
  contingency.table.var <- ftable(data[, 1:variable])
  contingency.table.var <-
    sapply(seq_len(tree@num.situation[variable]), function(x)
      contingency.table.var[x,])
  # contingency_table_var<-lapply(seq_len(tree@num_situation[variable]),function(x)
  #   contingency_table_var[x,]) To return a list.
  return(t(contingency.table.var))
}
