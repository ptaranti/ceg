
#' ContingencyTable
#'
#' This function calculates the contigency tables associated with each variable.
#' This enables us to calculate the number of units that visit each situation
#' in an Stratified.event.tree.
#' This function is used to generate a stratified.staged.tree when a well
#' behavioured data.frame is provided as an argument to Stratified.staged.tree()
#' constructor.
#'
#' @param data a data.frame structure  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @param stratified.event.tree an object of type "Stratified.event.tree"
#'
#' @return a list of matrices. Each matrix is the contigency table associate
#'         with a particular variable.
#
#'
#'
#'
#' @seealso \code{\link{ContingencyTableLevel}} and \code{\link{ContingencyTableVariable}}
#'
ContingencyTable <- function(data, stratified.event.tree) {
  contingency.table <-
    lapply(1:(stratified.event.tree@num.variable), function(x)
      ContingencyTableLevel(x, data, stratified.event.tree))
  return(contingency.table)
}


#' ContingencyTableLevel
#'
#' This function calculates the contigency tables associated with a specific
#' variable.
#'
#' @param level numeric - identifies the level in the infinite event tree.
#' @param data a data.frame structure  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @param stratified.event.tree  a Stratified.event.tree S4 object
#'
#' @return  a matrix. Each row represents the contigency table of a situation
#'       k associate with a particular variable.
#'
ContingencyTableLevel <- function(level, data, stratified.event.tree) {
  if (level <= stratified.event.tree@num.variable) {
    variable <- level
    result <- ContingencyTableVariable(variable, data, stratified.event.tree)
  } else {
    variable <- level - stratified.event.tree@num.variable
    aux <- variable + stratified.event.tree@num.variable
    result <-
      sapply(1:(stratified.event.tree@num.slice - 1), function(x)
        ContingencyTableVariable(aux,
                            data[, (1 + (x - 1) * stratified.event.tree@num.variable):(variable + x * stratified.event.tree@num.variable)], stratified.event.tree))
    result <- apply(result, 1, sum)
    dim(result) <-
      c(stratified.event.tree@num.situation[aux], stratified.event.tree@num.category[variable])
  }
  return(result)
}




#' ContingencyTableVariable
#'
#' This function calculates the contigency tables associated with a specific
#' variable according to a time-slice.
#'
#' @param variable numeric  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @param data  a data.frame structure  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @param stratified.event.tree an object of type Stratified.event.tree
#'
#' @return  a matrix. Each row represents the contigency table of a situation
#'          k associate with a particular variable.
#'
ContingencyTableVariable <- function(variable, data, stratified.event.tree) {
  contingency.table.var <- stats::ftable(data[, 1:variable])
  contingency.table.var <-
    sapply(seq_len(stratified.event.tree@num.situation[variable]), function(x)
      contingency.table.var[x,])
  # contingency_table_var<-lapply(seq_len(tree@num_situation[variable]),function(x)
  #   contingency_table_var[x,]) To return a list.
  return(t(contingency.table.var))
}
