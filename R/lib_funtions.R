# TODO(taranti) documentar
# TODO(taranti) enforce Google's R style

#' @title
#' Merge labels of multiple edges.
#'
#'
#' @description
#' Merge labels of multiple edges in order to plot them all.
#'
#' @details
#' This function mitigates a limitation from Rgraphviz, since it is not
#' possible to plot multiple edges between two nodes presenting the correct
#' edge label for each one. The authors did not find a graphical package
#' providing this capability. Contributions are wellcomed.
#'
#' This is a inner function, not intended to be exported to users.
#'
#' @seealso TODO(taranti)
#'
#' @param	edge.list	vector	list of positions that a children of a specific
#' position v1.
#' @param	edge 		numeric	a particular children "edge" of a specific position v1
#' @param	level 	vector	labels of classes corrresponding to the variable
#' associated with a position.
#'
#' @return	string	 merged labels associated with a specific position v1.
#'
#' @examples
#' TODO(taranti)
#'
# @export
#'
# @include


merge.label <- function(edge.list,edge,level){
  aux.merge <- which(edge.list == edge)
  aux <- length(aux.merge)
  aux.label <- level[aux.merge[1]]
  if (aux > 1) {
    for (i in 2:aux) {
      aux.label <- paste0(aux.label,"-",level[aux.merge[i]])
    }
  }
  return(aux.label)
}





#' @title Simple ceg structure to be ploted in RGraphviz.
#'
#' @description
#'  This function yields a data structure corresponding a simplified CEG to be
#' plotted using the package Rgraphviz.
#'
#' @details
#' This function mitigates a limitation from Rgraphviz, which does not support
#' plotting multiple edges between two nodes presenting the correct
#' edge label for each one. The decision was to merge all edges in one, and
#' presenting all labels in this resulting edge.
#'
#' This approach is temporary and not ideal, since the ceg is no more a
#' multi-graph. However, the authors did not find a graphical package
#' which provides the needed plotting features.
#' Contributions are wellcomed.
#'
#' This is a inner function, not intended to be exported to users.
#'
#' @seealso TODO(taranti)
#'
#' @param    tree           list       an object event.tree.
#'  TODO(taranti)  Verify:: See files 2T.DCEG.ModelSearch.OAHC, CEG.IP.ModelSearch and
#'  TODO(taranti)  Verify:: CEG.IP.ModelSearch.HS+AHC.
#' @param    position       list       an object ceg.position.
#' TODO(taranti)  Verify:: See file Position.Retrieve.
#' @param    range.color    numeric    it chooses the color source.
#'
#' @return   list
#'    \itemize{
#'    \item $node - node attributes
#'    \item $node$nodes (vector) - set of positions.
#'    \item node$variable (vector) - it identifies the variable asscoiated with
#'    each position.
#'    \item node$color (vector) - color of each position. All positions
#'    coincident with a stage are depicted in white.
#'    \item $edge - edge attributes
#'    \item $edge$edges (list) - set of list that emanates from each position.
#'    \item edge$label (vector) - position labels.
#'    \item weight (vector) - edge weight.
#'    }
#' @examples
#' TODO(taranti)
#'
#' @export
#'
# @include

CegGraphSimple <- function(tree, position, range.color = 1) {

  node.vector <- c()
  node.variable <- c()
  node.color <- c()
  edge.list <- list()
  edge.label <- c()
  edge.weight <- c()
  count.color <- 2
  count.pos <- -1


  if (range.color == 1) {
    color <- palette()
    color[1] <- "white"
  } else {
    color <- colors(1)
    color <- color[-21]
  }

  for (var in 1:(tree@num.variable - 1)) {
    start.pos <- count.pos
    edge.var.list <- c()
    num.situation <- length(position[[var]])
    for (stage in 1:num.situation) {
      num.pos <- length(position[[var]][[stage]])
      pos.next.var <- PositionVector(tree@num.situation[var + 1],
                                      position[[var + 1]])
      for (pos in 1:num.pos) {
        count.pos <- count.pos + 1
        node.vector <- c(node.vector, paste0("w", count.pos))
        node.variable <- c(node.variable, var)
        if (num.pos == 1) node.color <- c(node.color, color[1])
        else node.color <- c(node.color, color[count.color])
        aux <- (position[[var]][[stage]][[pos]][1] - 1) * tree@num.category[var]
        aux <- (aux + 1):(aux + tree@num.category[var])
        edge.var.list <- c(edge.var.list, pos.next.var[aux])
        edge.weight <- c(edge.weight, rep(round(1 / tree@num.category[var], 2),
                                          tree@num.category[var]))
      }
      if (num.pos != 1) count.color <- count.color + 1
    }
    edge.var.list <- edge.var.list + count.pos
    dim(edge.var.list) <- c(tree@num.category[var], length(edge.var.list) /
                              tree@num.category[var])
    edge.var.list <- as.matrix(edge.var.list)
    for (pos in start.pos:(count.pos - 1)) {
      edge.list[[pos + 2]] <- list()
      aux.edge.var.list <- unique(edge.var.list[,pos - start.pos + 1])
      edge.label <- c(edge.label,sapply(1:length(aux.edge.var.list),
                                      function(x) merge.label(
                                        edge.var.list[,pos - start.pos + 1],
                                        aux.edge.var.list[x],
                                        tree@label.category[[var]])))
      edge.list[[pos + 2]]$edges <- paste0("w",aux.edge.var.list)
    }
  }

  var <- tree@num.variable
  num.pos <- length(position[[var]])
  node.vector <- c(node.vector, paste0("w", count.pos + 1:num.pos))
  node.variable <- c(node.variable, rep(var, num.pos))
  node.color <- c(node.color, rep(color[1], num.pos))
  aux.label <- tree@label.category[[var]][1]
  for (i in 2:tree@num.category[var]) {
    aux.label <- paste0(aux.label,"-",tree@label.category[[var]][i])
  }
  edge.label <- c(edge.label,rep(aux.label,num.pos))
  edge.weight <- c(edge.weight, rep(rep(round(1 / tree@num.category[var], 2),
                                        tree@num.category[var]), num.pos))
  ref <- count.pos + num.pos + 1
  for (pos in 1:num.pos) {
    edge.list[[pos + count.pos + 1]] <- list()
    edge.list[[pos + count.pos + 1]]$edges <- paste0("w",ref)
  }

  ref <- ref + 1
  node.vector <- c(node.vector, paste0("w", ref - 1))
  node.color <- c(node.color, color[1])
  edge.list[[ref]] <- list()
  names(edge.list) <- node.vector

  graph <- list()
  graph$node <- list()
  graph$node$nodes <- node.vector
  graph$node$variable <- node.variable
  graph$node$color <- node.color
  graph$edge <- list()
  graph$edge$edges <- edge.list
  graph$edge$label <- edge.label
  graph$edge$weight <- edge.weight

  return(graph)
}






# TODO(Collazo) Retirar a marreta do parâmetro de entrada range.color.
#' @title tree structure to be ploted in RGraphviz.
#'
#' @description
#' tree structure to Event/Stage trees be ploted in \pkg{RGraphviz}.
#'
#' @details
#' This function yields a data structure corresponding to an Event/Stage Tree to
#' be plotted using the package \pkg{Rgraphviz}.
#'
#' @seealso
#'
#' @param tree (list) - an S4 object \code{\link{EventTree}}
#'   TODO(taranti)   See files 2T.DCEG.ModelSearch.OAHC,
#'   TODO(taranti)  CEG.IP.ModelSearch and CEG.IP.ModelSearch.HS+AHC.
#' @param solution  list with two components:
#' \itemize{
#' \item numeric - score associated with a level
#' \item list of vectors - stage structure
#' }
#' @param name vector of strings  - variable names
#' @param range.color (numeric) - it chooses the color source -> 1 - palette (8
#' colors); and any other value - colors(1) (501 colors)
#'
#' @return list: \itemize{
#'  \item  $node - node attributes
#'  \itemize{
#'    \item $node$nodes (vector) - set of situations.
#'    \item node$label (vector) - it identifies the variable asscoiated with
#'        each position.
#'    \item node$color (vector) - color of each situation. All situations
#'        coincident with a stage are depicted in black.
#'    }
#'  \item $edge - edge attributes
#'  \itemize{
#'    \item $edge$edges (list) - set of list that emanates from each situation.
#'    \item edge$label (vector) - edge labels.
#'    }
#'  }
#'
#' @examples
#' TODO(taranti)
#'
#' @export
#'
# It is required the following function:
# NodeSet - This function genereates the nodes of an event tree.
# edge.list - This function genereates the list of edges of an event tree.
# EdgeSituation - This function identifies the edges that emanate from a
# particular situation in an event tree.
# edge.label - This function yields the edge labels.
# node.label - This function yields the node labels.
# node.color - This function yields the node colors.
# list.to.vector - This function change a list of vectors in a vector.


TreeGraph <- function(tree, solution = list(), name = c(), range.color = 1){
  nodes <- NodeSet(tree)
  edgeList <- EdgeList(tree,nodes)
  node.label <- NodeLabel(tree@num.variable,tree@num.situation,
                           tree@num.category,name)
  edge.label <- EdgeLabel(tree@num.variable,tree@num.situation,
                           tree@label.category)
  node.color <- NodeColor(tree@num.variable,tree@num.situation,
                           tree@num.category,solution,range.color)
  graph <- list()
  graph$node <- list()
  graph$node$nodes <- nodes
  graph$node$label <- node.label
  graph$node$color <- node.color
  graph$edge <- list()
  graph$edge$edges <- edgeList
  graph$edge$label <- edge.label
  return(graph)
}





#' This function genereates the nodes of an event tree.
#'
#' @param tree an object event.tree. See files 2T.DCEG.ModelSearch.OAHC,
#' CEG.IP.ModelSearch and CEG.IP.ModelSearch.HS+AHC.
#'
#' @return  vector
#' @export
#'
#' @examples
#'

NodeSet <- function(tree) {
  tree
  num.node <- sum(tree@num.situation) + tree@num.situation[tree@num.variable] *
    tree@num.category[tree@num.variable]
  node <- paste("s", 1:num.node - 1, sep = "")
  return(node)
}




#' @title yields the node labels.
#'
#' @description
#' This function yields the node labels.
#'
#' @details
#' The nodes are labeled accordingly, do indicate diferente positions.
#' TODO(taranti)  verify details.
#'
#' @param num.variable   numeric - number of variables.
#' @param num.situation      vector - number of stages associated with each
#' variable.
#' @param num.category  vector - it identifies the number of edges that emanate
#'  from situations in each level.
#' @param label         list of vectors - each component is a vector that
#' contains the event names associated with each variable.
#'
#' @return   vector - node labels
#'
#' @export
#'
#' @examples

NodeLabel <- function(num.variable, num.situation, num.category, label) {
  result <- sapply(1:num.variable, function(x) rep(label[x], num.situation[x]))
  result <- ListToVector(result, num.variable)
  num.leaf <- num.category[num.variable] * num.situation[num.variable]
  aux <- sapply(1:num.leaf, function(x) paste("leaf", x))
  result <- c(result, aux)
  return(result)
}



#' @title change a list of vectors in a vector
#'
#' @description
#' This function change a list of vectors in a vector.
#'
#' @details
#' This function change a list of vectors in a vector.
#'
#' @param x list of vectors
#' @param n numeric
#'
#' @return vector
#' @export
#'
#' @examples
#'
#'
ListToVector <- function(x, n) {
  if (n < 1)
    return(c())
  return(c(ListToVector(x, n - 1), x[[n]]))
}





#' @title yields the edge labels.
#'
#' @description
#' This function yields the edge labels.
#'
#' @details
#' The edges are labeled accordingly the original data provided.
#' TODO(taranti)  verify details.
#'
#' @param num.variable numeric         - number of variables.
#' @param num.situation    vector          - number of stages associated with
#' each variable.
#' @param label        list of vectors - each component is a vector that
#' contains the event names associated with each variable.
#'
#' @return vector - edge labels
#' @export
#'
#' @examples
#'
EdgeLabel <- function(num.variable, num.situation, label) {
  result <-
    sapply(1:num.variable, function(x)
      rep(label[[x]], num.situation[x]))
  result <- ListToVector(result, num.variable)
  return(result)
}






#' @title genereates the list of edges.
#'
#' @description  function genereates the list of edges of an event tree.
#'
#'
#' @details function genereates the list of edges of an event tree.

#'
#' @param tree    an object event.tree. # TODO(taranti)  See files 2T.DCEG.ModelSearch.OAHC,
#' CEG.IP.ModelSearch and TODO CEG.IP.ModelSearch.HS+AHC.
#' @param node    (vector) - an object generated by the function node.list
#'
#' @return list of lists - each list component is a vector that represents the
#' edges that emanate from a vertice.
#' @export
#'
#' @examples
#'
#'

EdgeList <- function(tree, node) {
  start.situation <- cumsum(tree@num.situation)
  max <- sum(tree@num.situation) +
    tree@num.situation[tree@num.variable] * tree@num.category[tree@num.variable]
  start.situation <- start.situation + 1
  start.situation <- c(1, start.situation)
  edge.list <-
    lapply(1:max, function(x)
      EdgeSituation(x, start.situation, tree@num.category))
  names(edge.list) <- node
  return(edge.list)
}


#' @title edge situation
#'
#' @description identifies the edges from a situation (node)
#'
#'
#' @details
#'  This function identifies the edges that emanate from a particular situation
#' in an EventTree.
#'
#' This function identifies the edges that emanate from a particular situation
#' in an event tree.
#'
#' @param situation         numeric  - it identifies the target situation whose
#'  emanating edges are our interesting.
#' @param start.situation   vector   - it identifies the situation that begins
#' a new level.
#' @param num.category      vector   - it identifies the number of edges that
#' emanate from situations in each level.
#'
#' @return  list of lists - each list component is a vector that represents the
#'  edges that emanate from a vertice.
#' @export
#'
#' @examples
#'
#'
EdgeSituation <- function(situation, start.situation, num.category) {
  max <- start.situation[length(start.situation)]
  if (situation >= max)
    return(list())
  aux <- findInterval(situation, start.situation)
  result <- start.situation[aux + 1] - 2 +
    (situation - start.situation[aux]) * num.category[aux] + 1:num.category[aux]
  result <- paste("s", result, sep = "")
  result <- list(edges = result)
  return(result)
}








#' @title verifies data complience
#'
#' @description verifies data behaviour and removes NULL and void data
#'
#'
#' @details
#'  This function verifies data behaviour and removes NA and void data.
#'
#' @param data.frame  a data frame to be used to create stratified event/staged
#' trees
#'
#' @return  a data.frame with no void or NA values.
#'
#' @export
#'
#' @examples
#' TODO(taranti)
#'
DataFrameChecker <- function(data.frame) {
  if (!is(data.frame, "data.frame")) {
    message("Input is not a data frame, prease check it")
    return(NULL)
  }

  message("data.frame is ready!")

  data.frame[data.frame == ""] <- NA

  if (!anyNA(data.frame)) return(data.frame)

  message("All rows with <NA> or absent values were removed")

  return(Z.NA.Keep(data.frame))
}




#' @title Remove void and NA values from dataframe.
#'
#' @description
#' Remove void and NA values from dataframe.
#'
#' @details
#' The code is strongly based on the one provide by Jerry T. in
#'  \link{http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame}.
#'
#' @seealso
#' \link{http://stackoverflow.com/a/30461945/2292993}.
#'
#'
#' @param df  # TODO(taranti)
#' @param col # TODO(taranti) TODO TODO
#' @param n  # TODO(taranti)
#'
#'
#' @return describe.
#'
#' @examples
#' TODO
#'
#' @export
#'
# @include
#'
#' @examples
#' TODO
#'
Z.NA.Keep <- function(df, col = NULL, n = 0) {
  if (!is.null(col)) {
    df.temp = df[, col]
  } else {
    df.temp = df
  }

  if (length(n) == 1) {
    if (n == 0) {
      # simply call complete.cases which might be faster
      result = df[complete.cases(df.temp), ]
    } else {
      # credit: http://stackoverflow.com/a/30461945/2292993
      log <- apply(df.temp, 2, is.na)
      logindex <- apply(log, 1, function(x)
        sum(x) == n)
      result = df[logindex,]
    }
  }

  if (length(n) == 2) {
    min = n[1]
    max = n[2]
    log <- apply(df.temp, 2, is.na)
    logindex <-
      apply(log, 1, function(x) {
        sum(x) >= min && sum(x) <= max
      })
    result = df[logindex,]
  }
  return(result)
}
