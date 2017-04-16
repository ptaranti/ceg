
# TODO(collazo)  DEFINIR O QUE EH position (slot "list") e o que a difere da estrutura de estagios. Se é igual, o objeto CEG seria descartavel - ele é igual ao staged, os dados são os mesmos. O que muda é o plot.
# TODO(collazo)  VRF documentação das funçoes

#' Ceg.model S4 class
#'
#' \code{Ceg.model} is a S4 class which objects represents a Chain-Event Graph
#' (CEG) model, which is composed by a Staged Tree and its stages structure.

#'
#' @slot staged.tree "Staged.Tree"
#' @slot position list.
#'
#' @export
#'
#' @examples
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




#' Ceg.model Plotting
#'
#' Method to plot a chain event graph from a Ceg.model S4 object. The current
#' \code{ceg} package implementation depends on \code{Rgraphviz} package from
#' Bioconductor for plotting.
#'
#' @param ceg.model a Ceg.model S4 object.
#'
#' @return the plot and also a pdf version is saved in the working directory.
#' @export
#'
#' @examples
#' plot(ceg.model)
#'
setMethod(
  f = "plot",
  signature = "Ceg.model",
  definition = function(x, y, ...) {

    position <- CegPosition(x@staged.tree@stage.structure,
                            x@staged.tree@event.tree@num.category,
                            x@staged.tree@event.tree@num.situation[1:4])

    ceg.graph.simple <- CegGraphSimple(x@staged.tree@event.tree, position)

    g <-
      new(
        "graphNEL",
        nodes = ceg.graph.simple$node$nodes,
        edgeL = ceg.graph.simple$edge$edges ,
        edgemode = "directed"
      )

    # 1.  setting general graphics attributes
    attrsAtt <- list()
    graphAtt <-
      list(rankdir = "LR",
           size = "18.0,24.0",
           bgcolor = "white")  # LF (left-right) is the graph orientation
    edgeAtt   <- list(color = "cyan")
    nodeAtt  <-
      list(fillcolor = "lightgreen",
           shape = "ellipse",
           fixedsize = FALSE)
    attrsAtt <- list(node = nodeAtt,
                     edge = edgeAtt,
                     graph = graphAtt)


    #  2.  Nodes attributes
    # changing nodes names
    nodes.label.list <- ceg.graph.simple$node$nodes
    names(nodes.label.list) <- nodes(g)
    nAttrs <- list()
    nAttrs$label <- nodes.label.list

    #  3.  edges attributes
    # changing the edges names (default is no-name)


    #1st option - using edges ordered list to name edges
    edges.label.list <- ceg.graph.simple$edge$label
    names(edges.label.list) <- edgeNames(g)
    eAttrs <- list()
    eAttrs$label <- edges.label.list

    #primeiro estagio
    Rgraphviz::plot(
      g,
      main = "Chain Event Graph (propagation analysis) ",
      nodeAttrs = nAttrs,
      edgeAttrs = eAttrs,
      attrs = attrsAtt
    )
    pdf(
      "./figure01ceg.pdf",
      width = 8,
      height = 6,
      title = "Chain Event Graph (propagation analysis)"
    ) #width and heignt in inches / title is embed in the pdf image
    Rgraphviz::plot(
      g,
      main = "Chain Event Graph (propagation analysis)",
      nodeAttrs = nAttrs,
      edgeAttrs = eAttrs,
      attrs = attrsAtt
    )
    dev.off()

    # inserting colors
    nAttrs$fillcolor <- ceg.graph.simple$node$color
    names(nAttrs$fillcolor) <- nodes(g)

    #2nd stage
    Rgraphviz::plot(
      g,
      main = "Chain Event Graph",
      nodeAttrs = nAttrs,
      edgeAttrs = eAttrs,
      attrs = attrsAtt
    )
    pdf(
      "./figure02ceg.pdf",
      width = 8,
      height = 6,
      title = "Chain Event Graph"
    )
    Rgraphviz::plot(
      g,
      main = "Chain Event Graph",
      nodeAttrs = nAttrs,
      edgeAttrs = eAttrs,
      attrs = attrsAtt
    )
    dev.off()
  }
)



#' CegGraph function
#'
#' The function \code{CegGraph} produces a data structure corresponding a CEG
#' to be plotted using the package Rgraphviz.
#'
#' @param    event.tree           "Event.tree"       an S4 object
#' @param    position       list       an object ceg.position.# TODO(Collazo) DEFINIR MELHOR POSITION
#' @param    range.color    numeric    it chooses the color source.
#'
#' @return   list
#'    \itemize{
#'    \item $node - node attributes
#'    \item $node$nodes (vector) - set of positions.
#'    \item node$variable (vector) - it identifies the variable asscoiated with
#'          each position.
#'    \item node$color (vector) - color of each position. All positions
#'          coincident with a stage are depicted in white.
#'    \item $edge - edge attributes
#'    \item $edge$edges (list) - set of list that emanates from each position.
#'    \item $edge$label (vector) - position labels.
#'    \item $edge$weight (vector) - edge weight.
#'    }
#'
# @examples
#'
#' @seealso It uses \code{PositionVector}
CegGraph <- function(event.tree, position, range.color = 1) {
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

  for (var in 1:(event.tree@num.variable - 1)) {
    start.pos <- count.pos
    edge.var.list <- c()
    num.situation <- length(position[[var]])
    for (stage in 1:num.situation) {
      num.pos <- length(position[[var]][[stage]])
      pos.next.var <-
        PositionVector(event.tree@num.situation[var + 1],
                       position[[var + 1]])
      for (pos in 1:num.pos) {
        count.pos <- count.pos + 1
        node.vector <-
          c(node.vector, paste0("w", count.pos))
        node.variable <- c(node.variable, var)
        if (num.pos == 1)
          node.color <- c(node.color, color[1])
        else
          node.color <- c(node.color, color[count.color])
        aux <-
          (position[[var]][[stage]][[pos]][1] - 1) * event.tree@num.category[var]
        aux <- (aux + 1):(aux + event.tree@num.category[var])
        edge.var.list <-
          c(edge.var.list, pos.next.var[aux])
        edge.label <-
          c(edge.label, event.tree@label.category[[var]])
        edge.weight <-
          c(edge.weight, rep(round(1 / event.tree@num.category[var], 2),
                             event.tree@num.category[var]))
      }
      if (num.pos != 1)
        count.color <- count.color + 1
    }
    edge.var.list <- edge.var.list + count.pos
    edge.var.list <- paste0("w", edge.var.list)
    dim(edge.var.list) <-
      c(event.tree@num.category[var],
        length(edge.var.list) /
          event.tree@num.category[var])
    edge.var.list <- as.matrix(edge.var.list)
    for (pos in start.pos:(count.pos - 1)) {
      edge.list[[pos + 2]] <- list()
      edge.list[[pos + 2]]$edges <-
        edge.var.list[, pos - start.pos + 1]
    }
  }

  var <- event.tree@num.variable
  num.pos <- length(position[[var]])
  node.vector <-
    c(node.vector, paste0("w", count.pos + 1:num.pos))
  node.variable <- c(node.variable, rep(var, num.pos))
  node.color <- c(node.color, rep(color[1], num.pos))
  edge.label <-
    c(edge.label, rep(event.tree@label.category[[var]], num.pos))
  edge.weight <-
    c(edge.weight, rep(rep(
      round(1 / event.tree@num.category[var], 2),
      event.tree@num.category[var]
    ), num.pos))
  ref <- count.pos + num.pos + 1
  for (pos in 1:num.pos) {
    edge.list[[pos + count.pos + 1]] <- list()
    edge.list[[pos + count.pos + 1]]$edges <-
      paste0("w", rep(ref, event.tree@num.category[var]))
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





#' CegPosition
#'
#' This function obtain the position structure associated with a particular
#' level of a CEG.
#'
#' @param stage  (list) - stage structure associated with a particular level.
#' @param num.category (vector) - number of edges that unfolds from situations
#'        in each level.
#' @param num.situation (vector) - number of situations associated with each
#'        level.
#'
#' @return list of lists \itemize{
#'     \item First list level identifies a level 'l'.
#'     \item Second list level identifies the stage 'a' associated with
#'           level 'l'.
#'     \item The third list level identifies the positions associated with
#'           stage 'i' .
#'           }
#'
#'  @seealso \code{PositionLevel},  \code{PositionVector},
#'  \code{PositionStage}, \code{PairwisePosition}
#'
CegPosition <- function(stage, num.category, num.situation) {
  num.level <- length(num.category)
  result <- list()
  length(result) <- num.level
  result[[num.level]] <-
    PositionLevel(stage[[num.level]]@cluster, 0, num.situation[num.level])
  for (level in (num.level - 1):2) {
    result[[level]] <- PositionLevel(stage[[level]]@cluster,
                                     num.category[level],
                                     num.situation[level + 1],
                                     result[[level + 1]])
  }
  result[[1]] <- list(list(1))
  return(result)
}




#' PositionLevel
#'
#' This function obtain the position structure associated with a particular
#' level of a CEG.
#'
#' @param stage.list  (list) - stage structure associated with a particular
#'        level.
#' @param num.category (vector) - number of edges that unfolds from the
#'        situations
#' @param num.situation.next (numeric) - number of situation in the level
#'        that follows our target level.
#' @param pos.next.level  (list) - position structure associated with the
#'        level that follows our target level (see function PositionLevel)
#'
#' @return  list of lists - The first list level identifies a stage 'i' and the
#'          second list level identifies the positions associated with this
#'          stage 'i'.
#'
#'
#' @seealso  \code{\link{PositionVector}}, \code{\link{PositionStage}} and
#'          \code{\link{PairwisePosition}}
#'
#'
PositionLevel <- function(stage.list,
                          num.category,
                          num.situation.next,
                          pos.next.level = list()) {
  aux <- which(!is.na(stage.list))
  N <- length(aux)
  if (num.category == 0) {
    stage.list <- lapply(1:N, function(x) list(stage.list[[aux[x]]]))
    return(stage.list)
  }
  stage.list <- lapply(1:N, function(x) stage.list[[aux[x]]])
  pos.next.level <- PositionVector(num.situation.next, pos.next.level)
  result <- lapply(1:N, function(x)
    PositionStage(stage.list[[x]], num.category, pos.next.level))
  return(result)
}



#' \code{PositionVector} function rewrites a position structure associated with
#'       a particular level: from a list to a vector.
#'
#' @param num.situation  (numeric) - number of situation in a particular level.
#' @param pos.list  (list) - stage structure of the level that follows the level
#'        of our target position.
#'
#' @return  vector
# @export
#'
#' @examples
#'
PositionVector <- function(num.situation, pos.list) {
  num.situation <- length(pos.list)
  pos.vec <- rep(0, num.situation)
  count <- 1
  for (stage in 1:num.situation) {
    for (pos in 1:length(pos.list[[stage]])) {
      pos.vec[pos.list[[stage]][[pos]]] <- count
      count <- count + 1
    }
  }
  return(pos.vec)
}

#' PositionStage
#'
#' \code{PositionStage} function yields the position structure associated with
#' a particular
#' stage of a CEG.
#'
#' @param stage.vector (vector) - a set of situations that constitute a
#'        particular stage
#' @param num.category (numeric) - number of edges that unfolds from the
#'        situations
#' @param pos.next.level (vector) - It identifies the positions for all
#'        situations in the next level.#'
#' @return  list of vector - Each vector identifies a position.
#'
#' @seealso  \code{\link{PairwisePosition}}
#'
PositionStage <- function(stage.vector, num.category, pos.next.level) {
  stage.vector <- sort(stage.vector)
  result <- list()
  count <- 1

  stop <- FALSE
  N <- length(stage.vector)
  if (N == 1)
    return(list(stage.vector))

  while (stop == FALSE) {
    aux.stage <- sapply(2:N, function(x)
      PairwisePosition(c(stage.vector[1],
                         stage.vector[x]),
                       num.category,
                       pos.next.level))
    aux.stage <- c(TRUE, aux.stage)
    result[[count]] <- stage.vector[aux.stage]
    count <- count + 1
    stage.vector <- stage.vector[!aux.stage]
    N <- length(stage.vector)
    if (N == 1) {
      stop <- TRUE
      result[[count]] <- stage.vector
    } else if (N == 0)
      stop <- TRUE
  }
  return(result)
}




#' PairwisePosition
#'
#' The \code{PairwisePosition } function identifies if two situations are in
#' the same position given that they are in the same stage.
#'
#' @param pair.situation (vector) - situations to be analysed
#' @param num.category  (numeric) - number of edges that unfolds from the
#'        situations
#' @param pos.next.level  (vector) - It identifies the positions for all
#'        situations in the next level.
#'
#' @return  boolean
#'
PairwisePosition <- function(pair.situation, num.category, pos.next.level) {
  situation.1 <- (pair.situation[1] - 1) * num.category + 1:num.category
  #It identifies the situations that unfold from situation 1.
  situation.2 <- (pair.situation[2] - 1) * num.category + 1:num.category
  #It identifies the situations that unfold from situation 2.
  situation.1 <- pos.next.level[situation.1]
  #It identifies the positions that unfold from situation 1.
  situation.2 <- pos.next.level[situation.2]
  #It identifies the positions that unfold from situaion 2.
  aux <- sum(situation.1 == situation.2)
  if (aux != num.category)
    return(FALSE) else return(TRUE)
}
