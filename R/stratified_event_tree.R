

#' Stratified.event.tree S4 Class
#'
#' # TODO(Collazo) se considerar necessario, explicar o que é Stratified.event.tree
#'
#' @include event_tree.R
#'
#' @export
setClass(
  "Stratified.event.tree",
  representation(
    num.variable = "numeric",
    num.category = "numeric",
    label.category = "list",
    num.situation = "numeric",
    num.slice = "numeric",
    path = "list", # TODO(Collazo) Confirmar remover Path
    hyper.stage = "list"),
  contains = "Event.tree"
  #prototype(  )
)


setMethod(
  f = "initialize",
  signature = "Stratified.event.tree",
  definition = function(.Object,
                        num.variable,
                        num.category,
                        label.category,
                        num.situation,
                        num.slice,
                        path,
                        hyper.stage){
    cat("~~~ Stratified.event.tree: initializator ~~~ \n")
    # Assignment of the slots
    .Object@num.variable <- num.variable
    .Object@num.category <- num.category
    .Object@label.category <- label.category
    .Object@num.situation <- num.situation
    .Object@num.slice <- num.slice
    .Object@path <- path
    .Object@hyper.stage <- hyper.stage
    return(.Object)
    # return of the object
  }
)


#' Stratified.event.tree
#'
#' Constructor method to Stratified.event.tree S4 objects. It accepts different
#' sets for parameters types.
#'
#' @return a Stratified.event.tree S4 object
#' @export
#'
#'
setGeneric("Stratified.event.tree",
           function(x, ...) standardGeneric("Stratified.event.tree"),
           signature = "x")



#' @rdname Stratified.event.tree
#' @param Arguments (missing) \cr
#'  A call to \code{Stratified.event.tree( )} with no parameters will return
#'  an error message for missing argument.
#'
setMethod("Stratified.event.tree",
          signature("missing"),
          function(x) {
            stop("constructor S4 method Stratified.event.tree not implemented for missing argument")
          })

#' @rdname Stratified.event.tree
#' @param Arguments (ANY) \cr
#' A call to \code{Stratified.event.tree(x, ...)}, x not being a data.frame or
#' a list, will return an error message.
#'
setMethod("Stratified.event.tree",
          signature("ANY"),
          function(x, ...) {
            stop("constructor S4 method Stratified.event.tree not implemented for this argument")
          })

#' @rdname Stratified.event.tree
#' @param Arguments (data.frame) , where data.frame is a well behavioured data set.\cr
#'
setMethod("Stratified.event.tree",
          signature("data.frame"),
          function(x = "data.frame") {

            data.frame <- x

            num.variable <- length(data.frame[1, ])

            num.slice <- ncol(data.frame) / num.variable
            label.category <- lapply(1:num.variable,
                                     function(x)
                                       levels(factor(sapply(1:num.slice, function(y)
                                         levels(data.frame[, x + (y - 1) * num.variable])))))
            num.category <- c()
            num.category <- sapply(label.category, length)
            num.situation <- c(1, cumprod(num.category[1:(num.variable - 1)]))
            begin.stage <- c(0, cumsum(num.situation[1:(num.variable - 1)]))
            mergedlist <-
              sapply(1:(num.variable - 1), function(x)
                LabelStage(x, num.variable, num.situation, label.category, num.category))
            mergedlist <- lapply(1:(num.variable), function(x) {
              lapply(seq_len(num.situation[x]), function(y)
                mergedlist[y + begin.stage[x],])
            })

            hyper.stage <- list()

            return(new("Stratified.event.tree",
                       num.variable,
                       num.category,
                       label.category,
                       num.situation,
                       num.slice,
                       mergedlist,
                       hyper.stage))
          })



#' @rdname Stratified.event.tree
#' @param Arguments (list) , list of Variable S4 objects, in the expected order
#' of plotting. \cr
#' TODO(Collazo) VRF texto
#' @note
#' Consider a stratified.event.tree created using the following commands\cr
#' \code{input <- list(Variable("age",list(Category("old"), Category("medium"),
#' Category("new"))),Variable("state", list(Category("solid"),
#' Category("liquid"), Category("steam"))), Variable("costumer",
#' list(Category("good"), Category("average"), Category("very bad"),
#' Category("bad"))))} \cr
#' \code{et.manual <- Stratified.event.tree(input)} \cr
#'
setMethod("Stratified.event.tree",
          signature("list"),
          function(x = "list") {

            variable.list <- x
            if (is.null(variable.list))
              cat("list coul not be void")
            if (length(variable.list) == 0)
              cat("list coul not be void")
            for (variable in variable.list) {
              if (!is(variable, "Variable"))
                stop(" the input must be a list of ceg::Variable S4 objects")
            }

            num.variable <- length(variable.list)
            num.slice <- 1 # verificar se vai remover mesmo!!!!
            categories.list <- list()
            label.category <- list()
            # for()
            # while(...) {
            #   l[[i]] <- new.element
            #   i <- i + 1
            # }
            for (x in 1:num.variable) {


              variable <- as(variable.list[[x]], "Variable")


              temp.category.label.vector <- c()
              for (i in 1:length(variable@categories)) {
                category <- as(variable@categories[[i]], "Category")
                # print(category)


                temp.category.label.vector[[i]] <- category@label
              }
              label.category[[x]] <- temp.category.label.vector
            }

            num.category <- c()
            num.category <- sapply(label.category, length)

            num.situation <- c(1, cumprod(num.category[1:(num.variable - 1)]))
            begin.stage <- c(0, cumsum(num.situation[1:(num.variable - 1)]))
            mergedlist <-
              sapply(1:(num.variable - 1), function(x)
                LabelStage(x, num.variable, num.situation, label.category, num.category))
            mergedlist <- lapply(1:(num.variable), function(x) {
              lapply(seq_len(num.situation[x]), function(y)
                mergedlist[y + begin.stage[x], ])
            })

            hyper.stage <- list()
            path <- list()

            return(
              new(
                "Stratified.event.tree",
                num.variable,
                num.category,
                label.category,
                num.situation,
                num.slice,
                path,
                hyper.stage
              )
            )
          })



#' LabelStage
#'
#' This function identifies the edges arriving at the target level for paths
#' that exist from the root node to each situation in the event tree that
#' are in levels greater than the target level.
#'
#' @param    k    numeric
#' @param    num.variable
#' @param    num.situation
#' @param    label.category
#' @param    num.category
#' @return   label   a vector
#'
# @examples
#
#' @seealso   \code{\link{TruncatedPath}}
LabelStage <-
  function(k, num.variable, num.situation, label.category, num.category) {
    if (k > num.variable) {
      var <- k - num.variable
    } else {
      var <- k
    }
    label <-
      c(1, rep("NA", sum(num.situation[1:k]) - 1))
    #Label NA with regard each variable.
    label <-
      c(label, rep(label.category[[var]], num.situation[k]))
    # Classes of each variable. This sequence is repeated according to the
    # number of situations associated with a particular variable in the
    # event tree.
    if (k < (num.variable - 1)) {
      # Edges emanating from the descendent situations of each variable.
      # The levels of that variable are repeated in batch.
      label <-
        c(
          label, TruncatedPath(
            num.variable, k, var, num.category, num.situation, label.category
          )
        )
    }
    return(label)
  }





#' TruncatedPath
#'
#' This function identifies the edges arriving at the target level for all
#' paths that exist from the root node to each situation in the event tree.
#'
#' @param ref
#' @param k
#' @param var
#' @param num.category
#' @param num.situation
#' @param label.category
#'
# @return
# @export
#'
# @examples
TruncatedPath <- function(ref,
                           k,
                           var,
                           num.category,
                           num.situation,
                           label.category) {
  if (ref < k + 2) return(c())
  return(c(TruncatedPath(ref - 1, k, var, num.category,
                          num.situation, label.category),
           rep(label.category[[var]],
               each = num.situation[ref] / num.situation[k + 1],
               num.situation[k + 1] / num.category[var])))
}




#' Stratified.event.tree Plotting
#'
#' Method to plot a Stratified.event.tree S4 object. The current \code{ceg} package
#' implementation depends on \code{Rgraphviz} package from Bioconductor for
#' plotting.
#'
#' @param Stratified.event.tree
#'
#' @return the plot and also a pdf version is saved in the working directory.
#' @export
#'
#' @examples
#' plot(stratified.event.tree)
setMethod(
  f = "plot",
  signature = "Stratified.event.tree",
  definition = function(x,y,...){

    stratified.event.tree.graph <- StratifiedEventTreeGraph(x)


    g <- new("graphNEL", nodes = stratified.event.tree.graph$node$nodes, edgeL = stratified.event.tree.graph$edge$edges, edgemode = "directed")

    # 1.  Atributos do Grafico - Gerais
    attrsAtt <- list()
    graphAtt <- list(rankdir = "LR", size = "18.0,24.0", bgcolor = "white")  # o LR é que muda orientaçao
    edgeAtt <- list(color = "cyan")
    nodeAtt <- list(fillcolor = "lightgreen", shape = "ellipse", fixedsize = FALSE)
    attrsAtt <- list(node = nodeAtt, edge = edgeAtt, graph = graphAtt)

    #  2.  atributos de nós
    # mudando o nome de nós
    nodes.label.list <- stratified.event.tree.graph$node$nodes
    names(nodes.label.list) <- nodes(g)
    nAttrs <- list()
    #nAttrs$label <- c("s0"="rooooooot", "s2"="test")
    nAttrs$label <- nodes.label.list

    #  3.  atributos de arestas
    # mudando o nome de arestas (default é em branco)
    #opção 1 - atribuindo todos os nomes usando lista ordenada das arestas para atribuição
    #edgesLabelList <-c("nome-01", "nome-02","nome-03","nome-04","nome-05","nome-06","nome-07","nome-08","nome-09", "nome-10","nome-11", "nome-12","nome-13","nome-14","nome-15","nome-16","nome-17","nome-18","nome-19", "nome-20","nome-21", "nome-22","nome-23","nome-24","nome-25","nome-26","nome-27","nome-28","nome-29", "nome-30","nome-31", "nome-32","nome-33","nome-34","nome-35","nome-36","nome-37","nome-38","nome-39", "nome-40","nome-41", "nome-42")
    edges.label.list <- stratified.event.tree.graph$edge$label
    names(edges.label.list) <- edgeNames(g)
    eAttrs <- list()
    eAttrs$label <- edges.label.list

    # Inserindo cores
    nAttrs$fillcolor <- stratified.event.tree.graph$node$color
    names(nAttrs$fillcolor) <- nodes(g)
    graphics.off()
    par("mar")
    par(mar=c(1,1,1,1))
    Rgraphviz::plot(g, main = "Stratified Event Tree Graph", nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrsAtt)
    pdf("./stratified-event-tree-plot.pdf",  width = 8, height = 6, title = "")
    Rgraphviz::plot(g, main = "Stratified Event Tree Graph", nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrsAtt)
    dev.off()
  }
)


#' StratifiedEventTreeGraph
#'
#' @param event.tree  "Event.tree" S4 object
#'
#'  @return list contains a data structure to be used in a plot
#   @export
#'
# @examples
StratifiedEventTreeGraph <- function(event.tree){

  ###Data format to draw an event/staged tree
  ###Note that the difference between the event tree and stage tree are only to colour or not the nodes.
  #Nodes
  nodes <- NodeSet(event.tree)
  edgeList <- EdgeList(event.tree,nodes)

  #Edges
  node.label <- NodeLabel(event.tree@num.variable,event.tree@num.situation,event.tree@num.category,names(data))
  edge.label <- EdgeLabel(event.tree@num.variable,event.tree@num.situation,event.tree@label.category)

  total.node <- cumsum(event.tree@num.situation)
  node.color <- rep("white", total.node[event.tree@num.variable] +
                      event.tree@num.category[event.tree@num.variable] * event.tree@num.situation[event.tree@num.variable])

  #Final Format
  graph <- list()
  graph$node <- list()
  graph$node$nodes <- nodes
  graph$node$label <- node.label
  graph$node$color <- node.color
  graph$edge <- list()
  graph$edge$edges <- edgeList
  graph$edge$label <- edge.label
  graph$node$color <- rep("white", length(graph$node$color))
  return(graph)
}
