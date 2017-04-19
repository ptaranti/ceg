# TODO(collazo) invewrsao ordem do df  - para gerar erro:
#   df <- read.csv("./R/CHDS.latentexample1.csv")
#   df1 <- df[c(4,3,2,1)]
#   st  <- Stratified.staged.tree(df1)







#' Stratified.staged.tree
#'
#' #TODO(Collazo) Definir o que é stratified.staged.tree
#'
#' @include staged_tree.R event_tree.R stratified_event_tree.R
#'
#' #TODO(Collazo) espandir significado dos slots
#' @slot event.tree Stratified.event.tree.
#' @slot situation list.  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot contingency.table list.  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot stage.structure list.  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot stage.probability list.  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot prior.distribution list.  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot posterior.distribution list.  TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @slot model.score numeric.   TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#' @export
#'
setClass(
  "Stratified.staged.tree",
  representation(    situation = "list",
                     contingency.table = "list",
                     stage.structure = "list",
                     stage.probability = "list",
                     prior.distribution = "list",
                     posterior.distribution = "list",
                     model.score = "numeric"),
  contains = "Staged.tree",
  validity = function(object) {
    if (!methods::is(object@event.tree, "Stratified.event.tree"))
      stop("an instance of Stratified.event.tree S4 object must be provided to
           Stratified.staged.tree@event.tree slot ")
    else
      TRUE
  }
)


setMethod(
  f = "initialize",
  signature = "Stratified.staged.tree",
  definition = function(.Object,
                        event.tree = "Event.tree",
                        situation = "list",
                        contingency.table = "list",
                        stage.structure = "list",
                        stage.probability = "list",
                        prior.distribution = "list",
                        posterior.distribution = "list",
                        model.score = "numeric"
  ){
    cat("~~~ Stratified.staged.tree: initializator ~~~ \n")
    # Assignment of the slots
    .Object@event.tree <- event.tree
    .Object@situation <- situation
    .Object@contingency.table <- contingency.table
    .Object@stage.structure <- stage.structure
    .Object@stage.probability <- stage.probability
    .Object@prior.distribution <- prior.distribution
    .Object@posterior.distribution <- posterior.distribution
    .Object@model.score <- model.score
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)



#' Stratified.staged.tree
#'
#' Constructor method to Stratified.staged.tree S4 objects. It accepts different
#' sets for parameters types.
#'
#' @return a Stratified.staged.tree S4 object
#' @export
#'
#'
setGeneric("Stratified.staged.tree",
           function(x, y, z, ...) standardGeneric("Stratified.staged.tree")
)

#' @rdname Stratified.staged.tree
#' @param Arguments (missing) \cr
#'  A call to \code{Stratified.staged.tree( )} with no parameters will return
#'  an error message for missing argument.
#'
setMethod("Stratified.staged.tree",
          signature("missing"),
          function(x, ...) {
            stop("constructor S4 method Stratified.staged.tree not implemented for missing argument")
          })

#' @rdname Stratified.staged.tree
#' @param Arguments (ANY) \cr
#' A call to \code{Stratified.staged.tree(x, ...)}, x not being a data.frame or
#' a Event.tree, will return an error message.
#'
setMethod("Stratified.staged.tree",
          signature(x = "ANY"),
          function(x, ...) {
            stop("constructor S4 method Stratified.staged.tree not implemented
                 for this argument")
          })

#' @rdname Stratified.staged.tree
#' @param Arguments (data.frame, alpha, variable.order) , where data.frame is a well
#' behavioured data set, and the numeric values represent the alpha and
#' the variable order, respectively.\cr
#' The implementation admits providing the three arguments, or the first two, or
#'  even only the data.frame.\cr
#'  The default variable order is as in the data.frame and the default alpha is
#'  1L.
#'
setMethod("Stratified.staged.tree",
          signature( x = "data.frame", y = "numeric", z = "numeric"),
          function(x = "data.frame", y = 1L, z = 0L ) {
            data.frame <- x
            alpha <- y
            variable.order <- z

            if (!CheckForCleanData(data.frame)) {
              stop("Consider using CheckAndCleanData() function")
            }

            num.variable <- length(data.frame[1, ])
            # TODO(Taranti)  inserir validador de menor valor alpha (maior ou igual a 0)
            if (variable.order[1] != 0) {
              data.frame <- data.frame[variable.order]
            }

            event.tree <- Stratified.event.tree(data.frame)
            prior.distribution <- PriorDistribution(event.tree, alpha)
            contingency.table <- ContingencyTable(data.frame, event.tree)
            stage.structure <- lapply(1:(num.variable), function(x) OAHC(x, prior.distribution, contingency.table, event.tree))
            model.score <- sum(sapply(1:(num.variable), function(y) stage.structure[[y]]@score))


            out <-  new("Stratified.staged.tree",
                        event.tree,
                        situation = list(),
                        contingency.table,
                        stage.structure,
                        stage.probability = list(),
                        prior.distribution,
                        posterior.distribution = list(),
                        model.score)
            return(out)

          })

#' @rdname Stratified.staged.tree
setMethod("Stratified.staged.tree",
          signature( x = "data.frame", y = "numeric", z = "missing"),
          function(x = "data.frame", y = 1L) {  Stratified.staged.tree(x, y, 0 )}
)

#' @rdname Stratified.staged.tree
setMethod("Stratified.staged.tree",
          signature( x = "data.frame", y = "missing", z = "missing"),
          function(x = "data.frame") {  Stratified.staged.tree(x, 1, 0 )}
)


#' @rdname Stratified.staged.tree
#' @param Arguments (Stratified.event.tree, list)
#' The list represents the stage.structure. To construct it, the user must plot
#' the Stratified.event.tree graph and use the labelled number of each node. \cr
#' An example is provided in the note.
#' @note
#' Consider a stratified.event.tree created using the folloing commands\cr
#' \code{input <- list(Variable("age",list(Category("old"), Category("medium"),
#' Category("new"))),Variable("state", list(Category("solid"),
#' ("liquid"), Category("steam"))), Variable("costumer", list(Category("good"),
#' Category("average"), Category("very bad"), Category("bad"))))} \cr
#' \code{et.manual <- Stratified.event.tree(input)} \cr
#' plot the graph using the command\cr
#' \code{plot(et.manual)}\cr
#' Looking the graph, you can create the stage structure, such as follows:\cr
#' \code{stage.structure <- list(list(c(2,3)), list(c(4,7,12),c(5,8,9)))}\cr
#' Finally you can create your sStratified.event.tree:\cr
#' \code{st.manual <- Stratified.staged.tree(et.manual, stage.structure)}
setMethod("Stratified.staged.tree",
          signature(x = "Stratified.event.tree", y = "list" ),
          function(x = "Stratified.event.tree", y = "list") {

            event.tree <- x
            stage.structure <- y

            situation <-  list()
            contingency.table <-  list()
            stage.probability <-  list()
            prior.distribution <-  list()
            posterior.distribution <-  list()
            model.score <-  1

            # stage.structure <-  list()
            # logica:
            #verificar se a arvore esta corretya
            #verificar se a stage.structure nao tem mais itens que variavel menos 1 ( ultimo nivel)



            #para cada nnivel, ver numero de nos,
            #  para cada vetor
            #    ver se nao excede,
            #     crias lista de inteiro com N elementos com valores int = N
            #      colocar o vetor ordenado no no correto
            #        preencher com logic NA demais posicoes relativas ao vetor
            # Criar objetos OACH
            #
            # inserir na lista

            #a contagem inicia em 1 para o primeiro elemento da variavel. 01 aohc para cada variavel.


            PrepareStage <- function(nr.elementos, cluster.list){
              out <- as.list(1:nr.elementos)
              for (cluster in cluster.list) {
                cluster.sorted <- sort(cluster)
                out[[cluster.sorted[[1]]]] <- cluster.sorted
                for (x in 2:length(cluster.sorted))
                  out[[cluster.sorted[[x]]]] <- NA
              }
              out
            }



            out.stages <- vector("list", event.tree@num.variable)

            out.stages[[1]] <-  new("OAHC", 0, list(1))

            for (nr  in 1:length(stage.structure)) {


              correction.number <- sum(utils::head(event.tree@num.situation,nr)) - 1

              temp.stage <- lapply(stage.structure[[nr]], function(x) x - correction.number)




              out.stages[[nr + 1]] <- new("OAHC", 0, PrepareStage(event.tree@num.situation[[nr + 1]], temp.stage))

            }

            stage.structure <- out.stages

            return(
              new(
                "Stratified.staged.tree",
                event.tree,
                situation,
                contingency.table,
                stage.structure,
                stage.probability,
                prior.distribution,
                posterior.distribution,
                model.score
              ))

          }

)





#' Staged.tree Plotting
#'
#' Method to plot a Staged.tree S4 object. The current \code{ceg} package
#' implementation depends on \code{Rgraphviz} package from Bioconductor for
#' plotting.
#'
#' @param x Stratified.staged.tree S4 object
#'
#' @return the plot and also a pdf version is saved in the working directory.
#' @export
#'
#' @examples
  #' \dontrun{plot(stratified.staged.tree)}
setMethod(
  f = "plot",
  signature = "Stratified.staged.tree",
  definition = function(x){
    #staged.tree.graph <- tree.graph(x)
    staged.tree.graph <- TreeGraph(x@event.tree, x@stage.structure)


    g <- new(
      "graphNEL",
      nodes = staged.tree.graph$node$nodes,
      edgeL = staged.tree.graph$edge$edges ,
      edgemode = "directed"
      )

    # 1.  Atributos do Grafico - Gerais
    attrsAtt <- list()
    graphAtt <- list(rankdir = "LR", size = "18.0,24.0", bgcolor = "white")  # o LR é que muda orientaçao
    edgeAtt   <- list(color = "cyan")
    nodeAtt  <- list(fillcolor = "lightgreen", shape = "ellipse", fixedsize = FALSE)
    attrsAtt <- list(node = nodeAtt, edge = edgeAtt, graph = graphAtt)

    #  2.  atributos de nós
    # mudando o nome de nós
    nodesLabelList <- staged.tree.graph$node$nodes
    names(nodesLabelList) <- graph::nodes(g)
    nAttrs <- list()
    #nAttrs$label <- c("s0"="rooooooot", "s2"="test")
    nAttrs$label <- nodesLabelList

    #  3.  atributos de arestas
    # mudando o nome de arestas (default é em branco)

    #opção 1 - atribuindo todos os nomes usando lista ordenada das arestas para atribuição
    #edgesLabelList <-c("nome-01", "nome-02","nome-03","nome-04","nome-05","nome-06","nome-07","nome-08","nome-09", "nome-10","nome-11", "nome-12","nome-13","nome-14","nome-15","nome-16","nome-17","nome-18","nome-19", "nome-20","nome-21", "nome-22","nome-23","nome-24","nome-25","nome-26","nome-27","nome-28","nome-29", "nome-30","nome-31", "nome-32","nome-33","nome-34","nome-35","nome-36","nome-37","nome-38","nome-39", "nome-40","nome-41", "nome-42")
    edgesLabelList <- staged.tree.graph$edge$label
    names(edgesLabelList) <- graph::edgeNames(g)
    eAttrs <- list()
    eAttrs$label <- edgesLabelList

    # Inserindo cores
    nAttrs$fillcolor <- staged.tree.graph$node$color
    names(nAttrs$fillcolor) <- graph::nodes(g)

    Rgraphviz::plot(g, main = "Staged Tree Graph", nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrsAtt)
    grDevices::pdf("./staged.tree.graph.pdf",  width = 8, height = 6, title = "")
    Rgraphviz::plot(g, main = "Staged Tree Graph", nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrsAtt)
    grDevices::dev.off()
  }
)









#' NodeColor
#'
#' This function yields the node colors.
#'
#' @param num.variable  (numeric) - number of variables.
#' @param num.situation  (vector) - number of stages associated with each variable.
#' @param num.category (vector) - it identifies the number of edges that emanate from situations in each level.
#' @param stage.structure list with two components:
#'     \itemize{
#'     \item  numeric - score associated with a level
#'     \item  list of vectors - stage structure
#'     }
#' @param range.color  (numeric) - it chooses the color source. Default value 1L
#' a 8 color pallete will be used. Any other value will leads to a 501 colors
#' pallete usage.
#'
#' @return  vector - node colors
# @export
#'
NodeColor <- function(num.variable,
                      num.situation,
                      num.category,
                      stage.structure,
                      range.color) {
  total.node <- cumsum(num.situation)
  result <- rep("white", total.node[num.variable] +
                  num.category[num.variable] * num.situation[num.variable])
  count <- 2
  if (range.color == 1) {
    color <- grDevices::palette()
    color[1] <- "white"
  } else {
    color <- grDevices::colors(1)
    color <- color[-21]
  }
  for (i in 2:num.variable) {
    for (j in 1:num.situation[i]) {
      if (!is.na(stage.structure[[i]]@cluster[[j]][1])) {
        if (length(stage.structure[[i]]@cluster[[j]]) == 1)
          result[j + total.node[i - 1]] <- "white" else {
            result[stage.structure[[i]]@cluster[[j]] + total.node[i - 1]] <-
              color[count]
            count <- count + 1
          }
      }
    }
  }
  return(result)
}

