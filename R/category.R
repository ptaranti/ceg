

#' Category S4 Class
#'
#' Category S4 class contains a sigle slot with the category label. It is used
#' to construct Stratified.event.tree objects.
#'
#' @slot label character   TODO(Colazzo) Ampliar com tipo de dado e significado semantico
#'
#' @export
#'
setClass(
  "Category",
  representation(
    label = "character"),
  validity = function(object) {
       if (object@label == "") stop("Category@label cannot be void")
       if (grepl("^\\s*$", object@label)) stop("Category@label cannot be only blanc spaces")
    TRUE
  }
)



setMethod(
  f = "initialize",
  signature = "Category",
  definition = function(.Object,
                        label){
    cat("~~~ Category: initializator ~~~ \n")
    # Assignment of the slots
    .Object@label <- label
    methods::validObject(.Object)
    return(.Object)
    # return of the object
  }
)

#' Category(label)
#'
#' Category(label) is a function that act as constructor to Category S4 object.
#' Category S4 class contains a sigle slot with the category label. It is used
#' to construct S4 Variable objects, which, in turn, aim to be parameter in
#' Stratified.event.tree objects manual constructions.
#'
#' @param label character the category name
#'
#' @return a \code{\link{Category}} S4 object
#'
#' @export
#'
#' @examples
#' \dontrun{cat <- Category(category.name)}
#' \dontrun{Category(category.name2)}
#'
#'
Category <- function(label){
          new("Category", label)
    }






