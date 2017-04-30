## List of packages required to be able to run markdown files

# install.packages("collapsibleTree")
# install.packages("networkD3")

library(plyr)
library(dplyr)
library(collapsibleTree)


# collapsibleTree function updated to work directly on a data.tree
collapsibleTree1 <- function(node, hierarchy ,root = deparse(substitute(df)),
                             inputId = NULL, width = NULL, height = NULL,
                             attribute = "leafCount", aggFun = NULL, 
                             fill = "lightsteelblue", fillByLevel = TRUE,
                             linkLength = NULL, fontSize = 10, tooltip = FALSE) {
  
  # preserve this name before evaluating df
  root <- root
  
  # reject bad inputs
  if(!(is(node) %in% "Node")) stop("node must be a data tree")
  if(!is.character(fill)) stop("fill must be a character vector")
  if(length(hierarchy) <= 1) stop("hierarchy vector must be greater than length 1")
  # if(!all(hierarchy %in% colnames(df))) stop("hierarchy column names are incorrect")
  if(!(attribute %in% c(names(node), "leafCount"))) stop("attribute column name is incorrect")
  # if(sum(complete.cases(df[hierarchy])) != nrow(df)) stop("NAs in data frame")
  
  # calculate the right and left margins in pixels
  leftMargin <- nchar(root)
  # rightLabelVector <- as.character(df[[hierarchy[length(hierarchy)]]])
  rightMargin <- 1#max(sapply(rightLabelVector, nchar))
  
  # create a list that contains the options
  options <- list(
    hierarchy = hierarchy,
    input = inputId,
    attribute = attribute,
    linkLength = linkLength,
    fontSize = fontSize,
    tooltip = tooltip,
    margin = list(
      top = 20,
      bottom = 20,
      left = (leftMargin * fontSize/2) + 25,
      right = (rightMargin * fontSize/2) + 25
    )
  )
  
  
  # fill in the node colors, traversing down the tree
  if(length(fill)>1) {
    if(length(fill) != node$totalCount) {
      stop(paste("Expected fill vector of length", node$totalCount, "but got", length(fill)))
    }
    node$Set(fill = fill, traversal = ifelse(fillByLevel, "level", "pre-order"))
  } else {
    options$fill <- fill
  }
  
  # only necessary to perform these calculations if there is a tooltip
  if(tooltip & !is.null(aggFun)) {
    # traverse down the tree and compute the weights of each node for the tooltip
    t <- data.tree::Traverse(node, "pre-order")
    data.tree::Do(t, function(x) {
      x$WeightOfNode <- data.tree::Aggregate(x, attribute, aggFun)
      # make the tooltips look nice
      x$WeightOfNode <- prettyNum(
        x$WeightOfNode, big.mark = ",", digits = 3, scientific = FALSE
      )
    })
    jsonFields <- c("fill", "WeightOfNode")
  } else if(tooltip & is.null(aggFun)){
    node$Do(function(self) self$WeightOfNode = self[[attribute]])
    jsonFields <- c("fill", "WeightOfNode")
  } else
    jsonFields <- c("fill")
  
  # keep only the fill attribute in the final JSON
  data <- data.tree::ToListExplicit(node, unname = TRUE, keepOnly = jsonFields)
  
  # pass the data and options using 'x'
  x <- list(
    data = data,
    options = options
  )
  
  # create the widget
  htmlwidgets::createWidget(
    "collapsibleTree", x, width = width, height = height,
    htmlwidgets::sizingPolicy(viewer.padding = 0)
  )
}
