# sample of collapsible tree using edge, node datasets

library(data.tree)
library(magrittr)
library(plyr)
library(dplyr)

### node dataset ###
d_node = data.frame(node = c(0, 1, 2, 3, 4), 
                    type1 = c("country", "state", "state", "city", "city"), # attributes
                    stringsAsFactors = FALSE)

### edge dataset ###
d_edge = data.frame(from = c(-1, 0, 0, 1, 2), to = c(0, 1, 2, 3, 4))

d_comb = d_edge %>% left_join(d_node, by = c("to" = "node"))
a = FromDataFrameNetwork(d_comb)
a$Get("type1")
b = Clone(a$children[[1]])
b$Get("type1")

# plotting a collapsibleTree
# c = ToDataFrameTypeCol(b, type = "type1")

library(collapsibleTree)

collapsibleTree1 <- function(node, hierarchy ,root = deparse(substitute(df)),
                            inputId = NULL, width = NULL, height = NULL,
                            attribute = "leafCount", aggFun = sum,
                            fill = "lightsteelblue", fillByLevel = TRUE,
                            linkLength = NULL, fontSize = 10, tooltip = FALSE) {
  
  # preserve this name before evaluating df
  root <- root
  
  # reject bad inputs
  if(!(is(node) %in% "Node")) stop("node must be a data tree")
  if(!is.character(hierarchy)) stop("hierarchy must be a character vector")
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
  if(tooltip) {
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
  } else jsonFields <- "fill"
  
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

# incorrect
collapsibleTree(df = d_comb,
                hierarchy = c("from", "to"))

# correct
collapsibleTree1(node = b,
                 hierarchy = c("country", "city", "state"),
                 attribute = "type1",
                 tooltip = TRUE,
                 aggFun = I)


