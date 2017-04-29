# sample of collapsible tree using edge, node datasets
library(data.tree)
library(networkD3)
library(igraph)
acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")

acmeNetwork <- ToDataFrameNetwork(acme, "name")
simpleNetwork(acmeNetwork[-3], fontSize = 12)
acmeIgraph <- as.igraph.Node(acme)


as.data.frame.Node(acme)


### node dataset ###

### edge dataset ###
d_edge = data.frame(from = c(0, 0, 1, 2), to = c(1, 2, 3, 4))
d_node = data.frame(node = c(0, 1, 2, 3, 4), 
                    # type1 = c("1", "2", "2", "3", "3"))
                    type1 = c("country", "state", "state", "city", "city"))
# ,
                    # stringsAsFactors = FALSE)
d_comb = d_edge %>% left_join(d_node, by = c("to" = "node"))
a = FromDataFrameNetworkTemp(d_comb)
a$Get("type1", format = as.factor)

ig = graph_from_data_frame(d = d_edge, vertices = d_node)

nd3 = igraph_to_networkD3(ig)

forceNetwork(Links = d_edge, Nodes = d_node, Source = "from",
             Target = "to", NodeID = "node", Group = "level",
             opacity = 9, zoom = TRUE)




library(data.tree)

d_edge = data.frame(from = c(-1, 0, 0, 1, 2), to = c(0, 1, 2, 3, 4))
d_node = data.frame(node = c(0, 1, 2, 3, 4), 
                    type1 = c("country", "state", "state", "city", "city")) # attributes
d_comb = d_edge %>% left_join(d_node, by = c("to" = "node"))
a = FromDataFrameNetwork(d_comb)

a$Get("type1", format = as.factor)
b = Clone(a$children[[1]])
b$Get("type1", format = as.factor)
