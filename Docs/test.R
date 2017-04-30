# sample of collapsible tree using edge, node datasets

library(data.tree)

### node dataset ###
d_node = data.frame(node = c(0, 1, 2, 3, 4), 
                    type1 = c("country", "state", "state", "city", "city")) # attributes

### edge dataset ###
d_edge = data.frame(from = c(-1, 0, 0, 1, 2), to = c(0, 1, 2, 3, 4))

d_comb = d_edge %>% left_join(d_node, by = c("to" = "node"))
a = FromDataFrameNetwork(d_comb)
a$Get("type1", format = as.factor)
b = Clone(a$children[[1]])
b$Get("type1", format = as.factor)

