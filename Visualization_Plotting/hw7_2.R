edges <- read.csv("edges.csv")
users <- read.csv("users.csv")

library(igraph)

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

degree(g)
table(degree(g) >= 10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "green"
V(g)$color[V(g)$school == "AB"] = "blue"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "blue"
plot(g, vertex.label=NA)

?igraph.plotting 
rglplot(g)
plot(g, vertex.label=NA, edge.width=5)





