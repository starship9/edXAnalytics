edges <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/edges.csv")
users <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/users.csv")

str(users)
str(edges)
mean(edges$V1)

table(users$locale)
head(edges)
mean(edges$V1 + edges$V2)

table(users$school)

library(igraph)
g <-  graph.data.frame(edges, FALSE, users)
plot(g,vertex.size = 5,vertex.label = NA)

table(degree(g) >= 10)

V(g)$size  <-  degree(g)/2+2
plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)

V(g)$color <-  "black"

V(g)$color[V(g)$gender == "A"] <-  "red"

V(g)$color[V(g)$gender == "B"] <-  "gray"

V(g)$color[V(g)$school == "A"] <-  "red"

V(g)$color[V(g)$school == "AB"] <-  "blue"

V(g)$color[V(g)$locale == "A"] <-  "green"

V(g)$color[V(g)$locale == "B"] <-  "blue"
