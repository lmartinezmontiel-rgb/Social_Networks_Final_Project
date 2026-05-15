# Shiny App 
# Section 1. First install and activate all your required packages. 

library(shiny) 
library(bslib)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(visNetwork)

#Creating graphs

STD_Edges <- read_csv("Desktop/Shiny APP Stuff/STD_Edges_Data_FIXED.csv")
STD_Nodes<- read_csv("Desktop/Shiny APP Stuff/STD_Nodes_Data_Actully_Fixed.csv")


STD_Full <- graph_from_data_frame(d=STD_Edges, vertices = STD_Nodes, directed = FALSE)

###################################################################################################
STD_Full_Cluster <- as_tbl_graph(STD_Full)

STDCluster_L <- STD_Full_Cluster |> activate(nodes) |> 
  mutate(
    cluster     = group_louvain(),
    degree      = centrality_degree(),
    betweenness = centrality_betweenness())


Cluster<- STDCluster_L |>  activate(nodes) |> as_tibble()

table(Cluster$cluster, Cluster$Gender)

table(Cluster$cluster, Cluster$Wealth)

ggraph(STDCluster_L, layout = "fr") +
  geom_edge_link(alpha = .3) +
  geom_node_point(aes(color = as.factor(cluster)), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3)+
  labs(
    color = "Cluster",
    title = "Louvain Clustering of Social Network")+
  theme_graph()

###################################################################################################

STDCluster_L |>
  activate(nodes) |> mutate(degree = centrality_degree()) |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(size = degree, color = Wealth)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph() +
  labs(
    title = "Degree Centrality/Wealth Network",
    size = "Degree",
    color = "Wealth"
  )

###################################################################################################
#filtering for episodes

# Filter for ep1

STD_Edges_Ep1 <- STD_Edges |>
  filter(Episode == 1)

STD_Ep1 <- graph_from_data_frame(
  d = STD_Edges_Ep1,
  vertices = STD_Nodes,
  directed = FALSE)


#filtering for ep20

STD_Edges_Ep20 <- STD_Edges |>
  filter(Episode == 20)

STD_Ep20 <- graph_from_data_frame(
  d = STD_Edges_Ep20,
  vertices = STD_Nodes,
  directed = FALSE)

#filtering for ep40

STD_Edges_Ep40 <- STD_Edges |>
  filter(Episode == 40)

STD_Ep40 <- graph_from_data_frame(
  d = STD_Edges_Ep40,
  vertices = STD_Nodes,
  directed = FALSE)

#filtering for ep60

STD_Edges_Ep60 <- STD_Edges |>
  filter(Episode == 60)

STD_Ep60 <- graph_from_data_frame(
  d = STD_Edges_Ep60,
  vertices = STD_Nodes,
  directed = FALSE)


#filtering for ep80

STD_Edges_Ep80 <- STD_Edges |>
  filter(Episode == 80)

STD_Ep80 <- graph_from_data_frame(
  d = STD_Edges_Ep80,
  vertices = STD_Nodes,
  directed = FALSE)


#filtering for ep100

STD_Edges_Ep100 <- STD_Edges |>
  filter(Episode == 100)

STD_Ep100 <- graph_from_data_frame(
  d = STD_Edges_Ep100,
  vertices = STD_Nodes,
  directed = FALSE)

#filtering for ep120

STD_Edges_Ep120 <- STD_Edges |>
  filter(Episode == 120)

STD_Ep120 <- graph_from_data_frame(
  d = STD_Edges_Ep120,
  vertices = STD_Nodes,
  directed = FALSE)



#filtering for ep140

STD_Edges_Ep140 <- STD_Edges |>
  filter(Episode == 140)

STD_Ep140 <- graph_from_data_frame(
  d = STD_Edges_Ep140,
  vertices = STD_Nodes,
  directed = FALSE)


#filtering for ep142

STD_Edges_Ep142 <- STD_Edges |>
  filter(Episode == 142)

STD_Ep140 <- graph_from_data_frame(
  d = STD_Edges_Ep142,
  vertices = STD_Nodes,
  directed = FALSE)

###################################################################################################

#creating graphs

#Ep1
chars_ep1 <- unique(c(STD_Edges_Ep1$Source,
                      STD_Edges_Ep1$Target))

STD_Nodes_Ep1 <- STD_Nodes[STD_Nodes$Names %in% chars_ep1, ]

STD_Ep1 <- graph_from_data_frame(
  d = STD_Edges_Ep1,
  vertices = STD_Nodes_Ep1,
  directed = FALSE)

STD_Ep1 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()

#Ep20

chars_ep20 <- unique(c(STD_Edges_Ep20$Source,
                      STD_Edges_Ep20$Target))

STD_Nodes_Ep20 <- STD_Nodes[STD_Nodes$Names %in% chars_ep20, ]

STD_Ep20 <- graph_from_data_frame(
  d = STD_Edges_Ep20,
  vertices = STD_Nodes_Ep20,
  directed = FALSE)

STD_Ep20 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()

#Ep40
chars_ep40 <- unique(c(STD_Edges_Ep40$Source,
                       STD_Edges_Ep40$Target))

STD_Nodes_Ep40 <- STD_Nodes[STD_Nodes$Names %in% chars_ep40, ]

STD_Ep40 <- graph_from_data_frame(
  d = STD_Edges_Ep40,
  vertices = STD_Nodes_Ep40,
  directed = FALSE)

STD_Ep40 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()


#Ep60

chars_ep60 <- unique(c(STD_Edges_Ep60$Source,
                       STD_Edges_Ep60$Target))

STD_Nodes_Ep60 <- STD_Nodes[STD_Nodes$Names %in% chars_ep60, ]

STD_Ep60 <- graph_from_data_frame(
  d = STD_Edges_Ep60,
  vertices = STD_Nodes_Ep60,
  directed = FALSE)

STD_Ep60 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()


#Ep80

chars_ep80 <- unique(c(STD_Edges_Ep80$Source,
                       STD_Edges_Ep80$Target))

STD_Nodes_Ep80 <- STD_Nodes[STD_Nodes$Names %in% chars_ep80, ]

STD_Ep80 <- graph_from_data_frame(
  d = STD_Edges_Ep80,
  vertices = STD_Nodes_Ep80,
  directed = FALSE)

STD_Ep80 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()

#Ep100

chars_ep100 <- unique(c(STD_Edges_Ep100$Source,
                       STD_Edges_Ep100$Target))

STD_Nodes_Ep100 <- STD_Nodes[STD_Nodes$Names %in% chars_ep100, ]

STD_Ep100 <- graph_from_data_frame(
  d = STD_Edges_Ep100,
  vertices = STD_Nodes_Ep100,
  directed = FALSE)

STD_Ep100 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()


#Ep120
chars_ep120 <- unique(c(STD_Edges_Ep120$Source,
                       STD_Edges_Ep120$Target))

STD_Nodes_Ep120 <- STD_Nodes[STD_Nodes$Names %in% chars_ep120, ]

STD_Ep120 <- graph_from_data_frame(
  d = STD_Edges_Ep120,
  vertices = STD_Nodes_Ep120,
  directed = FALSE)

STD_Ep120 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()


#Ep140
chars_ep140 <- unique(c(STD_Edges_Ep140$Source,
                       STD_Edges_Ep140$Target))

STD_Nodes_Ep140 <- STD_Nodes[STD_Nodes$Names %in% chars_ep140, ]

STD_Ep140 <- graph_from_data_frame(
  d = STD_Edges_Ep140,
  vertices = STD_Nodes_Ep140,
  directed = FALSE)

STD_Ep140 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()

#Ep142

chars_ep142 <- unique(c(STD_Edges_Ep142$Source,
                       STD_Edges_Ep142$Target))

STD_Nodes_Ep142 <- STD_Nodes[STD_Nodes$Names %in% chars_ep142, ]

STD_Ep142 <- graph_from_data_frame(
  d = STD_Edges_Ep142,
  vertices = STD_Nodes_Ep142,
  directed = FALSE)

STD_Ep142 |>
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .3, color = "black") +
  geom_node_point(aes(color = Gender)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_graph()


########################################################################################

#alleps

gender_counts <- table(STD_Nodes$Gender)

barplot(gender_counts,
        main = "All Number of Males and Females",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


#Ep1
gender_counts1 <- table(STD_Nodes_Ep1$Gender)

barplot(gender_counts1,
        main = "Number of Males and Females in Ep1",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


#Ep20
gender_counts20 <- table(STD_Nodes_Ep20$Gender)

barplot(gender_counts20,
        main = "Number of Males and Females in Ep20",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


#Ep40
gender_counts40 <- table(STD_Nodes_Ep40$Gender)

barplot(gender_counts40,
        main = "Number of Males and Females in Ep40",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


#Ep60
gender_counts60 <- table(STD_Nodes_Ep60$Gender)

barplot(gender_counts60,
        main = "Number of Males and Females in Ep60",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


#Ep80

gender_counts80 <- table(STD_Nodes_Ep80$Gender)

barplot(gender_counts80,
        main = "Number of Males and Females in Ep80",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


#Ep100

gender_counts100 <- table(STD_Nodes_Ep100$Gender)

barplot(gender_counts100,
        main = "Number of Males and Females in Ep100",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


#Ep120

gender_counts120 <- table(STD_Nodes_Ep120$Gender)

barplot(gender_counts120,
        main = "Number of Males and Females in Ep120",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))



#Ep140

gender_counts140 <- table(STD_Nodes_Ep140$Gender)

barplot(gender_counts140,
        main = "Number of Males and Females in Ep140",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


#Ep142
gender_counts142 <- table(STD_Nodes_Ep142$Gender)

barplot(gender_counts142,
        main = "Number of Males and Females in Ep142",
        xlab = "Gender",
        ylab = "Count",
        col = c("lightblue", "pink"))


gender_counts142



