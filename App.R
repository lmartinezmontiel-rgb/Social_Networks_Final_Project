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
library(shiny)
library(shinythemes)
#Creating graphs

#STD_Edges <- read_csv("Desktop/Shiny APP Stuff/STD_Edges_Data_FIXED.csv")
#STD_Nodes<- read_csv("Desktop/Shiny APP Stuff/STD_Nodes_Data_Actully_Fixed.csv")

STD_Edges <- read_csv("STD_Edges_Data_FIXED.csv")
STD_Nodes <- read_csv("STD_Nodes_Data_Actully_Fixed.csv")


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


###################################################################################################################
#User interface
ui <- navbarPage(
  theme = shinythemes::shinytheme("superhero"),
  "Soy Tu Duena App",
  
  # First navbar
  tabPanel("Background Info", 
           h1("Introduction"),
           p("For my final project, I decided to create a network based on the Spanish telenovela, Soy Tu Duena. I grew up watching these shows with my mom, and I recently got back into watching them.Simultaneously we also started with this project, so it just seemed fitting to base my project about it. Soy Tu Duena is about Valentina, who is engaged to Alonso; however, he leaves her standing at the altar on their wedding day. After this incident, she moved to a farm/plantation that her deceased parents owned, where she had a complete personality change from happy, positive to hard and short tempered. Later on she meets Jose Miguel and thier dramatic love story begins to unravel.  "),
           
           h1("Data Info"),
           p("For this project I deceide to calculate how many times characters interact with one another. Essentially, if person A talks to person B, then that would count as one. If the scence cuts back and forth between other scenes, but A and B are still in the same conversation, that would not count. If person C enters A and B's conversation then that would count another for all three of them. This show had 142 (so to keep my sanity), I decided to collect data from episodes from ep 1, 20, 40, 60 ,80, 100, 120, 140, 142. Besides how many time each character talks to each other, I also recorded thier gender, class statue, and episode number"),
           
           h1("Final Notes"),
           p("The biggest change I would make if I were to redo this project, is I would actully collect ALL the data from the show. Even with the small sample that I was able to collect from the few selected episodes, there is alot of intresting visuals/stats that would only improve with more data. Another downside of this is the obvious missing data and other characters. This leads to having weaker ties to other characters when in reality they are a lot closer. Also, while the weight is a good indicator of relationship strength to other characters, the tone of these conversations and the length of the conversations are missing. While having a greater weight, this doesn't instantly mean that those 2 characters are close. Along the same lines, characters can have a few conversations, but these conversations could have included deep emotional feelings that allowed them to grow closer. However, with this method of data collection, it does not capture these aspects. Lastly, the most intresting points that caught my eye was the clustering graph, I initally thought that there would be less clusters because my inital impression of the show was that everyone interacts even with the smaller charcters, so there would be fewer clusters because everyone interacts with everyone. However, after looking at the clusters it showed all the different plot lines in the show, which really opened my eyes on how the show is setup.")
           
           
           ),
  
  # Second navbar 
  tabPanel("Data",
           mainPanel(
             tabsetPanel(
               tabPanel("Vis Network",
                        h1("Interactive VisNetwork"),
                        selectInput(
                          "size_by",
                          "Size nodes by:",
                          list("Degree" = "degree", "Betweenness" = "betweenness")
                        ),
                        visNetworkOutput("int_network")
               ),
               tabPanel("Wealth/Degree Centrality Graph", "This graph is colored by the characters class status. An intresting finding about this graph is that most of the wealth people are the in the main plot line and have other significant roles in the other plot lines. While as the other lower class character are treated more of side characters.",
                        plotOutput("degree_plot")
                        ),
               tabPanel("Louvin Clustering", "This graph is colored by the cluster groups in the show. This graph is a good representation on the different plotlines the show follows and what characters are part of multiple plotlines",
                        plotOutput("cluster_plot")
                        ),
               tabPanel("Gender Graph",
                        p("In this graph you can choose from what episode you want to see data from. This graph specifically shows the gender breakdown between the episodes. Blue is for men and pink is for women. One discovery with this graph was episodes who had more women than men were, episodes with more dramatic or messy plotlines. I found this intresting because of the narrative about women being emotional and making reckless descisions because of thier emotions. This show aired in the early 2010's, this could have contributed to this narrative as back in the 2010's it was less progressive."),
                        selectInput(
                          "gender_episode",
                          "Select Episode:",
                          list("All Episodes" = "0", "Episode 1" = "1", "Episode 20" = "20", "Episode 40" = "40",
                               "Episode 60" = "60", "Episode 80" = "80", "Episode 100" = "100",
                               "Episode 120" = "120", "Episode 140" = "140", "Episode 142" = "142")
                        ),
                        plotOutput("gender_plot")
               ),
               tabPanel("Ep Networks", 
                        p("In this graph you can see specific network breakdowns of each episode. Blue is for women and Black is for men."),
                        selectInput(
                          "ep_network",
                          "Select Episode:",
                          list( "All Episodes" = "0", "Episode 1" = "1", "Episode 20" = "20", "Episode 40" = "40",
                               "Episode 60" = "60", "Episode 80" = "80", "Episode 100" = "100",
                               "Episode 120" = "120", "Episode 140" = "140", "Episode 142" = "142")
                        ),
                        plotOutput("ep_plot")
               )
             )
           )
  )
)

###################################################################################################################
#Server/data

server <- function(input, output) {
  
  #card 1
  output$cluster_plot <- renderPlot({
    ggraph(STDCluster_L, layout = "fr") +
      geom_edge_link(alpha = .3) +
      geom_node_point(aes(color = as.factor(cluster)), size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      labs(
        color = "Cluster",
        title = "Louvain Clustering of Social Network") +
      theme_graph()
  })
  
  #card 2
  output$degree_plot <- renderPlot({
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
  })
  
  #Card 3
  network2 <- reactive({
    
    nodes_df <- STDCluster_L |>
      activate(nodes) |>
      as_tibble() |>
      mutate(
        id = row_number(),
        label = name,
        value = ifelse(input$size_by == "degree", degree, betweenness),
        group = cluster
      )
    
    edges_df <- STDCluster_L |>
      activate(edges) |>
      as_tibble() |>
      rename(from = from, to = to)
    
    list(nodes = nodes_df, edges = edges_df)
  })
  
  output$int_network <- renderVisNetwork({
    net2 <- network2()
    nodes <- net2$nodes
    edges <- net2$edges 
    
    
    visNetwork(nodes, edges) |> 
      
      visNodes(borderWidth = 1, 
               color = list(
                 background= NULL, 
                 border = "green", 
                 highlight =  "white"))|>
      
      visEdges(
        color = list(color = "white", highlight = "green")) |> 
      
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE), 
        nodesIdSelection = FALSE) |>
      
      visInteraction(
        dragNodes = TRUE, 
        dragView = TRUE, 
        zoomView = TRUE) |> 
      
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -200), stabilization = TRUE)
    
  })
  ep_data <- function(ep_input) {
    reactive({
      ep <- as.numeric(ep_input())
      
      if (ep == 0) {
        edges_filtered <- STD_Edges
        nodes_filtered <- STD_Nodes
      } else {
        edges_filtered <- STD_Edges |> filter(Episode == ep)
        chars          <- unique(c(edges_filtered$Source, edges_filtered$Target))
        nodes_filtered <- STD_Nodes[STD_Nodes$Names %in% chars, ]
      }
      graph <- graph_from_data_frame(
        d        = edges_filtered,
        vertices = nodes_filtered,
        directed = FALSE
      )
      list(graph = graph, nodes = nodes_filtered)
    })
  }
  
  ep_network_data  <- ep_data(reactive(input$ep_network))
  ep_gender_data   <- ep_data(reactive(input$gender_episode))
  
  
  output$ep_plot <- renderPlot({
    ep_network_data()$graph |>
      ggraph(layout = "fr") +
      geom_edge_link(alpha = .3, color = "black") +
      geom_node_point(aes(color = Gender)) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      theme_graph() +
      labs(title = paste("Episode", input$ep_network, "Network"))
  })
  
  output$gender_plot <- renderPlot({
    nodes <- ep_gender_data()$nodes
    gender_counts <- table(nodes$Gender)
    
    ep_label <- if (input$gender_episode == "0") "All Episodes" 
    else paste("Episode", input$gender_episode)
    
    barplot(gender_counts,
            main = paste("Number of Males and Females —", ep_label),
            xlab = "Gender",
            ylab = "Count",
            col  = c("lightblue", "pink"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)