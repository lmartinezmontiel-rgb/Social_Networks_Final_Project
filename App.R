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

STD_Edges <- read_csv("STD_Edges_Data_FIXED.csv")
STD_Nodes<- read_csv("STD_Nodes_Data_Actully_Fixed.csv")

STD_Full <- graph_from_data_frame(d=STD_Edges, vertices = STD_Nodes, directed = FALSE)

###################################################################################################
STD_Full_Cluster <- as_tbl_graph(STD_Full)

STDCluster_L <- STD_Full_Cluster |> activate(nodes) |> 
  mutate(
    cluster     = group_louvain(),
    degree      = centrality_degree(),
    betweenness = centrality_betweenness()
  )


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


# Section 2. Design the site in the UI section (US = User Interface). This is where we define how everything looks and 
# how people can use the app. 

ui <-fluidPage(
  
  titlePanel("Social Networks Final Project"),
  
  page_sidebar(
    title = "Calculating interations between characters in the spanish Telenovela Soy Tu Duena",
    
    sidebar = sidebar(
      selectInput(
        inputId = "size_by",
        label = "Size nodes by:",
        choices = c("Degree" = "degree", "betweenness" = "betweenness"),
        selected = "degree"
      )
    ),
    
    card(
      card_header("In this final project I decied to calculate how many interactions each character has 
                  with one another, in the spanish telenovela, Soy Tu Duena. "), 
      "This telenovela has 142 episodes which, is alot, however I decided to only take data 
      from only from the first,last and every 20 episodes in between."),
    
    card(card_header("Visualization #1 - VisNetwork"),
         visNetworkOutput("int_network", height = "600px")),
    
    card(card_header("Visualization #2 - Degree Centrality & Wealth"),
         plotOutput("degree_plot", height = "600px")),
    
    card(card_header("Visualization #3 - Louvain Clustering"),
         plotOutput("cluster_plot", height = "600px"))
    
  )
)


# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

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
                 border = "red", 
                 highlight =  "purple"))|>
      
      visEdges(
        color = list(color = "purple", highlight = "black")) |> 
      
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE), 
        nodesIdSelection = FALSE) |>
      
      visInteraction(
        dragNodes = TRUE, 
        dragView = TRUE, 
        zoomView = TRUE) |> 
      
      visPhysics(stabilization = TRUE)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)