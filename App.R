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

install.packages("rsconnect")
library(rsconnect)
rsconnect::writeManifest()

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