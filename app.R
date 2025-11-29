install.packages("shiny")
install.packages("bslib")
install.packages("tidyverse")
install.packages("stringr")
install.packages("ggvis")
install.packages("cluster")
install.packages("cluster.datasets")
install.packages("ggplot2")
install.packages("ggthemes")

library(shiny)
library(bslib)
library(tidyverse)
library(stringr)
library(ggvis)
library(cluster)
library(cluster.datasets)
library(ggplot2)
library(ggthemes)

BostonHousingDF <- read.csv("C:/Users/jmgre/OneDrive/Documents/Grad School/DS501/case-study-3/data/BostonHousing.csv")

# Drop first column (ID), ensure numeric
HouseDF <- BostonHousingDF[ , -1]
HouseDF <- as.data.frame(sapply(HouseDF, as.numeric))

# Clean missing/infinite values
HouseDF <- na.omit(HouseDF)
HouseDF <- HouseDF[apply(HouseDF, 1, function(x) all(is.finite(x))), ]

# Elbow method
wss <- numeric(15)
for (i in 1:15) {
  wss[i] <- kmeans(HouseDF, centers = i)$tot.withinss
}
sse <- data.frame(Clusters = 1:15, SSE = wss)

#visualize k-means analysis via cluster plot
ggplot(sse, aes(x = Clusters, y = SSE)) +
  geom_point(color = "blue") +
  geom_line() +
  theme_minimal() +
  labs(title = "Elbow Method for K-Means", x = "Number of Clusters", y = "SSE")

# Run k-means with 4 clusters
clusters <- kmeans(HouseDF, centers = 4)

HouseDF$Cluster <- clusters$cluster
head(HouseDF)
str(BostonHousingDF)

# Define palette for clusters
cluster_palette <- c("1" = "#1f77b4",
                     "2" = "#ff7f0e",
                     "3" = "#2ca02c",
                     "4" = "#d62728")


# Cluster plot
clusplot(HouseDF, clusters$cluster,
         color = TRUE, shade = FALSE, labels = 0, lines = 0,
         main = 'k-Means Cluster Analysis')


ggplot(HouseDF, aes(x = medv, y = rm, color = factor(Cluster))) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(
    title = "Boston Housing Clusters (avg # of rooms vs Value)",
    x = "Median value in $1000s (medv)",
    y = "Avg number of rooms per dwelling (rm)",
    color = "Cluster"
  ) +
  scale_color_manual(values = cluster_palette)

ggplot(HouseDF, aes(x = medv, fill = factor(Cluster))) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Median Value by Cluster", 
       fill = "Cluster") +
  scale_fill_manual(values = cluster_palette)

# Define UI for application
ui <- fluidPage(
  titlePanel("Boston Housing Cluster Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Choose plot:",
                  choices = c("Scatterplot size and values", "Density of clusters by value")),
      checkboxInput("smooth", "Add smooth line to scatterplot", value = TRUE),
      sliderInput("alpha", "Point/Density Transparency:", min = 0.1, max = 1, value = 0.7),
      checkboxGroupInput("clusters", "Select clusters to include:",
                         choices = c("1","2","3","4"), selected = c("1","2","3","4"))
    ),
    
    mainPanel(
      plotOutput("clusplot")
    )
  )
)

server <- function(input, output) {
  output$clusplot <- renderPlot({
    # Filter data reactively based on selected clusters
    df <- HouseDF[HouseDF$Cluster %in% input$clusters, ]
    
    if (input$plotType == "Scatterplot rm vs medv") {
      p <- ggplot(df, aes(x = medv, y = rm, color = Cluster)) +
        geom_point(alpha = input$alpha, size = 2) +
        theme_minimal() +
        labs(title = "Boston Housing Clusters (rm vs medv)",
             x = "Median value of owner-occupied homes in $1000's (medv)",
             y = "Avg number of rooms per dwelling (rm)",
             color = "Cluster") +
        scale_color_manual(values = cluster_palette)
      
      if (input$smooth) {
        p <- p + geom_smooth(se = FALSE)
      }
      p
      
    } else if (input$plotType == "Density of medv") {
      ggplot(df, aes(x = medv, fill = Cluster)) +
        geom_density(alpha = input$alpha) +
        labs(title = "Density of Median Value by Cluster", fill = "Cluster") +
        theme_minimal() +
        scale_fill_manual(values = cluster_palette)
    }
  })
}

shinyApp(ui, server)
