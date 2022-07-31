###############################################################################
# Package shiny
# Package used to build web base UI.
# Practice
# Albert Millan
# 7/29/22
###############################################################################
# Install packages
install.packages("shiny")
install.packages("factoextra")
install.packages("shiny")

# runExample("09_upload") 
# runExample("10_download")
# runExample("05_sliders")
###############################################################################
library(shiny)
library(factoextra)
library(tidyverse)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PCA Cluster"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 2,
      
      # Input: Select a file ----
      fileInput("file1", "Upload CSV Data File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select a file ----
      fileInput("file2", "Upload Metadata File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      selectInput("indiv", "Select Individuals", choices = NULL, multiple = TRUE),
      
      # Horizontal line ----
      tags$hr(),
      
      selectInput("var", "Select Variables", choices = NULL, multiple = TRUE),
      
      # Horizontal line ----
      tags$hr(),
      
      actionButton("update", "Update View"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      column(width = 6,
             # Output: plot ----
             plotOutput(outputId = "distPlot")
      ),
      column(width = 6,
             plotOutput(outputId = "distPlot4")
      ),
      column(width = 6,
             plotOutput(outputId = "distPlot2")
      ),
      column(width = 6,
             plotOutput(outputId = "distPlot3")
      ),
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  observe({
    req(input$file1)
    df <- read.csv(input$file1$datapath, row.names = 1, header = TRUE)
    df.t <- na.omit(t(df[-1,]))
    df.t <- df.t[ , which(apply(df.t, 2, var) != 0)]
    updateSelectInput(inputId = "indiv", choices = colnames(df))
    updateSelectInput(inputId = "var", choices = colnames(df.t))
  })
  
  formData <- eventReactive(input$update, {
    req(input$file1)
    df <- read.csv(input$file1$datapath, row.names = 1, header = TRUE)
    df<- df[,input$indiv]
    df.t <- na.omit(t(df[-1,]))
    df.t <- df.t[ , which(apply(df.t, 2, var) != 0)]
    res.pca <- prcomp(df.t, scale = TRUE)
    res.pca
  })
  
  metaData <- eventReactive(input$update, {
    req(input$file2)
    metadata <- read.csv(input$file2$datapath, row.names = 1, header = TRUE)
    metadata <- filter(metadata, rownames(metadata) %in% input$indiv)
    metadata$Group
  })
  
  output$distPlot <- renderPlot({
    
    fviz_eig(formData(), main = "Eigenvalues against the number of dimensions")
    
  })
  
  output$distPlot2 <- renderPlot({
    
    fviz_pca_var(formData(),
                 col.var = "contrib", # Color by contributions to the PC
                 select.var = list(name = input$var),
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE     # Avoid text overlapping
    )
  })
  output$distPlot3 <- renderPlot({
    
    fviz_pca_biplot(formData(), repel = TRUE,
                    select.var = list(name = input$var),
                    col.var = "#2E9FDF", # Variables color
                    col.ind = "#696969"  # Individuals color
    )
  })
  output$distPlot4 <- renderPlot({
    
    fviz_pca_ind(formData(),
                 habillage = metaData(),
                 addEllipses = TRUE,
                 ellipse.type = "confidence",
                 legend.title = "Groups",
                 repel = TRUE     # Avoid text overlapping
    )
  })
}

# Create Shiny app ----
shinyApp(ui, server)

