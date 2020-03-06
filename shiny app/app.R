library(shiny)
library(shinyWidgets)
library(ggplot2)

# A dataframe with NMDS "coordinates" and mapping data

withmap <- read.table("scores.csv", header=TRUE, sep=",")

variables <- c("Treatment Type"="trt_type", "Site"="site_ID", "Sample Period"="sample_period",
               "Crop"="crop")

# Define server logic

server <- function(input, output) {
  
  data <- reactive({
    if("X16S" %in% input$var) return(withmap$X16S2)
    if("nrfa11" %in% input$var) return(withmap$nrfa12)
    if("nrfa21" %in% input$var) return(withmap$nrfa22)
    if("nosz1" %in% input$var) return(withmap$nosz2)
    if("nirs1" %in% input$var) return(withmap$nirs2)
    if("nirk1" %in% input$var) return(withmap$nirk2)
    
  })
  
  dataset <- reactive({
    withmap
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$var, y=data())) + geom_point() +
      
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            legend.text=element_text(size=16),
            legend.title=element_text(size=18)) +
      
      guides(color= guide_legend(nrow=3,byrow=TRUE, title=names(variables[which(variables == input$color)]))) +
      
      theme(panel.background = element_rect(fill="#f5f5f5")) +
      
      theme(plot.background = element_rect(fill="#f5f5f5")) + #, linetype="solid", color="red")) +
      theme(legend.background=element_rect(fill="#f5f5f5")) +
      theme(legend.position="bottom") 
    
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    
    
    print(p)
    
  }, height=600)
  
}


# UI Part

dataset <- withmap

ui <- fluidPage(
  
  #titlePanel(title=div(img(src="logo.svg"))),
  #setBackgroundColor(color="gray"),
  
  setBackgroundColor(color="#303030"),
  
  h1(id="big-heading", "Microbe Explorer"),
  tags$style(HTML("#big-heading{color: #d3d3d3;}")),


  
  # headerPanel("Data Explorer"),
  
  sidebarPanel(
    
    selectInput("var",
                label = "Choose an ordination to display:",
                choices = c("All microbes"="X16S", "DNRA (nrfA1)"="nrfa11", "DNRA (nrfA2)"="nrfa21",
                            "Denitrification (nirK)"="nirk1", "Denitrification (nirS)"="nirs1",
                            "Denitrification (nosZ)"="nosz1"),
                selected = "16S")  ,
    
    
    selectInput('color', 'Color', c("Treatment Type"="trt_type", "Site"="site_ID", "Sample Period"="sample_period",
                                    "Crop"="crop")),
    
    selectInput('facet_col', 'Facet Column', c(None='.', 'Site'="site_ID", 'Timepoint'="sample_period"))
  ),
    
  
  mainPanel(
    plotOutput('plot', width="100%")
  )
)

shinyApp(ui = ui, server = server)

