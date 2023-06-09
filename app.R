library(shiny)
library(ggplot2)
library(ggpubr)
options(shiny.maxRequestSize=1000*1024^2) 

# Read the data (assuming the CSV is in the same directory as the Shiny app)
#data <- read.csv("2023-03-28_merged.csv")

# Extract unique collection dates
#unique_dates <- unique(data$Collection_date)

ui <- fluidPage(
  titlePanel("Wastewater Viral Enrichment Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("input_file", "Upload CSV", accept = c(".csv")),
      dateInput("date_selection", "Select Collection Date", value = Sys.Date())
    ),
    mainPanel(
      plotOutput("heatmap")
    )
  )
)
#selectInput("date_selection", "Select Collection Date", choices = unique_dates))

#---------------------------------------

server <- function(input, output) {
  # Read csv file
  data <- reactive({
    req(input$input_file)
    read.csv(input$input_file$datapath)
  })

  # create heatmap
  output$heatmap <- renderPlot({
    mdf <- data()

     # Filter the data based on the selected date
    mdf_filtered <- mdf[mdf$Collection_date == as.character(input$date_selection),]

    ggplot(mdf_filtered, aes(x = Virus, y = SampleID, fill = log10(Median_coverage))) +
      geom_tile() +
      xlab(label = "Virus") +
      labs(x = "Virus", y="Wastewater site") +
      scale_fill_gradient(low = "orange",
                          high = "darkgreen",
                          na.value = "grey80")+
       facet_grid(~ Collection_date ~ Virus, scales = "free", space = "free") +
  theme_pubclean() +
  theme(legend.position="right", axis.title=element_text(size=12),
    axis.text.y=element_text(size=8),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.background = element_rect(fill = "skyblue"), strip.text.x = element_blank())+ 
    rotate_x_text(angle = 45, hjust = 0.8, vjust = 0.8)
  })
}

shinyApp(ui = ui, server = server)
