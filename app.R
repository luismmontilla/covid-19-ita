library(shiny)
library(dplyr)
library(ggplot2)
#library(readr)

urlfile<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

df <- read.csv(urlfile)
df$data <- as.POSIXct(df$data)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19: Number of new daily cases in Italy"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("regionInput", "Region",
                        choices = levels(df$denominazione_regione))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        df %>%
            filter(denominazione_regione==input$regionInput) %>% 
            ggplot(aes(x = data, y = nuovi_attualmente_positivi)) +
            geom_point() +
            geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")), 
                       linetype="dashed", 
                       color = "black") +
            labs(x = "Date",
                 y = "New cases")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
