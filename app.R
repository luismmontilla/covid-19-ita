library(shiny)
library(dplyr)
library(ggplot2)


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
           plotOutput("distPlot"),
           p("All the data displayed in this visualization is provided by the 
             Italian Ministry of Health (Ministero della Salute) and elaborated 
             by Dipartimento della Protezione Civile. This work is therefore a 
             derivative of ",
             a("COVID-19 Italia - Monitoraggio situazione",
               href="https://github.com/pcm-dpc/COVID-19"),
             "licensed under CC BY 4.0")
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
            annotate("text", 
                     x = as.POSIXct(as.Date("2020-03-07")), 
                     y = max(df[df$denominazione_regione==input$regionInput,"nuovi_attualmente_positivi"]), 
                     label = "Announcement of lockdown") +
            labs(x = "Date",
                 y = "New cases")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
