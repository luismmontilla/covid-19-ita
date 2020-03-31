library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)

#data
url_a <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

region <- read.csv(url_a)
region$data <- as.POSIXct(sub(" .*", "", region$data))

url_b <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
nation <- read.csv(url_b)
nation$data <- as.POSIXct(sub(" .*", "", nation$data))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19: Number of new cases in Italy"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("regionInput", "Region",
                        choices = levels(region$denominazione_regione)),
            checkboxInput("nationInput", 
                          label = "Show national data", 
                          value = FALSE),
            checkboxInput("trendInput", 
                          label = "Show data trend", 
                          value = FALSE),
            p("All the data displayed in this visualization is provided by the 
             Italian Ministry of Health (Ministero della Salute) and elaborated 
             by Dipartimento della Protezione Civile. This work is therefore a 
             derivative of ",
              a("COVID-19 Italia - Monitoraggio situazione",
                href = "https://github.com/pcm-dpc/COVID-19"),
              "licensed under CC BY 4.0")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           p("Code available ",
             a("here",
               href = "https://github.com/luismmontilla/covid-19-ita"),
             align = "center")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        if(input$nationInput == TRUE){
            if(input$trendInput == TRUE) {
                
                p <- region %>%
                    filter(denominazione_regione==input$regionInput) %>% 
                    ggplot(aes(x = data, y = nuovi_positivi)) +
                    geom_point() +
                    geom_smooth() +
                    geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")), 
                               linetype="dashed", 
                               color = "black") +
                    labs(x = "Date",
                         y = "New cases") +
                    ylim(0, max(nation$nuovi_positivi))
                
                q <- nation %>% 
                    ggplot(aes(x = data, y = nuovi_positivi)) +
                    geom_point() +
                    geom_smooth() +
                    labs(x = "Date",
                         y = "") +
                    geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")), 
                               linetype="dashed", 
                               color = "black")
                
                p+q
            } else {
                p <- region %>%
                    filter(denominazione_regione==input$regionInput) %>% 
                    ggplot(aes(x = data, y = nuovi_positivi)) +
                    geom_point() +
                    geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")), 
                               linetype="dashed", 
                               color = "black") +
                    labs(x = "Date",
                         y = "New cases") +
                    ylim(0, max(nation$nuovi_positivi))
                
                q <- nation %>% 
                    ggplot(aes(x = data, y = nuovi_positivi)) +
                    geom_point() +
                    labs(x = "Date",
                         y = "") +
                    geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")), 
                               linetype="dashed", 
                               color = "black")
                
                p+q
            }
        
            

        } else {
            if(input$trendInput == TRUE) {
                region %>%
                    filter(denominazione_regione==input$regionInput) %>% 
                    ggplot(aes(x = data, y = nuovi_positivi)) +
                    geom_point() +
                    geom_smooth() +
                    geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")), 
                               linetype="dashed", 
                               color = "black") +
                    annotate("text", 
                             x = as.POSIXct(as.Date("2020-03-07")), 
                             y = max(region[region$denominazione_regione==input$regionInput,"nuovi_positivi"]), 
                             label = "Announcement of lockdown") +
                    labs(x = "Date",
                         y = "New cases")
            } else {
                region %>%
                    filter(denominazione_regione==input$regionInput) %>% 
                    ggplot(aes(x = data, y = nuovi_positivi)) +
                    geom_point() +
                    geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")), 
                               linetype="dashed", 
                               color = "black") +
                    annotate("text", 
                             x = as.POSIXct(as.Date("2020-03-07")), 
                             y = max(region[region$denominazione_regione==input$regionInput,"nuovi_positivi"]), 
                             label = "Announcement of lockdown") +
                    labs(x = "Date",
                         y = "New cases")
            }
            
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
