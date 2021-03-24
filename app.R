library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)

#data
url_a <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

region <- read.csv(url_a)
region$data <- as.POSIXct(sub(" .*", "", region$data))

url_b <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
nation <- read.csv(url_b)
nation$data <- as.POSIXct(sub(" .*", "", nation$data))

url_c <- "https://github.com/italia/covid19-opendata-vaccini/raw/master/dati/consegne-vaccini-latest.csv"
vaccine <- read.csv(url_c)
vaccine$data_consegna <- as.POSIXct(sub(" .*", "", vaccine$data_consegna))
#

ui <- fluidPage(
  
  title = "COVID-19: Number of new cases in Italy",
  
  plotOutput('distPlot'),
  
  hr(),
  
  fluidRow(
    column(3,
           offset = 3,
           h4("COVID-19 in Italy"),
           selectInput("regionInput", 
                       "Region",
                       choices = c("(None)",unique(region$denominazione_regione))
                       ),
           p("Code available ",
             a("here",
               href = "https://github.com/luismmontilla/covid-19-ita"),
             align = "center")
    ),
    column(3,
           #offset = 2,
           p("All the data displayed in this visualization is provided by the 
             Italian Ministry of Health (Ministero della Salute) and elaborated 
             by Dipartimento della Protezione Civile. This work is therefore a 
             derivative of ",
             a("COVID-19 Italia - Monitoraggio situazione",
               href = "https://github.com/pcm-dpc/COVID-19"),
             "licensed under CC BY 4.0")
      
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({

    #national plot
    n1 <- nation %>%
      filter(nuovi_positivi >= 0) %>%
      ggplot() +
      geom_point(aes(x = data, y = nuovi_positivi)) +
      labs(x = "Date",
           y = "New cases") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                 linetype="dashed",
                 color = "black") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                 linetype = "dashed",
                 color = "black") +
      geom_smooth(aes(x = data, y = nuovi_positivi), method = "gam")
    
    n2 <- nation %>%
      #filter(nuovi_positivi >= 0) %>%
      ggplot() +
      geom_point(aes(x = data, y = totale_ospedalizzati)) +
      labs(x = "Date",
           y = "Hospitalized") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                 linetype="dashed",
                 color = "black") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                 linetype = "dashed",
                 color = "black") +
      ylim(0, max(nation$nuovi_positivi))
    
    n3 <- nation %>%
      mutate(nuovi_deceduti = deceduti - lag(deceduti, default = first(deceduti))) %>% 
      filter(nuovi_deceduti >= 0) %>%
      ggplot() +
      geom_point(aes(x = data, y = nuovi_deceduti)) +
      geom_smooth(aes(x = data, y = nuovi_deceduti), method = "gam") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                 linetype="dashed",
                 color = "black") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                 linetype = "dashed",
                 color = "black") +
      labs(x = "Date",
           y = "Deceased") 
    
    n4 <- nation %>%
      mutate(nuovi_tamponi = tamponi - lag(tamponi, default = first(tamponi))) %>% 
      filter(nuovi_tamponi >= 0) %>%
      ggplot() +
      geom_point(aes(x = data, y = nuovi_tamponi)) +
        geom_smooth(aes(x = data, y = nuovi_tamponi), method = "gam") +
      labs(x = "Date",
           y = "PCR tests") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                 linetype="dashed",
                 color = "black") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                 linetype = "dashed",
                 color = "black")
    
    n5 <- vaccine %>% 
      group_by(data_consegna) %>% 
      arrange(data_consegna) %>% 
      summarise(numero_dosi = sum(numero_dosi)) %>% 
      mutate(cumsumv = cumsum(numero_dosi)) %>% 
      ggplot() +
      geom_point(aes(x = data_consegna, y = numero_dosi)) +
      labs(x = "Date",
           y = "Vaccination") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                 linetype="dashed",
                 color = "black") +
      geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                 linetype = "dashed",
                 color = "black") +
      geom_smooth(aes(x = data_consegna, y = numero_dosi), method = "gam")
    
    
    nt <- (n1 + n2 + n3 + n4 + n5) + plot_layout(ncol = 5)
    
    # tables for daily data
    
    nt1 <- nation %>% 
      slice_max(data) %>% 
      select(nuovi_positivi) %>% 
      #data.frame("new_cases" = max(nation$nuovi_positivi)) %>% 
      ggtexttable(rows = NULL, theme = ttheme(base_size = 18), 
                  cols = "New positive cases")
    
    nt2 <- nation %>% 
      slice_max(data) %>% 
      select(totale_ospedalizzati) %>% 
      ggtexttable(rows = NULL, cols = "Total hospitalized",
                  theme = ttheme(base_size = 18))
    
    nt3 <- nation %>% 
      slice_max(data) %>% 
      select(deceduti) %>% 
      ggtexttable(rows = NULL, cols = "Deceased",
                  theme = ttheme(base_size = 18))
    
    nt4 <- nation %>% 
      slice_max(data) %>% 
      select(tamponi) %>% 
      ggtexttable(rows = NULL, cols = "PCR tests",
                  theme = ttheme(base_size = 18))
    
    nt5 <- vaccine %>% 
      group_by(data_consegna) %>% 
      arrange(data_consegna) %>% 
      summarise(numero_dosi = sum(numero_dosi)) %>% 
      slice_max(data_consegna) %>% 
      select(numero_dosi) %>% 
      ggtexttable(rows = NULL, cols = "Administered vaccines",
                  theme = ttheme(base_size = 18))
    

    
    ntt <- nt1 + nt2 + nt3 + nt4 + nt5 + plot_layout(ncol = 5)
    
    
    nt/ntt
    
    if(input$regionInput != "(None)"){
      
      #regional plot
      r1 <- region %>%
        filter(denominazione_regione==input$regionInput) %>% 
        #filter(denominazione_regione == "Campania") %>% 
        filter(nuovi_positivi >= 0) %>% 
        ggplot(aes(x = data, y = nuovi_positivi)) +
        geom_point() + 
        geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                   linetype = "dashed",
                   color = "black") +
        geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                   linetype = "dashed",
                   color = "black") +
        geom_smooth(aes(x = data, y = nuovi_positivi), method = "gam") +
        labs(x = "Date",
             y = "New cases")
      
      r2 <- region %>%
        filter(denominazione_regione==input$regionInput) %>% 
        #filter(denominazione_regione == "Campania") %>% 
        filter(nuovi_positivi >= 0) %>% 
        ggplot(aes(x = data, y = totale_ospedalizzati)) +
        geom_point() + 
        geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                   linetype = "dashed",
                   color = "black") +
        geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                   linetype = "dashed",
                   color = "black") +
        labs(x = "Date",
             y = "Hospitalized") +
        ylim(0, max(region[region$denominazione_regione==input$regionInput,"nuovi_positivi"]))
      
      r3 <- region %>%
        filter(denominazione_regione==input$regionInput) %>% 
        mutate(nuovi_deceduti = deceduti - lag(deceduti, default = first(deceduti))) %>% 
        filter(nuovi_deceduti >= 0) %>%
        ggplot(aes(x = data, y = nuovi_deceduti)) +
        geom_point() + 
        geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                   linetype = "dashed",
                   color = "black") +
        geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                   linetype = "dashed",
                   color = "black") +
        geom_smooth(aes(x = data, y = nuovi_deceduti), method = "gam") +
        labs(x = "Date",
             y = "Deceased") 
      
      r4 <- region %>%
        filter(denominazione_regione==input$regionInput) %>% 
        mutate(nuovi_tamponi = tamponi - lag(tamponi, default = first(tamponi))) %>% 
        filter(nuovi_tamponi >= 0) %>%
        ggplot(aes(x = data, y = nuovi_tamponi)) +
        geom_point() + 
        geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                   linetype = "dashed",
                   color = "black") +
        geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                   linetype = "dashed",
                   color = "black") +
        geom_smooth(aes(x = data, y = nuovi_tamponi), method = "gam")
        labs(x = "Date",
             y = "PCR tests") 
      
      r5 <- vaccine %>% 
        group_by(nome_area, data_consegna) %>% 
        arrange(data_consegna) %>% 
        summarise(numero_dosi = sum(numero_dosi)) %>% 
        mutate(cumsumv = cumsum(numero_dosi)) %>% 
        filter(nome_area==input$regionInput) %>% 
        #filter(nome_area=="Campania") %>% 
        ggplot() +
        geom_point(aes(x = data_consegna, y = numero_dosi)) +
        labs(x = "Date",
             y = "Vaccination") +
        geom_vline(xintercept = as.POSIXct(as.Date("2020-03-09")),
                   linetype="dashed",
                   color = "black") +
        geom_vline(xintercept = as.POSIXct(as.Date("2020-05-04")),
                   linetype = "dashed",
                   color = "black") +
        geom_smooth(aes(x = data_consegna, y = numero_dosi), method = "gam") 
      
      rt <- r1 + r2 + r3 + r4 + r5 + plot_layout(ncol = 5) 
      
      # tables for daily data
      
      rt1 <- nation %>% 
        slice_max(data) %>% 
        mutate(area = "Italy") %>% 
        select(area, nuovi_positivi) %>% 
        bind_rows(region %>% 
                    filter(denominazione_regione == input$regionInput) %>% 
                    #filter(denominazione_regione == "Campania") %>% 
                    filter(nuovi_positivi >= 0) %>%
                    slice_max(data) %>%
                    select(denominazione_regione, nuovi_positivi) %>% 
                    rename(area = "denominazione_regione")
                  ) %>% 
        ggtexttable(rows = NULL, theme = ttheme(base_size = 18), 
                    cols = c("Area","New positive cases"))
      
      rt2 <- nation %>% 
        slice_max(data) %>% 
        select(totale_ospedalizzati) %>% 
        bind_rows(region %>% 
                    filter(denominazione_regione == input$regionInput) %>% 
                    #filter(denominazione_regione == "Campania") %>% 
                    filter(nuovi_positivi >= 0) %>%
                    slice_max(data) %>%
                    select(totale_ospedalizzati)
        ) %>% 
        ggtexttable(rows = NULL, theme = ttheme(base_size = 18), 
                    cols = c("Total hospitalized"))
      
      rt3 <- nation %>% 
        slice_max(data) %>% 
        select(deceduti) %>% 
        bind_rows(region %>% 
                    filter(denominazione_regione == input$regionInput) %>% 
                    #filter(denominazione_regione == "Campania") %>% 
                    filter(nuovi_positivi >= 0) %>%
                    slice_max(data) %>%
                    select(deceduti)
        ) %>% 
        ggtexttable(rows = NULL, theme = ttheme(base_size = 18), 
                    cols = c("Deceased"))
      
      rt4 <- nation %>% 
        slice_max(data) %>% 
        select(tamponi) %>% 
        bind_rows(region %>% 
                    filter(denominazione_regione == input$regionInput) %>% 
                    #filter(denominazione_regione == "Campania") %>% 
                    filter(nuovi_positivi >= 0) %>%
                    slice_max(data) %>%
                    select(tamponi)
        ) %>% 
        ggtexttable(rows = NULL, theme = ttheme(base_size = 18), 
                    cols = c("PCR tests"))
      
      rt5 <- vaccine %>% 
        group_by(data_consegna) %>% 
        arrange(data_consegna) %>% 
        summarise(numero_dosi = sum(numero_dosi)) %>% 
        slice_max(data_consegna) %>% 
        select(numero_dosi) %>% 
        #select(numero_dosi) %>% 
        bind_rows(vaccine %>% 
                    group_by(nome_area, data_consegna) %>% 
                    arrange(data_consegna) %>% 
                    summarise(numero_dosi = sum(numero_dosi)) %>% 
                    mutate(cumsumv = cumsum(numero_dosi)) %>% 
                    filter(nome_area==input$regionInput) %>% 
                    #filter(nome_area=="Campania") %>% 
                    slice_max(data_consegna) %>%
                    magrittr::extract("numero_dosi")) %>% 
        ggtexttable(rows = NULL, theme = ttheme(base_size = 18), 
                    cols = c("Administered vaccines"))
      
      rtt <- rt1+rt2+rt3+rt4 + rt5 + plot_layout(ncol = 5) 
      
      nt/rt/rtt
      


    } else {
      nt/ntt
    }
    

  })
 }

# Run the application 
shinyApp(ui = ui, server = server)
