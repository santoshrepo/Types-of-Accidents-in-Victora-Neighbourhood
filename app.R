library(shiny)
library(leaflet)
library(plotly)
library(shinythemes)
library(htmltools)
library(leaflet.extras)
library(plotly)
library(shinyalert)



crash <- read.csv("Filtered1617.csv")
house <- read.csv("House.csv")

ui <- navbarPage(
    theme = shinytheme("cosmo"),  
    "Types of Accidents in Victoria neighbourhood", id="nav",
                 
                 tabPanel("Main",
                          div(class="outer",
                              tags$head(
                                  includeCSS("styles.css")
                              ),
                              
                              absolutePanel(class = "panel panel-default",fixed = TRUE,draggable = FALSE, top = 63, left = 325, right = "auto", bottom = "auto",
                                            height = 450, width = 600,
                                            leafletOutput("siteMap", height = "100%", width = "200%"),
                                            ),
                              
                              absolutePanel(class = "panel panel-default",fixed = TRUE,draggable = FALSE, top = 520, left = 325, right = "auto", bottom = "auto",
                                            height = "auto", width = 910,
                                            plotlyOutput('maingraph1',height = 334)
                              ),
                              
                              absolutePanel(class = "panel panel-default",fixed = TRUE,draggable = FALSE, top = 520, left = 1240, right = "auto", bottom = "auto",
                                            height = "auto", width = 280,
                                            plotlyOutput('maingraph2',height = 334)
                              ),
                              absolutePanel(class = "panel panel-default",fixed = TRUE,draggable = FALSE, top = 750, left = 18, right = "auto", bottom = "auto",
                                            height = "auto", width = 298,
                                            h4("     Data References :"),
                                            uiOutput("tab"),
                                            uiOutput("tab1")
                                            #plotlyOutput('maingraph1',height = 200)
                              ),
                              
                            
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = 70, left = 20, right = "auto", bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("Accident explorer"),
                                            
                                            selectizeInput("Accidenttypes", "Accident Types", selected = c("Collision with vehicle","No collision/Vehicle Overturned"),unique(crash$ACCIDENT_TYPE),multiple = TRUE,options = list(maxItems = 4L)),
                                            selectizeInput("Surburb", "Surburbs",selected = c("Abbotsford","Carlton"), unique(house$Suburb) ,multiple = TRUE, options = list(maxItems = 9L)),
                                            selectInput("Bedrooms", "House",choices = unique(house$Housewithrooms)),
                                            selectizeInput("Severity", "Severity", selected = c("Serious injury accident","Fatal accident"),unique(crash$SEVERITY),multiple = TRUE,options = list(maxItems = 3L)),
                                            selectizeInput("LightConditions", "Light Conditions", selected = c("Day","Dusk/Dawn"),unique(crash$LIGHT_CONDITION),multiple = TRUE,options = list(maxItems = 5L)),
                                            
                                            dateRangeInput("DatesMerge", "Date Range Input", start =  min(crash$Date), end = max(crash$Date))

                              )
                          )
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    HouseIcon <- makeIcon(
        iconUrl = "https://image.flaticon.com/icons/svg/25/25694.svg",
        iconWidth = 25, iconHeight = 25
    )
    
    url <- a("Melbourne Housing Snapshot", href="https://www.kaggle.com/dansbecker/melbourne-housing-snapshot")
    output$tab <- renderUI({
        tagList("1. House data: ", url)
    })
    
    url1 <- a("Crash for the last five years", href="https://discover.data.vic.gov.au/dataset/crashes-last-five-years")
    output$tab1 <- renderUI({
        tagList("2. Crash data: ", url1)
    })
    
    observe({
        if(input$nav == "Main"){
            showModal(modalDialog(
                title = "Types of Accidents in Victoria neighbourhood",
                
                "The shiny app aims to correlate the types of accidents happening around Victoria with the different types of houses in Victoria. The target audience is VIC roads management, real estate brokers, and people who are keen to know about the accidents in the area while driving or traveling with family." ,
						
						tags$br(),
						
						tags$h4("Scenario 1:"),
						
						"In Victoria, there are different types of accidents happening in certain areas. For instance, the Suburbs with freeway or highway will most likely have speed-related accidents because of the speed limit compared to the residential suburbs which will have less speed limit. ",

						tags$h4("Scenario 2: "),
						
						"The chances of colliding with pedestrians in Melbourne CBD are more likely high than compared to suburbs with freeway or highway. We could determine the likelihood of accidents in the suburbs using the type of accidents that occurred in that area."
                
            ))
            
        }
        
    })
    
    
    observeEvent(input$Accidenttypes, {

        x <- length(input$Accidenttypes)

        if(length(x) == 1){
            shinyalert("Oops!", "Something went wrong.", type = "error")
        }
        else
        {
            return()
        }

    })
    
    data_input_crash <- reactive({
        
        input$Accidenttypes
        input$Severity
        input$LightConditions
        input$DatesMerge
        
        
        df <- crash %>% 
            filter(ACCIDENT_TYPE %in% input$Accidenttypes) %>%
            filter(SEVERITY %in% input$Severity) %>%
            filter(LIGHT_CONDITION %in% input$LightConditions) %>% 
            filter(Date >= input$DatesMerge[1] & Date <= input$DatesMerge[2]) %>%
            group_by(ACCIDENT_TYPE)

    }) 
    
    dataModal <- function(failed = FALSE) {
        modalDialog(
            span('Note:'),
            span('Should select atleast one from the options'),
            footer = tagList(
                modalButton("Dismiss")
            )
        )
    }
    
    observeEvent(input$Accidenttypes, {
        
        x <- input$Accidenttypes 
        if(length(x) <= 1){
            showModal(dataModal())    
        }
        
    })
    
    observeEvent(input$Severity, {
        
        x <- input$Severity 
        if(length(x) <= 1){
            showModal(dataModal())    
        }
        
    })
    
    
    observeEvent(input$LightConditions, {
        
        x <- input$LightConditions 
        if(length(x) <= 1){
            showModal(dataModal())    
        }
        
    })
    
    
    observeEvent(input$Surburb, {
        
        x <- input$Surburb 
        if(length(x) <= 1){
            showModal(dataModal())    
        }
        
    })
    
    data_input_house <- reactive({
        input$Surburb
        input$Bedrooms
        
        df_1 <- house %>% 
            filter(Suburb %in% input$Surburb)  %>% 
            filter(Housewithrooms %in% input$Bedrooms) %>% group_by(Suburb)
            
    })
    
    output$siteMap <- renderLeaflet({ 
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
            htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }") %>%
            addTiles() %>%
            addProviderTiles('CartoDB.Positron') %>%
            setView( 144.964600,-37.020100, zoom = 7)
    })
    
    
    output$maingraph1 <- renderPlotly({
        
        f <- list(
            family = "sans serif",
            size = 14,
            color = 'grey')
        
        a <- list(
            text = "Severity of Accident types",
            font = f,
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE
        )
        
        b <- list(
            text = "Accident Types in Light Conditions",
            font = f,
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE
        )   
    
        c <- list(
            text = "Accident types in Suburbs",
            font = f,
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE
        )

        
        crash1 <- data_input_crash()
        house1 <- data_input_house()
        
        df1 <- crash1 %>% 
            select(SEVERITY,ACCIDENT_TYPE) %>% 
            group_by (SEVERITY,ACCIDENT_TYPE) %>% 
            summarise(count = n())
        
        fig1 <- plot_ly(df1,x = ~SEVERITY, y = ~count, type = 'bar', color = ~ACCIDENT_TYPE,showlegend = T) %>%
            layout(yaxis = list(title = ''), barmode = 'stack',legend = list(orientation = "h",x = 0.25, y = -0.2))  %>% config(displayModeBar = F) %>%
            layout(annotations = a) %>% layout(
                xaxis = list(tickfont = list(size = 7),title = 'Severity'), 
                yaxis = list(tickfont = list(size = 7))) %>% layout(legend = list(font = list(size = 9)))
        
        df2 <- crash1 %>% 
            select(LIGHT_CONDITION,ACCIDENT_TYPE) %>% 
            group_by (LIGHT_CONDITION,ACCIDENT_TYPE) %>% 
            summarise(count = n())
        
        fig2 <- plot_ly(df2,x = ~LIGHT_CONDITION, y = ~count, type = 'bar', color = ~ACCIDENT_TYPE,showlegend = F) %>%
            layout(yaxis = list(title = ''), barmode = 'stack')%>% layout(annotations = b) %>% layout(
                xaxis = list(tickfont = list(size = 7),title = 'Light Conditions'), 
                yaxis = list(tickfont = list(size = 7)))
        
        #fig2 = layout(fig2,showlegend = FALSE)
        
        df3 <- house1 %>% 
            select(Suburb,Housewithrooms,Regionname)
        
        df4 <- crash1 %>% 
            select(ACCIDENT_TYPE,Regionname) 
        
        
        df5 <- merge(df3,df4,by="Regionname") 
        
        df6 <- df5 %>% 
            select(Suburb,ACCIDENT_TYPE) %>%
            group_by (Suburb,ACCIDENT_TYPE) %>% 
            summarise(count = n())
        
        
        fig3 <- plot_ly(df6,x = ~Suburb, y = ~count, type = 'bar', color = ~ACCIDENT_TYPE,showlegend = F) %>%
            layout(yaxis = list(title = 'Freq(Accident types)'), barmode = 'stack')  %>% layout(annotations = c)%>% layout(
                xaxis = list(tickfont = list(size = 7),title = 'Suburbs'), 
                yaxis = list(tickfont = list(size = 7))) 
            #layout(title = list(text = "Accidents types in Suburbs",orientation = "h",  x = 0.07),font=t)

        
        fig <- subplot(fig3, fig2,fig1,titleY = TRUE)
        
        fig
        })
    
    output$maingraph2 <- renderPlotly({
        
        f <- list(
            family = "sans serif",
            size = 14,
            color = 'grey')
        
        d <- list(
            text = "Accident types(Freq)",
            font = f,
            xref = "paper",
            yref = "paper",
            yanchor = "bottom",
            xanchor = "center",
            align = "center",
            x = 0.5,
            y = 1,
            showarrow = FALSE
        )
        
        
        crash9 <- data_input_crash()
        house10 <- data_input_house()
        
        df9 <- house10 %>% 
            select(Suburb,Housewithrooms,Regionname)
        
        df10 <- crash9 %>% 
            select(ACCIDENT_TYPE,Regionname) 
        
        df11 <- merge(df9,df10,by="Regionname") 
        
        df12 <- df11 %>% 
            select(Housewithrooms,Suburb,ACCIDENT_TYPE) %>%
            group_by (Housewithrooms,ACCIDENT_TYPE) %>% 
            summarise(count = n())
        
        
        fig4 <- plot_ly(df12, labels = ~ACCIDENT_TYPE, values = ~count, type = 'pie') %>% 
            layout(legend = list(orientation = "h",  x = 0.25, y = -0.2)) %>% layout(legend = list(font = list(size = 9))) %>%
            layout(annotations = d)
        
        fig4
        
    })
    
    observe({
        
        leafletProxy("siteMap") %>%
            clearMarkers() %>% clearMarkerClusters() %>%
            addCircleMarkers(data = data_input_house(),
                       lng = ~Longtitude, 
                       lat = ~Lattitude,color = "blue",
                       #clusterOptions = markerClusterOptions(),
                       radius = 50,
                       label = ~htmlEscape(data_input_house()$Housewithrooms)) %>%
            addCircleMarkers(data = data_input_crash(),
                             lng = ~LONGITUDE, 
                             lat = ~LATITUDE,
                             group = "ACCIDENT_TYPE",
                             clusterOptions = markerClusterOptions(),
                             radius = 50,
                             color = "red",
                             label = ~htmlEscape(data_input_crash()$ACCIDENT_TYPE)) %>%
            addMarkers(data = data_input_house(),lng = ~Longtitude, 
                              lat = ~Lattitude,icon = HouseIcon) 
    })
    
    
    observeEvent(input$Surburb,
                 {
                     x <<- input$Surburb
                     updateSelectInput(session, "Bedrooms",
                                       choices = unique(house[house$Suburb %in% input$Surburb, ])$Housewithrooms
                     )
                 }
    )
    
    observeEvent(input$Accidenttypes,
                 {
                     x <<- input$Accidenttypes
                     updateSelectInput(session, "Severity",
                                       choices = unique(crash[crash$ACCIDENT_TYPE %in% input$Accidenttypes, ])$Severity
                     )
                 }
    )
    
    observeEvent(input$Severity,
                 {
                     x <<- input$Severity
                     updateSelectInput(session, "LightConditions",
                                       choices = unique(crash[crash$SEVERITY %in% input$Severity, ])$LightConditions
                     )
                 }
    )

    observeEvent(input$LightConditions,
                 {
                     x <<- input$LightConditions

                     temp_df = crash[crash$LIGHT_CONDITION %in% x,]$Date
                    
                     updateDateRangeInput(session,"DatesMerge",start = min(temp_df),end = max(temp_df))

                 }
    )

    

}    

# Run the application 
shinyApp(ui = ui, server = server)
