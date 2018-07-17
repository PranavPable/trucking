library(shiny)
library(shinydashboard)
library(googleway)
TR = read.csv("C:/Users/Pable/Downloads/TR.csv",stringsAsFactors = F)
api_key <- "AIzaSyBfGmcW3hSmTOw36D3K04KQnhzDe7sYMMI"
map_key <- "AIzaSyBfGmcW3hSmTOw36D3K04KQnhzDe7sYMMI"
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Get Qoute",tabName = "Get_Qoute",icon = icon("dashboard")),
      menuItem("PIN Directory" , tabName = "PIN_Directory" , icon = icon("map-marker")),
      menuItem("Truck Agencies",tabName = "Truck_Agencies",icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Get_Qoute",
    column(width = 6,
           fluidRow(box(solidHeader = TRUE, height = 180,width = 8,
                        selectizeInput(inputId = "origin" , 
                                       label = "PORT CITY",
                                       choices = levels(as.factor(TR[[1]])),
                                       selected = levels(as.factor(TR[[1]]))[1],
                                       multiple = FALSE,
                                       options = list(create =TRUE)),
                        selectizeInput(inputId = "destination" , 
                                       label = "LOCATION",
                                       choices = levels(as.factor(TR[[2]])),
                                       selected = levels(as.factor(TR[[2]]))[1],
                                       multiple = FALSE,
                                       options = list(create =TRUE))
           ),
           box(solidHeader = TRUE, height = 180,width = 4,
               
               
               selectizeInput(inputId = "CONTAINER_SIZE", 
                              label = "Container Size", 
                              choices = c("20","40"),
                              selected = "20",
                              multiple = FALSE,
                              options = list(create =TRUE)),
               selectizeInput(inputId = "WEIGHT", 
                              label = "Weight Class", 
                              choices = c("Upto 22tn","22-28tn"),
                              selected = "Upto 22tn",
                              multiple = FALSE,
                              options = list(create =TRUE))
               
               
               
               
           )),
           actionButton(inputId = "getRoute", label = "Get Qoute",style ='text-align: center; display: inline-block ; width :100%')
    ),
    #textInput(inputId = "origin", label = "Origin"),
    #textInput(inputId = "destination", label = "Destination" ),
    
    column(width = 6, height =180,
           fluidRow(
             # A static valueBox
             valueBox(2000, "TOTAL ROUTES", icon = icon("truck"),width = 4),
             
             # Dynamic valueBoxes
             valueBoxOutput("distanceBox",width = 4),
             
             valueBoxOutput("progressBox",width = 4)
           ),
           fluidRow(
             valueBoxOutput("moneyDetailsBox" , width = 12)
           )),
    
    
    
    box(google_mapOutput("myMap") ,width = 12)
  ),
  tabItem(tabName = "PIN_Directory" ,
          
  
            DT::dataTableOutput("contentsPIN")
            

          
          ),
  tabItem(tabName = "Truck_Agencies",
          DT::dataTableOutput("contentsAGENCIES"))))
)


server <- function(input, output,session){
  
  api_key <- "AIzaSyBfGmcW3hSmTOw36D3K04KQnhzDe7sYMMI"
  map_key <- "AIzaSyBfGmcW3hSmTOw36D3K04KQnhzDe7sYMMI"
  
  

  output$myMap <- renderGoogle_map({
    google_map(key = map_key, 
               search_box = TRUE, 
               scale_control = TRUE, 
               height = 1000) %>%
      add_traffic()
  })
  
  observeEvent(input$getRoute,{
    
    print("getting route")
    
    o <- input$origin
  
    d <- input$destination
    output$distanceBox <- 
      renderValueBox({
        set_key("AIzaSyCJmDqKZsXn-whzZIpcPw3KreDQ-QsjxBE")
        x = google_distance(origins = o , destinations =d)[[3]]
        valueBox(
          x$elements[[1]][[1]][1,1] , "Approval", icon = icon("map-marker", lib = "glyphicon"),
          color = "purple"
        )
      })
    
    output$moneyDetailsBox <- renderValueBox({
      PRICE = 0
      for ( i in 1:length(TR[[1]]))
      {
        if(toupper(TR[[1]][i]) == toupper(o) & toupper(TR[[2]][i]) ==toupper(d))
        {
          if(input$CONTAINER_SIZE == "20" & input$WEIGHT == "Upto 22tn")
          {
            PRICE = TR[[7]][i]
          }
          else if(input$CONTAINER_SIZE == "20" & input$WEIGHT != "Upto 22tn")
          {
            PRICE = TR[[8]][i]
          }
          else if(input$CONTAINER_SIZE == "40" & input$WEIGHT == "Upto 22tn")
          {
            PRICE = TR[[9]][i]
          }
          else if(input$CONTAINER_SIZE == "40" & input$WEIGHT == "Upto 22tn")
          {
            PRICE = TR[[10]][i]
          }
        }
        
      }
      if(PRICE==0)
      {
        showModal(modalDialog("The pair of locations hasn't been added yet!"))
        PRICE == "The pair of locations is yet to be added!"
      }
      valueBox(
      PRICE, paste(toupper(o),"to",toupper(d),sep = " "), icon = icon("inr"),
        color = "purple"
      )
    })
    
  
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "PRICE", icon = icon("smile-o"),
      color = "yellow"
    )
  })
    
    res <- google_directions(key = api_key,
                             origin = o,
                             
                             destination = d,
                             
                             mode = "driving")
    
    df_route <- data.frame(route = res$routes$overview_polyline$points)
    
    df_way <- cbind(
      res$routes$legs[[1]]$end_location,
      data.frame(address = res$routes$legs[[1]]$end_address)
    )
    
    df_way$order <- as.character(1:nrow(df_way))
    
    
    google_map_update(map_id = "myMap") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_polylines(data = df_route,
                    polyline = "route",
                    stroke_colour = "#FF33D6",
                    stroke_weight = 7,
                    stroke_opacity = 0.7,
                    info_window = "New route",
                    load_interval = 100) %>%
      add_markers(data = df_way,
                  info_window = "end_address",
                  label = "order")
  })
  
  
  
  
  output$contentsPIN <-
    
    
    DT::renderDataTable({
      library(DT)
      library(shinyjs)
      
      DT::datatable(read.csv("DATA/PIN.csv"),filter = 'top',options =list(scrollX = TRUE,
                                                                  initcomplete = JS(
                                                                    "function(settings, json) {",
                                                                    "$(this.api().table().header()).css({'background-color': '#A0A0A0','color': '#fff'});",
                                                                    "}")
      ),editable = TRUE)
      
      
    })
  
  
  
  
  
  
  
  
  
  
  
  
  }




shinyApp(ui,server)
  