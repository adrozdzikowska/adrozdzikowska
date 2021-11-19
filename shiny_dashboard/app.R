library(shiny)
library(shinydashboard)
library(data.table)
#library(stringr)
library(highcharter)
library(plotly)
#library(shinyWidgets)
library(shinycssloaders) # spinners


ui <- dashboardPage(
  dashboardHeader(title = "App shiny dashboard"),
  dashboardSidebar(sidebarMenu(id = "menu", sidebarMenuOutput("menu"))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dash1", 
              fluidRow(
                column(6, withSpinner(uiOutput("first_box"), color = "#3c8dbc")),
                column(6, withSpinner(uiOutput("second_box"), color = "#3c8dbc"))
              ),
              fluidRow(column(width = 12, class = "well",
                              withSpinner(DT::dataTableOutput("tab"), color = "#3c8dbc"))
              )
      ),
      tabItem(tabName = "dash2", 
              withSpinner(plotlyOutput("trendPlot"), color = "#3c8dbc")
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$menu <- renderMenu({
    sidebarMenu( 
      id = "mytabitems",
      menuItem("Menu 1", tabName = "menu_1", icon = icon("globe"), startExpanded = TRUE,
               menuSubItem("Options:", tabName = "dash1",  icon = icon("calendar")),
               tagList(
                 tags$style(type = 'text/css',
                            " 
                            .irs--shiny .irs-min, .irs--shiny .irs-max {
                            color: #ccc !important;
                            }
                            
                            .irs--shiny .irs-grid-text {
                            color: azure !important;
                            }"
                 ),
                 sliderInput("n", "Number of observations", min = 1, max = nrow(iris), value = 10)
               )
      ), 
      menuItem("Menu 2",
               tabName = "menu_2", icon = icon("search"),
               menuSubItem("Options:", tabName = "dash2",  icon = icon("calendar")),
               selectInput('x', 'X', choices = nms, selected = "carat"),
               selectInput('y', 'Y', choices = nms, selected = "price"),
               selectInput('color', 'Color', choices = nms, selected = "clarity")
      ))
  })
  
  # Update active sidebar
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "Menu2") {
      updateTabItems(session, "mytabitems", "dash2")
    } else if(input$sidebarItemExpanded == "Menu1") {
      updateTabItems(session, "mytabitems", "dash1") 
    }
  })
  
  #### ------------------  Sheet 1 -------------------####
  
  # Data
  dt_app <- reactive({
    
    dt <- iris[1:input$n, ]
    return(dt)
    
  })
  
  # 1 box
  output$first_box <- renderValueBox({
    
    valueBox("Number of observations",
             icon = icon("list-ol"),
             value = input$n,
             color = "red"
    )
  })
  
  # 2 box
  output$second_box <- renderValueBox({
    
    dt_box <- dt_app()
    
    valueBox(mean(dt_box$Sepal.Length),
             "Mean Sepal.Length", 
             icon = icon("calculator"),
             color = "blue"
    )
    
  })
  
  # Table
  output$tab <- DT::renderDataTable({
    
    DT::datatable(dt_app(), options = list(pageLength = 15))
    
  })
  
  #### ------------------  Sheet 2 -------------------####
  
  # Data
  data(diamonds, package = "ggplot2")
  nms <- names(diamonds)
  
  # Add reactive data information. Dataset = built in diamonds data
  dataset <- reactive({
    diamonds[sample(nrow(diamonds), 500),]
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point()
    
    # change ggplot to plotly interactive plot
    ggplotly(p) 
    
  })
  
}

shinyApp(ui, server)
