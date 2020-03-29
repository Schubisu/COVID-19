#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source('visualization.r')
library(ggpubr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 visualization"),

    sidebarLayout(
        sidebarPanel(
            wellPanel(h3("Select Countries"),
                      checkboxInput("country_by_list",
                                    "from list",
                                    value = TRUE),
                      selectInput("countries",
                                  "Countries:",
                                  choices = long_data$Country.Region %>% unique(),
                                  multiple = TRUE),
                      checkboxInput("country_filter",
                                    "filter countries",
                                    value = FALSE)
                      ),
            tabsetPanel(
                tabPanel("Longitudinal",
                    radioButtons("longitudinal_choice",
                                 "Longitudinal",
                                 choices = c("Total", "Change", "Fatality")),
                    checkboxInput("log_scale",
                                  "Log scale",
                                  value = TRUE),
                    checkboxInput("by_population",
                                  "By Population",
                                  value = FALSE),
                    checkboxGroupInput("cases",
                                       "Cases",
                                       choices = list("Confirmed" = "confirmed",
                                                      "Deaths" = "deaths",
                                                      "Recovered" = "recovered"),
                                       selected = "confirmed")
                ),
                tabPanel("Latest")
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("linePlot"),
           sliderInput("date_range",
                       "Date Range",
                       min = min(long_data$date), max = max(long_data$date),
                       value = c(min(long_data$date), max(long_data$date)),
                       width = "100%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- reactive({
        df <- long_data
        df <- df %>% filter(Country.Region %in% input$countries)
        if (input$longitudinal_choice == "Fatality") {
            df <- df %>%
                select(Country.Region, date, fatality)
        } else {
            df <- df %>%
                select(c("Country.Region", "date", "population"), input$cases)
            if(input$by_population) {
               df <- df %>%
                    mutate_at(input$cases, function (x) {x / population})
            }
        }
        
        df <- df %>%
            pivot_longer(
                c(-Country.Region, -date, -population),
                names_to = "case",
                values_to = "value") %>%
            filter(date >= input$date_range[1], date <= input$date_range[2])    
            
        df
    })
    

    output$linePlot <- renderPlot({
        p <- data() %>%
            ggplot(aes(x = date, y = value, color = Country.Region, linetype = case)) +
            geom_line(size = 1.1) +
            theme_pubr()
        if (input$log_scale) {
            p <- p + scale_y_log10()
        }
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
