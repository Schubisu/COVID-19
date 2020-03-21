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
            selectInput("countries",
                        "Countries:",
                        choices = long_data$Country.Region %>% unique(),
                        multiple = TRUE),
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

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("linePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- reactive({
        df <- long_data %>%
            filter(Country.Region %in% input$countries) %>%
            filter(case %in% input$cases) %>%
            mutate(value = ifelse(rep(input$by_population, nrow(.)), value / population, value))
    })
    

    output$linePlot <- renderPlot({
        plot_data <- data()
        
        p <- plot_data %>%
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
