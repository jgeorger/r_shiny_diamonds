#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
diamonds=read.csv("~/Documents/WPI data science MS/DS501/Case Study 3/diamonds.csv", header=TRUE)

library(shiny)

# Define UI for application that draws a histogram
## TODO: Go for a NavBar layout....

ui <- navbarPage("Diamonds",
    tabPanel("About",
              verbatimTextOutput("summary")
    ),
    tabPanel("Histogram",
        sidebarLayout(
            sidebarPanel(
                selectInput('histvar', 'Feature', names(diamonds),
                            selected=names(diamonds)[[2]]),
                sliderInput('nbins', 'Number of bins', 1, 50, 10)
            ),
            mainPanel(
                plotOutput("histPlot")
            )
        )
    ),
    tabPanel("Scatterplot",
        sidebarLayout(
            sidebarPanel(
                selectInput('xcol', 'X Variable', names(diamonds),
                            selected=names(diamonds)[[2]]),
                selectInput('ycol', 'Y Variable', names(diamonds),
                            selected=names(diamonds)[[8]])
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
               plotOutput("scatterPlot")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$summary <- renderPrint({
        summary(diamonds)
    })
    
    output$histPlot <- renderPlot({
        hist(diamonds[[input$histvar]], input$nbins, xlab=input$histvar, col='green',
             main=NULL)
    })
    output$scatterPlot <- renderPlot({
        qplot(get(input$xcol), get(input$ycol) ,data=diamonds, xlab=input$xcol, 
              ylab=input$ycol) + geom_point(colour = "#3366FF", size = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)