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
ui <- fluidPage(

    # Application title
    titlePanel("Diamond Data"),

    # Sidebar
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

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterPlot <- renderPlot({
        qplot(get(input$xcol), get(input$ycol) ,data=diamonds, xlab=input$xcol, 
              ylab=input$ycol) + geom_point(colour = "#3366FF", size = 1)
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
