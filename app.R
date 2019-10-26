#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(caret)

# Execute only once
diamonds=read.csv("diamonds.csv", header=TRUE)
set.seed(2019)
splitVeh = caret::createDataPartition(diamonds[,1], p = 0.8, list=F, times=1)
trainVeh = diamonds[splitVeh,]
testVeh = diamonds[!row.names(diamonds) %in% row.names(trainVeh),]
testVeh = diamonds[-splitVeh,]

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
    ),
    tabPanel("Linear Regression",
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput("lmDepVars", "Dependent Variables", 
                                   names(diamonds[2:7]),
                                   selected = names(diamonds[2]))
            ),
            mainPanel(
                plotOutput("lrPlot")
            )
        ),
        verbatimTextOutput("foo")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$summary <- renderPrint({
        summary(diamonds)
    })
    
    output$histPlot <- renderPlot({
        if((input$histvar=='color') || (input$histvar=='clarity') || (input$histvar=='cut'))
            barplot(height=table(diamonds[[input$histvar]]),xlab=input$histvar,
                    ylab='Frequency', col='green')
        else
            hist(diamonds[[input$histvar]], input$nbins, xlab=input$histvar, col='green',
                 main=NULL)
    })
    output$scatterPlot <- renderPlot({
        qplot(get(input$xcol), get(input$ycol) ,data=diamonds, xlab=input$xcol, 
              ylab=input$ycol) + geom_point(colour = "#3366FF", size = 1)
    })
    output$lrPlot <- renderPlot({
        lr = lm(reformulate(input$lmDepVars, 'price'), data=trainVeh)
        predPrice = data.frame(predict(lr, newdata = testVeh))
        names(predPrice)[1] = 'Predicted'
        predPrice$Reference = testVeh[,c('price')]
        qplot(Reference, Predicted, data=predPrice) + geom_point(colour = "#006600", size = 3)
        #predVeh = data.frame(predict(lr, testVeh, level=.95, interval="confidence"))
        #predVeh$Reference = testVeh[,c('price')]
        #qplot(Reference, fit, data=predVeh) + geom_point(colour = "#3366FF", size = 3) + geom_errorbar(aes(ymin = lwr,ymax = upr))        
    })
    output$foo <- renderPrint({
        reformulate(input$lmDepVars, 'price')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
