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
             includeMarkdown('about.Rmd')
             #verbatimTextOutput("summary")
    ),
    tabPanel("Histogram",
        sidebarLayout(
            sidebarPanel(
                selectInput('histvar', 'Feature', names(diamonds[2:11]),
                            selected=names(diamonds)[[2]]),
                sliderInput('nbins', 'Number of bins', 1, 50, 25)
            ),
            mainPanel(
                plotOutput("histPlot")
            )
        )
    ),
    tabPanel("Scatterplot",
        sidebarLayout(
            sidebarPanel(
                selectInput('xcol', 'X Variable', names(diamonds[2:11]),
                            selected=names(diamonds)[[2]]),
                selectInput('ycol', 'Y Variable', names(diamonds[2:11]),
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
                p('Choose one or more features to use as the basis for predicting
                  a diamond\'s price. The scatterplot at the right will provide a visual
                  clue as to how well the model predicts the prices, as well as the
                  coefficient of determination below. Please refer to the \"About\" tab for
                  descriptions of the features.'),
                checkboxGroupInput("lmIndVars", "Independent Variables", 
                                   names(diamonds[-c(1,8)]),
                                   selected = names(diamonds[2]))
            ),
            mainPanel(
                plotOutput("lrPlot", hover='HOVER!')
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
    linearModel <- reactive({
        lm(reformulate(input$lmIndVars, 'price'), data=trainVeh)
    })
    predPr <- reactive({
        data.frame(predict(linearModel(), newdata = testVeh))
    })
    output$lrPlot <- renderPlot({
        predPrice = predPr()
        names(predPrice)[1] = 'Predicted'
        predPrice$Reference = testVeh[,c('price')]
        qplot(Reference, Predicted, data=predPrice, xlab='Actual Price (USD)',
              ylab='Predicted Price (USD)') + geom_point(colour = "#006600", size = 3) +
              geom_abline(intercept = 0, slope = 1)
        #qplot(Reference, fit, data=predVeh) + geom_point(colour = "#3366FF", size = 3) + geom_errorbar(aes(ymin = lwr,ymax = upr))        
    })
    output$foo <- renderText({
        predPrice = predPr()
        names(predPrice)[1] = 'Predicted'
        predPrice$Reference = testVeh[,c('price')]
        PRESS = sum((predPrice$Reference - predPrice$Predicted)^2)
        RMSEP = sqrt(PRESS/ nrow(predPrice))
        SST = sum((predPrice$Reference - mean(predPrice$Reference))^2)
        R2 = 1 - (PRESS/SST)
        paste('Coefficient of determination R^2 =', R2,
              '\nThis is the fraction of variance in the price that the model can explain - higher is better, 1 is perfect.',
              '\nAlso note how symmetric the points are about the line of true prediction.'
              )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
