setwd("C:/Users/Siddhesh Pisal/Documents/App-1")

library(shiny)
#topten <- read.csv("toptenmutualfunds.csv")


ui <- shinyUI(
  fluidPage(
    titlePanel(h1("Mutual Fund Analyzer/Ranker")),
    sidebarLayout(
      sidebarPanel(
        helpText("Top Ranking Schemes"),
        selectInput("var", "Choose one mutual fund: ", choices = unique(topten$Scheme.Name)),
        textOutput("text1"),textOutput("text2"),plotOutput("smallplot")),
      mainPanel(h1("Net asset value ~ Time"),
                plotOutput("plot"),
                dataTableOutput("table"))
)))


server <- shinyServer(function(input, output) {
  
#This function renders the Unique Scheme ID on the app
output$text1 <- renderText({
    id <- unique(topten$Scheme.Code[topten$Scheme.Name == input$var])
    paste("Scheme ID :", id)
  })

#This function renders the maximum NAV value till date for selected fund
  output$text2 <- renderText({
    navmax <- max(topten$Net.Asset.Value[topten$Scheme.Name == input$var])
    paste("Max N.A.V. till date :", navmax)
})

    
#This function renders the main NAV plot in our APP  
output$plot <- renderPlot({
    date <- topten$Date[topten$Scheme.Name == input$var]
    netval <- topten$Net.Asset.Value[topten$Scheme.Name == input$var]
    dataplot <- as.data.frame(cbind(as.Date(date), netval))
    ggplot(data = dataplot, aes(date, netval)) + geom_line(color = "orange", size = 1) +
      theme_bw() + xlab("Time") + ylab("Net Asset Value")
    
})
  
#This function prints the table for the selected scheme
output$table <- renderDataTable(topten[topten$Scheme.Name == input$var,])
})



shinyApp(ui, server)
