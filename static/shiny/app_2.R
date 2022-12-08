library(tidyverse)
library(shiny)
nba <- read_csv("playoffs.csv") %>% select(SeasonStart,PlayerName,`TS%`,AST,TRB,PTS,BLK,`3P%`)

var <- c("TS%","AST","TRB","PTS","BLK","3P%")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    radioButtons(inputId = "var_x",
                 label = "x variable:",
                 choices = var,
                 inline = TRUE),
    
    radioButtons(inputId = "var_y",
                 label = "y variable:",
                 choices = var,
                 inline = TRUE),
    
    plotOutput("plot",  brush = "plot_brush" ), 
    verbatimTextOutput("info")
)


server <- function (input, output) {
  output$plot <- renderPlot({
    ggplot(nba, aes_string(input$var_x, input$var_y)) + geom_point()
  })

  output$info <- renderPrint({
    cat(str_c("Window: ",
              input$var_x, "= (", input$plot_brush$xmin, ", ", input$plot_brush$xmax, ")\n", "        ",
              input$var_y, "= (", input$plot_brush$ymin, ", ", input$plot_brush$ymax, ")\n"))
    brushed <- brushedPoints(nba, input$plot_brush, xvar = input$var_x, yvar = input$var_y)
    print(brushed)
  })

}
shinyApp(ui, server)