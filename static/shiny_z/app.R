library(tidyverse)
library(shiny)
nba <- read_csv(here::here("dataset", "playoffs.csv")) %>% 
  select(SeasonStart,PlayerName,`TS%`,AST,TRB,PTS,BLK,`3P%`)

var <- c("TS%","AST","TRB","PTS","BLK","3P%")

var_value <- c("`TS%`","AST","TRB","PTS","BLK","`3P%`")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    radioButtons(inputId = "var_x",
                 label = "x variable:",
                 choiceNames = var,
                 choiceValues = var_value,
                 inline = TRUE),
    
    radioButtons(inputId = "var_y",
                 label = "y variable:",
                 choiceNames = var,
                 choiceValues = var_value,
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
    brushed <- brushedPoints(nba, input$plot_brush, xvar = str_remove_all(input$var_x,'`'), yvar = str_remove_all(input$var_y,'`'))
    print(brushed)
  })

}
shinyApp(ui, server)