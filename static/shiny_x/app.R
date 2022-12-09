
library(shiny)
library(tidyverse)

playoffs <- read_csv(here::here("dataset", "playoffs.csv"))

data <- playoffs %>% 
  filter((SeasonStart == "2013" & 
            Tm%in%c("IND","MIA", "TOR", "CHA", "WAS", "BRK","CHI", "ATL","SAS",
                            "OKC","LAC","HOU","POR","GSW","MEM","DAL")) | 
           (SeasonStart == "2014" & Tm%in%c("CLE","BOS", "TOR", "NOP", "WAS", 
                                            "BRK","CHI", "ATL","SAS","MIL",
                                            "LAC","HOU","POR","GSW","MEM",
                                            "DAL")) |
           (SeasonStart == "2015" & Tm%in%c("IND","MIA", "TOR", "CHA", "CLE", 
                                            "DET","BOS", "ATL","SAS", "OKC",
                                            "LAC","HOU","POR","GSW","MEM",
                                            "DAL")))
test <- data %>% 
  filter(SeasonStart == "2013") %>% 
  filter(Tm == "NOP")

Tm_choice <- playoffs %>% pull(Tm) %>%
  unique() %>% sort()

var <- c("`TS%`", "AST", "TRB", "PTS", "BLK", "`3P%`")

var_name <- c("TS%", "AST", "TRB", "PTS", "BLK", "3P%")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NBA Data"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("SeasonStart",
                        "Start Season:",
                        min = 2013,
                        max = 2015,
                        value = 2013),
        
        selectInput(inputId = "Tm",
                    label = "Team:",
                    choices = Tm_choice,
                    multiple = TRUE,
                    selected = "SAS"), 
        
        
        radioButtons(inputId = "var",
                    label = "Variable:",
                    choiceNames = var_name, 
                    choiceValues = var, 
                    inline = TRUE)
        
            ),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      data %>% 
        filter(SeasonStart == input$SeasonStart) %>% 
        filter(Tm %in% (input$Tm)) %>% 
        ggplot() +
        stat_summary(aes_string(x = "Tm", y = input$var), 
                     fun = mean, geom = "bar", na.rm = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
