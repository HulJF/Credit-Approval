
#http://shiny.rstudio.com/


library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(plotly)

Application <- read.csv("application_record.csv", stringsAsFactors = TRUE )

Record <- read.csv("credit_record.csv", stringsAsFactors = TRUE )

Credit <- merge(Application, Record, 
                by = "ID" , # by common variable
                all = FALSE, 
                all.Application = all, 
                all.Record = all,
                sort = TRUE, # Should the result be sorted on the by columns
                suffixes = c(".Application",".Record")
)

Credit$IncreaseCL <- as.factor(
  ifelse ( Credit$AMT_INCOME_TOTAL <= -109.73 & 
             Credit$STATUS == "X"|
             Credit$STATUS == "C", "Yes", ifelse(  Credit$STATUS == "4" | 
                                                   Credit$STATUS == "5", "No", "Possible") ))

Credit$MONTHS_BIRTH <- Credit$DAYS_BIRTH/30
Credit$MONTHS_EMPLOYED <- Credit$DAYS_EMPLOYED/30

data <- Credit

#Define User Interface
ui <- fluidPage(
  titlePanel("Credit Situation"),
  sidebarPanel(
    radioButtons(inputId = "CreditLine",label = "Increase Credit Line", 
                 choices = unique(Credit$IncreaseCL )),
    selectInput(inputId = "Education",label = "Education Level", 
                choices = unique(Credit$NAME_EDUCATION_TYPE)),
    selectInput(inputId = "Marital",label = "Marital Status", 
                choices = unique(Credit$NAME_FAMILY_STATUS)),
    sliderInput(inputId = "Child", label = "Number of Children",
                min = min(Credit$CNT_CHILDREN ),
                max = max(Credit$CNT_CHILDREN),
                value = min(Credit$CNT_CHILDREN)),
    selectInput(inputId = "Income",label = "Job type", 
                choices = unique(Credit$NAME_INCOME_TYPE ))
         ),

  # Show a plot of the generated distribution
mainPanel(
  plotlyOutput("MyPlot")
)
)


# Define server logic 
server <- function(input, output){
    output$MyPlot <- renderPlotly({
      plot <-
        Credit %>% filter(IncreaseCL == input$CreditLine) %>%
        filter(NAME_EDUCATION_TYPE == input$Education) %>%
        filter(NAME_INCOME_TYPE == input$Income) %>%
        filter(NAME_FAMILY_STATUS == input$Marital) %>%
        filter(CNT_CHILDREN == input$Child) %>% 
        ggplot() + 
        geom_point(mapping = aes(x = MONTHS_EMPLOYED, 
                                 y = MONTHS_BALANCE,
                                 color = CODE_GENDER), alpha = 3)+ 
        theme(legend.position = "none") +
        theme_clean()+
        labs(title = "Work Experience VS Credit Months Due", 
                           x = "Work Experience in months", 
                           y = "Credit Balance Duration in months",
             color = "Gender")
      ggplotly(plot, tooltip = "text")
})
}

# Run the application 
shinyApp(ui = ui, server = server)
