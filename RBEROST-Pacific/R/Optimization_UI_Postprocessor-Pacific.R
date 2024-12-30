PostprocessingTab <- tabPanel(
  title = "Postprocessing Step",
  titlePanel(title = "RBEROST Results Postprocessing Step"), 
  tabsetPanel(
    type = "tabs", 
    tabPanel(
      "File Preview", 
      conditionalPanel(
        condition = "input.preview > 0", 
        uiOutput("previews") %>% withSpinner(color = "#0dc5c1")# having the spinner here for whatever reason doesn't work. The headers load long before the tables
      )
    ), 
    ##################################################
    # 4. Results Tab
    ##################################################
    tabPanel(
      "View Results", 
      selectInput(
        inputId = "scenario", 
        label = "View available scenarios", 
        choices = c(1),
        selected = 1
      ),
      conditionalPanel(
        condition = "input.ViewReport > 0",
        uiOutput("report") %>% withSpinner(color = "#0dc5c1")
      )
    )
  ) 
)
