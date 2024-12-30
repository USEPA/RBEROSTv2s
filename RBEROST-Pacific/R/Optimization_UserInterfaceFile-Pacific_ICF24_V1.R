###########################################################################################
# TITLE: Postprocessing Shiny App User Interface
# PURPOSE: To allow users to input a text file and see the results of their NEOS optimization
# BY: Cathy Chamberlin
# DATE: 3/24/2022
###########################################################################################

# This file is NOT intended to be run by itself. It is sourced through 02_Optimization_RunShiny.R (which in turn is sourced through RunRBEROST.Rmd) along with Optimization_ServerFile.R to run the shiny app.
# Lines marked #*# may be changed by the user.
# Multiple components are commented out of the code - these include many of the Preprocessing inputs for the CT study.
# These lines have not been changed for the Puget Sound, because the Preprocessor has not yet been built into the Shiny App.
#*# KAM: I don't think this file needs to be changed any further for Puget Sound - perhaps just delete the commented out lines?
# The commented code may be useful for future work to include the Preprocessor into the Shiny App

# The components of the User Interface are roughly organized as such:

##################################################
# Table of Contents:
# 1. General Formatting
# 2. Side Bar for uploading files
# 3. Preview Tab
# 4. Results Tab
##################################################

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


ui <- fluidPage(
  # 1.General Formatting ------------------------------------------------------
  tags$head(
    tags$style(
      HTML(
        ".shiny-output-error-validation {color: #0dc5c1; font-style: italic;}"
      )
    )
  ),
  
  # 2. Side bar construction --------------------------------------------------
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      
      ## 2.1: NEOS results input -----------------------------------------------
      conditionalPanel(
        condition = "input.steps == 'Postprocessing Step'",
        fileInput( 
          inputId = "NEOS_results",
          label = "Choose Results File",
          multiple = FALSE, 
          accept = c("text/plain", ".txt")
        ),
        tags$hr() 
      ),   
      
      ## 2.2: UserSpecs Loading Targets input ----------------------------------
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'",
        fileInput( 
          inputId = "user_loading",
          label = "Choose the corresponding `01_UserSpecs_loadingtargets.csv` or `01_UserSpecs_loadingtargets_selected.csv` file",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),  
      
      ## 2.3: WWTP input ------------------------------------------------------ 
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'",
        fileInput( 
          inputId = "point_comid",
          label = "Choose WWTP File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),  
      
      ## 2.4 Sparrow input ----------------------------------------------------
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'", 
        fileInput( 
          inputId = "sparrow_in",
          label = "Choose SPARROW Inputs File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),  
      
      ## 2.5: StreamCat Cropland input ----------------------------------------
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'", 
        fileInput(
          inputId = "streamcat_crop_files_all", 
          label = "Choose State Cropland Streamcat Files", 
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr()
      ),
      
      #@ SE: Added urban and road data input boxes on 5-15-24
      ## 2.6: Urban Data Input ----------------------------------------
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'",
        fileInput( 
          inputId = "urban_input",
          label = "Choose Urban Area File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ), 
      
      ## 2.7: Road Data Input ----------------------------------------
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'",
        fileInput( 
          inputId = "road_input",
          label = "Choose Road Area File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),
      
      conditionalPanel(
        condition = "input.steps == 'Postprocessing Step'", 
        p(em("Important: Selecting 'Preview Uploads' or 'View NEOS Results' before all uploads have completed may produce unexpected results. Previews can be found under the 'File Preview' tab, and generated reports can be found under the 'View Results' tab.")),
        actionButton("preview", label = "Preview Uploads"),
        actionButton("ViewReport", label = "View NEOS Results")
      )
    ),
    mainPanel = mainPanel(
      tabsetPanel(
        type = "pills",
        id = "steps",
        # PreprocessingTab,
        # NEOSInterfaceTab,
        PostprocessingTab
      )
    )
  )
)
