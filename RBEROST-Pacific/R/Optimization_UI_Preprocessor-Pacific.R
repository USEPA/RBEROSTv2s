PreprocessingTab <- tabPanel(
      title = "Preprocessing Step",
      titlePanel(
        title = "River Basin Export Reduction Optimization Support Tool (RBEROST) Preprocessing Step"
      ),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
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
          
          tags$hr(), 
          
          fileInput( 
            inputId = "point_loadings_adj",
            label = "Choose WWTP Baseline Removals File",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  
          
          tags$hr(), 
          
          fileInput( 
            inputId = "point_efficiencies",
            label = "Choose WWTP Efficiencies File",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  

          tags$hr(), 
          
          fileInput( 
            inputId = "urban_efficiencies",
            label = "Choose Urban Efficiency Curves File",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ), 
          
          tags$hr(), 
          
          selectInput(
            inputId = "sparrow",
            label = "Select Northeast Sparrow Files",
            choices = c(
              "Inputs File" = "in", 
              "Total Nitrogen" = "TN", "
              Total Phosphorus" = "TP"
            )
          ),
          
          conditionalPanel(
            condition = "input.sparrow == 'in'", 
            fileInput( 
              inputId = "sparrow_in",
              label = "Choose SPARROW Inputs File",
              multiple = FALSE, 
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),  
          
          conditionalPanel(
            condition = "input.sparrow == 'TN'", 
            fileInput(
              "sparrow_tn", 
              label = "Choose SPARROW Nitrogen Outputs File", 
              multiple = FALSE, 
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.sparrow == 'TP'", 
            fileInput(
              "sparrow_tp", 
              label = "Choose SPARROW Phosphorus Outputs File", 
              multiple = FALSE, 
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          
          tags$hr(), 
          
          fileInput( 
            inputId = "streambank_in",
            label = "Choose Streambank File",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  
          
          tags$hr(), 
          
          fileInput( 
            inputId = "riparian_loadings",
            label = "Choose Riparian Loadings File",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  
          
          tags$hr(), 
          
          fileInput( 
            inputId = "riparian_efficiencies",
            label = "Choose Riparian Efficiencies File",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  
          
          tags$hr(), 
          
          fileInput( 
            inputId = "ACRE_summary",
            label = "Choose ACRE HUC12 HRU Summary",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  
          
          tags$hr(), 
          
          fileInput( 
            inputId = "fertmanure",
            label = "Choose Efficiency File for Fert_20 and Manure Injection",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  
          
          tags$hr(), 
          
          fileInput( 
            inputId = "NdepChange",
            label = "Choose Ndep Change File",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  
          
          tags$hr(), 
          
          fileInput( 
            inputId = "NHDplus_infiltration",
            label = "Choose NHD+v2 Infiltration Rates File",
            multiple = FALSE, 
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),  
          
          tags$hr(), 
          
          selectInput(
            inputId = "streamcat_crop",
            label = "Select Streamcat 2011 Cropland State File",
            choices = c("Vermont" = "VT", "New Hampshire" = "NH")
          ),
          
          conditionalPanel(
            condition = "input.streamcat_crop == 'VT'", 
            fileInput(
              "streamcat_crop_file_vt", 
              label = "Choose Streamcat File", 
              multiple = FALSE, 
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.streamcat_crop == 'NH'", 
            fileInput(
              "streamcat_crop_file_nh", 
              label = "Choose Streamcat File", 
              multiple = FALSE, 
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          
          tags$hr(), 
          
          selectInput(
            inputId = "streamcat_imp",
            label = "Select Streamcat 2011 Imperviousness State File",
            choices = c("Vermont" = "VT", "New Hampshire" = "NH")
          ),
          
          conditionalPanel(
            condition = "input.streamcat_imp == 'VT'", 
            fileInput(
              "streamcat_imp_file_vt", 
              label = "Choose Streamcat File", 
              multiple = FALSE, 
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.streamcat_imp == 'NH'", 
            fileInput(
              "streamcat_imp_file_nh", 
              label = "Choose Streamcat File", 
              multiple = FALSE, 
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          )
        ),
        mainPanel = mainPanel()
      )
    )