# Solute Transport Web App

## Only run this example in interactive R sessions
if (interactive()) {
  
  library(shiny)
  library(shinyjs)
  library(htmlwidgets)
  library(bs4Dash)
  library(shinyTime)
  library(tidyverse)
  library(lubridate)
  library(readr)
  
  shinyApp(
    ui = dashboardPage(
      
      ## header ##
      ############
      
      header = dashboardHeader(
        skin = "dark",
        title = dashboardBrand(
          title = "Reactive Solute Transport",
          color = "secondary"
        ) # dashboardBrand close
      ), #dashboardHeader close 
      
      ## Body ##
      ##########
      
      body = dashboardBody(
        
        # Reactive Tab Displays
        tabItems(
          
          ## Tab item model parameters ##
          ###############################
          
          tabItem(
            tabName = 'parameters',
            h1("Model Parameters"),
            h2("Template Download"),
            p("Start by downloading and completing a data template. To ensure that the program will work properly be sure to maintain headings, column formats, and .csv naming conventions."),
            
            ## Template Download ##
            
            fluidRow(
              bs4Card(
                title = "Template Download",
                width = 3,
                closable = FALSE,
                selectInput('template', 'Choose a template:',
                            choices = c('Conservative Tracer', 'Dissolved Oxygen', 'Nitrate', 'Conservative Tracer Model', 'DO Model', 'Nitrate Model')),
                downloadButton('downloadTemplate',
                               'Download Template:', 
                               icon=shiny::icon('download'))
              ),
              bs4Card(
                title = "Data Download Examples",
                p('The "datetime" column should be in the format "yyyy-mm-dd hh:mm:ss".'),
                p('The "time_min" column is the number of minutes between each observation starting with "0".'),
                width = 9,
                closable = FALSE,
                tableOutput('template_head')
              )
            ), # fluidRow close
            
            ## Data upload ##
            
            h2('Template Data Upload'),
            p('Upload data stored in the templates to the proper data upload section.'),
            fluidRow(
              column(width = 4,
                     
                     bs4Card(
                       title = "Solute Upload",
                       width = 12,
                       closable = F,
                       p('Upload the "Solute.csv" file after completing and saving your data to the template.'),
                       fileInput('solute_upload', 
                                 'Browse for CSV Solute Template', 
                                 accept = ".csv")
                     )
                     
              ), # column close
              
              column(width = 4,
                     
                     bs4Card(
                       title = "Dissolved Oxygen Upload",
                       width = 12,
                       closable = F,
                       p('Upload the "Dissolved_Oxygen.csv" file after completing and saving your data to the template.'),
                       fileInput('do_upload', 
                                 'Browse for CSV DO Template', 
                                 accept = ".csv")
                     )   
              ), # column close
              
              column(width = 4,
                     
                     bs4Card(
                       title = "Nitrate Upload",
                       width = 12,
                       closable = F,
                       p('Upload the "Nitrate.csv" file after completing and saving your data to the template.'),
                       fileInput('nitrate_upload', 
                                 'Browse for CSV Nitrate Template', 
                                 accept = ".csv")
                     )   
              ) # column close
            ), # fluidRow close
            
            
            ## Model Data Upload ##
            
            h2('Model Data Upload'),
            p('Upload model data stored in the model templates to the proper data upload section.'),
            fluidRow(
              column(width = 4,
                     
                     bs4Card(
                       title = "Solute Model Upload",
                       width = 12,
                       closable = FALSE,
                       fileInput('solute_model_upload', 
                                 'Browse for CSV Solute Model Data', 
                                 accept = ".csv")
                     )
                     
              ), # column close
              
              column(width = 4,
                     
                     bs4Card(
                       title = "Dissolved Oxygen Model Upload",
                       width = 12,
                       closable = FALSE,
                       fileInput('do_model_upload', 
                                 'Browse for CSV DO Model Data', 
                                 accept = ".csv")
                     )   
              ), # column close
              
              column(width = 4,
                     
                     bs4Card(
                       title = "Nitrate Model Upload",
                       width = 12,
                       closable = FALSE,
                       fileInput('nitrate_model_upload', 
                                 'Browse for CSV Nitrate Model Data', 
                                 accept = ".csv")
                     )   
              ) # column close
            ), # fluidRow close
            
            ## Model Parameters ##
            
            h2('Model Parameters'),
            p('Input the model parameters for the section of the river you wish to investigate.'),
            fluidRow(
              column(width = 6,
                     # Model Grid
                     bs4Card(
                       # Card info
                       title = "Model Grid",
                       closable = FALSE,
                       width = 12,
                       p('Input the distance between the upstream and downstream measurements which you are investigating within the model.'),
                       numericInput('riverlen', 
                                    'River Length between Upstream and Downstream Boundaries (m)',
                                    value = 1000),
                       p('Input the number of segments the model should run. Distance in meters for each segment is output below.'), 
                       sliderInput('Nb', 'Grid Number', value = 50, min = 1, max = 150),
                       "Segment Distance (m)",
                       verbatimTextOutput("dx")
                     ),
              ), # column close
              
              column(width = 3,
                     # Measured Model Parameters
                     bs4Card(
                       # Card info
                       title = "Measured Model Parameters",
                       closable = FALSE,
                       width = 12,
                       p('Input the average depth in meters for the length of river you are modeling. Input the average river discharge for the time you are modeling.'),
                       numericInput('depth', 'Average Depth (m)', value = 5),
                       numericInput('Q', 'Discharge (in m^3/s)', value = 5)
                     ),
              ),# column close
              
              column(width = 3,
                     # Calculate Model Parameters
                     bs4Card(
                       # Card info
                       title = "Calculated Model Parameters",
                       closable = FALSE,
                       width = 12,
                       numericInput('area', 'Channel Area (in m^2)', value = 10),
                       numericInput('store_area', 'Storage Area (in m^2)', value = 0.5),
                       numericInput('disp', 'Dispersion (in m^2/s)', value = 10),
                       numericInput('alpha', 'Exchange Coefficient', value = 0.00001)
                     ),
              ) # column close
            ), # fluidRow close
            
            
          ), # tabItem close
          
          ## Tab item conservative tracer ##
          ##################################
          
          tabItem(
            tabName = "tracer",
            h1("Conservative Tracer"),
            
            ## Model Output ##
            h2('Model Output'),
            fluidRow(
              column(4,
                     # Model distance
                     bs4Card(
                       # Card info
                       title = "Select Model distance from Upstream Value",
                       closable = FALSE,
                       width = 12,
                       uiOutput('select_dis_tracer')
                     ),
                     
                     # Model Summary Statistics
                     bs4Card(
                       # Card info
                       title = "Model Summary Statistics",
                       closable = FALSE,
                       width = 12,
                       # box output
                       tableOutput("con_trace_sum")
                     )
                     
                     
              ), # column close
              
              column(8,
                     # Conservative Tracer Graphic Model Output
                     bs4Card(
                       # Card info
                       title = 'Model Output of Conservative Tracer',
                       closable = FALSE,
                       width = 12,
                       # Graph selection and output
                       plotOutput('modelplot'),
                       downloadButton('download_modelplot', 'Download Plot')
                     )
              ),
            ), #FluidRow close
            
            ## Model Prediction Statistics ##
            h2('Conservative Tracer Model Prediction Statistics'),
            p('Set Model River Distance to Downstream Observed Location to test model output.'),
            
            fluidRow(
              # Graphic of Model Error
              bs4Card(
                # Card info
                title = "Model Observed by Predicted",
                closable = FALSE,
                width = 6,
                plotOutput('modelpred'),
                downloadButton('download_modelpred', 'Download Plot')
              ),
              
              # Model Prediction Statistics and Error
              bs4Card(
                # Card info
                title = "Model Prediction Statistics",
                closable = FALSE,
                width = 6,
                p(HTML("<b>Sum of Error Squared</b>")),
                textOutput('model_error'),
                p(HTML("<br>")),
                p(HTML("<b>R Squared</b>")),
                textOutput('model_r_square')
              )
            ), # fluidRow close
            
            
            h2('Conservative Tracer Model Table'),
            p('Select model distance from upstream boundry in the Model River Distance selection interface to see other modeled outputs in this table.'),
            fluidRow(
              # # Conservative Tracer Table
              bs4Card(
                title = "Conservative Tracer Model Table",
                closable = FALSE,
                width = 8,
                downloadButton('download_solutetable', 'Download Table'),
                p(HTML("<br>")),
                dataTableOutput('tracer_model_table')
              )
            ) # fluidRow close
          ), # tabItem close
          
          ## Tab item dissolved oxygen ##
          ###############################
          
          tabItem(
            tabName = "do",
            h1("Dissolved Oxygen"),
            
            # Reactive DO Parameters
            fluidRow(
              
              bs4Card(
                # Card info
                title = "Run Uploaded DO Data",
                closable = FALSE,
                width = 2,
                p("Run DO Data. Refresh the Dissolved Oxygen Data after data upload."),
                actionButton('do_slider_refresh', 'Refresh')
                
              ),
              bs4Card(
                title = "DO GPP Model Parameters",
                width = 4,
                closable = FALSE,
                p('Primary Production Rate Constant'),
                fluidRow(
                  column(6,
                         
                         numericInput('kpp_min', 
                                      'kpp min:', 
                                      value = 0.0004),
                  ), # close column
                  column(6,
                         numericInput('kpp_max',
                                      'kpp max:', 
                                      value = 0.0006)
                  ) # close column
                ), # close fluidRow
                uiOutput('kpp_slider'),
                p('Refreshing the Primary Production Rate Constant will update the Gross Primary Production Value.'),
                actionButton('kpp_update', 'Refresh Max/Min')
              ),
              
              # ER calculation
              bs4Card(
                title = "DO ER Model Parameters",
                width = 4,
                closable = FALSE,
                p('Respiration Rate Constant'),
                numericInput('kER', 'kER:', value = 0.002),
                p('Respiration Rate Exponent'),
                numericInput('nER', 'nER:', value = .3),
                h3('ER'),
                textOutput('er_out')
              )
              
            ), #fluidRow close
            
            ## PAR Output ##
            h2('Photosynthetically Active Radiation (PAR)'),
            
            fluidRow(
              # PAR Summary
              bs4Card(
                # Card info
                title = "PAR Input",
                closable = FALSE,
                width = 6,
                p('Adjust the upper and lower bounds of the slider to measure statistics for that selection in time.'),
                uiOutput("par_slide")
              ),
              
              bs4Card(
                # Card info
                title = "PAR Statistics",
                closable = FALSE,
                width = 6,
                tableOutput('par_stats')
              ),
              
              # PAR
              bs4Card(
                # Card info
                title = "PAR",
                closable = FALSE,
                width = 6,
                plotOutput('par'),
                p('PAR Figure displays the entirety of the uploaded PAR data. Adjusting the upper and lower bound of the PAR Input Slider will adjust the red indicator lines. These red verticle lines display the selection of data for statistical read out and the Detail View.'),
                downloadButton('download_par', 'Download Plot')
              ),
              
              # PAR
              bs4Card(
                # Card info
                title = "PAR Detail View",
                closable = FALSE,
                width = 6,
                plotOutput('par_zoom'),
                p('PAR Detail View. Adjust the upper and lower bounds on the PAR Input Slider to adjust the view selection.'),
                downloadButton('download_par_zoom', 'Download Plot')
              )
              
            ), # fluidRow close
            
            
            ## GPP Output ##
            h2('Gross Primary Production (GPP)'),
            p('GPP is a function of the Primary Production Rate Constant times Par divided by depth.'),
            p('Be sure to calculate each measure to determine GPP'),
            fluidRow(
              # GPP Summary
              bs4Card(
                # Card info
                title = "GPP Input",
                closable = FALSE,
                width = 6,
                p('Adjust the upper and lower bounds of the slider to measure statistics for that section in time.'),
                uiOutput('gpp_slide')
              ),
              
              bs4Card(
                # Card info
                title = "GPP Statistics",
                closable = FALSE,
                width = 6,
                tableOutput('gpp_stats')
              ),
              
              # Gross Primary Production
              bs4Card(
                # Card info
                title = "Gross Primary Production",
                closable = FALSE,
                width = 6,
                plotOutput('gpp'),
                p('GPP Figure displays the entirety of the GPP data. Adjusting the upper and lower bound of the GPP Input Slider will adjust the red indicator lines. These red verticle lines display the selection of data for statistical read out and the Detail View.'),
                downloadButton('download_gpp', 'Download Plot')
              ),
              
              # Gross Primary Production
              bs4Card(
                # Card info
                title = "Gross Primary Production Detail View",
                closable = FALSE,
                width = 6,
                plotOutput('gpp_zoom'),
                p('GPP Detail View. Adjust the upper and lower bounds on the GPP Input Slider to adjust the view selection.'),
                downloadButton('download_gpp_zoom', 'Download Plot')
              )
              
            ), # fluidRow close
            
            ## Temp Output ##
            h2('Water Tempurature (C)'),
            
            fluidRow(
              column(4,
                     # Temperature Summary
                     bs4Card(
                       # Card info
                       title = "Temperature Summary",
                       closable = FALSE,
                       width = 12,
                       uiOutput('temp_slide'),
                       tableOutput('temp_stats')
                     ),
                     
              ), # column close
              column(8,
                     # Average Temperature Graph
                     bs4Card(
                       # Card info
                       title = "Temperature",
                       closable = FALSE,
                       width = 12,
                       plotOutput('temp'),
                       downloadButton('download_temp', 'Download Plot')
                     ),
              ) # column close
            ), # fluidRow
            
            ## DO Output ##
            
            h2('Dissolved Oxygen Diel Model Output'),
            fluidRow(
              
              # do Model distance
              bs4Card(
                # Card info
                title = "Select Model Distance from Upstream Value",
                closable = FALSE,
                width = 6,
                uiOutput('select_dis_do'),
                uiOutput('do_slide')
              ),
              
              # do statistics
              bs4Card(
                # Card info
                title = "Dissolved Oxygen Statistics",
                closable = FALSE,
                width = 6,
                p(HTML('<b>Model Statistics</b>')),
                tableOutput('do_stats')
              ),
              
              # do model output
              bs4Card(
                #Card info
                title = "Dissolved Oxygen Diel Model",
                closable = FALSE,
                width = 6,
                plotOutput('do_model'),
                p('DO Figure displays the entirety of the DO data. Adjusting the upper and lower bound of the DO Input Slider will adjust the red indicator lines, selecting model locations will change model distance from the upstream boundary. These red verticle lines display the selection of data for statistical read out and the Detail View.'),
                downloadButton('download_do_model', 'Download Plot')
              ),
              
              # do model detail view
              bs4Card(
                #Card info
                title = "Dissolved Oxygen Diel Model Detail View",
                closable = FALSE,
                width = 6,
                plotOutput('do_model_zoom'),
                p('DO Model Detail View. Adjust the upper and lower bounds on the DO Input Slider to adjust the view selection.'),
                downloadButton('download_do_model_zoom', 'Download Plot')
              )
            ), # fluidRow close
            
            # DO Model predictions
            h2('Dissolved Oxygen Model Prediction Statistics'),
            p('Set Model River Distance to Downstream Observed Location to test model output.'),
            fluidRow(
              # Graphic of Model Error
              bs4Card(
                # Card info
                title = "DO Model Observed by Predicted",
                closable = FALSE,
                width = 6,
                plotOutput('domodelpred'),
                downloadButton('download_domodelpred', 'Download Plot')
              ),
              
              # Model Prediction Statistics and Error
              bs4Card(
                # Card info
                title = "DO Model Prediction Statistics",
                closable = FALSE,
                width = 6,
                p(HTML("<b>Sum of Error Squared</b>")),
                textOutput('do_model_error'),
                p(HTML("<br>")),
                p(HTML("<b>R Squared</b>")),
                textOutput('do_model_r_square')
              )
            ),
            
            # DO Model Table Output
            h2('Dissolved Oxygen Model Table'),
            p('Select model distance from upstream boundry in the Model River Distance selection interface to see other modeled outputs in this table.'),
            fluidRow(
              bs4Card(
                title = "Dissolved Oxygen Model Table",
                closable = FALSE,
                width = 12,
                downloadButton('download_dotable', 'Download Table'),
                p(HTML("<br>")),
                dataTableOutput('do_model_table')
              )
            ) # Table fluidRow Close
            
          ), # tabItem close
          
          ## Tab item Nitrate ##
          ######################
          
          tabItem(
            tabName = "no3",
            h1("Nitrate"),
            fluidRow(
              
              bs4Card(
                # Card info
                # Card info
                title = "Run Uploaded Nitrate Data", 
                closable = FALSE,
                width = 2,
                p("Run Nitrate Data."),
                actionButton('nitrate_refresh', 'Refresh')
              )
              
            ), # fluidRow close
            h2('Nitrate Diel Model Output'),
            fluidRow(
              # Nitrate Model input
              bs4Card(
                title = 'Select Nitrate Model Distance from Upstream Value',
                closable = FALSE,
                width = 6,
                uiOutput('select_dis_nitrate'),
                uiOutput('nitrate_slide')
              ),
              
              bs4Card(
                title = 'Nitrate Statistics',
                closable = FALSE,
                width = 6,
                p(HTML('<b>Model Statistics</b>')),
                tableOutput('nitrate_stats')
              ),
              
              # nitrate model output
              bs4Card(
                #Card info
                title = "Nitrate Diel Model",
                closable = FALSE,
                width = 6,
                plotOutput('nitrate_model'),
                p('Nitrate Figure displays the entirety of the Nitrate data. Adjusting the upper and lower bound of the Nitrate Input Slider will adjust the red indicator lines, selecting model locations will change model distance from the upstream boundary. These red verticle lines display the selection of data for statistical read out and the Detail View.'),
                downloadButton('download_nitrate_model', 'Download Plot')
              ),
              
              # nitrate model detail output
              bs4Card(
                #Card info
                title = "Nitrate Diel Model Detail View",
                closable = FALSE,
                width = 6,
                plotOutput('nitrate_model_zoom'),
                p('Mitrate Model Detail View. Adjust the upper and lower bounds on the DO Input Slider to adjust the view selection.'),
                downloadButton('download_nitrate_model_zoom', 'Download Plot')
              )
              
            ),# fluidRow close
            
            # Nitrate Model predictions
            h2('Nitrate Model Prediction Statistics'),
            p('Set Model River Distance to Downstream Observed Location to test model output.'),
            fluidRow(
              # Graphic of Model Error
              bs4Card(
                # Card info
                title = "Nitrate Model Observed by Predicted",
                closable = FALSE,
                width = 6,
                plotOutput('nitratemodelpred'),
                downloadButton('download_nitratemodelpred', 'Download Plot')
              ),
              
              # Model Prediction Statistics and Error
              bs4Card(
                # Card info
                title = "Nitrate Model Prediction Statistics",
                closable = FALSE,
                width = 6,
                p(HTML("<b>Sum of Error Squared</b>")),
                textOutput('nitrate_model_error'),
                p(HTML("<br>")),
                p(HTML("<b>R Squared</b>")),
                textOutput('nitrate_model_r_square')
              )
            ),
            
            # Nitrate Table
            h2('Nitrate Model Table'),
            p('Select model distance from upstream boundry in the Model River Distance selection interface to see other modeled outputs in this table.'),
            fluidRow(
              # DO Model Table Output
              bs4Card(
                title = "Nitrate Model Table",
                closable = FALSE,
                width = 8,
                downloadButton('download_nitratetable', 'Download Table'),
                p(HTML("<br>")),
                dataTableOutput('nitrate_model_table')
              )
            )
            
          ) # tabItem close
          
        ) #tabItems close
      ), # dashboadbody close
      
      ## Dashboard Sidebar Menu ##
      ############################
      
      sidebar = dashboardSidebar(
        skin = "light",
        inputId = "sidebarState",
        sidebarMenu(
          id = "sidebar",
          
          # menu item model parameters
          menuItem(
            text = "Model Parameters",
            tabName = "parameters",
            selected = TRUE,
            icon = icon('gears', lib = 'font-awesome', verify_fa = FALSE)
          ),
          
          # menu item conservative tracer
          menuItem(
            text = "Conservative Tracer",
            tabName = "tracer",
            selected = FALSE,
            icon = icon('fill-drip', lib = 'font-awesome', verify_fa = FALSE)
          ),
          
          # menu item dissolved oxygen
          menuItem(
            text = "Dissolved Oxygen",
            tabName = "do",
            icon = icon('circle', lib = 'font-awesome', verify_fa = FALSE)
          ),
          
          # menu item nitrate
          menuItem(
            text = "Nitrate",
            tabName = "no3",
            icon = icon('seedling', lib = 'font-awesome', verify_fa = FALSE)
          )
        ) # sidebarMenu close
      ), # dashboardSidebar close
      
      ## Footer ##
      ############
      
      footer = bs4DashFooter(
        right = "Andrew Hauer, Rafael Feijo del Lima",
        left = "University of Montana"
      ) # bs4DashFooter close
    ), # UI dashboardPage close
    
    ##################################################################    
    #                    server functions                            #
    ##################################################################
    
    server = function(input, output, session) {
      
      observe(print(input$sidebarItemExpanded))
      observe(print(input$sidebar))
      
      # update tabs
      observeEvent(input$controller,
                   {
                     updateTabItems(
                       session,
                       inputId = "sidebar",
                       selected = paste0("tab", input$controller)
                     )
                   },
                   ignoreInit = TRUE
      )
      
      ## Model Parameters Tab code ##
      ###############################
      
      # template select input
      templateInput <- reactive({
        Solute <- read_csv("templates/Solute.csv")
        Dissolved_Oxygen <- read_csv("templates/Dissolved_Oxygen.csv")
        Nitrate <- read_csv("templates/Nitrate.csv")
        conserv_trace_model_out <- read_csv("templates/conserv_trace_model_out.csv")
        do_model_out <- read_csv("templates/do_model_out.csv")
        nitrate_model_out <- read_csv("templates/nitrate_model_out.csv")
        
        switch(input$template,
               "Conservative Tracer" = Solute,
               "Dissolved Oxygen" = Dissolved_Oxygen,
               "Nitrate" = Nitrate,
               "Conservative Tracer Model" = conserv_trace_model_out,
               "DO Model" = do_model_out,
               "Nitrate Model" = nitrate_model_out)
      })
      
      # show the template table
      output$template_head <- renderTable({
        templateInput()
      })
      
      # download the template
      output$downloadTemplate <- downloadHandler(
        filename = function() {
          paste(input$template, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(templateInput(), file, row.names = FALSE)
        }
      )
      
      # upload solute data
      solute_us_ds <- reactive({
        req(input$solute_upload)
        
        solute_data <- input$solute_upload
        
        solute_us_ds <- read_csv(solute_data$datapath)
        return(solute_us_ds)
      })
      
      # upload DO data
      do_us_ds <- reactive({
        req(input$do_upload)
        
        do_data <- input$do_upload
        
        do_us_ds <- read_csv(do_data$datapath)
        return(do_us_ds)
      })
      
      # upload nitrate data
      nitrate_us_ds <- reactive({
        req(input$nitrate_upload)
        
        nitrate_data <- input$nitrate_upload
        
        nitrate_us_ds <- read_csv(nitrate_data$datapath)
        return(nitrate_us_ds)
      })
      
      ## import model data
      
      # upload solute model data
      solute_model_import <- reactive({
        req(input$solute_model_upload)
        
        solute_model <- input$solute_model_upload
        
        solute_model_import <- read_csv(solute_model$datapath)
        return(solute_model_import)
      })
      
      # upload DO data
      do_model_import <- reactive({
        req(input$do_model_upload)
        
        do_model <- input$do_model_upload
        
        do_model_import <- read_csv(do_model$datapath)
        return(do_model_import)
      })
      
      # upload nitrate data
      nitrate_model_import <- reactive({
        req(input$nitrate_model_upload)
        
        nitrate_model <- input$nitrate_model_upload
        
        nitrate_model_import <- read_csv(nitrate_model$datapath)
        return(nitrate_model_import)
      })
      
      solute_model <- function () {
        return(solute_model_import())
      }
      
      do_model <- function () {
        return(do_model_import())
      }
      
      nitrate_model <- function () {
        return(nitrate_model_import())
      }
      
      # Model Grid
      # Segment Distance
      output$dx <- renderPrint({
        dx <- input$riverlen/input$Nb
        print(round(dx, 1))
      })
      
      ## Conservative Tracer Tab code ##
      ##################################
      
      output$select_dis_tracer <- renderUI(
        selectInput('distance', 'Model River Distance (in m)', 
                    choices = colnames(solute_model())[c(-1, -2)])
      )
      
      # Plot of Model of Conservative Tracer by Time by Distance
      output$modelplot <- renderPlot({
        print(modelplot_fn())
        
      })
      
      modelplot_fn <- function() {
        ggplot()+
          geom_point(data = solute_us_ds(), aes(x = time_min, 
                                                y = us_sensor_obs), 
                     color = 'Green') +
          geom_point(data = solute_us_ds(), aes(x = time_min, 
                                                y = ds_sensor_obs), 
                     color = 'blue') +
          geom_line(data = solute_model(), aes(x = time_min, 
                                               y = solute_model()[[input$distance]]), 
                    color = 'red')+
          labs(x="Minutes", y="Conservative Trace (in ppb)", title= "Conservative Tracer Model", 
               subtitle = "Up stream observed values (in green) and Down stream observed values (in blue) with modeled values (in red)")+
          theme_bw()
      }
      
      output$download_modelplot <- downloadHandler(
        filename = "ConservativeTracer.png",
        content = function(file) {
          ggsave(file, plot = modelplot_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        }) 
      
      # Conservative Tracer Summary Statistics
      output$con_trace_sum <- renderTable({
        solute_model() %>%
          summarise(Max = max(solute_model()[input$distance]),
                    Min = min(solute_model()[input$distance]),
                    Mean = mean(unlist(solute_model()[input$distance])),
                    Median = median(unlist(solute_model()[input$distance])))
      })
      
      # Plot of Model prediction vs Observed Downstream value
      output$modelpred <- renderPlot({
        print(modelpred_fn())
      })
      
      modelpred_fn <- function() {
        solute_us_ds() %>% 
          left_join(solute_model()) %>%  
          ggplot(aes(x=ds_sensor_obs, y = solute_model()[[input$distance]])) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(x="Observed", y="Predicted", 
               title = "Conservative Tracer Observed by Predicted") +
          theme_bw()
      }
      
      output$download_modelpred <- downloadHandler(
        filename = "ConservativeTracer_predict.png",
        content = function(file) {
          ggsave(file, plot = modelpred_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        }) 
      
      
      
      # Model predicted vs observed summary stats
      
      
      output$model_error <- renderPrint({
        obs <- unlist(solute_us_ds() %>% 
                        select(ds_sensor_obs))
        pred <- unlist(solute_table_fn() %>% 
                         select(input$distance))
        SSE <- sum((obs-pred)^2)
        print(round(SSE, 2))
      })
      
      output$model_r_square <- renderPrint({
        df <- solute_table_fn() %>% 
          mutate(pred = solute_table_fn()[[input$distance]]) %>% 
          select(ds_sensor_obs, pred)
        fit <- lm(ds_sensor_obs ~ pred, data = df)
        print(summary(fit)$r.squared)
      })
      
      # Conservative Tracer Model Table
      output$tracer_model_table <- renderDataTable({
        print(solute_table_fn())
      })
      
      solute_table_fn <- function() {
        tracer_model_table <- solute_us_ds() %>% 
          left_join(solute_model()) %>% 
          select(datetime, time_min, us_sensor_obs, ds_sensor_obs, input$distance)
        return(tracer_model_table)
      }
      
      # download the do table
      output$download_solutetable <- downloadHandler(
        filename = 'solute_table_output.csv',
        content = function(file) {
          write.csv(solute_table_fn(), file, row.names = FALSE)
        }
      )
      
      ## Dissolved Oxygen Tab code ##
      ###############################
      
      # Update Sliders for Data Date Time
      # find min date
      min_do_dt <- eventReactive(input$do_slider_refresh, {
        min(do_us_ds()$datetime)
      })
      
      # find max date
      max_do_dt <- eventReactive(input$do_slider_refresh, {
        max(do_us_ds()$datetime)
      })
      
      # find time interval
      step_do_dt <- eventReactive(input$do_slider_refresh, {
        (do_us_ds()$time_min[2]-do_us_ds()$time_min[1])*60
      })
      
      output$select_dis_do <- renderUI(
        selectInput('do_distance', 'Model River Distance (in m)', 
                    choices = colnames(do_model())[c(-1, -2)])
      )
      
      
      # DO Model Parameters
      
      kpp_min <- eventReactive(input$kpp_update, {
        as.numeric(input$kpp_min)
      })
      
      kpp_max <- eventReactive(input$kpp_update, {
        as.numeric(input$kpp_max)
      })
      
      output$kpp_slider <- renderUI(
        sliderInput('kpp', 'kpp Slider',
                    min = kpp_min(), 
                    max = kpp_max(),
                    value = kpp_min() +(.5*(kpp_max()-kpp_min())))
      )
      
      
      
      # Par
      
      # create PAR slider with renderUI
      output$par_slide <- renderUI(
        sliderInput('par_time_sum', 'PAR Time Input:', 
                    timeFormat = "%F %T", 
                    min = as.POSIXct(min_do_dt()),
                    max = as.POSIXct(max_do_dt()),
                    value = c(min_do_dt(), min_do_dt() + (.25*(max_do_dt()-min_do_dt()))),
                    step = 1)
      )
      
      # plot PAR
      output$par <- renderPlot({
        print(par_fn())
      })
      
      par_fn <- function() {
        do_us_ds() %>%
          ggplot(aes(x=datetime, y=par))+
          geom_line(color="blue")+
          geom_vline(aes(xintercept=input$par_time_sum[1]), color="red")+
          geom_vline(aes(xintercept=input$par_time_sum[2]), color="red")+
          labs(x="Datetime", y="PAR", 
               title = "PAR by Date and Time")+
          theme_bw()
      }
      
      output$download_par <- downloadHandler(
        filename = "par.png",
        content = function(file) {
          ggsave(file, plot = par_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        }) 
      
      # PAR Stats for PAR Summary input
      output$par_stats <- renderTable(digits = 7,{
        do_us_ds() %>%
          filter(as.POSIXct(do_us_ds()$datetime) >= input$par_time_sum[1] 
                 & as.POSIXct(do_us_ds()$datetime) <= input$par_time_sum[2]) %>%
          summarise(Max = max(par),
                    Min = min(par),
                    Mean = mean(par)) 
        
      })
      
      
      # plot Zoom Par
      output$par_zoom <-  renderPlot({
        print(par_zoom_fn())
      })
      
      par_zoom_fn <- function() {
        do_us_ds() %>% 
          filter(as.POSIXct(datetime) >= input$par_time_sum[1] 
                 & as.POSIXct(datetime) <= input$par_time_sum[2]) %>%
          ggplot(aes(x=datetime, y=par))+
          geom_line(color="blue")+
          labs(x="Datetime", y = "PAR",
               title = "PAR by Date and Time",
               subtitle = "Detail View of PAR")+
          theme_bw()
      }
      
      output$download_par_zoom <- downloadHandler(
        filename = "par_detail.png",
        content = function(file) {
          ggsave(file, plot = par_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        })
      
      # Gross Primary Production
      # create GPP slider with renderUI
      output$gpp_slide <- renderUI(
        sliderInput('gpp_time_sum', 'GPP Time Input:', 
                    timeFormat = "%F %T", 
                    min = as.POSIXct(min_do_dt()),
                    max = as.POSIXct(max_do_dt()),
                    value = c(min_do_dt(), min_do_dt() + (.25*(max_do_dt()-min_do_dt()))),
                    step = 1)
      ) 
      
      # plot GPP
      output$gpp <- renderPlot({
        print(gpp_fn())
      })
      
      gpp_fn <- function() {
        do_us_ds() %>% 
          mutate(gpp = (((par*input$kpp)/input$depth)/1000)) %>% 
          ggplot(aes(x=datetime, y=gpp))+
          geom_line(color="blue")+
          geom_vline(aes(xintercept=input$gpp_time_sum[1]), color="red")+
          geom_vline(aes(xintercept=input$gpp_time_sum[2]), color="red")+
          labs(x="Datetime", y="Gross Primary Production", 
               title = "Gross Primary Production by Date and Time")+
          theme_bw()
      }
      
      output$download_gpp <- downloadHandler(
        filename = "gpp.png",
        content = function(file) {
          ggsave(file, plot = gpp_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        })
      
      # plot GPP Zoom
      output$gpp_zoom <- renderPlot({
        print(gpp_zoom_fn())
      })
      
      gpp_zoom_fn <- function() {
        do_us_ds() %>% 
          mutate(gpp = (((par*input$kpp)/input$depth)/1000)) %>%
          filter(as.POSIXct(datetime) >= input$gpp_time_sum[1] 
                 & as.POSIXct(datetime) <= input$gpp_time_sum[2]) %>%
          ggplot(aes(x=datetime, y=gpp))+
          geom_line(color="blue")+
          labs(x="Datetime", y="Gross Primary Production", 
               title = "Gross Primary Production by Date and Time",
               subtitle = "Detail View of GPP")+
          theme_bw()
      }
      
      output$download_gpp_zoom <- downloadHandler(
        filename = "gpp_detail.png",
        content = function(file) {
          ggsave(file, plot = gpp_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        })
      
      # Gross Primary Production Stats for GPP Summary input
      output$gpp_stats <- renderTable(digits = 7,{
        do_us_ds() %>%
          filter(as.POSIXct(do_us_ds()$datetime) >= input$gpp_time_sum[1] 
                 & as.POSIXct(do_us_ds()$datetime) <= input$gpp_time_sum[2]) %>% 
          mutate(gpp = (((par*input$kpp)/input$depth)/1000)) %>% 
          summarise(Max = max(gpp),
                    Min = min(gpp),
                    Mean = mean(gpp)) 
        
      })
      
      # create Temp slider with renderUI
      output$temp_slide <- renderUI(
        sliderInput('temp_time_sum', 'Temperature Time Input:', 
                    timeFormat = "%F %T", 
                    min = as.POSIXct(min_do_dt()),
                    max = as.POSIXct(max_do_dt()),
                    value = min_do_dt(),
                    step = step_do_dt())
      ) 
      
      # Temperature
      output$temp <- renderPlot({
        print(temp_fn())
      })
      
      temp_fn <- function () {
        do_us_ds() %>%
          ggplot(aes(x=datetime, y=avgtemp))+
          geom_line(color="purple")+
          geom_vline(aes(xintercept=input$temp_time_sum), color="red")+
          labs(x="Datetime", y="Temperature (c)", 
               title= "Average Temperature")+
          theme_bw()
      }
      
      output$download_temp <- downloadHandler(
        filename = "Tempurature.png",
        content = function(file) {
          ggsave(file, plot = temp_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        }
      )
      
      # Temperature table output
      output$temp_stats <- renderTable(digits = 2,{
        do_us_ds() %>%  
          filter(as.POSIXct(do_us_ds()$datetime) %in% input$temp_time_sum) %>% 
          mutate("Temperature (C)" = avgtemp) %>% 
          select("Temperature (C)")
      })
      
      ## DO Model
      # create Do model slider with renderUI
      output$do_slide <- renderUI(
        sliderInput('do_time_sum', 'DO Time Input:', 
                    timeFormat = "%F %T", 
                    min = as.POSIXct(min_do_dt()),
                    max = as.POSIXct(max_do_dt()),
                    value = c(min_do_dt(), min_do_dt() + (.25*(max_do_dt()-min_do_dt()))),
                    step = 1)
      ) 
      
      # do model plot output
      output$do_model <- renderPlot({
        print(do_model_fn())
      })
      
      do_model_fn <- function() {
        do_us_ds() %>% 
          ggplot()+
          geom_line(aes(x=datetime, y=us_station_obs), color="green")+
          geom_line(aes(x=datetime, y=ds_station_obs), color="blue")+
          geom_line(data = do_model(), aes(x= datetime, 
                                           y = do_model()[[input$do_distance]]),
                    color = "red")+
          geom_vline(aes(xintercept=input$do_time_sum[1]), color="red")+
          geom_vline(aes(xintercept=input$do_time_sum[2]), color="red")+
          labs(x="Datetime", y="Dissolved Oxygen", 
               title = "Dissolved Oxygen Model", 
               subtitle = "Up river observations in Green, Down river observations in Blue, Model in Red")+
          theme_classic()
      }
      
      output$download_do_model <- downloadHandler(
        filename = "DissolvedOxygenModel.png",
        content = function(file) {
          ggsave(file, plot = do_model_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        }
      )
      
      # plot DO Zoom
      output$do_model_zoom <- renderPlot({
        print(do_model_zoom_fn())
      })
      
      do_model_zoom_fn <- function() {
        do_us_ds() %>%
          mutate(model = do_model()[[input$do_distance]]) %>% 
          filter(as.POSIXct(do_us_ds()$datetime) >= input$do_time_sum[1] 
                 & as.POSIXct(do_us_ds()$datetime) <= input$do_time_sum[2]) %>%
          ggplot()+
          geom_line(aes(x = datetime, y = us_station_obs), color="green")+
          geom_line(aes(x = datetime, y = ds_station_obs), color="blue")+
          geom_line(aes(x = datetime,y = model),
                    color = "red")+
          labs(x="Datetime", y="Dissolved Oxygen", 
               title = "Dissolved Oxygen Model Detail View", 
               subtitle = "Up river observations in Green, Down river observations in Blue, Model in Red")+
          theme_classic()
      }
      
      output$download_do_model_zoom <- downloadHandler(
        filename = "do_model_detail.png",
        content = function(file) {
          ggsave(file, plot = do_model_zoom_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        })
      
      # DO Model Stats for DO Summary input
      output$do_stats <- renderTable(digits = 3,{
        
        x = input$do_distance
        
        # select the range of data to look at
        y <- do_model() %>% 
          filter(as.POSIXct(do_model()$datetime) >= input$do_time_sum[1] 
                 & as.POSIXct(do_model()$datetime) <= input$do_time_sum[2]) %>% 
          select(x)
        
        # unlist the stored y data and find max, min, mean
        y <- as.numeric(unlist(y))
        a = max(y)
        b = min(y)
        c = mean(y)
        
        # store in tibble to be called as rendered table
        do_stats <- tibble("Max" = a, "Min" = b, "Mean" = c)
        return(do_stats)
      })
      
      # Plot of Model prediction vs Observed Downstream value
      output$domodelpred <- renderPlot({
        print(domodelpred_fn())
      })
      
      domodelpred_fn <- function() {
        do_us_ds() %>% 
          left_join(do_model()) %>%  
          ggplot(aes(x=ds_station_obs, y = do_model()[[input$do_distance]])) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(x="Observed", y="Predicted", 
               title = "Dissolved Oxygen Observed by Predicted") +
          theme_bw()
      }
      
      output$download_domodelpred <- downloadHandler(
        filename = "DissolvedOxygenModel_prediction.png",
        content = function(file) {
          ggsave(file, plot = domodelpred_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        }
      )
      
      # DO Model predicted vs observed summary stats
      output$do_model_error <- renderPrint({
        obs <- do_us_ds() %>% 
          select(ds_station_obs)
        pred <- do_model() %>% 
          select(input$do_distance)
        SSE <- sum((obs-pred)^2)
        print(round(SSE, 2))
      })
      
      # DO model error statistics
      output$do_model_r_square <- renderPrint({
        obs <- do_us_ds() %>% 
          select(ds_station_obs)
        pred <- do_model() %>% 
          select(input$do_distance)
        df <- data.frame(obs = unlist(obs), pred = unlist(pred))
        fit <- lm(obs~pred, data = df)
        print(summary(fit)$r.squared)
      })
      
      # DO Model Table
      do_table_fn <- function() {
        do_model_table <- do_us_ds() %>% 
          left_join(do_model()) %>% 
          mutate(gpp = (((par*input$kpp)/input$depth)/1000)) %>%
          select(datetime, time_min, us_station_obs, ds_station_obs, par, gpp, avgtemp,input$do_distance)
        return(do_model_table)
      }
      
      output$do_model_table <- renderDataTable({
        print(do_table_fn())
      })
      
      # download the do table
      output$download_dotable <- downloadHandler(
        filename = 'DO_table_output.csv',
        content = function(file) {
          write.csv(do_table_fn(), file, row.names = FALSE)
        }
      )
      
      ## Nitrate Tab code ##
      ######################
      
      # find min date
      min_nitrate_dt <- eventReactive(input$nitrate_refresh, {
        min(nitrate_us_ds()$datetime)
      })
      
      # find max date
      max_nitrate_dt <- eventReactive(input$nitrate_refresh, {
        max(nitrate_us_ds()$datetime)
      })
      
      # find time interval
      step_nitrate_dt <- eventReactive(input$nitrate_refresh, {
        (nitrate_us_ds()$time_min[2]-nitrate_us_ds()$time_min[1])*60
      })
      
      ## Nitrate Model
      # create Nitrate model slider with renderUI
      output$nitrate_slide <- renderUI(
        sliderInput('nitrate_time_sum', 'Nitrate Time Input:', 
                    timeFormat = "%F %T", 
                    min = as.POSIXct(min_nitrate_dt()),
                    max = as.POSIXct(max_nitrate_dt()),
                    value = c(min_nitrate_dt(), min_nitrate_dt() + (.25*(max_nitrate_dt()-min_nitrate_dt()))),
                    step = 1)
      ) 
      
      output$select_dis_nitrate <- renderUI(
        selectInput('nitrate_distance', 'Model River Distance (in m)', 
                    choices = colnames(nitrate_model())[c(-1,-2)])
      )
      
      
      
      # Nitrate model plot output
      output$nitrate_model <- renderPlot({
        print(nitrate_model_fn())
      })
      
      nitrate_model_fn <- function() {
        nitrate_us_ds() %>% 
          ggplot()+
          geom_line(aes(x=datetime, y=us_station_obs), color="green")+
          geom_line(aes(x=datetime, y=ds_station_obs), color="blue")+
          geom_line(data = nitrate_model(), aes(x= datetime, 
                                                y = nitrate_model()[[input$nitrate_distance]]),
                    color = "red")+
          geom_vline(aes(xintercept=input$nitrate_time_sum[1]), color="red")+
          geom_vline(aes(xintercept=input$nitrate_time_sum[2]), color="red")+
          labs(x="Datetime", y="Nitrate", 
               title = "Nitrate Model", 
               subtitle = "Up river observations in Green, Down river observations in Blue, Model in Red")+
          theme_classic()
      }
      
      output$download_nitrate_model <- downloadHandler(
        filename = "NitrateModel.png",
        content = function(file) {
          ggsave(file, plot = nitrate_model_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        }
      )
      
      # plot Nitrate Zoom
      output$nitrate_model_zoom <- renderPlot({
        print(nitrate_model_zoom_fn())
      })
      
      nitrate_model_zoom_fn <- function() {
        nitrate_us_ds() %>%
          mutate(model = nitrate_model()[[input$nitrate_distance]]) %>% 
          filter(as.POSIXct(nitrate_us_ds()$datetime) >= input$nitrate_time_sum[1] 
                 & as.POSIXct(nitrate_us_ds()$datetime) <= input$nitrate_time_sum[2]) %>%
          ggplot()+
          geom_line(aes(x = datetime, y = us_station_obs), color="green")+
          geom_line(aes(x = datetime, y = ds_station_obs), color="blue")+
          geom_line(aes(x = datetime,y = model),
                    color = "red")+
          labs(x="Datetime", y="Nitrate", 
               title = "Nitrate Model Detail View", 
               subtitle = "Up river observations in Green, Down river observations in Blue, Model in Red")+
          theme_classic()
      }
      
      output$download_nitrate_model_zoom <- downloadHandler(
        filename = "nitrate_model_detail.png",
        content = function(file) {
          ggsave(file, plot = nitrate_model_zoom_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        })
      
      # Model Stats for Nitrate Summary input
      output$nitrate_stats <- renderTable(digits = 3,{
        
        x = input$nitrate_distance
        
        # select the range of data to look at
        y <- nitrate_model() %>% 
          filter(as.POSIXct(nitrate_model()$datetime) >= input$nitrate_time_sum[1] 
                 & as.POSIXct(nitrate_model()$datetime) <= input$nitrate_time_sum[2]) %>% 
          select(x)
        
        # unlist the stored y data and find max, min, mean
        y <- as.numeric(unlist(y))
        a = max(y)
        b = min(y)
        c = mean(y)
        
        # store in tibble to be called as rendered table
        nitrate_stats <- tibble("Max" = a, "Min" = b, "Mean" = c)
        return(nitrate_stats)
      })
      
      # Plot of Model prediction vs Observed Downstream value
      output$nitratemodelpred <- renderPlot({
        print(nitratemodelpred_fn())
      })
      
      nitratemodelpred_fn <- function() {
        nitrate_us_ds() %>% 
          left_join(nitrate_model()) %>%  
          ggplot(aes(x=ds_station_obs, y = nitrate_model()[[input$nitrate_distance]])) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(x="Observed", y="Predicted", 
               title = "Nitrate Observed by Predicted") +
          theme_bw()
      }
      
      output$download_nitratemodelpred <- downloadHandler(
        filename = "NitrateModel_prediction.png",
        content = function(file) {
          ggsave(file, plot = nitratemodelpred_fn(), width = 11, 
                 height = 8.5, units = 'in', dpi=320)
        }
      )
      
      # Nitrate Model predicted vs observed summary stats
      output$nitrate_model_error <- renderPrint({
        obs <- nitrate_us_ds() %>% 
          select(ds_station_obs)
        pred <- nitrate_model() %>% 
          select(input$nitrate_distance)
        SSE <- sum((obs-pred)^2)
        print(round(SSE, 2))
      })
      
      # Nitrate model error statistics
      output$nitrate_model_r_square <- renderPrint({
        obs <- nitrate_us_ds() %>% 
          select(ds_station_obs)
        pred <- nitrate_model() %>% 
          select(input$nitrate_distance)
        df <- data.frame(obs = unlist(obs), pred = unlist(pred))
        fit <- lm(obs~pred, data = df)
        print(summary(fit)$r.squared)
      })
      
      # Nitrate Model Table
      nitrate_table_fn <- function() {
        nitrate_model_table <- nitrate_us_ds() %>% 
          left_join(nitrate_model()) %>% 
          select(datetime, time_min, us_station_obs, ds_station_obs, input$nitrate_distance)
        return(nitrate_model_table)
      }
      
      output$nitrate_model_table <- renderDataTable({
        print(nitrate_table_fn())
      })
      
      # download the nitrate table
      output$download_nitratetable <- downloadHandler(
        filename = 'nitrate_table_output.csv',
        content = function(file) {
          write.csv(nitrate_table_fn(), file, row.names = FALSE)
        }
      )
      
    } # server functions close
  ) # shinyApp close
} # if iteractive close
