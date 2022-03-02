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
  
  # read in the test data, will be replaced with model function
  trace_model_2020 <- read_csv("data/conserv_trace_model_out.csv")
  do_model_2020 <- read_csv("data/do_model_out.csv")
  nitrate_model_2020 <- read_csv("data/nitrate_model_out.csv")
  
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
                            choices = c('Solute', 'Dissolved_Oxygen', 'Nitrate')),
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
            
            ## Model Parameters ##
            
            h2('Model Parameters'),
            p('Input the model parameters for the section of the river you wish to investigate.'),
            fluidRow(
              column(width = 6,
                     # Model Grid
                     bs4Card(
                       # Card info
                       title = "Model Grid",
                       collapsible = FALSE,
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
                       collapsible = FALSE,
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
                       collapsible = FALSE,
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
                       collapsible = FALSE,
                       closable = FALSE,
                       width = 12,
                       selectInput('distance', 'Model River Distance (in m)', 
                                   choices = colnames(trace_model_2020)[-1])
                     ),
                     
                     # Model Summary Statistics
                     bs4Card(
                       # Card info
                       title = "Model Summary Statistics",
                       collapsible = FALSE,
                       closable = FALSE,
                       width = 12,
                       # box output
                       verbatimTextOutput("con_trace_sum")
                     )
                     
                     
              ), # column close
              
              column(8,
                     # Conservative Tracer Graphic Model Output
                     bs4Card(
                       # Card info
                       title = 'Model Output of Conservative Tracer',
                       collapsible = FALSE,
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
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                plotOutput('modelpred'),
                downloadButton('download_modelpred', 'Download Plot')
              ),
              
              # Model Prediction Statistics and Error
              bs4Card(
                # Card info
                title = "Model Prediction Statistics",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                p(HTML("<b>Sum of Error Squared</b>")),
                textOutput('model_error'),
                p(HTML("<b>R Squared</b>")),
                textOutput('model_r_square')
              )
            ), # fluidRow close
            
            fluidRow(
              
              # # Conservative Tracer Table
              bs4Card(
                title = "Conservative Tracer Model Table",
                collapsible = TRUE,
                closable = FALSE,
                width = 8,
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
                collapsible = FALSE,
                closable = FALSE,
                width = 2,
                p("Run DO Data."),
                actionButton('do_slider_refresh', 'Refresh')
                
              ),
              bs4Card(
                title = "DO Model Parameters",
                width = 4,
                collapsible = FALSE,
                closable = FALSE,
                p('Primary production rate constant'),
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
                actionButton('kpp_update', 'Refresh Max/Min')
              ),
              
              
            ), #fluidRow close
            
            ## PAR Output ##
            h2('Photosynthetically Active Radiation (PAR)'),
            
            fluidRow(
              # PAR Summary
              bs4Card(
                # Card info
                title = "PAR Summary",
                collapsible = FALSE,
                closable = FALSE,
                width = 4,
                p('Adjust the upper and lower bounds of the slider to measure statistics for that section in time.'),
                uiOutput("par_slide"),
                p(HTML('<b>PAR Section Statistics</b>')),
                tableOutput('par_stats')
              ),
              
              # PAR
              bs4Card(
                # Card info
                title = "PAR",
                collapsible = FALSE,
                closable = FALSE,
                width = 8,
                plotOutput('par'),
                downloadButton('download_par', 'Download Plot')
              )
              
            ), # fluidRow close
            
            
            ## GPP Output ##
            h2('Gross Primary Production (GPP)'),
            
            fluidRow(
              column(4,
                     # GPP Summary
                     bs4Card(
                       # Card info
                       title = "GPP Summary",
                       collapsible = FALSE,
                       closable = FALSE,
                       width = 12,
                       uiOutput('gpp_slide'),
                       p(HTML('<b>GPP Section Statistics</b>')),
                       tableOutput('gpp_stats')
                       
                     ),
              ), #column close
              column(8,
                     # Gross Primary Production
                     bs4Card(
                       # Card info
                       title = "Gross Primary Production",
                       collapsible = FALSE,
                       closable = FALSE,
                       width = 12,
                       plotOutput('gpp'),
                       downloadButton('download_gpp', 'Download Plot')
                     ),
              ) #column close
            ), # fluidRow close
            
            ## Temp Output ##
            h2('Water Tempurature (C)'),
            
            fluidRow(
              column(4,
                     # Temperature Summary
                     bs4Card(
                       # Card info
                       title = "Temperature Summary",
                       collapsible = FALSE,
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
                       collapsible = FALSE,
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
                collapsible = FALSE,
                closable = FALSE,
                width = 4,
                selectInput('do_distance', 'Model River Distance (in m)', 
                            choices = colnames(do_model_2020)[c(3,4,5,6)]),
                uiOutput('do_slide'),
                p(HTML('<b>Model Statistics</b>')),
                tableOutput('do_stats')
              ),
              
              # do model output
              bs4Card(
                #Card info
                title = "Dissolved Oxygen Diel Model",
                collapsible = FALSE,
                closable = FALSE,
                width = 8,
                plotOutput('do_model'),
                downloadButton('download_do_model', 'Download Plot')
              )
              
            ), # fluidRow close
            
            # DO Model predictions
            fluidRow(
              # Graphic of Model Error
              bs4Card(
                # Card info
                title = "DO Model Observed by Predicted",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                plotOutput('domodelpred'),
                downloadButton('download_domodelpred', 'Download Plot')
              ),
              
              # Model Prediction Statistics and Error
              bs4Card(
                # Card info
                title = "DO Model Prediction Statistics",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                p(HTML("<b>Sum of Error Squared</b>")),
                textOutput('do_model_error'),
                p(HTML("<b>R Squared</b>")),
                textOutput('do_model_r_square')
              )
            ),
            
            fluidRow(
              
              # DO Model Table Output
              bs4Card(
                title = "Dissolved Oxygen Model Table",
                collapsible = TRUE,
                closable = FALSE,
                width = 12,
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
                collapsible = FALSE,
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
                title = 'Select Model Distance from Upstream Value',
                collapsible = FALSE,
                closable = FALSE,
                width = 4,
                selectInput('nitrate_distance', 'Model River Distance (in m)', 
                            choices = colnames(nitrate_model_2020)[c(3,4,5,6,7)]),
                uiOutput('nitrate_slide'),
                p(HTML('<b>Model Statistics</b>')),
                tableOutput('nitrate_stats')
              ),
              
              # do model output
              bs4Card(
                #Card info
                title = "Nitrate Diel Model",
                collapsible = FALSE,
                closable = FALSE,
                width = 8,
                plotOutput('nitrate_model'),
                downloadButton('download_nitrate_model', 'Download Plot')
              )
              
            ),# fluidRow close
            
            # Nitrate Model predictions
            fluidRow(
              # Graphic of Model Error
              bs4Card(
                # Card info
                title = "Nitrate Model Observed by Predicted",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                plotOutput('nitratemodelpred'),
                downloadButton('download_nitratemodelpred', 'Download Plot')
              ),
              
              # Model Prediction Statistics and Error
              bs4Card(
                # Card info
                title = "Nitrate Model Prediction Statistics",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                p(HTML("<b>Sum of Error Squared</b>")),
                textOutput('nitrate_model_error'),
                p(HTML("<b>R Squared</b>")),
                textOutput('nitrate_model_r_square')
              )
            ),
            
            # Nitrate Table
            fluidRow(
              # DO Model Table Output
              bs4Card(
                title = "Nitrate Model Table",
                collapsible = TRUE,
                closable = FALSE,
                width = 12,
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
      
      # Model Grid
      # Segment Distance
      output$dx <- renderPrint({
        dx <- input$riverlen/input$Nb
        print(round(dx, 1))
      })
      
      # upload solute data
      solute_us_ds <- reactive({
        req(input$solute_upload)
        validate(need(input$solute_upload == "Solute.csv", 
                      "Please upload the completed template, Solute.csv."))
        
        solute_data <- input$solute_upload
        
        solute_us_ds <- read_csv(solute_data$datapath)
        return(solute_us_ds)
      })
      
      # upload DO data
      do_us_ds <- reactive({
        req(input$do_upload)
        validate(need(input$do_upload == "Dissolved_Oxygen.csv", 
                      "Please upload the completed template, Dissolved_Oxygen.csv."))
        
        do_data <- input$do_upload
        
        do_us_ds <- read_csv(do_data$datapath)
        return(do_us_ds)
      })
      
      # upload nitrate data
      nitrate_us_ds <- reactive({
        req(input$nitrate_upload)
        validate(need(input$nitrate_upload == "Nitrate.csv", 
                      "Please upload the completed template, Nitrate.csv."))
        
        nitrate_data <- input$nitrate_upload
        
        nitrate_us_ds <- read_csv(nitrate_data$datapath)
        return(nitrate_us_ds)
      })
      
      # template select input
      templateInput <- reactive({
        Solute <- read_csv("templates/Solute.csv")
        Dissolved_Oxygen <- read_csv("templates/Dissolved_Oxygen.csv")
        Nitrate <- read_csv("templates/Nitrate.csv")
        
        switch(input$template,
               "Solute" = Solute,
               "Dissolved_Oxygen" = Dissolved_Oxygen,
               "Nitrate" = Nitrate)
      })
      
      # show the template table
      output$template_head <- renderTable({
        templateInput()
      })
      
      # downlod the template
      output$downloadTemplate <- downloadHandler(
        filename = function() {
          paste(input$template, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(templateInput(), file, row.names = FALSE)
        }
      )
      
      ## Conservative Tracer Tab code ##
      ##################################
      
      
      
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
          geom_line(data = trace_model_2020, aes(x = time_min, 
                                                 y = trace_model_2020[[input$distance]]), 
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
      output$con_trace_sum <- renderPrint({
        summary(trace_model_2020[input$distance])
      })
      
      # Plot of Model prediction vs Observed Downstream value
      output$modelpred <- renderPlot({
        print(modelpred_fn())
      })
      
      modelpred_fn <- function() {
        solute_us_ds() %>% 
          left_join(trace_model_2020) %>%  
          ggplot(aes(x=ds_sensor_obs, y = trace_model_2020[[input$distance]])) +
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
        obs <- solute_us_ds() %>% 
          select(ds_sensor_obs)
        pred <- trace_model_2020 %>% 
          select(input$distance)
        SSE <- sum((obs-pred)^2)
        print(round(SSE, 2))
      })
      
      output$model_r_square <- renderPrint({
        obs <- solute_us_ds() %>% 
          select(ds_sensor_obs)
        pred <- trace_model_2020 %>% 
          select(input$distance)
        df <- data.frame(obs = unlist(obs), pred = unlist(pred))
        fit <- lm(obs~pred, data = df)
        print(summary(fit)$r.squared)
      })
      
      # Conservative Tracer Model Table
      output$tracer_model_table <- renderDataTable({
        tracer_model_table <- solute_us_ds() %>% 
          left_join(trace_model_2020) %>% 
          select(datetime, time_min, us_sensor_obs, ds_sensor_obs, input$distance)
        return(tracer_model_table)
      })
      
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
          geom_line(data = do_model_2020, aes(x= datetime, 
                                              y = do_model_2020[[input$do_distance]]),
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
      
      # DO Model Stats for DO Summary input
      output$do_stats <- renderTable(digits = 3,{
        
        x = input$do_distance
        
        # select the range of data to look at
        y <- do_model_2020 %>% 
          filter(as.POSIXct(do_model_2020$datetime) >= input$do_time_sum[1] 
                 & as.POSIXct(do_model_2020$datetime) <= input$do_time_sum[2]) %>% 
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
          left_join(do_model_2020) %>%  
          ggplot(aes(x=ds_station_obs, y = do_model_2020[[input$do_distance]])) +
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
        pred <- do_model_2020 %>% 
          select(input$do_distance)
        SSE <- sum((obs-pred)^2)
        print(round(SSE, 2))
      })
      
      # DO model error statistics
      output$do_model_r_square <- renderPrint({
        obs <- do_us_ds() %>% 
          select(ds_station_obs)
        pred <- do_model_2020 %>% 
          select(input$do_distance)
        df <- data.frame(obs = unlist(obs), pred = unlist(pred))
        fit <- lm(obs~pred, data = df)
        print(summary(fit)$r.squared)
      })
      
      # DO Model Table
      output$do_model_table <- renderDataTable({
        do_model_table <- do_us_ds() %>% 
          left_join(do_model_2020)
        return(do_model_table)
      })
      
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
      
      # Nitrate model plot output
      output$nitrate_model <- renderPlot({
        print(nitrate_model_fn())
      })
      
      nitrate_model_fn <- function() {
        nitrate_us_ds() %>% 
          ggplot()+
          geom_line(aes(x=datetime, y=us_station_obs), color="green")+
          geom_line(aes(x=datetime, y=ds_station_obs), color="blue")+
          geom_line(data = nitrate_model_2020, aes(x= datetime, 
                                                   y = nitrate_model_2020[[input$nitrate_distance]]),
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
      
      # Model Stats for Nitrate Summary input
      output$nitrate_stats <- renderTable(digits = 3,{
        
        x = input$nitrate_distance
        
        # select the range of data to look at
        y <- nitrate_model_2020 %>% 
          filter(as.POSIXct(nitrate_model_2020$datetime) >= input$nitrate_time_sum[1] 
                 & as.POSIXct(nitrate_model_2020$datetime) <= input$nitrate_time_sum[2]) %>% 
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
          left_join(nitrate_model_2020) %>%  
          ggplot(aes(x=ds_station_obs, y = nitrate_model_2020[[input$nitrate_distance]])) +
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
        pred <- nitrate_model_2020 %>% 
          select(input$nitrate_distance)
        SSE <- sum((obs-pred)^2)
        print(round(SSE, 2))
      })
      
      # Nitrate model error statistics
      output$nitrate_model_r_square <- renderPrint({
        obs <- nitrate_us_ds() %>% 
          select(ds_station_obs)
        pred <- nitrate_model_2020 %>% 
          select(input$nitrate_distance)
        df <- data.frame(obs = unlist(obs), pred = unlist(pred))
        fit <- lm(obs~pred, data = df)
        print(summary(fit)$r.squared)
      })
      
      # Nitrate Model Table
      output$nitrate_model_table <- renderDataTable({
        nitrate_model_table <- nitrate_us_ds() %>% 
          left_join(nitrate_model_2020)
        return(nitrate_model_table)
      })
      
    } # server functions close
  ) # shinyApp close
} # if iteractive close
