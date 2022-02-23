# Solute Transport Web App

## Only run this example in interactive R sessions
if (interactive()) {
  
  library(shiny)
  library(bs4Dash)
  library(shinyTime)
  library(tidyverse)
  library(lubridate)
  library(readr)
  
  # read in the 2020 conservative tracer data
  trace_model_2020 <- read_csv("data/conserv_trace_model_out.csv")
  
  shinyApp(
    ui = dashboardPage(
      
      header = dashboardHeader(
        skin = "dark",
        title = dashboardBrand(
          title = "Reactive Solute Transport",
          color = "secondary"
        ) # dashboardBrand close
      ), #dashboardHeader close 
      
      body = dashboardBody(
        
        # Reactive Tab Displays
        tabItems(
          
          # Tab item model parameters
          tabItem(
            tabName = "parameters",
            h1("Model Parameters"),
            fluidRow(
              
              
              column(width = 6,
                     # Model Grid
                     bs4Card(
                       # Card info
                       title = "Model Grid",
                       collapsible = FALSE,
                       closable = FALSE,
                       width = 12,
                       numericInput('riverlen', 
                                    'River Length between Upstream and Downstream Boundaries (in m)',
                                    value = 1000),
                       sliderInput('Nb', 'Grid Number', value = 50, min = 1, max = 150),
                       "Segment Distance",
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
                       numericInput('depth', 'Average Depth (in m)', value = 5),
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
            
            # Data upload
            fluidRow(
              column(width = 4,
                     
                     bs4Card(
                       title = "Solute Upload",
                       width = 12,
                       closable = F,
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
                       fileInput('nitrate_upload', 
                                 'Browse for CSV Nitrate Template', 
                                 accept = ".csv")
                     )   
              ), # column close
              
              
            ), # fluidRow close
            
            # Template Download
            fluidRow(
              bs4Card(
                title = "Template Download",
                width = 3,
                closable = FALSE,
                selectInput('template', "Choose a template:",
                            choices = c('Solute', 'Dissolved_Oxygen', 'Nitrate')),
                downloadButton('downloadTemplate',
                               'Download Template:', 
                               icon=shiny::icon('download'))
              ),
              bs4Card(
                title = "Data Download Information",
                width = 9,
                closable = FALSE,
                tableOutput('template_head')
              )
            ), # fluidRow close
          ), # tabItem close
          
          # Tab item conservative tracer
          tabItem(
            tabName = "tracer",
            h1("Conservative Tracer"),
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
                       plotOutput('modelplot')
                     ),
              ),
              # Graphic of Model Error
              bs4Card(
                # Card info
                title = "Model Observed by Predicted",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                plotOutput('modelpred')
              ),
              
              # Model Prediction Statistics and Error
              bs4Card(
                # Card info
                title = "Model Prediction Statistics",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                "Sum of Error Squared",
                textOutput('model_error'),
                "R Squared",
                textOutput('model_r_square')
              )
              
            ), # fluidRow close
          ), # tabItem close
          
          # Tab item dissolved oxygen
          tabItem(
            tabName = "do",
            h1("Dissolved Oxygen"),
            fluidRow(
              # Reactive DO Parameters
              bs4Card(
                # Card info
                title = "GPP Reactive Parameters",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                numericInput('kpp', 
                             'Primary Production Constant (in mg-O/J): ', 
                             value = 0.0005)
              ),
              bs4Card(
                # Card info
                title = "Slider Inputs",
                collapsible = FALSE,
                closable = FALSE,
                width = 6,
                "Update Sliders to Uploaded DO data.",
                verbatimTextOutput("test"),
                actionButton('do_slider_refresh', 'Refresh Date/Time')
              ),
            ), #fluidRow close
            
            fluidRow(
              column(4,
                     # GPP Summary
                     bs4Card(
                       # Card info
                       title = "GPP Summary",
                       collapsible = FALSE,
                       closable = FALSE,
                       width = 12,
                       sliderInput('gpp_time_sum', 'GPP Time Input:', 
                                   timeFormat = "%F %T", 
                                   min = as.POSIXct('2020-01-01 12:00:00'),
                                   max = as.POSIXct('2020-01-30 12:00:00'),
                                   value = c(as.POSIXct('2020-01-01 12:00:00'),
                                             as.POSIXct('2020-01-15 12:00:00'))),
                       
                       tableOutput('gpp_stats')
                       
                     ),
              ),
              column(8,
                     # Gross Primary Production
                     bs4Card(
                       # Card info
                       title = "Gross Primary Production",
                       collapsible = FALSE,
                       closable = FALSE,
                       width = 12,
                       plotOutput('gpp')
                     ),
              ),
            ),
            fluidRow(
              column(4,
                     # Temperature Summary
                     bs4Card(
                       # Card info
                       title = "Temperature Summary",
                       collapsible = FALSE,
                       closable = FALSE,
                       width = 12,
                       sliderInput('temp_time_sum', 'Tempurature Time Input:', 
                                   timeFormat = "%F %T", 
                                   min = as.POSIXct('2020-01-01 12:00:00'),
                                   max = as.POSIXct('2020-01-30 12:00:00'),
                                   value = as.POSIXct('2020-01-01 12:00:00'),
                                   step = 600),
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
                       plotOutput('temp')
                     ),
                     
              ), # column close
            ), # fluidRow
            # DO Output
            
            fluidRow(
              bs4Card(
                #Card info
                title = "Disolved Oxygen Diel Model",
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                plotOutput('do_model')
              )
              
              
            ), # fluidRow close
          ), # tabItem close
          
          # Tab item Nitrate
          tabItem(
            tabName = "no3",
            h1("Nitrate"),
            fluidRow(
              
              bs4Card(
                # Card info
                title = "Nitrate",
                collapsible = FALSE,
                closable = FALSE,
                width = 6
              )
              
            ) # fluidRow close
          ) # tabItem close
        ) #tabItems close
      ), # dashboadbody close
      
      # Dashboard Sidebar Menu
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
            icon = icon('gears', lib = 'font-awesome')
          ),
          
          # menu item conservative tracer
          menuItem(
            text = "Conservative Tracer",
            tabName = "tracer",
            selected = FALSE,
            icon = icon('fill-drip', lib = 'font-awesome')
          ),
          
          # menu item dissolved oxygen
          menuItem(
            text = "Dissolved Oxygen",
            tabName = "do",
            icon = icon('circle', lib = 'font-awesome')
          ),
          
          # menu item nitrate
          menuItem(
            text = "Nitrate",
            tabName = "no3",
            icon = icon('seedling', lib = 'font-awesome')
          )
        ) # sidebarMenu close
      ), # dashboardSidebar close
      
      # Footer
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
      
      ## Model Parameters Tab code
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
      
      ## Conservative Tracer Tab code
      
      # Plot of Model of Conservative Tracer by Time by Distance
      output$modelplot <- renderPlot({
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
      })
      
      # Conservative Tracer Summary Statistics
      output$con_trace_sum <- renderPrint({
        summary(trace_model_2020[input$distance])
      })
      
      # Plot of Model prediction vs Observed Downstream value
      output$modelpred <- renderPlot({
        solute_us_ds() %>% 
          left_join(trace_model_2020) %>% 
          select(ds_sensor_obs, Loc_4320m) %>% 
          ggplot(aes(x=ds_sensor_obs, y = Loc_4320m)) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(x="Observed", y="Predicted", 
               title = "Conservative Tracer Observed by Predicted") +
          theme_bw()
      })
      
      # Model predicted vs observed summary stats
      output$model_error <- renderPrint({
        obs <- solute_us_ds() %>% 
          select(ds_sensor_obs)
        pred <- trace_model_2020 %>% 
          select(Loc_4320m)
        SSE <- sum((obs-pred)^2)
        print(round(SSE, 2))
      })
      
      output$model_r_square <- renderPrint({
        obs <- solute_us_ds() %>% 
          select(ds_sensor_obs)
        pred <- trace_model_2020 %>% 
          select(Loc_4320m)
        df <- data.frame(obs = unlist(obs), pred = unlist(pred))
        fit <- lm(obs~pred, data = df)
        print(summary(fit)$r.squared)
      })
      
      ## Dissolved Oxygen Tab code
      
      # Gross Primary Production
      
      
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
      #max_do_dt <- eventReactive(input$do_slider_refresh, {
      #  max(do_us_ds()$datetime)
      #})
      
      
      
      # update GPP slider reactive to do data
      observeEvent(input$do_slider_refresh,{
      val_min <- min_do_dt()
      val_max <- max_do_dt()
      
      updateSliderInput(session = getDefaultReactiveDomain(),
        'gpp_time_sum', 
        min = val_min,
        max = val_max,
        value = c(val_min,val_min + (.25*(val_max-val_min))))
      })  
      
      
      
      # update Temp slider reactive to do data
      observeEvent(input$do_slider_refresh,{
        val_min <- min_do_dt()
        val_max <- max_do_dt()
        
        
        updateSliderInput(session = getDefaultReactiveDomain(),
                          'temp_time_sum', 
                          min = val_min,
                          max = val_max,
                          value = val_min)
      })
      
      # plot GPP
      output$gpp <- renderPlot({
        do_us_ds() %>% 
          mutate(gpp = (((par*input$kpp)/input$depth)/1000)) %>% 
          ggplot(aes(x=datetime, y=gpp))+
          geom_line(color="blue")+
          geom_vline(aes(xintercept=input$gpp_time_sum[1]), color="red")+
          geom_vline(aes(xintercept=input$gpp_time_sum[2]), color="red")+
          labs(x="Datetime", y="Gross Primary Production", 
               title = "Gross Primary Production by Date and Time")+
          theme_bw()
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
      
      # Temperature
      output$temp <- renderPlot({
        do_us_ds() %>%
          ggplot(aes(x=datetime, y=avgtemp))+
          geom_line(color="purple")+
          geom_vline(aes(xintercept=input$temp_time_sum), color="red")+
          labs(x="Datetime", y="Temperature (c)", 
               title= "Average Temperature")+
          theme_bw()
      })
      
      # Temperature table output
      output$temp_stats <- renderTable(digits = 2,{
        do_us_ds() %>%  
          filter(as.POSIXct(do_us_ds()$datetime) %in% input$temp_time_sum) %>% 
          mutate("Temperature (C)" = avgtemp) %>% 
          select("Temperature (C)")
      })
      
      # do model plot output
      output$do_model <- renderPlot({
        do_us_ds() %>% 
          ggplot()+
          geom_line(aes(x=datetime, y=us_station_obs), color="darkgreen")+
          geom_line(aes(x=datetime, y=ds_station_obs), color="blue")+
          labs(x="Datetime", y="Dissolved Oxygen")+
          theme_classic()
      })
      
      ## Nitrate Tab code
      
      
    } # server functions close
  ) # shinyApp close
} # if iteractive close
