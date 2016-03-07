library(shiny)
# Helpers file
source("helpers.R")
couplingTypes <- sort(unique(circData$couplingType))
# Shiny server
shinyServer(
  function(input, output) {
    output$setid <- renderText({input$setid})
#     output$address <- renderText({
#       input$goButtonAdd
#       isolate(paste("http://brickset.com/sets/", 
#                     input$setid, sep=""))
#       
#     })
    #     getPage<-function(url) {
    #         return(tags$iframe(src = url, 
    #                            style="width:100%;",  
    #                            frameborder="0", id="iframe", 
    #                            height = "500px"))
    #     }
#     openPage <- function(url) {
#       return(tags$a(href=url, "Click here!", target="_blank"))
#     }
#     output$inc <- renderUI({ 
#       input$goButtonDirect
#       isolate(openPage(paste("http://brickset.com/sets/", 
#                              input$setid, sep="")))
#       ## Can't open iframe below 
#       # Got This request has been blocked; 
#       # the content must be served over HTTPS error msg
#       # Mixed Content: The page at 'https://xiaodan.shinyapps.io/LegoDatasetVisualization/' 
#       # was loaded over HTTPS, but requested an insecure resource 'http://brickset.com/sets/'. 
#       # This request has been blocked; the content must be served over HTTPS.
#       #isolate(getPage(paste("//brickset.com/sets/", 
#       #                       input$setid, sep="")))  
#     })
    # Reactive functions
    values <- reactiveValues()
    values$couplingTypes <- couplingTypes
    # Create event type checkbox
    output$couplingTypes <- renderUI({
      checkboxGroupInput('couplingTypes', 'Brain Region couplingTypes:', 
                         couplingTypes, selected = values$couplingTypes)
    })
 
55
    # Clear-all button
    observe({
      if(input$resetCouplingTypes == 0) return()
      values$couplingTypes <- c()
    })
    # Select-all button
    observe({
      if(input$checkAll == 0) return()
      values$couplingTypes <- couplingTypes
    })

    # Circadian data
    dataTable <- reactive({
      groupBycouplingType(circData, input$CircadianPeriod[1], 
                   input$CircadianPeriod[2], input$numberofOscillators[1],
                   input$numberofOscillators[2], input$couplingStrength[1], 
                   input$couplingStrength[2], input$couplingTypes)
    })
    
    dataTableBycouplingStrengthPeriod <- reactive({
      groupByperiodcouplingStrength(circData, input$CircadianPeriod[1], 
                     input$CircadianPeriod[2], input$numberofOscillators[1],
                     input$numberofOscillators[2], input$couplingStrength[1], 
                     input$couplingStrength[2], input$couplingTypes)
    })

    dataTableByPeriod <- reactive({
      groupByPeriodAgg(circData, input$CircadianPeriod[1], 
                     input$CircadianPeriod[2], input$numberofOscillators[1],
                     input$numberofOscillators[2], input$couplingStrength[1], 
                     input$couplingStrength[2], input$couplingTypes)
    })

    dataTableByOscillators <- reactive({
      groupByPeriodAll(circData, input$CircadianPeriod[1], 
                     input$CircadianPeriod[2], input$numberofOscillators[1],
                     input$numberofOscillators[2], input$couplingStrength[1], 
                     input$couplingStrength[2], input$couplingTypes)
    })

    dataTableByOscillatorsAvg <- reactive({
      groupBynumberofOscillatorsAvg(circData, input$CircadianPeriod[1], 
                      input$CircadianPeriod[2], input$numberofOscillators[1],
                      input$numberofOscillators[2], input$couplingStrength[1], 
                      input$couplingStrength[2], input$couplingTypes)
    })

    dataTableByOscillatorscouplingTypeAvg <- reactive({
      groupBynumberofOscillatorscouplingTypeAvg(circData, input$CircadianPeriod[1], 
                           input$CircadianPeriod[2], input$numberofOscillators[1],
                           input$numberofOscillators[2], input$couplingStrength[1], 
                           input$couplingStrength[2], input$couplingTypes)
    })

    # Render data 
    output$dTable <- renderDataTable({
      dataTable()
    } #, options = list(bFilter = FALSE, iDisplayLength = 50)
    )

    output$couplingStrengthByPeriod <- renderChart({
      plotcouplingStrengthCountByperiod(dataTableBycouplingStrengthPeriod())
    })

    output$couplingTypesByPeriod <- renderChart({
      plotcouplingTypesCountByPeriod(dataTableByPeriod())
    })
    
    output$numberofOscillatorsByPeriod <- renderChart({
      plotnumberofOscillatorsByPeriod(dataTableByOscillators())
    })

    output$numberofOscillatorsByPeriodAvg <- renderChart({
      plotnumberofOscillatorsByPeriodAvg(dataTableByOscillatorsAvg())
    })
    
    output$numberofOscillatorsBycouplingTypeAvg <- renderChart({
      plotnumberofOscillatorsBycouplingTypeAvg(dataTableByOscillatorscouplingTypeAvg())
    })
  } # end of function(input, output)
)
