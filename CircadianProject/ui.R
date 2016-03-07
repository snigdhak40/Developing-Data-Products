# UI for Shiny web appication
 
library(BH)
library(dplyr)
library(DT)
library(shiny)
library(rCharts)
require(markdown)
require(data.table)
shinyUI(
  navbarPage("Circadian Rhythms", 
             # UI with slider panels and plots
             tabPanel("Circadian Data",
                      sidebarPanel(
                        sliderInput("CircadianPeriod", 
                                    "Circadian Period:", 
                                    min = 0,
                                    max = 72,
                                    value = c(24, 48)),
                        sliderInput("numberofOscillators", 
                                    "Number of Molecular Oscillators:",
                                    min = 1,
                                    max = 500,
                                    value = c(1, 100) 
                        ),
                        sliderInput("couplingStrength", 
                                    "Coupling Strength:",
                                    min = 0,
                                    max = 1,
                                    value = c(.3, .7) 
                        ),
                        uiOutput("couplingTypes"), # Typ1 1,2 or 3
                        actionButton(inputId = "resetCouplingTypes", 
                                     label = "Reset Selection", 
                                     icon = icon("square-o")),
                        actionButton(inputId = "checkAll", 
                                     label = "Check all", 
                                     icon = icon("check-square-o"))
                      ),
                      mainPanel(
                        tabsetPanel(
                          # Selected Circadian Data 
                          tabPanel(p(icon("table"), "Selected Circadian Data"),
                                   dataTableOutput(outputId="dTable")
                          ), # Selected Circadian Data
                          tabPanel(p(icon("line-chart"), "Plots of Circadian Data"),
                                   h4('Coupling Strength by Period Aggregate', align = "center"),
                                   h5('Hover to see Period and Coupling Strength.', 
                                      align ="center"),
                                   showOutput("couplingStrengthByPeriod", "nvd3"),
                                   h4('Coupling Type', align = "center"),
                                   h5('Hover to Coupling Type', 
                                      align ="center"),
                                   showOutput("couplingTypesByPeriod", "nvd3"),
                                   h4('Number of oscillators by Period', align = "center"),
                                   h5('Hover for Coupling Strength and Type', 
                                      align ="center"),
                                   showOutput("numberofOscillatorsByPeriod", "nvd3"),
                                   h4('Average Molecular Oscillators by Period', align = "center"),
                                   showOutput("numberofOscillatorsByPeriodAvg", "nvd3"),
                                   h4('Average Oscillators by Coupling Type', align = "center"),
                                  showOutput("numberofOscillatorsBycouplingTypeAvg", "nvd3")
                          ) # Plots of Circadian data
                        )
                      )     
             ), # Circadian Data
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             ) # end of "About" tab panel
             )  
  )
