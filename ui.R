
library(shiny)
library(shinythemes)
library(docstring)
library(tidyverse)
library(lubridate)
library(plotly)
library(rsconnect)



# Define UI for application 
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Application title for first tab panel
                tabsetPanel(
                  tabPanel("COVID-19 POOL TESTING",
                           
                           
                           # Adding text
                           p("The novel coronavirus (COVID-19) is a highly contagious respiratory disease and pandemic,
            first originating in the United States during January 2020. The name, COVID-19, results from the SARS-CoV-2
            virus. With the origin of COVID-19, efforts have been made for efficient testing measures. The patients
            who are suspected to have COVID-19 in their systems go through a real-time reverse transcriptase-polymerase chain reaction
            (Source: The JAMA Network). The aim of RT-PCR is to check for RNA detection of serious acute respiratory syndrome (SARS-CoV-2). 
            The request for such tests to check for this detection (the SARS-CoV-2 RT-PCR tests) has led to a decreased supply.
            Hence, researchers are examining pooled testing measures.",
                             style="text-align:justify;color:black;background-color:lightpink;padding:15px;border-radius:10px"),
                           
                           br(), br(),
                           
                           p(strong("Pool testing")," relies on tests from pooled samples of multiple patients. When looking at the results of the pooled tests,
          if there is a negative result, we can state that all patients in the pooled sample do not have coronavirus. However, if the pooled 
          test generates a positive result, every patients must be individually tested. This method is most effective when there are shortages 
          with the number of tests available. However, if there is a sensitivity under 100%, the risk of a false-negative for the pool is
          likely. Therefore, it is imperative to consider three factors to approximate the likelihood of false negatives: the prevalence of
          COVID-19, sensitivity of the test, and pool size of patient. ",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                           
                           br(), br(),
                           
                           p("The data used in this application is publicly available on Kaggle. These datasets are originally adapted from the New York Times and the
            COVID-19 Tracking Project.The introductory information about COVID-19 and pooled testing simulation is adapted from the JAMA Network.",
                             style="text-align:justify;color:black;background-color:lightpink;padding:15px;border-radius:10px"),
                           
                           br(), br(),
                           
                           p("For access to the original data set on",strong("Kaggle"),", click",
                             a(href=" https://www.kaggle.com/sudalairajkumar/covid19-in-usa?select=us_states_covid19_daily.csv", "here",target="_blank"),
                             style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:5px"),
                           
                           
                           br(),tags$br(),
                           "For more information on Simulation of Covid-19 Pooled Testing to identify patients with COVID-19, check the reference article on", 
                           tags$a(href="https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2767513?resultClick=3", 
                                  "JAMA Network Open."),
                           
                           br(),br(),hr(),
                           
                           p(em("Developed by"),br(strong("Abraham Liu, Rophence Ojiambo and Anusha Kumar")),style="text-align:center; font-family: times")
                  ),
                  
                  # Second tab panel          
                  tabPanel("SIMULATION ANALYSIS",
                           
                           
                           # Sidebar panel for inputs ----
                           sidebarPanel(
                             p("User to enter a specified value for group size",
                               style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                             # Input: numeric values for the group size ----
                             numericInput("k", "Group Size:", value = 10),
                             
                             
                             # Input: Sliders to input sensitivity and specificity
                             br(),
                             p("User to select a sensitivity value between 0 and 1",
                               style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                             sliderInput(inputId = "se",
                                         label = "Sensitivity:",
                                         min = 0,
                                         max = 1,
                                         value = 0.95),
                             br(),
                             
                             p("User to select a specificity value between 0 and 1",
                               style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                             sliderInput(inputId = "sp",
                                         label = "Specificity:",
                                         min = 0,
                                         max = 1,
                                         value = 0.99),
                             
                             
                             br(),
                             
                             #Radio Button Options
                             uiOutput("filter_data"),
                             
                             conditionalPanel(
                               "input.rd == 1",
                               # Input: text to indicate State
                               br(),
                               
                               p("User to input an abbreviated state name",
                                 style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                               
                               textInput(inputId = "State",
                                         label = "State Initial:",
                                         value = "CA"),
                               
                               # Input: select date in the format of YY-MM-DD
                               br(),
                               
                               p("User to input the Date",
                                 style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                               
                               dateInput("date", label = strong("Date input"), value = "2020-03-04"),
                             ),
                             
                             conditionalPanel(
                               "input.rd == 2",
                               # Input: numeric to indicate prevalence
                               br(),
                               
                               p("User to input Prevalence Rate",
                                 style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                               
                               sliderInput(inputId = "prev",
                                           label = "Prevalence Rate",
                                           min = 0,
                                           max = 1,
                                           value = 0.01),
                               
                               # Input: numeric to indicate population size
                               br(),
                               
                               p("User to input Population Size",
                                 style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                               
                               numericInput(inputId = "pop",
                                            label = "Population Size",
                                            value = 94)#the amount of patients used for the article
                             ),
                             
                             
                             # Action button: to specify when to calculate
                             actionButton("plot", strong("Plot!")),
                             actionButton("calc", strong("Calculate!"))
                           ),
                           
                           
                           # Adding a main panel with a title and plot output
                           mainPanel(
                             plotOutput("distPlot"),
                             plotOutput("kmean"),
                             textOutput("lowest_k")
                           )
                  ),
                  # Third panel with two navigation bars           
                  tabPanel("DATA VISUALIZATION",
                           
                           # Visualizing the positive positive cases
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Covid Map",
                                        plotlyOutput("state_positive_plot"),
                               ),
                               
                               # Displaying the data used
                               
                               tabPanel("Data",
                                        numericInput("maxrows", "Rows to show", 25),
                                        verbatimTextOutput("rawtable"),
                                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),
                                        tags$br(),
                                        tags$br(),
                                        "The COVID-19 Data in USA from", strong("Kaggle"), "is adapted from the ", tags$a(href="https://www.kaggle.com/sudalairajkumar/covid19-in-usa?select=us_states_covid19_daily.csv", 
                                                                                                                          "COVID-19 Tracking project and NY Times.")
                                        
                               ))
                           ))
                )
)