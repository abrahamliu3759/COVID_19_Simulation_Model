library(shiny)
library(shinythemes)
library(docstring)
library(tidyverse)
library(lubridate)
library(plotly)
library(rsconnect)
# Define server logic 
server <- function(input, output) {
  
  #radio button options:
  output$filter_data<-renderUI({
    radioButtons("rd",h3("Select Type of Simulation Data:"),
                 choices = list("COVID-19 Dataset" = 1,
                                "Prevalence Simulation" = 2),selected = 1)
  })
  # Data preparation - will only run once
  
  covid <- read.csv("us_states_covid19_daily.csv")
  progress <- read.table("progresscheck.txt")
  
  # Transforming the date from number to date format
  df <- transform(covid, date = as.Date(as.character(date), "%Y%m%d"))
  
  
  #Since data is cumulative we only look at the last day data for the whole month
  covid_data<-df %>% 
    mutate(month=month(date)) %>%
    group_by(state) %>%
    arrange(date) %>%
    filter(month != lead(month))%>%
    select(c(date, state, positive, negative,death, positiveTestsViral,
             negativeTestsViral, positiveTestsAntibody, negativeTestsAntibody, positiveTestsAntigen))
  
  covid_data[is.na(covid_data)]<-0
  
  # Define functions used to generate data
  population <- function(df = df, state = state, date = date, prev = 0.01, pop = 10000) {
    #' Returns vector of 1's and 0's to simulate a population for COVID-19 Status
    #' @param df dataframe, default dataframe with unspecified dimensions
    #' @param state character, default length 1 character type
    #' @param date numeric, default length 1 numeric type
    #' @param prev numeric, default length 1 numeric type
    #' @param pop numeric, default length 1 numeric type
    #' @return the population vector with length of size population
    
    # if user decided to use the COVID-19 Dataset
    if (input$rd == 1) {
      positives <- df$positive[df$date == date & df$state == state] #grab number of positive cases
      negatives <- df$negative[df$date == date & df$state == state] #grab number of negative cases
    }
    #if user decided to simulate with using a prevalence parameter
    else {
      negatives <- rbinom(1, pop, 1-prev)
      positives <- pop - negatives
    }
    return(c(rep(1, negatives), rep(0, positives))) 
    #return vector with positive cases having values of 0, negatives with 1 
  }
  
  pooled_test <- function(sample=c(), se=0, sp=0, size = 2) {
    #' Returns test result status for a particular pooled group
    #' @param sample vector, default length size numeric type
    #' @param se numeric, default value 0 
    #' @param sp numeric, default value 0
    #' @param size numeric, default value 2
    #' @return value 1 or value size + 1 depending on sample and se + sp.
    
    pool <- prod(sample) #simulate pooling everyone into one sample
    
    if (pool == 1) { #if everyone within the test are truly negative
      test_prob <- runif(1,0,1)
      if (test_prob < sp) {return(1)} #true negative pooled result
      else {return(size + 1)} #false positive result
    }
    
    else {# if there exists at least one person that truly has COVID
      test_prob <- runif(1,0,1)
      if (test_prob < se) {return(size + 1)} #true positive pooled result
      else {return(1)} #false negative pooled result
    }
  }
  
  pooled_sample <- function(df=df,date=date, state=state, prev = 0.01, pop = 10000,
                            se=0,sp=0, size=2) {
    #' Returns the total number of tests needed in a random simulation of the pooled-sample test model
    #' @param df dataframe, default dataframe with unspecified dimensions
    #' @param date vector, default length 1 numeric type
    #' @param state vector, default length 1 character type
    #' @param prev numeric, default length 1 numeric type
    #' @param pop numeric, default length 1 numeric type
    #' @param se numeric, default value 0 
    #' @param sp numeric, default value 0
    #' @param size numeric, default value 2
    #' @return numeric value of at least 1
    
    # make our population vector
    pop <- population(df = df, state = state, date = date, prev = prev, pop = pop)
    #initialize our test counter
    total_tests <- 0
    
    for (i in 1:ceiling(length(pop)/size)) {#iterate for the amount of groups possible
      #gather indices for randomly selecting a group with "size" amount of people
      sample_indices <- sample(x = 1:length(pop), size = min(size,length(pop)), replace = FALSE)
      #run the pooled_test function with the newly created group, append the value to our counter
      total_tests <- total_tests + pooled_test(pop[sample_indices], se, sp, size)
      #update our population by removing the pooled samples using sample_indices
      pop <- pop[-sample_indices] 
    }
    return(total_tests)
  }
  
  binary_search <-function(df=df,date=date, state=state, prev=0.01, pop=10000,
                           se=0, sp=0) {
    #' Returns the estimated group size value that gives the lowest average tests conducted
    #' @param df dataframe, default dataframe with unspecified dimensions
    #' @param date vector, default length 1 numeric type
    #' @param state vector, default length 1 character type
    #' @param prev numeric, default length 1 numeric type
    #' @param pop numeric, default length 1 numeric type
    #' @param se numeric, default value 0 
    #' @param sp numeric, default value 0
    #' @return numeric value of at least 2
    
    #min group size possible is 2
    min_k <- 2
    #max group size bound the total population *3/4 rounded up
    #max group size possible would only have 2 possible values: 1 and population + 1
    #These 2 extremes would skew the average due to sensitivity and specificity parameters
    #Thus we pick a bound 3/4th of the total population for a better prediction
    if (input$rd == 1) {
      max_k <- ceiling(df$totalTestResults[df$date == date & df$state == state]*3/4)
    }
    else {
      max_k <- ceiling(pop*3/4)
    }
    
    #find empirical average of tests needed for max group size to compare with later
    test_results <- mean(sapply(1:100, function(x) {
      pooled_sample(df=df, date=date, state=state, prev = prev, pop = pop,
                    se=se, sp=sp, size = max_k)
    }))
    
    #while loop when there is more than 1 number in our search range
    while (max_k - min_k > 1) {
      #grab a number in the middle of the search range
      new_k <- ceiling((max_k+min_k)/2)
      #get an empirical average of tests under new k group 
      new_results <- mean(sapply(1:100, function(x) {
        pooled_sample(df=df, date=date, state=state, prev = prev, pop = pop,
                      se=se, sp=sp, size = new_k)
      }))
      
      #if this new average is greater than (or equal to) the test results
      if (new_results >= test_results) {
        #minimum is to the right. Change left bound for next iteration
        min_k <- new_k
      }
      #if new average is less than the test results
      else {
        #minimum is to the left. Change right bound for next iteration.
        max_k <- new_k
        #update test_results with new_results as the new value to be compared to
        test_results <- new_results
      }
    }
    return(ceiling((max_k+min_k)/2))#return median rounded up
  }
  
  frequencies <- eventReactive(input$plot, {
    #run a check to see if arguments are valid
    if (input$k < 2) {
      stop("Please enter a group size greater than 1")
    }
    if (input$k != round(input$k)) {
      stop("Please use an integer for group size (You can't have half of a person!")
    }
    if (input$rd == 1) {
      if (!(toupper(input$State) %in% unique(df$state))) {
        stop("Please enter the proper initials of a state.")
      }
    }
    if (input$rd == 2) {
      if (input$pop < 2) {
        stop("Please enter a proper population size")
      }
    }
    
    #generate data
    withProgress(message = 'Making plot', value = 0, {
      sapply(1:100, function(x) {
        incProgress(1/x, detail = paste(progress[x]))
        pooled_sample(df=df, date=input$date, state=toupper(input$State), 
                      prev = input$prev, pop = input$pop, se=input$se, 
                      sp=input$sp, size = input$k)
      })
    })
  })
  empirical <- eventReactive(input$calc, {
    #run a check to see if arguments are valid
    if (input$rd == 1) {
      if (!(toupper(input$State) %in% unique(df$state))) {
        stop("Please enter the proper initials of a state.")
      }
    }
    if (input$rd == 2) {
      if (input$pop < 2) {
        stop("Please enter a proper population size")
      }
    }
    withProgress(message = 'Making plot', value = 0, {
      sapply(1:100, function(x) {
        incProgress(1/x, detail = paste(progress[x]))
        binary_search(df=df, date=input$date, state=input$State, 
                      prev = input$prev, pop = input$pop, se=input$se, sp=input$sp)
      })
    })
    
    
    
  })
  
  prevalence <- eventReactive(input$rd, {
    #output statement 2 for optimal k value statement
    if (input$rd == 1) {#if we are on the COVID-19 dataset
      return("")#return an empty string
    }
    else {#if we are on the prevalence simulation selection
      return(paste("As for the article's simulation model (assuming population of 94),
                             the estimated lowest average tests is approximately", 1+(1/(sqrt(input$prev*input$se))), " "))
    }
  })
  # Defining the output density plot
  output$distPlot <- renderPlot({
    
    p <- ggplot() + 
      geom_bar(aes(frequencies())) +
      labs(x="Number of Tests Conducted", y = "Frequency", 
           title = paste("Frequency Plot of Tests Conducted with Groups of Size", 
                         input$k)) 
    return(p)
  })
  
  output$kmean <- renderPlot({
    p <- ggplot() + 
      geom_bar(aes(x=factor(empirical()))) +
      labs(x="Group Size Values", y = "Frequency", 
           title = "Frequency Plot Group Size Values with Estimated Lowest Average") 
    return(p)
  })
  
  output$lowest_k <- renderText({
    kvalues <- unique(empirical())
    averages <- sapply(1:length(kvalues), function(i) {
      mean(replicate(300, pooled_sample(df=df, date=input$date, state=toupper(input$State), 
                                        prev= input$prev, pop = input$pop, se=input$se, sp=input$sp, size = kvalues[i])))
    })
    statement_1 <- paste("The estimated k value(s) that gives the lowest average tests conducted is",
                         kvalues[which(averages == min(averages))], " ")
    statement_2 <- prevalence()
    return(paste(statement_1, paste(". ", statement_2, ""), ""))
  })
  
  
  
  # function to download data
  
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("Covid_Data", ".csv", sep="")
    },
    
    content = function(file) {
      
      covid_sub = covid_data
      names(covid_sub) = c("date", "state",  "positive_cases", "negative_cases", "deaths", "positive_viral_tests", "negative_viral_tests")
      write.csv(covid_sub, file)
    }
  )
  
  output$rawtable <- renderPrint({
    covid_sub = covid_data %>% select(c(date, state, positive, negative, death, positiveTestsViral,
                                        negativeTestsViral))
    
    names(covid_sub) = c("date", "state",  "positive_cases", "negative_cases", "deaths", "positive_viral_tests", "negative_viral_tests")
    covid_sub = data.frame(covid_sub)
    
    orig <- options(width = 1000)
    print(tail(covid_sub, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  # Extracting state names, positive cases, deaths, and dates names from covid data frame
  state_rep = covid_data$state
  positive_cases = covid_data$positive/100000
  deaths = covid_data$death
  date_cases = covid_data$date
  # create new data frame 
  states_df = data.frame(state_rep, date_cases,positive_cases, deaths)
  
  # Plot US Map
  state_positive_plot<-plot_ly(states_df, z= positive_cases,
                               locations=state_rep, 
                               text=paste0(states_df$state_rep,
                                           '<br>Date:',
                                           states_df$date_cases,
                                           '<br>Positive Cases:', 
                                           states_df$positive_cases, 
                                           '<br>Deaths:', 
                                           states_df$death), 
                               type="choropleth", 
                               locationmode="USA-states", 
                               colors = 'Reds')%>%
    layout(geo = list(scope="usa"), 
           title=list(text="Positive COVID-19 US Cases per 100,000 Population", 
                      y=0.96))
  
  
  output$state_positive_plot<- renderPlotly({
    state_positive_plot
    
  }) 
  
  
}
