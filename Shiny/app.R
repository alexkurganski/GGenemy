source("conditional_densities.R")
source("../R/sum_stats.R")

# UI
ui <- shiny::navbarPage("GGenemy", theme = shinythemes::shinytheme("superhero"),

    #First Tab - Reading Data
      shiny::tabPanel("Data-Upload",
                      shiny::sidebarLayout(
                            shiny::sidebarPanel(
                                # Input: Read CSV-Data
                                shiny::fileInput("file1", "Choose txt/csv File",
                                                 accept = c(
                                                     "txt/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),

                                # Input: Checkbox if file has header
                                shiny::tags$b("Header"),
                                
                                shiny::checkboxInput("header",
                                                     label = "Header",
                                                     TRUE),

                                # Input: Select separator
                                shiny::radioButtons("sep", "Separator",
                                                    choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t"),
                                                    selected = ","),

                                # Input: Select quotes
                                shiny::radioButtons("quote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"'),
                                
                                shiny::tags$b("Rownames"),
                                
                                shiny::checkboxInput("rownames",
                                                 label = "First column as rownames",
                                                 FALSE)
                                
                                ),
                            
                            shiny::mainPanel(
                              shiny::tableOutput("contents"),
                              
                              shiny::tableOutput("na1"),
                              
                              shiny::tableOutput("na2")
                              
                              )
                            )
                      ),
#############################################################################

    shiny::tabPanel("Data-Structure",
                    
                    shiny::sidebarLayout(
                        shiny::sidebarPanel(
                            
                            # Input Number Quantiles
                            shiny::checkboxGroupInput("as.factor",
                                                      label = "",
                                                      choices = NULL)
                        ),
                        
        shiny::mainPanel(
            shiny::verbatimTextOutput("summary1"),
            
            shiny::verbatimTextOutput("str1")
            ),
        )
        ),

#####################################################################################    
    shiny::tabPanel("Summary_Stats",
                
                    shiny::sidebarLayout(
                        shiny::sidebarPanel(
                        
                            shiny::radioButtons("var_name",
                                                label = "",
                                                choices = c("Dataset is missing")),
                            
                            shiny::sliderInput("quantiles_sum_stats",
                                               "Number of Quantiles:",
                                               min = 1,
                                               max = 10,
                                               value = 5),
                            
                            shiny::radioButtons("n_sum_stats",
                                                label = "Choice of sum stats",
                                                choices = list("Conditional Mean" = 1, "Conditional Variance" = 2,
                                                               "Conditional Skewness" = 3, "Conditional Kurtosis" = 4),
                                                selected = 3)
                        ),
                    
                        shiny::mainPanel(
                            shiny::verbatimTextOutput("sum_stats1")
                        )
                    )
        ),
#####################################################################################    


    shiny::tabPanel("Plot",

                    shiny::sidebarLayout(
                        shiny::sidebarPanel(

                            # Input Number Quantiles
                            shiny::sliderInput("quantiles",
                                               "Number of Quantiles:",
                                               min = 1,
                                               max = 10,
                                               value = 5),
                            
                            # Help Text
                            shiny::helpText("When conditioning to a factor variable,
                                            the number of quantiles is set to the number of factors."),

                            # Select conditional variable
                            shiny::radioButtons("var_name2",
                                                label = "",
                                                choices = c("Dataset is missing")),

                            # Select Variables to condition on
                            shiny::checkboxGroupInput("var_to_cond_on",
                                                      label = "",
                                                      choices = NULL),
                            
                            
                            shiny::actionButton(inputId = "clicks",
                                                label = "Calculate!")
                            

                            ),
                            
                        # Show a plot of the generated distribution
                        shiny::mainPanel(
                            lapply(1:25, function(i) {
                                shiny::plotOutput(paste0('condplot', i))
                                })
                            )
                        )
    )
)
#################################################################################
#################################################################################
# server
server <- function(input, output, session){
  
  data1 <- shiny::reactive({
    
    shiny::req(input$file1) # require that the input is available
    
    inFile <- input$file1
    
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote,
                   row.names = if(input$rownames){1} else {NULL}
                   )
    
    shiny::updateCheckboxGroupInput(session,
                                    inputId = "as.factor",
                                    label = "Which Variable is a factor?",
                                    choices = names(df))
            
    shiny::updateRadioButtons(session,
                              inputId = "var_name",
                              label = "Condition variable",
                              choices = names(df))
            
    shiny::updateRadioButtons(session,  
                              inputId = "var_name2",
                              label = "Condition variable",
                              choices = names(df))
    
    shiny::updateCheckboxGroupInput(session,
                                    inputId = "var_to_cond_on",
                                    label = "Variables to condition on",
                                    choices = names(df))
    return(df)
    })
  
  data2 <- shiny::reactive({
    
    shiny::req(input$file1) # require that the input is available
    
    inFile <- input$file1
    df <- read.csv(inFile$datapath, 
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote,
                   row.names = if(input$rownames){1} else {NULL}
                   )
    
    for(i in unlist(input$as.factor,use.names = FALSE)){
      df[,i] <- as.factor(df[,i])
      }
    df_reduced <- na.omit(df)
    return(df_reduced)
    })
  
####################################################################  
# Data-Upload Tab        

  output$contents <- shiny::renderTable({
    head(data1())
    })
        
  output$na1 <- shiny::renderPrint({
    paste("The dataset contains", nrow(data1()))
    })
        
  output$na2 <- shiny::renderPrint({
    paste("With deleting NA's, the dataset has",nrow(data2()),"rows")
    })
  
####################################################################        
# Data-Management Tab

  output$summary1 <- shiny::renderPrint({
    summary(data2())
    })
    
  output$str1 <- shiny::renderPrint({
    str(data2())
    })
    
####################################################################
#Sum-stats Tab  
  
  output$sum_stats1 <- shiny::renderPrint({
    sum_stats(input$var_name,
              data2(),
              input$quantiles_sum_stats,
              input$n_sum_stats
              )
    })
#####################################################################
#Cond Plot Densities

  shiny::observeEvent(input$clicks, {
    len <- length(input$var_to_cond_on)
    lapply(1:len,function(i) {
      output[[paste0("condplot",i)]] <- shiny::renderPlot({
        plot_single_conditional_density(shiny::isolate(data2()),
                                        shiny::isolate(input$var_name2),
                                        shiny::isolate(input$quantiles),
                                        shiny::isolate(input$var_to_cond_on[i])
                                        )
        })
      })
    },
    ignoreInit = TRUE)
  }

# Run the application
shiny::shinyApp(ui = ui, server = server)

#########################################################################################
# to do:
#########################################################################################

# 7. What to do with NA's? #good looking solutions

# 8. Spacing relevant? Example: Dataset with many variables won't fit with head().

# 9. obtain Code function?

# 10. Which Data do we allow to read into our dataset? We have now csv and txt.
# RDs? raw?
# readr

# 12. Progress of calulation

# 13. Varselectinput() instead of checkboxgroupinput()

# 14. actionbutton in colour