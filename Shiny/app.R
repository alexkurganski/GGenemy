source("shiny/conditional_densities.R")
source("R/sum_stats.R")
library(shinythemes)
# UI
ui <- shiny::navbarPage("Conditional Densities", theme = shinythemes::shinytheme("flatly"),

    #First Tab - Reading Data
      shiny::tabPanel("Data",
                      shiny::sidebarLayout(
                            shiny::sidebarPanel(
                                # Input: Read CSV-Data
                                shiny::fileInput("file1", "Choose CSV File",
                                                 accept = c(
                                                     "text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),

                                # Input: Checkbox if file has header
                                shiny::checkboxInput("header", "Header", TRUE),



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
                                                    selected = '"')

                            
                                # Not used, because we need full data
                                # Input: Select number of rows to display ----
                                # shiny::radioButtons("disp", "Display",
                                #                      choices = c(Head = "head",
                                #                                  All = "all"),
                                #                      selected = "head")

                                ),
            shiny::mainPanel(

                # Output: Data
                shiny::tableOutput("contents")
                )
            )
        ),
####################################################################################
    ##### CHOOSING FACTOR DOES NOT WORK PROPERLY ###########
###########################################################

    shiny::tabPanel("Summary/Structure",
                    
                    shiny::sidebarLayout(
                        shiny::sidebarPanel(
                            
                            # Input Number Quantiles
                            shiny::checkboxGroupInput("as.factor",
                                                      label = NULL,
                                                      choices = NULL),
                            
                            shiny::actionButton(inputId = "clicks2",
                                                label = "Calculate!")
                            

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
                                                label = NULL,
                                                choices = c("Dataset is missing")),
                            
                            shiny::sliderInput("quantiles_sum_stats",
                                               "Number of Quantiles:",
                                               min = 1,
                                               max = 10,
                                               value = 5),
                            
                            shiny::radioButtons("n_sum_stats",
                                                label = "Number of sum stats",
                                                choices = list("1. Derivative" = 1, "2. Derivative" = 2,
                                                               "3. Derivative" = 3, "4. Derivative" = 4),
                                                selected = 3)
                        ),
                    
                        shiny::mainPanel(
                            shiny::verbatimTextOutput("sum_stats1")
                        ),
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

                            # Select conditional variable
                            shiny::radioButtons("var_name2",
                                                label = NULL,
                                                choices = c("Dataset is missing")),

                            # Select Variables to condition on
                            shiny::checkboxGroupInput("var_to_cond_on",
                                                      label = NULL,
                                                      choices = NULL),
                            
                            
                            shiny::actionButton(inputId = "clicks",
                                                label = "Calculate!")
                            

                            ),
                            
                        # Show a plot of the generated distribution
                        shiny::mainPanel(
                            lapply(1:20, function(i) {
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
    
    varnames <- reactiveValues()

        data1 <- shiny::reactive({
            shiny::req(input$file1) # require that the input is available
        
            isolate(inFile <- input$file1)
            
            isolate(df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                           quote = input$quote))
            
            #eventReactive(input$clicks2, {
            #varnames$names <- names(df)})
            
            shiny::updateCheckboxGroupInput(session,
                                            inputId = "as.factor",
                                            label = "factor variable",
                                            choices = names(df))
            
            shiny::updateRadioButtons(session,
                                      inputId = "var_name",
                                      label = "condition variable",
                                      choices = names(df))
            
            shiny::updateRadioButtons(session,
                                      inputId = "var_name2",
                                      label = "condition variable",
                                      choices = names(df))
    
            shiny::updateCheckboxGroupInput(session,
                                     inputId = "var_to_cond_on",
                                     label = "Variables to condition on",
                                     choices = names(df))
            
            #df[,input$as.factor] <- as.factor(df[,input$as.factor])
            
          return(df)
        })

        data2 <- shiny::reactive({
            shiny::req(input$file1) # require that the input is available
            
            isolate(inFile <- input$file1)
            
            isolate(df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                                   quote = input$quote))
            
            for(i in unlist(input$as.factor,use.names = FALSE)){
            df[,i] <- as.factor(df[,i])
            }
            
            return(df)
        })
     #   observe({
     #       shiny::req(input$file1)
     #   shiny::updateCheckboxGroupInput(session,
     #                                   inputId = "as.factor",
     #                                   label = "factor variable",
     #                                   choices = names(data()$results))
     #   })
        
        #observeEvent(input$clicks2,
        #    data1() <- as.factor(data1$"V6")
        #)
        
        output$contents <- shiny::renderTable({
            head(data1())
        })
        output$summary1 <- shiny::renderPrint({
            summary(data2())
        })
    
        output$str1 <- shiny::renderPrint({
            str(data2())
        })
    
        output$sum_stats1 <- shiny::renderPrint({
            sum_stats(input$var_name,
                      data2(),
                      input$quantiles_sum_stats,
                      input$n_sum_stats
                      )
        })
    
        shiny::observeEvent(input$clicks, {
            len <- length(as.numeric(input$var_to_cond_on))
            lapply(1:len,
                   function(i) {
                       output[[paste0("condplot",i)]] <- shiny::renderPlot({
                           plot_single_conditional_density(data2(),
                                                           input$var_name2,
                                                           input$quantiles,
                                                           isolate(input$var_to_cond_on[i])
                                                           )
                    })
                })
        },
        ignoreInit = TRUE)

}

# Run the application
shiny::shinyApp(ui = ui, server = server)
# to do:
# 1.!!!!!!
# Fix categorical variables - can not fix it - Help Tabea:
# Eather I try it in the df part - reloades Input without values
# Or outside of it but it does not affect the dataset
# FIXED THIS! 
# I have already solved it, but I have to leave it as motivation
# so that I know that I can get everything solved. *Sonnenbrillensmiley*

# (2.)
# multiple Datasets? Is it necessary? One Dataset loaded should be enough or?
# Would make many things way more complicated for a low value.

# 3.
# fixed length number of plots. Is fixed at 20 now, can not be chosen with an input$
# I do not have an Idea how to fix this. Is it a big problem?

# 4.
# The 4th argument of sum stats only works properly.
# The function has big problems with factorial variables. Did not look into it,
# why it did so much problems

# 5.
# Better looking outputs for summary/str and sum stats
# Furthermore choose theme

# 6. Stop warnings