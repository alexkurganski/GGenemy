#' GGenemy
#'
#' Starts the Shiny App in which a dataset can be reviewed, several summary statistics can be
#' calculated and conditional plots can be drawn.
#'
#' @return A Shiny App.
#' 
#' @import shiny
#' @export
#'

GGenemy <- function() {
  ui <- navbarPage("GGenemy",
    theme = shinythemes::shinytheme("superhero"),

    # First Tab - Reading Data
    tabPanel(
      "1. Data Upload",
      sidebarLayout(
        sidebarPanel(
          # Input: Read CSV-Data
          fileInput("file1", "Choose a txt/csv File",
            accept = c(
              "txt/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          tags$style(".btn-file {background-color:#FF7F50; border-color: black;}"),


          # Input: Checkbox if file has header
          tags$b("Header"),

          checkboxInput("header",
            label = "Header",
            TRUE
          ),

          # Input: Select separator
          radioButtons("sep", "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ","
          ),

          # Input: Select quotes
          radioButtons("quote", "Which quotes are used?",
            choices = c(
              None = "",
              "Double Quotes" = '"',
              "Single Quotes" = "'"
            ),
            selected = '"'
          ),

          tags$b("Rownames"),

          checkboxInput("rownames",
            label = "Treat values of the first column as rownames",
            FALSE
          )
        ),

        mainPanel(
          tableOutput("contents"),

          tableOutput("na1"),

          tableOutput("na2")
        )
      )
    ),
    #############################################################################

    tabPanel(
      "2. Data Structure",

      sidebarLayout(
        sidebarPanel(

          # Input Number Quantiles
          checkboxGroupInput("as.factor",
            label = "",
            choices = NULL
          ),
          
          helpText("For factors with a vast amount of levels, only 
                          the 10 most common categories will be displayed.")
        ),

        mainPanel(
          verbatimTextOutput("summary1"),

          verbatimTextOutput("str1")
        )
      )
    ),

    #####################################################################################
    tabPanel(
      "3. Summary Stats",

      sidebarLayout(
        sidebarPanel(
          helpText("Factors will not be displayed."),
          
          radioButtons("given_var",
            label = "",
            choices = c("Dataset is missing")
          ),

          sliderInput("quantiles_sum_stats",
            "Number of quantiles for the given variable",
            min = 1,
            max = 10,
            value = 5
          ),

          radioButtons("n_sum_stats",
            label = "Choice of summary statistics",
            choices = list(
              "Conditional Mean" = 1, "Conditional Variance" = 2,
              "Conditional Skewness" = 3, "Conditional Kurtosis" = 4
            ),
            selected = 1
          ),
          
          tags$b("Plot"),
          
          checkboxInput("plotstats",
                        label = "Show summary statistics in a plot",
                        FALSE
          )              
        ),

        mainPanel(
          verbatimTextOutput("sum_stats1")
        )
      )
    ),
    #####################################################################################
    shiny::navbarMenu(
      "4. Plots",
      
      shiny::tabPanel(
      "Equal Quantiles",

      sidebarLayout(
        sidebarPanel(

          # Input Number Quantiles
          sliderInput("quantiles",
            "Number of quantiles for the given variable",
            min = 1,
            max = 10,
            value = 5
          ),
          
          # Help Text
          helpText("When conditioning on a factor variable,
                          the number of quantiles is set to the number of categories."),
          
          
          # Select given variable
          radioButtons("given_var2",
            label = "",
            choices = c("Dataset is missing")
          ),

          # Select Variables you want to plot
          checkboxGroupInput("var_to_plot",
            label = "",
            choices = NULL
          ),
          
          actionButton(
            inputId = "clicks",
            label = "Calculate!",
            icon("paper-plane"),
            style = "color: white; background-color: #FF7F50; border-color: black"
          ),
          
          downloadButton(
            "downloadPlot",
            label = "Download Plots",
            style = "color: white; background-color: #FF7F50; border-color: black"
          )
        ),

        # Show a plot of the generated distribution
        
        mainPanel(
          lapply(1:25, function(i) {
            plotOutput(paste0("condplot", i))
          })
        
        )
        
      )
    ),
    shiny::tabPanel(
      "Self Selected quantiles",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          
          shiny::radioButtons("var_name3",
                          label = "",
                          choices = c("Dataset is missing")
                          ),
      
          shiny::numericInput("firstquant1",
                         "First Quantile:",
                         value = 1
                         ),
          shiny::numericInput("firstquant2",
                              "First Quantile:",
                              value = 10
          ),
          
          shiny::numericInput("secondquant1",
                              "Second Quantile:",
                              value = NULL
          ),
          shiny::numericInput("secondquant2",
                              "Second Quantile:",
                              value = NULL
          ),
          
          shiny::numericInput("thirdquant1",
                              "Third Quantile:",
                              value = NULL
          ),
          shiny::numericInput("thirdquant2",
                              "Third Quantile:",
                              value = NULL
          ),
          
          shiny::checkboxGroupInput("var_to_cond_on2",
                                label = "",
                                choices = NULL
                                ),
          
          shiny::actionButton(
            inputId = "clicks2",
            label = "Calculate!",
            shiny::icon("paper-plane"),
            style = "color: white; background-color: #FF7F50; border-color: black"
          ),
          
          shiny::downloadButton(
            "downloadPlot2",
            label = "Download Plots",
            style = "color: white; background-color: #FF7F50; border-color: black"
          )
          
        ),
        shiny::mainPanel(
          lapply(1:25, function(i) {
            shiny::plotOutput(paste0("selfcondplot", i))
          })
        )
        )
      
      )
    )
  )
  #################################################################################
  #################################################################################
  # server
  server <- function(input, output, session) {
    data1 <- reactive({
      req(input$file1) # require that the input is available

      inFile <- input$file1

      df <- utils::read.csv(inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        row.names = if (input$rownames) {
          1
        } else {
          NULL
        }
      )

      updateCheckboxGroupInput(session,
        inputId = "as.factor",
        label = "Which variables are factors?",
        choices = names(df)
      )

      updateRadioButtons(session,
        inputId = "given_var",
        label = "Given variable",
        choices = names(df)
      )

      updateRadioButtons(session,
        inputId = "given_var2",
        label = "Given variable",
        choices = names(df)
      )
      
      shiny::updateRadioButtons(session,
        inputId = "var_name3",
        label = "Condition variable",
        choices = names(df)
      )

      updateCheckboxGroupInput(session,
        inputId = "var_to_plot",
        label = "Variables to plot",
        choices = names(df),
        selected = names(df)
      )
      
      shiny::updateCheckboxGroupInput(session,
        inputId = "var_to_cond_on2",
        label = "Variables to plot",
        choices = names(df),
        selected = names(df)
      )
      
      
      
      return(df)
    })

    data2 <- reactive({
      req(input$file1) # require that the input is available

      inFile <- input$file1
      df <- utils::read.csv(inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        row.names = if (input$rownames) {
          1
        } else {
          NULL
        }
      )

      for (i in unlist(input$as.factor, use.names = FALSE)) {
        df[,i] <- as.factor(df[,i])
      }
      df_reduced <- stats::na.omit(df)
      
      df_reduced2 <- df_reduced
      
      colnum <- which(sapply(df_reduced,is.factor))
      
      df_reduced2[colnum] <- NULL
      
      updateRadioButtons(session,
                                inputId = "given_var",
                                label = "Given variable",
                                choices = names(df_reduced2)
      )
      
      
      return(df_reduced)
    })
    
    data3 <- reactive({
      req(input$file1) # require that the input is available
      
      inFile <- input$file1
      df <- utils::read.csv(inFile$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote,
                            row.names = if (input$rownames) {
                              1
                            } else {
                              NULL
                            }
      )
      
      for (i in unlist(input$as.factor, use.names = FALSE)) {
        df[,i] <- as.factor(df[,i])
      }
      df_reduced <- stats::na.omit(df)
      
      colnum <- which(sapply(df_reduced,is.factor))
      
      df_reduced[colnum] <- NULL
      
      df_reduced2 <- df_reduced
      
      colnum <- which(sapply(df_reduced,is.factor))
      
      df_reduced2[colnum] <- NULL
      
      return(df_reduced)
    })

    
    ####################################################################
    # Data-Upload Tab

    output$contents <- renderTable({
      utils::head(data1())
    })

    output$na1 <- renderPrint({
      paste("The raw dataset consists of", nrow(data1()), "rows.")
    })

    output$na2 <- renderPrint({
      paste("After deleting NA's, the dataset has", nrow(data2()), "rows.")
    })

    ####################################################################
    # Data-Management Tab

    output$summary1 <- renderPrint({
      print.desc(describe(data2(), num.desc = c("min", "quantile0.25",
                                     "median", "mean", "quantile0.75",
                                     "max", "sd", "var", "valid.n"),
                        xname = "Dataset"))
    })
    

    output$str1 <- renderPrint({
      utils::str(data2())
    })

    ####################################################################
    # Sum-stats Tab

    output$sum_stats1 <- renderPrint({
      sum_stats(
        input$given_var,
        data3(),
        input$quantiles_sum_stats,
        input$n_sum_stats
      )
    })
    #####################################################################
    # Cond Plot Densities

    observeEvent(input$clicks, {
      lapply(1:25, function(i) {
        output[[paste0("condplot", i)]] <- NULL
      })
      len <- length(input$var_to_plot)
      lapply(1:len, function(i) {
        output[[paste0("condplot", i)]] <- renderPlot({
          plot_single_conditional_density(
            isolate(data2()),
            isolate(input$given_var2),
            isolate(input$quantiles),
            isolate(input$var_to_plot[i])
          )
        })
      })
    },
    ignoreInit = TRUE
    )
    
    shiny::observeEvent(input$clicks2, {
      lapply(1:25, function(i) {
        output[[paste0("selfcondplot", i)]] <- NULL
      })
      len <- length(input$var_to_cond_on)
      lapply(1:len, function(i) {
        output[[paste0("selfcondplot", i)]] <- shiny::renderPlot({
          plot_single_selected_quantile_density(
            shiny::isolate(data2()),
            shiny::isolate(input$var_name3),
            shiny::isolate(c(input$firstquant1,input$firstquant2)),
            shiny::isolate(c(input$secondquant1,input$secondquant2)),
            shiny::isolate(c(input$thirdquant1,input$thirdquant2)),
            shiny::isolate(input$var_to_cond_on2[i])
          )
        })
      })
    },
    ignoreInit = TRUE
    )
    
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {paste0("GGenemyPlot.pdf")},
    content = function(file) {
      grDevices::pdf(file, width = 11)
      gridExtra::marrangeGrob(
        print(plot_multiple_conditional_densities(
        isolate(data2()),
        isolate(input$given_var2),
        isolate(input$quantiles),
        isolate(input$var_to_plot)
        )),
        nrow = 1, ncol= 1)
      grDevices::dev.off()
    }
  )
  
  output$downloadPlot2 <- shiny::downloadHandler(
    filename = function() {paste0("GGenemyPlot.pdf")},
    content = function(file) {
      grDevices::pdf(file)
      gridExtra::marrangeGrob(
        print(plot_multiple_selected_quantile_densities(
          shiny::isolate(data2()),
          shiny::isolate(input$var_name3),
          shiny::isolate(c(input$firstquant1,input$firstquant2)),
          shiny::isolate(c(input$secondquant1,input$secondquant2)),
          shiny::isolate(c(input$thirdquant1,input$thirdquant2)),
          shiny::isolate(input$var_to_cond_on2))
        ),
        nrow = 1, ncol= 1)
      grDevices::dev.off()
    }
  )
  }
  # Run the application
  shinyApp(ui = ui, server = server)
}
