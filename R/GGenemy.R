#' GGenemy
#'
#' Starts the Shiny App in which a dataset can be reviewed, several summary statistics can be
#' calculated and conditional plots can be drawn.
#'
#' @return A Shiny App.
#' @export
#'

GGenemy <- function() {
  ui <- shiny::navbarPage("GGenemy",
    theme = shinythemes::shinytheme("superhero"),

    # First Tab - Reading Data
    shiny::tabPanel(
      "1. Data Upload",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # Input: Read CSV-Data
          shiny::fileInput("file1", "Choose a txt/csv File",
            accept = c(
              "txt/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          shiny::tags$style(".btn-file {background-color:#FF7F50; border-color: black;}"),


          # Input: Checkbox if file has header
          shiny::tags$b("Header"),

          shiny::checkboxInput("header",
            label = "Header",
            TRUE
          ),

          # Input: Select separator
          shiny::radioButtons("sep", "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ","
          ),

          # Input: Select quotes
          shiny::radioButtons("quote", "Which quotes are used?",
            choices = c(
              None = "",
              "Double Quotes" = '"',
              "Single Quotes" = "'"
            ),
            selected = '"'
          ),

          shiny::tags$b("Rownames"),

          shiny::checkboxInput("rownames",
            label = "Treat values of the first column as rownames",
            FALSE
          )
        ),

        shiny::mainPanel(
          shiny::tableOutput("contents"),

          shiny::tableOutput("na1"),

          shiny::tableOutput("na2")
        )
      )
    ),
    #############################################################################

    shiny::tabPanel(
      "2. Data Structure",

      shiny::sidebarLayout(
        shiny::sidebarPanel(

          # Input Number Quantiles
          shiny::checkboxGroupInput("as.factor",
            label = "",
            choices = NULL
          ),
          
          shiny::helpText("For factors with a vast amount of levels, only 
                          the 10 most common categories will be displayed.")
        ),

        shiny::mainPanel(
          shiny::verbatimTextOutput("summary1"),

          shiny::verbatimTextOutput("str1")
        )
      )
    ),

    #####################################################################################
    shiny::tabPanel(
      "3. Summary Stats",

      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::helpText("Factors will not be displayed."),
          
          shiny::radioButtons("var_name",
            label = "",
            choices = c("Dataset is missing")
          ),

          shiny::sliderInput("quantiles_sum_stats",
            "Number of quantiles for the given variable",
            min = 1,
            max = 10,
            value = 5
          ),

          shiny::radioButtons("n_sum_stats",
            label = "Choice of summary statistics",
            choices = list(
              "Conditional Mean" = 1, "Conditional Variance" = 2,
              "Conditional Skewness" = 3, "Conditional Kurtosis" = 4
            ),
            selected = 4
          )
        ),

        shiny::mainPanel(
          shiny::verbatimTextOutput("sum_stats1")
        )
      )
    ),
    #####################################################################################


    shiny::tabPanel(
      "4. Plots",

      shiny::sidebarLayout(
        shiny::sidebarPanel(

          # Input Number Quantiles
          shiny::sliderInput("quantiles",
            "Number of quantiles for the given variable",
            min = 1,
            max = 10,
            value = 5
          ),
          
          # Help Text
          shiny::helpText("When conditioning on a factor variable,
                          the number of quantiles is set to the number of categories."),
          
          
          # Select given variable
          shiny::radioButtons("var_name2",
            label = "",
            choices = c("Dataset is missing")
          ),

          # Select Variables to condition on
          shiny::checkboxGroupInput("var_to_cond_on",
            label = "",
            choices = NULL
          ),
          
          shiny::actionButton(
            inputId = "clicks",
            label = "Calculate!",
            shiny::icon("paper-plane"),
            style = "color: white; background-color: #FF7F50; border-color: black"
          ),
          
          shiny::downloadButton(
            "downloadPlot",
            label = "Download Plots",
            style = "color: white; background-color: #FF7F50; border-color: black"
          )
        ),

        # Show a plot of the generated distribution
        
        shiny::mainPanel(
          lapply(1:25, function(i) {
            shiny::plotOutput(paste0("condplot", i))
          })
        
        )
        
      )
    )
  )
  #################################################################################
  #################################################################################
  # server
  server <- function(input, output, session) {
    data1 <- shiny::reactive({
      shiny::req(input$file1) # require that the input is available

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

      shiny::updateCheckboxGroupInput(session,
        inputId = "as.factor",
        label = "Which variables are factors?",
        choices = names(df)
      )

      shiny::updateRadioButtons(session,
        inputId = "var_name",
        label = "Given variable",
        choices = names(df)
      )

      shiny::updateRadioButtons(session,
        inputId = "var_name2",
        label = "Given variable",
        choices = names(df)
      )

      shiny::updateCheckboxGroupInput(session,
        inputId = "var_to_cond_on",
        label = "Variables to plot",
        choices = names(df),
        selected = names(df)
      )
      return(df)
    })

    data2 <- shiny::reactive({
      shiny::req(input$file1) # require that the input is available

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
      
      shiny::updateRadioButtons(session,
                                inputId = "var_name",
                                label = "Given variable",
                                choices = names(df_reduced2)
      )
      return(df_reduced)
    })
    
    data3 <- shiny::reactive({
      shiny::req(input$file1) # require that the input is available
      
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
      
      return(df_reduced)
    })

    
    ####################################################################
    # Data-Upload Tab

    output$contents <- shiny::renderTable({
      utils::head(data1())
    })

    output$na1 <- shiny::renderPrint({
      paste("The raw dataset consists of", nrow(data1()), "rows.")
    })

    output$na2 <- shiny::renderPrint({
      paste("After deleting NA's, the dataset has", nrow(data2()), "rows.")
    })

    ####################################################################
    # Data-Management Tab

    output$summary1 <- shiny::renderPrint({
      describe(data2(), num.desc = c("min", "quantile0.25",
                                     "median", "mean", "quantile0.75",
                                     "max", "sd", "var", "valid.n"),
                        xname = "Dataset")
    })
    

    output$str1 <- shiny::renderPrint({
      utils::str(data2())
    })

    ####################################################################
    # Sum-stats Tab

    output$sum_stats1 <- shiny::renderPrint({
      sum_stats(
        input$var_name,
        data3(),
        input$quantiles_sum_stats,
        input$n_sum_stats
      )
    })
    #####################################################################
    # Cond Plot Densities

    shiny::observeEvent(input$clicks, {
      lapply(1:25, function(i) {
        output[[paste0("condplot", i)]] <- NULL
      })
      len <- length(input$var_to_cond_on)
      lapply(1:len, function(i) {
        output[[paste0("condplot", i)]] <- shiny::renderPlot({
          plot_single_conditional_density(
            shiny::isolate(data2()),
            shiny::isolate(input$var_name2),
            shiny::isolate(input$quantiles),
            shiny::isolate(input$var_to_cond_on[i])
          )
        })
      })
    },
    ignoreInit = TRUE
    )
    
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {paste0("GGenemyPlot.pdf")},
    content = function(file) {
      grDevices::pdf(file, width = 9)
      gridExtra::marrangeGrob(
        print(plot_multiple_conditional_densities(
        shiny::isolate(data2()),
        shiny::isolate(input$var_name2),
        shiny::isolate(input$quantiles),
        shiny::isolate(input$var_to_cond_on)
        )),
        nrow = 1, ncol= 1)
      grDevices::dev.off()
    }
  )   
  }
  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
