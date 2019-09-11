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

          tags$hr(style = "border-color: #white;"),


          # Input: Select separator
          radioButtons("sep", "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ","
          ),

          tags$hr(style = "border-color: #white;"),

          # Input: Select quotes
          radioButtons("quote", "Which quotes are used?",
            choices = c(
              None = "",
              "Double Quotes" = '"',
              "Single Quotes" = "'"
            ),
            selected = '"'
          ),

          tags$hr(style = "border-color: #white;"),

          tags$b("Decimals"),

          checkboxInput("decimals",
            label = "File uses a comma as the decimal character",
            FALSE
          ),

          tags$hr(style = "border-color: #white;"),

          tags$b("Rownames"),

          checkboxInput("rownames",
            label = "Treat values of the first column as rownames",
            FALSE
          )
        ),

        mainPanel(
          tableOutput("contents") # ,

          # tableOutput("na1"),

          # tableOutput("na2")
        )
      )
    ),
    #############################################################################

    tabPanel(
      "2. Data Structure",

      sidebarLayout(
        sidebarPanel(


          # Input Number Quantiles
          selectInput("as.factor",
            label = "",
            choices = NULL,
            multiple = TRUE,
            selectize = TRUE
          ),

          tags$hr(style = "border-color: #white;"),

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
      "3. Summary Statistics",

      sidebarLayout(
        sidebarPanel(
          helpText("Factors will not be displayed."),

          tags$hr(style = "border-color: #white;"),

          selectInput("given_var4",
            label = "",
            choices = c("Dataset is missing"),
            selectize = TRUE
          ),

          tags$hr(style = "border-color: #white;"),

          sliderInput("quantiles_sum_stats",
            "Number of quantiles for the given variable",
            min = 1,
            max = 10,
            value = 5
          ),

          tags$hr(style = "border-color: #white;"),

          checkboxGroupInput("n_sum_stats",
            label = "Choice of summary statistics",
            c(
              "Conditional Mean" = 1,
              "Conditional Variance" = 2,
              "Conditional Skewness" = 3,
              "Conditional Kurtosis" = 4
            ), selected = c(1,2)
          ),

          tags$hr(style = "border-color: #white;"),

          actionButton(
            inputId = "clicks3",
            label = "Calculate!",
            icon("paper-plane"),
            style = "color: white; background-color: #FF7F50; border-color: black"
          ),

          downloadButton(
            "downloadPlot3",
            label = "Download Plots",
            style = "color: white; background-color: #FF7F50; border-color: black"
          )
        ),

        mainPanel(
          verbatimTextOutput("sum_stats1"),

          lapply(1:4, function(i) {
            plotOutput(paste0("summary_stats_plot", i))
          })
        )
      )
    ),
    #####################################################################################
    navbarMenu(
      "4. Plots",

      tabPanel(
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

            tags$hr(style = "border-color: #white;"),

            # Select given variable
            selectInput("given_var2",
              label = "",
              choices = "",
              selectize = TRUE
            ),

            tags$hr(style = "border-color: #white;"),

            # Select Variables you want to plot
            selectInput("var_to_plot",
              label = "",
              choices = NULL,
              multiple = TRUE,
              selectize = TRUE
            ),

            tags$hr(style = "border-color: #white;"),

            selectInput("boxplots",
              label = "",
              choices = NULL,
              multiple = TRUE,
              selectize = TRUE
            ),



            tags$hr(style = "border-color: #white;"),

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
      tabPanel(
        "Self Selected quantiles",
        sidebarLayout(
          sidebarPanel(
            helpText("Quantiles for factors cannot be chosen."),

            selectInput("var_name3",
              label = "",
              choices = c("Dataset is missing"),
              selectize = TRUE
            ),

            tags$hr(style = "border-color: #white;"),

            div(
              style = "display:inline-block; width: 300px;height: 25px;",
              tags$b("First Quantile")
            ),

            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput("firstquant1",
                "From:",
                value = 1 # ,
              )
            ),

            div(style = "display: inline-block;vertical-align:top; width: 10px;", HTML("<br>")),

            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput("firstquant2",
                "To:",
                value = 10 # ,
              )
            ),

            div(
              style = "display:inline-block; width: 300px;height: 25px;",
              tags$b("Second Quantile")
            ),

            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput("secondquant1",
                "From:",
                value = NULL
              )
            ),

            div(style = "display: inline-block;vertical-align:top; width: 10px;", HTML("<br>")),

            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput("secondquant2",
                "To:",
                value = NULL
              )
            ),

            div(
              style = "display:inline-block; width: 300px;height: 25px;",
              tags$b("Third Quantile")
            ),

            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput("thirdquant1",
                "From:",
                value = NULL
              )
            ),
            div(style = "display: inline-block;vertical-align:top; width: 10px;", HTML("<br>")),

            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              numericInput("thirdquant2",
                "To:",
                value = NULL
              )
            ),

            tags$hr(style = "border-color: #white;"),

            tags$b("For Factor Variables"),

            textInput("factorvariable",
              label = "Insert the categories of the given variable.",
              width = "310px"
            ),

            tags$hr(style = "border-color: #white;"),

            selectInput("var_to_cond_on2",
              label = "",
              choices = NULL,
              multiple = TRUE,
              selectize = TRUE
            ),

            tags$hr(style = "border-color: #white;"),

            selectInput("boxplots2",
              label = "",
              choices = NULL,
              multiple = TRUE,
              selectize = TRUE
            ),

            tags$hr(style = "border-color: #white;"),

            tags$b("Remaining data points as an additional quantile?"),

            checkboxInput("remaining",
              label = "Remaining",
              TRUE
            ),

            tags$hr(style = "border-color: #white;"),

            div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),

            actionButton(
              inputId = "clicks2",
              label = "Calculate!",
              icon("paper-plane"),
              style = "color: white; background-color: #FF7F50; border-color: black"
            ),

            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              downloadButton(
                "downloadPlot2",
                label = "Download Plots",
                style = "color: white; background-color: #FF7F50; border-color: black"
              )
            )
          ),
          mainPanel(
            lapply(1:25, function(i) {
              plotOutput(paste0("selfcondplot", i))
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

      df <- utils::read.table(inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        dec = if (input$decimals) {
          ","
        } else {
          "."
        },
        row.names = if (input$rownames) {
          1
        } else {
          NULL
        }
      )

      df_reduced <- df

      colnum <- which(sapply(df_reduced, is.factor))
      df_reduced[colnum] <- NULL

      updateSelectInput(session,
        inputId = "as.factor",
        label = "Choose which numerics or logicals should be treated as factors",
        choices = names(df_reduced)
      )

      updateSelectInput(session,
        inputId = "given_var2",
        label = "Given variable",
        choices = names(df)
      )

      updateSelectInput(session,
        inputId = "var_to_plot",
        label = "Variables to plot",
        choices = names(df),
        selected = names(df)
      )

      updateSelectInput(session,
        inputId = "var_to_cond_on2",
        label = "Variables to plot",
        choices = names(df),
        selected = names(df)
      )

      updateSelectInput(session,
        inputId = "var_name3",
        label = "Condition variable",
        choices = names(df)
      )


      return(df)
    })

    data2 <- reactive({
      req(input$file1) # require that the input is available

      inFile <- input$file1
      df <- utils::read.table(inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        dec = if (input$decimals) {
          ","
        } else {
          "."
        },
        row.names = if (input$rownames) {
          1
        } else {
          NULL
        }
      )

      for (i in unlist(input$as.factor, use.names = FALSE)) {
        df[, i] <- as.factor(df[, i])
      }
      df_reduced <- stats::na.omit(df)

      df_reduced2 <- df_reduced

      colnum <- which(sapply(df_reduced, is.factor))

      df_reduced2[colnum] <- NULL

      updateSelectInput(session,
        inputId = "given_var4",
        label = "Given variable",
        choices = names(df_reduced2)
      )

      updateSelectInput(session,
        inputId = "boxplots",
        label = "Boxplots instead of densities for numerical variables?",
        choices = names(df_reduced),
        selected = ""
      )

      updateSelectInput(session,
        inputId = "boxplots2",
        label = "Boxplots instead of densities for numerical variables?",
        choices = names(df_reduced),
        selected = ""
      )

      return(df_reduced)
    })

    data3 <- reactive({
      req(input$file1) # require that the input is available

      inFile <- input$file1
      df <- utils::read.table(inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        dec = if (input$decimals) {
          ","
        } else {
          "."
        },
        row.names = if (input$rownames) {
          1
        } else {
          NULL
        }
      )

      for (i in unlist(input$as.factor, use.names = FALSE)) {
        df[, i] <- as.factor(df[, i])
      }
      df_reduced <- stats::na.omit(df)

      colnum <- which(sapply(df_reduced, is.factor))

      df_reduced[colnum] <- NULL

      df_reduced2 <- df_reduced

      colnum <- which(sapply(df_reduced, is.factor))

      df_reduced2[colnum] <- NULL

      return(df_reduced)
    })

    # inputfactorvar <- reactive({
    #  varchanging <- input$var_name3
    # })

    #  inputlevel <- reactive({
    #    level_factor <- list()
    #    for(i in 1:length(input$as.factor)){
    #      level_factor[[i]] <- levels(data()[,input$as.factor[i]])
    #    }
    #  })

    #  observeEvent(inputfactorvar()){
    #    updateSelectInput(session,"factorvariable",choices = inputlevel())
    #  }


    ####################################################################
    # Data-Upload Tab

    output$contents <- renderTable({
      utils::head(data1(), 10)
    })

    # output$na1 <- renderPrint({
    # paste("The raw dataset consists of", nrow(data1()), "rows.")
    # })

    # output$na2 <- renderPrint({
    # paste("After deleting NA's, the dataset has", nrow(data2()), "rows.")
    # })

    ####################################################################
    # Data-Management Tab

    output$summary1 <- renderPrint({
      print.desc(describe(data2(),
        num.desc = c(
          "min", "quantile0.25",
          "median", "mean", "quantile0.75",
          "max", "sd", "var", "valid.n"
        ),
        xname = "Dataset"
      ))
    })

    output$str1 <- renderPrint({
      utils::str(data2())
    })

    ####################################################################
    # Sum-stats Tab

    observeEvent(input$clicks3, {
      output$sum_stats1 <- renderPrint({
        print_sum_stats(sum_stats(
          isolate(data3()),
          isolate(input$given_var4),
          isolate(input$n_sum_stats),
          isolate(input$quantiles_sum_stats)
        ), isolate(input$given_var4))
      })

      lapply(1:4, function(i) {
        output[[paste0("summary_stats_plot", i)]] <- NULL
      })
      lapply(1:length(input$n_sum_stats), function(i) {
        output[[paste0("summary_stats_plot", i)]] <- renderPlot({
          plot_sum_stats(
            isolate(data3()),
            isolate(input$given_var4),
            isolate(input$n_sum_stats[i]),
            isolate(input$quantiles_sum_stats) # ,
            # single = TRUE,
            # whichstat = isolate(i)
          )
        })
      })
    },
    ignoreInit = TRUE
    )

    output$downloadPlot3 <- downloadHandler(
      filename = function() {
        paste0("GGenemyPlot.pdf")
      },
      content = function(file) {
        grDevices::pdf(file, width = 11)
        gridExtra::marrangeGrob(
          print(plot_sum_stats(
            isolate(data3()),
            isolate(input$given_var4),
            isolate(input$n_sum_stats),
            isolate(input$quantiles_sum_stats)
          )),
          nrow = 1, ncol = 1
        )
        grDevices::dev.off()
      }
    )

    #####################################################################
    # Cond Plot Densities


    observeEvent(input$clicks, {
      lapply(1:25, function(i) {
        output[[paste0("condplot", i)]] <- NULL
      })
      len <- length(input$var_to_plot)
      lapply(1:len, function(i) {
        output[[paste0("condplot", i)]] <- renderPlot({
          plot_GGenemy(
            isolate(data2()),
            isolate(input$given_var2),
            isolate(input$var_to_plot[i]),
            isolate(input$quantiles),
            isolate(
              if (any(i == match(input$boxplots, input$var_to_plot))) {
                boxplot <- TRUE
              }
              else {
                boxplot <- FALSE
              }
            )
          )
        })
      })
    },
    ignoreInit = TRUE
    )

    observeEvent(input$clicks2, {
      lapply(1:25, function(i) {
        output[[paste0("selfcondplot", i)]] <- NULL
      })
      len <- length(input$var_to_cond_on2)
      lapply(1:len, function(i) {
        output[[paste0("selfcondplot", i)]] <- renderPlot({
          plot_GGenemy(
            isolate(data2()),
            isolate(input$var_name3),
            isolate(input$var_to_cond_on2[i]),
            selfquantiles = isolate(
              if (is.factor(data2()[, input$var_name3])) {
                strsplit(input$factorvariable, ",")[[1]]
              } else {
                c(
                  input$firstquant1, input$firstquant2,
                  input$secondquant1, input$secondquant2,
                  input$thirdquant1, input$thirdquant2
                )
              }
            ),
            remaining = isolate(input$remaining),
            boxplot = isolate(input$boxplots2)
          )
        })
      })
    },
    ignoreInit = TRUE
    )

    #####################################################################

    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0("GGenemyPlot.pdf")
      },
      content = function(file) {
        grDevices::pdf(file, width = 11)
        gridExtra::marrangeGrob(
          print(plot_GGenemy(
            isolate(data2()),
            isolate(input$given_var2),
            isolate(input$var_to_plot),
            isolate(input$quantiles),
            isolate(input$boxplots)
          )),
          nrow = 1, ncol = 1
        )
        grDevices::dev.off()
      }
    )

    output$downloadPlot2 <- downloadHandler(
      filename = function() {
        paste0("GGenemyPlot.pdf")
      },
      content = function(file) {
        grDevices::pdf(file)
        gridExtra::marrangeGrob(
          print(plot_GGenemy(
            isolate(data2()),
            isolate(input$var_name3),
            isolate(input$var_to_cond_on2),
            selfquantiles = isolate(
              if (is.factor(data2()[, input$var_name3])) {
                strsplit(unlist(input$factorvariable), ",")[[1]]
              } else {
                c(
                  input$firstquant1, input$firstquant2,
                  input$secondquant1, input$secondquant2,
                  input$thirdquant1, input$thirdquant2
                )
              }
            ),
            remaining = isolate(input$remaining),
            boxplot = isolate(input$boxplots2)
          )),
          nrow = 1, ncol = 1
        )
        grDevices::dev.off()
      }
    )
  }
  # Run the application
  shinyApp(ui = ui, server = server)
}
