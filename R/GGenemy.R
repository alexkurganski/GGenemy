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
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script = "inst/hide_shiny_tabs.js"),
    navbarPage("GGenemy",
      id = "GGenemy",
      theme = shinythemes::shinytheme("superhero"),

      #1. ######################################################################
      # First Tab - Reading Data
      tabPanel(
        "1. Data Upload",
        sidebarLayout(
          sidebarPanel(
            
            checkboxInput("checktrue", "Upload own file",TRUE),
            
            selectInput("datframe", label = "Select an already uploaded DataFrame from you Global Environment",
                        choices = c("",search_dataframe()),
                        selectize = TRUE),
                        
            
            # Input: Read CSV/TXT-Data
            fileInput("file1", "Or choose a txt/csv File from your PC",
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
              selected = ",",
              inline = TRUE
            ),

            tags$hr(style = "border-color: #white;"),

            # Input: Select quotes
            radioButtons("quote", "Which quotes are used?",
              choices = c(
                None = "",
                "Double Quotes" = '"',
                "Single Quotes" = "'"
              ),
              selected = '"',
              inline = TRUE
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
            tableOutput("contents")
            
            
          )
        )
      ),
      #2. ######################################################################

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
                          the 10 most common categories will be displayed."),
            
            tags$style(HTML("caption {color: #FF7F50;}"))
          ),

          mainPanel(
            #verbatimTextOutput("summary1"),
            tableOutput("summary1"),
            lapply(1:100, function(i) {
            tableOutput(paste0("summary2", i))
            }),

            verbatimTextOutput("str1")
          )
        )
      ),

      #3. ######################################################################
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
              ), selected = c(1, 2)
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
          lapply(1:4, function(i) {
            tableOutput(paste0("sum_stats", i))
          }),
          
          lapply(1:4, function(i) {
            plotOutput(paste0("summary_stats_plot", i))
          })
          )
        )
      ),
      #4. ###################################################################
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
          "Self Selected Range",
          sidebarLayout(
            sidebarPanel(
              selectInput("var_name3",
                label = "",
                choices = c("Dataset is missing"),
                selectize = TRUE
              ),

              tags$hr(style = "border-color: #white;"),


              div(
                style = "display: inline-block;vertical-align:top;",
                numericInput("firstrange1",
                  "From:",
                  value = 1,
                  width = "150px"
                )
              ),

              div(style = "display: inline-block;vertical-align:top; width: 10px;", HTML("<br>")),

              div(
                style = "display: inline-block;vertical-align:top;",
                numericInput("firstrange2",
                  "To:",
                  value = 10,
                  width = "150px"
                )
              ),

              div(
                style = "display: inline-block;vertical-align:top;",
                numericInput("secondrange1",
                  "From:",
                  value = NULL,
                  width = "150px"
                )
              ),

              div(style = "display: inline-block;vertical-align:top; width: 10px;", HTML("<br>")),

              div(
                style = "display: inline-block;vertical-align:top;",
                numericInput("secondrange2",
                  "To:",
                  value = NULL,
                  width = "150px"
                )
              ),

              div(
                style = "display: inline-block;vertical-align:top;",
                numericInput("thirdrange1",
                  "From:",
                  value = NULL,
                  width = "150px"
                )
              ),

              div(style = "display: inline-block;vertical-align:top; width: 10px;", HTML("<br>")),

              div(
                style = "display: inline-block;vertical-align:top;",
                numericInput("thirdrange2",
                  "To:",
                  value = NULL,
                  width = "150px"
                )
              ),

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

              div(style = "display: inline-block;vertical-align:top; width: 10px;", HTML("<br>")),

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
              ),

              tags$hr(style = "border-color: #white;"),


              actionButton("pastecode",
                icon = icon("code"),
                label = "Obtain Code!", style = "color:white;
                                  background-color:#FF7F50; border-color: black"
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
  )
  
  # server #####################################################################
  server <- function(input, output, session) {
  
    
      data3 <- reactive({
      if (input$datframe != "" & !is.null(input$datframe)){
        df <- get(input$datframe, envir = .GlobalEnv)
      } else {
        df <- NULL
      }
      
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
      return(df)
    })
    
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
      return(df)
      })

    
    data2 <- reactive({
    if(input$checktrue == FALSE){
      req(input$datframe)
        
        df <- data3()
        
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
        
        #    updateSelectInput(session,
        #      inputId = "boxplots",
        #      label = "Boxplots instead of densities for numerical variables?",
        #      choices = names(df_reduced),
        #      selected = ""
        #    )
        
        updateSelectInput(session,
                          inputId = "boxplots2",
                          label = "Boxplots instead of densities for numerical variables?",
                          choices = names(df_reduced),
                          selected = ""
        )
        
        return(df_reduced)
    } else {
        req(input$file1)
        
        df <- data1()
        
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
        
        #    updateSelectInput(session,
        #      inputId = "boxplots",
        #      label = "Boxplots instead of densities for numerical variables?",
        #      choices = names(df_reduced),
        #      selected = ""
        #    )
        
        updateSelectInput(session,
                          inputId = "boxplots2",
                          label = "Boxplots instead of densities for numerical variables?",
                          choices = names(df_reduced),
                          selected = ""
        )
        
        return(df_reduced)
    }
        })

    # updateboxplots ################################################################

    updateboxplot <- function(session) {
      updateSelectInput(session,
        "boxplots",
        "Boxplots instead of densities for numerical variables?",
        choices = input$var_to_plot,
        selected = ""
      )
    }
    observeEvent(input$var_to_plot, updateboxplot(session))

    updateboxplot2 <- function(session) {
      updateSelectInput(session,
        "boxplots2",
        "Boxplots instead of densities for numerical variables?",
        choices = input$var_to_cond_on2,
        selected = ""
      )
    }
    observeEvent(input$var_to_cond_on2, updateboxplot2(session))

    # Data-Upload Tab #################################################
    
    
    output$contents <- renderTable({
      utils::head(NULL)
    })
    output$contents <- renderTable({
      utils::head(data2(), 10)
    })

    # Data-Management Tab ######################################################

    observeEvent(data2(), {
    
    output$summary1 <- renderTable({
      describe(data2(),
               num.desc = c(
                 "min", "quantile0.25",
                 "median", "mean", "quantile0.75",
                 "max", "sd", "var", "valid.n"
               ),
               xname = "Dataset"
      )[[1]] 
    }, caption = 
      names(describe(data2(),
                     num.desc = c(
                       "min", "quantile0.25",
                       "median", "mean", "quantile0.75",
                       "max", "sd", "var", "valid.n"
                     ),
                     xname = "Dataset"
      ))[1],
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
    )
    
    
    if(!is.null(describe(data2(),
                         num.desc = c(
                           "min", "quantile0.25",
                           "median", "mean", "quantile0.75",
                           "max", "sd", "var", "valid.n"
                         ),
                         xname = "Dataset"
    )[[2]])){
    
    lapply(1:length(describe(data2(),
                             num.desc = c(
                               "min", "quantile0.25",
                               "median", "mean", "quantile0.75",
                               "max", "sd", "var", "valid.n"
                             ),
                             xname = "Dataset"
    )[[2]]), function(i){
      output[[paste0("summary2", i)]] <- renderTable({
        describe(data2(),
                 num.desc = c(
                   "min", "quantile0.25",
                   "median", "mean", "quantile0.75",
                   "max", "sd", "var", "valid.n"
                 ),
                 xname = "Dataset"
        )[[2]][[i]]
      }, caption = 
        names(describe(data2(),
                       num.desc = c(
                         "min", "quantile0.25",
                         "median", "mean", "quantile0.75",
                         "max", "sd", "var", "valid.n"
                       ),
                       xname = "Dataset"
        ))[2],
      caption.placement = getOption("xtable.caption.placement", "top"),
      rownames = TRUE
      )
    })
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = FALSE)
    
    #output$summary1 <- renderPrint({
      #print.desc(describe(data2(),
        #num.desc = c(
          #"min", "quantile0.25",
          #"median", "mean", "quantile0.75",
          #"max", "sd", "var", "valid.n"
        #),
        #xname = "Dataset"
      #))
    #})

    output$str1 <- renderPrint({
      utils::str(data2())
    })

    # Sum-stats Tab ############################################################

    observeEvent(input$clicks3, {
      lapply(1:4, function(i) {
        output[[paste0("sum_stats", i)]] <- NULL
      })
      lapply(1:length(input$n_sum_stats), function(i){
        output[[paste0("sum_stats", i)]] <- renderTable({
          sum_stats(
            isolate(data2()),
            isolate(input$given_var4),
            isolate(input$n_sum_stats),
            isolate(input$quantiles_sum_stats)
          )[[i]]
        }, rownames = TRUE,
           caption = names(sum_stats(
             isolate(data2()),
             isolate(input$given_var4),
             isolate(input$n_sum_stats),
             isolate(input$quantiles_sum_stats)
           ))[i],
           caption.placement = getOption("xtable.caption.placement", "top"),
           #caption.width = getOption("xtable.caption.width", NULL),
           digits = 3
        )
      })



      lapply(1:4, function(i) {
        output[[paste0("summary_stats_plot", i)]] <- NULL
      })
      lapply(1:length(input$n_sum_stats), function(i) {
        output[[paste0("summary_stats_plot", i)]] <- renderPlot({
          plot_sum_stats(
            isolate(data2()),
            isolate(input$given_var4),
            isolate(input$n_sum_stats[i]),
            isolate(input$quantiles_sum_stats)
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
            isolate(data2()),
            isolate(input$given_var4),
            isolate(input$n_sum_stats),
            isolate(input$quantiles_sum_stats)
          )),
          nrow = 1, ncol = 1
        )
        grDevices::dev.off()
      }
    )

    # Cond Plot Densities #############################################

    observeEvent(input$clicks, {
      lapply(1:25, function(i) {
        output[[paste0("condplot", i)]] <- NULL
      })
      len <- length(input$var_to_plot)
      withProgress(message = "Making plot", value = 0, {
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
          incProgress(1 / len, detail = paste("Doing part", i))
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
      withProgress(message = "Making plot", value = 0, {
        lapply(1:len, function(i) {
          output[[paste0("selfcondplot", i)]] <- renderPlot({
            plot_GGenemy(
              isolate(data2()),
              isolate(input$var_name3),
              isolate(input$var_to_cond_on2[i]),
              selfrange = isolate(
                if (is.factor(data2()[, input$var_name3])) {
                  strsplit(input$factorvariable, ",")[[1]]
                } else {
                  c(
                    input$firstrange1, input$firstrange2,
                    input$secondrange1, input$secondrange2,
                    input$thirdrange1, input$thirdrange2
                  )
                }
              ),
              remaining = isolate(input$remaining),
              boxplot = isolate(input$boxplots2)
            )
          })
          incProgress(1 / len, detail = paste("Doing part", i))
        })
      })
    },
    ignoreInit = TRUE
    )

    # Downloads ###############################################################

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
            selfrange = isolate(
              if (is.factor(data2()[, input$var_name3])) {
                strsplit(unlist(input$factorvariable), ",")[[1]]
              } else {
                c(
                  input$firstrange1, input$firstrange2,
                  input$secondrange1, input$secondrange2,
                  input$thirdrange1, input$thirdrange2
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
    # Obtain code ##############################################################
    
    observeEvent(input$pastecode, {
      req(input$file1)
      if (input$rownames) {
        row <- 1
      } else {
        row <- paste("NULL")
      }
      if (input$quote == c("")) {
        read_data <- paste0(
          'data <- read.table("', input$file1, '",', "header = ",
          input$header, ",", 'sep = "', input$sep, '",', 'quote = "",',
          'dec = "', input$dec, '",', "row.names = ",
          row,
          ")"
        )
      } else {
        read_data <- paste0(
          'data <- read.table("', input$file1, '",', "header = ",
          input$header, ",", 'sep = "', input$sep, '",', 'quote = "\\',
          input$quote, '",', 'dec = "', input$dec, '",', "row.names = ",
          row,
          ")"
        )
      }

      if (is.factor(data2()[, input$var_name3])) {
        selfrange <- input$factorvariable
      } else {
        selfrange <- c(
          input$firstrange1, input$firstrange2,
          input$secondrange1, input$secondrange2,
          input$thirdrange1, input$thirdrange2
        )
      }

      if (is.null(input$boxplot)) {
        self <- paste0(
          'plot_GGenemy(read_data,"', input$var_name3, '","',
          input$var_to_cond_on, '","', input$var_to_cond_on, '",',
          selfrange, ',"', input$remaining, '")'
        )
      } else {
        self <- paste0(
          'plot_GGenemy(read_data,"', input$var_name3, '","', input$var_to_cond_on2, '",',
          selfrange, ',"', input$remaining, '",', input$boxplot, ")"
        )
      }

      code <- paste0(
        "Read data:", "\n",
        read_data,
        "\n Self selected Range:",
        "\n",
        self
      )

      showModal(modalDialog(
        title = "Obtain your R code",
        tags$pre(tags$code(code)),
        easyClose = TRUE
      ))
    })

    # hide elements ############################################################

     observeEvent(input$file1, {
       shinyjs::show(selector = '#GGenemy li a[data-value="2. Data Structure"]')
       shinyjs::show(selector = '#GGenemy li a[data-value="3. Summary Statistics"]')
       shinyjs::show(selector = '#GGenemy li a[data-value="4. Plots"]')
     })
    
    observeEvent(input$datframe, {
      shinyjs::show(selector = '#GGenemy li a[data-value="2. Data Structure"]')
      shinyjs::show(selector = '#GGenemy li a[data-value="3. Summary Statistics"]')
      shinyjs::show(selector = '#GGenemy li a[data-value="4. Plots"]')
    },
    ignoreInit = TRUE)
    
     
    observeEvent(input$var_name3, {
      if (is.factor(data2()[, input$var_name3])) {
        shinyjs::hide(id = "firstrange1")
        shinyjs::hide(id = "firstrange2")
        shinyjs::hide(id = "secondrange1")
        shinyjs::hide(id = "secondrange2")
        shinyjs::hide(id = "thirdrange1")
        shinyjs::hide(id = "thirdrange2")

        shinyjs::show(id = "factorvariable")
      } else {
        shinyjs::show(id = "firstrange1")
        shinyjs::show(id = "firstrange2")
        shinyjs::show(id = "secondrange1")
        shinyjs::show(id = "secondrange2")
        shinyjs::show(id = "thirdrange1")
        shinyjs::show(id = "thirdrange2")

        shinyjs::hide(id = "factorvariable")
      }
    })
    }
  

  # Run the application
  shinyApp(ui = ui, server = server)
}
