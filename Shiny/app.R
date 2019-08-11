source("shiny/conditional_densities.R")
# UI
ui <- shiny::navbarPage("Conditional Densities",

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
                            shiny::radioButtons("var_name",
                                                label = NULL,
                                                choices = c("Dataset is missing")),

                            # Select Variables to condition on
                            shiny::checkboxGroupInput("var_to_cond_on",
                                                      label = NULL,
                                                      choices = NULL)

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

server <- function(input, output, session){
    help <- list()


    data <- shiny::reactive({
        req(input$file1) ## ?req #  require that the input is available

        inFile <- input$file1

        df <- read.csv(inFile$datapath, header = input$header ,sep = input$sep,
                       quote = input$quote)

        updateRadioButtons(session,
                           inputId = "var_name",
                           label = "condition variable",
                           choices = names(df))

        updateCheckboxGroupInput(session,
                                 inputId = "var_to_cond_on",
                                 label = "Variables to condition on",
                                 choices = names(df))

        return(df)
    })

    output$contents <- shiny::renderTable({
        data()
    })

    #num_var <- reactive({1:length(input$var_to_cond_on)})


    lapply(1:20,
           function(i) {
    output[[paste0("condplot",i)]] <- shiny::renderPlot({
        plot_single_conditional_density(data(),
                                        input$var_name,
                                        input$quantiles,
                                        input$var_to_cond_on[i])
        })
    })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)

# to do:
#Button für Anwendung
#Fix categorical variables
# Factors are not factors when red
