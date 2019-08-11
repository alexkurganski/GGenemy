source("shiny/conditional_densities.R")
# Define UI for application that draws a histogram
ui <- shiny::navbarPage("Conditional Densities",

    shiny::tabPanel("Data",

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::fileInput("file1", "Choose CSV File",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            # Input: Checkbox if file has header ----
            shiny::checkboxInput("header", "Header", TRUE),



            # Input: Select separator ----
            shiny::radioButtons("sep", "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","),

            # Input: Select quotes ----
            shiny::radioButtons("quote", "Quote",
                                choices = c(None = "",
                                            "Double Quote" = '"',
                                            "Single Quote" = "'"),
                                selected = '"')

            # Input: Select number of rows to display ----
            # shiny::radioButtons("disp", "Display",
            #                      choices = c(Head = "head",
            #                                  All = "all"),
            #                      selected = "head")

            ),
        shiny::mainPanel(
            shiny::tableOutput("contents")
            )
        )
    ),

    shiny::tabPanel("Plot",
                    shiny::sidebarLayout(
                        shiny::sidebarPanel(
                            shiny::sliderInput("quantiles",
                                               "Number of Quantiles:",
                                               min = 1,
                                               max = 10,
                                               value = 5),
                            shiny::checkboxGroupInput("var_name",
                                                      "",
                                                      choices = NULL),

                            shiny::checkboxGroupInput("var_to_cond_on",
                                                      "",
                                                      choices = NULL)

                            ),
                 # Show a plot of the generated distribution
                 shiny::mainPanel(
                    lapply(1:7, function(i) {
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

        updateCheckboxGroupInput(session,
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
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

    names <- c("V1","V2","V3","V4","V5","V6","V7")


    lapply(1:2,function(i) {
    output[[paste0("condplot",i)]] <- shiny::renderPlot({
        plot_single_conditional_density(data(),input$var_name,input$quantiles,input$var_to_cond_on[i])
        })
    })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
vari <- read.csv("variables.csv",row.names = "X")
names(vari)
?reactive()
