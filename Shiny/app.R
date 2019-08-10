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
            #Horizontal Line
            tags$hr(),

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
                                selected = '"'),
            tags$hr(),

            # Input: Select number of rows to display ----
            shiny::radioButtons("disp", "Display",
                                choices = c(Head = "head",
                                            All = "all"),
                                selected = "head")


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
                                               value = 5)
                            ),
                 # Show a plot of the generated distribution
                 shiny::mainPanel(
                    lapply(1:length(names(df)), function(i) {
                        shiny::plotOutput(paste0('condplot', i))
                    })
                )
            )
    )
)

server <- function(input, output, session){
    help <- list()

    output$contents <- shiny::renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        shiny::req(input$file1)

        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)

        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }

    })

    names <- names(df)

    lapply(1:length(df),function(i) {
        output[[paste0("condplot",i)]] <- shiny::renderPlot({
            plot_single_conditional_density(df,"V1",input$quantiles,names[i])
    })
 })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
