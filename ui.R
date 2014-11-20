library(shiny)

shinyUI(fluidPage(
    titlePanel("Volcano Plot"),
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Choose CSV File',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            helpText("Note: The input file should be a ASCII text file (comma, tab, semicolon separated),
                     containing three columns named ID, logFC and P.Value.", tags$p(), " You can download the automatically loaded default example from",
                     tags$a(href="https://raw.githubusercontent.com/onertipaday/ShinyVolcanoPlot/master/data/example.csv","here"),"."),
            tags$p(),
            radioButtons('sep', 'Separator',
                         c(Tab='\t',
                           Comma=',',
                           Semicolon=';'
                           ),
                         selected=','),
            tags$hr(),
            h4("Axes"),
            sliderInput("lfcr", "Log2(Fold-Change) Range:", 
                        -10, 10, value = c(-2.5, 2.5), 
                        step=0.1, animate=FALSE),
            sliderInput("lo", "-Log10(P-Value):", 
                        0, 15, value = 4, step=0.05),
            tags$hr(),
            h4("Cut-offs Selection"),
            sliderInput("hl", "P-Value Threshold:",
                        1, 6, value = 1.30, step=0.1),
            verbatimTextOutput('conversion'),
            sliderInput("vl", "log2(FC) Threshold:", 
                        0,2, value = 0.8, step=0.1),
            tags$hr(),
            h4("Download Selected Genes"),
            downloadButton('downloadData', 'Download')
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot", width = "720px", height = "720px")), 
                        tabPanel("Cut-off Selected", dataTableOutput("tableOut"))  
            )
        )
    )
))