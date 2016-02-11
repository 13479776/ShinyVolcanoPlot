shinyUI(fluidPage(
    titlePanel("Volcano Plot"),
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Choose CSV File',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv')),
            "Note: The input file should be a ASCII text file (either comma or tab separated),
                     containing three columns named ID, logFC and P.Value, respectivelly.",
            tags$hr(),
            radioButtons('sep', 'Separator', c(Tab='\t', Comma=','), selected=','),
            checkboxInput("f_test", "F-test", value = FALSE),
            tags$hr(),
            uiOutput("ui"),
            tags$hr(),
            downloadButton('downloadData', 'Download Selected DE genes list'),
            downloadButton('downloadPlot', 'Download Volcano Plot (PDF)')  
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("ggPlot", plotOutput("ggplot", width = "720px", height = "720px")),
                        tabPanel("Cut-off Selected", dataTableOutput("tableOut"))
            )
        )
    )
))