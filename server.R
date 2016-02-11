suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))

shinyServer(function(input, output, session) {
    
    load("data/example.rda")
    
    data <-  reactive({
        inFile <- input$file1
        if(is.null(inFile)) {
            dataframe <- example          
        } else {
            
            dataframe <- read.csv(
                inFile$datapath, 
                sep=input$sep,
                quote='"',
                stringsAsFactors=FALSE
            )}
    })  
    
    output$ui <- renderUI({
        # Depending on input$f_test, we'll generate a different UI component and send it to the client.
        if(input$f_test){
            "cb1" = checkboxInput("gene_names", "Show gene names", value = FALSE)
            # tags$hr()
            h4("Axes")
            "sI1" = sliderInput("lfcr", "Log2(Fold-Change) Range:", 
                                0, 10, value = c(0,3), 
                                step=0.1, animate=FALSE)
            "sI2" = sliderInput("lo", "-Log10(P-Value):", 
                                0, 30, value = 25, step=0.05)
            tags$hr()
            h4("Cut-offs Selection")
            "sI3" = sliderInput("hl", "P-Value Threshold:",
                                1, 20, value = 1.30, step=0.1)
            "co" = verbatimTextOutput('conversion')
            "sI4" = sliderInput("vl", "log2(FC) Threshold:", 
                                0,3, value = 0.8, step=0.1)
            return(list(cb1, sI1, sI2, sI3, co, sI4))
        } else {
            "cb1t" = checkboxInput("gene_names", "Show gene names", value = FALSE)
            tags$hr()
            h4("Axes")
            "sI1t" = sliderInput("lfcr", "Log2(Fold-Change) Range:", 
                                -10, 10, value = c(-2.5, 2.5), 
                                step=0.1, animate=FALSE)
            "sI2t" = sliderInput("lo", "-Log10(P-Value):", 
                                0, 15, value = 4, step=0.05)
            tags$hr()
            h4("Cut-offs Selection")
            "sI3t" = sliderInput("hl", "P-Value Threshold:",
                                1, 6, value = 1.30, step=0.1)
            "co" = verbatimTextOutput('conversion')
            "sI4t" = sliderInput("vl", "log2(FC) Threshold:", 
                                0,2, value = 0.8, step=0.1)
            tags$hr()
            return(list(cb1t, sI1t, sI2t, sI3t, co, sI4t))
        }
    })
    
    ggplotInput <- reactive({ 
        dat <- data();
        if(input$f_test){
            dat2 <- data.frame(x=sqrt(as.numeric(dat$logFC)), y=-log10(as.numeric(dat$P.Value)), ID=dat$ID)
            p <- ggplot(dat2, aes(x, y, label= ID)) + geom_point() +
                geom_vline(xintercept = input$vl, color = "blue") + #add vertical line
                # geom_vline(xintercept = -input$vl, color = "blue") + #add vertical line
                geom_hline(yintercept = input$hl, color = "red") +  #add vertical line
                labs(x="log2(Fold-change)", y="-log10(P.Value)") + 
                scale_x_continuous("log2(Fold-change)", limits = input$lfcr) +
                scale_y_continuous("-log10(P.Value)", limits = range(0,input$lo)) + theme_bw()
            
            tmp <- dat[-log10(as.numeric(dat$P.Value))>input$hl & dat$logFC>input$vl,]

            q <- p + annotate("text", x=tmp$logFC, y=-log10(tmp$P.Value),
                              label=tmp$ID, vjust=-0.1, hjust=-0.1)
        } else {
            dat2 <- data.frame(x=as.numeric(dat$logFC), y=-log10(as.numeric(dat$P.Value)), ID=dat$ID)
            p <- ggplot(dat2, aes(x, y, label= ID)) + geom_point() +
                geom_vline(xintercept = input$vl, color = "blue") + #add vertical line
                geom_vline(xintercept = -input$vl, color = "blue") + #add vertical line
                geom_hline(yintercept = input$hl, color = "red") +  #add vertical line
                labs(x="log2(Fold-change)", y="-log10(P.Value)") + 
                scale_x_continuous("log2(Fold-change)", limits = input$lfcr) +
                scale_y_continuous("-log10(P.Value)", limits = range(0,input$lo)) + theme_bw()
            
            tmp <- dat[-log10(as.numeric(dat$P.Value))>input$hl & abs(dat$logFC)>input$vl,]
            
            q <- p + annotate("text", x=tmp$logFC, y=-log10(tmp$P.Value), 
                              label=tmp$ID, size=-log10(as.numeric(tmp$P.Value)), 
                              vjust=-0.1, hjust=-0.1)
        }
        
        if(input$gene_names) q else p
    })
    
    
    output$ggplot <- renderPlot({
        print(ggplotInput())
    })
    
    output$conversion <- renderPrint(10^-(input$hl))
    
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste(gsub(".csv","", input$file1), '_selected.csv', sep='') 
        },
        content = function(file) {
            dat <-  data.frame(data());
            write.csv(dat[-log10(as.numeric(dat$P.Value))>input$hl & abs(dat$logFC)>input$vl
                          ,c("ID","logFC","P.Value")], 
                      file, 
                      row.names=FALSE,
                      quote=FALSE)
        }
    )
    
    
    output$downloadPlot<- downloadHandler(
        filename <- function() {
            paste(gsub(".csv","", input$file1), Sys.Date(),'.pdf',sep='')
        },
        content = function(file) {
            pdf(file)
            print(ggplotInput())
            dev.off()
        }
    )
    
    output$tableOut <- renderDataTable({
        dat <-  data.frame(data())
        dat[-log10(as.numeric(dat$P.Value))>input$hl & abs(dat$logFC)>input$vl,c("ID","logFC","P.Value")]
    })
    
})
