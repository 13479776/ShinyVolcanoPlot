library(shiny)
shinyServer(function(input, output) {
    
    data <-  reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)

        dataframe <- read.csv(
            inFile$datapath, 
            header=input$header, 
            sep=input$sep,
            quote='"',
            stringsAsFactors=FALSE
        )
    })    
    
    output$plot <- renderPlot({ 
        if (is.null(input$file1)) return(NULL)
        dat <- data();
        plot(as.numeric(dat$logFC), -log10(as.numeric(dat$P.Value)),
             xlim=input$lfcr, ylim=range(0,input$lo), #Set limits
             xlab="log2 Fold-change", ylab="-log10(P.Value)") #Set axis labels
        abline(h=input$hl, col="red")
        abline(v=-input$vl, col="blue")
        abline(v=input$vl, col="blue")
        tmp <- dat[-log10(as.numeric(dat$P.Value))>input$hl & abs(dat$logFC)>input$vl,]
        try(text(tmp$logFC, -log10(tmp$P.Value), tmp$ID))
    })

    output$downloadData <- downloadHandler(
        filename = function() { 
            paste(gsub(".csv","", input$file1), '_selected.csv', sep='') 
        },
        content = function(file) {
            dat <-  data.frame(data());
            write.csv(dat[-log10(as.numeric(dat$P.Value))>input$hl & abs(dat$logFC)>input$vl
                            ,c("logFC","P.Value")], 
                      file, 
                      row.names=TRUE,
                      quote=FALSE)

        }
    )
    
    output$tableOut <- renderDataTable({
        dat <-  data.frame(data())
        if (is.null(input$file1)) return(NULL)
        dat[-log10(as.numeric(dat$P.Value))>input$hl & abs(dat$logFC)>input$vl,]
        
    })
       
})
