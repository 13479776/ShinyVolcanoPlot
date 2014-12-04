library(shiny)
shinyServer(function(input, output) {
    
    load("data/example.rda")
    
    data <-  reactive({
        inFile <- input$file1
#         if (is.null(inFile)) return(NULL)
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
    
    output$plot <- renderPlot({ 
        dat <- data();
        plot(as.numeric(dat$logFC), -log10(as.numeric(dat$P.Value)),
             xlim=input$lfcr, ylim=range(0,input$lo),
             xlab="log2(Fold-change)", ylab="-log10(P.Value)",
             cex = 0.35, pch=16)
        abline(h=input$hl, col="red")
        abline(v=-input$vl, col="blue")
        abline(v=input$vl, col="blue")
        tmp <- dat[-log10(as.numeric(dat$P.Value))>input$hl & abs(dat$logFC)>input$vl,]
        if(input$gene_names) try(text(tmp$logFC, -log10(tmp$P.Value), tmp$ID))
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
    
    output$tableOut <- renderDataTable({
        dat <-  data.frame(data())
        dat[-log10(as.numeric(dat$P.Value))>input$hl & abs(dat$logFC)>input$vl,c("ID","logFC","P.Value")]
    })
       
})
