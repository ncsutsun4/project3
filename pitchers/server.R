library(shiny)
library(pitchRx)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(knitr)
library(plyr)
library(DT)



# Download data set 
if(!exists("mondayBaseball")) {
    load(url("http://stat.duke.edu/courses/Summer17/sta101.001-2/uploads/project/mondayBaseball.Rdata"))
}

# Subset new data set with only pitches hit in play and remove some unrelated varables
InPlay <- mondayBaseball %>% filter(pitchResult == "IP") %>% 
    select(-c(gameString, gameDate, batterId, batterName, batterPosition, timesFaced,
              catcher, umpireId, umpire)) %>% select_if(~ !any(is.na(.)))

# balls : # of balls thrown before that thrown
# strikes : # of strickes batter obtained
# outs  :  players been declared out
# 
# pitch Velocity by pitch Type
# breakingballs <- c("CU","KC","SC","SL")
# changeups <- c("CH","KN","EP")
 fastballs <- c("FC","FF","FS","FT","SI")

sideLabs <- c("Top Inning", "Bottom Inning")
names(sideLabs) <- c("T", "B")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # For 2st navbarPage - EDA
    # Get updated data set
    plotData <- reactive({
        print(input$pitchType) ; print(input$side);
       plotData <- InPlay %>% dplyr::filter(side == input$side & pitchType == input$pitchType)
       print(head(plotData))
       plotData
    })
    
    output$hist <- renderPlotly({
        data <- plotData()
        plot_ly(data, x= ~battedBallDistance, type="histogram") %>% 
            layout(title = "Distribution of Batted Ball Distance", 
                   xaxis = list(title = "Distance From Home Plate"), 
                   yaxis = list(title = "Count"))
    })
    
    # output data
    output$table <- renderTable({
        table <- plotData() %>% arrange(battedBallDistance)
        head(table)
    })
    
    output$download1 <- downloadHandler(
        filename = function(){paste(input$pitchType, "_", input$side, ".csv")},
        content = function(file){write.csv(plotData(), file, row.names = FALSE)}
    )
    
    # Title
    output$subt1 <- renderUI({paste0("Histogram")})
    output$subt2 <- renderUI({paste0("Table")})
    
    
    # For 3rd navbarPage - PCA Analysis
    
    
    # Select ball related numeric data;
    pcaData <- reactive({
        print(input$pitcherHand) ; print(input$selectPc1); print(input$selectPc2)
        pcaData <- InPlay %>% dplyr::filter(pitcherHand == input$pitcherHand) %>%
            select(c(releaseVelocity, locationHoriz, locationVert,
                     movementHoriz, movementVert, battedBallAngle, battedBallDistance))
        print(head(pcaData))
        pcaData
    })

     # pcaAnalysis <- prcomp(pcaData, center = TRUE, scale = TRUE)
     # screPlot <- screeplot(pcaAnalysis, type="lines")

    output$screePlot <- renderPlot({
        pcaData <- pcaData()
        pcaAnalysis <- prcomp(pcaData, center = TRUE, scale = TRUE)
        screPlot <- screeplot(pcaAnalysis, type="lines")
        screPlot
    })

    # # download scree plot
     output$pcaDownload <- downloadHandler(
         filename = function(){paste(input$pitcherHand, "handed Scree plot.png")},
         content = function(file){
             png(file)
             pcaData <- pcaData()
             pcaAnalysis <- prcomp(pcaData, center = TRUE, scale = TRUE)
             screeplot(pcaAnalysis, type="lines")
             screPlot
             dev.off()
             }
     )

     # output table
     output$table2 <- renderTable({
         pcaData <- pcaData()
         pcaAnalysis <- prcomp(pcaData, center = TRUE, scale = TRUE)
         pcaAnalysis$rotation
     })
    
     output$download2 <- downloadHandler(
         filename = function(){paste(input$pitcherHand, "-Handed.csv")},
         content = function(file){write.csv(pcaData(), file, row.names = FALSE)}
    )
     
     output$subt3 <- renderUI({paste0(input$ptid2, " proportion of variable explained")})
     output$subt4 <- renderUI({paste0(input$ptid2, " Bioplot")})

    output$biPlot <- renderPlot({
        pcaData <- pcaData()
        pcaAnalysis <- prcomp(pcaData, center = TRUE, scale = TRUE)
        biplot(pcaAnalysis,xlabs = rep(".", nrow(pcaData)), cex = 1.2, choices=c(input$selectPc1, input$selectPc2))
    })
    
    # Fit models and predict navbarPage -3
    output$titleModel <- renderText({
        paste0("Fitting ", input$selectModel, " model")
    })
    
    mData <- reactive({
        mData <- InPlay %>%  select(pitchType, releaseVelocity, locationHoriz, locationVert,
                                    pitcherHand, batterHand, battedBallDistance, side) %>% 
            filter(pitcherHand == input$pHand & batterHand == input$bHand)%>% 
            mutate(FF_Type = ifelse(pitchType %in% fastballs, 1, 0))
    })
    
    # output plot-3
    output$mplot2 <- renderPlot({
        ggplot(mData(), aes(x = releaseVelocity)) +
            geom_histogram(binwidth = 1, color = "red") +
            facet_grid(~ side, labeller=labeller(side=sideLabs)) +
            xlim(60,105) +
            ylab("Frequency") +
            xlab("Pitch Speed (mph)") +
            ggtitle("Pitch Velocity by Top or Bottom Inning") +
            theme(panel.grid.minor = element_blank(),
                  axis.ticks = element_blank())
    })
    
    # output model coefs
    output$coefs <- renderTable({
        modelData <- mData()
        
        if(input$selectModel == "Linear Regression"){
            model <- lm(battedBallDistance ~ releaseVelocity + locationHoriz + locationVert, data = modelData)
        }else if(input$selectModel == "Logistic Regression"){
            model <- glm(FF_Type ~ releaseVelocity +  locationHoriz + locationVert, family="binomial", data=modelData)
        }
        print(model$coefficients)
        coefs <- as.data.frame(as.list(model$coefficients))
    })
    
    output$preds <- renderText({
        
        print(input$releaseVelocity);print(input$locationHoriz);print(input$locationVert)
        
        modelData <- mData()
        if(input$selectModel == "Linear Regression"){
            model <- lm(battedBallDistance ~ releaseVelocity + locationHoriz + locationVert   , data = modelData)
            preds <- predict(model, newdata = data.frame(releaseVelocity=input$releaseVelocity,
                                                locationHoriz = input$locationHoriz,
                                                locationVert = input$locationVert))
            text <- paste0("The predicted batted ball distance is: " , round(preds, 2), " feet.")
        }else if(input$selectModel == "Logistic Regression"){
            model <- glm(FF_Type ~ releaseVelocity +  locationHoriz + locationVert   , family="binomial", data=modelData)
            preds <-predict(model, newdata = data.frame(releaseVelocity=input$releaseVelocity,
                                                locationHoriz = input$locationHoriz,
                                                locationVert = input$locationVert), type="response")
            text <- paste0("The predicted type of the pitch is ", ifelse(preds > 0.5, "'Four-seam fastball'", "'Not a four-seam fastball'"), ".")
        }
        text

    })

    
    # Exporting dataset navbarPage -4
    exportData <- reactive({
        exportData <- InPlay 
        print(head(pcaData))
        exportData
    })
    # Table output
    output$exportData <- DT::renderDataTable({
        DT::datatable({
            exportData()},
            extensions = 'Buttons',
            
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'tB',
                buttons = c('copy', 'csv', 'excel')
            ),
            
            class = "display"
            
            )

    })
    #For download
    output$export <- downloadHandler(
        filename = function(){paste("Pitcher data.csv")},
        content = function(file){write.csv(exportData(), file, row.names = FALSE)}
    )
})
