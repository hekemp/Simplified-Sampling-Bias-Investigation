# This example converts the Reeses Pieces Applet seen here:
# http://www.rossmanchance.com/applets/Reeses/ReesesPieces.html
# to a shiny app.

library(shiny)
library(ggplot2)
library(data.table)

baboon <- read.csv("baboons.csv")
baboonA <- read.csv("baboons.csv")
baboonM <- read.csv("baboonsM.csv")
baboonF <- read.csv("baboonsF.csv")
numberOfRows <- nrow(baboon)
histLims <- c(8,29)
binLength <- 41
redLineText <- 10
redLineWord <- "hello"
legendLocation <- "topright"
xLabel <- "Mass (lbs)"
sampsize <- 25

shinyServer(function(input, output) {# For storing which rows have been excluded
  
baboon <- reactive({
    switch(input$popSelect, "all" = baboonA, "males" = baboonM, "females" = baboonF)
    })
  
keptValues <- reactiveValues(
    keeprows = rep(TRUE, numberOfRows)
    )
  
meanDataCollection <- reactiveValues(
    meanDataSet = c()
  )
  
meanDataCollection2 <- reactiveValues(
    meanLengthDataSet = c()
  )
  
sampleData <- reactiveValues(
    lastSample = c()
  )
  
output$plot1 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- baboon()[ keptValues$keeprows, , drop = FALSE]
    exclude <- baboon()[!keptValues$keeprows, , drop = FALSE]
    plot(c(66, 157), c(7,30), xlab = "Length (ft)", ylab = "Mass (lbs)", pch = 20, col = "white", main = paste(getFirstTitle(), "\n", getTitle1()))
    abline(v = mean(baboonA$length), col = "red")
    abline(h = mean(baboonA$mass), col = "red")
    points(keep$length, keep$mass, col = "black", bg = "black", pch = 20)
    points(exclude$length, exclude$mass, pch = 20 , col = "darkgray", bg = "darkgray")
    text(x = 130, y = 8, label = "Grayed out data points are unavailable for sampling.")
  })
  
observeEvent(input$popSelect, {
    meanDataCollection$meanDataSet <- c()
    meanDataCollection2$meanLengthDataSet <- c()
    sampleData$lastSample <- c()
    if(input$popSelect == "all")
        {numberOfRows <- nrow(baboonA)
        keptValues$keeprows <- rep(TRUE, numberOfRows)
        binLength <- 41
        histLims <- c(8,29)}
    if(input$popSelect == "males")
        {numberOfRows <- nrow(baboonM)
         keptValues$keeprows <- rep(TRUE, numberOfRows)
         binLength <- 41
        histLims <- c(19,29)
         }
    if(input$popSelect == "females")
        {numberOfRows <- nrow(baboonF)
        keptValues$keeprows <- rep(TRUE, numberOfRows)
        binLength <- 41
        histLims <- c(8,17)}
  })
  
  
observeEvent(input$selection, {
    if(input$selection == "default")
        {keptValues$keeprows <- rep(TRUE, numberOfRows)
        meanDataCollection$meanDataSet <- c()
        meanDataCollection2$meanLengthDataSet <- c()
        sampleData$lastSample <- c()
    }
    
    if(input$selection == "lowerMass")
        {limit <- 11.5
            if(input$popSelect == "all")
                {limit <- 11.5
            }
    
            if(input$popSelect == "males")
                {limit <- 23.2
            }
    
            if(input$popSelect == "females")
                {limit <- 11.1
            }
    
        keptValues$keeprows <- rep(TRUE, numberOfRows)
        res <- rep(TRUE, numberOfRows)
        res[which(baboon()$mass > limit)] <- FALSE
        keptValues$keeprows <- xor(keptValues$keeprows, res)
        meanDataCollection$meanDataSet <- c()
        meanDataCollection2$meanLengthDataSet <- c()
        sampleData$lastSample <- c()
         }
    
    if(input$selection == "upperMass")
        {limit <- 16.8
        if(input$popSelect == "all")
            {limit <- 16.8
        }
    
        if(input$popSelect == "males")
            {limit <- 25.9
        }
    
        if(input$popSelect == "females")
            {limit <- 12.8 
        }
    
        keptValues$keeprows <- rep(TRUE, numberOfRows)
        res <- rep(TRUE, numberOfRows)
        res[which(baboon()$mass < limit)] <- FALSE
        keptValues$keeprows <- xor(keptValues$keeprows, res)
        meanDataCollection$meanDataSet <- c()
        meanDataCollection2$meanLengthDataSet <- c()
        sampleData$lastSample <- c()
    }
    
    if(input$selection == "lowerLength")
        {limit <- 93.2
        if(input$popSelect == "all")
            {limit <- 93.2
        }
    
        if(input$popSelect == "males")
            {limit <- 122.8  
        }
    
        if(input$popSelect == "females")
            {limit <- 89.8 
        }
        keptValues$keeprows <- rep(TRUE, numberOfRows)
        res <- rep(TRUE, numberOfRows)
        res[which(baboon()$length > limit)] <- FALSE
        keptValues$keeprows <- xor(keptValues$keeprows, res)
        meanDataCollection$meanDataSet <- c()
        meanDataCollection2$meanLengthDataSet <- c()
        sampleData$lastSample <- c()
    }
    
    if(input$selection == "higherLength")
        {limit <- 119.7
        if(input$popSelect == "all")
            {limit <- 119.7
        }
    
        if(input$popSelect == "males")
            {limit <- 140.7
        }
    
        if(input$popSelect == "females")
            {limit <- 106.2
        }
        keptValues$keeprows <- rep(TRUE, numberOfRows)
        res <- rep(TRUE, numberOfRows)
        res[which(baboon()$length < limit)] <- FALSE
        keptValues$keeprows <- xor(keptValues$keeprows, res)
        meanDataCollection$meanDataSet <- c()
        meanDataCollection2$meanLengthDataSet <- c()
        sampleData$lastSample <- c()
        }
    
})
  
observeEvent(input$draw_1_Sample, {
    newSample <- getSample()
    meanDataCollection$meanDataSet[length(meanDataCollection$meanDataSet) + 1] <- round(mean(newSample$mass), 3)
    meanDataCollection2$meanLengthDataSet[length(meanDataCollection2$meanLengthDataSet) + 1] <- round(mean(newSample$length), 3)
    sampleData$lastSample <- newSample
})
  
observeEvent(input$draw_10_Sample, {
    for (timesExecuted in 1:10)
        {newSample <- getSample()
        meanDataCollection$meanDataSet[length(meanDataCollection$meanDataSet) + 1] <- round(mean(newSample$mass), 3)
        meanDataCollection2$meanLengthDataSet[length(meanDataCollection2$meanLengthDataSet) + 1] <- round(mean(newSample$length), 3)
        sampleData$lastSample <- newSample
    }
})
  
observeEvent(input$clear_Samples, {
    meanDataCollection$meanDataSet <- c()
    meanDataCollection2$meanLengthDataSet <- c()
    sampleData$lastSample <- c()
})
  
getTitle1 <- function() {
    if(input$varSelect == "Length")
        {paste("Population Length Mean = ", round(mean(baboon()$length),2), "ft | Population Length SD = ", round(sd(baboon()$length),2),"ft")
    }
    else
        {paste("Population Mass Mean = ", round(mean(baboon()$mass),2), "lbs | Population Mass SD = ", round(sd(baboon()$mass),2),"lbs")
    }
    
}
  
getFirstTitle <- function() {
    
    if(input$popSelect == "males")
        {paste("Population (Male Baboons)")
    }
    
    else if(input$popSelect == "females")
        {paste("Population (Female Baboons)")
    }
    
    else
        {paste("Population (All Baboons)")
    }
    
}
  
getSample <- function(){
    keep2 <- baboon()[keptValues$keeprows, , drop = FALSE]
    dataTableCars <- data.table(keep2)
    samSet <- dataTableCars[sample(.N, sampsize, replace = TRUE)]
    return(samSet)
}
  
getTitleVar <- function(dat) {
    if(length(dat) == 0)
        {if(input$varSelect == "Length")
            {paste("Sample Length Mean = N/A | Sample Length SD = N/A")}
         else{
            paste("Sample Mass Mean = N/A | Sample Mass SD = N/A")
          }
      }
    else if(input$varSelect == "Length")
        {paste("Sample Length Mean = ", round(mean(dat),2), "ft | Sample SD = ", round(sd(dat),2),"ft")
    }
    else
        {paste("Sample Mass Mean = ", round(mean(dat),2), "lbs | Sample SD = ", round(sd(dat),2),"lbs")
    }
    
}
  
getHistTitle <- function()
  {if(input$popSelect == "all")
      {paste("Relationship of Sample Means to the Population Mean \n Each Sample Mean is Based on a Random Sample of Size", sampsize)}
   else if(input$popSelect == "males")
      {paste("Relationship of Sample Means to the Male Population Mean \n Each Sample Mean is Based on a Random Sample of Size", sampsize)}
   else
      {paste("Relationship of Sample Means to the Female Population Mean \n Each Sample Mean is Based on a Random Sample of Size", sampsize)}
}
  
getHistTitle2 <- function()
    {if(input$popSelect == "all")
        {paste("Last Sample of 25 Baboons from the Available Baboon Population")}
    else if(input$popSelect == "males")
        {paste("Last Sample of 25 Baboons from the Available Male Baboon Population")}
    else
        {paste("Last Sample of 25 Baboons from the Available Female Population")
    }
}
  
output$numSamples <- renderText({
    paste("The number of samples is: ", length(meanDataCollection$meanDataSet))
  })
  
output$plotHist2 <- renderPlot({
    
    if(input$varSelect == "Mass")
    {
      if(input$popSelect == "all")
          {histLims <- c(8,29) 
          redLineText <- 16
          redLineWord <- "←Population Mean"
          legendLocation <- "topright"
      }
      if(input$popSelect == "males")
          {histLims <- c(15,29)
          redLineText <- 21
          redLineWord <- "Male Population Mean→"
          legendLocation <- "topleft"
      }
      if(input$popSelect == "females")
          {histLims <- c(8,17)
          redLineText <- 9
          redLineWord <- "Female Population Mean→"
          legendLocation <- "topright"
      }
      
      if(length(sampleData$lastSample) == 0)
          {plot(1, type="n", main = paste(getHistTitle2(), "\n", getTitleVar(sampleData$lastSample$mass)), xlab=xLabel, ylab="Frequency", xlim=histLims, ylim= c(0, 21))
          abline(v=mean(baboon()$mass),col="red")
          legend(legendLocation,  c("Population Mean", "Sample Mean"), fill=c("red", "orange"))
      }
      
      else 
          {bins <- seq(min(baboon()$mass), max(baboon()$mass), length.out = binLength)
          hist(sampleData$lastSample$mass, breaks =bins, col = 'darkgray', border = 'white', main = paste(getHistTitle2(), "\n", getTitleVar(sampleData$lastSample$mass)), xlab = xLabel, ylab = "Frequency", xlim= histLims) 
          abline(v=mean(baboon()$mass),col="red")
          abline(v=mean(sampleData$lastSample$mass),col="orange")
          legend(legendLocation,  c("Population Mean", "Sample Mean"), fill=c("red", "orange"))
      }
    }
    
    
    if(input$varSelect == "Length")
    {
      if(input$popSelect == "all")
          {histLims <- c(66,157) 
          redLineText <- 85
          redLineWord <- "Population Mean→"
          legendLocation <- "topright"
      }
      if(input$popSelect == "males")
          {histLims <- c(105,157)
          redLineText <- 132
          redLineWord <- "←Male Population Mean"
          legendLocation <- "topleft"
      }
      if(input$popSelect == "females")
          {histLims <- c(66,140)
          redLineText <- 75
          redLineWord <- "Female Population Mean→"
          legendLocation <- "topright"
      }
      
      if(length(meanDataCollection2$meanLengthDataSet) == 0)
          {plot(1, type="n", main = paste(getHistTitle2(), "\n", getTitleVar(sampleData$lastSample$mass)), xlab="Length (ft)", ylab="Frequency", xlim=histLims, ylim= c(0, 21))
          abline(v=mean(baboon()$length),col="red")
          legend(legendLocation,  c("Population Mean", "Sample Mean"), fill=c("red", "orange"))
      }
      
      else 
          {bins <- seq(min(baboon()$length), max(baboon()$length), length.out = binLength)
          hist(sampleData$lastSample$length, breaks =bins, col = 'darkgray', border = 'white', main = paste(getHistTitle2(), "\n", getTitleVar(sampleData$lastSample$mass)), xlab = xLabel, ylab = "Frequency", xlim= histLims) 
          abline(v=mean(baboon()$length),col="red")
          abline(v=mean(sampleData$lastSample$length),col="orange")
          legend(legendLocation,  c("Population Mean", "Sample Mean"), fill=c("red", "orange"))
      }
      
    }
    
})
  
output$plot3 <- renderPlot({
    
    if(input$varSelect == "Mass")
    {
      if(input$popSelect == "all")
          {histLims <- c(8,29) 
          redLineText <- 16
          redLineWord <- "←Population Mean"
          legendLocation <- "topright"
      }
      if(input$popSelect == "males")
          {histLims <- c(15,29)
          redLineText <- 21
          redLineWord <- "Male Population Mean→"
          legendLocation <- "topleft"
      }
      if(input$popSelect == "females")
          {histLims <- c(8,17)
          redLineText <- 9
          redLineWord <- "Female Population Mean→"
          legendLocation <- "topright"
      }
      
      if(length(meanDataCollection$meanDataSet) == 0)
          {plot(1, type="n", main = getHistTitle(), xlab=xLabel, ylab="Frequency", xlim=histLims, ylim= c(0, 21))
          abline(v=mean(baboon()$mass),col="red")
          text(x=redLineText,y=20,pos=4,label = redLineWord)
          legend(legendLocation,  c("Most Recent Sample", "Previous Samples"), fill=c("orange", "darkgray"))
      }
      
      else if(length(meanDataCollection$meanDataSet) == 1)
          {bins <- seq(min(baboon()$mass), max(baboon()$mass), length.out = binLength)
          hist(meanDataCollection$meanDataSet, breaks =bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency", xlim= histLims, ylim= c(0, 21)) 
          hist(mean(sampleData$lastSample$mass), breaks =bins, col = 'orange', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency", xlim=histLims, ylim= c(0, 21), add = T)
          abline(v=mean(baboon()$mass),col="red")
          text(x=redLineText,y=20,pos=4,label = redLineWord)
          legend(legendLocation,  c("Most Recent Sample", "Previous Samples"), fill=c("orange", "darkgray"))
      }
      
      else {
          bins <- seq(min(baboon()$mass), max(baboon()$mass), length.out = binLength)
          counta <- hist(meanDataCollection$meanDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency")$counts
          if(max(counta) <= 20)
              {counta <- hist(meanDataCollection$meanDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency")$counts
              hist(meanDataCollection$meanDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency", xlim=histLims, ylim = c(0, 21))
              hist(mean(sampleData$lastSample$mass), breaks =bins, col = 'orange', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency", xlim=histLims, ylim = c(0, 21), add = T)
              abline(v=mean(baboon()$mass),col="red")
              text(x=redLineText,y=20,pos=4,label = redLineWord)
              legend(legendLocation,  c("Most Recent Sample", "Previous Samples"), fill=c("orange", "darkgray"))
              }
        
          else
              {counta <- hist(meanDataCollection$meanDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency")$counts
              hist(meanDataCollection$meanDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency", xlim= histLims, ylim = c(0, max(counta)+1))
              hist(mean(sampleData$lastSample$mass), breaks =bins, col = 'orange', border = 'white', main = getHistTitle(), xlab = xLabel, ylab = "Frequency", xlim=histLims, ylim = c(0, max(counta)+1), add = T)
              abline(v=mean(baboon()$mass),col="red")
              text(x=redLineText,y=max(counta),pos=4,label = redLineWord)
              legend(legendLocation,  c("Most Recent Sample", "Previous Samples"), fill=c("orange", "darkgray"))
          }
      }
      
    }
  
    if(input$varSelect == "Length")
    {
        if(input$popSelect == "all")
            {histLims <- c(66,157) 
            redLineText <- 85
            redLineWord <- "Population Mean→"
            legendLocation <- "topright"
        }
        if(input$popSelect == "males")
            {histLims <- c(105,157)
            redLineText <- 132
            redLineWord <- "←Male Population Mean"
            legendLocation <- "topleft"
        }
        if(input$popSelect == "females")
            {histLims <- c(66,140)
            redLineText <- 75
            redLineWord <- "Female Population Mean→"
            legendLocation <- "topright"
            }
      
        if(length(meanDataCollection2$meanLengthDataSet) == 0)
            {plot(1, type="n", main = getHistTitle(), xlab="Length (ft)", ylab="Frequency", xlim=histLims, ylim= c(0, 21))
            abline(v=mean(baboon()$length),col="red")
            text(x=redLineText,y=20,pos=4,label = redLineWord)
            legend(legendLocation,  c("Most Recent Sample", "Previous Samples"), fill=c("orange", "darkgray"))
        }
      
        else if(length(meanDataCollection2$meanLengthDataSet) == 1)
            {bins <- seq(min(baboon()$length), max(baboon()$length), length.out = binLength)
            hist(meanDataCollection2$meanLengthDataSet, breaks =bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency", xlim= histLims, ylim= c(0, 21)) 
            hist(mean(sampleData$lastSample$length), breaks =bins, col = 'orange', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency", xlim=histLims, ylim= c(0, 21), add = T)
            abline(v=mean(baboon()$length),col="red")
            text(x=redLineText,y=20,pos=4,label = redLineWord)
            legend(legendLocation,  c("Most Recent Sample", "Previous Samples"), fill=c("orange", "darkgray"))
        }
      
        else {
          bins <- seq(min(baboon()$length), max(baboon()$length), length.out = binLength)
          counta <- hist(meanDataCollection2$meanLengthDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency")$counts
          if(max(counta) <= 20)
              {counta <- hist(meanDataCollection2$meanLengthDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency")$counts
              hist(meanDataCollection2$meanLengthDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency", xlim=histLims, ylim = c(0, 21))
              hist(mean(sampleData$lastSample$length), breaks =bins, col = 'orange', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency", xlim=histLims, ylim = c(0, 21), add = T)
              abline(v=mean(baboon()$length),col="red")
              text(x=redLineText,y=20,pos=4,label = redLineWord)
              legend(legendLocation,  c("Most Recent Sample", "Previous Samples"), fill=c("orange", "darkgray"))
           }
        
          else
              {counta <- hist(meanDataCollection2$meanLengthDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency")$counts
              hist(meanDataCollection2$meanLengthDataSet, breaks = bins, col = 'darkgray', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency", xlim= histLims, ylim = c(0, max(counta)+1))
              hist(mean(sampleData$lastSample$length), breaks =bins, col = 'orange', border = 'white', main = getHistTitle(), xlab = "Length (ft)", ylab = "Frequency", xlim=histLims, ylim = c(0, max(counta)+1), add = T)
              abline(v=mean(baboon()$length),col="red")
              text(x=redLineText,y=max(counta),pos=4,label = redLineWord)
              legend(legendLocation,  c("Most Recent Sample", "Previous Samples"), fill=c("orange", "darkgray"))
            }
      }
      
    }
    
})
  
})
