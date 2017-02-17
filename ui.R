# This example converts the Reeses Pieces Applet seen here:
# http://www.rossmanchance.com/applets/Reeses/ReesesPieces.html
# to a shiny app.

library(shiny)

biasChoices <<- list("No Bias" = "default", "Baboons that have the smallest mass (lbs) [~Lower 25%]" = "lowerMass", "Baboons that have the largest mass (lbs) [~Upper 25%]" = "upperMass", "Baboons with the shortest length (ft) [~Lower 25%]" = "lowerLength", "Baboons with the longest length [~Upper 25%]" = "higherLength")
populationChoices <<- list("All Baboons" = "all", "Male Baboons" = "males", "Female Baboons" = "females")
sampleVariableChoices <<- list("Mass" = "Mass", "Length" = "Length")

shinyUI(pageWithSidebar(
  
  headerPanel("Sampling Bias Investigation"),
  
  sidebarPanel(
    
    HTML("Step 1) Choose the population of baboons you are interested in studying."),
    radioButtons("popSelect", "", choices = populationChoices),
    helpText(" "),
    HTML('Step 2) Explore the effects of <font color="blue"><span title="Sampling bias results from flaws in the sampling process resulting in some segment of individuals in the population of interest having less chance (or no chance) of being selected for inclusion in the sample."> biased sampling </span></font> by <font color="blue"><span title="To see the impact of one of the pre-determined types of sampling bias, use the dropdown window to select a specific type of bias.">specifying the way</span></font> in which the bias will be present in the data.'),
    helpText(" "),
    HTML("Select a group of baboons to make <b>unavailable</b> for sampling:"),
    selectInput("selection", "", choices = biasChoices),
    helpText("Note: Changing this variable will reset the graph's settings."),
    HTML("Step 3) Select which variable to focus on for this study."),
    helpText(" "),
    radioButtons("varSelect", "Select a variable to plot your samples on:", choices = sampleVariableChoices),
    HTML("Step 4) Click on Draw 1 Sample to draw a random sample of baboons from the population of baboons that are available."),
    actionButton("draw_1_Sample", "Draw 1 Sample \n"),
    actionButton("draw_10_Sample", "Draw 10 Samples \n"),
    textOutput("numSamples"),
    actionButton("clear_Samples", "Clear Samples"),
    
  
    helpText("Notes: The red vertical line on the histogram indicates the population average when there is no bias and all baboons in the population of interest are measured.")
  ),
  
  
  fluidRow(
    column(width = 7,
           plotOutput("plot1"),
           plotOutput("plotHist2"),
           plotOutput("plot3"),
           HTML("Miscellaneous:"),
           helpText("A histogram will be constructed using the mean of the mass or length, depending on your selection, per sample with 41 bins.")
    )
  )
)
)
