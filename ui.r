################################################################################
#                                                                              #
#   User Interface for Zesimate MD Shiny App                                   #                                               #
#                                                                              #
################################################################################

# Set library(s)

  library(shiny)

# Define UI for miles per gallon application

shinyUI(fluidPage(
  
  # Application title
  titlePanel("King County Sales Analysis: Value of a View"),
  
  # Sidebar with controls to select the model and the variable to display
  
  sidebarLayout(
    sidebarPanel(
      
      h3("Data Filters"),
      
      sliderInput("priceLimits", "Price Range",
                  min = 50000, max = 5000000, 
                  value = c(100000, 5000000),step=10000),
      sliderInput("lotSizeLimits", "Lot Size Range (SqFt)",
                  min = 500, max = 435600, 
                  value = c(1000, 43560*2), step=20),
      sliderInput("homeSizeLimits", "Home Size Range (SqFt)",
                  min = 300, max = 15000, 
                  value = c(500, 3000), step=50),
      sliderInput("bedLimits", "# of Bedrooms Range ",
                  min = 0, max = 14, 
                  value = c(0, 8), step=1),
      
      h6(" . "),
      submitButton("Calculate!"),
      
      h3("Model Variables"),
      
      h6("Variables of Interest"),
      checkboxInput("viewMount", label = "Mountain View", value = TRUE),
      checkboxInput("viewWater", label = "Water View", value = TRUE),
      checkboxInput("viewOther", label = "Other View", value = TRUE),
      checkboxInput("byScore", label = "Use View Scores?", value = FALSE),
      
      h6("Control Variables"),
      checkboxInput("SqFtTotLiving", label = "Home Size", value = TRUE),
      checkboxInput("YrBuilt", label = "Year Built", value = TRUE),
      checkboxInput("YrRenovated", label = "Year Renovated", value = TRUE),
      checkboxInput("Baths", label = "Baths", value = TRUE),
      checkboxInput("BldgGrade", label = "Building Quality", value = TRUE),
      checkboxInput("Fireplaces", label = "# of Fireplaces", value = TRUE),
      checkboxInput("Townhome", label = "Is a Townhome?", value = TRUE),
      checkboxInput("SqFtLot", label = "SqFtLot", value = TRUE),
      checkboxInput("WFNT", label = "Water frontage", value = TRUE),
      checkboxInput("Month", label = "Month", value = TRUE)
    ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
   tabsetPanel(
    tabPanel("Model Coefficients", tableOutput("valTable")),
    tabPanel("Map", plotOutput("mapP")),
    tabPanel("View Premiums", plotOutput("valPlot2"))
    
    
     #,tabPanel("Comp Map", plotOutput("mapPlot"))
     #,tabPanel("AETrans Comps",div(tableOutput("taeTable"),style='font-size:80%'))
     #,tabPanel("AEZest Comps",div(tableOutput("zaeTable"),style='font-size:80%'))
   )
  )
 )
))
