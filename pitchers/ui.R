library(shiny)
library(pitchRx)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(knitr)
library(plyr)
library(DT)
library(shinythemes)

# Define UI
ui <- fluidPage(
    #Navbar structure for UI
    navbarPage("Patcher Data", theme = shinytheme("journal"), # theme = lumen
               # First Navbar - information section;
               tabPanel("Information", fluid = TRUE, icon = icon("globe-americas"),
                        fluidRow(
                            withMathJax(), # Include mathjax
                            # Data introduction
                            column(6,
                                   h1(p("Data Introduction")),
                                   box(background = "red", width=12,
                                   h4(p("PITCHf/x is a system developed by Sportvision and introduced in Major League Baseball (MLB) 
                                        during the 2006 playoffs.  It uses two cameras to record the position of the pitched baseball
                                        during its flight from the pitcher’s hand to home plate, and various parameters are measured 
                                        and calculated to describe the trajectory and speed of each pitch. It is now instituted in all ballparks in MLB."),
                                      p("Great amount of data was collected with PITCHf/x system. The data set in this project is comprised of 
                                        all pitches thrown on Mondays during the 2016 MLB regular season, excluding intentional walks. ",
                                        a("Here is the link", href="https://www2.stat.duke.edu/courses/Summer17/sta101.001-2/uploads/project/project.html")),
                                      p("The PITCHf/x collected parameter about each pitch, and a best fitting curve is fit to the following equations of motion."),
                                      p("$$ x(t) = x_0 + vX_0 * t + ax * t^2 $$"),
                                      p("$$ y(t) = y_0 + vy_0 * t + ay * t^2 $$"),
                                      p("$$ z(t) = z_0 + vz_0 * t + az * t^2 $$"),
                                      p("The fitted coefficients are saved as: $$x_0, y_0, z_0, vx_0, vy_0, vz_0, ax, ay, az.$$ "),
                                      p(a("More introduction can be see here", href="https://pitchrx.cpsievert.me/"))
                                   ))

                            ),
                            # APP introduction
                            column(6,
                                   h1(p("App introduction")),
                                   box(background="red", width=12,
                                   h4(p("This App is to exploring the data and developing some interactive models "),
                                      p(strong("Data Exploration"), ": The first tab is to see the distribution
                                        of the distance by different pitch types. The second tab is to show the original data. The third tab is to 
                                        check the scatter plot of the numeric variables in the data"),
                                      p(strong("PCA Analysis"), ": PCA analysis of the numeric variables in the dataset. Tab 1 showed how the Scree plot - 
                                        variances against the number of the principal component. The second tab is for checking the Biplot - 
                                        the check the principal components and data variables' relationship."),
                                      p(strong("Modeling"), ": Fit the data using two model: linear regression and logistic regression. You may select variable's values to 
                                        make predictions"),
                                      p(strong("Data Exporting"), ": To view and download the data you interested. ")
                                   ))
                            )
                        )
                        
               ),  
               # second Navbar - Data exploration section;
               tabPanel("Data Exploration", fluid = TRUE,icon = icon("bar-chart-o"),
                        titlePanel("Pitch distance by pitch type"),
                        fluidRow(
                            # selection panel
                            column(3,
                                   helpText(h3("Viewing the baseball distance by selecting different pitch type")),
                                   selectizeInput(inputId = "pitchType",
                                                  label = "Pitch Type",
                                                  choices = c("Four-seamfastball"="FF",
                                                              "Two-seamfastball"="FT",
                                                              "Slider"="SL",
                                                              "Curveball"="CU",
                                                              "Changeup"="CH",
                                                              "Cut-fastball"="FC",
                                                              "Sinker"="SI",
                                                              "Knucklecurveball"="KC",
                                                              "Split-fingerfastball"="FS",
                                                              "Unknown"="UN",
                                                              "Knuckleball"="KN",
                                                              "Eephus"="EP",
                                                              "Screwball"="SC"),
                                                  selected ="FF"
                                   ),
                                   selectInput(inputId = "side",
                                               label = "Top or Botton Innings",
                                               choices = c("Top" = "T", "Bottom" = "B"),
                                               selected = "T"),
                                   
                                   helpText(h3("Select school and event to create plots")),
                                   downloadButton("download1", "Download Data Set"),
                                   
                                   
                                   br(), br(), br(),
                                   helpText(h3("You may also view two variable's scatter plot by select two variables")),
                                   selectInput(inputId = "first", label="Select 1st variable",
                                               choices=c("releaseVelocity", "locationHoriz", "locationVert", "movementHoriz", "movementVert", "battedBallAngle","battedBallDistance"),
                                               selected="releaseVelocity"),
                                   
                                   selectInput(inputId = "second", label="Select 2nd variable",
                                               choices=c("releaseVelocity", "locationHoriz", "locationVert", "movementHoriz", "movementVert", "battedBallAngle","battedBallDistance"),
                                               selected="battedBallDistance")
                            ),
                            # output panel
                            column(9,
                                   tabsetPanel(
                                   tabPanel(uiOutput("subt1"),
                                               fluidRow(
                                                 column(12, plotlyOutput("hist")))),
                                   tabPanel(uiOutput("subt2"),
                                               fluidRow(
                                                 column(12, DT::dataTableOutput('table')))), 
                                   tabPanel(uiOutput("subt21"),
                                               fluidRow(
                                                 column(12, plotlyOutput("scatter"))))
                                     )
                                   )
                                )
               ),
               # Third Navbar - DPCA section;
               tabPanel("PCA Analysis", fluid = TRUE,icon = icon("refresh"),
                        titlePanel("PCA analysis"),
                        fluidRow(
                          # selection panel
                          column(3,
                                 selectizeInput(inputId = "pitcherHand",
                                                label = "Pitcher Handedness",
                                                choices = c("Left-Handed"="L",
                                                            "Right-Handed"="R"
                                                ),
                                                selected ="R"
                                 ),
                                 helpText("Select school and event to create plots"),
                                 downloadButton("download2", "Download Data Set"),
                                 br(),br(),

                                 h3("Select 2 of the variables to specify biplot algorithm"),
                                 numericInput("selectPc1",
                                                label ="Select First PC",
                                                min=1, max=7, value=1
                                               ),
                                 numericInput("selectPc2",
                                              label ="Select Second PC",
                                              min=1, max=7, value=2
                                              )
                          ),
                          # output panel
                          column(9,
                                 tabsetPanel(
                                   tabPanel(uiOutput("subt3"),
                                            fluidRow(
                                              column(12, plotOutput("screePlot")))),
                                   tabPanel(uiOutput("subt4"),
                                            fluidRow(
                                              column(12, plotOutput("biPlot"))))
                                 )
                          )
                        )
               ),
               
               # Fourth Navbar - Data modeling and prediction section;
               tabPanel("Modeling", fluid = TRUE, icon = icon("list-alt"),
                        titlePanel(textOutput("titleModel")), 
                        fluidRow(
                                plotOutput("mplot2"), # histogram plot
                                # selection panel
                          column(6,
                                 selectizeInput("selectModel","Select Regression Model",
                                                choices=c("Logistic Regression", "Linear Regression"),
                                                selected ="Linear Regression"),
                                 br(), br(), br(),
                                 conditionalPanel(
                                   condition = "input.selectModel == 'Linear Regression'",
                                   selectizeInput("pHand", "Pitcher Handedness", choices = c("Left-Handed"="L", "Right-Handed"="R"),selected ="R")
                                 ),
                                 conditionalPanel(
                                   condition = "input.selectModel == 'Logistic Regression'",
                                   selectizeInput("bHand", "Batter Handedness", choices = c("Left-Handed"="L", "Right-Handed"="R"),selected ="R")
                                 ),
                                 sliderInput("releaseVelocity", "Pitch velocity (mph)", 
                                             min = 60, max = 110, value = 88, step = 5),
                                 sliderInput("locationHoriz", "Horizontal distance from plate center (feet)", 
                                             min = 0, max = 5, value = 2.5, step = 0.5),
                                 sliderInput("locationVert", "Vertical distance from plate center (feet)", 
                                             min = -20, max = 20, value = 0, step = 5)
                                 
                                 ),
                          # prediction output panel
                          column(6,
                                 h4("Model coefficients: "),
                                 br(),
                                 tableOutput("coefs"),
                                 br(),
                                 br(),
                                 h4("The prediction is : "),
                                 br(),
                                 #Output the predict result
                                 h4(textOutput("preds"))
                                 )
                        )

               ),
               # Fifth Navbar - Data viewing and exporting section;
               tabPanel("Data Exporting", fluid = TRUE,icon = icon("table"),
                        fluidRow(
                          # download section
                          column(3, 
                                 h3("Download dataset here"),
                                 downloadButton("export", "Download Data Set")
                                 ),
                          # datatable section
                          column(9,
                                 # box(title = "Dataset", status = "primary", 
                                 #     div(style = 'overflow-x: scroll', DT::dataTableOutput('exportData')))
                                 DT::dataTableOutput('exportData')
                                 )
                        )
                            
               )
                        
                        
        )
)       
