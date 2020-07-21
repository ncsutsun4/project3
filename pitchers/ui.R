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
    navbarPage("Patcher Data", theme = shinytheme("lumen"),
               tabPanel("Information", fluid = TRUE, icon = icon("globe-americas"),
                        fluidRow(
                            column(6,
                                   h4(p("School Types")),
                                   h5(p("US News and World Report uses four categories of schools for their rankings system:"),
                                      p("National universities are those that offer a “full range” of undergraduate majors, while also offering graduate programs, including at the doctoral level.  Intercollegiate sports, including swimming, are generally pursued by undergrads, or occasionally students in master’s degree programs, so a university having nor not having doctoral programs isn’t directly relevant.  That said, doctoral programs and faculty research go hand-in-hand, so faculty at national universities are nearly always active in research, in addition to their teaching duties.  National universities are usually, though not always, large.  Most state flagship universities would fall under this category."),
                                      p("Regional universities are similar to national universities in that they have a wide range of undergrad programs, and some master’s programs as well.  They generally do not have large doctoral programs, and correspondingly less faculty research."),
                                      p("National liberal arts colleges are undergraduate focused, with few graduate programs.  They award the majority of their degrees in arts and sciences, and may or may not have other undergraduate programs, like engineering or professional studies."),
                                      p("Regional colleges are also undergraduate focused institutions, but do not award the majority of their degrees in arts and/or sciences.  These colleges may have a particular focus, like technology or agriculture, or they may be primarily two year institutions that also grant some four year degrees.")
                                   )

                            ),
                            column(6,
                                   h4(p("US News Rankings")),
                                   h5(p("Every year the US News and World Report issues a set of rankings for US colleges and universities.  They are a used in this setting as a guideline, and a general comparative device, but can often be misinterpreted or overvalued.  The major component of a given school’s rankings are graduation and retention rates, academic reputation (basically name recognition), and faculty resources (class size, faculty salary etc.).  Each school is given a score, and then placed in order.  That said the scored differences between schools of different rank can be quite small, so take the rankings with a grain of salt.
                                    The full methodology for the US News and World report college rankings can be found ",
                                        ))
                            )
                        )
                        
               ),  
               
               tabPanel("Data Exploration", fluid = TRUE,icon = icon("bar-chart-o")

                           
               ),
               tabPanel("PCA analysis", fluid = TRUE,icon = icon("refresh")

               ),
               tabPanel("Modeling", fluid = TRUE, icon = icon("list-alt")

                            
               ),
               tabPanel("Data Exporting", fluid = TRUE,icon = icon("table")
                            
               )
                        
                        
        )
)
             
