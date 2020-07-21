
library(shiny)


### SHINY UI ###
ui <- bootstrapPage(
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               "PitchRx", id="nav",
               
               tabPanel("Information",
                   tags$div(
                       tags$h4("Background"), 
                       "The data set pitches contains every four-seam and cutting fastball thrown by Mariano Rivera and Phil Hughes during the 2011 season.",
                       tags$br(),
                       "This project is to analysis the pitcher dataset and create prediction models for pitch.",
                       tags$br(),tags$br(),
                       "The code for pitch visualization is adopted from ",tags$a(href="https://pitchrx.cpsievert.me/", "Introduction to pitchRx package. "),
                       tags$br(),tags$br(),tags$h4("Code"),
                       "Code and output are available on ",tags$a(href="https://github.com/ncsutsun4/project3", "Github."),
                       tags$br(),tags$br(),tags$h4("Sources"),

                       tags$br(),tags$br(),tags$h4("Authors"),
                       "Tao Sun",tags$br(),
                       tags$br(),tags$br(),tags$h4("Contact"),
                       "tsun4@ncsu.edu",tags$br(),tags$br(),

                   )
                   
               ),
               
               tabPanel("Information",
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
                                
                                pickerInput("level_select", "Level:",   
                                            choices = c("Global", "Continent", "Country", "US state"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select", "Country/Region:",   
                                            choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = cv_today_reduced$country,
                                            multiple = TRUE), 
                                
                                pickerInput("outcome_select", "Outcome:",   
                                            choices = c("Deaths per 100,000", "Cases per 100,000", "Cases (total)", "Deaths (total)"), 
                                            selected = c("Deaths per 100,000"),
                                            multiple = FALSE),
                                
                                pickerInput("start_date", "Plotting start date:",   
                                            choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = "Date",
                                            multiple = FALSE), 
                                
                                sliderInput("minimum_date",
                                            "Minimum date:",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value=as.Date(cv_min_date),
                                            timeFormat="%d %b"),
                                
                                "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 1000 confirmed cases are included."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                                    tabPanel("New", plotlyOutput("country_plot")),
                                    tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
                                )
                            )
                        )
               ),
               
               tabPanel("Data Exploration",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("sars_map", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 55, width = 250, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          h3(textOutput("sars_reactive_case_count"), align = "right"),
                                          h4(textOutput("sars_reactive_death_count"), align = "right"),
                                          h6(textOutput("sars_clean_date_reactive"), align = "right"),
                                          h6(textOutput("sars_reactive_country_count"), align = "right"),
                                          plotOutput("sars_epi_curve", height="130px", width="100%"),
                                          plotOutput("sars_cumulative_plot", height="130px", width="100%"),
                                          span(("The final count appears to decrease as several cases initially classified as SARS were later re-assigned."),align = "left", style = "font-size:80%"),#tags$br(),
                                          span(("Circles show confirmed cases for COVID, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),
                                          
                                          sliderTextInput("sars_plot_date",
                                                          label = h5("Select mapping date"),
                                                          choices = format(unique(sars_cases$date), "%d %b %y"),
                                                          selected = format(sars_max_date, "%d %b %y"),
                                                          grid = TRUE,
                                                          animate=animationOptions(interval = 3000, loop = FALSE))
                            ),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 15, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                          tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                            
                            absolutePanel(id = "logo", class = "card", bottom = 15, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                          actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                       onclick = sprintf("window.open('%s')", 
                                                                         "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                        )
               ),
               
               tabPanel("Clustering Analysis",
                        
                        sidebarLayout(
                            sidebarPanel(
                                radioButtons("comparison_metric", h3("Select comparison:"),
                                             c("Cases" = "cases",
                                               "Deaths" = "deaths",
                                               "Countries/regions affected" = "countries",
                                               "Case fatality rate" = "cfr")),
                                textOutput("epi_notes_1"),
                                textOutput("epi_notes_2"),
                                textOutput("epi_notes_3")
                            ),
                            
                            mainPanel(plotlyOutput("comparison_plot"), width = 6)
                        )
               ),
               
               tabPanel("Data",
                        numericInput("maxrows", "Rows to show", 25),
                        verbatimTextOutput("rawtable"),
                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                        "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                           "Johns Hopkins Center for Systems Science and Engineering.")
               ),
               
               tabPanel("Model and Prediction",

               )
               
    )          
)
