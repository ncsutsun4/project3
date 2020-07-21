#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pitchRx)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(knitr)
library(plyr)
library(DT)

data <- pitchRx::pitches




### SHINY SERVER ###

server = function(input, output, session) {
    
    # covid tab 
    output$clean_date_reactive <- renderText({
        format(as.POSIXct(input$plot_date),"%d %B %Y")
    })
    
    reactive_db = reactive({
        cv_cases %>% filter(date == input$plot_date)
        # reactive = cv_cases %>% filter(date == "2020-04-25")
    })
    
    reactive_db_last24h = reactive({
        cv_cases %>% filter(date == input$plot_date & new_cases>0)
    })
    
    reactive_db_large = reactive({
        large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
        large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
        large_countries
    })
    
    reactive_db_large_last24h = reactive({
        large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        large_countries = large_countries[order(large_countries$alpha3),]
        large_countries
    })
    
    reactive_polygons = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
    })
    
    reactive_polygons_last24h = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last24h()$alpha3, ]
    })
    
    output$reactive_case_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
    })
    
    output$reactive_death_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$deaths), big.mark=","), " deaths")
    })
    
    output$reactive_recovered_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recovered")
    })
    
    output$reactive_active_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " active cases")
    })
    
    output$reactive_case_count_China <- renderText({
        paste0("Mainland China: ", prettyNum(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
               prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
    })
    
    output$reactive_case_count_row <- renderText({
        paste0("Other: ", prettyNum(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
               prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
    })
    
    output$reactive_country_count <- renderText({
        paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
    })
    
    output$reactive_new_cases_24h <- renderText({
        paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deathsper100k)) %>%
            # label = sprintf("<strong>%s", reactive_db_large()$country)) %>% #group = "2019-COVID (cumulative)",
            #  label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_large()$country, reactive_db_large()$cases, reactive_db_large()$deaths, reactive_db_large()$recovered, reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
            #  labelOptions = labelOptions(
            #               style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
            #              textsize = "15px", direction = "auto") %>%
            
            addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                             label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g<br/>Deaths per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k, reactive_db_last24h()$newdeathsper100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                             label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g<br/>Deaths per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k, reactive_db()$deathsper100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
                             label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto"))  %>%
            
            addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                             fillOpacity = 0.2, color = sars_col, group = "2003-SARS",
                             label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4), 
                             fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                             label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                             fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                             label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                                 textsize = "15px", direction = "auto"))
    })
    
    output$cumulative_plot <- renderPlot({
        cumulative_plot(cv_aggregated, input$plot_date)
    })
    
    output$epi_curve <- renderPlot({
        new_cases_plot(cv_aggregated, input$plot_date)
    })
    
    # sars tab 
    sars_mod_date = reactive({
        format(as.Date(input$sars_plot_date, format="%d %b %y"), "%Y-%m-%d")
    })
    
    output$sars_clean_date_reactive <- renderText({
        format(as.POSIXct(sars_mod_date()),"%d %B %Y")
    })
    
    sars_reactive_db = reactive({
        sars_cases %>% filter(date == sars_mod_date())
    })
    
    sars_reactive_db_large = reactive({
        large_countries = sars_reactive_db() %>% filter(country!="Singapore" & country!="Diamond Princess Cruise Ship" & country!="Hong Kong" & country!="Macao")
        large_countries = large_countries[order(large_countries$alpha3),]
        large_countries
    })
    
    sars_reactive_polygons = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% sars_reactive_db_large()$alpha3, ]
    })
    
    output$sars_reactive_case_count <- renderText({
        paste0(sum(sars_reactive_db()$cases), " cases")
    })
    
    output$sars_reactive_death_count <- renderText({
        paste0(sum(sars_reactive_db()$deaths), " deaths")
    })
    
    
    output$sars_reactive_country_count <- renderText({
        paste0(length(unique(sars_reactive_db()$country_group)), " countries/territories affected")
    })
    
    output$sars_map <- renderLeaflet({
        sars_basemap
    })
    
    observeEvent(input$sars_plot_date, {
        leafletProxy("sars_map") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            addPolygons(data = sars_reactive_polygons(), stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.1, fillColor = ~sars_pal(sars_reactive_db_large()$per100k), group = "2003-SARS (cumulative)",
                        label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_reactive_db_large()$country, sars_reactive_db_large()$cases, sars_reactive_db_large()$deaths, sars_reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                            textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = sars_reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                             fillOpacity = 0.2, color = sars_col, group = "2003-SARS (cumulative)",
                             label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_reactive_db()$country, sars_reactive_db()$cases, sars_reactive_db()$deaths, sars_reactive_db()$per100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5),
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID",
                             label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", cv_today$country, cv_today$cases, cv_today$deaths, cv_today$recovered, cv_today$per100k) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto"))  %>%
            
            addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4),
                             fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                             label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4),
                             fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                             label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                                 textsize = "15px", direction = "auto")) 
    })
    
    output$sars_cumulative_plot <- renderPlot({
        sars_cumulative_plot(sars_aggregated, sars_mod_date())
    })
    
    output$sars_epi_curve <- renderPlot({
        sars_new_cases_plot(sars_aggregated, sars_mod_date())
    })
    
    # comparison plot
    output$comparison_plot <- renderPlotly({
        comparison_plot(epi_comp, input$comparison_metric)
    })
    
    # add footnote for cases
    output$epi_notes_1 <- renderText({
        if(input$comparison_metric=="cases") { paste0("Note that the axis is on a log10 scale so moves in 10-fold increments.
                                                  The 60.8 million estimated cases of H1N1 dwarf all other outbreaks of plotted on a standard linear scale.") }
    })
    
    # add footnote for deaths
    output$epi_notes_2 <- renderText({
        if(input$comparison_metric=="deaths") { 
            paste0("For H1N1, the number of laboratory-confirmed deaths reported by the WHO is displayed. Subsequent modelling studies have estimated the actual number to be in the range of 123,000 to 203,000.")
        }
    })
    
    # add note for cfr
    output$epi_notes_3 <- renderText({
        if(input$comparison_metric=="cfr") { 
            paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
        }
    })
    
    # update region selections
    observeEvent(input$level_select, {
        if (input$level_select=="Global") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = "Global", selected = "Global")
        }
        
        if (input$level_select=="Continent") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                              selected = c("Africa", "Asia", "Europe", "North America", "South America"))
        }
        
        if (input$level_select=="US state") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_states_today[order(-cv_states_today$cases),]$state), 
                              selected = cv_states_today$state)
        }
        
        if (input$level_select=="Country") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                              selected = cv_today_reduced$country)
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db = reactive({
        if (input$level_select=="Global") { 
            db = cv_cases_global
            db$region = db$global_level
        }
        if (input$level_select=="Continent") { 
            db = cv_cases_continent 
            db$region = db$continent
        }
        if (input$level_select=="Country") { 
            db = cv_cases
            db$region = db$country
        }
        if (input$level_select=="US state") { 
            db = cv_states
            db$region = db$state
        }
        
        if (input$outcome_select=="Cases (total)") { 
            db$outcome = db$cases
            db$new_outcome = db$new_cases
        }
        
        if (input$outcome_select=="Deaths (total)") { 
            db$outcome = db$deaths 
            db$new_outcome = db$new_deaths 
        }
        
        if (input$outcome_select=="Cases per 100,000") { 
            db$outcome = db$per100k 
            db$new_outcome = db$newper100k 
        }
        
        if (input$outcome_select=="Deaths per 100,000") { 
            db$outcome = db$deathsper100k 
            db$new_outcome = db$newdeathsper100k 
        }
        
        db %>% filter(region %in% input$region_select)
    })
    
    # country-specific plots
    output$country_plot <- renderPlotly({
        country_cases_plot(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative <- renderPlotly({
        country_cases_cumulative(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative_log <- renderPlotly({
        country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("COVID_data_", cv_today$date[1], ".csv", sep="")
        },
        content = function(file) {
            write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                            recovered, new_recovered, active_cases, 
                                            per100k, newper100k, activeper100k, deathsper100k, newdeathsper100k)), file)
        }
    )
    
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                         recovered, new_recovered, active_cases, 
                                         per100k, newper100k, activeper100k, deathsper100k, newdeathsper100k)), input$maxrows), row.names = FALSE)
        options(orig)
    })
    
}