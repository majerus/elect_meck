# elect_meck shiny web application
options(scipen=999)

# load packages -----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(rgdal)
library(tigris)
library(janitor)
library(scales)
library(DT)
library(ggthemes)
library(plotly)
library(sf)


# Create list of election years included 
## add years here when new data is added
years <- c(2012, 2016, 2020)


# Load election results data ----
# there are some year-to-year differences in column names and formatting so each of these files is cleaned up individually

# 2012 presidential election results
results2012 <- 
    read_csv(paste0('data/results/', "results_pct_20121106.txt")) %>% 
    clean_names() %>% 
    mutate(precinct = ifelse(precinct=='078.1_', '078.1', precinct)) %>% 
    select(county, precinct, contest, choice, party, 
           election_day, one_stop, absentee_by_mail, provisional, total_votes) %>% 
    filter(county == "MECKLENBURG") %>% 
    select(-county)

# 2016 presidential election results
results2016 <-
    read_tsv(paste0('data/results/', "results_pct_20161108.txt")) %>%
    clean_names() %>%
    select(county, precinct, contest_name, choice, choice_party,
           election_day, one_stop, absentee_by_mail, provisional, total_votes)  %>%
    rename(party = choice_party,
           contest = contest_name) %>%
    filter(county == "MECKLENBURG") %>%
    select(-county)

# 2020 presidential election results
results2020 <-
    read_tsv(paste0('data/results/', "results_pct_20201103 2.txt")) %>%
    clean_names() %>%
    select(county, precinct, contest_name, choice, choice_party,
           election_day, one_stop, absentee_by_mail, provisional, total_votes)  %>%
    rename(party = choice_party,
           contest = contest_name) %>%
    filter(county == "MECKLENBURG") %>%
    select(-county)



# create percent democratic by precinct data file
pct_dem_2012 <-
    results2012 %>% 
    rename(total_candidate = total_votes) %>% 
    filter(str_detect(tolower(contest), "president")) %>% 
    mutate(precinct = ifelse(str_detect(tolower(precinct), "absentee|curbside|provisional"), tolower(str_extract(precinct, "[[:alpha:]]*")), precinct)) %>% 
    group_by(precinct, choice, party) %>%
    summarise(total_candidate = sum(total_candidate)) %>% 
    group_by(precinct) %>% 
    mutate(total_all_candidates = sum(total_candidate)) %>% 
    mutate(percent_of_vote = total_candidate/total_all_candidates) %>% 
    filter(party == "DEM") %>% 
    mutate(year = 2012)

pct_dem_2016 <-
    results2016 %>% 
    rename(total_candidate = total_votes) %>% 
    filter(str_detect(tolower(contest), "president")) %>% 
    mutate(precinct = ifelse(str_detect(tolower(precinct), "absentee|curbside|provisional"), tolower(str_extract(precinct, "[[:alpha:]]*")), precinct)) %>% 
    group_by(precinct, choice, party) %>%
    summarise(total_candidate = sum(total_candidate)) %>% 
    group_by(precinct) %>% 
    mutate(total_all_candidates = sum(total_candidate)) %>% 
    mutate(percent_of_vote = total_candidate/total_all_candidates) %>% 
    filter(party == "DEM") %>% 
    mutate(year = 2016)

pct_dem_2020 <-
    results2020 %>% 
    rename(total_candidate = total_votes) %>% 
    filter(str_detect(tolower(contest), "president")) %>% 
    mutate(precinct = ifelse(str_detect(tolower(precinct), "absentee|curbside|provisional"), tolower(str_extract(precinct, "[[:alpha:]]*")), precinct)) %>% 
    group_by(precinct, choice, party) %>%
    summarise(total_candidate = sum(total_candidate)) %>% 
    group_by(precinct) %>% 
    mutate(total_all_candidates = sum(total_candidate)) %>% 
    mutate(percent_of_vote = total_candidate/total_all_candidates) %>% 
    filter(party == "DEM") %>% 
    mutate(year = 2020)


pct_dem <- bind_rows(pct_dem_2012, pct_dem_2016, pct_dem_2020)


p12_16 <- 
    pct_dem %>% 
    select(precinct, percent_of_vote, year) %>% 
    mutate(category = ifelse(str_detect(tolower(precinct), "absentee|curbside|provisional"), "early/other", "polls")) %>%
    spread(year, percent_of_vote) %>% 
    ggplot(aes(text = paste0("Precinct: ", precinct, "<br>", 
                             "2012: ", percent(`2012`, accuracy = 0.1), "<br>", 
                             "2016: ", percent(`2016`, accuracy = 0.1)),
               `2012`, `2016`,
               color = category)) +
    geom_point() +
    theme_tufte() +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    labs(y = "% Democrat 2016", 
         x = "% Democrat 2012") +
    theme(legend.position = "none",
          legend.title = element_text("")) +
    geom_abline(slope = 1, intercept = 0, linetype="dotted") +
    scale_color_manual(values = c("Grey", "#2166ac"))

p12_16 <- ggplotly(p12_16, tooltip = "text")  

p16_20 <- 
    pct_dem %>% 
    select(precinct, percent_of_vote, year) %>% 
    mutate(category = ifelse(str_detect(tolower(precinct), "absentee|curbside|provisional"), "early/other", "polls")) %>%
    spread(year, percent_of_vote) %>% 
    ggplot(aes(text = paste0("Precinct: ", precinct, "<br>", 
                             "2016: ", percent(`2016`, accuracy = 0.1), "<br>", 
                             "2020: ", percent(`2020`, accuracy = 0.1)),
               x = `2016`, 
               y = `2020`,
               color = category)) +
    geom_point() +
    theme_tufte() +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    labs(y = "% Democrat 2020", 
         x = "% Democrat 2016") +
    theme(legend.position = "none",
          legend.title = element_text("")) +
    geom_abline(slope = 1, intercept = 0, linetype="dotted") +
    scale_color_manual(values = c("Grey", "#2166ac"))

p16_20 <- ggplotly(p16_20, tooltip = "text")  



# Load precinct boundary data for each election year ----
# update this to use sf and to load only county level files

# 2012 presidential election
shp2012 <- readOGR("data/precincts/SBE_PRECINCTS_09012012/SBE_PRECINCTS_09012012.shp", verbose=FALSE)
# convert from state planar coordinates to lat/lon pairs for mapping
proj4string(shp2012) <- CRS("+init=epsg:2264") # North Carolina
shp2012 <- spTransform(shp2012, CRS("+init=epsg:4326")) #WGS84
# filter precinct data to only meck county
shp2012 <- shp2012[shp2012$COUNTY_NAM == "MECKLENBURG",]

# 2016 presidential election
shp2016 <- readOGR("data/precincts/SBE_PRECINCTS_20161004/Precincts.shp", verbose=FALSE)
# convert from state planar coordinates to lat/lon pairs for mapping
proj4string(shp2016) <- CRS("+init=epsg:2264") # North Carolina
shp2016 <- spTransform(shp2016, CRS("+init=epsg:4326")) #WGS84
# filter precinct data to only meck county
shp2016 <- shp2016[shp2016$COUNTY_NAM == "MECKLENBURG",]

# 2020 presidential election
shp2020 <- readOGR("data/precincts/SBE_PRECINCTS_20201018/SBE_PRECINCTS_20201018.shp", verbose=FALSE)
# convert from state planar coordinates to lat/lon pairs for mapping
proj4string(shp2020) <- CRS("+init=epsg:2264") # North Carolina
shp2020 <- spTransform(shp2020, CRS("+init=epsg:4326")) #WGS84
# names in data slot are now lower case is 2020, make upper case to match previous years
names(shp2020@data) <- toupper(names(shp2020@data))
# filter precinct data to only meck county
shp2020 <- shp2020[shp2020$COUNTY_NAM == "MECKLENBURG",]

# UI ----
ui <- dashboardPage(
    
    # header 
    dashboardHeader(title = "Elect Meck",
                    titleWidth = 300),
    # sidebar
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            # definte tabs
            menuItem(text = "Election Analysis", tabName = "Analysis", icon = icon("dashboard")),
            menuItem(text = "Year-over-Year", tabName = "Longitudinal", icon = icon("calendar")),
            menuItem(text = "Data", tabName = "Data", icon = icon("th")),
            
            # define inputs
            selectInput(inputId = "file", label = "Select Year", choices = years, selected = max(years)),
            uiOutput('select.contest')
        )
    ),
    
    dashboardBody(
        tabItems(
            # Analysis Tab 
            tabItem(tabName = "Analysis",
                    fluidRow(
                        # map at precinct level
                        column(width = 7,
                               box(width = NULL,
                                   height = 800,
                                   title = "Precinct Map (comparing top two choices)", 
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   leafletOutput("map", height = 725)
                               )),
                        
                        # column 2 
                        column(width = 5, 
                               # table of total vote counts
                               box(width = NULL,
                                   height = 350,
                                   title = "Contest Results", 
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   dataTableOutput("votes_by_candidate_table")
                               ),
                               
                               # table of five precincts with closest margin of victory
                               box(width = NULL,
                                   height = 430,
                                   title = "Closest Precincts (Click row to see on map)", 
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   dataTableOutput("closest_precincts")
                               )
                        ) 
                        
                    ),
                    fluidRow(
                        box(width = 12,
                            title = "Two Top Choices by Precinct", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput('plot'))
                    )
            ), # end analysis tab
            
            # Longitudinal Tab
            tabItem(tabName = "Longitudinal",
                    fluidRow(
                        box(width = 12,
                            title = "2016 to 2020 % Democrat Vote by Precinct", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            p16_20
                        )
                    ),
                    
                    
                    fluidRow(
                        box(width = 12,
                            title = "2012 to 2016 % Democrat Vote by Precinct", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            p12_16
                        )
                    )
                    
                    
                    
            ), # end longitudinal tab
            
            # Data Tab
            tabItem(tabName = "Data",
                    fluidRow(
                        box(width = 12,
                            title = "Selected Data", 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            dataTableOutput("df_table")
                        )
                    )
            ) # end data tab
        ) # end tab items
    ) # end dashboard body
) # end ui

server <- function(session, input, output) {
    
    df <- reactive({
        ## add file names here for 2020
        switch(input$file,
               "2012" = results2012,
               "2016" = results2016,
               "2020" = results2020)
    })
    
    shp <- reactive({
        ## add file names here for 2020
        switch(input$file,
               "2012" = shp2012,
               "2016" = shp2016,
               "2020" = shp2020)
    })
    
    # create dropdown menu of all elections in contest column of selected file
    output$select.contest <- 
        renderUI(expr = selectInput(inputId = 'contest.name',
                                    label = 'Select Contest',
                                    choices = sort(unique(df()$contest)),
                                    selected = unique(df()$contest)[str_detect(tolower(unique(df()$contest)), "president")]
        )
        )
    
    # create table that summarise election contest result by candidate
    output$votes_by_candidate_table <- DT::renderDataTable({
        
        req(input$contest.name)
        req(input$file)
        
        df() %>%
            filter(contest == input$contest.name) %>% 
            mutate(choice = ifelse(!is.na(party), paste0(choice, " (", party, ")"), choice)) %>% 
            group_by(choice) %>% 
            summarise(total = sum(total_votes)) %>% 
            arrange(desc(total)) %>% 
            mutate(percent = total/sum(total)) %>% 
            adorn_totals("row") %>% 
            mutate(total = prettyNum(total, big.mark = ",")) %>% 
            DT::datatable(., 
                          options = list(pageLength = 10, dom = "t"),
                          rownames= FALSE) %>% 
            formatPercentage(c(3), 1)
    })
    
    # create reactive data frame that is used for map and table that are linked together    
    df_map_dt_linked <- reactive({
        
        req(input$file)
        req(input$contest.name)
        
        tmp2 <- 
            df() %>%
            filter(contest == input$contest.name,
                   total_votes > 0 ) %>% 
            mutate(choice = ifelse(!is.na(party), paste0(choice, " (", party, ")"), choice)) %>% 
            group_by(choice, precinct) %>% 
            summarise(total = sum(total_votes)) %>% 
            # select top two vote getters in each precinct
            group_by(precinct) %>% 
            arrange(desc(total)) %>% 
            mutate(percent = total/sum(total)) %>% 
            select(-total) %>% 
            slice(1:2) %>% 
            spread(choice, percent) %>% 
            ungroup() 
        
        if(ncol(tmp2) >= 3){
            
            tmp2 <-
                tmp2 %>% 
                mutate(Difference = abs(.[2] - .[3]))%>% 
                arrange(Difference) %>% 
                select(-Difference)
            
        }
        
        tmp2
        
    })
    
    
    output$closest_precincts <- DT::renderDataTable({
        
        req(input$contest.name)
        req(input$file)
        
        df_map_dt_linked() %>% 
            DT::datatable(., 
                          selection = "single", # only allow one row to be selected 
                          options = list(
                              pageLength = 5),
                          rownames= FALSE) %>% 
            formatPercentage(c(2:ncol(df_map_dt_linked())), digits = 1)
    })
    
    output$df_table = DT::renderDataTable(
        
        
        
        df(),
        filter = 'top',
        options = list(pageLength = 10),
        rownames= FALSE
    ) 
    
    
    # MAP
    map_data <- reactive({    
        
        req(input$contest.name)
        req(input$file)
        
        party_pct <- 
            df() %>% 
            filter(contest == input$contest.name) %>% 
            group_by(precinct) %>% 
            mutate(precinct_total_votes = sum(total_votes),
                   precinct_pct_votes = total_votes/precinct_total_votes,
                   choice = ifelse(!is.na(party), 
                                   paste0(choice, " (", party, ")"),
                                   choice)) %>% 
            select(precinct, choice, precinct_pct_votes) %>% 
            spread(choice, precinct_pct_votes) 
        
        # order columns by total
        party_pct <- bind_cols(precinct = party_pct$precinct, party_pct[,names(sort(colSums(party_pct[,-1], na.rm = TRUE), decreasing = TRUE))])
        
        # limit to top two vote getters and calculate difference
        # if dem and rep, put dem column first
        
        if(ncol(party_pct) > 3){
            party_pct <- party_pct[,c(1:3)]
        }
        
        if(any(str_detect(names(party_pct), "DEM"))){
            party_pct <- party_pct[,
                                   c("precinct",
                                     names(party_pct)[str_detect(names(party_pct), "DEM")],
                                     names(party_pct)[!str_detect(names(party_pct), "DEM|precinct")])
            ]
        }
        
        if(ncol(party_pct) == 3){
            party_pct <-
                party_pct %>% 
                mutate(difference = .[[2]] - .[[3]]) %>% 
                mutate(content = 
                           paste0("<b>Precinct: </b>", precinct, "<br>",
                                  names(.[2]), ": ", percent(.[[2]], accuracy = 0.1), "<br>",
                                  names(.[3]), ": ", percent(.[[3]], accuracy = 0.1)))
            
            
        }else if (ncol(party_pct == 2)) {
            
            party_pct <-
                party_pct %>% 
                mutate(difference = .[[2]] - 0) %>% 
                mutate(content = 
                           paste0("<b>Precinct: </b>", precinct, "<br>",
                                  names(.[2]), ": ", percent(.[[2]])))  
            
            
            
        }
        
        party_pct
    })
    
    
    
    
    legend_title <- reactive({
        
        req(input$file)
        req(input$contest.name)
        
        if(ncol(map_data()) == 4){
            legend_title <- paste0("% ", names(map_data())[2])
        }else{
            legend_title <- paste0("% ", names(map_data())[2], " - ", "<br/>", "% ", names(map_data())[3])
        }
    })
    
    map_df <- reactive({
        
        req(input$file)
        req(input$contest.name)
        
        shp() %>% 
            geo_join(map_data(),  by_sp = "PREC_ID", by_df = "precinct")  
    })
    
    
    output$map <- 
        renderLeaflet({
            
            req(input$contest.name)
            req(input$file)
            
            bins <- c(-1, -.1, -.05, -.01 ,0, .01, .05, .1, 1)
            pal <- colorBin(c('#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac'), 
                            domain = map_data()$difference, bins = bins)
            
            
            map_df() %>% 
                leaflet() %>% 
                addProviderTiles("CartoDB") %>%
                addPolygons(group = "all_precints",
                            weight = 1,
                            fillColor = ~pal(difference),
                            color = "white",
                            dashArray = "3",
                            fillOpacity = .8,
                            popup = ~content,
                            label = ~precinct) %>%
                setView(lng = -80.833145, lat = 35.27231, zoom = 11) %>%
                addLegend(pal = pal, values = ~difference, opacity = .8, title = legend_title())
        })
    
    
    
    
    
    #for some reason the selected row id doesn't print null when deselected when called from an observeEvent
    rows_selected <- reactive({
        input$closest_precincts_rows_selected
    })
    
    
    
    
    
    
    observeEvent(input$closest_precincts_rows_selected, {
        
        selected_precints <- reactive({
            
            req(input$file)
            req(input$contest.name)
            
            subset(map_df(),
                   map_df()@data$PREC_ID == df_map_dt_linked()$precinct[input$closest_precincts_rows_selected])
            
            # map_df()@data$PREC_ID[input$closest_precincts_rows_selected])
            
        })
        
        bins <- c(-1, -.1, -.05, -.01 ,0, .01, .05, .1, 1)
        pal <- colorBin(c('#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac'), 
                        domain = map_data()$difference, bins = bins)
        
        leafletProxy("map", session) %>%
            clearGroup("selected") %>% 
            #clearShapes() %>%
            addPolygons(data = selected_precints(),
                        weight = 4,
                        fillColor = ~pal(difference),
                        color = "black",
                        popup = ~content,
                        #dashArray = "3",
                        fillOpacity = .8,
                        label = ~precinct,
                        group = "selected") 
    })
    
    output$plot <- renderPlotly({
        
        req(input$file)
        req(input$contest.name)
        
        p <-
            ggplot(data = map_data(), 
                   aes(fct_reorder(precinct, -difference), 
                       difference*100, 
                       fill = difference, 
                       text = content)) +
            geom_bar(stat = "identity") +
            scale_fill_gradient2(low = "#b2182b", high = '#2166ac') +
            theme_tufte() +
            labs(y = "Percentage Point Difference", x = "Precinct") +
            theme(legend.position = "none",
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank()) 
        
        ggplotly(p, tooltip = c("text"))
        
    })
    
    
    
    
    
    
}

shinyApp(ui, server)


