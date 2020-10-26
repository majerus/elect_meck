# precinct boundary data from https://www.ncsbe.gov/results-data/voting-mapsredistricting
# result data from https://er.ncsbe.gov/

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

##LOAD DATA FILES HERE
# list.files(path = "data/results/")

# election years included 
years <- c(2012, 2016)

results2012 <- 
    read_csv(paste0('data/results/', "results_pct_20121106.txt")) %>% 
    clean_names() %>% 
    mutate(precinct = ifelse(precinct=='078.1_', '078.1', precinct)) %>% 
    select(county, precinct, contest, choice, party, 
           election_day, one_stop, absentee_by_mail, provisional, total_votes) %>% 
    filter(county == "MECKLENBURG") %>% 
    select(-county)

results2016 <-
    read_tsv(paste0('data/results/', "results_pct_20161108.txt")) %>%
    clean_names() %>%
    select(county, precinct, contest_name, choice, choice_party,
           election_day, one_stop, absentee_by_mail, provisional, total_votes)  %>%
    rename(party = choice_party,
           contest = contest_name) %>%
    filter(county == "MECKLENBURG") %>%
    select(-county)



shp2012 <- readOGR("data/precincts/SBE_PRECINCTS_09012012/SBE_PRECINCTS_09012012.shp", verbose=FALSE)
# convert from state planar coordinates to lat/lon pairs for mapping
proj4string(shp2012) <- CRS("+init=epsg:2264") # North Carolina
shp2012 <- spTransform(shp2012, CRS("+init=epsg:4326")) #WGS84
# filter precinct data to only meck county
shp2012 <- shp2012[shp2012$COUNTY_NAM == "MECKLENBURG",]


shp2016 <- readOGR("data/precincts/SBE_PRECINCTS_20161004/Precincts.shp", verbose=FALSE)
# convert from state planar coordinates to lat/lon pairs for mapping
proj4string(shp2016) <- CRS("+init=epsg:2264") # North Carolina
shp2016 <- spTransform(shp2016, CRS("+init=epsg:4326")) #WGS84
# filter precinct data to only meck county
shp2016 <- shp2016[shp2016$COUNTY_NAM == "MECKLENBURG",]



# options(shiny.error = function() {
#     stop("An error has occurred")
# })

# tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}")

ui <- dashboardPage(
    
    dashboardHeader(title = "Elect Meck",
                    titleWidth = 300),
    
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            menuItem(text = "Election Analysis", tabName = "Analysis", icon = icon("dashboard")),
            menuItem(text = "Longitudinal Analysis", tabName = "Longitudinal", icon = icon("calendar")),
            menuItem(text = "Data", tabName = "Data", icon = icon("th")),
            
            selectInput(inputId = "file", label = "Select Year", choices = years),
            
            uiOutput('select.contest')
        )
    ),
    
    dashboardBody(
        
        #   '#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac'
        #   
        #   tags$head(tags$style(HTML('
        # 
        #   /* value box */    
        # .small-box.bg-yellow { background-color: #AB9C63 !important; 
        # }
        # 
        #   /* logo */
        #   .skin-blue .main-header .logo {
        #                         background-color: #0C2B6C;
        #                         }
        # 
        #   /* logo when hovered */
        #   .skin-blue .main-header .logo:hover {
        #                         background-color: #AB9C63;
        #                         }
        # 
        #   /* navbar (rest of the header) */
        #   .skin-blue .main-header .navbar {
        #                         background-color: #0C2B6C;
        #                         }        
        # 
        #   /* main sidebar */
        #   .skin-blue .main-sidebar {
        #                         background-color: #0C2B6C;
        #                         }'))),
        
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
                                   title = "Closest Precincts (two top choices)", 
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   collapsible = TRUE,
                                   dataTableOutput("closest_precincts")
                               )
                               
                               # table of five precincts with largest margin of victory
                               
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
            ),
            
            # Longitudinal Tab
            tabItem(tabName = "Longitudinal"),
            
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
            )
        )
    )
    
) # end dashboard page

server <- function(session, input, output) {
    
    df <- reactive({
        ## add file names here for 2020
        switch(input$file,
               "2012" = results2012,
               "2016" = results2016)
    })
    
    shp <- reactive({
        ## add file names here for 2020
        switch(input$file,
               "2012" = shp2012,
               "2016" = shp2016)
    })
    
    
    output$select.contest <- 
        renderUI(expr = selectInput(inputId = 'contest.name',
                                    label = 'Select Contest',
                                    choices = sort(unique(df()$contest)),
                                    selected = "PRESIDENT AND VICE PRESIDENT OF THE UNITED STATES"))
    
    
    
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
    
    
df_map_dt_linked <- reactive({
    
    req(input$file)
    req(input$contest.name)
    
    tmp2 <- 
        df() %>%
        filter(contest == input$contest.name) %>% 
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
        party_pct <- bind_cols(precinct = party_pct$precinct, party_pct[,names(sort(colSums(party_pct[,-1]), decreasing = TRUE))])
        
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


