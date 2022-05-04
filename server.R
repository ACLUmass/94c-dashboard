library(tidyverse)
library(shiny)
library(plotly)
# library(fst)
library(data.table)
library(leaflet)
library(lubridate)
library(scales)
# library(shinyjs)
library(sf)
# library(DBI)
# library(RSQLite)
# library(pool)

# Load data with fread
charge_cats_df <- read_csv("data/94c_charge_codes.csv")
disp_cats_df <- read_csv("data/94c_dispositions.csv")
disp_colors <- readRDS("data/disp_colors.rds")
ma_towns <- read_rds("data/ma_towns.rds")
DAs <- read_csv("data/DAs.csv")

combined94c_data <- fread("94c_combined.csv") %>%
  merge(charge_cats_df, by="charge") %>%
  merge(disp_cats_df, by="disposition")

mass_cntys <- tigris::counties(state=25, cb=T)
mass_DA_dists <- mass_cntys %>%
  mutate(district = case_when(
    NAME %in% c("Hampshire", "Franklin") ~ "Northwestern",
    NAME %in% c("Dukes", "Barnstable", "Nantucket") ~ "Cape and Islands",
    T ~ NAME
  )) %>%
  group_by(district) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# Define list of counties
all_DAs <- c("Berkshire", "Bristol", "Cape and Islands", "Essex", 
             "Hampden", "Middlesex", "Norfolk", "Northwestern",
             "Plymouth", "Suffolk", "Worcester")

# Further condense disposition categories
combined94c_data <- combined94c_data %>% 
  mutate(disposition_cat = case_when(
    disposition_cat %in% c("CWOF", "Dismissed (Lab Misconduct)", "Dismissed", "Guilty", "Not Guilty", "Nolle Prosequi", "Filed Without Sentence", "Delinquent (Juvenile)", "Not Delinquent (Juvenile)", "Not Listed") ~ disposition_cat,
    T ~ "Other"
  ),
  disposition_cat = factor(disposition_cat, 
                           levels=c("Dismissed", "Dismissed (Lab Misconduct)",
                                    "Guilty", "Not Guilty", "CWOF", 
                                    "Nolle Prosequi", "Filed Without Sentence", 
                                    "Delinquent (Juvenile)", 
                                    "Not Delinquent (Juvenile)", 
                                    "Not Listed", "Other")))


all_courts <- combined94c_data[order(court), court] %>%
  unique()

# Rename courts to be more accessible
court_names <- data.frame(all_courts) %>%
  rename(court = all_courts) %>%
  mutate(court = as.character(court),
         court_name= case_when(
           str_detect(court, "BMC") ~ paste0("Boston Municipal Court (", str_remove(court, "BMC "), ")"),
           str_detect(court, 'Court$', negate=T) ~ paste(str_remove(court, " Criminal"), "Superior Court"), # I did check this was true
           T ~ court
         )) %>%
  pull(court_name)

names(all_courts) <- court_names

function(input, output, session) {

    # Helper functions - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    # Empty plotly
    empty_plotly <- function(label) {
        plot_ly() %>%
            layout(yaxis = list(zeroline = F, showticklabels = F),
                   xaxis = list(zeroline = F, showticklabels = F),
                   font=list(family = "GT America"),
                   hoverlabel=list(font = list(family = "GT America")),
                   annotations = list(list(
                       showarrow = F,
                       x = .5, xref="paper", xanchor = "center",
                       y = .5, yref="paper", yanchor = "center",
                       text = paste("<i>No", label, "for selected filters.</i>"),
                       opacity = 0.7
                   ))
            )
    }
    
    # Filter data and get descriptive sentence
    filter_and_get_label <- function(data, town = "All cities and towns",
                                     agency = "All departments",
                                     crt = "All courts",
                                     chrg = "All charges",
                                     disp = "All dispositions",
                                     year_type=NA,
                                     start_year=NA,
                                     end_year=NA) {
      
      town_str <- "in Massachusetts"
      dept_str <- ""
      court_str <- ""
      disp_str <- ""
      year_str <- ""
      charge_str <- ""
      
      if(town != "All cities and towns") {
        data <- data[jurisdiction == town]
        town_str <- paste("in", town)
      }
      
      if(agency != "All departments") {
        data <- data[department == agency]
        dept_str <- paste("by the", agency, "Police Department")
      }
      
      if(crt != "All courts") {
        cat("court:", crt, "\n")
        data <- data[court == crt]
        i_court_name = match(crt, all_courts)
        court_str <- paste("and heard by the", names(all_courts)[i_court_name])
      }
      
      if(chrg != "All charges") {
        cat("charge:", chrg, "\n")
        data <- data[charge_cat == chrg]
        charge_str <- chrg
      }
      
      if(disp != "All dispositions") {
        data <- data[disposition_cat == disp]
        disp_str <- paste("disposed as", disp)
      }
      
      if (!is.na(year_type)) {
        if (year_type == "Offense") {
          data <- data[offense_year >= start_year & offense_year <= end_year]
          verb <- "allegedly committed"
        } else if (year_type == "Filing") {
          data <- data[file_year >= start_year & file_year <= end_year]
          verb <- "filed"
        } else if (year_type == "Arrest") {
          data <- data[arrest_year >= start_year & arrest_year <= end_year]
          verb <- "resulting in arrest"
        } else if (year_type == "Disposition") {
          data <- data[disposition_year >= start_year & disposition_year <= end_year]
          verb <- "disposed"
        }
        year_str <- paste(verb, "between", start_year, "and", end_year)
      }
      
      label <- paste("drug-related", tolower(charge_str), "charges", year_str,
                     town_str, dept_str, court_str, disp_str)
      label <- str_squish(label)
      
      list(data, label)
      
    }
    
    # Determine the year bounds
    get_year_bounds <- function(year_type, start_year, end_year) {
        if (year_type == "Arrest") {
          yr_max <- combined94c_data %>% pull(arrest_year) %>% max(na.rm = TRUE)
          yr_min <- combined94c_data %>% pull(arrest_year) %>% min(na.rm = TRUE)
        } else if (year_type == "Disposition") {
          yr_max <- combined94c_data %>% pull(disposition_year) %>% max(na.rm = TRUE)
          yr_min <- combined94c_data %>% pull(disposition_year) %>% min(na.rm = TRUE)
        } else if (year_type == "Filing") {
          yr_max <- combined94c_data %>% pull(file_year) %>% max(na.rm = TRUE)
          yr_min <- combined94c_data %>% pull(file_year) %>% min(na.rm = TRUE)
        } else if (year_type == "Offense") {
          yr_max <- combined94c_data %>% pull(offense_year) %>% max(na.rm = TRUE)
          yr_min <- combined94c_data %>% pull(offense_year) %>% min(na.rm = TRUE)
        }
        
        start_value <- start_year
        if (start_year < yr_min) {
          start_value <- yr_min
        }
        
        end_value <- end_year
        if (end_year > yr_max) {
          end_value <- yr_max
        }
        
        list(yr_min, yr_max, start_value, end_value)
    }
#     
#     build_query <- function(start_date="2002-01-01", 
#                             end_date="2021-02-04", 
#                             town="All cities and towns", 
#                             agency="All agencies", 
#                             officer="All officers", 
#                             outcome="All outcomes", col="*", group="") {
#         
#         query <- paste("SELECT", col, "FROM `statewide_2002_21` WHERE")
#         and_needed <- F
#         
#         if (start_date != "2002-01-01" | end_date != "2021-02-04") {
#             query <- paste0(query, " `date` BETWEEN ", as.numeric(as_date(start_date)), 
#                                 " AND ", as.numeric(as_date(end_date)))
#             and_needed <- T
#         }
#         
#         if (town != "All cities and towns") {
#             
#             query <- paste0(query, ifelse(and_needed, " AND", ""), " `loc` = '", town, "'")
#             and_needed <- T
#         }
#         
#         if (agency != "All agencies") {
#             query <- paste0(query, ifelse(and_needed, " AND", ""), 
#                            " `agency` = '", agency, "'")
#             and_needed <- T
#             
#             if (officer != "All officers" & officer != "") {
#                 query <- paste0(query, " AND `officer` = '", officer, "'")
#             }
#         }
#         
#         if (outcome != "All outcomes") {
#             
#             query <- paste0(query, ifelse(and_needed, " AND", ""), " `type` = '", 
#                             outcome, "'")
#         }
#         
#         if (str_ends(query, "WHERE")) {
#             cat("querying entire database?? wuh-oh\n")
#         }
#         
#         query <- paste(query, 
#                        ifelse(group=="", "", paste("GROUP BY", group)))
#         
#         cat(query, "\n")
#         
#         return(query)
#         
#     }
#     
#     # Download data subset ------------------------------------------------------------
#     
#     # Link to this page from landing page
#     observeEvent(input$link_to_download, {
#         updateTabsetPanel(session, "panels", "Download the Data")
#     })
#     
#     # Update the list of officers based on the selected agency
#     observeEvent(input$download_agency, {
#         
#         if (input$download_agency == 'All agencies') {
#             updateSelectizeInput(session, "download_officer",
#                                  choices = c("Please select agency"=""), server=T)
#         } else {
#             
#             selected_officers <- officers_per_agency[agency == input$download_agency, 
#                                                      list(officer)]
#     
#             updateSelectizeInput(session, "download_officer",
#                                  choices = c("All officers", selected_officers), server=T)
#         }
#     })
#     
#     download_values <- reactiveValues(agency = NULL)
#     
#     observeEvent(input$download_filters, {
#         download_values$officer <-      input$download_officer
#         download_values$town <-         input$download_town
#         download_values$agency <-       input$download_agency
#         download_values$start_date <-   input$download_start_date
#         download_values$end_date <-     input$download_end_date
#         
#     })
#     
#     output$download_size <- renderText({
#         validate(
#             need(download_values$agency, 'Please select filters and press "Go" to view estimated download size.')
#         )
#         
#         if (download_values$town == "All cities and towns" & 
#             download_values$agency == "All agencies"  &
#             (download_values$officer != "All officers" |
#              download_values$officer != "") &
#             download_values$end_date - download_values$start_date > years(2)) {
#             
#             cat("no!!!\n")
#             disable("download_button")
#             download_values$filtered <- F
#             
#             
#             validate(need(download_values$filtered, "For a manageable download, please either select a single town, agency, or officer ID; or restrict the date range to less than two years."))
#             
#         } else {
#             
#             download_values$filtered <- T
#             cat("applying data filters\n")
#             
#             q <- build_query(start_date = download_values$start_date, 
#                              end_date = download_values$end_date, 
#                              town = download_values$town, 
#                              agency = download_values$agency, 
#                              officer = download_values$officer)
#             
#             download_values$data <- 
#                 dbGetQuery(sqldb, q) %>%
#                 mutate(date = as_date(date))
#             
#             validate(
#                 need(nrow(download_values$data) > 0, 
#                      "No data for selected filters. Please try a different selection.")
#             )
#             
#             enable("download_button")
#         }
#         
#         object.size(download_values$data) %>% 
#             format(units="auto", standard="SI") %>%
#             paste("The dataset with the applied filters is estimated to be", .)
#     })
#     
#     output$download_button <- downloadHandler(
#         filename = "MassDOT_stops.csv",
#         content = function(file) {
#             cat("Download commencing!!\n")
#             withProgress(message = 'Downloading...', value = 1, {
#                 write_csv(download_values$data, file)
#             })
#         }
#     )
#     
    # District Attorneys ----------------------------------------------------------
    DA_values <- reactiveValues(done = NULL)
    
    DA_data <- combined94c_data %>%
      mutate(county = case_when(
        county %in% c("Hampshire", "Franklin") ~ "Northwestern",
        county %in% c("Dukes", "Barnstable", "Nantucket") ~ "Cape and Islands",
        T ~ county
      ))
    
    # Plot the county 
    output$DA_map <- renderLeaflet({
      map_data <- mass_DA_dists %>%
        mutate(is_our_county = district == input$DA_county)
      
      pal <- colorNumeric(
        palette = c("grey", "#00343a"),
        domain = 0:1,
        na.color = "#bbbbbb"#viridis_pal(option="inferno")(10) %>% head(1)
      )
      
      leaflet(options = leafletOptions(attributionControl = T)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(data = map_data,
                    fillOpacity = 0.7,
                    weight = 1,
                    fillColor = ~pal(is_our_county),
                    color="grey",
                    stroke=T,
                    smoothFactor=.5,
                    label = ~lapply(paste0("<b>", district, " District</b></br>"),
                                    htmltools::HTML),
                    group="poly")  %>%
        addEasyButton(easyButton(
          icon="fa-home", title="Reset",
          onClick=JS("function(btn, map){
                   var groupLayer = map.layerManager.getLayerGroup('poly');
                   map.fitBounds(groupLayer.getBounds());
               }"))) %>%
        addControl("<img src='Logo_White_CMYK_Massachusetts.png' style='max-width:100px;'>",
                   "bottomleft", className="logo-control")
    })

    output$DA_dashboard <- renderUI({
      
        DA_values$DA_cty <- input$DA_county
        DA_values$start_year <- input$DA_start_year
        DA_values$end_year <- input$DA_end_year
        
        DA_data <- DA_data[county == DA_values$DA_cty & 
                             file_year <= DA_values$end_year & 
                             file_year >= DA_values$start_year]
        
        DA_names <- DAs %>%
          filter(District == DA_values$DA_cty) %>%
          filter((Left_num >= input$DA_end_year & Elected <= input$DA_end_year) |
                   (Elected <= input$DA_start_year & Left_num >= input$DA_start_year) |
                   (Elected >= input$DA_start_year & Left_num <= input$DA_end_year)) %>%
          arrange(Left_num) %>%
          mutate(string = paste0(Name, " (", Elected, "-", Left, ")")) %>%
          pull(string)
        
        n_DAs <- length(DA_names)
        DA_name_str <- paste(DA_names, collapse = ", ")
          
        # Calculate total stops
        DA_values$total_charges <- DA_data %>%
            count(name="N") %>%
            pull(1)
        
        scandal_charges <- DA_data %>%
          mutate(in_scandal = disposition_cat == "Dismissed (Lab Misconduct)") %>%
          filter(in_scandal) %>%
          count(name="N") %>%
          pull(1)
        
        # Percent implicated in scandal
        output$DA_pie <- renderPlotly({
          
          DA_data %>%
            mutate(in_scandal = disposition_cat == "Dismissed (Lab Misconduct)") %>%
            count(in_scandal, name="N") %>%
            arrange(in_scandal) %>%
            plot_ly(sort=F,
                    direction = "clockwise",
                    marker = list(line = list(color = 'lightgrey', width = 1),
                                  colors=c("black", "#a7d7b5")),
                    labels = ~in_scandal, values = ~N,
                    textposition = "inside") %>%
            add_pie(hovertemplate = '%{value} charges (%{percent})<extra></extra>') %>%
            layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   showlegend = F,
                   margin = list(l = 20, r = 20),
                   font=list(family = "GT America"),
                   hoverlabel=list(font = list(family = "GT America")),
                   legend = list(x = 100, y = 0.5))
        })

        # Top agencies
        output$DA_top_agencies <- renderTable({

            DA_data %>%
                filter(!is.na(department)) %>%
                count(department, name="N") %>%
                slice_max(N, n=10) %>%
                mutate(Rank = min_rank(-N),
                       N = number(N, big.mark=",", accuracy=1)) %>%
                head(10) %>%
                select(Rank, `Agency` = department, `Charges filed`=N)
        })

        # Top courts
        output$DA_top_courts <- renderTable({

            DA_data %>%
                filter(!is.na(court)) %>%
                mutate(court_name = names(all_courts[match(court, all_courts)])) %>%
                count(court_name, name="N") %>%
                slice_max(N, n=10) %>%
                mutate(Rank = min_rank(-N),
                       N = number(N, big.mark=",", accuracy=1)) %>%
                head(10) %>%
                select(Rank, `Court` = court_name, `Number of Charges`=N)
        })

        # Top charges
        output$DA_top_charges <- renderTable({

            DA_data %>%
                count(charge_cat, name="N") %>%
                slice_max(N, n=10) %>%
                mutate(N = number(N, big.mark=",", accuracy=1)) %>%
                rename(`Number of Charges` = N, `Charge Type` = charge_cat)

        })

        tagList(
          div(id="DA_numbers",
            h1(number(DA_values$total_charges, big.mark=","),
               style="text-align: center;  display: inline;"),
            p(em("94C charges filed in", DA_values$DA_cty, "District", br(), "between",
                 input$DA_start_year, "and",
                 input$DA_end_year, style="text-align: center; display: inline;")), 
                 hr(style="border: grey solid .5px;margin-top: 0rem; margin-bottom: 1rem;"),
                 em(paste0("District Attorney", if_else(n_DAs > 1, "s: ", ": "), DA_name_str),
              style="text-align: center; display: inline-block; margin-bottom: 1rem;")),
            splitLayout(id="dashboard_split",
                        div(h3(number(scandal_charges, big.mark=","), style="margin:0px;"), 
                               h4("charges dismissed due to"), br(), h4(paste("lab scandal in", 
                                 DA_values$DA_cty, "county")),
                        plotlyOutput("DA_pie", height="300px")),
                div(h4("Top Agencies"),
                    tableOutput("DA_top_agencies"))
            ),
            splitLayout(id="dashboard_split",
                div(h4("Top Courts"),
                    tableOutput("DA_top_courts")),
                div(h4("Most Common Charges"),
                    tableOutput("DA_top_charges")
            ))
        )
    })

    # Mapping stops -------------------------------------------------------------------

    charges_map <- reactiveValues(data=NULL)
    
    # Update year ranges based on year type
    observe({
      validate(
        need(is.numeric(input$map_start_year), 'Please enter a valid year.'),
        need(is.numeric(input$map_end_year), 'Please enter a valid year.')
      )
      
      bounds <- get_year_bounds(input$map_yr_type, input$map_start_year, input$map_end_year)
      yr_min <- bounds[[1]]
      yr_max <- bounds[[2]]
      start_value <- bounds[[3]]
      end_value <- bounds[[4]]
      
      updateNumericInput(session,
                         "map_start_year", value=start_value,
                         max=yr_max, min=yr_min)
      updateNumericInput(session,
                         "map_end_year", value=end_value,
                         max=yr_max, min=yr_min)
      
    })

    observeEvent(input$map_button, {
      
      results <- filter_and_get_label(
        combined94c_data, 
        agency = input$map_dept,
        crt = input$map_court,
        chrg = input$map_charge,
        year_type = input$map_yr_type,
        start_year=input$map_start_year,
        end_year=input$map_end_year)
      
      data <- results[[1]]
      # label <- results[[2]]

        charges_sf <- data[, .N, jurisdiction] %>%
            mutate(jurisdiction = case_when(
              jurisdiction == "North Attleboro" ~ "N Attleborough",
              str_starts(jurisdiction, "East ") ~ str_replace(jurisdiction, "East ", "E "),
              str_starts(jurisdiction, "West ") ~ str_replace(jurisdiction, "West ", "W "),
              str_starts(jurisdiction, "North ") ~ str_replace(jurisdiction, "North ", "N "),
              str_starts(jurisdiction, "South ") ~ str_replace(jurisdiction, "South ", "S "),
              str_starts(jurisdiction, "Mount ") ~ str_replace(jurisdiction, "Mount ", "Mt "),
              str_starts(jurisdiction, "Great ") ~ str_replace(jurisdiction, "Great ", "Gt "),
              jurisdiction == "Lunenberg" ~ "Lunenburg",
              jurisdiction == "Manchester" ~ "Manchester-By-The-Sea",
              T ~ jurisdiction
            )) %>%
            merge(ma_towns, by.y = "town", by.x = "jurisdiction", all=T) %>%
            filter(!is.na(pop)) %>%
            mutate(N= replace_na(N, 0)) %>% # Fill in 0s for towns with no stops
            select(-TOWN, town=jurisdiction) %>%
            st_as_sf() %>%
            st_transform('+proj=longlat +datum=WGS84')
        
        if (input$map_radio == "Charges per capita") {
          charges_sf <- charges_sf %>%
                mutate(N = (N / pop) * 1e3)
        }

        charges_map$data <- charges_sf
        charges_map$log <- input$map_log
        charges_map$percap <- input$map_radio
    })

    # Replace legend labels with custom format
    stopsLabelFormat = function(..., log){
        if (log) {
            function(type = "numeric", cuts){
                10**as.numeric(cuts) %>%
                    scales::number(big.mark=",", accuracy=1)
            }
        } else {
            labelFormat(...)
        }
    }

    output$charges_by_town <- renderLeaflet({
        validate(
            need(charges_map$data, 'Please select date range and press "Go."')
        )

        palette_domain <- if (charges_map$log) log10(charges_map$data$N) else charges_map$data$N
        palette_domain <- replace(palette_domain,
                                  palette_domain == -Inf, NA)

        if (charges_map$percap == "Total charges") {
            legend_title <- "<a style='font-family:GT America; color: dimgrey'>Total <br> 94C charges</a>"
            label_accuracy <- 1
            label_suffix <- ""
        } else {
            legend_title <- "<a style='font-family:GT America; color: dimgrey'>94C charges<br>per 1,000</a>"
            label_accuracy <- 1
            label_suffix <- " per 1,000 population"
        }

        pal_total_stops <- colorNumeric(
            palette = "inferno",
            domain = palette_domain,
            na.color = "#bbbbbb"#viridis_pal(option="inferno")(10) %>% head(1)
        )
        
        pal_one_town <- colorNumeric(
          palette = c("#bbbbbb", "red"),
          domain = c(0, max(palette_domain, na.rm=T)),
          na.color = viridis_pal(option="inferno")(10) %>% tail(1)
        )
        
        pal_total_stops_noNA <- colorNumeric(
            palette = "inferno",
            domain = palette_domain,
            na.color = NA
        )
        
        one_town <- charges_map$data %>%
          filter(N != 0) %>%
          nrow() == 1
          
        leaflet(options = leafletOptions(attributionControl = T)) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            addPolygons(data = charges_map$data,
                        fillOpacity = 0.8,
                        weight = 1,
                        fillColor = if (one_town & charges_map$log) {
                          ~pal_one_town(N)
                        } else if (charges_map$log) {
                          ~pal_total_stops(log10(N))
                        } else {
                          ~pal_total_stops(N)
                        },
                        stroke=F,
                        smoothFactor=.5,
                        label = ~lapply(paste0("<b>", town, "</b></br>",
                                               scales::number(N, big.mark=",", accuracy=label_accuracy), " charges", label_suffix),
                                        htmltools::HTML),
                        color="none",
                        group="poly")  %>%
            addLegend(pal = pal_total_stops_noNA,
                      values = palette_domain,
                      labFormat = stopsLabelFormat(log=charges_map$log),
                      position = "topright",
                      title = legend_title,
            )  %>%
            addEasyButton(easyButton(
                icon="fa-home", title="Reset",
                onClick=JS("function(btn, map){
                   var groupLayer = map.layerManager.getLayerGroup('poly');
                   map.fitBounds(groupLayer.getBounds());
               }"))) %>%
            addControl("<img src='Logo_White_CMYK_Massachusetts.png' style='max-width:100px;'>",
                       "bottomleft", className="logo-control")
    })
     
    # Disposition -----------------------------------------------------------------
    
    disp_values <- reactiveValues(agency = NULL)
    
    # Update year ranges based on year type
    observe({
      validate(
        need(is.numeric(input$disp_start_year), 'Please enter a valid year.'),
        need(is.numeric(input$disp_end_year), 'Please enter a valid year.')
        )
      
      bounds <- get_year_bounds(input$disp_yr_type, 
                                input$disp_start_year, 
                                input$disp_end_year)
      yr_min <- bounds[[1]]
      yr_max <- bounds[[2]]
      start_value <- bounds[[3]]
      end_value <- bounds[[4]]
      
      updateNumericInput(session,
                         "disp_start_year", value=start_value,
                         max=yr_max, min=yr_min)
      updateNumericInput(session,
                         "disp_end_year", value=end_value,
                         max=yr_max, min=yr_min)
      
    })
    
    # Calculate all the values for disposition plot
    observeEvent(input$disp_button, {

        disp_values$town <- input$disp_city
        disp_values$agency <- input$disp_dept
        disp_values$court <- input$disp_court
        disp_values$charge <- input$disp_charge
        disp_values$disp_yr_type <- input$disp_yr_type
        disp_values$start_yr <- input$disp_start_year
        disp_values$end_yr <- input$disp_end_year

        results <- filter_and_get_label(
          combined94c_data, town = disp_values$town,
          agency = disp_values$agency,
          crt = disp_values$court,
          chrg = disp_values$charge,
          year_type = disp_values$disp_yr_type,
          start_year=disp_values$start_yr,
          end_year=disp_values$end_yr)
        
        data <- results[[1]]
        disp_values$data <- data[, .N, disposition_cat]
        
        label <- results[[2]]
        output$disp_str <- renderText(label)
    })

    # Create disposition plot
    output$disposition <- renderPlotly({
      
        validate(
            need(disp_values$data, 'Please select filters and press "Go."')
        )

        data <- disp_values$data %>%
          arrange(disposition_cat)
        
        output$disp_count_str <- renderText(data %>% pull(N) %>%sum() %>% scales::comma())
        
        disp_colors_here <- disp_colors[data$disposition_cat]
        disp_colors_here

        if (nrow(data) > 0) {
          
          data %>%
          plot_ly(sort=F,
                  direction = "clockwise",
                  marker = list(line = list(color = 'lightgrey', width = 1),
                                colors=disp_colors_here),
                  labels = ~disposition_cat, values = ~N,
                  textposition = "inside"
                  ) %>%
            add_pie(hovertemplate = '<i>Disposition</i>: %{label}<br>%{value} charges (%{percent})<extra></extra>') %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     showlegend = TRUE,
                     font=list(family = "GT America"),
                     hoverlabel=list(font = list(family = "GT America")),
                     legend = list(x = 100, y = 0.5))
          
        } else {
            # If there are no stops for the filter
            empty_plotly("charges")
        }

    })
    
    # Stops over time -----------------------------------------------------------------
    
    time_values <- reactiveValues(agency = NULL)
    
    observeEvent(input$time_button, {
      
      time_values$town <- input$time_city
      time_values$agency <- input$time_dept
      time_values$court <- input$time_court
      time_values$charge <- input$time_charge
      time_values$disp <- input$time_disp
      
      time_values$compare <- input$compare_time
      
      time_values$town2 <- input$time_city2
      time_values$agency2 <- input$time_dept2
      time_values$court2 <- input$time_court2
      time_values$charge2 <- input$time_charge2
      time_values$disp2 <- input$time_disp2
      
      results <- filter_and_get_label(
        combined94c_data, 
        town = time_values$town,
        agency = time_values$agency,
        crt = time_values$court,
        chrg = time_values$charge,
        disp = time_values$disp)
      
      data <- results[[1]]
      time_values$data <- data
      
      time_values$label <- results[[2]]
      
      if (time_values$compare) {
          
        results <- filter_and_get_label(
          combined94c_data, 
          town = time_values$town2,
          agency = time_values$agency2,
          crt = time_values$court2,
          chrg = time_values$charge2,
          disp = time_values$disp2)
        
        data <- results[[1]]
        time_values$data2 <- data#[, .N, disposition_cat]
        
        time_values$label2 <- results[[2]]
        
      } else {
        time_values$data2 <- NULL
      }
    })
    
    output$stops_v_time <- renderPlotly({
      
      validate(
        need(time_values$agency, 'Please select filters and press "Go."')
      )
      
      data <- time_values$data
      data2 <- if (time_values$compare) time_values$data2 else NULL
      
      if (input$year_type == "Arrest") {
        data <- data[, .N, .(x=arrest_year)] %>% arrange(x)
        data2 <- if (time_values$compare) data2[, .N, .(x=arrest_year)] %>% arrange(x) else NULL
        
      } else if (input$year_type == "Disposition") {
        data <- data[, .N, .(x=disposition_year)] %>% arrange(x)
        data2 <- if (time_values$compare) data2[, .N, .(x=disposition_year)] %>% arrange(x) else NULL
        
      } else if (input$year_type == "Filing") {
        data <- data[, .N, .(x=file_year)] %>% arrange(x)
        data2 <- if (time_values$compare) data2[, .N, .(x=file_year)] %>% arrange(x) else NULL
        
      } else if (input$year_type == "Offense") {
        data <- data[, .N, .(x=offense_year)] %>% arrange(x)
        data2 <- if (time_values$compare) data2[, .N, .(x=offense_year)] %>% arrange(x) else NULL
      }
      
      if (time_values$compare == F & nrow(data) > 0) {
        
        data %>%
          plot_ly(hovertemplate = '%{y:,} charges in %{x}<extra></extra>',
                  line = list(color = '#3c3532', width=3.5)) %>% 
          add_lines(x=~x, y=~N)%>%
          layout(yaxis = list(title = "Number of charges", zeroline = F),
                 xaxis = list(title = paste("Year of", input$year_type), zeroline = F),
                 font=list(family = "GT America"),
                 hoverlabel=list(font = list(family = "GT America")),
                 annotations = list(list(
                   showarrow = F, opacity = 0.7,
                   x = .5, xref="paper", xanchor = "center",
                   y = 1.05, yref="paper",
                   text = "<i>Click and drag to zoom in on a specific date range</i>"
                 )))
        
      } else if (time_values$compare == T) {
        
        if (nrow(data) > 0 | nrow(data2) > 0) {
          
          plot_ly() %>% 
            add_lines(data=data, x=~x, y=~N, name=time_values$label, opacity=.7,
                      line = list(color = '#3c3532', width=3.5),
                      hovertemplate = '%{y:,} charges in %{x}<extra></extra>')%>%
            add_lines(data=data2, x=~x, y=~N,name=time_values$label2, opacity=.7,
                      line = list(color = "#ef404d", width=3.5),
                      hovertemplate = '%{y:,} charges in %{x}<extra></extra>') %>%
            add_annotations(showarrow = F, opacity = 0.7,
                            x = .5, xref="paper", xanchor = "center",
                            y = 1.05, yref="paper",
                            text = "<i>Click and drag to zoom in on a specific date range</i>") %>%
            layout(yaxis = list(title = "Number of charges", zeroline = F),
                   xaxis = list(title = paste("Year of", input$year_type), zeroline = F),
                   font=list(family = "GT America"),
                   hoverlabel=list(font = list(family = "GT America")),
                   legend = list(x = 0.5, y=-.4,
                                 xanchor="center",
                                 bgcolor = alpha('lightgray', 0.4)))
        } else {
          empty_plotly("charges")
        }
      
      } else {
        # If there are no stops for the filter
        empty_plotly("charges")
      }
      
    })

#     # Stops by offense ----------------------------------------------------------------
#     
#     # Update officer list when agency is selected
#     observeEvent(input$offense_agency, {
#         
#         if (input$offense_agency != "All agencies") {
#             selected_officers <- officers_per_agency[agency == input$offense_agency, 
#                                                      list(officer)]
#             
#             updateSelectizeInput(session, "offense_officer",
#                                  choices = c("All officers", selected_officers), server=T)
#         } else {
#             updateSelectizeInput(session, "offense_officer",
#                                  choices = c("Please select agency" = ""))
#         }
#     })
#     
#     offense_values <- reactiveValues(town = NULL)
#     
#     observeEvent(input$offense_button, {
#         offense_values$town <- input$offense_town
#         offense_values$agency <- input$offense_agency
#         offense_values$officer <- input$offense_officer
#         offense_values$start_date <- input$offense_start_date
#         offense_values$end_date <- input$offense_end_date
#     })
#     
#     output$offenses <- renderPlotly({
#         validate(
#             need(offense_values$town, 'Please select filters and press "Go."')
#         )
#         
#         if (offense_values$town == "All cities and towns" &
#             offense_values$agency == "All agencies") {
#             
#             data <- all_offenses[date >= offense_values$start_date &
#                                      date <= offense_values$end_date] %>%
#                 group_by(group) %>%
#                 summarize(n = sum(n)) %>%
#                 arrange(group == "Other", desc(n))
#             
#         } else {
#             
#             q <- build_query(start_date = offense_values$start_date,
#                              end_date = offense_values$end_date,
#                              town = offense_values$town, 
#                              agency = offense_values$agency, 
#                              officer = offense_values$officer, 
#                              col = "offense")
#             
#             data <- dbGetQuery(sqldb, q) %>%
#                 merge(violations, by="offense", all.x=T) %>%
#                 count(group) %>%
#                 arrange(group == "Other", desc(n))
#         }
#         
#         offense_colors <- named_colors[data$group]
#         
#         if (nrow(data) > 0) {
#             
#             annotation <- get_legend_name(offense_values$town, offense_values$agency, 
#                                           offense_values$officer, "Offenses from stops",
#                                           pie_label=T)
#         
#             data %>%
#                 plot_ly(sort=F,direction = "clockwise",
#                         hovertemplate = '<i>Offense</i>: %{label}<br><i>Number stopped</i>: %{value} (%{percent})<extra></extra>',
#                         marker = list(line = list(color = 'lightgrey', width = 1),
#                                       colors = offense_colors)) %>%
#                 add_pie(labels=~group, values=~n,
#                         textposition = "inside",
#                         title = annotation) %>%
#                 layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                        font=list(family = "GT America"),
#                        hoverlabel=list(font = list(family = "GT America")),
#                        legend = list(itemclick=F, itemdoubleclick=F))
#             
#         } else  {
#             empty_plotly("stops")
#         }
#         
#     })
#     
#     # Agency stats ------------------------------------------------------------
# 
#     agency_values <- reactiveValues(agency = NULL)
# 
#     observeEvent(input$agency_button, {
#         agency_values$agency <- input$agency_agency
#         agency_values$start_date <- input$agency_start_date
#         agency_values$end_date <- input$agency_end_date
#     })
# 
#     output$top_towns <- renderTable({agency_values$top_towns})
# 
#     output$agency_dashboard <- renderUI({
#         validate(
#             need(agency_values$agency, 'Please select filters and press "Go."')
#         )
#         
#         q <- build_query(start_date = agency_values$start_date,
#                          end_date = agency_values$end_date,
#                          agency = agency_values$agency, 
#                          col = "loc, officer, offense, citation")
#         
#         agency_data <- dbGetQuery(sqldb, q)
#         cat("done with agency query\n")
#         
#         # Calculate total stops
#         agency_values$total_stops <- agency_data %>%
#             select(-offense) %>% # Don't double-count multiple offenses in 1 stop
#             distinct() %>%
#             count(name="N") %>%
#             pull(1)
#         
#         # Top towns
#         agency_values$top_towns <- agency_data %>%
#             select(-offense) %>% # Don't double-count multiple offenses in 1 stop
#             distinct() %>%
#             count(loc, name="N") %>%
#             slice_max(N, n=10) %>%
#             mutate(Rank = min_rank(-N),
#                    N = number(N, big.mark=",", accuracy=1)) %>%
#             head(10) %>%
#             select(Rank, `Town/City`=loc, `Number of Stops`=N)
#         
#         # Top officers
#         output$top_officers <- renderTable({
#             
#             agency_data %>%
#                 filter(!is.na(officer)) %>%
#                 select(-offense) %>% # Don't double-count multiple offenses in 1 stop
#                 distinct() %>%
#                 count(officer, name="N") %>%
#                 slice_max(N, n=10) %>%
#                 mutate(Rank = min_rank(-N),
#                        N = number(N, big.mark=",", accuracy=1)) %>%
#                 head(10) %>%
#                 select(Rank, `Officer ID` = officer, `Number of Stops`=N)
#         })
#         
#         # Top offenses
#         output$top_offenses <- renderTable({
#             
#             agency_data %>%
#                 count(offense, name="N") %>%
#                 slice_max(N, n=10) %>%
#                 separate(offense, into = c("Offense", "Statute"), sep= " \\* | (?=c)") %>%
#                 mutate(N = number(N, big.mark=",", accuracy=1)) %>%
#                 rename(`Number of Stops` = N)
# 
#         })
# 
#         tagList(
#             h2(number(agency_values$total_stops, big.mark=","), style="text-align: center;"),
#             p(em("Stops made by", agency_values$agency, br(), "between",
#               format(agency_values$start_date, "%b %d, %Y"), "and",
#               format(agency_values$end_date, "%b %d, %Y")),
#               style="text-align: center;"),
#             splitLayout(id="dashboard_split",
#                 div(h4("Top Cities & Towns"),
#                     tableOutput("top_towns")),
#                 div(h4("Most Active Officers"),
#                     tableOutput("top_officers"))
#             ),
#             h4("Most Common Traffic Violations"),
#             tableOutput("top_offenses")
#         )
#     })
# 
#     # Town overview -------------------------------------------------------------------
# 
#     townover_values <- reactiveValues(done = NULL)
# 
#     observeEvent(input$townover_button, {
#         townover_values$town <- input$townover_town
#         townover_values$start_date <- input$townover_start_date
#         townover_values$end_date <- input$townover_end_date
#     })
# 
#     output$townover_dashboard <- renderUI({
#         validate(
#             need(townover_values$town, 'Please select filters and press "Go."')
#         )
#         
#         q <- build_query(start_date = townover_values$start_date,
#                          end_date = townover_values$end_date,
#                          town = townover_values$town, 
#                          col = "agency, officer, offense, citation")
#         
#         town_data <- dbGetQuery(sqldb, q)
#         cat("done with town query\n")
#         
#         # Calculate total stops
#         townover_values$total_stops <- town_data %>%
#             select(-offense) %>% # Don't double-count multiple offenses in 1 stop
#             distinct() %>%
#             count(name="N") %>%
#             pull(1)
#         
#         # Top agencies
#         output$town_top_agencies <- renderTable({
#             
#             town_data %>%
#                 filter(!is.na(agency)) %>%
#                 select(-offense) %>% # Don't double-count multiple offenses in 1 stop
#                 distinct() %>%
#                 count(agency, name="N") %>%
#                 slice_max(N, n=10) %>%
#                 mutate(Rank = min_rank(-N),
#                        N = number(N, big.mark=",", accuracy=1)) %>%
#                 head(10) %>%
#                 select(Rank, `Agency` = agency, `Number of Stops`=N)
#         })
#     
#         # Top officers
#         output$townover_top_officers <- renderTable({
#             
#             town_data %>%
#                 filter(!is.na(officer)) %>%
#                 select(-offense) %>% # Don't double-count multiple offenses in 1 stop
#                 distinct() %>%
#                 count(agency, officer, name="N") %>%
#                 slice_max(N, n=10) %>%
#                 mutate(Rank = min_rank(-N),
#                        N = number(N, big.mark=",", accuracy=1)) %>%
#                 head(10) %>%
#                 select(Rank, `Officer ID` = officer,
#                        `Agency` = agency, `Number of Stops`=N)
#         })
# 
#         # Top offenses
#         output$townover_top_offenses <- renderTable({ 
#             
#             town_data %>%
#                 count(offense, name="N") %>%
#                 slice_max(N, n=10) %>%
#                 separate(offense, into = c("Offense", "Statute"), sep= " \\* | (?=c)") %>%
#                 mutate(N = number(N, big.mark=",", accuracy=1)) %>%
#                 rename(`Number of Stops` = N)
# 
#         })
# 
#         tagList(
#             h2(number(townover_values$total_stops, big.mark=","),
#                style="text-align: center;"),
#             p(em("Stops made in", townover_values$town, br(), "between",
#                  format(townover_values$start_date, "%b %d, %Y"), "and",
#                  format(townover_values$end_date, "%b %d, %Y")),
#               style="text-align: center;"),
#             splitLayout(id="dashboard_split",
#                 div(h4("Top Agencies"),
#                     tableOutput("town_top_agencies")),
#                 div(h4("Most Active Officers"),
#                     tableOutput("townover_top_officers"))
#             ),
#             h4("Most Common Traffic Violations"),
#             tableOutput("townover_top_offenses")
#         )
#         
#     })
# 
#     # Town's stops by race ------------------------------------------------------------
# 
#     town_values <- reactiveValues()
# 
#     observeEvent(input$town_button, {
#         town_values$town <- input$town_town
#         town_values$agency <- input$town_agency
#         town_values$start_date <- input$town_race_start_date
#         town_values$end_date <- input$town_race_end_date
#     })
# 
#     output$town_demog <- renderPlotly({
#         shinyjs::hide("town_race_legend")
# 
#         validate(
#             need(town_values$town, 'Please select a city or town.')
#         )
#         
#         q <- build_query(start_date = town_values$start_date,
#                          end_date = town_values$end_date,
#                          town = town_values$town, 
#                          agency = town_values$agency,
#                          col="race", group="citation")
#         
#         data_town <- dbGetQuery(sqldb, q) %>%
#             count(race) %>%
#             mutate(var = factor(race, 
#                                  levels = c("White", "Black", "Hispanic/Latinx", 
#                                             "Asian", "Middle Eastern", "Native American", 
#                                             "Unknown"))) %>%
#             arrange(var)
#         
#         if (nrow(data_town) > 0) {
#             
#             shinyjs::show("town_race_legend")
#             
#             title_suffix <- if (town_values$agency != "All agencies") 
#                 paste("by", town_values$agency) else ""
# 
#             data_town_pop <- town_race_pop %>%
#                 filter(town == town_values$town) %>%
#                 select(n=pop, var = race) %>%
#                 arrange(var)
#     
#             town_stop_colors <- colors[data_town$var]
#             town_pop_colors <- colors[data_town_pop$var]
#             
#             annotation <- get_legend_name(town_values$town, town_values$agency, 
#                                           "All officers", "Race of stops",
#                                           pie_label=T) %>%
#                 str_replace(" in", "\nin")
#     
#             plot_ly(sort=F,
#                     direction = "clockwise",
#                     marker = list(line = list(color = 'lightgrey', width = 1)),
#                     labels = ~var, values = ~n,
#                     textposition = "inside"
#                     ) %>%
#                 add_pie(data = data_town,
#                         title = annotation,
#                         domain = list(x = c(0, .5), y = c(0, 1)),
#                         marker = list(colors = town_stop_colors),
#                         hovertemplate = '<i>Race</i>: %{label}<br><i>Number stopped</i>: %{value} (%{percent})<extra></extra>') %>%
#                 add_pie(data = data_town_pop, 
#                         title = paste("<span style='font-size:1.2rem'>", 
#                                       town_values$town, "Population\n(2018 estimate)\n </span>"),
#                         domain = list(x = c(.5, 1), y = c(.2, .8)),
#                         marker = list(colors = town_pop_colors),
#                         hovertemplate = '<i>Race</i>: %{label}<br><i>Population (2018 estimate)</i>: %{value} (%{percent})<extra></extra>') %>%
#                 layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                        showlegend = FALSE,
#                        font=list(family = "GT America"),
#                        hoverlabel=list(font = list(family = "GT America")))
#         } else {
#             empty_plotly("stops")
#         }
# 
#     })
# 
#     # Officer's stops by race ---------------------------------------------------------
# 
#     observeEvent(input$officer_agency, {
#         selected_officers <- officers_per_agency[agency == input$officer_agency,
#                                                  list(officer)]
#         
#         updateSelectizeInput(session, "officer_officer",
#                              choices = c(selected_officers), server=T)
#     })
# 
#     officer_values <- reactiveValues()
# 
#     observeEvent(input$officer_button, {
#         officer_values$officer <- input$officer_officer
#         officer_values$agency <- input$officer_agency
#         officer_values$start_date <- input$officer_race_start_date
#         officer_values$end_date <- input$officer_race_end_date
#     })
# 
#     output$officer_demog <- renderPlotly({
#         shinyjs::hide("officer_race_legend")
# 
#         validate(
#             need(officer_values$officer, 'Please select an officer ID.')
#         )
#         
#         q <- build_query(start_date = officer_values$start_date,
#                          end_date = officer_values$end_date, 
#                          agency = officer_values$agency,
#                          officer = officer_values$officer,
#                          col = "race", group="citation")
#         
#         data_officer <- dbGetQuery(sqldb, q) %>%
#             count(race) %>%
#             mutate(var = factor(race, 
#                                  levels = c("White", "Black", "Hispanic/Latinx", 
#                                             "Asian", "Middle Eastern", "Native American", 
#                                             "Unknown"))) %>%
#             arrange(var)
#         
#         if (nrow(data_officer) > 0) {
#             
#             shinyjs::show("officer_race_legend")
#             
#             q <- build_query(start_date = officer_values$start_date,
#                              end_date = officer_values$end_date, 
#                              agency = officer_values$agency,
#                              col = "race", group="citation")
#         
#             data_agency <- dbGetQuery(sqldb, q) %>%
#                 count(race) %>%
#                 mutate(var = factor(race, 
#                                     levels = c("White", "Black", "Hispanic/Latinx", 
#                                                "Asian", "Middle Eastern", "Native American", 
#                                                "Unknown"))) %>%
#                 arrange(var)
#     
#             officer_colors <- colors[data_officer$var]
#             agency_colors <- colors[data_agency$var]
#             
#             officer_annotation <- get_legend_name("All cities and towns", officer_values$agency, 
#                                           officer_values$officer, "Stops",
#                                           pie_label=T) %>%
#                 str_replace(" in", "\nin")
#             
#             outline_css <- "text-shadow: -1px -1px 0 #fff, 0 -1px 0 #fff, 1px -1px 0 #fff, 1px 0 0 #fff, 1px 1px 0 #fff, 0 1px 0 #fff, -1px 1px 0 #fff, -1px 0 0 #fff;"
#     
#             plot_ly(sort=F,
#                     direction = "clockwise",
#                     hovertemplate = '<i>Race</i>: %{label}<br><i>Number stopped</i>: %{value} (%{percent})<extra></extra>',
#                     marker = list(line = list(color = 'lightgrey', width = 1)),
#                     labels = ~var, values = ~n,
#                     textposition = "inside") %>%
#                 add_pie(data = data_officer,
#                         title = officer_annotation,
#                         domain = list(x = c(.25, 0.75), y = c(0.3, 1)),
#                         marker = list(colors = officer_colors)) %>%
#                 add_pie(data = data_agency, 
#                         title = paste0("<span style='font-size:1.2rem; ", outline_css,
#                                        "'>Stops by the\n",
#                                        officer_values$agency,
#                                        "</span>"),
#                         domain = list(x = c(0, .4), y = c(0, 0.5)),
#                         marker = list(colors = agency_colors)) %>%
#                 add_pie(data = data_mass_race, 
#                         title = paste0("<span style='font-size:1.2rem; ", outline_css, 
#                                        "'>All Massachusetts Stops\n </span>"),
#                         domain = list(x = c(.6, 1), y = c(0, 0.5)),
#                         marker = list(colors = colors)) %>%
#                 layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                        showlegend = FALSE,
#                        font=list(family = "GT America"),
#                        hoverlabel=list(font = list(family = "GT America")))
#             
#         } else {
#             empty_plotly("stops")
#         }
# 
        # })
}
    