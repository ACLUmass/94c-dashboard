library(tidyverse)
library(shiny)
library(plotly)
# library(fst)
library(data.table)
# library(leaflet)
library(lubridate)
library(scales)
# library(shinyjs)
# library(sf)
# library(DBI)
# library(RSQLite)
# library(pool)

# Load data with fread
charge_cats_df <- read_csv("data/94c_charge_codes.csv")
disp_cats_df <- read_csv("data/94c_dispositions.csv")
disp_colors <- readRDS("data/disp_colors.RDS")

combined94c_data <- fread("94c_combined.csv") %>%
  merge(charge_cats_df, by="charge") %>%
  merge(disp_cats_df, by="disposition")

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

# # Connect to sql database using pool to manage connections
# sqldb <- dbPool(
#     drv = SQLite(),
#     dbname = "data/statewide_2002_21.sqlite"
# )
# 
# # Load other datasets
# officers_per_agency <- read_rds("data/sep/officers_per_agency.rds")
# mapping_df <- read_rds("data/sep/mapping.rds") # stops, not offenses
# ma_towns <- read_rds("data/ma_towns.rds")
# all_loc_agency_v_time <- fread("data/sep/all_loc_agency_stops_v_time.csv")  %>%
#     mutate_at(vars(date, month), as_date)
# all_offenses <- fread("data/sep/all_offenses_by_date.csv")  %>%
#     mutate(date = as_date(date))
# data_mass_race <- read_rds("data/mass_race.RDS") %>%
#     rename(var = race) %>%
#     arrange(var)
# town_race_pop <- readRDS("data/towns_race.rds")
# named_colors <- readRDS("data/offense_colors.rds")
# violations <- read_csv("data/violations.csv",
#                        col_types = cols(
#                            offense = col_character(),
#                            group = col_character()
#                        ))
# 
# colors <- c("White" = "#3c3532", 
#             "Black" = "#681b40", 
#             "Hispanic/Latinx" = "#ef404d",
#             "Asian" = "#fabeaf", 
#             "Middle Eastern" =  "#fbb416", 
#             "Native American" = "#a7d7b5", 
#             "Unknown" = "white",
#             "Other" = "white")
# 
function(input, output, session) {

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
#     # Mapping stops -------------------------------------------------------------------
#     
#     stops_per_county <- reactiveValues(data=NULL)
#     
#     observeEvent(input$map_stops_button, {
#         
#         stops_sf <- mapping_df[date >= input$town_start_date & 
#                                    date <= input$town_end_date, 
#                                .(N=sum(N)), loc] %>%
#             merge(ma_towns, by.y = "town", by.x = "loc", all=T) %>% 
#             mutate(N= replace_na(N, 0)) %>% # Fill in 0s for towns with no stops
#             select(-TOWN, town=loc) %>%
#             st_as_sf() %>%
#             st_transform('+proj=longlat +datum=WGS84') 
#         
#         if (input$towns_radio == "Stops per capita") {
#             stops_sf <- stops_sf %>%
#                 mutate(N = (N / pop) * 1e3)
#         }
#         
#         stops_per_county$data <- stops_sf
#         stops_per_county$log <- input$town_log
#         stops_per_county$percap <- input$towns_radio
#     })
#     
#     # Replace legend labels with custom format
#     stopsLabelFormat = function(..., log){ 
#         if (log) {
#             function(type = "numeric", cuts){ 
#                 10**as.numeric(cuts) %>%
#                     scales::number(big.mark=",")
#             } 
#         } else {
#             labelFormat(...)
#         }
#     }
#     
#     output$stops_by_town <- renderLeaflet({
#         validate(
#             need(stops_per_county$data, 'Please select date range and press "Go."')
#         )
#         
#         palette_domain <- if (stops_per_county$log) log10(stops_per_county$data$N) else stops_per_county$data$N
#         palette_domain <- replace(palette_domain, 
#                                   palette_domain == -Inf, NA)
#         
#         if (stops_per_county$percap == "Total stops") {
#             legend_title <- "<a style='font-family:GT America; color: dimgrey'>Total <br>traffic stops</a>"
#             label_accuracy <- 1
#             label_suffix <- ""
#         } else {
#             legend_title <- "<a style='font-family:GT America; color: dimgrey'>Traffic stops<br>per 1,000</a>"
#             label_accuracy <- 1
#             label_suffix <- "per 1,000 population"
#         }
#         
#         pal_total_stops <- colorNumeric(
#             palette = "inferno",
#             domain = palette_domain,
#             na.color = viridis_pal(option="inferno")(10) %>% head(1)
#         )
#         
#         pal_total_stops_noNA <- colorNumeric(
#             palette = "inferno",
#             domain = palette_domain,
#             na.color = NA
#         )
#         
#         leaflet(options = leafletOptions(attributionControl = T)) %>%
#             addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#             addPolygons(data = stops_per_county$data,
#                         fillOpacity = 0.8, 
#                         weight = 1, 
#                         fillColor = if(stops_per_county$log) ~pal_total_stops(log10(N)) else  ~pal_total_stops(N),
#                         stroke=F,
#                         smoothFactor=.5,
#                         label = ~lapply(paste0("<b>", town, "</b></br>", 
#                                                scales::number(N, big.mark=",", accuracy=label_accuracy), " traffic stops ", label_suffix), 
#                                         htmltools::HTML),
#                         color="none",
#                         group="poly")  %>%
#             addLegend(pal = pal_total_stops_noNA,
#                       values = palette_domain,
#                       labFormat = stopsLabelFormat(log=stops_per_county$log),
#                       position = "topright",
#                       title = legend_title,
#             )  %>%
#             addEasyButton(easyButton(
#                 icon="fa-home", title="Reset",
#                 onClick=JS("function(btn, map){ 
#                    var groupLayer = map.layerManager.getLayerGroup('poly');
#                    map.fitBounds(groupLayer.getBounds());
#                }"))) %>%
#             addControl("<img src='Logo_White_CMYK_Massachusetts.png' style='max-width:100px;'>", 
#                        "bottomleft", className="logo-control")
#     })
#     
    # Disposition -----------------------------------------------------------------
    
    disp_values <- reactiveValues(agency = NULL)
    
    # Update year ranges based on year type
    observe({
      validate(
        need(is.numeric(input$disp_start_year), 'Please enter a valid year.'),
        need(is.numeric(input$disp_end_year), 'Please enter a valid year.')
        )
      
      print("year has changed")
      
      if (input$disp_yr_type == "Arrest") {
        yr_max <- combined94c_data %>% pull(arrest_year) %>% max(na.rm = TRUE)
        yr_min <- combined94c_data %>% pull(arrest_year) %>% min(na.rm = TRUE)
      } else if (input$disp_yr_type == "Disposition") {
        yr_max <- combined94c_data %>% pull(disposition_year) %>% max(na.rm = TRUE)
        yr_min <- combined94c_data %>% pull(disposition_year) %>% min(na.rm = TRUE)
      } else if (input$disp_yr_type == "Filing") {
        yr_max <- combined94c_data %>% pull(file_year) %>% max(na.rm = TRUE)
        yr_min <- combined94c_data %>% pull(file_year) %>% min(na.rm = TRUE)
      } else if (input$disp_yr_type == "Offense") {
        yr_max <- combined94c_data %>% pull(offense_year) %>% max(na.rm = TRUE)
        yr_min <- combined94c_data %>% pull(offense_year) %>% min(na.rm = TRUE)
      }
      
      start_value <- input$disp_start_year
      if (input$disp_start_year < yr_min) {
        start_value <- yr_min
      }
      
      end_value <- input$disp_end_year
      if (input$disp_end_year > yr_max) {
        end_value <- yr_max
      }
      
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
        disp_values$data <- combined94c_data
        
        town_str <- "in Massachusetts"
        dept_str <- ""
        court_str <- ""
        charge_str <- ""
        
        if(disp_values$town != "All cities and towns") {
          disp_values$data <- disp_values$data[jurisdiction == disp_values$town]
          town_str <- paste("in", disp_values$town)
        }
        
        if(disp_values$agency != "All departments") {
          disp_values$data <- disp_values$data[department == disp_values$agency]
          dept_str <- paste("by the", disp_values$agency, "Police Department")
        }
        
        if(disp_values$court != "All courts") {
          disp_values$data <- disp_values$data[court == disp_values$court]
          i_court_name = match(disp_values$court, all_courts)
          court_str <- paste("and heard by the", names(all_courts)[i_court_name])
        }
        
        if(disp_values$charge != "All charges") {
          disp_values$data <- disp_values$data[charge_cat == disp_values$charge]
          charge_str <- disp_values$charge
        }

        if (disp_values$disp_yr_type == "Offense") {
          disp_values$data <- disp_values$data[offense_year >= input$disp_start_year & 
                                                 offense_year <= input$disp_end_year, 
                                               .N, disposition_cat]
          verb <- "allegedly committed"
        } else if (disp_values$disp_yr_type == "Filing") {
          disp_values$data <- disp_values$data[file_year >= input$disp_start_year & 
                                                 file_year <= input$disp_end_year,
                                               .N, disposition_cat]
          verb <- "filed"
        } else if (disp_values$disp_yr_type == "Arrest") {
          disp_values$data <- disp_values$data[arrest_year >= input$disp_start_year & 
                                                 arrest_year <= input$disp_end_year, 
                                               .N, disposition_cat]
          verb <- "resulting in arrest"
        } else if (disp_values$disp_yr_type == "Disposition") {
          disp_values$data <- disp_values$data[disposition_year >= input$disp_start_year & 
                                                 disposition_year <= input$disp_end_year, 
                                               .N, disposition_cat]
          verb <- "disposed"
        }
        
        output$disp_str <- renderText(paste("drug-related", tolower(charge_str), "charges", 
                                            verb, "between", disp_values$start_yr, "and", 
                                            disp_values$end_yr, town_str,  dept_str, court_str))
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
                     hoverlabel=list(font = list(family = "GT America")))
          
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
    