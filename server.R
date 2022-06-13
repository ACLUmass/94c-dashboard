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
charge_colors <- readRDS("data/charge_colors.rds")
disp_cats_df <- read_csv("data/94c_dispositions.csv")
disp_colors <- readRDS("data/disp_colors.rds")
ma_towns <- read_rds("data/ma_towns.rds")
DAs <- read_csv("data/DAs.csv")
all_towns <- read_rds("data/all_towns.rds")

load("data/violent_data.RData")

# Load data
combined94c_data_raw <- fread("94c_combined.csv") 

combined94c_data <- combined94c_data_raw %>%
  merge(charge_cats_df, by="charge") %>%
  merge(disp_cats_df, by="disposition")

# Standardize departments and jurisdictions
combined94c_data <- combined94c_data %>%
  mutate(department = case_when(
    str_detect(department, "^Sp |State Police") ~ "Massachusetts State Police",
    str_detect(department, "Israel| Med |Medical|Med$|Health|Hosp") ~ "Hospital Police",
    str_detect(department, "Coll|Campus|Univ|Community|Comm |U Mass|Wpi|Tech|Insti|Western") ~ "College or University Police",
    str_detect(department, " Da") ~ "District Attorney",
    str_detect(department, "Mbta") ~ "MBTA Police",
    str_detect(department, "House|Hoc|Hous$|Jail") ~ "County Jail Police",
    str_detect(department, "Mci|Doc|Corr") ~ "Prison Police",
    str_detect(department, "Boston P.d.|Boston Pd") ~ "Boston PD",
    str_detect(department, "^E ") ~ paste(str_replace(department, "E ", "East "), "PD"),
    str_detect(department, "Cnty") ~ str_replace(department, "Cnty.*", "County Sheriff"),
    str_detect(department, "y Sheri") ~ str_replace(department, "Sheri.*", "Sheriff"),
    str_detect(department, "Sheriff") ~ str_replace(department, "Sheriff.*", "County Sheriff"),
    str_detect(department, " Rr|^Bos$|Bureau|^Do Not Use$|Post|Env|Massport|Munic|Inve|Pub|Housing") ~ "Other",
    str_detect(department, "Police") ~ str_replace(department, "Police", "PD"),
    str_detect(department, "Brookfield Pd") ~ "Brookfield PD",
    str_detect(department, "Hampshire County") ~ "Hampshire County Sheriff",
    is.na(department) ~ "Other",
    T ~ paste(department, "PD")
  ),
  jurisdiction = case_when(
    jurisdiction == "North Attleboro" ~ "N Attleborough",
    str_starts(jurisdiction, "East ") ~ str_replace(jurisdiction, "East ", "E "),
    str_starts(jurisdiction, "West ") ~ str_replace(jurisdiction, "West ", "W "),
    str_starts(jurisdiction, "North ") ~ str_replace(jurisdiction, "North ", "N "),
    str_starts(jurisdiction, "South ") ~ str_replace(jurisdiction, "South ", "S "),
    str_starts(jurisdiction, "Mount ") ~ str_replace(jurisdiction, "Mount ", "Mt "),
    str_starts(jurisdiction, "Great ") ~ str_replace(jurisdiction, "Great ", "Gt "),
    jurisdiction == "Lunenberg" ~ "Lunenburg",
    jurisdiction == "Manchester" ~ "Manchester-By-The-Sea",
    jurisdiction == "(DO NOT USE) Middleton" ~ "Middleton",
    is.na(jurisdiction) ~ "Not Listed",
    T ~ jurisdiction
  ))

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

# Get simple list of courts & names
all_courts <- combined94c_data[order(court), court] %>%
  unique()

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

# Further condense charge categories
combined94c_data <- combined94c_data %>%
  mutate(charge_cat = factor(charge_cat, 
                             levels=c("Possession", "Distribution", "Violation",
                                      "Conspiracy", "Trafficking", "Fraud",
                                      "Prescriptions", "Paraphernalia/Needles",
                                      "Minors", "Larceny", "Labelling",
                                      "Not Listed"))) 

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
        if (town != "Not Listed") {
          town_str <- paste("in", town)
        } else {
          town_str <- "without reporting town/city"
        }
      }
      
      if(agency != "All departments") {
        data <- data[department == agency]
        dept_str <- paste("by the", agency)
      }
      
      if (crt == "All Superior courts") { 
        cat("court:", crt, "\n")
        data <- data %>%
          filter(court %in% all_courts[str_detect(court_names, "Superior")])
        court_str <- paste("and heard by a Superior court")
      } else if (crt == "All District courts") { 
        cat("court:", crt, "\n")
        data <- data %>%
          filter(str_detect(court, "District"))
        court_str <- paste("and heard by a District court")
      } else if (crt == "All Boston Municipal courts") { 
        cat("court:", crt, "\n")
        data <- data %>%
          filter(str_detect(court, "BMC"))
        court_str <- paste("and heard by a Boston Municipal court")
      } else if (crt == "All Juvenile courts") { 
        cat("court:", crt, "\n")
        data <- data %>%
          filter(court %in% all_courts[str_detect(court_names, "Juv")])
        court_str <- paste("and heard by a Juvenile court")
      } else if(crt != "All courts") {
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
    # Download data subset ------------------------------------------------------------

    # Link to this page from landing page
    observeEvent(input$link_to_download, {
        updateTabsetPanel(session, "panels", "Download the Data")
    })
    
    output$download_button <- downloadHandler(
        filename = "94C_charges_MassachusettsTrialCourt_2003_2014.csv",
        content = function(file) {
            cat("Download commencing!\n")
            withProgress(message = 'Downloading...', value = 1, {
                write_csv(combined94c_data_raw, file)
            })
        }
    )

    # District Attorneys ----------------------------------------------------------
    
    # Update year ranges based on year type
    observe({
      yr_min <- 2003
      yr_max <- 2014
      
      if (input$DA_start_year < yr_min) {
        updateNumericInput(session,
                           "DA_start_year", value=yr_min,
                           max=yr_max, min=yr_min)
      } else if (input$DA_end_year < yr_min) {
        updateNumericInput(session,
                           "DA_end_year", value=yr_min,
                           max=yr_max, min=yr_min)
      } else if (input$DA_start_year > yr_max) {
        updateNumericInput(session,
                           "DA_start_year", value=yr_max,
                           max=yr_max, min=yr_min)
      } else if (input$DA_end_year > yr_max) {
        updateNumericInput(session,
                           "DA_end_year", value=yr_max,
                           max=yr_max, min=yr_min)
      }
    })
    
    DA_values <- reactiveValues(done = NULL)
    
    DA_data <- combined94c_data %>%
      mutate(county = case_when(
        county %in% c("Hampshire", "Franklin") ~ "Northwestern",
        county %in% c("Dukes", "Barnstable", "Nantucket") ~ "Cape and Islands",
        T ~ county
      ))
    
    pal <- colorNumeric(
      palette = c("grey", "#00343a"),
      domain = 0:1,
      na.color = "#bbbbbb"#viridis_pal(option="inferno")(10) %>% head(1)
    )
    
    # Plot the county 
    output$DA_map <- renderLeaflet({
      
      map_data <- mass_DA_dists %>%
        mutate(is_our_county = district == input$DA_county)
      
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
    
    # Change county on click
    observeEvent(input$DA_map_shape_click, {
      # Get lat & lng of click
      p <- input$DA_map_shape_click
      
      # Convert to sf
      pt <- data.frame(longitude = p$lng, latitude = p$lat) %>%
        st_as_sf(coords = c("longitude", "latitude"), crs=4326) 
      
      # Determine which county it was in
      selected_cty <- st_join(pt, mass_DA_dists, join = st_intersects)$district
      
      # Update county
      updateSelectInput(session, "DA_county", selected = selected_cty)
      
    })

    output$DA_dashboard <- renderUI({
      
        DA_values$DA_cty <- input$DA_county
        DA_values$start_year <- input$DA_start_year
        DA_values$end_year <- input$DA_end_year
        
        DA_data <- DA_data[county == input$DA_county & 
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
            mutate(in_scandal = ifelse(disposition_cat == "Dismissed (Lab Misconduct)", 
              "Charges dismissed due to scandal", "Other 94C charges")) %>%
            count(in_scandal, name="N") %>%
            arrange(in_scandal) %>%
            plot_ly(sort=F,
                    direction = "clockwise",
                    marker = list(line = list(color = 'lightgrey', width = 1),
                                  colors=c("#a7d7b5", "black")),
                    labels = ~in_scandal, values = ~N,
                    textposition = "inside") %>%
            add_pie(hovertemplate = '%{value} charges (%{percent})<extra></extra>') %>%
            layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   showlegend = T,
                   margin = list(l = 20, r = 20),
                   font=list(family = "GT America"),
                   hoverlabel=list(font = list(family = "GT America")),
                   legend = list(x = .5, y = -0.05, xanchor="center", yanchor="top"))
        })

        # Top agencies
        output$DA_top_agencies <- renderTable({

            DA_data %>%
                filter(!is.na(department)) %>%
                add_count(department, name="N") %>%
                mutate(was_dismissed = disposition_cat == "Dismissed") %>%
                filter(was_dismissed) %>%
                add_count(department, name="N_dismissed") %>%
                # View()
                distinct(department, N, N_dismissed) %>%
                slice_max(N, n=10) %>%
                mutate(pct = N_dismissed / N,
                       Rank = min_rank(-N),
                       N = number(N, big.mark=",", accuracy=1),
                       N_dismissed = paste0(number(N_dismissed, big.mark=",", accuracy=1), " (",
                                            percent(pct, accuracy=0.1), ")")) %>%
                head(10) %>%
                select(Rank, `Agency` = department, `Charges filed`=N, `Charges dismissed*`=N_dismissed)
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
            fluidRow(
                        column(4, h3(number(scandal_charges, big.mark=","), style="margin:0px;"), 
                               h4("charges dismissed due to"), br(), h4(paste("lab scandal in", 
                                 DA_values$DA_cty, "district")),
                        plotlyOutput("DA_pie", height="300px")),
                column(8, h4("Top Agencies"),
                    tableOutput("DA_top_agencies"),
                    em("*Dismissed for any reason, not only the lab scandal.")),
                style="margin-bottom: 3rem;"
            ),
            fluidRow(
                column(6, h4("Top Courts"),
                    tableOutput("DA_top_courts")),
                column(6, h4("Most Common Charges"),
                    tableOutput("DA_top_charges")))
        )
    })
    
    # Scandal -------------------------------------------------------------------

    # Update geo dropdown
    observeEvent(input$scandal_radio, {
      if (input$scandal_radio == "State") {
        disable("scandal_geo")
        updateSelectInput(session, "scandal_geo",
                          choices = c())
      } else if (input$scandal_radio == "County") {
        enable("scandal_geo")
        updateSelectInput(session, "scandal_geo",
                          choices = combined94c_data %>% pull(county) %>% unique() %>% sort())
      } else if (input$scandal_radio == "City/Town") {
        enable("scandal_geo")
        updateSelectInput(session, "scandal_geo",
                          choices = tail(all_towns, n=length(all_towns) -1) %>% sort())
      }
    })
    
    # Watch for button press
    scandal_values <- reactiveValues(radio=NULL)
    observeEvent(input$scandal_button, {
      scandal_values$radio <- input$scandal_radio
      scandal_values$geo <- input$scandal_geo
    })
    
    # Update plots
    output$scandal_dashboard <- renderUI({
      validate(
        need(scandal_values$radio, 'Please select a geography and press "Go".')
      )
      
      if (scandal_values$radio== "State") {
        data <- combined94c_data
        text <- "Massachusetts"
      } else if (scandal_values$radio  == "County") {
        data <- combined94c_data %>%
          filter(county == scandal_values$geo)
        text <- paste(scandal_values$geo, "County")
      } else if (scandal_values$radio  == "City/Town") {
        data <- combined94c_data %>%
          filter(jurisdiction == scandal_values$geo)
        text <- scandal_values$geo
      }
      
      n_charges <- data %>% 
          filter(disposition_cat == "Dismissed (Lab Misconduct)") %>%
        nrow()
      
      output$n_charges <- renderText(number(n_charges, big.mark=","))
      
      output$n_cases <- renderText(number(
        data %>% 
          filter(disposition_cat == "Dismissed (Lab Misconduct)") %>%
          distinct(case_id) %>% nrow(), 
        big.mark=","))
      
      output$geo_str <- renderText(text)
      output$geo_str1 <- renderText(paste0(text, ":"))
      
      # Percent implicated in scandal
      output$scandal_pie <- renderPlotly({
        
        data %>%
          mutate(in_scandal = ifelse(
            disposition_cat == "Dismissed (Lab Misconduct)", 
            "Charges dismissed due to scandal", "Other 94C charges")) %>%
          count(in_scandal, name="N") %>%
          arrange(in_scandal) %>%
          plot_ly(sort=F,
                  direction = "clockwise",
                  marker = list(line = list(color = 'lightgrey', width = 1),
                                colors=c("#a7d7b5", "black")),
                  labels = ~in_scandal, values = ~N,
                  textposition = "inside") %>%
          add_pie(hovertemplate = '%{value} charges (%{percent})<extra></extra>') %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 showlegend = T,
                 margin = list(l = 20, r = 20),
                 font=list(family = "GT America"),
                 hoverlabel=list(font = list(family = "GT America")),
                 legend = list(x = .5, y = -10))
      })
      
      # Crime after dismissals
      output$crime <- renderPlotly({
        if (scandal_values$radio  == "State") {
          violent_data <- ma_violent_df
        } else if (scandal_values$radio  == "County") {
          violent_data <- cnty_violent_df %>%
            filter(geo == paste(scandal_values$geo, "County"))
        } else if (scandal_values$radio  == "City/Town") {
          violent_data <- twns_violent_df  %>%
            filter(geo == scandal_values$geo)
        }
        
        violent_data <- violent_data %>%
          filter(year(date) >= 2016)
        
        
        if (n_charges > 0) {
          violent_data %>%
            plot_ly(x = ~date, y = ~actual_violent_offenses, 
                    type = 'scatter', mode = 'lines',
                    hovertemplate = '%{y:,.0f} violent offenses in %{x}<extra></extra>',
                    line = list(color = '#3c3532', width=2.5), opacity=.7) %>%
            layout(yaxis = list(title = "Number of violent offenses", zeroline = F),
                   xaxis = list(title="Date", zeroline = F),
                   font=list(family = "GT America"),
                   hoverlabel=list(font = list(family = "GT America")),
                   annotations = list(list(
                     showarrow = F, opacity = 0.7,
                     x = .5, xref="paper", xanchor = "center",
                     y = 1.01, yref="paper",yanchor="bottom",
                     text = "<i>Click and drag to zoom in on a specific date range</i>"
                   ))) %>%
            layout(shapes = list(
              list(type="line", x0=ymd("20170420"),x1=ymd("20170420"), y0=0, y1=1, yref="paper", line = list(color = "#d62728", dash="dash")),
              list(type="line", x0=ymd("20180405"),x1=ymd("20180405"), y0=0, y1=1, yref="paper", line = list(color = "#0055aa", dash="dash")),
              list(type="line", x0=ymd("20181011"),x1=ymd("20181011"), y0=0, y1=1, yref="paper", line = list(color = "#0055aa", dash="dash"))
              )) %>%
            add_annotations(text="Dookhan\ndismissals",
                      x=ymd("20170420"),
                      y=0, yref="paper", textangle=270, 
                      font = list(family = "GT America",color="#d62728"),
                      yanchor="bottom", xanchor="right",showarrow=F,
                      bgcolor="rgba(255,255,255,0.5)") %>%
            add_annotations(text="Farak\ndismissals",
                            x=ymd("20180405"),
                            y=0, yref="paper", textangle=270, 
                            font = list(family = "GT America",color="#0055aa"),
                            yanchor="bottom", xanchor="right",showarrow=F,
                            bgcolor="rgba(255,255,255,0.5)") %>%
            add_annotations(text="Farak\ndismissals",
                            x=ymd("20181011"),
                            y=0, yref="paper", textangle=270, 
                            font = list(family = "GT America",color="#0055aa"),
                            yanchor="bottom", xanchor="right",showarrow=F,
                            bgcolor="rgba(255,255,255,0.5)") 
        } else {
          empty_plotly("dismissed charges")
        }
      })
        
        tagList(
          div(class="scandal_numbers",
              h3(textOutput("geo_str1", inline=T)),
              h4(textOutput("n_charges")), "charges dismissed across",
              h4(textOutput("n_cases")), "cases in ", textOutput("geo_str", inline=T)
          ),
          fluidRow(
            column(4, div(class="scandal_title", "What percentage of drug-related\ncharges were dismissed?"), plotlyOutput("scandal_pie")),
            column(8, div(class="scandal_title", "How does the dismissal timeline compare to\nstate-reported rates of", a("violent crime", href="https://masscrime.chs.state.ma.us/public/Browse/browsetables.aspx?PerspectiveLanguage=en", target="_blank"), "?"), plotlyOutput("crime"))
          ))
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
        year_type = input$map_yr_type,
        start_year=input$map_start_year,
        end_year=input$map_end_year)
      
      data <- results[[1]]
      # label <- results[[2]]

        charges_sf <- data[, .N, jurisdiction] %>%
            merge(ma_towns, by.y = "town", by.x = "jurisdiction", all=T) %>%
            filter(!is.na(pop)) %>%
            mutate(N= replace_na(N, 0)) %>% # Fill in 0s for towns with no stops
            select(-TOWN, town=jurisdiction) %>%
            st_as_sf() %>%
            st_transform('+proj=longlat +datum=WGS84')
        
        charge_types_sf <- data[, .N, .(jurisdiction, charge_cat)] %>%
          # combined94c_data[, .N, .(jurisdiction, charge_cat)] %>%
          merge(ma_towns, by.y = "town", by.x = "jurisdiction", all=T) %>%
          filter(!is.na(pop)) %>%
          mutate(N= replace_na(N, 0)) %>% # Fill in 0s for towns with no stops
          select(-TOWN, town=jurisdiction) %>%
          st_as_sf() %>%
          st_transform('+proj=longlat +datum=WGS84') %>%
          group_by(town) %>%
          mutate(TotalCharges = sum(N)) %>%
          ungroup() %>%
          filter(charge_cat == "Possession") %>%
          mutate(pct = N / TotalCharges) %>%
          mutate(pct = pct*100)

        charges_map$data <- charges_sf
        charges_map$type_data <- charge_types_sf
        # charges_map$log <- input$map_log
        # charges_map$percap <- input$map_radio
    })

    # Replace legend labels with custom format
    stopsLabelFormat = function(..., log, pct=F){
        if (log) {
            function(type = "numeric", cuts){
                10**as.numeric(cuts) %>%
                    scales::number(big.mark=",", accuracy=1)
            }
        } else if (pct) {
            function(type = "numeric", cuts){
              scales::percent(cuts/100, accuracy=.1)
            }
        } else {
            labelFormat(...)
        }
    }

    output$charges_by_town <- renderLeaflet({
        validate(
            need(charges_map$data, 'Please select date range and press "Go."')
        )
      
      data <- charges_map$data %>%
        filter(N > 0)

        palette_domain <- if (input$map_log) log10(data$N) else data$N
        palette_domain <- replace(palette_domain,
                                  palette_domain == -Inf, NA)

        legend_title <- "<a style='font-family:GT America; color: dimgrey'>Total <br> 94C charges</a>"
        label_accuracy <- 1
        label_suffix <- ""

        pal_total_stops <- colorNumeric(
            palette = "Greens",#"inferno",
            domain = palette_domain,
            na.color = "#bbbbbb"#viridis_pal(option="inferno")(10) %>% head(1)
        )
        
        pal_one_town <- colorNumeric(
          palette = c("#bbbbbb", "red"),
          domain = c(0, max(palette_domain, na.rm=T)),
          na.color = viridis_pal(option="inferno")(10) %>% tail(1)
        )
        
        pal_total_stops_noNA <- colorNumeric(
            palette = "Greens",#"inferno",
            domain = palette_domain,
            na.color = NA
        )
        
        one_town <- data %>%
          filter(N != 0) %>%
          nrow() == 1
          
        leaflet(options = leafletOptions(attributionControl = T)) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            addPolygons(data = data,
                        fillOpacity = 0.8,
                        weight = 1,
                        fillColor = if (one_town & input$map_log) {
                          ~pal_one_town(N)
                        } else if (input$map_log) {
                          ~pal_total_stops(log10(N))
                        } else {
                          ~pal_total_stops(N)
                        },
                        stroke=T,
                        smoothFactor=.5,
                        label = ~lapply(paste0("<b>", town, "</b></br>",
                                               scales::number(N, big.mark=",", accuracy=label_accuracy), " charges", label_suffix),
                                        htmltools::HTML),
                        color="grey",
                        group="poly")  %>%
            addLegend(pal = pal_total_stops_noNA,
                      values = palette_domain,
                      labFormat = stopsLabelFormat(log=input$map_log),
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
    
    output$charges_by_town_percap <- renderLeaflet({
      validate(
        need(charges_map$data, 'Please select date range and press "Go."')
      )
      
      data <- charges_map$data %>%
        mutate(N = (N / pop) * 1e3) %>% 
        filter(N > 0)
      
      palette_domain <- if (input$map_log) log10(data$N) else data$N
      palette_domain <- replace(palette_domain,
                                palette_domain == -Inf, NA)
      
      legend_title <- "<a style='font-family:GT America; color: dimgrey'>94C charges<br>per 1,000</a>"
      label_accuracy <- 1
      label_suffix <- " per 1,000 population"
      
      pal_total_stops <- colorNumeric(
        palette = "Greens",#inferno",
        domain = palette_domain,
        na.color = "#bbbbbb"#viridis_pal(option="inferno")(10) %>% head(1)
      )
      
      pal_one_town <- colorNumeric(
        palette = c("#bbbbbb", "red"),
        domain = c(0, max(palette_domain, na.rm=T)),
        na.color = viridis_pal(option="inferno")(10) %>% tail(1)
      )
      
      pal_total_stops_noNA <- colorNumeric(
        palette = "Greens",#"inferno",
        domain = palette_domain,
        na.color = NA
      )
      
      one_town <- data %>%
        filter(N != 0) %>%
        nrow() == 1
      
      leaflet(options = leafletOptions(attributionControl = T)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(data = data,
                    fillOpacity = 0.8,
                    weight = 1,
                    fillColor = if (one_town & input$map_log) {
                      ~pal_one_town(N)
                    } else if (input$map_log) {
                      ~pal_total_stops(log10(N))
                    } else {
                      ~pal_total_stops(N)
                    },
                    stroke=T,
                    smoothFactor=.5,
                    label = ~lapply(paste0("<b>", town, "</b></br>",
                                           scales::number(N, big.mark=",", accuracy=label_accuracy), " charges", label_suffix),
                                    htmltools::HTML),
                    color="grey",
                    group="poly")  %>%
        addLegend(pal = pal_total_stops_noNA,
                  values = palette_domain,
                  labFormat = stopsLabelFormat(log=input$map_log),
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
    
    output$charge_types_by_town <- renderLeaflet({
      validate(
        need(charges_map$type_data, 'Please select date range and press "Go."')
      )
      
      data <- charges_map$type_data
      
      palette_domain <- 0:100
      
      legend_title <- paste0("<a style='font-family:GT America; color: dimgrey'>What percentage of <br>94C charges were<br>for drug possession?</a>")
      
      pal_total_stops <- colorNumeric(
        # palette = "inferno",
        palette = "Greens",
        domain = palette_domain,
        na.color = "#bbbbbb"#viridis_pal(option="inferno")(10) %>% head(1)
      )
      
      pal_one_town <- colorNumeric(
        palette = c("#bbbbbb", "red"),
        domain = c(0, max(palette_domain, na.rm=T)),
        na.color = viridis_pal(option="inferno")(10) %>% tail(1)
      )
      
      pal_total_stops_noNA <- colorNumeric(
        # palette = "inferno",s
        palette = "Greens",
        domain = palette_domain,
        na.color = NA
      )
      
      one_town <- data %>%
        filter(pct != 0) %>%
        nrow() == 1
      
      leaflet(options = leafletOptions(attributionControl = T)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(data = data,
                    fillOpacity = 0.8,
                    weight = 1,
                    fillColor = if (one_town) {
                      print("one town log")
                      ~pal_one_town(pct)
                    } else {
                      print("none")
                      ~pal_total_stops(pct)
                    },
                    stroke=T,
                    smoothFactor=.5,
                    label = ~lapply(paste0("<b>", town, "</b></br>",
                                           scales::percent(pct/100, accuracy=.1), " charges for drug possession"),
                                    htmltools::HTML),
                    color="grey",
                    group="poly")  %>%
        addLegend(pal = pal_total_stops_noNA,
                  values = palette_domain,
                  labFormat = stopsLabelFormat(log=F, pct=T),
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
        
        if (input$screen_width > 767) {
          legend_x <- 100
          legend_y <- 0.5
          xanchor <- "left"
          yanchor <- "center"
          orientation <- "v"
          height <- 400
        } else {
          legend_x <- .5
          legend_y <- -0.05
          xanchor <- "center"
          yanchor <- "top"
          orientation <- "h"
          height <- 450
        }

        if (nrow(data) > 0) {
          
          data %>%
          plot_ly(sort=F,
                  direction = "clockwise",
                  marker = list(line = list(color = 'lightgrey', width = 1),
                                colors=disp_colors_here),
                  labels = ~disposition_cat, values = ~N,
                  textposition = "inside",
                  height = height
                  ) %>%
            add_pie(hovertemplate = '<i>Disposition</i>: %{label}<br>%{value} charges (%{percent})<extra></extra>') %>%
              layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     showlegend = TRUE,
                     font=list(family = "GT America"),
                     hoverlabel=list(font = list(family = "GT America")),
                     legend = list(x = legend_x, y = legend_y, 
                                   xanchor = xanchor, yanchor = yanchor,
                                   orientation = orientation))
          
        } else {
            # If there are no stops for the filter
            empty_plotly("charges")
        }

    })
    
    # Charge Type -----------------------------------------------------------------
    
    charge_values <- reactiveValues(agency = NULL)
    
    # Update year ranges based on year type
    observe({
      validate(
        need(is.numeric(input$charge_start_year), 'Please enter a valid year.'),
        need(is.numeric(input$charge_end_year), 'Please enter a valid year.')
      )
      
      bounds <- get_year_bounds(input$charge_yr_type, 
                                input$charge_start_year, 
                                input$charge_end_year)
      yr_min <- bounds[[1]]
      yr_max <- bounds[[2]]
      start_value <- bounds[[3]]
      end_value <- bounds[[4]]
      
      updateNumericInput(session,
                         "charge_start_year", value=start_value,
                         max=yr_max, min=yr_min)
      updateNumericInput(session,
                         "charge_end_year", value=end_value,
                         max=yr_max, min=yr_min)
      
    })
    
    # Calculate all the values for charge type plot
    observeEvent(input$charge_button, {
      
      charge_values$town <- input$charge_city
      charge_values$agency <- input$charge_dept
      charge_values$court <- input$charge_court
      charge_values$disp <- input$charge_disp
      charge_values$yr_type <- input$charge_yr_type
      charge_values$start_yr <- input$charge_start_year
      charge_values$end_yr <- input$charge_end_year
      
      results <- filter_and_get_label(
        combined94c_data, town = charge_values$town,
        agency = charge_values$agency,
        crt = charge_values$court,
        disp = charge_values$disp,
        year_type = charge_values$yr_type,
        start_year=charge_values$start_yr,
        end_year=charge_values$end_yr)
      
      data <- results[[1]]
      charge_values$data <- data[, .N, charge_cat] 
      
      label <- results[[2]]
      output$charge_str <- renderText(label)
    })
    
    # Create charge plot
    output$charge <- renderPlotly({
      
      validate(
        need(charge_values$data, 'Please select filters and press "Go."')
      )
      
      data <- charge_values$data %>%
        arrange(charge_cat) %>%
        filter(!is.na(charge_cat))
      
      output$charge_count_str <- renderText(data %>% pull(N) %>%sum() %>% scales::comma())
      
      charge_colors_here <- charge_colors[data$charge_cat]
      
      if (input$screen_width > 767) {
        legend_x <- 100
        legend_y <- 0.5
        xanchor <- "left"
        yanchor <- "center"
        orientation <- "v"
        height <- 400
      } else {
        legend_x <- .5
        legend_y <- -0.05
        xanchor <- "center"
        yanchor <- "top"
        orientation <- "h"
        height <- 450
      }
      
      if (nrow(data) > 0) {
        
        data %>%
          plot_ly(sort=F,
                  direction = "clockwise",
                  marker = list(line = list(color = 'lightgrey', width = 1),
                                colors=charge_colors_here),
                  labels = ~charge_cat, values = ~N,
                  textposition = "inside",
                  height = height
          ) %>%
          add_pie(hovertemplate = '<i>Charge</i>: %{label}<br>%{value} charges (%{percent})<extra></extra>') %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 showlegend = TRUE,
                 font=list(family = "GT America"),
                 hoverlabel=list(font = list(family = "GT America")),
                 legend = list(x = legend_x, y = legend_y,
                               xanchor = xanchor, yanchor = yanchor,
                               orientation = orientation))
        
      } else {
        # If there are no stops for the filter
        empty_plotly("charges")
      }
      
    })
    
    # Drug class -----------------------------------------------------------------
    
    class_values <- reactiveValues(agency = NULL)
    
    observeEvent(input$class_button, {
      
      class_values$town <- input$class_city
      class_values$agency <- input$class_dept
      class_values$court <- input$class_court
      class_values$disp <- input$class_disp
      
      results <- filter_and_get_label(
        combined94c_data, 
        town = class_values$town,
        agency = class_values$agency,
        crt = class_values$court,
        disp = class_values$disp)
      
      data <- results[[1]]
      class_values$data <- data
      
      class_values$label <- results[[2]]
      
    })
    
    output$class_v_time <- renderPlotly({
      
      validate(
        need(class_values$agency, 'Please select filters and press "Go."')
      )
      
      data <- class_values$data %>%
        mutate(has_class = str_detect(charge_subcat, "Class"),
               class = str_extract(charge_subcat, "Class [A-E]"),
               x=file_year) %>%
        filter(has_class, x <= 2014) %>%
        count(class, x, name="N") %>%
        pivot_wider(names_from=matches("class"), values_from=matches("N"))
      
      if (nrow(data) > 0) {
        
        data %>%
          plot_ly(hovertemplate = '%{y:,} %{text} charges in %{x}<extra></extra>',
                  line = list(width=3.5, color="#882255"), x=~x, y=~`Class A`, 
                  text="Class A", 
                  name="Class A", type = 'scatter', mode = 'lines', opacity=.7)%>%
          add_trace(y = ~`Class B`, name = 'Class B', mode = 'lines', text="Class B", 
                    line = list(width=3.5, color="#0055aa")) %>%
          add_trace(y = ~`Class C`, name = 'Class C', mode = 'lines', text="Class C",
                    line = list(width=3.5, color="#DDCC77")) %>%
          add_trace(y = ~`Class D`, name = 'Class D', mode = 'lines', text="Class D",
                    line = list(width=3.5, color="#117733")) %>%
          add_trace(y = ~`Class E`, name = 'Class E', mode = 'lines', text="Class E",
                    line = list(width=3.5, color="#CC6677")) %>%
          layout(yaxis = list(title = "Number of charges", zeroline = F),
                 xaxis = list(title = "Year of filing", zeroline = F),
                 legend = list(x=100, y=0.5),
                 font=list(family = "GT America"),
                 hoverlabel=list(font = list(family = "GT America")),
                 shapes = list(
                   list(type="line", x0=2008,x1=2008, y0=0, y1=1,
                   yref="paper", line = list(color = "#d62728", dash="dash"))),
                 annotations = list(list(
                     showarrow = F, opacity = 0.7,
                     x = .5, xref="paper", xanchor = "center",
                     y = 1.05, yref="paper",
                     text = "<i>Click and drag to zoom in on a specific date range</i>"
                   ),
                   list(text="Marijuana\ndecriminalization",
                        x=2008,
                        y=0, yref="paper", textangle=270, 
                        font = list(family = "GT America", color="#d62728"),
                        yanchor="bottom", xanchor="right",showarrow=F,
                        bgcolor="rgba(255,255,255,0.5)")))
        
      } else {
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
    
    # Demographics -----------------------------------------------------------------
    
    dem_values <- reactiveValues(agency = NULL)
    
    dems <- c("Male under 18", "Male aged 19 - 25", 
              "Male aged 26 - 35", "Male aged 36 - 45", 
              "Male aged 46 - 55", "Male over 55",
              "Female under 18", "Female aged 19 - 25",
              "Female aged 26 - 35", "Female aged 36 - 45", 
              "Female aged 46 - 55", "Female over 55",
              "Unknown")
    
    # Define pie colors 
    dem_colors <- c(rep("rgba(51, 34, 136", 6), rep("rgba(136, 34, 85", 6), "rgb(211,211,211)")
    dem_opacity <- c(seq(1, .2, length.out=6), seq(1, .2, length.out=6), 1) 
    dem_colors <- paste0(dem_colors, ", ", dem_opacity, ")")
    names(dem_colors) <- dems
    
    # Update year ranges based on year type
    observe({
      validate(
        need(is.numeric(input$dem_start_year), 'Please enter a valid year.'),
        need(is.numeric(input$dem_end_year), 'Please enter a valid year.')
      )
      
      bounds <- get_year_bounds(input$dem_yr_type, 
                                input$dem_start_year, 
                                input$dem_end_year)
      yr_min <- bounds[[1]]
      yr_max <- bounds[[2]]
      start_value <- bounds[[3]]
      end_value <- bounds[[4]]
      
      updateNumericInput(session,
                         "dem_start_year", value=start_value,
                         max=yr_max, min=yr_min)
      updateNumericInput(session,
                         "dem_end_year", value=end_value,
                         max=yr_max, min=yr_min)
      
    })
    
    # Calculate all the values for demographics plot
    observeEvent(input$dem_button, {
      
      dem_values$town <- input$dem_city
      dem_values$agency <- input$dem_dept
      dem_values$court <- input$dem_court
      dem_values$charge <- input$dem_charge
      dem_values$disp <- input$dem_disp
      dem_values$yr_type <- input$dem_yr_type
      dem_values$start_yr <- input$dem_start_year
      dem_values$end_yr <- input$dem_end_year
      
      results <- filter_and_get_label(
        combined94c_data, 
        town = dem_values$town,
        agency = dem_values$agency,
        crt = dem_values$court,
        chrg = dem_values$charge,
        disp = dem_values$disp,
        year_type = dem_values$yr_type,
        start_year=dem_values$start_yr,
        end_year=dem_values$end_yr)
      
      data <- results[[1]]
      dem_values$data <- data %>%
        mutate(age_bin = case_when(
                  age_at_file < 18 ~ "under 18",
                  age_at_file <=25 ~ "aged 19 - 25",
                  age_at_file <=35 ~ "aged 26 - 35",
                  age_at_file <=45 ~ "aged 36 - 45",
                  age_at_file <=55 ~ "aged 46 - 55",
                  age_at_file > 55 ~ "over 55",
                  T ~ as.character(age_at_file)),
               dem = ifelse((is.na(age_bin)| is.na(gender)),
                            "Unknown",
                            paste(gender, age_bin)),
               dem = factor(dem, levels=dems)) %>%
        count(dem, name="N") %>%
        arrange(dem)
      
      dem_values$data_gender <- data[, .N, gender] %>%
        mutate(gender = ifelse(is.na(gender), "Unknown", gender),
               gender = factor(gender, levels=c("Male", "Female", "Unknown"))) %>%
        arrange(gender) 
      
      dem_values$data_age <- data$age_at_file
    })
    
    # Create demographics pie chart
    output$demographics <- renderPlotly({
      
      data <- dem_values$data %>%
        arrange(dem)
      
      dem_colors_here <- dem_colors[data$dem]
      
      if (input$screen_width > 767) {
        legend_x <- 100
        legend_y <- 0.5
        xanchor <- "left"
        yanchor <- "center"
        orientation <- "v"
        height <- 400
      } else {
        legend_x <- .5
        legend_y <- -0.05
        xanchor <- "center"
        yanchor <- "top"
        orientation <- "h"
        height <- 450
      }
      
      data %>%
        plot_ly(sort=F,                 
                direction = "clockwise",
                marker = list(line = list(color = 'lightgrey', width = 1),
                              colors=dem_colors_here),
                labels = ~dem, values = ~N,
                textposition = "inside",
                height = height
        ) %>%
        add_pie(hovertemplate = '<i>Gender</i>: %{label}<br>%{value} charges (%{percent})<extra></extra>') %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               showlegend = TRUE,
               font=list(family = "GT America"),
               hoverlabel=list(font = list(family = "GT America")),
               legend = list(x = legend_x, y = legend_y,
                             xanchor = xanchor, yanchor = yanchor,
                             orientation = orientation))
    })
    
    output$dem_dashboard <- renderUI({
      
      validate(
        need(dem_values$data_gender, 'Please select filters and press "Go."')
      )  
      
      plotly_modebar_to_remove <- c("zoom2d","pan2d","select2d","lasso2d",
                                    "zoomIn2d","zoomOut2d","autoScale2d")
      
      
      
      # Create gender bar plot
      output$demographics_gender <- renderPlotly({
        
        dem_values$data_gender %>%
          ggplot(aes(x=0, y=-N, 
                     fill=gender,
                     text=sprintf("Gender: %s<br># Charges: %s<br>%% Charges: %s", 
                                  gender, number(N, big.mark=","), 
                                  percent(N/sum(N), accuracy=.1)))) +
          geom_col(width=.2, show.legend=F) +
          coord_flip() +
          scale_fill_manual(values=c("#332288", "#882255", "grey")) +        
          xlim(-.5, .5) +
          theme_void()
        
        N_m <- dem_values$data_gender %>%
          filter(gender == "Male") %>%
          pull(N)
        N_f <- dem_values$data_gender %>%
          filter(gender == "Female") %>%
          pull(N)
        N_u <- dem_values$data_gender %>%
          filter(gender == "Unknown") %>%
          pull(N)
        
        N_m <- ifelse(length(N_m) > 0, N_m, 0)
        N_f <- ifelse(length(N_f) > 0, N_f, 0)
        N_u <- ifelse(length(N_u) > 0, N_u, 0)
        
        m_str <- ifelse(N_m > 0, paste(ifelse(N_m > 1,
                                              paste(number(N_m, big.mark=","), "charges"),
                                              "1 charge"),
                                       "\nto male individuals"),
                        "No male\nindividuals charged")
        f_str <-ifelse(N_f > 0, paste(ifelse(N_f > 1,
                                             paste(number(N_f, big.mark=","), "charges"),
                                             "1 charge"),
                                      "\nto female individuals"),
                       "No female\nindividuals charged")
        u_str <- ifelse(N_u > 0, paste(ifelse(N_u > 1,
                                              paste(number(N_u, big.mark=","), "charges"),
                                              "1 charge"),
                                       "\nto individuals of\nunknown gender"),
                        "No individuals\nof unknown gender\ncharged")
        
        ggplotly(tooltip="text", height=200) %>%
          add_annotations(x = 0,
                          y = .4,
                          text = m_str,
                          text_position = "left",
                          xanchor="left",
                          yanchor="top",
                          font=list(color="#332288"),
                          showarrow=F,
                          xref = "paper",
                          yref = "paper") %>%
          add_annotations(x = 1, 
                          y = .6,
                          text = f_str,
                          text_position = "inside",
                          xanchor="right", 
                          yanchor="bottom", 
                          font=list(color="#882255"),
                          showarrow=F,
                          xref = "paper",
                          yref = "paper") %>%
          add_annotations(x = 1,
                          y = .4,
                          text = u_str,
                          text_position = "inside",
                          xanchor="right",
                          yanchor="top",
                          font=list(color="darkgrey"),
                          showarrow=F,
                          xref = "paper",
                          yref = "paper") %>%
          layout(xaxis = list(zeroline = F, showgrid = FALSE, showticklabels = FALSE, zerolinecolor = '#ffff'),
                 yaxis = list(zeroline = F, showgrid = FALSE, showticklabels = FALSE, zerolinecolor = '#ffff'),
                 showlegend = F,
                 font=list(family = "GT America"),
                 hoverlabel=list(font = list(family = "GT America"))) %>%
          config(modeBarButtonsToRemove = plotly_modebar_to_remove)
      })
      
      
      # Create age histogram 
      output$demographics_age <- renderPlotly({
        
        plot_ly(x = dem_values$data_age, 
                type = "histogram",
                nbinsx=50,
                marker = list(color = "darkgray",
                              line = list(color = "darkgray",
                                          width = 2)),
                hovertemplate = paste('<i>Age</i>: %{x}',
                                      '<br>Number charged: %{y:,.0f}',
                                      '<extra></extra>'),
                height=200) %>%
          layout(xaxis = list(title = 'Age (years)'), 
                 yaxis = list(title = 'Number of\nindividuals'),
                 showlegend = F,
                 font=list(family = "GT America"),
                 hoverlabel=list(font = list(family = "GT America"))) %>%
          config(modeBarButtonsToRemove = plotly_modebar_to_remove)
      })
      
      output$empty <- renderPlotly({
        empty_plotly("charges")
      })
      

      if (nrow(dem_values$data) > 0) {
        tagList(
          fluidRow(div(withSpinner(plotlyOutput("demographics_gender"), 
                                   type=4, color="#b5b5b5", size=0.5), class="col-md-6"), 
                   div(withSpinner(plotlyOutput("demographics_age"), 
                                   type=4, color="#b5b5b5", size=0.5), class="col-md-6")),
          withSpinner(plotlyOutput("demographics"), type=4, color="#b5b5b5", size=0.5)
        )
      } else {
        withSpinner(plotlyOutput("empty"), type=4, color="#b5b5b5", size=0.5)
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
    