library(tidyverse)
library(shiny)
library(shinycssloaders)
library(showtext)
library(plotly)
library(shinyjs)
library(data.table)
library(readr)
library(leaflet)

combined94c_data <- fread("94c_combined.csv")
charge_cats <- read_csv("data/94c_charge_codes.csv")
disp_cats <- read_csv("data/94c_dispositions.csv")

all_towns <- combined94c_data[order(jurisdiction), jurisdiction] %>%
  unique()
all_depts <- read_rds("data/all_depts.rds")
all_courts <- combined94c_data[order(court), court] %>%
  unique()
all_charges <- charge_cats$charge_cat
all_disps <- readRDS("data/disp_colors.rds") %>% names()

# Define list of counties
all_DAs <- c("Berkshire", "Bristol", "Cape and Islands", "Essex", 
             "Hampden", "Middlesex", "Norfolk", "Northwestern",
              "Plymouth", "Suffolk", "Worcester")

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

# all_agencies <- readRDS("data/all_agencies.RDS")
# all_towns <- readRDS("data/all_towns.rds")
# 
# all_outcomes <- c("All outcomes"="All outcomes", 
#                   "Warning"="Warn", 
#                   "Civil Citation"="Civil",
#                   "Criminal Citation"="Crim", 
#                   "Arrest"="Arrest")
# 
log_tooltip_html <- "
<div id='log-tooltip' width=20px>
    <b>What is a logarithmic scale?</b>
    <p>Logarithmic scales are an alternative way of presenting numerical data that can more helpfully represent relative difference between wide-ranging values.</p>

    <p>Instead of spacing values along a scale linearly (e.g., 1, 2, 3, 4), a logarithmic scale spaces out values logarithmically (e.g., 1, 10, 100, 1,000).</p>
    <img width=200px src='log_example.png'>
</div>
"

# officer_tooltip_html <- "
# <div id='officer-id-tooltip' width=20px>
#     The format of officer identifiers varies widely between law enforcement agencies - some agencies just use numbers, some include letters, etc. The options presented here reflect the IDs exactly as reported by MassDOT. We anticipate some may be typos.
# </div>
# "

# Initialization --------------------------------------------------------------

# Set ggplot settings
theme_set(theme_minimal())

theme_update(text = element_text(family="GT America"),
             plot.title = element_text(family="GT America", face="bold"))

# UI --------------------------------------------------------------------------

fluidPage(
  # useShinyjs(),  # Set up shinyjs
  theme = "94c_app.css",
    
    # Add favicon          
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon.png")
    ),
  
    # Add javascript for log tooltip
    tags$script(HTML('
         $( document ).on("shiny:sessioninitialized", function(event) {
              $(\'a[data-toggle="tooltip"]\').tooltip({
                  animated: "fade",
                  placement: "bottom",
                  html: true
              });
         });'
    )),
    
    # App title
    div(id="title",
        titlePanel("Prosecuting Drugs in Massachusetts, 2003 - 2014")
    ),
    
    div(navlistPanel(widths = c(3, 9), id="panels", 
    
       # About ----------------------------------------------------
       tabPanel("About", 
                
          h3("Explore the History of Drug Prosecution in Massachusetts"),
          br(),
          p("Explore the complete record of drug-related criminal charges filed between 2003 and 2014 and prosecuted under",
            a("MGL Chapter 94C,", href="https://malegislature.gov/laws/generallaws/parti/titlexv/chapter94c"), 
             "as documented in data obtained from the Massachusetts Trial Court."),
       # 
       # div(id="dev-wait",
       #     wellPanel(
       #       icon('exclamation-triangle'),
       #       h4("Disclaimer"),
       #       em("Based on discussions with MassDOT, ACLUM understands that MassDOT’s historical record of traffic stops can change due to updates in reporting requirements, corrections of records, delays by municipalities in reporting their warnings and citations, and other factors. Therefore, the record of traffic stops presented here reflects only the MassDOT database as it was on February 4, 2021.",)
       #     )
       # ),
       
       h3("About the Data"),
       
       h4("Where did the data come from?"),
       "The data available here are the result of a 2017 lawsuit, brought in response to the misconduct of state drug-lab chemist Sonja Farak and subsequent mishandling by the Massachusetts Attorney General's Office:",
        a(href="https://www.aclum.org/en/cases/committee-public-counsel-services-v-attorney-general",
          em("Committee for Public Counsel Services (CPCS) v. Attorney General.")),
       
       "For more information about the drug scandal, see the",
         a("Massachusetts Trial Court website,", href="https://www.mass.gov/info-details/drug-lab-cases-information"), "or the", 
         HTML("<a href='https://www.netflix.com/title/80233339'>Netflix documentary <i>How to Fix a Drug Scandal</i></a>."),
       #          
                br(),br(),
                h4("What's included?"),
                "All drug charges filed by Massachustts prosecutors as documented by the Trial Court between 2003 and 2014. The dataset includes information regarding the criminal statute under Chapter 94C applicable to each charge; the years of offense, arrest, case filing, and case disposition (i.e. outcome); the  location, jurisdiction, arresting department, and presiding court for each case; the age and gender of the individuals charged; and the disposition of each charge.",
                
                br(),br(),
                h4("What's not included?"),
                "As required and verified by the Court, the data available here have been scrubbed of any personally identifiable information regarding the individuals involved in the criminal cases.",
                
                br(),br(),
                h4("Why does it matter?"),
                
                "Not only did these data enable the dismissal of tens of thousands of cases in response to misconduct by the AGO and drug chemists",
                  a(href="https://www.bostonglobe.com/metro/2019/09/25/charges-tossed-because-they-were-tainted-former-amherst-lab-chemist-misconduct/MUPgdHeLy8bdrzl5KGtvIN/story.html",
                    target="_blank",
                    "Sonja Farak"), "and",
                a(href="https://www.npr.org/sections/thetwo-way/2017/04/20/524894955/massachusetts-throws-out-more-than-21-000-convictions-in-drug-testing-scandal", "Annie Dookhan;", target="_blank"),
                "this dataset documents the intricacies of drug prosecution across Massachusetts for over a decade. Notably, the timeframe captured by the data includes Massachusetts'",
                a("decriminalization", href="https://archive.boston.com/news/local/articles/2008/11/05/voters_approve_marijuana_law_change/", target="_blank"),
                "of marijuana in 2008, enabling direct analysis of how drug prosecution changed in response to sweeping policy change.",
    
                br(),br(),
                h4("Where can I get it?"),
    
                "The data sourced for all visualizations on this site are available for download",
                  actionLink("link_to_download", "here."),
       
                
                h3("Source Code"),
                p("Interested programmers can view the source code for this app, written in R, on",
                  a("GitHub (BROKEN).", href="#")),
       br(),br()
       ),
       
       # Download data --------------------------------------------
       tabPanel("Download the Data",
                 p("You may download a CSV of the full Massachusetts Trial Court dataset, as provided to the parties in", 
                   em("Committee for Public Counsel Services (CPCS) v. Attorney General"), 
                   "but scrubbed of personally identifiable information, here:",
                   style="text-align: center"),
                downloadButton("download_button", "Download", style="margin-left: auto; margin-right: auto; display: grid; width: 100px;"),
                div(em("File size: 166 MB"), style="text-align: center; margin-top: 1rem")
       ),
       
       "Explore the Data:",
       
       # District Attorneys -----------------------------------------------------------
       
       tabPanel("District Attorney Lookup", 
          fluidRow(id="DA_control",
            column(6, style="display: flex; align-items: center;",
            wellPanel(id="internal_well",
                      selectizeInput("DA_county", "District", choices=all_DAs),
                        numericInput("DA_start_year", "Start Year",
                                     value = "2003", min="2003", max="2014"),
                        numericInput("DA_end_year", "End Year",
                                     value = "2014", min="2003", max="2014"))),
                      # actionButton("DA_button", "Go"))),
            column(6, withSpinner(div(leafletOutput("DA_map"), 
                                      em("Click to select county."), style="text-align: center;"), 
                                  type=4, color="#b5b5b5", size=0.5))),
          withSpinner(uiOutput("DA_dashboard"), type=4, color="#b5b5b5", size=0.5)
          ),
       
       # Mapping charges --------------------------------------------------------------
       tabPanel("Mapping Charges",
          wellPanel(id="internal_well",
             em("Explore the 94C charges across Massachusetts. Filter the charges with the following criteria:"),
             fluidRow(
               fluidRow(
               column(4,
                 selectizeInput("map_dept", label="Agency / Department", c("All departments", all_depts))),
               column(4, selectizeInput("map_court", "Court", c("All courts"="All courts", all_courts))),
               column(4, selectizeInput("map_charge", label="Charge", c("All charges", all_charges)))),
               fluidRow(
                 column(4, selectizeInput("map_yr_type", label="Year of...", c("Arrest", "Disposition", "Filing", "Offense"), selected="Filing")),
                 column(4, numericInput("map_start_year", "Start Year",
                              value = "2000", min="2000", max="2018")),
                 column(4, numericInput("map_end_year", "End Year",
                              value = "2014", min="2000", max="2018"))),
               fluidRow(
                 column(6, radioButtons("map_radio", "Value Type",
                              choiceValues=c("Total charges",
                                        "Charges per capita"),
                              choiceNames=c("Total charges",
                                             "Charges per 1,000 population"),
                              selected="Total charges", inline=F)),
                 column(6, div(id="map_log_span",
                     div(tags$b("Numeric Scale")),
                      checkboxInput("map_log",
                                    span("Plot logarithmic scale",
                                         a(icon("info-circle"),
                                           id="log_tooltip",
                                           `data-toggle`="tooltip",
                                           title=log_tooltip_html)),
                                    value=T)
                      )))),
               actionButton("map_button", "Go")),
         withSpinner(leafletOutput("charges_by_town"), type=4, color="#b5b5b5", size=0.5)
       ),
       
       # Disposition ------------------------------------------
       tabPanel("Disposition", 
                wellPanel(id="internal_well",
                          em("Explore the dispositions (outcomes) of the 94C charges in Massachusetts. Filter the charges with the following criteria:"),
                  fluidRow(
                    fluidRow(
                      column(6, selectizeInput("disp_city", "Town/City", c("All cities and towns", all_towns))),
                      column(6, selectizeInput("disp_dept", label="Agency / Department", c("All departments", all_depts)))
                    ),
                    fluidRow(
                      column(6, selectizeInput("disp_court", "Court", c("All courts"="All courts", all_courts))),
                      column(6, selectizeInput("disp_charge", label="Charge", c("All charges", all_charges)))
                    ),
                  fluidRow(
                      column(4, selectizeInput("disp_yr_type", label="Year of...", c("Arrest", "Disposition", "Filing", "Offense"), selected="Filing")),
                      column(4 ,numericInput("disp_start_year", "Start Year",
                                value = "2000", min="2000", max="2018")),
                      column(4, numericInput("disp_end_year", "End Year",
                                   value = "2014", min="2000", max="2018"))),
                actionButton("disp_button", "Go"))
                ),
                h2(textOutput("disp_count_str"), align="center"),
                p(textOutput("disp_str", inline=T), align="center"),
                withSpinner(plotlyOutput("disposition"), type=4, color="#b5b5b5", size=0.5)
                ),
       
       # Charges over time ------------------------------------------
       tabPanel("Compare charges over time", 
                wellPanel(id="internal_well",
                          
                          em("Explore 94C charging trends over time. Filter the charges with the following criteria:"),
                          fluidRow(
                            fluidRow(
                              column(6, selectizeInput("time_city", "Town/City", c("All cities and towns", all_towns))),
                              column(6, selectizeInput("time_dept", label="Agency / Department", c("All departments", all_depts)))
                            ),
                            fluidRow(
                              column(6, selectizeInput("time_court", "Court", c("All courts"="All courts", all_courts))),
                              column(6, selectizeInput("time_disp", label="Disposition", c("All dispositions", all_disps)))
                            ),
                            selectizeInput("time_charge", label="Charge", c("All charges", all_charges))
                          ),
                          checkboxInput("compare_time", label="Select a second set of criteria to compare?", value=F),
                          conditionalPanel(
                            condition = "input.compare_time == true",
                            fluidRow(
                              fluidRow(
                                column(6, selectizeInput("time_city2", "Town/City", 
                                               c("All cities and towns", all_towns))),
                                column(6, selectizeInput("time_dept2", label="Agency / Department", 
                                               c("All departments", all_depts)))
                              ),
                              fluidRow(
                                column(6, selectizeInput("time_court2", "Court", c("All courts"="All courts", all_courts))),
                                column(6, selectizeInput("time_disp2", label="Disposition", c("All dispositions", all_disps)))
                              ),
                              selectizeInput("time_charge2", label="Charge", c("All charges", all_charges))
                            )),
                          actionButton("time_button", "Go")),
                radioButtons("year_type", "Plot by year of:", choices=c("Arrest", "Disposition", "Filing", "Offense"), 
                             selected="Filing", inline=T),
                withSpinner(plotlyOutput("stops_v_time"), type=4, color="#b5b5b5", size=0.5)
       ),
       
       # Demographics  ------------------------------------------
       tabPanel("Demographics", 
                wellPanel(id="internal_well",
                          em("Explore the demographics of individuals charged under Chapter 94C in Massachusetts. Filter with the following criteria:"),
                          fluidRow(
                            fluidRow(
                              column(6, selectizeInput("dem_city", "Town/City", c("All cities and towns", all_towns))),
                              column(6, selectizeInput("dem_dept", label="Agency / Department", c("All departments", all_depts)))
                            ),
                            fluidRow(
                              column(4, selectizeInput("dem_court", "Court", c("All courts"="All courts", all_courts))),
                              column(4, selectizeInput("dem_charge", label="Charge", c("All charges", all_charges))),
                              column(4, selectizeInput("dem_disp", label="Disposition", c("All dispositions", all_disps)))
                            ),
                            fluidRow(
                              column(4, selectizeInput("dem_yr_type", label="Year of...", c("Arrest", "Disposition", "Filing", "Offense"), selected="Filing")),
                              column(4, numericInput("dem_start_year", "Start Year",
                                           value = "2000", min="2000", max="2018")),
                              column(4, numericInput("dem_end_year", "End Year",
                                           value = "2014", min="2000", max="2018"))),
                            actionButton("dem_button", "Go"))
                ),
                # h2(textOutput("disp_count_str"), align="center"),
                # p(textOutput("disp_str", inline=T), align="center"),
                fluidRow(div(withSpinner(plotlyOutput("demographics_gender"), 
                                               type=4, color="#b5b5b5", size=0.5), class="col-md-6"), 
                         div(withSpinner(plotlyOutput("demographics_age"), 
                                               type=4, color="#b5b5b5", size=0.5), class="col-md-6")),
                withSpinner(plotlyOutput("demographics"), type=4, color="#b5b5b5", size=0.5)
       )
       
       
       # Stops by offense ------------------------------------------
       # tabPanel("Stops by offense"#, 
                # wellPanel(id="internal_well",
                #           fluidRow(
                #             column(4, selectizeInput("offense_town", "Town/City", c("All cities and towns", all_towns))),
                #             column(4, selectizeInput("offense_agency", 
                #                            label="Agency/Department", c("All agencies", all_agencies))),
                #             column(4, div(id="custom_label_div",
                #                 tags$b("Officer ID"),
                #                 a(icon("info-circle"), id="officer_tooltip",
                #                   `data-toggle`="tooltip", title=officer_tooltip_html),
                #                 selectizeInput("offense_officer", 
                #                                label=NULL, 
                #                                c("Loading, please wait..." = ""))))
                #             ),
                #           
                #           splitLayout(
                #             dateInput("offense_start_date", "Start Date",
                #                       value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                #             dateInput("offense_end_date", "End Date",
                #                       value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                #           actionButton("offense_button", "Go")),#style="text-align: center;"),
                # withSpinner(plotlyOutput("offenses"), type=4, color="#b5b5b5", size=0.5)
       # ),
                
       # Agencies ------------------------------------------
       # "Agency Lookup",
       # tabPanel("Agency Lookup"#, 
                # wellPanel(id="internal_well",
                #   selectizeInput("agency_agency", 
                #                  label="Agency/Department", 
                #                  c(all_agencies)),
                #   splitLayout(
                #     dateInput("agency_start_date", "Start Date",
                #               value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                #     dateInput("agency_end_date", "End Date",
                #               value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                #   actionButton("agency_button", "Go")),
                # withSpinner(uiOutput("agency_dashboard"), type=4, color="#b5b5b5", size=0.5)
       # ),
    
       # TOWN LOOKUP ----------------------------------------------
       # "Town Lookup",
       
       # Town overview ----------------------------------------------
       # tabPanel("Town Lookup"#, 
                # wellPanel(id="internal_well",
                #           selectizeInput("townover_town", "Town/City", all_towns),
                #           splitLayout(
                #             dateInput("townover_start_date", "Start Date",
                #                       value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                #             dateInput("townover_end_date", "End Date",
                #                       value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                #           actionButton("townover_button", "Go")),
                # withSpinner(uiOutput("townover_dashboard"), type=4, color="#b5b5b5", size=0.5)
                # ),
       
       # Town stops by race ---------------------------------------
       # tabPanel("Race of Stops by Town"#, 
                # wellPanel(id="internal_well",
                #   splitLayout(
                #     selectizeInput("town_town", "Town/City", all_towns),
                #     selectizeInput("town_agency", 
                #                    label="Agency/Department",
                #                    choices=c("All agencies", all_agencies))
                #   ),
                #   splitLayout(
                #     dateInput("town_race_start_date", "Start Date",
                #               value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                #     dateInput("town_race_end_date", "End Date",
                #               value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                #   actionButton("town_button", "Go")
                # ),
                # hidden(img(src="race_legend.png", id="town_race_legend")),
                # withSpinner(plotlyOutput("town_demog"), type=4, color="#b5b5b5", size=0.5)
                # ),
    
       # "Officer ID Lookup",
       # Officer stops by race ------------------------------------
       # tabPanel("Race of Stops by Officer"#, 
                # wellPanel(id="internal_well",
                #   splitLayout(
                #     selectizeInput("officer_agency", 
                #                    label="Agency / Department", all_agencies),
                #     div(id="custom_label_div",
                #         tags$b("Officer ID"),
                #         a(icon("info-circle"), id="officer_tooltip",
                #           `data-toggle`="tooltip", title=officer_tooltip_html),
                #         selectizeInput("officer_officer", 
                #                        label=NULL, 
                #                        choices=c("Loading, please wait..." = "")))
                #     ),
                #   splitLayout(
                #     dateInput("officer_race_start_date", "Start Date",
                #               value = "2002-01-01", min="2002-01-01", max="2021-02-04"),
                #     dateInput("officer_race_end_date", "End Date",
                #               value = "2021-02-04", min="2002-01-01", max="2021-02-04")),
                #   actionButton("officer_button", "Go")
                # ),
                # hidden(img(src="race_legend.png", id="officer_race_legend")),
                # withSpinner(plotlyOutput("officer_demog"), type=4, color="#b5b5b5", size=0.5)
                # )
                # 
                # 
        )
        # )
       ),
    
    div(id="footer",
        # hr(),
        div(align="center",
            a(href="https://www.aclum.org/", target="_blank",
              img(src="Logo_CMYK_Massachusetts_Massachusetts.png", height="50px", 
                  style="display: inline; margin: 10px;")),
            a(href="https://www.data.aclum.org/",  target="_blank",
              img(src="D4J-logo.png", height="50px", 
                  style="display: inline; margin: 10px;"))),
        p("Please contact data4justice@aclum.org with questions.", align="center", style="opacity: 0.6;"),
        p("Icons by FontAwesome and", a("Flaticon", href="https://www.flaticon.com/free-icon/police_811976"), align="center", style="opacity: 0.6; margin-top: -10px; font-size: 1rem; margin-bottom: 0px")
    )
  )