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

all_towns <- read_rds("data/all_towns.rds")
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

log_tooltip_html <- "
<div id='log-tooltip' width=20px>
    <b>What is a logarithmic scale?</b>
    <p>Logarithmic scales are an alternative way of presenting numerical data that can more helpfully represent relative difference between wide-ranging values.</p>

    <p>Instead of spacing values along a scale linearly (e.g., 1, 2, 3, 4), a logarithmic scale spaces out values logarithmically (e.g., 1, 10, 100, 1,000).</p>
    <img width=200px src='log_example.png'>
</div>
"

yr_tooltip_html <- "
<div id='yr-tooltip' width=20px>
    <p>You may filter the charge data based on four different years reported by the Court:</p>
    <ul>
      <li><i>Offense</i> - the year in which the  offense leading to the charge allegedly took place</li>
      <li><i>Arrest</i> - the year in which the arrest leading to the charge took place</li>
      <li><i>Filing</i> - the year in which the charge was filed</li>
      <li><i>Disposition</i> - the year in which the charge outcome was decided (e.g., dismissed, convicted, acquitted)</li>
    </ul>
</div>
"

disp_tooltip_html <- "
<div id='disp-tooltip' width=20px>
    <p>'Disposition' is the legal term for how a criminal case in Massachusetts is resolved.</p>
    <p>Many dispositions are intuitive, like charges being dismissed, the defendant being judged guilty, or the defendant being judged not guilty. However, Massachusetts courts also commonly determine cases with less intuitive dispositions:</p>
    <ul>
      <li><i>Continuance Without a Finding (CWOF)</i> - a disposition that involves the defendant admitting to sufficient facts to support a finding of guilt, but instead of entering a finding of guilt, the court continues the case until a later date and will dismiss it on that date so long as the defendant complies with specific conditions or probation. </li>
      <li><i>Nolle Prosequi</i> - a notice that the prosecution is abandoning a pending charge.</li>
    </ul>
</div>
"

# Initialization --------------------------------------------------------------

# Set ggplot settings
theme_set(theme_minimal())

theme_update(text = element_text(family="GT America"),
             plot.title = element_text(family="GT America", face="bold"))

# UI --------------------------------------------------------------------------

fluidPage(
  useShinyjs(),  # Set up shinyjs
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
  
    # Add javascript for measuring screen width
    tags$script('var screen_width = 0;
                $(document).on("shiny:connected", function(e) {
                    screen_width = window.innerWidth;
                    Shiny.onInputChange("screen_width", screen_width);
                });
                $(window).resize(function(e) {
                    screen_width = window.innerWidth;
                    Shiny.onInputChange("screen_width", screen_width);
                });
            '),
    
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
          em("Committee for Public Counsel Services (CPCS) v. Attorney General."), target="_blank"),
       
       "The court provided the data to the parties in the case with an express goal: aiding in the identification of defendants whose cases were affected by the drug lab misconduct.",
       #          
                br(),br(),
                h4("Woah, what drug scandal?"),
                "The CPCS lawsuit was the second of two suits resulting from drug lab scandals in Massachusetts; the first involved chemist Annie Dookhan and resulted in the 2012 case", a(em("Bridgeman v. District Attorney for Suffolk County."), href="https://www.aclum.org/en/cases/bridgeman-v-district-attorney-suffolk-county", target="_blank"),
       "For more information about the scandals, see the",
       a("Massachusetts Trial Court website,", href="https://www.mass.gov/info-details/drug-lab-cases-information", target="_blank"), "this", a("timeline of events,", href="https://cdn.knightlab.com/libs/timeline3/latest/embed/index.html?source=1be8ugTZsBbTWZrvfQ011dHCnJ2XXpQpwHdMyYGN23eE", target="_blank"), "or the", 
       HTML("<a href='https://www.netflix.com/title/80233339' target='_blank'>Netflix documentary <i>How to Fix a Drug Scandal</i></a>."),
       
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
                  a("GitHub.", href="https://github.com/ACLUmass/94c-dashboard", target="_blank")),
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
                                     value = 2003, min=2003, max=2014),
                        numericInput("DA_end_year", "End Year",
                                     value = 2014, min=2003, max=2014))),
                      # actionButton("DA_button", "Go"))),
            column(6, withSpinner(div(leafletOutput("DA_map"), 
                                      em("Click on district to display DA statistics."), 
                                      style="text-align: center;"), 
                                  type=4, color="#b5b5b5", size=0.5))),
          withSpinner(uiOutput("DA_dashboard"), type=4, color="#b5b5b5", size=0.5)
          ),
       
       # Scandal -----------------------------------------------------------
       tabPanel("The Drug Scandal",
                fluidRow(
                  column(9, p("Over a period of ten years, two state chemists working at Massachusetts engaged in misconduct that called into serious question the accuracy of their tests. State drug lab tests are used to identify confiscated substances and are critical information used to convict those accused of violating drug law. Tests conducted by chemist Sonja Farak between 2004 and 2013 at the Amherst Drug Lab and by Annie Dookhan between 2003 and 2011 at the Hinton State Laboratory were called into question. As a result, the Massachusetts Supreme Judicial Court ordered the dismissal of over 60,000 charges across over 38,000 cases."),
                p("For more information about the drug scandal, see the",
                  a("Massachusetts Trial Court website,", href="https://www.mass.gov/info-details/drug-lab-cases-information"), 
                  "this", a("timeline of events,", 
                            href="https://cdn.knightlab.com/libs/timeline3/latest/embed/index.html?source=1be8ugTZsBbTWZrvfQ011dHCnJ2XXpQpwHdMyYGN23eE", target="_blank"),
                  "or the", 
                  HTML("<a href='https://www.netflix.com/title/80233339'>Netflix documentary <i>How to Fix a Drug Scandal</i></a>.")), 
                style="margin-bottom:3rem"),
                column(3, div(class="scandal_numbers",
                  h1("60,245", style="margin-bottom: 0"), "charges dismissed across",
                  h1("38,008", style="margin: 0"),
                  p("cases", style="margin-bottom: 2.5rem; margin-left: 0;")
                ))
                ),
                wellPanel(id="internal_well", class="scandal_well",
                          em("Explore the effects of the scandal across various Massachusetts counties, cities, and towns. Select a geography to explore:"),
                          span(
                          radioButtons("scandal_radio", label=NULL, choices=c("State", "County", "City/Town"), inline=T), style="width:50%; margin-right:6%; display:inline-block;"),
                          span(disabled(selectizeInput("scandal_geo",label=NULL, choices=c())), style="width:43%; display:inline-block;"),
                          actionButton("scandal_button", "Go")
                          ),
                withSpinner(uiOutput("scandal_dashboard"), type=4, color="#b5b5b5", size=0.5)
       ),
       
       # Mapping charges --------------------------------------------------------------
       tabPanel("Mapping Charges",
          wellPanel(id="internal_well",
             em("Explore the 94C charges across Massachusetts. Filter the charges with the following criteria:"),
               fluidRow(
               column(6,
                 selectizeInput("map_dept", label="Agency / Department", c("All departments", all_depts))),
               column(6, selectizeInput("map_court", "Court", c("All courts"="All courts", all_courts)))),
               fluidRow(
                 column(4, selectizeInput("map_yr_type", label=HTML(paste0('Year of... <a id="yr_tooltip" data-toggle="tooltip" title="" data-original-title="', yr_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), c("Arrest", "Disposition", "Filing", "Offense"), selected="Filing")),
                 column(4, numericInput("map_start_year", "Start Year",
                              value = "2000", min="2000", max="2018")),
                 column(4, numericInput("map_end_year", "End Year",
                              value = "2014", min="2000", max="2018"))),
                actionButton("map_button", "Go")),
          div(id="map_log_div", checkboxInput("map_log",
                        span("Plot logarithmic scale",
                             a(icon("info-circle"),
                               id="log_tooltip",
                               `data-toggle`="tooltip",
                               title=log_tooltip_html)), value=T)),
          tabsetPanel(id="map_tabs",
                       
            tabPanel("Total Charges",
                     withSpinner(leafletOutput("charges_by_town"), 
                                 type=4, color="#b5b5b5", size=0.5)
            ),
            tabPanel("Charges per 1,000",
                     withSpinner(leafletOutput("charges_by_town_percap"), 
                                 type=4, color="#b5b5b5", size=0.5)
            ),
            tabPanel("Prosecuting Possession",
                     withSpinner(leafletOutput("charge_types_by_town"), 
                                 type=4, color="#b5b5b5", size=0.5)
            )
          )
       ),
       
       # Disposition ------------------------------------------
       tabPanel("Disposition", 
                wellPanel(id="internal_well",
                          em("Explore the dispositions", HTML(paste0('<a id="disp_tooltip" data-toggle="tooltip" title="" data-original-title="', disp_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), " of the 94C charges in Massachusetts. Filter the charges with the following criteria:"),
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
                      column(4, selectizeInput("disp_yr_type", label=HTML(paste0('Year of... <a id="yr_tooltip" data-toggle="tooltip" title="" data-original-title="', yr_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), 
                                               c("Arrest", "Disposition", "Filing", "Offense"), selected="Filing")),
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
       
       # Charge type ------------------------------------------
       tabPanel("Charge Type", 
                wellPanel(id="internal_well",
                          em("Explore the types of the 94C charges in Massachusetts. Filter the charges with the following criteria:"),
                          fluidRow(
                            fluidRow(
                              column(6, selectizeInput("charge_city", "Town/City", c("All cities and towns", all_towns))),
                              column(6, selectizeInput("charge_dept", label="Agency / Department", c("All departments", all_depts)))
                            ),
                            fluidRow(
                              column(6, selectizeInput("charge_court", "Court", c("All courts"="All courts", all_courts))),
                              column(6, selectizeInput("charge_disp", label=HTML(paste0('Disposition <a id="disp_tooltip" data-toggle="tooltip" title="" data-original-title="', disp_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), c("All dispositions", all_disps)))
                            ),
                            fluidRow(
                              column(4, selectizeInput("charge_yr_type", label=HTML(paste0('Year of... <a id="yr_tooltip" data-toggle="tooltip" title="" data-original-title="', yr_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), c("Arrest", "Disposition", "Filing", "Offense"), selected="Filing")),
                              column(4 ,numericInput("charge_start_year", "Start Year",
                                                     value = "2000", min="2000", max="2018")),
                              column(4, numericInput("charge_end_year", "End Year",
                                                     value = "2014", min="2000", max="2018"))),
                            actionButton("charge_button", "Go"))
                ),
                h2(textOutput("charge_count_str"), align="center"),
                p(textOutput("charge_str", inline=T), align="center"),
                withSpinner(plotlyOutput("charge"), type=4, color="#b5b5b5", size=0.5)
       ),
       
       # Drug Class ------------------------------------------
       tabPanel("Drugs by Class", 
                wellPanel(id="internal_well",
                          
                          em("Explore different classes of drug involved in 94C violations over time. Filter the charges with the following criteria:"),
                          fluidRow(
                            fluidRow(
                              column(6, selectizeInput("class_city", "Town/City", c("All cities and towns", all_towns))),
                              column(6, selectizeInput("class_dept", label="Agency / Department", c("All departments", all_depts)))
                            ),
                            fluidRow(
                              column(6, selectizeInput("class_court", "Court", c("All courts"="All courts", all_courts))),
                              column(6, selectizeInput("class_disp", label=HTML(paste0('Disposition <a id="disp_tooltip" data-toggle="tooltip" title="" data-original-title="', disp_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), c("All dispositions", all_disps)))
                            )
                          ),
                          actionButton("class_button", "Go")),
                div(id="class_desc", 
                    em("Drug class examples from the", a("Massachusetts General Laws", href="https://malegislature.gov/laws/generallaws/parti/titlexv/chapter94c/section31", target="_blank"), ":",
                tags$ul(
                  tags$li("Class A - e.g., heroin, fentanyl, ecstasy"),
                  tags$li("Class B - e.g., cocaine, LSD, methamphetamines, Oxycodone"),
                  tags$li("Class C - e.g., magic mushrooms, Valium, Vicodin"),
                  tags$li("Class D - marijuana"),
                  tags$li("Class E - other prescription drugs"),
                  ))),
                withSpinner(plotlyOutput("class_v_time"), type=4, color="#b5b5b5", size=0.5)
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
                              column(6, selectizeInput("time_disp", label=HTML(paste0('Disposition <a id="disp_tooltip" data-toggle="tooltip" title="" data-original-title="', disp_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), c("All dispositions", all_disps)))
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
                                column(6, selectizeInput("time_disp2", label=HTML(paste0('Disposition <a id="disp_tooltip" data-toggle="tooltip" title="" data-original-title="', disp_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), c("All dispositions", all_disps)))
                              ),
                              selectizeInput("time_charge2", label="Charge", c("All charges", all_charges))
                            )),
                          actionButton("time_button", "Go")),
                radioButtons("year_type", HTML(paste0('Plot by year of... <a id="yr_tooltip" data-toggle="tooltip" title="" data-original-title="', yr_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), choices=c("Arrest", "Disposition", "Filing", "Offense"), 
                             selected="Filing", inline=T),
                withSpinner(plotlyOutput("stops_v_time"), type=4, color="#b5b5b5", size=0.5),
                em("Please note both that (1) the court only provided partial-year data for charges filed after 2014, and (2) there are lags between offense date, arrest date, filing date, and disposition date (e.g., a charge filed in 2003 might have result from a 2002 arrest, and might not be decided until 2006). As such, dropoffs at the beginning or end of the year ranges shown above do not necessarily reflect the entire picture of drug prosecution in those years.", 
                style="text-align:center;")),
       
       # Demographics  ------------------------------------------
       tabPanel("Demographics", 
                wellPanel(id="internal_well",
                          em("Explore the age and gender of individuals charged under Chapter 94C in Massachusetts (race was not reported by the Court). Filter with the following criteria:"),
                          fluidRow(
                            fluidRow(
                              column(6, selectizeInput("dem_city", "Town/City", c("All cities and towns", all_towns))),
                              column(6, selectizeInput("dem_dept", label="Agency / Department", c("All departments", all_depts)))
                            ),
                            fluidRow(
                              column(4, selectizeInput("dem_court", "Court", c("All courts"="All courts", all_courts))),
                              column(4, selectizeInput("dem_charge", label="Charge", c("All charges", all_charges))),
                              column(4, selectizeInput("dem_disp", label=HTML(paste0('Disposition <a id="disp_tooltip" data-toggle="tooltip" title="" data-original-title="', disp_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), c("All dispositions", all_disps)))
                            ),
                            fluidRow(
                              column(4, selectizeInput("dem_yr_type", label=HTML(paste0('Year of... <a id="yr_tooltip" data-toggle="tooltip" title="" data-original-title="', yr_tooltip_html, '"><i class="fa fa-info-circle" role="presentation" aria-label="info-circle icon"></i></a>')), c("Arrest", "Disposition", "Filing", "Offense"), selected="Filing")),
                              column(4, numericInput("dem_start_year", "Start Year",
                                           value = "2000", min="2000", max="2018")),
                              column(4, numericInput("dem_end_year", "End Year",
                                           value = "2014", min="2000", max="2018"))),
                            actionButton("dem_button", "Go"))
                ),
                # h2(textOutput("disp_count_str"), align="center"),
                # p(textOutput("disp_str", inline=T), align="center"),
                withSpinner(uiOutput("dem_dashboard"), type=4, color="#b5b5b5", size=0.5)
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
        p("Please contact data4justice@aclum.org with questions.", align="center", style="opacity: 0.6;")
        # p("Icons by FontAwesome and", a("Flaticon", href="https://www.flaticon.com/free-icon/police_811976"), align="center", style="opacity: 0.6; margin-top: -10px; font-size: 1rem; margin-bottom: 0px")
    )
  )