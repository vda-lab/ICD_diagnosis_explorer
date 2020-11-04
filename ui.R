


shinyUI(

  ####################################
  # Dashboard structure
  ###################################
  dashboardPagePlus(

    dashboardHeaderPlus(
      fixed = TRUE,
      title = tagList(
        span(class = "logo-lg", "Diagnosis explorer"), 
        img(src = "https://image.flaticon.com/icons/svg/1021/1021799.svg")),
      left_menu = tagList(
        tags$head(tags$style(HTML('.dropdown-toggle { color: #333 !important; }')))
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("About ", icon = icon("question-circle"), tabName = "about")
      )
      
    ),
    dashboardBody(
      
      useShinyjs(),
      
      shinyDashboardThemes(
        theme = "onenote"
      ),
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      
        ####################################
        # Dashboard
        ###################################
        tabItems(
          tabItem("dashboard",
        
            ####################################
            # Information Box
            ###################################
            fluidRow(
              column(12,
                     boxPlus(
                       title = "Description", 
                       closable = FALSE, 
                       width = NULL,
                       status = "info",
                       solidHeader = FALSE, 
                       collapsible = TRUE,
                       HTML(
                            "<p>Diagnosis explorer is an interface to investigate diagnosis profiles of ~60,000 patients 
                                from <a href='https://mimic.physionet.org/' target='_blank'>MIMIC </a> database. <br>
                                The information  analyzed is contained in table 
                                <a href='https://mimic.physionet.org/mimictables/diagnoses_icd/' target='_blank'>DIAGNOSES_ICD</a>
                                which includes the <b>ICD Code</b> of patients and the order (<b>SEQ_NUM</b>) 
                                in which the ICD diagnoses relate to the patient. ICD diagnoses are ordered 
                                by priority - and the order does have an impact on the reimbursement for treatment.</p>"
                            )
                    )
              )
            ),
            ####################################
            # First row: Settings weight and height
            ###################################
            fluidRow( 
              column(10,
                     selectInput("icd", 
                                 "Select a ICD Code to explore:", 
                                 icd_choices, 
                                 selected = icd_choices[354],
                                 width = "100%")
              )
            ),
            ####################################
            # Title: Add the ICD code
            ###################################
            titlePanel(textOutput('title')),
            
            ####################################
            # Second row: Network plot and Beeswarm 
            ###################################
            fluidRow(
              column(12,
                     boxPlus(
                       title = "Selection", 
                       closable = FALSE, 
                       width = NULL,
                       status = "info",
                       solidHeader = FALSE, 
                       collapsible = TRUE,
                       HTML(
                         "<p>Select a group or groups to analyze the network and press the button under the 
                         network (“Select patients”) to analyze them. You can use the drop-down menu “Select by the group” or 
                         double-click on the nodes. Multiselect is available by pressing (CTRL in Windows/Linux 
                         and ⌘ in Mac)<br>
                         The node size can be controlled through the slides located on top of the navigation bar “Node size”.
                         </p>"
                       )
                     )
              )
            ),
            ####################################
            # Second row: Network plot and Beeswarm 
            ###################################
            
            fluidRow(
              column(4,             
                dropdownButton(
                  circle = FALSE,
                  label = "Change node size",
                  badgeStatus = NULL,
                  sliderInput(
                    inputId = "node_size",
                    label = "Node Size Network",
                    min = 1, max = 100, value = 25
                  )
                )
              )
            ),
            fluidRow(column(12, visNetworkOutput("network_id", height = "640px") )),
            fluidRow(column(12, 
                            actionBttn(input = "select", 
                                       style = "gradient", 
                                       label = "Select patients", 
                                       color = "primary",
                                       size = "md",
                                       icon = icon("mouse-pointer"),
                                       block = TRUE) )),
            ####################################
            # Individual plots 
            ###################################
            fluidRow(
              column(12,
                     HTML("<br>"),
                     boxPlus(
                       id = "bar_chart_box",
                       title = "Bar chart", 
                       closable = FALSE, 
                       width = NULL,
                       status = "info",
                       solidHeader = FALSE, 
                       collapsible = TRUE,
                       HTML("<p> The bar chart illustrates the distributions of ICD codes by position. 
                        Note that diagnosis codes (ICD)  are ordered by priority. Therefore all patients have a 
                        diagnostic code in position one but not in the last position of series. <br>
                        Colored bars represent ICD codes which appear with a proportion of 10% in the same position of the ranking.
                        This value can be increased or decreased using the slider “Bar-chart filter”)</p>")
                     )
              )
            ),
            fluidRow(
              column(4, 
                dropdownButton(
                  circle = FALSE,
                  label = "ICD codes displayed",
                  sliderInput(
                    inputId = "obs",
                    label = "Percentage of ICD Codes common in identical position",
                    min = 1, max = 100, value = 10, post  = " %"
                  )
                )
              )
            ),
            ###################################
            fluidRow( 
              column(6,
                fluidRow(
                  column(9, h3(textOutput("summ_codes1")) ),
                  column(3, 
                         HTML("<br>"), 
                         actionBttn(input = "close1", style = "gradient", color = "warning", size = "xs", icon = icon("times"))
                  ),
                  column(12,ggvisOutput("dist_codes1") )
                )
              ),
              column(6, 
               fluidRow(
                 column(9, h3(textOutput("summ_codes2")) ),
                 column(3, 
                        HTML("<br>"), 
                        actionBttn(input = "close2", style = "gradient", color = "warning", size = "xs", icon = icon("times"))
                 ),
                 column(12,ggvisOutput("dist_codes2") )
               )
              ),
              column(6,
               fluidRow(
                 column(9, h3(textOutput("summ_codes3")) ),
                 column(3, 
                        HTML("<br>"), 
                        actionBttn(input = "close3", style = "gradient", color = "warning", size = "xs", icon = icon("times"))
                 ),
                 column(12,ggvisOutput("dist_codes3") )
               )
              ),
              column(6,
               fluidRow(
                 column(9, h3(textOutput("summ_codes4")) ),
                 column(3, 
                        HTML("<br>"), 
                        actionBttn(input = "close4", style = "gradient", color = "warning", size = "xs", icon = icon("times"))
                 ),
                 column(12,ggvisOutput("dist_codes4") )
               )
              ) 
            )
        ), # End dahsboard
        tabItem("about", 
           p("To Be Continued")
        )
      ) # End tabs
    ) # End body
  ) # End dashborad page
) # End shiny UI