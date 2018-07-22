library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)

shinyUI(dashboardPage(
    #Header Title
    dashboardHeader(title = "bdclean"),
    
    #Sidebar
    dashboardSidebar(
        sidebarMenu(
            id = "sideBar",
            menuItem(
                "Add Data",
                tabName = "add",
                icon = icon("plus-circle")
            ),
            menuItem(
                "Configure Cleaning",
                tabName = "configure",
                icon = icon("wrench")
            ),
            menuItem("Flag & Clean", tabName = "flag", icon = icon("flag")),
            menuItem(
                "Artifacts & Documentation",
                tabName = "document",
                icon = icon("file")
            ),
            menuItem("Citations", tabName = "citTab", icon = icon("bookmark"))
        )
    ),
    
    #Dashboard Tabs
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "checkbox.css")
        ),
        useShinyjs(),
        tabItems(
            tabItem("add",
                    fluidRow(column(
                        12,
                        dataInputUI("datafile", "User data (.csv format)"),
                        div(
                            id = "dataToConfigureDiv", 
                            class = "activeButton",
                            actionButton("dataToConfigure", "Next: Configure Cleaning")
                        )
                       
                        
                    ))),
            tabItem("configure",
                    fluidRow(column(
                        12,
                        h1("Configure Cleaning"),
                        column(
                            12,
                            tabsetPanel(
                                type = "tabs",
                                tabPanel(
                                    "Option 01",
                                    div(class = "secondaryHeaders", h3("Option 01: Questionnaire")),
                                    helpText("Note: If you have limited knowledge in Biodiversity data, this option is preferred.",
                                             "Answer a few questions and let bdclean take care of the cleaning."),
                                   
                                    
                                    # -------------------------------
                                    
                                    uiOutput("questionnaire")
                                    
                                    
                                    # -------------------------------
                                ),
                                tabPanel(
                                    "Option 02",
                                    div(class = "secondaryHeaders", h3("Option 02: Customized Checks")),
                                    helpText("Note: Select the quality checks you prefer and 
                                             continue cleaning with just those checks"),
                                   
                                    uiOutput("qualityChecks")
                                ),
                                tabPanel(
                                    "Option 03",
                                    div(class = "secondaryHeaders", h3("Option 03: Cleaning Templates")),
                                    helpText("Note: Choose the cleaning, customized for special domains and needs"),
                                    
                                    uiOutput("domainCleaning")
                                ),
                                div(class = "progressStep", taskItem(
                                    value = 30, color = "green",
                                    "Step 2 of 6"
                                ))
                            ),
                            div(class = "activeButton", actionButton("configureToFlag", "Next: Flagging"))
                            
                        )
                        
                    ))),
            
            tabItem("flag",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Flag Data"),
                            br(),
                            
                            h4("Input Data"),
                            
                            div(
                                class = "center",
                                fluidRow(
                                    infoBox("# of Records", textOutput("inputDataRows"), icon = icon("list-ol")),
                                    infoBox(
                                        "# of Fields",
                                        textOutput("inputDataColumns"),
                                        icon = icon("th-list"),
                                        color = "purple"
                                    ),
                                    infoBox(
                                        "# of Unique Scientific Names",
                                        textOutput("inputDataSpecies"),
                                        icon = icon("paw"),
                                        color = "yellow"
                                    )
                                ),
                                
                                div(class = "progressStep", taskItem(
                                    value = 45, color = "yellow",
                                    "Step 3 of 6"
                                ))                                ,
                                
                                fluidRow(actionButton("flagButton", label = "Flag Data"))
                            ),
                            
                            br(),
                            
                            uiOutput("flaggedContentUI"),
                            
                            uiOutput("cleanedResultsUI")
                        )
                    ))),
            tabItem("document",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Artifacts and Reports"),
                            br(),
                            uiOutput("documentContentUI")
                        )
                    )))
            
        )
    )
))
