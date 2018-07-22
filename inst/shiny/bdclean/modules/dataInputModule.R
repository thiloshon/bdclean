# Module UI function
dataInputUI <- function(id, label = "Data Input") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        useShinyjs(),
        h1("Add Occurrence Data"),
        column(
            3,
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Option 01",
                    div(class = "secondaryHeaders", h3("Option 01: From Online Database")),
                    textInput(
                        ns("scientificName"),
                        label = h3("Scientific Name:"),
                        value = "Puma concolor"
                    ),

                    sliderInput(
                        ns("recordSize"),
                        label = h3("Record Size:"),
                        min = 0,
                        max = 50000,
                        value = 500
                    ),
                    checkboxGroupInput(
                        ns("queryDB"),
                        label = h3("Online Database:"),
                        choices = list(
                            "GBIF" = 'gbif',
                            "Vertnet" = 'vertnet',
                            "Bison" = 3,
                            "Inat" = 4,
                            "eBird" = 5,
                            "Ecoengine" = 6,
                            "Vertnet" = 7
                        ),
                        selected = 'gbif'
                    ),
                    br(),
                    div(
                        id = ns("queryDatabaseDiv"),
                        class = "activeButton",
                        actionButton(ns("queryDatabase"), "Query Database", icon("download"))
                    )
                ),
                tabPanel(
                    "Option 02",
                    div(class = "secondaryHeaders", h3("Option 02: From Local Disk")),
                    div(
                        id = ns("inputFileDiv"),
                        class = "activeButton",
                        fileInput(
                            ns("inputFile"),
                            label = h3("CSV file input"),
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                        )
                    )
                ),

                div(class = "progressStep", taskItem(
                    value = 15, color = "orange",
                    "Step 1 of 6"
                ))

            )

        ),
        column(9,
               tabsetPanel(
                   type = "tabs",
                   tabPanel(
                       "Map View",
                       leafletOutput(ns("mymap"), height = "700"),
                       absolutePanel(
                           top = 60,
                           right = 20,
                           selectInput(
                               ns("mapTexture"),
                               "Map Texture",
                               choices = list(
                                   "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
                                   "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
                                   "Stamen.Toner" = "Stamen.Toner",
                                   "CartoDB.Positron" = "CartoDB.Positron",
                                   "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
                                   "Stamen.Watercolor" = "Stamen.Watercolor",
                                   "Stamen.Terrain" = "Stamen.Terrain",
                                   "Esri.WorldImagery" = "Esri.WorldImagery",
                                   "Esri.WorldTerrain" = "Esri.WorldTerrain"
                               ),
                               selected = "Stamen.Watercolor"
                           ),
                           selectInput(
                               ns("mapColor"),
                               "Points Color",
                               choices = list(
                                   "Red" = 'red',
                                   "Green" = "green",
                                   "Blue" = "blue",
                                   "Black" = "black"
                               )
                           )
                       )
                   ),
                   tabPanel("Table View",
                            DT::dataTableOutput(ns("inputDataTable")))
               ))
    )
}

dataInput <- function(input, output, session, stringsAsFactors) {
    message("triggered")
    inputData <- reactive(data.frame())
    # ------------- Add Data Module -------------------
    
    map <- leafletProxy("mymap")

    observeEvent(input$queryDatabase, {
        message("Insideo")
        withProgress(message = paste("Querying", input$queryDB, "..."), {
            if (length(input$queryDatabase == 1) &&
                input$queryDatabase == "gbif") {
                print("in")
                data <-
                    occ_search(input$scientificName, limit = input$recordSize)
                inputData <<- reactive(data$data)

            } else {
                data <-
                    spocc::occ(input$scientificName,
                               input$queryDB,
                               limit = input$recordSize)
                inputData <<- reactive(data$gbif$data$Puma_concolor)
            }
        })

        dataLoadedTask(inputData())
    })

    observeEvent(input$inputFile, {
        withProgress(message = paste("Reading", input$inputFile, "..."), {
            if (is.null(input$inputFile))
                return("No data to view")

            inputData <<- reactive(read.csv(input$inputFile$datapath))
        })

        dataLoadedTask(inputData())
    })

    observeEvent(input$mapTexture, {
        if (length(inputData()) == 0) {
            return(NULL)
        }
        leafletProxy("mymap", data = inputData()) %>%
            clearShapes() %>%
            addCircles( ~ longitude, ~ latitude, color = input$mapColor)
    })

    observeEvent(input$mapColor, {
        if (length(inputData()) == 0) {
            return(NULL)
        }
        leafletProxy("mymap", data = inputData()) %>%
            clearShapes() %>%
            addCircles( ~ longitude, ~ latitude, color = input$mapColor)
    })

    
    
    
    dataLoadedTask <- function(data) {
        leafletProxy("mymap", data = data) %>%
            clearShapes() %>%
            addCircles( ~ longitude, ~ latitude, color = input$mapColor)

        tempData <- data

        tempData <-
            tempData[, c("scientificName", "taxonRank", "eventDate", "country")]
        tempData$eventDate <- as.Date(tempData$eventDate)
        tempData <- cbind(tempData, data)

        cols <- c()
        tempData[] <- lapply(tempData, as.character)

        for (i in 1:length(names(tempData))) {
            f <- mean(sapply(tempData[,i],function(x) nchar(x)), na.rm = T)
            if(f > 50){
                cols <- c(cols, i)
            }
        }

        tempData <- tempData[, c(cols * -1)]

        output$inputDataTable <- DT::renderDataTable(DT::datatable({
            tempData
        }, options = list(scrollX = TRUE)))

        
        shinyjs::addClass(id = 'queryDatabaseDiv',
                          class = 'readyButton')
        shinyjs::removeClass(id = 'queryDatabaseDiv',
                             class = 'activeButton')

        shinyjs::addClass(id = 'inputFileDiv',
                          class = 'readyButton')
        shinyjs::removeClass(id = 'inputFileDiv',
                             class = 'activeButton')
        
        # shinyjs::addClass(id = 'dataToConfigureDiv',
        #                   class = 'completedButton')
        # shinyjs::removeClass(id = 'queryDatabaseDiv',
        #                      class = 'readyButton')

        showNotification("Read Data Succesfully", duration = 2)
    }

    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(input$mapTexture) %>%
            setView(0, 0, zoom = 2)
    })
    
    # ------------- End of Add Data Module -------------------
    # Return the reactive that yields the data frame
    
    
    
    return(inputData)
}