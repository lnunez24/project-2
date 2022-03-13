library(ggplot2)
library(scales)
library(lubridate)
library(dplyr)
library(shiny)
library(leaflet)
library(stringr)
library(forcats)
library(DT)
options(digits = 6)
print("loaded packages")
# Read in files
A <- read.table(file="1.csv",sep=",",header=TRUE)
print("read in file1")
B <- read.table(file="2.csv",sep=",",header=TRUE)
print("read in file2")
C <- read.table(file="3.csv",sep=",",header=TRUE)
print("read in file3")
D <- read.table(file="4.csv",sep=",",header=TRUE)
print("read in file4")
E <- read.table(file="5.csv",sep=",",header=TRUE)
print("read in file5")
FF <- read.table(file="6.csv",sep=",",header=TRUE)
print("read in file6")
G <- read.table(file="7.csv",sep=",",header=TRUE)
print("read in file7")
H <- read.table(file="8.csv",sep=",",header=TRUE)
print("read in file8")
I <- read.table(file="9.csv",sep=",",header=TRUE)
print("read in file9")
J <- read.table(file="10.csv",sep=",",header=TRUE)
print("read in file10")
K <- read.table(file="11.csv",sep=",",header=TRUE)
print("read in file11")
ridershipData <- bind_rows(A,B,C,D,E,FF,G,H,I,J,K)
A <- NULL
B <- NULL
C <- NULL
D <- NULL
E <- NULL
FF <- NULL
G <- NULL
H <- NULL
I <- NULL
J <- NULL
K <- NULL
L <- read.table(file="12.csv",sep=",",header=TRUE)
print("read in file12")
M <- read.table(file="13.csv",sep=",",header=TRUE)
print("read in file13")
N <- read.table(file="14.csv",sep=",",header=TRUE)
print("read in file14")
stationInfo2 <- bind_rows(L,M,N)
L <- NULL
M <- NULL
N <- NULL
print("robjects made")
colnames(ridershipData) <- c("station_id","Station","date","daytype","rides")
print("changed col names")
# Shorten the station data to a dataframe with no duplicate map ID's (will be used in combining with other station data)
stationInfo2 <- stationInfo2[!duplicated(stationInfo2$MAP_ID),]
print("shortened statiion info")
# Remove unwanted columns
stationInfo2 <- stationInfo2[-c(7:16)]
print("removed cols from station info")
# Change location to have no parentheses or commas
for(i in 1:nrow(stationInfo2)) 
{
  stationInfo2[i,7] = str_remove(str_remove(str_remove(stationInfo2[i,7], "[(]"), "[)]"),"[,]")
}
i <- NULL
print("for loop end")
# Split up the location and add lat, long columns
stationInfo2 <- stationInfo2 %>% rowwise() %>% mutate(lat = as.numeric(str_split_fixed(Location," ",2)[1]), long = as.double(str_split_fixed(Location," ",2)[2]))
stationCords <- stationInfo2
stationCords <- stationCords[-c(1,2,4,5,7)]
print("stationcords")
# Change to character
stationCords <- stationCords %>% rowwise() %>% mutate(MAP_ID = as.character(MAP_ID))
print("stationcords again")
# Add locations manually
stationCords$lat[stationCords$STOP_NAME == "Randolph/Wabash"] <- 41.884431
stationCords$lat[stationCords$STOP_NAME == "Madison/Wabash"] <- 41.882023
stationCords$lat[stationCords$STOP_NAME == "Washington/State"] <- 41.8837
stationCords$lat[stationCords$STOP_NAME == "Homan"] <- 41.884914
stationCords$long[stationCords$STOP_NAME == "Randolph/Wabash"] <- -87.626149
stationCords$long[stationCords$STOP_NAME == "Madison/Wabash"] <- -87.626098
stationCords$long[stationCords$STOP_NAME == "Washington/State"] <- -87.6278
stationCords$long[stationCords$STOP_NAME == "Homan"] <- -87.711327
print("manually updated")
# Change date
ridershipData$date <- mdy(ridershipData$date)
print("changed date")
# Remove unneeded column
ridershipData <- ridershipData[-c(4)]
print("removed col from ridership data")
# Merge the frames
combined <- merge(ridershipData, stationCords, by.x = "station_id", by.y = "MAP_ID",all=TRUE)
print("merged ridership and stationcords")
# Remove unneeded columns
combined <- combined[-c(5)]
print("removed cols from combined")
# Create choices for list of stops
stopChoices <- c("No Stop")
stops <- combined[!duplicated(combined$station_id),]$Station
stops <- sort(stops)
stopChoices <- append(stopChoices, stops)
print("Made tables")
ridershipData <- NULL
stationInfo2 <- NULL
choosingStop <- FALSE
print("Cleared tables")
stopChoices

ui <- fluidPage(
  includeCSS("mystyles.css"),
  titlePanel("Project 2: CTA Stop Data Mapped"),
  sidebarLayout(
    sidebarPanel(
      width=2,
      br(),
      br(),
      br(),
      br(),
      h2("Menu"),
      h4("Chart"),
      div(
        class="chartOptions",
        radioButtons(
          "topHalfViz",
          label="Chart Type",
          choiceValues = (c("default","table")),
          choiceNames = (c("Bar chart (default)","Table")),
          selected="default"
        ),
        radioButtons(
          "topHalfOrder",
          label="Chart Order",
          choiceValues = (c("default","ascending")),
          choiceNames = (c("ABC Order (default)","Ascending Order")),
          selected="default"
        ),
      ),
      h4("Map"),
      h4("Date Information"),
      p("Select the number of dates that you would like to view"),
      radioButtons(
        "nDates",
        label="Number of Dates",
        choiceValues = (c("default","two")),
        choiceNames = (c("One Date","Two Dates")),
        selected="default"
      ),
      h4("Individual Stop View"),
      p("Choose which map you would like to see"),
      selectInput(
        inputId="chooseStop",
        label="Stop Choice",
        choices = stopChoices,
        selected="No Stop",
        multiple=FALSE
      ),
      conditionalPanel(
        condition = "input.chooseStop != 'No Stop'",
        sliderInput(
          label="Year",
          inputId="yearChoice",
          min=2001,
          max=2021,
          value=2021,
          sep=""
        ),
        selectInput(
          inputId="stopViz",
          label="Stop Data Visualization",
          choices = c("Bar charts", "Tables"),
          selected="Bar charts",
          multiple=FALSE
        )
      )
    ),
    mainPanel(
      width = 10,
      br(),
      br(),
      br(),
      tabsetPanel(
        tabPanel("Data",
                 conditionalPanel(
                   condition = "input.chooseStop != 'No Stop'",
                   # LEFT SIDE BAR OR TABLES
                   conditionalPanel(
                     condition = "input.stopViz == 'Bar charts'",
                     column(width=3,
                            plotOutput("byYear"),
                            plotOutput("byMonth"),
                     )
                   ),
                   conditionalPanel(
                     condition= "input.stopViz == 'Tables'",
                     column(width=3,
                            dataTableOutput("byYearTable"),
                            dataTableOutput("byMonthTable")
                     )
                   ),
                   # CENTER CHART
                   column(width=6,
                          fluidRow(
                            conditionalPanel(
                              condition ="input.topHalfViz == 'default'",
                              plotOutput("barChart2")
                            ),
                            conditionalPanel(
                              condition ="input.topHalfViz == 'table'",
                              dataTableOutput("dataTable2")
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.nDates == 'default'",
                              div(
                                class="secondrow",
                                actionButton(
                                  "prevDay2",
                                  label="Prev Day"
                                ),
                                dateInput(
                                  "dateInput2",
                                  label="Date to View",
                                  value="2021-08-23",
                                  min="2001-01-01",
                                  max="2021-11-30"
                                ),
                                actionButton(
                                  "nextDay2",
                                  label="Next Day"
                                ),
                              )
                            ),
                            conditionalPanel(
                              condition = "input.nDates == 'two'",
                              div(
                                class="secondrow2",
                                dateInput(
                                  "startDate2",
                                  label="Start Date (from...)",
                                  value="2021-08-23",
                                  min="2001-01-01",
                                  max="2021-11-30"
                                ),
                                dateInput(
                                  "endDate2",
                                  label="End Date (to...)",
                                  value="2021-08-24",
                                  min="2001-01-01",
                                  max="2021-11-30"
                                ),
                              )
                            )
                          ),
                          fluidRow(
                            leafletOutput("map2", height="36vh")
                          )
                   ),
                   # RIGHT SIDE CHARTS OR TABLES
                   conditionalPanel(
                     condition = "input.stopViz == 'Bar charts'",
                     column(width=3,
                            plotOutput("byDay"),
                            plotOutput("byWeekday"),
                     )
                   ),
                   conditionalPanel(
                     condition= "input.stopViz == 'Tables'",
                     column(width=3,
                            dataTableOutput("byDayTable"),
                            dataTableOutput("byWeekdayTable")
                     )
                   ),
                 ),
                 # NOT VIEWING A STOP (ORIGINAL, MAIN)
                 conditionalPanel(
                   condition = "input.chooseStop == 'No Stop'",
                   # SINGLE PART
                   fluidRow(
                     conditionalPanel(
                       condition ="input.topHalfViz == 'default'",
                       plotOutput("barChart")
                     ),
                     conditionalPanel(
                       condition ="input.topHalfViz == 'table'",
                       dataTableOutput("dataTable")
                     )
                   ),
                   fluidRow(
                     conditionalPanel(
                       condition = "input.nDates == 'default'",
                       div(
                         class="secondrow",
                         actionButton(
                           "prevDay",
                           label="Prev Day"
                         ),
                         dateInput(
                           "dateInput",
                           label="Date to View",
                           value="2021-08-23",
                           min="2001-01-01",
                           max="2021-11-30"
                         ),
                         actionButton(
                           "nextDay",
                           label="Next Day"
                         ),
                       )
                     ),
                     conditionalPanel(
                       condition = "input.nDates == 'two'",
                       div(
                         class="secondrow2",
                         dateInput(
                           "startDate",
                           label="Start Date (from...)",
                           value="2021-08-23",
                           min="2001-01-01",
                           max="2021-11-30"
                         ),
                         dateInput(
                           "endDate",
                           label="End Date (to...)",
                           value="2021-08-24",
                           min="2001-01-01",
                           max="2021-11-30"
                         ),
                       )
                     )
                   ),
                   fluidRow(
                     leafletOutput("map", height="36vh")
                   )
                 )#End stop viewing screen
        ),
        tabPanel("About",
                 h1("About"),
                 div(
                   h2("App Author"),
                   p("Luis Nunez, Gmail: Lnunez24@uic.edu"),
                   p("Published March 12th 2022"),
                   h2("Motivation"),
                   p("This app is meant to advance the goals of the previous project; to display the ridership data for the CTA transit 'El' system. What is new is the addition of a map and a bar chart for all stops for a specific date"),
                   strong("What's new?"),
                   p("-A bar chart to show the riderhip at each stop for a given date"),
                   p("-A leaflet with three backgrounds and points to represent ridership"),
                   p("-An option to choose a specific date for the bar chart and leaflet"),
                   p("-Representation of ridership changes from one date to another"),
                   h2("Information and credits"),
                   h3("Data Owner"),
                   p("Chicago Transit Authority"),
                   p("https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
                   p("https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme"),
                   h3("Helpful Resources"),
                   h4("Charts and coloring them:"),
                   p("ggplot https://ggplot2.tidyverse.org/"),
                   p("datatable https://www.rdocumentation.org/packages/DT/versions/0.20/topics/datatable, https://rstudio.github.io/DT/shiny.html"),
                   h4("Layout:"),
                   p("shiny https://shiny.rstudio.com/reference/shiny/latest/"),
                   h4("Data Operations and R"),
                   p("r documentation https://www.rdocumentation.org/"),
                   p("aggregate https://r-coder.com/aggregate-r/"),
                   p("https://statisticsglobe.com/"),
                   p("forcats https://forcats.tidyverse.org/reference/"),
                   p("tidyverse https://dplyr.tidyverse.org/reference/mutate.html"),
                   h4("Leaflet docs (super helpful in showing off leaflets and different ways to represent markers"),
                   p("Leaflet https://rstudio.github.io/leaflet")
                 )
        )
      ),
      # VIEWING A STOP
    )# End main panel
  )# End sidebar
)# End ui

server <- function(input, output) {
  #-----------DATA MANIPULATION FOR THE VIEW WITHOUT A SPECIFIC STOP-----------#
  dataForMapAndBarchart <- reactive({
    dateToUse <- NULL
    # Choose the right date depending on the screen
    if (input$chooseStop == "No Stop") {
      dateToUse <- input$dateInput
    }
    else {
      dateToUse <- input$dateInput2
    }
    # Choose the right date depending on the screen
    if (input$nDates == "default") {
      # Subset by the date 
      dataToShow <- subset(combined,date==dateToUse)
      dataToShow
    }
    else {
      start <- NULL
      end <- NULL
      # Obtain start and end date
      if (input$chooseStop == "No Stop") {
        start <- input$startDate
        end <- input$endDate
      }
      else {
        start <- input$startDate2
        end <- input$endDate2
      }
      # Obtain data sets by subsetting via start and end dates
      startData <- subset(combined,date==start)
      endData <- subset(combined,date==end)
      # Calculate the difference between end and start date data (end - start)
      difference <- endData
      difference <- difference %>% mutate(rides = endData$rides[endData$station_id == station_id] -  startData$rides[startData$station_id == station_id] )
      dataForMapAndBarchart <- difference
    }
  })
  eventReactive(input$chooseStop,{
    choosingStop <- TRUE
  })
  observeEvent(input$nextDay,{
    currDate <- input$dateInput
    nextDay <- currDate + 1
    updateDateInput(getDefaultReactiveDomain(),"dateInput",
                    label="Date to View",
                    value=nextDay,
                    min=as.Date("2001-01-01"),
                    max=as.Date("2021-11-30"))
  })
  observeEvent(input$prevDay,{
    currDate <- input$dateInput
    prevDay <- currDate - 1
    updateDateInput(getDefaultReactiveDomain(),"dateInput",
                    label="Date to View",
                    value=prevDay,
                    min=as.Date("2001-01-01"),
                    max=as.Date("2021-11-30"))
    
  })
  output$barChart <- renderPlot({
    day <- mday(input$dateInput)
    weekday <- wday(ymd(input$dateInput),label=TRUE,abbr = FALSE)
    month <- month(ymd(input$dateInput),label=TRUE,abbr = FALSE)
    name <- NULL
    year <- year(input$dateInput2)
    if (day == 1 || day == 21 || day == 31) {
      name <- paste(day, "st",sep="")
    }
    else if (day == 2 || day == 22) {
      name <- paste(day, "nd",sep="")
    }
    else if (day == 3 || day == 23) {
      name <- paste(day, "rd",sep="")
    }
    else {
      name <- paste(day, "th",sep="")
    }
    # Set up dates
    from <- input$startDate
    to <-input$endDate
    # Get to date
    today <- mday(to)
    tomonth <- month(ymd(to),label=TRUE,abbr = FALSE)
    toname <- NULL
    toyear <- year(to)
    # Get from date
    fromday <- mday(from)
    frommonth <- month(ymd(from),label=TRUE,abbr = FALSE)
    fromname <- NULL
    fromyear <- year(from)
    # Make title
    twoDateTitle <- paste("Rides from",frommonth,fromday,fromyear,"to",tomonth,today,toyear)
    oneDateTitle <- paste("Rides on", weekday,"the",name,"of",month,year)
    trueTitle <- ifelse(input$nDates == "two",twoDateTitle,oneDateTitle)
    if (input$topHalfOrder == "ascending") {
      sorted <- arrange(dataForMapAndBarchart(),rides)
      sorted <- mutate(sorted, Station = fct_reorder(Station, rides)) 
      ggplot(sorted, aes( x=Station,y=rides, fill=ifelse(rides >= 0 ,"black", "red"))) + geom_col() + scale_fill_manual(values=c("black","red")) + labs(title=trueTitle) + theme(panel.border = element_rect(color="black",size=1,fill=NA), axis.text.x = element_text(angle=90), plot.title = element_text(hjust=0.5, face="bold"),legend.position = "none") + scale_y_continuous(labels=label_comma())
    }
    else {
      ggplot(dataForMapAndBarchart(), aes(x=Station,y=rides, fill=ifelse(rides >= 0 ,"black", "red") ) ) + geom_col() + scale_fill_manual(values=c("black","red")) + labs(title=trueTitle) + theme(panel.border = element_rect(color="black",size=1,fill=NA), axis.text.x = element_text(angle=90), plot.title = element_text(hjust=0.5, face="bold"),legend.position = "none") + scale_y_continuous(labels=label_comma())
    }
  })
  output$map <- renderLeaflet({
    if (input$nDates == "two") {
      leaflet(data=dataForMapAndBarchart()) %>% 
        addCircleMarkers(lng=~long, lat=~lat, label=~paste(Station,": ",rides), color=~ifelse(rides >= 0,"black","red"), radius=~log(abs(rides)) * (log(abs(rides)) * .05), stroke=TRUE, fillOpacity=~log(abs(rides)) / 6 ) %>%  
        addTiles(group = "Background1") %>% addProviderTiles(providers$CartoDB.Voyager, group = "Background 2") %>% 
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Background 3") %>% addLegend(title="Rides Display",position="bottomright",colors=c("black","red"), labels = c("Positive change","Negative change")) %>%
      addLayersControl(baseGroups=c("Background 1","Background 2","Background 3"),options=layersControlOptions(collapsed=FALSE))
    }
    else {
      leaflet(data=dataForMapAndBarchart()) %>% 
        addCircleMarkers(lng=~long, lat=~lat, label=~paste(Station,": ",rides), color=~ifelse(rides >= 0,"black","red"), radius=~log(abs(rides)) * (log(abs(rides)) * .05), stroke=TRUE, fillOpacity=~log(abs(rides)) / 6 ) %>%  
        addTiles(group = "Background1") %>% addProviderTiles(providers$CartoDB.Voyager, group = "Background 2") %>% 
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Background 3") %>% addLegend(title="Rides Display", position="bottomright",colors=c("black"), labels = c("Rides")) %>%
      addLayersControl(baseGroups=c("Background 1","Background 2","Background 3"),options=layersControlOptions(collapsed=FALSE))
    }
  })
  output$dataTable <- renderDataTable({
    if (input$topHalfOrder == "ascending") {
      sorted <- arrange(dataForMapAndBarchart(),rides)
      datatable(sorted, options=list(pageLength=10))
    }
    else {
      sorted <- arrange(dataForMapAndBarchart(),Station)
      datatable(sorted, options=list(pageLength=10))
    }    
  })
  #-----------DATA MANIPULATION FOR THE VIEW WITHOUT A SPECIFIC STOP (END)-----------#
  #-----------DATA MANIPULATION FOR THE VIEW WITHOUT A SPECIFIC STOP (END)-----------#
  
  #-----------DATA MANIPULATION FOR THE VIEW WITH A SPECIFIC STOP-----------#
  #-----------DATA MANIPULATION FOR THE VIEW WITH A SPECIFIC STOP-----------#
  
  #----- Date inputs for the display with a single stop----- 
  #----- Date inputs for the display with a single stop----- 
  eventReactive(input$chooseStop,
                {
                  choosingStop <- TRUE
  })
  observeEvent(input$nextDay2,{
    currDate <- input$dateInput2
    nextDay <- currDate + 1
    updateDateInput(getDefaultReactiveDomain(),"dateInput2",
                    label="Date to View",
                    value=nextDay,
                    min=as.Date("2001-01-01"),
                    max=as.Date("2021-11-30"))
  })
  observeEvent(input$prevDay2,{
    currDate <- input$dateInput2
    prevDay <- currDate - 1
    updateDateInput(getDefaultReactiveDomain(),"dateInput2",
                    label="Date to View",
                    value=prevDay,
                    min=as.Date("2001-01-01"),
                    max=as.Date("2021-11-30"))
  })
  output$barChart2 <- renderPlot({
    # Obtain date information
    day <- mday(input$dateInput2)
    weekday <- wday(ymd(input$dateInput2),label=TRUE,abbr = FALSE)
    month <- month(ymd(input$dateInput2),label=TRUE,abbr = FALSE)
    name <- NULL
    year <- year(input$dateInput2)
    if (day == 1 || day == 21 || day == 31) {
      name <- paste(day, "st",sep="")
    }
    else if (day == 2 || day == 22) {
      name <- paste(day, "nd",sep="")
    }
    else if (day == 3 || day == 23) {
      name <- paste(day, "rd",sep="")
    }
    else {
      name <- paste(day, "th",sep="")
    }
    # Set up dates
    from <- input$startDate2
    to <-input$endDate2
    # Get to date
    today <- mday(to)
    tomonth <- month(ymd(to),label=TRUE,abbr = FALSE)
    toname <- NULL
    toyear <- year(to)
    # Get from date
    fromday <- mday(from)
    frommonth <- month(ymd(from),label=TRUE,abbr = FALSE)
    fromname <- NULL
    fromyear <- year(from)
    # Make title
    twoDateTitle <- paste("Rides from",frommonth,fromday,fromyear,"to",tomonth,today,toyear)
    oneDateTitle <- paste("Rides on", weekday,"the",name,"of",month,year)
    trueTitle <- ifelse(input$nDates == "two",twoDateTitle,oneDateTitle)
    # Color the chart bars according to negative and positive value
    # aes has a fill param. Can do ifelse there
    # default colors override ifelse conditions. scale_fill_manual() fixes it
    if (input$topHalfOrder == "ascending") {
      sorted <- arrange(dataForMapAndBarchart(),rides)
      sorted <- mutate(sorted, Station = fct_reorder(Station, rides)) 
      ggplot(sorted, aes( x=Station,y=rides, fill=ifelse(rides >= 0 ,"black", "red"))) + geom_col() + scale_fill_manual(values=c("black","red")) + labs(title=trueTitle) + theme(panel.border = element_rect(color="black",size=1,fill=NA), axis.text.x = element_text(angle=90), plot.title = element_text(hjust=0.5, face="bold"),legend.position = "none") + scale_y_continuous(labels=label_comma())
    }
    else {
      ggplot(dataForMapAndBarchart(), aes(x=Station,y=rides, fill=ifelse(rides >= 0 ,"black", "red") ) ) + geom_col() + scale_fill_manual(values=c("black","red")) + labs(title=trueTitle) + theme(panel.border = element_rect(color="black",size=1,fill=NA), axis.text.x = element_text(angle=90), plot.title = element_text(hjust=0.5, face="bold"),legend.position = "none") + scale_y_continuous(labels=label_comma())
    }
  })
  #----- Data table and map for the display with a single stop----- 
  #----- Data table and map for the display with a single stop----- 
  output$map2 <- renderLeaflet({
    if (input$nDates == "two") {
      leaflet(data=dataForMapAndBarchart()) %>% 
        addCircleMarkers(lng=~long, lat=~lat, label=~paste(Station,": ",rides), color=~ifelse(rides >= 0,"black","red"), radius=~log(abs(rides)) * (log(abs(rides)) * .05), stroke=TRUE, fillOpacity=~log(abs(rides)) / 6 ) %>%  
        addTiles(group = "Background1") %>% addProviderTiles(providers$CartoDB.Voyager, group = "Background 2") %>% 
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Background 3") %>% addLegend(title="Rides Display",position="bottomright",colors=c("black","red"), labels = c("Positive change","Negative change")) %>%
        addLayersControl(baseGroups=c("Background 1","Background 2","Background 3"),options=layersControlOptions(collapsed=FALSE))
    }
    else {
      leaflet(data=dataForMapAndBarchart()) %>% 
        addCircleMarkers(lng=~long, lat=~lat, label=~paste(Station,": ",rides), color=~ifelse(rides >= 0,"black","red"), radius=~log(abs(rides)) * (log(abs(rides)) * .05), stroke=TRUE, fillOpacity=~log(abs(rides)) / 6 ) %>%  
        addTiles(group = "Background1") %>% addProviderTiles(providers$CartoDB.Voyager, group = "Background 2") %>% 
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Background 3") %>% addLegend(title="Rides Display", position="bottomright",colors=c("black"), labels = c("Rides")) %>%
        addLayersControl(baseGroups=c("Background 1","Background 2","Background 3"),options=layersControlOptions(collapsed=FALSE))
    }
  })
  output$dataTable2 <- renderDataTable({
    if (input$topHalfOrder == "ascending") {
      # Sort table by rides
      sorted <- arrange(dataForMapAndBarchart(),rides)
      datatable(sorted, options=list(pageLength=10))
    }
    else {
      # Sort table by station
      sorted <- arrange(dataForMapAndBarchart(),Station)
      datatable(sorted, options=list(pageLength=10))
    }
  })
  #----- Rendering data tables and charts for a stop----- 
  #----- Rendering data tables and charts for a stop----- 
  output$byYear <- renderPlot({
    # Isolate stop and year
    stop <- input$chooseStop
    # Subset
    x <- subset(combined, Station == stop) 
    ggplot(x, aes(x=year(date),y=rides)) + geom_col(fill="dodgerblue") + xlab("Year") + labs(title=paste("Rides at",stop,"by Year"))+ theme(panel.border = element_rect(color="black",size=1,fill=NA), plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
  })
  output$byMonth <- renderPlot({
    # Isolate stop and year
    stop <- input$chooseStop
    year <- input$yearChoice
    # Subset
    x <- subset(combined, Station == stop & year(date) ==  year) 
    ggplot(x, aes(x=month(date, label=TRUE, abbr=TRUE),y=rides)) + geom_col(fill="dodgerblue") + xlab("Month") + labs(title=paste("Rides at",stop,"by Month",year))+ theme(panel.border = element_rect(color="black",size=1,fill=NA), plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
  })
  output$byWeekday <- renderPlot({
    # Isolate stop and year
    stop <- input$chooseStop
    year <- input$yearChoice
    # Subset
    x <- subset(combined, Station == stop & year(date) ==  year) 
    ggplot(x, aes(x=wday(date,label=TRUE, abbr=TRUE),y=rides)) + geom_col(fill="dodgerblue") + xlab("Weekday") + labs(title=paste("Rides at",stop,"by Weekday",year))+ theme(panel.border = element_rect(color="black",size=1,fill=NA), plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
  })
  output$byDay <- renderPlot({
    # Isolate stop and year
    stop <- input$chooseStop
    year <- input$yearChoice
    # Subset
    x <- subset(combined, Station == stop & year(date) ==  year) 
    ggplot(x, aes(x=yday(date),y=rides)) + geom_col(fill="dodgerblue") + xlab("Day") + labs(title=paste("Rides at",stop,"by Day",year))+ theme(panel.border = element_rect(color="black",size=1,fill=NA), plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
  })
  output$byYearTable <- renderDataTable({
    # Isolate stop
    stop <- input$chooseStop
    # Stop
    x <- subset(combined, Station == stop) 
    x$Year <- year(x$date)
    # Make table
    byyear <- aggregate(x$rides, by=list(x$Year),FUN=sum)
    colnames(byyear) <-c("Year", "rides")
    datatable(byyear, options=list(pageLength=10))
  })
  output$byMonthTable <- renderDataTable({
    # Isolate stop and year
    stop <- input$chooseStop
    year <- input$yearChoice
    # Subset
    x <- subset(combined, Station == stop & year(date) ==  year) 
    # Make table
    x$Month <- month(x$date, label = TRUE, abbr = FALSE)
    byMonthTable <- aggregate(x$rides, by=list(x$Month),FUN=sum)
    colnames(byMonthTable) <- c("Month","rides")
    datatable(byMonthTable, options=list(pageLength=10))
  })
  output$byDayTable <- renderDataTable({
    # Isolate stop and year
    stop <- input$chooseStop
    year <- input$yearChoice
    # Subset
    x <- subset(combined, Station == stop & year(date) ==  year) 
    # Make table
    byDayTable <- aggregate(x$rides, by=list(x$date),FUN=sum)
    colnames(byDayTable) <- c("Day","rides")
    byDayTable
    datatable(byDayTable, options=list(pageLength=10))
  })
  output$byWeekdayTable <- renderDataTable({
    # Isolate stop and year
    stop <- input$chooseStop
    year <- input$yearChoice
    # Subset
    x <- subset(combined, Station == stop & year(date) ==  year) 
    # Change the date to a weekday
    x$date <- wday(x$date, label = TRUE, abbr = FALSE)
    # Rename colnames
    colnames(x) <- c("station_id", "Station", "Weekday", "rides")
    # Make table
    byWeekdayTable <- aggregate(x$rides, by=list(x$Weekday),FUN=sum)
    colnames(x) <- c("Weekday","rides")
    datatable(byWeekdayTable, options=list(pageLength=10))
  })
  #----- Handle events for choosing a map----- 
  #----- Handle events for choosing a map----- 
  observeEvent(input$map_marker_click,{
    # The input$map_marker_click has lat, long items, getting those 
    latt <- input$map_marker_click$lat
    longg <- input$map_marker_click$lng
    # Subset the data based on these coordinates 
    d <- filter(combined, lat == latt, long == longg)
    # Get the station
    chosenStation <- d$Station[1]
    # Update
    updateSelectInput(getDefaultReactiveDomain(),"chooseStop",
                        label="Stop Choice",
                        choices = stopChoices,
                        selected = chosenStation)
  })
  observeEvent(input$map2_marker_click,{
    # The input$map_marker_click has lat, long items, getting those 
    latt <- input$map2_marker_click$lat
    longg <- input$map2_marker_click$lng
    # Subset the data based on these coordinates 
    d <- filter(combined, lat == latt, long == longg)
    # Get the station
    chosenStation <- d$Station[1]
    # Update
    updateSelectInput(getDefaultReactiveDomain(),"chooseStop",
                      label="Stop Choice",
                      choices = stopChoices,
                      selected = chosenStation)
  })
  #-----------DATA MANIPULATION FOR THE VIEW WITH A SPECIFIC STOP(END)-----------#
}
shinyApp(ui=ui,server=server)
