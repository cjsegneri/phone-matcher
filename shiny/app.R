
library(shiny)
library(DT)
library(shinyjs)
library(shinythemes)

#### COMPUTATIONAL CODE ####
cleanData = function(dirty){
  # convert empties to NAs dates
  dirty$First.Seen..TJ[dirty$First.Seen..TJ == ''] = NA
  dirty$First.Seen..TRAC[dirty$First.Seen..TRAC == ''] = NA
  dirty$Last.Seen..TJ[dirty$Last.Seen..TJ == ''] = NA
  dirty$Last.Seen..TRAC[dirty$First.Seen..TRAC == ''] = NA
  # convert empties to NAs hits
  dirty$Hit.Count..TJ[dirty$Hit.Count..TJ == ''] = NA
  dirty$Use.Count..TRAC[dirty$Use.Count..TRAC == ''] = NA
  # clean NAs for dates
  dirty = dirty[!is.na(dirty$First.Seen..TJ),]
  dirty = dirty[!is.na(dirty$First.Seen..TRAC),]
  dirty = dirty[!is.na(dirty$Last.Seen..TJ),]
  dirty = dirty[!is.na(dirty$Last.Seen..TRAC),]
  # clean NAs for hits
  dirty = dirty[!is.na(dirty$Use.Count..TRAC),]
  dirty = dirty[!is.na(dirty$Hit.Count..TJ),]
  # convert to characters
  dirty$First.Seen..TJ = as.character(dirty$First.Seen..TJ)
  dirty$First.Seen..TRAC = as.character(dirty$First.Seen..TRAC)
  dirty$Last.Seen..TJ = as.character(dirty$Last.Seen..TJ)
  dirty$Last.Seen..TRAC = as.character(dirty$Last.Seen..TRAC)
  # split the date times
  dateTimeFTJ = strsplit(dirty$First.Seen..TJ, ' ')
  dateTimeLTJ = strsplit(dirty$Last.Seen..TJ, ' ')
  dateTimeFTrac = strsplit(dirty$First.Seen..TRAC, ' ')
  dateTimeLTrac = strsplit(dirty$Last.Seen..TRAC, ' ')
  # get all the individual dates from the split
  dirty$First.Seen.Date.TJ = rep('', dim(dirty)[1])
  dirty$First.Seen.Date.Trac = rep('', dim(dirty)[1])
  dirty$Last.Seen.Date.TJ = rep('', dim(dirty)[1])
  dirty$Last.Seen.Date.Trac = rep('', dim(dirty)[1])
  for (x in 1:dim(dirty)[1]){
    dirty$First.Seen.Date.TJ[x] = as.character(dateTimeFTJ[[x]][1])
    dirty$First.Seen.Date.Trac[x] = as.character(dateTimeFTrac[[x]][1])
    dirty$Last.Seen.Date.TJ[x] = as.character(dateTimeLTJ[[x]][1])
    dirty$Last.Seen.Date.Trac[x] = as.character(dateTimeLTrac[[x]][1])
  }
  dirty$First.Seen.Date.TJ = as.Date(dirty$First.Seen.Date.TJ)
  dirty$First.Seen.Date.Trac = as.Date(dirty$First.Seen.Date.Trac)
  dirty$Last.Seen.Date.TJ = as.Date(dirty$Last.Seen.Date.TJ)
  dirty$Last.Seen.Date.Trac = as.Date(dirty$Last.Seen.Date.Trac)
  
  return(dirty)
}

computeMatches = function(phoneData, tjHits, tracHits, weeks){
  # verify hits
  tjMatch = phoneData$Hit.Count..TJ >= tjHits
  tracMatch = phoneData$Use.Count..TRAC >= tracHits
  hitMatches = tjMatch & tracMatch
  
  # verify date overlapping
  firstOverlapTrac = phoneData$First.Seen.Date.Trac >= phoneData$First.Seen.Date.TJ &
    phoneData$First.Seen.Date.Trac <= phoneData$Last.Seen.Date.TJ
  secondOverlapTrac = phoneData$Last.Seen.Date.Trac <= phoneData$Last.Seen.Date.TJ &
    phoneData$Last.Seen.Date.Trac >= phoneData$First.Seen.Date.TJ
  firstOverlapTJ = phoneData$First.Seen.Date.TJ >= phoneData$First.Seen.Date.Trac &
    phoneData$First.Seen.Date.TJ <= phoneData$Last.Seen.Date.Trac
  secondOverlapTJ = phoneData$Last.Seen.Date.TJ <= phoneData$Last.Seen.Date.Trac &
    phoneData$Last.Seen.Date.TJ >= phoneData$First.Seen.Date.Trac
  # check the weeks difference
  firstCheck = floor(abs((phoneData$Last.Seen.Date.Trac - phoneData$First.Seen.Date.TJ)/7)) <= weeks
  secondCheck = floor(abs((phoneData$Last.Seen.Date.TJ - phoneData$First.Seen.Date.Trac)/7)) <= weeks
  # get the final boolean
  finalDatesBool = firstOverlapTJ | secondOverlapTJ | firstOverlapTrac | secondOverlapTrac |
    firstCheck | secondCheck
  
  # filter the data
  Matches = phoneData[hitMatches & finalDatesBool, 1:(dim(phoneData)[2]-4)]
  
  return(Matches)
}


#### UI ####
ui <- fluidPage(theme = shinytheme("cosmo"),
                fluidRow( align = 'center',
                          ## input and refresh panel ##
                          column(2,
                                 inputPanel(
                                   fileInput("file1", "Choose CSV File",
                                             accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                                   
                                   actionButton("refreshBtn", "Refresh Table")
                                 )),
                          ## parameter panel ##
                          column(10,
                                 inputPanel(
                                   numericInput("tjHits", "Only select records for TJ with hits above:", 0),
                                   
                                   numericInput("tracHits", "Only select records for Trac with hits above:", 0),
                                   
                                   sliderInput("weeks", "Weeks apart from dates overlapping:",
                                               min = 0, max = 10, value = 0)
                                 ))
                ),
                fluidRow(
                  ## data table ##
                  column(10,
                         dataTableOutput("phoneTable")
                  ),
                  ## download options ##
                  column(2,
                         inputPanel(
                           actionLink("selectAll", "Select All"),
                           
                           checkboxGroupInput("downOptions", "Download Options",
                                              choices = list(
                                                "Phone" = "Phone",
                                                "Use.Count..TRAC" = "Use.Count..TRAC",
                                                "Hit.Count..TJ" = "Hit.Count..TJ",
                                                "First.Seen..TRAC" = "First.Seen..TRAC",
                                                "Last.Seen..TRAC" = "Last.Seen..TRAC",
                                                "First.Seen..TJ" = "First.Seen..TJ",
                                                "Last.Seen..TJ" = "Last.Seen..TJ",
                                                "Avg..TRAC" = "Avg..TRAC",
                                                "Sum..TRAC" = "Sum..TRAC",
                                                "Date.Checked..TJ" = "Date.Checked..TJ"
                                              )),
                           
                           uiOutput("downBtn")
                         ))
                )
)


##### SERVER ####
server <- function(input, output, session) {
  
  # configure the download button at load
  output$downBtn = renderUI({
    actionButton("downError", "Download", icon = icon("download"))
  })
  
  # handle the download error
  observeEvent(input$downError, {
    showModal(modalDialog(
      title = "Alert",
      "No data to download."
    ))
  })
  
  # handle downloading the data
  output$downData = downloadHandler(
    filename = function() {
      # create the filename for the download
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      # filter the data based on the download options
      userData = as.data.frame(matchedData[,c(input$downOptions)])
      colnames(userData) = input$downOptions
      # if someone is just exporting phone numbers, then remove duplicates
      if ("Phone" %in% colnames(userData) & length(colnames(userData)) == 1){
        cleanedPhones = data.frame(Phone = unique(userData$Phone))
        write.csv(cleanedPhones, con, row.names = FALSE)
      }else{
        write.csv(userData, con, row.names = FALSE)
      }
    }
  )
  
  # reading in the data
  observeEvent(input$file1, {
    inFile = input$file1
    if (is.null(inFile)){
      return (NULL)
    }else{
      rawData = read.csv(inFile$datapath)
      cleanedData <<- cleanData(rawData)
      matchedData <<- computeMatches(cleanedData, input$tjHits, input$tracHits, input$weeks)
      
      # now that the data is imported, the download button works
      output$downBtn = renderUI({downloadButton("downData", "Download")})
    }
  })
  
  # if the refresh button is hit and there is no data, make an alert
  observeEvent(input$refreshBtn, {
    if (is.null(input$file1)){
      showModal(modalDialog(
        title = "Alert",
        "No data to view."
      ))
    }
  })
  
  # if the refresh button is hit, read in the data, clean it, and find the matches
  refreshedPhoneTable = eventReactive(input$refreshBtn, {
    matchedData <<- computeMatches(cleanedData, input$tjHits, input$tracHits, input$weeks)
  })
  
  # display the matches in a table
  output$phoneTable = renderDataTable({
    # create a blank table when a file is not loaded
    if (is.null(input$file1)){
      datatable(data.frame(0))
    }else{
      datatable(refreshedPhoneTable())
    }
  })
  
  # handle select all button
  observeEvent(input$selectAll, {
    choices = list(
      "Phone",
      "Use.Count..TRAC",
      "Hit.Count..TJ",
      "First.Seen..TRAC",
      "Last.Seen..TRAC",
      "First.Seen..TJ",
      "Last.Seen..TJ",
      "Avg..TRAC",
      "Sum..TRAC",
      "Date.Checked..TJ"
    )
    if (input$selectAll%%2==0) {
      updateCheckboxGroupInput(session, "downOptions", "Download Options", choices = choices)
    }else{
      updateCheckboxGroupInput(session, "downOptions", "Download Options", choices = choices, selected = choices)
    }
  })
}

#### CREATE THE SHINY APPLICATION ####
shinyApp(ui = ui, server = server)

