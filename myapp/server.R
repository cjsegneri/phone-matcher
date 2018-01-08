function(input, output, session) {
  
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