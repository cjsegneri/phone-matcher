fluidPage(theme = shinytheme("cosmo"),
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