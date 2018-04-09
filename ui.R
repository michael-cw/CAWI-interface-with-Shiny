# NRA Phone survey 2018
library("shiny")
library("shinydashboard")
library("shinythemes")
mimTypes<-c('application/vnd.ms-excel',
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
            '.xls',
            '.xlsx')

shinyUI(fluidPage(
  ################################
  ##  SELECT SHINY THEME
  ##    simplex or lumen
  
  theme = shinytheme("lumen"),
  title = "SURVEY SOLUTIONS WEBSURVEY",
  
  # Application title
  titlePanel(div(img(src="nra_logo_small.png"), "SURVEY SOLUTIONS CAWI APPLICATION")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width=4,
                 ###################################
                 ##  Blue style sheet DT
                 includeCSS("www/DT_styleSheetBlue.css"),
                 ## Questionnaire Selection
                 conditionalPanel("input.box1=='1' & input.qTable_rows_selected>0",
                                  fluidRow(
                                    column(2),
                                    column(9,
                                           h3("Current Status of the selected Questionnaire:")),
                                    column(1)
                                  ),
                                  fluidRow(
                                    column(1),
                                    column(10,
                                           DT::dataTableOutput("qSel"), br()),
                                    column(1)
                                  ),
                                  fluidRow(
                                    column(4),
                                    column(4,
                                           actionButton("qAss", "Confirm Selection", width = "100%"), br(),br(),
                                           actionButton("emText", "Compose   Email", width = "100%"),br(),br()),
                                    column(4)
                                  )),
                                  
                        
                 
                 ## Upload of List and SS calculation
                 conditionalPanel("input.box1=='2'",
                                  fluidRow(
                                    column(4),
                                    column(5,
                                           h3("Sampling Frame")),
                                    column(3)
                                  ),
                                  fluidRow(
                                    column(3),
                                    column(6,
                                           radioButtons("frameNew", "Upload New Frame or Select Existing?",
                                                        choices = c("New"=1, "Existing (from Directory)"=2), 
                                                        inline = T)),
                                    column(3)
                                  ),
                                  ##  UPLOAD NEW FRAME
                                  conditionalPanel("input.frameNew==1",
                                                   fluidRow(
                                                     fileInput("frameList", "", accept = mimTypes),
                                                     radioButtons("fileHelp", "Display File Format Instructions?",
                                                                  choices = c("Yes"=1, "No"=2), inline = T, selected = 2),
                                                     conditionalPanel("input.fileHelp==1",
                                                                      h4("File format instructions:"),
                                                                      helpText("i. The first row must be a header, containing at
                                             least first name, last name and email address."),
                                                                      helpText("ii. The corresponding column names must be FIRST, LAST,
                                             and EMAIL, and must be written in the FIRST ROW OF
                                             EACH SHEET."),
                                                                      downloadLink("template", "Download Template"),
                                                                      helpText("iii. It must contain one row per respondent."),
                                                                      helpText("iv. Each respondent can only have 
                                             ONE name and ONE email address."),
                                                                      helpText("v. The file must not contain any merged cells 
                                             or other irrelevant components."),
                                                                      helpText("vi. Each frame (=list of respondents) requires a separate
                                             sheet. This means you can put multiple list into one file."))),
                                                   fluidRow(
                                                     selectInput("sheetSel", "Select Sheet:", c("Upload file first"=0)))),
                                  conditionalPanel("input.frameNew==2",
                                                   fluidRow(
                                                     column(1),
                                                     column(10,
                                                            h4("Existing Lists"),
                                                            DT::dataTableOutput("frameDir")),
                                                     column(1)
                                                   )),
                                  fluidRow(column(1),
                                           column(10,
                                                  DT::dataTableOutput("sampleSizeTable")),
                                           column(1)),
                                  fluidRow(numericInput("sampSizeFinal", "Provide Sample Size",
                                                        value = 1, step = 1)),
                                  fluidRow(actionButton("nAss", "Confirm Sample Size"))),
                 
                 ## Survey Initiation
                 conditionalPanel("input.box1=='3'",
                                  fluidRow(
                                    column(3),
                                    column(6,
                                           h3("Assignment Creation")),
                                    column(3)
                                  ),
                                  fluidRow(
                                    column(2),
                                    column(8,
                                           numericInput("n_ass", "Number of Assignments", 
                                                        value = 1, step = 1)),
                                    column(2)
                                  )),
                 ## Survey Initiation
                 conditionalPanel("input.box1=='4'|input.box1=='5'",
                                  fluidRow(
                                    column(3),
                                    column(6,
                                           h3("Existing Surveys")),
                                    column(3)
                                  ),
                                  fluidRow(
                                    column(1),
                                    column(10,
                                           DT::dataTableOutput("sampleDir")),
                                    column(1)
                                  ),
                                  conditionalPanel("input.box1==4",
                                                   fluidRow(
                                                     column(3),
                                                     column(6,
                                                            radioButtons("remNewAss", "Create New Assignments or Keep Old?",
                                                                         choices = c("New"=1, "Old"=2), inline = T, selected = 1)),
                                                     column(3)
                                                   ),
                                                   fluidRow(
                                                     column(1),
                                                     column(10,
                                                            helpText("If you select New, the new assignments will be created, 
                                                    and the old (unused) ones are devalidated. You should select New,
                                                    if many respondents had problems to access the link.")),
                                                     column(1)
                                                   ))
                 ),
                 
                 conditionalPanel("input.box1=='3'|(input.box1=='4'&input.remNewAss==1)|
                                  input.box1==5",
                                  fluidRow(
                                    column(2),
                                    column(8,
                                           selectInput("teamSV", "Select Team", c("NONE")),
                                           selectInput("teamINT", "Select Team Member", c("Select Team first!"))),
                                    column(2)
                                  )
                 ),
                 conditionalPanel("input.box1=='3'",
                                  fluidRow(
                                    column(3),
                                    column(6,
                                           actionButton("create", "Create Assignments")),
                                    column(3)
                                  )
                 ),
                 
                 conditionalPanel("input.box1=='4'",
                                  fluidRow(
                                    column(1),
                                    column(10,
                                           actionButton("reminder", "Send Reminder")),
                                    column(1)
                                  )
                 ),
                 
                 conditionalPanel("input.box1=='5'",
                                  # CATI PW:
                                  # 1 Jklw*45vds
                                  # 2 qoow#56Qjkj
                                  fluidRow(
                                    column(1),
                                    column(10,
                                           actionButton("assignCATI", 
                                                        "Assign to Phone Interviewer")),
                                    column(1)
                                  )
                 ),
                 ############################################################
                 ##    ADMIN SETTINGS
                 ############################################################
                 fluidRow(
                   column(4),
                   column(4,br(),br(),br(),br(),
                          radioButtons("admin", "Show Admin Settings", inline = T,
                                       c("Yes"=1, "No"=2),
                                       selected = 2)),
                   column(4)
                 )
                 
    ),
    #########################################
    ##    Main Panel 
    mainPanel(
      fluidPage(
        fluidRow(
          tabBox(width = 8,id = "box1",
                 tabPanel(title = "Questionnaire", value=1,
                          h3("Select one of the questionnaires bellow 
                                   for your survey"),
                          helpText("Clicking on the questionnaire in the table shows the status. To select it for the survey,
                                   You need to confirm again with the button to the left."),
                          DT::dataTableOutput("qTable")),
                 tabPanel(title = "Sample Selection", value=2,
                          DT::dataTableOutput("frameTable")),
                 tabPanel(title = "Assignment Creation", value=3,
                          h3("List of Final Sample"),
                          DT::dataTableOutput("FinalSampList")),
                 tabPanel(title = "Sample Adminsistration", value=4,
                          h3("List of Respondents"),
                          DT::dataTableOutput("SampleAdminList")),
                 tabPanel(title = "Phone Survey", value=5,
                          h3("List of Respondents"),
                          helpText("For those respondents who haven't respondet even 
                                   after the final reminder, you may want to follow up by assigning them to a 
                                   CATI interviewer. Just select them from the table bellow, 
                                   and then assign them to the responsible interviewer."),
                          DT::dataTableOutput("SampleAdminList1"))
          )
        )
      )
    )
  )
))
