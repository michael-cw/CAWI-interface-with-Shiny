#######################################################
##  API DEMO 2017
##  v1.0.1
##  30-08-2017
#######################################################
##  1. Load Packages
library(leaflet)
library(plyr)
library(dplyr)
library(RSQLite)
library(RgoogleMaps)
library(googleVis)
library(raster)
library(data.table)
library(pool)
library(rgdal)
library(rgeos)
library(maptools)
library(shinythemes)
library(DT)
library(readr)
library(plotGoogleMaps)
library("knitr")
library("rmarkdown")
library("openxlsx")
library("stringr")

## XLConnect for download
library("XLConnect")
library(pwr)
##  1.1. Running HELPERS and GLOBALS
source("helpers/sysSample.R")
source("helpers/suso_api_export_files.R")
source("helpers/suso_api_user.R")
source("helpers/suso_api_create_assignments.R")
source("helpers/suso_api_quest.R")
source("helpers/genWebCode.R")
source("helpers/create_mail_v2.R")
source("helpers/suso_api_assignments.R")


###############################
#       ADMIN VARIABLES
fields <- c("email.adress", "email.subject", "email.smtp",
            "email.user", "email.pass", "email.port",
            "suso.server", "suso.user", "suso.pass")
#######################################
##  1.1 CHECK IF SETTINGS FILE EXITS
admin.files<-list.files("data/admin")
if (length(admin.files)>0){
  ##  if some files exists, check if content is ok
  tmp.admin<-readRDS(paste0("data/admin/", admin.files[1]))
} else {
  tmp.admin<-character(0)
}

##  Must contain 9 fields (change if necessary!)
if (length(tmp.admin)==9){
  admin.vars<-tmp.admin
} else {
  admin.vars<-rep("TBD", 9)
  names(admin.vars)<-fields
}
admin.check<-sum(admin.vars=="TBD")
###############################
##  1.2. SUSO SERVER
server<-admin.vars[["suso.server"]]
APIuser<-admin.vars[["suso.user"]]
APIpass<-admin.vars[["suso.pass"]]

###############################
##  Other options
##  a) DT
smTab<-list(dom="t")
##  b) mime
mimeTypes<-c('application/vnd.ms-excel',
             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
             '.xls',
             '.xlsx')

shinyServer(function(input, output, session) {
  
  
  ######################################################################################
  ##                SERVER ADMIN SETTINGS
  ######################################################################################
  ADMIN<-reactiveValues()
  ##  0. Load Initial Settings
  observe({
    if(admin.check==0) {
      ADMIN$settings<-admin.vars
    }
    
  })
  ##  1. MODAL
  observeEvent(input$admin,{
    ##  OPENS ONLY: when admin is selected, or when any setting is TBD
    if (input$admin==1 | admin.check>0){
      showModal(modalDialog(title = tags$div(
        HTML("<center><font color='red'><big>ADMIN SETTINGS<big></font></center>")),
        tagList(fluidPage(
          conditionalPanel("input.admin==1",
                           ## A. Survey Solutions
                           fluidRow(
                             column(1),
                             column(10,
                                    helpText("ATTENTION: This is only for experienced users, and you must know what you do!")),
                             column(1)
                           ),
                           fluidRow(
                             column(4,
                                    textInput("suso.server", "Provide SuSo Server",
                                              value = admin.vars[["suso.server"]]),br(),br()),
                             
                             column(4,
                                    textInput("suso.user", "Provide SuSo user",
                                              value = admin.vars[["suso.user"]]),br(),br()),
                             column(4,
                                    textInput("suso.pass", "Provide SuSo password", 
                                              value = admin.vars[["suso.pass"]]),br(),br())
                           ),
                           ## B Email Settings
                           fluidRow(
                             column(4,
                                    textInput("email.adress", "Provide Email",
                                              value = admin.vars[["email.adress"]]),br(),br()),
                             column(4,
                                    textInput("email.subject", "Provide Email Subject",
                                              value = admin.vars[["email.subject"]]),br(),br()),
                             column(4,
                                    textInput("email.smtp", "Provide SMTP server",
                                              value = admin.vars[["email.smtp"]]),br(),br())
                           ), br(), br(),
                           fluidRow(
                             column(4,
                                    textInput("email.user", "Provide Email user name",
                                              value = admin.vars[["email.user"]]),br(),br()),
                             
                             column(4,
                                    textInput("email.pass", "Provide Email password",
                                              value = admin.vars[["email.pass"]]),br(),br()),
                             column(4,
                                    numericInput("email.port", "Provide Email server port", 
                                                 min=0, max=10000, step=1, 
                                                 value = as.numeric(admin.vars[["email.port"]])),br(),br())
                           )))),
        footer = tagList(actionButton("admin_save","Save Settings?")),
        easyClose = T, size = "l"
      ))
    }
  })
  
  # Whenever a field is filled, aggregate all form data
  formData <- eventReactive(input$admin_save, {
    removeModal()
    eadr<-input$email.adress
    esub<-input$email.subject
    esmt<-input$email.smtp
    eusr<-input$email.user
    epas<-input$email.pass
    epor<-input$email.port
    
    shiny::validate(need(eadr, message = F),
                    need(esub, message = F),
                    need(esmt, message = F),
                    need(eusr, message = F),
                    need(epas, message = F),
                    need(epor, message = F))
    data <- sapply(fields, function(x) input[[x]])
    #print(data)
  })
  
  ##  2. SETTINGS SAVE
  observeEvent(input$admin_save, {
    shiny::validate(need(formData(), message = F))
    #################
    ##  1. Store Settings in local directory
    file.remove(paste0("data/admin/", admin.files[1]))
    saveRDS(formData(), paste0("data/admin/admin_", format(Sys.time(), "%Y-%b-%dT%H:%M:%S"),
                               ".rds"))
    
    
    ##  2. Hand Over Settings
    ADMIN$settings<-formData()
    
  })
  
  ######################################################################################
  ##                Questionnaire Management
  ######################################################################################
  ##  1. Get list of questionnaires
  ##  FRAME_FINAL collects all final inputs
  FRAME_FINAL<-reactiveValues()
  quest<-reactiveValues()
  
  
  output$qTable <- DT::renderDataTable({
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    tab<-data.table(getQuest(url = settings[["suso.server"]], 
                             usr = settings[["suso.user"]], 
                             pass = settings[["suso.pass"]])$Questionnaires, key = c("Title", "Version"))
    quest$ID<-tab[,.(QuestionnaireId, Version)]
    tab<-tab[,.(Title, Version, QuestionnaireId, LastEntryDate)]
    quest$fullList<-tab
    DT::datatable(tab, selection = "single")
  })
  ##  2. Select one
  observeEvent(input$qTable_rows_selected,{
    sel<-input$qTable_rows_selected
    shiny::validate(need(sel, message = F))
    id<-quest$ID
    quest$id_sel<-id[sel, QuestionnaireId]
    quest$v_sel<-id[sel, Version]
  })
  
  ##  3. Display Status
  output$qSel <- DT::renderDataTable({
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    sel<-quest$id_sel
    v<-quest$v_sel
    shiny::validate(need(sel, message = "Select Questionnaire first!"))
    tab<-data.table(getQuestDet(qid=sel, 
                                url = settings[["suso.server"]], 
                                usr = settings[["suso.user"]], 
                                pass = settings[["suso.pass"]], 
                                v = v)$Interviews)
    tabl<-data.table(character(4), numeric(4), numeric(4))
    tabl[,V1:=c("Total Interviews", "Completed", "HQ Approved", "SV Approved")]
    tabl[,V2:=c(nrow(tab), 
                sum(tab$Status=="ApprovedByHeadquarters"),
                sum(tab$Status=="Completed"),
                sum(tab$Status=="Completed"))]
    DT::datatable(tabl, smTab, selection = "single", rownames = F,
                  colnames = c("","Count","Errors"), 
                  style = "bootstrap")
  })
  
  ##  4. Confirm selection for survey, by using modal Dialog
  observeEvent(input$qAss, {
    fullList<-quest$fullList
    sel<-quest$id_sel
    shiny::validate(need(sel, message = F))
    quest$id_sel_for_survey<-sel
    
    ## Modal for confirmation
    tab<-fullList[QuestionnaireId==sel]
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='red'><big>Please confirm the selection of your questionnaire!<big></font></center>")),
      DT::renderDataTable({
        DT::datatable(tab, options = list(dom = 't'))
      }),
      footer = tagList(actionButton("conf","Confirm?")),
      easyClose = T, size = "l"
    ))
  })
  
  observeEvent(input$conf, {
    removeModal()
    
    fullList<-quest$fullList
    sel<-quest$id_sel
    shiny::validate(need(sel, message = F))
    quest$id_sel_for_survey<-sel
    
    ## Modal for confirmation
    FRAME_FINAL$quest<-fullList[QuestionnaireId==sel]
  })
  
  ######################################################################################
  ##                Sample Management
  ######################################################################################
  frame<-reactiveValues()
  
  ##  1.1. NEW LIST - Get sheet names and create drop down
  observeEvent(input$frameList,{
    pathFrame<-input$frameList
    sn<-getSheetNames(pathFrame$datapath)
    updateSelectInput(session, "sheetSel",
                      "Select Sheet:",
                      sn)
    
  })
  
  ##  1.2. EXISTING LIST - Load the frame
  output$frameDir<-DT::renderDataTable({
    filesInDir<-file.info(list.files(path = "data/frames", pattern = ".csv", full.names = T))
    
    if(nrow(filesInDir)>0){
      ##  ATT: 2 splits to get only NAME of file
      filesInDir$name<-str_split(str_split(rownames(filesInDir), 
                                           pattern = c("data/frames/"), simplify = T)[,2], 
                                 pattern = ".csv", simplify = T)[,1]
      rownames(filesInDir)<-NULL
      filesInDir$size<- filesInDir$size/1000
      tab<-filesInDir[,c("name", "mtime", "size")]
      frame$fileList<-tab
      DT::datatable(tab, rownames = F, 
                    style = "bootstrap", 
                    colnames = c("List", "Upload date", "Size (in kb)"),
                    selection = "single")
    } else {
      tab<-data.table(a=character(1), b="No existing frames found!", c=character(1))
      DT::datatable(tab, rownames = F, 
                    style = "bootstrap", 
                    colnames = c("List", "Upload date", "Size (in kb)"),
                    selection = "none")
    }
    
  })
  
  
  ##  2. Load the selected sheet
  ##  2.1. FROM UPLOAD
  observeEvent(input$sheetSel,{
    pathFrame<-input$frameList
    frameSel<-input$sheetSel
    if(frameSel==0) return(NULL)
    frameSelTabel<-data.table(read.xlsx(pathFrame$datapath, 
                                        sheet = frameSel,colNames = T))
    
    ##  2.1. Check the selected Sheet
    col_email<-grep("email", tolower(names(frameSelTabel)))
    if (length(col_email)!=1 | nrow(frameSelTabel)==0) {
      showModal(modalDialog(title = tags$div(
        HTML("<center><font color='red'><big>The uploaded list is not in the required format!<big></font></center>")),
        renderUI(HTML(paste0("<big><center>Please read the instructions to the left</center></big><br>
                             <center>(You also see this message if the first sheet is not compatible, if this is the case,
                             just select another sheet from the drop-down menu to the left)")))))
    } else {
      frame$frameSel<-frameSelTabel
      frame$popsize<-nrow(frameSelTabel)
    }
  }, suspended = F)
  
  
  ##  2.2. From FILE
  observeEvent(input$frameDir_rows_selected,{
    pathFrame<-frame$fileList[input$frameDir_rows_selected,]
    frameSelTabel<-data.table(read_csv(file = paste0("data/frames/", pathFrame$name, ".csv")))
    frame$fileName<-pathFrame$name
    ##  2.1. Check the selected Sheet
    col_email<-grep("email", tolower(names(frameSelTabel)))
    if (length(col_email)!=1 | nrow(frameSelTabel)==0) {
      showModal(modalDialog(title = tags$div(
        HTML("<center><font color='red'><big>The uploaded list is not in the required format!<big></font></center>")),
        renderUI(HTML(paste0("<big><center>Please read the instructions to the left</center></big><br>
                             <center>(You also see this message if the first sheet is not compatible, if this is the case,
                             just select another sheet from the drop-down menu to the left)")))))
    } else {
      frame$frameSel<-frameSelTabel
      frame$popsize<-nrow(frameSelTabel)
    }
  }, suspended = F)
  
  ##  3. Count units in sheet
  output$PopSize<-renderText({
    shiny::validate(need(frame$popsize, message = F))
    n_units<-as.character(frame$popSize)
    return(n_units)
  })
  
  ##  3.1. Create table with number of Total Pop, and Minimum sample size
  observe({
    shiny::validate(need(frame$popsize, message = F))
    popsize<-frame$popsize
    ###############
    # sample size parameters:
    # proportion base on 0.3, t.test, one sample (CHANGE LATER!!)
    power<-1
    n_ttestProp<-popsize+10
    while (n_ttestProp>=popsize) {
      ##  Decrease Power until sample size works
      n_ttestProp<-ceiling(pwr.2p.test(h=0.3, power = power, 
                                       alternative = "two.sided")$n)
      power<-power-0.05
      
      if (power<0.1) {
        n_ttestProp<-popsize
        power<-0.1
        break
      }
    }
    
    ##  Continue when while LOOP is finished
    shiny::validate(need(n_ttestProp, message = F))
    sampleSizeTab<-data.table(A=c("Total Units in the list:",
                                  "Minimum recommended sample size:",
                                  "Maximum possible power:"),
                              N=c(popsize, n_ttestProp, power))
    frame$sampleSizeTab<-sampleSizeTab
    frame$n_ttestProp<-n_ttestProp
    frame$power<-power
  })
  
  ## 3.2. Table displayed in side bar
  output$sampleSizeTable<-DT::renderDataTable({
    tab<-frame$sampleSizeTab
    shiny::validate(need(tab, message = F))
    DT::datatable(tab, smTab, rownames = F,
                  selection = "none",
                  class = "hover",
                  style = "bootstrap",
                  colnames = c("","")) %>% 
      formatStyle("N",target = "row" ,
                  fontWeight = "bold",
                  backgroundColor = styleInterval(c(0.5 ,0.7), c('#EB1C2D','#FDB714', '00AB51')))
  })
  
  
  ##  4. Display selected table
  output$frameTable <- DT::renderDataTable({
    tab<-data.table(frame$frameSel)
    DT::datatable(tab, rownames = F, style = "bootstrap")
  })
  
  ##  5. Download of EXCEL template
  ##    --> Header: First, Last, Email, Phone
  output$template<-downloadHandler(
    filename = function(){"template.xlsx"},
    content = function(file) {
      tmpName <- paste0(file, ".xlsx")
      dwlcont<-data.table(First=character(0), 
                          Last=character(0), 
                          Email=character(0),
                          Phone=character(0))
      dwl<-loadWorkbook(tmpName, create = T)
      createSheet(dwl, name = "List1")
      writeWorksheet(dwl,dwlcont ,sheet = "List1")
      saveWorkbook(dwl)
      file.rename(tmpName, file)
    }
  )
  
  ##  5. Final sample selection
  ##  5.1 Update Numeric Field
  observe({
    n_ttestProp<-frame$n_ttestProp
    shiny::validate(need(n_ttestProp, message = F))
    updateNumericInput(session,
                       "sampSizeFinal", "Provide Sample Size",
                       value = n_ttestProp, step = 1)
  })
  
  observeEvent(input$nAss, {
    ##  5.2. Show the model with sample size and power!
    showModal(modalDialog(title = tags$div(
      HTML("<center><font color='red'><big>Please confirm the size of your sample!<big></font></center>")),
      renderUI(HTML(paste0(
        '<center>
        <big><font color="red">',
        input$sampSizeFinal, 
        '</font> Respondents</big><br>
        <big><font color="red">',
        frame$power, 
        '</font> Power</big>
                            </center>'))),
      footer = tagList(actionButton("conf1","Confirm?")),
      easyClose = T, size = "m"
    ))
    
    ##  5.3. Save the frame
    ##    --> Only if option new frame is selected
    if(input$frameNew==1){
      frameSel<-frame$frameSel
      shiny::validate(need(frameSel, message = "Please upload frame first!"))
      write_csv(frameSel, path = paste0("data/frames/", input$sheetSel, ".csv"))
    }
  })
  
  
  
  observeEvent(input$conf1, {
    removeModal()
    ss<-input$sampSizeFinal
    shiny::validate(need(ss, message = F))
    frame$sampSizeFinal<-ss
    
    ##  Pass on to assignments
    updateNumericInput(session, 
                       "n_ass", 
                       "Number of Assignments", 
                       value = ss, step = 1)
    
    ##  Create the sample
    frameSel<-frame$frameSel
    shiny::validate(need(frameSel, message = "Please upload frame first!"))
    frameSelFinal<-frameSel[, .SD[sample(.N, ss)]]
    frame$frameSelFinal<-frameSelFinal
    col_email<-grep("email", tolower(names(frameSelFinal)))
    
    shiny::validate(need(col_email, message ="NO EMAIL COLUMN FOUND!"))
    FRAME_FINAL$ssFinal<-ss
    FRAME_FINAL$prFinal<-frame$power
  })
  
  ######################################################################################
  ##            Assignment creation and control
  ###################################################################################### 
  ##  1. Display the selected sample
  output$FinalSampList<-DT::renderDataTable({
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    tab<-frame$frameSelFinal
    ss<-FRAME_FINAL$ssFinal
    shiny::validate(need(tab, message = F))
    ######################
    ##  Generate LOGIN code
    tab<-genCode(data=tab)
    
    ######################
    ##  Generat quest_num
    ##  --> get the assignments and calculate the last number, when empty, start with 1
    a<-assignments(server = settings[["suso.server"]],
                   apiUser = settings[["suso.user"]], 
                   apiPass = settings[["suso.pass"]])
    startID<-ifelse(is.null(a), 1, a[.N, Id]+1)
    tab[, quest_num:=seq.int(from = startID, to = (startID+.N-1))]
    ##  Save the sample
    ##    --> This is required for administration
    frameName<-ifelse(input$frameNew==1, input$sheetSel, frame$fileName)
    
    FRAME_FINAL$FINAL_Sample<-tab
    FRAME_FINAL$frameName<-frameName
    DT::datatable(tab, rownames = F, style = "bootstrap")
  })
  
  ###################################################################################### 
  ##  2. Create Email
  ##  Must contain 3 blocks
  ##      <Block1>
  ##        link
  ##      <Block2>
  ##        code
  ##      <Block3>
  ##  2.1 Block 1
  observeEvent(input$emText,{
    showModal(modalDialog(title = "Please provide text for Block 1",
                          tagList(fluidPage(
                            fluidRow(
                              column(width=6, textAreaInput("emText1", "Text Block 1",rows = 23)),
                              column(width=6, tags$img(src="emailP1.png"))))),
                          footer = tagList(actionButton("em_cont1","Continue?")),
                          easyClose = T, size = "l"
    ))
  })
  
  ##  2.2. Block 2
  observeEvent(input$em_cont1,{
    removeModal()
    showModal(modalDialog(title = "Please provide text for Block 2",
                          tagList(fluidPage(
                            fluidRow(
                              column(width=6, textAreaInput("emText2", "Text Block 2",rows = 23)),
                              column(width=6, tags$img(src="emailP2.png"))))),
                          footer = tagList(actionButton("em_cont2","Continue?")),
                          easyClose = T, size = "l"
    ))
  })
  
  ##  2.3. Block 3
  observeEvent(input$em_cont2,{
    removeModal()
    showModal(modalDialog(title = "Please provide text for Block 3",
                          tagList(fluidPage(
                            fluidRow(
                              column(width=6, textAreaInput("emText3", "Text Block 3",rows = 20)),
                              column(width=6, tags$img(src="emailP3.png"))),
                            fluidRow(radioButtons("instrLang", "Select Language of Questionnaire Instructions!", 
                                                  c("Nepalese", "English"), inline = T)))),
                          footer = tagList(downloadButton("emInspect", "Review Email?"),
                                           actionButton("em_cont3","Continue?")),
                          easyClose = T, size = "l"
    ))
  })
  
  observeEvent(input$em_cont3, {
    removeModal()
    
  })
  
  ##  2.4. Download of EMAIL template
  output$emInspect<-downloadHandler(
    filename = "EmailTemplate.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "emFullSample.Rmd")
      file.copy("mail/emFullSample.Rmd", tempReport, overwrite = TRUE)
      file.copy("mail/logo.png", tempdir(), overwrite = TRUE)
      file.copy("mail/style.css", tempdir(), overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file)
    }
  )
  
  ##  2.5. Get interviewers
  admin<-reactiveValues()
  observe({
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    
    SV<-getSV(url = settings[["suso.server"]], 
              usr = settings[["suso.user"]], 
              pass = settings[["suso.pass"]])$Users
    admin$sv<-SV
    SV<-SV$UserName
    updateSelectInput(session = session,
                      inputId = "teamSV", 
                      label = "Select Team", 
                      choices = SV)
  })
  
  observe({
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    sv_sel<-input$teamSV
    shiny::validate(need(sv_sel!="Select Team first!", message = "Select team first!"))
    sv<-data.table(admin$sv)
    svID<-sv[UserName==sv_sel, UserId]
    INT<-getINT(url = settings[["suso.server"]], 
                usr = settings[["suso.user"]], 
                pass = settings[["suso.pass"]], 
                sv =svID )$Users$UserName
    updateSelectInput(session = session,
                      inputId = "teamINT", 
                      label = "Select Team Member", 
                      choices = INT)
  })
  
  observe({
    FRAME_FINAL$teamINT<-input$teamINT
  })
  
  ###################################################################################### 
  ##  3. Confirm Survey Initiation
  observeEvent(input$create,{
    ##  Get final survey parameters and display
    # a) Questionnaire
    q<-FRAME_FINAL$quest[,.(Title)]
    ss<-as.character(FRAME_FINAL$ssFinal)
    pr<-as.character(FRAME_FINAL$prFinal)
    resp<-FRAME_FINAL$teamINT
    FULL_SETUP<-c(q,ss,pr, resp)
    
    ##  Check for NA and open warning if not 0
    CHECK<-length(FULL_SETUP)
    ##  a. All complete
    if (CHECK==4) {
      tab<-data.table(a=c("Questionnaire", "Sample Size", "Power", "Responsible"), b=FULL_SETUP)
      showModal(modalDialog(title = tags$div(HTML("<center>
                                                Attention, this will initate the survey with the following parameters:
                                                </center>")),
                            DT::renderDataTable(
                              DT::datatable(tab,smTab, selection = "single", rownames = F,
                                            colnames = c("","Parameter"), 
                                            style = "bootstrap")
                            ),
                            footer = tagList(actionButton("conf2","Confirm?")),
                            easyClose = T, size = "m"
      ))
    } else {
      showModal(modalDialog(title = tags$div(HTML("<center>
                                        <font color='red'> Some inputs are missing!</font>
                                        </center>")),
                            renderUI(HTML("<center>Please provide all required inputs, 
                                           i.e. Questionnaire, Sample and Email Text in the required format</center>")))) 
      
    } 
    
  })
  
  
  
  ##  FINAL CONFIRMATION --> THIS WILL START THE SURVEY
  ##  3. Confirm Survey Initiation
  observeEvent(input$conf2,{
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='red'><big>ATTENTION! THIS WILL START THE SURVEY<big></font></center>")),
      renderUI(HTML(paste0("<big><center>Do you wish to proceed?</center></big>"))),
      footer = tagList(actionButton("stop3","No"), actionButton("conf3","Yes")),
      easyClose = F, size = "m"
    ))
  })
  
  observeEvent(input$stop3,{
    removeModal()
  })
  
  observeEvent(input$conf3,{
    removeModal()
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    ##########################################
    ##  4. SEND EMAIL 
    nr<-FRAME_FINAL$FINAL_Sample
    frameName<-FRAME_FINAL$frameName
    ss<-FRAME_FINAL$ssFinal
    q_id<-FRAME_FINAL$quest[,.(QuestionnaireId)]
    q_v<-FRAME_FINAL$quest[,.(Version)]
    shiny::validate(need(nr, message = F),
                    need(input$instrLang, message="Select Instruction Language first!"),
                    need(input$teamINT, message="Select Responsible Team Member first!"),
                    need(q_id, message = F))
    
    
    
    ##  4.1. Create separate vectors for each element
    mail<-nr[["Email"]]
    idcode<-nr[["code"]]
    linkNUM<-nr[["quest_num"]]
    
    ##  4.2. Loop over email vector and send out emails
    ##     --> 
    withProgress(message = 'Creating Assignments', value = 0, {
      for(i in 1:length(mail)) {
        single.assignment<-data.table(WEBID=idcode[i], responsible=input$teamINT)
        resp<-createASS(df = single.assignment,
                        url = server, 
                        usr = APIuser, 
                        pass = APIpass,
                        QUID = q_id,
                        version =q_v)
        
        linkNUM[i]<-resp[[1]]$Assignment$Id
        create_mail(textBlock=c(input$emText1, input$emText2, input$emText3), 
                    sendMail=T, 
                    mail =mail[i], 
                    lang = input$instrLang, 
                    idcode =idcode[i],
                    qid = linkNUM[i],
                    link = settings[["suso.server"]],
                    email.adress = settings[["email.adress"]], 
                    email.smtp = settings[["email.smtp"]], 
                    email.subject = settings[["email.subject"]],
                    email.user = settings[["email.user"]],
                    email.pass = settings[["email.pass"]],
                    email.port = as.numeric(settings[["email.port"]]))
        
        
        incProgress(1/length(mail), detail = paste("Creating Assignment", i))
      }
    })
    #################################################
    ##  FINAL ID CODE FOR ASSIGNMENT
    ##    -is taken from the response on assignment creation
    ##    -only now the file has the correct websurvey id
    nr[,quest_num:=linkNUM]
    write_excel_csv(nr, path = paste0("data/sample/sample_",
                                       frameName,
                                       "_r", 
                                       ss, "_d", 
                                       format(Sys.time(), "%Y-%b-%dT%H:%M:%S"),
                                       ".csv"))
    
  })
  
  
  
  ######################################################################################
  ##            Sample Administration and control
  ###################################################################################### 
  ##  1. Get Data from selected questionnaire on p1, and select sample
  ##  1.1. EXISTING LIST - Show directory
  sampleAdmin<-reactiveValues()
  SAMPLE.ADMIN<-reactiveValues()
  output$sampleDir<-DT::renderDataTable({
    filesInDir<-file.info(list.files(path = "data/sample", pattern = ".csv", full.names = T))
    
    if(nrow(filesInDir)>0){
      ##  ATT: 2 splits to get only NAME of file
      tab<-str_split(str_split(rownames(filesInDir), 
                               pattern = c("data/sample/sample_"), simplify = T)[,2], 
                     pattern = "(_r)|(_d)|(.csv)", simplify = T)[,c(1,3,2)]
      rem<-str_split(str_split(rownames(filesInDir), 
                               pattern = c("data/sample/sample_"), simplify = T)[,2], 
                     pattern = "(_r)|(_d)|(.csv)", simplify = T)[,c(4)]
      sampleAdmin$fileList<-tab
      tab<-as.data.frame(tab)
      tab$rem<-ifelse(rem=="em", "Yes", "No")
      DT::datatable(tab, rownames = F, 
                    style = "bootstrap", 
                    colnames = c("List", "Creation date", "Sample Size", "Reminder"),
                    selection = "single")
    } else {
      tab<-data.table(a=character(1), b="No existing frames found!", c=character(1))
      DT::datatable(tab, rownames = F, 
                    style = "bootstrap", 
                    colnames = c("List", "Creation date", "Sample Size"),
                    selection = "none")
    }
    
  })
  
  
  ##  1.2. Load the data
  observeEvent(input$sampleDir_rows_selected,{
    pathFrame<-sampleAdmin$fileList[input$sampleDir_rows_selected,]
    sampleAdmin$pathFrame<-pathFrame
    
    frameSelTabel<-data.table(read_csv(file = paste0("data/sample/sample_", pathFrame[1],"_r",
                                                     pathFrame[3], "_d", pathFrame[2],
                                                     ".csv")))
    #frame$fileName<-pathFrame$name
    ##  2.1. Check the selected Sheet
    col_email<-grep("email", tolower(names(frameSelTabel)))
    
    if (length(col_email)!=1 | nrow(frameSelTabel)==0) {
      showModal(modalDialog(title = tags$div(
        HTML("<center><font color='red'><big>The uploaded list is not in the required format!<big></font></center>")),
        renderUI(HTML(paste0("<big><center>Please read the instructions to the left</center></big><br>
                             <center>(You also see this message if the first sheet is not compatible, if this is the case,
                             just select another sheet from the drop-down menu to the left)")))))
    } else {
      sampleAdmin$frameSel<-frameSelTabel
      sampleAdmin$popsize<-nrow(frameSelTabel)
    }
  }, suspended = F)
  
  ##  1.3  Display the selected sample
  output$SampleAdminList<-DT::renderDataTable({
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    
    tab<-sampleAdmin$frameSel
    q_name<-FRAME_FINAL$quest
    q_name<-q_name$Title
    q_id<-FRAME_FINAL$quest[,.(QuestionnaireId)]
    q_v<-FRAME_FINAL$quest[,.(Version)]
    shiny::validate(need(tab, message = F),
                    need(q_name, message = F))
    ######################
    ##  COMMENT: Check if WEBID has been provided, and questionnaire started
    ##    - marke respondet ones in green
    ##  i. Import the data
    completed<-data.table(fileCollector(questName = q_name,
                                        questID = q_id,
                                        version = q_v,
                                        inShinyApp = F,server = settings[["suso.server"]], 
                                        apiUser = settings[["suso.user"]], 
                                        apiPass = settings[["suso.pass"]],
                                        status = "Completed")[[1]][[1]])
    
    ##  i. select questionnaire with provided webid
    completed<-completed[webiduser != "##N/A##"]
    tab<-tab[,Part:=ifelse((code %in% completed$webiduser), 1, 0)]
    
    ######################
    # Write out Resp/NR
    sampleAdmin$Full_count<-nrow(tab)
    sampleAdmin$NR<-tab[Part==0]
    DT::datatable(tab, rownames = F, style = "bootstrap")  %>% formatStyle(
      'Part',
      target='row',
      backgroundColor = styleEqual(c(0, 1), c('#f1f1f1', 'yellow'))
    )
  })
  
  
  ###################################################################################### 
  ##  3. Send email Reminder
  ##  3.1. Final Check
  observeEvent(input$reminder, {
    ##  Get final survey parameters and display
    # a) Questionnaire
    q<-FRAME_FINAL$quest[,.(Title)]
    #   --> If not new assignment, then set interviewer to NONE
    resp<-ifelse(input$remNewAss==2,"NONE",FRAME_FINAL$teamINT)
    FULL_SETUP<-c(q, resp)
    print(input$teamINT)
    # b) Sample
    full_count<-as.character(sampleAdmin$Full_count)
    nr<-sampleAdmin$NR
    nr_count<-as.character(nrow(nr))
    
    FULL_SETUP<-c(q, resp, full_count, nr_count)
    ##  Check for NA and open warning if not 0
    CHECK<-length(FULL_SETUP)
    ##  a. All complete
    if (CHECK==4 & resp!="Select Team first!") {
      tab<-data.table(a=c("Questionnaire:", "Responsible:", "Sample Size:", "Number of Reminder:"), b=FULL_SETUP)
      showModal(modalDialog(title = tags$div(HTML("<center>
                                                Attention, this will send out the reminder with the following parameters:
                                                </center>")),
                            DT::renderDataTable(
                              DT::datatable(tab,smTab, selection = "single", rownames = F,
                                            colnames = c("","Parameter"), 
                                            style = "bootstrap")
                            ),
                            footer = tagList(actionButton("conf4","Confirm?")),
                            easyClose = T, size = "m"
      ))
    } else {
      showModal(modalDialog(title = tags$div(HTML("<center>
                                        <font color='red'> Some inputs are missing!</font>
                                        </center>")),
                            renderUI(HTML("<center>Please provide all required inputs, 
                                           i.e. Questionnaire, Sample and Email Text in the required format</center>")))) 
      
    } 
    
  })
  
  
  
  ##  FINAL CONFIRMATION --> THIS WILL START THE SURVEY
  ##  3. Confirm Survey Initiation
  observeEvent(input$conf4,{
    showModal(modalDialog(title =tags$div(
      HTML("<center><font color='red'><big>ATTENTION! THIS WILL SEND OUT THE REMINDER<big></font></center>")),
      renderUI(HTML(paste0("<big><center>Do you wish to proceed?</center></big>"))),
      footer = tagList(actionButton("stop3","No"), actionButton("conf5","Yes")),
      easyClose = F, size = "m"
    ))
  })
  
  observeEvent(input$stop3,{
    removeModal()
  })
  
  observeEvent(input$conf5,{
    removeModal()
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    ##########################################
    ##  4. SEND EMAIL
    full_count<-sampleAdmin$Full_count
    nr<-sampleAdmin$NR
    
    nr_count<-nrow(nr)
    respInt<-ifelse(input$remNewAss==2,"NONE",FRAME_FINAL$teamINT)
    q_id<-FRAME_FINAL$quest[,.(QuestionnaireId)]
    q_v<-FRAME_FINAL$quest[,.(Version)]
    shiny::validate(need(nr, message = F),
                    need(input$instrLang, message="Select Instruction Language first!"))
    
    ##  3.1 Creation of new quest_num (if selected!)
    ##    ALL IN LOOP NOW
    ##      IF NEW IS SELECTED:  
    ##          new id is taken from the assignment
    ##          written into vector, and saved with rem at the end
    
    
    ##  3.2. Create separate vectors for each element
    mail<-nr[["Email"]]
    idcode<-nr[["code"]]
    linkNUM<-nr[["quest_num"]]
    
    ##  3.3. Send email/create assignment
    withProgress(message = 'Creating Reminder', value = 0, {
      for(i in 1:length(mail)) {
        ##  If NEW assignment option is chosen
        if (input$remNewAss==1) {
        single.assignment<-data.table(WEBID=idcode[i], responsible=input$teamINT)
        resp<-createASS(df = single.assignment,
                        url = server, 
                        usr = APIuser, 
                        pass = APIpass,
                        QUID = q_id,
                        version =q_v)
        linkNUM[i]<-resp[[1]]$Assignment$Id
        }
        create_mail(textBlock=c(input$emText1, input$emText2, input$emText3), 
                    sendMail=T, 
                    mail =mail[i], 
                    lang = input$instrLang, 
                    idcode =idcode[i],
                    qid = linkNUM[i],
                    link = settings[["suso.server"]],
                    email.adress = settings[["email.adress"]], 
                    email.smtp = settings[["email.smtp"]], 
                    email.subject = settings[["email.subject"]],
                    email.user = settings[["email.user"]],
                    email.pass = settings[["email.pass"]],
                    email.port = as.numeric(settings[["email.port"]]))
        incProgress(1/length(mail), detail = paste("Creating Reminder", i))
      }
    })
    if (input$remNewAss==1) {
      nr[,quest_num:=linkNUM]
      pathFrame<-sampleAdmin$pathFrame
      p<-paste0("data/sample/sample_", pathFrame[1],"_r",
                pathFrame[3], "_d", pathFrame[2],
                "_rem.csv")
      write_excel_csv(nr, path = p)
      
    }
  }) 
  
  ######################################################################################
  ##                PHONE follow up
  ######################################################################################
  ##  1. Display the sample and check for completed ones
  output$SampleAdminList1<-DT::renderDataTable({
    settings<-ADMIN$settings
    shiny::validate(need(settings, message="No Server Settings Provided"))
    tab<-sampleAdmin$frameSel
    q_name<-FRAME_FINAL$quest
    q_name<-q_name$Title
    q_id<-FRAME_FINAL$quest[,.(QuestionnaireId)]
    q_v<-FRAME_FINAL$quest[,.(Version)]
    shiny::validate(need(tab, message = F),
                    need(q_name, message = F))
    ######################
    ##  COMMENT: Check if WEBID has been provided, and questionnaire started
    ##    - marke respondet ones in green
    ##  i. Import the data
    completed<-data.table(fileCollector(questName = q_name,
                                        questID = q_id,
                                        version = q_v,
                                        inShinyApp = F,server = settings[["suso.server"]], 
                                        apiUser = settings[["suso.user"]], 
                                        apiPass = settings[["suso.pass"]],
                                        status = "Completed")[[1]][[1]])
    ##  i. select questionnaire with provided webid
    completed<-completed[webiduser != "##N/A##"]
    tab<-tab[,Part:=ifelse((code %in% completed$webiduser), 1, 0)]
    
    ######################
    # Write out Resp/NR
    sampleAdmin$NRphone<-tab
    DT::datatable(tab, rownames = F, style = "bootstrap")  %>% formatStyle(
      'Part',
      target='row',
      backgroundColor = styleEqual(c(0, 1), c('#f1f1f1', 'yellow'))
    )
  })
  
  ##  2. Take the selected ones, and assign them to the responsible phone interviewer.
  ##  2.1. Load the data
  observeEvent(input$SampleAdminList1_rows_selected,{
    phoneInterviewer<-sampleAdmin$NRphone[input$SampleAdminList1_rows_selected]
    sampleAdmin$phoneInterviewer<-phoneInterviewer
  }, suspended = F)
  
  ##  2.2 Show list in modal
  observeEvent(input$assignCATI, {
    tab<-sampleAdmin$phoneInterviewer
    showModal(modalDialog(title = tags$div(HTML("<center>
                                                Attention, this will assign the following interviews
                                                to the selected Phone interviewer:
                                                </center>")),
                          DT::renderDataTable(
                            DT::datatable(tab,smTab, selection = "none", rownames = F,
                                          colnames = c("","Parameter"), 
                                          style = "bootstrap")
                          ),
                          footer = tagList(downloadButton("dwlPHONE","Download list for Interviewer"),
                                           actionButton("conf6","Confirm?")),
                          easyClose = T, size = "l"
    ))
    
  })
  
  output$dwlPHONE<-downloadHandler(
    filename = function(){paste0("phonelist",input$teamINT, ".xlsx")},
    content = function(file) {
      tmpName <- paste0(file, ".xlsx")
      dwlcont<-data.table(sampleAdmin$phoneInterviewer)
      dwl<-loadWorkbook(tmpName, create = T)
      createSheet(dwl, name = "List1")
      writeWorksheet(dwl,dwlcont ,sheet = "List1")
      saveWorkbook(dwl)
      file.rename(tmpName, file)
    }
  )
  
  observeEvent(input$conf6, {
    removeModal()
    
    
  })
  
  
  
  
  ######################################################################################
  ##                User Management
  ######################################################################################
  staff<-reactiveValues()
  output$svTable <- DT::renderDataTable({
    tab<-data.table(getSV()$Users)
    staff$SV<-tab
    DT::datatable(tab, selection = "single")
  })
  
  observeEvent(input$svTable_rows_selected,{
    sel<-input$svTable_rows_selected
    shiny::validate(need(sel, message = "Select Supervisor first!"))
    sv<-staff$SV
    sv_sel<-sv[sel, 3]
    staff$sv_sel<-sv_sel
  })
  
  
  output$intTable <- DT::renderDataTable({
    sel<-staff$sv_sel
    shiny::validate(need(sel, message = "Select Supervisor first!"))
    tab<-data.table(getINT(sv=sel)$Users)
    DT::datatable(tab, smTab, selection = "single", style = "bootstrap")
  })
  
  
  
  #############END ###########################################END ##############################
  
})
