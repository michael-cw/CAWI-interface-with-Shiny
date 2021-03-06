#try(setwd("../"), silent = T)
########################################################################################
##  API module to retrieve the data
##    i) Function returns a LIST with up to 4 different lists
##    ii) Number of lists depends on the level of nesting
##    iii) All variable names are transformed to lower case
##    iv) Consistent id variables are generated with
##          a) Questionnaire id transformed to id
##          b) parent ids consistently number starting from id (questionnairid) to idX (maximum id3)
##    v) Uses data.table
##    vi) Version includes re-load time in SECONDS
########################################################################################

fileCollector<-function(questName="",
                        server= "xxx.mysurvey.solutions",
                        apiUser="apiUser",
                        apiPass="apiPass",
                        questID="xxx-xxx",
                        version=1,
                        status="ApprovedByHeadquarter",
                        reloadTimeDiff=0,
                        inShinyApp=F){
  #######################################
  ## Load the libraries
  library("httr")
  library("xml2")
  library("jsonlite")
  library("haven")
  library("data.table")
  library("plyr")
  library("dplyr")
  library("stringdist")
  
  #######################################
  ##  Update DETAILS
  source("helpers/suso_api_export_details.R")
  ##  OPTIONS: i) No scientific notation
  options("scipen"=100, "digits"=4)
  url<-paste0(server, "/api/v1/export/")
  usr<-apiUser
  pass<-apiPass
  quid<-paste0(questID, "$", version)
  format<-"STATA"
  type<-""
  
  ###############################################
  ##  check last generation of file by detail TIME
  time_limit<-strptime(getSuSoDetails()$LastUpdateDate, format = "%Y-%m-%dT%H:%M:%S")
  current_time<-strptime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  ##  TIME difference in HOURS (default is 12)
  ##  IFF time diff is larger than treshold, then new file is generated.
  if(difftime(current_time, time_limit)>reloadTimeDiff){
    test_post<-POST(url = paste0(url, format,"/", quid, "/", "start"), authenticate(usr, pass, type = "basic"))
  }
  ##  Receive the status
  test_detail<-GET(url = paste0(url, format,"/", quid, "/", "details"), authenticate(usr, pass, type = "basic"))
  aJsonFile<-tempfile()
  writeBin(content(test_detail, "raw"), aJsonFile)
  test_json<-fromJSON(aJsonFile)
  
  ## Wait until file is loaded.
  while (!is.null(test_json$RunningProcess)) {
    ####print(paste0(test_json$RunningProcess$ProgressInPercents, "..."))
    test_detail<-GET(url = paste0(url, format,"/", quid, "/", "details"), authenticate(usr, pass, type = "basic"))
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    ## For progressindicator in Shiny
    if(inShinyApp) incProgress(0.1)
  }
  
  ##  Get the last updated date
  lastUpdate<-test_json$LastUpdateDate
  
  ##  Download the file
  test_exp<-GET(url = paste0(url, format,"/", quid, "/", ""), authenticate(usr, pass, type = "basic"))
  # write the content of the download to a binary file
  
  ### Create a tempfile aZipFile
  ### Write the raw data from the content to the tempfile
  ### Unzip the tempfile
  
  aZipFile<-tempfile()
  writeBin(content(test_exp, "raw"), aZipFile)
  file_list<-unzip(aZipFile, files = questName, list=T)
  
  #########################################
  ## Extracting files with loop
  files<-file_list[file_list$Length>0,1]
  files<-files[files!="export__readme.txt"]
  
  ##  Functions for files
  file_collector<-list()
  file_collector.main<-list()
  file_collector.rost.L1<-list()
  file_collector.rost.L2<-list()
  file_collector.rost.L3<-list()
  
  ##  Extract to tmpdir
  tmp_zip_dir<-tempdir()
  unzip(aZipFile, exdir = tmp_zip_dir)
  statusControl<-read.dta13(paste0(tmp_zip_dir,"/interview__actions.dta"), 
                            generate.factors = T, encoding = "UTF-8", convert.factors = T)
  names(statusControl)<-tolower(names(statusControl))
  statusControl<-plyr::rename(statusControl, replace=c("interview__id"="id"))
  tmpName<-names(statusControl)
  MAINparIDcol<-grep(pattern = "^id", tmpName)
  statusControl<-data.table(statusControl, key=tmpName[MAINparIDcol])
  ###print(statusControl)
  statusControl<-statusControl[action==status]
  statusControl<-unique(statusControl, by=tmpName[MAINparIDcol])
  ########################################
  ##  The export loop
  ##    - all from tmp dir
  ##    - checks roster by id
  ##    - USE haven stata, as else file corrupted
  CHECKlist<-list()
  for (file_zip in files) {
    name<-strsplit(file_zip, ".dta")[[1]]
    #CHECKlist[[name]]<-read.dta13(file = paste0(tmp_zip_dir,"/", file_zip), fromEncoding = "UTF-8", convert.factors = T, generate.factors = T)
    if (exists("tmp_file")) tmp_file<-NULL
    tmp_file <-data.table(haven::read_dta(paste0(tmp_zip_dir,"/", file_zip), encoding = "UTF-8"))
    
    ######################################################################
    ##  LABELS
    ##  1. Value Labels (single select)
    ##    - identified by attribute...?? check for other solutions, not stable!!!
    catVars<-sapply(names(tmp_file), function(x) ifelse(is.null(attr(tmp_file[[x]], "labels")), 
                                                        return(NULL), return(attr(tmp_file[[x]], "labels"))), simplify = T, USE.NAMES = T)
    #print(catVars)
    catVars<-catVars[-which(sapply(catVars, is.null))]
    ##  1.2. Assign
    coll<-names(catVars)
    for (v in coll) {
      #print(v)
      #tmp_file[,(v):=.(tmp=as_factor(v), tmp)]
      set(tmp_file, NULL, v, as_factor(tmp_file[[v]]))
    }
    
    ##  2. Multi Select
    ms<-grep("__[0-9]*$",names(tmp_file), perl=T)
    ###print(names(tmp_file)[ms])
    ##print(names(tmp_file))
    CHECKlist[[name]]<-tmp_file
    ######################################################################
    ##  READING THE FILES
    ##  1. MAIN FILE (defined by user)
    if (name==questName){
      ###print("HOLLA")
      #CHECKER2<<-statusControl
      tmpName<-tolower(names(tmp_file))
      names(tmp_file)<-tmpName
      MAINparIDcol<-grep(pattern = "^.+__id$", tmpName)
      main_file<-copy(tmp_file)
      setkeyv(main_file, tmpName[MAINparIDcol])
      CHECKER<<-main_file
      attributes(tmp_file)$rosterlevel<-"main"
      ###print(statusControl)
      ###print(main_file)
      main_file<-main_file[statusControl, nomatch=0]
      ###print(main_file)
      file_collector.main[[paste(name)]]<-main_file
    } else if (name=="interview__comments") {
      ##  2. COMMENT FILE (standard name, parentid=interview__id)
      tmpName<-tolower(names(tmp_file))
      names(tmp_file)<-tmpName
      tmp.parIDcol<-grep(pattern = "^interview__id", names(tmp_file))
      tmp.idCol<-grep(pattern = "^id", names(tmp_file))
      comments<-data.table(tmp_file, key=tmpName[tmp.parIDcol])
      file_collector.main[[paste(name)]]<-comments
      
    } else {
      ##  4. OTHER FILE (length parentid defines nesting)
      tmpName<-tolower(names(tmp_file))
      names(tmp_file)<-tmpName
      tmp.parIDcol<-grep(pattern = "parentid", names(tmp_file))
      nesting<-ifelse(is.numeric(tmp_file$id), length(tmp.parIDcol), length(tmp.parIDcol)-1)
      ###print(nesting)
      if (nesting==1 & length(tmp.parIDcol)==1 & nrow(tmp_file)!=0){
        ##  4.1. ROSTER LEVEL 1
        names(tmp_file)<-tolower(names(tmp_file))
        tmp_file<-plyr::rename(tmp_file, replace=c("parentid1"="id", "id"="id1"))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])
        
        
        tmp_file<-merge(tmp_file, statusControl, by="id", allow.cartesian=TRUE)
        setkeyv(tmp_file, c(tmpName[MAINparIDcol], tmpName[r1id]) )
        file_collector.rost.L1[[paste(name)]]<-tmp_file
      } else if (nesting==1 & length(tmp.parIDcol)==2& nrow(tmp_file)!=0){
        ##  4.1. ROSTER LEVEL 1 + FIXED ROSTER
        tmp_file<-plyr::rename(tmp_file, replace=c("parentid2"="id","parentid1"="id1", "id"="fix.id"))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])
        tmp_file<-merge(tmp_file, statusControl, by="id", allow.cartesian=TRUE)
        setkeyv(tmp_file, c(tmpName[MAINparIDcol], tmpName[r1id]) )
        file_collector.rost.L1[[paste(name)]]<-tmp_file
      } else if (nesting==2& nrow(tmp_file)!=0) {
        ##  4.2. ROSTER LEVEL 2
        tmp_file<-plyr::rename(tmp_file, replace=c("parentid2"="id", "parentid1"="id1", "id"="id2"))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        r2id<-grep(pattern = "^id2$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])
        tmp_file<-merge(tmp_file, statusControl, by="id", allow.cartesian=TRUE)
        setkeyv(tmp_file, c(tmpName[MAINparIDcol], tmpName[r1id], tmpName[r2id]))
        file_collector.rost.L2[[paste(name)]]<-tmp_file
      } else if (nesting==2& nrow(tmp_file)!=0) {
        ##  4.3. ROSTER LEVEL 2
        tmp_file<-plyr::rename(tmp_file, replace=c("parentid3"="id", "parentid2"="id1", "parentid3"="id2", "id"="id3"))
        tmpName<-tolower(names(tmp_file))
        MAINparIDcol<-grep(pattern = "^id$", tmpName)
        r1id<-grep(pattern = "^id1$", tmpName)
        r2id<-grep(pattern = "^id2$", tmpName)
        r3id<-grep(pattern = "^id3$", tmpName)
        tmp_file<-data.table(tmp_file, key=tmpName[MAINparIDcol])
        tmp_file<-merge(tmp_file, statusControl, by="id", allow.cartesian=TRUE)
        setkeyv(tmp_file, c(tmpName[MAINparIDcol], tmpName[r1id], tmpName[r2id], tmpName[r3id]))
        file_collector.rost.L3[[paste(name)]]<-tmp_file
      }
    }
    
    if (inShinyApp) incProgress(0.1)
  }
  
  ######################################################################
  ##  MERGING THE FILES
  ##    i) By Section for Nested roster
  ##    ii) Exclude the nested rosters by using is.numeric
  ##  1. Split of section identifier, must be by "_"
  sectionR1<-sapply(names(file_collector.rost.L1), function(x) strsplit(x, "_")[[1]][1], simplify = T)
  attributes(sectionR1)<-NULL
  sectionR2<-sapply(names(file_collector.rost.L2), function(x) strsplit(x, "_")[[1]][1], simplify = T)
  attributes(sectionR2)<-NULL
  
  ##  2. Merge them 
  if(is.matrix(sectionR1) & is.matrix(sectionR2)){
    R1inR2<-as.data.frame(stringdistmatrix(sectionR1, sectionR2, method = "osa"))
    if (nrow(R1inR2)!=0){
      R1inR2match<-lapply(1:length(R1inR2[,1]), function(i) which(R1inR2[i,]==0) )
      for (i in 1:length(file_collector.rost.L1)){
        if (length(R1inR2match[[i]])!=0){
          for (k in R1inR2match[[i]]) {
            file_collector.rost.L1[[i]]<-plyr::join(file_collector.rost.L1[[i]], file_collector.rost.L2[[k]],
                                                    by=c("id", "id1"))
          }
        }
      }
    }} else if (is.matrix(sectionR1)) {
      for (i in 1:length(file_collector.rost.L1)){
        file_collector.rost.L1[[i]]<-file_collector.rost.L1[[i]]
      }
      
    }
  ######################################################################
  ##  COLLECTING THE LISTS
  ##  1. MAIN (always exists, no check)
  file_collector[["main"]]<-file_collector.main
  ##  2. ROSTER LEVEL 1
  
  if(exists("file_collector.rost.L1")){
    file_collector[["R1"]]<-file_collector.rost.L1}
  ##  3. ROSTER LEVEL 2
  if(exists("file_collector.rost.L2")){
    file_collector[["R2"]]<-file_collector.rost.L2}
  ##  4. ROSTER LEVEL 3
  if(exists("file_collector.rost.L3")){
    file_collector[["R3"]]<-file_collector.rost.L3}
  #return(CHECKlist)
  return(file_collector)
  
  
  ################################################ FIN ######################################################
}

