#######################
##  Questionnaire 
##  1. get questionnaires
getQuest<-function(url=settings[["suso.server"]],
                   usr=settings[["suso.user"]],
                   pass=settings[["suso.pass"]]){
  ##  Define the api 
  url=paste0(url, "/api/v1/questionnaires")
  test_detail<-GET(url = paste0(url, "?limit=200"), 
                   authenticate(usr, pass, type = "basic"))
  aJsonFile<-tempfile()
  writeBin(content(test_detail, "raw"), aJsonFile)
  test_json<-fromJSON(aJsonFile)
  return(test_json)
}


getQuestDet<-function(url=settings[["suso.server"]],
                      usr=settings[["suso.user"]],
                      pass=settings[["suso.pass"]],
                      qid="da66f577-633a-41f6-8530-e662d8e97fe4",
                      v=1){
  ##  Define the api 
  url=paste0(url, "/api/v1/questionnaires/")
  test_detail<-GET(url = paste0(url, qid, "/", v , "/interviews"), 
                   authenticate(usr, pass, type = "basic"))
  
  aJsonFile<-tempfile()
  writeBin(content(test_detail, "raw"), aJsonFile)
  test_json<-fromJSON(aJsonFile)
  return(test_json)
  
}


