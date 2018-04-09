#############################################
###     VERSION 2.0
###     Works with KNIT2HTML
###       --server side


########################################
##  MAILSURVEY CREATOR 0.1
create_mail<-function(textBlock=c(input$emText1,
                                  input$emText2,
                                  input$emText3),
                      link="",
                      linkposition=2,
                      idcode="",
                      idposition=4,
                      qid=67,
                      sendMail=F,
                      mail="",
                      lang="Nepalese",
                      email.adress="",
                      email.subject="",
                      email.smtp="",
                      email.user="",
                      email.pass="",
                      email.port=465) {
  
  ###################  ###################  ###################  ###################  ###################
  ##  1. Modify link and the code
  ##    --> values are handed over by quid and 
  
  link<-paste0('<center><a class="c18" href="',link,'/WebInterview/', qid ,
               '/Start">',link,'/WebInterview/',qid,
               '/Start</a></span></p><br><br></center>')
  idcode<-paste0("<br><br><center>", idcode, "</center><br><br>")
  email.adress<-paste0("<", email.adress, ">")
  
  
  ###################  ###################  ###################  ###################  ###################
  ## 2. CREATE document with knit2html (only this works with sendmail)
  ## 2.1 Prepare email content
  instr_file<-ifelse(lang=="Nepalese", "mail/QUESTIONNAIRE_INSTRUCTIONS_NEPAL.pdf",
                     "mail/QUESTIONNAIRE_INSTRUCTIONS_ENGL.pdf")
  textElement<-list(emText1=textBlock[1],
                    link=link,
                    emText2=textBlock[2],
                    idcode=idcode,
                    emText3=textBlock[3])
  
  #################
  ## 2.2. Set up the temporary Directory
  require(knitr)
  TMPDIR<-tempdir()
  fp<-paste0(TMPDIR, "/tempMail.html")
  tempReport<-file.path(TMPDIR, "emFullKnitr.Rmd")
  tempStyle<-file.path(TMPDIR, "style.css")
  tempLogo<-file.path(TMPDIR, "logo.png")
  file.copy("mail/emFullKnitr.Rmd", tempReport, overwrite = T)
  file.copy("mail/style.css", tempStyle, overwrite = T)
  file.copy("mail/logo.png", tempLogo, overwrite = T)
  #################
  ## 2.3 KNITR
  ##  -->Change the working directory for knitting the report
  wd.old<-getwd()
  setwd(TMPDIR)
  knitr::knit2html(input = "emFullKnitr.Rmd",
                   output = "tempMail.html", 
                   options="", 
                   encoding = "UTF-8",
                   stylesheet = "style.css")
  setwd(wd.old)
  
  ###################  ###################  ###################  ###################  ###################
  ## 3. Send the email
  
  if(sendMail==TRUE) {
    library("mailR")
    send.mail(from = email.adress,
              to = mail,
              subject = email.subject,
              body = fp, 
              encoding = "utf-8",
              html = TRUE,
              inline = T,
              smtp = list(host.name = email.smtp, 
                          port = email.port, 
                          user.name = email.user, 
                          passwd = email.pass, 
                          ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              attach.files = instr_file,
              file.names = c("QUESTIONNAIRE_INSTRUCTIONS.pdf"))
  } else {
    file.copy(from = fp, to = "check2.html", overwrite = T)
  }
  
}
