# Computer Assisted Web Interview (CAWI) interface for Survey Solutions with R's Shiny library
A web-interview interface for Survey Solutions, all programmed in shiny. If you want to use this as is, then you need to use the questionnaire from the link bellow. 

## Description

This is a completely external interface for the the Survey Solutions CAWI capabilities. It allows you to control the server, almost without using the
already comprehensive standard interface on the main server. 

Its main functions are the following:

* Select the questionnaire from the ones available on your SuSo server.
* Upload your sampling frame, and also receive power and samplel size calculation.
* Create email and the assignment in one go.
* Finally you can manage the frames/samples and also send out reminders.


## Restrictions/Requirements:
* If you want to use the application without modifications in the code, then use this questionnaire:      https://designer.mysurvey.solutions/questionnaire/public/6d45e18f218b4468b726d88d55c39ee2
* This shiny app, works with a WEBID variable (which needs to be provided to the questionniare). The rest of your questionnaire can be modified as you wish.
* You need to use your own email server for this (if you are not bound to a company server, gmail works quite well, depending on the size of your sample).

## Further information on Survey Solutions
http://support.mysurvey.solutions/

https://www.youtube.com/channel/UCzGrZ3FiEu5NbDJB2GKazOA





