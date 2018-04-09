# CAWI-interface-with-Shiny
A web-interview interface for Survey Solutions, all programmed in shiny. If you want to use this as is, then you need to use the questionnaire from this link.


## Description

This is a completely external interface for the the Survey Solutions CAWI capabilities. It allows you to control the server, almost without using the
already comprehensive standard interface. 

Its main functions are the following:

* Select the questionnaire from the ones available on your SuSo server
** if you want to use the application without modifications in the code, then use this questionnaire: https://designer.mysurvey.solutions/questionnaire/details/57bce558bd8c466e870fc2cc828507a7/chapter/a404e5d57e00379098f2a530a6f442f6/group/a404e5d57e00379098f2a530a6f442f6
** in the shiny app shared in this repository, it works with a self created WEBID variable. The resto of your questionnaire can be modified
* Create email and the assignment in one go
** You need to use an smtp server for this


