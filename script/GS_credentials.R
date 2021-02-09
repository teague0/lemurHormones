#Set google credentials

library(tidyverse)
library(googlesheets4)

myemail <-  "replaceEmail@gmail.com"
gs4_auth(email = myemail)
