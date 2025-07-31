### TITLE: Análisis de campañas ###
### AUTHOR: GEORGINA UREÑA BALLESTERO ###
### DATE: 2024-01-03 ###

# Libraries
library(dplyr)
library(jsonlite)

# Buscar la ruta del archivo
file.choose()


### --- Dataset de campañas

# Carga
campaigns = fromJSON("C:\\Users\\gurenab\\Documents\\Análisis de campañas\\campaigns_dst.json", flatten = TRUE)
str(campaigns)
skimr::skim(campaigns)

# Depuración
campaigns = campaigns %>%
  select(campaignid, campaignname, campaigndescription, campaigndatetimestart, campaigndatetimeend, countryname, supervisorname, username, cli_identification, debtorid)

campaigns$campaignid = as.factor(campaigns$campaignid)
campaigns$campaigndatetimestart = as.Date(campaigns$campaigndatetimestart)
campaigns$campaigndatetimeend = as.Date(campaigns$campaigndatetimeend)
campaigns$countryname = as.factor(campaigns$countryname)
campaigns$debtorid = as.factor(campaigns$debtorid)

campaigns$campaignname = trimws(campaigns$campaignname)
campaigns$campaignname = toupper(stringi::stri_trans_general(campaigns$campaignname,"Latin-ASCII"))
campaigns$campaigndescription = trimws(campaigns$campaigndescription)
campaigns$campaigndescription = toupper(stringi::stri_trans_general(campaigns$campaigndescription,"Latin-ASCII"))
campaigns$supervisorname = trimws(campaigns$supervisorname)
campaigns$supervisorname = toupper(stringi::stri_trans_general(campaigns$supervisorname,"Latin-ASCII"))
campaigns$username = trimws(campaigns$username)
campaigns$username = toupper(stringi::stri_trans_general(campaigns$username,"Latin-ASCII"))

skimr::skim(campaigns)

# Se guarda el archivo
write.csv(campaigns, "campaigns.csv")


### --- Dataset de llamadas

# Carga
calls = fromJSON("C:\\Users\\gurenab\\Documents\\Análisis de campañas\\calls_dst.json", flatten = TRUE)
str(calls)
skimr::skim(calls)

# Depuración
calls = calls %>%
  select(cli_identification, identification, year, month, day, fullname, company, birthdate, countryname, institutiondescription, debtortypedescription, 
         acconttypedescription, communicationtype, ind_status, calldate, lastapp, duration, billsec, disposition, username, campaignname, campaignid, debtorid)

calls$year = as.factor(calls$year)
calls$month = as.factor(calls$month)
calls$day = as.factor(calls$day)
calls$company = as.factor(calls$company)
calls$birthdate = as.Date(calls$birthdate)
calls$countryname = as.factor(calls$countryname)
calls$institutiondescription = as.factor(calls$institutiondescription)
calls$debtortypedescription = as.factor(calls$debtortypedescription)
calls$acconttypedescription = as.factor(calls$acconttypedescription)
calls$communicationtype = as.factor(calls$communicationtype)
calls$ind_status = as.factor(calls$ind_status)
calls$calldate = as.Date(calls$calldate)
calls$lastapp = as.factor(calls$lastapp )
calls$disposition = as.factor(calls$disposition)
calls$campaignid = as.factor(calls$campaignid)
calls$debtorid = as.factor(calls$debtorid)

calls$fullname = trimws(calls$fullname)
calls$fullname = toupper(stringi::stri_trans_general(calls$fullname,"Latin-ASCII"))
calls$company = trimws(calls$company)
calls$company = toupper(stringi::stri_trans_general(calls$company,"Latin-ASCII"))
calls$username = trimws(calls$username)
calls$username = toupper(stringi::stri_trans_general(calls$username,"Latin-ASCII"))
calls$campaignname = trimws(calls$campaignname)
calls$campaignname = toupper(stringi::stri_trans_general(calls$campaignname ,"Latin-ASCII"))

# Para calcular la edad a partir de la fecha de nacimiento
calls$birth_year = as.numeric(substr(calls$birthdate, 1,4))

today = Sys.Date() %>%
  format("%Y") %>%
  as.numeric()
calls = calls %>%
  mutate(age = today-birth_year)
table(calls$age,useNA = "always")

calls$birth_year = as.factor(calls$birth_year)

skimr::skim(calls)

# Se guarda el archivo
write.csv(calls, "calls.csv")


### --- Dataset de SMS

# Carga
sms = fromJSON("C:\\Users\\gurenab\\Documents\\Análisis de campañas\\sms_dst.json", flatten = TRUE)
str(sms)
skimr::skim(sms)

# Depuración
sms = sms %>%
  select(cli_identification, identification, year, month, day, fullname, company, birthdate, countryname, institutiondescription, debtortypedescription, 
         acconttypedescription, communicationtype, ind_status, message, senddate, sms_quantity, campaignname, campaignid, debtorid)

sms$year = as.factor(sms$year)
sms$month = as.factor(sms$month)
sms$day = as.factor(sms$day)
sms$birthdate = as.Date(sms$birthdate)
sms$countryname = as.factor(sms$countryname)
sms$institutiondescription = as.factor(sms$institutiondescription)
sms$debtortypedescription = as.factor(sms$debtortypedescription)
sms$acconttypedescription = as.factor(sms$acconttypedescription)
sms$communicationtype = as.factor(sms$communicationtype)
sms$ind_status = as.factor(sms$ind_status)
sms$senddate = as.Date(sms$senddate)
sms$campaignid = as.factor(sms$campaignid)
sms$debtorid = as.factor(sms$debtorid)

sms$fullname = trimws(sms$fullname)
sms$fullname = toupper(stringi::stri_trans_general(sms$fullname,"Latin-ASCII"))
sms$company = trimws(sms$company)
sms$company = toupper(stringi::stri_trans_general(sms$company,"Latin-ASCII"))
sms$campaignname = trimws(sms$campaignname)
sms$campaignname = toupper(stringi::stri_trans_general(sms$campaignname,"Latin-ASCII"))

# Para calcular la edad a partir de la fecha de nacimiento
sms$birth_year = as.numeric(substr(sms$birthdate, 1,4))

today = Sys.Date() %>%
  format("%Y") %>%
  as.numeric()
sms = sms %>%
  mutate(age = today-birth_year)
table(sms$age,useNA = "always")

sms$birth_year = as.factor(sms$birth_year)

skimr::skim(sms)

# Se guarda el archivo
write.csv(sms, "sms.csv")


### --- Dataset de Whatsapp

# Carga
whatsapps = fromJSON("C:\\Users\\gurenab\\Documents\\Análisis de campañas\\whatsapp_dst.json", flatten = TRUE)
str(whatsapps)
skimr::skim(whatsapps)

# Depuración
whatsapps = whatsapps %>%
  select(cli_identification, identification, year, month, day, fullname, company, birthdate, countryname, institutiondescription, debtortypedescription,
         acconttypedescription, communicationtype, ind_status, message, messagetype, senddate, read_flag, username, campaignname, campaignid, debtorid)

whatsapps$year = as.factor(whatsapps$year) 
whatsapps$month = as.factor(whatsapps$month)
whatsapps$day = as.factor(whatsapps$day)
whatsapps$birthdate = as.Date(whatsapps$birthdate)
whatsapps$countryname= as.factor(whatsapps$countryname)
whatsapps$institutiondescription = as.factor(whatsapps$institutiondescription)
whatsapps$debtortypedescription = as.factor(whatsapps$debtortypedescription)
whatsapps$acconttypedescription= as.factor(whatsapps$acconttypedescription) 
whatsapps$communicationtype = as.factor(whatsapps$communicationtype)
whatsapps$ind_status = as.factor(whatsapps$ind_status)
whatsapps$messagetype = as.factor(whatsapps$messagetype) 
whatsapps$senddate = as.Date(whatsapps$senddate)  
whatsapps$campaignid = as.factor(whatsapps$campaignid)
whatsapps$debtorid = as.factor(whatsapps$debtorid)  
 
whatsapps$fullname = trimws(whatsapps$fullname)
whatsapps$fullname = toupper(stringi::stri_trans_general(whatsapps$fullname,"Latin-ASCII")) 
whatsapps$company = trimws(whatsapps$company)
whatsapps$company = toupper(stringi::stri_trans_general(whatsapps$company,"Latin-ASCII")) 
whatsapps$username = trimws(whatsapps$username)
whatsapps$username = toupper(stringi::stri_trans_general(whatsapps$username,"Latin-ASCII")) 
whatsapps$campaignname = trimws(whatsapps$campaignname)
whatsapps$campaignname = toupper(stringi::stri_trans_general(whatsapps$campaignname,"Latin-ASCII")) 

# Para calcular la edad a partir de la fecha de nacimiento
whatsapps$birth_year = as.numeric(substr(whatsapps$birthdate, 1,4))

today = Sys.Date() %>%
  format("%Y") %>%
  as.numeric()
whatsapps = whatsapps %>%
  mutate(age = today-birth_year)
table(whatsapps$age,useNA = "always")

whatsapps$birth_year = as.factor(whatsapps$birth_year)

skimr::skim(whatsapps)

# Se guarda el archivo
write.csv(whatsapps, "whatsapps.csv")
