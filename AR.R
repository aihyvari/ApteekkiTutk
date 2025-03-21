library(tidyverse)
library(readxl)

aptRek<-"https://koodistopalvelu.kanta.fi/codeserver/pages/download?name=Apteekkirekisteri.xls&pKey=pubfiles0"
p1f <- tempfile()
download.file(aptRek, p1f, mode="wb")
aptRek<-read_excel(path = p1f, sheet = 1)

#otetaan pää- ja sivuapteekit vain
aptRek<-aptRek[aptRek$ApteekkiTyyppi=="Apteekki"|aptRek$ApteekkiTyyppi=="Sivuapteekki",]

Vuosi=2024
####################################
aptRek$`Voimassaolo päättyy`<-as.Date(aptRek$`Voimassaolo päättyy`, tryformats=c("%Y%m%d", "%Y%M%D"))
aptRek$`Voimassaolo alkaa`<-as.Date(aptRek$`Voimassaolo alkaa`, tryformats=c("%Y%m%d", "%Y%M%D"))

aptRek<-aptRek[aptRek$`Voimassaolo päättyy`>paste(Vuosi-2, "-12-31", sep=""),]
aptRek<-aptRek[aptRek$`Voimassaolo alkaa`< paste(Vuosi+2, "-01-01", sep=""),]

#ja yksinkertaistetaan poimimalla joitain muuttujia
aptRek<- aptRek %>%
  select(Tunniste, # tää on oikeasti OID
         "Ylempi yksikkö", 
         #Lyhenne, 
         "Pitkä nimi" 
         #OID, 
         #"Postinumero", "ApteekkariYtunnus", "ApteekkiTyyppi", #"ApteekkariOID" ,
         #"Katuosoite" , "Postiosoite"
  )