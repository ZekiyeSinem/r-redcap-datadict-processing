pacman::p_load(tidyverse, lubridate, tableone, PerformanceAnalytics,
               survminer, RTCGA.clinical, survival,
               naniar, cutpointr, Hmisc, janitor,
               magrittr, broom, plyr, car, rcompanion, lmtest,
               ggdag, dagitty, mvtnorm, simstudy,
               mosaic, PerformanceAnalytics, FSA,
               psych, olsrr, blorr, dplyr, ggpubr, pROC,
               foreign , nnet, ggplot2,reshape2)
data <- read.csv("C:/Users/User/Documents/NCRCDienstleistungenKopie_DataDictionary_2021-11-25_datum.csv", header=TRUE, sep=",", encoding = "UTF-8")
new_option <-  '1, Beratung'
data_fl<- data %>% filter(data[6] != "1,Beratung",data[4]=="checkbox", data[3] != "") #Filtern 
d <- data.frame(as.integer(rownames(data_fl))) # #Anzahl der Zeilen 
for(s in ((d=1):(d=26))){ 
  first_option2 <- str_sub(data_fl[s,6], start = 0, end = 11) #Erstellen eines Substrings, welcher die Länge von new_option hat (zum Vergleichen)
  if(new_option != first_option2){ #Wenn 1,Beratung nicht die 1. Dienstleistung ist:
    singled_options1 <- strsplit(data_fl[s, 6] ,split=' | ', fixed=TRUE) #Splitten der einzelnen Dienstleistungen in jeder Zeile
    for(h in (1: length(singled_options1 [[1]]))){ #für jede Zeile wird folgendes durchgeführt:
      if(!is.na(singled_options1[[1]] [h])){#Nas werden ignoriert
        char_n <- substring(singled_options1 [[1]] [h], 1, 2) #in char_n wird "1," etc gepeichert für die spätere Inkrementierung
        rest <- substring(singled_options1 [[1]] [h], 3, 123) #Speichern der einzelnen Dienstleistungen
        if(char_n == 99){#somit wird die Sonstige Dienstleistung übersprungen
          break
        }
        char_nkomma <- str_replace_all(char_n, "[^[:alnum:]]", " ")#entfernen von non-numeric characters (bspl:,;.%! etc)
        char_neu <- as.integer(char_nkomma) #Umwandlung in einen Integer
        num_nkomma <- char_neu +1 #Inkrementierung
        #for(p in (1:length(h))) {
        neu <- paste(num_nkomma, rest,  sep = ' | ') #Zusammenführen von der Zahl, die inkrementiert wurde und dem "Rest" 
        index2 <- substring(neu[1], 1,1)#"Filtern" der 1. Zahl, um diese später mit der entsprechenden Dienstleistung zusammen, mit new_option zusammenzufügen
        changed <- paste(num_nkomma, rest, sep = ',', collapse = " ")#Inkrementierte Zahl und entsprechende Dienstleistung werden verkettet
        if(index2 == 2){ #Idee: Überprüfen ob die Zahl der Zahl zwei entspricht, sodass anschließend new_option und changed nacheinander eingefügt werden können
          s_1 <-paste(new_option, changed, sep = " | ", collapse = " ") #verknüpfen von new_option (1,Beratung) und changed (inkrementierteZahl, "Dienstleistung")
        }
        s_2 <- paste(s_1, changed, sep = " | ", collapse = " ")# aneinanderhängen von s_1 und changed (vorgesehen war es, eine weitere Dienstleitung einzufügen)
        print(s_2)
        #}
      }
    }
  }
}
