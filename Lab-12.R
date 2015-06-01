################################################################################
#########################      LABORATORIA  12     #############################
################################################################################

#########################        ZESTAW 9       ################################
sciezkaDoPliku <- "H:/DM/UCIarff/segment.arff"
Dane <- read.table(sciezkaDoPliku, head=FALSE, skip=96, sep=",", dec=".")
library("rpart")

PodzielZbior <- function(dane, dlugosc_testowy){
  dlugosc_caly<-nrow(dane)
  wybrane<-sample(1:dlugosc_caly,dlugosc_testowy,replace=FALSE)
  l<-list(dane[wybrane,],dane[-wybrane,])
  names(l)<-c("testowy","treningowy")
  l
}

DanePodzielone <- PodzielZbior(Dane, nrow(Dane)/3)
# zbiorTreningowy <- DanePodzielone$treningowy
# zbiorTestowy <- DanePodzielone$testowy
# B <- 20

#Wolno dziaĹ‚a, ale dobrze
Bagging <- function(zbiorTreningowy, zbiorTestowy, B){
  
  #Losuje proby Bootstrap
  nTreningowy <- nrow(zbiorTreningowy)
  probyBootstrap <- vector("list", B)
  for(i in 1:B){
    ktore <- sample(1:nTreningowy, nTreningowy, replace=TRUE)
    probyBootstrap[[i]] <- zbiorTreningowy[ktore, ]    
  }
  
  #Buduje klasyfikatory Bagging
  drzewaKlasyfikujace <- vector("list", B)
  for(i in 1:B){
    drzewaKlasyfikujace[[i]] <- rpart(as.factor(V20)~., data=probyBootstrap[[i]], 
                                 cp=0.01, minsplit=5)
  }
  
  #Predykcja komitetu drzew
  
  for(i in 1:B){
    Przewiduj <- predict(drzewaKlasyfikujace[[i]], newdata=zbiorTestowy)
    PrzewidywaneKlasy <- character(nrow(zbiorTestowy))
    for(j in 1:nrow(zbiorTestowy)){
      PrzewidywaneKlasy[j] <- colnames(Przewiduj)[
        which(Przewiduj[j, ]==max(Przewiduj[j, ]))]
    }
    
    if(i==1) {
      predykcjaKomitetu <- matrix(PrzewidywaneKlasy, ncol=1)
    }else {
      predykcjaKomitetu <- cbind(predykcjaKomitetu, PrzewidywaneKlasy)
    }
    
  }
  
  ostatecznaDecyzjaKomitetu <- character(nrow(zbiorTestowy))
  for(i in 1:nrow(zbiorTestowy)){
    podsumowanie <- table(predykcjaKomitetu[i, ])
    ostatecznaDecyzjaKomitetu[i] <- names(podsumowanie)[
      which(podsumowanie==max(podsumowanie))]
  }
  
  bladKomitetuDrzew <- 1 - sum(ostatecznaDecyzjaKomitetu==
                             as.character(zbiorTestowy$V20))/nrow(zbiorTestowy)
  
  
  
  #Buduje pojedyncze drzewo
  drzewoPojedyncze <- rpart(as.factor(V20)~., data=zbiorTreningowy, 
                            cp=0.01, minsplit=5)
  
  #Predykcja pojedynczego drzewa
  Przewiduj <- predict(drzewoPojedyncze, newdata=zbiorTestowy)
  PrzewidywaneKlasy <- character(nrow(zbiorTestowy))
  for(i in 1:nrow(zbiorTestowy)){
    PrzewidywaneKlasy[i] <- colnames(Przewiduj)[
                          which(Przewiduj[i, ]==max(Przewiduj[i, ]))]
  }
  
  #Ile obserwacji zle zaklasyfikowanych?
  bladPojedynczegoDrzewa <- 1 - sum(PrzewidywaneKlasy==
                                as.character(zbiorTestowy$V20))/nrow(zbiorTestowy)
  
  c(bladKomitetuDrzew, bladPojedynczegoDrzewa)
}


#Wykonuje funkcje - dziele 100 razy na zbior testowy i trenigowy
bledyPojedynczegoDrzewa <- numeric(100)
bledyKomitetu <- numeric(100)

for(i in 1:100){
  DanePodzielone <- PodzielZbior(Dane, nrow(Dane)/3)
  blad <- Bagging(DanePodzielone$treningowy, DanePodzielone$testowy, 100)
  bledyKomitetu[i] <- blad[1]
  bledyPojedynczegoDrzewa[i] <- blad[2]
  print(i)
}


boxplot(bledyKomitetu, bledyPojedynczegoDrzewa, names=c("Komitet", "Pojedyncze"))

#PrĂłbowaĹ‚am poprawiÄ‡ - nie testowana jeszcze
Bagging1 <- function(zbiorTreningowy, zbiorTestowy, B){
  
  #Losuje proby Bootstrap
  nTreningowy <- nrow(zbiorTreningowy)
  nTestowy <- nrow(zbiorTestowy)

  for(i in 1:B){
    #Losuje probe Bootstrap
    ktore <- sample(1:nTreningowy, nTreningowy, replace=TRUE)
    probyBootstrap <- zbiorTreningowy[ktore, ]   
    
    #Buduje klasyfikator Bagging
    drzewaKlasyfikujace <- rpart(as.factor(V20)~., data=probyBootstrap[[i]], 
                                      cp=0.01, minsplit=5)
    
    #Predykcja tego drzewa
    Przewiduj <- predict(drzewaKlasyfikujace, newdata=zbiorTestowy)
    PrzewidywaneKlasy <- character(nrow(zbiorTestowy))
    for(j in 1:nrow(zbiorTestowy)){
      PrzewidywaneKlasy[j] <- colnames(Przewiduj)[
        which(Przewiduj[j, ]==max(Przewiduj[j, ]))]
    }
    
    if(i==1) {
      predykcjaKomitetu <- matrix(PrzewidywaneKlasy, ncol=1)
    }else {
      predykcjaKomitetu <- cbind(predykcjaKomitetu, PrzewidywaneKlasy)
    }   
  }
  
  ostatecznaDecyzjaKomitetu <- character(nrow(zbiorTestowy))
  for(i in 1:nrow(zbiorTestowy)){
    podsumowanie <- table(predykcjaKomitetu[i, ])
    ostatecznaDecyzjaKomitetu[i] <- names(podsumowanie)[
      which(podsumowanie==max(podsumowanie))]
  }
  
  bladKomitetuDrzew <- 1 - sum(ostatecznaDecyzjaKomitetu==
                                 as.character(zbiorTestowy$V20))/nrow(zbiorTestowy)
  
  
  
  #Buduje pojedyncze drzewo
  drzewoPojedyncze <- rpart(as.factor(V20)~., data=zbiorTreningowy, 
                            cp=0.01, minsplit=5)
  
  #Predykcja pojedynczego drzewa
  Przewiduj <- predict(drzewoPojedyncze, newdata=zbiorTestowy)
  PrzewidywaneKlasy <- character(nrow(zbiorTestowy))
  for(i in 1:nrow(zbiorTestowy)){
    PrzewidywaneKlasy[i] <- colnames(Przewiduj)[
      which(Przewiduj[i, ]==max(Przewiduj[i, ]))]
  }
  
  #Ile obserwacji zle zaklasyfikowanych?
  bladPojedynczegoDrzewa <- 1 - sum(PrzewidywaneKlasy==
                                      as.character(zbiorTestowy$V20))/nrow(zbiorTestowy)
  
  c(bladKomitetuDrzew, bladPojedynczegoDrzewa)
}