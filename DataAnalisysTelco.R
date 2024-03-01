## Importing packages
library(tseries)
library(tree)
library(dplyr)
library(tree)
library(randomForest)
library(boot)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
library(readr)

library(xlsx)
# installare e caricare il pacchetto

library("readxl")

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

telco <- read_csv("/path/Telco_Customer_Churn.csv")
str(telco)
telco1 <- read_excel("path/Telco.xlsx")

myvars<-names(telco1) %in% c("customerID","CLTV","ChurnScore","")
telco1<-telco1[myvars]
str(telco1)
Telco <- merge(telco,telco1,by=c("customerID"))
str(telco)



Telco$SeniorCitizen <- as.factor(Telco$SeniorCitizen)

P=nrow(Telco)
for (i in 1:P){
  if(Telco$MultipleLines[i] =="No phone service"){
    Telco$MultipleLines[i] <- "No"
  }
  if(Telco$OnlineBackup[i] =="No internet service"){
    Telco$OnlineBackup[i] <- "No"
  }
  if(Telco$OnlineSecurity[i] =="No internet service"){
    Telco$OnlineSecurity[i] <- "No"
  }
  if(Telco$DeviceProtection[i] =="No internet service"){
    Telco$DeviceProtection[i] <- "No"
  }
  if(Telco$TechSupport[i] =="No internet service"){
    Telco$TechSupport[i] <- "No"
  }
  if(Telco$StreamingTV[i] =="No internet service"){
    Telco$StreamingTV[i] <- "No"
  }
  if(Telco$StreamingMovies[i] =="No internet service"){
    Telco$StreamingMovies[i] <- "No"
  }
  if(Telco$PaymentMethod[i]=="Bank transfer (automatic)"){
    Telco$PaymentMethod[i] <- "AutoBankTransfer"
  }
  if(Telco$PaymentMethod[i]=="Credit card (automatic)"){
    Telco$PaymentMethod[i] <- "AutoCreditCard"
  }
}
glimpse(Telco)
# Rimuovo i Nan

Telco<-na.omit(Telco)
# Ci sono solamente 11 missing value nel campo TotalCharges,quindi sbarazzarsi di quelle righe dal set di dati.
# Ci sono tre variabili continue e sono Tenure, MonthlyCharges e TotalCharges.
# SeniorCitizen è in formato 'int', che può essere cambiato in categorico.
Telco <- Telco[complete.cases(Telco),]
#Un vettore logico che specifica quali osservazioni/righe non presentano valori 
#mancanti nell'intera sequenza.
# Trasformo SeniorCitizen in variabile categorica
Telco$SeniorCitizen <- as.factor(ifelse(Telco$SeniorCitizen==1, 'YES', 'NO'))
glimpse(Telco)
#VISUALIZZANDO PRIMA I DATI CATEGORICI RELATIVI AL CHURN:
#Le colonne CHURN ci indicano il numero di clienti che sono usciti nell'ultimo mese.
# Circa il 26% dei clienti ha lasciato la piattaforma nell'ultimo mese.
options(repr.plot.width = 6, repr.plot.height = 4)
Telco %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent,fill = Churn), fill = Churn)+coord_flip()+
  geom_col(color="black",fill = c("chocolate1", "steelblue2"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)),fontface="bold",nudge_y= -3,
            color="black",
            size = 3,)+ 
  theme_bw()+ 
  xlab("Churn") + 
  ylab("Percent")

# (Gender)Sesso - La percentuale di abbandono è quasi uguale nel caso di maschi e femmine
# La percentuale di abbandono è più alta nel caso degli anziani
# I clienti con partner e persone a carico hanno un tasso di abbandono inferiore rispetto
# a quelli che non hanno partner e persone a carico.
#1 Istogrammi delle caratterische demografiche
options(repr.plot.width = 8, repr.plot.height = 24)
plot_grid(ggplot(Telco, aes(x=gender,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+ 
            ylab("Frequency")+xlab("Gender")+coord_flip()+ geom_bar(color="black")+theme_minimal(), 
          ggplot(Telco, aes(x=SeniorCitizen,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+coord_flip()+ylab("Frequency")+ geom_bar(color="black")+theme_minimal(),
          ggplot(Telco, aes(x=Partner,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+coord_flip()+ylab("Frequency")+ geom_bar(color="black")+theme_minimal(),
          ggplot(Telco, aes(x=Dependents,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+ylab("Frequency")+coord_flip()+ geom_bar(color="black")+theme_minimal()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 5)),
          align = "h")

#2 Istogrammi dei servizi1
options(repr.plot.width = 8, repr.plot.height = 24)
plot_grid(ggplot(Telco, aes(x=PhoneService,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+ 
            ylab("Frequency")+coord_flip()+ geom_bar(color="black",show.legend = F)+theme_minimal(), 
          ggplot(Telco, aes(x=MultipleLines,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+coord_flip()+ylab("Frequency")+ geom_bar(color="black",show.legend = F)+theme_minimal(),
          ggplot(Telco, aes(x=InternetService,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+coord_flip()+ylab("Frequency")+ geom_bar(color="black",show.legend = F)+theme_minimal(),
          ggplot(Telco, aes(x=OnlineBackup,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+ylab("Frequency")+coord_flip()+ geom_bar(color="black",show.legend = F)+theme_minimal()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 5)),
          align = "h")


#3 Istogrammi dei servizi2
options(repr.plot.width = 8, repr.plot.height = 24)
plot_grid(ggplot(Telco, aes(x=OnlineSecurity,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+ 
            ylab("Frequency")+coord_flip()+ geom_bar(color="black",show.legend = F)+theme_minimal(), 
          ggplot(Telco, aes(x=DeviceProtection,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+coord_flip()+ylab("Frequency")+ geom_bar(color="black",show.legend = F)+theme_minimal(),
          ggplot(Telco, aes(x=StreamingTV,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+coord_flip()+ylab("Frequency")+ geom_bar(color="black",show.legend = F)+theme_minimal(),
          ggplot(Telco, aes(x=TechSupport,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+ylab("Frequency")+coord_flip()+ geom_bar(color="black",show.legend = F)+theme_minimal()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 5)),
          align = "h")
#4 Istogramma dei servizi3
options(repr.plot.width = 8, repr.plot.height = 24)
plot_grid(ggplot(Telco, aes(x=StreamingMovies,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+ 
            ylab("Frequency")+coord_flip()+ geom_bar(color="black",show.legend = F)+theme_minimal(), 
          ggplot(Telco, aes(x=PaymentMethod,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+coord_flip()+ylab("Frequency")+ geom_bar(color="black",show.legend = F)+theme_minimal(),
          ggplot(Telco, aes(x=Contract,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+coord_flip()+ylab("Frequency")+ geom_bar(color="black",show.legend = F)+theme_minimal(),
          ggplot(Telco, aes(x=PaperlessBilling,fill=Churn))+scale_fill_manual("legend", values = c("No" = "chocolate1", "Yes" = "steelblue2"))+ylab("Frequency")+coord_flip()+ geom_bar(color="black",show.legend = F)+theme_minimal()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 5)),
          align = "h")







#Il tasso di abbandono è molto più elevato in caso di servizi Internet in fibra ottica.
#I clienti che non dispongono di servizi come No OnlineSecurity, OnlineBackup e TechSupport
#hanno lasciato la piattaforma nell'ultimo mese.

#Una percentuale maggiore di Clienti con abbonamento mensile è andata via rispetto ai Clienti con 
#contratto di uno o due anni.
#La percentuale di abbandono è più alta in caso di cutsomers che hanno l'opzione di fatturazione senza carta.
#I clienti che hanno ElectronicCheck PaymentMethod tendono a lasciare la piattaforma di più rispetto ad altre
# opzioni.


#Analizzando le tre variabili continue w.r.t CHURN:
  
#Tenure (Durata): la permanenza media per i clienti che se ne sono andati è di circa 10 mesi.
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(Telco, aes(y= tenure, x = "")) + geom_boxplot(color="black", fill="lightblue")+coord_flip()+
  theme_bw()

#Addebiti mensili: i clienti che hanno abbattuto hanno addebiti mensili elevati. La mediana è superiore a 75.

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(Telco, aes(y= MonthlyCharges, x = "")) + geom_boxplot(color="black", fill="lightblue")+coord_flip()+
  theme_bw()

#TotalCharges (Addebiti totali):* Gli addebiti totali mediani dei clienti che hanno abbandonato sono bassi.
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(Telco, aes(y= TotalCharges, x = "")) + geom_boxplot(color="black", fill="lightblue")+coord_flip()+
  theme_bw()
#CLTV
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(Telco, aes(y=CLTV, x = "")) + geom_boxplot(color="black", fill="lightblue")+coord_flip()+
  theme_bw()
#Churn Score
ggplot(Telco, aes(y=`ChurnScore`, x = "", fill = Churn)) + geom_boxplot(color="black", fill="lightblue")+coord_flip()+ 
  theme_bw()+
  xlab(" ")


#Verifica della correlazione tra variabili continue
#Le spese totali hanno una correlazione positiva con le spese mensili e il mandato(tenure)
options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(Telco[,c("tenure", "MonthlyCharges", "TotalCharges","CLTV","ChurnScore")]), 1)

ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

#Controllando i valori anomali nelle variabili continue, e sembra che nessuno dei valori sia oltre
#i baffi qui.
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(Telco$tenure,col = "red")$out+theme_bw()
boxplot(Telco$MonthlyCharge,col="red")$out
boxplot(Telco$TotalCharges,col="red")$out
boxplot(Telco$CLTV,col="red")$out
boxplot(Telco$ChurnScore,col="red")$out

#PREPARAZIONE DEI DATI:
  
#Pulizia delle caratteristiche categoriche
#Standardizzazione delle funzioni continue
#Creazione di feature derivate
#Creazione di variabili fittizie per variabili fattore
#Creazione del set di dati finale
#Suddivisione dei dati in treno e set di convalida.
#Pulizia delle caratteristiche categoriche

#Dall'EDA sopra, sappiamo che ci sono alcune caratteristiche categoriche che hanno 'No' e 
#'Nessun servizio Internet' o 'Nessun servizio telefonico' come categoria, possiamo renderle come 
#''No' e pulire queste caratteristiche.

Telco <- data.frame(lapply(Telco, function(x) {
  gsub("No internet service", "No", x)}))

Telco <- data.frame(lapply(Telco, function(x) {
  gsub("No phone service", "No", x)}))

# Standardizzazione delle funzioni continue
#CS<-telco$ChurnScore
#CS<-as.numeric(CS)
#telco$ChurnScore<-CS
#CLTV<-telco$CLTV
#CLTV<-as.numeric(CLTV)
#telco$CLTV<-CLTV

num_columns <- c("tenure", "MonthlyCharges", "TotalCharges","CLTV","ChurnScore")
Telco[num_columns] <- sapply(Telco[num_columns], as.numeric)

telco_int <- Telco[,c("tenure", "MonthlyCharges", "TotalCharges","CLTV","ChurnScore")]
telco_int <- data.frame(scale(telco_int))


#Creazione di feature derivate

#Sto cercando di creare una funzione derivata dal mandato, in cui ho creato diversi 
#contenitori di mandato (che è in mesi) come "0-1 anno", "2-3 anni", "3-4 anni" ecc.

#max(telco$tenure)
#min(telco$tenure)
#telco <- mutate(telco, tenure_bin = tenure)

#telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
#telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
#telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
#telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
#telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
#telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'

#telco$tenure_bin <- as.factor(telco$tenure_bin)

#Dopo aver verificato la distribuzione dei dati in ogni intervallo di permanenza,
#abbiamo scoperto che il numero massimo di clienti ha una durata di 0-1 anni e seguita da 5-6 anni.
#options(repr.plot.width =6, repr.plot.height = 3)
#ggplot(telco, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+ theme_bw()

#Creo una variabile derivata di CLVT:
#telco <- mutate(telco, CLTV_bin = CLTV)

#telco$CLTV_bin[telco$CLTV_bin >=2000 & telco$CLTV_bin <= 3000] <- '1'
#telco$CLTV_bin[telco$CLTV_bin > 3000 & telco$CLTV_bin <= 4000] <- '2'
#telco$CLTV_bin[telco$CLTV_bin > 4000 & telco$CLTV_bin <= 5000] <- '3'
#telco$CLTV_bin[telco$CLTV_bin > 5000 & telco$CLTV_bin <= 6000] <- '4'
#telco$CLTV_bin[telco$CLTV_bin > 6000 & telco$CLTV_bin <= 7000] <- '5'


#telco$CLTV_bin <- as.factor(telco$CLTV_bin)

#options(repr.plot.width =6, repr.plot.height = 3)
#ggplot(telco, aes(CLTV_bin, fill = CLTV_bin)) + geom_bar()+ theme_bw()


#Creo una variabile derivata di Churn Score:


#telco <- mutate(telco, Churn_Score_bin=telco$ChurnScore)

#telco$Churn_Score_bin[telco$Churn_Score_bin >=0 & telco$Churn_Score_bin <= 20]<-"1"
#telco$Churn_Score_bin[telco$Churn_Score_bin > 20 & telco$Churn_Score_bin <= 40]<-"2"
#telco$Churn_Score_bin[telco$Churn_Score_bin > 40 & telco$Churn_Score_bin <= 60]<-"3"
#telco$Churn_Score_bin[telco$Churn_Score_bin > 60 & telco$Churn_Score_bin <= 80]<-"4"
#telco$Churn_Score_bin[telco$Churn_Score_bin > 80 ]<-"5"
#P=nrow(telco)
#for (i in 1:P){
#  if(telco$Churn_Score_bin[i] =="100"){
#    telco$Churn_Score_bin[i] <- 5
#  }
#}

#telco$Churn_Score_bin <- as.factor(telco$Churn_Score_bin)

#options(repr.plot.width =6, repr.plot.height = 3)
#ggplot(telco, aes(Churn_Score_bin, fill = Churn_Score_bin)) + geom_bar()+ theme_bw()

telco_cat <- Telco[,-c(1,6,19,20,22,23)]

#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))

head(dummy)

#Creating the final dataset by combining the numeric and dummy data frames.

#Combining the data
telco_final <- cbind(telco_int,dummy)
head(telco_final)


#MODEL BUILDING 1

#Starting with Logistic Regression

## LOGISTIC REGRESSION
# Creation of a balanced training subset
set.seed(123)
N <- nrow(telco_final)
counters<-table(telco_final$Churn)
counters <- table(telco_final$Churn)
index_yes <- array(dim = counters[2])
index_no <- array(dim = counters[1])
idx_yes <- 1
idx_no <- 1
for (i in 1:N){
  if(telco_final[i,26] ==0){
    index_no[idx_no] <- i
    idx_no <- idx_no+1
  }else{
    index_yes[idx_yes] <- i
    idx_yes <- idx_yes+1
  }
}
train1 <- sample(index_yes, floor(length(index_yes)*0.7))
train2 <- sample(index_no, floor(length(index_no)*0.7))
train <- sort(append(train1, train2))

logistic_model <- glm(Churn ~., 
                      data = telco_final, family = binomial, 
                      subset = train)
summary(logistic_model)
test1 <- setdiff(index_yes, train1)
test2 <- setdiff(index_no, train2)
test <- sort(append(test1, test2))

yhat <- predict(logistic_model, newdata = telco_final[test,], type = "response")
pred_churn <- factor(ifelse(yhat >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(telco_final[test,]$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)

step_model <- stepAIC(logistic_model, direction ="both")
summary(step_model)
yhat <- predict(logistic_model, newdata = telco_final[test,], type = "response")
pred_churn <- factor(ifelse(yhat >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(telco_final[test,]$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)
err_rate<-(table(actual_churn,pred_churn)[1,2]+table(actual_churn,pred_churn)[2,1])/sum(table(actual_churn,pred_churn))
err_rate
vif(step_model)

# Tolgo StreamingMovies che non sono significativi

model_3 <-glm(formula = Churn ~ tenure+MonthlyCharges +ChurnScore + SeniorCitizen+ 
                PhoneService+ +TechSupport + OnlineSecurity + OnlineBackup+ Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                PaymentMethod.xElectronic.check, family = binomial, data = telco_final, 
              subset = train)
summary(model_3)
vif(model_3)
yhat <- predict(model_3, newdata = telco_final[test,], type = "response")
pred_churn <- factor(ifelse(yhat >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(telco_final[test,]$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)
err_rate<-(table(actual_churn,pred_churn)[1,2]+table(actual_churn,pred_churn)[2,1])/sum(table(actual_churn,pred_churn))
err_rate

final_model<-model_3
yhat_LR<-yhat
cutoff_churn <- factor(ifelse(yhat_LR >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn)
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity


# Function

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(yhat_LR >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, actual_churn, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



options(repr.plot.width =8, repr.plot.height =6)
summary(yhat)
s = seq(0.01,0.90,length=1000)
OUT = matrix(0,1000,3)

for(i in 1:1000)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.33873874, col="red", lwd=1, lty=2)
abline(v = 0.33873874, col="black", lwd=1, lty=2)

#cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


pred_churn <- factor(ifelse(yhat_LR >= 0.34141141, "Yes", "No"))
actual_churn <- factor(ifelse(telco_final[test,]$Churn==1,"Yes","No"))
cutoff_churn <- factor(ifelse(yhat_LR >=0.34141141 , "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
table(pred_churn,actual_churn)
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity
err_rate<-(table(pred_churn,actual_churn)[1,2]+table(pred_churn,actual_churn)[2,1])/sum(table(pred_churn,actual_churn))
err_rate
varImp(final_model)



#ALBERO Decisionale:
library(tree)
set.seed(123)
# Valuate performance con subset di train e test
tree_model <- tree(as.factor(Churn) ~.,data = telco_final,subset = train)
summary(tree_model)
plot(tree_model)
text(tree_model)
pred_value <- predict(tree_model, newdata = telco_final[test,],type = "class")
table(pred_value,telco_final$Churn[-train])
test.err.rate=(table(pred_value,telco_final$Churn[-train])[1,2]+
                 table(pred_value,telco_final$Churn[-train])[2,1])/
  sum(table(pred_value,telco_final$Churn[-train]))
test.err.rate
accuracy<-(table(pred_value,telco_final$Churn[-train])[1,1]+
             table(pred_value,telco_final$Churn[-train])[2,2])/
  sum(table(pred_value,telco_final$Churn[-train]))
accuracy
sensibility<-(table(pred_value,telco_final$Churn[-train])[2,2])/
  sum(table(pred_value,telco_final$Churn[-train])[,2])
sensibility
specificity<-(table(pred_value,telco_final$Churn[-train])[1,1])/
  sum(table(pred_value,telco_final$Churn[-train])[,1])
specificity
# Cross validation per potare l'albero
tree_cv <- cv.tree(tree_model, FUN = prune.misclass)
plot(tree_cv)
plot(tree_cv$size, tree_cv$dev)
best = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
k = min(tree_cv$k[tree_cv$dev == min(tree_cv$dev)])

prune <- prune.misclass(tree_model, best = best)
summary(prune)

plot(prune, type = c("uniform"))
text(prune, pretty = 0)
#simile al precedente
#proviamo con size =4 
#plot(prune, type = "uniform")
best2 = tree_cv$size[tree_cv$size==4]
prune2 <- prune.misclass(tree_model, best = best2)
plot(prune2, type = c("uniform"))
text(prune2, pretty = 0)
pred_value <- predict(prune2, newdata = telco_final[-train,],type = "class")
table(pred_value,telco_final$Churn[-train])
test.err.rate=(table(pred_value,telco_final$Churn[-train])[1,2]+
                 table(pred_value,telco_final$Churn[-train])[2,1])/
  sum(table(pred_value,telco_final$Churn[-train]))
test.err.rate
accuracy<-(table(pred_value,telco_final$Churn[-train])[1,1]+
             table(pred_value,telco_final$Churn[-train])[2,2])/
  sum(table(pred_value,telco_final$Churn[-train]))
accuracy
sensibility<-(table(pred_value,telco_final$Churn[-train])[2,2])/
  sum(table(pred_value,telco_final$Churn[-train])[,2])
sensibility
specificity<-(table(pred_value,telco_final$Churn[-train])[1,1])/
  sum(table(pred_value,telco_final$Churn[-train])[,1])
specificity
#BAGGING E RANDOM FOREST
bagging <- randomForest(as.factor(Churn) ~.,data = telco_final, 
                        subset = train,
                        mtry = 14, importance = TRUE,replace = TRUE)
#decrease tree
#mtry=Numero di variabili campionate casualmente come candidate ad ogni suddivisione.
bagging <- randomForest(as.factor(Churn) ~.,data = telco_final ,
                        subset = train,
                        mtry = ncol(telco_final)-1, importance = TRUE,replace = TRUE,ntree= 500)
bagging
plot(bagging)
yhat_bag <- predict(bagging, newdata = telco_final[-train,])
table(yhat_bag,telco_final$Churn[-train])
tset.err.rate=(table(yhat_bag,telco_final$Churn[-train])[1,2]+
                 table(yhat_bag,telco_final$Churn[-train])[2,1])/
  sum(table(yhat_bag,telco_final$Churn[-train]))
tset.err.rate
importance(bagging)
bagging$err.rate[500]
#Checking the variable Importance Plot
varImpPlot(bagging)


rf <- randomForest(as.factor(Churn) ~.,data = telco_final,
                   subset = train,
                   mtry = floor(sqrt(14)), importance = TRUE,replace = TRUE,ntree= 500)
rf$err.rate[500]
#choose best m
oob.err.rate = double(14)
test.err.rate = double(14)
for(mtry in 1:14){
  fit = randomForest(as.factor(Churn) ~.,data = telco_final , subset=train, mtry=mtry, ntree = 500)
  oob.err.rate[mtry] = fit$err.rate[500]
  yhat_RF <- predict(fit, newdata = telco_final[-train,])
  test.err.rate[mtry] = (table(yhat_RF,telco_final$Churn[-train])[1,2]+
                           table(yhat_RF,telco_final$Churn[-train])[2,1])/
    sum(table(yhat_RF,telco_final$Churn[-train]))
}
plot(test.err.rate,type='b',main="Random Forest(m)",
     ylab="train vs test err rate", xlab="m",col='orange', pch=18,
     lwd=3,col.axis='white')
par(new=TRUE)
plot(oob.err.rate,type='b',main="Random Forest(m)",
     ylab="train vs test err rate", xlab="m",col='lightblue', 
     pch=18,lwd=3)
legend(x = "topright",          # Position
       legend = c("train error rate", "test error rate"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c("lightblue", "orange"),           # Line colors
       lwd = 2)    
min(test.err.rate)
#con m = 4 risulta migliore
rf <- randomForest(as.factor(Churn) ~.,data = telco_final ,
                   subset = train,
                   mtry = 4, importance = TRUE,replace = TRUE,ntree= 500)
yhat_RF <- predict(rf, newdata = telco_final[-train,])
table(yhat_RF,telco_final$Churn[-train])
tset.err.rate=(table(yhat_RF,telco_final$Churn[-train])[1,2]+
                 table(yhat_RF,telco_final$Churn[-train])[2,1])/
  sum(table(yhat_RF,telco_final$Churn[-train]))
tset.err.rate
importance(rf)
#Compare
yhat_RF <- predict(rf, newdata = telco_final[-train,])
table(yhat,telco_final$Churn[-train])
plot(rf$err.rate[,1],type='l',pch = "+",col='orange',lwd=2,col.axis='white', ylab="Error",xlab="",main="Bagging Vs Random Forest(m=4)")
par(new=TRUE)
plot(bagging$err.rate[,1],type='l',pch='o',col='lightblue',lwd=2 ,ylab="",xlab="")
legend(x = "topright",          # Position
       legend = c("Bagging Error", "Random Forest (m=4)"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c("lightblue", "orange"),           # Line colors
       lwd = 2)                 # Line width
oob.err.rate[14]
test.err.rate[14]
oob.err.rate[3]
test.err.rate[3]
varImpPlot(rf)
accuracy<-(table(yhat_RF,telco_final$Churn[-train])[1,1]+
             table(yhat_RF,telco_final$Churn[-train])[2,2])/
  sum(table(yhat_RF,telco_final$Churn[-train]))
accuracy
sensibility<-(table(yhat_RF,telco_final$Churn[-train])[2,2])/
  sum(table(yhat_RF,telco_final$Churn[-train])[,2])
sensibility
specificity<-(table(yhat_RF,telco_final$Churn[-train])[1,1])/
  sum(table(yhat_RF,telco_final$Churn[-train])[,1])
specificity
#boosting
library ( gbm )
set.seed(1)

ntree = 500; 
boost_model <- gbm(telco_final[train,]$Churn ~ .,
                   data = telco_final[train,], 
                   distribution = "gaussian" , n.trees = ntree,
                   interaction.depth = 4, shrinkage = 0.01 , verbose = F)
summary(boost_model)

pred_gbm_test <- predict.gbm(object=boost_model, newdata = telco_final[-train,],n.trees=500, type="response")





#SVM
set.seed(1)
dataframe_train<-data.frame(x=telco_final[train,-26],y=as.factor(telco_final$Churn[train]))
# Fit the SVM model 
# cost = cost of a margin violation 
# kernel = linear for SVM
# scale = no trasform of input space 
# Cross validation for select best value of cost
# Cross validation for select best value of cost
svm_cross1 <-  tune ( svm , y ~ . , data = dataframe_train, kernel = "linear" ,
                     ranges = list(cost=c(0.001 , 0.01 , 0.1 , 1 , 5 , 10 , 100)))
summary(svm_cross1)
svmfit_linear <- svm_cross1$best.model
print(svmfit_linear)
dataframe_test<-data.frame(x=telco_final[test,-26],y=as.factor(telco_final$Churn[test]))
yhat_svmfit_linear<-predict(svmfit_linear,dataframe_test)
table(predic = yhat_svmfit_linear, truth = dataframe_test$y)
tset.err.rate=(table(yhat_svmfit_linear,dataframe_test$y)[1,2]+
                 table(yhat_svmfit_linear,dataframe_test$y)[2,1])/
  sum(table(yhat_svmfit_linear,dataframe_test$y))
tset.err.rate
# Linear Model with not all regressor
svm_cross2 <-  tune ( svm , y ~ . , data = dataframe_train[,c(1,2,5,21,24,12,26)], kernel = "linear" ,
                      ranges = list(cost=c(0.001 , 0.01 , 0.1 , 1 , 5 , 10 , 100)))
summary(svm_cross2)
svmfit_linear2 <- svm_cross2$best.model
print(svmfit_linear2)
dataframe_test2<-data.frame(x=telco_final[test,c(1,2,5,21,24,12)],y=as.factor(telco_final$Churn[test]))
yhat_svmfit_linear2<-predict(svmfit_linear2,dataframe_test2)
table(predic = yhat_svmfit_linear2, truth = dataframe_test2$y)
tset.err.rate=(table(yhat_svmfit_linear2,dataframe_test2$y)[1,2]+
                 table(yhat_svmfit_linear2,dataframe_test2$y)[2,1])/
  sum(table(yhat_svmfit_linear2,dataframe_test2$y))
tset.err.rate
accuracy<-(table(yhat_svmfit_linear2,dataframe_test2$y)[1,1]+
             table(yhat_svmfit_linear2,dataframe_test2$y)[2,2])/
  sum(table(yhat_svmfit_linear2,dataframe_test2$y))
accuracy
sensibility<-(table(yhat_svmfit_linear2,dataframe_test2$y)[2,2])/
  sum(table(yhat_RF,dataframe_test2$y)[,2])
sensibility
specificity<-(table(yhat_svmfit_linear2,dataframe_test2$y)[1,1])/
  sum(table(yhat_svmfit_linear2,dataframe_test2$y)[,1])
specificity


#radial Kernel
svm_cross3 <-  tune ( svm , y ~ . , data = dataframe_train, kernel = "radial" ,
                     ranges = list (
                       cost = c (0.1 , 1 , 10 , 100 , 1000) ,
                       gamma = c (0.5 , 1 , 2 , 3 , 4)))
summary(svm_cross3)
svmfit_radial1 <- svm_cross3$best.model
print(svmfit_radial1)
yhat_svmfit_radial1<-predict(svmfit_radial1,dataframe_test)
table(predic = yhat_svmfit_radial1, truth = dataframe_test$y)
tset.err.rate=(table(yhat_svmfit_radial1,dataframe_test$y)[1,2]+
                 table(yhat_svmfit_radial1,dataframe_test$y)[2,1])/
  sum(table(yhat_svmfit_radial1,dataframe_test$y))
tset.err.rate
plot(svmfit_radial1,dataframe_train)
#radial Kernel reduced
svm_cross4 <-  tune ( svm , y ~ . , data = dataframe_train[,c(1,2,5,21,24,12,26)], kernel = "radial" ,
                     ranges = list (
                       cost = c (0.1 , 1 , 10 , 100 , 1000) ,
                       gamma = c (0.5 , 1 , 2 , 3 , 4)))
summary(svm_cross4)
svmfit_radial2 <- svm_cross4$best.model
print(svmfit_radial2)
yhat_svmfit_radial2<-predict(svmfit_radial2,dataframe_test2)
table(predic = yhat_svmfit_radial2, truth = dataframe_test2$y)
tset.err.rate=(table(yhat_svmfit_radial2,dataframe_test2$y)[1,2]+
                 table(yhat_svmfit_radial2,dataframe_test2$y)[2,1])/
  sum(table(yhat_svmfit_radial2,dataframe_test2$y))
tset.err.rate
plot(svmfit_radial2,dataframe_train)
accuracy<-(table(yhat_svmfit_radial2,dataframe_test2$y)[1,1]+
             table(yhat_svmfit_radial2,dataframe_test2$y)[2,2])/
  sum(table(yhat_svmfit_radial2,dataframe_test2$y))
accuracy
sensibility<-(table(yhat_svmfit_radial2,dataframe_test2$y)[2,2])/
  sum(table(yhat_svmfit_radial2,dataframe_test2$y)[,2])
sensibility
specificity<-(table(yhat_svmfit_radial2,dataframe_test2$y)[1,1])/
  sum(table(yhat_svmfit_radial2,dataframe_test2$y)[,1])
specificity
#Last Kernel
svm_cross5 <-  tune ( svm , y ~ . , data = dataframe_train[,c(1,5,26)], kernel = "polynomial" ,
                      ranges = list (
                        cost = c (0.1 , 1 , 10 , 100 , 1000) ,
                        gamma = c (0.5 , 1 , 2 , 3 , 4)))
summary(svm_cross5)
svmfit_radial3 <- svm_cross5$best.model
print(svmfit_radial5)
dataframe_test3<-data.frame(x=telco_final[test,c(1,5)],y=as.factor(telco_final$Churn[test]))
yhat_svmfit_radial2<-predict(svmfit_radial3,dataframe_test3)
table(predic = yhat_svmfit_radial2, truth = dataframe_test3$y)
tset.err.rate=(table(yhat_svmfit_radial2,dataframe_test2$y)[1,2]+
                 table(yhat_svmfit_radial2,dataframe_test2$y)[2,1])/
  sum(table(yhat_svmfit_radial2,dataframe_test2$y))
tset.err.rate

# Checking the AUC for all three models:

lr <- roc(response = telco_final$Churn[test], predictor = as.numeric(yhat_RF))
bag <- roc(response = telco_final$Churn[test], predictor = as.numeric(yhat_bag))
rf <- roc(response = telco_final$Churn[test], predictor = as.numeric(yhat_LR))
svm1<-roc(telco_final$Churn[test], predictor=as.numeric(yhat_svmfit_linear2))
svm2<-roc(telco_final$Churn[test], predictor=as.numeric(yhat_svmfit_radial2))
plot(lr,      legacy.axes = TRUE, print.auc.y = 0.4, print.auc = TRUE)
plot(gb, col = "blue", add = TRUE, print.auc.y = 0.5, print.auc = TRUE)
plot(bag, col = "red" , add = TRUE, print.auc.y = 0.6, print.auc = TRUE)
plot(svm1,col = "orange" , add = TRUE, print.auc.y = 0.7, print.auc = TRUE)
plot(svm2,col = "green" , add = TRUE, print.auc.y = 0.8, print.auc = TRUE)
legend(0.0,0.2, c("Random Forest", "Bagging", "Logistic","SVM (Linear)","SVM (Radial)"), lty = c(1,1), lwd = c(2, 2), col = c("black", "blue", "red", "orange","green"), cex = 0.50)

