# Organisation du travail
## Création du répertoire de travail
setwd("~/Desktop/analyse perfermance joueur ligue 1")
getwd()

## Liste des packages chargés
library(dplyr)
library(tidyr)
library(lubridate)
library(tsoutliers)
library(readxl)
library(tidyverse)
library(stringr)
library(patchwork)
library(lattice)
library(DataExplorer) 
library(VIM)
library(summarytools)
library("funModeling")
library(GGally)
library(ISLR)
library(leaps)
library(knitr)
library(kableExtra)


# Manipulation des bases de données
## Chargement des bases de données à fuisonner

tirs <- read_excel("perfomance_joueurs.xlsx", 
                   sheet = "tirs")

prepa_tirs_buts <- read_excel("perfomance_joueurs.xlsx", 
                              sheet = "preparation tirs et buts")

temps_jeu<- read_excel("perfomance_joueurs.xlsx", 
                       sheet = "temps de jeu")

head(tirs)
dim(tirs)

head(prepa_tirs_buts)
dim(prepa_tirs_buts)


head(temps_jeu)
dim(temps_jeu)

## Fusion des bases de données   
df= merge(temps_jeu,prepa_tirs_buts,by=c("Joueur","Équipe"),all.x = T)
df= merge(df,tirs,by=c("Joueur","Équipe"),all.x = T)
head(df)
dim(df)

## Renomination des variables

names(df)#Nom des variables : nous devons renommer la majorité des variables pour une meilleure lecture.

df= rename(df,age="Âge",
           equipe="Équipe",
           minutes_jouees_90="90",
           Mn_MJ="Mn/MJ",
           pourcentage_TC="TC%",
           Tir_90="Tir/90",
           TC_90="TC/90",
           B_Tir="B/Tir",
           Mn_Debute="Mn/Débuté",
           buts_marques_net_avec_joueur="+/-",
           buts_marques_net_avec_joueur_par_match="+/-90",
           Sur_En_dehors_du_terrain="Sur/En dehors du terrain",
           PassJeu_tirs="PassJeu...5",
           PassArr_tirs="PassArr...6",
           Drib_tirs="Drib...7",
           tirs_tirs="Tirs...8",
           Ftp_tirs="Ftp...9",
           Mn_Remp="Mn/Remp",
           pourcentage_min="Min%",
           def_tirs="Déf...10",
           passjeu_buts="PassJeu...13",
           passarr_buts="PassArr...14",
           Drib_buts="Drib...15",
           tirs_buts ="Tirs...16",
           Ftp_buts="Ftp...17",
           Def_buts="Déf...18",
           B_TC="B/TC",
           PenM="PénM",
           PenT="PénT")



colnames(df)= str_to_lower(colnames(df))#Pour obtenir le nom des variables en minuscule.
colnames(df)


##Classe des variables
str(df)#Variables dans un format inadéquat. En particulier la variable 'age'.


### Modification de la variable age dans le bon format
age=rename(data.frame(joueur = df$joueur
                      ,equipe=df$equipe,
                      str_split_fixed(df$age, "-", 2))
           ,age_annee=X1,age_jours=X2)
age = update_columns(age, c("age_annee","age_jours"),
                     as.numeric)
age = mutate(age,age=age_annee+(age_jours/365))
age=select(age,joueur,equipe,age)
head(age,3)


df=df[,-37] #'df' sans l'ancienne variable 'age' située à la place 37.

df=merge(df,age,by=c('joueur','equipe'),all.x = T) #'df' avec la nouvelle variable 'age' créée dans le bon format.
str(df$age)

### Modification de la classe des autres variables
df = update_columns(df, 
                    c("mj","min","mn_mj",
                      "pourcentage_min","titulaire",
                      "tirs_tirs", "mn_debute","compl",
                      "remp","mn_remp","rempne","ppm",
                      "bt","be","buts_marques_net_avec_joueur",
                      "buts_marques_net_avec_joueur_par_match",
                      "sur_en_dehors_du_terrain","amt90",
                      "passjeu_tirs","passarr_tirs",
                      "drib_tirs","tirs","ftp_tirs","def_tirs",
                      "amb","amb90","passjeu_buts","passarr_buts",
                      "drib_buts","tirs_buts","ftp_buts",
                      "def_buts","minutes_jouees_90",
                      "buts","tc","pourcentage_tc",
                      "tir_90","tc_90","b_tir","b_tc",
                      "dist","cf","penm","pent","buts")
                    , as.numeric)

df = update_columns(df, c("joueur","equipe","nation","pos"
                          ,"naissance")
                    , as.factor)
str(df)

## Relocalisation des variables

#- La variable 'joueur' est 1ère position pour identifier les joueurs.

#- les variables qualitatives signalitiques seront placées avant les variables continues.

#- La variable 'buts' sera en 2ème position car elle sera considérée plus tard comme variable endogène dans un modèle de maching learnig avec application économétrique.

df= relocate(df,c("nation","pos","naissance"),.before = mj)
df= relocate(df,buts,.before = equipe)
colnames(df[,1:8])

# Analyse exploratoire de la bdd
#- Pour rappel, l'exploration de données dans un projet de données précède les statistiques descriptives et/ou les inférences statistiques. Cette étape est essentielle à la compréhension de la base de données en nous fournissant un premier aperçu de celle-ci.
#- La variable ***'buts'*** ayant une grande importance dans l'analyse des performances offensives, nous ne retenons que les joueurs ayant marqué au moins 1 but dans la saison.

df=filter(df,buts>0)
nrow(df)

## Structure des données
plot_intro(df)

## Données manquantes
profile_missing(df)
df_NA <- aggr(df,
              col=c('navyblue','red'),
              numbers=TRUE,
              sortVars=TRUE,
              labels=names(data),
              cex.axis=.7, gap=3,
              ylab=c("Histogramme des valeurs manquantes","Pattern"))
df_NA


## distribution des variables
### Variables qualitatives


plot_bar(
  select(df,naissance,pos,equipe),
  title = "Répartition des joueurs par caractéristiques")


### Variables continues
#### Normalité

#Histogramme
plot_histogram(split_columns(df)$continuous)


#Densité
plot_density(split_columns(df)$continuous)


#qq-plot
plot_qq(split_columns(df)$continuous)


#### Evolution du nombre de buts en fonction des autres variables continues
plot_scatterplot(split_columns(df)$continuous, by = "buts") 


#Les variables 'def_tirs', 'ftp_tirs', 'passjeu_buts', 'tirs_tirs', 'def_buts,drib_buts', 'ftp_buts','passarr_buts', 'tirs_buts', 'penm' et 'pent' seront discrétisés.
df_a_discritise = select(df,
                         def_tirs,ftp_tirs,passjeu_buts,
                         tirs_tirs,def_buts,drib_buts,
                         ftp_buts,passarr_buts,tirs_buts,
                         penm,pent)

df_a_discritise = update_columns(
  df_a_discritise,
  c("def_tirs","ftp_tirs","passjeu_buts","tirs_tirs",
    "def_buts","drib_buts","ftp_buts","passarr_buts",
    "tirs_buts","penm","pent")
                    , as.factor)
colnames(df_a_discritise) <- paste("discr",
                                   colnames(df_a_discritise),
                                   sep="_")

summary(df_a_discritise)

df1=data.frame(df,df_a_discritise)
dim(df1)
dim(df)