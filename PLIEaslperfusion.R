library(ggplot2)
library(readr)
library(plyr)
library(dplyr)
library(reshape)
library(tidyr)
library(broom)
library(purrr)
library(corrplot)
library(psych)
library(ggpubr)
library(RColorBrewer)
library(ggubpr)
library(ggpmisc)
library(cowplot)
library(gridExtra)
library(ggrepel)

#To remove an environment: rm(list=ls())
#This resets the graphs environment: dev.off()
#To write csv: write.csv(mydata, "mydata.csv") 
#Change 0s to NA: PLIENormalized[PLIENormalized == 0] <- NA
#Change NAs to 0: PLIENormalized[is.na(PLIENormalized)] <- 0
#colnames(ADASCOG_t)[1:1] <- paste(colnames(ADASCOG_t)[1:1], "V1", sep = "_")

#setwd("B:/shared/Steven/PLIE")
dataDir <- "/Users/stevenmartinez/Desktop/SFVA/PLIE/"
outputDir <- "/Users/stevenmartinez/Desktop/SFVA/PLIE/output"

PLIENormalized<-read.csv(paste(dataDir,"PLIENormalized.csv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
PLIE_EyeTracking<-read.csv(paste(dataDir,"PLIE_EyeTracking.csv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
AccessData<-read.csv(paste(dataDir,"qrySteven_200316.csv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
Demographics<-read.csv(paste(dataDir,"qryPLIEDOD_Demo.csv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
pidn<-read.csv(paste(dataDir,"qryPLIEDOD_pidn.csv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

###Imaging Subsetting
Cerebellum <- subset(PLIENormalized, select = c(Region,PLIE034.1NC,PLIE034.2NC,PLIE041.1NC,PLIE041.2NC,PLIE045.1NC,PLIE045.2NC,PLIE065.1NC,PLIE065.2NC,PLIE082.1NC,PLIE082.2NC,PLIE135.1NC,PLIE135.2NC,PLIE136.1NC,PLIE136.2NC,PLIE154.1NC,PLIE154.2NC,PLIE166.1NC,PLIE166.2NC,PLIE169.1NC,PLIE169.2NC,PLIE185.1NC,PLIE185.2NC,PLIE235.1NC,PLIE235.2NC,PLIE247.1NC,PLIE247.2NC,PLIE251.1NC,PLIE251.2NC,PLIE253.1NC,PLIE253.2NC))
CerebellumV1 <- subset(Cerebellum, select = c(Region,PLIE034.1NC,PLIE041.1NC,PLIE045.1NC,PLIE065.1NC,PLIE082.1NC,PLIE135.1NC,PLIE136.1NC,PLIE154.1NC,PLIE166.1NC,PLIE169.1NC,PLIE185.1NC,PLIE235.1NC,PLIE247.1NC,PLIE251.1NC,PLIE253.1NC))
names(CerebellumV1) <- sub(".1NC", "", names(CerebellumV1))
CerebellumV2 <- subset(Cerebellum, select = c(Region,PLIE034.2NC,PLIE041.2NC,PLIE045.2NC,PLIE065.2NC,PLIE082.2NC,PLIE135.2NC,PLIE136.2NC,PLIE154.2NC,PLIE166.2NC,PLIE169.2NC,PLIE185.2NC,PLIE235.2NC,PLIE247.2NC,PLIE251.2NC,PLIE253.2NC))

Precentral <- subset(PLIENormalized, select = c(Region,PLIE034.1NP,PLIE034.2NP,PLIE041.1NP,PLIE041.2NP,PLIE045.1NP,PLIE045.2NP,PLIE065.1NP,PLIE065.2NP,PLIE082.1NP,PLIE082.2NP,PLIE135.1NP,PLIE135.2NP,PLIE136.1NP,PLIE136.2NP,PLIE154.1NP,PLIE154.2NP,PLIE166.1NP,PLIE166.2NP,PLIE169.1NP,PLIE169.2NP,PLIE185.1NP,PLIE185.2NP,PLIE235.1NP,PLIE235.2NP,PLIE247.1NP,PLIE247.2NP,PLIE251.1NP,PLIE251.2NP,PLIE253.1NP,PLIE253.2NP))
PrecentralV1 <- subset(Precentral, select = c(Region,PLIE034.1NP,PLIE041.1NP,PLIE045.1NP,PLIE065.1NP,PLIE082.1NP,PLIE135.1NP,PLIE136.1NP,PLIE154.1NP,PLIE166.1NP,PLIE169.1NP,PLIE185.1NP,PLIE235.1NP,PLIE247.1NP,PLIE251.1NP,PLIE253.1NP))
names(PrecentralV1) <- sub(".1NP", "", names(PrecentralV1))
PrecentralV2 <- subset(Precentral, select = c(Region,PLIE034.2NP,PLIE041.2NP,PLIE045.2NP,PLIE065.2NP,PLIE082.2NP,PLIE135.2NP,PLIE136.2NP,PLIE154.2NP,PLIE166.2NP,PLIE169.2NP,PLIE185.2NP,PLIE235.2NP,PLIE247.2NP,PLIE251.2NP,PLIE253.2NP))

####Transpose df
####AND remove PLIE012 because it does not have a pidn
CerebellumV1 <- as.data.frame(t(CerebellumV1))
colnames(CerebellumV1) <- as.character(unlist(CerebellumV1[1,]))
CerebellumV1 = CerebellumV1[-(1),]
colnames(CerebellumV1) <- paste(colnames(CerebellumV1), "V1", sep = "")

CerebellumV2 <- as.data.frame(t(CerebellumV2))
colnames(CerebellumV2) <- as.character(unlist(CerebellumV2[1,]))
CerebellumV2 = CerebellumV2[-(1),]
colnames(CerebellumV2) <- paste(colnames(CerebellumV2), "V2", sep = "")

PrecentralV1<- as.data.frame(t(PrecentralV1))
colnames(PrecentralV1) <- as.character(unlist(PrecentralV1[1,]))
PrecentralV1 = PrecentralV1[-(1),]
colnames(PrecentralV1) <- paste(colnames(PrecentralV1), "V1", sep = "")

PrecentralV2<- as.data.frame(t(PrecentralV2))
colnames(PrecentralV2) <- as.character(unlist(PrecentralV2[1,]))
PrecentralV2 = PrecentralV2[-(1),]
colnames(PrecentralV2) <- paste(colnames(PrecentralV2), "V2", sep = "")

###As.Numeric
PLIENormalized[, 2:ncol(PLIENormalized)]           <- sapply(PLIENormalized[, 2:ncol(PLIENormalized)], as.numeric)
PLIE_EyeTracking[, 5:ncol(PLIE_EyeTracking)]           <- sapply(PLIE_EyeTracking[, 5:ncol(PLIE_EyeTracking)], as.numeric)
Cerebellum[, 2:ncol(Cerebellum)]           <- sapply(Cerebellum[, 2:ncol(Cerebellum)], as.numeric)
Precentral[, 2:ncol(Precentral)]           <- sapply(Precentral[, 2:ncol(Precentral)], as.numeric)
AccessData[, 26:ncol(AccessData)]           <- sapply(AccessData[, 26:ncol(AccessData)], as.numeric)


CerebellumV1[] <- lapply(CerebellumV1, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
#sapply(CerebellumV1, class)

CerebellumV2[] <- lapply(CerebellumV2, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
#sapply(CerebellumV2, class)

PrecentralV1[] <- lapply(PrecentralV1, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
#sapply(PrecentralV1, class)

PrecentralV2[] <- lapply(PrecentralV2, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
#sapply(PrecentralV1, class)

####Imaging Clean-Up
CerebellumV1 <- subset(CerebellumV1, select = c (HPV1,InsulaV1,TemporalV1,ParietalV1,FrontalV1,OccipitalV1,SuperiorFrontalV1,CaudalACCV1,SuperiorParietalV1,InferiorParietalV1,PrecuneusV1,SupramarginalV1))
CerebellumV2 <- subset(CerebellumV2, select = c (HPV2,InsulaV2,TemporalV2,ParietalV2,FrontalV2,OccipitalV2,SuperiorFrontalV2,CaudalACCV2,SuperiorParietalV2,InferiorParietalV2,PrecuneusV2,SupramarginalV2))
PrecentralV1 <- subset(PrecentralV1, select = c (HPV1,InsulaV1,TemporalV1,ParietalV1,FrontalV1,OccipitalV1,SuperiorFrontalV1,CaudalACCV1,SuperiorParietalV1,InferiorParietalV1,PrecuneusV1,SupramarginalV1))
PrecentralV2 <- subset(PrecentralV2, select = c (HPV2,InsulaV2,TemporalV2,ParietalV2,FrontalV2,OccipitalV2,SuperiorFrontalV2,CaudalACCV2,SuperiorParietalV2,InferiorParietalV2,PrecuneusV2,SupramarginalV2))

#####Merge by cbind
CerebellumFinal <- cbind(CerebellumV1,CerebellumV2)
PrecentralFinal <- cbind(PrecentralV1,PrecentralV2)

####Behavioral/Cognitive Subsetting
PLIEDemographics <- subset(Demographics, select = c (pidn,sid,screener23,screener25,screener26,screener27___1,screener27___2,screener27___3,screener27___4,screener27___5,screener27___8,screener27___9,screener27a,screener28,screener29))
Checklist <- subset(AccessData, select = c (pidn,VisitNumber,CompletionID,PP_Completion.VisitID,PP_Completion.Notes,ADAS,QOL,SF12,PP_Completion.Mood,SPPB,MAIA,Mobility,Affect,Isolation,PLIE.Experience,PerformedPerProtocol,AdverseEvents,Unblinded,GroupGuess,GroupGuess_confidence,GroupGuess_desc,AssistiveDevice))
ADASCOG <- subset(AccessData, select=c(pidn,VisitNumber,ADAS_ID, ADAS_Cog.VisitID,Word.recall.1,Word.recall.2,Word.recall.3,Word.recall,Word.recall.auto.mean,Naming,Commands,Constructional.Praxis,Ideational.Praxis,Orientation1,Delay,Recognition,Remember.Instructions,Spoken.language.ability,Word.finding.difficulty,Comprehension,ADAS_Cog.Total,ADAS_Cog.AutoTotal,ADAS_Cog.Notes))
MAIA <- subset(AccessData, select=c(pidn,VisitNumber,MAIA.VisitID,MAIA1,MAIA2,MAIA3,MAIA4,MAIA5,MAIA6,MAIA7,MAIA8,MAIA9,MAIA10,MAIA11,MAIA_AR_Mean,MAIA_SR_Mean,MAIA_AR_AutoMean,MAIA_SR_AutoMean))
Mood <- subset(AccessData, select=c(pidn,VisitNumber,Mood.Mood_ID,Mood.VisitID,Mood.Notes,Mood1,Mood2,Mood3,Mood4,Mood5,Mood6,Mood7,Mood8,Mood9,Mood10,Mood11,Mood12,Mood13,Mood14,Mood15,Mood_Total,Mood_AutoTotal))
Mobility <- subset(AccessData, select=c(pidn,VisitNumber,NeuroQOL_Mobility.VisitID,NeuroQOL_Mobility.Notes,Mobility1,Mobility2,Mobility3,Mobility4,Mobility5,Mobility6,Mobility7,Mobility7,Mobility_Total,Mobility_AutoTotal))
PAWB <- subset(AccessData, select=c(pidn,VisitNumber,PAWBID,NeuroQOL_PAWB.VisitID,NeuroQOL_PAWB.Notes,PAWB1,PAWB2,PAWB3,PAWB4,PAWB5,PAWB6,PAWB7,PAWB8,PAWB9,PAWB_Total,PAWB_AutoTotal))
PLIE_Experience <- subset(AccessData, select=c(pidn,VisitNumber,PLIE.Experience.IsolationID,PLIE.Experience.VisitID,PLIE.Experience.Notes,PLIE_Experience1,PLIE_Experience2,PLIE_Experience3,PLIE_Experience4,PLIE_Experience5,PLIE_Experience6,PLIE_Experience7,PLIE_Experience8))
Isolation <- subset(AccessData, select=c(pidn,VisitNumber,PROMIS_Isolation.IsolationID,PROMIS_Isolation.VisitID,PROMIS_Isolation.Notes,Isolation1,Isolation2,Isolation3,Isolation4,Isolation_Total,Isolation_AutoTotal))
QOL <- subset(AccessData, select=c(pidn,VisitNumber,QOL_ID,QOL.VisitID,QOL.Notes,Physical.Health,Energy,QOL.Mood,Living.Situation,Memory,Family,Marriage,Friends,Self.as.a.whole,Ability.house.chores,Ability.fun,Money,Life.as.a.whole,QOL.Total,QOL.AutoTotal))
SF12 <- subset(AccessData, select=c(pidn,VisitNumber,SF12_ID,SF12.VisitID,sfhealth,sfmodact,sfclimbingstairs,sfacclessphyheal,sflimkindwork,sfacclessemopro,sflesscareemopro,sfpaininterf,sfcalmpeace,sfenergy,sffeltdown,sfsocialact,SF12.Notes))
SPPB <- subset(AccessData, select=c(pidn,VisitNumber,SPPB_ID,SPPB.VisitID,Side.by.side,Side.by.side_Reason,SbS.SEC,Semi.Tandem,Semi.Tandem_Reason,SemiT.SEC,Tandem.Stand,Tandem.Stand_Reason,T.Stn.SEC,Total.Balance.score,AutoTotal.Balance.Score,First.Gait.speed.time..sec,First.Gait.speed_Reason,Second.Gait.speed.time..sec,Second.Gait.speed_Reason,shorter.of.two.times..sec,Gait_Aids,X10.ft.walk.Gait.speed..score,Single.chair.stand,Repeated.chair.stand_Reason,Chair.stands.attempted,Chair_stand_not_attempted_Reason,Attempts.time..sec,Able.to.stand.five.times..sec,ChairStandBlankets,Repeated.chair.stand.Score.,TOTAL.SPPB.Score.,AutoTotal.SPPB.Score,SFT.BckSrt,SFT.S.R,SFT.Leg,SFT.S.R_Left1,SFT.S.R_Left2,SFT.S.R_Right1,SFT.S.R_Right2,SFT.8FT1,SFT.8FT2,SPPB.Notes))


###Behvioral/Cogntiive As.Numeric
PLIEDemographics[, 3:ncol(PLIEDemographics)]           <- sapply(PLIEDemographics[, 3:ncol(PLIEDemographics)], as.numeric)
ADASCOG[, 5:ncol(ADASCOG)]           <- sapply(ADASCOG[, 5:ncol(ADASCOG)], as.numeric)
MAIA[, 4:ncol(MAIA)]           <- sapply(MAIA[, 4:ncol(MAIA)], as.numeric)
Mood[, 6:ncol(Mood)]           <- sapply(Mood[, 6:ncol(Mood)], as.numeric)
Mobility[, 5:ncol(Mobility)]           <- sapply(Mobility[, 5:ncol(Mobility)], as.numeric)
PAWB[, 6:ncol(PAWB)]           <- sapply(PAWB[, 6:ncol(PAWB)], as.numeric)
PLIE_Experience[, 2:ncol(PLIE_Experience)]           <- sapply(PLIE_Experience[, 2:ncol(PLIE_Experience)], as.numeric)
Isolation[, 5:ncol(Isolation)]           <- sapply(Isolation[, 5:ncol(Isolation)], as.numeric)
QOL[, 6:ncol(QOL)]           <- sapply(QOL[, 6:ncol(QOL)], as.numeric)
SF12[, 5:ncol(SF12)]           <- sapply(SF12[, 5:ncol(SF12)], as.numeric)
SPPB[, 4:ncol(SPPB)]           <- sapply(SPPB[, 4:ncol(SPPB)], as.numeric)

###Transpose first
ADASCOG_Final <- data.frame(t(ADASCOG[-1]))
colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(ADASCOG_Final, is.numeric)
ADASCOG_Final[is.num] <- lapply(ADASCOG_Final[is.num], round, 2)
names(ADASCOG_Final) <- sub("V1", "", names(ADASCOG_Final))

######## Subset ADAS_Cog
ADASCOG_Final <- subset(ADASCOG_Final, select = c("701", "701V2", "702", "702V2", "704", "704V2", "705", "705V2", "706", "706V2", "710", "710V2", "716", "716V2", "718", "718V2", "719", "719V2", "721", "721V2", "724", "724V2", "725", "725V2", "726", "726V2", "728", "728V2", "730", "730V2", "731", "731V2", "732", "732V2", "733", "733V2"))
ADASCOGV1 <- subset(ADASCOG_Final, select = c("701", "702", "704", "705","706", "710", "716", "718", "719", "721", "724", "725", "726", "728", "730", "731", "732", "733"))
ADASCOGV2 <- subset(ADASCOG_Final, select = c("701V2", "702V2", "704V2", "705V2", "706V2", "710V2", "716V2", "718V2", "719V2", "721V2", "724V2", "725V2", "726V2", "728V2", "730V2", "731V2", "732V2", "733V2"))

####Re-transpose lmaoooo
###Transpose again
ADASCOGV1 <- data.frame(t(ADASCOGV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(ADASCOGV1, is.numeric)
ADASCOGV1[is.num] <- lapply(ADASCOGV1[is.num], round, 2)

ADASCOGV2 <- data.frame(t(ADASCOGV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(ADASCOGV2, is.numeric)
ADASCOGV2[is.num] <- lapply(ADASCOGV2[is.num], round, 2)

####Reshape data
ADASCOGV1V2 <- rbind(ADASCOGV1,ADASCOGV2)

####Eye-Tracking Transposing, Subsetting, Transposing again, then Reshaping for Graphs

EyeTracking <- data.frame(t(PLIE_EyeTracking[-1]))
colnames(EyeTracking) <- PLIE_EyeTracking[, 1]
is.num <- sapply(EyeTracking, is.numeric)
EyeTracking[is.num] <- lapply(EyeTracking[is.num], round, 2)

EyeTracking <- subset(EyeTracking, select = c(PLIE135.1,PLIE135.2,PLIE154.1,PLIE154.2,PLIE166.1,PLIE166.2))

EyeV1 <- subset(EyeTracking, select = c(PLIE135.1,PLIE154.1,PLIE166.1))
#names(EyeV1) <- sub(".1", "", names(EyeV1))
EyeV2 <- subset(EyeTracking, select = c(PLIE135.2,PLIE154.2,PLIE166.2))

EyeV1 <- data.frame(t(EyeV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(EyeV1, is.numeric)
EyeV1[is.num] <- lapply(EyeV1[is.num], round, 2)

EyeV2 <- data.frame(t(EyeV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(EyeV2, is.numeric)
EyeV2[is.num] <- lapply(EyeV2[is.num], round, 2)

Eye_Final <- rbind(EyeV1,EyeV2)

####SPPB Transposing, Subsetting, Transposing again, then Reshaping for Graphs

SPPB_Final <- data.frame(t(SPPB[-1]))
colnames(SPPB_Final) <- SPPB[, 1]
is.num <- sapply(SPPB_Final, is.numeric)
SPPB_Final[is.num] <- lapply(SPPB_Final[is.num], round, 2)
names(SPPB_Final) <- sub("V1", "", names(SPPB_Final))

SPPB_Final <- subset(SPPB_Final, select = c("701", "701V2", "702", "702V2", "704", "704V2", "705", "705V2", "706", "706V2", "710", "710V2", "716", "716V2", "718", "718V2", "719", "719V2", "721", "721V2", "724", "724V2", "725", "725V2", "726", "726V2", "728", "728V2", "730", "730V2", "731", "731V2", "732", "732V2", "733", "733V2"))
SPPBV1 <- subset(SPPB_Final, select = c("701", "702", "704", "705","706", "710", "716", "718", "719", "721", "724", "725", "726", "728", "730", "731", "732", "733"))
SPPBV2 <- subset(SPPB_Final, select = c("701V2", "702V2", "704V2", "705V2", "706V2", "710V2", "716V2", "718V2", "719V2", "721V2", "724V2", "725V2", "726V2", "728V2", "730V2", "731V2", "732V2", "733V2"))

SPPBV1 <- data.frame(t(SPPBV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(SPPBV1, is.numeric)
SPPBV1[is.num] <- lapply(SPPBV1[is.num], round, 2)

SPPBV2 <- data.frame(t(SPPBV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(SPPBV2, is.numeric)
SPPBV2[is.num] <- lapply(SPPBV2[is.num], round, 2)

SPPBV1V2 <- rbind(SPPBV1,SPPBV2)

####QOL Transposing, Subsetting, Transposing again, then Reshaping for Graphs
QOL_Final <- data.frame(t(QOL[-1]))
colnames(QOL_Final) <- QOL[, 1]
is.num <- sapply(QOL_Final, is.numeric)
QOL_Final[is.num] <- lapply(QOL_Final[is.num], round, 2)
names(QOL_Final) <- sub("V1", "", names(QOL_Final))

QOL_Final <- subset(QOL_Final, select = c("701", "701V2", "702", "702V2", "704", "704V2", "705", "705V2", "706", "706V2", "710", "710V2", "716", "716V2", "718", "718V2", "719", "719V2", "721", "721V2", "724", "724V2", "725", "725V2", "726", "726V2", "728", "728V2", "730", "730V2", "731", "731V2", "732", "732V2", "733", "733V2"))
QOLV1 <- subset(QOL_Final, select = c("701", "702", "704", "705", "706", "710", "716", "718", "719", "721", "724", "725", "726", "728", "730", "731", "732", "733"))
QOLV2 <- subset(QOL_Final, select = c("701V2", "702V2", "704V2", "705V2", "706V2", "710V2", "716V2", "718V2", "719V2", "721V2", "724V2", "725V2", "726V2", "728V2", "730V2", "731V2", "732V2", "733V2"))

QOLV1 <- data.frame(t(QOLV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(QOLV1, is.numeric)
QOLV1[is.num] <- lapply(QOLV1[is.num], round, 2)

QOLV2 <- data.frame(t(QOLV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(QOLV2, is.numeric)
QOLV2[is.num] <- lapply(QOLV2[is.num], round, 2)

QOLV1V2 <- rbind(QOLV1,QOLV2)

####Mood Transposing, Subsetting, Transposing again, then Reshaping for Graphs
Mood_Final <- data.frame(t(Mood[-1]))
colnames(Mood_Final) <- Mood[, 1]
is.num <- sapply(Mood_Final, is.numeric)
Mood_Final[is.num] <- lapply(Mood_Final[is.num], round, 2)
names(Mood_Final) <- sub("V1", "", names(Mood_Final))

Mood_Final <- subset(Mood_Final, select = c("701", "701V2", "702", "702V2", "704", "704V2", "705", "705V2", "706", "706V2", "710", "710V2", "716", "716V2", "718", "718V2", "719", "719V2", "721", "721V2", "724", "724V2", "725", "725V2", "726", "726V2", "728", "728V2", "730", "730V2", "731", "731V2", "732", "732V2", "733", "733V2"))
MoodV1 <- subset(Mood_Final, select = c("701", "702", "704", "705", "706", "710", "716", "718", "719", "721", "724", "725", "726", "728", "730", "731", "732", "733"))
MoodV2 <- subset(Mood_Final, select = c("701V2", "702V2", "704V2", "705V2", "706V2", "710V2", "716V2", "718V2", "719V2", "721V2", "724V2", "725V2", "726V2", "728V2", "730V2", "731V2", "732V2", "733V2"))

MoodV1 <- data.frame(t(MoodV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(MoodV1, is.numeric)
MoodV1[is.num] <- lapply(MoodV1[is.num], round, 2)

MoodV2 <- data.frame(t(MoodV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(MoodV2, is.numeric)
MoodV2[is.num] <- lapply(MoodV2[is.num], round, 2)

MoodV1V2 <- rbind(MoodV1,MoodV2)

####MAIA Transposing, Subsetting, Transposing again, then Reshaping for Graphs

MAIA_Final <- data.frame(t(MAIA[-1]))
colnames(MAIA_Final) <- MAIA[, 1]
is.num <- sapply(MAIA_Final, is.numeric)
MAIA_Final[is.num] <- lapply(MAIA_Final[is.num], round, 2)
names(MAIA_Final) <- sub("V1", "", names(MAIA_Final))

MAIA_Final <- subset(MAIA_Final, select = c("701", "701V2", "702", "702V2", "704", "704V2", "705", "705V2", "706", "706V2", "710", "710V2", "716", "716V2", "718", "718V2", "719", "719V2", "721", "721V2", "724", "724V2", "725", "725V2", "726", "726V2", "728", "728V2", "730", "730V2", "731", "731V2", "732", "732V2", "733", "733V2"))
MAIAV1 <- subset(MAIA_Final, select = c("701", "702", "704", "705", "706", "710", "716", "718", "719", "721", "724", "725", "726", "728", "730", "731", "732", "733"))
MAIAV2 <- subset(MAIA_Final, select = c("701V2", "702V2", "704V2", "705V2", "706V2", "710V2", "716V2", "718V2", "719V2", "721V2", "724V2", "725V2", "726V2", "728V2", "730V2", "731V2", "732V2", "733V2"))

MAIAV1 <- data.frame(t(MAIAV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(MAIAV1, is.numeric)
MAIAV1[is.num] <- lapply(MAIAV1[is.num], round, 2)

MAIAV2 <- data.frame(t(MAIAV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(MAIAV2, is.numeric)
MAIAV2[is.num] <- lapply(MAIAV2[is.num], round, 2)

MAIAV1V2 <- rbind(MAIAV1,MAIAV2)

####Isolation Transposing, Subsetting, Transposing again, then Reshaping for Graphs

Isolation_Final <- data.frame(t(Isolation[-1]))
colnames(Isolation_Final) <- Isolation[, 1]
is.num <- sapply(Isolation_Final, is.numeric)
Isolation_Final[is.num] <- lapply(Isolation_Final[is.num], round, 2)
names(Isolation_Final) <- sub("V1", "", names(Isolation_Final))

Isolation_Final <- subset(Isolation_Final, select = c("701", "701V2", "702", "702V2", "704", "704V2", "705", "705V2", "706", "706V2", "710", "710V2", "716", "716V2", "718", "718V2", "719", "719V2", "721", "721V2", "724", "724V2", "725", "725V2", "726", "726V2", "728", "728V2", "730", "730V2", "731", "731V2", "732", "732V2", "733", "733V2"))
IsolationV1 <- subset(Isolation_Final, select = c("701", "702", "704", "705", "706", "710", "716", "718", "719", "721", "724", "725", "726", "728", "730", "731", "732", "733"))
IsolationV2 <- subset(Isolation_Final, select = c("701V2", "702V2", "704V2", "705V2", "706V2", "710V2", "716V2", "718V2", "719V2", "721V2", "724V2", "725V2", "726V2", "728V2", "730V2", "731V2", "732V2", "733V2"))

IsolationV1 <- data.frame(t(IsolationV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(IsolationV1, is.numeric)
IsolationV1[is.num] <- lapply(IsolationV1[is.num], round, 2)

IsolationV2 <- data.frame(t(IsolationV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(IsolationV2, is.numeric)
IsolationV2[is.num] <- lapply(IsolationV2[is.num], round, 2)

IsolationV1V2 <- rbind(IsolationV1,IsolationV2)

####PAWB Transposing, Subsetting, Transposing again, then Reshaping for Graphs

PAWB_Final <- data.frame(t(PAWB[-1]))
colnames(PAWB_Final) <- PAWB[, 1]
is.num <- sapply(PAWB_Final, is.numeric)
PAWB_Final[is.num] <- lapply(PAWB_Final[is.num], round, 2)
names(PAWB_Final) <- sub("V1", "", names(PAWB_Final))

PAWB_Final <- subset(PAWB_Final, select = c("701", "701V2", "702", "702V2", "704", "704V2", "705", "705V2", "706", "706V2", "710", "710V2", "716", "716V2", "718", "718V2", "719", "719V2", "721", "721V2", "724", "724V2", "725", "725V2", "726", "726V2", "728", "728V2", "730", "730V2", "731", "731V2", "732", "732V2", "733", "733V2"))
PAWBV1 <- subset(PAWB_Final, select = c("701", "702", "704", "705", "706", "710", "716", "718", "719", "721", "724", "725", "726", "728", "730", "731", "732", "733"))
PAWBV2 <- subset(PAWB_Final, select = c("701V2", "702V2", "704V2", "705V2", "706V2", "710V2", "716V2", "718V2", "719V2", "721V2", "724V2", "725V2", "726V2", "728V2", "730V2", "731V2", "732V2", "733V2"))

PAWBV1 <- data.frame(t(PAWBV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(PAWBV1, is.numeric)
PAWBV1[is.num] <- lapply(PAWBV1[is.num], round, 2)

PAWBV2 <- data.frame(t(PAWBV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(PAWBV2, is.numeric)
PAWBV2[is.num] <- lapply(PAWBV2[is.num], round, 2)

PAWBV1V2 <- rbind(PAWBV1,PAWBV2)

###################################################################################################################

####ADAS-Cog
colnames(ADASCOGV1) <- paste(colnames(ADASCOGV1), "V1", sep = "")
colnames(ADASCOGV2) <- paste(colnames(ADASCOGV2), "V2", sep = "")
ADASCOGV1V2_S <- cbind(ADASCOGV1,ADASCOGV2)
ADASCOGV1V2_S$pidn <- rownames(ADASCOGV1V2_S) 
ADASCOGV1V2_S$pidn <- factor(ADASCOGV1V2_S$pidn, levels=unique(ADASCOGV1V2_S$pidn))
ADASCOGV1V2_S <- ADASCOGV1V2_S %>%
  select(pidn, everything())

ADASCOGV1V2_S <- melt(ADASCOGV1V2_S, id.vars="pidn", measure.vars=c("ADAS_Cog.AutoTotalV1","ADAS_Cog.AutoTotalV2"))

#####SPPB
colnames(SPPBV1) <- paste(colnames(SPPBV1), "V1", sep = "")
colnames(SPPBV2) <- paste(colnames(SPPBV2), "V2", sep = "")
SPPBV1V2_S <- cbind(SPPBV1,SPPBV2)
SPPBV1V2_S$pidn <- rownames(SPPBV1V2_S) 
SPPBV1V2_S$pidn <- factor(SPPBV1V2_S$pidn, levels=unique(SPPBV1V2_S$pidn))
SPPBV1V2_S <- SPPBV1V2_S %>%
  select(pidn, everything())

SPPBV1V2_S_ <- melt(SPPBV1V2_S, id.vars="pidn", measure.vars=c("AutoTotal.SPPB.ScoreV1","AutoTotal.SPPB.ScoreV2"))
GaitV1V2_S <- melt(SPPBV1V2_S, id.vars="pidn", measure.vars=c("shorter.of.two.times..secV1","shorter.of.two.times..secV2"))

#SPPB_test <- subset(SPPBV1V2_S, select=c("pidn", "AutoTotal.SPPB.ScoreV1","AutoTotal.SPPB.ScoreV2"))
#t.test(SPPBV1V2_S$AutoTotal.SPPB.ScoreV1,SPPBV1V2_S$AutoTotal.SPPB.ScoreV2, paired=TRUE, alternative = "two.sided")

####QOL
colnames(QOLV1) <- paste(colnames(QOLV1), "V1", sep = "")
colnames(QOLV2) <- paste(colnames(QOLV2), "V2", sep = "")
QOLV1V2_S <- cbind(QOLV1,QOLV2)
QOLV1V2_S$pidn <- rownames(QOLV1V2_S) 
QOLV1V2_S$pidn <- factor(QOLV1V2_S$pidn, levels=unique(QOLV1V2_S$pidn))
QOLV1V2_S <- QOLV1V2_S %>%
  select(pidn, everything())

QOLV1V2_S <- melt(QOLV1V2_S, id.vars="pidn", measure.vars=c("QOL.AutoTotalV1","QOL.AutoTotalV2"))


####Mood
colnames(MoodV1) <- paste(colnames(MoodV1), "V1", sep = "")
colnames(MoodV2) <- paste(colnames(MoodV2), "V2", sep = "")
MoodV1V2_S <- cbind(MoodV1,MoodV2)
MoodV1V2_S$pidn <- rownames(MoodV1V2_S) 
MoodV1V2_S$pidn <- factor(MoodV1V2_S$pidn, levels=unique(MoodV1V2_S$pidn))
MoodV1V2_S <- MoodV1V2_S %>%
  select(pidn, everything())

MoodV1V2_S <- melt(MoodV1V2_S, id.vars="pidn", measure.vars=c("Mood_AutoTotalV1","Mood_AutoTotalV2"))


####MAIA
colnames(MAIAV1) <- paste(colnames(MAIAV1), "V1", sep = "")
colnames(MAIAV2) <- paste(colnames(MAIAV2), "V2", sep = "")
MAIAV1V2_S <- cbind(MAIAV1,MAIAV2)
MAIAV1V2_S$pidn <- rownames(MAIAV1V2_S) 
MAIAV1V2_S$pidn <- factor(MAIAV1V2_S$pidn, levels=unique(MAIAV1V2_S$pidn))
MAIAV1V2_S <- MAIAV1V2_S %>%
  select(pidn, everything())

###PIDN706 is missing the AR_AutoTotal value, but has a AR_Total value, how should I handle it?
MAIAV1V2_AR <- melt(MAIAV1V2_S, id.vars="pidn", measure.vars=c("MAIA_AR_AutoMeanV1","MAIA_AR_AutoMeanV2"))
MAIAV1V2_SR <- melt(MAIAV1V2_S, id.vars="pidn", measure.vars=c("MAIA_SR_AutoMeanV1","MAIA_SR_AutoMeanV2"))

#MAIAV2_AR_test <- subset(MAIAV1V2_S, select=c("pidn", "MAIA_AR_AutoMeanV1","MAIA_AR_AutoMeanV2"))
#t.test(MAIAV1V2_S$MAIA_AR_AutoMeanV1,MAIAV1V2_S$MAIA_AR_AutoMeanV2, paired=TRUE, alternative = "two.sided")

#MAIAV2_SR_test <- subset(MAIAV1V2_S, select=c("pidn", "MAIA_SR_AutoMeanV1","MAIA_SR_AutoMeanV2"))
#t.test(MAIAV1V2_S$MAIA_SR_AutoMeanV1,MAIAV1V2_S$MAIA_SR_AutoMeanV2, paired=TRUE, alternative = "two.sided")


####Isolation
colnames(IsolationV1) <- paste(colnames(IsolationV1), "V1", sep = "")
colnames(IsolationV2) <- paste(colnames(IsolationV2), "V2", sep = "")
IsolationV1V2_S <- cbind(IsolationV1,IsolationV2)
IsolationV1V2_S$pidn <- rownames(IsolationV1V2_S) 
IsolationV1V2_S$pidn <- factor(IsolationV1V2_S$pidn, levels=unique(IsolationV1V2_S$pidn))
IsolationV1V2_S <- IsolationV1V2_S %>%
  select(pidn, everything())

IsolationV1V2_S <- melt(IsolationV1V2_S, id.vars="pidn", measure.vars=c("Isolation_AutoTotalV1","Isolation_AutoTotalV2"))


####PAWB
colnames(PAWBV1) <- paste(colnames(PAWBV1), "V1", sep = "")
colnames(PAWBV2) <- paste(colnames(PAWBV2), "V2", sep = "")
PAWBV1V2_S <- cbind(PAWBV1,PAWBV2)
PAWBV1V2_S$pidn <- rownames(PAWBV1V2_S) 
PAWBV1V2_S$pidn <- factor(PAWBV1V2_S$pidn, levels=unique(PAWBV1V2_S$pidn))
PAWBV1V2_S <- PAWBV1V2_S %>%
  select(pidn, everything())

PAWBV1V2_S <- melt(PAWBV1V2_S, id.vars="pidn", measure.vars=c("PAWB_AutoTotalV1","PAWB_AutoTotalV2"))

######### Access Data
AccessData <- as.data.frame(t(AccessData))
colnames(AccessData) <- as.character(unlist(AccessData[2,]))
AccessData = AccessData[-(2),]
#colnames(AccessData) <- paste(colnames(AccessData), "V1", sep = "")

AccessData <- subset(AccessData, select = c ("701V1", "701V2", "702V1", "702V2", "704V1", "704V2", "705V1", "705V2", "706V1", "706V2", "710V1", "710V2", "716V1", "716V2", "718V1", "718V2", "719V1", "719V2", "721V1", "721V2", "724V1", "724V2", "725V1", "725V2", "726V1", "726V2", "728V1", "728V2", "730V1", "730V2", "731V1", "731V2", "732V1", "732V2", "733V1", "733V2"))
AccessDataV1 <- subset(AccessData, select = c("701V1", "702V1", "704V1", "705V1", "706V1", "710V1", "716V1", "718V1", "719V1", "721V1", "724V1", "725V1", "726V1", "728V1", "730V1", "731V1", "732V1", "733V1"))
AccessDataV2 <- subset(AccessData, select = c("701V2", "702V2", "704V2", "705V2", "706V2", "710V2", "716V2", "718V2", "719V2", "721V2", "724V2", "725V2", "726V2", "728V2", "730V2", "731V2", "732V2", "733V2"))

AccessDataV1 <- data.frame(t(AccessDataV1))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(AccessDataV1, is.numeric)
AccessDataV1[is.num] <- lapply(AccessDataV1[is.num], round, 2)

AccessDataV2 <- data.frame(t(AccessDataV2))
#colnames(ADASCOG_Final) <- ADASCOG[, 1]
is.num <- sapply(AccessDataV2, is.numeric)
AccessDataV2[is.num] <- lapply(AccessDataV2[is.num], round, 2)

colnames(AccessDataV1)[2:ncol(AccessDataV1)] <- paste(colnames(AccessDataV1[2:ncol(AccessDataV2)]), "V1", sep = "")
colnames(AccessDataV2)[2:ncol(AccessDataV2)] <- paste(colnames(AccessDataV2[2:ncol(AccessDataV2)]), "V2", sep = "")
AccessDataV1V2 <- cbind(AccessDataV1,AccessDataV2)

#Subset for Linda
AccessData_Clean <- subset(AccessDataV1V2, select = c(PIDN, ADAS_Cog.AutoTotalV1, ADAS_Cog.AutoTotalV2, QOL.AutoTotalV1,QOL.AutoTotalV2, AutoTotal.SPPB.ScoreV1,AutoTotal.SPPB.ScoreV2, Mood_AutoTotalV1, Mood_AutoTotalV2, MAIA_AR_AutoMeanV1,MAIA_AR_AutoMeanV2, MAIA_SR_AutoMeanV1,MAIA_SR_AutoMeanV2, Mobility_AutoTotalV1, Mobility_AutoTotalV2, Isolation_AutoTotalV1, Isolation_AutoTotalV2, PAWB_AutoTotalV1, PAWB_AutoTotalV2))
CerebellumPrecentral_Clean <- cbind(CerebellumFinal,PrecentralFinal)
#write.csv(AccessData_Clean, "AccessData_Clean.csv")
#write.csv(PLIE_Experience, "PLIE_Experience.csv")
#write.csv(CerebellumPrecentral_Clean, "CerebellumPrecentral_Clean.csv")


###Subset for Deb
#SPPB- shorter of two times sec
#SPPB: Able to stand five times: sec (All the 0s should be removed because that means they weren't able to do it)
#Sit-and-Reach: Pick the most positive value of all 4 
#8ft get-up (Shorter of-the-two values)

Shorter.of.two.times <- subset(SPPBV1V2_S, select=c("pidn", "shorter.of.two.times..secV1", "shorter.of.two.times..secV2"))
Sit_and_reach <- subset(SPPBV1V2_S, select=c("pidn","SFT.S.RV1","SFT.LegV1","SFT.S.R_Left1V1","SFT.S.R_Left2V1","SFT.S.R_Right1V1","SFT.S.R_Right2V1","SFT.S.RV2","SFT.LegV2","SFT.S.R_Left1V2","SFT.S.R_Left2V2","SFT.S.R_Right1V2","SFT.S.R_Right2V2"))
ChairStand <- subset(SPPBV1V2_S, select=c("pidn","Single.chair.standV1","Repeated.chair.stand_ReasonV1","Chair.stands.attemptedV1","Chair_stand_not_attempted_ReasonV1","Attempts.time..secV1","Able.to.stand.five.times..secV1","ChairStandBlanketsV1","Repeated.chair.stand.Score.V1","Single.chair.standV2","Repeated.chair.stand_ReasonV2","Chair.stands.attemptedV2","Chair_stand_not_attempted_ReasonV2","Attempts.time..secV2","Able.to.stand.five.times..secV2","ChairStandBlanketsV2","Repeated.chair.stand.Score.V2"))
EightFootGetUpAndGo <- subset(SPPBV1V2_S, select=c("pidn","SFT.8FT1V1","SFT.8FT2V1","SFT.8FT1V2","SFT.8FT2V2"))

#write.csv(Shorter.of.two.times, "Shorter.of.two.times.csv")
#write.csv(Sit_and_reach, "Sit_and_reach.csv")
#write.csv(ChairStand, "ChairStand.csv")
#write.csv(EightFootGetUpAndGo, "EightFootGetUpAndGo.csv")

###Subset ADASCog for Linda
#write.csv(ADASCOGV1V2, "ADASCOGV1V2_vertical.csv")
#write.csv(ADASCOGV1V2_S, "ADASCOGV1V2_horizontal.csv")


###SPPB shorter of two times sec
##################Perfusion Data GET STUCK IN!!!###################
CerebellumFinal$pidn <- rownames(CerebellumFinal) 
CerebellumFinal <- CerebellumFinal %>%
  select(pidn, everything())
CerebellumFinal$pidn <- factor(CerebellumFinal$pidn, levels=unique(CerebellumFinal$pidn))

Cerebellum_HP <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("HPV1","HPV2"))
Cerebellum_Insula <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("InsulaV1","InsulaV2"))
Cerebellum_Temporal <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("TemporalV1","TemporalV2"))
Cerebellum_Parietal <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("ParietalV1","ParietalV2"))
Cerebellum_Frontal <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("FrontalV1","FrontalV2"))
Cerebellum_Occipital <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("OccipitalV1","OccipitalV2"))
Cerebellum_SuperiorFrontal <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("SuperiorFrontalV1","SuperiorFrontalV2"))
Cerebellum_CaudalACC <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("CaudalACCV1","CaudalACCV2"))
Cerebellum_SuperiorParietal <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("SuperiorParietalV1","SuperiorParietalV2"))
Cerebellum_InferiorParietal <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("InferiorParietalV1","InferiorParietalV2"))
Cerebellum_Precuneus <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("PrecuneusV1","PrecuneusV2"))
Cerebellum_Supramarginal <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("SupramarginalV1","SupramarginalV2"))

PrecentralFinal$pidn <- rownames(PrecentralFinal) 
PrecentralFinal <- PrecentralFinal %>%
  select(pidn, everything())
PrecentralFinal$pidn <- factor(PrecentralFinal$pidn, levels=unique(PrecentralFinal$pidn))

Precentral_HP <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("HPV1","HPV2"))
Precentral_Insula <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("InsulaV1","InsulaV2"))
Precentral_Temporal <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("TemporalV1","TemporalV2"))
Precentral_Parietal <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("ParietalV1","ParietalV2"))
Precentral_Frontal <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("FrontalV1","FrontalV2"))
Precentral_Occipital <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("OccipitalV1","OccipitalV2"))
Precentral_SuperiorFrontal <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("SuperiorFrontalV1","SuperiorFrontalV2"))
Precentral_CaudalACC <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("CaudalACCV1","CaudalACCV2"))
Precentral_SuperiorParietal <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("SuperiorParietalV1","SuperiorParietalV2"))
Precentral_InferiorParietal <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("InferiorParietalV1","InferiorParietalV2"))
Precentral_Precuneus <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("PrecuneusV1","PrecuneusV2"))
Precentral_Supramarginal <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("SupramarginalV1","SupramarginalV2"))

Cerebell_All <- melt(CerebellumFinal, id.vars="pidn", measure.vars=c("HPV1","HPV2","InsulaV1","InsulaV2","TemporalV1","TemporalV2","ParietalV1","ParietalV2","FrontalV1","FrontalV2","OccipitalV1","OccipitalV2","SuperiorFrontalV1","SuperiorFrontalV2","CaudalACCV1","CaudalACCV2","SuperiorParietalV1","SuperiorParietalV2","InferiorParietalV1","InferiorParietalV2","PrecuneusV1","PrecuneusV2","SupramarginalV1","SupramarginalV2"))
#ggplot(Cerebellum_All,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))

Precentral_All <- melt(PrecentralFinal, id.vars="pidn", measure.vars=c("HPV1","HPV2","InsulaV1","InsulaV2","TemporalV1","TemporalV2","ParietalV1","ParietalV2","FrontalV1","FrontalV2","OccipitalV1","OccipitalV2","SuperiorFrontalV1","SuperiorFrontalV2","CaudalACCV1","CaudalACCV2","SuperiorParietalV1","SuperiorParietalV2","InferiorParietalV1","InferiorParietalV2","PrecuneusV1","PrecuneusV2","SupramarginalV1","SupramarginalV2"))
#ggplot(Precentral_All,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))

########### Eye-Tracking
colnames(EyeV1) <- paste(colnames(EyeV1), "V1", sep = "")
colnames(EyeV2) <- paste(colnames(EyeV2), "V2", sep = "")
Eye_Final_S <- cbind(EyeV1,EyeV2)
Eye_Final_S$pidn <- rownames(Eye_Final_S) 
Eye_Final_S <- Eye_Final_S %>%
  select(pidn, everything())
Eye_Final_S$pidn <- factor(Eye_Final_S$pidn, levels=unique(Eye_Final_S$pidn))

###################################### Melting everything to prep for graphs ##########################
Eye_Final_All <- melt(Eye_Final_S, id.vars="pidn", measure.vars=c("latency.msV1","latency.msV2","accel.degrees2V1","accel.degrees2V2","gain.SS.V1","gain.SS.V2","sacc.V1","sacc.V2","amp.degreesV1","amp.degreesV2","prop.smoothV1","prop.smoothV2","anisotropyV1","anisotropyV2","DirectionTuningasymmetryV1","DirectionTuningasymmetryV2","DirectionTuningnoiseV1","DirectionTuningnoiseV2","SpeedTuningresponsivenessV1","SpeedTuningresponsivenessV2","SpeedTuningnoise.degreesV1","SpeedTuningnoise.degreesV2"))
Eye_latency <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("latency.msV1","latency.msV2"))
Eye_accel <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("accel.degrees2V1","accel.degrees2V2"))
Eye_gains <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("gain.SS.V1","gain.SS.V2"))
Eye_sacc <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("sacc.V1","sacc.V2"))
Eye_amp.degrees <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("amp.degreesV1","amp.degreesV2"))
Eye_prop.smooth <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("prop.smoothV1","prop.smoothV2"))
Eye_anisotropy <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("anisotropyV1","anisotropyV2"))
Eye_DirectionTuningasymmetry <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("DirectionTuningasymmetryV1","DirectionTuningasymmetryV2"))
Eye_DirectionTuningnoise <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("DirectionTuningnoiseV1","DirectionTuningnoiseV2"))
Eye_SpeedTuningresponsiveness <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("SpeedTuningresponsivenessV1","SpeedTuningresponsivenessV2"))
Eye_SpeedTuningnoise <- melt(Eye_Final_S , id.vars="pidn", measure.vars=c("SpeedTuningnoise.degreesV1","SpeedTuningnoise.degreesV2"))


HP_C <- ggplot(Cerebellum_HP,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Insula_C <- ggplot(Cerebellum_Insula,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Temporal_C <- ggplot(Cerebellum_Temporal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Parietal_C <- ggplot(Cerebellum_Parietal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Frontal_C <- ggplot(Cerebellum_Frontal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Occipital_C <- ggplot(Cerebellum_Occipital,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
SuperiorFrontal_C <- ggplot(Cerebellum_SuperiorFrontal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
CaudalACC_C <- ggplot(Cerebellum_CaudalACC,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
SuperiorParietal_C <- ggplot(Cerebellum_SuperiorParietal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
InferiorParietal_C <- ggplot(Cerebellum_InferiorParietal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Precuneus_C <- ggplot(Cerebellum_Precuneus,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Supramarginal_C <- ggplot(Cerebellum_Supramarginal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))


HP_P <- ggplot(Precentral_HP,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Insula_P <- ggplot(Precentral_Insula,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Temporal_P <- ggplot(Precentral_Temporal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Parietal_P <- ggplot(Precentral_Parietal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Frontal_P <- ggplot(Precentral_Frontal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Occipital_P <- ggplot(Precentral_Occipital,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
SuperiorFrontal_P <- ggplot(Precentral_SuperiorFrontal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
CaudalACC_P <- ggplot(Precentral_CaudalACC,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
SuperiorParietal_P <- ggplot(Precentral_SuperiorParietal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
InferiorParietal_P <- ggplot(Precentral_InferiorParietal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Precuneus_P <- ggplot(Precentral_Precuneus,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Supramarginal_P <- ggplot(Precentral_Supramarginal,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))


#ggplot(Eye_Final_All,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Latency <- ggplot(Eye_latency,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Accel <- ggplot(Eye_accel,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Gains <- ggplot(Eye_gains,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Sacc <- ggplot(Eye_sacc,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Amp.degrees <- ggplot(Eye_amp.degrees,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Prop.smooth <- ggplot(Eye_prop.smooth,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
Anisotropy <- ggplot(Eye_anisotropy,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
DirectionTuningasymmetry <- ggplot(Eye_DirectionTuningasymmetry,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
DirectionTuningnoise <- ggplot(Eye_DirectionTuningnoise,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
SpeedTuningresponsiveness <- ggplot(Eye_SpeedTuningresponsiveness,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))
SpeedTuningnoise <- ggplot(Eye_SpeedTuningnoise,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn))

ADASCOGV1V2_S$label = ADASCOGV1V2_S$pidn
ADASCOGV1V2_S$label <- as.character(ADASCOGV1V2_S$label)
ADASCOGV1V2_S$label[19:36] <- ""
ADASPlot <- ggplot(ADASCOGV1V2_S,aes(x=variable, y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="ADASCog", y="ADASCog Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 38, size=5, family = "American Typewriter")
ADASPlot + theme(plot.title=element_text(size=20, 
                                       face="bold", 
                                       family="American Typewriter",
                                       color="black",
                                       hjust=0.5,
                                       lineheight=1.2),  # title
                axis.title.x=element_text(size=15, 
                                         face="bold", 
                                         family="American Typewriter",
                                         color="black",
                                         hjust=0.5,
                                         lineheight=1.2),  # title
                axis.text.x = element_text(size=12, 
                                           face="bold", 
                                           family="Palatino",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                axis.title.y=element_text(size=15, 
                                          face="bold", 
                                          family="American Typewriter",
                                          color="black",
                                          hjust=0.5,
                                          lineheight=1.2),  # title
                axis.text.y = element_text(size=12, 
                                           family="Palatino",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
               plot.subtitle=element_text(size=15, 
                                          family="American Typewriter",
                                          face="bold",
                                          hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                     vjust = 0,
                                                                                                                                                                                                                                                                                     hjust = 0,
                                                                                                                                                                                                                                                                                     box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                     point.padding = 0.25,
                                                                                                                                                                                                                                                                                     nudge_y = 1.2,
                                                                                                                                                                                                                                                                                     direction = "x",
                                                                                                                                                                                                                                                                                     segment.color = 'grey50') 


SPPBV1V2_S_$label = SPPBV1V2_S_$pidn
SPPBV1V2_S_$label <- as.character(SPPBV1V2_S_$label)
SPPBV1V2_S_$label[19:36] <- ""
SPPBPlot <- ggplot(SPPBV1V2_S_,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="Short Physical Performance Battery", y="SPPB Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 12, size=5, family = "American Typewriter")
SPPBPlot + theme(plot.title=element_text(size=20, 
                                         face="bold", 
                                         family="American Typewriter",
                                         color="black",
                                         hjust=0.5,
                                         lineheight=1.2),  # title
                 axis.title.x=element_text(size=15, 
                                           face="bold", 
                                           family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                 axis.text.x = element_text(size=12, 
                                            face="bold", 
                                            family="Palatino",
                                            color="black",
                                            hjust=0.5,
                                            lineheight=1.2),  # title
                 axis.title.y=element_text(size=15, 
                                           face="bold", 
                                           family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                 axis.text.y = element_text(size=12, 
                                            family="Palatino",
                                            color="black",
                                            hjust=0.5,
                                            lineheight=1.2),  # title
                 plot.subtitle=element_text(size=15, 
                                            family="American Typewriter",
                                            face="bold",
                                            hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                       vjust = 0,
                                                                                                                                                                                                                                                                                       hjust = 0,
                                                                                                                                                                                                                                                                                       box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                       point.padding = 0.25,
                                                                                                                                                                                                                                                                                       nudge_y = 0.6,
                                                                                                                                                                                                                                                                                       direction = "x",
                                                                                                                                                                                                                                                                                       segment.color = 'grey50') 


MoodV1V2_S$label = MoodV1V2_S$pidn
MoodV1V2_S$label <- as.character(MoodV1V2_S$label)
MoodV1V2_S$label[19:36] <- ""
MoodPlot <- ggplot(MoodV1V2_S,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="Mood", y="Mood Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 12.5, size=5, family = "American Typewriter")
MoodPlot + theme(plot.title=element_text(size=20, 
                                         face="bold", 
                                         family="American Typewriter",
                                         color="black",
                                         hjust=0.5,
                                         lineheight=1.2),  # title
                 axis.title.x=element_text(size=15, 
                                           face="bold", 
                                           family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                 axis.text.x = element_text(size=12, 
                                            face="bold", 
                                            family="Palatino",
                                            color="black",
                                            hjust=0.5,
                                            lineheight=1.2),  # title
                 axis.title.y=element_text(size=15, 
                                           face="bold", 
                                           family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                 axis.text.y = element_text(size=12, 
                                            family="Palatino",
                                            color="black",
                                            hjust=0.5,
                                            lineheight=1.2),  # title
                 plot.subtitle=element_text(size=15, 
                                            family="American Typewriter",
                                            face="bold",
                                            hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                       vjust = 0,
                                                                                                                                                                                                                                                                                       hjust = 0,
                                                                                                                                                                                                                                                                                       box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                       point.padding = 0.25,
                                                                                                                                                                                                                                                                                       nudge_y = 1.2,
                                                                                                                                                                                                                                                                                       direction = "x",
                                                                                                                                                                                                                                                                                       segment.color = 'grey50') 


QOLV1V2_S$label = QOLV1V2_S$pidn
QOLV1V2_S$label <- as.character(QOLV1V2_S$label)
QOLV1V2_S$label[19:36] <- ""
QOLPlot <-ggplot(QOLV1V2_S,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="Quality of Life", y="QOL Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 47, size=5, family = "American Typewriter")
QOLPlot + theme(plot.title=element_text(size=20, 
                                        face="bold", 
                                        family="American Typewriter",
                                        color="black",
                                        hjust=0.5,
                                        lineheight=1.2),  # title
                axis.title.x=element_text(size=15, 
                                          face="bold", 
                                          family="American Typewriter",
                                          color="black",
                                          hjust=0.5,
                                          lineheight=1.2),  # title
                axis.text.x = element_text(size=12, 
                                           face="bold", 
                                           family="Palatino",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                axis.title.y=element_text(size=15, 
                                          face="bold", 
                                          family="American Typewriter",
                                          color="black",
                                          hjust=0.5,
                                          lineheight=1.2),  # title
                axis.text.y = element_text(size=12, 
                                           family="Palatino",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                plot.subtitle=element_text(size=15, 
                                           family="American Typewriter",
                                           face="bold",
                                           hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                      vjust = 0,
                                                                                                                                                                                                                                                                                      hjust = 0,
                                                                                                                                                                                                                                                                                      box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                      point.padding = 0.25,
                                                                                                                                                                                                                                                                                      nudge_y = 1.2,
                                                                                                                                                                                                                                                                                      direction = "x",
                                                                                                                                                                                                                                                                                      segment.color = 'grey50') 


GaitV1V2_S$label = GaitV1V2_S$pidn
GaitV1V2_S$label <- as.character(GaitV1V2_S$label)
GaitV1V2_S$label[19:36] <- ""
GaitPlot <- ggplot(GaitV1V2_S,aes(x=variable ,y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="Gait", y="Shorter of Two Times Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 8.5, size=5, family = "American Typewriter")
GaitPlot + theme(plot.title=element_text(size=20, 
                                         face="bold", 
                                         family="American Typewriter",
                                         color="black",
                                         hjust=0.5,
                                         lineheight=1.2),  # title
                 axis.title.x=element_text(size=15, 
                                           face="bold", 
                                           family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                 axis.text.x = element_text(size=12, 
                                            face="bold", 
                                            family="Palatino",
                                            color="black",
                                            hjust=0.5,
                                            lineheight=1.2),  # title
                 axis.title.y=element_text(size=15, 
                                           face="bold", 
                                           family="American Typewriter",
                                           color="black",
                                           hjust=0.5,
                                           lineheight=1.2),  # title
                 axis.text.y = element_text(size=12, 
                                            family="Palatino",
                                            color="black",
                                            hjust=0.5,
                                            lineheight=1.2),  # title
                 plot.subtitle=element_text(size=15, 
                                            family="American Typewriter",
                                            face="bold",
                                            hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                       vjust = 0,
                                                                                                                                                                                                                                                                                       hjust = 0,
                                                                                                                                                                                                                                                                                       box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                       point.padding = 0.25,
                                                                                                                                                                                                                                                                                       nudge_y = 0.4,
                                                                                                                                                                                                                                                                                       direction = "x",
                                                                                                                                                                                                                                                                                       segment.color = 'grey50') 


IsolationV1V2_S$label = IsolationV1V2_S$pidn
IsolationV1V2_S$label <- as.character(IsolationV1V2_S$label)
IsolationV1V2_S$label[19:36] <- ""
IsolationPlot <- ggplot(na.omit(IsolationV1V2_S),aes(x=variable, y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="Isolation", y="Isolation AutoTotal Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 12.5, size=5, family = "American Typewriter")
IsolationPlot + theme(plot.title=element_text(size=20, 
                                              face="bold", 
                                              family="American Typewriter",
                                              color="black",
                                              hjust=0.5,
                                              lineheight=1.2),  # title
                      axis.title.x=element_text(size=15, 
                                                face="bold", 
                                                family="American Typewriter",
                                                color="black",
                                                hjust=0.5,
                                                lineheight=1.2),  # title
                      axis.text.x = element_text(size=12, 
                                                 face="bold", 
                                                 family="Palatino",
                                                 color="black",
                                                 hjust=0.5,
                                                 lineheight=1.2),  # title
                      axis.title.y=element_text(size=15, 
                                                face="bold", 
                                                family="American Typewriter",
                                                color="black",
                                                hjust=0.5,
                                                lineheight=1.2),  # title
                      axis.text.y = element_text(size=12, 
                                                 family="Palatino",
                                                 color="black",
                                                 hjust=0.5,
                                                 lineheight=1.2),  # title
                      plot.subtitle=element_text(size=15, 
                                                 family="American Typewriter",
                                                 face="bold",
                                                 hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                            vjust = 0,
                                                                                                                                                                                                                                                                                            hjust = 0,
                                                                                                                                                                                                                                                                                            box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                            point.padding = 0.25,
                                                                                                                                                                                                                                                                                            nudge_y = 0.7,
                                                                                                                                                                                                                                                                                            direction = "x",
                                                                                                                                                                                                                                                                                            segment.color = 'grey50') 


MAIAV1V2_SR$label = MAIAV1V2_SR$pidn
MAIAV1V2_SR$label <- as.character(MAIAV1V2_SR$label)
MAIAV1V2_SR$label[19:36] <- ""
MAIAV1V2_SRPlot <- ggplot(MAIAV1V2_SR,aes(x=variable, y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="Multidimensional Assessment of Interoceptive Awareness Self-Regulation", y="MAIA SR AutoTotal Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 5.25, size=5, family = "American Typewriter")
MAIAV1V2_SRPlot + theme(plot.title=element_text(size=20,
                                                face="bold", 
                                                family="American Typewriter",
                                                color="black",
                                                hjust=0.5,
                                                lineheight=1.2),  # title
                        axis.title.x=element_text(size=15, 
                                                  face="bold", 
                                                  family="American Typewriter",
                                                  color="black",
                                                  hjust=0.5,
                                                  lineheight=1.2),  # title
                        axis.text.x = element_text(size=12, 
                                                   face="bold", 
                                                   family="Palatino",
                                                   color="black",
                                                   hjust=0.5,
                                                   lineheight=1.2),  # title
                        axis.title.y=element_text(size=15, 
                                                  face="bold", 
                                                  family="American Typewriter",
                                                  color="black",
                                                  hjust=0.5,
                                                  lineheight=1.2),  # title
                        axis.text.y = element_text(size=12, 
                                                   family="Palatino",
                                                   color="black",
                                                   hjust=0.5,
                                                   lineheight=1.2),  # title
                        plot.subtitle=element_text(size=15, 
                                                   family="American Typewriter",
                                                   face="bold",
                                                   hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                              vjust = 0,
                                                                                                                                                                                                                                                                                              hjust = 0.1,
                                                                                                                                                                                                                                                                                              box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                              point.padding = 0.25,
                                                                                                                                                                                                                                                                                              nudge_y = 0.2,
                                                                                                                                                                                                                                                                                              direction = "x",
                                                                                                                                                                                                                                                                                              segment.color = 'grey50') 


MAIAV1V2_AR$label = MAIAV1V2_AR$pidn
MAIAV1V2_AR$label <- as.character(MAIAV1V2_AR$label)
MAIAV1V2_AR$label[19:36] <- ""
MAIAV1V2_ARPlot <- ggplot(MAIAV1V2_AR,aes(x=variable, y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="Multidimensional Assessment of Interoceptive Awareness Auto-Regulation", y="MAIA AR AutoTotal Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 5.25, size=5, family = "American Typewriter")
MAIAV1V2_ARPlot + theme(plot.title=element_text(size=20, 
                                                face="bold", 
                                                family="American Typewriter",
                                                color="black",
                                                hjust=0.5,
                                                lineheight=1.2),  # title
                        axis.title.x=element_text(size=15, 
                                                  face="bold", 
                                                  family="American Typewriter",
                                                  color="black",
                                                  hjust=0.5,
                                                  lineheight=1.2),  # title
                        axis.text.x = element_text(size=12, 
                                                   face="bold", 
                                                   family="Palatino",
                                                   color="black",
                                                   hjust=0.5,
                                                   lineheight=1.2),  # title
                        axis.title.y=element_text(size=15, 
                                                  face="bold", 
                                                  family="American Typewriter",
                                                  color="black",
                                                  hjust=0.5,
                                                  lineheight=1.2),  # title
                        axis.text.y = element_text(size=12, 
                                                   family="Palatino",
                                                   color="black",
                                                   hjust=0.5,
                                                   lineheight=1.2),  # title
                        plot.subtitle=element_text(size=15, 
                                                   family="American Typewriter",
                                                   face="bold",
                                                   hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                              vjust = 0,
                                                                                                                                                                                                                                                                                              hjust = 0.1,
                                                                                                                                                                                                                                                                                              box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                              point.padding = 0.25,
                                                                                                                                                                                                                                                                                              nudge_y = 0.2,
                                                                                                                                                                                                                                                                                              direction = "x",
                                                                                                                                                                                                                                                                                              segment.color = 'grey50') 


PAWBV1V2_S$label = PAWBV1V2_S$pidn
PAWBV1V2_S$label <- as.character(PAWBV1V2_S$label)
PAWBV1V2_S$label[19:36] <- ""
PAWBV1V2_SPlot <- ggplot(na.omit(PAWBV1V2_S),aes(x=variable, y=value)) + geom_line(aes(color=pidn, group=pidn)) + labs(title="Positive Affect and Well-Being", y="PAWB AutoTotal Scores", x="V1-V2 Timepoints") + stat_compare_means(method="t.test", paired = TRUE, aes(label = paste0("p = ", ..p.format.., digits = 6)), label.x = 2.2, label.y = 46, size=5, family = "American Typewriter")
PAWBV1V2_SPlot + theme(plot.title=element_text(size=20, 
                                               face="bold", 
                                               family="American Typewriter",
                                               color="black",
                                               hjust=0.5,
                                               lineheight=1.2),  # title
                       axis.title.x=element_text(size=15, 
                                                 face="bold", 
                                                 family="American Typewriter",
                                                 color="black",
                                                 hjust=0.5,
                                                 lineheight=1.2),  # title
                       axis.text.x = element_text(size=12, 
                                                  face="bold", 
                                                  family="Palatino",
                                                  color="black",
                                                  hjust=0.5,
                                                  lineheight=1.2),  # title
                       axis.title.y=element_text(size=15, 
                                                 face="bold", 
                                                 family="American Typewriter",
                                                 color="black",
                                                 hjust=0.5,
                                                 lineheight=1.2),  # title
                       axis.text.y = element_text(size=12, 
                                                  family="Palatino",
                                                  color="black",
                                                  hjust=0.5,
                                                  lineheight=1.2),  # title
                       plot.subtitle=element_text(size=15, 
                                                  family="American Typewriter",
                                                  face="bold",
                                                  hjust=0.5))  +  scale_color_discrete(name="PLIE Participants") + scale_size_continuous(name = "Density", guide = F) + theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black")) + geom_label_repel(aes(label = label),
                                                                                                                                                                                                                                                                                             vjust = 0,
                                                                                                                                                                                                                                                                                             hjust = 0,
                                                                                                                                                                                                                                                                                             box.padding   = 0.35, 
                                                                                                                                                                                                                                                                                             point.padding = 0.25,
                                                                                                                                                                                                                                                                                             nudge_y = 1.2,
                                                                                                                                                                                                                                                                                             direction = "x",
                                                                                                                                                                                                                                                                                             segment.color = 'grey50') 



####Plot all on one page
ggarrange(HP_C, Insula_C,Temporal_C,Parietal_C,Frontal_C,Occipital_C,SuperiorFrontal_C,CaudalACC_C,SuperiorParietal_C,InferiorParietal_C,Precuneus_C,Supramarginal_C, labels=c("HP","Insula","Temporal","Parietal","Frontal","Occipital","SuperiorFrontal","CaudalACC","SuperiorParietal","InferiorParietal","Precuneus","Supramarginal"),ncol=3, nrow=4)
ggarrange(HP_P, Insula_P,Temporal_P,Parietal_P,Frontal_P,Occipital_P,SuperiorFrontal_P,CaudalACC_P,SuperiorParietal_P,InferiorParietal_P,Precuneus_P,Supramarginal_P, labels=c("HP","Insula","Temporal","Parietal","Frontal","Occipital","SuperiorFrontal","CaudalACC","SuperiorParietal","InferiorParietal","Precuneus","Supramarginal"),ncol=3, nrow=4)
ggarrange(Latency, Accel,Gains,Sacc,Amp.degrees,Prop.smooth,Anisotropy,DirectionTuningasymmetry,DirectionTuningnoise,SpeedTuningresponsiveness,SpeedTuningnoise, labels=c("Latency","Accel","Gains","Sacc","Amp.degrees","Prop.smooth","Anisotropy","DirectionTuningasymmetry","DirectionTuningnoise","SpeedTuningresponsiveness","SpeedTuningnoise"),ncol=3, nrow=4)
ggarrange(ADASPlot, SPPBPlot, QOLPlot, MoodPlot, GaitPlot, labels=c("ADASCOG.AutoTotal", "SPPB.AutoTotal", "QOL.AutoTotal", "Mood_Total", "Gait_ShorterOfTwoTimes"), ncol=2, nrow=3)

ggarrange(HP_C, Insula_C,Temporal_C,Parietal_C,Frontal_C,Occipital_C,SuperiorFrontal_C, labels=c("HP","Insula","Temporal","Parietal","Frontal","Occipital","SuperiorFrontal"),ncol=3, nrow=3)


#####Make graphs of trends?

###Word-Recall & ADASCOG_AutoTotal
###Histogram
qplot(ADASCOGV1V2$Word.recall.auto.mean, geom="histogram", main="Word Recall Total", xlab="Visits", fill=I("blue"), col=I("white"), alpha=I(.2))

####Line plot
ggplot(ADASCOGV1V2, aes(x=pidn, y=Word.recall.auto.mean, group=1)) + geom_line(color="blue") + geom_point()
####Scatterplot no regression line
ggplot(ADASCOGV1V2, aes(x=pidn, y=Word.recall.auto.mean)) + geom_point(color="blue") + geom_smooth(method="lm", se=FALSE, linetype="dashed", color="darkred") + stat_cor()
####Barplot
WordRecall_Total <- ggplot(ADASCOGV1V2, aes(x=pidn, y=Word.recall.auto.mean)) + geom_bar(stat="identity", color="white", fill="steelblue") 
print(WordRecall_Total)

ADASCOG_Total <- ggplot(ADASCOGV1V2, aes(x=pidn, y=ADAS_Cog.AutoTotal)) + geom_bar(stat="identity", color="white", fill="steelblue") 
print(ADASCOG_Total)


####Eye-tracking
ggplot(EyeTracking, aes(x=pidn, y=accel.degrees2)) + geom_point(color="blue") + geom_smooth(method="lm", se=FALSE, linetype="dashed", color="darkred") + stat_cor()

Accel.degrees <- ggplot(EyeTracking, aes(x=pidn, y=accel.degrees2)) + geom_bar(stat="identity", color="white", fill="steelblue") 
print(Accel.degrees)

####SPPB
AutoTotalSPPB_Score <- ggplot(SPPBV1V2, aes(x=pidn, y=AutoTotal.SPPB.Score)) + geom_bar(stat="identity", color="white", fill="steelblue") 
print(AutoTotalSPPB_Score)

ggplot(SPPBV1V2, aes(x=pidn, y=AutoTotal.SPPB.Score, group=1)) + geom_line(color="blue") + geom_point()
ggplot(SPPBV1V2, aes(x=pidn, y=AutoTotal.SPPB.Score)) + geom_point(color="blue") + geom_smooth(method="lm", se=FALSE, linetype="dashed", color="darkred") + stat_cor()

####QOL

QOL_Total <- ggplot(QOLV1V2, aes(x=pidn, y=QOL.AutoTotal)) + geom_bar(stat="identity", color="white", fill="steelblue") 
print(QOL_Total)

####Cerebellum T-Tests
HP_NC<-t.test(CerebellumFinal$HPV1, CerebellumFinal$HPV1, paired=TRUE)
Insula_NC<-t.test(CerebellumFinal$InsulaV1, CerebellumFinal$InsulaV1, paired=TRUE)
Frontal_NC<-t.test(CerebellumFinal$FrontalV1, CerebellumFinal$FrontalV1, paired=TRUE)
Temporal_NC<-t.test(CerebellumFinal$TemporalV1, CerebellumFinal$TemporalV1, paired=TRUE)
Parietal_NC<-t.test(CerebellumFinal$ParietalV1, CerebellumFinal$ParietalV1, paired=TRUE)
Occipital_NC<-t.test(CerebellumFinal$OccipitalV1, CerebellumFinal$OccipitalV1, paired=TRUE)
SuperiorFrontal_NC<-t.test(CerebellumFinal$SuperiorFrontalV1, CerebellumFinal$SuperiorFrontalV1, paired=TRUE)
CaudalACC_NC<-t.test(CerebellumFinal$CaudalACCV1, CerebellumFinal$CaudalACCV1, paired=TRUE)
SuperiorParietal_NC<-t.test(CerebellumFinal$SuperiorParietalV1, CerebellumFinal$SuperiorParietalV1, paired=TRUE)
InferiorParietal_NC<-t.test(CerebellumFinal$InferiorParietalV1, CerebellumFinal$InferiorParietalV1, paired=TRUE)
Precuneus_NC<-t.test(CerebellumFinal$PrecuneusV1, CerebellumFinal$PrecuneusV1, paired=TRUE)
Supramarginal_NC<-t.test(CerebellumFinal$SupramarginalV1, CerebellumFinal$SupramarginalV1, paired=TRUE)

####Write Cerebellum T-tests to table R
CerebellumTtests<- map_df(list(HP_NC,Insula_NC,Frontal_NC,Temporal_NC,Parietal_NC,Occipital_NC,SuperiorFrontal_NC,CaudalACC_NC,SuperiorParietal_NC,InferiorParietal_NC,Precuneus_NC,Supramarginal_NC), tidy)
CerebellumTtests[c("estimate", "statistic", "p.value", "conf.low", "conf.high")]
CerebellumTtests$Regions <- NA
CerebellumTtests <- CerebellumTtests[, c("Regions", "estimate", "statistic", "p.value", "parameter", "conf.low", "conf.high", "method", "alternative")]
CerebellumTtests[1, 1] <- "HP"
CerebellumTtests[2, 1] <- "Insula"
CerebellumTtests[3, 1] <- "Frontal"
CerebellumTtests[4, 1] <- "Temporal"
CerebellumTtests[5, 1] <- "Parietal"
CerebellumTtests[6, 1] <- "Occipital"
CerebellumTtests[7, 1] <- "SuperiorFrontal"
CerebellumTtests[8, 1] <- "CaudalACC"
CerebellumTtests[9, 1] <- "SuperiorPariteal"
CerebellumTtests[10, 1] <- "InferiorParietal"
CerebellumTtests[11, 1] <- "Precuneus"
CerebellumTtests[12, 1] <- "Supramarginal"

####Precentral T-Tests
HP_NP<-t.test(PrecentralFinal$HPV1, PrecentralFinal$HPV1, paired=TRUE)
Insula_NP<-t.test(PrecentralFinal$InsulaV1, PrecentralFinal$InsulaV1, paired=TRUE)
Frontal_NP<-t.test(PrecentralFinal$FrontalV1, PrecentralFinal$FrontalV1, paired=TRUE)
Temporal_NP<-t.test(PrecentralFinal$TemporalV1, PrecentralFinal$TemporalV1, paired=TRUE)
Parietal_NP<-t.test(PrecentralFinal$ParietalV1, PrecentralFinal$ParietalV1, paired=TRUE)
Occipital_NP<-t.test(PrecentralFinal$OccipitalV1, PrecentralFinal$OccipitalV1, paired=TRUE)
SuperiorFrontal_NP<-t.test(PrecentralFinal$SuperiorFrontalV1, PrecentralFinal$SuperiorFrontalV1, paired=TRUE)
CaudalACC_NP<-t.test(PrecentralFinal$CaudalACCV1, PrecentralFinal$CaudalACCV1, paired=TRUE)
SuperiorParietal_NP<-t.test(PrecentralFinal$SuperiorParietalV1, PrecentralFinal$SuperiorParietalV1, paired=TRUE)
InferiorParietal_NP<-t.test(PrecentralFinal$InferiorParietalV1, PrecentralFinal$InferiorParietalV1, paired=TRUE)
Precuneus_NP<-t.test(PrecentralFinal$PrecuneusV1, PrecentralFinal$PrecuneusV1, paired=TRUE)
Supramarginal_NP<-t.test(PrecentralFinal$SupramarginalV1, PrecentralFinal$SupramarginalV1, paired=TRUE)

###Write Precentral T-tests to table
PrecentralTtests<- map_df(list(HP_NP,Insula_NP,Frontal_NP,Temporal_NP,Parietal_NP,Occipital_NP,SuperiorFrontal_NP,CaudalACC_NP,SuperiorParietal_NP,InferiorParietal_NP,Precuneus_NP,Supramarginal_NP), tidy)
PrecentralTtests[c("estimate", "statistic", "p.value", "conf.low", "conf.high")]
PrecentralTtests$Regions <- NA
PrecentralTtests <- PrecentralTtests[, c("Regions", "estimate", "statistic", "p.value", "parameter", "conf.low", "conf.high", "method", "alternative")]
PrecentralTtests[1, 1] <- "HP"
PrecentralTtests[2, 1] <- "Insula"
PrecentralTtests[3, 1] <- "Frontal"
PrecentralTtests[4, 1] <- "Temporal"
PrecentralTtests[5, 1] <- "Parietal"
PrecentralTtests[6, 1] <- "Occipital"
PrecentralTtests[7, 1] <- "SuperiorFrontal"
PrecentralTtests[8, 1] <- "CaudalACC"
PrecentralTtests[9, 1] <- "SuperiorPariteal"
PrecentralTtests[10, 1] <- "InferiorParietal"
PrecentralTtests[11, 1] <- "Precuneus"
PrecentralTtests[12, 1] <- "Supramarginal"


#########Multidimensional Scaling #############
AccessDataV1 <- subset(AccessDataV1, select = c(ADAS_Cog.AutoTotal, QOL.AutoTotal, Mood_AutoTotal, MAIA_AR_AutoMean, MAIA_SR_AutoMean, shorter.of.two.times..sec, AutoTotal.SPPB.Score, Mobility_AutoTotal, Isolation_AutoTotal, PAWB_AutoTotal))
AccessDataV1_SM <- AccessDataV1[complete.cases(AccessDataV1), ]
AccessDataV1_SM <- as.data.frame(t(AccessDataV1_SM))
d <- dist(AccessDataV1_SM) # euclidean distances between the rows
mds.cmdscale       <- as.data.frame(cmdscale(as.matrix(d)))
mds.cmdscale$names <- rownames(mds.cmdscale)
mds.cmdscale$types <- c("ADAS_Cog","QOL","Mood",
                        "MAIA_AR","MAIA_SR", "Gait",
                        "SPPB","Mobility","Isolation",
                        "PAWB")

ggplot(mds.cmdscale, aes(V1, V2, label=types)) +
  geom_point(aes(fill=names),colour="black",pch=21, size=10) +
  labs(title="V1 Assessment Measures", x="", y="") + theme_minimal() +
  theme(legend.position="none", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  geom_text(aes(colour=factor(names)), size=7.0,
            hjust = "center", vjust = "bottom", nudge_x = 0.5, nudge_y = 0.5)

AccessDataV2 <- subset(AccessDataV2, select = c(ADAS_Cog.AutoTotal, QOL.AutoTotal, Mood_AutoTotal, MAIA_AR_AutoMean, MAIA_SR_AutoMean, shorter.of.two.times..sec, AutoTotal.SPPB.Score, Mobility_AutoTotal, Isolation_AutoTotal, PAWB_AutoTotal))
AccessDataV2_SM <- AccessDataV2[complete.cases(AccessDataV2), ]
AccessDataV2_SM <- as.data.frame(t(AccessDataV2_SM))
d <- dist(AccessDataV2_SM) # euclidean distances between the rows
mds.cmdscale       <- as.data.frame(cmdscale(as.matrix(d)))
mds.cmdscale$names <- rownames(mds.cmdscale)
mds.cmdscale$types <- c("ADAS_Cog","QOL","Mood",
                        "MAIA_AR","MAIA_SR", "Gait",
                        "SPPB","Mobility","Isolation",
                        "PAWB")

ggplot(mds.cmdscale, aes(V1, V2, label=types)) +
  geom_point(aes(fill=names),colour="black",pch=21, size=10) +
  labs(title="V2 Assessment Measures", x="", y="") + theme_minimal() +
  theme(legend.position="none", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  geom_text(aes(colour=factor(names)), size=7.0,
            hjust = "center", vjust = "bottom", nudge_x = 0.5, nudge_y = 0.5)

############ Can we do the participants?
AccessDataV1 <- subset(AccessDataV1, select = c(ADAS_Cog.AutoTotal, QOL.AutoTotal, Mood_AutoTotal, MAIA_AR_AutoMean, MAIA_SR_AutoMean, shorter.of.two.times..sec, AutoTotal.SPPB.Score, Mobility_AutoTotal, Isolation_AutoTotal, PAWB_AutoTotal))
AccessDataV1_PIDN <- AccessDataV1[complete.cases(AccessDataV1), ]
d <- dist(AccessDataV1_PIDN) # euclidean distances between the rows
mds.cmdscale       <- as.data.frame(cmdscale(as.matrix(d)))
mds.cmdscale$names <- rownames(mds.cmdscale)
mds.cmdscale$types <- c("716","718","719",
                        "721","724", "725",
                        "726","728","730",
                        "731", "732", "733")

ggplot(mds.cmdscale, aes(V1, V2, label=types)) +
  geom_point(aes(fill=names),colour="black",pch=21, size=10) +
  labs(title="Participants V1 Visit", x="", y="") + theme_minimal() +
  theme(legend.position="none", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  geom_text(aes(colour=factor(names)), size=7.0,
            hjust = "left", vjust = "bottom", nudge_x = 0.5, nudge_y = 0.005)

AccessDataV2 <- subset(AccessDataV2, select = c(ADAS_Cog.AutoTotal, QOL.AutoTotal, Mood_AutoTotal, MAIA_AR_AutoMean, MAIA_SR_AutoMean, shorter.of.two.times..sec, AutoTotal.SPPB.Score, Mobility_AutoTotal, Isolation_AutoTotal, PAWB_AutoTotal))
AccessDataV2_PIDN <- AccessDataV2[complete.cases(AccessDataV2), ]
d <- dist(AccessDataV2_PIDN) # euclidean distances between the rows
mds.cmdscale       <- as.data.frame(cmdscale(as.matrix(d)))
mds.cmdscale$names <- rownames(mds.cmdscale)
mds.cmdscale$types <- c("716","718","719",
                        "721","724", "725",
                        "726","728","730",
                        "731", "732", "733")

ggplot(mds.cmdscale, aes(V1, V2, label=types)) +
  geom_point(aes(fill=names),colour="black",pch=21, size=10) +
  labs(title="Participants V2 Visit", x="", y="") + theme_minimal() +
  theme(legend.position="none", panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  geom_text(aes(colour=factor(names)), size=7.0,
            hjust = "left", vjust = "bottom", nudge_x = 0.5, nudge_y = 0.005)

############## PLIE Experience ###############
#PLIE_Experience <- subset(AccessData, select=c(pidn,VisitNumber,PLIE.Experience.IsolationID,PLIE.Experience.VisitID,PLIE.Experience.Notes,PLIE_Experience1,PLIE_Experience2,PLIE_Experience3,PLIE_Experience4,PLIE_Experience5,PLIE_Experience6,PLIE_Experience7,PLIE_Experience8))
PLIE_Experience <- subset(PLIE_Experience, select=c(pidn, PLIE_Experience1,PLIE_Experience2,PLIE_Experience3,PLIE_Experience4,PLIE_Experience5,PLIE_Experience6,PLIE_Experience7,PLIE_Experience8))
PLIE_Experience <- PLIE_Experience[complete.cases(PLIE_Experience), ]