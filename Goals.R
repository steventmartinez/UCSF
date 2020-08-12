################################## Data Formatting Script for Anna, Sky, & Brian ###########################

###There are certain "packages" you want to install and load in order to play with the data in R

###To install a package
    #install.packages("Name of package here")
#Example: install.packages("ggplot2")

###To load a package in R
    #library("Name of package here")
#Example: library("ggplot2")

#####General helpful commands to know

#To remove the data environment: rm(list=ls())
#To reset the graph environment: dev.off()
#To export a data frame to a csv file: write.csv(mydata, "mydata.csv") 
#Change 0s to NA: dataframe[dataframe == 0] <- NA
#Change NAs to 0: dataframe[is.na(dataframe)] <- 0
#To add a suffix to column name: colnames(dataframe) <- paste(colnames(dataframe), "V1", sep = "")

####These are the packages you'll need for this script
##You will need to do install.packages("") for each of these packages, but after you do it once, you won't have to do it anymore, as it will already be installed on your R system. That's why you don't see those commands in the script anymore
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
library(cowplot)
library(gridExtra)

###You need to set your directory in order to pull the data from the appropriate folder on your computer
##Here, we use datadir and outputDir as variables to pull our data
##Before you do this, you'll need to create a folder called "output" and store it in your file pathway
#dataDir <- "B:/shared/GOALS/"
#outputDir <- "B:/shared/GOALS/output"

dataDir <- "/Users/stevenmartinez/Desktop/SFVA/PLIE/"
outputDir <- "/Users/stevenmartinez/Desktop/SFVA/PLIE/output"

###Here, we read the data file into R. 
#na.strings will turn any blanks into NAs, which makes it cleaner/easier for any analyses in R
#stringAsFactors will help you convert the data into numeric after you read in the data
CIND<-read.csv(paste(dataDir,"CIND_Unfiltered_LongitudinalFreeSurfer_SubcorticalVolume.csv",sep=""), na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

###Transpose the data first
##This will help us subset the data 
CIND_Transposed <- as.data.frame(t(CIND))

###Here, I'm copying the column names from the 2nd row values, as indicated by (Cind_Transposed[2,]))
colnames(CIND_Transposed) <- as.character(unlist(CIND_Transposed[2,]))

###Subsetting. Here, I'm splitting up the time points, by taking participants from either timepoint 1 OR timepoint 2, and putting each participant in its respective data frame
GOALSV1 <- subset(CIND_Transposed, select=c("GOALS9001-1a5","GOALS9002-1a5","GOALS9003-1a5","GOALS9004-1a5","GOALS9005-1a5","GOALS9006-1a5","GOALS9007-1a5","GOALS9008-1a5","GOALS9010-1a5","GOALS9011-1a5","GOALS9013-1a5","GOALS9014-1a5","GOALS9016-1a5","GOALS9017-1a5","GOALS9019-1a5","GOALS9020-1a5","GOALS9022-1a5","GOALS9026-1a5","GOALS9029-1a5","GOALS9030-1a5","GOALS9031-1a5","GOALS9032-1a5","GOALS9034-1a5","GOALS9037-1a5"))
GOALSV2 <- subset(CIND_Transposed, select=c("GOALS9001-2a5","GOALS9002-2a5","GOALS9003-2a5","GOALS9004-2a5","GOALS9005-2a5","GOALS9006-2a5","GOALS9007-2a5","GOALS9008-2a5","GOALS9010-2a5","GOALS9011-2a5","GOALS9013-2a5","GOALS9014-2a5","GOALS9016-2a5","GOALS9017-2a5","GOALS9019-2a5","GOALS9020-2a5","GOALS9022-2a5","GOALS9026-2a5","GOALS9029-2a5","GOALS9030-2a5","GOALS9031-2a5","GOALS9032-2a5","GOALS9034-2a5","GOALS9037-2a5"))

###Transposing the data AGAIN, and adding a suffix of "V1" or "V2" to each column name, to help us distinguish timepoints
GOALSV1 <- as.data.frame(t(GOALSV1))
colnames(GOALSV1) <- paste(colnames(GOALSV1), "V1", sep = "")

GOALSV2 <- as.data.frame(t(GOALSV2))
colnames(GOALSV2) <- paste(colnames(GOALSV2), "V2", sep = "")

###Converting the data to numeric
GOALSV1[] <- lapply(GOALSV1, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
#sapply(GOALSV1, class)

GOALSV2[] <- lapply(GOALSV2, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
#sapply(GOALSV1, class)

####Now, I'm splitting up the brain region timepoints to its respective data frames
GOALSV1 <- subset(GOALSV1, select = c (RightHippocampusV1,LeftHippocampusV1,RightCerebellumCortexV1,LeftCerebellumCortexV1,RightCerebellumWMV1,LeftCerebellumWMV1,CorpusCallosumAnteriorV1,CorpusCallosumMidAnteriorV1,CorpusCallosumCentralV1,CorpusCallosumMidPosteriorV1,CorpusCallosumPosteriorV1,RightCaudateV1,LeftCaudateV1,RightThalamusV1,LeftThalamusV1,RightPutamenV1,LeftPutamenV1,BrainstemV1,RightAccumbensAreaV1,LeftAccumbensAreaV1,RightAmygdalaV1,LeftAmygdalaV1,RightPallidumV1,LeftPallidumV1,RightLateralVentricleV1,LeftLateralVentricleV1,RightInferiorLateralVentricleV1,LeftInferiorLateralVentricleV1,ThirdVentricleV1,FourthVentricleV1,FifthVentricleV1,RightVentralDCV1,LeftVentralDCV1,NonWMHypoIntensitiesV1,RightNonWMHypoIntensitiesV1,LeftNonWMHypoIntensitiesV1,WMHypoIntensitiesV1,RightWMHypoIntensitiesV1,LeftWMHypoIntensitiesV1,CsfV1,RightChoroidPlexusV1,LeftChoroidPlexusV1,RightVesselV1,LeftVesselV1,OpticChiasmV1,RightInteriorV1,LeftInteriorV1,RightUndeterminedV1,LeftUndeterminedV1,ProjectIDV1))
GOALSV2 <- subset(GOALSV2, select = c (RightHippocampusV2,LeftHippocampusV2,RightCerebellumCortexV2,LeftCerebellumCortexV2,RightCerebellumWMV2,LeftCerebellumWMV2,CorpusCallosumAnteriorV2,CorpusCallosumMidAnteriorV2,CorpusCallosumCentralV2,CorpusCallosumMidPosteriorV2,CorpusCallosumPosteriorV2,RightCaudateV2,LeftCaudateV2,RightThalamusV2,LeftThalamusV2,RightPutamenV2,LeftPutamenV2,BrainstemV2,RightAccumbensAreaV2,LeftAccumbensAreaV2,RightAmygdalaV2,LeftAmygdalaV2,RightPallidumV2,LeftPallidumV2,RightLateralVentricleV2,LeftLateralVentricleV2,RightInferiorLateralVentricleV2,LeftInferiorLateralVentricleV2,ThirdVentricleV2,FourthVentricleV2,FifthVentricleV2,RightVentralDCV2,LeftVentralDCV2,NonWMHypoIntensitiesV2,RightNonWMHypoIntensitiesV2,LeftNonWMHypoIntensitiesV2,WMHypoIntensitiesV2,RightWMHypoIntensitiesV2,LeftWMHypoIntensitiesV2,CsfV2,RightChoroidPlexusV2,LeftChoroidPlexusV2,RightVesselV2,LeftVesselV2,OpticChiasmV2,RightInteriorV2,LeftInteriorV2,RightUndeterminedV2,LeftUndeterminedV2,ProjectIDV2))

###Combining the two data frames horizontally
GOALS_Final <- cbind(GOALSV1,GOALSV2)


####Copying the first column that's not really a column and pasting it as the first column so we can extract the participant labels for future analyses!
GOALS_Final$ScanCode <- rownames(GOALS_Final) 
GOALS_Final <- GOALS_Final %>%
  select(ScanCode, everything())
GOALS_Final$ScanCode <- factor(GOALS_Final$ScanCode, levels=unique(GOALS_Final$ScanCode))


###This is the worstttttt part, melting/stacking the data into individual data frames, comparing respsective timepoints vertically. For some reason which I cannot comprehend conceptually, spaghetti slots can only be created from melted data frames.
###When you want to add the third timepoint, you'll need to add it to each of these data frames.
#RightHippocampus <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightHippocampusV1","LeftHippocampusV2","LeftHippocampusV3"))

RightHippocampus <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightHippocampusV1","RightHippocampusV2"))
LeftHippocampus <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftHippocampusV1","LeftHippocampusV2"))
RightCerebellumCortex <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightCerebellumCortexV1","RightCerebellumCortexV2"))
LeftCerebellumCortex <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftCerebellumCortexV1","LeftCerebellumCortexV2"))
RightCerebellumWM <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightCerebellumWMV1","RightCerebellumWMV2"))
LeftCerebellumWM <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftCerebellumWMV1","LeftCerebellumWMV2"))
CorpusCallosumAnterior <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("CorpusCallosumAnteriorV1","CorpusCallosumAnteriorV2"))
CorpusCallosumMidAnterior <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("CorpusCallosumMidAnteriorV1","CorpusCallosumMidAnteriorV2"))
CorpusCallosumCentral <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("CorpusCallosumCentralV1","CorpusCallosumCentralV2"))
CorpusCallosumMidPosterior <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("CorpusCallosumMidPosteriorV1","CorpusCallosumMidPosteriorV2"))
RightCaudate <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightCaudateV1","RightCaudateV2"))
LeftCaudate <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftCaudateV1","LeftCaudateV2"))
RightThalamus <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightThalamusV1","RightThalamusV2"))
LeftThalamus <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftThalamusV1","LeftThalamusV2"))
RightPutamen <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightPutamenV1","RightPutamenV2"))
LeftPutamen <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftPutamenV1","LeftPutamenV2"))
Brainstem <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("BrainstemV1","BrainstemV2"))
RightAccumbensArea <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightAccumbensAreaV1","RightAccumbensAreaV2"))
LeftAccumbensArea <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftAccumbensAreaV1","LeftAccumbensAreaV2"))
RightAmygdala <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightAmygdalaV1","RightAmygdalaV2"))
LeftAmygdala <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftAmygdalaV1","LeftAmygdalaV2"))
RightPallidum <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightPallidumV1","RightPallidumV2"))
LeftPallidum <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftPallidumV1","LeftPallidumV2"))
RightLateralVentricle <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightLateralVentricleV1","RightLateralVentricleV2"))
LeftLateralVentricle <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftLateralVentricleV1","LeftLateralVentricleV2"))
RightInferiorLateralVentricle <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightInferiorLateralVentricleV1","RightInferiorLateralVentricleV2"))
LeftInferiorLateralVentricle <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftInferiorLateralVentricleV1","LeftInferiorLateralVentricleV2"))
ThirdVentricle <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("ThirdVentricleV1","ThirdVentricleV2"))
FourthVentricle <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("FourthVentricleV1","FourthVentricleV2"))
FifthVentricle <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("FifthVentricleV1","FifthVentricleV2"))
RightVentralDC <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightVentralDCV1","RightVentralDCV2"))
LeftVentralDC <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftVentralDCV1","LeftVentralDCV2"))
NonWMHypoIntensities <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("NonWMHypoIntensitiesV1","NonWMHypoIntensitiesV2"))
RightNonWMHypoIntensities <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightNonWMHypoIntensitiesV1","RightNonWMHypoIntensitiesV2"))
LeftNonWMHypoIntensities <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftNonWMHypoIntensitiesV1","LeftNonWMHypoIntensitiesV2"))
WMHypoIntensities <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("WMHypoIntensitiesV1","WMHypoIntensitiesV2"))
RightWMHypoIntensities <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightWMHypoIntensitiesV1","RightWMHypoIntensitiesV2"))
LeftWMHypoIntensities <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftWMHypoIntensitiesV1","LeftWMHypoIntensitiesV2"))
Csf <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("CsfV1","CsfV2"))
RightChoroidPlexus <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightChoroidPlexusV1","RightChoroidPlexusV2"))
LeftChoroidPlexus <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftChoroidPlexusV1","LeftChoroidPlexusV2"))
RightVessel <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightVesselV1","RightVesselV2"))
LeftVessel <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftVesselV1","LeftVesselV2"))
OpticChiasm <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("OpticChiasmV1","OpticChiasmV2"))
RightInterior <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightInteriorV1","RightInteriorV2"))
LeftInterior <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftInteriorV1","LeftInteriorV2"))
RightUndetermined <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("RightUndeterminedV1","RightUndeterminedV2"))
LeftUndetermined <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("LeftUndeterminedV1","LeftUndeterminedV2"))
#ProjectID <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("InferiorParietalV1","InferiorParietalV2"))


###Here, I'm exporting the individual graphs into separate data frames, so you can compare them side-by-side with the next command
ggplot(RightHippocampus,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftHippocampus,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightCerebellumCortex,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftCerebellumCortex,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightCerebellumWM,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftCerebellumWM,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(CorpusCallosumAnterior,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(CorpusCallosumMidAnterior,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(CorpusCallosumCentral,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(CorpusCallosumMidPosterior,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightCaudate,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftCaudate,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightThalamus,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftThalamus,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightPutamen,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftPutamen,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(Brainstem,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightAccumbensArea,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftAccumbensArea,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightAmygdala,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftAmygdala,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightPallidum,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftPallidum,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightLateralVentricle,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftLateralVentricle,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightInferiorLateralVentricle,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftInferiorLateralVentricle,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(ThirdVentricle,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(FourthVentricle,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(FifthVentricle,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightVentralDC,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftVentralDC,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(NonWMHypoIntensities,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightNonWMHypoIntensities,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftNonWMHypoIntensities,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(WMHypoIntensities,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightWMHypoIntensities,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftWMHypoIntensities,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(Csf ,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightChoroidPlexus,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftChoroidPlexus,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightVessel,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftVessel,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(OpticChiasm ,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightInterior,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftInterior,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(RightUndetermined,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
ggplot(LeftUndetermined,aes(x=variable ,y=value)) + geom_line(aes(color=ScanCode, group=ScanCode))
#ProjectID <- melt(GOALS_Final, id.vars="ScanCode", measure.vars=c("InferiorParietalV1","InferiorParietalV2"))

##This is where all your hard work pays off!!
#Here, we're arranging the individual graph side-by-side which you can export to a single PDF
#ggarrange(RightHippocampus,LeftHippocampus,RightCerebellumCortex,LeftCerebellumCortex,labels=c("RightHippocampus","LeftHippocampus","RightCerebellumCortex","LeftCerebellumCortex", ncols=2, nrows=2))
#ggarrange(RightHippocampus,LeftHippocampus,RightCerebellumCortex,LeftCerebellumCortex,RightCerebellumWM,LeftCerebellumWM,CorpusCallosumAnterior,CorpusCallosumMidAnterior,CorpusCallosumCentral,CorpusCallosumMidPosterior,RightCaudate,LeftCaudate,RightThalamus,LeftThalamus,RightPutamen,LeftPutamen,Brainstem, labels=c("RightHippocampus","LeftHippocampus","RightCerebellumCortex","LeftCerebellumCortex","RightCerebellumWM","LeftCerebellumWM","CorpusCallosumAnterior","CorpusCallosumMidAnterior","CorpusCallosumCentral","CorpusCallosumMidPosterior","RightCaudate","LeftCaudate","RightThalamus","LeftThalamus","RightPutamen","LeftPutamen","Brainstem", ncols=5, nrows=5))
#ggarrange(RightHippocampus,LeftHippocampus,RightCerebellumCortex,LeftCerebellumCortex,RightCerebellumWM,LeftCerebellumWM,labels=c("RightHippocampus","LeftHippocampus","RightCerebellumCortex","LeftCerebellumCortex","RightCerebellumWM","LeftCerebellumWM", ncols=5, nrows=2))


###Regression Analysis

