
library(dplyr)
library(corrplot)
library(PerformanceAnalytics)

setwd("C:/Users/thoma/Rprojects/Databases/BSMdata/BsM_data")

BSM_WBC_LUT=read.csv("WBC_BSM_LUT.csv")  #Broadscale Monitoring data (BsM) linked to WaterbodyCodes look up table
head(BSM_WBC_LUT)

HG_AGE_BSM=read.csv("Age_linked_all.csv")  #Mercury database linked with fish age database by Stephanie
head(HG_AGE_BSM)

HG_AGE_BSM.SS=HG_AGE_BSM[,-c(5,10,12,14,20:24)] #A subset of above database with fewer columns

colnames=c("WATERBODY_cODE", "LAT", "LONG", "SPECIES", "SAMPLE_DATE", "NETTING_START_DATE","NETTING_END_DATE", "LENGTH", "WEIGHT",
           "SEX", "MERCURY", "AGE_STR", "AGE", "AGE_CONFIDENCE", "WATERBODY_LID")

colnames(HG_AGE_BSM.SS)=colnames
head(HG_AGE_BSM.SS)


WATER_CHEM=read.csv("WaterChemLakeStatistics.csv")
head(WATER_CHEM)
names(WATER_CHEM)[4]="WATERBODY_LID"  ##Ensure the varaible used as the common factor in merging databases have same name


head(WATER_CHEM)
head(HG_AGE_BSM.SS)
HG_AGE_BSM2WATERCHEM=merge(x=WATER_CHEM, y = HG_AGE_BSM.SS, by = "WATERBODY_LID") ##Merging the databases
head(HG_AGE_BSM2WATERCHEM)


##calculate lake-level average measures of fish variables and water-chemistry variables
##use dplyr functions to summarise by waterbody ID's

AGG_HG_AGE2WATERCHEM=HG_AGE_BSM2WATERCHEM %>%
 na.omit %>%  
  group_by(WATERBODY_LID) %>%
   summarise(
     LAT=min(LAT),
     LONG= min(LONG),
  
     avgHG=mean(MERCURY),
     samples=length(MERCURY),
     avgLength=mean(LENGTH),
     avgWeight=mean(WEIGHT),

     avgDIC=mean(DIC),             ## Disssolved Inorganic Carbon
     avgDOC=mean(DOC),             ## Disssolved Organic Carbon
     avgSO4=mean(SSO4UR),          ## Is this sulphate levels?
     avgDEPTH=mean(Secchi_Depth), 
     avgPH=mean(pH)
      )

AGG_HG_AGE2WATERCHEM.DF=as.data.frame(AGG_HG_AGE2WATERCHEM) ## tibble to dataframe
head(AGG_HG_AGE2WATERCHEM.DF)

##Lots of missing lakes is evident in the merged lake database, I shall try to catch the missing lakes; just to double check!!
HG_AGE_BSM.SS.UNIQ=(unique(HG_AGE_BSM.SS$WATERBODY_LID))
MISSING_HG_AGE_BSM_LAKES=HG_AGE_BSM.SS.UNIQ [!(HG_AGE_BSM.SS.UNIQ%in%AGG_HG_AGE2WATERCHEM$WATERBODY_LID)]
head(MISSING_HG_AGE_BSM_LAKES)

##OKAY!! I get it... Lots of missing variable records (seems to be mostly PH values) from Lake WaterChem database renders the total number of 
##lakes with full data to mere 227!! Ugh!! :(

HG_AGE_BSM2WATERCHEM.naomit=na.omit(HG_AGE_BSM2WATERCHEM)
head(HG_AGE_BSM2WATERCHEM.naomit)
HG_AGE_BSM2WATERCHEM.naomit.UNIQ=unique(HG_AGE_BSM2WATERCHEM.naomit$WATERBODY_LID)
length(HG_AGE_BSM2WATERCHEM.naomit.UNIQ)


agg.ss=AGG_HG_AGE2WATERCHEM.DF[,-c(1)] ## subset the dataframe to remove non-numeric columns
head(agg.ss)
corr.mx=cor(agg.ss)                   ## correlation matrix
corrplot(corr.mx, method = "number")  ## fancy ploting of correlation matrix



#################################################################################################################################################################
#################################################################################################################################################################

BSM_WBC_LUT=read.csv("WBC_BSM_LUT.csv") 
names(BSM_WBC_LUT)[2]="WATERBODY_LID" ##Ensure the varaible used as the common factor in merging databases have same name

BSM_LAKE_CHARS=read.csv("BSM_Lake_chars.csv")
names(BSM_LAKE_CHARS)[1]="WATERBODY_LID"

WATER_CHEM=read.csv("WaterChemLakeStatistics.csv")
names(WATER_CHEM)[4]="WATERBODY_LID"  

HG_AGE_BSM=read.csv("Age_Hg_linked.csv")

WATERCHEM2BSMWBCLUT=merge(x=WATER_CHEM, y = BSM_WBC_LUT, by = "WATERBODY_LID")

HG_AGE_BSM2WATERCHEMLAKECHARS=merge(x=WATERCHEM2BSMWBCLUT, y = HG_AGE_BSM.SS, by = "WATERBODY_LID")

AGG_HG_AGE2WATERCHEMLAKECHARS=HG_AGE_BSM2WATERCHEMLAKECHARS %>%
  na.omit %>%  
  group_by(WATERBODY_LID) %>%
  summarise(
    LAT=max(LAT),
    LONG= max(LONG),
    
    avgHG=mean(MERCURY),
    samples=length(MERCURY),
    avgLength=mean(LENGTH),
    avgWeight=mean(WEIGHT),
    
    LakeOrder=mean(Strahler),
    LakeArea=mean(AreaCalcm2),
    
    avgDIC=mean(DIC),             ## Disssolved Inorganic Carbon
    avgDOC=mean(DOC),             ## Disssolved Organic Carbon
    avgSO4=mean(SSO4UR),          ## Is this sulphate levels?
    avgDEPTH=mean(Secchi_Depth), 
    avgPH=mean(pH)
  )

AGG_HG_AGE2WATERCHEMLAKECAHARS.DF=as.data.frame(AGG_HG_AGE2WATERCHEMLAKECHARS)
head(AGG_HG_AGE2WATERCHEMLAKECAHARS.DF)

