
library(dplyr)
library(corrplot)
library(PerformanceAnalytics)

setwd("C:/Users/thoma/Rprojects/Databases")

#First read all the databases

HG_AGE_BSM=read.csv("Age_Hg_linked.csv") # The long-running mercury database with age included from Stephanie
head(HG_AGE_BSM)
HG_AGE_BSM.SS=HG_AGE_BSM[,-c(5,10,12,14,20:24)] #A subset of above database with fewer columns

colnames=c("WATERBODY_cODE", "LAT", "LONG", "SPECIES", "SAMPLE_DATE", "NETTING_START_DATE","NETTING_END_DATE", "LENGTH", "WEIGHT",
           "SEX", "MERCURY", "AGE_STR", "AGE", "AGE_CONFIDENCE", "WATERBODY_LID")

colnames(HG_AGE_BSM.SS)=colnames
head(HG_AGE_BSM.SS)

BSM_WBC_LUT=read.csv("WBC_BSM_LUT.csv") # A look up table linking Waterbody code with BsM Lake Ids
names(BSM_WBC_LUT)[2]="WATERBODY_LID" ##Ensure the varaible used as the common factor in merging databases have same name
head(BSM_WBC_LUT)

BSM_LAKE_CHARS=read.csv("BSM_Lake_chars.csv") #Lake characterstics including size, lake order
names(BSM_LAKE_CHARS)[1]="WATERBODY_LID"
head(BSM_LAKE_CHARS)

WATER_CHEM=read.csv("WaterChemLakeStatistics.csv") # All the waterchemistry variables from BsM
names(WATER_CHEM)[4]="WATERBODY_LID"
head(WATER_CHEM)

####################################################################################
## First Merge: Waterchem variables with Lake variables
WATERCHEM2BSMLAKECHAR=merge(x=WATER_CHEM, y = BSM_LAKE_CHARS, by = "WATERBODY_LID")
head(WATERCHEM2BSMLAKECHAR)
length(WATERCHEM2BSMLAKECHAR[,1])

## Second merge Above merged table with fish mercury database
HG_AGE_BSM2WATERCHEMLAKECHARS=merge(x=WATERCHEM2BSMLAKECHAR, y = HG_AGE_BSM.SS, by = "WATERBODY_LID")
length(HG_AGE_BSM2WATERCHEMLAKECHARS[,1])
head(HG_AGE_BSM2WATERCHEMLAKECHARS)

## Finally capture lake-level meterics for all variables including fish data
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

AGG_HG_AGE2WATERCHEMLAKECAHARS.DF=as.data.frame(AGG_HG_AGE2WATERCHEMLAKECHARS) # convert to dataframe
head(AGG_HG_AGE2WATERCHEMLAKECAHARS.DF)

AGG_HG_AGE2WATERCHEMLAKECAHARS.DF_BSMLUT=merge(x=AGG_HG_AGE2WATERCHEMLAKECAHARS.DF, y = BSM_WBC_LUT, by = "WATERBODY_LID")
tail(AGG_HG_AGE2WATERCHEMLAKECAHARS.DF_BSMLUT)
length(unique(AGG_HG_AGE2WATERCHEMLAKECAHARS.DF_BSMLUT$WATERBODY_LID))

write.csv(AGG_HG_AGE2WATERCHEMLAKECAHARS.DF, "HG_LAKE_WATERCHEM.csv")
write.csv(AGG_HG_AGE2WATERCHEMLAKECAHARS.DF_BSMLUT, "HG_LAKE_WATERCHEM_latlong.csv")

merged_data=read.csv("MergingDatabases/Mercury_Lake_Waterchem_merged.csv")

png("CorrelationPlot-LakeWaterChemVars.png", width=5, height = 5, units = "in", res = 600)
merged.correlation=chart.Correlation(merged_data[,c(-1)], histogram=TRUE, pch=19)
dev.off()
