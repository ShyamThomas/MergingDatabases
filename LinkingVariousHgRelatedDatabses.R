
library(dplyr)
library(corrplot)
library(PerformanceAnalytics)

setwd("C:/Users/thoma/Rprojects/Databases")

#######################################################MERGING DATABASE WIITHOUT AGE INFORMATION#########################################
#Read all the disparate sources of databases

Hg_Data_allyears=read.csv("Hg_data_1970_Mar2016.csv") #The original HG database with fewer columns from Sayendra (OMECP)
head(Hg_Data_allyears)
length(unique(Hg_Data_allyears$SAMPLE_DATE)) #get a count

BSM_WBC_LUT=read.csv("WBC_BSM_LUT.csv") # A look up table linking Waterbody code with BsM Lake Ids ("WATERBODY_LID")
names(BSM_WBC_LUT)[2]="WATERBODY_LID" ##Ensure the varaible used as the common factor in merging databases have same name
head(BSM_WBC_LUT)

###First join Hg_Data_allyears with BSM_WBC_LUT using "WATERBODY_CODE" as the joining variable
Hg_BSM_innerjoin=inner_join(BSM_WBC_LUT,Hg_Data_allyears, by="WATERBODY_CODE") #Joining the above tables
write.csv(Hg_BSM_innerjoin, "HG_BSM_LUT_ALLYEARS.csv")

WATER_CHEM=read.csv("WaterChemLakeStatistics.csv") # All the waterchemistry variables from BsM
names(WATER_CHEM)[4]="WATERBODY_LID"
head(WATER_CHEM)

### Need to get an estimate of all the missing values in Waterchemistry database
na_count = sapply(WATER_CHEM, function(WATER_CHEM) sum(length(which(is.na(WATER_CHEM)))))
na_count = data.frame(na_count)
na_count
                  
## New Waterchem database without NA's                  
WaterChem_nona=na.omit(WATER_CHEM)
tail(WaterChem_nona)
length(WaterChem_nona$WATERBODY_LID)
                  
### Second join between Hg_BSM_innerjoin and Waterchem_nona                  
WaterChemNoNa_HGBSM=inner_join(WaterChem_nona, Hg_BSM_innerjoin, by = "WATERBODY_LID")
tail(WaterChemNoNa_HGBSM)
length(WaterChemNoNa_HGBSM$WATERBODY_LID)
write.csv(WaterChemNoNa_HGBSM,"WaterChem_nonaInnerJoined_HGBSM.csv")
                  
### Subset the above joint database for years 2008-2012; first BSM cycle
WaterChemNoNa_HGBSM.ss=subset(WaterChemNoNa_HGBSM,WaterChemNoNa_HGBSM$SAMPLE_YEAR>2007 & WaterChemNoNa_HGBSM$SAMPLE_YEAR<2013)
head(WaterChemNoNa_HGBSM.ss)
min(WaterChemNoNa_HGBSM.ss$SAMPLE_YEAR)
max(WaterChemNoNa_HGBSM.ss$SAMPLE_YEAR)
length(unique(WaterChemNoNa_HGBSM.ss$WATERBODY_LID))
write.csv(WaterChemNoNa_HGBSM.ss,"WaterChem_nonaInnerJoined_HGBSM_2008to2012.csv")
                  
### Joining in BSM dissolved oygen database to above database
BSM_DO_min=read.csv("./BSMdata/BSM_DO.csv")
BSM_DO_min=na.omit(BSM_D0)
head(BSM_DO_min)
HgWaterChem_DO_innerjoin=inner_join(BSM_DO_min,HGBSM_WATERCHEM_2008TO2012, by="WATERBODY_LID")
tail(HgWaterChem_DO_innerjoin)
HG_BSM_WATERCHEM_DO=write.csv(HgWaterChem_DO_innerjoin)
                  
### Join in Lake specific features which includes lake strahler and shreve estimates
BSM_LAKE_CHARS=read.csv("BSM_Lake_chars.csv") #Lake characterstics including size, lake order; shreve; GDD
names(BSM_LAKE_CHARS)[1]="WATERBODY_LID"
head(BSM_LAKE_CHARS)
HgWaterChem_DO_LakeCharsinnerjoin=inner_join(HG_WATERCHEM_DO,BSM_LAKE_CHARS, by="WATERBODY_LID")
head(HgWaterChem_DO_LakeCharsinnerjoin)
length(unique(HgWaterChem_DO_LakeCharsinnerjoin$WATERBODY_LID))
write.csv(HgWaterChem_DO_LakeCharsinnerjoin, "HG_WATERCHERM_DO_LAKECHAR.csv")
                  
##Merging Lakeshed LULC from ArcMap shapefile dbf to HG_WATERCHEM_DO_LAKECHAR database
LakeshedsLulc=read.csv("lakeshed_lulc.csv")
head(LakeshedsLulc)

LakeshedLULC.tbl=as.table(as.matrix(LakeshedLULC[,-c(1:4)])) ## Get rid of first few columns including water as its lakesize
LakeshedLULC.prpn2=prop.table(LakeshedLULC.tbl,1) #make a proportion table of all the LULC variables for each lakeshed
head(LakeshedLULC.prpn2)
chart.Correlation(LakeshedLULC.prpn2, pch=21)
write.csv(LakeshedLULC.prpn2, "LakeshedLULC.prpn2.csv")

LakeshedLULC=read.csv("LakeshedLULC.prpn2.csv")
head(LakeshedLULC)
                 
##Lets merge LULC with previous database
HgWaterChemDOLakeChars_LULCinnerjoin=inner_join(HgWaterChem_DO_LakeCharsinnerjoin,LakeshedLULC, by="WATERBODY_LID") 
length(HgWaterChemDOLakeChars_LULCinnerjoin$WATERBODY_LID)
length(unique(HgWaterChemDOLakeChars_LULCinnerjoin$WATERBODY_LID))
head(HgWaterChemDOLakeChars_LULCinnerjoin)
write.csv(HgWaterChemDOLakeChars_LULCinnerjoin,"HG_WATERCHEM_DO_LAKECHAR_LULC.csv")

###############################################################################################################################
                                        ##Read the Age database##
                  
HG_AGE_BSM=read.csv("Age_Hg_linked.csv") # The long-running mercury database with age included from Stephanie
head(HG_AGE_BSM)
HG_AGE_BSM.SS=HG_AGE_BSM[,-c(5,10,12,14,20:24)] #A subset of above database with fewer columns

colnames=c("WATERBODY_cODE", "LAT", "LONG", "SPECIES", "SAMPLE_DATE", "NETTING_START_DATE","NETTING_END_DATE", "LENGTH", "WEIGHT",
           "SEX", "MERCURY", "AGE_STR", "AGE", "AGE_CONFIDENCE", "WATERBODY_LID")

colnames(HG_AGE_BSM.SS)=colnames
head(HG_AGE_BSM.SS)
                  
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
