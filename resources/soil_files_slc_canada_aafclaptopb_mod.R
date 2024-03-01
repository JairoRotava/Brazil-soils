# Arquivo original modificado para funcionar no meu ambiente
#Soil_Name and Soil_Layer filoes were pre=processed to replace the operator "$" with "_"
rm (list = ls()) # Clear Global Envirnment
#setwd("C:/Users/cordeirom/Documents/Manuscripts/Man02_Soils_SWAT_Can/Files/")
library("foreign")
library("stringr")
SOIL_NAME_FILE = "./Data/Canada/soil_name_canada_v2r20140529.dbf"
SOIL_LAYER_FILE = "./Data/Canada/soil_layer_canada_v2r20140529.dbf"
SLC_soils <- (read.dbf(SOIL_NAME_FILE))
SN_id_unique <- as.vector(unique(SLC_soils$SOIL_ID))
SN <- read.dbf(SOIL_NAME_FILE)
SL <- read.dbf(SOIL_LAYER_FILE)
SLpositive <- SL[SL$UDEPTH>=0,] #Removes information above the soil surface (i.e., upper depth negative; Leaf layer and organic soils)
SL_id_unique <- as.vector(unique(SLpositive$SOIL_ID))
Missing_SN <- SL_id_unique[!(SL_id_unique %in% SN_id_unique)==TRUE]# Soils in Soil Layer file but not in the Soil Name file
Missing_SL <- SN_id_unique[!(SN_id_unique %in% SL_id_unique)==TRUE]#Soils in the Soil Name  file but not in the Soil Layer file
Incomp <- subset(SLpositive, (TSAND==-9 & TSILT==-9 & TCLAY==-9) | ORGCARB==-9 | KSAT==-9 | BD==-9 | (KP10==-9 & KP33==-9) | KP1500==-9) #Other soils present in the Soil Layer table but with incomplete info.
Incomp <- unique(Incomp$SOIL_ID)
SN_id_unique <- SN_id_unique[!(SN_id_unique %in% Missing_SL)] # Remove soils present in the Soil Name table but missing in the Soil Layer table.
SN_id_unique <- SN_id_unique[!(SN_id_unique %in% Incomp)]
REDROCK <- SLpositive[SLpositive$SOIL_CODE=="RDK",] # Identifies the Redrock soil code, which occurs in NT and does not have any info in the SLT.
REDROCK <- REDROCK$SOIL_ID #Gets soil names in the Redrock class.
SN_id_unique <- SN_id_unique[!(SN_id_unique %in% REDROCK)] # Remove Redrock layers from the unique Soil_ID list.
ROCK <- SL[SL$HZN_MAS=="R",] # identifies layers of rock
SLpositive <- SLpositive[!(SLpositive %in% ROCK)] # Remove rock layers from SLT.
COMP <- matrix(NA,1,131)
COL_NAMES_FILE = "./Data/Canada/colnames_COMP.csv"
Columns <- read.csv(COL_NAMES_FILE, header=FALSE, stringsAsFactors=FALSE)
colnames(COMP) <- Columns
#Loop for creating soil file structure and populating it
for (i in 1:length(SN_id_unique)) {
  Soil <-SN_id_unique[i]
  SN_i <- SN[SN$SOIL_ID==Soil,] #Selecting the entries belonging to the unique soil ID
  SL_i <- SLpositive[SLpositive$SOIL_ID==Soil,]
  df <- matrix(NA,45,10) #Create empty matrix (45 rows; 10 columns)
  CLAY <- matrix(NA,1,10) #Creates a matrix to hold calculations on Clay content, not included in the original code for APEX files.
  MUID <- SN_i[1,1] #Set to SOIL_ID
  SNAM <- SN_i[1,1]#Set to SOIL_ID since it will be used by SWAt to link to GIS layer
  S5ID <- paste("SLC",SN_i[1,2], sep = "")
  DRAIN <- as.character(SN_i[1,11])
  #Populating parameters
  df[2,1]=0.1 #Albedo
  WT <- if (DRAIN=="VP"){
    WT=0
  } else if (DRAIN=="P"){
    WT=25
  } else if (DRAIN=="I"){
    WT=75
  } else if (DRAIN=="MW"){
    WT=100
  } else WT=125
  ImpL_depth <- if (SN_i[SN_i$RESTR_TYPE] %in% c("BN", "CR", "CT", "CY", "DU", "FP", "LI", "OR", "PL", "UN")){
     ImpL_depth = SL_i[SL_i$LAYER_NO==SN_i$ROOTRESTRI,"UDEPTH"]
  } else if (min(SL_i$KSAT)<0.1) {
    ImpL_depth = SL_i[SL_i$KSAT==min(SL_i$KSAT),"UDEPTH"][1]
  } else ImpL_depth = SL_i$LDEPTH[nrow(SL_i)]
  KSATmin_depth <- if ((ImpL_depth>=50 && ImpL_depth<100) && WT<60){
    KSATmin_depth=60
  } else if ((ImpL_depth>=50 && ImpL_depth<100) && WT>=60) {
    KSATmin_depth=50
  } else if (ImpL_depth>=100 && WT<60) {
    KSATmin_depth=100
  } else if (ImpL_depth>=100 && (WT>=60 && WT<100)) {
    KSATmin_depth=50
  } else if (ImpL_depth>=100 && WT>=100) {
    KSATmin_depth=100
  } else KSATmin_depth=100
  SL_i_ksat <- SL_i[SL_i$UDEPTH<= KSATmin_depth,]
  KSATmin <- SL_i_ksat[SL_i_ksat$KSAT==min(SL_i_ksat$KSAT),"KSAT"]*2.77778 # cm/h to um/sec --> multiply by 2.77778
  KSATmin <- KSATmin[1]
  HSG <-  if (ImpL_depth<50) { #Case 1
    HSG=4
  } else if (ImpL_depth>=50 && ImpL_depth<100 && WT<60 && KSATmin> 40 && KSATmin_depth<=60) { #Case 2
    HSG=1
  } else if (ImpL_depth>=50 && ImpL_depth<100 && WT<60 && (KSATmin>10 && KSATmin<=40) && KSATmin_depth<=60) { #Case 3
    HSG=2
  } else if (ImpL_depth>=50 && ImpL_depth<100 && WT<60 && (KSATmin>1 && KSATmin<=10) && KSATmin_depth<=60) { #Case 4
    HSG=3
  } else if (ImpL_depth>=50 && ImpL_depth<100 && WT<60 && KSATmin<= 1 && KSATmin_depth<=60) { #Case 5
    HSG=4
  } else if (ImpL_depth>=50 && ImpL_depth<100 && WT>=60 && KSATmin> 40 && KSATmin_depth<=50) { #Case 6
    HSG=1
  } else if (ImpL_depth>=50 && ImpL_depth<100 && WT>=60 && (KSATmin>10 && KSATmin<=40) && KSATmin_depth<=50) { #Case 7
    HSG=2
  } else if (ImpL_depth>=50 && ImpL_depth<100 && WT>=60 && (KSATmin>1 && KSATmin<=10) && KSATmin_depth<=50) { #Case 8
    HSG=3
  } else if (ImpL_depth>=50 && ImpL_depth<100 && WT>=60 && KSATmin<=1 && KSATmin_depth<=50) { #Case 9
    HSG=4
  } else if (ImpL_depth>=100 && WT<60 && KSATmin> 10 && KSATmin_depth<=100) { #Case 10
    HSG=1
  } else if (ImpL_depth>=100 && WT<60 && (KSATmin>4 && KSATmin<=10) && KSATmin_depth<=100) { #Case 11
    HSG=2
  } else if (ImpL_depth>=100 && WT<60 && (KSATmin>0.4 && KSATmin<=4) && KSATmin_depth<=100) { #Case 12
    HSG=3
  } else if (ImpL_depth>=100 && WT<60 && KSATmin<=0.4 && KSATmin_depth<=100) { #Case 13
    HSG=4
  } else if (ImpL_depth>=100 && (WT>=60 && WT<100) && KSATmin>40 && KSATmin_depth<=50) { #Case 14
    HSG=1
  } else if (ImpL_depth>=100 && (WT>=60 && WT<100) && (KSATmin>10 && KSATmin<=40) && KSATmin_depth<=50) { #Case 15
    HSG=2  
  } else if (ImpL_depth>=100 && (WT>=60 && WT<100) && (KSATmin>1 && KSATmin<=10) && KSATmin_depth<=50) { #Case 16
    HSG=3
  } else if (ImpL_depth>=100 && (WT>=60 && WT<100) && KSATmin<=1 && KSATmin_depth<=50) { #Case 17
    HSG=4
  } else if (WT>=100 && KSATmin>10 && KSATmin_depth<=100) { #Case 18
    HSG=1
  } else if (WT>=100 && (KSATmin >4 && KSATmin<=10) && KSATmin_depth<=100) { #Case 19
    HSG=2
  } else if (WT>=100 && (KSATmin>0.4 && KSATmin<=4) && KSATmin_depth<=100) { #Case 20
    HSG=3
  } else if (WT>=100 && KSATmin<=0.4 && KSATmin_depth<=100) { #Case 21
    HSG=4
  } else HSG=0
  df[2,2]=HSG #Hydrologic soil group. Calculated from Ksat.
  df[2,3]=0.3 #Initial soil content (fraction of field capacity). Arbitrarily defined.
  df[2,4]=0 #WTMN=Minimum depth to water table.Unknown=0.
  df[2,5]=0 #WTMX=Maximum depth to water table. Unknown=0.
  df[2,6]=100 #WTBL=Initial water table height. Set to maximum.
  df[2,7]=0 #GWST=Groundwater storage. Unknown=0.
  df[2,8]=0 #GWMX=Maximum groundwater storage. Unknown=0.
  df[2,9]=0 #RFTT=Groundwater residence time in days. Unknown=0.
  df[2,10]=NA #RFPK=Return flow/(return flow + deep percolation. Unknown=0.
  df[3,1]=nrow(SL_i) #Maximum number of soil layers after splitting. Based on # layers in AGRASID.
  df[3,2]=0 #Soil weathering code. 0=Calcareous and non-calcareous soils without weathering information.
  df[3,3]=50 #Number of Years of Cultivation at Start of Simulation.
  XIDK_levels <- levels(SN$PMTEX1) # Range of soil textures
  xidk <-if (SN_i$PMTEX1 %in% c("F", "VF")) { #Loop to define XIDK
    xidk=3
  } else if (SN_i$PMTEX1 %in% c("M", "MF")) {
    xidk=2
  } else xidk=1
  df[3,4]=xidk #Replacing the selected value in the data frame df.
  df[3,5]=0.1 #ZQT=Minimum Thickness of Maximum Layer. Default value.
  df[3,6]=0.1 #ZF=Minimum profile thickness. Default value.
  df[3,7]=0.1 #ZTK Minimum layer thickness for beginning simulation layer.Set same as other variables.
  df[3,8]=0.04 #FBM=Fraction of Org C in biomass Pool. Average value used. 
  df[3,9]=0.5 #FHP=Fraction of Org C in passive Pool. Average value used.
  df[3,10]=NA #Code written automatically by .SOT (NOT USER INPUTTED).
  for (m in 1:nrow(SL_i)){ # Loop to populate the layered information (line 4 onwards)
    df[4,m]=SL_i$LDEPTH[m]
    if (SL_i$BD[m]<=0 | is.na(SL_i$BD[m])){
      df[5,m]=NA
    } else df[5,m]=SL_i$BD[m]
    if (SL_i$KP1500[m]<=0 | is.na(SL_i$KP1500[m])){
      df[6,m]=0
    } else df[6,m]=SL_i$KP1500[m]
    if((SL_i$KP33[m]<=0 | is.na(SL_i$KP33[m])) && (SL_i$KP1500[m]>1 && SL_i$KP10>0)){
      df[7,m]=SL_i$KP10[m]
    } else if ((SL_i$KP33[m]<=0 | is.na(SL_i$KP33[m])) && (SL_i$KP1500[m]<=0 | is.na(SL_i$KP1500[m]))){
      df[7,m]=0
      } else df[7,m]=SL_i$KP33[m]
    if ((SL_i$TSAND[m]<0 && SL_i$HZN_MAS[m]=="O") | (SL_i$TSAND[m]<0 && SL_i$TSILT[m]<0)){
      df[8,m]=0
    } else df[8,m]=SL_i$TSAND[m]
    if ((SL_i$TSILT[m]<0 && SL_i$HZN_MAS[m]=="O") | (SL_i$TSILT[m]<0 && SL_i$TSAND[m]<0) | (SL_i$TSAND[m]==100)) {
      df[9,m]=0
    } else df[9,m]=SL_i$TSILT[m]
    df[10,m]=0 #WN=Initial organic N Concentration.Unknown=0
    if (SL_i$PH2[m]<=0){
      df[11,m]=NA
    } else df[11,m]=SL_i$PH2[m]
    df[12,m]=0 #SMB=Sum of Bases. Unknown=0
    if (SL_i$ORGCARB[m]<0){
      df[13,m]=NA
    } else df[13,m]=SL_i$ORGCARB[m]
    if (SL_i$CACO3[m]<0){
      df[14,m]=NA
    } else df[14,m]=SL_i$CACO3[m]
    if (SL_i$CEC[m]<0){
      df[15,m]=0
    } else df[15,m]=SL_i$CEC[m]
    if (SL_i$COFRAG[m]<0){
      df[16,m]=0
    } else df[16,m]=SL_i$COFRAG[m]
    df[17,m]=0 #CNDS=Initial soluble N concentration. Unknown=0.
    df[18,m]=0 #SSF=Initial soluble P concentration. Unknown=0.
    df[19,m]=0 #RSD=Crop residue. Unknown=0.
    df[20,m]=0 #BDD=Dry Bulk density (oven dry). Unknown=0.
    df[21,m]=0 #PSP=Phosphorus sorption ratio. Unknown=0.
    if (SL_i$KSAT[m]<0){
      df[22,m]=0 #Unknown=0
    } else df[22,m]=SL_i$KSAT[m]*10 #Transformed from cm/h to mm/h
    df[23,m]=0 #HCL=Lateral hydraulic conductivity. Unknown=0.
    df[24,m]=0 #WPO=Initial organic P concentration. Unknown=0.
    df[25,m]=0 #EXCK=Exchangeable K Concentration. No info available. Set to 0 (range: 0 to 200).
    if (SL_i$EC[m]<0){
      df[26,m]=0
    } else  df[26,m]=SL_i$EC[m]
    df[27,m]=0 #STFR=Fraction of Storage Interacting With Nitrate Leaching. Unknown=0.
    df[28,m]=0.3 #ST=Initial Soil Water Storage. Set t0 30% of FC.
    df[29,m]=0 #CPRV=Fraction inflow partitioned to vertical crack or pipe flow. Unknown=0. 
    df[30,m]=0 #CPRH=Fraction inflow partitioned to horizontal crack or pipe flow. Unknown=0.
    df[31,m]=0 #WLS=Structural litter.  Unknown=0.
    df[32,m]=0 #WLM=Metabolic litter. Unknown=0.  
    df[33,m]=0 #WLSL=Lignin Content of Structural Litter. Unknown=0. 
    df[34,m]=0 #WLSC=Carbon Content of Structural Litter. Unknown=0.
    df[35,m]=0 #WLMC=Carbon Content of Metabolic Litter. Unknown=0.
    df[36,m]=0 #WLSLC=Carbon Content of Lignin of Structural Litter. Unknown=0.
    df[37,m]=0 # WLSLNC=Nitrogen Content of Lignin of Structural Litter. Unknown=0.
    df[38,m]=0 #WBMC=Carbon Content of Biomass. Unknown=0.
    df[39,m]=0 #WHSC=Carbon Content Slow Humus. Unknown=0.
    df[40,m]=0 #WHPC=Carbon Content of Passive Humus. Unknown=0.
    df[41,m]=0 #WLSN=Nitrogen Content of Structural Litter. Unknown=0.
    df[42,m]=0 #WLMN=Nitrogen Content of Metabolic Litter. Unknown=0.
    df[43,m]=0 #WBMN=Nitrogen Content of Biomass. Unknown=0.
    df[44,m]=0 #WHSN=Nitrogen Content of Slow Humus. Unknown=0.
    df[45,m]=0 #WHPN=Nitrogen Content of Passive Humus. Unknown=0.
  if (df[8,m]==0 && df[9,m]==0){
    CLAY[1,m]=0
  } else CLAY[1,m]= 100-(df[8,m]+df[9,m])
    }
  #This section of the code rearranges the calculations done above and calculates missing parameters for the SWAT usersoil table
  SOL_1 <- as.data.frame(df[2:3,]) #Separate the first 2 rows from df containing info
  SOL_1 <- SOL_1[,colSums(is.na(SOL_1))<nrow(SOL_1)] #Remove empty columns from Sol_1
  SOL_2 <- as.data.frame(df[4:nrow(df),]) #Separates the soil layer info from df
  SOL_2 <- SOL_2[,colSums(is.na(SOL_2))<nrow(SOL_2)] #Remove empty columns from Sol_2
  SOL_2<- as.data.frame(SOL_2)# to avoid error in previous line if SOL_2 has only one column, which causes R to chage the object from data frame to vector.
  RUN <- matrix(NA,1,131)
  colnames(RUN) <- Columns
  RUN[1,1] <- as.character(MUID) # MUID
  RUN[1,2] <- 1000+i #SEQN
  RUN[1,3] <- as.character(SNAM) #SNAM
  RUN[1,4] <- S5ID #S5ID
  RUN[1,5] <- 100 #CMPPCT
  RUN[1,6] <- SOL_1[2,1] #NLAYERS
  RUN[1,7] <- SOL_1[1,2] #HYDGRP
  RUN[1,8] <- SOL_2[1,ncol(SOL_2)]*10 # SOL_ZMX=Maximum rooting depth assumed to be the dpeth of the entire profile. Multiplied by 10 (cm to mm)
  RUN[1,9] <- 0.5 # ANION_EXC. Default value if this parameter is not available
  RUN[1,10] <- 0.5 #sOL_CRK. Set to same value as all the existing soils in the APEX database
  RUN[1,11] <- "Not available" # TEXTURE. Set to NA since this info is not used by the model.
  RUN[1,12] <- SOL_2[1,1]*10 #SOL_Z1=Depth from surface to bottom of layer 1. x10 from cm to mm.
  RUN[1,13] <- SOL_2[2,1] # SOL_BD1=Bulk density (g/cm3)
  RUN[1,14] <- SOL_2[4,1]-SOL_2[3,1]# SOL_AWC1= layer 1
  RUN[1,15] <- SOL_2[19,1] #Ksat (mm/h)
  RUN[1,16] <- SOL_2[10,1] #SOL_CBN1=Soil organic carbon of layer 1 (% of soil weight)
  RUN[1,17] <- CLAY[1,1] #CLAY1= clay content of layer 1 (% of soil weight)
  RUN[1,18] <- SOL_2[6,1] #SILT1= Silt content layer 1 (% of soil weight)
  RUN[1,19] <- SOL_2[5,1] #SAND1= Sand content layer 1 (% of soil weight) 
  RUN[1,20] <- SOL_2[13,1] #ROCK1=rock fragment content (% of soil weight)
  RUN[1,21] <- SOL_1[1,1]#SOL_ALB1=Soil albedo (top layer only; others=0)
  # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
  fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,19])*(1-as.numeric(RUN[1,18])/100)))
  fcl_si <- (as.numeric(RUN[1,18])/(as.numeric(RUN[1,17])+as.numeric(RUN[1,19])))^0.3
  forgc <- (1-(0.25*as.numeric(RUN[1,16])/(as.numeric(RUN[1,16])+exp(3.72-2.95*as.numeric(RUN[1,16])))))
  fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
  RUN[1,22] <- fcsand*fcl_si*forgc*fhisand #USLE_K1=USLE soil erodibility factor.
  RUN[1,23] <- SOL_2[23,1] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  if ("V2" %in% colnames(SOL_2)==FALSE){
    RUN[1,24:35]<-0
  } else {
    RUN[1,24] <- SOL_2[1,2]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,25] <- SOL_2[2,2] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,26] <- SOL_2[4,2]-SOL_2[3,2]# SOL_AWC2= layer 1
    RUN[1,27] <- SOL_2[19,2] #Ksat (mm/h)
    RUN[1,28] <- SOL_2[10,2] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,29] <- CLAY[1,2] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,30] <- SOL_2[6,2] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,31] <- SOL_2[5,2] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,32] <- SOL_2[13,2] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,33] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,31])*(1-as.numeric(RUN[1,30])/100)))
    fcl_si <- (as.numeric(RUN[1,30])/(as.numeric(RUN[1,29])+as.numeric(RUN[1,31])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,28])/(as.numeric(RUN[1,28])+exp(3.72-2.95*as.numeric(RUN[1,28])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,34] <- 0 #USLE_K2=USLE soil erodibility factor.
    RUN[1,35] <- SOL_2[23,2] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  if ("V3" %in% colnames(SOL_2)==FALSE){
    RUN[1,36:47]<-0
  } else {
    RUN[1,36] <- SOL_2[1,3]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,37] <- SOL_2[2,3] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,38] <- SOL_2[4,3]-SOL_2[3,3]# SOL_AWC2= layer 1
    RUN[1,39] <- SOL_2[19,3] #Ksat (mm/h)
    RUN[1,40] <- SOL_2[10,3] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,41] <- CLAY[1,3] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,42] <- SOL_2[6,3] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,43] <- SOL_2[5,3] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,44] <- SOL_2[13,3] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,45] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,43])*(1-as.numeric(RUN[1,42])/100)))
    fcl_si <- (as.numeric(RUN[1,42])/(as.numeric(RUN[1,41])+as.numeric(RUN[1,43])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,40])/(as.numeric(RUN[1,40])+exp(3.72-2.95*as.numeric(RUN[1,40])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,46] <- 0 #USLE_K3=USLE soil erodibility factor.
    RUN[1,47] <- SOL_2[23,3] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  if ("V4" %in% colnames(SOL_2)==FALSE){
    RUN[1,48:59]<-0
  } else {
    RUN[1,48] <- SOL_2[1,4]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,49] <- SOL_2[2,4] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,50] <- SOL_2[4,4]-SOL_2[3,4]# SOL_AWC2= layer 1
    RUN[1,51] <- SOL_2[19,4] #Ksat (mm/h)
    RUN[1,52] <- SOL_2[10,4] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,53] <- CLAY[1,4] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,54] <- SOL_2[6,4] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,55] <- SOL_2[5,4] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,56] <- SOL_2[13,4] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,57] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,55])*(1-as.numeric(RUN[1,54])/100)))
    fcl_si <- (as.numeric(RUN[1,54])/(as.numeric(RUN[1,53])+as.numeric(RUN[1,55])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,52])/(as.numeric(RUN[1,52])+exp(3.72-2.95*as.numeric(RUN[1,52])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,58] <- 0 #USLE_K4=USLE soil erodibility factor.
    RUN[1,59] <- SOL_2[23,4] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  if ("V5" %in% colnames(SOL_2)==FALSE){
    RUN[1,60:71]<-0
  } else {
    RUN[1,60] <- SOL_2[1,5]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,61] <- SOL_2[2,5] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,62] <- SOL_2[4,5]-SOL_2[3,5]# SOL_AWC2= layer 1
    RUN[1,63] <- SOL_2[19,5] #Ksat (mm/h)
    RUN[1,64] <- SOL_2[10,5] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,65] <- CLAY[1,5] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,66] <- SOL_2[6,5] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,67] <- SOL_2[5,5] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,68] <- SOL_2[13,5] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,69] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,67])*(1-as.numeric(RUN[1,66])/100)))
    fcl_si <- (as.numeric(RUN[1,66])/(as.numeric(RUN[1,65])+as.numeric(RUN[1,67])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,64])/(as.numeric(RUN[1,64])+exp(3.72-2.95*as.numeric(RUN[1,64])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,70] <- 0 #USLE_K5=USLE soil erodibility factor.
    RUN[1,71] <- SOL_2[23,5] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  if ("V6" %in% colnames(SOL_2)==FALSE){
    RUN[1,72:83]<-0
  } else {
    RUN[1,72] <- SOL_2[1,6]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,73] <- SOL_2[2,6] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,74] <- SOL_2[4,6]-SOL_2[3,6]# SOL_AWC2= layer 1
    RUN[1,75] <- SOL_2[19,6] #Ksat (mm/h)
    RUN[1,76] <- SOL_2[10,6] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,77] <- CLAY[1,6] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,78] <- SOL_2[6,6] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,79] <- SOL_2[5,6] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,80] <- SOL_2[13,6] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,81] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,79])*(1-as.numeric(RUN[1,78])/100)))
    fcl_si <- (as.numeric(RUN[1,78])/(as.numeric(RUN[1,77])+as.numeric(RUN[1,79])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,76])/(as.numeric(RUN[1,76])+exp(3.72-2.95*as.numeric(RUN[1,76])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,82] <- 0 #USLE_K6=USLE soil erodibility factor.
    RUN[1,83] <- SOL_2[23,6] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  if ("V7" %in% colnames(SOL_2)==FALSE){
    RUN[1,84:95]<-0
  } else {
    RUN[1,84] <- SOL_2[1,7]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,85] <- SOL_2[2,7] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,86] <- SOL_2[4,7]-SOL_2[3,7]# SOL_AWC2= layer 1
    RUN[1,87] <- SOL_2[19,7] #Ksat (mm/h)
    RUN[1,88] <- SOL_2[10,7] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,89] <- CLAY[1,7] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,90] <- SOL_2[6,7] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,91] <- SOL_2[5,7] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,92] <- SOL_2[13,7] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,93] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,91])*(1-as.numeric(RUN[1,90])/100)))
    fcl_si <- (as.numeric(RUN[1,90])/(as.numeric(RUN[1,89])+as.numeric(RUN[1,91])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,88])/(as.numeric(RUN[1,88])+exp(3.72-2.95*as.numeric(RUN[1,88])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,94] <- 0 #USLE_K7=USLE soil erodibility factor.
    RUN[1,95] <- SOL_2[23,7] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  if ("V8" %in% colnames(SOL_2)==FALSE){
    RUN[1,96:107]<-0
  } else {
    RUN[1,96] <- SOL_2[1,8]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,97] <- SOL_2[2,8] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,98] <- SOL_2[4,8]-SOL_2[3,8]# SOL_AWC2= layer 1
    RUN[1,99] <- SOL_2[19,8] #Ksat (mm/h)
    RUN[1,100] <- SOL_2[10,8] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,101] <- CLAY[1,8] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,102] <- SOL_2[6,8] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,103] <- SOL_2[5,8] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,104] <- SOL_2[13,8] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,105] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,103])*(1-as.numeric(RUN[1,102])/100)))
    fcl_si <- (as.numeric(RUN[1,102])/(as.numeric(RUN[1,101])+as.numeric(RUN[1,103])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,100])/(as.numeric(RUN[1,100])+exp(3.72-2.95*as.numeric(RUN[1,100])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,106] <- 0 #USLE_K8=USLE soil erodibility factor.
    RUN[1,107] <- SOL_2[23,8] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  if ("V9" %in% colnames(SOL_2)==FALSE){
    RUN[1,108:119]<-0
  } else {
    RUN[1,108] <- SOL_2[1,9]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,109] <- SOL_2[2,9] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,110] <- SOL_2[4,9]-SOL_2[3,9]# SOL_AWC2= layer 1
    RUN[1,111] <- SOL_2[19,9] #Ksat (mm/h)
    RUN[1,112] <- SOL_2[10,9] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,113] <- CLAY[1,9] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,114] <- SOL_2[6,9] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,115] <- SOL_2[5,9] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,116] <- SOL_2[13,9] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,117] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,115])*(1-as.numeric(RUN[1,114])/100)))
    fcl_si <- (as.numeric(RUN[1,114])/(as.numeric(RUN[1,113])+as.numeric(RUN[1,115])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,112])/(as.numeric(RUN[1,112])+exp(3.72-2.95*as.numeric(RUN[1,112])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,118] <- 0 #USLE_K9=USLE soil erodibility factor.
    RUN[1,119] <- SOL_2[23,9] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  if ("V10" %in% colnames(SOL_2)==FALSE){
    RUN[1,120:131]<-0
  } else {
    RUN[1,120] <- SOL_2[1,10]*10 #SOL_Z2=Depth from surface to bottom of layer 1. x10 from cm to mm.
    RUN[1,121] <- SOL_2[2,10] # SOL_BD2=Bulk density (g/cm3)
    RUN[1,122] <- SOL_2[4,10]-SOL_2[3,10]# SOL_AWC2= layer 1
    RUN[1,123] <- SOL_2[19,10] #Ksat (mm/h)
    RUN[1,124] <- SOL_2[10,10] #SOL_CBN2=Soil organic carbon of layer 2 (% of soil weight)
    RUN[1,125] <- CLAY[1,10] #CLAY2= clay content of layer 2 (% of soil weight)
    RUN[1,126] <- SOL_2[6,10] #SILT2= Silt content layer 2 (% of soil weight)
    RUN[1,127] <- SOL_2[5,10] #SAND2= Sand content layer 2 (% of soil weight) 
    RUN[1,128] <- SOL_2[13,10] #ROCK2=rock fragment content (% of soil weight)
    RUN[1,129] <- 0 #SOL_ALB2=Soil albedo (top layer only; others=0)
    # the following 4 calculations are part of the Kusle formula gven in the SWAT theoretical documentation (p.280)
    fcsand <- (0.2+0.3*exp(-0.256*as.numeric(RUN[1,127])*(1-as.numeric(RUN[1,126])/100)))
    fcl_si <- (as.numeric(RUN[1,126])/(as.numeric(RUN[1,125])+as.numeric(RUN[1,127])))^0.3
    forgc <- (1-(0.25*as.numeric(RUN[1,124])/(as.numeric(RUN[1,124])+exp(3.72-2.95*as.numeric(RUN[1,124])))))
    fhisand <- 1-(0.7*(1-as.numeric(RUN[1,19])/100))/((1-as.numeric(RUN[1,19])/100)+exp(-5.51+22.9*(1-as.numeric(RUN[1,19])/100)))
    RUN[1,130] <- 0 #USLE_K10=USLE soil erodibility factor.
    RUN[1,131] <- SOL_2[23,10] #SOL_EC=electrical conductivity (dS/m=mmho/cm)
  }
  COMP <- rbind(COMP,RUN)
}
COMP <- COMP[-1,]  
OBJECTID <- as.numeric(seq(1001,(1000+nrow(COMP)),1))
COMP <- cbind(OBJECTID,COMP)
CA_PH <- matrix(0,nrow(COMP),20)
Col_CAL <- paste("SOL_CAL", seq(1,10), sep="")
Col_PH <- paste("SOL_PH", seq(1,10), sep="")
colnames(CA_PH) <- c(Col_CAL,Col_PH)
COMP <- cbind(COMP, CA_PH)
FILE_OUT = "./Data/Canada/SLCsoils_SWAT_COMPLETE.csv"
write.csv(COMP,FILE_OUT, row.names = FALSE)
