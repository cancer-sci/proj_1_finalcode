##REQUIRES mpg.db in local environment - see PC specific import scripts.
#CLEAR local environment: rm(list=ls())
## Pre-processing ----
#Create edited mpg.db without patients with NOHIST, NOSCANS and FALLOPIAN CANCER
mpg <- mpg.db
mpg <- mpg[-which(mpg$PATIENT_STATUS=="NOSCANS"),]
mpg <- mpg[-which(mpg$PATIENT_STATUS=="NOHIST"),]
mpg <- mpg[-which(mpg$PATIENT_STATUS=="FALLOPIAN CANCER"),]
#View(mpg)

#Scale down MRI readings by x10-3
mpg$MRI_ADC_LP_1 <- mpg$MRI_ADC_LP_1/1000
mpg$MRI_ADC_LP_2 <- mpg$MRI_ADC_LP_2/1000
mpg$MRI_ADC_RP_1 <- mpg$MRI_ADC_RP_1/1000
mpg$MRI_ADC_RP_2 <- mpg$MRI_ADC_RP_2/1000
mpg$MRI_ADC_PALN_1 <- mpg$MRI_ADC_PALN_1/1000
mpg$MRI_ADC_PALN_2 <- mpg$MRI_ADC_PALN_2/1000
mpg$MRI_ADC_PT_1 <- mpg$MRI_ADC_PT_1/1000
mpg$MRI_ADC_PT_2 <- mpg$MRI_ADC_PT_2/1000


## 1.1 - UNI - FDG IN METASTATIC VS BENIGN LNs ----
#Create db of just FDG measurements + histology
mpg.fdg.uni <- mpg
mpg.fdg.uni <- mpg.fdg.uni[-which(mpg.fdg.uni$PATIENT_STATUS=="FDG DYN"),]
mpg.fdg.uni.columns <- c(
  "PATIENT",
  "PATIENT_SCAN_FDG",
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "CANC_CER",
  "CANC_ENDO",
  "FDG_SUV_PT")
mpg.fdg.uni <- mpg.fdg.uni[,mpg.fdg.uni.columns]
mpg.fdg.uni <- mpg.fdg.uni[which(mpg.fdg.uni$PATIENT_SCAN_FDG==1),]
##View(mpg.fdg.uni)

#Stratify into endometrial cancer and cervical cancer
mpg.fdg.uni.cer <- mpg.fdg.uni[which(mpg.fdg.uni$CANC_CER==1),]
mpg.fdg.uni.endo <- mpg.fdg.uni[which(mpg.fdg.uni$CANC_ENDO==1),]
##View(mpg.fdg.uni.cer) #12 patients
##View(mpg.fdg.uni.endo) #34 patients

#Bind LP, RP and PALN columns together into new objects and remove rows with no scans OR histology
fdg.uni.lp.columns <- c(
  "PATIENT",
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "HIST_LP")

mpg.fdg.uni.cer.lp <- mpg.fdg.uni.cer[,fdg.uni.lp.columns]
if(sum(is.na(mpg.fdg.uni.cer.lp$HIST_LP))!=0){
  mpg.fdg.uni.cer.lp <- mpg.fdg.uni.cer.lp[-which(is.na(mpg.fdg.uni.cer.lp$HIST_LP)),]
  mpg.fdg.uni.cer.lp <- mpg.fdg.uni.cer.lp[-which(is.na(
    mpg.fdg.uni.cer.lp$FDG_SUV_LP_1|
      mpg.fdg.uni.cer.lp$FDG_SUV_LP_2)),]
} else {
  mpg.fdg.uni.cer.lp <- mpg.fdg.uni.cer.lp[-which(is.na(
    mpg.fdg.uni.cer.lp$FDG_SUV_LP_1|
      mpg.fdg.uni.cer.lp$FDG_SUV_LP_2)),]
}

mpg.fdg.uni.endo.lp <- mpg.fdg.uni.endo[,fdg.uni.lp.columns]
if(sum(is.na(mpg.fdg.uni.endo.lp$HIST_LP))!=0){
  mpg.fdg.uni.endo.lp <- mpg.fdg.uni.endo.lp[-which(is.na(mpg.fdg.uni.endo.lp$HIST_LP)),]
  mpg.fdg.uni.endo.lp <- mpg.fdg.uni.endo.lp[-which(is.na(
    mpg.fdg.uni.endo.lp$FDG_SUV_LP_1|
      mpg.fdg.uni.endo.lp$FDG_SUV_LP_2)),]
} else {
  mpg.fdg.uni.endo.lp <- mpg.fdg.uni.endo.lp[-which(is.na(
    mpg.fdg.uni.endo.lp$FDG_SUV_LP_1|
      mpg.fdg.uni.endo.lp$FDG_SUV_LP_2)),]
}

fdg.uni.rp.columns <- c(
  "PATIENT",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "HIST_RP")
mpg.fdg.uni.cer.rp <- mpg.fdg.uni.cer[,fdg.uni.rp.columns]
if(sum(is.na(mpg.fdg.uni.cer.rp$HIST_RP))!=0){
  mpg.fdg.uni.cer.rp <- mpg.fdg.uni.cer.rp[-which(is.na(mpg.fdg.uni.cer.rp$HIST_RP)),]
  mpg.fdg.uni.cer.rp <- mpg.fdg.uni.cer.rp[-which(is.na(
    mpg.fdg.uni.cer.rp$FDG_SUV_RP_1|
      mpg.fdg.uni.cer.rp$FDG_SUV_RP_2)),]
} else {
  mpg.fdg.uni.cer.rp <- mpg.fdg.uni.cer.rp[-which(is.na(
    mpg.fdg.uni.cer.rp$FDG_SUV_RP_1|
      mpg.fdg.uni.cer.rp$FDG_SUV_RP_2)),]
}

mpg.fdg.uni.endo.rp <- mpg.fdg.uni.endo[,fdg.uni.rp.columns]
if(sum(is.na(mpg.fdg.uni.endo.rp$HIST_RP))!=0){
  mpg.fdg.uni.endo.rp <- mpg.fdg.uni.endo.rp[-which(is.na(mpg.fdg.uni.endo.rp$HIST_RP)),]
  mpg.fdg.uni.endo.rp <- mpg.fdg.uni.endo.rp[-which(is.na(
    mpg.fdg.uni.endo.rp$FDG_SUV_RP_1|
      mpg.fdg.uni.endo.rp$FDG_SUV_RP_2)),]
} else {
  mpg.fdg.uni.endo.rp <- mpg.fdg.uni.endo.rp[-which(is.na(
    mpg.fdg.uni.endo.rp$FDG_SUV_RP_1|
      mpg.fdg.uni.endo.rp$FDG_SUV_RP_2)),]
}

fdg.uni.paln.columns <- c(
  "PATIENT",
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "HIST_PALN")
mpg.fdg.uni.cer.paln <- mpg.fdg.uni.cer[,fdg.uni.paln.columns]
if(sum(is.na(mpg.fdg.uni.cer.paln$HIST_PALN))!=0){
  mpg.fdg.uni.cer.paln <- mpg.fdg.uni.cer.paln[-which(is.na(mpg.fdg.uni.cer.paln$HIST_PALN)),]
  mpg.fdg.uni.cer.paln <- mpg.fdg.uni.cer.paln[-which(is.na(
    mpg.fdg.uni.cer.paln$FDG_SUV_PALN_1|
      mpg.fdg.uni.cer.paln$FDG_SUV_PALN_2)),]
} else {
  mpg.fdg.uni.cer.paln <- mpg.fdg.uni.cer.paln[-which(is.na(
    mpg.fdg.uni.cer.paln$FDG_SUV_PALN_1|
      mpg.fdg.uni.cer.paln$FDG_SUV_PALN_2)),]
}

mpg.fdg.uni.endo.paln <- mpg.fdg.uni.endo[,fdg.uni.paln.columns]
if(sum(is.na(mpg.fdg.uni.endo.paln$HIST_PALN))!=0){
  mpg.fdg.uni.endo.paln <- mpg.fdg.uni.endo.paln[-which(is.na(mpg.fdg.uni.endo.paln$HIST_PALN)),]
  mpg.fdg.uni.endo.paln <- mpg.fdg.uni.endo.paln[-which(is.na(
    mpg.fdg.uni.endo.paln$FDG_SUV_PALN_1|
      mpg.fdg.uni.endo.paln$FDG_SUV_PALN_2)),]
} else {
  mpg.fdg.uni.endo.paln <- mpg.fdg.uni.endo.paln[-which(is.na(
    mpg.fdg.uni.endo.paln$FDG_SUV_PALN_1|
      mpg.fdg.uni.endo.paln$FDG_SUV_PALN_2)),]
}

##View(mpg.fdg.uni.cer.lp) #8 patients
##View(mpg.fdg.uni.cer.rp) #6 patients
##View(mpg.fdg.uni.cer.paln) #0 patients

##View(mpg.fdg.uni.endo.lp) #23 patients
##View(mpg.fdg.uni.endo.rp) #27 patients
##View(mpg.fdg.uni.endo.paln) #10 patients

## 1.2 - UNI - FEC IN METASTATIC VS BENIGN LNs ----
#Create db of just FEC measurements + histology
mpg.fec.uni <- mpg
mpg.fec.uni.columns <- c(
  "PATIENT",
  "PATIENT_SCAN_FEC",
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "CANC_CER",
  "CANC_ENDO",
  "FEC_SUV_PT")
mpg.fec.uni <- mpg.fec.uni[,mpg.fec.uni.columns]
mpg.fec.uni <- mpg.fec.uni[which(mpg.fec.uni$PATIENT_SCAN_FEC==1),]
##View(mpg.fec.uni)

#Stratify into endometrial cancer and cervical cancer
mpg.fec.uni.cer <- mpg.fec.uni[which(mpg.fec.uni$CANC_CER==1),]
mpg.fec.uni.endo <- mpg.fec.uni[which(mpg.fec.uni$CANC_ENDO==1),]
##View(mpg.fec.uni.cer) #8 patients
##View(mpg.fec.uni.endo) #18 patients

#Bind LP, RP and PALN columns together into new objects and remove rows with no scans OR histology
fec.lp.columns <- c(
  "PATIENT",
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "HIST_LP")
mpg.fec.uni.cer.lp <- mpg.fec.uni.cer[,fec.lp.columns]
if(sum(is.na(mpg.fec.uni.cer.lp$HIST_LP))!=0){
  mpg.fec.uni.cer.lp <- mpg.fec.uni.cer.lp[-which(is.na(mpg.fec.uni.cer.lp$HIST_LP)),]
  mpg.fec.uni.cer.lp <- mpg.fec.uni.cer.lp[-which(is.na(
    mpg.fec.uni.cer.lp$FEC_SUV_LP_1|
      mpg.fec.uni.cer.lp$FEC_SUV_LP_2)),]
} else {
  mpg.fec.uni.cer.lp <- mpg.fec.uni.cer.lp[-which(is.na(
    mpg.fec.uni.cer.lp$FEC_SUV_LP_1|
      mpg.fec.uni.cer.lp$FEC_SUV_LP_2)),]
}
mpg.fec.uni.endo.lp <- mpg.fec.uni.endo[,fec.lp.columns]
if(sum(is.na(mpg.fec.uni.endo.lp$HIST_LP))!=0){
  mpg.fec.uni.endo.lp <- mpg.fec.uni.endo.lp[-which(is.na(mpg.fec.uni.endo.lp$HIST_LP)),]
  mpg.fec.uni.endo.lp <- mpg.fec.uni.endo.lp[-which(is.na(
    mpg.fec.uni.endo.lp$FEC_SUV_LP_1|
      mpg.fec.uni.endo.lp$FEC_SUV_LP_2)),]
} else {
  mpg.fec.uni.endo.lp <- mpg.fec.uni.endo.lp[-which(is.na(
    mpg.fec.uni.endo.lp$FEC_SUV_LP_1|
      mpg.fec.uni.endo.lp$FEC_SUV_LP_2)),]
}

fec.rp.columns <- c(
  "PATIENT",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "HIST_RP")
mpg.fec.uni.cer.rp <- mpg.fec.uni.cer[,fec.rp.columns]
if(sum(is.na(mpg.fec.uni.cer.rp$HIST_RP))!=0){
  mpg.fec.uni.cer.rp <- mpg.fec.uni.cer.rp[-which(is.na(mpg.fec.uni.cer.rp$HIST_RP)),]
  mpg.fec.uni.cer.rp <- mpg.fec.uni.cer.rp[-which(is.na(
    mpg.fec.uni.cer.rp$FEC_SUV_RP_1|
      mpg.fec.uni.cer.rp$FEC_SUV_RP_2)),]
} else {
  mpg.fec.uni.cer.rp <- mpg.fec.uni.cer.rp[-which(is.na(
    mpg.fec.uni.cer.rp$FEC_SUV_RP_1|
      mpg.fec.uni.cer.rp$FEC_SUV_RP_2)),]
}
mpg.fec.uni.endo.rp <- mpg.fec.uni.endo[,fec.rp.columns]
if(sum(is.na(mpg.fec.uni.endo.rp$HIST_RP))!=0){
  mpg.fec.uni.endo.rp <- mpg.fec.uni.endo.rp[-which(is.na(mpg.fec.uni.endo.rp$HIST_RP)),]
  mpg.fec.uni.endo.rp <- mpg.fec.uni.endo.rp[-which(is.na(
    mpg.fec.uni.endo.rp$FEC_SUV_RP_1|
      mpg.fec.uni.endo.rp$FEC_SUV_RP_2)),]
} else {
  mpg.fec.uni.endo.rp <- mpg.fec.uni.endo.rp[-which(is.na(
    mpg.fec.uni.endo.rp$FEC_SUV_RP_1|
      mpg.fec.uni.endo.rp$FEC_SUV_RP_2)),]
}

fec.paln.columns <- c(
  "PATIENT",
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "HIST_PALN")
mpg.fec.uni.cer.paln <- mpg.fec.uni.cer[,fec.paln.columns]
if(sum(is.na(mpg.fec.uni.cer.paln$HIST_PALN))!=0){
  mpg.fec.uni.cer.paln <- mpg.fec.uni.cer.paln[-which(is.na(mpg.fec.uni.cer.paln$HIST_PALN)),]
  mpg.fec.uni.cer.paln <- mpg.fec.uni.cer.paln[-which(is.na(
    mpg.fec.uni.cer.paln$FEC_SUV_PALN_1|
      mpg.fec.uni.cer.paln$FEC_SUV_PALN_2)),]
} else {
  mpg.fec.uni.cer.paln <- mpg.fec.uni.cer.paln[-which(is.na(
    mpg.fec.uni.cer.paln$FEC_SUV_PALN_1|
      mpg.fec.uni.cer.paln$FEC_SUV_PALN_2)),]
}
mpg.fec.uni.endo.paln <- mpg.fec.uni.endo[,fec.paln.columns]
if(sum(is.na(mpg.fec.uni.endo.paln$HIST_PALN))!=0){
  mpg.fec.uni.endo.paln <- mpg.fec.uni.endo.paln[-which(is.na(mpg.fec.uni.endo.paln$HIST_PALN)),]
  mpg.fec.uni.endo.paln <- mpg.fec.uni.endo.paln[-which(is.na(
    mpg.fec.uni.endo.paln$FEC_SUV_PALN_1|
      mpg.fec.uni.endo.paln$FEC_SUV_PALN_2)),]
} else {
  mpg.fec.uni.endo.paln <- mpg.fec.uni.endo.paln[-which(is.na(
    mpg.fec.uni.endo.paln$FEC_SUV_PALN_1|
      mpg.fec.uni.endo.paln$FEC_SUV_PALN_2)),]
}

##View(mpg.fec.uni.cer.lp) #5 patients
##View(mpg.fec.uni.cer.rp) #4 patients
##View(mpg.fec.uni.cer.paln) #0 patients

##View(mpg.fec.uni.endo.lp) #13 patients
##View(mpg.fec.uni.endo.rp) #12 patients
##View(mpg.fec.uni.endo.paln) #6 patients

## 1.3 - UNI - MRI ADC IN METASTATIC VS BENIGN LNs ----
#Create db of just MRI measurements + histology
mpg.mri.uni <- mpg
mpg.mri.uni.columns <- c(
  "PATIENT",
  "PATIENT_SCAN_MRI",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "CANC_CER",
  "CANC_ENDO",
  "MRI_ADC_PT_1",
  "MRI_ADC_PT_2")
mpg.mri.uni <- mpg.mri.uni[,mpg.mri.uni.columns]
mpg.mri.uni <- mpg.mri.uni[which(mpg.mri.uni$PATIENT_SCAN_MRI==1),]
##View(mpg.mri.uni)

#Stratify into endometrial cancer and cervical cancer
mpg.mri.uni.cer <- mpg.mri.uni[which(mpg.mri.uni$CANC_CER==1),]
mpg.mri.uni.endo <- mpg.mri.uni[which(mpg.mri.uni$CANC_ENDO==1),]
##View(mpg.mri.uni.cer) #27 patients
##View(mpg.mri.uni.endo) #59 patients

#Bind LP, RP and PALN columns together into new objects and remove rows with no scans OR histology
mri.lp.columns <- c(
  "PATIENT",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "HIST_LP",
  "CANC_CER")
mpg.mri.uni.cer.lp <- mpg.mri.uni.cer[,mri.lp.columns]
if(sum(is.na(mpg.mri.uni.cer.lp$HIST_LP))!=0){
  mpg.mri.uni.cer.lp <- mpg.mri.uni.cer.lp[-which(is.na(mpg.mri.uni.cer.lp$HIST_LP)),]
  mpg.mri.uni.cer.lp <- mpg.mri.uni.cer.lp[-which(is.na(
    mpg.mri.uni.cer.lp$MRI_ADC_LP_1|
      mpg.mri.uni.cer.lp$MRI_ADC_LP_2)),]
} else {
  mpg.mri.uni.cer.lp <- mpg.mri.uni.cer.lp[-which(is.na(
    mpg.mri.uni.cer.lp$MRI_ADC_LP_1|
      mpg.mri.uni.cer.lp$MRI_ADC_LP_2)),]
}
mpg.mri.uni.endo.lp <- mpg.mri.uni.endo[,mri.lp.columns]
if(sum(is.na(mpg.mri.uni.endo.lp$HIST_LP))!=0){
  mpg.mri.uni.endo.lp <- mpg.mri.uni.endo.lp[-which(is.na(mpg.mri.uni.endo.lp$HIST_LP)),]
  mpg.mri.uni.endo.lp <- mpg.mri.uni.endo.lp[-which(is.na(
    mpg.mri.uni.endo.lp$MRI_ADC_LP_1|
      mpg.mri.uni.endo.lp$MRI_ADC_LP_2)),]
} else {
  mpg.mri.uni.endo.lp <- mpg.mri.uni.endo.lp[-which(is.na(
    mpg.mri.uni.endo.lp$MRI_ADC_LP_1|
      mpg.mri.uni.endo.lp$MRI_ADC_LP_2)),]
}

mri.rp.columns <- c(
  "PATIENT",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "HIST_RP")
mpg.mri.uni.cer.rp <- mpg.mri.uni.cer[,mri.rp.columns]
if(sum(is.na(mpg.mri.uni.cer.rp$HIST_RP))!=0){
  mpg.mri.uni.cer.rp <- mpg.mri.uni.cer.rp[-which(is.na(mpg.mri.uni.cer.rp$HIST_RP)),]
  mpg.mri.uni.cer.rp <- mpg.mri.uni.cer.rp[-which(is.na(
    mpg.mri.uni.cer.rp$MRI_ADC_RP_1|
      mpg.mri.uni.cer.rp$MRI_ADC_RP_2)),]
} else {
  mpg.mri.uni.cer.rp <- mpg.mri.uni.cer.rp[-which(is.na(
    mpg.mri.uni.cer.rp$MRI_ADC_RP_1|
      mpg.mri.uni.cer.rp$MRI_ADC_RP_2)),]
}
mpg.mri.uni.endo.rp <- mpg.mri.uni.endo[,mri.rp.columns]
if(sum(is.na(mpg.mri.uni.endo.rp$HIST_RP))!=0){
  mpg.mri.uni.endo.rp <- mpg.mri.uni.endo.rp[-which(is.na(mpg.mri.uni.endo.rp$HIST_RP)),]
  mpg.mri.uni.endo.rp <- mpg.mri.uni.endo.rp[-which(is.na(
    mpg.mri.uni.endo.rp$MRI_ADC_RP_1|
      mpg.mri.uni.endo.rp$MRI_ADC_RP_2)),]
} else {
  mpg.mri.uni.endo.rp <- mpg.mri.uni.endo.rp[-which(is.na(
    mpg.mri.uni.endo.rp$MRI_ADC_RP_1|
      mpg.mri.uni.endo.rp$MRI_ADC_RP_2)),]
}

mri.paln.columns <- c(
  "PATIENT",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "HIST_PALN")
mpg.mri.uni.cer.paln <- mpg.mri.uni.cer[,mri.paln.columns]
if(sum(is.na(mpg.mri.uni.cer.paln$HIST_PALN))!=0){
  mpg.mri.uni.cer.paln <- mpg.mri.uni.cer.paln[-which(is.na(mpg.mri.uni.cer.paln$HIST_PALN)),]
  mpg.mri.uni.cer.paln <- mpg.mri.uni.cer.paln[-which(is.na(
    mpg.mri.uni.cer.paln$MRI_ADC_PALN_1|
      mpg.mri.uni.cer.paln$MRI_ADC_PALN_2)),]
} else {
  mpg.mri.uni.cer.paln <- mpg.mri.uni.cer.paln[-which(is.na(
    mpg.mri.uni.cer.paln$MRI_ADC_PALN_1|
      mpg.mri.uni.cer.paln$MRI_ADC_PALN_2)),]
}
mpg.mri.uni.endo.paln <- mpg.mri.uni.endo[,mri.paln.columns]
if(sum(is.na(mpg.mri.uni.endo.paln$HIST_PALN))!=0){
  mpg.mri.uni.endo.paln <- mpg.mri.uni.endo.paln[-which(is.na(mpg.mri.uni.endo.paln$HIST_PALN)),]
  mpg.mri.uni.endo.paln <- mpg.mri.uni.endo.paln[-which(is.na(
    mpg.mri.uni.endo.paln$MRI_ADC_PALN_1|
      mpg.mri.uni.endo.paln$MRI_ADC_PALN_2)),]
} else {
  mpg.mri.uni.endo.paln <- mpg.mri.uni.endo.paln[-which(is.na(
    mpg.mri.uni.endo.paln$MRI_ADC_PALN_1|
      mpg.mri.uni.endo.paln$MRI_ADC_PALN_2)),]
}

##View(mpg.mri.uni.cer.lp) #20 patients
##View(mpg.mri.uni.cer.rp) #21 patients
##View(mpg.mri.uni.cer.paln) #1 patients

##View(mpg.mri.uni.endo.lp) #44 patients
#View(mpg.mri.uni.endo.rp) #48 patients
#View(mpg.mri.uni.endo.paln) #14 patients

## 1.4 - UNI - ORGANISE INTO DF FOR ANALYSIS ----
# 1.4.1 > FDG DFs ----
#CER LP
mpg.fdg.uni.cer.lp.df <- 
  rbind(
    mpg.fdg.uni.cer.lp[which(mpg.fdg.uni.cer.lp$HIST_LP==1),],
    mpg.fdg.uni.cer.lp[which(mpg.fdg.uni.cer.lp$HIST_LP==0),]
)
mpg.fdg.uni.cer.lp.means <-
  rep(NA, nrow(mpg.fdg.uni.cer.lp),ncol=1)
for(i in 1:nrow(mpg.fdg.uni.cer.lp.df)){
  mpg.fdg.uni.cer.lp.means[i] <- 
    mean(c(
      mpg.fdg.uni.cer.lp.df$FDG_SUV_LP_1[i],
      mpg.fdg.uni.cer.lp.df$FDG_SUV_LP_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.uni.cer.lp.df <-
  cbind(
    mpg.fdg.uni.cer.lp.df$HIST_LP,
    mpg.fdg.uni.cer.lp.means)
colnames(mpg.fdg.uni.cer.lp.df) <- c("HIST","MEAN")
#View(mpg.fdg.uni.cer.lp.df)

#FDG CER RP
mpg.fdg.uni.cer.rp.df <- 
  rbind(
    mpg.fdg.uni.cer.rp[which(mpg.fdg.uni.cer.rp$HIST_RP==1),],
    mpg.fdg.uni.cer.rp[which(mpg.fdg.uni.cer.rp$HIST_RP==0),]
  )
mpg.fdg.uni.cer.rp.means <-
  rep(NA, nrow(mpg.fdg.uni.cer.rp),ncol=1)
for(i in 1:nrow(mpg.fdg.uni.cer.rp.df)){
  mpg.fdg.uni.cer.rp.means[i] <- 
    mean(c(
      mpg.fdg.uni.cer.rp.df$FDG_SUV_RP_1[i],
      mpg.fdg.uni.cer.rp.df$FDG_SUV_RP_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.uni.cer.rp.df <-
  cbind(
    mpg.fdg.uni.cer.rp.df$HIST_RP,
    mpg.fdg.uni.cer.rp.means)
colnames(mpg.fdg.uni.cer.rp.df) <- c("HIST","MEAN")
#View(mpg.fdg.uni.cer.rp.df)

#FDG CER PELVIC
mpg.fdg.uni.cer.pel.df <- 
  rbind(
    mpg.fdg.uni.cer.rp.df,
    mpg.fdg.uni.cer.lp.df
  )

#FDG ENDO LP
mpg.fdg.uni.endo.lp.df <- 
  rbind(
    mpg.fdg.uni.endo.lp[which(mpg.fdg.uni.endo.lp$HIST_LP==1),],
    mpg.fdg.uni.endo.lp[which(mpg.fdg.uni.endo.lp$HIST_LP==0),]
  )
mpg.fdg.uni.endo.lp.means <-
  rep(NA, nrow(mpg.fdg.uni.endo.lp),ncol=1)
for(i in 1:nrow(mpg.fdg.uni.endo.lp.df)){
  mpg.fdg.uni.endo.lp.means[i] <- 
    mean(c(
      mpg.fdg.uni.endo.lp.df$FDG_SUV_LP_1[i],
      mpg.fdg.uni.endo.lp.df$FDG_SUV_LP_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.uni.endo.lp.df <-
  cbind(
    mpg.fdg.uni.endo.lp.df$HIST_LP,
    mpg.fdg.uni.endo.lp.means)
colnames(mpg.fdg.uni.endo.lp.df) <- c("HIST","MEAN")
mpg.fdg.uni.endo.lp.df <- as.data.frame(mpg.fdg.uni.endo.lp.df)
mpg.fdg.uni.endo.lp.df$HIST <- as.factor(mpg.fdg.uni.endo.lp.df$HIST)
#View(mpg.fdg.uni.endo.lp.df)

#FDG ENDO RP
mpg.fdg.uni.endo.rp.df <- 
  rbind(
    mpg.fdg.uni.endo.rp[which(mpg.fdg.uni.endo.rp$HIST_RP==1),],
    mpg.fdg.uni.endo.rp[which(mpg.fdg.uni.endo.rp$HIST_RP==0),]
  )
mpg.fdg.uni.endo.rp.means <-
  rep(NA, nrow(mpg.fdg.uni.endo.rp),ncol=1)
for(i in 1:nrow(mpg.fdg.uni.endo.rp.df)){
  mpg.fdg.uni.endo.rp.means[i] <- 
    mean(c(
      mpg.fdg.uni.endo.rp.df$FDG_SUV_RP_1[i],
      mpg.fdg.uni.endo.rp.df$FDG_SUV_RP_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.uni.endo.rp.df <-
  cbind(
    mpg.fdg.uni.endo.rp.df$HIST_RP,
    mpg.fdg.uni.endo.rp.means)
colnames(mpg.fdg.uni.endo.rp.df) <- c("HIST","MEAN")
mpg.fdg.uni.endo.rp.df <- as.data.frame(mpg.fdg.uni.endo.rp.df)
mpg.fdg.uni.endo.rp.df$HIST <- as.factor(mpg.fdg.uni.endo.rp.df$HIST)
#View(mpg.fdg.uni.endo.rp.df)

#FDG ENDO PELVIC
mpg.fdg.uni.endo.pel.df <- 
  as.data.frame(rbind(
    mpg.fdg.uni.endo.rp.df,
    mpg.fdg.uni.endo.lp.df
  ))
mpg.fdg.uni.endo.pel.df$HIST <- as.factor(mpg.fdg.uni.endo.pel.df$HIST)

#FDG ENDO PALN
mpg.fdg.uni.endo.paln.df <- 
  rbind(
    mpg.fdg.uni.endo.paln[which(mpg.fdg.uni.endo.paln$HIST_PALN==1),],
    mpg.fdg.uni.endo.paln[which(mpg.fdg.uni.endo.paln$HIST_PALN==0),]
  )
mpg.fdg.uni.endo.paln.means <-
  rep(NA, nrow(mpg.fdg.uni.endo.paln),ncol=1)
for(i in 1:nrow(mpg.fdg.uni.endo.paln.df)){
  mpg.fdg.uni.endo.paln.means[i] <- 
    mean(c(
      mpg.fdg.uni.endo.paln.df$FDG_SUV_PALN_1[i],
      mpg.fdg.uni.endo.paln.df$FDG_SUV_PALN_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.uni.endo.paln.df <-
  cbind(
    mpg.fdg.uni.endo.paln.df$HIST_PALN,
    mpg.fdg.uni.endo.paln.means)
colnames(mpg.fdg.uni.endo.paln.df) <- c("HIST","MEAN")
mpg.fdg.uni.endo.paln.df <- as.data.frame(mpg.fdg.uni.endo.paln.df)
mpg.fdg.uni.endo.paln.df$HIST <- as.factor(mpg.fdg.uni.endo.paln.df$HIST)
#View(mpg.fdg.uni.endo.paln.df)

#FDG ENDO ALL

mpg.fdg.uni.endo.all.df <- 
  as.data.frame(rbind(
    mpg.fdg.uni.endo.rp.df,
    mpg.fdg.uni.endo.lp.df,
    mpg.fdg.uni.endo.paln.df
  ))
mpg.fdg.uni.endo.all.df$HIST <- as.factor(mpg.fdg.uni.endo.all.df$HIST)

# 1.4.2 > FEC DFs ----
#FEC CER LP
mpg.fec.uni.cer.lp.df <- 
  rbind(
    mpg.fec.uni.cer.lp[which(mpg.fec.uni.cer.lp$HIST_LP==1),],
    mpg.fec.uni.cer.lp[which(mpg.fec.uni.cer.lp$HIST_LP==0),]
  )
mpg.fec.uni.cer.lp.means <-
  rep(NA, nrow(mpg.fec.uni.cer.lp),ncol=1)
for(i in 1:nrow(mpg.fec.uni.cer.lp.df)){
  mpg.fec.uni.cer.lp.means[i] <- 
    mean(c(
      mpg.fec.uni.cer.lp.df$FEC_SUV_LP_1[i],
      mpg.fec.uni.cer.lp.df$FEC_SUV_LP_2[i]), 
      na.rm = TRUE)
}
mpg.fec.uni.cer.lp.df <-
  cbind(
    mpg.fec.uni.cer.lp.df$HIST_LP,
    mpg.fec.uni.cer.lp.means)
colnames(mpg.fec.uni.cer.lp.df) <- c("HIST","MEAN")
#View(mpg.fec.uni.cer.lp.df)

#FEC CER RP
mpg.fec.uni.cer.rp.df <- 
  rbind(
    mpg.fec.uni.cer.rp[which(mpg.fec.uni.cer.rp$HIST_RP==1),],
    mpg.fec.uni.cer.rp[which(mpg.fec.uni.cer.rp$HIST_RP==0),]
  )
mpg.fec.uni.cer.rp.means <-
  rep(NA, nrow(mpg.fec.uni.cer.rp),ncol=1)
for(i in 1:nrow(mpg.fec.uni.cer.rp.df)){
  mpg.fec.uni.cer.rp.means[i] <- 
    mean(c(
      mpg.fec.uni.cer.rp.df$FEC_SUV_RP_1[i],
      mpg.fec.uni.cer.rp.df$FEC_SUV_RP_2[i]), 
      na.rm = TRUE)
}
mpg.fec.uni.cer.rp.df <-
  cbind(
    mpg.fec.uni.cer.rp.df$HIST_RP,
    mpg.fec.uni.cer.rp.means)
colnames(mpg.fec.uni.cer.rp.df) <- c("HIST","MEAN")
#View(mpg.fec.uni.cer.rp.df)

#FEC CER PEL
mpg.fec.uni.cer.pel.df <- 
  rbind(
    mpg.fec.uni.cer.rp.df,
    mpg.fec.uni.cer.lp.df
  )

#FEC ENDO LP
mpg.fec.uni.endo.lp.df <- 
  rbind(
    mpg.fec.uni.endo.lp[which(mpg.fec.uni.endo.lp$HIST_LP==1),],
    mpg.fec.uni.endo.lp[which(mpg.fec.uni.endo.lp$HIST_LP==0),]
  )
mpg.fec.uni.endo.lp.means <-
  rep(NA, nrow(mpg.fec.uni.endo.lp),ncol=1)
for(i in 1:nrow(mpg.fec.uni.endo.lp.df)){
  mpg.fec.uni.endo.lp.means[i] <- 
    mean(c(
      mpg.fec.uni.endo.lp.df$FEC_SUV_LP_1[i],
      mpg.fec.uni.endo.lp.df$FEC_SUV_LP_2[i]), 
      na.rm = TRUE)
}
mpg.fec.uni.endo.lp.df <-
  cbind(
    mpg.fec.uni.endo.lp.df$HIST_LP,
    mpg.fec.uni.endo.lp.means)
colnames(mpg.fec.uni.endo.lp.df) <- c("HIST","MEAN")
#View(mpg.fec.uni.endo.lp.df)

#FEC ENDO RP
mpg.fec.uni.endo.rp.df <- 
  rbind(
    mpg.fec.uni.endo.rp[which(mpg.fec.uni.endo.rp$HIST_RP==1),],
    mpg.fec.uni.endo.rp[which(mpg.fec.uni.endo.rp$HIST_RP==0),]
  )
mpg.fec.uni.endo.rp.means <-
  rep(NA, nrow(mpg.fec.uni.endo.rp),ncol=1)
for(i in 1:nrow(mpg.fec.uni.endo.rp.df)){
  mpg.fec.uni.endo.rp.means[i] <- 
    mean(c(
      mpg.fec.uni.endo.rp.df$FEC_SUV_RP_1[i],
      mpg.fec.uni.endo.rp.df$FEC_SUV_RP_2[i]), 
      na.rm = TRUE)
}
mpg.fec.uni.endo.rp.df <-
  cbind(
    mpg.fec.uni.endo.rp.df$HIST_RP,
    mpg.fec.uni.endo.rp.means)
colnames(mpg.fec.uni.endo.rp.df) <- c("HIST","MEAN")
#View(mpg.fec.uni.endo.rp.df)

#FEC ENDO PELVIC
mpg.fec.uni.endo.pel.df <- 
  rbind(
    mpg.fec.uni.endo.rp.df,
    mpg.fec.uni.endo.lp.df
  )


#FEC ENDO PALN
mpg.fec.uni.endo.paln.df <- 
  rbind(
    mpg.fec.uni.endo.paln[which(mpg.fec.uni.endo.paln$HIST_PALN==1),],
    mpg.fec.uni.endo.paln[which(mpg.fec.uni.endo.paln$HIST_PALN==0),]
  )
mpg.fec.uni.endo.paln.means <-
  rep(NA, nrow(mpg.fec.uni.endo.paln),ncol=1)
for(i in 1:nrow(mpg.fec.uni.endo.paln.df)){
  mpg.fec.uni.endo.paln.means[i] <- 
    mean(c(
      mpg.fec.uni.endo.paln.df$FEC_SUV_PALN_1[i],
      mpg.fec.uni.endo.paln.df$FEC_SUV_PALN_2[i]), 
      na.rm = TRUE)
}
mpg.fec.uni.endo.paln.df <-
  cbind(
    mpg.fec.uni.endo.paln.df$HIST_PALN,
    mpg.fec.uni.endo.paln.means)
colnames(mpg.fec.uni.endo.paln.df) <- c("HIST","MEAN")
#View(mpg.fec.uni.endo.paln.df)

#FEC ENDO ALL
mpg.fec.uni.endo.all.df <- 
  rbind(
    mpg.fec.uni.endo.rp.df,
    mpg.fec.uni.endo.lp.df,
    mpg.fec.uni.endo.paln.df
  )

# 1.4.3 > MRI DFs ----
#MRI CER LP
mpg.mri.uni.cer.lp.df <- 
  rbind(
    mpg.mri.uni.cer.lp[which(mpg.mri.uni.cer.lp$HIST_LP==1),],
    mpg.mri.uni.cer.lp[which(mpg.mri.uni.cer.lp$HIST_LP==0),]
  )
mpg.mri.uni.cer.lp.means <-
  rep(NA, nrow(mpg.mri.uni.cer.lp),ncol=1)
for(i in 1:nrow(mpg.mri.uni.cer.lp.df)){
  mpg.mri.uni.cer.lp.means[i] <- 
    mean(c(
      mpg.mri.uni.cer.lp.df$MRI_ADC_LP_1[i],
      mpg.mri.uni.cer.lp.df$MRI_ADC_LP_2[i]), 
      na.rm = TRUE)
}
mpg.mri.uni.cer.lp.df <-
  cbind(
    mpg.mri.uni.cer.lp.df$HIST_LP,
    mpg.mri.uni.cer.lp.means)
colnames(mpg.mri.uni.cer.lp.df) <- c("HIST","MEAN")
#View(mpg.mri.uni.cer.lp.df)

#MRI CER RP
mpg.mri.uni.cer.rp.df <- 
  rbind(
    mpg.mri.uni.cer.rp[which(mpg.mri.uni.cer.rp$HIST_RP==1),],
    mpg.mri.uni.cer.rp[which(mpg.mri.uni.cer.rp$HIST_RP==0),]
  )
mpg.mri.uni.cer.rp.means <-
  rep(NA, nrow(mpg.mri.uni.cer.rp),ncol=1)
for(i in 1:nrow(mpg.mri.uni.cer.rp.df)){
  mpg.mri.uni.cer.rp.means[i] <- 
    mean(c(
      mpg.mri.uni.cer.rp.df$MRI_ADC_RP_1[i],
      mpg.mri.uni.cer.rp.df$MRI_ADC_RP_2[i]), 
      na.rm = TRUE)
}
mpg.mri.uni.cer.rp.df <-
  cbind(
    mpg.mri.uni.cer.rp.df$HIST_RP,
    mpg.mri.uni.cer.rp.means)
colnames(mpg.mri.uni.cer.rp.df) <- c("HIST","MEAN")
#View(mpg.mri.uni.cer.rp.df)

#MRI CER PELVIC
mpg.mri.uni.cer.pel.df <- 
  rbind(
    mpg.mri.uni.cer.rp.df,
    mpg.mri.uni.cer.lp.df
  )

#MRI ENDO LP
mpg.mri.uni.endo.lp.df <- 
  rbind(
    mpg.mri.uni.endo.lp[which(mpg.mri.uni.endo.lp$HIST_LP==1),],
    mpg.mri.uni.endo.lp[which(mpg.mri.uni.endo.lp$HIST_LP==0),]
  )
mpg.mri.uni.endo.lp.means <-
  rep(NA, nrow(mpg.mri.uni.endo.lp),ncol=1)
for(i in 1:nrow(mpg.mri.uni.endo.lp.df)){
  mpg.mri.uni.endo.lp.means[i] <- 
    mean(c(
      mpg.mri.uni.endo.lp.df$MRI_ADC_LP_1[i],
      mpg.mri.uni.endo.lp.df$MRI_ADC_LP_2[i]), 
      na.rm = TRUE)
}
mpg.mri.uni.endo.lp.df <-
  cbind(
    mpg.mri.uni.endo.lp.df$HIST_LP,
    mpg.mri.uni.endo.lp.means)
colnames(mpg.mri.uni.endo.lp.df) <- c("HIST","MEAN")
#View(mpg.mri.uni.endo.lp.df)

#MRI ENDO RP
mpg.mri.uni.endo.rp.df <- 
  rbind(
    mpg.mri.uni.endo.rp[which(mpg.mri.uni.endo.rp$HIST_RP==1),],
    mpg.mri.uni.endo.rp[which(mpg.mri.uni.endo.rp$HIST_RP==0),]
  )
mpg.mri.uni.endo.rp.means <-
  rep(NA, nrow(mpg.mri.uni.endo.rp),ncol=1)
for(i in 1:nrow(mpg.mri.uni.endo.rp.df)){
  mpg.mri.uni.endo.rp.means[i] <- 
    mean(c(
      mpg.mri.uni.endo.rp.df$MRI_ADC_RP_1[i],
      mpg.mri.uni.endo.rp.df$MRI_ADC_RP_2[i]), 
      na.rm = TRUE)
}
mpg.mri.uni.endo.rp.df <-
  cbind(
    mpg.mri.uni.endo.rp.df$HIST_RP,
    mpg.mri.uni.endo.rp.means)
colnames(mpg.mri.uni.endo.rp.df) <- c("HIST","MEAN")
#View(mpg.mri.uni.endo.rp.df)

#MRI ENDO PEL
mpg.mri.uni.endo.pel.df <- 
  rbind(
    mpg.mri.uni.endo.rp.df,
    mpg.mri.uni.endo.lp.df
  )

#MRI ENDO PALN
mpg.mri.uni.endo.paln.df <- 
  rbind(
    mpg.mri.uni.endo.paln[which(mpg.mri.uni.endo.paln$HIST_PALN==1),],
    mpg.mri.uni.endo.paln[which(mpg.mri.uni.endo.paln$HIST_PALN==0),]
  )
mpg.mri.uni.endo.paln.means <-
  rep(NA, nrow(mpg.mri.uni.endo.paln),ncol=1)
for(i in 1:nrow(mpg.mri.uni.endo.paln.df)){
  mpg.mri.uni.endo.paln.means[i] <- 
    mean(c(
      mpg.mri.uni.endo.paln.df$MRI_ADC_PALN_1[i],
      mpg.mri.uni.endo.paln.df$MRI_ADC_PALN_2[i]), 
      na.rm = TRUE)
}
mpg.mri.uni.endo.paln.df <-
  cbind(
    mpg.mri.uni.endo.paln.df$HIST_PALN,
    mpg.mri.uni.endo.paln.means)
colnames(mpg.mri.uni.endo.paln.df) <- c("HIST","MEAN")
#View(mpg.mri.uni.endo.paln.df)

#MRI ENDO ALL
mpg.mri.uni.endo.all.df <- 
  rbind(
    mpg.mri.uni.endo.rp.df,
    mpg.mri.uni.endo.lp.df,
    mpg.mri.uni.endo.paln.df
  )

## 1.5 - UNI - MANN-WHITNEY U-TEST ANALYSIS ----
# 1.5.1 > MWU-test analysis ----
mpg.fdg.uni.cer.lp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fdg.uni.cer.lp.df)
mpg.fdg.uni.cer.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fdg.uni.cer.rp.df)
mpg.fdg.uni.cer.pel.mwu <- wilcox.test(MEAN~HIST, data = mpg.fdg.uni.cer.pel.df)

mpg.fdg.uni.endo.lp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fdg.uni.endo.lp.df)
mpg.fdg.uni.endo.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fdg.uni.endo.rp.df)
mpg.fdg.uni.endo.paln.mwu <- wilcox.test(MEAN~HIST, data = mpg.fdg.uni.endo.paln.df)
mpg.fdg.uni.endo.pel.mwu <- wilcox.test(MEAN~HIST, data = mpg.fdg.uni.endo.pel.df)
mpg.fdg.uni.endo.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fdg.uni.endo.rp.df)

mpg.fec.uni.cer.lp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fec.uni.cer.lp.df)
mpg.fec.uni.cer.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fec.uni.cer.rp.df)
mpg.fec.uni.cer.pel.mwu <- wilcox.test(MEAN~HIST, data = mpg.fec.uni.cer.pel.df)

mpg.fec.uni.endo.lp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fec.uni.endo.lp.df)
mpg.fec.uni.endo.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fec.uni.endo.rp.df)
mpg.fec.uni.endo.paln.mwu <- wilcox.test(MEAN~HIST, data = mpg.fec.uni.endo.paln.df)
mpg.fec.uni.endo.pel.mwu <- wilcox.test(MEAN~HIST, data = mpg.fec.uni.endo.pel.df)
mpg.fec.uni.endo.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.fec.uni.endo.rp.df)

mpg.mri.uni.cer.lp.mwu <- wilcox.test(MEAN~HIST, data = mpg.mri.uni.cer.lp.df)
mpg.mri.uni.cer.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.mri.uni.cer.rp.df)
mpg.mri.uni.cer.pel.mwu <- wilcox.test(MEAN~HIST, data = mpg.mri.uni.cer.pel.df)

mpg.mri.uni.endo.lp.mwu <- wilcox.test(MEAN~HIST, data = mpg.mri.uni.endo.lp.df)
mpg.mri.uni.endo.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.mri.uni.endo.rp.df)
mpg.mri.uni.endo.paln.mwu <- wilcox.test(MEAN~HIST, data = mpg.mri.uni.endo.paln.df)
mpg.mri.uni.endo.pel.mwu <- wilcox.test(MEAN~HIST, data = mpg.mri.uni.endo.pel.df)
mpg.mri.uni.endo.rp.mwu <- wilcox.test(MEAN~HIST, data = mpg.mri.uni.endo.rp.df)

# 1.5.2 > Store MWU-test results ----
mpg.uni.mwu.results.rows <- c(
  "FDG CER LP", 
  "FDG CER RP",
  "FDG CER PEL",
  "FDG ENDO LP", 
  "FDG ENDO RP",
  "FDG ENDO PALN",
  "FDG ENDO PEL",
  "FDG ENDO ALL",
  "FEC CER LP",
  "FEC CER RP",
  "FEC CER PEL",
  "FEC ENDO LP",
  "FEC ENDO RP",
  "FEC ENDO PALN",
  "FEC ENDO PEL",
  "FEC ENDO ALL",
  "MRI CER LP",
  "MRI CER RP",
  "MRI CER PEL",
  "MRI ENDO LP",
  "MRI ENDO RP",
  "MRI ENDO PALN",
  "MRI ENDO PEL",
  "MRI ENDO ALL"
  )

mpg.uni.mwu.results <- list(
  mpg.fdg.uni.cer.lp.mwu,
  mpg.fdg.uni.cer.rp.mwu,
  mpg.fdg.uni.cer.pel.mwu,
  mpg.fdg.uni.endo.lp.mwu,
  mpg.fdg.uni.endo.rp.mwu,
  mpg.fdg.uni.endo.paln.mwu,
  mpg.fdg.uni.endo.pel.mwu,
  mpg.fdg.uni.endo.rp.mwu,
  mpg.fec.uni.cer.lp.mwu,
  mpg.fec.uni.cer.rp.mwu,
  mpg.fec.uni.cer.pel.mwu,
  mpg.fec.uni.endo.lp.mwu,
  mpg.fec.uni.endo.rp.mwu,
  mpg.fec.uni.endo.paln.mwu,
  mpg.fec.uni.endo.pel.mwu,
  mpg.fec.uni.endo.rp.mwu,
  mpg.mri.uni.cer.lp.mwu,
  mpg.mri.uni.cer.rp.mwu,
  mpg.mri.uni.cer.pel.mwu,
  mpg.mri.uni.endo.lp.mwu,
  mpg.mri.uni.endo.rp.mwu,
  mpg.mri.uni.endo.paln.mwu,
  mpg.mri.uni.endo.pel.mwu,
  mpg.mri.uni.endo.rp.mwu
  )

mpg.uni.mwu.results.df <- data.frame(matrix(NA, ncol=2, nrow=length(mpg.uni.mwu.results)))
colnames(mpg.uni.mwu.results.df) <- c("W-statistic", "p-value")
rownames(mpg.uni.mwu.results.df) <- mpg.uni.mwu.results.rows

for(i in 1:length(mpg.uni.mwu.results)){
  mpg.uni.mwu.results.df[i,1] <- mpg.uni.mwu.results[[i]][1]
  mpg.uni.mwu.results.df[i,2] <- mpg.uni.mwu.results[[i]][3]
}

#Adjust p-values for multiple testing
mpg.uni.mwu.results.df$adj.p.val <- p.adjust(mpg.uni.mwu.results.df$`p-value`, method="fdr")

## 1.6 - Visualise results with boxplots (active) ----
library(ggpubr)
#Multi-plot 1 - FDG, FEC & MRI in ENDO
#Plot 1
mpg.fdg.uni.endo.all.df <- as.data.frame(mpg.fdg.uni.endo.all.df)
mpg.fdg.uni.endo.all.df$HIST <- factor(mpg.fdg.uni.endo.all.df$HIST)
mpg.fdg.uni.endo.all.bp <- ggplot(data = as.data.frame(mpg.fdg.uni.endo.all.df),
                                  aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  geom_bracket(
    xmin = "0", xmax = "1",
    y.position = 23,
    label = "*",
    tip.length = c(0.4,0.05),
    size = 0.5,
    inherit.aes = FALSE) +
  labs(x="",y=expression(SUV[max])) +
  annotate("text", x = 1.5, y = 20, label = expression(rho<".01")) + 
  scale_x_discrete(labels= c("n=36","n=24")) +
  ylim(0,25) 
#Plot 2
mpg.fdg.uni.endo.pel.df <- as.data.frame(mpg.fdg.uni.endo.pel.df)
mpg.fdg.uni.endo.pel.df$HIST <- factor(mpg.fdg.uni.endo.pel.df$HIST)
mpg.fdg.uni.endo.pel.bp <- ggplot(
  data = as.data.frame(mpg.fdg.uni.endo.pel.df),aes(x=HIST,y=MEAN,color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  geom_bracket(xmin = "0", 
               xmax = "1",
               y.position = 23,
               label = "*",
               tip.length = c(0.5,0.05),
               size = 0.5,
               inherit.aes = FALSE) + 
  annotate("text", x = 1.5, y = 20, label = expression(rho<".01")) + 
  scale_x_discrete(labels= c("n=30","n=20")) +
  labs(x="",y="") +
  ylim(0,25)
#Plot 3
mpg.fdg.uni.endo.lp.df <- as.data.frame(mpg.fdg.uni.endo.lp.df)
mpg.fdg.uni.endo.lp.df$HIST <- factor(mpg.fdg.uni.endo.lp.df$HIST)
mpg.fdg.uni.endo.lp.bp <- ggplot(data = as.data.frame(mpg.fdg.uni.endo.lp.df),
                                 aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  geom_bracket(xmin = "0", xmax = "1",
               y.position = 23,
               label = "*",
               tip.length = c(0.6,0.05),
               size = 0.5,
               inherit.aes = FALSE) + 
  annotate("text", x = 1.5, y = 20, label = expression(rho==".01"))+ 
  scale_x_discrete(labels= c("n=15","n=8")) +
  labs(x="",y="") +
  ylim(0,25)
#Plot 4
mpg.fdg.uni.endo.rp.df <- as.data.frame(mpg.fdg.uni.endo.rp.df)
mpg.fdg.uni.endo.rp.df$HIST <- factor(mpg.fdg.uni.endo.rp.df$HIST)
mpg.fdg.uni.endo.rp.bp <- ggplot(data = as.data.frame(mpg.fdg.uni.endo.rp.df),
                                aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  geom_bracket(xmin = "0", xmax = "1",
               y.position = 23,
               label = "*",
               tip.length = c(0.35,0.05),
               size = 0.5,
               inherit.aes = FALSE) + 
  annotate("text", x = 1.5, y = 20, label = expression(rho<".01")) + 
  scale_x_discrete(labels= c("n=15","n=12")) +
  labs(x="",y="") +
  ylim(0,25)
#Plot 5
mpg.fdg.uni.endo.paln.df <- as.data.frame(mpg.fdg.uni.endo.paln.df)
mpg.fdg.uni.endo.paln.df$HIST <- factor(mpg.fdg.uni.endo.paln.df$HIST)
mpg.fdg.uni.endo.paln.bp <- ggplot(data = as.data.frame(mpg.fdg.uni.endo.paln.df),
                                   aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,25) +
  scale_x_discrete(labels= c("n=6","n=4")) + 
  annotate("text", x = 1.5, y = 20, label = expression(rho==".70"))

#Plot 6
mpg.fec.uni.endo.all.df <- as.data.frame(mpg.fec.uni.endo.all.df)
mpg.fec.uni.endo.all.df$HIST <- factor(mpg.fec.uni.endo.all.df$HIST)
mpg.fec.uni.endo.all.bp <- ggplot(data = as.data.frame(mpg.fec.uni.endo.all.df),
                                  aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y=expression(SUV[max])) +
  ylim(0,10) +
  scale_x_discrete(labels= c("n=22","n=9")) + 
  annotate("text", x = 1.5, y = 8, label = expression(rho==".23"))
#Plot 7
mpg.fec.uni.endo.pel.df <- as.data.frame(mpg.fec.uni.endo.pel.df)
mpg.fec.uni.endo.pel.df$HIST <- factor(mpg.fec.uni.endo.pel.df$HIST)
mpg.fec.uni.endo.pel.bp <- ggplot(data = as.data.frame(mpg.fec.uni.endo.pel.df),
                                  aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,10) +
  scale_x_discrete(labels= c("n=17","n=8")) + 
  annotate("text", x = 1.5, y = 8, label = expression(rho==".081"))

#Plot 8
mpg.fec.uni.endo.lp.df <- as.data.frame(mpg.fec.uni.endo.lp.df)
mpg.fec.uni.endo.lp.df$HIST <- factor(mpg.fec.uni.endo.lp.df$HIST)
mpg.fec.uni.endo.lp.bp <- ggplot(data = as.data.frame(mpg.fec.uni.endo.lp.df),
                                 aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,10) + 
  scale_x_discrete(labels= c("n=10","n=3")) + 
  annotate("text", x = 1.5, y = 8, label = expression(rho==".56"))
#Plot 9
mpg.fec.uni.endo.rp.df <- as.data.frame(mpg.fec.uni.endo.rp.df)
mpg.fec.uni.endo.rp.df$HIST <- factor(mpg.fec.uni.endo.rp.df$HIST)
mpg.fec.uni.endo.rp.bp <- ggplot(data = as.data.frame(mpg.fec.uni.endo.rp.df),
                                 aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,10) + 
  scale_x_discrete(labels= c("n=7","n=5"))+ 
  annotate("text", x = 1.5, y = 8, label = expression(rho==".23"))
#Plot 10
mpg.fec.uni.endo.paln.df <- as.data.frame(mpg.fec.uni.endo.paln.df)
mpg.fec.uni.endo.paln.df$HIST <- factor(mpg.fec.uni.endo.paln.df$HIST)
mpg.fec.uni.endo.paln.bp <- ggplot(data = as.data.frame(mpg.fec.uni.endo.paln.df),
                                   aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,10) + 
  scale_x_discrete(labels= c("n=5","n=1"))+ 
  annotate("text", x = 1.5, y = 8, label = expression(rho=="1.0"))
#Plot 11
mpg.mri.uni.endo.all.df <- as.data.frame(mpg.mri.uni.endo.all.df)
mpg.mri.uni.endo.all.df$HIST <- factor(mpg.mri.uni.endo.all.df$HIST)
mpg.mri.uni.endo.all.bp <- ggplot(data = as.data.frame(mpg.mri.uni.endo.all.df),
                                  aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y=expression(ADC[mean])) +
  ylim(0,1.5) + 
  scale_x_discrete(labels= c("n=78","n=28")) + 
  annotate("text", x = 1.5, y = 1.4, label = expression(rho==".32")) 
#Plot 12
mpg.mri.uni.endo.pel.df <- as.data.frame(mpg.mri.uni.endo.pel.df)
mpg.mri.uni.endo.pel.df$HIST <- factor(mpg.mri.uni.endo.pel.df$HIST)
mpg.mri.uni.endo.pel.bp <- ggplot(data = as.data.frame(mpg.mri.uni.endo.pel.df),
                                  aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.5) + 
  scale_x_discrete(labels= c("n=68","n=24")) + 
  annotate("text", x = 1.5, y = 1.4, label = expression(rho==".23"))

#Plot 13
mpg.mri.uni.endo.lp.df <- as.data.frame(mpg.mri.uni.endo.lp.df)
mpg.mri.uni.endo.lp.df$HIST <- factor(mpg.mri.uni.endo.lp.df$HIST)
mpg.mri.uni.endo.lp.bp <- ggplot(data = as.data.frame(mpg.mri.uni.endo.lp.df),
                                 aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.5) + 
  scale_x_discrete(labels= c("n=34","n=10")) + 
  annotate("text", x = 1.5, y = 1.4, label = expression(rho==".49"))
#Plot 14
mpg.mri.uni.endo.rp.df <- as.data.frame(mpg.mri.uni.endo.rp.df)
mpg.mri.uni.endo.rp.df$HIST <- factor(mpg.mri.uni.endo.rp.df$HIST)
mpg.mri.uni.endo.rp.bp <- ggplot(data = as.data.frame(mpg.mri.uni.endo.rp.df),
                                 aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.5) + 
  scale_x_discrete(labels= c("n=34","n=14")) + 
  annotate("text", x = 1.5, y = 1.4, label = expression(rho==".32"))
#Plot 15
mpg.mri.uni.endo.paln.df <- as.data.frame(mpg.mri.uni.endo.paln.df)
mpg.mri.uni.endo.paln.df$HIST <- factor(mpg.mri.uni.endo.paln.df$HIST)
mpg.mri.uni.endo.paln.bp <- ggplot(data = as.data.frame(mpg.mri.uni.endo.paln.df),
                                   aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.5) + 
  scale_x_discrete(labels= c("n=10","n=4")) + 
  annotate("text", x = 1.5, y = 1.4, label = expression(rho==".46"))

png("FDG_FEC_MRI_ENDO_UNI_BP.png",width = 12, height = 7, units ='in', res = 300)
ggarrange(mpg.fdg.uni.endo.all.bp,
          mpg.fdg.uni.endo.pel.bp,
          mpg.fdg.uni.endo.lp.bp,
          mpg.fdg.uni.endo.rp.bp,
          mpg.fdg.uni.endo.paln.bp,
          mpg.fec.uni.endo.all.bp,
          mpg.fec.uni.endo.pel.bp,
          mpg.fec.uni.endo.lp.bp,
          mpg.fec.uni.endo.rp.bp,
          mpg.fec.uni.endo.paln.bp,
          mpg.mri.uni.endo.all.bp,
          mpg.mri.uni.endo.pel.bp,
          mpg.mri.uni.endo.lp.bp,
          mpg.mri.uni.endo.rp.bp,
          mpg.mri.uni.endo.paln.bp,
          label.x = -.05,
          labels = c("A1","A2","A3","A4","A5","B1","B2","B3","B4","B5","C1","C2","C3","C4","C5"),
          ncol = 5, nrow = 3,
          align = "hv",
          common.legend = TRUE)
dev.off()

##Multi-plot 2 - FDG, FEC, MRI in CER
#Plot 1
mpg.fdg.uni.cer.pel.df <- as.data.frame(mpg.fdg.uni.cer.pel.df)
mpg.fdg.uni.cer.pel.df$HIST <- factor(mpg.fdg.uni.cer.pel.df$HIST)
mpg.fdg.uni.cer.pel.bp <- ggplot(data = as.data.frame(mpg.fdg.uni.cer.pel.df),
                                 aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y=expression(SUV[max])) +
  ylim(0,10) + scale_x_discrete(labels= c("n=9","n=5"))+ annotate(
                                       "text", x = 1.5, y = 7.5, label = expression(rho==".46"), size = 3.5) +
  font("ylab", size = 13)
#Plot 2
mpg.fdg.uni.cer.lp.df <- as.data.frame(mpg.fdg.uni.cer.lp.df)
mpg.fdg.uni.cer.lp.df$HIST <- factor(mpg.fdg.uni.cer.lp.df$HIST)
mpg.fdg.uni.cer.lp.bp <- ggplot(data = as.data.frame(mpg.fdg.uni.cer.lp.df),
                                aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,10) + scale_x_discrete(labels= c("n=4","n=4"))+ annotate(
                                       "text", x = 1.5, y = 7.5, label = expression(rho==".17"), size = 3.5) +
  font("ylab", size = 13)
#Plot 3
mpg.fdg.uni.cer.rp.df <- as.data.frame(mpg.fdg.uni.cer.rp.df)
mpg.fdg.uni.cer.rp.df$HIST <- factor(mpg.fdg.uni.cer.rp.df$HIST)
mpg.fdg.uni.cer.rp.bp <-ggplot(data = as.data.frame(mpg.fdg.uni.cer.rp.df),
                               aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,10) + scale_x_discrete(labels= c("n=5","n=1"))+ annotate(
                                      "text", x = 1.5, y = 7.5, label = expression(rho==".49"), size = 3.5) +
  font("ylab", size = 13)
#Plot 4
mpg.fec.uni.cer.pel.df <- as.data.frame(mpg.fec.uni.cer.pel.df)
mpg.fec.uni.cer.pel.df$HIST <- factor(mpg.fec.uni.cer.pel.df$HIST)
mpg.fec.uni.cer.pel.bp <- ggplot(data = as.data.frame(mpg.fec.uni.cer.pel.df),
                                 aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y=expression(SUV[max])) +
  ylim(0,5) + scale_x_discrete(labels= c("n=6","n=3"))+ annotate(
                                       "text", x = 1.5, y = 4.5, label = expression(rho==".46"), size = 3.5) +
  font("ylab", size = 13)
#Plot 5
mpg.fec.uni.cer.lp.df <- as.data.frame(mpg.fec.uni.cer.lp.df)
mpg.fec.uni.cer.lp.df$HIST <- factor(mpg.fec.uni.cer.lp.df$HIST)
mpg.fec.uni.cer.lp.bp <- ggplot(data = as.data.frame(mpg.fec.uni.cer.lp.df),
                                aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,5) + scale_x_discrete(labels= c("n=3","n=2"))+ annotate(
                                       "text", x = 1.5, y = 4.5, label = expression(rho==".87"), size = 3.5) +
  font("ylab", size = 13)
#Plot 6
mpg.fec.uni.cer.rp.df <- as.data.frame(mpg.fec.uni.cer.rp.df)
mpg.fec.uni.cer.rp.df$HIST <- factor(mpg.fec.uni.cer.rp.df$HIST)
mpg.fec.uni.cer.rp.bp <- ggplot(data = as.data.frame(mpg.fec.uni.cer.rp.df),
                                aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,5) + scale_x_discrete(labels= c("n=3","n=1"))+ annotate(
                                      "text", x = 1.5, y = 4.5, label = expression(rho=="1.0"), size = 3.5) +
  font("ylab", size = 13)
#Plot 7
mpg.mri.uni.cer.pel.df <- as.data.frame(mpg.mri.uni.cer.pel.df)
mpg.mri.uni.cer.pel.df$HIST <- factor(mpg.mri.uni.cer.pel.df$HIST)
mpg.mri.uni.cer.pel.bp <- ggplot(data = as.data.frame(mpg.mri.uni.cer.pel.df),
                                 aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y=expression(ADC[mean])) +
  ylim(0,2) + scale_x_discrete(labels= c("n=32","n=9"))+ annotate(
                                       "text", x = 1.5, y =1.6, label = expression(rho==".14"), size = 3.5) +
  font("ylab", size = 13)
#Plot 8
mpg.mri.uni.cer.lp.df <- as.data.frame(mpg.mri.uni.cer.lp.df)
mpg.mri.uni.cer.lp.df$HIST <- factor(mpg.mri.uni.cer.lp.df$HIST)
mpg.mri.uni.cer.lp.bp <- ggplot(data = as.data.frame(mpg.mri.uni.cer.lp.df),
                                aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,2) + scale_x_discrete(labels= c("n=15","n=5"))+ annotate(
                                       "text", x = 1.5, y = 1.6, label = expression(rho==".49"), size = 3.5) +
  font("ylab", size = 13)
#Plot 9
mpg.mri.uni.cer.rp.df <- as.data.frame(mpg.mri.uni.cer.rp.df)
mpg.mri.uni.cer.rp.df$HIST <- factor(mpg.mri.uni.cer.rp.df$HIST)
mpg.mri.uni.cer.rp.bp <- ggplot(data = as.data.frame(mpg.mri.uni.cer.rp.df),
                               aes(x = HIST,y= MEAN, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,2) + scale_x_discrete(labels= c("n=17","n=4")) + 
  annotate( "text", x = 1.5, y = 1.6, label = expression(rho==".17"), size = 3.5) +
  font("ylab", size = 13)

png("FDG_FEC_MRI_CER_UNI_BP.png", width = 6, height = 6, units ='in', res = 300)
ggarrange(mpg.fdg.uni.cer.pel.bp,
          mpg.fdg.uni.cer.lp.bp,
          mpg.fdg.uni.cer.rp.bp,
          mpg.fec.uni.cer.pel.bp,
          mpg.fec.uni.cer.lp.bp,
          mpg.fec.uni.cer.rp.bp,
          mpg.mri.uni.cer.pel.bp,
          mpg.mri.uni.cer.lp.bp,
          mpg.mri.uni.cer.rp.bp,
          label.x = -0.07,
          align = "hv",
          common.legend = TRUE,
          labels = c("A1","A2","A3","B1","B2","B3","C1","C2","C3"),
          ncol = 3, nrow = 3)
dev.off()



#....----
## 2.1 - Nodal-to-tumour ratio ----
#We have data for: 
#FDG: CER LP, CER RP, ENDO LP, ENDO RP, ENDO PALN
#FEC: CER LP, CER RP, ENDO LP, ENDO RP, ENDO PALN
#MRI: CER LP, CER RP, ENDO LP, ENDO RP, ENDO PALN
# 2.1.1 > FDG TNR raw dataframes ----
#CER LP
fdg.ntr.lp.columns <- c(
  "PATIENT",
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "FDG_SUV_PT",
  "MRI_SA_LP",
  "HIST_LP")

mpg.fdg.ntr.cer.lp <- mpg.fdg.uni.cer[,fdg.ntr.lp.columns]
if(sum(is.na(mpg.fdg.ntr.cer.lp$HIST_LP))!=0){
  mpg.fdg.ntr.cer.lp <- mpg.fdg.ntr.cer.lp[-which(is.na(mpg.fdg.ntr.cer.lp$HIST_LP)),]
  mpg.fdg.ntr.cer.lp <- mpg.fdg.ntr.cer.lp[-which(is.na(mpg.fdg.ntr.cer.lp$FDG_SUV_PT)),]
  mpg.fdg.ntr.cer.lp <- mpg.fdg.ntr.cer.lp[-which(is.na(
    mpg.fdg.ntr.cer.lp$FDG_SUV_LP_1|
      mpg.fdg.ntr.cer.lp$FDG_SUV_LP_2)),]
} else {
  mpg.fdg.ntr.cer.lp <- mpg.fdg.ntr.cer.lp[-which(is.na(mpg.fdg.ntr.cer.lp$FDG_SUV_PT)),]
  mpg.fdg.ntr.cer.lp <- mpg.fdg.ntr.cer.lp[-which(is.na(
    mpg.fdg.ntr.cer.lp$FDG_SUV_LP_1|
      mpg.fdg.ntr.cer.lp$FDG_SUV_LP_2)),]
}
##View(mpg.fdg.ntr.cer.lp)

#CER RP
fdg.ntr.rp.columns <- c(
  "PATIENT",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "FDG_SUV_PT",
  "MRI_SA_RP",
  "HIST_RP")

mpg.fdg.ntr.cer.rp <- mpg.fdg.uni.cer[,fdg.ntr.rp.columns]
if(sum(is.na(mpg.fdg.ntr.cer.rp$HIST_RP))!=0){
  mpg.fdg.ntr.cer.rp <- mpg.fdg.ntr.cer.rp[-which(is.na(mpg.fdg.ntr.cer.rp$HIST_RP)),]
  mpg.fdg.ntr.cer.rp <- mpg.fdg.ntr.cer.rp[-which(is.na(mpg.fdg.ntr.cer.rp$FDG_SUV_PT)),]
  mpg.fdg.ntr.cer.rp <- mpg.fdg.ntr.cer.rp[-which(is.na(
    mpg.fdg.ntr.cer.rp$FDG_SUV_RP_1|
      mpg.fdg.ntr.cer.rp$FDG_SUV_RP_2)),]
} else {
  mpg.fdg.ntr.cer.rp <- mpg.fdg.ntr.cer.rp[-which(is.na(mpg.fdg.ntr.cer.rp$FDG_SUV_PT)),]
  mpg.fdg.ntr.cer.rp <- mpg.fdg.ntr.cer.rp[-which(is.na(
    mpg.fdg.ntr.cer.rp$FDG_SUV_RP_1|
      mpg.fdg.ntr.cer.rp$FDG_SUV_RP_2)),]
}
##View(mpg.fdg.ntr.cer.rp)

#ENDO LP
mpg.fdg.ntr.endo.lp <- mpg.fdg.uni.endo[,fdg.ntr.lp.columns]
if(sum(is.na(mpg.fdg.ntr.endo.lp$HIST_LP))!=0){
  mpg.fdg.ntr.endo.lp <- mpg.fdg.ntr.endo.lp[-which(is.na(mpg.fdg.ntr.endo.lp$HIST_LP)),]
  mpg.fdg.ntr.endo.lp <- mpg.fdg.ntr.endo.lp[-which(is.na(mpg.fdg.ntr.endo.lp$FDG_SUV_PT)),]
  mpg.fdg.ntr.endo.lp <- mpg.fdg.ntr.endo.lp[-which(is.na(
    mpg.fdg.ntr.endo.lp$FDG_SUV_LP_1|
      mpg.fdg.ntr.endo.lp$FDG_SUV_LP_2)),]
} else {
  mpg.fdg.ntr.endo.lp <- mpg.fdg.ntr.endo.lp[-which(is.na(mpg.fdg.ntr.endo.lp$FDG_SUV_PT)),]
  mpg.fdg.ntr.endo.lp <- mpg.fdg.ntr.endo.lp[-which(is.na(
    mpg.fdg.ntr.endo.lp$FDG_SUV_LP_1|
      mpg.fdg.ntr.endo.lp$FDG_SUV_LP_2)),]
}
##View(mpg.fdg.ntr.endo.lp)

#ENDO RP
mpg.fdg.ntr.endo.rp <- mpg.fdg.uni.endo[,fdg.ntr.rp.columns]
if(sum(is.na(mpg.fdg.ntr.endo.rp$HIST_RP))!=0){
  mpg.fdg.ntr.endo.rp <- mpg.fdg.ntr.endo.rp[-which(is.na(mpg.fdg.ntr.endo.rp$HIST_RP)),]
  mpg.fdg.ntr.endo.rp <- mpg.fdg.ntr.endo.rp[-which(is.na(mpg.fdg.ntr.endo.rp$FDG_SUV_PT)),]
  mpg.fdg.ntr.endo.rp <- mpg.fdg.ntr.endo.rp[-which(is.na(
    mpg.fdg.ntr.endo.rp$FDG_SUV_RP_1|
      mpg.fdg.ntr.endo.rp$FDG_SUV_RP_2)),]
} else {
  mpg.fdg.ntr.endo.rp <- mpg.fdg.ntr.endo.rp[-which(is.na(mpg.fdg.ntr.endo.rp$FDG_SUV_PT)),]
  mpg.fdg.ntr.endo.rp <- mpg.fdg.ntr.endo.rp[-which(is.na(
    mpg.fdg.ntr.endo.rp$FDG_SUV_RP_1|
      mpg.fdg.ntr.endo.rp$FDG_SUV_RP_2)),]
}
##View(mpg.fdg.ntr.endo.rp)

#ENDO PALN
fdg.ntr.paln.columns <- c(
  "PATIENT",
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "FDG_SUV_PT",
  "MRI_SA_PALN",
  "HIST_PALN")

mpg.fdg.ntr.endo.paln <- mpg.fdg.uni.endo[,fdg.ntr.paln.columns]
if(sum(is.na(mpg.fdg.ntr.endo.paln$HIST_PALN))!=0){
  mpg.fdg.ntr.endo.paln <- mpg.fdg.ntr.endo.paln[-which(is.na(mpg.fdg.ntr.endo.paln$HIST_PALN)),]
  mpg.fdg.ntr.endo.paln <- mpg.fdg.ntr.endo.paln[-which(is.na(mpg.fdg.ntr.endo.paln$FDG_SUV_PT)),]
  mpg.fdg.ntr.endo.paln <- mpg.fdg.ntr.endo.paln[-which(is.na(
    mpg.fdg.ntr.endo.paln$FDG_SUV_PALN_1|
      mpg.fdg.ntr.endo.paln$FDG_SUV_PALN_2)),]
} else {
  mpg.fdg.ntr.endo.paln <- mpg.fdg.ntr.endo.paln[-which(is.na(mpg.fdg.ntr.endo.paln$FDG_SUV_PT)),]
  mpg.fdg.ntr.endo.paln <- mpg.fdg.ntr.endo.paln[-which(is.na(
    mpg.fdg.ntr.endo.paln$FDG_SUV_PALN_1|
      mpg.fdg.ntr.endo.paln$FDG_SUV_PALN_2)),]
}
##View(mpg.fdg.ntr.endo.paln)

# 2.1.2 > FEC TNR raw dataframes ----

#CER LP
fec.ntr.lp.columns <- c(
  "PATIENT",
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "FEC_SUV_PT",
  "MRI_SA_LP",
  "HIST_LP")
mpg.fec.ntr.cer.lp <- mpg.fec.uni.cer[,fec.ntr.lp.columns]
if(sum(is.na(mpg.fec.ntr.cer.lp$HIST_LP))!=0){
  mpg.fec.ntr.cer.lp <- mpg.fec.ntr.cer.lp[-which(is.na(mpg.fec.ntr.cer.lp$HIST_LP)),]
  mpg.fec.ntr.cer.lp <- mpg.fec.ntr.cer.lp[-which(is.na(mpg.fec.ntr.cer.lp$FEC_SUV_PT)),]
  mpg.fec.ntr.cer.lp <- mpg.fec.ntr.cer.lp[-which(is.na(
    mpg.fec.ntr.cer.lp$FEC_SUV_LP_1|
      mpg.fec.ntr.cer.lp$FEC_SUV_LP_2)),]
} else {
  mpg.fec.ntr.cer.lp <- mpg.fec.ntr.cer.lp[-which(is.na(mpg.fec.ntr.cer.lp$FEC_SUV_PT)),]
  mpg.fec.ntr.cer.lp <- mpg.fec.ntr.cer.lp[-which(is.na(
    mpg.fec.ntr.cer.lp$FEC_SUV_LP_1|
      mpg.fec.ntr.cer.lp$FEC_SUV_LP_2)),]
}
#View(mpg.fec.ntr.cer.lp)

#CER RP
fec.ntr.rp.columns <- c(
  "PATIENT",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "FEC_SUV_PT",
  "MRI_SA_RP",
  "HIST_RP")

mpg.fec.ntr.cer.rp <- mpg.fec.uni.cer[,fec.ntr.rp.columns]
#View(mpg.fec.ntr.cer.rp)
if(sum(is.na(mpg.fec.ntr.cer.rp$HIST_RP))!=0){
  mpg.fec.ntr.cer.rp <- mpg.fec.ntr.cer.rp[-which(is.na(mpg.fec.ntr.cer.rp$HIST_RP)),]
  mpg.fec.ntr.cer.rp <- mpg.fec.ntr.cer.rp[-which(is.na(mpg.fec.ntr.cer.rp$FEC_SUV_PT)),]
  mpg.fec.ntr.cer.rp <- mpg.fec.ntr.cer.rp[-which(is.na(
    mpg.fec.ntr.cer.rp$FEC_SUV_RP_1|
      mpg.fec.ntr.cer.rp$FEC_SUV_RP_2)),]
} else {
  mpg.fec.ntr.cer.rp <- mpg.fec.ntr.cer.rp[-which(is.na(mpg.fec.ntr.cer.rp$FEC_SUV_PT)),]
  mpg.fec.ntr.cer.rp <- mpg.fec.ntr.cer.rp[-which(is.na(
    mpg.fec.ntr.cer.rp$FEC_SUV_RP_1|
      mpg.fec.ntr.cer.rp$FEC_SUV_RP_2)),]
}

#ENDO LP
mpg.fec.ntr.endo.lp <- mpg.fec.uni.endo[,fec.ntr.lp.columns]
#View(mpg.fec.ntr.endo.lp)
if(sum(is.na(mpg.fec.ntr.endo.lp$HIST_LP))!=0){
  mpg.fec.ntr.endo.lp <- mpg.fec.ntr.endo.lp[-which(is.na(mpg.fec.ntr.endo.lp$HIST_LP)),]
  mpg.fec.ntr.endo.lp <- mpg.fec.ntr.endo.lp[-which(is.na(mpg.fec.ntr.endo.lp$FEC_SUV_PT)),]
  mpg.fec.ntr.endo.lp <- mpg.fec.ntr.endo.lp[-which(is.na(
    mpg.fec.ntr.endo.lp$FEC_SUV_LP_1|
      mpg.fec.ntr.endo.lp$FEC_SUV_LP_2)),]
} else {
  mpg.fec.ntr.endo.lp <- mpg.fec.ntr.endo.lp[-which(is.na(mpg.fec.ntr.endo.lp$FEC_SUV_PT)),]
  mpg.fec.ntr.endo.lp <- mpg.fec.ntr.endo.lp[-which(is.na(
    mpg.fec.ntr.endo.lp$FEC_SUV_LP_1|
      mpg.fec.ntr.endo.lp$FEC_SUV_LP_2)),]
}


#ENDO RP
mpg.fec.ntr.endo.rp <- mpg.fec.uni.endo[,fec.ntr.rp.columns]
#View(mpg.fec.ntr.endo.rp)
if(sum(is.na(mpg.fec.ntr.endo.rp$HIST_RP))!=0){
  mpg.fec.ntr.endo.rp <- mpg.fec.ntr.endo.rp[-which(is.na(mpg.fec.ntr.endo.rp$HIST_RP)),]
  mpg.fec.ntr.endo.rp <- mpg.fec.ntr.endo.rp[-which(is.na(mpg.fec.ntr.endo.rp$FEC_SUV_PT)),]
  mpg.fec.ntr.endo.rp <- mpg.fec.ntr.endo.rp[-which(is.na(
    mpg.fec.ntr.endo.rp$FEC_SUV_RP_1|
      mpg.fec.ntr.endo.rp$FEC_SUV_RP_2)),]
} else {
  mpg.fec.ntr.endo.rp <- mpg.fec.ntr.endo.rp[-which(is.na(mpg.fec.ntr.endo.rp$FEC_SUV_PT)),]
  mpg.fec.ntr.endo.rp <- mpg.fec.ntr.endo.rp[-which(is.na(
    mpg.fec.ntr.endo.rp$FEC_SUV_RP_1|
      mpg.fec.ntr.endo.rp$FEC_SUV_RP_2)),]
}

#ENDO PALN
fec.ntr.paln.columns <- c(
  "PATIENT",
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "FEC_SUV_PT",
  "MRI_SA_PALN",
  "HIST_PALN")

mpg.fec.ntr.endo.paln <- mpg.fec.uni.endo[,fec.ntr.paln.columns]
#View(mpg.fec.ntr.endo.paln)
if(sum(is.na(mpg.fec.ntr.endo.paln$HIST_PALN))!=0){
  mpg.fec.ntr.endo.paln <- mpg.fec.ntr.endo.paln[-which(is.na(mpg.fec.ntr.endo.paln$HIST_PALN)),]
  mpg.fec.ntr.endo.paln <- mpg.fec.ntr.endo.paln[-which(is.na(mpg.fec.ntr.endo.paln$FEC_SUV_PT)),]
  mpg.fec.ntr.endo.paln <- mpg.fec.ntr.endo.paln[-which(is.na(
    mpg.fec.ntr.endo.paln$FEC_SUV_PALN_1|
      mpg.fec.ntr.endo.paln$FEC_SUV_PALN_2)),]
} else {
  mpg.fec.ntr.endo.paln <- mpg.fec.ntr.endo.paln[-which(is.na(mpg.fec.ntr.endo.paln$FEC_SUV_PT)),]
  mpg.fec.ntr.endo.paln <- mpg.fec.ntr.endo.paln[-which(is.na(
    mpg.fec.ntr.endo.paln$FEC_SUV_PALN_1|
      mpg.fec.ntr.endo.paln$FEC_SUV_PALN_2)),]
}

# 2.1.3 > MRI TNR raw dataframes ----
#CER LP
mri.ntr.lp.columns <- c(
  "PATIENT",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_ADC_PT_1",
  "MRI_ADC_PT_2",
  "MRI_SA_LP",
  "HIST_LP")
mpg.mri.ntr.cer.lp <- mpg.mri.uni.cer[,mri.ntr.lp.columns]
#View(mpg.mri.ntr.cer.lp)
if(sum(is.na(mpg.mri.ntr.cer.lp$HIST_LP))!=0){
  mpg.mri.ntr.cer.lp <- mpg.mri.ntr.cer.lp[-which(is.na(mpg.mri.ntr.cer.lp$HIST_LP)),]
  mpg.mri.ntr.cer.lp <- mpg.mri.ntr.cer.lp[-which(is.na(
    mpg.mri.ntr.cer.lp$MRI_ADC_PT_1|
      mpg.mri.ntr.cer.lp$MRI_ADC_PT_2)),]
  mpg.mri.ntr.cer.lp <- mpg.mri.ntr.cer.lp[-which(is.na(
    mpg.mri.ntr.cer.lp$MRI_ADC_LP_1|
      mpg.mri.ntr.cer.lp$MRI_ADC_LP_2)),]
} else {
  mpg.mri.ntr.cer.lp <- mpg.mri.ntr.cer.lp[-which(is.na(
    mpg.mri.ntr.cer.lp$MRI_ADC_PT_1|
      mpg.mri.ntr.cer.lp$MRI_ADC_PT_2)),]
  mpg.mri.ntr.cer.lp <- mpg.mri.ntr.cer.lp[-which(is.na(
    mpg.mri.ntr.cer.lp$MRI_ADC_LP_1|
      mpg.mri.ntr.cer.lp$MRI_ADC_LP_2)),]
}

#CER RP
mri.ntr.rp.columns <- c(
  "PATIENT",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_ADC_PT_1",
  "MRI_ADC_PT_2",
  "MRI_SA_RP",
  "HIST_RP")
mpg.mri.ntr.cer.rp <- mpg.mri.uni.cer[,mri.ntr.rp.columns]
#View(mpg.mri.ntr.cer.rp)
if(sum(is.na(mpg.mri.ntr.cer.rp$HIST_RP))!=0){
  mpg.mri.ntr.cer.rp <- mpg.mri.ntr.cer.rp[-which(is.na(mpg.mri.ntr.cer.rp$HIST_RP)),]
  mpg.mri.ntr.cer.rp <- mpg.mri.ntr.cer.rp[-which(is.na(
    mpg.mri.ntr.cer.rp$MRI_ADC_PT_1|
      mpg.mri.ntr.cer.rp$MRI_ADC_PT_2)),]
  mpg.mri.ntr.cer.rp <- mpg.mri.ntr.cer.rp[-which(is.na(
    mpg.mri.ntr.cer.rp$MRI_ADC_RP_1|
      mpg.mri.ntr.cer.rp$MRI_ADC_RP_2)),]
} else {
  mpg.mri.ntr.cer.rp <- mpg.mri.ntr.cer.rp[-which(is.na(
    mpg.mri.ntr.cer.rp$MRI_ADC_PT_1|
      mpg.mri.ntr.cer.rp$MRI_ADC_PT_2)),]
  mpg.mri.ntr.cer.rp <- mpg.mri.ntr.cer.rp[-which(is.na(
    mpg.mri.ntr.cer.rp$MRI_ADC_RP_1|
      mpg.mri.ntr.cer.rp$MRI_ADC_RP_2)),]
}

#ENDO LP
mpg.mri.ntr.endo.lp <- mpg.mri.uni.endo[,mri.ntr.lp.columns]
#View(mpg.mri.ntr.endo.lp)
if(sum(is.na(mpg.mri.ntr.endo.lp$HIST_LP))!=0){
  mpg.mri.ntr.endo.lp <- mpg.mri.ntr.endo.lp[-which(is.na(mpg.mri.ntr.endo.lp$HIST_LP)),]
  mpg.mri.ntr.endo.lp <- mpg.mri.ntr.endo.lp[-which(is.na(
    mpg.mri.ntr.endo.lp$MRI_ADC_PT_1|
      mpg.mri.ntr.endo.lp$MRI_ADC_PT_2)),]
  mpg.mri.ntr.endo.lp <- mpg.mri.ntr.endo.lp[-which(is.na(
    mpg.mri.ntr.endo.lp$MRI_ADC_LP_1|
      mpg.mri.ntr.endo.lp$MRI_ADC_LP_2)),]
} else {
  mpg.mri.ntr.endo.lp <- mpg.mri.ntr.endo.lp[-which(is.na(
    mpg.mri.ntr.endo.lp$MRI_ADC_PT_1|
      mpg.mri.ntr.endo.lp$MRI_ADC_PT_2)),]
  mpg.mri.ntr.endo.lp <- mpg.mri.ntr.endo.lp[-which(is.na(
    mpg.mri.ntr.endo.lp$MRI_ADC_LP_1|
      mpg.mri.ntr.endo.lp$MRI_ADC_LP_2)),]
}

#ENDO RP
mpg.mri.ntr.endo.rp <- mpg.mri.uni.endo[,mri.ntr.rp.columns]
#View(mpg.mri.ntr.endo.rp)
if(sum(is.na(mpg.mri.ntr.endo.rp$HIST_RP))!=0){
  mpg.mri.ntr.endo.rp <- mpg.mri.ntr.endo.rp[-which(is.na(mpg.mri.ntr.endo.rp$HIST_RP)),]
  mpg.mri.ntr.endo.rp <- mpg.mri.ntr.endo.rp[-which(is.na(
    mpg.mri.ntr.endo.rp$MRI_ADC_PT_1|
      mpg.mri.ntr.endo.rp$MRI_ADC_PT_2)),]
  mpg.mri.ntr.endo.rp <- mpg.mri.ntr.endo.rp[-which(is.na(
    mpg.mri.ntr.endo.rp$MRI_ADC_RP_1|
      mpg.mri.ntr.endo.rp$MRI_ADC_RP_2)),]
} else {
  mpg.mri.ntr.endo.rp <- mpg.mri.ntr.endo.rp[-which(is.na(
    mpg.mri.ntr.endo.rp$MRI_ADC_PT_1|
      mpg.mri.ntr.endo.rp$MRI_ADC_PT_2)),]
  mpg.mri.ntr.endo.rp <- mpg.mri.ntr.endo.rp[-which(is.na(
    mpg.mri.ntr.endo.rp$MRI_ADC_RP_1|
      mpg.mri.ntr.endo.rp$MRI_ADC_RP_2)),]
}

#ENDO PALN
mri.ntr.paln.columns <- c(
  "PATIENT",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "MRI_ADC_PT_1",
  "MRI_ADC_PT_2",
  "MRI_SA_PALN",
  "HIST_PALN")

mpg.mri.ntr.endo.paln <- mpg.mri.uni.endo[,mri.ntr.paln.columns]
#View(mpg.mri.ntr.endo.paln)
if(sum(is.na(mpg.mri.ntr.endo.paln$HIST_PALN))!=0){
  mpg.mri.ntr.endo.paln <- mpg.mri.ntr.endo.paln[-which(is.na(mpg.mri.ntr.endo.paln$HIST_PALN)),]
  mpg.mri.ntr.endo.paln <- mpg.mri.ntr.endo.paln[-which(is.na(
    mpg.mri.ntr.endo.paln$MRI_ADC_PT_1|
      mpg.mri.ntr.endo.paln$MRI_ADC_PT_2)),]
  mpg.mri.ntr.endo.paln <- mpg.mri.ntr.endo.paln[-which(is.na(
    mpg.mri.ntr.endo.paln$MRI_ADC_PALN_1|
      mpg.mri.ntr.endo.paln$MRI_ADC_PALN_2)),]
} else {
  mpg.mri.ntr.endo.paln <- mpg.mri.ntr.endo.paln[-which(is.na(
    mpg.mri.ntr.endo.paln$MRI_ADC_PT_1|
      mpg.mri.ntr.endo.paln$MRI_ADC_PT_2)),]
  mpg.mri.ntr.endo.paln <- mpg.mri.ntr.endo.paln[-which(is.na(
    mpg.mri.ntr.endo.paln$MRI_ADC_PALN_1|
      mpg.mri.ntr.endo.paln$MRI_ADC_PALN_2)),]
}

#Calculate average PT and average LN readings and calculate TNR
#Then do another MWU-test

## 2.2 - MW U-test DFs ----
# 2.2.1 > FDG PT & LN ratios ----
#CER LP
mpg.fdg.ntr.cer.lp.df <- 
  rbind(
    mpg.fdg.ntr.cer.lp[which(mpg.fdg.ntr.cer.lp$HIST_LP==1),],
    mpg.fdg.ntr.cer.lp[which(mpg.fdg.ntr.cer.lp$HIST_LP==0),]
  )
mpg.fdg.ntr.cer.lp.means <-
  rep(NA, nrow(mpg.fdg.ntr.cer.lp),ncol=1)
for(i in 1:nrow(mpg.fdg.ntr.cer.lp.df)){
  mpg.fdg.ntr.cer.lp.means[i] <- 
    mean(c(
      mpg.fdg.ntr.cer.lp.df$FDG_SUV_LP_1[i],
      mpg.fdg.ntr.cer.lp.df$FDG_SUV_LP_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.ntr.cer.lp.df <-
  cbind(
    mpg.fdg.ntr.cer.lp.df$HIST_LP,
    mpg.fdg.ntr.cer.lp.means,
    mpg.fdg.ntr.cer.lp.df$FDG_SUV_PT,
    mpg.fdg.ntr.cer.lp.df$MRI_SA_LP)
colnames(mpg.fdg.ntr.cer.lp.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fdg.ntr.cer.lp.df <- as.data.frame(mpg.fdg.ntr.cer.lp.df)
#View(mpg.fdg.ntr.cer.lp.df)

for(i in 1:nrow(mpg.fdg.ntr.cer.lp.df)){
  mpg.fdg.ntr.cer.lp.df$LN_T_RATIO[i] <- 
    mpg.fdg.ntr.cer.lp.df$MEAN_LN[i]/mpg.fdg.ntr.cer.lp.df$PT_SUV[i]
}

#CER RP
mpg.fdg.ntr.cer.rp.df <- 
  rbind(
    mpg.fdg.ntr.cer.rp[which(mpg.fdg.ntr.cer.rp$HIST_RP==1),],
    mpg.fdg.ntr.cer.rp[which(mpg.fdg.ntr.cer.rp$HIST_RP==0),]
  )
mpg.fdg.ntr.cer.rp.means <-
  rep(NA, nrow(mpg.fdg.ntr.cer.rp),ncol=1)
for(i in 1:nrow(mpg.fdg.ntr.cer.rp.df)){
  mpg.fdg.ntr.cer.rp.means[i] <- 
    mean(c(
      mpg.fdg.ntr.cer.rp.df$FDG_SUV_RP_1[i],
      mpg.fdg.ntr.cer.rp.df$FDG_SUV_RP_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.ntr.cer.rp.df <-
  cbind(
    mpg.fdg.ntr.cer.rp.df$HIST_RP,
    mpg.fdg.ntr.cer.rp.means,
    mpg.fdg.ntr.cer.rp.df$FDG_SUV_PT,
    mpg.fdg.ntr.cer.rp.df$MRI_SA_RP)
colnames(mpg.fdg.ntr.cer.rp.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fdg.ntr.cer.rp.df <- as.data.frame(mpg.fdg.ntr.cer.rp.df)
#View(mpg.fdg.ntr.cer.rp.df)

for(i in 1:nrow(mpg.fdg.ntr.cer.rp.df)){
  mpg.fdg.ntr.cer.rp.df$LN_T_RATIO[i] <- 
    mpg.fdg.ntr.cer.rp.df$MEAN_LN[i]/mpg.fdg.ntr.cer.rp.df$PT_SUV[i]
}

#FDG CER PEL
mpg.fdg.ntr.cer.pel.df <-
  rbind(
    mpg.fdg.ntr.cer.lp.df,
    mpg.fdg.ntr.cer.rp.df
  )

#ENDO LP
mpg.fdg.ntr.endo.lp.df <- 
  rbind(
    mpg.fdg.ntr.endo.lp[which(mpg.fdg.ntr.endo.lp$HIST_LP==1),],
    mpg.fdg.ntr.endo.lp[which(mpg.fdg.ntr.endo.lp$HIST_LP==0),]
  )
mpg.fdg.ntr.endo.lp.means <-
  rep(NA, nrow(mpg.fdg.ntr.endo.lp),ncol=1)
for(i in 1:nrow(mpg.fdg.ntr.endo.lp.df)){
  mpg.fdg.ntr.endo.lp.means[i] <- 
    mean(c(
      mpg.fdg.ntr.endo.lp.df$FDG_SUV_LP_1[i],
      mpg.fdg.ntr.endo.lp.df$FDG_SUV_LP_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.ntr.endo.lp.df <-
  cbind(
    mpg.fdg.ntr.endo.lp.df$HIST_LP,
    mpg.fdg.ntr.endo.lp.means,
    mpg.fdg.ntr.endo.lp.df$FDG_SUV_PT,
    mpg.fdg.ntr.endo.lp.df$MRI_SA_LP)
colnames(mpg.fdg.ntr.endo.lp.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fdg.ntr.endo.lp.df <- as.data.frame(mpg.fdg.ntr.endo.lp.df)
#View(mpg.fdg.ntr.endo.lp.df)

for(i in 1:nrow(mpg.fdg.ntr.endo.lp.df)){
  mpg.fdg.ntr.endo.lp.df$LN_T_RATIO[i] <- 
    mpg.fdg.ntr.endo.lp.df$MEAN_LN[i]/mpg.fdg.ntr.endo.lp.df$PT_SUV[i]
}

#ENDO RP
mpg.fdg.ntr.endo.rp.df <- 
  rbind(
    mpg.fdg.ntr.endo.rp[which(mpg.fdg.ntr.endo.rp$HIST_RP==1),],
    mpg.fdg.ntr.endo.rp[which(mpg.fdg.ntr.endo.rp$HIST_RP==0),]
  )
mpg.fdg.ntr.endo.rp.means <-
  rep(NA, nrow(mpg.fdg.ntr.endo.rp),ncol=1)
for(i in 1:nrow(mpg.fdg.ntr.endo.rp.df)){
  mpg.fdg.ntr.endo.rp.means[i] <- 
    mean(c(
      mpg.fdg.ntr.endo.rp.df$FDG_SUV_RP_1[i],
      mpg.fdg.ntr.endo.rp.df$FDG_SUV_RP_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.ntr.endo.rp.df <-
  cbind(
    mpg.fdg.ntr.endo.rp.df$HIST_RP,
    mpg.fdg.ntr.endo.rp.means,
    mpg.fdg.ntr.endo.rp.df$FDG_SUV_PT,
    mpg.fdg.ntr.endo.rp.df$MRI_SA_RP)
colnames(mpg.fdg.ntr.endo.rp.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fdg.ntr.endo.rp.df <- as.data.frame(mpg.fdg.ntr.endo.rp.df)
#View(mpg.fdg.ntr.endo.rp.df)

for(i in 1:nrow(mpg.fdg.ntr.endo.rp.df)){
  mpg.fdg.ntr.endo.rp.df$LN_T_RATIO[i] <- 
    mpg.fdg.ntr.endo.rp.df$MEAN_LN[i]/mpg.fdg.ntr.endo.rp.df$PT_SUV[i]
}

#FDG ENDO PEL
mpg.fdg.ntr.endo.pel.df <-
  rbind(
    mpg.fdg.ntr.endo.lp.df,
    mpg.fdg.ntr.endo.rp.df
  )

#ENDO PALN
mpg.fdg.ntr.endo.paln.df <- 
  rbind(
    mpg.fdg.ntr.endo.paln[which(mpg.fdg.ntr.endo.paln$HIST_PALN==1),],
    mpg.fdg.ntr.endo.paln[which(mpg.fdg.ntr.endo.paln$HIST_PALN==0),]
  )
mpg.fdg.ntr.endo.paln.means <-
  rep(NA, nrow(mpg.fdg.ntr.endo.paln),ncol=1)
for(i in 1:nrow(mpg.fdg.ntr.endo.paln.df)){
  mpg.fdg.ntr.endo.paln.means[i] <- 
    mean(c(
      mpg.fdg.ntr.endo.paln.df$FDG_SUV_PALN_1[i],
      mpg.fdg.ntr.endo.paln.df$FDG_SUV_PALN_2[i]), 
      na.rm = TRUE)
}
mpg.fdg.ntr.endo.paln.df <-
  cbind(
    mpg.fdg.ntr.endo.paln.df$HIST_PALN,
    mpg.fdg.ntr.endo.paln.means,
    mpg.fdg.ntr.endo.paln.df$FDG_SUV_PT,
    mpg.fdg.ntr.endo.paln.df$MRI_SA_PALN)
colnames(mpg.fdg.ntr.endo.paln.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fdg.ntr.endo.paln.df <- as.data.frame(mpg.fdg.ntr.endo.paln.df)
#View(mpg.fdg.ntr.endo.paln.df)

for(i in 1:nrow(mpg.fdg.ntr.endo.paln.df)){
  mpg.fdg.ntr.endo.paln.df$LN_T_RATIO[i] <- 
    mpg.fdg.ntr.endo.paln.df$MEAN_LN[i]/mpg.fdg.ntr.endo.paln.df$PT_SUV[i]
}

#FDG ENDO ALL
mpg.fdg.ntr.endo.all.df <-
  rbind(
    mpg.fdg.ntr.endo.lp.df,
    mpg.fdg.ntr.endo.rp.df,
    mpg.fdg.ntr.endo.paln.df
  )

# 2.2.2 > FEC PT & LN ratios ----
#CER LP
mpg.fec.ntr.cer.lp.df <- 
  rbind(
    mpg.fec.ntr.cer.lp[which(mpg.fec.ntr.cer.lp$HIST_LP==1),],
    mpg.fec.ntr.cer.lp[which(mpg.fec.ntr.cer.lp$HIST_LP==0),]
  )
mpg.fec.ntr.cer.lp.means <-
  rep(NA, nrow(mpg.fec.ntr.cer.lp),ncol=1)
for(i in 1:nrow(mpg.fec.ntr.cer.lp.df)){
  mpg.fec.ntr.cer.lp.means[i] <- 
    mean(c(
      mpg.fec.ntr.cer.lp.df$FEC_SUV_LP_1[i],
      mpg.fec.ntr.cer.lp.df$FEC_SUV_LP_2[i]), 
      na.rm = TRUE)
}
mpg.fec.ntr.cer.lp.df <-
  cbind(
    mpg.fec.ntr.cer.lp.df$HIST_LP,
    mpg.fec.ntr.cer.lp.means,
    mpg.fec.ntr.cer.lp.df$FEC_SUV_PT,
    mpg.fec.ntr.cer.lp.df$MRI_SA_LP)
colnames(mpg.fec.ntr.cer.lp.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fec.ntr.cer.lp.df <- as.data.frame(mpg.fec.ntr.cer.lp.df)
#View(mpg.fec.ntr.cer.lp.df)

for(i in 1:nrow(mpg.fec.ntr.cer.lp.df)){
  mpg.fec.ntr.cer.lp.df$LN_T_RATIO[i] <- 
    mpg.fec.ntr.cer.lp.df$MEAN_LN[i]/mpg.fec.ntr.cer.lp.df$PT_SUV[i]
}

#CER RP
mpg.fec.ntr.cer.rp.df <- 
  rbind(
    mpg.fec.ntr.cer.rp[which(mpg.fec.ntr.cer.rp$HIST_RP==1),],
    mpg.fec.ntr.cer.rp[which(mpg.fec.ntr.cer.rp$HIST_RP==0),]
  )
mpg.fec.ntr.cer.rp.means <-
  rep(NA, nrow(mpg.fec.ntr.cer.rp),ncol=1)
for(i in 1:nrow(mpg.fec.ntr.cer.rp.df)){
  mpg.fec.ntr.cer.rp.means[i] <- 
    mean(c(
      mpg.fec.ntr.cer.rp.df$FEC_SUV_RP_1[i],
      mpg.fec.ntr.cer.rp.df$FEC_SUV_RP_2[i]), 
      na.rm = TRUE)
}
mpg.fec.ntr.cer.rp.df <-
  cbind(
    mpg.fec.ntr.cer.rp.df$HIST_RP,
    mpg.fec.ntr.cer.rp.means,
    mpg.fec.ntr.cer.rp.df$FEC_SUV_PT,
    mpg.fec.ntr.cer.rp.df$MRI_SA_RP)
colnames(mpg.fec.ntr.cer.rp.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fec.ntr.cer.rp.df <- as.data.frame(mpg.fec.ntr.cer.rp.df)
#View(mpg.fec.ntr.cer.rp.df)

for(i in 1:nrow(mpg.fec.ntr.cer.rp.df)){
  mpg.fec.ntr.cer.rp.df$LN_T_RATIO[i] <- 
    mpg.fec.ntr.cer.rp.df$MEAN_LN[i]/mpg.fec.ntr.cer.rp.df$PT_SUV[i]
}

#FEC CER PEL
mpg.fec.ntr.cer.pel.df <-
  rbind(
    mpg.fec.ntr.cer.lp.df,
    mpg.fec.ntr.cer.rp.df
  )

#ENDO LP
mpg.fec.ntr.endo.lp.df <- 
  rbind(
    mpg.fec.ntr.endo.lp[which(mpg.fec.ntr.endo.lp$HIST_LP==1),],
    mpg.fec.ntr.endo.lp[which(mpg.fec.ntr.endo.lp$HIST_LP==0),]
  )
mpg.fec.ntr.endo.lp.means <-
  rep(NA, nrow(mpg.fec.ntr.endo.lp),ncol=1)
for(i in 1:nrow(mpg.fec.ntr.endo.lp.df)){
  mpg.fec.ntr.endo.lp.means[i] <- 
    mean(c(
      mpg.fec.ntr.endo.lp.df$FEC_SUV_LP_1[i],
      mpg.fec.ntr.endo.lp.df$FEC_SUV_LP_2[i]), 
      na.rm = TRUE)
}
mpg.fec.ntr.endo.lp.df <-
  cbind(
    mpg.fec.ntr.endo.lp.df$HIST_LP,
    mpg.fec.ntr.endo.lp.means,
    mpg.fec.ntr.endo.lp.df$FEC_SUV_PT,
    mpg.fec.ntr.endo.lp.df$MRI_SA_LP)
colnames(mpg.fec.ntr.endo.lp.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fec.ntr.endo.lp.df <- as.data.frame(mpg.fec.ntr.endo.lp.df)
#View(mpg.fec.ntr.endo.lp.df)

for(i in 1:nrow(mpg.fec.ntr.endo.lp.df)){
  mpg.fec.ntr.endo.lp.df$LN_T_RATIO[i] <- 
    mpg.fec.ntr.endo.lp.df$MEAN_LN[i]/mpg.fec.ntr.endo.lp.df$PT_SUV[i]
}

#ENDO RP
mpg.fec.ntr.endo.rp.df <- 
  rbind(
    mpg.fec.ntr.endo.rp[which(mpg.fec.ntr.endo.rp$HIST_RP==1),],
    mpg.fec.ntr.endo.rp[which(mpg.fec.ntr.endo.rp$HIST_RP==0),]
  )
mpg.fec.ntr.endo.rp.means <-
  rep(NA, nrow(mpg.fec.ntr.endo.rp),ncol=1)
for(i in 1:nrow(mpg.fec.ntr.endo.rp.df)){
  mpg.fec.ntr.endo.rp.means[i] <- 
    mean(c(
      mpg.fec.ntr.endo.rp.df$FEC_SUV_RP_1[i],
      mpg.fec.ntr.endo.rp.df$FEC_SUV_RP_2[i]), 
      na.rm = TRUE)
}
mpg.fec.ntr.endo.rp.df <-
  cbind(
    mpg.fec.ntr.endo.rp.df$HIST_RP,
    mpg.fec.ntr.endo.rp.means,
    mpg.fec.ntr.endo.rp.df$FEC_SUV_PT,
    mpg.fec.ntr.endo.rp.df$MRI_SA_RP)
colnames(mpg.fec.ntr.endo.rp.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fec.ntr.endo.rp.df <- as.data.frame(mpg.fec.ntr.endo.rp.df)
#View(mpg.fec.ntr.endo.rp.df)

for(i in 1:nrow(mpg.fec.ntr.endo.rp.df)){
  mpg.fec.ntr.endo.rp.df$LN_T_RATIO[i] <- 
    mpg.fec.ntr.endo.rp.df$MEAN_LN[i]/mpg.fec.ntr.endo.rp.df$PT_SUV[i]
}

#FEC ENDO PEL
mpg.fec.ntr.endo.pel.df <-
  rbind(
    mpg.fec.ntr.endo.lp.df,
    mpg.fec.ntr.endo.rp.df
  )

#ENDO PALN
mpg.fec.ntr.endo.paln.df <- 
  rbind(
    mpg.fec.ntr.endo.paln[which(mpg.fec.ntr.endo.paln$HIST_PALN==1),],
    mpg.fec.ntr.endo.paln[which(mpg.fec.ntr.endo.paln$HIST_PALN==0),]
  )
mpg.fec.ntr.endo.paln.means <-
  rep(NA, nrow(mpg.fec.ntr.endo.paln),ncol=1)
for(i in 1:nrow(mpg.fec.ntr.endo.paln.df)){
  mpg.fec.ntr.endo.paln.means[i] <- 
    mean(c(
      mpg.fec.ntr.endo.paln.df$FEC_SUV_PALN_1[i],
      mpg.fec.ntr.endo.paln.df$FEC_SUV_PALN_2[i]), 
      na.rm = TRUE)
}
mpg.fec.ntr.endo.paln.df <-
  cbind(
    mpg.fec.ntr.endo.paln.df$HIST_PALN,
    mpg.fec.ntr.endo.paln.means,
    mpg.fec.ntr.endo.paln.df$FEC_SUV_PT,
    mpg.fec.ntr.endo.paln.df$MRI_SA_PALN)
colnames(mpg.fec.ntr.endo.paln.df) <- c("HIST","MEAN_LN","PT_SUV","MRI_SA")
mpg.fec.ntr.endo.paln.df <- as.data.frame(mpg.fec.ntr.endo.paln.df)
#View(mpg.fec.ntr.endo.paln.df)

for(i in 1:nrow(mpg.fec.ntr.endo.paln.df)){
  mpg.fec.ntr.endo.paln.df$LN_T_RATIO[i] <- 
    mpg.fec.ntr.endo.paln.df$MEAN_LN[i]/mpg.fec.ntr.endo.paln.df$PT_SUV[i]
}

#FDG ENDO ALL
mpg.fec.ntr.endo.all.df <-
  rbind(
    mpg.fec.ntr.endo.lp.df,
    mpg.fec.ntr.endo.rp.df,
    mpg.fec.ntr.endo.paln.df
  )

# 2.2.3 > MRI PT & LN ratios ----
#CER LP
mpg.mri.ntr.cer.lp.df <- 
  data.frame(rbind(
    mpg.mri.ntr.cer.lp[which(mpg.mri.ntr.cer.lp$HIST_LP==1),],
    mpg.mri.ntr.cer.lp[which(mpg.mri.ntr.cer.lp$HIST_LP==0),]
  ))
mpg.mri.ntr.cer.lp.means <-
  data.frame(matrix(NA, nrow(mpg.mri.ntr.cer.lp),ncol=2))
colnames(mpg.mri.ntr.cer.lp.means) <- c("LN_ADC","PT_ADC")
for(i in 1:nrow(mpg.mri.ntr.cer.lp.df)){
  mpg.mri.ntr.cer.lp.means[i,1] <- 
    mean(c(
      mpg.mri.ntr.cer.lp.df$MRI_ADC_LP_1[i],
      mpg.mri.ntr.cer.lp.df$MRI_ADC_LP_2[i]), 
      na.rm = TRUE)
  mpg.mri.ntr.cer.lp.means[i,2] <- 
    mean(c(
      mpg.mri.ntr.cer.lp.df$MRI_ADC_PT_1[i],
      mpg.mri.ntr.cer.lp.df$MRI_ADC_PT_2[i]),
      na.rm = TRUE)
}
mpg.mri.ntr.cer.lp.df <-
  cbind(
    mpg.mri.ntr.cer.lp.df$HIST_LP,
    mpg.mri.ntr.cer.lp.means,
    mpg.mri.ntr.cer.lp.df$MRI_SA_LP)
colnames(mpg.mri.ntr.cer.lp.df) <- c("HIST","MEAN_LN","MEAN_PT","MRI_SA")
#View(mpg.mri.ntr.cer.lp.df)

for(i in 1:nrow(mpg.mri.ntr.cer.lp.df)){
  mpg.mri.ntr.cer.lp.df$LN_T_RATIO[i] <- 
    mpg.mri.ntr.cer.lp.df$MEAN_LN[i]/mpg.mri.ntr.cer.lp.df$MEAN_PT[i]
}

#CER RP
mpg.mri.ntr.cer.rp.df <- 
  data.frame(rbind(
    mpg.mri.ntr.cer.rp[which(mpg.mri.ntr.cer.rp$HIST_RP==1),],
    mpg.mri.ntr.cer.rp[which(mpg.mri.ntr.cer.rp$HIST_RP==0),]
  ))
mpg.mri.ntr.cer.rp.means <-
  data.frame(matrix(NA, nrow(mpg.mri.ntr.cer.rp),ncol=2))
colnames(mpg.mri.ntr.cer.rp.means) <- c("LN_ADC","PT_ADC")
for(i in 1:nrow(mpg.mri.ntr.cer.rp.df)){
  mpg.mri.ntr.cer.rp.means[i,1] <- 
    mean(c(
      mpg.mri.ntr.cer.rp.df$MRI_ADC_RP_1[i],
      mpg.mri.ntr.cer.rp.df$MRI_ADC_RP_2[i]), 
      na.rm = TRUE)
  mpg.mri.ntr.cer.rp.means[i,2] <- 
    mean(c(
      mpg.mri.ntr.cer.rp.df$MRI_ADC_PT_1[i],
      mpg.mri.ntr.cer.rp.df$MRI_ADC_PT_2[i]),
      na.rm = TRUE)
}
mpg.mri.ntr.cer.rp.df <-
  cbind(
    mpg.mri.ntr.cer.rp.df$HIST_RP,
    mpg.mri.ntr.cer.rp.means,
    mpg.mri.ntr.cer.rp.df$MRI_SA_RP)
colnames(mpg.mri.ntr.cer.rp.df) <- c("HIST","MEAN_LN","MEAN_PT","MRI_SA")
#View(mpg.mri.ntr.cer.rp.df)

for(i in 1:nrow(mpg.mri.ntr.cer.rp.df)){
  mpg.mri.ntr.cer.rp.df$LN_T_RATIO[i] <- 
    mpg.mri.ntr.cer.rp.df$MEAN_LN[i]/mpg.mri.ntr.cer.rp.df$MEAN_PT[i]
}

#mri CER PEL
mpg.mri.ntr.cer.pel.df <-
  rbind(
    mpg.mri.ntr.cer.lp.df,
    mpg.mri.ntr.cer.rp.df
  )

#ENDO LP
mpg.mri.ntr.endo.lp.df <- 
  data.frame(rbind(
    mpg.mri.ntr.endo.lp[which(mpg.mri.ntr.endo.lp$HIST_LP==1),],
    mpg.mri.ntr.endo.lp[which(mpg.mri.ntr.endo.lp$HIST_LP==0),]
  ))
mpg.mri.ntr.endo.lp.means <-
  data.frame(matrix(NA, nrow(mpg.mri.ntr.endo.lp),ncol=2))
colnames(mpg.mri.ntr.endo.lp.means) <- c("LN_ADC","PT_ADC")
for(i in 1:nrow(mpg.mri.ntr.endo.lp.df)){
  mpg.mri.ntr.endo.lp.means[i,1] <- 
    mean(c(
      mpg.mri.ntr.endo.lp.df$MRI_ADC_LP_1[i],
      mpg.mri.ntr.endo.lp.df$MRI_ADC_LP_2[i]), 
      na.rm = TRUE)
  mpg.mri.ntr.endo.lp.means[i,2] <- 
    mean(c(
      mpg.mri.ntr.endo.lp.df$MRI_ADC_PT_1[i],
      mpg.mri.ntr.endo.lp.df$MRI_ADC_PT_2[i]),
      na.rm = TRUE)
}
mpg.mri.ntr.endo.lp.df <-
  cbind(
    mpg.mri.ntr.endo.lp.df$HIST_LP,
    mpg.mri.ntr.endo.lp.means,
    mpg.mri.ntr.endo.lp.df$MRI_SA_LP)
colnames(mpg.mri.ntr.endo.lp.df) <- c("HIST","MEAN_LN","MEAN_PT","MRI_SA")
#View(mpg.mri.ntr.endo.lp.df)

for(i in 1:nrow(mpg.mri.ntr.endo.lp.df)){
  mpg.mri.ntr.endo.lp.df$LN_T_RATIO[i] <- 
    mpg.mri.ntr.endo.lp.df$MEAN_LN[i]/mpg.mri.ntr.endo.lp.df$MEAN_PT[i]
}

#ENDO RP
mpg.mri.ntr.endo.rp.df <- 
  data.frame(rbind(
    mpg.mri.ntr.endo.rp[which(mpg.mri.ntr.endo.rp$HIST_RP==1),],
    mpg.mri.ntr.endo.rp[which(mpg.mri.ntr.endo.rp$HIST_RP==0),]
  ))
mpg.mri.ntr.endo.rp.means <-
  data.frame(matrix(NA, nrow(mpg.mri.ntr.endo.rp),ncol=2))
colnames(mpg.mri.ntr.endo.rp.means) <- c("LN_ADC","PT_ADC")
for(i in 1:nrow(mpg.mri.ntr.endo.rp.df)){
  mpg.mri.ntr.endo.rp.means[i,1] <- 
    mean(c(
      mpg.mri.ntr.endo.rp.df$MRI_ADC_RP_1[i],
      mpg.mri.ntr.endo.rp.df$MRI_ADC_RP_2[i]), 
      na.rm = TRUE)
  mpg.mri.ntr.endo.rp.means[i,2] <- 
    mean(c(
      mpg.mri.ntr.endo.rp.df$MRI_ADC_PT_1[i],
      mpg.mri.ntr.endo.rp.df$MRI_ADC_PT_2[i]),
      na.rm = TRUE)
}
mpg.mri.ntr.endo.rp.df <-
  cbind(
    mpg.mri.ntr.endo.rp.df$HIST_RP,
    mpg.mri.ntr.endo.rp.means,
    mpg.mri.ntr.endo.rp.df$MRI_SA_RP)
colnames(mpg.mri.ntr.endo.rp.df) <- c("HIST","MEAN_LN","MEAN_PT","MRI_SA")
#View(mpg.mri.ntr.endo.rp.df)

for(i in 1:nrow(mpg.mri.ntr.endo.rp.df)){
  mpg.mri.ntr.endo.rp.df$LN_T_RATIO[i] <- 
    mpg.mri.ntr.endo.rp.df$MEAN_LN[i]/mpg.mri.ntr.endo.rp.df$MEAN_PT[i]
}

#mri endo PEL
mpg.mri.ntr.endo.pel.df <-
  rbind(
    mpg.mri.ntr.endo.lp.df,
    mpg.mri.ntr.endo.rp.df
  )

#ENDO PALN
mpg.mri.ntr.endo.paln.df <- 
  data.frame(rbind(
    mpg.mri.ntr.endo.paln[which(mpg.mri.ntr.endo.paln$HIST_PALN==1),],
    mpg.mri.ntr.endo.paln[which(mpg.mri.ntr.endo.paln$HIST_PALN==0),]
  ))
mpg.mri.ntr.endo.paln.means <-
  data.frame(matrix(NA, nrow(mpg.mri.ntr.endo.paln),ncol=2))
colnames(mpg.mri.ntr.endo.paln.means) <- c("LN_ADC","PT_ADC")
for(i in 1:nrow(mpg.mri.ntr.endo.paln.df)){
  mpg.mri.ntr.endo.paln.means[i,1] <- 
    mean(c(
      mpg.mri.ntr.endo.paln.df$MRI_ADC_PALN_1[i],
      mpg.mri.ntr.endo.paln.df$MRI_ADC_PALN_2[i]), 
      na.rm = TRUE)
  mpg.mri.ntr.endo.paln.means[i,2] <- 
    mean(c(
      mpg.mri.ntr.endo.paln.df$MRI_ADC_PT_1[i],
      mpg.mri.ntr.endo.paln.df$MRI_ADC_PT_2[i]),
      na.rm = TRUE)
}
mpg.mri.ntr.endo.paln.df <-
  cbind(
    mpg.mri.ntr.endo.paln.df$HIST_PALN,
    mpg.mri.ntr.endo.paln.means,
    mpg.mri.ntr.endo.paln.df$MRI_SA_PALN)
colnames(mpg.mri.ntr.endo.paln.df) <- c("HIST","MEAN_LN","MEAN_PT","MRI_SA")
#View(mpg.mri.ntr.endo.paln.df)

for(i in 1:nrow(mpg.mri.ntr.endo.paln.df)){
  mpg.mri.ntr.endo.paln.df$LN_T_RATIO[i] <- 
    mpg.mri.ntr.endo.paln.df$MEAN_LN[i]/mpg.mri.ntr.endo.paln.df$MEAN_PT[i]
}

#MRI ENDO ALL
mpg.mri.ntr.endo.all.df <-
  rbind(
    mpg.mri.ntr.endo.lp.df,
    mpg.mri.ntr.endo.rp.df,
    mpg.mri.ntr.endo.paln.df
  )


## 2.3 - MW U-test analysis----
mpg.fdg.ntr.cer.lp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fdg.ntr.cer.lp.df)
mpg.fdg.ntr.cer.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fdg.ntr.cer.rp.df)
mpg.fdg.ntr.cer.pel.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fdg.ntr.cer.pel.df)

mpg.fdg.ntr.endo.lp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fdg.ntr.endo.lp.df)
mpg.fdg.ntr.endo.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fdg.ntr.endo.rp.df)
mpg.fdg.ntr.endo.paln.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fdg.ntr.endo.paln.df)
mpg.fdg.ntr.endo.pel.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fdg.ntr.endo.pel.df)
mpg.fdg.ntr.endo.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fdg.ntr.endo.rp.df)

mpg.fec.ntr.cer.lp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fec.ntr.cer.lp.df)
mpg.fec.ntr.cer.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fec.ntr.cer.rp.df)
mpg.fec.ntr.cer.pel.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fec.ntr.cer.pel.df)

mpg.fec.ntr.endo.lp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fec.ntr.endo.lp.df)
mpg.fec.ntr.endo.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fec.ntr.endo.rp.df)
mpg.fec.ntr.endo.paln.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fec.ntr.endo.paln.df)
mpg.fec.ntr.endo.pel.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fec.ntr.endo.pel.df)
mpg.fec.ntr.endo.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.fec.ntr.endo.rp.df)

mpg.mri.ntr.cer.lp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.mri.ntr.cer.lp.df)
mpg.mri.ntr.cer.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.mri.ntr.cer.rp.df)
mpg.mri.ntr.cer.pel.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.mri.ntr.cer.pel.df)

mpg.mri.ntr.endo.lp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.mri.ntr.endo.lp.df)
mpg.mri.ntr.endo.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.mri.ntr.endo.rp.df)
mpg.mri.ntr.endo.paln.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.mri.ntr.endo.paln.df)
mpg.mri.ntr.endo.pel.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.mri.ntr.endo.pel.df)
mpg.mri.ntr.endo.rp.mwu <- wilcox.test(LN_T_RATIO~HIST, data = mpg.mri.ntr.endo.rp.df)

#Store results
mpg.ntr.mwu.results.rows <- c(
  "FDG CER LP", 
  "FDG CER RP",
  "FDG CER PEL",
  "FDG ENDO LP", 
  "FDG ENDO RP",
  "FDG ENDO PALN",
  "FDG ENDO PEL",
  "FDG ENDO ALL",
  "FEC CER LP",
  "FEC CER RP",
  "FEC CER PEL",
  "FEC ENDO LP",
  "FEC ENDO RP",
  "FEC ENDO PALN",
  "FEC ENDO PEL",
  "FEC ENDO ALL",
  "MRI CER LP",
  "MRI CER RP",
  "MRI CER PEL",
  "MRI ENDO LP",
  "MRI ENDO RP",
  "MRI ENDO PALN",
  "MRI ENDO PEL",
  "MRI ENDO ALL"
)

mpg.ntr.mwu.results <- list(
  mpg.fdg.ntr.cer.lp.mwu,
  mpg.fdg.ntr.cer.rp.mwu,
  mpg.fdg.ntr.cer.pel.mwu,
  mpg.fdg.ntr.endo.lp.mwu,
  mpg.fdg.ntr.endo.rp.mwu,
  mpg.fdg.ntr.endo.paln.mwu,
  mpg.fdg.ntr.endo.pel.mwu,
  mpg.fdg.ntr.endo.rp.mwu,
  mpg.fec.ntr.cer.lp.mwu,
  mpg.fec.ntr.cer.rp.mwu,
  mpg.fec.ntr.cer.pel.mwu,
  mpg.fec.ntr.endo.lp.mwu,
  mpg.fec.ntr.endo.rp.mwu,
  mpg.fec.ntr.endo.paln.mwu,
  mpg.fec.ntr.endo.pel.mwu,
  mpg.fec.ntr.endo.rp.mwu,
  mpg.mri.ntr.cer.lp.mwu,
  mpg.mri.ntr.cer.rp.mwu,
  mpg.mri.ntr.cer.pel.mwu,
  mpg.mri.ntr.endo.lp.mwu,
  mpg.mri.ntr.endo.rp.mwu,
  mpg.mri.ntr.endo.paln.mwu,
  mpg.mri.ntr.endo.pel.mwu,
  mpg.mri.ntr.endo.rp.mwu
)

mpg.ntr.mwu.results.df <- data.frame(matrix(NA, ncol=2, nrow=length(mpg.ntr.mwu.results)))
colnames(mpg.ntr.mwu.results.df) <- c("W-statistic", "p-value")
rownames(mpg.ntr.mwu.results.df) <- mpg.ntr.mwu.results.rows

for(i in 1:length(mpg.ntr.mwu.results)){
  mpg.ntr.mwu.results.df[i,1] <- mpg.ntr.mwu.results[[i]][1]
  mpg.ntr.mwu.results.df[i,2] <- mpg.ntr.mwu.results[[i]][3]
}

#Adjust p-values for multiple testing
mpg.ntr.mwu.results.df$adj.p.val <- p.adjust(mpg.ntr.mwu.results.df$`p-value`, method="fdr")

# 2.4 - Visualise results with boxplots (active) ----
# library(ggpubr)
##Multi-plot 1 - FDG ,FEC, MRI in ENDO LP
#Plot 1
mpg.fdg.ntr.endo.all.df <- as.data.frame(mpg.fdg.ntr.endo.all.df)
mpg.fdg.ntr.endo.all.df$HIST <- factor(mpg.fdg.ntr.endo.all.df$HIST)
mpg.fdg.ntr.endo.all.bp <- ggplot(data = as.data.frame(mpg.fdg.ntr.endo.all.df),
                                  aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y=expression(NTR~SUV)) +
  ylim(0,1.05) + 
  annotate("text", x = 1.5, y = 0.8, label = expression(rho==".076")) + 
  scale_x_discrete(labels= c("n=35","n=23")) + font("ylab", size = 13)

#Plot 2
mpg.fdg.ntr.endo.pel.df <- as.data.frame(mpg.fdg.ntr.endo.pel.df)
mpg.fdg.ntr.endo.pel.df$HIST <- factor(mpg.fdg.ntr.endo.pel.df$HIST)
mpg.fdg.ntr.endo.pel.bp <- ggplot(data = as.data.frame(mpg.fdg.ntr.endo.all.df),
                                  aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.05) + 
  annotate("text", x = 1.5, y = 0.8, label = expression(rho<".01")) + 
  scale_x_discrete(labels= c("n=29","n=20")) + 
  geom_bracket(xmin = "0", xmax = "1",
               y.position = 1,
               label = "*",
               tip.length = c(0.3,0.02),
               size = 0.5,
               inherit.aes = FALSE) 
#Plot 3
mpg.fdg.ntr.endo.lp.df <- as.data.frame(mpg.fdg.ntr.endo.lp.df)
mpg.fdg.ntr.endo.lp.df$HIST <- factor(mpg.fdg.ntr.endo.lp.df$HIST)
mpg.fdg.ntr.endo.lp.bp <- ggplot(data = as.data.frame(mpg.fdg.ntr.endo.lp.df),
                                 aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.05) + annotate(
            "text", x = 1.5, y = 0.8, label = expression(rho==".076")) + scale_x_discrete(
              labels= c("n=15","n=8"))
#Plot 4
mpg.fdg.ntr.endo.rp.df <- as.data.frame(mpg.fdg.ntr.endo.rp.df)
mpg.fdg.ntr.endo.rp.df$HIST <- factor(mpg.fdg.ntr.endo.rp.df$HIST)
mpg.fdg.ntr.endo.rp.bp <- ggplot(data = as.data.frame(mpg.fdg.ntr.endo.rp.df),
                                aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.05) + annotate(
            "text", x = 1.5, y = 0.8, label = expression(rho==".076")) + scale_x_discrete(
              labels= c("n=14","n=12"))
#Plot 5
mpg.fdg.ntr.endo.paln.df <- as.data.frame(mpg.fdg.ntr.endo.paln.df)
mpg.fdg.ntr.endo.paln.df$HIST <- factor(mpg.fdg.ntr.endo.paln.df$HIST)
mpg.fdg.ntr.endo.paln.bp <- ggplot(data = as.data.frame(mpg.fdg.ntr.endo.paln.df),
                                   aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.05) + annotate(
            "text", x = 1.5, y = 0.8, label = expression(rho==".57")) + scale_x_discrete(
              labels= c("n=6","n=3"))
#Plot 6
mpg.fec.ntr.endo.all.df <- as.data.frame(mpg.fec.ntr.endo.all.df)
mpg.fec.ntr.endo.all.df$HIST <- factor(mpg.fec.ntr.endo.all.df$HIST)
mpg.fec.ntr.endo.all.bp <- ggplot(data = as.data.frame(mpg.fec.ntr.endo.all.df),
                                  aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y=expression(NTR~SUV)) +
  ylim(0,1.05) + annotate("text", x = 1.5, y = 0.9, label = expression(rho==".16")) + scale_x_discrete(
                                         labels= c("n=19","n=9")) + font("ylab", size = 13)
#Plot 7
mpg.fec.ntr.endo.pel.df <- as.data.frame(mpg.fec.ntr.endo.pel.df)
mpg.fec.ntr.endo.pel.df$HIST <- factor(mpg.fec.ntr.endo.pel.df$HIST)
mpg.fec.ntr.endo.pel.bp <- ggplot(data = as.data.frame(mpg.fec.ntr.endo.pel.df),
                                  aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.05) + annotate(
                                       "text", x = 1.5, y = 0.9, label = expression(rho==".17")) + scale_x_discrete(
                                         labels= c("n=15","n=8"))
#Plot 8
mpg.fec.ntr.endo.lp.df <- as.data.frame(mpg.fec.ntr.endo.lp.df)
mpg.fec.ntr.endo.lp.df$HIST <- factor(mpg.fec.ntr.endo.lp.df$HIST)
mpg.fec.ntr.endo.lp.bp <- ggplot(data = as.data.frame(mpg.fec.ntr.endo.lp.df),
                                 aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.05) + annotate(
            "text", x = 1.5, y = 0.9, label = expression(rho==".91")) + scale_x_discrete(
              labels= c("n=9","n=3"))
#Plot 9
mpg.fec.ntr.endo.rp.df <- as.data.frame(mpg.fec.ntr.endo.rp.df)
mpg.fec.ntr.endo.rp.df$HIST <- factor(mpg.fec.ntr.endo.rp.df$HIST)
mpg.fec.ntr.endo.rp.bp <- ggplot(data = as.data.frame(mpg.fec.ntr.endo.rp.df),
                                 aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.05) + annotate(
            "text", x = 1.5, y = 0.9, label = expression(rho==".16")) + scale_x_discrete(
              labels= c("n=6","n=5"))
#Plot 10
mpg.fec.ntr.endo.paln.df <- as.data.frame(mpg.fec.ntr.endo.paln.df)
mpg.fec.ntr.endo.paln.df$HIST <- factor(mpg.fec.ntr.endo.paln.df$HIST)
mpg.fec.ntr.endo.paln.bp <- ggplot(data = as.data.frame(mpg.fec.ntr.endo.paln.df),
                                   aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.05) + annotate(
            "text", x = 1.5, y = 0.9, label = expression(rho==".91")) + scale_x_discrete(
              labels= c("n=4","n=1"))
#Plot 11
mpg.mri.ntr.endo.all.df <- as.data.frame(mpg.mri.ntr.endo.all.df)
mpg.mri.ntr.endo.all.df$HIST <- factor(mpg.mri.ntr.endo.all.df$HIST)
mpg.mri.ntr.endo.all.bp <- ggplot(data = as.data.frame(mpg.mri.ntr.endo.all.df),
                                  aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y=expression(NTR~ADC)) +
  ylim(0,3.5) + annotate("text", x = 1.5, y = 2.8, label = expression(rho==".91")) + 
  scale_x_discrete(labels= c("n=56","n=23")) + font("ylab", size = 13)

#Plot 12
mpg.mri.ntr.endo.pel.df <- as.data.frame(mpg.mri.ntr.endo.pel.df)
mpg.mri.ntr.endo.pel.df$HIST <- factor(mpg.mri.ntr.endo.pel.df$HIST)
mpg.mri.ntr.endo.pel.bp <- ggplot(data = as.data.frame(mpg.mri.ntr.endo.pel.df),
                                  aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0.5,3.5) + annotate("text", x = 1.5, y = 2.8, label = expression(rho==".80")) + 
  scale_x_discrete(labels= c("n=48","n=19"))
#Plot 13
mpg.mri.ntr.endo.lp.df <- as.data.frame(mpg.mri.ntr.endo.lp.df)
mpg.mri.ntr.endo.lp.df$HIST <- factor(mpg.mri.ntr.endo.lp.df$HIST)
mpg.mri.ntr.endo.lp.bp <- ggplot(data = as.data.frame(mpg.mri.ntr.endo.lp.df),
                                 aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0.5,3.5) + annotate(
                                      "text", x = 1.5, y = 2.8, label = expression(rho==".80")) + scale_x_discrete(
                                        labels= c("n=23","n=9"))
#Plot 14
mpg.mri.ntr.endo.rp.df <- as.data.frame(mpg.mri.ntr.endo.rp.df)
mpg.mri.ntr.endo.rp.df$HIST <- factor(mpg.mri.ntr.endo.rp.df$HIST)
mpg.mri.ntr.endo.rp.bp <-ggplot(data = as.data.frame(mpg.mri.ntr.endo.rp.df),
                                aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0.5,3.5) + annotate(
                                     "text", x = 1.5, y = 2.8, label = expression(rho==".91")) + scale_x_discrete(
                                       labels= c("n=25","n=10"))
#Plot 15
mpg.mri.ntr.endo.paln.df <- as.data.frame(mpg.mri.ntr.endo.paln.df)
mpg.mri.ntr.endo.paln.df$HIST <- factor(mpg.mri.ntr.endo.paln.df$HIST)
mpg.mri.ntr.endo.paln.bp <-ggplot(data = as.data.frame(mpg.mri.ntr.endo.paln.df),
                                  aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0.5,3.5) + annotate(
                                       "text", x = 1.5, y = 2.8, label = expression(rho=="1.0")) + scale_x_discrete(
                                         labels= c("n=8","n=4"))

png("NTR_FDG_FEC_MRI_ENDO_BP.png",width = 12, height = 7, units ='in', res = 300)
ggarrange(mpg.fdg.ntr.endo.all.bp,
          mpg.fdg.ntr.endo.pel.bp,
          mpg.fdg.ntr.endo.lp.bp,
          mpg.fdg.ntr.endo.rp.bp,
          mpg.fdg.ntr.endo.paln.bp,
          mpg.fec.ntr.endo.all.bp,
          mpg.fec.ntr.endo.pel.bp,
          mpg.fec.ntr.endo.lp.bp,
          mpg.fec.ntr.endo.rp.bp,
          mpg.fec.ntr.endo.paln.bp,
          mpg.mri.ntr.endo.all.bp,
          mpg.mri.ntr.endo.pel.bp,
          mpg.mri.ntr.endo.lp.bp,
          mpg.mri.ntr.endo.rp.bp,
          mpg.mri.ntr.endo.paln.bp,
          label.x = -.05,
          labels = c("A1","A2","A3","A4","A5","B1","B2","B3","B4","B5","C1","C2","C3","C4","C5"),
          align = "hv",
          common.legend = TRUE,
          ncol = 5, nrow = 3)
dev.off()

##Multi-plot 2 - FDG, FEC & MRI in CER
#Plot 1
mpg.fdg.ntr.cer.pel.df <- as.data.frame(mpg.fdg.ntr.cer.pel.df)
mpg.fdg.ntr.cer.pel.df$HIST <- factor(mpg.fdg.ntr.cer.pel.df$HIST)
mpg.fdg.ntr.cer.pel.bp <- ggplot(data=as.data.frame(mpg.fdg.ntr.cer.pel.df),
                                 aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="NTR SUV") +
  ylim(0,1.2) + annotate("text", x = 1.5, y = 0.85, label = expression(rho==".91"), size = 3.5) + scale_x_discrete(
                                        labels= c("n=7","n=3")) + font("ylab", size=13)
#Plot 2
mpg.fdg.ntr.cer.lp.df <- as.data.frame(mpg.fdg.ntr.cer.lp.df)
mpg.fdg.ntr.cer.lp.df$HIST <- factor(mpg.fdg.ntr.cer.lp.df$HIST)
mpg.fdg.ntr.cer.lp.bp <- ggplot(data = as.data.frame(mpg.fdg.ntr.cer.lp.df),
                                aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.2) + annotate("text", x = 1.5, y = 0.85, label = expression(rho==".32"), size = 3.5) + scale_x_discrete(
                                        labels= c("n=4","n=2"))
#Plot 3
mpg.fdg.ntr.cer.rp.df <- as.data.frame(mpg.fdg.ntr.cer.rp.df)
mpg.fdg.ntr.cer.rp.df$HIST <- factor(mpg.fdg.ntr.cer.rp.df$HIST)
mpg.fdg.ntr.cer.rp.bp <- ggplot(data = as.data.frame(mpg.fdg.ntr.cer.rp.df),
                                aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.2) + annotate("text", x = 1.5, y = 0.85, label = expression(rho==".86"), size = 3.5) + scale_x_discrete(
                                       labels= c("n=3","n=1"))

#Plot 4
mpg.fec.ntr.cer.pel.df <- as.data.frame(mpg.fec.ntr.cer.pel.df)
mpg.fec.ntr.cer.pel.df$HIST <- factor(mpg.fec.ntr.cer.pel.df$HIST)
mpg.fec.ntr.cer.pel.bp <- ggplot(data = as.data.frame(mpg.fec.ntr.cer.pel.df),
                                 aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="NTR SUV") +
  ylim(0,1.5) + annotate("text", x = 1.5, y = 0.9, label = expression(rho==".91"), size = 3.5) + scale_x_discrete(
                                        labels= c("n=6","n=3")) + font("ylab", size=13)
#Plot 5
mpg.fec.ntr.cer.lp.df <- as.data.frame(mpg.fec.ntr.cer.lp.df)
mpg.fec.ntr.cer.lp.df$HIST <- factor(mpg.fec.ntr.cer.lp.df$HIST)
mpg.fec.ntr.cer.lp.bp <- ggplot(data = as.data.frame(mpg.fec.ntr.cer.lp.df),
                                aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.5) + annotate("text", x = 1.5, y = 0.9, label = expression(rho==".32"), size = 3.5) + scale_x_discrete(
                                       labels= c("n=3","n=2"))
#Plot 6
mpg.fec.ntr.cer.rp.df <- as.data.frame(mpg.fec.ntr.cer.rp.df)
mpg.fec.ntr.cer.rp.df$HIST <- factor(mpg.fec.ntr.cer.rp.df$HIST)
mpg.fec.ntr.cer.rp.bp <-ggplot(data = as.data.frame(mpg.fec.ntr.cer.rp.df),
                               aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,1.5) + annotate("text", x = 1.5, y = 0.9, label = expression(rho=="1.0"), size = 3.5) + scale_x_discrete(
                                      labels= c("n=3","n=1"))
#Plot 7
mpg.mri.ntr.cer.pel.df <- as.data.frame(mpg.mri.ntr.cer.pel.df)
mpg.mri.ntr.cer.pel.df$HIST <- factor(mpg.mri.ntr.cer.pel.df$HIST)
mpg.mri.ntr.cer.pel.bp <- ggplot(data = as.data.frame(mpg.mri.ntr.cer.pel.df),
                                 aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="NTR ADC") +
  ylim(0,3) + annotate("text", x = 1.5, y = 2.2, label = expression(rho==".12"), size = 3.5) + scale_x_discrete(
                                        labels= c("n=19","n=7")) + font("ylab", size=13)
#Plot 8
mpg.mri.ntr.cer.lp.df <- as.data.frame(mpg.mri.ntr.cer.lp.df)
mpg.mri.ntr.cer.lp.df$HIST <- factor(mpg.mri.ntr.cer.lp.df$HIST)
mpg.mri.ntr.cer.lp.bp <- ggplot(data = as.data.frame(mpg.mri.ntr.cer.lp.df),
                                aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,3) + annotate("text", x = 1.5, y = 2.2, label = expression(rho==".91"), size = 3.5) + scale_x_discrete(
                                       labels= c("n=9","n=3"))
#Plot 9
mpg.mri.ntr.cer.rp.df <- as.data.frame(mpg.mri.ntr.cer.rp.df)
mpg.mri.ntr.cer.rp.df$HIST <- factor(mpg.mri.ntr.cer.rp.df$HIST)
mpg.mri.ntr.cer.rp.bp <- ggplot(data = as.data.frame(mpg.mri.ntr.cer.rp.df),
                               aes(x = HIST,y= LN_T_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,3) + annotate("text", x = 1.5, y = 2.2, label = expression(rho==".14"), size = 3.5) + scale_x_discrete(
                                      labels= c("n=10","n=4"))

png("NTR_FDG_FEC_MRI_CER_BP.png",width = 6, height = 6, units ='in', res = 300)
ggarrange(mpg.fdg.ntr.cer.pel.bp,
          mpg.fdg.ntr.cer.lp.bp,
          mpg.fdg.ntr.cer.rp.bp,
          mpg.fec.ntr.cer.pel.bp,
          mpg.fec.ntr.cer.lp.bp,
          mpg.fec.ntr.cer.rp.bp,
          mpg.mri.ntr.cer.pel.bp,
          mpg.mri.ntr.cer.lp.bp,
          mpg.mri.ntr.cer.rp.bp,
          label.x = -.05,
          labels = c("A1","A2","A3","B1","B2","B3","C1","C2","C3"),
          align = "hv",
          common.legend = TRUE,
          ncol = 3, nrow = 3)
dev.off()




#....----
## 3.1 - SUV-to-ADC ratio ----
#n.b. - labelled 'mpg.fdg.sta' as is FDG SUVmax / ADC (may do FEC in future!)
# View(mpg.fdg.sta.cer.lp) #6 patients
# View(mpg.fdg.sta.cer.rp) #5 patients
# View(mpg.fdg.sta.endo.lp) #19 patients
# View(mpg.fdg.sta.endo.rp) #25 patients
# View(mpg.fdg.sta.endo.paln) #8 patients
# 
# View(mpg.fec.sta.cer.lp) #3 patients
# View(mpg.fec.sta.cer.rp) #4 patients
# View(mpg.fec.sta.endo.lp) #5 patients
# View(mpg.fec.sta.endo.rp) #9 patients
# View(mpg.fec.sta.endo.paln) #3 patients

# 3.1.1 > FDG SUV-to-ADC ratio DF ----
mpg.fdg.sta <- mpg[which(mpg$PATIENT_SCAN_FDG==1
                     &mpg$PATIENT_SCAN_MRI==1),]
mpg.fdg.sta.cer <- mpg.fdg.sta[which(mpg.fdg.sta$CANC_CER==1),]
mpg.fdg.sta.endo <- mpg.fdg.sta[which(mpg.fdg.sta$CANC_ENDO==1),]

mpg.fdg.sta.columns <- c(
  "PATIENT",
  "PATIENT_SCAN_FDG",
  "PATIENT_SCAN_MRI",
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "CANC_CER",
  "CANC_ENDO")

mpg.fdg.sta.cer <- mpg.fdg.sta.cer[,mpg.fdg.sta.columns]
mpg.fdg.sta.endo <- mpg.fdg.sta.endo[,mpg.fdg.sta.columns]

#FDG CER LP
mpg.fdg.sta.cer.lp.columns <- c(
  "PATIENT",
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_SA_LP",
  "HIST_LP")

mpg.fdg.sta.cer.lp <- mpg.fdg.sta.cer[,mpg.fdg.sta.cer.lp.columns]
mpg.fdg.sta.cer.lp.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fdg.sta.cer.lp),ncol = 2))
for(i in 1:nrow(mpg.fdg.sta.cer.lp)){
  mpg.fdg.sta.cer.lp.means[i,1] <- 
    mean(c(
      mpg.fdg.sta.cer.lp$FDG_SUV_LP_1[i],
      mpg.fdg.sta.cer.lp$FDG_SUV_LP_2[i]), 
      na.rm = T)
  mpg.fdg.sta.cer.lp.means[i,2] <- 
    mean(c(
      mpg.fdg.sta.cer.lp$MRI_ADC_LP_1[i],
      mpg.fdg.sta.cer.lp$MRI_ADC_LP_2[i]),
      na.rm = T)
}
mpg.fdg.sta.cer.lp <- cbind(
  mpg.fdg.sta.cer.lp$HIST_LP,
  mpg.fdg.sta.cer.lp.means,
  mpg.fdg.sta.cer.lp$MRI_SA_LP
)
colnames(mpg.fdg.sta.cer.lp) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")

mpg.fdg.sta.cer.lp <- mpg.fdg.sta.cer.lp[-which(is.na(mpg.fdg.sta.cer.lp$SUV_MEAN)),]
mpg.fdg.sta.cer.lp <- mpg.fdg.sta.cer.lp[-which(is.na(mpg.fdg.sta.cer.lp$ADC_MEAN)),]

mpg.fdg.sta.cer.lp <- rbind(
  mpg.fdg.sta.cer.lp[which(mpg.fdg.sta.cer.lp$HIST==1),],
  mpg.fdg.sta.cer.lp[which(mpg.fdg.sta.cer.lp$HIST==0),]
)

for(i in 1:nrow(mpg.fdg.sta.cer.lp)){
  mpg.fdg.sta.cer.lp$STA_RATIO[i] <- 
    mpg.fdg.sta.cer.lp$SUV_MEAN[i] / mpg.fdg.sta.cer.lp$ADC_MEAN[i]
}

#FDG CER RP
mpg.fdg.sta.cer.rp.columns <- c(
  "PATIENT",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_SA_RP",
  "HIST_RP")

mpg.fdg.sta.cer.rp <- mpg.fdg.sta.cer[,mpg.fdg.sta.cer.rp.columns]
mpg.fdg.sta.cer.rp.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fdg.sta.cer.rp),ncol = 2))
for(i in 1:nrow(mpg.fdg.sta.cer.rp)){
  mpg.fdg.sta.cer.rp.means[i,1] <- 
    mean(c(
      mpg.fdg.sta.cer.rp$FDG_SUV_RP_1[i],
      mpg.fdg.sta.cer.rp$FDG_SUV_RP_2[i]), 
      na.rm = T)
  mpg.fdg.sta.cer.rp.means[i,2] <- 
    mean(c(
      mpg.fdg.sta.cer.rp$MRI_ADC_RP_1[i],
      mpg.fdg.sta.cer.rp$MRI_ADC_RP_2[i]),
      na.rm = T)
}

mpg.fdg.sta.cer.rp <- cbind(
  mpg.fdg.sta.cer.rp$HIST_RP,
  mpg.fdg.sta.cer.rp.means,
  mpg.fdg.sta.cer.rp$MRI_SA_RP
)
colnames(mpg.fdg.sta.cer.rp) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")
#View(mpg.fdg.sta.cer.rp)

mpg.fdg.sta.cer.rp <- mpg.fdg.sta.cer.rp[-which(is.na(mpg.fdg.sta.cer.rp$HIST)),]
mpg.fdg.sta.cer.rp <- mpg.fdg.sta.cer.rp[-which(is.na(mpg.fdg.sta.cer.rp$SUV_MEAN)),]
mpg.fdg.sta.cer.rp <- mpg.fdg.sta.cer.rp[-which(is.na(mpg.fdg.sta.cer.rp$ADC_MEAN)),]

mpg.fdg.sta.cer.rp <- rbind(
  mpg.fdg.sta.cer.rp[which(mpg.fdg.sta.cer.rp$HIST==1),],
  mpg.fdg.sta.cer.rp[which(mpg.fdg.sta.cer.rp$HIST==0),]
)

for(i in 1:nrow(mpg.fdg.sta.cer.rp)){
  mpg.fdg.sta.cer.rp$STA_RATIO[i] <- 
    mpg.fdg.sta.cer.rp$SUV_MEAN[i] / mpg.fdg.sta.cer.rp$ADC_MEAN[i]
}

#FDG CER PEL
mpg.fdg.sta.cer.pel <- 
  rbind(
    mpg.fdg.sta.cer.lp,
    mpg.fdg.sta.cer.rp
  )

#FDG ENDO LP
mpg.fdg.sta.endo.lp.columns <- c(
  "PATIENT",
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_SA_LP",
  "HIST_LP")

mpg.fdg.sta.endo.lp <- mpg.fdg.sta.endo[,mpg.fdg.sta.endo.lp.columns]
mpg.fdg.sta.endo.lp.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fdg.sta.endo.lp),ncol = 2))
for(i in 1:nrow(mpg.fdg.sta.endo.lp)){
  mpg.fdg.sta.endo.lp.means[i,1] <- 
    mean(c(
      mpg.fdg.sta.endo.lp$FDG_SUV_LP_1[i],
      mpg.fdg.sta.endo.lp$FDG_SUV_LP_2[i]), 
      na.rm = T)
  mpg.fdg.sta.endo.lp.means[i,2] <- 
    mean(c(
      mpg.fdg.sta.endo.lp$MRI_ADC_LP_1[i],
      mpg.fdg.sta.endo.lp$MRI_ADC_LP_2[i]),
      na.rm = T)
}
mpg.fdg.sta.endo.lp <- cbind(
  mpg.fdg.sta.endo.lp$HIST_LP,
  mpg.fdg.sta.endo.lp.means,
  mpg.fdg.sta.endo.lp$MRI_SA_LP
)
colnames(mpg.fdg.sta.endo.lp) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")

mpg.fdg.sta.endo.lp <- mpg.fdg.sta.endo.lp[-which(is.na(mpg.fdg.sta.endo.lp$HIST)),]
mpg.fdg.sta.endo.lp <- mpg.fdg.sta.endo.lp[-which(is.na(mpg.fdg.sta.endo.lp$SUV_MEAN)),]
mpg.fdg.sta.endo.lp <- mpg.fdg.sta.endo.lp[-which(is.na(mpg.fdg.sta.endo.lp$ADC_MEAN)),]

mpg.fdg.sta.endo.lp <- rbind(
  mpg.fdg.sta.endo.lp[which(mpg.fdg.sta.endo.lp$HIST==1),],
  mpg.fdg.sta.endo.lp[which(mpg.fdg.sta.endo.lp$HIST==0),]
)

for(i in 1:nrow(mpg.fdg.sta.endo.lp)){
  mpg.fdg.sta.endo.lp$STA_RATIO[i] <- 
    mpg.fdg.sta.endo.lp$SUV_MEAN[i] / mpg.fdg.sta.endo.lp$ADC_MEAN[i]
}

#FDG ENDO RP
mpg.fdg.sta.endo.rp.columns <- c(
  "PATIENT",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_SA_RP",
  "HIST_RP")

mpg.fdg.sta.endo.rp <- mpg.fdg.sta.endo[,mpg.fdg.sta.endo.rp.columns]
mpg.fdg.sta.endo.rp.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fdg.sta.endo.rp),ncol = 2))
for(i in 1:nrow(mpg.fdg.sta.endo.rp)){
  mpg.fdg.sta.endo.rp.means[i,1] <- 
    mean(c(
      mpg.fdg.sta.endo.rp$FDG_SUV_RP_1[i],
      mpg.fdg.sta.endo.rp$FDG_SUV_RP_2[i]), 
      na.rm = T)
  mpg.fdg.sta.endo.rp.means[i,2] <- 
    mean(c(
      mpg.fdg.sta.endo.rp$MRI_ADC_RP_1[i],
      mpg.fdg.sta.endo.rp$MRI_ADC_RP_2[i]),
      na.rm = T)
}
mpg.fdg.sta.endo.rp <- cbind(
  mpg.fdg.sta.endo.rp$HIST_RP,
  mpg.fdg.sta.endo.rp.means,
  mpg.fdg.sta.endo.rp$MRI_SA_RP
)
colnames(mpg.fdg.sta.endo.rp) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")
#View(mpg.fdg.sta.endo.rp)

mpg.fdg.sta.endo.rp <- mpg.fdg.sta.endo.rp[-which(is.na(mpg.fdg.sta.endo.rp$SUV_MEAN)),]
mpg.fdg.sta.endo.rp <- mpg.fdg.sta.endo.rp[-which(is.na(mpg.fdg.sta.endo.rp$ADC_MEAN)),]

mpg.fdg.sta.endo.rp <- rbind(
  mpg.fdg.sta.endo.rp[which(mpg.fdg.sta.endo.rp$HIST==1),],
  mpg.fdg.sta.endo.rp[which(mpg.fdg.sta.endo.rp$HIST==0),]
)

for(i in 1:nrow(mpg.fdg.sta.endo.rp)){
  mpg.fdg.sta.endo.rp$STA_RATIO[i] <- 
    mpg.fdg.sta.endo.rp$SUV_MEAN[i] / mpg.fdg.sta.endo.rp$ADC_MEAN[i]
}

#FDG ENDO PEL
mpg.fdg.sta.endo.pel <- 
  rbind(
    mpg.fdg.sta.endo.lp,
    mpg.fdg.sta.endo.rp
  )

#FDG ENDO PALN
mpg.fdg.sta.endo.paln.columns <- c(
  "PATIENT",
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "MRI_SA_PALN",
  "HIST_PALN")

mpg.fdg.sta.endo.paln <- mpg.fdg.sta.endo[,mpg.fdg.sta.endo.paln.columns]
mpg.fdg.sta.endo.paln.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fdg.sta.endo.paln),ncol = 2))
for(i in 1:nrow(mpg.fdg.sta.endo.paln)){
  mpg.fdg.sta.endo.paln.means[i,1] <- 
    mean(c(
      mpg.fdg.sta.endo.paln$FDG_SUV_PALN_1[i],
      mpg.fdg.sta.endo.paln$FDG_SUV_PALN_2[i]), 
      na.rm = T)
  mpg.fdg.sta.endo.paln.means[i,2] <- 
    mean(c(
      mpg.fdg.sta.endo.paln$MRI_ADC_PALN_1[i],
      mpg.fdg.sta.endo.paln$MRI_ADC_PALN_2[i]),
      na.rm = T)
}
mpg.fdg.sta.endo.paln <- cbind(
  mpg.fdg.sta.endo.paln$HIST_PALN,
  mpg.fdg.sta.endo.paln.means,
  mpg.fdg.sta.endo.paln$MRI_SA_PALN
)
colnames(mpg.fdg.sta.endo.paln) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")
#View(mpg.fdg.sta.endo.paln)

mpg.fdg.sta.endo.paln <- mpg.fdg.sta.endo.paln[-which(is.na(mpg.fdg.sta.endo.paln$HIST)),]
mpg.fdg.sta.endo.paln <- mpg.fdg.sta.endo.paln[-which(is.na(mpg.fdg.sta.endo.paln$SUV_MEAN)),]
mpg.fdg.sta.endo.paln <- mpg.fdg.sta.endo.paln[-which(is.na(mpg.fdg.sta.endo.paln$ADC_MEAN)),]

mpg.fdg.sta.endo.paln <- rbind(
  mpg.fdg.sta.endo.paln[which(mpg.fdg.sta.endo.paln$HIST==1),],
  mpg.fdg.sta.endo.paln[which(mpg.fdg.sta.endo.paln$HIST==0),]
)

for(i in 1:nrow(mpg.fdg.sta.endo.paln)){
  mpg.fdg.sta.endo.paln$STA_RATIO[i] <- 
    mpg.fdg.sta.endo.paln$SUV_MEAN[i] / mpg.fdg.sta.endo.paln$ADC_MEAN[i]
}

#FDG ENDO ALL
mpg.fdg.sta.endo.all <- 
  rbind(
    mpg.fdg.sta.endo.lp,
    mpg.fdg.sta.endo.rp,
    mpg.fdg.sta.endo.paln
  )


# 3.1.2 > FEC SUV-to-ADC ratio DF ----
mpg.fec.sta <- mpg[which(mpg$PATIENT_SCAN_FEC==1
                         &mpg$PATIENT_SCAN_MRI==1),]
mpg.fec.sta.cer <- mpg.fec.sta[which(mpg.fec.sta$CANC_CER==1),]
mpg.fec.sta.endo <- mpg.fec.sta[which(mpg.fec.sta$CANC_ENDO==1),]
mpg.fec.sta.columns <- c(
  "PATIENT",
  "PATIENT_SCAN_FEC",
  "PATIENT_SCAN_MRI",
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "CANC_CER",
  "CANC_ENDO")
mpg.fec.sta.cer <- mpg.fec.sta.cer[,mpg.fec.sta.columns]
mpg.fec.sta.endo <- mpg.fec.sta.endo[,mpg.fec.sta.columns]

#FEC SUV/ADC LP
mpg.fec.sta.cer.lp.columns <- c(
  "PATIENT",
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_SA_LP",
  "HIST_LP")

mpg.fec.sta.cer.lp <- mpg.fec.sta.cer[,mpg.fec.sta.cer.lp.columns]
mpg.fec.sta.cer.lp.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fec.sta.cer.lp),ncol = 2))
for(i in 1:nrow(mpg.fec.sta.cer.lp)){
  mpg.fec.sta.cer.lp.means[i,1] <- 
    mean(c(
      mpg.fec.sta.cer.lp$FEC_SUV_LP_1[i],
      mpg.fec.sta.cer.lp$FEC_SUV_LP_2[i]), 
      na.rm = T)
  mpg.fec.sta.cer.lp.means[i,2] <- 
    mean(c(
      mpg.fec.sta.cer.lp$MRI_ADC_LP_1[i],
      mpg.fec.sta.cer.lp$MRI_ADC_LP_2[i]),
      na.rm = T)
}
mpg.fec.sta.cer.lp <- cbind(
  mpg.fec.sta.cer.lp$HIST_LP,
  mpg.fec.sta.cer.lp.means,
  mpg.fec.sta.cer.lp$MRI_SA_LP
)
colnames(mpg.fec.sta.cer.lp) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")
#View(mpg.fec.sta.cer.lp)

mpg.fec.sta.cer.lp <- mpg.fec.sta.cer.lp[-which(is.na(mpg.fec.sta.cer.lp$SUV_MEAN)),]
mpg.fec.sta.cer.lp <- mpg.fec.sta.cer.lp[-which(is.na(mpg.fec.sta.cer.lp$ADC_MEAN)),]

mpg.fec.sta.cer.lp <- rbind(
  mpg.fec.sta.cer.lp[which(mpg.fec.sta.cer.lp$HIST==1),],
  mpg.fec.sta.cer.lp[which(mpg.fec.sta.cer.lp$HIST==0),]
)

for(i in 1:nrow(mpg.fec.sta.cer.lp)){
  mpg.fec.sta.cer.lp$STA_RATIO[i] <- 
    mpg.fec.sta.cer.lp$SUV_MEAN[i] / mpg.fec.sta.cer.lp$ADC_MEAN[i]
}

#FEC CER RP
mpg.fec.sta.cer.rp.columns <- c(
  "PATIENT",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_SA_RP",
  "HIST_RP")

mpg.fec.sta.cer.rp <- mpg.fec.sta.cer[,mpg.fec.sta.cer.rp.columns]
mpg.fec.sta.cer.rp.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fec.sta.cer.rp),ncol = 2))
for(i in 1:nrow(mpg.fec.sta.cer.rp)){
  mpg.fec.sta.cer.rp.means[i,1] <- 
    mean(c(
      mpg.fec.sta.cer.rp$FEC_SUV_RP_1[i],
      mpg.fec.sta.cer.rp$FEC_SUV_RP_2[i]), 
      na.rm = T)
  mpg.fec.sta.cer.rp.means[i,2] <- 
    mean(c(
      mpg.fec.sta.cer.rp$MRI_ADC_RP_1[i],
      mpg.fec.sta.cer.rp$MRI_ADC_RP_2[i]),
      na.rm = T)
}
mpg.fec.sta.cer.rp <- cbind(
  mpg.fec.sta.cer.rp$HIST_RP,
  mpg.fec.sta.cer.rp.means,
  mpg.fec.sta.cer.rp$MRI_SA_RP
)
colnames(mpg.fec.sta.cer.rp) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")
#View(mpg.fec.sta.cer.rp)

mpg.fec.sta.cer.rp <- mpg.fec.sta.cer.rp[-which(is.na(mpg.fec.sta.cer.rp$SUV_MEAN)),]

mpg.fec.sta.cer.rp <- rbind(
  mpg.fec.sta.cer.rp[which(mpg.fec.sta.cer.rp$HIST==1),],
  mpg.fec.sta.cer.rp[which(mpg.fec.sta.cer.rp$HIST==0),]
)

for(i in 1:nrow(mpg.fec.sta.cer.rp)){
  mpg.fec.sta.cer.rp$STA_RATIO[i] <- 
    mpg.fec.sta.cer.rp$SUV_MEAN[i] / mpg.fec.sta.cer.rp$ADC_MEAN[i]
}

#fec CER PEL
mpg.fec.sta.cer.pel <- 
  rbind(
    mpg.fec.sta.cer.lp,
    mpg.fec.sta.cer.rp
  )


#FEC ENDO LP
mpg.fec.sta.endo.lp.columns <- c(
  "PATIENT",
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_SA_LP",
  "HIST_LP")

mpg.fec.sta.endo.lp <- mpg.fec.sta.endo[,mpg.fec.sta.endo.lp.columns]
mpg.fec.sta.endo.lp.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fec.sta.endo.lp),ncol = 2))
for(i in 1:nrow(mpg.fec.sta.endo.lp)){
  mpg.fec.sta.endo.lp.means[i,1] <- 
    mean(c(
      mpg.fec.sta.endo.lp$FEC_SUV_LP_1[i],
      mpg.fec.sta.endo.lp$FEC_SUV_LP_2[i]), 
      na.rm = T)
  mpg.fec.sta.endo.lp.means[i,2] <- 
    mean(c(
      mpg.fec.sta.endo.lp$MRI_ADC_LP_1[i],
      mpg.fec.sta.endo.lp$MRI_ADC_LP_2[i]),
      na.rm = T)
}
mpg.fec.sta.endo.lp <- cbind(
  mpg.fec.sta.endo.lp$HIST_LP,
  mpg.fec.sta.endo.lp.means,
  mpg.fec.sta.endo.lp$MRI_SA_LP
)
colnames(mpg.fec.sta.endo.lp) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")
#View(mpg.fec.sta.endo.lp)

mpg.fec.sta.endo.lp <- mpg.fec.sta.endo.lp[-which(is.na(mpg.fec.sta.endo.lp$HIST)),]
mpg.fec.sta.endo.lp <- mpg.fec.sta.endo.lp[-which(is.na(mpg.fec.sta.endo.lp$ADC_MEAN)),]

mpg.fec.sta.endo.lp <- rbind(
  mpg.fec.sta.endo.lp[which(mpg.fec.sta.endo.lp$HIST==1),],
  mpg.fec.sta.endo.lp[which(mpg.fec.sta.endo.lp$HIST==0),]
)

for(i in 1:nrow(mpg.fec.sta.endo.lp)){
  mpg.fec.sta.endo.lp$STA_RATIO[i] <- 
    mpg.fec.sta.endo.lp$SUV_MEAN[i] / mpg.fec.sta.endo.lp$ADC_MEAN[i]
}

#FEC ENDO RP
mpg.fec.sta.endo.rp.columns <- c(
  "PATIENT",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_SA_RP",
  "HIST_RP")

mpg.fec.sta.endo.rp <- mpg.fec.sta.endo[,mpg.fec.sta.endo.rp.columns]
mpg.fec.sta.endo.rp.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fec.sta.endo.rp),ncol = 2))
for(i in 1:nrow(mpg.fec.sta.endo.rp)){
  mpg.fec.sta.endo.rp.means[i,1] <- 
    mean(c(
      mpg.fec.sta.endo.rp$FEC_SUV_RP_1[i],
      mpg.fec.sta.endo.rp$FEC_SUV_RP_2[i]), 
      na.rm = T)
  mpg.fec.sta.endo.rp.means[i,2] <- 
    mean(c(
      mpg.fec.sta.endo.rp$MRI_ADC_RP_1[i],
      mpg.fec.sta.endo.rp$MRI_ADC_RP_2[i]),
      na.rm = T)
}
mpg.fec.sta.endo.rp <- cbind(
  mpg.fec.sta.endo.rp$HIST_RP,
  mpg.fec.sta.endo.rp.means,
  mpg.fec.sta.endo.rp$MRI_SA_RP
)
colnames(mpg.fec.sta.endo.rp) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")
#View(mpg.fec.sta.endo.rp)

mpg.fec.sta.endo.rp <- mpg.fec.sta.endo.rp[-which(is.na(mpg.fec.sta.endo.rp$ADC_MEAN)),]
mpg.fec.sta.endo.rp <- mpg.fec.sta.endo.rp[-which(is.na(mpg.fec.sta.endo.rp$SUV_MEAN)),]

mpg.fec.sta.endo.rp <- rbind(
  mpg.fec.sta.endo.rp[which(mpg.fec.sta.endo.rp$HIST==1),],
  mpg.fec.sta.endo.rp[which(mpg.fec.sta.endo.rp$HIST==0),]
)

for(i in 1:nrow(mpg.fec.sta.endo.rp)){
  mpg.fec.sta.endo.rp$STA_RATIO[i] <- 
    mpg.fec.sta.endo.rp$SUV_MEAN[i] / mpg.fec.sta.endo.rp$ADC_MEAN[i]
}

#fec ENDO PEL
mpg.fec.sta.endo.pel <- 
  rbind(
    mpg.fec.sta.endo.lp,
    mpg.fec.sta.endo.rp
  )


#FEC ENDO PALN
mpg.fec.sta.endo.paln.columns <- c(
  "PATIENT",
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "MRI_SA_PALN",
  "HIST_PALN")

mpg.fec.sta.endo.paln <- mpg.fec.sta.endo[,mpg.fec.sta.endo.paln.columns]
mpg.fec.sta.endo.paln.means <- data.frame(matrix(
  NA, nrow = nrow(mpg.fec.sta.endo.paln),ncol = 2))
for(i in 1:nrow(mpg.fec.sta.endo.paln)){
  mpg.fec.sta.endo.paln.means[i,1] <- 
    mean(c(
      mpg.fec.sta.endo.paln$FEC_SUV_PALN_1[i],
      mpg.fec.sta.endo.paln$FEC_SUV_PALN_2[i]), 
      na.rm = T)
  mpg.fec.sta.endo.paln.means[i,2] <- 
    mean(c(
      mpg.fec.sta.endo.paln$MRI_ADC_PALN_1[i],
      mpg.fec.sta.endo.paln$MRI_ADC_PALN_2[i]),
      na.rm = T)
}
mpg.fec.sta.endo.paln <- cbind(
  mpg.fec.sta.endo.paln$HIST_PALN,
  mpg.fec.sta.endo.paln.means,
  mpg.fec.sta.endo.paln$MRI_SA_PALN
)
colnames(mpg.fec.sta.endo.paln) <- c("HIST","SUV_MEAN","ADC_MEAN","MRI_SA")
#View(mpg.fec.sta.endo.paln)

mpg.fec.sta.endo.paln <- mpg.fec.sta.endo.paln[-which(is.na(mpg.fec.sta.endo.paln$HIST)),]
mpg.fec.sta.endo.paln <- mpg.fec.sta.endo.paln[-which(is.na(mpg.fec.sta.endo.paln$SUV_MEAN)),]
mpg.fec.sta.endo.paln <- mpg.fec.sta.endo.paln[-which(is.na(mpg.fec.sta.endo.paln$ADC_MEAN)),]

mpg.fec.sta.endo.paln <- rbind(
  mpg.fec.sta.endo.paln[which(mpg.fec.sta.endo.paln$HIST==1),],
  mpg.fec.sta.endo.paln[which(mpg.fec.sta.endo.paln$HIST==0),]
)

for(i in 1:nrow(mpg.fec.sta.endo.paln)){
  mpg.fec.sta.endo.paln$STA_RATIO[i] <- 
    mpg.fec.sta.endo.paln$SUV_MEAN[i] / mpg.fec.sta.endo.paln$ADC_MEAN[i]
}

#fec ENDO ALL
mpg.fec.sta.endo.all <- 
  rbind(
    mpg.fec.sta.endo.lp,
    mpg.fec.sta.endo.rp,
    mpg.fec.sta.endo.paln
  )



## 3.2 - MW U-test ----
mpg.fdg.sta.cer.lp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fdg.sta.cer.lp)
mpg.fdg.sta.cer.rp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fdg.sta.cer.rp)
mpg.fdg.sta.cer.pel.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fdg.sta.cer.pel)

mpg.fdg.sta.endo.lp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fdg.sta.endo.lp)
mpg.fdg.sta.endo.rp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fdg.sta.endo.rp)
mpg.fdg.sta.endo.paln.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fdg.sta.endo.paln)
mpg.fdg.sta.endo.pel.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fdg.sta.endo.pel)
mpg.fdg.sta.endo.rp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fdg.sta.endo.rp)

mpg.fec.sta.cer.lp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fec.sta.cer.lp)
mpg.fec.sta.cer.rp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fec.sta.cer.rp)
mpg.fec.sta.cer.pel.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fec.sta.cer.pel)

mpg.fec.sta.endo.lp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fec.sta.endo.lp)
mpg.fec.sta.endo.rp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fec.sta.endo.rp)
mpg.fec.sta.endo.paln.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fec.sta.endo.paln)
mpg.fec.sta.endo.pel.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fec.sta.endo.pel)
mpg.fec.sta.endo.rp.mwu <- wilcox.test(STA_RATIO~HIST, data = mpg.fec.sta.endo.rp)

#Store results
mpg.sta.mwu.results.rows <- c(
  "FDG-ADC CER LP", 
  "FDG-ADC CER RP",
  "FDG-ADC CER PEL",
  "FDG-ADC ENDO LP", 
  "FDG-ADC ENDO RP",
  "FDG-ADC ENDO PALN",
  "FDG-ADC ENDO PEL",
  "FDG-ADC ENDO ALL",
  "FEC-ADC CER LP",
  "FEC-ADC CER RP",
  "FEC-ADC CER PEL",
  "FEC-ADC ENDO LP",
  "FEC-ADC ENDO RP",
  "FEC-ADC ENDO PALN",
  "FEC-ADC ENDO PEL",
  "FEC-ADC ENDO ALL"
)

mpg.sta.mwu.results <- list(
  mpg.fdg.sta.cer.lp.mwu,
  mpg.fdg.sta.cer.rp.mwu,
  mpg.fdg.sta.cer.pel.mwu,
  mpg.fdg.sta.endo.lp.mwu,
  mpg.fdg.sta.endo.rp.mwu,
  mpg.fdg.sta.endo.paln.mwu,
  mpg.fdg.sta.endo.pel.mwu,
  mpg.fdg.sta.endo.rp.mwu,
  mpg.fec.sta.cer.lp.mwu,
  mpg.fec.sta.cer.rp.mwu,
  mpg.fec.sta.cer.pel.mwu,
  mpg.fec.sta.endo.lp.mwu,
  mpg.fec.sta.endo.rp.mwu,
  mpg.fec.sta.endo.paln.mwu,
  mpg.fec.sta.endo.pel.mwu,
  mpg.fec.sta.endo.rp.mwu
)

mpg.sta.mwu.results.df <- data.frame(matrix(NA, ncol=2, nrow=length(mpg.sta.mwu.results)))
colnames(mpg.sta.mwu.results.df) <- c("W-statistic", "p-value")
rownames(mpg.sta.mwu.results.df) <- mpg.sta.mwu.results.rows

for(i in 1:length(mpg.sta.mwu.results)){
  mpg.sta.mwu.results.df[i,1] <- mpg.sta.mwu.results[[i]][1]
  mpg.sta.mwu.results.df[i,2] <- mpg.sta.mwu.results[[i]][3]
}

#Adjust p-values for multiple testing
mpg.sta.mwu.results.df$adj.p.val <- p.adjust(mpg.sta.mwu.results.df$`p-value`, method="fdr")

## 3.3 - Visualise results with boxplots (active) ----
# library(ggpubr)
#Multi-plot 1 - FDG ,FEC, MRI in ENDO LP
#Plot 1
mpg.fdg.sta.endo.all <- as.data.frame(mpg.fdg.sta.endo.all)
mpg.fdg.sta.endo.all$HIST <- factor(mpg.fdg.sta.endo.all$HIST)
mpg.fdg.sta.endo.all.bp <- ggplot(data = as.data.frame(mpg.fdg.sta.endo.all),
                                  aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="STAR") +
  ylim(0,40) + 
  annotate("text", x = 1.5, y = 30, 
           label = expression(rho<".01")) + 
  scale_x_discrete(labels= c("n=30","n=22")) + 
  geom_bracket(xmin = "0", 
               xmax = "1",
               y.position = 38,
               label = "*",
               tip.length = c(0.54,0.02),
               size = 0.5,
               inherit.aes = FALSE)
#Plot 2
mpg.fdg.sta.endo.pel <- as.data.frame(mpg.fdg.sta.endo.pel)
mpg.fdg.sta.endo.pel$HIST <- factor(mpg.fdg.sta.endo.pel$HIST)
mpg.fdg.sta.endo.pel.bp <- ggplot(data = as.data.frame(mpg.fdg.sta.endo.pel),
       aes(x=HIST,y=STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,40) + 
  annotate("text", x = 1.5, y = 30, label = expression(rho<".01")) + 
  scale_x_discrete(labels= c("n=25","n=19")) +
  geom_bracket(xmin = "0", 
               xmax = "1",
               y.position = 38,
               label = "*",
               tip.length = c(0.6,0.02),
               size = 0.5,
               inherit.aes = FALSE)
#Plot 3
mpg.fdg.sta.endo.lp <- as.data.frame(mpg.fdg.sta.endo.lp)
mpg.fdg.sta.endo.lp$HIST <- factor(mpg.fdg.sta.endo.lp$HIST)
mpg.fdg.sta.endo.lp.bp <- ggplot(data = as.data.frame(mpg.fdg.sta.endo.lp),
                                 aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,40) +  
  annotate("text", x = 1.5, y = 30, label = expression(rho<".01")) + 
  scale_x_discrete(labels= c("n=12","n=7")) + 
  geom_bracket(
            xmin = "0", xmax = "1",
            y.position = 38,
            label = "*",
            tip.length = c(0.6,0.25),
            size = 0.5,
            inherit.aes = FALSE)
#Plot 4
mpg.fdg.sta.endo.rp <- as.data.frame(mpg.fdg.sta.endo.rp)
mpg.fdg.sta.endo.rp$HIST <- factor(mpg.fdg.sta.endo.rp$HIST)
mpg.fdg.sta.endo.rp.bp <- ggplot(data = as.data.frame(mpg.fdg.sta.endo.rp),
                                 aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,40) +  
  annotate(
            "text", x = 1.5, y = 30, label = expression(rho<".01")) + 
  scale_x_discrete(
              labels= c("n=13","n=12")) + 
  geom_bracket(
            xmin = "0", xmax = "1",
            y.position = 38,
            label = "*",
            tip.length = c(0.62,0.02),
            size = 0.5,
            inherit.aes = FALSE)
#Plot 5
mpg.fdg.sta.endo.paln <- as.data.frame(mpg.fdg.sta.endo.paln)
mpg.fdg.sta.endo.paln$HIST <- factor(mpg.fdg.sta.endo.paln$HIST)
mpg.fdg.sta.endo.paln.bp <- ggplot(data = as.data.frame(mpg.fdg.sta.endo.paln),
                                   aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,40) + 
  annotate("text", x = 1.5, y = 30, label = expression(rho=="1.0")) + 
  scale_x_discrete(labels= c("n=5","n=3"))
#Plot 1
mpg.fec.sta.endo.all <- as.data.frame(mpg.fec.sta.endo.all)
mpg.fec.sta.endo.all$HIST <- factor(mpg.fec.sta.endo.all$HIST)
mpg.fec.sta.endo.all.bp <- ggplot(data = as.data.frame(mpg.fec.sta.endo.all),
                                  aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="STAR") +
  ylim(0,15) + 
  annotate("text", x = 1.5, y = 11, label = expression(rho==".24")) + 
  scale_x_discrete(labels= c("n=14","n=8"))
#Plot 2
mpg.fec.sta.endo.pel <- as.data.frame(mpg.fec.sta.endo.pel)
mpg.fec.sta.endo.pel$HIST <- factor(mpg.fec.sta.endo.pel$HIST)
mpg.fec.sta.endo.pel.bp <- ggplot(data = as.data.frame(mpg.fec.sta.endo.pel),
                                  aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,12) + 
  annotate("text", x = 1.5, y = 11, label = expression(rho==".031")) + 
  scale_x_discrete(labels= c("n=12","n=7")) + 
  font("ylab", size=10) + 
  geom_bracket(xmin = "0", 
               xmax = "1",
               y.position = 12,
               label = "*",
               tip.length = c(0.55,0.02),
               size = 0.5,
               inherit.aes = FALSE)
#Plot 3
mpg.fec.sta.endo.lp <- as.data.frame(mpg.fec.sta.endo.lp)
mpg.fec.sta.endo.lp$HIST <- factor(mpg.fec.sta.endo.lp$HIST)
mpg.fec.sta.endo.lp.bp <- ggplot(data = as.data.frame(mpg.fec.sta.endo.lp),
                                 aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,12) + 
  annotate("text", x = 1.5, y = 11, label = expression(rho==".13")) + 
  scale_x_discrete(labels= c("n=7","n=2")) + 
  font("ylab", size=10)
#Plot 4
mpg.fec.sta.endo.rp <- as.data.frame(mpg.fec.sta.endo.rp)
mpg.fec.sta.endo.rp$HIST <- factor(mpg.fec.sta.endo.rp$HIST)
mpg.fec.sta.endo.rp.bp <- ggplot(data = as.data.frame(mpg.fec.sta.endo.rp),
                                 aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,12) + 
  annotate("text", x = 1.5, y = 11, label = expression(rho==".24")) + 
  scale_x_discrete(labels= c("n=5","n=5"))
#Plot 5
mpg.fec.sta.endo.paln <- as.data.frame(mpg.fec.sta.endo.paln)
mpg.fec.sta.endo.paln$HIST <- factor(mpg.fec.sta.endo.paln$HIST)
mpg.fec.sta.endo.paln.bp <- ggplot(data = as.data.frame(mpg.fec.sta.endo.paln),
                                   aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,12) + 
  annotate("text", x = 1.5, y = 11, label = expression(rho=="1.0")) + 
  scale_x_discrete(labels= c("n=2","n=1"))

png("STA_FDG_FEC_ENDO_BP.png",width = 12, height = 5, units ='in', res = 300)
ggarrange(mpg.fdg.sta.endo.all.bp,
          mpg.fdg.sta.endo.pel.bp,
          mpg.fdg.sta.endo.lp.bp,
          mpg.fdg.sta.endo.rp.bp,
          mpg.fdg.sta.endo.paln.bp,
          mpg.fec.sta.endo.all.bp,
          mpg.fec.sta.endo.pel.bp,
          mpg.fec.sta.endo.lp.bp,
          mpg.fec.sta.endo.rp.bp,
          mpg.fec.sta.endo.paln.bp,
          label.x = -.05,
          label.y = 1,
          labels = c("A1","A2","A3","A4","A5","B1","B2","B3","B4","B5"),
          ncol = 5, nrow = 2,
          align = "hv",
          common.legend = TRUE)
dev.off()

#CER
#Plot 1
mpg.fdg.sta.cer.pel <- as.data.frame(mpg.fdg.sta.cer.pel)
mpg.fdg.sta.cer.pel$HIST <- factor(mpg.fdg.sta.cer.pel$HIST)
mpg.fdg.sta.cer.pel.bp <- ggplot(data = as.data.frame(mpg.fdg.sta.cer.pel),
                                 aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="STAR") +
  ylim(0,15) + annotate("text", x = 1.5, y = 12, label = expression(rho==".12")) + scale_x_discrete(
                                         labels= c("n=6","n=5")) + font("ylab", size = 13)
#Plot 3
mpg.fdg.sta.cer.lp <- as.data.frame(mpg.fdg.sta.cer.lp)
mpg.fdg.sta.cer.lp$HIST <- factor(mpg.fdg.sta.cer.lp$HIST)
mpg.fdg.sta.cer.lp.bp <- ggplot(data = as.data.frame(mpg.fdg.sta.cer.lp),
                                aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,15) + annotate("text", x = 1.5, y = 12, label = expression(rho==".24")) + scale_x_discrete(
                                        labels= c("n=2","n=4")) 
#Plot 4
mpg.fdg.sta.cer.rp <- as.data.frame(mpg.fdg.sta.cer.rp)
mpg.fdg.sta.cer.rp$HIST <- factor(mpg.fdg.sta.cer.rp$HIST)
mpg.fdg.sta.cer.rp.bp <- ggplot(data = as.data.frame(mpg.fdg.sta.cer.rp),
                                aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,15) + annotate("text", x = 1.5, y = 12, label = expression(rho=="1.0")) + scale_x_discrete(
                                        labels= c("n=4","n=1")) + font("ylab", size = 13)
#Plot 1
mpg.fec.sta.cer.pel <- as.data.frame(mpg.fec.sta.cer.pel)
mpg.fec.sta.cer.pel$HIST <- factor(mpg.fec.sta.cer.pel$HIST)
mpg.fec.sta.cer.pel.bp <- ggplot(data = as.data.frame(mpg.fec.sta.cer.pel),
                                 aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="STAR") +
  ylim(0,10) + annotate(
                                       "text", x = 1.5, y = 9, label = expression(rho==".58")) + scale_x_discrete(
                                         labels= c("n=2","n=4")) + font("ylab", size=13) 
#Plot 2
mpg.fec.sta.cer.lp <- as.data.frame(mpg.fec.sta.cer.lp)
mpg.fec.sta.cer.lp$HIST <- factor(mpg.fec.sta.cer.lp$HIST)
mpg.fec.sta.cer.lp.bp <- ggplot(data = as.data.frame(mpg.fec.sta.cer.lp),
                                aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,10) + annotate("text", x = 1.5, y = 9, label = expression(rho=="1.0")) + scale_x_discrete(
                                        labels= c("n=1","n=2")) 
#Plot 3
mpg.fec.sta.cer.rp <- as.data.frame(mpg.fec.sta.cer.rp)
mpg.fec.sta.cer.rp$HIST <- factor(mpg.fec.sta.cer.rp$HIST)
mpg.fec.sta.cer.rp.bp <- ggplot(data = as.data.frame(mpg.fec.sta.cer.rp),
                                aes(x = HIST,y= STA_RATIO, color=HIST)) + 
  theme_classic() +
  theme(text = element_text(size = 15)) +
  scale_color_manual(values = c("black","red"), name = "Histology", labels = c("Benign","Malignant")) +
  geom_point(position = position_jitter(width = 0.1, height = 0, seed = 2), alpha = 0.3) + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               color = "#0388fc",
               size = .6,
               stroke = 1,
               pch = 23,
               fill = "yellow") +
  labs(x="",y="") +
  ylim(0,10) + annotate("text", x = 1.5, y = 9, label = expression(rho=="1.0")) + scale_x_discrete(
                                        labels= c("n=3","n=1")) 


png("STA_FDG_FEC_CER_BP.png",width = 8, height = 5, units ='in', res = 300)
ggarrange(mpg.fdg.sta.cer.pel.bp,
          mpg.fdg.sta.cer.lp.bp,
          mpg.fdg.sta.cer.rp.bp,
          mpg.fec.sta.cer.pel.bp,
          mpg.fec.sta.cer.lp.bp,
          mpg.fec.sta.cer.rp.bp,
          label.x = -.05,
          label.y = 1,
          labels = c("A1","A2","A3","B1","B2","B3"),
          ncol = 3, nrow = 2,
          align = "hv",
          common.legend = TRUE)
dev.off()
#....----

# 4.1 - ROC curve analysis ----
#Using cutpointr, construct ROC that maximises sens + spec
#For significant:
#UNI - FDG ENDO ALL, PEL, LP, RP
#NTR - FDG ENDO ALL, PEL
#STA - FDG ENDO LP, RP, PEL, ALL 
#STA - FEC ENDO PEL
library(cutpointr)
library(pROC)

# 4.1.1 > Cutpointr analysis ----
#FDG
#UNI
#FDG CER PEL
mpg.fdg.uni.cer.pel.df <- as.data.frame(mpg.fdg.uni.cer.pel.df)
mpg.fdg.uni.cer.pel.cp <- cutpointr(mpg.fdg.uni.cer.pel.df,MEAN,HIST,method=maximize_metric,metric = youden)
summary(mpg.fdg.uni.cer.pel.cp)
#FDG ENDO ALL
mpg.fdg.uni.endo.all.df <- as.data.frame(mpg.fdg.uni.endo.all.df)
mpg.fdg.uni.endo.all.cp <- cutpointr(mpg.fdg.uni.endo.all.df,MEAN,HIST,method=maximize_metric,metric = youden)
summary(mpg.fdg.uni.endo.all.cp)

#NTR
#FDG CER PEL
mpg.fdg.ntr.cer.pel.df <- as.data.frame(mpg.fdg.ntr.cer.pel.df)
mpg.fdg.ntr.cer.pel.cp <- cutpointr(mpg.fdg.ntr.cer.pel.df,LN_T_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.fdg.ntr.cer.pel.cp)
#FDG ENDO ALL
mpg.fdg.ntr.endo.all.df <- as.data.frame(mpg.fdg.ntr.endo.all.df)
mpg.fdg.ntr.endo.all.cp <- cutpointr(mpg.fdg.ntr.endo.all.df,LN_T_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.fdg.ntr.endo.all.cp)

#STA
#FDG CER PEL
mpg.fdg.sta.cer.pel <- as.data.frame(mpg.fdg.sta.cer.pel)
mpg.fdg.sta.cer.pel.cp <- cutpointr(mpg.fdg.sta.cer.pel,STA_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.fdg.sta.cer.pel.cp)
#FDG ENDO ALL
mpg.fdg.sta.endo.all <- as.data.frame(mpg.fdg.sta.endo.all)
mpg.fdg.sta.endo.all.cp <- cutpointr(mpg.fdg.sta.endo.all,STA_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.fdg.sta.endo.all.cp)

#FEC
#UNI
#FDG CER PEL
mpg.fec.uni.cer.pel.df <- as.data.frame(mpg.fec.uni.cer.pel.df)
mpg.fec.uni.cer.pel.cp <- cutpointr(mpg.fec.uni.cer.pel.df,MEAN,HIST,method=maximize_metric,metric = youden)
summary(mpg.fec.uni.cer.pel.cp)
#FDG ENDO ALL
mpg.fec.uni.endo.all.df <- as.data.frame(mpg.fec.uni.endo.all.df)
mpg.fec.uni.endo.all.cp <- cutpointr(mpg.fec.uni.endo.all.df,MEAN,HIST,method=maximize_metric,metric = youden)
summary(mpg.fec.uni.endo.all.cp)

#NTR
#FDG CER PEL
mpg.fec.ntr.cer.pel.df <- as.data.frame(mpg.fec.ntr.cer.pel.df)
mpg.fec.ntr.cer.pel.cp <- cutpointr(mpg.fec.ntr.cer.pel.df,LN_T_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.fec.ntr.cer.pel.cp)
#FDG ENDO ALL
mpg.fec.ntr.endo.all.df <- as.data.frame(mpg.fec.ntr.endo.all.df)
mpg.fec.ntr.endo.all.cp <- cutpointr(mpg.fec.ntr.endo.all.df,LN_T_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.fec.ntr.endo.all.cp)

#STA
#FDG CER PEL
mpg.fec.sta.cer.pel <- as.data.frame(mpg.fec.sta.cer.pel)
mpg.fec.sta.cer.pel.cp <- cutpointr(mpg.fec.sta.cer.pel,STA_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.fec.sta.cer.pel.cp)
#FDG ENDO ALL
mpg.fec.sta.endo.all <- as.data.frame(mpg.fec.sta.endo.all)
mpg.fec.sta.endo.all.cp <- cutpointr(mpg.fec.sta.endo.all,STA_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.fec.sta.endo.all.cp)

#MRI
#FDG CER PEL
mpg.mri.uni.cer.pel.df <- as.data.frame(mpg.mri.uni.cer.pel.df)
mpg.mri.uni.cer.pel.cp <- cutpointr(mpg.mri.uni.cer.pel.df,MEAN,HIST,method=maximize_metric,metric = youden)
summary(mpg.mri.uni.cer.pel.cp)
#FDG ENDO ALL
mpg.mri.uni.endo.all.df <- as.data.frame(mpg.mri.uni.endo.all.df)
mpg.mri.uni.endo.all.cp <- cutpointr(mpg.mri.uni.endo.all.df,MEAN,HIST,method=maximize_metric,metric = youden)
summary(mpg.mri.uni.endo.all.cp)

#NTR
#FDG CER PEL
mpg.mri.ntr.cer.pel.df <- as.data.frame(mpg.mri.ntr.cer.pel.df)
mpg.mri.ntr.cer.pel.cp <- cutpointr(mpg.mri.ntr.cer.pel.df,LN_T_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.mri.ntr.cer.pel.cp)
#FDG ENDO ALL
mpg.mri.ntr.endo.all.df <- as.data.frame(mpg.mri.ntr.endo.all.df)
mpg.mri.ntr.endo.all.cp <- cutpointr(mpg.mri.ntr.endo.all.df,LN_T_RATIO,HIST,method=maximize_metric,metric = youden)
summary(mpg.mri.ntr.endo.all.cp)

##Using pROC for other needs
#FDG
#UNI
#FDG CER PEL
mpg.fdg.uni.cer.pel.df <- as.data.frame(mpg.fdg.uni.cer.pel.df)
mpg.fdg.uni.cer.pel.cproc <- roc(mpg.fdg.uni.cer.pel.df,HIST,MEAN,auc = TRUE, ci = TRUE)
auc(mpg.fdg.uni.cer.pel.cproc)
ci(mpg.fdg.uni.cer.pel.cproc)

#FDG ENDO ALL
mpg.fdg.uni.endo.all.df <- as.data.frame(mpg.fdg.uni.endo.all.df)
mpg.fdg.uni.endo.all.cproc <- roc(mpg.fdg.uni.endo.all.df,HIST,MEAN,auc = TRUE, ci = TRUE)
auc(mpg.fdg.uni.endo.all.cproc)
ci(mpg.fdg.uni.endo.all.cproc)

#NTR
#FDG CER PEL
mpg.fdg.ntr.cer.pel.df <- as.data.frame(mpg.fdg.ntr.cer.pel.df)
mpg.fdg.ntr.cer.pel.cproc <- roc(mpg.fdg.uni.endo.pel.df,HIST,MEAN,auc = TRUE, ci = TRUE)
auc(mpg.fdg.uni.endo.pel.cproc)
ci(mpg.fdg.uni.endo.pel.cproc)

#FDG ENDO ALL
mpg.fdg.ntr.endo.all.df <- as.data.frame(mpg.fdg.ntr.endo.all.df)
mpg.fdg.ntr.endo.all.cproc <- roc(mpg.fdg.ntr.endo.all.df,HIST,LN_T_RATIO)
auc(mpg.fdg.ntr.endo.all.cproc)
ci(mpg.fdg.ntr.endo.all.cproc)

#STA
#FDG CER PEL
mpg.fdg.sta.cer.pel <- as.data.frame(mpg.fdg.sta.cer.pel)
mpg.fdg.sta.cer.pel.cproc <- roc(mpg.fdg.sta.cer.pel,HIST,STA_RATIO)
auc(mpg.fdg.sta.cer.pel.cproc)
ci(mpg.fdg.sta.cer.pel.cproc)

#FDG ENDO ALL
mpg.fdg.sta.endo.all <- as.data.frame(mpg.fdg.sta.endo.all)
mpg.fdg.sta.endo.all.cproc <- roc(mpg.fdg.sta.endo.all,HIST,STA_RATIO)
auc(mpg.fdg.sta.endo.all.cproc)
ci(mpg.fdg.sta.endo.all.cproc)

#FEC
#UNI
#FDG CER PEL
mpg.fec.uni.cer.pel.df <- as.data.frame(mpg.fec.uni.cer.pel.df)
mpg.fec.uni.cer.pel.cproc <- roc(mpg.fec.uni.cer.pel.df,HIST,MEAN)
auc(mpg.fec.uni.cer.pel.cproc)
ci(mpg.fec.uni.cer.pel.cproc)
#FDG ENDO ALL
mpg.fec.uni.endo.all.df <- as.data.frame(mpg.fec.uni.endo.all.df)
mpg.fec.uni.endo.all.cproc <- roc(mpg.fec.uni.endo.all.df,HIST,MEAN)
auc(mpg.fec.uni.endo.all.cproc)
ci(mpg.fec.uni.endo.all.cproc)

#NTR
#FDG CER PEL
mpg.fec.ntr.cer.pel.df <- as.data.frame(mpg.fec.ntr.cer.pel.df)
mpg.fec.ntr.cer.pel.cproc <- roc(mpg.fec.ntr.cer.pel.df,HIST,LN_T_RATIO)
auc(mpg.fec.ntr.cer.pel.cproc)
ci(mpg.fec.ntr.cer.pel.cproc)
#FDG ENDO ALL
mpg.fec.ntr.endo.all.df <- as.data.frame(mpg.fec.ntr.endo.all.df)
mpg.fec.ntr.endo.all.cproc <- roc(mpg.fec.ntr.endo.all.df,HIST,LN_T_RATIO)
auc(mpg.fec.ntr.endo.all.cproc)
ci(mpg.fec.ntr.endo.all.cproc)
#STA
#FDG CER PEL
mpg.fec.sta.cer.pel <- as.data.frame(mpg.fec.sta.cer.pel)
mpg.fec.sta.cer.pel.cproc <- roc(mpg.fec.sta.cer.pel,HIST,STA_RATIO)
auc(mpg.fec.sta.cer.pel.cproc)
ci(mpg.fec.sta.cer.pel.cproc)
#FDG ENDO ALL
mpg.fec.sta.endo.all <- as.data.frame(mpg.fec.sta.endo.all)
mpg.fec.sta.endo.all.cproc <- roc(mpg.fec.sta.endo.all,HIST,STA_RATIO)
auc(mpg.fec.sta.endo.all.cproc)
ci(mpg.fec.sta.endo.all.cproc)
#MRI
#FDG CER PEL
mpg.mri.uni.cer.pel.df <- as.data.frame(mpg.mri.uni.cer.pel.df)
mpg.mri.uni.cer.pel.cproc <- roc(mpg.mri.uni.cer.pel.df,HIST,MEAN)
auc(mpg.mri.uni.cer.pel.cproc)
ci(mpg.mri.uni.cer.pel.cproc)
#FDG ENDO ALL
mpg.mri.uni.endo.all.df <- as.data.frame(mpg.mri.uni.endo.all.df)
mpg.mri.uni.endo.all.cproc <- roc(mpg.mri.uni.endo.all.df,HIST,MEAN)
auc(mpg.mri.uni.endo.all.cproc)
ci(mpg.mri.uni.endo.all.cproc)
#NTR
#FDG CER PEL
mpg.mri.ntr.cer.pel.df <- as.data.frame(mpg.mri.ntr.cer.pel.df)
mpg.mri.ntr.cer.pel.cproc <- roc(mpg.mri.ntr.cer.pel.df,HIST,LN_T_RATIO)
auc(mpg.mri.ntr.cer.pel.cproc)
ci(mpg.mri.ntr.cer.pel.cproc)
#FDG ENDO ALL
mpg.mri.ntr.endo.all.df <- as.data.frame(mpg.mri.ntr.endo.all.df)
mpg.mri.ntr.endo.all.cproc <- roc(mpg.mri.ntr.endo.all.df,HIST,LN_T_RATIO)
auc(mpg.mri.ntr.endo.all.cproc)
ci(mpg.mri.ntr.endo.all.cproc)


#Extract sensitivities, specificities
mpg.roc.results.df <- data.frame(matrix(NA, nrow = 16, ncol = 10))
colnames(mpg.roc.results.df) <- c(
  "sens",
  "spec",
  "cutpoint",
  "AUC",
  "lci",
  "uci",
  "tp",
  "fn",
  "fp",
  "tn"
)
rownames(mpg.roc.results.df) <- c(
  "UNI FDG CER PEL",
  "UNI FDG ENDO ALL",
  "NTR FDG CER PEL",
  "NTR FDG ENDO ALL",
  "STA FDG CER PEL",
  "STA FDG ENDO ALL",
  "UNI FEC CER PEL",
  "UNI FEC ENDO ALL",
  "NTR FEC CER PEL",
  "NTR FEC ENDO ALL",
  "STA FEC CER PEL",
  "STA FEC ENDO ALL",
  "UNI MRI CER PEL",
  "UNI MRI ENDO ALL",
  "NTR MRI CER PEL",
  "NTR MRI ENDO ALL"
)

mpg.roc.results <- list(
  mpg.fdg.uni.cer.pel.cp,
  mpg.fdg.uni.endo.all.cp,
  mpg.fdg.ntr.cer.pel.cp,
  mpg.fdg.ntr.endo.all.cp,
  mpg.fdg.sta.cer.pel.cp,
  mpg.fdg.sta.endo.all.cp,
  mpg.fec.uni.cer.pel.cp,
  mpg.fec.uni.endo.all.cp,
  mpg.fec.ntr.cer.pel.cp,
  mpg.fec.ntr.endo.all.cp,
  mpg.fec.sta.cer.pel.cp,
  mpg.fec.sta.endo.all.cp,
  mpg.mri.uni.cer.pel.cp,
  mpg.mri.uni.endo.all.cp,
  mpg.mri.ntr.cer.pel.cp,
  mpg.mri.ntr.endo.all.cp
)

mpg.roc.results.cproc <- list(
  mpg.fdg.uni.cer.pel.cproc,
  mpg.fdg.uni.endo.all.cproc,
  mpg.fdg.ntr.cer.pel.cproc,
  mpg.fdg.ntr.endo.all.cproc,
  mpg.fdg.sta.cer.pel.cproc,
  mpg.fdg.sta.endo.all.cproc,
  mpg.fec.uni.cer.pel.cproc,
  mpg.fec.uni.endo.all.cproc,
  mpg.fec.ntr.cer.pel.cproc,
  mpg.fec.ntr.endo.all.cproc,
  mpg.fec.sta.cer.pel.cproc,
  mpg.fec.sta.endo.all.cproc,
  mpg.mri.uni.cer.pel.cproc,
  mpg.mri.uni.endo.all.cproc,
  mpg.mri.ntr.cer.pel.cproc,
  mpg.mri.ntr.endo.all.cproc
)

for(i in 1:length(mpg.roc.results)){
  mpg.roc.results.df$sens[i] <- mpg.roc.results[[i]]$sensitivity
  mpg.roc.results.df$spec[i] <- mpg.roc.results[[i]]$specificity
  mpg.roc.results.df$cutpoint[i] <- mpg.roc.results[[i]]$optimal_cutpoint
  mpg.roc.results.df$AUC[i] <- auc(mpg.roc.results.cproc[[i]])
  mpg.roc.results.df$lci[i] <- ci(mpg.roc.results.cproc[[i]])[1]
  mpg.roc.results.df$uci[i] <- ci(mpg.roc.results.cproc[[i]])[3]
  mpg.roc.results.df$tp[i] <- summary(mpg.roc.results[[i]])$confusion_matrix[[1]]$tp
  mpg.roc.results.df$fn[i] <- summary(mpg.roc.results[[i]])$confusion_matrix[[1]]$fn
  mpg.roc.results.df$fp[i] <- summary(mpg.roc.results[[i]])$confusion_matrix[[1]]$fp
  mpg.roc.results.df$tn[i] <- summary(mpg.roc.results[[i]])$confusion_matrix[[1]]$tn
}

#PPV
for(i in 1:nrow(mpg.roc.results.df)){
  mpg.roc.results.df$PPV[i] <- (mpg.roc.results.df$tp[i]/(mpg.roc.results.df$tp[i]+mpg.roc.results.df$fp[i]))*100
}

#NPV
for(i in 1:nrow(mpg.roc.results.df)){
  mpg.roc.results.df$NPV[i] <- (mpg.roc.results.df$tn[i]/(mpg.roc.results.df$fn[i]+mpg.roc.results.df$tn[i]))*100
}

#Positive LR
for(i in 1:nrow(mpg.roc.results.df)){
  mpg.roc.results.df$PLR[i] <- mpg.roc.results.df$sens[i]/(1-mpg.roc.results.df$spec[i])
}

#Negative LR
for(i in 1:nrow(mpg.roc.results.df)){
  mpg.roc.results.df$NLR[i] <- (1-mpg.roc.results.df$sens[i])/mpg.roc.results.df$spec[i]
}

#Youden's J index
for(i in 1:nrow(mpg.roc.results.df)){
  mpg.roc.results.df$youden[i] <- (mpg.roc.results.df$sens[i]+mpg.roc.results.df$spec[i])-1
}

#Accuracy
for(i in 1:nrow(mpg.roc.results.df)){
  mpg.roc.results.df$accuracy[i] <- 
    ((mpg.roc.results.df$tn[i]+mpg.roc.results.df$tp[i])/
    (mpg.roc.results.df$tn[i]+mpg.roc.results.df$tp[i]+mpg.roc.results.df$fn[i]+mpg.roc.results.df$fp[i]))*100
}

write.csv(mpg.roc.results.df, "roc_results_table.csv")
# 4.1.2 > ROC plots ----
#Plot multiple ROCs on one graph:
library(pROC)
library(ggplot2)

#Utility of FDG scans in CER.
mpg.fdg.uni.cer.pel.roc <- roc(mpg.fdg.uni.cer.pel.df,HIST,MEAN)
mpg.fdg.ntr.cer.pel.roc <- roc(mpg.fdg.ntr.cer.pel.df,HIST,LN_T_RATIO)
mpg.fdg.sta.cer.pel.roc <- roc(mpg.fdg.sta.cer.pel,HIST,STA_RATIO)

mpg.roc.fdg.cer.pel.list <- list(
  mpg.fdg.uni.cer.pel.roc,
  mpg.fdg.ntr.cer.pel.roc,
  mpg.fdg.sta.cer.pel.roc)
#png("ROC_FDG_CER_PEL.png",width = 6, height = 6, units ='in', res = 300)
mpg.roc.fdg.cer.pel <- ggroc(mpg.roc.fdg.cer.pel.list,
                             legacy.axes = TRUE) +
  theme_bw() +
  geom_line(size=1) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("black","red","orange"), labels = c("QM","NTR","STAR")) +
  rremove("x.axis") +
  annotate("text",x = 0.75, y = 0.3, label = "AUC: 0.69 (0.35-1.0)", size = 3) +
  annotate("text",x = 0.75, y = 0.2, label = "AUC: 0.57 (0.79-0.97)", size = 3) + 
  annotate("text",x = 0.75, y = 0.1, label = "AUC: 0.87 (0.64-1.0)", size = 3)
#dev.off()

#Utility of FDG scans in ENDO.
mpg.fdg.uni.endo.all.roc <- roc(mpg.fdg.uni.endo.all.df,HIST,MEAN)
mpg.fdg.ntr.endo.all.roc <- roc(mpg.fdg.ntr.endo.all.df,HIST,LN_T_RATIO)
mpg.fdg.sta.endo.all.roc <- roc(mpg.fdg.sta.endo.all,HIST,STA_RATIO)

mpg.roc.fdg.endo.all.list <- list(
  mpg.fdg.uni.endo.all.roc,
  mpg.fdg.ntr.endo.all.roc,
  mpg.fdg.sta.endo.all.roc)
#png("ROC_FDG_ENDO_ALL.png",width = 6, height = 6, units ='in', res = 300)
mpg.roc.fdg.endo.all <- ggroc(mpg.roc.fdg.endo.all.list,
                              legacy.axes = TRUE) +
  theme_bw() +
  geom_line(size=1) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("black","red","orange"), labels = c("QM","NTR","STAR")) +
  rremove("x.axis") +
  annotate("text",x = 0.75, y = 0.3, label = "AUC: 0.84 (0.74-0.94)", size = 3) +
  annotate("text",x = 0.75, y = 0.2, label = "AUC: 0.80 (0.67-0.92)", size = 3) +
  annotate("text",x = 0.75, y = 0.1, label = "AUC: 0.86 (0.76-0.96)", size = 3)
  
#dev.off()

#Utility of FEC scans in CER.
mpg.fec.uni.cer.pel.roc <- roc(mpg.fec.uni.cer.pel.df,HIST,MEAN)
mpg.fec.ntr.cer.pel.roc <- roc(mpg.fec.ntr.cer.pel.df,HIST,LN_T_RATIO)
mpg.fec.sta.cer.pel.roc <- roc(mpg.fec.sta.cer.pel,HIST,STA_RATIO)

mpg.roc.fec.cer.pel.list <- list(
  mpg.fec.uni.cer.pel.roc,
  mpg.fec.ntr.cer.pel.roc,
  mpg.fec.sta.cer.pel.roc)
#png("ROC_FEC_ENDO_ALL.png",width = 6, height = 6, units ='in', res = 300)
mpg.roc.fec.cer.pel <- ggroc(mpg.roc.fec.cer.pel.list,
                             legacy.axes = TRUE) +
  theme_bw() +
  geom_line(size=1) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("black","red","orange"), labels = c("QM","NTR","STAR")) +
  rremove("x.axis") +
  annotate("text",x = 0.75, y = 0.25, label = "AUC: 0.75 (0.40-1.0)", size = 3) +  
  annotate("text",x = 0.75, y = 0.15, label = "AUC: 0.61 (0.20-1.0)", size = 3) +
  annotate("text",x = 0.75, y = 0.05, label = "AUC: 0.75 (0.33-1.0)", size = 3)
#dev.off()

#Utility of FEC scans in ENDO.
mpg.fec.uni.endo.all.roc <- roc(mpg.fec.uni.endo.all.df,HIST,MEAN)
mpg.fec.ntr.endo.all.roc <- roc(mpg.fec.ntr.endo.all.df,HIST,LN_T_RATIO)
mpg.fec.sta.endo.all.roc <- roc(mpg.fec.sta.endo.all,HIST,STA_RATIO)

mpg.roc.fec.endo.all.list <- list(
  mpg.fec.uni.endo.all.roc,
  mpg.fec.ntr.endo.all.roc,
  mpg.fec.sta.endo.all.roc)
#png("ROC_FEC_ENDO_ALL.png",width = 6, height = 6, units ='in', res = 300)
mpg.roc.fec.endo.all <- ggroc(mpg.roc.fec.endo.all.list,
      legacy.axes = TRUE) +
  theme_bw() +
  geom_line(size=1) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("black","red","orange"), labels = c("QM","NTR","STAR")) +
  annotate("text",x = 0.75, y = 0.3, label = "AUC: 0.76 (0.54-0.98)", size = 3) +
  annotate("text",x = 0.75, y = 0.2, label = "AUC: 0.68 (0.45-0.91)", size = 3) +
  annotate("text",x = 0.75, y = 0.1, label = "AUC: 0.79 (0.61-0.98)", size = 3)
#dev.off()

#Utility of MRI scans in CER
mpg.mri.uni.cer.pel.roc <- roc(mpg.mri.uni.cer.pel.df,HIST,MEAN)
mpg.mri.ntr.cer.pel.roc <- roc(mpg.mri.ntr.cer.pel.df,HIST,LN_T_RATIO)

mpg.roc.mri.cer.pel.list <- list(
  mpg.mri.uni.cer.pel.roc,
  mpg.mri.ntr.cer.pel.roc)
#png("ROC_MRI_CER_PEL.png",width = 6, height = 6, units ='in', res = 300)
mpg.roc.mri.cer.pel <- ggroc(mpg.roc.mri.cer.pel.list,
                             legacy.axes = TRUE) +
  theme_bw() +
  geom_line(size=1) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("black","red","orange"), labels = c("QM","NTR","STAR")) +
  rremove("x.axis") +
  annotate("text",x = 0.75, y = 0.2, label = "AUC: 0.73 (0.55-0.91)", size = 3) +
  annotate("text",x = 0.75, y = 0.1, label = "AUC: 0.79 (0.62-0.96)", size = 3)
  
#dev.off()

#Utility of MRI scans in ENDO
mpg.mri.uni.endo.all.roc <- roc(mpg.mri.uni.endo.all.df,HIST,MEAN)
mpg.mri.ntr.endo.all.roc <- roc(mpg.mri.ntr.endo.all.df,HIST,LN_T_RATIO)

mpg.roc.mri.endo.all.list <- list(
  mpg.mri.uni.endo.all.roc,
  mpg.mri.ntr.endo.all.roc)
#png("ROC_MRI_ENDO_ALL.png",width = 6, height = 6, units ='in', res = 300)
mpg.roc.mri.endo.all <- ggroc(mpg.roc.mri.endo.all.list,
      legacy.axes = TRUE) +
  theme_bw() +
  geom_line(size=1) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("black","red","orange"), labels = c("QM","NTR","STAR")) +
  rremove("x.axis") +
  annotate("text",x = 0.75, y = 0.2, label = "AUC: 0.63 (0.51-0.74)", size = 3) +
  annotate("text",x = 0.75, y = 0.1, label = "AUC: 0.55 (0.41-0.69)", size = 3)
#dev.off()

png("FDG_FEC_MRI__ENDO_ROC.png",width = 3, height = 6, units ='in', res = 300)
ggarrange(mpg.roc.fdg.endo.all,
          mpg.roc.fec.endo.all,
          mpg.roc.mri.endo.all,
          labels = c("A","B","C"),
          align = "hv",
          common.legend = TRUE,
          ncol = 1, nrow = 3)
dev.off()

png("FDG_FEC_MRI_CER_ROC.png",width = 3, height = 6, units ='in', res = 300)
ggarrange(mpg.roc.fdg.cer.pel,
          mpg.roc.fec.cer.pel,
          mpg.roc.mri.cer.pel,
          labels = c("A","B","C"),
          align = "hv",
          common.legend = TRUE,
          ncol = 1, nrow = 3)
dev.off()

#....----
## 5.1 - Logistic binomial regression - UNI ----
# 5.1.1 > - Retrieve DFs and run univariate logistic regression ----

#FDG CER PEL
mpg.fdg.uni.cer.pel.df.log <- as.data.frame(mpg.fdg.uni.cer.pel.df)
colnames(mpg.fdg.uni.cer.pel.df) <- c("HIST","MEAN")
logit.fdg.uni.cer.pel <- glm(HIST~MEAN,data = mpg.fdg.uni.cer.pel.df.log,family="binomial")
summary(logit.fdg.uni.cer.pel)


#FDG ENDO PALN
mpg.fdg.uni.endo.paln.df.log <- as.data.frame(mpg.fdg.uni.endo.paln.df)
colnames(mpg.fdg.uni.endo.paln.df) <- c("HIST","MEAN")
logit.fdg.uni.endo.paln <- glm(HIST~MEAN,data = mpg.fdg.uni.endo.paln.df.log,family="binomial")
summary(logit.fdg.uni.endo.paln)
#FDG ENDO PEL
mpg.fdg.uni.endo.pel.df.log <- as.data.frame(mpg.fdg.uni.endo.pel.df)
colnames(mpg.fdg.uni.endo.pel.df) <- c("HIST","MEAN")
logit.fdg.uni.endo.pel <- glm(HIST~MEAN,data = mpg.fdg.uni.endo.pel.df.log,family="binomial")
summary(logit.fdg.uni.endo.pel)
#FDG ENDO ALL
mpg.fdg.uni.endo.all.df.log <- as.data.frame(mpg.fdg.uni.endo.all.df)
colnames(mpg.fdg.uni.endo.all.df) <- c("HIST","MEAN")
logit.fdg.uni.endo.all <- glm(HIST~MEAN,data = mpg.fdg.uni.endo.all.df.log,family="binomial")
summary(logit.fdg.uni.endo.all)

#fec CER PEL
mpg.fec.uni.cer.pel.df.log <- as.data.frame(mpg.fec.uni.cer.pel.df)
colnames(mpg.fec.uni.cer.pel.df) <- c("HIST","MEAN")
logit.fec.uni.cer.pel <- glm(HIST~MEAN,data = mpg.fec.uni.cer.pel.df.log,family="binomial")
summary(logit.fec.uni.cer.pel)


#fec ENDO PALN
mpg.fec.uni.endo.paln.df.log <- as.data.frame(mpg.fec.uni.endo.paln.df)
colnames(mpg.fec.uni.endo.paln.df) <- c("HIST","MEAN")
logit.fec.uni.endo.paln <- glm(HIST~MEAN,data = mpg.fec.uni.endo.paln.df.log,family="binomial")
summary(logit.fec.uni.endo.paln)
#fec ENDO PEL
mpg.fec.uni.endo.pel.df.log <- as.data.frame(mpg.fec.uni.endo.pel.df)
colnames(mpg.fec.uni.endo.pel.df) <- c("HIST","MEAN")
logit.fec.uni.endo.pel <- glm(HIST~MEAN,data = mpg.fec.uni.endo.pel.df.log,family="binomial")
summary(logit.fec.uni.endo.pel)
#fec ENDO ALL
mpg.fec.uni.endo.all.df.log <- as.data.frame(mpg.fec.uni.endo.all.df)
colnames(mpg.fec.uni.endo.all.df) <- c("HIST","MEAN")
logit.fec.uni.endo.all <- glm(HIST~MEAN,data = mpg.fec.uni.endo.all.df.log,family="binomial")
summary(logit.fec.uni.endo.all)

#mri CER PEL
mpg.mri.uni.cer.pel.df.log <- as.data.frame(mpg.mri.uni.cer.pel.df)
colnames(mpg.mri.uni.cer.pel.df) <- c("HIST","MEAN")
logit.mri.uni.cer.pel <- glm(HIST~MEAN,data = mpg.mri.uni.cer.pel.df.log,family="binomial")
summary(logit.mri.uni.cer.pel)

#mri ENDO PALN
mpg.mri.uni.endo.paln.df.log <- as.data.frame(mpg.mri.uni.endo.paln.df)
colnames(mpg.mri.uni.endo.paln.df) <- c("HIST","MEAN")
logit.mri.uni.endo.paln <- glm(HIST~MEAN,data = mpg.mri.uni.endo.paln.df.log,family="binomial")
summary(logit.mri.uni.endo.paln)
#mri ENDO PEL
mpg.mri.uni.endo.pel.df.log <- as.data.frame(mpg.mri.uni.endo.pel.df)
colnames(mpg.mri.uni.endo.pel.df) <- c("HIST","MEAN")
logit.mri.uni.endo.pel <- glm(HIST~MEAN,data = mpg.mri.uni.endo.pel.df.log,family="binomial")
summary(logit.mri.uni.endo.pel)
#mri ENDO ALL
mpg.mri.uni.endo.all.df.log <- as.data.frame(mpg.mri.uni.endo.all.df)
colnames(mpg.mri.uni.endo.all.df) <- c("HIST","MEAN")
logit.mri.uni.endo.all <- glm(HIST~MEAN,data = mpg.mri.uni.endo.all.df.log,family="binomial")
summary(logit.mri.uni.endo.all)

#Store p-values
logit.uni.results.list <- list(
  logit.fdg.uni.cer.pel,
  logit.fdg.uni.endo.paln,
  logit.fdg.uni.endo.pel,
  logit.fdg.uni.endo.all,
  logit.fec.uni.cer.pel,
  logit.fec.uni.endo.paln,
  logit.fec.uni.endo.pel,
  logit.fec.uni.endo.all,
  logit.mri.uni.cer.pel,
  logit.mri.uni.endo.paln,
  logit.mri.uni.endo.pel,
  logit.mri.uni.endo.all
)

logit.uni.results.df <- data.frame(matrix(NA,nrow=length(logit.uni.results.list),ncol=2))
colnames(logit.uni.results.df) <- c("P-values","")
rownames(logit.uni.results.df) <- c("FDG CER PEL",
                                        "FDG ENDO PALN",
                                        "FDG ENDO PEL",
                                        "FDG ENDO ALL",
                                        "FEC CER PEL",
                                        "FEC ENDO PALN",
                                        "FEC ENDO PEL",
                                        "FEC ENDO ALL",
                                        "MRI CER PEL",
                                        "MRI ENDO PALN",
                                        "MRI ENDO PEL",
                                        "MRI ENDO ALL")



for(i in 1:length(logit.uni.results.list)){
  logit.uni.results.df$`P-values`[i] <- coef(summary(logit.uni.results.list[[i]]))[2,4]
}

View(logit.uni.results.df)

## 5.2 - Logistic binomial regression - MULTI ----
mpg.fdg.multi.cer <- mpg.db
mpg.fdg.multi.cer <- mpg.fdg.multi.cer[-which(mpg.fdg.multi.cer$PATIENT_STATUS=="FDG DYN"),]
mpg.fdg.multi.cer <- mpg.fdg.multi.cer[which(mpg.fdg.multi.cer$CANC_CER==1),]
mpg.fdg.multi.cer <- mpg.fdg.multi.cer[which(mpg.fdg.multi.cer$PATIENT_SCAN_FDG==1),]

mpg.fdg.multi.endo <- mpg.db
mpg.fdg.multi.endo <- mpg.fdg.multi.endo[which(mpg.fdg.multi.endo$CANC_ENDO==1),]
mpg.fdg.multi.endo <- mpg.fdg.multi.endo[which(mpg.fdg.multi.endo$PATIENT_SCAN_FDG==1),]

mpg.fec.multi.cer <- mpg.db
mpg.fec.multi.cer <- mpg.fec.multi.cer[which(mpg.fec.multi.cer$CANC_CER==1),]
mpg.fec.multi.cer <- mpg.fec.multi.cer[which(mpg.fec.multi.cer$PATIENT_SCAN_FEC==1),]

mpg.fec.multi.endo <- mpg.db
mpg.fec.multi.endo <- mpg.fec.multi.endo[which(mpg.fec.multi.endo$CANC_ENDO==1),]
mpg.fec.multi.endo <- mpg.fec.multi.endo[which(mpg.fec.multi.endo$PATIENT_SCAN_FEC==1),]

mpg.mri.multi.cer <- mpg.db
mpg.mri.multi.cer <- mpg.mri.multi.cer[which(mpg.mri.multi.cer$CANC_CER==1),]
mpg.mri.multi.cer <- mpg.mri.multi.cer[which(mpg.mri.multi.cer$PATIENT_SCAN_MRI==1),]

mpg.mri.multi.endo <- mpg.db
mpg.mri.multi.endo <- mpg.mri.multi.endo[which(mpg.mri.multi.endo$CANC_ENDO==1),]
mpg.mri.multi.endo <- mpg.mri.multi.endo[which(mpg.mri.multi.endo$PATIENT_SCAN_MRI==1),]

# 5.2.1 > FDG CER ----
mpg.fdg.multi.cer.all <- mpg.fdg.multi.cer[,c(
  "PATIENT",
  "PATIENT_SCAN_FDG",
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "FDG_PW",
  "DEMO_AGE")]

#Take means of FDG_SUV_LP and RP
mpg.fdg.multi.cer.lp <- mpg.fdg.multi.cer.all
if(sum(is.na(mpg.fdg.multi.cer.all$HIST_LP>0))){
  mpg.fdg.multi.cer.lp <- mpg.fdg.multi.cer.all[-which(is.na(mpg.fdg.multi.cer.all$HIST_LP)),]
}
for(i in 1:nrow(mpg.fdg.multi.cer.lp)){
  mpg.fdg.multi.cer.lp$FDG_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fdg.multi.cer.lp$FDG_SUV_LP_1[i],
    mpg.fdg.multi.cer.lp$FDG_SUV_LP_2[i]), 
    na.rm = TRUE)
}
mpg.fdg.multi.cer.lp$HIST <- mpg.fdg.multi.cer.lp$HIST_LP
mpg.fdg.multi.cer.lp$MRI_SA <- mpg.fdg.multi.cer.lp$MRI_SA_LP
mpg.fdg.multi.cer.lp <- mpg.fdg.multi.cer.lp[,c(
  "HIST",
  "FDG_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fdg.multi.cer.rp <- mpg.fdg.multi.cer.all
if(sum(is.na(mpg.fdg.multi.cer.all$HIST_RP>0))){
  mpg.fdg.multi.cer.rp <- mpg.fdg.multi.cer.all[-which(is.na(mpg.fdg.multi.cer.all$HIST_RP)),]
}
for(i in 1:nrow(mpg.fdg.multi.cer.rp)){
  mpg.fdg.multi.cer.rp$FDG_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fdg.multi.cer.rp$FDG_SUV_RP_1[i],
    mpg.fdg.multi.cer.rp$FDG_SUV_RP_2[i]), 
    na.rm = TRUE)
}
mpg.fdg.multi.cer.rp$HIST <- mpg.fdg.multi.cer.rp$HIST_RP
mpg.fdg.multi.cer.rp$MRI_SA <- mpg.fdg.multi.cer.rp$MRI_SA_RP
mpg.fdg.multi.cer.rp <- mpg.fdg.multi.cer.rp[,c(
  "HIST",
  "FDG_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fdg.multi.cer.pel.df <- rbind(
  mpg.fdg.multi.cer.lp,
  mpg.fdg.multi.cer.rp
)

#Check if MRI_SA is correlated with FDG_SUV_COMB_MEAN
mpg.fdg.multi.cer.pel.expllogit <- summary(glm(FDG_SUV_COMB_MEAN ~ MRI_SA,data=mpg.fdg.multi.cer.pel.df))

#Univariate logistic regression
mpg.fdg.uni.cer.pel.logit <- summary(glm(
  HIST ~ FDG_SUV_COMB_MEAN, data=mpg.fdg.multi.cer.pel.df, family = "binomial"))

#Multivariate logistic regression
mpg.fdg.multi.cer.pel.logit <- summary(glm(
  HIST ~ FDG_SUV_COMB_MEAN + MRI_SA, data=mpg.fdg.multi.cer.pel.df, family = "binomial"))

#Calculated odds ratios
mpg.fdg.uni.cer.pel.or <- exp(mpg.fdg.uni.cer.pel.logit$coefficients[2,1])
mpg.fdg.multi.cer.pel.or <- exp(mpg.fdg.multi.cer.pel.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fdg.uni.cer.pel.or.uci <- exp(mpg.fdg.uni.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fdg.uni.cer.pel.logit$coefficients[2,2]))
mpg.fdg.uni.cer.pel.or.lci <- exp(mpg.fdg.uni.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fdg.uni.cer.pel.logit$coefficients[2,2]))
mpg.fdg.multi.cer.pel.or.uci <- exp(mpg.fdg.multi.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fdg.multi.cer.pel.logit$coefficients[2,2]))
mpg.fdg.multi.cer.pel.or.lci <- exp(mpg.fdg.multi.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fdg.multi.cer.pel.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fdg.cer.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fdg.cer.logit.results) <- c(
  "RAW FDG CER SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fdg.cer.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fdg.cer.logit.results$ORs[2] <- mpg.fdg.uni.cer.pel.or
mpg.fdg.cer.logit.results$ORs[3] <- mpg.fdg.multi.cer.pel.or
mpg.fdg.cer.logit.results$`p-values`[1] <- mpg.fdg.multi.cer.pel.expllogit$coefficients[2,4]
mpg.fdg.cer.logit.results$`p-values`[2] <- mpg.fdg.uni.cer.pel.logit$coefficients[2,4]
mpg.fdg.cer.logit.results$`p-values`[3] <- mpg.fdg.multi.cer.pel.logit$coefficients[2,4]
mpg.fdg.cer.logit.results$UCI[2] <- mpg.fdg.uni.cer.pel.or.uci
mpg.fdg.cer.logit.results$UCI[3] <- mpg.fdg.multi.cer.pel.or.uci
mpg.fdg.cer.logit.results$LCI[2] <- mpg.fdg.uni.cer.pel.or.lci
mpg.fdg.cer.logit.results$LCI[3] <- mpg.fdg.multi.cer.pel.or.lci

# 5.2.2 > FDG ENDO ----
mpg.fdg.multi.endo.all <- mpg.fdg.multi.endo[,c(
  "PATIENT",
  "PATIENT_SCAN_FDG",
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "FDG_PW",
  "DEMO_AGE")]

#Take means of FDG_SUV_LP and RP
mpg.fdg.multi.endo.lp <- mpg.fdg.multi.endo.all
if(sum(is.na(mpg.fdg.multi.endo.all$HIST_LP>0))){
  mpg.fdg.multi.endo.lp <- mpg.fdg.multi.endo.all[-which(is.na(mpg.fdg.multi.endo.all$HIST_LP)),]
}
for(i in 1:nrow(mpg.fdg.multi.endo.lp)){
  mpg.fdg.multi.endo.lp$FDG_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fdg.multi.endo.lp$FDG_SUV_LP_1[i],
    mpg.fdg.multi.endo.lp$FDG_SUV_LP_2[i]), 
    na.rm = TRUE)
}
mpg.fdg.multi.endo.lp$HIST <- mpg.fdg.multi.endo.lp$HIST_LP
mpg.fdg.multi.endo.lp$MRI_SA <- mpg.fdg.multi.endo.lp$MRI_SA_LP
mpg.fdg.multi.endo.lp <- mpg.fdg.multi.endo.lp[,c(
  "HIST",
  "FDG_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fdg.multi.endo.rp <- mpg.fdg.multi.endo.all
if(sum(is.na(mpg.fdg.multi.endo.all$HIST_RP>0))){
  mpg.fdg.multi.endo.rp <- mpg.fdg.multi.endo.all[-which(is.na(mpg.fdg.multi.endo.all$HIST_RP)),]
}
for(i in 1:nrow(mpg.fdg.multi.endo.rp)){
  mpg.fdg.multi.endo.rp$FDG_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fdg.multi.endo.rp$FDG_SUV_RP_1[i],
    mpg.fdg.multi.endo.rp$FDG_SUV_RP_2[i]), 
    na.rm = TRUE)
}
mpg.fdg.multi.endo.rp$HIST <- mpg.fdg.multi.endo.rp$HIST_RP
mpg.fdg.multi.endo.rp$MRI_SA <- mpg.fdg.multi.endo.rp$MRI_SA_RP
mpg.fdg.multi.endo.rp <- mpg.fdg.multi.endo.rp[,c(
  "HIST",
  "FDG_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fdg.multi.endo.paln <- mpg.fdg.multi.endo.all
if(sum(is.na(mpg.fdg.multi.endo.all$HIST_PALN>0))){
  mpg.fdg.multi.endo.paln <- mpg.fdg.multi.endo.all[-which(is.na(mpg.fdg.multi.endo.all$HIST_PALN)),]
}
for(i in 1:nrow(mpg.fdg.multi.endo.paln)){
  mpg.fdg.multi.endo.paln$FDG_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fdg.multi.endo.paln$FDG_SUV_PALN_1[i],
    mpg.fdg.multi.endo.paln$FDG_SUV_PALN_2[i]), 
    na.rm = TRUE)
}
mpg.fdg.multi.endo.paln$HIST <- mpg.fdg.multi.endo.paln$HIST_PALN
mpg.fdg.multi.endo.paln$MRI_SA <- mpg.fdg.multi.endo.paln$MRI_SA_PALN
mpg.fdg.multi.endo.paln <- mpg.fdg.multi.endo.paln[,c(
  "HIST",
  "FDG_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fdg.multi.endo.all.df <- rbind(
  mpg.fdg.multi.endo.lp,
  mpg.fdg.multi.endo.rp,
  mpg.fdg.multi.endo.paln
)

#Check if MRI_SA is correlated with FDG_SUV_COMB_MEAN
mpg.fdg.multi.endo.all.expllogit <- summary(glm(FDG_SUV_COMB_MEAN ~ MRI_SA,data=mpg.fdg.multi.endo.all.df))

#Univariate logistic regression
mpg.fdg.uni.endo.all.logit <- summary(glm(
  HIST ~ FDG_SUV_COMB_MEAN, data=mpg.fdg.multi.endo.all.df, family = "binomial"))

#Multivariate logistic regression
mpg.fdg.multi.endo.all.logit <- summary(glm(
  HIST ~ FDG_SUV_COMB_MEAN + MRI_SA, data=mpg.fdg.multi.endo.all.df, family = "binomial"))

#Calculated odds ratios
mpg.fdg.uni.endo.all.or <- exp(mpg.fdg.uni.endo.all.logit$coefficients[2,1])
mpg.fdg.multi.endo.all.or <- exp(mpg.fdg.multi.endo.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fdg.uni.endo.all.or.uci <- exp(mpg.fdg.uni.endo.all.logit$coefficients[2,1]+(1.96*mpg.fdg.uni.endo.all.logit$coefficients[2,2]))
mpg.fdg.uni.endo.all.or.lci <- exp(mpg.fdg.uni.endo.all.logit$coefficients[2,1]-(1.96*mpg.fdg.uni.endo.all.logit$coefficients[2,2]))
mpg.fdg.multi.endo.all.or.uci <- exp(mpg.fdg.multi.endo.all.logit$coefficients[2,1]+(1.96*mpg.fdg.multi.endo.all.logit$coefficients[2,2]))
mpg.fdg.multi.endo.all.or.lci <- exp(mpg.fdg.multi.endo.all.logit$coefficients[2,1]-(1.96*mpg.fdg.multi.endo.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fdg.endo.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fdg.endo.logit.results) <- c(
  "RAW FDG ENDO SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fdg.endo.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fdg.endo.logit.results$ORs[2] <- mpg.fdg.uni.endo.all.or
mpg.fdg.endo.logit.results$ORs[3] <- mpg.fdg.multi.endo.all.or
mpg.fdg.endo.logit.results$`p-values`[1] <- mpg.fdg.multi.endo.all.expllogit$coefficients[2,4]
mpg.fdg.endo.logit.results$`p-values`[2] <- mpg.fdg.uni.endo.all.logit$coefficients[2,4]
mpg.fdg.endo.logit.results$`p-values`[3] <- mpg.fdg.multi.endo.all.logit$coefficients[2,4]
mpg.fdg.endo.logit.results$UCI[2] <- mpg.fdg.uni.endo.all.or.uci
mpg.fdg.endo.logit.results$UCI[3] <- mpg.fdg.multi.endo.all.or.uci
mpg.fdg.endo.logit.results$LCI[2] <- mpg.fdg.uni.endo.all.or.lci
mpg.fdg.endo.logit.results$LCI[3] <- mpg.fdg.multi.endo.all.or.lci


# 5.2.3 > FEC CER ----
mpg.fec.multi.cer.all <- mpg.fec.multi.cer[,c(
  "PATIENT",
  "PATIENT_SCAN_FEC",
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "FEC_PW",
  "DEMO_AGE")]

#Take means of FEC_SUV_LP and RP
mpg.fec.multi.cer.lp <- mpg.fec.multi.cer.all
if(sum(is.na(mpg.fec.multi.cer.all$HIST_LP>0))){
  mpg.fec.multi.cer.lp <- mpg.fec.multi.cer.all[-which(is.na(mpg.fec.multi.cer.all$HIST_LP)),]
}
for(i in 1:nrow(mpg.fec.multi.cer.lp)){
  mpg.fec.multi.cer.lp$FEC_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fec.multi.cer.lp$FEC_SUV_LP_1[i],
    mpg.fec.multi.cer.lp$FEC_SUV_LP_2[i]), 
    na.rm = TRUE)
}
mpg.fec.multi.cer.lp$HIST <- mpg.fec.multi.cer.lp$HIST_LP
mpg.fec.multi.cer.lp$MRI_SA <- mpg.fec.multi.cer.lp$MRI_SA_LP
mpg.fec.multi.cer.lp <- mpg.fec.multi.cer.lp[,c(
  "HIST",
  "FEC_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fec.multi.cer.rp <- mpg.fec.multi.cer.all
if(sum(is.na(mpg.fec.multi.cer.all$HIST_RP>0))){
  mpg.fec.multi.cer.rp <- mpg.fec.multi.cer.all[-which(is.na(mpg.fec.multi.cer.all$HIST_RP)),]
}
for(i in 1:nrow(mpg.fec.multi.cer.rp)){
  mpg.fec.multi.cer.rp$FEC_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fec.multi.cer.rp$FEC_SUV_RP_1[i],
    mpg.fec.multi.cer.rp$FEC_SUV_RP_2[i]), 
    na.rm = TRUE)
}
mpg.fec.multi.cer.rp$HIST <- mpg.fec.multi.cer.rp$HIST_RP
mpg.fec.multi.cer.rp$MRI_SA <- mpg.fec.multi.cer.rp$MRI_SA_RP
mpg.fec.multi.cer.rp <- mpg.fec.multi.cer.rp[,c(
  "HIST",
  "FEC_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fec.multi.cer.pel.df <- as.data.frame(rbind(
  mpg.fec.multi.cer.lp,
  mpg.fec.multi.cer.rp
))

#Check if MRI_SA is correlated with FEC_SUV_COMB_MEAN
mpg.fec.multi.cer.pel.expllogit <- summary(glm(FEC_SUV_COMB_MEAN ~ MRI_SA,data=mpg.fec.multi.cer.pel.df))

#Univariate logistic regression
mpg.fec.uni.cer.pel.logit <- summary(glm(
  HIST ~ FEC_SUV_COMB_MEAN, data=mpg.fec.multi.cer.pel.df, family = "binomial"))

#Multivariate logistic regression
mpg.fec.multi.cer.pel.logit <- summary(glm(
  HIST ~ FEC_SUV_COMB_MEAN + MRI_SA, data=mpg.fec.multi.cer.pel.df, family = "binomial"))

#Calculated odds ratios
mpg.fec.uni.cer.pel.or <- exp(mpg.fec.uni.cer.pel.logit$coefficients[2,1])
mpg.fec.multi.cer.pel.or <- exp(mpg.fec.multi.cer.pel.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fec.uni.cer.pel.or.uci <- exp(mpg.fec.uni.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fec.uni.cer.pel.logit$coefficients[2,2]))
mpg.fec.uni.cer.pel.or.lci <- exp(mpg.fec.uni.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fec.uni.cer.pel.logit$coefficients[2,2]))
mpg.fec.multi.cer.pel.or.uci <- exp(mpg.fec.multi.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fec.multi.cer.pel.logit$coefficients[2,2]))
mpg.fec.multi.cer.pel.or.lci <- exp(mpg.fec.multi.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fec.multi.cer.pel.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fec.cer.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fec.cer.logit.results) <- c(
  "RAW FEC CER SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fec.cer.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fec.cer.logit.results$ORs[2] <- mpg.fec.uni.cer.pel.or
mpg.fec.cer.logit.results$ORs[3] <- mpg.fec.multi.cer.pel.or
mpg.fec.cer.logit.results$`p-values`[1] <- mpg.fec.multi.cer.pel.expllogit$coefficients[2,4]
mpg.fec.cer.logit.results$`p-values`[2] <- mpg.fec.uni.cer.pel.logit$coefficients[2,4]
mpg.fec.cer.logit.results$`p-values`[3] <- mpg.fec.multi.cer.pel.logit$coefficients[2,4]
mpg.fec.cer.logit.results$UCI[2] <- mpg.fec.uni.cer.pel.or.uci
mpg.fec.cer.logit.results$UCI[3] <- mpg.fec.multi.cer.pel.or.uci
mpg.fec.cer.logit.results$LCI[2] <- mpg.fec.uni.cer.pel.or.lci
mpg.fec.cer.logit.results$LCI[3] <- mpg.fec.multi.cer.pel.or.lci
# 5.2.4 > FEC ENDO ----
mpg.fec.multi.endo.all <- mpg.fec.multi.endo[,c(
  "PATIENT",
  "PATIENT_SCAN_FEC",
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "FEC_PW",
  "DEMO_AGE")]

#Take means of FEC_SUV_LP and RP
mpg.fec.multi.endo.lp <- mpg.fec.multi.endo.all
if(sum(is.na(mpg.fec.multi.endo.all$HIST_LP>0))){
  mpg.fec.multi.endo.lp <- mpg.fec.multi.endo.all[-which(is.na(mpg.fec.multi.endo.all$HIST_LP)),]
}
for(i in 1:nrow(mpg.fec.multi.endo.lp)){
  mpg.fec.multi.endo.lp$FEC_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fec.multi.endo.lp$FEC_SUV_LP_1[i],
    mpg.fec.multi.endo.lp$FEC_SUV_LP_2[i]), 
    na.rm = TRUE)
}
mpg.fec.multi.endo.lp$HIST <- mpg.fec.multi.endo.lp$HIST_LP
mpg.fec.multi.endo.lp$MRI_SA <- mpg.fec.multi.endo.lp$MRI_SA_LP
mpg.fec.multi.endo.lp <- mpg.fec.multi.endo.lp[,c(
  "HIST",
  "FEC_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fec.multi.endo.rp <- mpg.fec.multi.endo.all
if(sum(is.na(mpg.fec.multi.endo.all$HIST_RP>0))){
  mpg.fec.multi.endo.rp <- mpg.fec.multi.endo.all[-which(is.na(mpg.fec.multi.endo.all$HIST_RP)),]
}
for(i in 1:nrow(mpg.fec.multi.endo.rp)){
  mpg.fec.multi.endo.rp$FEC_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fec.multi.endo.rp$FEC_SUV_RP_1[i],
    mpg.fec.multi.endo.rp$FEC_SUV_RP_2[i]), 
    na.rm = TRUE)
}
mpg.fec.multi.endo.rp$HIST <- mpg.fec.multi.endo.rp$HIST_RP
mpg.fec.multi.endo.rp$MRI_SA <- mpg.fec.multi.endo.rp$MRI_SA_RP
mpg.fec.multi.endo.rp <- mpg.fec.multi.endo.rp[,c(
  "HIST",
  "FEC_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fec.multi.endo.paln <- mpg.fec.multi.endo.all
if(sum(is.na(mpg.fec.multi.endo.all$HIST_PALN>0))){
  mpg.fec.multi.endo.paln <- mpg.fec.multi.endo.all[-which(is.na(mpg.fec.multi.endo.all$HIST_PALN)),]
}
for(i in 1:nrow(mpg.fec.multi.endo.paln)){
  mpg.fec.multi.endo.paln$FEC_SUV_COMB_MEAN[i] <- mean(c(
    mpg.fec.multi.endo.paln$FEC_SUV_PALN_1[i],
    mpg.fec.multi.endo.paln$FEC_SUV_PALN_2[i]), 
    na.rm = TRUE)
}
mpg.fec.multi.endo.paln$HIST <- mpg.fec.multi.endo.paln$HIST_PALN
mpg.fec.multi.endo.paln$MRI_SA <- mpg.fec.multi.endo.paln$MRI_SA_PALN
mpg.fec.multi.endo.paln <- mpg.fec.multi.endo.paln[,c(
  "HIST",
  "FEC_SUV_COMB_MEAN",
  "MRI_SA"
)]

mpg.fec.multi.endo.all.df <- rbind(
  mpg.fec.multi.endo.lp,
  mpg.fec.multi.endo.rp,
  mpg.fec.multi.endo.paln
)

#Check if MRI_SA is correlated with FEC_SUV_COMB_MEAN
mpg.fec.multi.endo.all.expllogit <- summary(glm(FEC_SUV_COMB_MEAN ~ MRI_SA,data=mpg.fec.multi.endo.all.df))

#Univariate logistic regression
mpg.fec.uni.endo.all.logit <- summary(glm(
  HIST ~ FEC_SUV_COMB_MEAN, data=mpg.fec.multi.endo.all.df, family = "binomial"))

#Multivariate logistic regression
mpg.fec.multi.endo.all.logit <- summary(glm(
  HIST ~ FEC_SUV_COMB_MEAN + MRI_SA, data=mpg.fec.multi.endo.all.df, family = "binomial"))

#Calculated odds ratios
mpg.fec.uni.endo.all.or <- exp(mpg.fec.uni.endo.all.logit$coefficients[2,1])
mpg.fec.multi.endo.all.or <- exp(mpg.fec.multi.endo.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fec.uni.endo.all.or.uci <- exp(mpg.fec.uni.endo.all.logit$coefficients[2,1]+(1.96*mpg.fec.uni.endo.all.logit$coefficients[2,2]))
mpg.fec.uni.endo.all.or.lci <- exp(mpg.fec.uni.endo.all.logit$coefficients[2,1]-(1.96*mpg.fec.uni.endo.all.logit$coefficients[2,2]))
mpg.fec.multi.endo.all.or.uci <- exp(mpg.fec.multi.endo.all.logit$coefficients[2,1]+(1.96*mpg.fec.multi.endo.all.logit$coefficients[2,2]))
mpg.fec.multi.endo.all.or.lci <- exp(mpg.fec.multi.endo.all.logit$coefficients[2,1]-(1.96*mpg.fec.multi.endo.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fec.endo.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fec.endo.logit.results) <- c(
  "RAW FEC ENDO SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fec.endo.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fec.endo.logit.results$ORs[2] <- mpg.fec.uni.endo.all.or
mpg.fec.endo.logit.results$ORs[3] <- mpg.fec.multi.endo.all.or
mpg.fec.endo.logit.results$`p-values`[1] <- mpg.fec.multi.endo.all.expllogit$coefficients[2,4]
mpg.fec.endo.logit.results$`p-values`[2] <- mpg.fec.uni.endo.all.logit$coefficients[2,4]
mpg.fec.endo.logit.results$`p-values`[3] <- mpg.fec.multi.endo.all.logit$coefficients[2,4]
mpg.fec.endo.logit.results$UCI[2] <- mpg.fec.uni.endo.all.or.uci
mpg.fec.endo.logit.results$UCI[3] <- mpg.fec.multi.endo.all.or.uci
mpg.fec.endo.logit.results$LCI[2] <- mpg.fec.uni.endo.all.or.lci
mpg.fec.endo.logit.results$LCI[3] <- mpg.fec.multi.endo.all.or.lci
# 5.2.5 > MRI CER ----
mpg.mri.multi.cer.all <- mpg.mri.multi.cer[,c(
  "PATIENT",
  "PATIENT_SCAN_FDG",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "FDG_PW",
  "DEMO_AGE")]

#Take means of MRI_ADC_LP and RP
mpg.mri.multi.cer.lp <- mpg.mri.multi.cer.all
if(sum(is.na(mpg.mri.multi.cer.all$HIST_LP>0))){
  mpg.mri.multi.cer.lp <- mpg.mri.multi.cer.all[-which(is.na(mpg.mri.multi.cer.all$HIST_LP)),]
}
for(i in 1:nrow(mpg.mri.multi.cer.lp)){
  mpg.mri.multi.cer.lp$MRI_ADC_COMB_MEAN[i] <- mean(c(
    mpg.mri.multi.cer.lp$MRI_ADC_LP_1[i],
    mpg.mri.multi.cer.lp$MRI_ADC_LP_2[i]), 
    na.rm = TRUE)
}
mpg.mri.multi.cer.lp$HIST <- mpg.mri.multi.cer.lp$HIST_LP
mpg.mri.multi.cer.lp$MRI_SA <- mpg.mri.multi.cer.lp$MRI_SA_LP
mpg.mri.multi.cer.lp <- mpg.mri.multi.cer.lp[,c(
  "HIST",
  "MRI_ADC_COMB_MEAN",
  "MRI_SA"
)]

mpg.mri.multi.cer.rp <- mpg.mri.multi.cer.all
if(sum(is.na(mpg.mri.multi.cer.all$HIST_RP>0))){
  mpg.mri.multi.cer.rp <- mpg.mri.multi.cer.all[-which(is.na(mpg.mri.multi.cer.all$HIST_RP)),]
}
for(i in 1:nrow(mpg.mri.multi.cer.rp)){
  mpg.mri.multi.cer.rp$MRI_ADC_COMB_MEAN[i] <- mean(c(
    mpg.mri.multi.cer.rp$MRI_ADC_RP_1[i],
    mpg.mri.multi.cer.rp$MRI_ADC_RP_2[i]), 
    na.rm = TRUE)
}
mpg.mri.multi.cer.rp$HIST <- mpg.mri.multi.cer.rp$HIST_RP
mpg.mri.multi.cer.rp$MRI_SA <- mpg.mri.multi.cer.rp$MRI_SA_RP
mpg.mri.multi.cer.rp <- mpg.mri.multi.cer.rp[,c(
  "HIST",
  "MRI_ADC_COMB_MEAN",
  "MRI_SA"
)]

mpg.mri.multi.cer.paln <- mpg.mri.multi.cer.all
if(sum(is.na(mpg.mri.multi.cer.all$HIST_PALN>0))){
  mpg.mri.multi.cer.paln <- mpg.mri.multi.cer.all[-which(is.na(mpg.mri.multi.cer.all$HIST_PALN)),]
}
for(i in 1:nrow(mpg.mri.multi.cer.paln)){
  mpg.mri.multi.cer.paln$MRI_ADC_COMB_MEAN[i] <- mean(c(
    mpg.mri.multi.cer.paln$MRI_ADC_PALN_1[i],
    mpg.mri.multi.cer.paln$MRI_ADC_PALN_2[i]), 
    na.rm = TRUE)
}
mpg.mri.multi.cer.paln$HIST <- mpg.mri.multi.cer.paln$HIST_PALN
mpg.mri.multi.cer.paln$MRI_SA <- mpg.mri.multi.cer.paln$MRI_SA_PALN
mpg.mri.multi.cer.paln <- mpg.mri.multi.cer.paln[,c(
  "HIST",
  "MRI_ADC_COMB_MEAN",
  "MRI_SA"
)]

mpg.mri.multi.cer.all.df <- rbind(
  mpg.mri.multi.cer.lp,
  mpg.mri.multi.cer.rp,
  mpg.mri.multi.cer.paln
)

#Check if MRI_SA is correlated with MRI_ADC_COMB_MEAN
mpg.mri.multi.cer.all.expllogit <- summary(glm(MRI_ADC_COMB_MEAN ~ MRI_SA,data=mpg.mri.multi.cer.all.df))

#Univariate logistic regression
mpg.mri.uni.cer.all.logit <- summary(glm(
  HIST ~ MRI_ADC_COMB_MEAN, data=mpg.mri.multi.cer.all.df, family = "binomial"))

#Multivariate logistic regression
mpg.mri.multi.cer.all.logit <- summary(glm(
  HIST ~ MRI_ADC_COMB_MEAN + MRI_SA, data=mpg.mri.multi.cer.all.df, family = "binomial"))

#Calculated odds ratios
mpg.mri.uni.cer.all.or <- exp(mpg.mri.uni.cer.all.logit$coefficients[2,1])
mpg.mri.multi.cer.all.or <- exp(mpg.mri.multi.cer.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.mri.uni.cer.all.or.uci <- exp(mpg.mri.uni.cer.all.logit$coefficients[2,1]+(1.96*mpg.mri.uni.cer.all.logit$coefficients[2,2]))
mpg.mri.uni.cer.all.or.lci <- exp(mpg.mri.uni.cer.all.logit$coefficients[2,1]-(1.96*mpg.mri.uni.cer.all.logit$coefficients[2,2]))
mpg.mri.multi.cer.all.or.uci <- exp(mpg.mri.multi.cer.all.logit$coefficients[2,1]+(1.96*mpg.mri.multi.cer.all.logit$coefficients[2,2]))
mpg.mri.multi.cer.all.or.lci <- exp(mpg.mri.multi.cer.all.logit$coefficients[2,1]-(1.96*mpg.mri.multi.cer.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.mri.cer.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.mri.cer.logit.results) <- c(
  "RAW MRI SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.mri.cer.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.mri.cer.logit.results$ORs[2] <- mpg.mri.uni.cer.all.or
mpg.mri.cer.logit.results$ORs[3] <- mpg.mri.multi.cer.all.or
mpg.mri.cer.logit.results$`p-values`[1] <- mpg.mri.multi.cer.all.expllogit$coefficients[2,4]
mpg.mri.cer.logit.results$`p-values`[2] <- mpg.mri.uni.cer.all.logit$coefficients[2,4]
mpg.mri.cer.logit.results$`p-values`[3] <- mpg.mri.multi.cer.all.logit$coefficients[2,4]
mpg.mri.cer.logit.results$UCI[2] <- mpg.mri.uni.cer.all.or.uci
mpg.mri.cer.logit.results$UCI[3] <- mpg.mri.multi.cer.all.or.uci
mpg.mri.cer.logit.results$LCI[2] <- mpg.mri.uni.cer.all.or.lci
mpg.mri.cer.logit.results$LCI[3] <- mpg.mri.multi.cer.all.or.lci

# 5.2.6 > MRI ENDO ----
mpg.mri.multi.endo.all <- mpg.mri.multi.endo[,c(
  "PATIENT",
  "PATIENT_SCAN_FDG",
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN",
  "MRI_SA_LP",
  "MRI_SA_RP",
  "MRI_SA_PALN",
  "FDG_PW",
  "DEMO_AGE")]

#Take means of MRI_ADC_LP and RP
mpg.mri.multi.endo.lp <- mpg.mri.multi.endo.all
if(sum(is.na(mpg.mri.multi.endo.all$HIST_LP>0))){
  mpg.mri.multi.endo.lp <- mpg.mri.multi.endo.all[-which(is.na(mpg.mri.multi.endo.all$HIST_LP)),]
}
for(i in 1:nrow(mpg.mri.multi.endo.lp)){
  mpg.mri.multi.endo.lp$MRI_ADC_COMB_MEAN[i] <- mean(c(
    mpg.mri.multi.endo.lp$MRI_ADC_LP_1[i],
    mpg.mri.multi.endo.lp$MRI_ADC_LP_2[i]), 
    na.rm = TRUE)
}
mpg.mri.multi.endo.lp$HIST <- mpg.mri.multi.endo.lp$HIST_LP
mpg.mri.multi.endo.lp$MRI_SA <- mpg.mri.multi.endo.lp$MRI_SA_LP
mpg.mri.multi.endo.lp <- mpg.mri.multi.endo.lp[,c(
  "HIST",
  "MRI_ADC_COMB_MEAN",
  "MRI_SA"
)]

mpg.mri.multi.endo.rp <- mpg.mri.multi.endo.all
if(sum(is.na(mpg.mri.multi.endo.all$HIST_RP>0))){
  mpg.mri.multi.endo.rp <- mpg.mri.multi.endo.all[-which(is.na(mpg.mri.multi.endo.all$HIST_RP)),]
}
for(i in 1:nrow(mpg.mri.multi.endo.rp)){
  mpg.mri.multi.endo.rp$MRI_ADC_COMB_MEAN[i] <- mean(c(
    mpg.mri.multi.endo.rp$MRI_ADC_RP_1[i],
    mpg.mri.multi.endo.rp$MRI_ADC_RP_2[i]), 
    na.rm = TRUE)
}
mpg.mri.multi.endo.rp$HIST <- mpg.mri.multi.endo.rp$HIST_RP
mpg.mri.multi.endo.rp$MRI_SA <- mpg.mri.multi.endo.rp$MRI_SA_RP
mpg.mri.multi.endo.rp <- mpg.mri.multi.endo.rp[,c(
  "HIST",
  "MRI_ADC_COMB_MEAN",
  "MRI_SA"
)]

mpg.mri.multi.endo.paln <- mpg.mri.multi.endo.all
if(sum(is.na(mpg.mri.multi.endo.all$HIST_PALN>0))){
  mpg.mri.multi.endo.paln <- mpg.mri.multi.endo.all[-which(is.na(mpg.mri.multi.endo.all$HIST_PALN)),]
}
for(i in 1:nrow(mpg.mri.multi.endo.paln)){
  mpg.mri.multi.endo.paln$MRI_ADC_COMB_MEAN[i] <- mean(c(
    mpg.mri.multi.endo.paln$MRI_ADC_PALN_1[i],
    mpg.mri.multi.endo.paln$MRI_ADC_PALN_2[i]), 
    na.rm = TRUE)
}
mpg.mri.multi.endo.paln$HIST <- mpg.mri.multi.endo.paln$HIST_PALN
mpg.mri.multi.endo.paln$MRI_SA <- mpg.mri.multi.endo.paln$MRI_SA_PALN
mpg.mri.multi.endo.paln <- mpg.mri.multi.endo.paln[,c(
  "HIST",
  "MRI_ADC_COMB_MEAN",
  "MRI_SA"
)]

mpg.mri.multi.endo.all.df <- rbind(
  mpg.mri.multi.endo.lp,
  mpg.mri.multi.endo.rp,
  mpg.mri.multi.endo.paln
)

#Check if MRI_SA is correlated with MRI_ADC_COMB_MEAN
mpg.mri.multi.endo.all.expllogit <- summary(glm(MRI_ADC_COMB_MEAN ~ MRI_SA,data=mpg.mri.multi.endo.all.df))

#Univariate logistic regression
mpg.mri.uni.endo.all.logit <- summary(glm(
  HIST ~ MRI_ADC_COMB_MEAN, data=mpg.mri.multi.endo.all.df, family = "binomial"))

#Multivariate logistic regression
mpg.mri.multi.endo.all.logit <- summary(glm(
  HIST ~ MRI_ADC_COMB_MEAN + MRI_SA, data=mpg.mri.multi.endo.all.df, family = "binomial"))

#Calculated odds ratios
mpg.mri.uni.endo.all.or <- exp(mpg.mri.uni.endo.all.logit$coefficients[2,1])
mpg.mri.multi.endo.all.or <- exp(mpg.mri.multi.endo.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.mri.uni.endo.all.or.uci <- exp(mpg.mri.uni.endo.all.logit$coefficients[2,1]+(1.96*mpg.mri.uni.endo.all.logit$coefficients[2,2]))
mpg.mri.uni.endo.all.or.lci <- exp(mpg.mri.uni.endo.all.logit$coefficients[2,1]-(1.96*mpg.mri.uni.endo.all.logit$coefficients[2,2]))
mpg.mri.multi.endo.all.or.uci <- exp(mpg.mri.multi.endo.all.logit$coefficients[2,1]+(1.96*mpg.mri.multi.endo.all.logit$coefficients[2,2]))
mpg.mri.multi.endo.all.or.lci <- exp(mpg.mri.multi.endo.all.logit$coefficients[2,1]-(1.96*mpg.mri.multi.endo.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.mri.endo.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.mri.endo.logit.results) <- c(
  "RAW MRI SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.mri.endo.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.mri.endo.logit.results$ORs[2] <- mpg.mri.uni.endo.all.or
mpg.mri.endo.logit.results$ORs[3] <- mpg.mri.multi.endo.all.or
mpg.mri.endo.logit.results$`p-values`[1] <- mpg.mri.multi.endo.all.expllogit$coefficients[2,4]
mpg.mri.endo.logit.results$`p-values`[2] <- mpg.mri.uni.endo.all.logit$coefficients[2,4]
mpg.mri.endo.logit.results$`p-values`[3] <- mpg.mri.multi.endo.all.logit$coefficients[2,4]
mpg.mri.endo.logit.results$UCI[2] <- mpg.mri.uni.endo.all.or.uci
mpg.mri.endo.logit.results$UCI[3] <- mpg.mri.multi.endo.all.or.uci
mpg.mri.endo.logit.results$LCI[2] <- mpg.mri.uni.endo.all.or.lci
mpg.mri.endo.logit.results$LCI[3] <- mpg.mri.multi.endo.all.or.lci

## 5.3 - NTR logit ----
# 5.3.1 > FDG CER ----
#Check if MRI_SA is correlated with FDG_SUV_COMB_MEAN
mpg.fdg.ntr.multi.cer.pel.expllogit <- summary(glm(LN_T_RATIO ~ MRI_SA,data=mpg.fdg.ntr.cer.pel.df))

#Univariate logistic regression
mpg.fdg.ntr.uni.cer.pel.logit <- summary(glm(
  HIST ~ LN_T_RATIO, data=mpg.fdg.ntr.cer.pel.df, family = "binomial"))

#Multivariate logistic regression
mpg.fdg.ntr.multi.cer.pel.logit <- summary(glm(
  HIST ~ LN_T_RATIO + MRI_SA, data=mpg.fdg.ntr.cer.pel.df, family = "binomial"))

#Calculated odds ratios
mpg.fdg.ntr.uni.cer.pel.or <- exp(mpg.fdg.ntr.uni.cer.pel.logit$coefficients[2,1])
mpg.fdg.ntr.multi.cer.pel.or <- exp(mpg.fdg.ntr.multi.cer.pel.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fdg.ntr.uni.cer.pel.or.uci <- exp(mpg.fdg.ntr.uni.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fdg.uni.cer.pel.logit$coefficients[2,2]))
mpg.fdg.ntr.uni.cer.pel.or.lci <- exp(mpg.fdg.ntr.uni.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fdg.uni.cer.pel.logit$coefficients[2,2]))
mpg.fdg.ntr.multi.cer.pel.or.uci <- exp(mpg.fdg.ntr.multi.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fdg.multi.cer.pel.logit$coefficients[2,2]))
mpg.fdg.ntr.multi.cer.pel.or.lci <- exp(mpg.fdg.ntr.multi.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fdg.multi.cer.pel.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fdg.ntr.cer.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fdg.ntr.cer.logit.results) <- c(
  "NTR FDG CER SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fdg.ntr.cer.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fdg.ntr.cer.logit.results$ORs[2] <- mpg.fdg.ntr.uni.cer.pel.or
mpg.fdg.ntr.cer.logit.results$ORs[3] <- mpg.fdg.ntr.multi.cer.pel.or
mpg.fdg.ntr.cer.logit.results$`p-values`[1] <- mpg.fdg.ntr.multi.cer.pel.expllogit$coefficients[2,4]
mpg.fdg.ntr.cer.logit.results$`p-values`[2] <- mpg.fdg.ntr.uni.cer.pel.logit$coefficients[2,4]
mpg.fdg.ntr.cer.logit.results$`p-values`[3] <- mpg.fdg.ntr.multi.cer.pel.logit$coefficients[2,4]
mpg.fdg.ntr.cer.logit.results$UCI[2] <- mpg.fdg.ntr.uni.cer.pel.or.uci
mpg.fdg.ntr.cer.logit.results$UCI[3] <- mpg.fdg.ntr.multi.cer.pel.or.uci
mpg.fdg.ntr.cer.logit.results$LCI[2] <- mpg.fdg.ntr.uni.cer.pel.or.lci
mpg.fdg.ntr.cer.logit.results$LCI[3] <- mpg.fdg.ntr.multi.cer.pel.or.lci
# 5.3.2 > FDG ENDO ----
#Check if MRI_SA is correlated with FDG_SUV_COMB_MEAN
mpg.fdg.ntr.multi.endo.all.expllogit <- summary(glm(LN_T_RATIO ~ MRI_SA,data=mpg.fdg.ntr.endo.all.df))

#Univariate logistic regression
mpg.fdg.ntr.uni.endo.all.logit <- summary(glm(
  HIST ~ LN_T_RATIO, data=mpg.fdg.ntr.endo.all.df, family = "binomial"))

#Multivariate logistic regression
mpg.fdg.ntr.multi.endo.all.logit <- summary(glm(
  HIST ~ LN_T_RATIO + MRI_SA, data=mpg.fdg.ntr.endo.all.df, family = "binomial"))

#Calculated odds ratios
mpg.fdg.ntr.uni.endo.all.or <- exp(mpg.fdg.ntr.uni.endo.all.logit$coefficients[2,1])
mpg.fdg.ntr.multi.endo.all.or <- exp(mpg.fdg.ntr.multi.endo.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fdg.ntr.uni.endo.all.or.uci <- exp(mpg.fdg.ntr.uni.endo.all.logit$coefficients[2,1]+(1.96*mpg.fdg.uni.endo.all.logit$coefficients[2,2]))
mpg.fdg.ntr.uni.endo.all.or.lci <- exp(mpg.fdg.ntr.uni.endo.all.logit$coefficients[2,1]-(1.96*mpg.fdg.uni.endo.all.logit$coefficients[2,2]))
mpg.fdg.ntr.multi.endo.all.or.uci <- exp(mpg.fdg.ntr.multi.endo.all.logit$coefficients[2,1]+(1.96*mpg.fdg.multi.endo.all.logit$coefficients[2,2]))
mpg.fdg.ntr.multi.endo.all.or.lci <- exp(mpg.fdg.ntr.multi.endo.all.logit$coefficients[2,1]-(1.96*mpg.fdg.multi.endo.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fdg.ntr.endo.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fdg.ntr.endo.logit.results) <- c(
  "NTR FDG ENDO SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fdg.ntr.endo.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fdg.ntr.endo.logit.results$ORs[2] <- mpg.fdg.ntr.uni.endo.all.or
mpg.fdg.ntr.endo.logit.results$ORs[3] <- mpg.fdg.ntr.multi.endo.all.or
mpg.fdg.ntr.endo.logit.results$`p-values`[1] <- mpg.fdg.ntr.multi.endo.all.expllogit$coefficients[2,4]
mpg.fdg.ntr.endo.logit.results$`p-values`[2] <- mpg.fdg.ntr.uni.endo.all.logit$coefficients[2,4]
mpg.fdg.ntr.endo.logit.results$`p-values`[3] <- mpg.fdg.ntr.multi.endo.all.logit$coefficients[2,4]
mpg.fdg.ntr.endo.logit.results$UCI[2] <- mpg.fdg.ntr.uni.endo.all.or.uci
mpg.fdg.ntr.endo.logit.results$UCI[3] <- mpg.fdg.ntr.multi.endo.all.or.uci
mpg.fdg.ntr.endo.logit.results$LCI[2] <- mpg.fdg.ntr.uni.endo.all.or.lci
mpg.fdg.ntr.endo.logit.results$LCI[3] <- mpg.fdg.ntr.multi.endo.all.or.lci
# 5.3.3 > FEC CER ----
#Check if MRI_SA is correlated with FEC_SUV_COMB_MEAN
mpg.fec.ntr.multi.cer.pel.expllogit <- summary(glm(LN_T_RATIO ~ MRI_SA,data=mpg.fec.ntr.cer.pel.df))

#Univariate logistic regression
mpg.fec.ntr.uni.cer.pel.logit <- summary(glm(
  HIST ~ LN_T_RATIO, data=mpg.fec.ntr.cer.pel.df, family = "binomial"))

#Multivariate logistic regression
mpg.fec.ntr.multi.cer.pel.logit <- summary(glm(
  HIST ~ LN_T_RATIO + MRI_SA, data=mpg.fec.ntr.cer.pel.df, family = "binomial"))

#Calculated odds ratios
mpg.fec.ntr.uni.cer.pel.or <- exp(mpg.fec.ntr.uni.cer.pel.logit$coefficients[2,1])
mpg.fec.ntr.multi.cer.pel.or <- exp(mpg.fec.ntr.multi.cer.pel.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fec.ntr.uni.cer.pel.or.uci <- exp(mpg.fec.ntr.uni.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fec.uni.cer.pel.logit$coefficients[2,2]))
mpg.fec.ntr.uni.cer.pel.or.lci <- exp(mpg.fec.ntr.uni.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fec.uni.cer.pel.logit$coefficients[2,2]))
mpg.fec.ntr.multi.cer.pel.or.uci <- exp(mpg.fec.ntr.multi.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fec.multi.cer.pel.logit$coefficients[2,2]))
mpg.fec.ntr.multi.cer.pel.or.lci <- exp(mpg.fec.ntr.multi.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fec.multi.cer.pel.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fec.ntr.cer.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fec.ntr.cer.logit.results) <- c(
  "FEC CER SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fec.ntr.cer.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fec.ntr.cer.logit.results$ORs[2] <- mpg.fec.ntr.uni.cer.pel.or
mpg.fec.ntr.cer.logit.results$ORs[3] <- mpg.fec.ntr.multi.cer.pel.or
mpg.fec.ntr.cer.logit.results$`p-values`[1] <- mpg.fec.ntr.multi.cer.pel.expllogit$coefficients[2,4]
mpg.fec.ntr.cer.logit.results$`p-values`[2] <- mpg.fec.ntr.uni.cer.pel.logit$coefficients[2,4]
mpg.fec.ntr.cer.logit.results$`p-values`[3] <- mpg.fec.ntr.multi.cer.pel.logit$coefficients[2,4]
mpg.fec.ntr.cer.logit.results$UCI[2] <- mpg.fec.ntr.uni.cer.pel.or.uci
mpg.fec.ntr.cer.logit.results$UCI[3] <- mpg.fec.ntr.multi.cer.pel.or.uci
mpg.fec.ntr.cer.logit.results$LCI[2] <- mpg.fec.ntr.uni.cer.pel.or.lci
mpg.fec.ntr.cer.logit.results$LCI[3] <- mpg.fec.ntr.multi.cer.pel.or.lci
# 5.3.4 > FEC ENDO ----
#Check if MRI_SA is correlated with FEC_SUV_COMB_MEAN
mpg.fec.ntr.multi.endo.all.expllogit <- summary(glm(LN_T_RATIO ~ MRI_SA,data=mpg.fec.ntr.endo.all.df))

#Univariate logistic regression
mpg.fec.ntr.uni.endo.all.logit <- summary(glm(
  HIST ~ LN_T_RATIO, data=mpg.fec.ntr.endo.all.df, family = "binomial"))

#Multivariate logistic regression
mpg.fec.ntr.multi.endo.all.logit <- summary(glm(
  HIST ~ LN_T_RATIO + MRI_SA, data=mpg.fec.ntr.endo.all.df, family = "binomial"))

#Calculated odds ratios
mpg.fec.ntr.uni.endo.all.or <- exp(mpg.fec.ntr.uni.endo.all.logit$coefficients[2,1])
mpg.fec.ntr.multi.endo.all.or <- exp(mpg.fec.ntr.multi.endo.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fec.ntr.uni.endo.all.or.uci <- exp(mpg.fec.ntr.uni.endo.all.logit$coefficients[2,1]+(1.96*mpg.fec.uni.endo.all.logit$coefficients[2,2]))
mpg.fec.ntr.uni.endo.all.or.lci <- exp(mpg.fec.ntr.uni.endo.all.logit$coefficients[2,1]-(1.96*mpg.fec.uni.endo.all.logit$coefficients[2,2]))
mpg.fec.ntr.multi.endo.all.or.uci <- exp(mpg.fec.ntr.multi.endo.all.logit$coefficients[2,1]+(1.96*mpg.fec.multi.endo.all.logit$coefficients[2,2]))
mpg.fec.ntr.multi.endo.all.or.lci <- exp(mpg.fec.ntr.multi.endo.all.logit$coefficients[2,1]-(1.96*mpg.fec.multi.endo.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fec.ntr.endo.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fec.ntr.endo.logit.results) <- c(
  "FEC ENDO SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fec.ntr.endo.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fec.ntr.endo.logit.results$ORs[2] <- mpg.fec.ntr.uni.endo.all.or
mpg.fec.ntr.endo.logit.results$ORs[3] <- mpg.fec.ntr.multi.endo.all.or
mpg.fec.ntr.endo.logit.results$`p-values`[1] <- mpg.fec.ntr.multi.endo.all.expllogit$coefficients[2,4]
mpg.fec.ntr.endo.logit.results$`p-values`[2] <- mpg.fec.ntr.uni.endo.all.logit$coefficients[2,4]
mpg.fec.ntr.endo.logit.results$`p-values`[3] <- mpg.fec.ntr.multi.endo.all.logit$coefficients[2,4]
mpg.fec.ntr.endo.logit.results$UCI[2] <- mpg.fec.ntr.uni.endo.all.or.uci
mpg.fec.ntr.endo.logit.results$UCI[3] <- mpg.fec.ntr.multi.endo.all.or.uci
mpg.fec.ntr.endo.logit.results$LCI[2] <- mpg.fec.ntr.uni.endo.all.or.lci
mpg.fec.ntr.endo.logit.results$LCI[3] <- mpg.fec.ntr.multi.endo.all.or.lci
# 5.4.5 > MRI CER ----
#Check if MRI_SA is correlated with FDG_SUV_COMB_MEAN
mpg.mri.ntr.multi.cer.pel.expllogit <- summary(glm(LN_T_RATIO ~ MRI_SA,data=mpg.mri.ntr.cer.pel.df))

#Univariate logistic regression
mpg.mri.ntr.uni.cer.pel.logit <- summary(glm(
  HIST ~ LN_T_RATIO, data=mpg.mri.ntr.cer.pel.df, family = "binomial"))

#Multivariate logistic regression
mpg.mri.ntr.multi.cer.pel.logit <- summary(glm(
  HIST ~ LN_T_RATIO + MRI_SA, data=mpg.mri.ntr.cer.pel.df, family = "binomial"))

#Calculated odds ratios
mpg.mri.ntr.uni.cer.pel.or <- exp(mpg.mri.ntr.uni.cer.pel.logit$coefficients[2,1])
mpg.mri.ntr.multi.cer.pel.or <- exp(mpg.mri.ntr.multi.cer.pel.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.mri.ntr.uni.cer.pel.or.uci <- exp(mpg.mri.ntr.uni.cer.pel.logit$coefficients[2,1]+(1.96*mpg.mri.ntr.uni.cer.pel.logit$coefficients[2,2]))
mpg.mri.ntr.uni.cer.pel.or.lci <- exp(mpg.mri.ntr.uni.cer.pel.logit$coefficients[2,1]-(1.96*mpg.mri.ntr.uni.cer.pel.logit$coefficients[2,2]))
mpg.mri.ntr.multi.cer.pel.or.uci <- exp(mpg.mri.ntr.multi.cer.pel.logit$coefficients[2,1]+(1.96*mpg.mri.ntr.multi.cer.pel.logit$coefficients[2,2]))
mpg.mri.ntr.multi.cer.pel.or.lci <- exp(mpg.mri.ntr.multi.cer.pel.logit$coefficients[2,1]-(1.96*mpg.mri.ntr.multi.cer.pel.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.mri.ntr.cer.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.mri.ntr.cer.logit.results) <- c(
  "NTR MRI CER SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.mri.ntr.cer.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.mri.ntr.cer.logit.results$ORs[2] <- mpg.mri.ntr.uni.cer.pel.or
mpg.mri.ntr.cer.logit.results$ORs[3] <- mpg.mri.ntr.multi.cer.pel.or
mpg.mri.ntr.cer.logit.results$`p-values`[1] <- mpg.mri.ntr.multi.cer.pel.expllogit$coefficients[2,4]
mpg.mri.ntr.cer.logit.results$`p-values`[2] <- mpg.mri.ntr.uni.cer.pel.logit$coefficients[2,4]
mpg.mri.ntr.cer.logit.results$`p-values`[3] <- mpg.mri.ntr.multi.cer.pel.logit$coefficients[2,4]
mpg.mri.ntr.cer.logit.results$UCI[2] <- mpg.mri.ntr.uni.cer.pel.or.uci
mpg.mri.ntr.cer.logit.results$UCI[3] <- mpg.mri.ntr.multi.cer.pel.or.uci
mpg.mri.ntr.cer.logit.results$LCI[2] <- mpg.mri.ntr.uni.cer.pel.or.lci
mpg.mri.ntr.cer.logit.results$LCI[3] <- mpg.mri.ntr.multi.cer.pel.or.lci
# 5.4.6 > MRI ENDO ----
#Check if MRI_SA is correlated with FDG_SUV_COMB_MEAN
mpg.mri.ntr.multi.endo.all.expllogit <- summary(glm(LN_T_RATIO ~ MRI_SA,data=mpg.mri.ntr.endo.all.df))

#Univariate logistic regression
mpg.mri.ntr.uni.endo.all.logit <- summary(glm(
  HIST ~ LN_T_RATIO, data=mpg.mri.ntr.endo.all.df, family = "binomial"))

#Multivariate logistic regression
mpg.mri.ntr.multi.endo.all.logit <- summary(glm(
  HIST ~ LN_T_RATIO + MRI_SA, data=mpg.mri.ntr.endo.all.df, family = "binomial"))

#Calculated odds ratios
mpg.mri.ntr.uni.endo.all.or <- exp(mpg.mri.ntr.uni.endo.all.logit$coefficients[2,1])
mpg.mri.ntr.multi.endo.all.or <- exp(mpg.mri.ntr.multi.endo.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.mri.ntr.uni.endo.all.or.uci <- exp(mpg.mri.ntr.uni.endo.all.logit$coefficients[2,1]+(1.96*mpg.mri.uni.endo.all.logit$coefficients[2,2]))
mpg.mri.ntr.uni.endo.all.or.lci <- exp(mpg.mri.ntr.uni.endo.all.logit$coefficients[2,1]-(1.96*mpg.mri.uni.endo.all.logit$coefficients[2,2]))
mpg.mri.ntr.multi.endo.all.or.uci <- exp(mpg.mri.ntr.multi.endo.all.logit$coefficients[2,1]+(1.96*mpg.mri.multi.endo.all.logit$coefficients[2,2]))
mpg.mri.ntr.multi.endo.all.or.lci <- exp(mpg.mri.ntr.multi.endo.all.logit$coefficients[2,1]-(1.96*mpg.mri.multi.endo.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.mri.ntr.endo.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.mri.ntr.endo.logit.results) <- c(
  "NTR MRI ENDO SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.mri.ntr.endo.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.mri.ntr.endo.logit.results$ORs[2] <- mpg.mri.ntr.uni.endo.all.or
mpg.mri.ntr.endo.logit.results$ORs[3] <- mpg.mri.ntr.multi.endo.all.or
mpg.mri.ntr.endo.logit.results$`p-values`[1] <- mpg.mri.ntr.multi.endo.all.expllogit$coefficients[2,4]
mpg.mri.ntr.endo.logit.results$`p-values`[2] <- mpg.mri.ntr.uni.endo.all.logit$coefficients[2,4]
mpg.mri.ntr.endo.logit.results$`p-values`[3] <- mpg.mri.ntr.multi.endo.all.logit$coefficients[2,4]
mpg.mri.ntr.endo.logit.results$UCI[2] <- mpg.mri.ntr.uni.endo.all.or.uci
mpg.mri.ntr.endo.logit.results$UCI[3] <- mpg.mri.ntr.multi.endo.all.or.uci
mpg.mri.ntr.endo.logit.results$LCI[2] <- mpg.mri.ntr.uni.endo.all.or.lci
mpg.mri.ntr.endo.logit.results$LCI[3] <- mpg.mri.ntr.multi.endo.all.or.lci
## 5.4 - STAR logit ----
# 5.4.1 > FDG CER ----
#Check if MRI_SA is correlated with FDG_SUV_COMB_MEAN
mpg.fdg.sta.multi.cer.pel.expllogit <- summary(glm(STA_RATIO ~ MRI_SA,data=mpg.fdg.sta.cer.pel))

#Univariate logistic regression
mpg.fdg.sta.uni.cer.pel.logit <- summary(glm(
  HIST ~ STA_RATIO, data=mpg.fdg.sta.cer.pel, family = "binomial"))

#Multivariate logistic regression
mpg.fdg.sta.multi.cer.pel.logit <- summary(glm(
  HIST ~ STA_RATIO + MRI_SA, data=mpg.fdg.sta.cer.pel, family = "binomial"))

#Calculated odds ratios
mpg.fdg.sta.uni.cer.pel.or <- exp(mpg.fdg.sta.uni.cer.pel.logit$coefficients[2,1])
mpg.fdg.sta.multi.cer.pel.or <- exp(mpg.fdg.sta.multi.cer.pel.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fdg.sta.uni.cer.pel.or.uci <- exp(mpg.fdg.sta.uni.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fdg.uni.cer.pel.logit$coefficients[2,2]))
mpg.fdg.sta.uni.cer.pel.or.lci <- exp(mpg.fdg.sta.uni.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fdg.uni.cer.pel.logit$coefficients[2,2]))
mpg.fdg.sta.multi.cer.pel.or.uci <- exp(mpg.fdg.sta.multi.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fdg.multi.cer.pel.logit$coefficients[2,2]))
mpg.fdg.sta.multi.cer.pel.or.lci <- exp(mpg.fdg.sta.multi.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fdg.multi.cer.pel.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fdg.sta.cer.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fdg.sta.cer.logit.results) <- c(
  "STA FDG SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fdg.sta.cer.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fdg.sta.cer.logit.results$ORs[2] <- mpg.fdg.sta.uni.cer.pel.or
mpg.fdg.sta.cer.logit.results$ORs[3] <- mpg.fdg.sta.multi.cer.pel.or
mpg.fdg.sta.cer.logit.results$`p-values`[1] <- mpg.fdg.sta.multi.cer.pel.expllogit$coefficients[2,4]
mpg.fdg.sta.cer.logit.results$`p-values`[2] <- mpg.fdg.sta.uni.cer.pel.logit$coefficients[2,4]
mpg.fdg.sta.cer.logit.results$`p-values`[3] <- mpg.fdg.sta.multi.cer.pel.logit$coefficients[2,4]
mpg.fdg.sta.cer.logit.results$UCI[2] <- mpg.fdg.sta.uni.cer.pel.or.uci
mpg.fdg.sta.cer.logit.results$UCI[3] <- mpg.fdg.sta.multi.cer.pel.or.uci
mpg.fdg.sta.cer.logit.results$LCI[2] <- mpg.fdg.sta.uni.cer.pel.or.lci
mpg.fdg.sta.cer.logit.results$LCI[3] <- mpg.fdg.sta.multi.cer.pel.or.lci
# 5.4.2 > FDG ENDO ----
#Check if MRI_SA is correlated with FDG_SUV_COMB_MEAN
mpg.fdg.sta.multi.endo.all.expllogit <- summary(glm(STA_RATIO ~ MRI_SA,data=mpg.fdg.sta.endo.all))

#Univariate logistic regression
mpg.fdg.sta.uni.endo.all.logit <- summary(glm(
  HIST ~ STA_RATIO, data=mpg.fdg.sta.endo.all, family = "binomial"))

#Multivariate logistic regression
mpg.fdg.sta.multi.endo.all.logit <- summary(glm(
  HIST ~ STA_RATIO + MRI_SA, data=mpg.fdg.sta.endo.all, family = "binomial"))

#Calculated odds ratios
mpg.fdg.sta.uni.endo.all.or <- exp(mpg.fdg.sta.uni.endo.all.logit$coefficients[2,1])
mpg.fdg.sta.multi.endo.all.or <- exp(mpg.fdg.sta.multi.endo.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fdg.sta.uni.endo.all.or.uci <- exp(mpg.fdg.sta.uni.endo.all.logit$coefficients[2,1]+(1.96*mpg.fdg.uni.endo.all.logit$coefficients[2,2]))
mpg.fdg.sta.uni.endo.all.or.lci <- exp(mpg.fdg.sta.uni.endo.all.logit$coefficients[2,1]-(1.96*mpg.fdg.uni.endo.all.logit$coefficients[2,2]))
mpg.fdg.sta.multi.endo.all.or.uci <- exp(mpg.fdg.sta.multi.endo.all.logit$coefficients[2,1]+(1.96*mpg.fdg.multi.endo.all.logit$coefficients[2,2]))
mpg.fdg.sta.multi.endo.all.or.lci <- exp(mpg.fdg.sta.multi.endo.all.logit$coefficients[2,1]-(1.96*mpg.fdg.multi.endo.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fdg.sta.endo.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fdg.sta.endo.logit.results) <- c(
  "STA FDG ENDO SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fdg.sta.endo.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fdg.sta.endo.logit.results$ORs[2] <- mpg.fdg.sta.uni.endo.all.or
mpg.fdg.sta.endo.logit.results$ORs[3] <- mpg.fdg.sta.multi.endo.all.or
mpg.fdg.sta.endo.logit.results$`p-values`[1] <- mpg.fdg.sta.multi.endo.all.expllogit$coefficients[2,4]
mpg.fdg.sta.endo.logit.results$`p-values`[2] <- mpg.fdg.sta.uni.endo.all.logit$coefficients[2,4]
mpg.fdg.sta.endo.logit.results$`p-values`[3] <- mpg.fdg.sta.multi.endo.all.logit$coefficients[2,4]
mpg.fdg.sta.endo.logit.results$UCI[2] <- mpg.fdg.sta.uni.endo.all.or.uci
mpg.fdg.sta.endo.logit.results$UCI[3] <- mpg.fdg.sta.multi.endo.all.or.uci
mpg.fdg.sta.endo.logit.results$LCI[2] <- mpg.fdg.sta.uni.endo.all.or.lci
mpg.fdg.sta.endo.logit.results$LCI[3] <- mpg.fdg.sta.multi.endo.all.or.lci

# 5.4.3 > FEC CER ----
#Check if MRI_SA is correlated with fec_SUV_COMB_MEAN
mpg.fec.sta.multi.cer.pel.expllogit <- summary(glm(STA_RATIO ~ MRI_SA,data=mpg.fec.sta.cer.pel))

#Univariate logistic regression
mpg.fec.sta.uni.cer.pel.logit <- summary(glm(
  HIST ~ STA_RATIO, data=mpg.fec.sta.cer.pel, family = "binomial"))

#Multivariate logistic regression
mpg.fec.sta.multi.cer.pel.logit <- summary(glm(
  HIST ~ STA_RATIO + MRI_SA, data=mpg.fec.sta.cer.pel, family = "binomial"))

#Calculated odds ratios
mpg.fec.sta.uni.cer.pel.or <- exp(mpg.fec.sta.uni.cer.pel.logit$coefficients[2,1])
mpg.fec.sta.multi.cer.pel.or <- exp(mpg.fec.sta.multi.cer.pel.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fec.sta.uni.cer.pel.or.uci <- exp(mpg.fec.sta.uni.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fec.uni.cer.pel.logit$coefficients[2,2]))
mpg.fec.sta.uni.cer.pel.or.lci <- exp(mpg.fec.sta.uni.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fec.uni.cer.pel.logit$coefficients[2,2]))
mpg.fec.sta.multi.cer.pel.or.uci <- exp(mpg.fec.sta.multi.cer.pel.logit$coefficients[2,1]+(1.96*mpg.fec.multi.cer.pel.logit$coefficients[2,2]))
mpg.fec.sta.multi.cer.pel.or.lci <- exp(mpg.fec.sta.multi.cer.pel.logit$coefficients[2,1]-(1.96*mpg.fec.multi.cer.pel.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fec.sta.cer.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fec.sta.cer.logit.results) <- c(
  "STA FEC CER SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fec.sta.cer.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fec.sta.cer.logit.results$ORs[2] <- mpg.fec.sta.uni.cer.pel.or
mpg.fec.sta.cer.logit.results$ORs[3] <- mpg.fec.sta.multi.cer.pel.or
mpg.fec.sta.cer.logit.results$`p-values`[1] <- mpg.fec.sta.multi.cer.pel.expllogit$coefficients[2,4]
mpg.fec.sta.cer.logit.results$`p-values`[2] <- mpg.fec.sta.uni.cer.pel.logit$coefficients[2,4]
mpg.fec.sta.cer.logit.results$`p-values`[3] <- mpg.fec.sta.multi.cer.pel.logit$coefficients[2,4]
mpg.fec.sta.cer.logit.results$UCI[2] <- mpg.fec.sta.uni.cer.pel.or.uci
mpg.fec.sta.cer.logit.results$UCI[3] <- mpg.fec.sta.multi.cer.pel.or.uci
mpg.fec.sta.cer.logit.results$LCI[2] <- mpg.fec.sta.uni.cer.pel.or.lci
mpg.fec.sta.cer.logit.results$LCI[3] <- mpg.fec.sta.multi.cer.pel.or.lci

# 5.4.4 > FEC ENDO ----
#Check if MRI_SA is correlated with fec_SUV_COMB_MEAN
mpg.fec.sta.multi.endo.all.expllogit <- summary(glm(STA_RATIO ~ MRI_SA,data=mpg.fec.sta.endo.all))

#Univariate logistic regression
mpg.fec.sta.uni.endo.all.logit <- summary(glm(
  HIST ~ STA_RATIO, data=mpg.fec.sta.endo.all, family = "binomial"))

#Multivariate logistic regression
mpg.fec.sta.multi.endo.all.logit <- summary(glm(
  HIST ~ STA_RATIO + MRI_SA, data=mpg.fec.sta.endo.all, family = "binomial"))

#Calculated odds ratios
mpg.fec.sta.uni.endo.all.or <- exp(mpg.fec.sta.uni.endo.all.logit$coefficients[2,1])
mpg.fec.sta.multi.endo.all.or <- exp(mpg.fec.sta.multi.endo.all.logit$coefficients[2,1])

#Calculated CIs of ORs
mpg.fec.sta.uni.endo.all.or.uci <- exp(mpg.fec.sta.uni.endo.all.logit$coefficients[2,1]+(1.96*mpg.fec.uni.endo.all.logit$coefficients[2,2]))
mpg.fec.sta.uni.endo.all.or.lci <- exp(mpg.fec.sta.uni.endo.all.logit$coefficients[2,1]-(1.96*mpg.fec.uni.endo.all.logit$coefficients[2,2]))
mpg.fec.sta.multi.endo.all.or.uci <- exp(mpg.fec.sta.multi.endo.all.logit$coefficients[2,1]+(1.96*mpg.fec.multi.endo.all.logit$coefficients[2,2]))
mpg.fec.sta.multi.endo.all.or.lci <- exp(mpg.fec.sta.multi.endo.all.logit$coefficients[2,1]-(1.96*mpg.fec.multi.endo.all.logit$coefficients[2,2]))

#Cute lil df to hold these results
mpg.fec.sta.endo.logit.results <- data.frame(matrix(NA, nrow = 3, ncol = 4))
rownames(mpg.fec.sta.endo.logit.results) <- c(
  "STA FEC ENDO SUV ~ SA",
  "HIST ~ SUV",
  "HIST ~ SUV + SA"
)
colnames(mpg.fec.sta.endo.logit.results) <- c(
  "ORs",
  "p-values",
  "UCI",
  "LCI"
)
mpg.fec.sta.endo.logit.results$ORs[2] <- mpg.fec.sta.uni.endo.all.or
mpg.fec.sta.endo.logit.results$ORs[3] <- mpg.fec.sta.multi.endo.all.or
mpg.fec.sta.endo.logit.results$`p-values`[1] <- mpg.fec.sta.multi.endo.all.expllogit$coefficients[2,4]
mpg.fec.sta.endo.logit.results$`p-values`[2] <- mpg.fec.sta.uni.endo.all.logit$coefficients[2,4]
mpg.fec.sta.endo.logit.results$`p-values`[3] <- mpg.fec.sta.multi.endo.all.logit$coefficients[2,4]
mpg.fec.sta.endo.logit.results$UCI[2] <- mpg.fec.sta.uni.endo.all.or.uci
mpg.fec.sta.endo.logit.results$UCI[3] <- mpg.fec.sta.multi.endo.all.or.uci
mpg.fec.sta.endo.logit.results$LCI[2] <- mpg.fec.sta.uni.endo.all.or.lci
mpg.fec.sta.endo.logit.results$LCI[3] <- mpg.fec.sta.multi.endo.all.or.lci
# 5.5 - Bind and write to CSV ----
logit.multi.binder <- rbind(
  mpg.fdg.cer.logit.results,
  mpg.fec.cer.logit.results,
  mpg.mri.cer.logit.results,
  mpg.fdg.ntr.cer.logit.results,
  mpg.fec.ntr.cer.logit.results,
  mpg.mri.ntr.cer.logit.results,
  mpg.fdg.sta.cer.logit.results,
  mpg.fec.sta.cer.logit.results,
  mpg.fdg.endo.logit.results,
  mpg.fec.endo.logit.results,
  mpg.mri.endo.logit.results,
  mpg.fdg.ntr.endo.logit.results,
  mpg.fec.ntr.endo.logit.results,
  mpg.mri.ntr.endo.logit.results,
  mpg.fdg.sta.endo.logit.results,
  mpg.fec.sta.endo.logit.results
)

write.csv(logit.multi.binder, "multi_logit_results.csv")
#....----
## 6.1 - Intraclass correlation ----
library(irr)

#FDG
mpg.icc.fdg.lp <- mpg.fdg.uni[,c("FDG_SUV_LP_1","FDG_SUV_LP_2")]
mpg.icc.fdg.lp <- mpg.icc.fdg.lp[-which(is.na(
  mpg.icc.fdg.lp$FDG_SUV_LP_1
  & mpg.icc.fdg.lp$FDG_SUV_LP_2)),]
colnames(mpg.icc.fdg.lp) <- c("R1","R2")

mpg.icc.fdg.rp <- mpg.fdg.uni[,c("FDG_SUV_RP_1","FDG_SUV_RP_2")]
mpg.icc.fdg.rp <- mpg.icc.fdg.rp[-which(is.na(
  mpg.icc.fdg.rp$FDG_SUV_RP_1
  & mpg.icc.fdg.rp$FDG_SUV_RP_2)),]
colnames(mpg.icc.fdg.rp) <- c("R1","R2")

mpg.icc.fdg.paln <- mpg.fdg.uni[,c("FDG_SUV_PALN_1","FDG_SUV_PALN_2")]
mpg.icc.fdg.paln <- mpg.icc.fdg.paln[-which(is.na(
  mpg.icc.fdg.paln$FDG_SUV_PALN_1
  & mpg.icc.fdg.paln$FDG_SUV_PALN_2)),]
colnames(mpg.icc.fdg.paln) <- c("R1","R2")

mpg.icc.fdg.df <- rbind(
  mpg.icc.fdg.lp,
  mpg.icc.fdg.rp,
  mpg.icc.fdg.paln
)


mpg.icc.fdg <- icc(mpg.icc.fdg.df, model = "twoway", type = "agreement", r0 = 0, conf.level = 0.95)

#FEC
mpg.icc.fec.lp <- mpg.fec.uni[,c("FEC_SUV_LP_1","FEC_SUV_LP_2")]
mpg.icc.fec.lp <- mpg.icc.fec.lp[-which(is.na(
  mpg.icc.fec.lp$FEC_SUV_LP_1
  & mpg.icc.fec.lp$FEC_SUV_LP_2)),]
colnames(mpg.icc.fec.lp) <- c("R1","R2")

mpg.icc.fec.rp <- mpg.fec.uni[,c("FEC_SUV_RP_1","FEC_SUV_RP_2")]
mpg.icc.fec.rp <- mpg.icc.fec.rp[-which(is.na(
  mpg.icc.fec.rp$FEC_SUV_RP_1
  & mpg.icc.fec.rp$FEC_SUV_RP_2)),]
colnames(mpg.icc.fec.rp) <- c("R1","R2")

mpg.icc.fec.paln <- mpg.fec.uni[,c("FEC_SUV_PALN_1","FEC_SUV_PALN_2")]
mpg.icc.fec.paln <- mpg.icc.fec.paln[-which(is.na(
  mpg.icc.fec.paln$FEC_SUV_PALN_1
  & mpg.icc.fec.paln$FEC_SUV_PALN_2)),]
colnames(mpg.icc.fec.paln) <- c("R1","R2")


mpg.icc.fec.df <- rbind(
  mpg.icc.fec.lp,
  mpg.icc.fec.rp,
  mpg.icc.fec.paln
)

mpg.icc.fec <- icc(mpg.icc.fec.df, model = "twoway", type = "agreement", r0 = 0, conf.level = 0.95)

#MRI
mpg.icc.mri.lp <- mpg.mri.uni[,c("MRI_ADC_LP_1","MRI_ADC_LP_2")]
mpg.icc.mri.lp <- mpg.icc.mri.lp[-which(is.na(
  mpg.icc.mri.lp$MRI_ADC_LP_1
  & mpg.icc.mri.lp$MRI_ADC_LP_2)),]
colnames(mpg.icc.mri.lp) <- c("R1","R2")

mpg.icc.mri.rp <- mpg.mri.uni[,c("MRI_ADC_RP_1","MRI_ADC_RP_2")]
mpg.icc.mri.rp <- mpg.icc.mri.rp[-which(is.na(
  mpg.icc.mri.rp$MRI_ADC_RP_1
  & mpg.icc.mri.rp$MRI_ADC_RP_2)),]
colnames(mpg.icc.mri.rp) <- c("R1","R2")

mpg.icc.mri.paln <- mpg.mri.uni[,c("MRI_ADC_PALN_1","MRI_ADC_PALN_2")]
mpg.icc.mri.paln <- mpg.icc.mri.paln[-which(is.na(
  mpg.icc.mri.paln$MRI_ADC_PALN_1
  & mpg.icc.mri.paln$MRI_ADC_PALN_2)),]
colnames(mpg.icc.mri.paln) <- c("R1","R2")

mpg.icc.mri.pt <- mpg.mri.uni[,c("MRI_ADC_PT_1","MRI_ADC_PT_2")]
mpg.icc.mri.pt <- mpg.icc.mri.pt[-which(is.na(
  mpg.icc.mri.pt$MRI_ADC_PT_1
  & mpg.icc.mri.pt$MRI_ADC_PT_2)),]
colnames(mpg.icc.mri.pt) <- c("R1","R2")

mpg.icc.mri.df <- rbind(
  mpg.icc.mri.lp,
  mpg.icc.mri.rp,
  mpg.icc.mri.paln
)

mpg.icc.mri <- icc(mpg.icc.mri.df, model = "twoway", type = "agreement", r0 = 0, conf.level = 0.95)

mpg.icc.mri.pt <- icc(mpg.icc.mri.pt, model = "twoway", type = "agreement", r0 = 0, conf.level = 0.95)

mpg.icc.values <- data.frame(matrix(NA,nrow=4,ncol=1))
rownames(mpg.icc.values) <- c("FDG","FEC","MRI","PT")
colnames(mpg.icc.values) <- c("ICC")
mpg.icc.values$ICC[1] <- mpg.icc.fdg$value
mpg.icc.values$ICC[2] <- mpg.icc.fec$value
mpg.icc.values$ICC[3] <- mpg.icc.mri$value
mpg.icc.values$ICC[4] <- mpg.icc.mri.pt$value



## 6.2 - Rate of measurement ----
#For FDG, calculate percentages of patients with +ve LNs when single vs double read.
icc.expl.fdg.df <- mpg.fdg.uni[,c(
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN"
)]

icc.expl.fdg.lp <- icc.expl.fdg.df[,c(
  "FDG_SUV_LP_1",
  "FDG_SUV_LP_2",
  "HIST_LP"
)]

icc.expl.fdg.rp <- icc.expl.fdg.df[,c(
  "FDG_SUV_RP_1",
  "FDG_SUV_RP_2",
  "HIST_RP"
)]

icc.expl.fdg.paln <- icc.expl.fdg.df[,c(
  "FDG_SUV_PALN_1",
  "FDG_SUV_PALN_2",
  "HIST_PALN"
)]

colnames(icc.expl.fdg.lp) <- c("1","2","HIST")
colnames(icc.expl.fdg.rp) <- c("1","2","HIST")
colnames(icc.expl.fdg.paln) <- c("1","2","HIST")

icc.expl.fdg.final <- rbind(
  icc.expl.fdg.lp,
  icc.expl.fdg.rp,
  icc.expl.fdg.paln
)

icc.expl.fdg.final <- icc.expl.fdg.final[-which(is.na(icc.expl.fdg.final$HIST)),]
icc.expl.fdg.final <- icc.expl.fdg.final[-which(is.na(icc.expl.fdg.final$'1'|icc.expl.fdg.final$'2')),]
- 
icc.expl.fdg.final.1 <- icc.expl.fdg.final[which(icc.expl.fdg.final$HIST==1),]
icc.expl.fdg.final.1.r1 <- nrow(icc.expl.fdg.final.1[which(is.na(icc.expl.fdg.final.1$'1'&icc.expl.fdg.final.1$'2')),])
icc.expl.fdg.final.1.r2 <- nrow(icc.expl.fdg.final.1[-which(is.na(icc.expl.fdg.final.1$'1'&icc.expl.fdg.final.1$'2')),])
icc.expl.fdg.final.1.perc <- (icc.expl.fdg.final.1.r2/(icc.expl.fdg.final.1.r1+icc.expl.fdg.final.1.r2))*100

icc.expl.fdg.final.0 <- icc.expl.fdg.final[which(icc.expl.fdg.final$HIST==0),]
icc.expl.fdg.final.0.r1 <- nrow(icc.expl.fdg.final.0[which(is.na(icc.expl.fdg.final.0$'1'&icc.expl.fdg.final.0$'2')),])
icc.expl.fdg.final.0.r2 <- nrow(icc.expl.fdg.final.0[-which(is.na(icc.expl.fdg.final.0$'1'&icc.expl.fdg.final.0$'2')),])
icc.expl.fdg.final.0.perc <- (icc.expl.fdg.final.0.r2/(icc.expl.fdg.final.0.r1+icc.expl.fdg.final.0.r2))*100


#FEC
icc.expl.fec.df <- mpg.fec.uni[,c(
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN"
)]

icc.expl.fec.lp <- icc.expl.fec.df[,c(
  "FEC_SUV_LP_1",
  "FEC_SUV_LP_2",
  "HIST_LP"
)]

icc.expl.fec.rp <- icc.expl.fec.df[,c(
  "FEC_SUV_RP_1",
  "FEC_SUV_RP_2",
  "HIST_RP"
)]

icc.expl.fec.paln <- icc.expl.fec.df[,c(
  "FEC_SUV_PALN_1",
  "FEC_SUV_PALN_2",
  "HIST_PALN"
)]

colnames(icc.expl.fec.lp) <- c("1","2","HIST")
colnames(icc.expl.fec.rp) <- c("1","2","HIST")
colnames(icc.expl.fec.paln) <- c("1","2","HIST")

icc.expl.fec.final <- rbind(
  icc.expl.fec.lp,
  icc.expl.fec.rp,
  icc.expl.fec.paln
)

icc.expl.fec.final <- icc.expl.fec.final[-which(is.na(icc.expl.fec.final$HIST)),]
icc.expl.fec.final <- icc.expl.fec.final[-which(is.na(icc.expl.fec.final$'1'|icc.expl.fec.final$'2')),]

icc.expl.fec.final.1 <- icc.expl.fec.final[which(icc.expl.fec.final$HIST==1),]
icc.expl.fec.final.1.r1 <- nrow(icc.expl.fec.final.1[which(is.na(icc.expl.fec.final.1$'1'&icc.expl.fec.final.1$'2')),])
icc.expl.fec.final.1.r2 <- nrow(icc.expl.fec.final.1[-which(is.na(icc.expl.fec.final.1$'1'&icc.expl.fec.final.1$'2')),])
icc.expl.fec.final.1.perc <- (icc.expl.fec.final.1.r2/(icc.expl.fec.final.1.r1+icc.expl.fec.final.1.r2))*100

icc.expl.fec.final.0 <- icc.expl.fec.final[which(icc.expl.fec.final$HIST==0),]
icc.expl.fec.final.0.r1 <- nrow(icc.expl.fec.final.0[which(is.na(icc.expl.fec.final.0$'1'&icc.expl.fec.final.0$'2')),])
icc.expl.fec.final.0.r2 <- nrow(icc.expl.fec.final.0[-which(is.na(icc.expl.fec.final.0$'1'&icc.expl.fec.final.0$'2')),])
icc.expl.fec.final.0.perc <- (icc.expl.fec.final.0.r2/(icc.expl.fec.final.0.r1+icc.expl.fec.final.0.r2))*100


#MRI
icc.expl.mri.df <- mpg.mri.uni[,c(
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "HIST_LP",
  "HIST_RP",
  "HIST_PALN"
)]

icc.expl.mri.lp <- icc.expl.mri.df[,c(
  "MRI_ADC_LP_1",
  "MRI_ADC_LP_2",
  "HIST_LP"
)]

icc.expl.mri.rp <- icc.expl.mri.df[,c(
  "MRI_ADC_RP_1",
  "MRI_ADC_RP_2",
  "HIST_RP"
)]

icc.expl.mri.paln <- icc.expl.mri.df[,c(
  "MRI_ADC_PALN_1",
  "MRI_ADC_PALN_2",
  "HIST_PALN"
)]

colnames(icc.expl.mri.lp) <- c("1","2","HIST")
colnames(icc.expl.mri.rp) <- c("1","2","HIST")
colnames(icc.expl.mri.paln) <- c("1","2","HIST")

icc.expl.mri.final <- rbind(
  icc.expl.mri.lp,
  icc.expl.mri.rp,
  icc.expl.mri.paln
)

icc.expl.mri.final <- icc.expl.mri.final[-which(is.na(icc.expl.mri.final$HIST)),]
icc.expl.mri.final <- icc.expl.mri.final[-which(is.na(icc.expl.mri.final$'1'|icc.expl.mri.final$'2')),]

icc.expl.mri.final.1 <- icc.expl.mri.final[which(icc.expl.mri.final$HIST==1),]
icc.expl.mri.final.1.r1 <- nrow(icc.expl.mri.final.1[which(is.na(icc.expl.mri.final.1$'1'&icc.expl.mri.final.1$'2')),])
icc.expl.mri.final.1.r2 <- nrow(icc.expl.mri.final.1[-which(is.na(icc.expl.mri.final.1$'1'&icc.expl.mri.final.1$'2')),])
icc.expl.mri.final.1.perc <- (icc.expl.mri.final.1.r2/(icc.expl.mri.final.1.r1+icc.expl.mri.final.1.r2))*100

icc.expl.mri.final.0 <- icc.expl.mri.final[which(icc.expl.mri.final$HIST==0),]
icc.expl.mri.final.0.r1 <- nrow(icc.expl.mri.final.0[which(is.na(icc.expl.mri.final.0$'1'&icc.expl.mri.final.0$'2')),])
icc.expl.mri.final.0.r2 <- nrow(icc.expl.mri.final.0[-which(is.na(icc.expl.mri.final.0$'1'&icc.expl.mri.final.0$'2')),])
icc.expl.mri.final.0.perc <- (icc.expl.mri.final.0.r2/(icc.expl.mri.final.0.r1+icc.expl.mri.final.0.r2))*100
