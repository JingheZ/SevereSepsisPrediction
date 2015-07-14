#Compare patients in terms of overlaps and mortality under different definitions of severe sepsis

#Read lab, bld culture, and vital signs data
lab <- read.csv("pts_lab_new.csv", header=TRUE)
bld<- read.csv("pts_bldcultures3_new.csv", header=TRUE)
charts <- read.csv('pts_vitals_new.csv')

#get the icuseq info for bld culture
icuseqs <- read.csv('icuseqs_blds.csv',header = F)
#get the hospseq info for vital signs
hospseqs <- read.csv('hospseqs_vitals.csv', header = F)

bld$icuseq <- as.vector(unlist(icuseqs), mode = 'numeric')
charts$hospseq <- as.vector(unlist(hospseqs), mode = 'numeric')

bld <- bld[bld$icuseq > 0, ]
bld$id <- paste(bld$subject_id, bld$hospital_seq, bld$icuseq, sep='#%#')
lab$id <- paste(lab$subject_id, lab$hospital_seq, lab$icustay_seq, sep='#%#')
charts$id <- paste(charts$subject_id, charts$hospseq, charts$icustay_seq, sep='#%#')


bld1 <- data.frame(bld$id, bld$charttime, bld$spec_itemid, bld$hospital_seq)
bld1$itemid <- rep(NA, nrow(bld1))
bld1$valuenum <- rep(NA, nrow(bld1))
names(bld1) <- c('id', 'charttime', 'spec_itemid', 'hospital_seq', 'itemid', 'valuenum')

lab1 <- data.frame(lab$id, lab$charttime, lab$hospital_seq, lab$itemid, lab$valuenum)
lab1$spec_itemid <- rep(NA, nrow(lab1))
names(lab1) <- c('id', 'charttime', 'hospital_seq', 'itemid', 'valuenum', 'spec_itemid')


#combine the lab and bld culture data
complete <- rbind(lab1, bld1) 

library(lubridate)
complete$charttime <- ymd_hms(complete$charttime)
complete <- complete[order(complete$charttime),]

## Add indicator columns to data frame
complete$Blood_Culture <- c(rep(0,nrow(complete)))
complete$High_Lactate <- c(rep(0,nrow(complete)))
complete$Severe_Sepsis <- c(rep(0,nrow(complete)))

#find patients with high lactate value
lactates <- complete[complete$itemid == 50010,]       # lactate lab values
length(table(lactates$id))          # total patients had lactate tested  #24,358
highlactates <- lactates[lactates$valuenum >= 4,] 

hl <- highlactates$id[!duplicated(highlactates$id) & !is.na(highlactates$id)]            # Identify individuals with a high lactate
#2740 patients

bldcultures <- complete[!is.na(complete$spec_itemid),] 
bc <- bldcultures$id[!duplicated(bldcultures$id) & !is.na(bldcultures$id)]     # Identify patients with a blood culture taken (12,845)

## From 'complete' data set, select only patients who have high lactate and blood culture
hl.bc <- intersect(hl, bc)  #2,024
# hl.bc <- as.list(hl.bc)

possible.sepsis <- complete[(complete$id %in% hl.bc),] 

reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]
# Identify patients in 'possible.sepsis' data set who have missing chart times for blood culture
bc.missing <- possible.sepsis[possible.sepsis$HALFHOUR=="" & !is.na(possible.sepsis$SPEC_ITEMID),]         # Try this command if the one right below does not work
bc.missing <- possible.sepsis[is.na(possible.sepsis$charttime) & !is.na(possible.sepsis$spec_itemid),]      
bc.missing.patients <- bc.missing[!duplicated(bc.missing$id),1] #417 pts

## Create reduced subset of 'complete' that excludes patients without chart time for blood culture
reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]

# Count how many patients in 'reduced.sepsis' : 
length(reduced.sepsis$id[!duplicated(reduced.sepsis$id)]) #1,607
reduced.sepsis1 <- reduced.sepsis[reduced.sepsis$itemid ==50010 | !is.na(reduced.sepsis$spec_itemid),]
### Function for above code 
z.0 <- function(complete){
  #complete <- data[data$Sid==patient,]
  ## Identify rows with blood culture taken
  for (i in 1:nrow(complete))
    if (!is.na(complete$spec_itemid[i]))
      complete$Blood_Culture[i] <- 1 
    ## Identify rows with high lactate
    for (i in 1:nrow(complete))
      if(!is.na(complete$itemid[i]) & !is.na(complete$valuenum[i]) & complete$itemid[i] == 50010 & complete$valuenum[i] >=4)
        complete$High_Lactate[i] <- 1
      ## Identify rows with severe sepsis
      # Empty list for chart times for blood culture
      time.bc <- c()
      # Get chart times for blood culture
      for (i in 1:nrow(complete)) 
        if (complete$Blood_Culture[i] == 1)
          time.bc <- c(complete$charttime[i], time.bc)
      time.bc <- rev(time.bc)
      # Assign severe sepsis to rows with high lactate that occur within 24 hours of a blood culture
      for (i in 1:nrow(complete))
        if (complete$High_Lactate[i] == 1 & min(abs(difftime(time.bc, complete$charttime[i], units="hours"))) < 24)
          complete$Severe_Sepsis[i] <- 1
      return(complete)
}


library(plyr)
## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
data1 <- ddply(reduced.sepsis1, .(id), z.0) 
## Identify patients from 'data1' who have severe sepsis
severe.sepsis.observations <- data1[data1$Severe_Sepsis==1,]
severe.sepsis.pt <- severe.sepsis.observations[!duplicated(severe.sepsis.observations$id),1]

## Count number of patients classified with severe.sepsis: 1,143
length(severe.sepsis.pt)

## Create new data set with patients who have severe sepsis
severe.sepsis <- data1[data1$id %in% severe.sepsis.pt,]

bc.missing.sepsis.1 <- possible.sepsis[(possible.sepsis$id %in% bc.missing.patients),]
# Subset of blood culture with time (excludes blood culture without time)
bc.missing.sepsis.2 <- bc.missing.sepsis.1[!(is.na(bc.missing.sepsis.1$spec_itemid)) & !(is.na(bc.missing.sepsis.1$charttime)),]

## Identify patients who have at least one blood culture with a chart time: 395
bc.patients.0 <- bc.missing.sepsis.2[!duplicated(bc.missing.sepsis.2$id),1]

## Subset of data for patients who have at least one blood culture with a chart time
bc.missing.sepsis.3 <- bc.missing.sepsis.1[bc.missing.sepsis.1$id %in% bc.patients.0,]
# Choose only values with chart time
bc.missing.sepsis.4 <- bc.missing.sepsis.3[!is.na(bc.missing.sepsis.3$charttime),]

## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
bc.missing.sepsis.5 <- bc.missing.sepsis.4[!is.na(bc.missing.sepsis.4$spec_itemid) | bc.missing.sepsis.4$itemid==50010,]
data.2 <- ddply(bc.missing.sepsis.5, .(id), z.0) 

## Identify first instance of high lactate for each patient (from 'data.2' - patients previously excluded)
hl.subset.0 <- data.2[data.2$High_Lactate==1,]                         # Returns all instances of high lactate
hl.subset.1 <- hl.subset.0[!duplicated(hl.subset.0$id),]

## Identify patients whose first high lactate is also a severe sepsis event
hl.subset.2 <- hl.subset.1[hl.subset.1$Severe_Sepsis==1,]
additional.ss.index <- hl.subset.2[!duplicated(hl.subset.2$id), 1] #230 patients

## If a patient's first high lactate was also a severe sepsis event, add that patients data to the Severe Sepsis group
additional.ss <- data.2[(data.2$id %in% additional.ss.index),]
severe.sepsis.all <- rbind(severe.sepsis, additional.ss)                  # Combine with 'severe.sepsis' which is the first dataset that originally excluded the second group
length(severe.sepsis.all$id[!duplicated(severe.sepsis.all$id)]) #1,353 patients
severe.sepsis.id <- severe.sepsis.all$id[!duplicated(severe.sepsis.all$id)]


#===========================decrease the lactate values===========================
highlactates <- lactates[lactates$valuenum >= 2.5,] 

hl <- highlactates$id[!duplicated(highlactates$id) & !is.na(highlactates$id)]            # Identify individuals with a high lactate
#2740 patients

bldcultures <- complete[!is.na(complete$spec_itemid),] 
bc <- bldcultures$id[!duplicated(bldcultures$id) & !is.na(bldcultures$id)]     # Identify patients with a blood culture taken (12,845)

## From 'complete' data set, select only patients who have high lactate and blood culture
hl.bc <- intersect(hl, bc)  #3,767; 3,801
# hl.bc <- as.list(hl.bc)

# 
possible.sepsis <- complete[(complete$id %in% hl.bc),] 
reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]
# Identify patients in 'possible.sepsis' data set who have missing chart times for blood culture
# Try this command if the one right below does not work
bc.missing <- possible.sepsis[is.na(possible.sepsis$charttime) & !is.na(possible.sepsis$spec_itemid),]      
bc.missing.patients <- bc.missing[!duplicated(bc.missing$id),1] #741 pts

## Create reduced subset of 'complete' that excludes patients without chart time for blood culture
reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]

# Count how many patients in 'reduced.sepsis' : 
length(reduced.sepsis$id[!duplicated(reduced.sepsis$id)]) #3,026; 3,060
reduced.sepsis1 <- reduced.sepsis[reduced.sepsis$itemid ==50010 | !is.na(reduced.sepsis$spec_itemid),]
### Function for above code 
z.1 <- function(complete){
  #complete <- data[data$Sid==patient,]
  ## Identify rows with blood culture taken
  for (i in 1:nrow(complete))
    if (!is.na(complete$spec_itemid[i]))
      complete$Blood_Culture[i] <- 1 
    ## Identify rows with high lactate
    for (i in 1:nrow(complete))
      if(!is.na(complete$itemid[i]) & !is.na(complete$valuenum[i]) & complete$itemid[i] == 50010 & complete$valuenum[i] >=2.5)
        complete$High_Lactate[i] <- 1
      ## Identify rows with severe sepsis
      # Empty list for chart times for blood culture
      time.bc <- c()
      # Get chart times for blood culture
      for (i in 1:nrow(complete)) 
        if (complete$Blood_Culture[i] == 1)
          time.bc <- c(complete$charttime[i], time.bc)
      time.bc <- rev(time.bc)
      # Assign severe sepsis to rows with high lactate that occur within 24 hours of a blood culture
      for (i in 1:nrow(complete))
        if (complete$High_Lactate[i] == 1 & min(abs(difftime(time.bc, complete$charttime[i], units="hours"))) < 24)
          complete$Severe_Sepsis[i] <- 1
      return(complete)
}


library(plyr)
## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
data1 <- ddply(reduced.sepsis1, .(id), z.1) 
## Identify patients from 'data1' who have severe sepsis
severe.sepsis.observations <- data1[data1$Severe_Sepsis==1,]
severe.sepsis.pt <- severe.sepsis.observations[!duplicated(severe.sepsis.observations$id),1]

## Count number of patients classified with severe.sepsis: 2,166; 2,204
length(severe.sepsis.pt)

## Create new data set with patients who have severe sepsis
severe.sepsis <- data1[data1$id %in% severe.sepsis.pt,]

bc.missing.sepsis.1 <- possible.sepsis[(possible.sepsis$id %in% bc.missing.patients),]
# Subset of blood culture with time (excludes blood culture without time)
bc.missing.sepsis.2 <- bc.missing.sepsis.1[!(is.na(bc.missing.sepsis.1$spec_itemid)) & !(is.na(bc.missing.sepsis.1$charttime)),]

## Identify patients who have at least one blood culture with a chart time: 691
bc.patients.0 <- bc.missing.sepsis.2[!duplicated(bc.missing.sepsis.2$id),1]

## Subset of data for patients who have at least one blood culture with a chart time
bc.missing.sepsis.3 <- bc.missing.sepsis.1[bc.missing.sepsis.1$id %in% bc.patients.0,]
# Choose only values with chart time
bc.missing.sepsis.4 <- bc.missing.sepsis.3[!is.na(bc.missing.sepsis.3$charttime),]

## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
bc.missing.sepsis.5 <- bc.missing.sepsis.4[!is.na(bc.missing.sepsis.4$spec_itemid) | bc.missing.sepsis.4$itemid==50010,]
data.2 <- ddply(bc.missing.sepsis.5, .(id), z.1) 

## Identify first instance of high lactate for each patient (from 'data.2' - patients previously excluded)
hl.subset.0 <- data.2[data.2$High_Lactate==1,]                         # Returns all instances of high lactate
hl.subset.1 <- hl.subset.0[!duplicated(hl.subset.0$id),]

## Identify patients whose first high lactate is also a severe sepsis event
hl.subset.2 <- hl.subset.1[hl.subset.1$Severe_Sepsis==1,]
additional.ss.index <- hl.subset.2[!duplicated(hl.subset.2$id), 1] #230 patients

## If a patient's first high lactate was also a severe sepsis event, add that patients data to the Severe Sepsis group
additional.ss <- data.2[(data.2$id %in% additional.ss.index),]
severe.sepsis.all <- rbind(severe.sepsis, additional.ss)                  # Combine with 'severe.sepsis' which is the first dataset that originally excluded the second group
length(severe.sepsis.all$id[!duplicated(severe.sepsis.all$id)]) #1,353 patients
severe.sepsis.id2 <- severe.sepsis.all$id[!duplicated(severe.sepsis.all$id)] #2,396


# #================patients with infections and organ dysfunction==================================
# 
# ## Patients with Infection and Organ Dysfunction -----------------------
infect <- read.csv("infection_byicustay.csv", header = T)
infect <- infect[!is.na(infect$code),]
infect$id <- paste(infect$subject_id, infect$hospital_seq, infect$icustay_seq, sep='#%#')
infect.id <- unique(infect$id) #10,984

dysfunc <- read.csv("dysfunction_byicustay.csv", header = T)
dysfunc <- dysfunc[!is.na(dysfunc$code),]
dysfunc$id <- paste(dysfunc$subject_id, dysfunc$hospital_seq, dysfunc$icustay_seq, sep='#%#')
dysfunc.id <- unique(dysfunc$id) #9,051

infectdysfun.id <- intersect(infect.id, dysfunc.id) #5,739

sofa3 <- read.csv("sofas_byicu.csv", header = T)
sofa3$id <- paste(sofa3$subject_id, sofa3$hospital_seq, sofa3$icustay_seq, sep='#%#')
sofa3.id <- unique(sofa3$id) #8,872

#cardiovascular failure by icd9
cardiofail1 <- read.csv("cardiovascular_dysfunctionbyICD9ByICU.csv", header = T)
cardiofail1 <- cardiofail1[!is.na(cardiofail1$code),]
cardiofail1$id <- paste(cardiofail1$subject_id, cardiofail1$hospital_seq, cardiofail1$icustay_seq, sep='#%#')
cardiofail1.id <- unique(cardiofail1$id) #3,749

#cardiovascular failure by lowest MAP or lowest systolic pressure
# cardiofail2 <- read.csv("low_systolic_pressure.csv", header = T)
# cardiofail2$id <- paste(cardiofail2$subject_id, cardiofail2$hospital_seq, sep='#%#')
# cardiofail2.id <- unique(cardiofail2$id) #10,514
# 


lowsystolicBP <- read.csv("lowsystolic.csv", header = T)
lowsystolicBP <- lowsystolicBP[!is.na(lowsystolicBP$lowsystolic) & lowsystolicBP$lowsystolic < 90, ]
lowsystolicBP$id <- paste(lowsystolicBP$subject_id, lowsystolicBP$hospital_seq, lowsystolicBP$icustay_seq, sep='#%#')
lowsystolicBP.id <- unique(lowsystolicBP$id) #11041


lowMAP <- read.csv("lowMAP.csv", header = T)
lowMAP <- lowMAP[!is.na(lowMAP$lowmap) & lowMAP$lowmap < 65, ]
lowMAP$id <- paste(lowMAP$subject_id, lowMAP$hospital_seq, lowMAP$icustay_seq, sep='#%#')
lowMAP.id <- unique(lowMAP$id) #16,608


cardiofail.id <- union(cardiofail1.id, lowsystolicBP.id) #12,146
cardiofail.id <- union(cardiofail.id, lowMAP.id) #17,538
dysfunc2.id <- union(sofa3.id, cardiofail.id) #19,798
infectdysfun2.id <- intersect(infect.id, dysfunc2.id) #9,550
 

# Patients with severe sepsis by our definition-------
hlbc.sp.id <- severe.sepsis.pt #1,143


charts$id <- paste(charts$subject_id, charts$hospseq, charts$icustay_seq,sep='#%#') 
heart.rate.id <- unique(charts$id[charts$itemid==211 & charts$value1num > 90]) #17,986
resp.rate.id <- unique(charts$id[charts$itemid==618 & charts$value1num > 20]) #20,262
pco2.id <- unique(complete$id[complete$itemid==50016 & complete$valuenum < 32]) #6,321
resp.pco2.id <- union(resp.rate.id, pco2.id) #21,192
wbc.id <- unique(complete$id[complete$itemid==50316 & (complete$valuenum > 12 | complete$valuenum < 4)]) #55
temp.id <- unique(charts$id[charts$itemid==678 & (charts$value1num > 100.4 | charts$value1num < 96.8)]) #15,023
heart.temp.id <- intersect(heart.rate.id, temp.id) #13,394
temp.resp.pco2.id <- intersect(resp.pco2.id, temp.id) #14,713; 14,757
temp.wbc.id <- intersect(wbc.id, temp.id) #43
heart.resp.pco2.id <- intersect(heart.rate.id, resp.pco2.id) #17,630
heart.wbc.id <- intersect(heart.rate.id, wbc.id) #46
resp.pco2.wbc.id <- intersect(resp.pco2.id, wbc.id) #50
u1 <- union(heart.temp.id, temp.resp.pco2.id) #14,922; 14,930
u2 <- union(temp.wbc.id, heart.resp.pco2.id) #17,630
u3 <- union(heart.wbc.id, resp.pco2.wbc.id) #50
u4 <- union(u1, u2) #19,331; 19,339
u5 <- union(u3, u4) #19,335; 19,343

sirs.id <- u5 #19,335; 19,343

sirs.infectdysfun.id <- intersect(sirs.id, infectdysfun.id) #4,272 patients visits; 4,273

sirs.infectdysfun2.id <- intersect(sirs.id, infectdysfun2.id) #7,310 patients visits; 7,311


#Compare differet defitions of severe sepsis
# df1: hl + bl in 24 hours
# df2: infection + organ dysfunction1 (by icd9)
# dd3: 2/4sirs +infection + organ dysfunction (by icd9)
# df4: infection + organ dysfunction1 (by sofa+cardiovascular failure)
# dd5: 2/4sirs +infection + organ dysfunction (by sofa+cardiovascular failure)
df1 <- severe.sepsis.id #1,353; 1,373
df2 <- infectdysfun.id #5,739
df3 <- sirs.infectdysfun.id #4,272
df4 <- infectdysfun2.id #9,550
df5 <- sirs.infectdysfun2.id #7,310
df6 <- severe.sepsis.id2

df1.ndf2 <-setdiff(df1, df2) #443
df1.df2 <-intersect(df1, df2) #680
df2.ndf1 <- setdiff(df2, df1) #5,059

df1.ndf3 <- setdiff(df1, df3) #580
df1.df3 <- intersect(df1, df3) #543
df3.ndf1 <- setdiff(df3, df1) #3,729

df1.ndf4 <-setdiff(df1, df4) #319
df1.df4 <-intersect(df1, df4) #804
df4.ndf1 <- setdiff(df4, df1) #8,746

df1.ndf5 <-setdiff(df1, df5) #489
df1.df5 <-intersect(df1, df5) #634
df5.ndf1 <- setdiff(df5, df1) #6,676
# 

df6.ndf2 <-setdiff(df6, df2) #443
df6.df2 <-intersect(df6, df2) #680
df2.ndf6 <- setdiff(df2, df6) #5,059

df6.ndf3 <- setdiff(df6, df3) #580
df6.df3 <- intersect(df6, df3) #543
df3.ndf6 <- setdiff(df3, df6) #3,729

df6.ndf4 <-setdiff(df6, df4) #319
df6.df4 <-intersect(df6, df4) #804
df4.ndf6 <- setdiff(df4, df6) #8,746

df6.ndf5 <-setdiff(df6, df5) #489
df6.df5 <-intersect(df6, df5) #634
df5.ndf6 <- setdiff(df5, df6) #6,676
# ==========================================mortiality info=================================================================
mortalityinfos <- read.csv('mortality.csv', header = T)
mortalityinfos$id <- paste(mortalityinfos$subject_id, mortalityinfos$hospital_seq, mortalityinfos$icustay_seq, sep='#%#')
names(mortalityinfos)

mortalityinfos2 <- read.csv('pticu_infos.csv', header = T)
mortalityinfos2$id <- paste(mortalityinfos2$subject_id, mortalityinfos2$hospital_seq, mortalityinfos2$icustay_seq, sep='#%#')
names(mortalityinfos2)
mortalityinfos2$id0 <- paste(mortalityinfos2$subject_id, mortalityinfos2$hospital_seq, sep='#%#')
mortalityinfos2$hospital_disch_dt <- ymd_hms(mortalityinfos2$hospital_disch_dt)
mortalityinfos2$icustay_outtime <- ymd_hms(mortalityinfos2$icustay_outtime)
mortalityinfos2$dod <- ymd_hms(mortalityinfos2$dod)

mortalityinfos2.hosp30 <- mortalityinfos2[mortalityinfos2$hospital_expire_flg == 'N' & (difftime(mortalityinfos2$dod, mortalityinfos2$hospital_disch_dt, units="days") < 30), ]
mortalityinfos2.icu30 <- mortalityinfos2[mortalityinfos2$icustay_expire_flg == 'N' & (difftime(mortalityinfos2$dod, mortalityinfos2$icustay_outtime, units="days") < 30), ]

mortalityinfos2.hosp30.id <- unique(mortalityinfos2.hosp30$id)
mortalityinfos2.icu30.id <- unique(mortalityinfos2.icu30$id)

bld$id0 <- paste(bld$subject_id, bld$hospital_seq, sep='#%#')
pts.ids <- unique(mortalityinfos2$id)
pts.id0 <- mortalityinfos2$id0
bld.ids <- unique(bld$id)
bld.id0 <- unique(bld$id0)
bld.missingicu.id <- unique(bld$id0[bld$icuseq==0]) #1043; subject_id and hospital_seq with missing icustay_id in bld culture data
pts.ids.missingicu.id <- unique(pts.ids[pts.id0 %in% bld.missingicu.id]) # 2,235; the complete id infos of patients with missing icu stays
bld.ids.withicu.id <- unique(bld$id[bld$icuseq>0]) #22,851; the complete id infos of patients with icu-seq info
dff1 <- setdiff(pts.ids.missingicu.id, bld.ids.withicu.id)# 1,213; number of patients in bld culture data with missing icu_seq info
intersect1 <- intersect(pts.ids.missingicu.id, bld.ids.withicu.id) #1,022; number of patients in bld culture data with at least one bld culture taken
#hence, there are 2,235 possible icu-stays with 1,043 hospital visits which have undecided bld culture taken; 
#Among those 2,235 possible icu-stays, 1,022icu-stays had at least one bld-culture taken, while 1,213 had no bld culture taken or unknown since the timestamp is missing

hosp_mortality <- mortalityinfos[!is.na(mortalityinfos$hospital_expire_flg) & mortalityinfos$hospital_expire_flg == 'Y',]
hosp_nonmortality <- mortalityinfos[!is.na(mortalityinfos$hospital_expire_flg) & mortalityinfos$hospital_expire_flg == 'N',]
hosp_mortality.id <- unique(hosp_mortality$id) #3,028
hosp_nonmortality.id <- unique(hosp_nonmortality$id) #21,172

icu_mortality <- mortalityinfos[!is.na(mortalityinfos$icustay_expire_flg) & mortalityinfos$icustay_expire_flg == 'Y',]
icu_nonmortality <- mortalityinfos[!is.na(mortalityinfos$icustay_expire_flg) & mortalityinfos$icustay_expire_flg == 'N',]

icu_mortality.id <- unique(icu_mortality$id) # 2,026
icu_nonmortality.id <- unique(icu_nonmortality$id) #22,174

#========================morality in each definition of severe sepsis=================================
df1.hosp_mortality <- intersect(df1, hosp_mortality.id) #515
df1.hosp_nonmortality <- intersect(df1, hosp_nonmortality.id) #608

df2.hosp_mortality <- intersect(df2, hosp_mortality.id) #1,465
df2.hosp_nonmortality <- intersect(df2, hosp_nonmortality.id) #4,274

df3.hosp_mortality <- intersect(df3, hosp_mortality.id) #1,102
df3.hosp_nonmortality <- intersect(df3, hosp_nonmortality.id) #3,170

df4.hosp_mortality <- intersect(df4, hosp_mortality.id) #1,928
df4.hosp_nonmortality <- intersect(df4, hosp_nonmortality.id) #7,622

df5.hosp_mortality <- intersect(df5, hosp_mortality.id) #1,476
df5.hosp_nonmortality <- intersect(df5, hosp_nonmortality.id) #5,834

df6.hosp_mortality <- intersect(df6, hosp_mortality.id) #565
df6.hosp_nonmortality <- intersect(df6, hosp_nonmortality.id) #608


df1.icu_mortality <- intersect(df1, icu_mortality.id) #435
df1.icu_nonmortality <- intersect(df1, icu_nonmortality.id) #688

df2.icu_mortality <- intersect(df2, icu_mortality.id) #987
df2.icu_nonmortality <- intersect(df2, icu_nonmortality.id) #4,752

df3.icu_mortality <- intersect(df3, icu_mortality.id) #764
df3.icu_nonmortality <- intersect(df3, icu_nonmortality.id) #3,508

df4.icu_mortality <- intersect(df4, icu_mortality.id) #1,291
df4.icu_nonmortality <- intersect(df4, icu_nonmortality.id) #8,259

df5.icu_mortality <- intersect(df5, icu_mortality.id) #1,020
df5.icu_nonmortality <- intersect(df5, icu_nonmortality.id) #6,290

df6.icu_mortality <- intersect(df6, icu_mortality.id) #435
df6.icu_nonmortality <- intersect(df6, icu_nonmortality.id) #688


df1.mortalityinfos2.hosp30 <- intersect(df1, mortalityinfos2.hosp30.id) #515

df2.mortalityinfos2.hosp30 <- intersect(df2, mortalityinfos2.hosp30.id) #1,465

df3.mortalityinfos2.hosp30 <- intersect(df3, mortalityinfos2.hosp30.id) #1,102

df4.mortalityinfos2.hosp30 <- intersect(df4, mortalityinfos2.hosp30.id) #1,928

df5.mortalityinfos2.hosp30 <- intersect(df5, mortalityinfos2.hosp30.id) #1,476

df6.mortalityinfos2.hosp30 <- intersect(df6, mortalityinfos2.hosp30.id) #565


df1.mortalityinfos2.icu30 <- intersect(df1, mortalityinfos2.icu30.id) #435

df2.mortalityinfos2.icu30 <- intersect(df2, mortalityinfos2.icu30.id) #987

df3.mortalityinfos2.icu30 <- intersect(df3, mortalityinfos2.icu30.id) #764

df4.mortalityinfos2.icu30 <- intersect(df4, mortalityinfos2.icu30.id) #1,291

df5.mortalityinfos2.icu30 <- intersect(df5, mortalityinfos2.icu30.id) #1,020

df6.mortalityinfos2.icu30 <- intersect(df6, mortalityinfos2.icu30.id) #435

#===========================HIV and severe sepsis==========================
HIVicd9 <- read.csv('HIVicd9.csv', header = T)
HIVicd9$id <- paste(HIVicd9$subject_id, HIVicd9$hospital_seq, HIVicd9$icustay_seq, sep='#%#')
HIVicd9.id <- unique(HIVicd9$id) #404

HIVtests <- read.csv('HIVtests.csv', header = T)
HIVtests$id <- paste(HIVtests$subject_id, HIVtests$hospital_seq, HIVtests$icustay_seq, sep='#%#')
HIVtests1 <- HIVtests[HIVtests$itemid==50356 & (HIVtests$valuenum > 15 | HIVtests$valuenum < 6), ]
HIVtests1.id <- unique(HIVtests1$id) #96

HIVid <- union(HIVicd9.id, HIVtests1.id) #455
HIVid.hospmorality <- intersect(HIVid, hosp_mortality.id) #71
HIVid.nonhospmorality <- intersect(HIVid, hosp_nonmortality.id) #383

HIVid.icupmorality <- intersect(HIVid, icu_mortality.id) #50
HIVid.nonicupmorality <- intersect(HIVid, icu_nonmortality.id) #404

HIVid.hospmorality30 <- intersect(HIVid, mortalityinfos2.hosp30.id) #71
HIVid.icupmorality30 <- intersect(HIVid, mortalityinfos2.icu30.id) #50

HIVid.ndf1 <-setdiff(HIVid, df1) #407
HIVid.df1 <-intersect(HIVid, df1) #48
df1.nHIVid  <- setdiff(df1, HIVid) #1,075

HIVid.ndf2 <-setdiff(HIVid, df2) #284
HIVid.df2 <-intersect(HIVid, df2) #171
df2.nHIVid  <- setdiff(df2, HIVid) #5,568

HIVid.ndf3 <-setdiff(HIVid, df3) #326
HIVid.df3 <-intersect(HIVid, df3) #129
df3.nHIVid  <- setdiff(df3, HIVid) #4,143

HIVid.ndf4 <-setdiff(HIVid, df4) #205
HIVid.df4 <-intersect(HIVid, df4) #250
df4.nHIVid  <- setdiff(df4, HIVid) #9,300

HIVid.ndf5 <-setdiff(HIVid, df5) #265
HIVid.df5 <-intersect(HIVid, df5) #190
df5.nHIVid  <- setdiff(df5, HIVid) #7,120

HIVid.ndf6 <-setdiff(HIVid, df6) #407
HIVid.df6 <-intersect(HIVid, df6) #48
df6.nHIVid  <- setdiff(df6, HIVid) #1,075





