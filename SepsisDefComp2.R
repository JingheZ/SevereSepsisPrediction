#Prepare patient data in target group and control group, respectively.

#Read lab, bld culture, and vital signs data
lab <- read.csv("pts_lab_new.csv", header=TRUE)
bld<- read.csv("pts_bldcultures3_new.csv", header=TRUE)
charts <- read.csv('pts_vitals1b_new.csv', header = TRUE)


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

bld1 <- data.frame(bld$id, bld$charttime, bld$spec_itemid)
bld1$valuenum <- rep(NA, nrow(bld1))
names(bld1) <- c('id', 'charttime', 'itemid', 'valuenum')

lab1 <- data.frame(lab$id, lab$charttime, lab$itemid, lab$valuenum)
names(lab1) <- c('id', 'charttime', 'itemid', 'valuenum')

bp.value2 <- charts[charts$itemid == 455,]
bp.value2[,c(6)] <- bp.value2[,c(7)]
bp.value2$itemid <- 4552  
# assign dbp to new put back into chart1
chart1 <- data.frame(rbind(charts, bp.value2))
# # temperature in F
# temp.C <- chart1[chart1$itemid == 678,]
names(chart1)
chart2 <- data.frame(chart1$id, chart1$charttime, chart1$itemid, chart1$value1num)
names(chart2) <- c('id', 'charttime', 'itemid', 'valuenum')

chart2.lactate <- chart2[!is.na(chart2$itemid)&(chart2$itemid == 818 | chart2$itemid == 1531), ]
chart2.lactate.id <- unique(chart2.lactate$id) #11,172

lab1.lactate <- lab1[!is.na(lab1$itemid)&(lab1$itemid == 50010), ]
lab1.lactate.id <- unique(lab1.lactate$id) #12,541

lactate.union <- union(lab1.lactate.id, chart2.lactate.id) #13,488


#combine the lab and bld culture data
complete <- rbind(lab1, bld1) 
complete <- rbind(complete, chart2) 

library(lubridate)
complete$charttime <- ymd_hms(complete$charttime)
complete <- complete[order(complete$charttime),]

## Add indicator columns to data frame
complete$Blood_Culture <- c(rep(0,nrow(complete)))
complete$High_Lactate <- c(rep(0,nrow(complete)))
complete$Severe_Sepsis <- c(rep(0,nrow(complete)))


#find patients with high lactate value
lactates <- complete[!is.na(complete$itemid) & (complete$itemid == 50010 | complete$itemid == 818 | complete$itemid == 1531),]       # lactate lab values
length(table(lactates$id))          # total patients had lactate tested  #24,951


# ===========================Severe Sepsis 1: lactate > 4 and blood culture taken within 24 hrs================================

highlactates <- lactates[lactates$valuenum > 4,] 
chart2.highlactate <- chart2.lactate[!is.na(chart2.lactate$itemid)&(chart2.lactate$itemid == 818 | chart2.lactate$itemid == 1531) & chart2.lactate$valuenum > 4, ]
chart2.highlactate.id <- unique(chart2.highlactate$id) #2,351

lab1.highlactate <- lab1.lactate[!is.na(lab1.lactate$itemid)&(lab1.lactate$itemid == 50010) & lab1.lactate$valuenum > 4, ]
lab1.highlactate.id <- unique(lab1.highlactate$id) #2,609

highlactate.union <- union(lab1.highlactate.id, chart2.highlactate.id) #2,898

hl <- highlactates$id[!duplicated(highlactates$id) & !is.na(highlactates$id)]            # Identify individuals with a high lactate
#3,043 patients

bldcultures <- complete[!is.na(complete$itemid) & (complete$itemid == 70011 | complete$itemid == 70012),] 
bc <- bldcultures$id[!duplicated(bldcultures$id) & !is.na(bldcultures$id)]     # Identify patients with a blood culture taken (13,099)

## From 'complete' data set, select only patients who have high lactate and blood culture
hl.bc <- intersect(hl, bc)  #2,087
# hl.bc <- as.list(hl.bc)

possible.sepsis <- complete[(complete$id %in% hl.bc),] 
# Identify patients in 'possible.sepsis' data set who have missing chart times for blood culture
# bc.missing <- possible.sepsis[possible.sepsis$HALFHOUR=="" & !is.na(possible.sepsis$SPEC_ITEMID),]         # Try this command if the one right below does not work
# bc.missing <- possible.sepsis[is.na(possible.sepsis$charttime) & (possible.sepsis$itemid == 70011 | possible.sepsis$itemid == 70012),]      
# bc.missing.patients <- bc.missing[!duplicated(bc.missing$id),1] #417 pts

## Create reduced subset of 'complete' that excludes patients without chart time for blood culture
# reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]
reduced.sepsis <- possible.sepsis[!is.na(possible.sepsis$charttime), ]
# Count how many patients in 'reduced.sepsis' : 
length(reduced.sepsis$id[!duplicated(reduced.sepsis$id)]) # 2,087
### Function for above code 

z.0 <- function(complete) {
  #complete <- data[data$Sid==patient,]
  ## Identify rows with blood culture taken
  for (i in 1:nrow(complete))
    if (!is.na(complete$itemid[i]) & (complete$itemid[i] == 70011 | complete$itemid[i] == 70012))
      complete$Blood_Culture[i] <- 1 
    ## Identify rows with high lactate
    for (i in 1:nrow(complete))
      if(!is.na(complete$itemid[i]) & !is.na(complete$valuenum[i]) & (complete$itemid[i] == 50010 | complete$itemid[i] == 818 | complete$itemid[i] == 1531) & complete$valuenum[i] > 4)
        complete$High_Lactate[i] <- 1
      ## Identify rows with severe sepsis
      # Empty list for chart times for blood culture
      time.bc <- c()
      # Get chart times for blood culture
      for (i in 1:nrow(complete)) 
        if (complete$Blood_Culture[i] == 1)
          time.bc <- c((complete$charttime[i]), time.bc)
      time.bc <- rev(time.bc)
      if (!is.null(time.bc)) {
        # Assign severe sepsis to rows with high lactate that occur within 24 hours of a blood culture
        for (i in 1:nrow(complete))
          if (complete$High_Lactate[i] == 1 & min(abs(difftime(time.bc, complete$charttime[i], units="hours"))) < 24)
            complete$Severe_Sepsis[i] <- 1 }
      return(complete)
}


library(plyr)
## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
data1 <- ddply(reduced.sepsis, .(id), z.0) 

## Identify patients from 'data1' who have severe sepsis
severe.sepsis.observations <- data1[data1$Severe_Sepsis==1,]
severe.sepsis.pt <- severe.sepsis.observations[!duplicated(severe.sepsis.observations$id),1] #1,349

## Count number of patients classified with severe.sepsis: 1,349
length(severe.sepsis.pt)

## Create new data set with patients who have severe sepsis
severe.sepsis <- data1[data1$id %in% severe.sepsis.pt,]
save(severe.sepsis, file = 'severe.sepsis.RData')
# load(file = 'severe.sepsis.RData')
severe.sepsis.pt <- unique(severe.sepsis$id)
write.csv(severe.sepsis.pt, file = 'severe.sepsis.pt.RData')

# ===========================Severe Sepsis 2: lactate > 2.5 and blood culture taken within 24 hrs================================
highlactates <- lactates[lactates$valuenum > 2.5,] 
chart2.highlactate <- chart2.lactate[!is.na(chart2.lactate$itemid)&(chart2.lactate$itemid == 818 | chart2.lactate$itemid == 1531) & chart2.lactate$valuenum > 2.5, ]
chart2.highlactate.id <- unique(chart2.highlactate$id) #4,742

lab1.highlactate <- lab1.lactate[!is.na(lab1.lactate$itemid)&(lab1.lactate$itemid == 50010) & lab1.lactate$valuenum > 2.5, ]
lab1.highlactate.id <- unique(lab1.highlactate$id) #5,238

highlactate.union <- union(lab1.highlactate.id, chart2.highlactate.id) #5,766

hl <- highlactates$id[!duplicated(highlactates$id) & !is.na(highlactates$id)]            # Identify individuals with a high lactate
#5,765 patients

bldcultures <- complete[!is.na(complete$itemid) & (complete$itemid == 70011 | complete$itemid == 70012),] 
bc <- bldcultures$id[!duplicated(bldcultures$id) & !is.na(bldcultures$id)]     # Identify patients with a blood culture taken (13,099)

## From 'complete' data set, select only patients who have high lactate and blood culture
hl.bc <- intersect(hl, bc)  #3,847
# hl.bc <- as.list(hl.bc)

possible.sepsis <- complete[(complete$id %in% hl.bc),] 
# Identify patients in 'possible.sepsis' data set who have missing chart times for blood culture
# bc.missing <- possible.sepsis[possible.sepsis$HALFHOUR=="" & !is.na(possible.sepsis$SPEC_ITEMID),]         # Try this command if the one right below does not work
# bc.missing <- possible.sepsis[is.na(possible.sepsis$charttime) & (possible.sepsis$itemid == 70011 | possible.sepsis$itemid == 70012),]      
# bc.missing.patients <- bc.missing[!duplicated(bc.missing$id),1] #417 pts

## Create reduced subset of 'complete' that excludes patients without chart time for blood culture
# reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]
reduced.sepsis <- possible.sepsis[!is.na(possible.sepsis$charttime), ]
# Count how many patients in 'reduced.sepsis' : 
length(reduced.sepsis$id[!duplicated(reduced.sepsis$id)]) # 3,847
### Function for above code 

z.0 <- function(complete) {
  #complete <- data[data$Sid==patient,]
  ## Identify rows with blood culture taken
  for (i in 1:nrow(complete))
    if (!is.na(complete$itemid[i]) & (complete$itemid[i] == 70011 | complete$itemid[i] == 70012))
      complete$Blood_Culture[i] <- 1 
    ## Identify rows with high lactate
    for (i in 1:nrow(complete))
      if(!is.na(complete$itemid[i]) & !is.na(complete$valuenum[i]) & (complete$itemid[i] == 50010 | complete$itemid[i] == 818 | complete$itemid[i] == 1531) & complete$valuenum[i] > 2.5)
        complete$High_Lactate[i] <- 1
      ## Identify rows with severe sepsis
      # Empty list for chart times for blood culture
      time.bc <- c()
      # Get chart times for blood culture
      for (i in 1:nrow(complete)) 
        if (complete$Blood_Culture[i] == 1)
          time.bc <- c((complete$charttime[i]), time.bc)
      time.bc <- rev(time.bc)
      if (!is.null(time.bc)) {
        # Assign severe sepsis to rows with high lactate that occur within 24 hours of a blood culture
        for (i in 1:nrow(complete))
          if (complete$High_Lactate[i] == 1 & min(abs(difftime(time.bc, complete$charttime[i], units="hours"))) < 24)
            complete$Severe_Sepsis[i] <- 1 }
      return(complete)
}


library(plyr)
## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
data1 <- ddply(reduced.sepsis, .(id), z.0) 

## Identify patients from 'data1' who have severe sepsis
severe.sepsis.observations <- data1[data1$Severe_Sepsis==1,]
severe.sepsis.pt <- severe.sepsis.observations[!duplicated(severe.sepsis.observations$id),1] #1,397

## Count number of patients classified with severe.sepsis: 2,539
length(severe.sepsis.pt)

## Create new data set with patients who have severe sepsis
severe.sepsis2 <- data1[data1$id %in% severe.sepsis.pt,]
save(severe.sepsis2, file = 'severe.sepsis2.RData')
load(file = 'severe.sepsis2.RData')
severe.sepsis.pt2 <- unique(severe.sepsis2$id)
write.csv(severe.sepsis.pt, file = 'severe.sepsis.pt.csv')
save(severe.sepsis2, file = 'severe.sepsis2.RData')



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


# charts$id <- paste(charts$subject_id, charts$hospseq, charts$icustay_seq,sep='#%#') 
heart.rate.id <- unique(charts$id[charts$itemid==211 & charts$value1num > 90]) #17,986
resp.rate.id <- unique(charts$id[charts$itemid==618 & charts$value1num > 20]) #20,262
pco2.id <- unique(complete$id[complete$itemid==50016 & complete$valuenum < 32]) #6,321
resp.pco2.id <- union(resp.rate.id, pco2.id) #21,192
wbc.id <- unique(complete$id[(complete$itemid==50316 | complete$itemid==50468) & (complete$valuenum > 12 | complete$valuenum < 4)]) #15,785
temp.id <- unique(charts$id[charts$itemid==678 & (charts$value1num > 100.4 | charts$value1num < 96.8)]) #15,023
heart.temp.id <- intersect(heart.rate.id, temp.id) #13,394
temp.resp.pco2.id <- intersect(resp.pco2.id, temp.id) #14,713; 14,757
temp.wbc.id <- intersect(wbc.id, temp.id) #10,222
heart.resp.pco2.id <- intersect(heart.rate.id, resp.pco2.id) #17,630
heart.wbc.id <- intersect(heart.rate.id, wbc.id) #12,182
resp.pco2.wbc.id <- intersect(resp.pco2.id, wbc.id) #13,881
u1 <- union(heart.temp.id, temp.resp.pco2.id) #14,922; 14,930
u2 <- union(temp.wbc.id, heart.resp.pco2.id) #18,467
u3 <- union(heart.wbc.id, resp.pco2.wbc.id) #14,059
u4 <- union(u1, u2) #19,377
u5 <- union(u3, u4) #20,633

sirs.id <- u5 #20,633


sirs4 <- intersect(heart.temp.id, resp.pco2.wbc.id)

sirs3.1 <- intersect(heart.temp.id, resp.pco2.id)
sirs3.2 <- intersect(heart.temp.id, wbc.id)
sirs3.3 <- intersect(temp.resp.pco2.id, wbc.id)
sirs3.4 <- intersect(heart.resp.pco2.id, wbc.id)
sirs3 <- union(sirs3.1, sirs3.2)
sirs3 <- union(sirs3, sirs3.3)
sirs34 <- union(sirs3, sirs3.4)
sirs3 <- setdiff(sirs34, sirs4)

sirs234 <- sirs.id
sirs2 <- setdiff(sirs234, sirs34)

sirs1 <- union(temp.id, heart.rate.id)
sirs1 <- union(sirs1, resp.pco2.id)
sirs1234 <- union(sirs1, wbc.id)
sirs1 <- setdiff(sirs1234, sirs234)



sirs.infectdysfun.id <- intersect(sirs.id, infectdysfun.id) #4,272 patients visits; 4,273

sirs.infectdysfun2.id <- intersect(sirs.id, infectdysfun2.id) #7,310 patients visits; 7,311


#Compare differet defitions of severe sepsis
# df1: hl + bl in 24 hours
# df2: infection + organ dysfunction1 (by icd9)
# dd3: 2/4sirs +infection + organ dysfunction (by icd9)
# df4: infection + organ dysfunction1 (by sofa+cardiovascular failure)
# dd5: 2/4sirs +infection + organ dysfunction (by sofa+cardiovascular failure)
df1 <- severe.sepsis.pt #1,349
df2 <- infectdysfun.id #5,739
df3 <- sirs.infectdysfun.id #4,273
df4 <- infectdysfun2.id #9,550
df5 <- sirs.infectdysfun2.id #7,311
df6 <- severe.sepsis.pt2 #2,539

write.csv(df2, file = 'df2.csv')
write.csv(df3, file = 'df3.csv')
write.csv(df4, file = 'df4.csv')
write.csv(df5, file = 'df5.csv')


df1.ndf2 <-setdiff(df1, df2) #533
df1.df2 <-intersect(df1, df2) #816
df2.ndf1 <- setdiff(df2, df1) #4,923

df1.ndf3 <- setdiff(df1, df3) #600
df1.df3 <- intersect(df1, df3) #749
df3.ndf1 <- setdiff(df3, df1) #3,947

df1.ndf4 <-setdiff(df1, df4) #363
df1.df4 <-intersect(df1, df4) #986
df4.ndf1 <- setdiff(df4, df1) #8,564

df1.ndf5 <-setdiff(df1, df5) #575
df1.df5 <-intersect(df1, df5) #774
df5.ndf1 <- setdiff(df5, df1) #6,537
# 

df6.ndf2 <-setdiff(df6, df2) #1105
df6.df2 <-intersect(df6, df2) #1434
df2.ndf6 <- setdiff(df2, df6) #4,305

df6.ndf3 <- setdiff(df6, df3) #1265
df6.df3 <- intersect(df6, df3) #1274
df3.ndf6 <- setdiff(df3, df6) #3,422

df6.ndf4 <-setdiff(df6, df4) #729
df6.df4 <-intersect(df6, df4) #1810
df4.ndf6 <- setdiff(df4, df6) #7740

df6.ndf5 <-setdiff(df6, df5) #929
df6.df5 <-intersect(df6, df5) #1610
df5.ndf6 <- setdiff(df5, df6) #6291

write.csv(df2, file = 'df2.csv')

# ==========================================mortiality info=================================================================
mortalityinfos <- read.csv('mortality.csv', header = T)
mortalityinfos$id <- paste(mortalityinfos$subject_id, mortalityinfos$hospital_seq, mortalityinfos$icustay_seq, sep='#%#')
names(mortalityinfos)

mortalityinfos2 <- read.csv('pticu_infos.csv', header = T)
mortalityinfos2$id <- paste(mortalityinfos2$subject_id, mortalityinfos2$hospital_seq, mortalityinfos2$icustay_seq, sep='#%#')
names(mortalityinfos2)
length(unique(mortalityinfos2$id)) #24200
mortalityinfos2$id0 <- paste(mortalityinfos2$subject_id, mortalityinfos2$hospital_seq, sep='#%#')
length(unique(mortalityinfos2$id0)) #22756
mortalityinfos2$hospital_disch_dt <- ymd_hms(mortalityinfos2$hospital_disch_dt)
mortalityinfos2$icustay_outtime <- ymd_hms(mortalityinfos2$icustay_outtime)
mortalityinfos2$dod <- ymd_hms(mortalityinfos2$dod)

hosp_mortality <- mortalityinfos2[!is.na(mortalityinfos2$hospital_expire_flg) & mortalityinfos2$hospital_expire_flg == 'Y',]
hosp_nonmortality <- mortalityinfos2[!is.na(mortalityinfos2$hospital_expire_flg) & mortalityinfos2$hospital_expire_flg == 'N',]
hosp_mortality.id <- unique(hosp_mortality$id) #3,028
hosp_mortality.id2 <- unique(hosp_mortality$id0) #2,733
hosp_nonmortality.id <- unique(hosp_nonmortality$id) #21,172

icu_mortality <- mortalityinfos2[!is.na(mortalityinfos2$icustay_expire_flg) & mortalityinfos2$icustay_expire_flg == 'Y',]
icu_nonmortality <- mortalityinfos2[!is.na(mortalityinfos2$icustay_expire_flg) & mortalityinfos2$icustay_expire_flg == 'N',]

icu_mortality.id <- unique(icu_mortality$id) # 2,026
icu_nonmortality.id <- unique(icu_nonmortality$id) #22,174

load('severe.sepsis.timeofevent.RData')
df1.timeofevent <- severe.sepsis.timeofevent
df1.mortality30 <- merge(df1.timeofevent, mortalityinfos2, by.x = 'id', by.y = 'id', all.x = T)
df1.mortality30 <- df1.mortality30[!is.na(df1.mortality30$dod),]
str(df1.mortality30)
df1.mortality30.id <- c()
for (i in 1:nrow(df1.mortality30)) 
  if (!is.na(df1.mortality30$dod[i]) & difftime(df1.mortality30$dod[i], df1.mortality30$time.event[i], units="hours") < 30*24) {
    df1.mortality30.id <- c(df1.mortality30.id, as.character(df1.mortality30$id[i]))
  }


df6.timeofevent <- severe.sepsis.timeofevent2
df6.mortality30 <- merge(df6.timeofevent, mortalityinfos2, by.x = 'id', by.y = 'id', all.x = T)
df6.mortality30 <- df6.mortality30[!is.na(df6.mortality30$dod),]
head(df6.mortality30)
df6.mortality30.id <- c()
for (i in 1:nrow(df6.mortality30)) 
  if (difftime(df6.mortality30$dod[i], df6.mortality30$time.event[i], units="hours") < 30*24) {
    df6.mortality30.id <- c(df6.mortality30.id, as.character(df6.mortality30$id[i]))
  }
length(df6.mortality30.id)
length(df6.mortality30.id)/2539
# mortalityinfos2.hosp30 <- mortalityinfos2[mortalityinfos2$hospital_expire_flg == 'N' & (difftime(mortalityinfos2$dod, mortalityinfos2$hospital_disch_dt, units="days") < 30), ]
# mortalityinfos2.icu30 <- mortalityinfos2[mortalityinfos2$icustay_expire_flg == 'N' & (difftime(mortalityinfos2$dod, mortalityinfos2$icustay_outtime, units="days") < 30), ]
# 
# mortalityinfos2.hosp30.id <- unique(mortalityinfos2.hosp30$id) #931
# mortalityinfos2.icu30.id <- unique(mortalityinfos2.icu30$id)
#30-day moratlity (from time of severe sepsis event) of HIV & severe sepsis patients (by definition 1)

#========================morality in each definition of severe sepsis=================================
# install.packages("stringr")
library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unique(unlist(ids2))
  return(ids2)
}


df1.id2 <- hospids(df1) #1335
df2.id2 <- hospids(df2) #5080
df3.id2 <- hospids(df3) #4208
df4.id2 <- hospids(df4) #8666
df5.id2 <- hospids(df5) #6661
df6.id2 <- hospids(df6) #2495


df1.df2.id2 <- hospids(df1.df2) #804
df1.df3.id2 <- hospids(df1.df3) #738
df1.df4.id2 <- hospids(df1.df4) #972
df1.df5.id2 <- hospids(df1.df5) #892

df6.df2.id2 <- hospids(df6.df2) #1401
df6.df3.id2 <- hospids(df6.df3) #1246
df6.df4.id2 <- hospids(df6.df4) #1768
df6.df5.id2 <- hospids(df6.df5) #1576

df1.hosp_mortality <- intersect(df1, hosp_mortality.id) #627
df2.hosp_mortality <- intersect(df2, hosp_mortality.id) #1,465
df3.hosp_mortality <- intersect(df3, hosp_mortality.id) #1,261
df4.hosp_mortality <- intersect(df4, hosp_mortality.id) #1,928
df5.hosp_mortality <- intersect(df5, hosp_mortality.id) #1,664
df6.hosp_mortality <- intersect(df6, hosp_mortality.id) #948


df1.df2.hosp_mortality <- intersect(df1.df2, hosp_mortality.id) #437
df1.df3.hosp_mortality <- intersect(df1.df3, hosp_mortality.id) #407
df1.df4.hosp_mortality <- intersect(df1.df4, hosp_mortality.id) #487
df1.df5.hosp_mortality <- intersect(df1.df5, hosp_mortality.id) #450

df6.df2.hosp_mortality <- intersect(df6.df2, hosp_mortality.id) #640
df6.df3.hosp_mortality <- intersect(df6.df3, hosp_mortality.id) #586
df6.df4.hosp_mortality <- intersect(df6.df4, hosp_mortality.id) #731
df6.df5.hosp_mortality <- intersect(df6.df5, hosp_mortality.id) #667


df1.icu_mortality <- intersect(df1, icu_mortality.id) #530
df2.icu_mortality <- intersect(df2, icu_mortality.id) #987
df3.icu_mortality <- intersect(df3, icu_mortality.id) #881
df4.icu_mortality <- intersect(df4, icu_mortality.id) #1,291
df5.icu_mortality <- intersect(df5, icu_mortality.id) #1,163
df6.icu_mortality <- intersect(df6, icu_mortality.id) #772

df1.df2.icu_mortality <- intersect(df1.df2, icu_mortality.id) #368
df1.df3.icu_mortality <- intersect(df1.df3, icu_mortality.id) #343
df1.df4.icu_mortality <- intersect(df1.df4, icu_mortality.id) #416
df1.df5.icu_mortality <- intersect(df1.df5, icu_mortality.id) #387

df6.df2.icu_mortality <- intersect(df6.df2, icu_mortality.id) #520
df6.df3.icu_mortality <- intersect(df6.df3, icu_mortality.id) #477
df6.df4.icu_mortality <- intersect(df6.df4, icu_mortality.id) #598
df6.df5.icu_mortality <- intersect(df6.df5, icu_mortality.id) #550


# df1.mortalityinfos2.hosp30 <- intersect(df1, mortalityinfos2.hosp30.id) #515
# df2.mortalityinfos2.hosp30 <- intersect(df2, mortalityinfos2.hosp30.id) #1,465
# df3.mortalityinfos2.hosp30 <- intersect(df3, mortalityinfos2.hosp30.id) #1,102
# df4.mortalityinfos2.hosp30 <- intersect(df4, mortalityinfos2.hosp30.id) #1,928
# df5.mortalityinfos2.hosp30 <- intersect(df5, mortalityinfos2.hosp30.id) #1,476
# df6.mortalityinfos2.hosp30 <- intersect(df6, mortalityinfos2.hosp30.id) #565
# 
# df1.mortalityinfos2.icu30 <- intersect(df1, mortalityinfos2.icu30.id) #435
# df2.mortalityinfos2.icu30 <- intersect(df2, mortalityinfos2.icu30.id) #987
# df3.mortalityinfos2.icu30 <- intersect(df3, mortalityinfos2.icu30.id) #764
# df4.mortalityinfos2.icu30 <- intersect(df4, mortalityinfos2.icu30.id) #1,291
# df5.mortalityinfos2.icu30 <- intersect(df5, mortalityinfos2.icu30.id) #1,020
# df6.mortalityinfos2.icu30 <- intersect(df6, mortalityinfos2.icu30.id) #435

#============mortalities among patients satisfying SIRS criteria==================
sirs1.id2 <- hospids(sirs1) #2616
sirs2.id2 <- hospids(sirs2) #3953
sirs3.id2 <- hospids(sirs3) #7011
sirs4.id2 <- hospids(sirs4) #8977
sirs234.id2 <- hospids(sirs234) #19252


sirs1.hosp_mortality <- intersect(sirs1, hosp_mortality.id) #283
sirs2.hosp_mortality <- intersect(sirs2, hosp_mortality.id) #359
sirs3.hosp_mortality <- intersect(sirs3, hosp_mortality.id) #589
sirs4.hosp_mortality <- intersect(sirs4, hosp_mortality.id) #1709
sirs234.hosp_mortality <- intersect(sirs234, hosp_mortality.id) #2657


sirs1.icu_mortality <- intersect(sirs1, icu_mortality.id) #152
sirs2.icu_mortality <- intersect(sirs2, icu_mortality.id) #230
sirs3.icu_mortality <- intersect(sirs3, icu_mortality.id) #351
sirs4.icu_mortality <- intersect(sirs4, icu_mortality.id) #1258
sirs234.icu_mortality <- intersect(sirs234, icu_mortality.id) #1839

length(sirs1.icu_mortality) / length(sirs1)
length(sirs2.icu_mortality) / length(sirs2)
length(sirs3.icu_mortality) / length(sirs3)
length(sirs4.icu_mortality) / length(sirs4)
length(sirs234.icu_mortality) / length(sirs234)

length(sirs1.hosp_mortality) / length(sirs1.id2)
length(sirs2.hosp_mortality) / length(sirs2.id2)
length(sirs3.hosp_mortality) / length(sirs3.id2)
length(sirs4.hosp_mortality) / length(sirs4.id2)
length(sirs234.hosp_mortality) / length(sirs234.id2)


#===========================HIV and severe sepsis==========================
HIVicd9 <- read.csv('HIVicd9.csv', header = T)
HIVicd9$id <- paste(HIVicd9$subject_id, HIVicd9$hospital_seq, HIVicd9$icustay_seq, sep='#%#')
HIVicd9$id0 <- paste(HIVicd9$subject_id, HIVicd9$hospital_seq, sep='#%#')
HIVicd9.id <- unique(HIVicd9$id) #404
HIVicd9.id0 <- unique(HIVicd9$id0) #376
HIVicd9.id00 <- unique(HIVicd9$subject_id) #308
HIVtests <- read.csv('HIVtests.csv', header = T)
HIVtests$id <- paste(HIVtests$subject_id, HIVtests$hospital_seq, HIVtests$icustay_seq, sep='#%#')
# HIVtests1 <- HIVtests[HIVtests$itemid==50356 & (HIVtests$valuenum > 15 | HIVtests$valuenum < 6), ]
# HIVtests1.id <- unique(HIVtests1$id) #96

# HIVid <- union(HIVicd9.id, HIVtests1.id) #455
HIVid <- HIVicd9.id
HIVid.hospmortality <- intersect(HIVid, hosp_mortality.id) #57
HIVid.icumortality <- intersect(HIVid, icu_mortality.id) #37

HIVid.id2 <- hospids(HIVid)
57/length(HIVid.id2)


# HIVid.ndf1 <-setdiff(HIVid, df1) #407
HIVid.df1 <-intersect(HIVid, df1) #53
# df1.nHIVid  <- setdiff(df1, HIVid) #1,075

# HIVid.ndf2 <-setdiff(HIVid, df2) #284
HIVid.df2 <-intersect(HIVid, df2) #146
# df2.nHIVid  <- setdiff(df2, HIVid) #5,568

# HIVid.ndf3 <-setdiff(HIVid, df3) #326
HIVid.df3 <-intersect(HIVid, df3) #107
# df3.nHIVid  <- setdiff(df3, HIVid) #4,143

# HIVid.ndf4 <-setdiff(HIVid, df4) #205
HIVid.df4 <-intersect(HIVid, df4) #216
# df4.nHIVid  <- setdiff(df4, HIVid) #9,300

# HIVid.ndf5 <-setdiff(HIVid, df5) #265
HIVid.df5 <-intersect(HIVid, df5) #158
# df5.nHIVid  <- setdiff(df5, HIVid) #7,120

# HIVid.ndf6 <-setdiff(HIVid, df6) #407
HIVid.df6 <-intersect(HIVid, df6) #81
# df6.nHIVid  <- setdiff(df6, HIVid) #1,075

HIVid.df1.id2 <- hospids(HIVid.df1)
HIVid.df2.id2 <- hospids(HIVid.df2)
HIVid.df3.id2 <- hospids(HIVid.df3)
HIVid.df4.id2 <- hospids(HIVid.df4)
HIVid.df5.id2 <- hospids(HIVid.df5)
HIVid.df6.id2 <- hospids(HIVid.df6)


# HIVid.ndf1.icumortality <- intersect(HIVid.ndf1, HIVid.icumortality)
# HIVid.ndf2.icumortality <- intersect(HIVid.ndf2, HIVid.icumortality)
# HIVid.ndf3.icumortality <- intersect(HIVid.ndf3, HIVid.icumortality)
# HIVid.ndf4.icumortality <- intersect(HIVid.ndf4, HIVid.icumortality)
# HIVid.ndf5.icumortality <- intersect(HIVid.ndf5, HIVid.icumortality)
# HIVid.ndf6.icumortality <- intersect(HIVid.ndf6, HIVid.icumortality)

HIVid.df1.icumortality <- intersect(HIVid.df1, HIVid.icumortality)
HIVid.df2.icumortality <- intersect(HIVid.df2, HIVid.icumortality)
HIVid.df3.icumortality <- intersect(HIVid.df3, HIVid.icumortality)
HIVid.df4.icumortality <- intersect(HIVid.df4, HIVid.icumortality)
HIVid.df5.icumortality <- intersect(HIVid.df5, HIVid.icumortality)
HIVid.df6.icumortality <- intersect(HIVid.df6, HIVid.icumortality)

# length(HIVid.ndf1.icumortality) / length(HIVid.ndf1)
# length(HIVid.ndf2.icumortality) / length(HIVid.ndf2)
# length(HIVid.ndf3.icumortality) / length(HIVid.ndf3)
# length(HIVid.ndf4.icumortality) / length(HIVid.ndf4)
# length(HIVid.ndf5.icumortality) / length(HIVid.ndf5)
# length(HIVid.ndf6.icumortality) / length(HIVid.ndf6)

length(HIVid.df1.icumortality) / length(HIVid.df1)
length(HIVid.df2.icumortality) / length(HIVid.df2)
length(HIVid.df3.icumortality) / length(HIVid.df3)
length(HIVid.df4.icumortality) / length(HIVid.df4)
length(HIVid.df5.icumortality) / length(HIVid.df5)
length(HIVid.df6.icumortality) / length(HIVid.df6)


# HIVid.ndf1.hospmortality <- intersect(HIVid.ndf1, HIVid.hospmortality)
# HIVid.ndf2.hospmortality <- intersect(HIVid.ndf2, HIVid.hospmortality)
# HIVid.ndf3.hospmortality <- intersect(HIVid.ndf3, HIVid.hospmortality)
# HIVid.ndf4.hospmortality <- intersect(HIVid.ndf4, HIVid.hospmortality)
# HIVid.ndf5.hospmortality <- intersect(HIVid.ndf5, HIVid.hospmortality)
# HIVid.ndf6.hospmortality <- intersect(HIVid.ndf6, HIVid.hospmortality)

HIVid.df1.hospmortality <- intersect(HIVid.df1, HIVid.hospmortality)
HIVid.df2.hospmortality <- intersect(HIVid.df2, HIVid.hospmortality)
HIVid.df3.hospmortality <- intersect(HIVid.df3, HIVid.hospmortality)
HIVid.df4.hospmortality <- intersect(HIVid.df4, HIVid.hospmortality)
HIVid.df5.hospmortality <- intersect(HIVid.df5, HIVid.hospmortality)
HIVid.df6.hospmortality <- intersect(HIVid.df6, HIVid.hospmortality)


# length(HIVid.ndf1.hospmortality) / length(HIVid.ndf1)
# length(HIVid.ndf2.hospmortality) / length(HIVid.ndf2)
# length(HIVid.ndf3.hospmortality) / length(HIVid.ndf3)
# length(HIVid.ndf4.hospmortality) / length(HIVid.ndf4)
# length(HIVid.ndf5.hospmortality) / length(HIVid.ndf5)
# length(HIVid.ndf6.hospmortality) / length(HIVid.ndf6)

length(HIVid.df1.hospmortality) / length(HIVid.df1.id2)
length(HIVid.df2.hospmortality) / length(HIVid.df2.id2)
length(HIVid.df3.hospmortality) / length(HIVid.df3.id2)
length(HIVid.df4.hospmortality) / length(HIVid.df4.id2)
length(HIVid.df5.hospmortality) / length(HIVid.df5.id2)
length(HIVid.df6.hospmortality) / length(HIVid.df6.id2)

#30-day moratlity (from time of severe sepsis event) of HIV & severe sepsis patients (by definition 1)
HIVid.df1.df <- data.frame(HIVid.df1)
names(HIVid.df1.df) <- c('id')
HIVid.df1.mortality30 <- merge(HIVid.df1.df, df6.mortality30, by.x = 'id', by.y = 'id', all.x = T)
HIVid.df1.mortality30 <- HIVid.df1.mortality30[!is.na(HIVid.df1.mortality30$dod),]
head(HIVid.df1.mortality30)
HIVid.df1.mortality30.id <- c()
for (i in 1:nrow(HIVid.df1.mortality30)) 
  if (difftime(HIVid.df1.mortality30$dod[i], HIVid.df1.mortality30$time.event[i], units="hours") < 30*24) {
    HIVid.df1.mortality30.id <- c(HIVid.df1.mortality30.id, HIVid.df1.mortality30$id[i])
  }
length(HIVid.df1.mortality30.id)
length(HIVid.df1.mortality30.id)/length(HIVid.df1)

#30-day moratlity (from time of severe sepsis event) of HIV & severe sepsis patients (by definition 6)
HIVid.df6.df <- data.frame(HIVid.df6)
names(HIVid.df6.df) <- c('id')
HIVid.df6.mortality30 <- merge(HIVid.df6.df, df6.mortality30, by.x = 'id', by.y = 'id', all.x = T)
HIVid.df6.mortality30 <- HIVid.df6.mortality30[!is.na(HIVid.df6.mortality30$dod),]
head(HIVid.df6.mortality30)
HIVid.df6.mortality30.id <- c()
for (i in 1:nrow(HIVid.df6.mortality30)) 
  if (difftime(HIVid.df6.mortality30$dod[i], HIVid.df6.mortality30$time.event[i], units="hours") < 30*24) {
    HIVid.df6.mortality30.id <- c(HIVid.df6.mortality30.id, HIVid.df6.mortality30$id[i])
  }
length(HIVid.df6.mortality30.id)
length(HIVid.df6.mortality30.id)/length(HIVid.df6)


#=====================analyze the prevalence of infections in HIV & Sepsis patients=======================
HIVinfect <- read.csv('HIV_infections.csv', header = T)
HIVinfect$id <- paste(HIVinfect$subject_id, HIVinfect$hospital_seq, sep='#%#')
HIVinfect$code <- as.character(HIVinfect$code)
head(HIVinfect)

HIVinfect.hosp <- data.frame(HIVinfect$id, HIVinfect$code, HIVinfect$description)
names(HIVinfect.hosp) <- c('id', 'code', 'description')
length(unique(HIVinfect.hosp$code))
counts <- as.data.frame(table(HIVinfect.hosp$code))
counts.2 <- counts[order(counts$Freq, decreasing = T),]
barplot(counts.2$Freq, ylim = c(0,60), main = 'Prevelance of Infections among HIV Patients')
counts.2$rate <- counts.2$Freq / length(HIVicd9.id0)
HIVinfect.hosp[HIVinfect.hosp$code=='112.5',]



HIVid.df1.id2.df <- data.frame(HIVid.df1.id2)
names(HIVid.df1.id2.df) <- c('id')
HIVinfect.hosp.df1 <- merge(HIVid.df1.id2.df, HIVinfect.hosp, by.x = 'id', by.y = 'id', all.x = T)
counts.df1 <- as.data.frame(table(HIVinfect.hosp.df1$code))
counts.df1.2 <- counts.df1[order(counts.df1$Freq, decreasing = T),]
barplot(counts.df1.2$Freq, ylim = c(0,10),main = 'Prevelance of Infections among HIV & Severe Sepsis Patients (df1)')
counts.df1.2$rate <- counts.df1.2$Freq / length(HIVid.df1.id2)

HIVid.df2.id2.df <- data.frame(HIVid.df2.id2)
names(HIVid.df2.id2.df) <- c('id')
HIVinfect.hosp.df2 <- merge(HIVid.df2.id2.df, HIVinfect.hosp, by.x = 'id', by.y = 'id', all.x = T)
counts.df2 <- as.data.frame(table(HIVinfect.hosp.df2$code))
counts.df2.2 <- counts.df2[order(counts.df2$Freq, decreasing = T),]
barplot(counts.df2.2$Freq, ylim = c(0,40),main = 'Prevelance of Infections among HIV & Severe Sepsis Patients')
counts.df2.2$rate <- counts.df2.2$Freq / length(HIVid.df2.id2)


HIVid.df3.id2.df <- data.frame(HIVid.df3.id2)
names(HIVid.df3.id2.df) <- c('id')
HIVinfect.hosp.df3 <- merge(HIVid.df3.id2.df, HIVinfect.hosp, by.x = 'id', by.y = 'id', all.x = T)
counts.df3 <- as.data.frame(table(HIVinfect.hosp.df3$code))
counts.df3.2 <- counts.df3[order(counts.df3$Freq, decreasing = T),]
barplot(counts.df3.2$Freq, ylim = c(0,10),main = 'Prevelance of Infections among HIV & Severe Sepsis Patients (df3)')
counts.df3.2$rate <- counts.df3.2$Freq / length(HIVid.df3.id2)

HIVid.df4.id2.df <- data.frame(HIVid.df4.id2)
names(HIVid.df4.id2.df) <- c('id')
HIVinfect.hosp.df4 <- merge(HIVid.df4.id2.df, HIVinfect.hosp, by.x = 'id', by.y = 'id', all.x = T)
counts.df4 <- as.data.frame(table(HIVinfect.hosp.df4$code))
counts.df4.2 <- counts.df4[order(counts.df4$Freq, decreasing = T),]
barplot(counts.df4.2$Freq, ylim = c(0,10),main = 'Prevelance of Infections among HIV & Severe Sepsis Patients (df4)')
counts.df4.2$rate <- counts.df4.2$Freq / length(HIVid.df4.id2)

HIVid.df5.id2.df <- data.frame(HIVid.df5.id2)
names(HIVid.df5.id2.df) <- c('id')
HIVinfect.hosp.df5 <- merge(HIVid.df5.id2.df, HIVinfect.hosp, by.x = 'id', by.y = 'id', all.x = T)
counts.df5 <- as.data.frame(table(HIVinfect.hosp.df5$code))
counts.df5.2 <- counts.df5[order(counts.df5$Freq, decreasing = T),]
barplot(counts.df5.2$Freq, ylim = c(0,10),main = 'Prevelance of Infections among HIV & Severe Sepsis Patients (df5)')
counts.df5.2$rate <- counts.df5.2$Freq / length(HIVid.df5.id2)

HIVid.df6.id2.df <- data.frame(HIVid.df6.id2)
names(HIVid.df6.id2.df) <- c('id')
HIVinfect.hosp.df6 <- merge(HIVid.df6.id2.df, HIVinfect.hosp, by.x = 'id', by.y = 'id', all.x = T)
counts.df6 <- as.data.frame(table(HIVinfect.hosp.df6$code))
counts.df6.2 <- counts.df6[order(counts.df6$Freq, decreasing = T),]
barplot(counts.df6.2$Freq, ylim = c(0,10),main = 'Prevelance of Infections among HIV & Severe Sepsis Patients (df6)')
counts.df6.2$rate <- counts.df6.2$Freq / length(HIVid.df6.id2)
