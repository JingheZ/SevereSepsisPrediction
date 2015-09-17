# 1. co-clustering to show what antibiotics are given to same patient (do not consider time order)
# a. convert to pivot table 
# b. perform co-clustering using the blockcluster package in r
# c. plot the abx clusters by color and pt types (lactate cleared or not) by data point shape
# 
# 2. frequent sequential pattern mining to show what antibiotics are given to same patient (consider time order)
# a. convert to the correct data format
# b. perform sequential pattern mining using the arulesSequences package in r
# c. try to plot or list the learned patterns

#=======================co-clucstering==========================================================
install.packages('blockcluster')
library(blockcluster)
library(reshape2)


str(binarydata)
head(binarydata)
out<-cocluster(binarydata,datatype="binary",nbcocluster=c(2,3))
summary(out)
plot(out)

#patients with their antibiotics
antibiotics5a.count.noNA <- antibiotics5a.count[!is.na(antibiotics5a.count$itemidG),-3]
names(antibiotics5a.count.noNA)
antibiotics5a.count.noNA$value <- 1

antibiotics5a.count.noNA.pivot <- dcast(antibiotics5a.count.noNA, id ~ itemidG, value.var = 'value', fill = 0)
str(antibiotics5a.count.noNA.pivot)
antibiotics5a.count.noNA.matrix <- data.matrix(antibiotics5a.count.noNA.pivot[,-1])

#perform co-clustering 
out<-cocluster(t(antibiotics5a.count.noNA.matrix), datatype="binary", nbcocluster=c(3,1))
# summary(out)
plot(out)

#patients without their antibiotics
pt.noABX <- data.frame(pts.0ab) #491
names(pt.noABX) <- c('id')
pt.noABX$itemidG <- NA
pt.noABX$value <- 0


#combine patients and their antibiotics to get a whole list of patients
pts.abx <- rbind(antibiotics5a.count.noNA, pt.noABX)

#create a pivot of patients with antibiotics as the columns (binary)
antibiotics5a.count.pivot <- dcast(pts.abx, id ~ itemidG, value.var = 'value', fill = 0)
antibiotics5a.count.pivot <- antibiotics5a.count.pivot[, -ncol(antibiotics5a.count.pivot)]
str(antibiotics5a.count.pivot)
antibiotics5a.count.matrix <- data.matrix(antibiotics5a.count.pivot[,-1])

#perform co-clustering 
out<-cocluster(antibiotics5a.count.matrix, datatype="binary", nbcocluster=c(2,5))
summary(out)
plot(out)



#========================association rule mining for aantibiotics=====================
install.packages('arules')
library(arules)
load('titanic.raw.rdata')
rules <- apriori(antibiotics5a.count.matrix, parameter = list(supp = 0.1, conf = 0.05, target = "rules"))
summary(rules)
inspect(rules)

#get the frequent abx sets among patients given antibiotics
freq_itemsets <- apriori(antibiotics5a.count.noNA.matrix, parameter = list(supp = 0.01, conf = 1, target = "frequent itemsets")) #conf is not considered when looking for frequent itemsets
freq_itemsets_sorted <- sort(freq_itemsets, decreasing = TRUE, na.last = NA,
     by = "support")
inspect(freq_itemsets_sorted)



# freq_itemsets_max <- apriori(antibiotics5a.count.noNA.matrix, parameter = list(supp = 0.01, conf = 1, target = "maximally frequent itemsets")) #conf is not considered when looking for frequent itemsets
# freq_itemsets_max_sorted <- sort(freq_itemsets_max, decreasing = TRUE, na.last = NA,
#                              by = "support")
# inspect(freq_itemsets_max_sorted)


#patients with antibiotics and 10% lactate clearance
cleared.ids <- lactateclearanceB$id[lactateclearanceB$censor == 1]

#antibiotics of patients who cleared the lactate by 10%
antibiotics5a.count.noNA.pivot.cleared <- antibiotics5a.count.noNA.pivot[antibiotics5a.count.noNA.pivot$id %in% cleared.ids,]
antibiotics5a.count.noNA.cleared.matrix <- data.matrix(antibiotics5a.count.noNA.pivot.cleared[,-1])

#get the frequent abx sets among patients given antibiotics and cleared their lactate by 10%
freq_itemsets.cleared <- apriori(antibiotics5a.count.noNA.cleared.matrix, parameter = list(supp = 0.01, conf = 1, target = "frequent itemsets")) #conf is not considered when looking for frequent itemsets
freq_itemsets_sorted.cleared <- sort(freq_itemsets.cleared, decreasing = TRUE, na.last = NA,
                             by = "support")
inspect(freq_itemsets_sorted.cleared)

#antibiotics of patients who did not cleare the lactate by 10%
antibiotics5a.count.noNA.pivot.nocleared <- antibiotics5a.count.noNA.pivot[!(antibiotics5a.count.noNA.pivot$id %in% cleared.ids),]
antibiotics5a.count.noNA.nocleared.matrix <- data.matrix(antibiotics5a.count.noNA.pivot.nocleared[,-1])

#get the frequent abx sets among patients given antibiotics and cleared their lactate by 10%
freq_itemsets.nocleared <- apriori(antibiotics5a.count.noNA.nocleared.matrix, parameter = list(supp = 0.02, conf = 1, target = "frequent itemsets")) #conf is not considered when looking for frequent itemsets
freq_itemsets_sorted.nocleared <- sort(freq_itemsets.nocleared, decreasing = TRUE, na.last = NA,
                                     by = "support")
inspect(freq_itemsets_sorted.nocleared)
