# preprocessing on IV fluids

# read the input and output item description table
ioitems <- read.csv('ioitems.csv', header = T)
summary(ioitems$category)
fluids <- ioitems[ioitems$category == 'IV Infusions',]
fluids.ids <- unique(fluids$itemid)


# read the sepsis patient table
fluidData <- read.csv('sepsisptsdf1_fliuds.csv', header = T)
names(fluidData)
fluidData0 <- fluidData[fluidData$itemid %in% fluids.ids,]
pt.fluid.ids <- unique(fluidData0$itemid)

# get the itemid and corresponding description of the IV fluids appeared among the selected patients
fluids.selected <- fluids[fluids$itemid %in% pt.fluid.ids, ]

write.table(fluids.selected, file = 'ivfluids.csv', row.names=F, sep = ',')
