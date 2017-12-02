
rm(list = ls())

setwd("C:/Users/cyril.michel/Desktop/Delta Routing Winter Run")

library(RMark)
library(Hmisc)
library(msm)

## Load detections
detections <- read.csv("JSATS_Filter_Winter_H_2015_20170825_1348.csv")

## Load tag metadata

tagging_meta <- read.csv("Tagging_Fish_Metadata.txt", stringsAsFactors = F)
tagging_meta$Rel_datetime <- as.POSIXct(tagging_meta$Rel_datetime, tz="Etc/GMT+8", format = "%m/%d/%Y %H:%M:%S")

## Reformat dates, make sure we have hours/mins/secs
detections$DetectDate <- as.POSIXct(detections$DT*(60*60*24), origin="1899-12-30", tz="Etc/GMT+8")

## Change GEN to no longer be a factor, to allow some changes
detections$GEN <- as.character(detections$GEN)

## Now, estimate the lowest point in the river each fish went
min.riverkm <- merge(aggregate(detections$RKM, by=list(detections$FishID), min), tagging_meta[,c("FishID", "StudyID")], by.x="Group.1", by.y="FishID")

## Now remove any fish that didn't get to riverkm 170 (I80_Br), or lower
remove.fish <- min.riverkm[which((min.riverkm$x > 171 | is.na(min.riverkm$x))), "Group.1"]

## Now remove them from the detections file
detections <- detections[!detections$FishID %in% remove.fish,]

## Now reset FishID as a factor here so that fish removed by the next step (i.e. weren't detected at any of the 5 detection locations)
detections$FishID <- factor(detections$FishID)

## Remove detections at locations that we aren't interested in (i.e. upstream of riverkm 155)
detects <- detections[detections$GenRKM < 155,]

## Find out how often single location detects occur, I'd like to put a single detection filter on but worry that 2011 fish will suffer since detection efficieny was so poor that year
detects$FishID_Genloc <- paste(detects$FishID,detects$GEN, sep = "")
detect_count <- aggregate(detects$FishID, by=list(detects$FishID_Genloc), FUN=length)
detect_count[detect_count$x == 1,]

## Remove certain locations not needed for this routing analysis
remove <- c("RioVistaBr", "Blw_Sutter", "SouthofBW", "MiddleRiver", "Freeport", "Above_Hood", "YB_LibertyIsBase", "MinorBlwSutter")
detects <- detects[!detects$GEN %in% remove,]

## Combine certain locations into combos
detects$GEN_COMBO <- detects$GEN
detects[detects$GEN == "GoldenGateE", "GEN_COMBO"] <- "GG"
detects[detects$GEN == "GoldenGateW", "GEN_COMBO"] <- "GG"
detects[detects$GEN == "Hwy84Ferry", "GEN_COMBO"] <- "BaseMinerSutSteam"
detects[detects$GEN == "BaseSutterSteam", "GEN_COMBO"] <- "BaseMinerSutSteam"
detects[detects$GEN == "SteamboatSlough", "GEN_COMBO"] <- "SutSteam"
detects[detects$GEN == "SutterSlough", "GEN_COMBO"] <- "SutSteam"
detects[detects$GEN == "SutterSteamJunc", "GEN_COMBO"] <- "MidMinerSutSteam"
detects[detects$GEN == "MinerNorth", "GEN_COMBO"] <- "MidMinerSutSteam"

## Look for upstream movements, these should likely be removed since they mess with routing if fish enters different route going upstream
## First, sort data by fish and then chronologically
detects <- detects[(order(detects$FishID,detects$DetectDate)),]

## Create coding column, prefilled with "1"s

detects$prev_GEN_COMBO <- NA

## Now make an ifelse for loop to find GEN transitions
for (i in 2:length(detects$GEN_COMBO)) {
  if(detects$FishID[i]==detects$FishID[i-1] && detects$GEN_COMBO[i]!=detects$GEN_COMBO[i-1])
#        {detects$mov_event[i] <- detects$GenRKM[i-1] - detects$GenRKM[i]}
         {detects$prev_GEN_COMBO[i] <- as.character(detects$GEN_COMBO[i-1])}
  if(detects$FishID[i]==detects$FishID[i-1] && detects$GEN_COMBO[i]==detects$GEN_COMBO[i-1])
#        {detects$mov_event[i] <- detects$mov_event[i-1]}
         {detects$prev_GEN_COMBO[i] <- detects$prev_GEN_COMBO[i-1]}
  ## Put in a counter to keep track of progress, it's slow
  if(i%%1000==0){cat(i, "\n")}
}


## From looking at mov_event < 0 from below, we've determined problem areas
## There are two problem areas, fish going from BaseMinerSutSteam to Sr_Mouth, and vice-versa, and fish going from Antioch to Decker, and vice-versa
detects <- detects[-which(detects$GEN_COMBO == "AntiochBridge" & detects$prev_GEN_COMBO == "DeckerIsland"),]
detects <- detects[-which(detects$GEN_COMBO == "DeckerIsland" & detects$prev_GEN_COMBO == "AntiochBridge"),]
detects <- detects[-which(detects$GEN_COMBO == "BaseMinerSutSteam" & detects$prev_GEN_COMBO == "SR_Mouth"),]
detects <- detects[-which(detects$GEN_COMBO == "SR_Mouth" & detects$prev_GEN_COMBO == "BaseMinerSutSteam"),]
detects <- detects[-which(detects$GEN_COMBO == "BaseMinerSutSteam" & detects$prev_GEN_COMBO == "DeckerIsland"),]
detects <- detects[-which(detects$GEN_COMBO == "BaseMinerSutSteam" & detects$prev_GEN_COMBO == "GeorgeMok"),]

## Now make an ifelse for loop to estimate riverkm movement
detects$mov_event <- 0

for (i in 2:length(detects$mov_event)) {
  if(detects$FishID[i]==detects$FishID[i-1] && detects$GEN_COMBO[i]!=detects$GEN_COMBO[i-1])
        {detects$mov_event[i] <- detects$GenRKM[i-1] - detects$GenRKM[i]}
  if(detects$FishID[i]==detects$FishID[i-1] && detects$GEN_COMBO[i]==detects$GEN_COMBO[i-1])
        {detects$mov_event[i] <- detects$mov_event[i-1]}
  ## Put in a counter to keep track of progress, it's slow
  if(i%%1000==0){cat(i, "\n")}
}


## Make a crosstab query with frequencies for all tag/location combination
detects$GEN_COMBO <- factor(detects$GEN_COMBO)
mytable <- table(detects$FishID,detects$GEN_COMBO) # A will be rows, B will be columns

## Change all frequencies bigger than 1 to 1. Here you could change your minimum cutoff to 2 detections, and then make another command that changes all detections=1 to 0
mytable[mytable>0] <- 1

mytable2 <- mytable[, c("Hood", "SutSteam", "Blw_Steamboat", "Georgiana_Slough", "BlwGeorgiana", "MidMinerSutSteam", "SR_Mouth", "BaseMinerSutSteam", "GeorgeMok", "DeckerIsland", "AntiochBridge", "Benicia", "GG")]
   
## Now sort the crosstab rows alphabetically
mytable2 <- mytable2[order(row.names(mytable2)),]

mytable2[which(mytable2[, "Hood"]=="1"), "Hood"] <- "A"
mytable2[which(mytable2[, "Blw_Steamboat"]=="1"), "Blw_Steamboat"] <- "A"
mytable2[which(mytable2[, "BlwGeorgiana"]=="1"), "BlwGeorgiana"] <- "A"
mytable2[which(mytable2[, "SR_Mouth"]=="1"), "SR_Mouth"] <- "A"
mytable2[which(mytable2[, "DeckerIsland"]=="1"), "DeckerIsland"] <- "A"
mytable2[which(mytable2[,"SutSteam"]=="1"), "SutSteam"] <- "B"
mytable2[which(mytable2[, "MidMinerSutSteam"]=="1"), "MidMinerSutSteam"] <- "B"
mytable2[which(mytable2[, "BaseMinerSutSteam"]=="1"), "BaseMinerSutSteam"] <- "B"
mytable2[which(mytable2[, "Georgiana_Slough"]=="1"), "Georgiana_Slough"] <- "C"
mytable2[which(mytable2[, "GeorgeMok"]=="1"), "GeorgeMok"] <- "C"
mytable2[which(mytable2[, "AntiochBridge"]=="1"), "AntiochBridge"] <- "C"
mytable2[which(mytable2[, "Benicia"]=="1"), "Benicia"] <- "A"
mytable2[which(mytable2[, "GG"]=="1"), "GG"] <- "A"

## Now subset the tagging meta to be just the fish we are concerned with
tagging_meta <- tagging_meta[tagging_meta$FishID %in% row.names(mytable2),]
tagging_meta <- tagging_meta[order(tagging_meta$FishID),]

## Paste together (concatenate) the data from each column of the crosstab into one string per row, add to tagging_meta.
## For this step, make sure both are sorted by FishID
tagging_meta$inp_part1 <- mytable2[,1]
tagging_meta$inp_partA <- apply(mytable2[,c(3,5,7,10)],1,paste,collapse="")
tagging_meta$inp_partB <- apply(mytable2[,c(2,6,8,10)],1,paste,collapse="")
tagging_meta$inp_partC <- apply(mytable2[,c(3,4,9,11)],1,paste,collapse="")
tagging_meta$inp_part3 <- mytable2[,12:13]

## Now look at all the unique combinations of route A/B/C per fish to find out issues
tagging_meta$all_parts <- paste(tagging_meta$inp_partA, tagging_meta$inp_partB, tagging_meta$inp_partC, sep = "-")
sort(table(tagging_meta$all_parts),decreasing = T)

## Some of these rarer capture histories involve route jumping in odd places (likely because of upstream migrations, several missed detections, and/or three-mile slough)
## These should be mostly addressed by choosing which routes' encounter history below
## But first, let's examine the detection history of some of these to determine likely route choice
View(detects[detects$FishID == tagging_meta[tagging_meta$all_parts == "0A0A-00BA-0000", "FishID"],]) ## Assign to route A
View(detects[detects$FishID == tagging_meta[tagging_meta$all_parts == "A00A-000A-ACC0", "FishID"],]) ## Assign to C
View(detects[detects$FishID == tagging_meta[tagging_meta$all_parts == "AA00-00B0-A000", "FishID"],]) ## Assign to A
View(detects[detects$FishID == tagging_meta[tagging_meta$all_parts == "AA0A-000A-ACC0", "FishID"],]) ## Assign to C
View(detects[detects$FishID == tagging_meta[tagging_meta$all_parts == "AA0A-00BA-A000", "FishID"],]) ## Assign to A

## Now concatenate the correct combinations, depending on route use
## first, assume route A is the default
tagging_meta$inp <- apply(tagging_meta[, c("inp_part1", "inp_partA", "inp_part3")], 1, paste, collapse = "")
## The select cases where route B is the route
tagging_meta[tagging_meta$inp_partB %in% c("BBBA", "B0B0", "BB00", "B0BA", "BBB0", "B000", "00B0", "00BA", "0BBA", "BB0A", "B00A"), "inp"] <- apply(tagging_meta[tagging_meta$inp_partB %in% c("BBBA", "B0B0", "BB00", "B0BA", "BBB0", "B000", "00B0", "00BA", "0BBA", "BB0A", "B00A"), c("inp_part1", "inp_partB", "inp_part3")], 1, paste, collapse = "")
## And route C. 
tagging_meta[tagging_meta$inp_partC %in% c("0CC0", "ACC0", "ACCC", "0CCC", "0C00", "0C0C", "AC00"), "inp"] <- apply(tagging_meta[tagging_meta$inp_partC %in% c("0CC0", "ACC0", "ACCC", "0CCC", "0C00", "0C0C", "AC00"), c("inp_part1", "inp_partC", "inp_part3")], 1, paste, collapse = "")
## Finally, in those rare situations mentioned above, detection histories were manually looked at and route was determined
tagging_meta[tagging_meta$all_parts == "0A0A-00BA-0000", "inp"] <- apply(tagging_meta[tagging_meta$all_parts == "0A0A-00BA-0000", c("inp_part1", "inp_partA", "inp_part3")], 1, paste, collapse = "")
tagging_meta[tagging_meta$all_parts == "A00A-000A-ACC0", "inp"] <- apply(tagging_meta[tagging_meta$all_parts == "A00A-000A-ACC0", c("inp_part1", "inp_partC", "inp_part3")], 1, paste, collapse = "")
tagging_meta[tagging_meta$all_parts == "AA00-00B0-A000", "inp"] <- apply(tagging_meta[tagging_meta$all_parts == "AA00-00B0-A000", c("inp_part1", "inp_partA", "inp_part3")], 1, paste, collapse = "")
tagging_meta[tagging_meta$all_parts == "AA0A-000A-ACC0", "inp"] <- apply(tagging_meta[tagging_meta$all_parts == "AA0A-000A-ACC0", c("inp_part1", "inp_partC", "inp_part3")], 1, paste, collapse = "")
tagging_meta[tagging_meta$all_parts == "AA0A-00BA-A000", "inp"] <- apply(tagging_meta[tagging_meta$all_parts == "AA0A-00BA-A000", c("inp_part1", "inp_partA", "inp_part3")], 1, paste, collapse = "")


## Now look at all unique capture histories
ch <- aggregate(tagging_meta$FishID, by=list(tagging_meta$inp), FUN= length)
ch <- ch[order(ch$x, decreasing = TRUE),]

## Paste the above with the tag # and appropriate punctuation and formatting. Add 1 to beginning for I80Br release
final_inp <- paste("/*",rownames(mytable2),"*/ ","A",tagging_meta$inp, " 1;", sep = "")

## Write it to an .inp file, make sure to have no quotes, row.names, or col.names so that it is ready to go straight into Mark or RMark
write.table(final_inp,"mark_file.inp", row.names = FALSE, col.names = FALSE, quote = FALSE)


### Now let's try this in RMark
src <- convert.inp("mark_file.inp")
dp=process.data(src,model="Multistrata")
ddl=make.design.data(dp)


#           1A                   (release at I80Br)
#           |                    Survival reach 1
#           2A                  (Hood detections)
#         /   \                  Survival reach 2 
#       3A     3B            (A = detection at BlwSteam, B = detection at Sutter+Steam)
#      /  \    |                 Survival reach 3
#     4C  4A   4B          (A = detection at BlwGeorgiana, B = detection at SutterSteamJ + MinerNorth, C = detection at Georgiana Sl)
#      |   |   |                 Survival reach 4
#     5C  5A   5B          (A = detection at SR_Mouth, B = detection at Hwy84 + BaseSutSteam, C = detection at GeorgMok)
#      |   \  /                  Survival reach 5
#     6C    6A              (A = detection at Decker, c = detection at Antioch)
#       \  /                    Survival reach 6
#        7A                   (Detection at Benicia)
#        |                      Survival reach 7
#        8A                   (Detection at GG)

#### p ####
# Can't be seen at 2B, 2C, 3C, 6B, 7B, 7C, 8B, 8C
ddl$p$fix=NA
ddl$p$fix[ddl$p$stratum == "B" & ddl$p$time %in% c(2,6,7,8)]=0
ddl$p$fix[ddl$p$stratum == "C" & ddl$p$time %in% c(2,3,7,8)]=0

#### Psi ####
# Only 4 transitions allowed, from A to B at time interval 2 to 3, from A to C at time interval 3 to 4,
# from B to A at time interval 5 to 6, and from C to A at time interval 6 to 7
# Rest are fixed values
ddl$Psi$fix=0
# A to B can only happen for interval 2-3
ddl$Psi$fix[ddl$Psi$stratum=="A"&
                     ddl$Psi$tostratum=="B" & ddl$Psi$time==2]=NA
# A to C can only happen for interval 3-4
ddl$Psi$fix[ddl$Psi$stratum=="A"&
              ddl$Psi$tostratum=="C" & ddl$Psi$time==3]=NA
# Force B to go to A for interval 5-6, i.e. set to 1
ddl$Psi$fix[ddl$Psi$stratum=="B"&ddl$Psi$tostratum=="A"&
                     ddl$Psi$time==5]=1
# Force C to go to A for interval 6-7, i.e. set to 1
ddl$Psi$fix[ddl$Psi$stratum=="C"&ddl$Psi$tostratum=="A"&
              ddl$Psi$time==6]=1

#### Phi a.k.a. S ####
ddl$S$fix=NA
# None in B for reaches 1,2,6 and 7 so fixing S to 1
ddl$S$fix[ddl$S$stratum=="B" & ddl$S$time %in% c(1,2,6,7)]=1
# None in C for reaches 1,2,3 and 7 so fixing S to 1
ddl$S$fix[ddl$S$stratum=="C" & ddl$S$time %in% c(1,2,3,7)]=1

# fit model
#p.timexstratum.tag=list(formula=~-1+stratum*time)
#Psi.stratumxtime=list(formula=~-1+stratum*time)
#S.stratumxtime=list(formula=~-1+stratum*time)

p.timexstratum=list(formula=~-1+stratum:time)
Psi.stratumxtime=list(formula=~-1+stratum:time)
S.stratumxtime=list(formula=~-1+stratum:time)
#
S.timexstratum.p.timexstratum.Psi.timexstratum=mark(dp,ddl, model.parameters=list(S=S.stratumxtime,p= p.timexstratum,Psi=Psi.stratumxtime))

write.csv(S.timexstratum.p.timexstratum.Psi.timexstratum$results$real, "estimates.csv")
