# Download DSNY Monthly Tonnage Data.csv. This data set records the monthly collection tonnages
# that the Department of Sanitation collects from NYC residences and institutions. We will analyze this data set for this assignment. You can find further details about this data set, including
# descriptions for each column headings, at https://data.cityofnewyork.us/City-Government/
#  DSNY-Monthly-Tonnage-Data/ebb7-mvp5.


tbl <- read.csv("DSNY_Monthly_Tonnage_Data.csv")
View(tbl)

#1)What is the total amount of refuse collected (i.e. garabage) collected throughout NYC on 2018
# / 7 (i.e. July 2018) according to this data set?

sum(tbl$REFUSETONSCOLLECTED[(1:nrow(tbl))[tbl$MONTH == "2018 / 07"]])


myFamilyAges <- c(43, 42, 12, 8, 5)
myFamilyAges[2]




#2) NYC has five boroughs: Manhattan, Bronx Brooklyn, Queens and Staten Island. You can
# access boroughs by matching with the column BOROUGH or with BOROUGH ID. What is the average
# amount of refuse collected per month from each borough? (Hint: Look up na.rm for the mean
# function.)

#mean(tbl$REFUSETONSCOLLECTED[(1:nrow(tbl))[tbl$BOROUGH == "Manhattan"]], na.rm = TRUE)
#mean(tbl$REFUSETONSCOLLECTED[(1:nrow(tbl))[tbl$BOROUGH == "Brooklyn"]], na.rm = TRUE)
#mean(tbl$REFUSETONSCOLLECTED[(1:nrow(tbl))[tbl$BOROUGH == "Queens"]], na.rm = TRUE)
#mean(tbl$REFUSETONSCOLLECTED[(1:nrow(tbl))[tbl$BOROUGH == "Staten Island"]], na.rm = TRUE)
#mean(tbl$REFUSETONSCOLLECTED[(1:nrow(tbl))[tbl$BOROUGH == "Bronx"]], na.rm = TRUE)

boroughF <- factor(tbl$BOROUGH)
refCol <- tbl$REFUSETONSCOLLECTED
tapply_borough <- tapply(refCol, boroughF, mean, na.rm = TRUE)
tapply_borough





#3) NYC is divided up into 59 sanitation districts (indexed by COMMUNITYDISTRICT). A careful
# inspection of the data set will show that some districts do not record refuse collection data. Which
# sanitation districts have data recorded in this data set?

(1:nrow(tbl))[is.na(tbl$REFUSETONSCOLLECTED)]
unique(tbl$COMMUNITYDISTRICT)

unique(tbl$COMMUNITYDISTRICT[(1:nrow(tbl))[!is.na(tbl$REFUSETONSCOLLECTED)]]) # This is the answer. Above are for concluding purposes








#4) Using tapply, compute the average refuse collected for each sanitation district

sanF <- factor(tbl$COMMUNITYDISTRICT)
levels(sanF)
refCol <- tbl$REFUSETONSCOLLECTED

tapply(refCol, sanF, mean)

tapply_out <- tapply(refCol, sanF, mean) 
tapply_out



# tapply(tbl$REFUSETONSCOLLECTED, factor(tbl$COMMUNITYDISTRICT), mean)






#5) Produce a vector that explains why district 7A does not have an average refuse collected.
refCol7A <- c(tbl$REFUSETONSCOLLECTED[(1:nrow(tbl))[tbl$COMMUNITYDISTRICT == "7A"]])
refCol7A # We don't have an average because it is full of NA values
#class(refCol7A)









#6) The following code converts the output of tapply in #4 to a data frame and removes the row
# for district 7A
tapply_out <- tapply(refCol, sanF, mean) 
tapply_out

dist_refuse <- data.frame(COMMUNITYDISTRICT = names(tapply_out), 
                          AVGREFUSETONSCOLLECTED = tapply_out )

dist_refuse

dist_refuse <- dist_refuse[!(dist_refuse$COMMUNITYDISTRICT == "7A"),]
dist_refuse


# Using this approach, create a data frame that gives the average paper recycling and average metal,
# glass and plastic recycling for each sanitation district.

sanF <- factor(tbl$COMMUNITYDISTRICT)
levels(sanF)
paperCol <- tbl$PAPERTONSCOLLECTED
tapply_paper <- tapply(paperCol, sanF, mean, na.rm = TRUE)

dist_paper <- data.frame(COMMUNITYDISTRICT = names(tapply_paper),
                         AVGPAPERTONSCOLLECTED = tapply_paper)
dist_paper

dist_paper <- dist_paper[!(dist_paper$COMMUNITYDISTRICT == "7A"),]
dist_paper

mgpCol <- tbl$MGPTONSCOLLECTED
tapply_mgp <- tapply(mgpCol, sanF, mean, na.rm = TRUE)
tapply_mgp

dist_mgp <- data.frame(COMMUNITYDISTRICT = names(tapply_mgp),
                       AVGMGPTONSCOLLECTED = tapply_mgp)
dist_mgp

dist_mgp <- dist_mgp[!(dist_mgp$COMMUNITYDISTRICT == "7A"),]
dist_mgp








#7) Which sanitation districts, on average, have over 300 tons of paper recycling and less than 400 tons of metal,
# glass and plastic recycling?

# unique(tbl$COMMUNITYDISTRICT[(1:nrow(tbl))[(tbl$PAPERTONSCOLLECTED > 300) & (tbl$MGPTONSCOLLECTED < 400)]]) # Has an NA value

# unique(tbl$COMMUNITYDISTRICT[na.omit((1:nrow(tbl))[(tbl$PAPERTONSCOLLECTED > 300) & (tbl$MGPTONSCOLLECTED < 400)])])

dfnotna <- tbl[!is.na(tbl$PAPERTONSCOLLECTED) & !is.na(tbl$MGPTONSCOLLECTED),]

sanF <- factor(dfnotna$COMMUNITYDISTRICT)
paperCol <- dfnotna$PAPERTONSCOLLECTED
tapply_paper <- tapply(paperCol, sanF, mean)
tapply_paper

mgpCol <- dfnotna$MGPTONSCOLLECTED
tapply_mgp <- tapply(mgpCol, sanF, mean)
tapply_mgp


dist_recycle <- data.frame(COMMUNITYDISTRICT=names(tapply_paper),
                           AVGPAPERTONSCOLLECTED = tapply_paper,
                           AVGMGPTONSCOLLECTED = tapply_mgp)

dist_recycle

dist_recycle$COMMUNITYDISTRICT[(dist_recycle$AVGPAPERTONSCOLLECTED > 300) & (dist_recycle$AVGMGPTONSCOLLECTED < 400)]




#8) Which sanitation districts have less than 300 tons of paper recycling or less than 350 tons of
# metal, glass and plastic recycling?

# unique(tbl$COMMUNITYDISTRICT[na.omit((1:nrow(tbl))[(tbl$PAPERTONSCOLLECTED < 300) | (tbl$MGPTONSCOLLECTED < 350)])])

dist_recycle$COMMUNITYDISTRICT[(dist_recycle$AVGPAPERTONSCOLLECTED <300) | (dist_recycle$AVGPAPERTONSCOLLECTED < 350)]









#9) Identify the sanitation districts recycles more metals, plastics and glass than paper.

library(tidyverse)

dist_recycle %>% filter(AVGPAPERTONSCOLLECTED < AVGMGPTONSCOLLECTED) %>% 
  select(COMMUNITYDISTRICT)




# 10) Identify the sanitation district with the highest ratio of recycling tonnage (including paper
# and metal, glass and plastic) to refuse tonnage. (Hint: Do not add averages!)

sanF <- factor(tbl$COMMUNITYDISTRICT)
levels(sanF)

paperCol <- tbl$PAPERTONSCOLLECTED
mgpCol <- tbl$MGPTONSCOLLECTED

paperCol
mgpCol

recycled <- paperCol + mgpCol
recycled

refCol <- tbl$REFUSETONSCOLLECTED
refCol

ratio <- recycled/refCol
ratio

tapply_ratio <- tapply(ratio, sanF, mean, na.rm = TRUE)
tapply_ratio

ratio_df <- data.frame(COMMUNITYDISTRICT = names(tapply_ratio),
                       RATIO = tapply_ratio)
ratio_df

ratio_df <- ratio_df[!(ratio_df$COMMUNITYDISTRICT == "7A"),]
ratio_df

ratio_df[which.max(ratio_df$RATIO),]
