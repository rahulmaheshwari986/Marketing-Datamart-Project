#--Load necesssary packages.
library(data.table)
library(dplyr)

#--Define file names. They must be in the same directory as the R script.
UserDailyAggregationFile <- "RawDataIIUserDailyAggregation.csv"
DemographicsFile <- "RawDataIDemographics.csv"
PokerTransactionsFile <- "RawDataIIIPokerChipConversions.csv"
SampleAnalyticDataFile <- "AnalyticDataInternetGambling.csv"

#--Read the csv files. The fread function from data.table package is supposed be fastest and most efficient.
#--For Demographics, we drop all the Firstplay date columns as we are going to recalculate this metric later .
#--From AnalyticsDataset, we only capture age column, since it is not present in any other raw data file.
UserDailyAgg <- fread(UserDailyAggregationFile, colClasses = c(Date = "character"))
Demographics <- fread(DemographicsFile, colClasses = c(FirstPay = "character"), drop = 6:11)
Poker <- fread(PokerTransactionsFile)
Age <- fread(SampleAnalyticDataFile, select = c(1, 5))
setnames(Age, c("UserID", "Age"))

#--Remove any duplicate entries inPoker table and convert 'TransDateTime' to Date object keeping just the date part.
setkey(Poker)
Poker <- unique(Poker)
Poker[, TransDateTime := as.Date(TransDateTime, "%m/%d/%Y")]

#--The following code processes the Poker table to make it structurally similar to UserAggregation table, 
#--and later rbinds the two.

#--Count the transactions of each type for each user per day and also add up the amounts.
Poker <- Poker[, .(TransAmount = sum(TransAmount), Bets = .N), by = .(UserID, TransDateTime, TransType)]

#--Segregate the 'Buy'and 'Sell' transactions into two tables.
#--Consider 'Buy' amounts as 'Stakes' and each buy transaction on a day as a bet.
Buy <- Poker[TransType == 24]
setnames(Buy, "TransAmount", "Stakes")
Sell <- Poker[TransType == 124, -5, with = FALSE]
setnames(Sell, "TransAmount", "Sell")
Buy[, TransType := NULL]
Sell[, TransType := NULL]

#--Full-outer join the two tables by User ID and Day columns to have the 'Buy' and 'Sell' amounts for each user
#--on each day in one row. Convert all NAs into 0 and calculate Winnings as 'Sell' - 'Buy'.
#--Add a ProductID column with the fixed value of 3 and rename and reorder all columns as per UserAggregation table.
#--Clean-up the temp tables to free the memory
Poker <- merge(Sell, Buy, by = c("UserID", "TransDateTime"), all = TRUE)
Poker[is.na(Sell), Sell := 0][is.na(Stakes), Stakes := 0][is.na(Bets), Bets := 0]
Poker[, Winnings := (Sell - Stakes)][, Sell := NULL][Winnings < 0, Winnings := 0][, ProductID := 3]
setnames(Poker, "TransDateTime", "Date")
setcolorder(Poker, c("UserID", "Date", "ProductID", "Stakes", "Winnings", "Bets"))
rm(Buy, Sell)

#--Convert 'Date' in UserAggregation table to a date object and rbind the Poker table entries.
#--This now makes one comprehensive transaction table covering all the product types for each user.
UserDailyAgg[, Date := as.Date(Date, "%Y%m%d")]
UserDailyAgg <- rbind(UserDailyAgg, Poker)

#--Convert 'RegDate' and 'FirstPay' in Demographics into Date objects and merge with UserAggregation to form
#--first-level DataCube. Apply the rules from codebook to filter out users and transactions.
Demographics[, `:=`(RegDate = as.Date(RegDate, "%m/%d/%Y"), FirstPay = as.Date(FirstPay, format="%Y%m%d"))]
Demographics[is.na(Gender), Gender := 0]

Datacube <- merge(Demographics, UserDailyAgg, by = "UserID")
Datacube <- Datacube[(RegDate >= "2005-02-01") & (RegDate <= "2005-02-27")] [Date >= FirstPay]

#--Calculate Total active days per product as well the overall active days across all products for each user.
#--Only consider days where any bets were placed for this calculation, in accordnace with sample Analytic dataset
Datacube[Bets != 0, TotalDaysActive := .N, by = .(UserID, ProductID)]
Datacube[Bets != 0, TotalActDays_Overall := length(unique(Date)), by = UserID]

#--Add up the Stakes, Winnings and Bets per product for each user and calculate the first and last active dates per product 
cols <- c("Stakes", "Winnings", "Bets")
Datacube[, eval(cols) := lapply(.SD, sum), by = .(UserID, ProductID), .SDcols = cols]
Datacube[, `:=`(FirstActiveDate = min(Date), LastActiveDate = max(Date)), by = .(UserID, ProductID)]

#--clean up the Datacube of leftout entries where 'Bets' were 0 and columns which are no longer needed.
#--Then de-duplicate to ensure one row per product for each user.
#--Calculate the overall first and last active dates for each user across all products 
Datacube <- Datacube[!is.na(TotalDaysActive)]
cols <- c("Date", "FirstPay")
Datacube[, eval(cols) := NULL]
setkey(Datacube, UserID, ProductID)
Datacube <- unique(Datacube)
Datacube[, `:=`(FirstActDate_Overall = min(FirstActiveDate), LastActDate_Overall = max(LastActiveDate)), by = UserID]

#--Round the 'Stakes' and 'Winnings' to whole numbers and the cleaned up DataCube is ready
cols <- c("Stakes", "Winnings")
Datacube[, eval(cols) := lapply(.SD, round), .SDcols = cols]

#--Now add the 'Age' column to the Datacube and clean-up the Age table as it is no longer needed.
Datacube <- merge(Age, Datacube, by = "UserID", all.y = TRUE)
Datacube[is.na(Age), Age := 0]
rm(Age)

#--Here we define user segments based on a combination of gender and typical human age groups.
#--For age groups, youth (upto 24), young adults (25-39), middle-aged adults (40-64) and old adults (at or above 65)
#--have been considered. Segments are also created for users with no Age information.
Datacube[(Age == 0) & (Gender == 1), UserSegment := "Unknown_Age M", by = UserID]
Datacube[(Age == 0) & (Gender == 0), UserSegment := "Unknown_Age F", by = UserID]
Datacube[(Age %in% 1:24) & (Gender == 1), UserSegment := "<=24 M", by = UserID]
Datacube[(Age %in% 1:24) & (Gender == 0), UserSegment := "<=24 F", by = UserID]
Datacube[(Age %in% 25:39) & (Gender == 1), UserSegment := "25-39 M", by = UserID]
Datacube[(Age %in% 25:39) & (Gender == 0), UserSegment := "25-39 F", by = UserID]
Datacube[(Age %in% 40:64) & (Gender == 1), UserSegment := "40-64 M", by = UserID]
Datacube[(Age %in% 40:64) & (Gender == 0), UserSegment := "40-64 F", by = UserID]
Datacube[(Age >= 65) & (Gender == 1), UserSegment := ">=65 M", by = UserID]
Datacube[(Age >= 65) & (Gender == 0), UserSegment := ">=65 F", by = UserID]

#--Next we form a User Segment matrix with the segments ranked as per their calculated profitability.
UserSegmentMatrix <- Datacube[, .("Profitability(euros)" = sum(Stakes - Winnings)), by = UserSegment]
UserSegmentMatrix <- UserSegmentMatrix %>% mutate(Rank = rank(-`Profitability(euros)`)) %>% arrange(Rank)

#--Left Join the matrix with the Datacube to assign the ranks to the users.
#--Drop the columns no longer needed and rearrange the columns. Also set the key to UserID to have the table sorted.
Datacube <- merge(Datacube, UserSegmentMatrix, by = "UserSegment", all.x = TRUE)
Datacube[, c("UserSegment", "Profitability(euros)", "Age", "Gender") := NULL]
cols <- c("UserID", "Country", "Language", "RegDate", "FirstActDate_Overall", 
          "LastActDate_Overall", "TotalActDays_Overall", "Rank", "ProductID",
          "FirstActiveDate", "LastActiveDate", "TotalDaysActive", "Stakes", "Winnings", "Bets")
setcolorder(Datacube, cols)
setnames(Datacube, "Rank", "Ranked Segment")
setkey(Datacube, UserID)

#--Calculate product-level metrics in the Datacube.
Datacube[Stakes != 0, "B/D ratio" := round(Bets/Stakes, 2), by = .(UserID,ProductID)][Stakes == 0, "B/D ratio" := NA]
Datacube[Stakes != 0, "Profit Margin" := paste0(round(100*((Stakes-Winnings)/Stakes), 1), "%"), 
         by = .(UserID,ProductID)][Stakes == 0, "Profit Margin" := NA]
Datacube[, "Playing Frequency" := round(TotalDaysActive / (as.numeric(LastActiveDate - FirstActiveDate) + 1), 2), by = .(UserID,ProductID)]

#--Initiate the Datamart by putting the demographic information along with the Ranked sement from the Datacube.
Datamart <- Datacube[, 1:8, with = FALSE]
Datamart <- unique(Datamart)

#--Here we calculate global marketing metrics from the Datacube which are per user only. This involves 
#--extracting information from Datacube to form multiple 2-column tables that can be later merged to the Datamart.

#--First is the lag between the registratoin date and the first playing date.
FirstActivityLag <- Datacube[, .("First Activity Lag (days)" = as.numeric(FirstActDate_Overall - RegDate)), by = UserID]
FirstActivityLag <- unique(FirstActivityLag)

#--Second is the total number of products played by the user.
ProductCount <- Datacube[, .("Total Products Played" = length(ProductID)), by = UserID]

#--Third is the overall playing frequency of the user.
PlayingFrequency <- Datacube[, .("Overall Playing Frequency" = 
                                   round(TotalActDays_Overall / (as.numeric(LastActDate_Overall - FirstActDate_Overall) + 1), 2)), by = UserID]
PlayingFrequency <- unique(PlayingFrequency)

#--Fourth is the favorite product of each user.
#--NOTE: This code is taking time to execute.
FavoriteProduct <- Datacube[, .SD[`Playing Frequency` == max(`Playing Frequency`), .("Favorite Product" = ProductID)], 
                                   by = UserID][list(unique(UserID)), mult = "first"]

#--Fifth is the overall prodit margin for the company for each user. All values have been formatted logically.
UserProfitMargin <- Datacube[, .(Stakes = sum(Stakes), Winnings = sum(Winnings)), by = UserID]
UserProfitMargin[Stakes != 0, Margin := (Stakes - Winnings)/Stakes]
UserProfitMargin[, `:=`("Overall Stakes" = paste("???", Stakes), 
                        "Overall Winnings" = paste("???", Winnings))][Stakes != 0, `:=`("Overall Profit Margin" = paste0(round(100*Margin, 1), "%"))]
UserProfitMargin[Stakes == 0, "Overall Profit Margin" := NA]

UserProfitMargin <- UserProfitMargin[, -c(2:4), with = FALSE]

#--Sixth is the approximate Lifetime Value of each user.
LifetimeValue <- Datacube[, .("Rev per bet" = sum(Stakes-Winnings)/sum(Bets), 
                              "Bets per day" = sum(Bets)/mean(TotalActDays_Overall),
                              "Retention Time" = mean(LastActDate_Overall - FirstActDate_Overall + 1)), 
                          by = UserID]
LifetimeValue[, "Lifetime Value (Indicative)" := paste("???", round(`Rev per bet`*`Bets per day`*`Retention Time`))]
LifetimeValue <- LifetimeValue[, -c(2:4), with = FALSE]

#--Merge all the per userID global metric tables with the Datamart and clean-up the memory.
Datamart <- Reduce(merge, list(Datamart, FirstActivityLag, ProductCount, PlayingFrequency, FavoriteProduct, 
                               UserProfitMargin, LifetimeValue))
rm(FirstActivityLag, ProductCount, PlayingFrequency, FavoriteProduct, UserProfitMargin, LifetimeValue)

#--Here we pick up the Product-wise metrics calculated earlier to put them into separate columns such that the 
#--Datmart has only one row representing each UserID. The code is ProductID agnostic for maintainability.
ProductIDs <- sort(Datacube[, unique(ProductID)])

for(i in ProductIDs) {
  assign(paste0("Prod", i), 
         Datacube[ProductID == i, .(UserID = UserID, `B/D ratio`, `Profit Margin`, `Playing Frequency`)])
  
  setnames(get(paste0("Prod", i)), c("UserID", paste0("P", i, " B/D Ratio"), paste0("P", i, " Profit Margin"),
                                     paste0("P", i, " Playing Frequency")))  

  Datamart <- merge(Datamart, get(paste0("Prod", i)), by = "UserID", all.x = TRUE)
  rm(list = paste0("Prod", i))
}

#--Finally, write the completed Datamart table as well as the segment matrix into csv files in the same folder.
write.csv(UserSegmentMatrix, "UserSegmentMatrix.csv", row.names = FALSE)
write.csv(Datamart, "Datamart.csv", row.names = FALSE, na = "0")

#--The following version of the csv is created for use by the R markdown report code for ease of handiling NA values.
#--Uncomment and run only if it is required to run the Report.rmd again to generate the report.
write.csv(Datamart, "Datamart_temp.csv", row.names = FALSE, na = "NA")