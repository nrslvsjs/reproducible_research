library(WDI)
WDIsearch("fertilizer consumption")
FertConsumptionData <- WDI(indicator = "AG.CON.FERT.ZS")
head(FertConsumptionData)

dim(FertConsumptionData)
summary(FertConsumptionData)
summary(FertConsumptionData$AG.CON.FERT.ZS)

library(tidyr)
library(dplyr)

#reshaping data
#setting up wide data to gather
SpreadFert <- spread(FertConsumptionData, year, AG.CON.FERT.ZS)
SpreadFert <- arrange(SpreadFert, country)
SpreadFert <- select(.data = SpreadFert, c(1, 2, `2005`:`2011`))
names(SpreadFert)
head(SpreadFert[ , 1:5])
head(SpreadFert)
glimpse(SpreadFert)
ncol(SpreadFert)
GatheredFert <- gather(SpreadFert, Year, Fert, `2005`:`2011`)
head(GatheredFert)

#renaming variables
GatheredFert <- rename(GatheredFert,
                       year = Year,
                       FertilizerConsumption = Fert)
head(GatheredFert)

#ordering data
#order by subject-time
GatheredFert <- GatheredFert[order(GatheredFert$country,
                                   GatheredFert$year), ]
head(GatheredFert)
##dplyr  GatheredFert <- arrange(GatheredFert, country, year)

#subsetting data
#create outlier dataframe
FertOutliers <- subset(x = GatheredFert,
                       FertilizerConsumption > 1000)
head(FertOutliers)
#drop outliers from dataset
GatheredFertSub <- subset(GatheredFert, FertilizerConsumption <= 1000)
#drop Arab World type
GatheredFertSub <- subset(GatheredFertSub, country != "Arab World")
#remove missing values of FertilizerConsumption
GatheredFertSub <- subset(GatheredFertSub, !is.na(FertilizerConsumption))
summary(GatheredFertSub$FertilizerConsumption)

#recoding variables
GatheredFertSub$country[GatheredFertSub$country == "Korea, Rep."] <- "South Korea"

#create new variables from old
#create variable of natural log of FertilizerConsumption
GatheredFertSub$logFertConsumption <- log(GatheredFertSub$FertilizerConsumption)
summary(GatheredFertSub$logFertConsumption)
#R calculates log of zero as negative infinity -Inf
#recode zeros in FertilizerConsumption as small nonnegative number
GatheredFertSub$FertilizerConsumption[GatheredFertSub$FertilizerConsumption == 0] <- 0.001
#natural log transform FertilizerConsumption
GatheredFertSub$logFertConsumption <- log(
  GatheredFertSub$FertilizerConsumption
)
summary(GatheredFertSub$logFertConsumption)

#create factor variables
#create numeric factor levels variable
attach(GatheredFertSub)
GatheredFertSub$FertConsGroup[FertilizerConsumption < 18] <- 1
GatheredFertSub$FertConsGroup[FertilizerConsumption >= 18 &
                                FertilizerConsumption < 81] <- 2
GatheredFertSub$FertConsGroup[FertilizerConsumption >= 81 &
                                FertilizerConsumption < 158] <- 3
GatheredFertSub$FertConsGroup[FertilizerConsumption >= 158] <- 4
GatheredFertSub$FertConsGroup[is.na(FertilizerConsumption)] <- NA
detach(GatheredFertSub)
summary(GatheredFertSub$FertConsGroup)
FCLabels <- c("low", "medium low", "medium high", "high")
GatheredFertSub$FertConsGroup <- factor(GatheredFertSub$FertConsGroup,
                                        labels = FCLabels)
summary(GatheredFertSub$FertConsGroup)

#use cut command
FertFactor <- cut(GatheredFertSub$FertilizerConsumption,
                  breaks = c(-0.01, 17.99, 80.99, 157.99, 999.99),
                  labels = c("low", "medium low",
                             "medium high", "high"))
summary(FertFactor)

#merging datasets
#create two datasets to merge
library(RCurl)
UrlAddress <- paste0("https://raw.githubusercontent.com/",
                     "christophergandrud/Disproportionality",
                     "_Data/master/Disproportionality.csv")
DataUrl <- getURL(UrlAddress)
DispropData <- read.table(textConnection(DataUrl),
                          sep = ",", header = TRUE)
library(repmis)
FinDataFull <- source_data("fin_research_note.csv", "exh4iobbm2p5p1v",
                                          sep = ",", header = TRUE)

FinURL <- paste0("https://raw.githubusercontent.com/christophergandrud/Disproportionality_Data/master/Disproportionality.csv")

# Download data
FinDataFull <- repmis::source_data(FinURL,
                             sep = ",",
                             header = TRUE)
