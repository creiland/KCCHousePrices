require(dplyr)
require (tidyr)


options(stringsAsFactors = FALSE)

setwd("C:/Users/creil/Desktop/Info370/Final")

data <- read.csv("data/kc_house_data.csv")

income <- read.csv("data/ACS_16_5YR_S1901_with_ann.csv")

#take relevant columns from income data
income_clean <- select(income, GEO.id2, HC01_EST_VC13, HC01_EST_VC15)

#clean column names
colnames(income_clean) <- c("zipcode", "median_income", "mean_income")

#remove row 1
income_clean <- income_clean[-c(1),]


factorToNum <- function (f){
  return(as.numeric(levels(f))[f])
}

income_clean$zipcode <- factorToNum(income_clean$zipcode)
income_clean$mean_income <- factorToNum(income_clean$mean_income)
income_clean$median_income <- factorToNum(income_clean$median_income)

joined <- left_join(data, income_clean, by="zipcode")

colnames(joined)[22:23] <- paste(colnames(joined)[22:23], "_by_zip", sep="")

write.csv(joined, file = "output/withincome.csv")


