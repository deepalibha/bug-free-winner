#load librarues
library (tidyr)
library (dplyr)
library(stringr)
library(psych)



#Import .csv file as  a data frame
titanic <- read.csv("C:/Users/deepa/OneDrive/Documents/titanic_original.csv",header = TRUE, sep = ",", quote = "\"",blank.lines.skip = TRUE, strip.white = TRUE,na.strings=c("NA","NaN", ""), stringsAsFactors = FALSE, skipNul = TRUE)

titanic <- tbl_df(titanic)
head(titanic)

#remove rows with all columns NA
titanic <- titanic[!(rowSums(is.na(titanic)) == length(colnames(titanic))),]

#CHECKING NA with embarked column and replace with S
dplyr::filter(titanic, is.na(embarked))
titanic$embarked[is.na(titanic$embarked)] <- 'S'
dplyr::filter(titanic, is.na(embarked))

#check missing values in age column and replace it with mean
dplyr::filter(titanic, is.na(age))
titanic$age[is.na(titanic$age)] <- mean(titanic$age, na.rm = TRUE)
dplyr::filter(titanic, is.na(age))

#checkin to see NA in boat column and replace with "None"
dplyr::filter(titanic, is.na(boat))
titanic$boat[is.na(titanic$boat)] <- 'None'
dplyr::filter(titanic, is.na(boat))

#unsure of filling in missing cabin number hence creating binary column for cabin number
titanic <- titanic %>% rowwise() %>% mutate(has_cabin_number = ifelse(is.na(cabin),0,1))

#write final csv
write.csv(titanic, "C:/Users/deepa/OneDrive/Documents/titanic_clean.csv", quote = TRUE, row.names = FALSE)