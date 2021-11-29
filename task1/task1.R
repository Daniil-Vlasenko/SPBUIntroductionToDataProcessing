if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyr")) install.packages("dplyr")
library("lubridate")
library("dplyr")

# 1.
# Create a dateframe of self-employed.
df <- read.table("study_fall2021_intror_creditcard.txt", header=TRUE, sep=";")
df <- filter(df, selfemp == "yes")
# Convert amount of months applicant living at his/her current address to days.
today <- as.Date("2000-01-01")
df$months <- as.numeric(today - (today %m-% months(df$months)))
names(df)[names(df) == "months"] <- "days"
# Add column with income per family member.
# Due to the lack of information about a person's family, 
# we assume that dependents are family members without the applicant.
df <- mutate(df, incomePerFamilyMember = income / (dependents + 1))
# Define criterion of income level, and split data according to levels of this criterion.
incomeLevel <- function(x){
  if (x < 2.5) "low"
  else if (x <= 8) "average"
  else "high"
}
df$incomeLevel <- sapply(df$income, FUN =  incomeLevel)
# 2.
# Print some information about dataset.
file.create("output.txt", showWarnings = TRUE)
output <-file("output.txt")
write("Average amount of dependents for people don't own their home:", file="output.txt")
write(mean(filter(df, owner == "no")$dependents), file="output.txt", append=TRUE)

write("Average age in each income groups:", file="output.txt", append=TRUE)
write.table(df %>% group_by(incomeLevel) %>% summarise(averageAge = mean(age)), 
            file = "output.txt", sep = ";", append=TRUE, row.names=FALSE, col.names=FALSE)

write("Top 5 youngest people, whose application was declined: ", 
           file = "output.txt", append=TRUE)
dftmp <- filter(df, card == "no")
dftmp <- dftmp[order(dftmp$age),]
write.table(dftmp[1:5,], file = "output.txt", sep = ";", append=TRUE)
write("Top 5 eldest people, whose application was declined: ", file = "output.txt", append=TRUE)
write.table(tail(dftmp, 5), file = "output.txt", sep = ";", append=TRUE)

write("Average number of major CCs held for people with top 10 income: ", 
      file = "output.txt", append=TRUE)
write(sort(df$income) %>% tail(10) %>% mean(), 
      file = "output.txt", append=TRUE)
close(output)