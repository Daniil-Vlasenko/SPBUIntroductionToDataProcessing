if (!require("lubridate")) install.packages("lubridate")
library("lubridate")

# 1.
# Create a dateframe of self-employed.
df <- read.table("study_fall2021_intror_creditcard.txt", header=TRUE, sep=";")
df <- df[ df[, "selfemp"] == "yes", ]
# Convert amount of months applicant living at his/her current address to days.
today <- as.Date("2000-01-01")
df[, "months"] <- sapply(df["months"], FUN = function(x) as.numeric(today - (today %m-% months(x))))
names(df)[names(df) == "months"] <- "days"
# Add column with income per family member.
# Due to the lack of information about a person's family, 
# we assume that dependents are family members without the applicant.
df["incomePerFamilyMember"] <- df["income"] / (df["dependents"] + 1)
# Define criterion of income level, and split data according to levels of this criterion.
df[df[, "income"] < 2.5, "incomeLevel"] = "low"
df[df[, "income"] >= 2.5 & df[, "income"] < 8, "incomeLevel"] = "average"
df[df[, "income"] >= 8, "incomeLevel"] = "high"

# 2.
# Print some information about dataset.
file.create("output.txt", showWarnings = TRUE)
output <-file("output.txt")
writeLines(c("Average amount of dependents for people don't own their home:", 
           mean(df[df[, "owner"] == "no", "dependents"]), 
           "Average amount of age for people with low income:", 
           mean(df[df[, "incomeLevel"] == "low", "age"]),
           "Average amount of age for people with average income:",
           mean(df[df[, "incomeLevel"] == "average", "age"]),
           "Average amount of age for people with high income:",
           mean(df[df[, "incomeLevel"] == "high", "age"])),
           output)

dfSorted <- df[order(df[,"age"]),]
dfSorted <- dfSorted[dfSorted[,"card"] == "no",]
write("Top 5 youngest people, whose application was declined: ", 
           file = "output.txt", append=TRUE)
write.table(dfSorted[1:5,], file = "output.txt", sep = ";", append=TRUE)
write("Top 5 eldest people, whose application was declined: ", 
      file = "output.txt", append=TRUE)
write.table(tail(dfSorted, 5), file = "output.txt", sep = ";", append=TRUE, col.names = FALSE)

dfSorted <- df[order(df[,"income"]),]
write("Average number of major CCs held for people with top 10 income: ", 
      file = "output.txt", append=TRUE)
write(mean(tail(dfSorted, 10)[, "majorcards"]), 
      file = "output.txt", append=TRUE)
close(output)