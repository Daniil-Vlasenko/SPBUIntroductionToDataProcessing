if (!require("lubridate")) install.packages("lubridate")
library("lubridate")

# 1.
# Create a dateframe of self-employed.
df <- read.table("study_fall2021_intror_creditcard.txt", header=TRUE, sep=";")
df <- subset(df, selfemp == "yes",)
# Convert amount of months applicant living at his/her current address to days.
today <- as.Date("2000-01-01")
df[, "months"] <- lapply(df["months"], function(x) as.numeric(today - (today %m-% months(x))))
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
           mean(df[df[, "owner"] == "no", "dependents"])), output)

sub.df <- aggregate(df["age"], df["incomeLevel"], FUN=mean)
names(sub.df)[names(sub.df) == "age"] <- "averageAge"
write("Average age in each income groups:", output)
write.table(sub.df, file = "output.txt", sep = ";", append=TRUE)

dfSorted <- df[order(df[,"age"]),]
dfSorted <- subset(dfSorted, card == "no",)
write("Top 5 youngest people, whose application was declined: ", 
           file = "output.txt", append=TRUE)
write.table(dfSorted[1:5,], file = "output.txt", sep = ";", append=TRUE)
write("Top 5 eldest people, whose application was declined: ", 
      file = "output.txt", append=TRUE)
write.table(tail(dfSorted, 5), file = "output.txt", sep = ";", append=TRUE)

dfSorted <- df[order(df[,"income"]),]
write("Average number of major CCs held for people with top 10 income: ", 
      file = "output.txt", append=TRUE)
write(mean(tail(dfSorted, 10)[, "majorcards"]), 
      file = "output.txt", append=TRUE)
close(output)
