library("lubridate")
library("dplyr")

# 1.
# Create a dateframe of self-employed.
# Convert amount of months applicant living at his/her current address to days.
# Define criterion of income level, and split data according to levels of this criterion.
# Add column with income per family member.
# Due to the lack of information about a person's family, 
# we assume that dependents are family members without the applicant.
today <- as.Date("2000-01-01")
df <- read.table("study_fall2021_intror_creditcard.txt", header=TRUE, sep=";") %>% filter(selfemp == "yes") %>%
  mutate(days = as.numeric(today - (today %m-% months(months))), incomeLevel = case_when(
    income < 2.5 ~ "low",
    income <= 8 ~ "average",
    TRUE ~ "high"
  ), incomePerFamilyMember = income / (dependents + 1))

# 2.
# Print some information about dataset.
file.create("output.txt", showWarnings = TRUE)
output <-file("output.txt")
write("Average amount of dependents for people don't own their home:", file="output.txt")
write((filter(df, owner == "no") %>% summarise(meanDependents = mean(dependents)))$meanDependents, 
      file="output.txt", append=TRUE)

write("Average age in each income groups:", file="output.txt", append=TRUE)
write.table(df %>% group_by(incomeLevel) %>% summarise(averageAge = mean(age)), 
            file = "output.txt", sep = ";", append=TRUE, row.names=FALSE, col.names=FALSE)

write("Top 5 youngest people, whose application was declined: ", 
           file = "output.txt", append=TRUE)
dftmp <- filter(df, card == "no") %>% arrange(age)
write.table(dftmp[1:5,], file = "output.txt", sep = ";", append=TRUE)
write("Top 5 eldest people, whose application was declined: ", file = "output.txt", append=TRUE)
write.table(tail(dftmp, 5), file = "output.txt", sep = ";", append=TRUE)

write("Average number of major CCs held for people with top 10 income: ", 
      file = "output.txt", append=TRUE)
write((arrange(df, income) %>% tail(10))$majorcards %>% mean(), 
      file = "output.txt", append=TRUE)
close(output)

