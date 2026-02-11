# Question 1

df1 <- data.frame(
  Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
         'Richards','George','Ema','Samantha','Catherine'),
  State=c('Alaska','California','Texas','North Carolina','California','Texas',
          'Alaska','Texas','North Carolina','Alaska','California','Texas'),
  Sales=c(14,24,31,12,13,7,9,31,18,16,18,14)
)

print(aggregate(df1$Sales, by=list(df1$State), FUN=sum))

library(dplyr)
print(
  df1 %>%
    group_by(State) %>%
    summarise(sum_sales = sum(Sales))
)



# Question 2 
wc <- read.csv("WorldCupMatches.csv")

# (a) size of data frame
print(dim(wc))
print(nrow(wc))
print(ncol(wc))

# (b) summary
print(summary(wc))

# (c) how many unique locations?
print(length(unique(wc$City)))

# (d) average attendance
print(mean(wc$Attendance, na.rm = TRUE))

# (e) total Home Team goals
home_goals <- wc %>%
  group_by(`Home.Team.Name`) %>%
  summarise(total_home_goals = sum(`Home.Team.Goals`, na.rm = TRUE)) %>%
  arrange(desc(total_home_goals))

print(home_goals)

# (f) average attendance per year
att_by_year <- wc %>%
  group_by(Year) %>%
  summarise(avg_attendance = mean(Attendance, na.rm = TRUE)) %>%
  arrange(Year)

print(att_by_year)



# Question 3 
met <- read.csv("metabolite.csv")

# (a) number of Alzheimer patients
print(sum(met$Label == "Alzheimer", na.rm = TRUE))

# (b) number of missing values per column
print(colSums(is.na(met)))

# (c) remove rows with missing Dopamine values
met2 <- met[!is.na(met$Dopamine), ]
print(dim(met2))

# (d) replace missing c4-OH-Pro with median
med_c4 <- median(met2$`c4-OH-Pro`, na.rm = TRUE)
met2$`c4-OH-Pro`[is.na(met2$`c4-OH-Pro`)] <- med_c4

print(sum(is.na(met2$`c4-OH-Pro`)))






