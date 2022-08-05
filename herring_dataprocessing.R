library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(lubridate)
library(pander)
library(xtable)
library(gridExtra)
library(ggthemes)

rm(list = ls())
options(stringsAsFactors=FALSE)

# Converts all herring counts to an integer value.
make_ints <- function(X) {
  X$Count <- as.integer(X$Count)
  return(X)
}

# Removes identical rows using distinct(). Removes the date field from the 
# time stamp, and formats it into 24-hour time. Formats the date field.
make_distinct  <- function(UPL_ints) {
  UPL_distinct <- UPL_ints %>%
    distinct(across(c(Date, Start, End, Count, Name)),.keep_all=TRUE) 
  UPL_distinct$Timestamp <- mdy_hm(UPL_distinct$Timestamp)
  UPL_distinct$Timestamp <- format(UPL_distinct$Timestamp,"%H:%M:%S")
  UPL_distinct$Date <- mdy(UPL_distinct$Date)
  return(UPL_distinct) 
}

# Updates obviously incorrect years to this year.
change_year <- function(X) {
  year(X$Date) <- 2021
  return(X)
}

# Filter between April 11 and June 6, sorts dates into ascending order.
date_range <- function(UPL_yr_correct) {
  UPL_yr_correct <- UPL_yr_correct %>%
    select(Timestamp,Date,Start,End,Count,Name) %>%
    filter(Date >= as.Date("2021-04-11") & Date <= as.Date("2021-06-27"))
  UPL_yr_correct <- UPL_yr_correct[order(as.Date(UPL_yr_correct$Date, format="%Y/%m/%d")),]
  return(UPL_yr_correct)
}

# Compares the original data set to the data set filtered by date range from the 
# date_range function. Uses anti_join to create a subset of data outside of the 
# specified date range for further analysis. Formats the start and end times of 
# the new subset into 24-hour-time.
compare_sets <- function(X,Y) {
  differences <- anti_join(X,Y, by='Date')
  differences$issue <- "Dates out of bounds"
  differences <- differences[,c("Timestamp","Date","Start", "End", "Count", "Name", "issue")]
  differences$Start <-format(strptime(differences$Start, "%I:%M:%S %p"), format="%H:%M:%S")
  differences$End <- format(strptime(differences$End, "%I:%M:%S %p"), format="%H:%M:%S")
  return(differences)
}

# Formats the start and end times of the data set into 24-hour time.
military_time  <- function(X) {
  time <- X 
  time$Start <- format(strptime(time$Start, "%I:%M:%S %p"), format="%H:%M:%S")
  time$End <-format(strptime(time$End, "%I:%M:%S %p"), format="%H:%M:%S")
  
  return(time)
}

# Sorts the data set to include only start times between 7:00 am and 
# 7:20 pm and end times between 7:00 and and 7:30 pm. 
checktimes <- function(time) {
  time <- time[time$Start >= "07:00:00" & time$Start <= "19:20:00",]
  time <- time[time$End >= "07:00:00" & time$End <= "19:30:00",]
  return(time)
}

# Subsets the data set by comparing the original data to the filtered data
# given by the checktimes function in the "Start" column.
time_followup_start <- function(X,Y) {
  tdifferences <- anti_join(X,Y, by='Start')
  return(tdifferences)
}

# Subsets the data set by comparing the original data to the filtered data
# given by the checktimes function in the "End" column.
time_followup_end <- function(X,Y) {
  tdifferences <- anti_join(X,Y, by='End')
  return(tdifferences)
}

# Creates a distinct data set from the return values of time_followup_start and
# time_followup_end. 
find_unique_timefu <- function(X) {
  t <- X %>%
    distinct(across(c(Date, Start, End, Count, Name)),.keep_all=TRUE)
  t$issue <- "Outside of daily monitoring window"
  return(t)
}

# Appends the data set with a new column indicating the difference between the 
# start and end times of the monitoring window. 
append_split <- function(timesort) {
  time_E <- hms(timesort$End)
  time_S <- hms(timesort$Start)
  diftest <- time_E-time_S
  timesort$Timerange <- diftest
  return(timesort)
}

# Filters out entries where the difference between the start and end times is 
# not equal to 10 minutes. 
split_filter <- function(X) {
  X <- X[X$Timerange == "10M 0S",]
  return(X)
}

# Creates a subset of entries for followup regarding the length of monitoring 
# window.
timesplit_followup <- function(X) {
  X <- X[X$Timerange != "10M 0S",]
  X$issue <- "Monitoring window is not 10 minutes long"
  X <- X[,-7] # removes column "Timerange" so the data formatting is consistent 
              # with the other followups
  return(X)
}

# Finds the dates where there were more than 12 herring count entries per day.
UPL_perday <- function(timesplit) {
  nperday <- timesplit %>%
    select(Date,Start,End,Count, Name, Timerange) %>%
    group_by(Date)%>%
    summarise(n=n())
  nperday <- nperday[nperday$n > 12,]
  return(nperday)
}

# Removes the dates where there were more than 12 herring count entries.
remove_overcounts <- function(data,overcounts) {
  data <- data[!(data$Date %in% overcounts$Date),]
  return(data)
}

# Creates a subset of dates where there were more than 12 herring count entries 
# for future analysis.
find_overcount_followups <- function(no_overcounts, overcounts) {
  fup <- anti_join(overcounts,no_overcounts, by='Date')
  fup <- fup[,-7] # Removes column "Timerange" so the data formatting is 
                  #consistent with the other followups
  fup$issue <- "More than 12 counts recorder per day"
  return(fup)
}

# Takes daily averages of the fully sorted data set. 
take_daily_averages <- function(X) {
  UML_daily <- overcounts_removed %>%
    group_by(Date) %>%
    summarise(daily = round((mean(Count))))
  return(UML_daily)
}

# Combines the data sets where future investigation will be needed to verify 
# data input.
combine_followups <- function (W,X,Y,Z) {
  all <- rbind(W,X,Y,Z)
  return(all)
}

#--------------------------SCRIPT BEGINS HERE----------------------------------#

UPL <- read.csv("herringcount.csv", strip.white=TRUE)
colnames(UPL) <- (c("Timestamp", "Date", "Start","End", "Count", 
                    "Weather", "Comment", "Name"))
UPL <- UPL[,-9]
UPL <- UPL[-493,] #Correction for this dataset becasue the headers are not in the first row

UPL_ints <- make_ints(UPL) # converts fish counts to ints
UPL_distinct <- make_distinct(UPL_ints) # Sorts for uniqueness
UPL_yr_correct <- change_year(UPL_distinct) #Corrects obvious year errors
UPL_daterange <- date_range(UPL_yr_correct) #Filters for date range
date_followup <- compare_sets(UPL_yr_correct,UPL_daterange)
mtime <- military_time(UPL_daterange)
UPL_timesort <- checktimes(mtime)
time_followup1 <- time_followup_start(mtime,UPL_timesort)
time_followup2 <- time_followup_end(mtime,UPL_timesort)
conc_time <- rbind(time_followup1,time_followup2)
time_follup<- find_unique_timefu(conc_time)
split_app <- append_split(UPL_timesort)
timesplit_sorted <- split_filter(split_app)
timesplit_followup <- timesplit_followup(split_app) 
overcounts <- UPL_perday(timesplit_sorted)
overcounts_removed <- remove_overcounts(timesplit_sorted, overcounts) 
overcount_followup <- find_overcount_followups(overcounts_removed, timesplit_sorted)

#--------------------------------- Deliverables -------------------------------#

UML_daily <- take_daily_averages(overcounts_removed)
all_followups <- combine_followups(date_followup,time_follup, 
                                   timesplit_followup, overcount_followup)

write.csv(UML_daily, file = "daily_average.csv")
write.csv(all_followups, file = "sorted_followups.csv")


maximum = max(UML_daily$daily)
max_hourly = 6*maximum
max_hourly

#-------------------------------- Plotting ------------------------------------#

UML_daily$Date <- as.Date(UML_daily$Date)
g <- ggplot(UML_daily, aes(x = Date, y = daily*6), na.omit=TRUE)
g + geom_bar(stat="identity", color = "black") + 
  labs(title = "Upper Mystic Lake Herring Count, Hourly Averages",
       subtitle = "2021",
       x = "Date", y = "Average Herring Count") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ylim(0,3600)

#same as above but group by day of timestamp, to inspect differences. 
# UML_timestamp <- UPL %>%
#   group_by(lubridate::date(Timestamp))%>%
#   summarise(daily = mean(Count))

## WRITING FILES
#write.csv(wq2, file = "./Rcode/Sandbox/Andy/Baseline/wq_export.csv")
#write.csv(base, file = "\\\\psf/Home/Dropbox/MysticDB/Rcode/Sandbox/Andy/Baseline_export.csv")


## GGPLOT LABELS
# from Jeff's code

#   scale_x_continuous(breaks = scales::pretty_breaks(n = 20), expand = c(0, 0), labels = label_hour) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 6), expand = c(0, 0), limits = c(0, 2750)) +
#   scale_fill_brewer("Period", type = "div", palette = "Spectral") +
#   labs(x = "Hour of Day", y = "Average Estimated Hourly Run\n(# fish/hr)", title = "Average Estimated Hourly Run by Hour of the Day") +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)


## COLOR BREWER PALETTES: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
## Ex. scale_color_brewer(palette="Set2")    for points
## Ex. scale_fill_brewer(palette="Set2")    for bars, etc. 
#videos <- read.csv("videos_.csv", strip.white=TRUE)

# Diverging
# BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# 
# Qualitative
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
# 
# Sequential
# Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd
# 
# geom_hline(aes(yintercept=BoatingStandard), 
#            data=filter(bac_standards, CharacteristicID==bacteria_type),
#            color='red', size=1) +
#   geom_hline(aes(yintercept=SwimmingStandard), 
#              data=filter(bac_standards, CharacteristicID==bacteria_type),
#              color='orange') +
#   geom_text(aes(x=1, y=BoatingStandard,
#                 label=paste0('Boating Standard: ', BoatingStandard, ' #/100ml')), 
#             data=filter(bac_standards, CharacteristicID==bacteria_type), 
#             hjust=0, vjust=1.1, alpha=0.8, size=3) +
#   geom_text(aes(x=1, y=SwimmingStandard,
#                 label=paste0('Swimming Standard: ', SwimmingStandard, ' #/100ml')), 
#             data=filter(bac_standards, CharacteristicID==bacteria_type), 
#             hjust=0, vjust=1.1, alpha=0.8, size=3) +
#   labs(y=paste0(filter(bac_standards, CharacteristicID==bacteria_type)$BacteriaLabel, ' #/100ml'),
#        x='Site ID') +
#   scale_y_log10(breaks=10^seq(0, 7), labels=comma) +
#   scale_color_manual('Weather', values=c('Dry'='cadetblue2', 'Wet'='deepskyblue4'), drop=FALSE) +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=NULL))
# theme_set(theme_bw())
