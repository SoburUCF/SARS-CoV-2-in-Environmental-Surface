## Import requried packages
library(ggplot2)
library(cowplot)
library(scales)
library(dplyr)
library(lubridate)
library(readxl)

#set working directory 
setwd("~/Documents/swab_study/")

############# Figure 3A,Florida State and Orange county cases figure #############

# download COVID-19 case data

url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2022.csv"
destfile <- "~/Documents/swab_study/raw_data/us-state2.txt"
download.file(url,destfile)

#import dataset 
county_case2 <- read.csv("~/Documents/swab_study/raw_data/us-state2.txt")

##Safe the date in date format  
county_case2$date <- as.Date(county_case2$date)

#Subset FL data
county_case2 <- subset(county_case2, county_case2$state == "Florida")

#Subset Orange County data
county_case2 <- subset(county_case2, county_case2$county == "Orange")

# remove other non-essential column
county_case2 <- select(county_case2, !-c(date, cases, cases_avg))

#Subset of data during the study period
county_case2 <- county_case2 %>% subset(date >= as.Date('2022-01-20') & date <= as.Date('2022-11-30'))

## Prepare Florida data
# Import data
Florida_cases <- read.csv("~/Documents/swab_study/raw_data/us-statedata.txt")

##Safe the date in date format  
Florida_cases$date <- as.Date(Florida_cases$date)

#Subset FL data
Florida_cases <- subset(Florida_cases, Florida_cases$state == "Florida")

# remove other column
Florida_cases <- select(Florida_cases, !-c(date, cases, cases_avg))

#Select data for study period
Florida_cases <- Florida_cases %>% subset(date >= as.Date('2022-01-25') & date <= as.Date('2022-11-30'))


## create new dataset with cases of Florida and Orange county by Data

Ocases <- county_case2 %>% select(!-c(date,cases_avg))
Fcases <- Florida_cases %>% select(!-c(date,cases_avg)) 

Fcases <- Fcases%>% rename(fcases_avg = cases_avg)


case_dataset <- merge(Fcases, Ocases, by = "date")

write.csv(case_dataset, "Florida_orange_casedata.csv")

#import data 
case_dataset <- read.csv("~/Documents/swab_study/clean_data/Florida_orange_casedata.csv")

#Save date as Date format
case_dataset$date <- as.Date(case_dataset$date)


# Florida and Orange county COVID-19 cases trend plot

ggplot(case_dataset, aes(x = date)) +
  geom_line(aes(y = cases_avg, colour = "Orange county"), size = 1.2) +
  geom_line(aes(y = fcases_avg/10, colour = "Florida State"), size = 1.2) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Florida 7-day average cases")) +
  scale_colour_manual(values = c("blue", "red")) +
  scale_x_date(date_labels ="%y-%b",date_breaks ="1 month", date_minor_breaks = "1 month", expand = c(0,0)) +
  labs(y = "Orange county 7-day average cases", x = "", colour = "") +
  theme_classic() +
  theme(panel.grid = element_blank(), axis.text = element_text(color = "black"), legend.position = c(0.8, 0.8))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13, margin = margin(b = 20)), 
        axis.text.y = element_text(size = 11, color = "black", margin = margin(l = 12)),
        axis.text.y.right = element_text(margin = margin(r = 12)))


############# Figure 3B,UCF cases and positivity figure #############

#Import data
ucfcasesV2 <- read.csv("~/Documents/swab_study/clean_data/ucfcasesV2.csv")

# Set week as character
ucfcasesV2$week <- as.character(ucfcasesV2$week)

# Replace NA with 0
ucfcasesV2$positive[is.na(ucfcasesV2$positive)] <- 0

#University weekly cases and positivity plot
ggplot(ucfcasesV2, aes(x = as.numeric(week))) +
  geom_line(aes(y = caserate, colour = "Test positivity rate", group = 1), size = 1.2) +
  geom_line(aes(y = positive/1.2, colour = "Weekly confirm cases", group = 1), size = 1.2) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.5, name = "Universituy COVID-19 weekly cases")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "University COVID-19 test positivity rate", x = "Weeks", colour = "") +
  theme_classic() +
  theme(panel.grid = element_blank(), axis.text = element_text(color = "black"), legend.position = c(0.84, .99)) +
  scale_x_continuous(limits = c(5, NA), expand = c(0, 0)) + # adjust limits and remove space
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 13, margin = margin(b = 20)), 
        axis.text.y = element_text(size = 11, color = "black", margin = margin(l = 12)),
        axis.text.y.right = element_text(margin = margin(r = 12)))+
  annotate("segment", x = 11, xend = 11, y = -Inf, yend = Inf, linetype="dashed", size = 0.5, colour="grey")+
  annotate("text", x = 11, y = 30.2, label = "Spring Break", angle = 90, hjust = 0.1, vjust = -0.7, size = 3.5 , colour = "black")+
  annotate("segment", x = 18, xend = 18, y = -Inf, yend = Inf, linetype="dashed", size = 0.5, colour="grey")+
  annotate("text", x = 18, y = 30.2, label = "Finals Week", angle = 90, hjust = 0.1, vjust = -0.7, size = 3.5 , colour = "black")+
  annotate("segment", x = 20, xend = 20, y = -Inf, yend = Inf, linetype="dashed", size = 0.5, colour="grey")+
  annotate("segment", x = 31, xend = 31, y = -Inf, yend = Inf, linetype="dashed", size = 0.5, colour="grey")+
  annotate("text", x = 23, y = 48.2, label = "Summer 2022", hjust = 0, vjust = 0, size = 3.5 , fontface = "bold", colour = "black")+
  annotate("segment", x = 34, xend = 34, y = -Inf, yend = Inf, linetype="dashed", size = 0.5, colour="grey")+
  annotate("text", x = 34, y = 30.2, label = "Fall Begins", angle = 90, hjust = 0.1, vjust = -0.7, size = 3.5 , colour = "black")+
  annotate("segment", x = 38, xend = 38, y = -Inf, yend = Inf, linetype="dashed", size = 0.5, colour="grey")+
  annotate("text", x = 38, y = 30.2, label = "Hurricane Ian", angle = 90, hjust = 0.1, vjust = -0.7, size = 3.5 , colour = "black")+
  annotate("segment", x = 44, xend = 44, y = -Inf, yend = Inf, linetype="dashed", size = 0.5, colour="grey")+
  annotate("text", x = 44, y = 30.2, label = "Homecoming", angle = 90, hjust = 0.1, vjust = -.7, size = 3.5 , colour = "black")




########### Figure 3C , Viral gene copy trend ###############


# Import  viral gene copy number data 

viral_load_sites <- read.csv("~/Documents/swab_study/clean_data/viralload2.csv")
viral_load_avg <- read.csv("~/Documents/swab_study/raw_data/viralload.csv")

#Save date variable as Date format
viral_load_sites$date <- as.Date(viral_load_sites$date,"%m/%d/%Y") 
viral_load_avg$date <- as.Date(viral_load_avg$date, "%m/%d/%Y")

# convert the date to a character
viral_load_sites$date <- format(viral_load_sites$date, "%m.%d.%y")
viral_load_avg$date <- format(viral_load_avg$date, "%m.%d.%y")

#viral gene copy number trend plot

ggplot()+
  geom_boxplot(data = viral_load_sites, aes(x = date, y = viral_load), fill = "#00CC00", alpha = 0.8)+
  theme_classic()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90,  hjust= 0.5, vjust = 0.5))+
  ylab(expression("Environmental surface viral Log10(gc/cm"^" 2"*")")) + xlab("Date of sample collection") +
  geom_line(data = viral_load_avg, aes(x=date, y=avg, group=1), color = "red", alpha = 0.9, size = 1.2)+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13), 
        axis.text = element_text(size = 11, color = "black")) 
