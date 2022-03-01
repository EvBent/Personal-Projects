---
title: "NYPD Shooting Data Report"
author: "E. Benton"
date: "2/22/2022"
output:
  html_document:
    df_print: paged
  pdf_document: default
---

### **Introduction**

This data breaks down every shooting that has happened in New York City from 2006 through the end of 2020. Shooting data contains information about the incident, location and time of occurrence, and victim/suspect demographics. Data is updated through end of the previous calendar year.

In this analysis I aim to determine if there is any significant or obvious relationship between shootings and the time of day in New York City.

**Reading in the Data**
```{r get_shooting_data}
#First the data from the CSV file will be imported
library(tidyverse)

link_in <- 'https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv'
shooting_data <- read_csv(link_in)
summary(shooting_data)
head(shooting_data)
```

**Tidying the Data**
```{r tidy_shooting_data}
##Removing columns not need for my analysis
shooting_data <- shooting_data %>%
select(-c(INCIDENT_KEY, PRECINCT, JURISDICTION_CODE, PERP_AGE_GROUP, PERP_SEX,
PERP_RACE, VIC_AGE_GROUP, VIC_SEX, VIC_RACE, X_COORD_CD, Y_COORD_CD,
Latitude, Longitude, Lon_Lat))
shooting_data
##Separating shooting incidents into 3 main groups, All incidents, murders, and non-murders
##No Murder
shootings_no_murder <- shooting_data %>%
filter(STATISTICAL_MURDER_FLAG == FALSE)
##Murder
shootings_murder <- shooting_data %>%
filter(STATISTICAL_MURDER_FLAG == TRUE)
##shooting_data has the data for both
```

**Visualizing the Data**
```{r visualize_shooting_data}
#Visualizing the frequency of all shooting incidents from 2006-2021 by hour
shooting_data$OCCUR_TIME <- as.POSIXct(shooting_data$OCCUR_TIME, format = "%H:%M:%S")
all_incidents <- hist(shooting_data$OCCUR_TIME, "hours", freq = TRUE,
main = "All Shooting Incidents in NYC by Hour",
xlab = "Time of Day", labels = TRUE)
##Visualizing the frequency of all shooting incidents not flagged as murder from 2006-2021 by hour
shootings_no_murder$OCCUR_TIME <- as.POSIXct(shootings_no_murder$OCCUR_TIME, format = "%H:%M:%S")
no_murder <- hist(shootings_no_murder$OCCUR_TIME, "hours", freq = TRUE,
main = "Shooting Incidents Not Flagged as Murder in NYC by Hour",
xlab = "Time of Day", labels = TRUE)
##Visualizing the frequency of all shooting incidents flagged as murder from 2006-2021 by hour
shootings_murder$OCCUR_TIME <- as.POSIXct(shootings_murder$OCCUR_TIME, format = "%H:%M:%S")
murder <- hist(shootings_murder$OCCUR_TIME, "hours", freq = TRUE,
main = "Shooting Incidents Flagged as Murder in NYC by Hour",
xlab = "Time of Day", labels = TRUE)
##Overlapping shooting incidents flagged as murder over all shooting incidents in NYC
plot(all_incidents, col = rgb(0,0,1,1/4), labels = TRUE, main = "Comparison of Shooting Incidents by Type in NYC")
plot(murder, col = rgb(1,0,0,1/4), add = T, labels = TRUE)
```

**Analyzing the Data**
```{r analysing_shooting_data}
##Comparing the number of shooting incidents to shooting incidents flagged as murders in NYC
shooting_data %>%
count(STATISTICAL_MURDER_FLAG == TRUE)
##Finding the day with the highest amount of shootings in NYC
Mode <- function(x) {
uniqux <- unique(x)
uniqux[which.max(tabulate(match(x, uniqux)))]
}
x <- shooting_data$OCCUR_DATE
result <- Mode(x)
result
```

**Modeling Density**
```{r modeling_density}
##Modeling density curves for all shooting incidents in NYC vs those flagged as murder
density_all_incidents <- density(as.numeric(shooting_data$OCCUR_TIME))
density_murder <- density(as.numeric(shootings_murder$OCCUR_TIME))
plot(density_all_incidents, lwd = 3, col = "red",
main = "Density Comparison of All Shooting Incidents in
Red vs Shooting Incidents Flagged as Murder in Blue",
xlab = "")
lines(density_murder, col = "blue", lwd = 3)
```

**Visualization and Analysis Breakdown**

At the time of creating this report, this relatively simple analysis can shed insight into shootings ocurring in NYC between 1970 and 2020. As I expected before looking at this data, shooting incidents of all types are most common at night. Shooting incidents are at their peak during the hours of 12-1am. All incidents are at their lowest between the hours of 10 and 11am. While the total amount of shooting incidents is at its peak between 12 and 1 am, shooting incidents flagged as murder occur most often between 10 and 11 pm. 
Further analysis into this data found that in this data set 19085 incidents were not flagged as a statistical murder while 4500 were. The day with the most shooting incidents in this data set was 7/5/2020. Seeing 2020 listed as the date with the highest number of shootings is not surprising, and would be interesting with additional data. During this time period NYC was fighting with the COVID-19 pandemic and like many other states faced strict lockdowns. I would be curious to know what proportion of the shooting incidents were random or from domestic violence as a result of being told to stay indoors with one another in close proximity,likely increasing the frequency of domestic disputes.

**Conclusion and Bias Identification**

Shooting incidents including those flagged as murder are most active during the hours between 2pm and 6am. The day with the most shooting incidents in this data set is 7/5/2020. A possible source of bias in this report is that it would seem intuitive for shooting incidents to occur more frequently at night since it may be harder to be seen or easier to avoid being caught.Thinking about this for a moment reveals that there are many possible reasons why these hours have more shooting incidents. People are less likely to be working, bars are more likely to be open, and events like rush hour may drive up altercations between individuals leading to shooting incidents. In this project one personal bias I held was that all shooting incidents included in this report were a crime. In reality, several of these shooting incidents could be related to self defense, police shootings and more. Although I did not look at this data in my analysis and differentiate between the type of incident besides those flagged as murder and not, I made sure to keep an open mind at these high numbers for cases not flagged as murder. Some of these shooting instances may have saved lives and prevented further casualties. 
