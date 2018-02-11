library(tidyverse)
library(lubridate)


##### Create the data ##### 

data <- tibble::tribble(
  ~Plane,     ~Status,            ~Start,              ~End,
     "A",      "Idle",  "2018-02-01 00:00", "2018-02-01 18:00",
     "A", "Operation",  "2018-02-01 18:00", "2018-02-03 22:00",
     "A",    "Repair",  "2018-02-03 22:00", "2018-02-05 00:00",
     "B",    "Repair",  "2018-02-01 00:00", "2018-02-04 10:00",
     "B", "Operation",  "2018-02-04 10:00", "2018-02-05 00:00",
     "C",      "Idle",  "2018-02-01 00:00", "2018-02-02 05:00",
     "C", "Operation",  "2018-02-02 05:00", "2018-02-05 00:00"
  )


##### Wrangle the dates ##### 

data <- data %>% 
  mutate(Start = as.POSIXct(Start, format = "%Y-%m-%d %R"),
         End   = as.POSIXct(End  , format = "%Y-%m-%d %R")) %>%
  mutate(start_date = as.POSIXct(paste(substr(Start, 1, 10), " 00:00:00", sep=""),format = "%Y-%m-%d %R"),
         end_date   = as.POSIXct(paste(substr(End  , 1, 10), " 24:00:00", sep=""),format = "%Y-%m-%d %R"))
  

##### Create our first test ##### 

test <- data %>%
  mutate(difference = difftime(End, Start, units = "hour"))


##### Create function ##### 
# Function from 
# https://stackoverflow.com/questions/47165434/breaking-up-rows-with-intervals-spanning-over-an-hour-issues-with-time-convers

expand.row <- function(x) {
    with(x, {
      h <- trunc(start_date, 'hour') + 86400 
      if (h > end_date)
        p <- c(start_date, end_date)
      else
        p <- unique(c(start_date, seq(h, end_date, 86400), end_date))
      n <- length(p)
      data.frame(Plane, Status,Start, start_date = p[seq(1, n - 1)], End, end_date = p[seq(2, n)])
    })
}


##### Apply the function ##### 

expanded_data <- do.call(rbind, lapply(split(data, seq(nrow(data))), expand.row))


##### Test again ##### 

test2 <- expanded_data %>%
  mutate(start_difference = difftime(Start, start_date, units = "hour"),
         end_difference = difftime(end_date, End, units = "hour")) %>% 
  mutate(start_difference = ifelse(start_difference <= 0 , 0, start_difference),
         end_difference = ifelse(end_difference <= 0 , 0, end_difference)) %>% 
  mutate(total_difference = start_difference + end_difference) %>% 
  mutate(total_difference = ifelse(total_difference == 0, 24, total_difference))


##### Create and tidy the final dataset ##### 

final_data <- test2 %>% 
  group_by(Plane, Status, start_date) %>% 
  mutate(StatusHours = sum(total_difference)) %>% 
  ungroup() %>%
  select(Plane, Status, Date = start_date, StatusHours ) %>% 
  distinct()
         