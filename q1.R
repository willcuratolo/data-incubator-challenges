# libraries for data tools
library(tidyverse)

# load in all the data
dates = c("2009Q3", "2009Q4", 
          "2010Q1", "2010Q2", "2010Q3", "2010Q4",
          "2011Q1", "2011Q2", "2011Q3", "2011Q4", 
          "2012Q1", "2012Q2", "2012Q3", "2012Q4", 
          "2013Q1", "2013Q2", "2013Q3", "2013Q4",
          "2014Q1", "2014Q2", "2014Q3", "2014Q4",
          "2015Q1", "2015Q2", "2015Q3", "2015Q4",
          "2016Q1", "2016Q2", "2016Q3", "2016Q4",
          "2017Q1", "2017Q2", "2017Q3", "2017Q4",
          "2018Q1")

# initialize empty dataframe
data_all_quarters <- list()

# load in all the data into one dataframe
for (i in 1:length(dates)){
  filepath = paste("house-office-expenditures-with-readme/", dates[i],
                   "-house-disburse-detail.csv", sep = "")
  data_all_quarters[[i]] <- read_csv(filepath)
  # quick and dirty way to deal with some type issues
  data_all_quarters[[i]]$YEAR <- as.character(data_all_quarters[[i]]$YEAR)
  data_all_quarters[[i]]$PURPOSE <- as.character(data_all_quarters[[i]]$PURPOSE)
}

# put all the data in one dataframe
all_data <- bind_rows(data_all_quarters)

# Get dates in proper format
all_data <- all_data %>%
  mutate(STARTDATE = as.Date(all_data$`START DATE`, format="%m/%d/%y"),
         ENDDATE = as.Date(all_data$`END DATE`, format="%m/%d/%y"))

# answer q1
total <- sum(all_data$AMOUNT)
print(total)

# answer q2
q2_answer <- all_data %>%
  filter(AMOUNT > 0) %>%
  mutate(cov_period = ENDDATE - STARTDATE) %>%
  filter(!is.na(cov_period)) %>%
  mutate(cov_period = abs(as.integer(cov_period)))

q2_value <- sd(q2_answer$cov_period)
  
# answer q3
q3_answer <- all_data %>%
  filter(AMOUNT > 0) %>%
  filter(STARTDATE >= as.Date("1/1/2010", format="%m/%d/%Y")) %>%
  filter(STARTDATE <= as.Date("12/31/2016", format="%m/%d/%Y"))

q3_value <- mean(q3_answer$AMOUNT)

# answer q4
q4_answer <- all_data %>%
  filter(STARTDATE <= as.Date("12/31/2016", format="%m/%d/%Y")) %>%
  filter(STARTDATE >= as.Date("1/1/2016", format="%m/%d/%Y")) 

total_2016 <- sum(q4_answer$AMOUNT)

q4_answer1 <- q4_answer %>%
  group_by(OFFICE) %>%
  summarise(total = sum(AMOUNT)) %>%
  arrange(desc(total))
  
q4_office <- q4_answer1$OFFICE[1]

q4_answer2 <- q4_answer %>%
  filter(OFFICE == q4_office) %>%
  group_by(PURPOSE) %>%
  summarise(total = sum(AMOUNT)) %>%
  arrange(desc(total))

q4_purpose_total <- q4_answer2$total[1]

q4_final_answer <- (q4_purpose_total / total_2016)

# answer q5
q5_answer <- all_data %>%
  filter(STARTDATE <= as.Date("12/31/2016", format="%m/%d/%Y")) %>%
  filter(STARTDATE >= as.Date("1/1/2016", format="%m/%d/%Y")) %>%
  filter(CATEGORY == "PERSONNEL COMPENSATION") %>%
  group_by(OFFICE, PAYEE) %>%
  summarize(total_pay = sum(AMOUNT)) %>%
  group_by(OFFICE) %>%
  summarize(avg_employee_pay = mean(total_pay)) %>%
  arrange(desc(avg_employee_pay))
  

# answer q6

# clean data so we can match all entries for the same
# office
q6_answer <- all_data %>%
  filter(grepl("HON",OFFICE)) %>%
  filter(CATEGORY == "PERSONNEL COMPENSATION") %>%
  filter(STARTDATE <= as.Date("12/31/2016", format="%m/%d/%Y")) %>%
  filter(STARTDATE >= as.Date("1/1/2010", format="%m/%d/%Y")) %>%
  mutate(rep = str_extract(OFFICE, "(HON\\.)([[:alpha:]]|[[:space:]]|(\\.))*")) %>%
  mutate(rep = str_replace_all(rep, "\\.", "")) %>%
  mutate(year = as.integer(YEAR))

# make sure we only get data that fits the restrictions given
q6_answer1 <- q6_answer %>%
  group_by(rep, YEAR, PAYEE) %>%
  summarize() %>%
  group_by(rep, YEAR) %>%
  summarize(num_payees = n()) %>%
  group_by(rep) %>%
  summarize(years_in_office = n(), min_payees = min(num_payees)) %>%
  filter(years_in_office >= 4, min_payees >= 5)

# get a list of all the offices that meet the restrictions
good_reps <- q6_answer1$rep

# get only the data for those offices
q6_answer2 <- q6_answer %>%
  filter(rep %in% good_reps) %>%
  group_by(rep, year, PAYEE) %>%
  summarize() %>%
  mutate(leave_next_year = FALSE)

# initialize the empty list of turnover rates
turnovers <- c()

# for each rep
for (rep1 in good_reps) {

  #get that rep's data
  rep_data <- q6_answer2 %>%
    filter(rep == rep1)
  
  # initialize an empty list of their turnovers
  rep_turnovers <- c()
  
  # for each year
  for (year1 in 2010:2015) {
    # get this year and next years data
    this_year_data <- rep_data %>%
      filter(year == year1)
    
    nex_year_data <- rep_data %>%
      filter(year == year1 + 1)
    
    # get this year's and next year's emplyee lists
    employees <- this_year_data$PAYEE
    next_year_employees <- nex_year_data$PAYEE
    
    # initialize the factors for turnover
    num_employees <- length(employees)
    num_who_left <- 0
    
    # check if each employee left next year and 
    # increment if appropriate
    for (employee in employees) {
      if (!(employee %in% next_year_employees)) {
        num_who_left <- num_who_left + 1
      }
    }
    # calculate turnover and add it to the rep's list
    current_turnover <- (num_who_left / num_employees)
    rep_turnovers <- c(rep_turnovers, current_turnover)  
    
  }
  # add each rep's list to the master list
  turnovers <- c(turnovers, rep_turnovers)
}

# return the value we want
print(median(turnovers[which(!is.na(turnovers))]))



#########
#below is trash
#########

for (i in 1:length(q6_answer2[[1]])){
  entry <- q6_answer2[i,]
  rep1 <- entry[1]
  year1 <- entry[2]
  employee <- entry[3]
  
  data <- q6_answer2 %>%
    filter(rep == rep1, year == year1 + 1) 
  
  if (!(employee %in% data$PAYEE)) {
    q6_answer2[i,4] <- TRUE
  }
}

q6_answer3 <- q6_answer2 %>%
  group_by(rep, year) %>%
  summarise(count = n(), num_leaving = sum(leave_next_year)) %>%
  mutate(turnover = num_leaving / count)

q6_value <- median(q6_answer3$turnover)


test_data2 <- test_data %>%
  group_by(PAYEE)

payees <- test_data2$PAYEE

for rep1 in good_reps{
  data <- q6_answer %>%
    filter(rep == rep1)
}

%>%
  group_by(rep, YEAR) %>%
  summarize(num_payees = n()) %>%
  mutate(year = as.integer(YEAR))

%>%
  group_by(rep) %>%


str_extract(q6_answer$OFFICE, "(HON\\.)([[:alpha:]]|[[:space:]]|(\\.))*")

((([[:alpha:]])|(\\.))*)*






filter(all_data, is.na(AMOUNT)) %>%
  select(QUARTER) %>%
  group_by(QUARTER) %>%
  summarize()




for (i in 1:length(dates)){
  data_all_quarters[[i]]$AMOUNT <- as.double(data_all_quarters[[i]]$AMOUNT)
}
# change year to character for certain dfs
for (i in c(15,19,30,31,32,34,35)){
  data_all_quarters[[i]]$YEAR <- as.character(data_all_quarters[[i]]$YEAR)
}



trouble_data <- read_csv(paste("house-office-expenditures-with-readme/", dates[15],
                               "-house-disburse-detail.csv", sep = ""))

trouble_data$YEAR <- as.character(trouble_data$YEAR)

c(15,19,30,31,32,34,35)



#data <- read_csv(paste("house-office-expenditures-with-readme/", "2013Q1",
#                       "-house-disburse-detail.csv", sep = ""),
#                 col_types = "ccccDcDDcdccccc")

print(filepath)

data_2009_q3 <- read_csv(filepath)

print(dates[1])
