

###### Exercise Code for final assignment #######

### Option 2 

#### Research question:

# "Oral and written questions allow Members of Parliament (MPs) in the House of Commons to query the government and its ministers on their work 
# (see [here](https://www.parliament.uk/about/how/business/)). 
# What, if any, characteristics and factors discriminate MPs who tend to ask questions about economic issues from MPs
# who tend to ask questions about health and welfare issues?"

# Factors/ Characteristics of Discrimination:

# Party
# Length of Answer
# Gender
# Seniority
# Expertise
# Constituency Demographics
# Declaration of Members' Interests
# questionStatus
# number of words in answer


# Named day questions only occur in the House of Commons. 
# The MP tabling the question specifies the date on which they should receive an answer.
# MPs may not table more than five named day questions on a single day.


#### Required data source:

# [UK Parliament API](https://developer.parliament.uk/)


#### Getting to know the data: #####

## API Directory ##

# Some of the following Parliamentary APIs are versioned. Versioned APIs can be distinguished from others by the "/api/v{x}" portion of the URL, 
# x being the version number. A version of an API endpoint can have parts of its inputs or outputs extended, but not removed or modified. 
# If an endpoint requires input or output modifications or removal, this would result in a new version.

### Premiership of Theresa May ###
## 13 July 2016 – 24 July 2019 ##

# I am using data from 11/2017 to 08/2019 for several reasons:
# firstly it only includes on cabinet with theresa may as prime minister which means that the data is not skewed by different governments and their different ideological positions
# secondly, I deliberately do not include data during covide
# thirdly, by having data from 21 months I am able to compare different times of the year


library(jsonlite)
library(tidyverse)

# I will offer several identification strategies:

#1st: classifying data by answering body
#2nd: classifying data by topic using dictionary and gsub?



### Written Questions with Answers ####

written_questions_url <- "https://questions-statements-api.parliament.uk/api/writtenquestions/questions"
written_questions <- fromJSON(written_questions_url)

# Creating a loop that takes the first 100 entries of each months in the years 2017 to 2020
# I deliberately exclude the covid years as the unusual amount of attention on the health policies does not allow for an accurate description of the data

### test data 

written_questions_url_2017_12 <- "https://oralquestionsandmotions-api.parliament.uk/oralquestions/list?&parameters.take=100&parameters.answeringDateStart=2017-12-01"
written_questions_url_2017_12 <- fromJSON(written_questions_url_2017_12)

new <- rbind(written_questions_2017_11, written_questions_url_2017_12)

written_questions_2017_07_all_url <- "https://questions-statements-api.parliament.uk/api/writtenquestions/questions?tabledWhenFrom=2017-07-01&tabledWhenTo=2017-08-01&answered=Any&questionStatus=AllQuestions&includeWithdrawn=true&expandMember=true&take=100"
written_questions_2017_07_all <- fromJSON(written_questions_2017_07_all_url)

written_questions_2017_07_all_results <- written_questions_2017_07_all$results



#### vectors with dates for questions


dates_1 <- c("2017-07-01","2017-08-01","2017-09-01","2017-10-01", "2017-11-01", "2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2018-06-01", "2018-07-01", "2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-07-01")
dates_2 <- c("2017-08-01","2017-09-01","2017-10-01", "2017-11-01","2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2018-06-01", "2018-07-01", "2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-06-30", "2019-08-01")


written_questions_url <- "https://questions-statements-api.parliament.uk/api/writtenquestions/questions?answered=Any&questionStatus=AllQuestions&includeWithdrawn=true&expandMember=true&house=Commons&take=100"

# writing function to loop through dates in dates vector to get 100 questions for each months
# this way I account for potential time effects (e.g. whether or not some questions are penalized when government has just formed)

# i have to update the function to take in two values: from and to 

url <- paste0(written_questions_url,"&tabledWhenFrom=", dates_1[1], "&tabledWhenTo=", dates_2[1])

### Function 1

fetch_data_written_all <- function(url, dates_1, dates_2) {
  # Initialize an empty dataframe to store the results
  result_df <- data.frame()
  
  for (i in 1:length(dates_1)) {
    # Fetch data for each date pair
    data <- fromJSON(paste0(url, "&tabledWhenFrom=", dates_1[i], "&tabledWhenTo=", dates_2[i]))
    data_results <- data$results 
    
    # Append the data to the result dataframe
    result_df <- bind_rows(result_df, data_results)
  }
  
  return(result_df)
}


written_questions_data <- fetch_data_written_all(written_questions_url, dates_1, dates_2)
written_questions_data <- written_questions_data$value

table(written_questions_data$house)

#### trying out a loop 

result_df <- data.frame()

for (i in 1:length(dates_1)) {
  # Form the API URL
  api_url <- paste0(written_questions_url, "&tabledWhenFrom=", dates_1[i], "&tabledWhenTo=", dates_2[i])
  
  # Print API URL for debugging
  cat("API URL:", api_url, "\n")
  
  # Fetch data for each date pair
  data <- fromJSON(api_url)
  data_response <- data$results
  
  # Append the data to the result dataframe
  result_df <- bind_rows(result_df, data_response)
}

# Print the result dataframe
print(result_df)



###################################################
### creating data with oral and written questions
###################################################


dates_1_1 <- c("2017-11-01", "2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2018-06-01", "2018-07-01", "2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-07-01")
dates_2_2 <- c("2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", "2018-05-01", "2018-06-01", "2018-07-01", "2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-06-30", "2019-08-01")


written_question_url_no2 <- "https://questions-statements-api.parliament.uk/api/writtenquestions/questions?expandMember=true&take=100"


fetch_written_data <- function(url, dates_1_1, dates_2_2) {
  result_df <- data.frame()
  
  for (i in 1:length(dates_1_1)) {
    # Form the API URL
    data <- fromJSON(paste0(url, "&answeredWhenFrom=", dates_1_1[i], "&answeredWhenTo=", dates_2_2[i]))
    data_response <- data$results
    
    # Append the data to the result dataframe
    result_df <- bind_rows(result_df, data_response)
  }
  return(result_df)
}


written_data <- fetch_written_data(written_question_url_no2, dates_1_1, dates_2_2)
written_data <- written_data$value

written_data$oral_written <- ("written")

MP_name <- written_data$askingMember$listAs
written_data$MP_name <- MP_name

# subsetting to merge with oral questions

written_questions_data <- written_data %>% select(c(id,askingMemberId, MP_name, dateTabled, dateAnswered, uin, questionText,answeringBodyName,oral_written ))

##### getting data for oral questions 

fetch_data <- function(url, dates) {
  # Initialize an empty dataframe to store the results
  result_dataframe <- data.frame()
  
  for (i in dates) {
    # Fetch data for each date
    data_oral <- fromJSON(paste0(url, i))
    
    data_results <- data_oral$Response
    
    # Append the data to the result dataframe
    result_dataframe <- bind_rows(result_dataframe, data_results)
  }
  
  return(result_dataframe)
}


oral_questions_url <- "https://oralquestionsandmotions-api.parliament.uk/oralquestions/list?parameters.take=100&parameters.answeringDateStart="
oral_questions_data <- fetch_data(oral_questions_url, dates_1_1)

oral_questions_data$oral_written <- ("oral")



oral_questions_data$MP_name <- oral_questions_data$AskingMember$ListAs

oral_questions_data <- oral_questions_data %>% rename(dateTabled = TabledWhen) %>% 
  rename(askingMemberId = AskingMemberId) %>% rename(uin = UIN) %>% rename(questionText = QuestionText) %>% 
  rename(answeringBodyName = AnsweringBody) %>%
  rename(dateAnswered = AnsweringWhen) %>% rename(id = Id)

oral_questions_data <- oral_questions_data %>% select(c(id,askingMemberId, MP_name, dateTabled, dateAnswered, uin, questionText,answeringBodyName,oral_written ))


### merge oral and written questions data 

all_questions_data <- rbind(written_questions_data, oral_questions_data)


######## written questions 

# how to classify the data?
# either categorizing by answering body (e.g econ = economics minister) or using specific strings and words with gsub and filter data accordingly

table(written_questions_data$answeringBodyName)
table(written_questions_data$answeringBodyId)


### Economy Question:###

# Department for Communities and Local Government
# Treasury 
# Department for International Trade
# HM Treasury
# Department for Business, Energy and Industrial Strategy


### Health and Welfare Questions ###

# Department of Health
# Department of Health and Social Care
# Women and Equalities
# Department for Work and Pensions


written_questions_topical <- written_questions_data %>% filter(answeringBodyName %in% c('Department for Communities and Local Government', 'Treasury','Department for International Trade', 'HM Treasury','Department for Business, Energy and Industrial Strategy', 'Department of Health', 'Department of Health and Social Care', 'Women and Equalities','Department for Work and Pensions'))


all_questions_topical <- all_questions_data  %>% filter(answeringBodyName %in% c('Department for Communities and Local Government', 'Treasury','Department for International Trade', 'HM Treasury','Department for Business, Energy and Industrial Strategy', 'Department of Health', 'Department of Health and Social Care', 'Women and Equalities','Department for Work and Pensions'))



written_questions_topical <- written_questions_topical %>% 
  mutate(economic_welfare = case_when(
    answeringBodyName %in% c('Department for Communities and Local Government', 'Treasury', 'Department for International Trade', 'HM Treasury', 'Department for Business, Energy and Industrial Strategy') ~ "economics",
    answeringBodyName %in% c('Department of Health', 'Department of Health and Social Care', 'Women and Equalities', 'Department for Work and Pensions') ~ "welfare" ))

table(written_questions_topical$economic_welfare)


all_questions_topical <- all_questions_topical %>% 
  mutate(economic_welfare = case_when(
    answeringBodyName %in% c('Department for Communities and Local Government', 'Treasury', 'Department for International Trade', 'HM Treasury', 'Department for Business, Energy and Industrial Strategy') ~ "economics",
    answeringBodyName %in% c('Department of Health', 'Department of Health and Social Care', 'Women and Equalities', 'Department for Work and Pensions') ~ "welfare" ))


### identifying factors: 

# MP characteristics 

table(written_questions_topical$askingMember$party)

written_questions_topical %>% group_by(economic_welfare, askingMember$party) %>%
  count()


table(written_questions_topical$answerText)

written_questions_topical %>% mutate(time_answer = dateAnswered - dateTabled)


written_1 <- written_questions_topical %>% mutate(length_answer = nchar(answerText))
table(written_1$length_answer)

length(written_questions_topical$answerText)

# length of answer is capped at 257 characters 


###############################
#### getting additional MP data
##############################

askingmembers <- written_questions_topical$askingMember$listAs

Askingmembers <- all_questions_topical$MP_name

members_url <- "https://members-api.parliament.uk/api/Members/Search?Name="


library(stringr)

# Extracting the first word from each string
data_members_first_words <- str_extract(Askingmembers, "[^,\\s]+")

data_members_first_words <- unique(data_members_first_words) # attention I am not getting all with the same name

data23 <- fromJSON(paste0(members_url, data_members_first_words[1]))
data23$items$value$gender


### Function 

test_data_members <-data_members_first_words[1:10]

get_members_data <- function(url, names) {
  
  data_members <-  data.frame()
  
  for (i in names) {
    member_data <- fromJSON(paste0(url, i, "&take=1&MembershipInDateRange.WasMemberOnOrAfter=2017-11-01"))
    member_data <- member_data$items
    
    data_members <- bind_rows(data_members, member_data)
  }
  
  return(data_members)
}


members_data_final <- get_members_data(members_url, data_members_first_words)

members_data_final <- members_data_final$value

members_data_final <-members_data_final %>% rename( "askingMemberId" = "id")

table(members_data_final$latestHouseMembership$membershipEndDate)


## looking at thumbnail url
table(members_data_final$thumbnailUrl)

members_data_final$thumbnailUrl[1]



### merge members and questions data together


written_questions_merged <- left_join(written_questions_topical, members_data_final, by = "askingMemberId")
written_questions_merged <- unique(written_questions_merged)

written_questions_merged$economic_welfare.factor <- as.factor(written_questions_merged$economic_welfare)

levels(written_questions_merged$economic_welfare.factor) <- c("0", "1")

written_questions_merged$economic_welfare.factor <- factor(written_questions_merged$economic_welfare.factor,
                                                           levels = c(0,1),
                                                           labels = c("economics", "welfare"))

levels(written_questions_merged$economic_welfare.factor)

written_questions_merged$askingMember$party



##### plot counts ###

written_questions_merged %>% group_by(economic_welfare) %>%
  count()

## Logit regression 

levels(written_questions_merged$gender)

model1 <- glm(economic_welfare.factor ~ askingMember$party + gender, family = binomial, data = written_questions_merged)

stargazer::stargazer(model1, type = "text")

?stargazer()
?glm()





##### calculate time until answer 
# using: dateForAnswer and dateTabled



date1 <- "2017-07-20T00:00:00"
date2 <- "2017-08-15T00:00:00"

# Load the lubridate package
library(lubridate)

# Parse the dates
parsed_date1 <- ymd_hms(date1)
parsed_date2 <- ymd_hms(date2)

# Calculate the time difference in days
time_difference_days <- as.numeric(difftime(parsed_date2, parsed_date1, units = "days"))

written_questions_merged$dateTabled <- ymd_hms(written_questions_merged$dateTabled)
written_questions_merged$dateForAnswer <- ymd_hms(written_questions_merged$dateForAnswer)
written_questions_merged$dateAnswered <- ymd_hms(written_questions_merged$dateAnswered)

written_questions_merged <- written_questions_merged %>% mutate(answering_time = as.numeric(difftime(dateForAnswer,dateTabled, units = "days")))

written_questions_merged <- written_questions_merged %>% mutate(answering_time_2 = as.numeric(difftime(dateAnswered,dateTabled, units = "days")))





model2 <- lm(answering_time ~ economic_welfare.factor, data = written_questions_merged)

written_questions_merged %>% group_by(economic_welfare) %>%
  summarize(mean_answering_time = mean(answering_time_2, na.rm = TRUE))

written_questions_merged %>% group_by(id, economic_welfare) %>%
  summarize(answering_time_2) %>% ggplot(aes(x = id, y = answering_time_2, fill= economic_welfare, color = economic_welfare)) +
  geom_col() +
  scale_color_manual(values = c("lightblue","darkgreen")) +
  scale_fill_manual(values = c("lightblue", "darkgreen")) +
  theme_bw() +
  ylab("Days until Answer") +
  xlab("Question ID")

?scale_color_manual()
str(written_questions_merged$answering_time)


written_questions_merged %>% group_by(gender, economic_welfare) %>%
  count()

#### deprivation of constituencies ###

deprivation <- readxl::read_excel("Desktop/Data_for_Data_Scientists/Final-Assignment-my472/deprivation-dashboard.xlsx", 
                          sheet = "Data constituencies")

written_questions_merged$askingMember$memberFrom

ConstituencyName <- written_questions_merged$askingMember$memberFrom

written_questions_merged$ConstituencyName <- ConstituencyName

# merge both datasets 

written_questions_constituencies <- left_join(written_questions_merged,deprivation, by = "ConstituencyName")

written_questions_constituencies$Income

model2 <- glm(economic_welfare.factor ~ Income, data = written_questions_constituencies, family = binomial)

written_questions_constituencies$Income




#### measuring seniority of MPs #####

# using membership startdate 

members_data_final$latestHouseMembership$membershipStartDate

## new variable with enddate of government 

enddate_government <- c("2019-07-25T00:00:00")

written_questions_constituencies$enddate_government <- enddate_government

parliament_entry <- written_questions_constituencies$latestHouseMembership$membershipStartDate 
written_questions_constituencies$parliament_entry <- parliament_entry

written_questions_constituencies$parliament_entry <- ymd_hms(written_questions_constituencies$parliament_entry)
written_questions_constituencies$enddate_government <- ymd_hms(written_questions_constituencies$enddate_government)

str(written_questions_constituencies$parliament_entry)
written_questions_constituencies <- written_questions_constituencies %>% mutate (seniority = as.numeric(difftime(enddate_government,parliament_entry, units = "days")))

written_questions_constituencies$seniority
written_questions_constituencies$`Health deprivation and disability`

model4 <- glm(economic_welfare.factor ~ seniority, data = written_questions_constituencies, family = binomial)


model_all <- glm(economic_welfare.factor ~  askingMember$party  + gender + seniority + Income + `Health deprivation and disability`, data = written_questions_constituencies, family = binomial)

stargazer::stargazer(model_all, type = "text")


######### working with UK Geodata for constituencies #######

library(sf)
library(tmap)

# load data
uk_constituencies <- sf::read_sf("Data_for_Data_Scientists/Final-Assignment-my472/UK constituency data/WPC_Dec_2018_FCB_UK.shp")

plot(uk_constituencies)

geo_data_written_questions_constituencies <- left_join(written_questions_constituencies,uk_constituencies, by = c("ONSConstID", "pcon18cd"))


written_questions_constituencies$ONSConstID
uk_constituencies$pcon18cd

# PLAN

# select relevant variables from oral and written questions
# add variable indicating wether oral or written
# merge oral and written questions
# create unique vector with names 
# get members data with function 
# merge all data together in one final dataset
# get information on MPs from external datasets


# Idea for presentation 
# do something with constituencies and map them
# get data for each constituency's economic status (e.g. vulnerabilities) --> are members from vulnerable communities more likely to ask economic questions?
# OR: get data on MPs registered interests and check if they are more likely to have related interests
# check committees/ position congruence


## upload committees data 


committees <- readxl::read_excel("Data_for_Data_Scientists/Final-Assignment-my472/committees.xlsx")
committees <- committees %>% rename( "askingMemberId"= "mnis_id")


members_committees <- left_join(members_data_final, committees, by = "askingMemberId")



### sort committees into economics and health/ welfare and other and check congruence 
## 

table(members_committees$committee_name)


### Economy Question:###

# Department for Communities and Local Government
# Treasury 
# Department for International Trade
# HM Treasury
# Department for Business, Energy and Industrial Strategy

# Committees
# Business and Trade Committee
# Treasury Committee
# Work and Pensions Committee
# Finance Committee
# Levelling Up, Housing and Communities Committee
# Economic Affairs Committee



### Health and Welfare Questions ###

# Department of Health
# Department of Health and Social Care
# Women and Equalities
# Department for Work and Pensions

# Women and Equalities Committee
# Health and Social Care Select Committee



members_committees <- members_committees %>% mutate(econ_welfare_committee = case_when(committee_name %in% 
                                                                                         c("Business and Trade Committee", 
                                                                                           "Treasury Committee",
                                                                                           "Finance Committee", 
                                                                                           "Levelling Up, Housing and Communities Committee", 
                                                                                           "Economic Affairs Committee") ~ "economics",
                                                                                       committee_name %in% 
                                                                                         c("Health and Social Care Committee", 
                                                                                           "Women and Equalities Committee") ~ "welfare",
                                                                                       TRUE ~ "other"))

table(members_committees$econ_welfare_committee)


members_committees <- members_committees %>% mutate(topical_congruence = case_when((economic_welfare == "economics" & econ_welfare_committee == "economics") | 
                                                                                     (economic_welfare == "welfare" & econ_welfare_committee == "welfare") ~ 1, 
                                                                                   TRUE ~ 0))

