### Economy Question:###

# Treasury 
# Department for International Trade
# HM Treasury
# Department for Business, Energy and Industrial Strategy


### Health and Welfare Questions ###

# Department of Health
# Department of Health and Social Care
# Women and Equalities
# Department for Work and Pensions

# Department for Communities and Local Government


##### written and oral questions ####

library(jsonlite)
library(tidyverse)


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


written_data$MP_name <- written_data$askingMember$listAs
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

oral_questions_data <- oral_questions_data %>% select(c(id,askingMemberId, MP_name, dateTabled, dateAnswered, uin, questionText,answeringBodyName,oral_written))


### merge oral and written questions data 

all_questions_data <- rbind(written_questions_data, oral_questions_data)



all_questions_topical <- all_questions_data  %>% filter(answeringBodyName %in% c('Treasury','Department for International Trade', 'HM Treasury','Department for Business, Energy and Industrial Strategy', 'Department of Health', 'Department of Health and Social Care', 'Women and Equalities','Department for Work and Pensions'))

all_questions_topical <- all_questions_topical %>% 
  mutate(economic_welfare = case_when(
    answeringBodyName %in% c('Treasury', 'Department for International Trade', 'HM Treasury', 'Department for Business, Energy and Industrial Strategy') ~ "economics",
    answeringBodyName %in% c('Department of Health', 'Department of Health and Social Care', 'Women and Equalities', 'Department for Work and Pensions') ~ "welfare" ))

### identifying factors: 

# MP characteristics 

###############################
#### getting additional MP data
##############################


Askingmembers <- all_questions_topical$MP_name

members_url <- "https://members-api.parliament.uk/api/Members/Search?Name="


library(stringr)

# Extracting the first word from each string
data_members_first_words <- str_extract(Askingmembers, "[^,\\s]+")

data_members_first_words <- unique(data_members_first_words) # attention I am not getting all with the same name



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


### merge members and questions data together


questions_merged <- left_join(all_questions_topical, members_data_final, by = "askingMemberId")
questions_merged <- unique(questions_merged)

questions_merged$economic_welfare.factor <- as.factor(questions_merged$economic_welfare)

levels(questions_merged$economic_welfare.factor) <- c("0", "1")

questions_merged$economic_welfare.factor <- factor(questions_merged$economic_welfare.factor,
                                                   levels = c(0,1),
                                                   labels = c("economics", "welfare"))

levels(questions_merged$economic_welfare.factor)

questions_merged$latestParty$name



##### plot counts ###

questions_merged %>% group_by(economic_welfare) %>%
  count()

## Logit regression 

levels(questions_merged$gender)

model1 <- glm(economic_welfare.factor ~ latestParty$name+ gender, family = binomial, data = questions_merged)

stargazer::stargazer(model1, type = "text")

?stargazer()
?glm()

questions_merged$latestParty


questions_merged$oral_written

##### calculate time until answer 
# using: dateForAnswer and dateTabled



# Load the lubridate package
library(lubridate)


questions_merged$dateTabled <- ymd_hms(questions_merged$dateTabled)
questions_merged$dateAnswered <- ymd_hms(questions_merged$dateAnswered)


questions_merged <- questions_merged %>% mutate(answering_time = as.numeric(difftime(dateAnswered,dateTabled, units = "days")))


questions_merged$answering_time


model2 <- lm(answering_time ~ economic_welfare.factor, data = questions_merged)

questions_merged %>% group_by(economic_welfare) %>%
  summarize(mean_answering_time = mean(answering_time, na.rm = TRUE))



questions_merged %>% group_by(dateTabled, economic_welfare, oral_written) %>%
  summarize(answering_time)


questions_merged %>% group_by(dateTabled, economic_welfare, oral_written) %>%
  summarize(answering_time) %>% ggplot(aes(x = dateTabled, y = answering_time, fill= economic_welfare, color = economic_welfare)) +
  geom_col() +
  scale_color_manual(values = c("lightblue","darkgreen")) +
  scale_fill_manual(values = c("lightblue", "darkgreen")) +
  facet_wrap(~ oral_written)+ 
  theme_bw() +
  ylab("Days until Answer") +
  xlab("Question ID")

?scale_color_manual()
str(questions_merged$answering_time)

questions_merged %>% group_by(gender, economic_welfare) %>%
  count()

#### deprivation of constituencies ###

deprivation <- readxl::read_excel("Desktop/Data_for_Data_Scientists/Final-Assignment-my472/deprivation-dashboard.xlsx", 
                                  sheet = "Data constituencies")

questions_merged$latestHouseMembership$membershipFrom

ConstituencyName <- questions_merged$latestHouseMembership$membershipFrom

questions_merged$ConstituencyName <- ConstituencyName

# merge both datasets 

written_questions_constituencies <- left_join(questions_merged,deprivation, by = "ConstituencyName")

written_questions_constituencies$Income

model2 <- glm(economic_welfare.factor ~ Income, data = written_questions_constituencies, family = binomial)

written_questions_constituencies$Income


# Load the lubridate package
library(lubridate)

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
written_questions_constituencies$health_index <- written_questions_constituencies$`Health deprivation and disability`
written_questions_constituencies$party <- written_questions_constituencies$latestParty$name


model4 <- glm(economic_welfare.factor ~ seniority, data = written_questions_constituencies, family = binomial)


model_all <- glm(economic_welfare.factor ~ as.factor(party) + gender + seniority + Income + health_index, data = written_questions_constituencies, family = binomial)

stargazer::stargazer(model_all, type = "text")


coefficients(model_all)
residuals(model_all)

sjPlot::plot_model(model_all, "pred")


?plot_model()

health <- ggeffects::ggemmeans(model_all, "health_index[all]")
income <- ggeffects::ggemmeans(model_all, "Income[all]")  
gender <- ggeffects::ggemmeans(model_all, "gender") 
party <- ggeffects::ggemmeans(model_all, "party") 
seniority <- ggeffects::ggemmeans(model_all, "seniority[all]") 


health %>% ggplot(aes(x=x, y = predicted)) + geom_line() + geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)

income %>% ggplot(aes(x=x, y = predicted)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_bw() +
  
  gender %>%  ggplot(aes(x= x, y = predicted)) + geom_bar(stat = "identity", position = "dodge", color = "black") 




#### table with descriptives 

library(xtable)
library(vtable)

sumtable(questions_merged, vars = c('oral_written', 'seniority', 'party', 'gender'),
         labels=labels,
         summ = c('notNA(x)', 'countNA(x)', 'mean(x)',   'sd(x)', 'min(x)', 'pctile(x)[25]', 'pctile(x)[75]', 'max(x)'),
         #summ.names = summnames,
         title='Summary Statistics:')
# out = 'latex', 
#file='UK1.tex')

?ggpredict()

written_questions_constituencies %>% group_by(oral_written) %>%
  count()

written_questions_constituencies %>% group_by(party) %>%
  count()

written_questions_constituencies %>% group_by(seniority) %>%
  count()

### calculating marginal effects ##

table(written_questions_constituencies$answeringBodyName)


######### working with UK Geodata for constituencies #######

library(sf)
library(tmap)

# load data


uk_constituencies <- sf::read_sf("Desktop/Data_for_Data_Scientists/Final-Assignment-my472/UK_constituency data 2/WPC_Dec_2018_GCB_GB.shp")

uk_constituencies <- uk_constituencies %>% rename(ConstituencyName = pcon18nm)

geo_data_written_questions_constituencies <- left_join(uk_constituencies, written_questions_constituencies, by = "ConstituencyName")


geo_data_written_questions_constituencies <- geo_data_written_questions_constituencies %>% select(c(health_index,Income, geometry, ConstituencyName))

geo_data_written_questions_constituencies <- unique(geo_data_written_questions_constituencies)

constituency_questions <- written_questions_constituencies %>% group_by(ConstituencyName, economic_welfare) %>%
  count()

geo_data_written_questions_constituencies <- left_join(geo_data_written_questions_constituencies, constituency_questions, by = "ConstituencyName")


geo_data_written_questions_constituencies$n <- geo_data_written_questions_constituencies$n %>% replace_na(0)

#geo_data_written_questions_constituencies$economic_welfare <- geo_data_written_questions_constituencies$economic_welfare %>% replace_na("neither")


geo_data_written_questions_constituencies <- geo_data_written_questions_constituencies %>% select(c(geometry, economic_welfare, n, ConstituencyName, health_index, Income ))

ggplot() + geom_sf(data = geo_data_written_questions_constituencies, aes(fill = n))

geo_data_simplified <- st_simplify(geo_data_written_questions_constituencies, preserveTopology = FALSE, dTolerance = 1000)

library(pacman)

tmap_mode("plot")



filtered_data <- geo_data_written_questions_constituencies %>%
  filter(!is.na(economic_welfare))


tm_shape(geo_data_written_questions_constituencies) + tm_polygons() +
  tm_shape(filtered_data) +
  tm_fill("n", breaks = c(0, 1, 2, 3, 4, 5, 10, 20), palette = "Blues", title = "n") +
  tm_facets(by = "economic_welfare", drop.empty.facets = TRUE)



tm_shape(geo_data_written_questions_constituencies) + tm_polygons() +
  tm_shape(geo_data_written_questions_constituencies$economic_welfare == "economic") + 
  tm_fill("n", breaks = c(0, 1, 2, 3, 4, 5, 10, 20), palette = "Blues", title = "Number of Questions from MP") +
  tm_shape(geo_data_written_questions_constituencies$economic_welfare == "welfare") + 
  tm_fill("n", breaks = c(0, 1, 2, 3, 4, 5, 10, 20), palette = "Blues", title = "n") 


tm_shape(geo_data_written_questions_constituencies) +
  tm_fill("health_index", style = "cont", palette = "Blues")





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

