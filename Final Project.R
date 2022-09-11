#Final Project
#Xilin Yang
#The goal of this project is to evaluate the effect of lockdowns, rate the effectiveness of all 2020 pandemic policies, and investigate their correlations with the total number of cases.
library(tidyverse)
library(haven)
library(rvest)
library(lubridate)
library(dplyr)
library(USAboundaries)
library(usmap)
library(shiny)
library(plotly)
library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(readxl)
library(rvest)

#setwd("/Users/xilinyang/Documents/GitHub/final-project-xilin-yang")

#Part 1. We want to investigate the correlation between the length of lockdown and total number of cases in 2020.
#Numerous lockdowns took place in the first half year of 2020: "https://ballotpedia.org/States_that_issued_lockdown_and_stay-at-home_orders_in_response_to_the_coronavirus_(COVID-19)_pandemic,_2020"
get_url <- function(u){
  url_u <- u
  request_u <- read_html(url_u)
}

request <- get_url("https://ballotpedia.org/States_that_issued_lockdown_and_stay-at-home_orders_in_response_to_the_coronavirus_(COVID-19)_pandemic,_2020")

table <- request %>%
  html_node("table.marqueetable") %>%
  html_table(header = FALSE)
table <- table[-c(1,2), ]

lockdown <- table %>% 
  rename(State = X1) %>% 
  rename(Dates = X2) %>% 
  select(State, Dates)
lockdown$State <- state.abb[match(lockdown$State,state.name)]

#Clean the column "Dates": remove brackets and calculate lockdown periods
lockdown$Dates <- gsub("\\[.*?\\]", "", lockdown$Dates)
lockdown$Dates <- str_split(lockdown$Dates, "- ", n = 2, simplify = FALSE)
lockdown <- lockdown %>% 
  unnest(Dates)
lock <- lockdown %>% 
  group_by(State) %>% 
  mutate(id = paste0("Date", row_number())) 
lock$Dates <- as.Date(lock$Dates,format='%B %d')
lock <- lock %>% 
  pivot_wider(values_from = Dates,
              names_from = id)

#Change the dates into datetime format
lock$Date1 <- as.Date(lock$Date1,format='%B %d')
lock$Date1 <- as.Date(lock$Date1,format='%B %d')

lock <- lock %>% 
  mutate(period = Date2 - Date1) %>% 
  rename(state = State)

#we download a filtered data set showing the total reported cases up till 12/31/2020.
cases <- read.csv("2020 Cases.csv")

#Join the two sets
merge1 <- right_join(x = cases, y = lock, by = "state")

#Is there a relationship between the length of lockdown of the total number of reported cases?
ggplot(merge1, mapping = aes(x = period, y = tot_cases)) +
  geom_point() +
  geom_smooth(method=lm)

#The standard error is very large. So there is a weak positive relationship between the two.
cor(as.numeric(merge1$period), merge1$tot_cases,  method = "pearson", use = "complete.obs")
cor(as.numeric(merge1$period), merge1$tot_death,  method = "pearson", use = "complete.obs")

#What could be the reason for this kind of weak positive correlation?
#1) Long Lockdowns generally take place in states with the most serious pandemic situation.
#2) Lockdown may only be useful in a limited time period.

#Which state has the longest lockdown? (The following code is inspired by https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html)
lock %>% 
  arrange(desc(period)) #NM, CA, NY, KY, and OR has the longest lockdown.

#MA, NY, HI, CA, an GA has the highest rating in policy effectiveness.
lock <- lock %>% 
  mutate(period = ifelse(is.na(period), 0, period))

plot_usmap(data = lock, 
           values = "period", 
           color = "pink") + 
  scale_fill_continuous(name = "COVID-19 lockdown Period Length by State") + 
  theme(legend.position = "right")

#Part 2. We review a second study on Covid policy effectiveness(https://www.nature.com/articles/s41562-020-01009-0#Abs1), 
#Using this scale, we want to rate the effectiveness of pandemic policies state by state. 
#Eventually, we want to compare the correlation between a states' efficiency "score" and total number of cases.
req2 <- get_url("https://www.nature.com/articles/s41562-020-01009-0/tables/1")
table2 <- req2 %>%
  html_node("table.data.last-table") %>%
  html_table(header = FALSE)

names(table2) <-  unlist(table2[1, ]) #replace original column names
table2 <- table2[-1, ]
table2 <- table2 %>% 
  select(`L2 category`, `Score (%)`) %>% 
  mutate(`Score (%)` = as.numeric(`Score (%)`)/100)
score <- table2 %>% 
  pivot_wider(names_from = `L2 category`,
              values_from = `Score (%)`)

#What policies did each state take in the second half year of 2020?
policies <- read_dta("final_state_month_covid_npi.dta")
policy <- policies %>% 
  filter(year == 2020)

#Compute the scores by state
names(table2) <-  unlist(table2[1, ]) #replace original column names

#In this case, we treat "stay at home order" as Individual movement restrictions, and neglect business/restaurant/bar closures since there are no exact matches.
new_policy <- policy %>%
  rename(`Mass gathering cancellation` = largegatheringsban,
        `Quarantine` = mandatoryquarantinefortravelers,
         `Individual movement restrictions` = stayathomeorder,
        `Closure of educational institutions` = schoolclosures) %>% 
  filter(state != "")

#Assign effectiveness rating to policies related to movement restrictions;
intersection <- intersect(names(new_policy), names(score))
rate <- mapply("*", new_policy[intersection], score[intersection])
rate_policy <- cbind(new_policy[c("state", "year", "month")], rate)
rate_policy <- rate_policy %>% 
  rowwise %>% mutate(overall = sum(`Individual movement restrictions`, `Quarantine`, `Mass gathering cancellation`,`Closure of educational institutions`, na.rm = TRUE))

rate_policy <- rate_policy %>% 
  group_by(state, year) %>% 
  summarize(overall = sum(overall, na.rm = TRUE))

#Which state has the most "effective" Covid policies? (The following code is inspired by https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html)
rate_policy %>% 
  arrange(overall)

#MA, NY, HI, CA, an GA has the highest rating in policy effectiveness.
plot_usmap(data = rate_policy, 
           values = "overall", 
           color = "gold") + 
  scale_fill_continuous(name = "COVID-19 Policy Effectiveness Rating by State") + 
  theme(legend.position = "right")

#Are these policies actually effective? Let's take a look again at the 2020 total Covid cases.
merge2 <- right_join(x = cases, y = rate_policy, by = "state")
merge2 <- merge2 %>% 
  select("submission_date", "state", "tot_cases", "new_case", "tot_death", "new_death", "year", "overall")

#Correlation:
ggplot(merge2, mapping = aes(x = overall, y = tot_cases)) +
  geom_point() +
  geom_smooth(method=lm)

cor(merge2$overall, merge2$tot_cases,  method = "pearson", use = "complete.obs")
cor(merge2$overall, merge2$tot_death,  method = "pearson", use = "complete.obs")
cor(merge2$overall, merge2$new_case,  method = "pearson", use = "complete.obs")
cor(merge2$overall, merge2$new_death,  method = "pearson", use = "complete.obs")

#What could be the reason, again, for all the weak positive correlations?
#1) Policies are subjected to response lags.
#2) States that take more measures to combat Covid tend to have a much larger population and more serious pandemic challenges.
#3) These measures can only alleviate but cannot cure, because the U.S. adopts a mitigation strategy (mitigating Covid effects instead of eradicating Covid).

#Finally, we want to create a Shiny App to show the length of lockdown and the effectiveness of Covid policies by state.

#Shiny App: showing state-by-state Covid policy effectiveness rating
#Merge so that we can see all policies within 2020
merge3 <- left_join(lock, merge2, by = "state")
merge3 <- merge3 %>% 
  select(state, period, submission_date, tot_cases, tot_death, year, overall)

#ShinyApp showing total cases, the lockdown period length, and Map the Effectiveness Rating state by state.
ui <- fluidPage(
  selectInput(inputId = "st",
              label = "Choose a state",
              choices = merge3$state),
  plotlyOutput("ts"),
  tableOutput("state_disp")
)

server <- function(input, output) {
  state <- reactive({input$st})
  data <- reactive({
    filter(merge3, state == state())
  })
  plt <- reactive({plot_usmap(data = merge3, 
                              values = "overall", 
                              color = "gold") + 
      scale_fill_continuous(name = "COVID-19 Policy Effectiveness Rating by State") + 
      theme(legend.position = "right")})
  output$ts <- renderPlotly(plt())
  
  output$state_disp <- renderTable({data()})
}

shinyApp(ui = ui, server = server)

#See the following link for the shiny app publication: https://xiliny.shinyapps.io/final-project-xilin-yang/

#Supplemental Section: Text Analysis
#The txt file is a Washington Post Article: https://www.washingtonpost.com/health/coronavirus-is-harming-the-mental-health-of-tens-of-millions-of-people-in-us-new-poll-finds/2020/04/02/565e6744-74ee-11ea-85cb-8670579b863d_story.html
#Parse it using natural language processing:
mental <- read_file("/Users/xilinyang/Documents/GitHub/final-project-xilin-yang/Covid Mental Health.txt")
parsed <- udpipe(mental, "english")
parsed %>% 
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  group_by(lemma) %>%
  count(sort = TRUE)

parsed$stem <- wordStem(parsed$token, language = "porter")
view(select(parsed, "token", "stem", "lemma", "upos"))

#Parse it into word tokens for sentiment analysis
new_text <- tibble(text = mental)
word_tokens_new <- unnest_tokens(new_text, word_tokens, text, token = "words")
new1 <- anti_join(word_tokens_new, stop_words, by = c("word_tokens" = "word"))
view(new1)

for (s in c("nrc", "afinn", "bing")) {
  new1 <- new1 %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

get_sentiments()

#Sentiment Analysis
ggplot(data = filter(new1, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count", fill = "#FF6666") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Sentiment Analysis for the Washington Post Article")
#The text has a lot of negative messages.

#Plot the Afinn Analysis
ggplot(data = filter(new1, !is.na(afinn))) +
  geom_histogram(aes(afinn), stat = "count", fill = "blue") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Afinn for the Washington Post Article") +
  xlim(-3, 3)

