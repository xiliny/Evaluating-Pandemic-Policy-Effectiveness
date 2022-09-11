# Data and Programming for Public Policy II
# PPHA 30536

## Final Project: Reproducible Research
## Winter 2022

#Xilin Yang 
#This project is my own work. No other students participated in this study.
#The goal of this project is to evaluate the effectiveness of 2020 pandemic policies, and its correlation with citizens' subjective well-being. This project will pay a special attention to lockdown policies. The study consists of three parts.

Part I. Lockdown policies (2 datasets used)
From line 22 - line 94, we analyze the lockdown policies across the US in the first half of year 2020. We proceed with the following steps:
1) Web scrape a table from the internet that presents start dates and end dates of lockdowns in each state.
2) Clean the data, transform the dates into the "datetime" format, and calculate the length of the lockdown period state by state.
3) Graph the state-by-state lockdown period on a static US map.
4) Download a filtered data set from the U.S. census bureau to check state-by-state total Covid-19 cases up till 12/31/2020.
5) Check if there is a correlation between the length of lockdown and the total number of cases. Provide an explanation for the correlation coefficient.

Part II. Rating the Effectiveness of Covid-19 policies in the second half of year 2020. (2 more datasets introduced)
See line 99 - line 203.
1) We review a second study on Covid policy effectiveness(https://www.nature.com/articles/s41562-020-01009-0#Abs1). This study gives a score for each Covid policy and puts all the scores in a table. We web scrape the table and clean it up side down to ensure that we only have one row (so that we can proceed with multiplication in Step #4).
2) We obtain a data set quantifying Covid policies state by state. We filter it so that we only have the data for year 2020.
3) Because the names of policies do not exactly match, we only focus on four policies in this data set: Mass gathering cancellation, quarantine, Individual movement restrictions (we treat it as equivalent to the stay at home order), and the closure of educational institutions. We clean the data sets to make sure that the names of these policies match.
4) Because we have produced a table with only one row in Step #1, we now multiply all values to rate the effectiveness of a certain policy for a given state in a given month. Then, we add these values to obtain the total score for a month, then add values again to obtain the total effectiveness score for each state in the second half year of 2020. We check if there is any correlation between the rating and the total number of cases.
5) We plot the Covid effectiveness on a static map. 
6) We then plot the Effectiveness Rating on ShinyApp, and include an interactive table below the map to show the selected state name, length of lockdown, total cases in 2020, and the rated score. The link to this ShinyApp map is https://xiliny.shinyapps.io/final-project-xilin-yang/

Part III. Supplemental Text Analysis.
See line 207 and afterward.
1) We process text analysis for an txt file. The txt file is a Washington Post Article: https://www.washingtonpost.com/health/coronavirus-is-harming-the-mental-health-of-tens-of-millions-of-people-in-us-new-poll-finds/2020/04/02/565e6744-74ee-11ea-85cb-8670579b863d_story.html.
2) We run a sentiment analysis of this file.

Conclusion:
1. Up till this stage, I have used four datasets. I have cleaned, merged, and reshaped all of them using skills built from Homework1.
2. I have created two static plots using ggplot, and one interactive Shiny plot shared on https://xiliny.shinyapps.io/final-project-xilin-yang/
3. I introduced text analysis, similar to that of homework 3.
4. This project includes basic correlation analysis and reports the results of the analysis.

As mentioned, this project aims to rate policy effectiveness state by state, and explore the potential relationship between policy effectiveness and the number of cases. We also explored the correlation between the length of lockdown and the number of cases. The result is that both policy effectiveness rating and lockdown length have weak positive correlation with the number of cases. What could be the reason for them? My explanation is that, first, policies are subjected to response lags. We don't immediately see cases dropping after lockdowns. Second, states that take more measures to combat Covid tend to have a much larger population and more serious pandemic challenges. Third, because most states want to preserve economic development, they use a mitigation strategy (mitigating Covid effects instead of eradicating Covid). Hence, the policies can only alleviate the situation but cannot eliminate all cases at once.
