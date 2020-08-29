#Load libraties
library(janitor)
library(rvest)
library(tidyverse)
library(plotly)
library(xts)
library(reshape2)
library(lubridate)

##### THE FUNCTIONS BELOW SCRAPE, CLEAN AND BUILD A DATABASE OF COVID-19 IN NIGERIA.
##### DETAILED CODE ARE GIVEN BENEATH IT.

#Get yesterday's date configured FOR the web-scraping function
mydate = today()
ddate = mydate -1

#### Function to use for daily scraping...
Aug27 = td(Aug27)

#Convert strings to numeric
#Call the function numb_convert to convert them
Aug27 = numb_convert(Aug27)

#To get new cases, we extract and manipulate the previous day data
#Also clean names for Report_Date
A27 = Aug27
Aug27 = nd(n = A27, m = A26)

#Bind the new df to the complete data (Total.States)
# First convert date column  of the new df to factor
Aug26$Date = as.factor(Aug26$Date)
total.states = rbind(total.states, Aug27)




###### DETAILS BELOW

#Scrape Data from the web, clean names, add_column with function td(date)
td = function(x){
  y = read_html("https://covid19.ncdc.gov.ng/") %>% 
    html_table(fill = TRUE)
  z = y[[1]]
  x = z %>% clean_names() %>% add_column(Date = ddate) %>% select(Date, everything()) %>% 
    rename(no_of_active_cases = no_of_cases_on_admission)
  return(x)
}

# Convert string to numeric
#The function detail is described below
numb_convert = function(x) {
  x$no_of_cases_lab_confirmed = as.numeric(gsub(",","", x$no_of_cases_lab_confirmed))
  x$no_of_active_cases = as.numeric(gsub(",", "", x$no_of_active_cases))
  x$no_discharged = as.numeric(gsub(",", "", x$no_discharged))
  x$no_of_deaths = as.numeric(gsub("", "", x$no_of_deaths))
  
  return(x)
}
#Optional - export to csv
write.csv(Aug18, "Aug18.csv")

##### Join current data with previous day data to get daily increase, then build a comprehensive data
##### The function below generate the date called out from the above line
nd = function(x, m, n){
  cur_day = n
  prev_day = m
  y  = left_join(m, n, by = "states_affected")  
  z = y %>% group_by(states_affected) %>% 
    transmute(New.Cases = no_of_cases_lab_confirmed.y - no_of_cases_lab_confirmed.x,
              Daily.Discharge = no_discharged.y - no_discharged.x,
              Daily.Death = no_of_deaths.y - no_of_deaths.x)
    x = z %>% left_join(n, z, by = "states_affected") %>% 
    rename(State = states_affected,
           Total.Cases = no_of_cases_lab_confirmed,
           Active.Cases = no_of_active_cases,
           Discharges = no_discharged,
           Deaths = no_of_deaths,
           Daily.Discharges = Daily.Discharge,
           Daily.Deaths = Daily.Death) %>%
    select(Date, State, 
           Total.Cases, 
           Active.Cases, 
           Discharges, 
           Deaths, 
           everything()) %>%
      tibble()
    x$Date = as.factor(x$Date)
  
  return(x)
}

#### For current analysis, convert date to factor
Aug26$Date = as.factor(Aug26$Date)

#### Bind new dataframe to the data table
total.states = rbind(total.states, Aug27)



#Get total of months
daily.data = total.states %>% group_by(Date) %>% 
  summarize(daily.total = sum(Total.Cases),
           daily.active = sum(Active.Cases),
           daily.death = sum(Deaths),
           daily.discharge = sum(Discharges),
           daily.new.cases = sum(New.Cases),
           daily.new.discharge = sum(Daily.Discharges),
           daily.new.death = sum(Daily.Deaths))


#Plot for specific states
########CALL THE FUNCTION
a = plot_new_cases(a = "Kano")
state_trend = ggfun(dat = a, x.var = Dates, y.var = Daily_Numbers, Patients_Status )

ggplotly(state_trend)



######DEFINE THE FUNCTION CALLED ABOVE
plot_new_cases = function(a, final) {
  place = total.states %>% filter(State == a)
  place = place %>% select(Date, New.Cases, Daily.Discharges, Daily.Deaths)
  final = xts(place[,-1], order.by = as.Date(place[,1]))
  final = data.frame(index(final), stack(as.data.frame(coredata(final))))
  
  names(final)[1] = "Dates"
  names(final)[2] = "Daily_Numbers"
  names(final)[3] = "Patients_Status"
  
  return(final)
}






kano = total.states %>% filter(State == "Kano")
lagos = ntotal %>% filter(State == "Lagos")
abuja = total.states %>% filter(State == "FCT")
oyo = total.states %>% filter(State == "Oyo")
rivers = total.states %>% filter(State == "Rivers")
edo = total.states %>% filter(State == "Edo")

lplot = FCT %>% select(Date, New.Cases, Daily.Discharges, Daily.Deaths)

#Create an xts object
lplot = xts(lplot[,-1], order.by = as.Date(lplot[,1]))

#Need to stack the coredata (the observations) by the index(date)
#New dataframe will result
lplot = data.frame(index(lplot), stack(as.data.frame(coredata(lplot))))

#Change name to easy to remember names
names(lplot)[1] = "Dates"
names(lplot)[2] = "Daily_Numbers"
names(lplot)[3] = "Patients_Status"


# USING FUNCTION TO PLOT AND ATTACH PLOTLY

ggfun <- function(dat, x.var, y.var, z.var){
  x.var <- enquo(x.var)
  y.var <- enquo(y.var)
  z.var <- enquo(z.var)
  ggp <- ggplot(data = dat,
                aes(x = !! x.var,
                    y = !! y.var,
                    color = !! z.var)) +
    geom_line() +
    ggtitle("Daily Trend of Covid19 in Lagos (March - July)") + 
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text("red", size = 15),
          axis.text.x = element_text("red", size = 15),
          axis.title.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line = element_line(colour = "black", size = 1),
          plot.title = element_text(lineheight = .8, face = "bold"),
          legend.position = "top")
  
  
  return(ggp)
}
##########Plotting selected states




#THE OLD WAY OF PLOTTING WHICH DIDN'T REQUIRE FUNCTION

l = ggplot(lplot, aes(x = Dates, y = Daily_Numbers, 
                         color = Patients_Status)) + geom_line()
l = l + ggtitle("Daily Trend of Covid19 in Lagos (March - July)")
l = l + theme(axis.title.x = element_blank(),
                axis.text.y = element_text("red", size = 15),
                axis.text.x = element_text("red", size = 15),
                axis.title.y = element_blank(),
                panel.background = element_rect(fill = "white"),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                axis.line = element_line(colour = "black", size = 1),
                plot.title = element_text(lineheight = .8, face = "bold"),
                legend.position = "top")
ggplotly(o)
ggplotly(a)
ggplotly(l)
ggplotly(e)

grid.arrange(l, a, o, e, nrow = 2, ncol = 2)


  #Note
j8$no_of_cases_lab_confirmed[j8$states_affected == 'Kano'] <- 1004

saveRDS(daily.data, file = "dailyCovid.rds")

grid.arrange()



ggfun <- function(dat, x.var, y.var, z.var){
  x.var <- enquo(x.var)
  y.var <- enquo(y.var)
  z.var <- enquo(z.var)
  ggp <- ggplot(data = dat,
                aes(x = !! x.var,
                    y = !! y.var,
                    color = !! z.var)) +
    geom_line() 
  
  return(ggp)
}

 stateplot = ggfun(dat = lplot, x.var = Dates, y.var = Daily_Numbers, Patients_Status )
 
 
 
