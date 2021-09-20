#Data expression through R

###Data
##The data used here is from the [Bureau of Transportation Statistics].

#load packages
library(statsr)
library(dplyr)
library(ggplot2)

#Store data into variable
dat <- nycflights

#View summary of the data
head(dat)

##Analysis

#display distribution of all delayed flights through histogram
ggplot(data=dat, aes(x=dep_delay)) + geom_histogram()

#adjust bindwidth for better viewing
ggplot(data=dat, aes(x=dep_delay)) + geom_histogram(binwidth=50)

#data is right skewed which means the median is possibly a better indicator
#for a summary and lower than the mean value.

#find departure delays for flgihts headed to RDU only and display histogram
#data is expected to be right skewed, median < mean, median better for summary
rdu_flights <- dat %>% filter(dest == 'RDU')
ggplot(data=rdu_flights, aes(x=dep_delay)) + geom_histogram()

#display mean, median, sd, and sample size
rdu_flights %>% summarise(mean_dd = mean(dep_delay),
                          median_dd = median(dep_delay),
                          sd_dd = sd(dep_delay),
                          n = n())

#filter on multiple criteria: flights to SFO on February
sfo_feb_flights <- dat %>% filter(dest == 'SFO',
                                  month == 2)

#create a histogram and calculate summary statistics for arrival delays of sfo
ggplot(data=sfo_feb_flights, aes(x=arr_delay)) + geom_histogram(binwidth = 20)

sfo_feb_flights %>% summarise(mean_dd = mean(arr_delay),
                              median_dd = median(arr_delay),
                              sd_dd = sd(arr_delay),
                              n = n())

#get summary stats for delays from each origin airport
dat %>% group_by(origin) %>% summarise(mean_dd = mean(dep_delay),
                                       median_dd = median(dep_delay),
                                       sd_dd = sd(dep_delay))

#calculate the median and IQR for arr_delay in SFO flight grouped by carrier

sfo_feb_flights %>% group_by(carrier) %>% summarise(median_dd = median(arr_delay),
                                                    IQR_dd = IQR(arr_delay))

#which month has the highest average delay departing from NYC airport

ny_flights <- dat %>% filter(origin == 'JFK' | origin == 'LGA')
ny_flights %>% group_by(month) %>% summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))

#which month has the highest median departuyre from NYC airport
ny_flights %>% group_by(month) %>% summarise(median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))

#display distribution of departure delays across months using box plots
ggplot(data=dat , aes(x=factor(month), y=dep_delay)) + geom_boxplot()

#which three major NY airports has best on time departure rate 

dat['status'] <- ifelse(dat['dep_delay'] < 5,'on time', 'delayed') 
nyc_status_flights <- dat %>% group_by(origin) %>% summarise(
  ot_dep_rate = sum(status == 'on time') / n()) %>% arrange(desc(ot_dep_rate))
head(nyc_status_flights)

#create a column that includes the avg speed of each plane
dat['speed'] <- dat['distance'] / (dat['air_time']/60)

#create scatterplot of avg_speed vs distance
ggplot(data = dat, aes(x=distance, y=speed)) + geom_point()
