---
title: "Reproducible Res. Assignment1"
output: html_document
keep_md: true 
---






1. Downloading data and reading it using read.csv():

```r
act.dat <- read.csv("activity.csv", header = T)
summary(act.dat)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
### list missings by dates
n.miss.by.date <- act.dat %>%
                  group_by(date) %>%
                  summarise(na_count = sum(is.na(steps)==T)) %>%
                  filter(na_count>0)
n.miss.by.date
```

```
## # A tibble: 8 x 2
##   date       na_count
##   <fct>         <int>
## 1 2012-10-01      288
## 2 2012-10-08      288
## 3 2012-11-01      288
## 4 2012-11-04      288
## 5 2012-11-09      288
## 6 2012-11-10      288
## 7 2012-11-14      288
## 8 2012-11-30      288
```



2. Histogram of the total number of steps taken each day. I use impute steps variable for this histogram plot.


```r
sum.steps.bydate <- act.dat %>%
                    group_by(date) %>%
                    summarise(sum.steps = sum(steps, na.rm=T))
                    
hist(sum.steps.bydate$sum.steps,main="Histogram of Total Steps per Day",col="grey",xlab="Total Steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

3. Mean and median number of steps taken each day


```r
sum.steps.bydate <- act.dat %>%
                    group_by(date) %>%
                    summarise(mean.steps = mean(steps, na.rm=T),
                      median.steps = median(steps, na.rm=T))

sum.steps.bydate
```

```
## # A tibble: 61 x 3
##    date       mean.steps median.steps
##    <fct>           <dbl>        <dbl>
##  1 2012-10-01    NaN               NA
##  2 2012-10-02      0.438            0
##  3 2012-10-03     39.4              0
##  4 2012-10-04     42.1              0
##  5 2012-10-05     46.2              0
##  6 2012-10-06     53.5              0
##  7 2012-10-07     38.2              0
##  8 2012-10-08    NaN               NA
##  9 2012-10-09     44.5              0
## 10 2012-10-10     34.4              0
## # ... with 51 more rows
```

4. Time series plot of the average number of steps taken


```r
par(mar=c(6,4,2,1))

plot(sum.steps.bydate$date,sum.steps.bydate$mean.steps,type="p",pch=19,xlab="Date",ylab="Average Steps",main="Average Steps Taken By Date",xaxt="n")
axis(1, at=sum.steps.bydate$date, labels=sum.steps.bydate$date, las = 2, cex.axis = 0.5)
points(sum.steps.bydate$date,sum.steps.bydate$mean.steps,type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

5. The 5-minute interval that, on average, contains the maximum number of steps


```r
ave.steps.byint <- act.dat %>%
                    group_by(interval) %>%
                    summarise(mean.steps = mean(steps, na.rm=T)) %>%
                    filter(mean.steps==max(mean.steps))
  
ave.steps.byint
```

```
## # A tibble: 1 x 2
##   interval mean.steps
##      <int>      <dbl>
## 1      835       206.
```

6. Code to describe and show a strategy for imputing missing data

We noticed that there are quite many missings in the "steps" variable. We will fill in those missings in steps (named "steps.imput"") by taking the median value of that specific interval. If there is a missing in that specific date, then all missings in steps were in that specific date. Note, this is just a demo and it was not a sophisticated way. 


```r
### fill in missings.  ###
med.step.by.int <- act.dat %>%
                   group_by(interval) %>%
                   summarise(med.int = median(steps, na.rm=T)) %>%
                   as.data.frame()

act.dat$steps.imput <- act.dat$steps

### replace NA as med.step.by.int for the date with missing. note, the act.dat$interval order were exactly matched to med.step.by.int
act.dat$steps.imput <- act.dat$steps
  for( i in 1: length(n.miss.by.date$date)){
    act.dat$steps.imput[as.character(act.dat$date)==as.character(n.miss.by.date$date)[i]] <- med.step.by.int$med.int 
  } ### end for i


summary(act.dat[,c("steps","steps.imput")])
```

```
##      steps         steps.imput 
##  Min.   :  0.00   Min.   :  0  
##  1st Qu.:  0.00   1st Qu.:  0  
##  Median :  0.00   Median :  0  
##  Mean   : 37.38   Mean   : 33  
##  3rd Qu.: 12.00   3rd Qu.:  8  
##  Max.   :806.00   Max.   :806  
##  NA's   :2304
```

7.Histogram of the total number of steps taken each day after missing values are imputed


```r
sum.steps.imput.bydate <- act.dat %>%
                    group_by(date) %>%
                    summarise(sum.steps.imput = sum(steps.imput, na.rm=T))
                    
hist(sum.steps.imput.bydate$sum.steps.imput,main="Histogram of Total Steps (impute) per Day",col="grey",xlab="Total Steps per day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends. We visually scaned the plot, weekend had more steps in time interval between 1000-2000 compared to weekdays.




```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)

ave.step.byint.wkday <- act.dat %>%
                mutate(wkday = wday(as.Date(date),label=TRUE),
                       I.wkend = ifelse(as.character(wkday) %in% c("Sun", "Sat"), "Weekend", "Weekday")) %>%
                group_by(I.wkend,interval) %>%
                summarise(ave.step.byint = mean(steps,na.rm=T)) 

ggplot(ave.step.byint.wkday,aes(x=interval,y=ave.step.byint))+
  geom_line() +
  labs(x = "Time Interval", y = "Average Steps") + 
  facet_wrap(~I.wkend,nrow=2)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
