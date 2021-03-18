##ANOMALY DETECTION IN R
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(zoo)
library(purrr)
library(grid)
library(pBrackets)

## CREATE A LIST OF FILES OF RAINFALL DATA
rain <- list.files(pattern = ".*chirps.*\\.csv$$", recursive = FALSE)

## READ THEM IN AS DATAFRAMES IN A LIST
rainfall <- lapply(rain,read.csv)

## SET THE NAMES OF EACH LIST FROM THE FILENAME
names(rainfall) <- c("ahsun", "ANPUM-LOKLUNG", "anpum_west", "champet_bodo", "dambuk", "nizamghat_east", "nizamghat_west", "yagpo")

## REMOVE ALL NA VALUES
rain_na_omit <- lapply(rainfall,na.omit)

## change the date_time column to POSIX format in all the dataframes
rain_na_omit[[1]]$date_time <- ymd_hms(rain_na_omit[[1]]$date_time)
rain_na_omit[[2]]$date_time <- ymd_hms(rain_na_omit[[2]]$date_time)
rain_na_omit[[3]]$date_time <- ymd_hms(rain_na_omit[[3]]$date_time)
rain_na_omit[[4]]$date_time <- ymd_hms(rain_na_omit[[4]]$date_time)
rain_na_omit[[5]]$date_time <- ymd_hms(rain_na_omit[[5]]$date_time)
rain_na_omit[[6]]$date_time <- ymd_hms(rain_na_omit[[6]]$date_time)
rain_na_omit[[7]]$date_time <- ymd_hms(rain_na_omit[[7]]$date_time)
rain_na_omit[[8]]$date_time <- ymd_hms(rain_na_omit[[8]]$date_time)


## CREATE A TIBBLE
rain_tbl <- lapply(rain_na_omit,as_tibble)


## FUNCTION TO GET DAILY RAINFALL TOTAL FROM DAILY RAINFALL
daily_monthly_rain <- lapply(rain_tbl, function(x){
	
	mutate(x, date_time = date(date_time))															%>%
	group_by(date_time) 																			%>% 
	summarize(value=sum(rainfall_mm)) 																%>% 
    mutate(day=format(date_time,"%d"),month=format(date_time, "%m"),year=format(date_time,"%Y")) 	%>% 
    group_by(day, month,year)																		%>% 
    summarize(total=sum(value)) })

## OBTAIN MONTHLY RAINFALL TOTAL FROM DAILY RAINFALL
monthly_rain <- lapply(rain_tbl, function(x){
	
	mutate(x, date_time = date(date_time))												%>%
	group_by(date_time) 																%>% 
	summarize(value=sum(rainfall_mm)) 													%>% 
    mutate(month=format(date_time, "%m"),year=format(date_time,"%Y")) 					%>% 
    group_by(month,year)																%>% 
    summarize(total=sum(value)) })



## GET LONG TERM MEAN MONTHLY RAINFALL FROM 1981 TO 2019LTmean_join
mean_monthly_rain_1981_2020 <- lapply(monthly_rain, function(x,...){x %>% summarize(mean_monthly = mean(total), SD=sd(total))})

## GET LONG TERM MEAN DAILY RAINFALL FROM 1981 TO 2019
#mean_daily_rain_1981_2020 <- lapply(daily_monthly_rain, function(x,...){x %>% summarize(mean_daily = mean(total), SD=sd(total))})

## FILTER ALL YEARS BEFORE 2003 TO COMPARE THE LONG TERM MEAN 
longtermfilter <- lapply(monthly_rain, function(x,y,...){x %>% filter(year>2011) %>% filter(year<2021)})


## JOIN THE LONG TERM MEAN WITH THE DATA FROM 2009 AND LATER
#LTmean_join <- map2(longtermfilter, mean_monthly_rain_1998_2019, right_join, by="month")

LTmean_join <- map2(longtermfilter, mean_monthly_rain_1981_2020, right_join, by=c("month"))


## CALCULATE A DEPARTURE FROM MEAN +VE VALUES ARE ABOVE NORMAL AND -VE ARE BELOW
departure <- lapply(LTmean_join, function(x,...){x %>% mutate(departure=total-(mean_monthly))})

## CREATE A COLUMN SAYING POSITIVE AND NEGATIVE
departure_cat <- lapply(departure, function(x,...){x %>% mutate(mycolor = ifelse(departure>0, "positive", "negative"))})


##CREATE A NEW COLUMN BY MERGING THE MONTH AND YEAR COLUMNS
departure_cat[[2]]$month_year <- as.yearmon(paste(departure_cat[[2]]$year, departure_cat[[2]]$month), "%Y %m")


## CREATE A NEW COLUMN WITH DATE AS YEAR MONTH AND FIRST DAY
departure_cat[[2]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[2]]$month)],"01", departure_cat[[2]]$year, sep="-"), format = "%b-%d-%Y")


## RENAME THE DATAFRAMES OF THE LIST TO THE LOCATIONS OF THE RAINFALL DATA
names(departure_cat)


## START AND END FOR THE BOUNDING BOX IN THE PLOT
# start1 <- as.Date("2016-07-15")
# start <- as.Date("2015-07-15")

# end <- as.Date("2015-09-15")
# end1 <- as.Date("2016-09-15")


## PLOTTING FUNCTION TO NEST WIHTIN LAPPLY
 						  departure_plot <- function(P,TITLE,...) {ggplot(P) +

## A GEOM RECTANGLES TO SHOW BOXES OF THE RAINFALL
##                                geom_rect(data=x, aes(xmin=start, xmax=end, ymin=-300, ymax=300), 
##								     						   color='white', fill=NA, size=0.05) +  
##                              geom_rect(data=x, aes(xmin=start1, xmax=end1, ymin=-300, ymax=300), 
##                                                             color='white', fill=NA, size=0.05) +

## RIBBON FOR THE STANDARD DEVIATION															  
  				   	   	geom_ribbon(aes(x=date, ymin = mean_monthly - SD, ymax = mean_monthly + SD), fill = "grey70",alpha=0.3) +

## SEGMENTS FOR THE BARS SHOWING total precip
##  			  		geom_segment( aes(x=date, xend=date, y=0, yend=total-departure, color=mycolor), size=1.3) +

						# geom_step(aes(x=date, ymin=mean_monthly - SD, ymax = mean_monthly + SD),size=0.6, color="grey70") +

##						geom_line(aes(x=date,y=total), colour="white", size = 0.3) +

						geom_point(aes(x=date, y=total), colour="yellow", size=0.6) +


## SET THE hrbr THEME   			  	   
theme_ft_rc(axis_title_size = 14, axis_title_face="bold", plot_title_size = 18, axis_title_just = "ct") +

## SET COLOURS FOR THE +VE AND _VE SEGMENTS
  				                               		 scale_color_manual(values=c("#ef8a62", "#67a9cf")) +  

## THEME CONTROLS
theme(axis.ticks.x = element_line(colour = 'grey80', size = 0.3), axis.ticks.length.x = unit(2, "mm"),
axis.ticks.y = element_line(colour = 'grey80', size = 0.3), 
										   axis.ticks.length.y = unit(1, "mm"), legend.position="none") +

## PLOT TITLE
   							    	  labs(title=TITLE, subtitle="Precipitation departure from normal") + 

## X-AXIS TITLE
   							    		  												   xlab("TIME") +

## Y-AXIS TITLE
															                 ylab("PRECIPITATION [mm]") + 

## TIME SERIES LABEL TICKS
									              scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
									        

## Y-AXIS TICKS AND BREAKS
                     scale_y_continuous(breaks=c(0, 200, 400, 600, 800, 1000, 1200)) }

p <- departure_plot(departure_cat[[2]],names(departure_cat[2]))

p



anpumplot <- (
	departure_plot(departure_cat[[2]],names(departure_cat[2]))        +
## ANNOTATIONS FOR EACH YEAR
## 2004
# annotate(geom = "curve", x = as.Date("2012-04-01"), y = 800,color="white",
# xend = as.Date("2012-09-01"), yend = 800,  curvature = .2, 
# arrow = arrow(length = unit(2, "mm"))) 											+
# annotate("text", x = as.Date("2005-06-01"), y = 330, size=5.5,
# label = "Sediment deposition from Dibang begins in Anpum", color="white",family="Arial Narrow") 	+

# ## 2010
# annotate(geom = "curve", x = as.Date("2010-05-15"), y = 390,color="white",
# xend = as.Date("2010-04-01"), yend = 330, curvature = 0.1, 
# arrow = arrow(length = unit(2, "mm"))) 											+
# annotate("text", x = as.Date("2011-01-01"), y = 410, size=5.5,
# 	label = "Sediment deposition", color="white", family="Arial Narrow") 		+

## 2012
annotate(geom = "curve", x = as.Date("2012-12-01"), y = 1000,color="white",
xend = as.Date("2012-09-05"), yend = 840,  curvature = 0.1, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2013-01-01"), y = 1100, size=4,
label = "Bank erosion, Anpum river expands.", color="white", family="Arial Narrow") 	+

## 2015
annotate(geom = "curve", x = as.Date("2015-11-01"), y = 1050,color="white",
xend = as.Date("2015-08-07"), yend = 920,  curvature = .1, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2015-11-15"), y = 1100,size=4, 
label = "Loklung destroyed by major flood.", color="white", family="Arial Narrow")  			+

## 2016
annotate(geom = "curve", x = as.Date("2016-12-01"), y = 1050,color="white",
xend = as.Date("2016-08-15"), yend = 650,  curvature = .3, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2017-07-15"), y = 1100, size=4,
	label = "Destruction of Anpum complete.", color="white", family="Arial Narrow") 		

																		    )

ggsave(filename="anpum-rainfall-2012-2020", plot=anpumplot, device=cairo_ps ,height=10, width=40,dpi=300,units="cm", fallback_resolution = 600)


