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
rain <- list.files(pattern = ".*3B42.*\\.csv$$", recursive = FALSE)

## READ THEM IN AS DATAFRAMES IN A LIST
rainfall <- lapply(rain,read.csv)

## SET THE NAMES OF EACH LIST FROM THE FILENAME
names(rainfall) <- c("ALUNDI","ANDRA","ANPUM","APRI","ASHAN","ASON","DEOPANI","DRI","EBRANLI","ELON","MATHUN","RUSHON","SESSERI","SINZEN","TANGON")

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
rain_na_omit[[9]]$date_time <- ymd_hms(rain_na_omit[[9]]$date_time)
rain_na_omit[[10]]$date_time <- ymd_hms(rain_na_omit[[10]]$date_time)
rain_na_omit[[11]]$date_time <- ymd_hms(rain_na_omit[[11]]$date_time)
rain_na_omit[[12]]$date_time <- ymd_hms(rain_na_omit[[12]]$date_time)
rain_na_omit[[13]]$date_time <- ymd_hms(rain_na_omit[[13]]$date_time)
rain_na_omit[[14]]$date_time <- ymd_hms(rain_na_omit[[14]]$date_time)
rain_na_omit[[15]]$date_time <- ymd_hms(rain_na_omit[[15]]$date_time)

## CREATE A TIBBLE
rain_tbl <- lapply(rain_na_omit,as_tibble)

## FUNCTION TO GET 3-HOURLY RAINFALL FROM HOURLY RAINFALL THEN AFTER GROUPING BY DATE SUM EACH DATE'S HOURLY RAINFALL 
## %>% THEN CREATE A  MONTH AND YEAR COLUMN AND GET TOTAL VALUES FOR EACH MONTH IN WACH YEAR
monthly_rain <- lapply(rain_tbl, function(x){

	mutate(x, date_col = date(date_time), three_hourly_rainfall_sum = rainfall_per_hour*3)  %>% 
	group_by(date_col) 																		%>% 
	summarize(value=sum(three_hourly_rainfall_sum)) 										%>% 
    mutate(month=format(date_col, "%m"),year=format(date_col,"%Y")) 						%>% 
    group_by(month,year)																	%>% 
    summarize(total=sum(value)) })

## FUNCTION TO GET MONTHLY RAINFALL TOTAL FROM DAILY RAINFALL
monthly_rain <- lapply(rain_tbl, function(x){
	
	mutate(x, date_time = date(date_time))													%>%
	group_by(date_time) 																	%>% 
	summarize(value=sum(rainfall_mm)) 														%>% 
    mutate(month=format(date_time, "%m"),year=format(date_time,"%Y")) 						%>% 
    group_by(month,year)																	%>% 
    summarize(total=sum(value)) })

## GET LONG TERM MEAN RAINFALL FROM 1998 TO 2019
mean_monthly_rain_1998_2019 <- lapply(monthly_rain, function(x,...){x %>% summarize(mean_monthly = mean(total), SD=sd(total))})

## GET LONG TERM MEAN RAINFALL FROM 1981 TO 2019
mean_monthly_rain_1981_2020 <- lapply(monthly_rain, function(x,...){x %>% summarize(mean_monthly = mean(total), SD=sd(total))})

## FILTER ALL YEARS BEFORE 2003 TO COMPARE THE LONG TERM MEAN 
longtermfilter <- lapply(monthly_rain, function(x,y,...){x %>% filter(year>1991)})


## JOIN THE LONG TERM MEAN WITH THE DATA FROM 2009 AND LATER
LTmean_join <- map2(longtermfilter, mean_monthly_rain_1998_2019, right_join, by="month")

LTmean_join <- map2(longtermfilter, mean_monthly_rain_1981_2020, right_join, by="month")



## CALCULATE A DEPARTURE FROM MEAN +VE VALUES ARE ABOVE NORMAL AND -VE ARE BELOW
departure <- lapply(LTmean_join, function(x,...){x %>% mutate(departure=total-mean_monthly)})


## CREATE A COLUMN SAYING POSITIVE AND NEGATIVE
departure_cat <- lapply(departure, function(x,...){x %>% mutate(mycolor = ifelse(departure>0, "positive", "negative"))})


##CREATE A NEW COLUMN BY MERGING THE MONTH AND YEAR COLUMNS
departure_cat[[1]]$month_year <- as.yearmon(paste(departure_cat[[1]]$year, departure_cat[[1]]$month), "%Y %m")
departure_cat[[2]]$month_year <- as.yearmon(paste(departure_cat[[2]]$year, departure_cat[[2]]$month), "%Y %m")
departure_cat[[3]]$month_year <- as.yearmon(paste(departure_cat[[3]]$year, departure_cat[[3]]$month), "%Y %m")
departure_cat[[4]]$month_year <- as.yearmon(paste(departure_cat[[4]]$year, departure_cat[[4]]$month), "%Y %m")
departure_cat[[5]]$month_year <- as.yearmon(paste(departure_cat[[5]]$year, departure_cat[[5]]$month), "%Y %m")
departure_cat[[6]]$month_year <- as.yearmon(paste(departure_cat[[6]]$year, departure_cat[[6]]$month), "%Y %m")
departure_cat[[7]]$month_year <- as.yearmon(paste(departure_cat[[7]]$year, departure_cat[[7]]$month), "%Y %m")
departure_cat[[8]]$month_year <- as.yearmon(paste(departure_cat[[8]]$year, departure_cat[[8]]$month), "%Y %m")
departure_cat[[9]]$month_year <- as.yearmon(paste(departure_cat[[9]]$year, departure_cat[[9]]$month), "%Y %m")
departure_cat[[10]]$month_year <- as.yearmon(paste(departure_cat[[10]]$year,departure_cat[[10]]$month), "%Y %m")
departure_cat[[11]]$month_year <- as.yearmon(paste(departure_cat[[11]]$year,departure_cat[[11]]$month), "%Y %m")
departure_cat[[12]]$month_year <- as.yearmon(paste(departure_cat[[12]]$year,departure_cat[[12]]$month), "%Y %m")
departure_cat[[13]]$month_year <- as.yearmon(paste(departure_cat[[13]]$year,departure_cat[[13]]$month), "%Y %m")
departure_cat[[14]]$month_year <- as.yearmon(paste(departure_cat[[14]]$year,departure_cat[[14]]$month), "%Y %m")
departure_cat[[15]]$month_year <- as.yearmon(paste(departure_cat[[15]]$year,departure_cat[[15]]$month), "%Y %m")


## CREATE A NEW COLUMN WITH DATE AS YEAR MONTH AND FIRST DAY
departure_cat[[1]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[1]]$month)],"01", departure_cat[[1]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[2]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[2]]$month)],"01", departure_cat[[2]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[3]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[3]]$month)],"01", departure_cat[[3]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[4]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[4]]$month)],"01", departure_cat[[4]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[5]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[5]]$month)],"01", departure_cat[[5]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[6]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[6]]$month)],"01", departure_cat[[6]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[7]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[7]]$month)],"01", departure_cat[[7]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[8]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[8]]$month)],"01", departure_cat[[8]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[9]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[9]]$month)],"01", departure_cat[[9]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[10]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[10]]$month)],"01", departure_cat[[10]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[11]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[11]]$month)],"01", departure_cat[[11]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[12]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[12]]$month)],"01", departure_cat[[12]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[13]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[13]]$month)],"01", departure_cat[[13]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[14]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[14]]$month)],"01", departure_cat[[14]]$year, sep="-"),format = "%b-%d-%Y")
departure_cat[[15]]$date <- as.Date(paste(month.abb[as.numeric(departure_cat[[15]]$month)],"01", departure_cat[[15]]$year, sep="-"),format = "%b-%d-%Y")


## RENAME THE DATAFRAMES OF THE LIST TO THE LOCATIONS OF THE RAINFALL DATA
names(departure_cat)


## START AND END FOR THE BOUNDING BOX IN THE PLOT
# start1 <- as.Date("2016-07-15")
# start <- as.Date("2015-07-15")

# end <- as.Date("2015-09-15")
# end1 <- as.Date("2016-09-15")


## PLOTTING FUNCTION TO NEST WIHTIN LAPPLY
 						  departure_plot <- function(P,TITLE,...) {ggplot(P, aes(x=date, y=departure)) +

## A GEOM RECTANGLES TO SHOW BOXES OF THE RAINFALL
##                                geom_rect(data=x, aes(xmin=start, xmax=end, ymin=-300, ymax=300), 
##								     						   color='white', fill=NA, size=0.05) +  
##                              geom_rect(data=x, aes(xmin=start1, xmax=end1, ymin=-300, ymax=300), 
##                                                             color='white', fill=NA, size=0.05) +

## RIBBON FOR THE STANDARD DEVIATION															  
   				   	    	    geom_ribbon(aes(ymin = 0 - SD, ymax = 0+SD), fill = "grey70",alpha=0.3) +

## SEGMENTS FOR THE BARS SHOWING DEPARTURE FROM MEAN
   			  		geom_segment( aes(x=date, xend=date, y=0, yend=departure, color=mycolor), size=1.3) +

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
                     scale_y_continuous(breaks=c(-500,-400,-300,-200,-100,0,100,200,300,400)) }




anpumplot <- (departure_plot(departure_cat[[3]],names(departure_cat[3]))        +
## ANNOTATIONS FOR EACH YEAR
## 2004
annotate(geom = "curve", x = as.Date("2004-07-01"), y = 300,color="white",
xend = as.Date("2004-06-01"), yend = 250,  curvature = .3, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2005-06-01"), y = 330, size=5.5,
label = "Anpum R. captured by Dibang R.", color="white",family="Arial Narrow") 	+

## 2010
annotate(geom = "curve", x = as.Date("2010-05-15"), y = 390,color="white",
xend = as.Date("2010-04-01"), yend = 330, curvature = 0.1, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2011-01-01"), y = 410, size=5.5,
	label = "Sediment deposition", color="white", family="Arial Narrow") 		+

## 2012
annotate(geom = "curve", x = as.Date("2012-09-30"), y = 300,color="white",
xend = as.Date("2012-07-01"), yend = 250,  curvature = 0.2, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2013-05-01"), y = 320, size=5.5,
label = "Bank erosion, river expands", color="white", family="Arial Narrow") 	+

## 2015
annotate(geom = "curve", x = as.Date("2015-11-01"), y = 300,color="white",
xend = as.Date("2015-08-01"), yend = 200,  curvature = .3, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2016-04-15"), y = 320,size=5.5, 
label = "AUG 2015 Major Flood", color="white", family="Arial Narrow")  			+

## 2016
annotate(geom = "curve", x = as.Date("2016-12-01"), y = 200,color="white",
xend = as.Date("2016-09-01"), yend = 130,  curvature = .3, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2017-05-15"), y = 220, size=5.5,
	label = "SEPT 2016 Major Flood", color="white", family="Arial Narrow") 		

																		    )

ggsave(filename="plots/anpum-rainfall-2004-2019", plot=anpumplot, device=cairo_ps ,height=15, width=40,dpi=300,units="cm", fallback_resolution = 600)




driplot <- ( departure_plot(departure_cat[[8]],names(departure_cat[8]))        +
## ANNOTATIONS FOR EACH YEAR
## 2004
annotate(geom = "curve", x = as.Date("2004-07-01"), y = 200,color="white",
xend = as.Date("2004-05-01"), yend = 140,  curvature = .3, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2005-06-01"), y = 230, size=5.5,
label = "Anpum R. captured by Dibang R.", color="white",family="Arial Narrow") 	+

## 2010
annotate(geom = "curve", x = as.Date("2010-05-15"), y = 200,color="white",
xend = as.Date("2010-04-01"), yend = 170, curvature = 0.1, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2011-01-01"), y =  220, size=5.5,
	label = "Sediment deposition", color="white", family="Arial Narrow") 		+

## 2012
annotate(geom = "curve", x = as.Date("2012-09-30"), y = 200,color="white",
xend = as.Date("2012-09-01"), yend = 170,  curvature = 0.2, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2013-05-01"), y = 220, size=5.5,
label = "Bank erosion, river expands", color="white", family="Arial Narrow") 	+

## 2015
annotate(geom = "curve", x = as.Date("2015-11-01"), y = 200,color="white",
xend = as.Date("2015-08-01"), yend = 140,  curvature = .3, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2016-04-15"), y = 220,size=5.5, 
label = "AUG 2015 Major Flood", color="white", family="Arial Narrow")  			+

## 2016
annotate(geom = "curve", x = as.Date("2016-12-01"), y = 150,color="white",
xend = as.Date("2016-09-01"), yend = 80,  curvature = .3, 
arrow = arrow(length = unit(2, "mm"))) 											+
annotate("text", x = as.Date("2017-05-15"), y = 160, size=5.5,
	label = "SEPT 2016 Major Flood", color="white", family="Arial Narrow") 	)


ggsave(filename="plots/dri-rainfall-2004-2019", plot=driplot, device=cairo_ps ,height=15, width=40,dpi=300,units="cm", fallback_resolution = 600)
