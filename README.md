---
title: "An Analysis and Exploration of Car Crashes in Montgomery County, Maryland"
author: "Joel Casillas and Ryan LeBon"  
date: "April 7, 2018"
output: html_document
---

  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
##Introduction

Montgomery County, is a county in the U.S. state of Maryland. As of the 2010 census, the population was 971,777. It is the most populous county in Maryland. This data set provides information on motor vehicle operators (drivers) involved in traffic collisions occuring on county and local roadways. The dataset reports details of all traffic collisions occurring on county and local roadways within [Montgomery County](https://data.montgomerycountymd.gov/Public-Safety/Crash-Reporting-Drivers-Data/mmzv-x632), as collected via the Automated Crash Reporting System (ACRS) of the Maryland State Police, and reported by the Montgomery County Police, Gaithersburg Police, Rockville Police, or the Maryland-National Capital Park Police. This dataset shows each collision recorded and the drivers involved. 


##Initial Data Exploration

This data comes from the Automated Crash Reporting System (ACRS) of the Maryland State Police, and it contains 32 columns and over 69 thousand rows. According to the website, this dataset was created on September 19, 2017. This dataset contains 3 years worth of collisions, and it gets updated weekly. In the process of understanding and exploring the data, we began to come up with questions that we could answer. We explored the dataset and we began to develop ideas on what we could potentially visualize in R. Some of the main types of columns include the following; agency name, crash date and time, road name, collision types, weather, injury severity, and whether or not the driver was under substance abuse. 

##Data Cleaning & Preprocessing 

This dataset was scattered with N/A's and empty strings, so it was vital that we clean the data so we could come up with the foremost graphs. We decided the best way to approach the bad/missing data was by removing it from the dataset. We made new data frames that didn't have the N/A's and empty strings. This gave us clean data sets that allowed us to answer our questions in a clear and effective way.

##Data Exploration and Visualization of Montgomery County
```{r}
dat = read.csv("https://raw.githubusercontent.com/Ryanbbq/datascience_mini_project/master/Crash_Reporting.csv")
```

### What are the top 10 states that got in a crash, besides Maryland?
```{r,fig.align="center"}
par(mar=c(5,5,3,3))

# data extraction and cleaning, removed MD, blank and XX
driver_states <- table(dat$Drivers.License.State[dat$Drivers.License.State!="MD" & dat$Drivers.License.State != "" & dat$Drivers.License.State != "XX"])

# took the top 10 states sorted it as well
top_10 <- head(sort(driver_states,decreasing=T),10)

#plot the top 10 states
plot(top_10,ylim=c(0,2500),col="firebrick",ylab="Number of Crashes",xlab="License Plates",main="Top 10 Other States that got in a crash")

```
This is an initial data exploration, we wanted to see the top 10 out of state driver licenses that were involved in crashes in Montgomery County. In this plot, 3 states were ommited. The first state that was omitted was Maryland, because it is the highest, at 55581 there was no reason to see this. The y parameter would be too high. The next state was totally blank, and of course we didnt want to make an assumption of which state it was, at 2,726. The last state was "XX" which was also unknown, and this state, whatever it is, contained 346 crashes. 


### What were the types of collisions done to the vehicles?

### How were the vehicles moving during the collisions?

```{r,fig.align='left'}
# 2 rows, 1 column
par(mfrow=c(2,1))
par(mar=c(2,16,3,3))

#data cleaning/extraction
damage_extent <- dat$Vehicle.Damage.Extent[dat$Vehicle.Damage.Extent != "UNKNOWN" & dat$Vehicle.Damage.Extent != "OTHER" & dat$Vehicle.Damage.Extent != "N/A"]

# turn into a table
damage_extent <- table(damage_extent)

# data cleaning
damage_extent <- damage_extent[damage_extent != 0]

# plotting
barplot(sort(damage_extent,decr=T),las=1,horiz=T,col="firebrick",main="Vehicle Damage",xlim=c(0,25000),xlab="Number of Crashes")

# data cleaning/extraction
vehicle_movement <- dat$Vehicle.Movement[dat$Vehicle.Movement != "UNKNOWN" & dat$Vehicle.Movement != "N/A" ]
vehicle_movement[grepl("OTHER|LEAVING TRAFFIC LANE|RIGHT TURN ON RED|NEGOTIATING A CURVE|ENTERING TRAFFIC LANE|PASSING|DRIVERLESS MOVING VEH.|LEAVING TRAFFIC LANE|RIGHT TURN ON RED|PARKED|PARKING|STARTING FROM PARKED|MAKING U TURN|SKIDDING|CHANGING LANES|STARTING FROM LANE|MAKING RIGHT TURN|BACKING|ACCELERATING",vehicle_movement)] <- "OTHER"

# turn into a table
vehicle_movement <- table(vehicle_movement)

# data cleaning
vehicle_movement <- vehicle_movement[vehicle_movement != 0]

# plotting
barplot(sort(vehicle_movement,decr=T),horiz=T,las=1,col="firebrick",xlim=c(0,25000),main="Vehicle Movement",xlab="Number of Crashes")

```
The first barplot shows how the vehicle was effected after the collision. As you can see many vehicles were disabled, but there were still a decent amount of cars which were still functional. The bar 'superficial' represents cars that had damage done to the outside of the car, these types of cars will probably have to be repaired by an auto body shop.

The second barplot shows how the vehicle was moving at the time of the crash. Some of the questions we could also ask is which way was the vehicle making a turn, or was the vehicle stoppped in traffic? The bar 'other' represents the sub-categories such as; accelerating, skidding, backing up, parking and several other scenarios. We were suprised to see that the category 'stopped in traffic lane' was so high, but it makes sense because getting rear ended is common.


### Which direction was the car moving when it got into a collision?

### How was the driver distracted during the accident?
```{r,fig.align='left'}
par(mfrow=c(2,1))
par(mar=c(2,16,3,3))
type_of_collision <- dat$Collision.Type[dat$Collision.Type != "N/A" & dat$Collision.Type != "UNKNOWN"]
type_of_collision[grepl("OPPOSITE DIR BOTH LEFT TURN|ANGLE MEETS LEFT HEAD ON|SAME DIR BOTH LEFT TURN|SAME DIR REND RIGHT TURN|SAME DIR REND LEFT TURN|ANGLE MEETS RIGHT TURN|OPPOSITE DIRECTION SIDESWIPE|HEAD ON|SAME DIRECTION LEFT TURN|SAME DIRECTION RIGHT TURN|OTHER|HEAD ON LEFT TURN|ANGLE MEETS LEFT TURN",type_of_collision)]<-"OTHER"
type_of_collision <- table(type_of_collision)
type_of_collision <- type_of_collision[type_of_collision != 0]
barplot(sort(type_of_collision,decreasing=T),horiz=T,las=1,main="Types of Collision",col="firebrick",xlim=c(0,25000),xlab="Number of Crashes")


distracted_driving <- dat$Driver.Distracted.By[dat$Driver.Distracted.By != "UNKNOWN" & dat$Driver.Distracted.By != "NOT DISTRACTED"]

distracted_driving[grepl("SMOKING RELATED|DIALING CELLULAR PHONE|EATING OR DRINKING|BY MOVING OR DRINKING|BY MOVING OBJECT IN VEHICLE|NO DRIVER PRESENT|USING DEVICE OBJECT BROUGHT INTO VEHICLE|USING OTHER DEVICE CONTROLS INTEGRAL TO VEHICLE| ADJUSTING AUDIO AND OR CLIMATE CONTROLS|OTHER CELLULAR PHONE RELATED|ADJUSTING AUDIO AND OR CLIMATE CONTROLS|TALKING OR LISTENING TO CELLULAR PHONE|TALKING OR LISTENING TO CELLULAR PHONE|OTHER ELECTRONIC DEVICE|DISTRACTED BY OUTSIDE",distracted_driving)] <- "OTHER DISTRACTION"




distracted_driving <- table(distracted_driving)
distracted_driving <- distracted_driving[distracted_driving != 0]
barplot(sort(distracted_driving,decreasing=T),horiz=T,las=1,main="Distracted Drivers",col="firebrick",xlim=c(0,8000),xlab="Number of Crashes")

```

The first barplot shows how the car was driving when it got into a collision. The bar 'other' represents the sub-categories such as; head on left turn, angle meets left turn, angle meets left head on, and head on collisions.
 
This second barplot shows how the driver was distracted at the time of the crash. The bar 'other' represents the sub-categories such as; smoking, on their phone, eating or drinking, distracted by outside events, and several other scenarios. We were also surprised to see that the amount of texting from a cellular phone was so low, when we assumed it would be high, although people may have not been honest when filling out this form.
 

###Were there more collisions at a stop sign or a yield sign?
```{r, fig.align='left'}
dat1 = dat[dat$Traffic.Control %in% c("YIELD SIGN", "STOP SIGN","TRAFFIC SIGNAL") &
dat$Injury.Severity %in% c("POSSIBLE INJURY", "SUSPECTED MINOR INJURY", "SUSPECTED
SERIOUS INJURY"),]
tbl = table(as.character(dat1$Injury.Severity), dat1$Traffic.Control == "YIELD SIGN")
tbl_normed = scale(tbl, center=FALSE, scale=apply(tbl, 2, sum))
cols = c("yellow", "red", "green")
barplot(tbl_normed, names.arg=c("stop", "yield"), main="Injury severity for stop and yield signs", col=cols)
legend("bottomright", legend=rev(c("possible", "minor", "serious")), col=rev(cols), lwd=5)
```

This graphs shows that collisions with respect to yield and stop signs do not differ too much.


###Which district in Montgomery County would be the safest and how many fatal crashes were in it?
```{r,fig.align='left'}
temp2=dat
fatal=temp2[temp2$ACRS.Report.Type=="Fatal Crash" & temp2$ACRS.Report.Type!="N/A" & temp2$ACRS.Report.Type!="UNKNOWN" ,]
fatal=fatal[fatal$Municipality!="" & fatal$Municipality!="WASHINGTON GROVE" & fatal$Municipality!="SOMERSET" &  fatal$Municipality!="N/A",]
par(mar=c(3,14,3,3))
barplot(sort(table(as.character(fatal$Municipality)),decreasing = TRUE),col = "firebrick",horiz = TRUE,las=1,main = "Municipalities ordered by Fatal Crashes")

``` 

The graph shows that the Chevy Chase View and Chevy Chase #4 were the municipalities with the least fatal crashes which makes sense. Chevy Chase View is a really small town, so the amount of fatal crashes isn't too surprising. The Municipality with the most fatal crashes is Rockville which is much bigger than Chevy Chase View. I expected there to be more fatal crashes than 6 in Rockville, but I'm not sure if by Fatal Crashes, they mean fatal on site. So I'm not too sure how they collected this data and in what pretext it was in. It makes sense that these values were so low because fatal crashes represents deaths in Montgomery County.




###We wanted to see whether someone who had Alcohol present on them at the collision wasn't at fault for it. I'm expecting to see that just because alcohol was present, they won't be at fault for the majority of the collisions, since they're not inebriated.
```{r,fig.align='left'}
temp3=dat
drunk=temp3[temp3$Driver.Substance.Abuse=="ALCOHOL PRESENT",]
barplot(sort(table(drunk$Driver.At.Fault=="No"),decreasing = TRUE),las=1,main = "Collisions where alcohol is present and the crash was their fault",col = "firebrick",ylab = "Amount of crashes",ylim=c(0,2000))
```

It seems that the vast majority of the collisions were caused by those who had alcohol present with them. 


### Between a transit bus or school bus, which type of bus caused more injuries?
```{r,fig.align='left'}
temp=dat
transitbus=temp[temp$Vehicle.Body.Type=="TRANSIT BUS" & temp$Vehicle.Body.Type!="N/A" & temp$Vehicle.Body.Type!="UNKNOWN" ,]
transitbus=transitbus[transitbus$Injury.Severity!="FATAL INJURY" & transitbus$Injury.Severity!="NO APPARENT INJURY" & transitbus$Injury.Severity!="SUSPECTED SERIOUS INJURY",]

schoolbus=temp[temp$Vehicle.Body.Type=="SCHOOL BUS" & temp$Vehicle.Body.Type!="N/A" & temp$Vehicle.Body.Type!="UNKNOWN" ,]
schoolbus=schoolbus[schoolbus$Injury.Severity!="FATAL INJURY" & schoolbus$Injury.Severity!="NO APPARENT INJURY",]


#barplot(sort(table(as.character(transitbus$Injury.Severity)),decreasing=TRUE),horiz = T,las=1,main = "Transit Bus Injuries",col = "firebrick")
#barplot(sort(table(as.character(schoolbus$Injury.Severity)),decreasing = TRUE),horiz = T,las=1,main = "School Bus Injuries",col = "firebrick")
k <- merge(as.character(schoolbus$Injury.Severity),as.character(transitbus$Injury.Severity))
colnames(k)<- c("School Bus","Transit Bus")

k <- table(k)
barplot(k,beside=T,las=1,col=topo.colors(2),names.arg=colnames(k),ylim=c(0,300))
legend("topright",legend=c("School Bus","Transit Bus"),fill=topo.colors(2))


```

So the graph shows that the transit bus was responsible for more injuries which is not surprising. Transit Buses operate all day, whereas School Buses operate much less, so the results are not too surprising in that aspect. What is surprising is that neither buses were responsible for a fatal injury, according to the data.



###Between a pedestrian or a bicyclist, which was involved in more property damage crashes?
```{r,fig.align='left'}
temp4=dat
par(mfrow=c(1,2))


bicycle=temp4[temp4$Related.Non.Motorist=="BICYCLIST"  & temp4$Related.Non.Motorist!="",]
bicycle=bicycle[bicycle$ACRS.Report.Type=="Property Damage Crash" ,]

pedestrian=temp4[temp4$Related.Non.Motorist=="PEDESTRIAN" & temp4$Related.Non.Motorist!="" ,]
pedestrian=pedestrian[pedestrian$ACRS.Report.Type=="Property Damage Crash" ,]


barplot(sort(table(as.character(bicycle$ACRS.Report.Type)),decreasing=TRUE),ylim = c(0,60),las=1,col = "firebrick",main = "Bicyclist")
barplot(sort(table(as.character(pedestrian$ACRS.Report.Type)),decreasing = TRUE),ylim = c(0,60) ,las=1,col = "firebrick",main = "Pedestrian")

```

The graph shows that bicycle collisions were more involved in property damage crashes, whereas a pedestrian was less involved. This  makes sense because considering that a person who has a bike gets in the collision, the bike has to be accounted for, and that can end up hitting property also, which would result in more property damage.

###How many people in Montgomery County got in an accident because of substance abuse?

```{r,fig.align='left'}
par(mar=c(4,16,3,4))

sub_abuse <- dat$Driver.Substance.Abuse[dat$Driver.Substance.Abuse != "UNKNOWN" & dat$Driver.Substance.Abuse != "NONE DETECTED" & dat$Driver.Substance.Abuse != "N/A"]

sub_abuse[grepl("OTHER|COMBINATION CONTRIBUTED|ILLEGAL DRUG CONTRIBUTED|MEDICATION CONTRIBUTED|ALCOHOL CONTRIBUTED",sub_abuse)] <- "OTHER"

sub_abuse <- table(sub_abuse)
sub_abuse <- sub_abuse[sub_abuse != 0]

barplot(sort(sub_abuse,decreasing=T),horiz=T,las=1,main="Substance Abuse Present and Crashes",col="firebrick",xlim=c(0,2000),xlab="Number of Crashes")

```

This graph shows the categories of substance abuse present in drivers of Montgomery County, and the total number of crashes. The bar 'other' represents the sub-categories such as; combination contributed, illegal drugs contributed, and medication contributed. According to this barplot, we can conclude that a driver who is under the influence of alcohol will get in more crashes than someone who has medication present.



### What are the exact gps coordinates of where the collision occured? 

```{r,fig.align='left'}

plot(Latitude~Longitude,data=dat,asp=1,pch=".",col=rainbow(10),xlim=c(-77.3,-77),ylim=c(39.0,39.3),xlab="Latitude",ylab="Longitude",main=("GPS Location of Car Crashes"))



```

This graph shows the exact location of where the collision occured at. The graph shows that the majority of the collisions occured within a fairly close proximity. The data is spread out to the point where you can tell that the data is in separate counties. The data also shows how most accidents occured on major roads.



### Conclusion:

In conclusion, this project taught us and really made us realize that data cleaning and preprocessing is one of the most important steps in Data Science. The hardest thing we faced in this project was removing bad data. We spent about 60% of our time trying to figure out how to get rid of bad data. For example, we spent time removing values such as unknowns, N/A's, and missing data types. We spent about 20% of our time coming up with valuable questions for the data. We spent the last 20% of our time in trying to be creative and making visualizations that were informative and meaningful. Formatting for the graphs was also a challenge because sometimes the labels woul be too big, or the graph would go out of the document. 

Overall this was a very fun and exciting project, which was challenging at times, but nonetheless fun. This project taught us alot and showed us how much time needs to be devoted in order to get a decent looking report. This personally reaffirms our desire to become Data Scientist's and showed us how beautiful R really is.
