

#######################
# DATA PRE-PROCESSING #
#######################

# INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("INLA")
#install.packages("sf")
#install.packages("spdep")
#install.packages("nimble")
#install.packages("did")
#install.packages("stringr")
#install.packages("tidycensus")
#install.packages("RColorBrewer")


require(INLA)
require(sf)
require(spdep)
require(nimble)
require(did)
require(stringr)
require(tidycensus)
require(RColorBrewer)


# CHANGE TO ROOT DIRECTORY
root <- ""


# READ IN DATA 
basedata_trt <- read.csv(paste(root,"Data\\OhioCrashes_1617.csv",sep=""))
basedata_ctrl <- read.csv(paste(root,"Data\\OhioCrashes_3034.csv",sep=""))

LD_data <- read.csv(paste(root,"Data\\Num_LD_Main_File.csv",sep=""))

pop_00_09 <- read.csv(paste(root,"Data\\age_grp_ohio_county_pop_00_09.csv",sep=""))
pop_10_18 <- read.csv(paste(root,"Data\\age_grp_ohio_county_pop_10_18.csv",sep=""))

poverty_2008 <- read.csv(paste(root,"Data\\est08all.csv",sep=""))
UIC <- read.csv(paste(root,"Data\\Ohio_UIC.csv",sep=""))

county_map <- st_read(paste(root,"Data\\shapefile\\cb_2018_us_county_500k.shp",sep=""))
ohio_map <- county_map[which(county_map$STATEFP=="39"),]
rm(county_map)

police_data <- read.csv(paste(root,"Data\\police_data.csv",sep=""))


vmt <- read.csv(paste(root,"Data\\VMT2008.csv",sep=""))

###################
# DATA PROCESSING #
###################

## PROCESS THE POPULATION DATA ##	

subset_pop <- pop_00_09[which((pop_00_09$year==2008)& (pop_00_09$AGEGRP==4)),1:9]


## PROCESS THE LICENSED DRIVERS DATA ##

# Convert variables 
LD_data$LD_16_17 <- as.numeric(LD_data$LD_16_17)
LD_data$LD_30_34 <- as.numeric(LD_data$LD_30_34)

# Delete extra columns
LD_data_keep <- LD_data[which(LD_data$year %in% seq(2003,2017)),c(1:4,19:21)]

# Sort 
LD_data_keep <- LD_data_keep[order(LD_data_keep$county,LD_data_keep$year),]



## PROCESS POLICE DATA ##

police_data$num_dept <- as.numeric(police_data$num_dept)
police_data$num_officer <- as.numeric(police_data$num_officer )
police_data$budget <- as.numeric(police_data$budget)
police_data$pop <- subset_pop$TOT_POP

police_data$officers_per <- police_data$num_officer/police_data$pop
police_data$budget_per <- police_data$budget/police_data$pop


## PROCESS VMT DATA ##

interstate <- vmt$`FC...01`
total <- vmt$Total
total_per_pop <- total/subset_pop$TOT_POP
pct_urban <- vmt$URBAN/vmt$Total


## PROCESS THE INJURY DATA ##


# Reformat COUNT
basedata_trt$COUNT <- as.numeric(basedata_trt$COUNT)
basedata_ctrl$COUNT <- as.numeric(basedata_ctrl$COUNT)

# Subset
trt_data_step1 <- basedata_trt[which(basedata_trt$year %in% seq(2003,2017)),]
ctrl_data_step1 <- basedata_ctrl[which(basedata_ctrl$year %in% seq(2003,2017)),]

# Sort 
trt_data_step2 <- trt_data_step1[order(trt_data_step1$county,trt_data_step1$year),]
ctrl_data_step2 <- ctrl_data_step1[order(ctrl_data_step1$county,ctrl_data_step1$year),]


## MERGE INJURY AND LICENSED DRIVERS DATA ##

# Merge
trt_data_step3 <- merge(trt_data_step2,LD_data_keep,by = c("county","year"))
ctrl_data_step3 <- merge(ctrl_data_step2,LD_data_keep,by = c("county","year"))

# Create some variables
trt_data_step3$AGEGRP = rep("16-17",nrow(trt_data_step3))
trt_data_step3$trt = rep(1,nrow(trt_data_step3))
trt_data_step3$period <- as.numeric(trt_data_step3$year > 2007)
trt_data_step3$injury_rate <- trt_data_step3$COUNT/trt_data_step3$LD_16_17*1000

ctrl_data_step3$AGEGRP = rep("30-34",nrow(ctrl_data_step3))
ctrl_data_step3$trt = rep(0,nrow(ctrl_data_step3))
ctrl_data_step3$period <- as.numeric(ctrl_data_step3$year > 2007)
ctrl_data_step3$injury_rate <- ctrl_data_step3$COUNT/ctrl_data_step3$LD_30_34*1000

# Combine
analysis_data_step1 <- rbind(trt_data_step3,ctrl_data_step3)

# Create exposures
analysis_data_step1$treatment_time <- as.numeric(analysis_data_step1$AGEGRP=="16-17")*2008
analysis_data_step1$county_num <- as.numeric(analysis_data_step1$county)

# Sort and finalize
analysis_data <- analysis_data_step1[order(analysis_data_step1$county,analysis_data_step1$year,analysis_data_step1$AGEGRP),]

# Get counties
counties <- as.numeric(names(table(analysis_data$county)))



overall_count <- aggregate(analysis_data$COUNT,by=list(analysis_data$year,analysis_data$AGEGRP),FUN=sum)$x
overall_denom_16_17 <- aggregate(analysis_data$LD_16_17 ,by=list(analysis_data$year),FUN=sum)$x/2
overall_denom_30_34 <- aggregate(analysis_data$LD_30_34 ,by=list(analysis_data$year),FUN=sum)$x/2




##################
# START ANALYSIS #
##################



# SPAGHETTI PLOTS 


# RAW CRASH RATE (NOT INCLUDED IN MANUSCRIPT)
counties <- as.numeric(names(table(analysis_data$county)))
plot(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     analysis_data$injury_rate[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     type="l",ylim=c(0,215),xlab="Year",ylab="Crash Rate per 1,000 Licensed Drivers",col="red")
lines(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="30-34"))],
     analysis_data$injury_rate[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="30-34"))],col="blue")

for(i in 1:length(counties)){

	tmp <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="16-17")),]
	lines(tmp$year,tmp$injury_rate,col="red")

	tmp2 <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="30-34")),]
	lines(tmp2$year,tmp2$injury_rate,col="blue",lty=1)

}
abline(v=2007.5)
legend("topright",lty=c(1,1,2,2),lwd=c(1,1,4,4),col=c("red","blue","darkred","darkblue"),
		legend=c("16-17-year-olds (county)","30-34-year-olds (county)","16-17-year-olds (statewide)","30-34-year-olds (statewide)"))
lines(seq(2003,2017),overall_count[1:15]/overall_denom_16_17*1000,lwd=4,col="darkred",lty=2)
lines(seq(2003,2017),overall_count[16:30]/overall_denom_30_34*1000,lwd=4,col="darkblue",lty=2)

##############################
# LOG CRASH RATES: FIGURE A1 #
##############################

counties <- as.numeric(names(table(analysis_data$county)))
plot(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     log(analysis_data$injury_rate)[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     type="l",ylim=c(2.5,6),xlab="Year",ylab="Log Crash Rate per 1,000 Licensed Drivers",col=2)
lines(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="30-34"))],
     log(analysis_data$injury_rate)[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="30-34"))],col=4)

for(i in 1:length(counties)){

	tmp <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="16-17")),]
	lines(tmp$year,log(tmp$injury_rate),col=2)

	tmp2 <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="30-34")),]
	lines(tmp2$year,log(tmp2$injury_rate),col=4)

}
abline(v=2007.5)
legend("topright",lty=c(1,1,2,2),lwd=c(1,1,4,4),col=c(2,4,"firebrick4","blue4"),
		legend=c("16-17-year-olds (county)","30-34-year-olds (county)","16-17-year-olds (statewide)","30-34-year-olds (statewide)"))
lines(seq(2003,2017),log(overall_count[1:15]/overall_denom_16_17*1000),lwd=4,col="firebrick4",lty=2)
lines(seq(2003,2017),log(overall_count[16:30]/overall_denom_30_34*1000),lwd=4,col="blue4",lty=2)




##############################
# STATE CRASH RATES: TABLE 1 #
##############################

# TREATED GROUP #
analysis_data_16_17 <- analysis_data[which(analysis_data$AGEGRP=="16-17"),]
overall_trt <- sum(analysis_data_16_17$COUNT)/sum(analysis_data_16_17$LD_16_17)*1000
pre_trt <- sum(analysis_data_16_17$COUNT[which(analysis_data_16_17$year < 2008)])/sum(analysis_data_16_17$LD_16_17[which(analysis_data_16_17$year < 2008)])*1000
post_trt <- sum(analysis_data_16_17$COUNT[which(analysis_data_16_17$year >= 2008)])/sum(analysis_data_16_17$LD_16_17[which(analysis_data_16_17$year >= 2008)])*1000

diff_trt <- sum(analysis_data_16_17$COUNT[which(analysis_data_16_17$year < 2008)])/sum(analysis_data_16_17$LD_16_17[which(analysis_data_16_17$year < 2008)])*1000-
sum(analysis_data_16_17$COUNT[which(analysis_data_16_17$year >= 2008)])/sum(analysis_data_16_17$LD_16_17[which(analysis_data_16_17$year >= 2008)])*1000

percent_trt <- diff_trt/pre_trt*100

overall_trt
pre_trt
post_trt
diff_trt
percent_trt


# CONTROL GROUP
analysis_data_30_34 <- analysis_data[which(analysis_data$AGEGRP=="30-34"),]

overall_ctrl <- sum(analysis_data_30_34$COUNT)/sum(analysis_data_30_34$LD_30_34)*1000
pre_ctrl <- sum(analysis_data_30_34$COUNT[which(analysis_data_30_34$year < 2008)])/sum(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year < 2008)])*1000
post_ctrl <- sum(analysis_data_30_34$COUNT[which(analysis_data_30_34$year >= 2008)])/sum(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year >= 2008)])*1000

diff_ctrl <- sum(analysis_data_30_34$COUNT[which(analysis_data_30_34$year < 2008)])/sum(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year < 2008)])*1000-
sum(analysis_data_30_34$COUNT[which(analysis_data_30_34$year >= 2008)])/sum(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year >= 2008)])*1000

percent_ctrl <- diff_ctrl/pre_ctrl*100


overall_ctrl
pre_ctrl
post_ctrl
diff_ctrl
percent_ctrl

##########################################
# COUNTY DESCRIPTIVE STATISTICS: TABLE 2 #
##########################################

analysis_data_16_17 <- analysis_data[which(analysis_data$AGEGRP=="16-17"),]
overall_trt <- aggregate(analysis_data_16_17$COUNT,FUN="sum",by=list(analysis_data_16_17$county))$x/
			aggregate(analysis_data_16_17$LD_16_17,FUN="sum",by=list(analysis_data_16_17$county))$x*1000
pre_trt <- aggregate(analysis_data_16_17$COUNT[which(analysis_data_16_17$year < 2008)],FUN="sum",by=list(analysis_data_16_17$county[which(analysis_data_16_17$year < 2008)]))$x/
		aggregate(analysis_data_16_17$LD_16_17[which(analysis_data_16_17$year < 2008)],FUN="sum",by=list(analysis_data_16_17$county[which(analysis_data_16_17$year < 2008)]))$x*1000

post_trt <- aggregate(analysis_data_16_17$COUNT[which(analysis_data_16_17$year >= 2008)],FUN="sum",by=list(analysis_data_16_17$county[which(analysis_data_16_17$year >= 2008)]))$x/
		aggregate(analysis_data_16_17$LD_16_17[which(analysis_data_16_17$year >= 2008)],FUN="sum",by=list(analysis_data_16_17$county[which(analysis_data_16_17$year >= 2008)]))$x*1000


diff_trt <- pre_trt-post_trt

percent_trt <- diff_trt/pre_trt*100

summary(overall_trt)

summary(pre_trt)
summary(post_trt)
summary(diff_trt)
summary(percent_trt)



# CONTROL GROUP
analysis_data_30_34 <- analysis_data[which(analysis_data$AGEGRP=="30-34"),]
overall_trt <- aggregate(analysis_data_30_34$COUNT,FUN="sum",by=list(analysis_data_30_34$county))$x/
			aggregate(analysis_data_30_34$LD_30_34,FUN="sum",by=list(analysis_data_30_34$county))$x*1000
pre_trt <- aggregate(analysis_data_30_34$COUNT[which(analysis_data_30_34$year < 2008)],FUN="sum",by=list(analysis_data_30_34$county[which(analysis_data_30_34$year < 2008)]))$x/
		aggregate(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year < 2008)],FUN="sum",by=list(analysis_data_30_34$county[which(analysis_data_30_34$year < 2008)]))$x*1000

post_trt <- aggregate(analysis_data_30_34$COUNT[which(analysis_data_30_34$year >= 2008)],FUN="sum",by=list(analysis_data_30_34$county[which(analysis_data_30_34$year >= 2008)]))$x/
		aggregate(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year >= 2008)],FUN="sum",by=list(analysis_data_30_34$county[which(analysis_data_30_34$year >= 2008)]))$x*1000


diff_trt <- pre_trt-post_trt

percent_trt <- diff_trt/pre_trt*100

summary(overall_trt)
summary(pre_trt)
summary(post_trt)
summary(diff_trt)
summary(percent_trt)




#########################################
# NUMBER OF CRASHES BY COUNTY: TABLE A1 #
#########################################

aggregate(analysis_data$COUNT,by=list(analysis_data$COUNTY),FUN=sum)
t1<-aggregate(analysis_data$COUNT[which(analysis_data$trt==1)],by=list(analysis_data$COUNTY[which(analysis_data$trt==1)]),FUN=sum)
t2<-aggregate(analysis_data$COUNT[which(analysis_data$trt==0)],by=list(analysis_data$COUNTY[which(analysis_data$trt==0)]),FUN=sum)

t3<-aggregate(analysis_data$LD_16_17[which(analysis_data$trt==1)],by=list(analysis_data$COUNTY[which(analysis_data$trt==1)]),FUN=sum)
t4<-aggregate(analysis_data$LD_30_34[which(analysis_data$trt==0)],by=list(analysis_data$COUNTY[which(analysis_data$trt==0)]),FUN=sum)


