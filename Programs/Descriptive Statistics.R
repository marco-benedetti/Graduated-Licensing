

###############################
# STEP 1: DATA PRE-PROCESSING #
###############################

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


# READ IN DATA 
# CHANGE FILE PATHS

basedata_trt <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\OhioCrashes_1617.csv")
basedata_ctrl <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\OhioCrashes_1824.csv")

LD_data <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\Num_LD_Main_File.csv")

pop_00_09 <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\age_grp_ohio_county_pop_00_09.csv")
pop_10_18 <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\age_grp_ohio_county_pop_10_18.csv")

poverty_2008 <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\est08all.csv")
UIC <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\Ohio_UIC.csv")

county_map <- st_read("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\shapefile\\cb_2018_us_county_500k.shp")
ohio_map <- county_map[which(county_map$STATEFP=="39"),]
rm(county_map)

police_data <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Send for Code Check\\Data\\police_data.csv")

vmt <- read.csv("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Final Programs\\Data\\VMT2008.csv")


###########################
# STEP 2: DATA PROCESSING #
###########################

## PROCESS THE POPULATION DATA ##	

subset_pop <- pop_00_09[which((pop_00_09$year==2008)& (pop_00_09$AGEGRP==4)),1:9]


## PROCESS THE LICENSED DRIVERS DATA ##

# Convert variables 
LD_data$LD_16_17 <- as.numeric(LD_data$LD_16_17)
LD_data$LD_18_24 <- as.numeric(LD_data$LD_18_24)
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

ctrl_data_step3$AGEGRP = rep("18-24",nrow(ctrl_data_step3))
ctrl_data_step3$trt = rep(0,nrow(ctrl_data_step3))
ctrl_data_step3$period <- as.numeric(ctrl_data_step3$year > 2007)
ctrl_data_step3$injury_rate <- ctrl_data_step3$COUNT/ctrl_data_step3$LD_18_24*1000

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
overall_denom_18_24 <- aggregate(analysis_data$LD_18_24 ,by=list(analysis_data$year),FUN=sum)$x/2




##################
# START ANALYSIS #
##################



# SPAGHETTI PLOTS 
# png("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Final Programs\\Output\\Figures\\spaghetti_plotV2.png",height=8,width=8,units="in",res=500)
counties <- as.numeric(names(table(analysis_data$county)))
plot(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     analysis_data$injury_rate[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     type="l",ylim=c(0,215),xlab="Year",ylab="Crash Rate per 1,000 Licensed Drivers",col="#4B0092")
lines(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="18-24"))],
     analysis_data$injury_rate[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="18-24"))],col="#1AFF1A")

for(i in 1:length(counties)){

	tmp <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="16-17")),]
	lines(tmp$year,tmp$injury_rate,col="#4B0092")

	tmp2 <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="18-24")),]
	lines(tmp2$year,tmp2$injury_rate,col="#1AFF1A",lty=1)

}
abline(v=2007.5)
legend("topright",lty=c(1,1,2,2),lwd=c(1,1,4,4),col=c("#4B0092","#1AFF1A","#4B0092","#1AFF1A"),
		legend=c("16-17-year-olds (county)","18-24-year-olds (county)","16-17-year-olds (statewide)","18-24-year-olds (statewide)"))
lines(seq(2003,2017),overall_count[1:15]/overall_denom_16_17*1000,lwd=4,col="#197602",lty=2)
lines(seq(2003,2017),overall_count[16:30]/overall_denom_18_24*1000,lwd=4,col="#009688",lty=2)
#dev.off()

#png("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Final Programs\\Output\\Figures\\log_spaghetti_plot.png",height=8,width=8,units="in",res=500)
counties <- as.numeric(names(table(analysis_data$county)))
plot(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     log(analysis_data$injury_rate)[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     type="l",ylim=c(2.5,6),xlab="Year",ylab="Log Crash Rate per 1,000 Licensed Drivers",col=2)
lines(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="18-24"))],
     log(analysis_data$injury_rate)[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="18-24"))],col=4)

for(i in 1:length(counties)){

	tmp <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="16-17")),]
	lines(tmp$year,log(tmp$injury_rate),col=2)

	tmp2 <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="18-24")),]
	lines(tmp2$year,log(tmp2$injury_rate),col=4)

}
abline(v=2007.5)
legend("topright",lty=c(1,1,2,2),lwd=c(1,1,4,4),col=c(2,4,"firebrick4","blue4"),
		legend=c("16-17-year-olds (county)","18-24-year-olds (county)","16-17-year-olds (statewide)","18-24-year-olds (statewide)"))
lines(seq(2003,2017),log(overall_count[1:15]/overall_denom_16_17*1000),lwd=4,col="firebrick4",lty=2)
lines(seq(2003,2017),log(overall_count[16:30]/overall_denom_18_24*1000),lwd=4,col="blue4",lty=2)
#dev.off()

# CRASH RATES #
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
analysis_data_18_24 <- analysis_data[which(analysis_data$AGEGRP=="18-24"),]

overall_ctrl <- sum(analysis_data_18_24$COUNT)/sum(analysis_data_18_24$LD_18_24)*1000
pre_ctrl <- sum(analysis_data_18_24$COUNT[which(analysis_data_18_24$year < 2008)])/sum(analysis_data_18_24$LD_18_24[which(analysis_data_18_24$year < 2008)])*1000
post_ctrl <- sum(analysis_data_18_24$COUNT[which(analysis_data_18_24$year >= 2008)])/sum(analysis_data_18_24$LD_18_24[which(analysis_data_18_24$year >= 2008)])*1000

diff_ctrl <- sum(analysis_data_18_24$COUNT[which(analysis_data_18_24$year < 2008)])/sum(analysis_data_18_24$LD_18_24[which(analysis_data_18_24$year < 2008)])*1000-
sum(analysis_data_18_24$COUNT[which(analysis_data_18_24$year >= 2008)])/sum(analysis_data_18_24$LD_18_24[which(analysis_data_18_24$year >= 2008)])*1000

percent_ctrl <- diff_ctrl/pre_ctrl*100


overall_ctrl
pre_ctrl
post_ctrl
diff_ctrl
percent_ctrl


## INDIVIDUAL RATES ##




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
analysis_data_18_24 <- analysis_data[which(analysis_data$AGEGRP=="18-24"),]
overall_trt <- aggregate(analysis_data_18_24$COUNT,FUN="sum",by=list(analysis_data_18_24$county))$x/
			aggregate(analysis_data_18_24$LD_18_24,FUN="sum",by=list(analysis_data_18_24$county))$x*1000
pre_trt <- aggregate(analysis_data_18_24$COUNT[which(analysis_data_18_24$year < 2008)],FUN="sum",by=list(analysis_data_18_24$county[which(analysis_data_18_24$year < 2008)]))$x/
		aggregate(analysis_data_18_24$LD_18_24[which(analysis_data_18_24$year < 2008)],FUN="sum",by=list(analysis_data_18_24$county[which(analysis_data_18_24$year < 2008)]))$x*1000

post_trt <- aggregate(analysis_data_18_24$COUNT[which(analysis_data_18_24$year >= 2008)],FUN="sum",by=list(analysis_data_18_24$county[which(analysis_data_18_24$year >= 2008)]))$x/
		aggregate(analysis_data_18_24$LD_18_24[which(analysis_data_18_24$year >= 2008)],FUN="sum",by=list(analysis_data_18_24$county[which(analysis_data_18_24$year >= 2008)]))$x*1000


diff_trt <- pre_trt-post_trt

percent_trt <- diff_trt/pre_trt*100

summary(pre_trt)
summary(post_trt)
summary(diff_trt)
summary(percent_trt)
