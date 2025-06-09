

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
basedata_trt <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\OhioCrashes_1617.csv")
basedata_ctrl <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\OhioCrashes_3034.csv")

LD_data <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\Num_LD_Main_File.csv")

pop_00_09 <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\age_grp_ohio_county_pop_00_09.csv")
pop_10_18 <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\age_grp_ohio_county_pop_10_18.csv")

poverty_2008 <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\est08all.csv")
UIC <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\Ohio_UIC.csv")

county_map <- st_read("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\shapefile\\cb_2018_us_county_500k.shp")
ohio_map <- county_map[which(county_map$STATEFP=="39"),]
rm(county_map)

police_data <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\police_data.csv")


vmt <- read.csv("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Data\\VMT2008.csv")

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



## PROCESS VMT DATA ##

interstate <- vmt$`FC...01`
total <- vmt$Total
total_per_pop <- total/subset_pop$TOT_POP
pct_urban <- vmt$URBAN/vmt$Total


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



# IF YOU WANT TO JUST READ IN THE EXISTING OUTPUT:
main_MCMC <- readRDS("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Output_beta_1_independent_30_34.rds")



# CREATE POINT ESTIMATES AND 95% CREDIBLE INTERVALS
ncounty <- 88
point_est <- LL <- UL <- rep(0,ncounty)
for(i in 1:ncounty){

  point_est[i] <- mean(exp(main_MCMC$samples[,4050+i]))
  LL[i] <- quantile(exp(main_MCMC$samples[,4050+i]),probs = 0.025)
  UL[i] <- quantile(exp(main_MCMC$samples[,4050+i]),probs = 1-0.025)
}





## ORDER BY EFFECT SIZE ##



county_names <- rep("",ncounty)
for(i in 1:ncounty){
  tmp <- LD_data$CTYNAME[which(LD_data$county==counties[i])][1]
  county_names[i] <- substr(tmp,1,nchar(tmp)-7)
}

png("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Figures\\Figure 1 a for Talk.png",height=4,width=12,unit="in",res=1000)

par(las=2)
par(mar=c(6,6,1,1))
plot(1:44, point_est[order(point_est)][1:44],,ylim=c(0.70,1.0),pch=18,ylab=expression(paste("E(",exp(beta[2][i]),"|Y)")),xlab = "",xaxt='n')

for(i in 1:44){
  segments(i,LL[order(point_est)][i],i,UL[order(point_est)][i])
  segments(i-0.2,LL[order(point_est)][i],i+0.2,LL[order(point_est)][i])
  segments(i-0.2,UL[order(point_est)][i],i+0.2,UL[order(point_est)][i])
}
abline(h=1)
abline(h=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=1,at=1:44,label=rep("",44))
text(x = 1:44,
     y = par("usr")[3]-0.014,
     labels = county_names[order(point_est)][1:44],
     xpd = NA,
     srt = 55,
     cex = 0.9,
     adj = 1)
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white",cex=0.8)
dev.off()



png("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Figures\\Figure 1 b for Talk.png",height=4,width=12,unit="in",res=1000)
par(las=2)
par(mar=c(6,6,1,1))
plot(1:44,point_est[order(point_est)][45:88],ylim=c(0.70,1.0),pch=18,ylab=expression(paste("E(",exp(beta[2][i]),"|Y)")),xlab = "",xaxt='n')

for(i in 45:88){
  segments(i-44,LL[order(point_est)][i],i-44,UL[order(point_est)][i])
  segments(i-44-0.2,LL[order(point_est)][i],i-44+0.2,LL[order(point_est)][i])
  segments(i-44-0.2,UL[order(point_est)][i],i-44+0.2,UL[order(point_est)][i])
}
abline(h=1)
abline(h=mean(exp(main_MCMC$samples[,4050])),lty=2)

axis(side=1,at=44:1,label=rep("",44))
text(x = 1:44,
     y = par("usr")[3]-0.014,
     labels = county_names[order(point_est)][45:88],
     xpd = NA,
     srt = 55,
     cex = 0.9,
     adj = 1)

legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white",cex=0.8)
dev.off()

####################################


sup_MCMC <-  readRDS("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Output_evertyhing_independent_30_34.rds")

dimnames(sup_MCMC$samples)[[2]][4000:4500]

# CREATE POINT ESTIMATES AND 95% CREDIBLE INTERVALS
ind_point_est <- ind_LL <- ind_UL <- rep(0,ncounty)
for(i in 1:ncounty){
  
  ind_point_est[i] <- mean(exp(sup_MCMC$samples[,4050+i]))
  ind_LL[i] <- quantile(exp(sup_MCMC$samples[,4050+i]),probs = 0.025)
  ind_UL[i] <- quantile(exp(sup_MCMC$samples[,4050+i]),probs = 1-0.025)
}


plot(ind_point_est,point_est,xlim=c(0.7,0.9),ylim=c(0.75,0.9),pch=18,ylab="Main Model",xlab="Independent Model")
abline(0,1)