

###############################
# STEP 1: DATA PRE-PROCESSING #
###############################

# INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

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


## CREATE ADJACENCY AND WEIGHT MATRICES ##
# Based off of: https://gkonstantinoudis.github.io/nimble/RmarkSLC.html#The_BYM2_model

adj_step1 <- poly2nb(ohio_map,queen=T)
adj_step2 <- nb2WB(nb=adj_step1)

#table(st_is_valid(ohio_map))

W_step1 <- nb2mat(adj_step1,zero.policy=T,style="B")

W_scale <- -W_step1;
diag(W_scale) <- abs(apply(W_scale,1,sum))

Q = inla.scale.model(W_scale, constr=list(A=matrix(1, nrow=1, ncol=nrow(W_scale)), e=0))
scale_parm <- exp((1/nrow(W_scale))*sum(log(1/diag(Q))))

## CREATE NIMBLE DATA ##

nimble_data <- list()

nimble_data$y <- as.numeric(analysis_data$COUNT)
nimble_data$offset <- ifelse(analysis_data$AGEGRP=="30-34",analysis_data$LD_30_34,analysis_data$LD_16_17)
nimble_data$trt <- analysis_data$trt

nimble_data$I03 <-ifelse(analysis_data$year==2003,1,0)
nimble_data$I04 <-ifelse(analysis_data$year==2004,1,0)
nimble_data$I05 <-ifelse(analysis_data$year==2005,1,0)
nimble_data$I06 <-ifelse(analysis_data$year==2006,1,0)
nimble_data$I07 <-ifelse(analysis_data$year==2007,1,0)
nimble_data$I08 <-ifelse(analysis_data$year==2008,1,0)
nimble_data$I09 <-ifelse(analysis_data$year==2009,1,0)
nimble_data$I10 <-ifelse(analysis_data$year==2010,1,0)
nimble_data$I11 <-ifelse(analysis_data$year==2011,1,0)
nimble_data$I12 <-ifelse(analysis_data$year==2012,1,0)
nimble_data$I13 <-ifelse(analysis_data$year==2013,1,0)
nimble_data$I14 <-ifelse(analysis_data$year==2014,1,0)
nimble_data$I15 <-ifelse(analysis_data$year==2015,1,0)
nimble_data$I16 <-ifelse(analysis_data$year==2016,1,0)
nimble_data$I17 <-ifelse(analysis_data$year==2017,1,0)


nimble_data$law = analysis_data$trt*analysis_data$period

for(i in 1:nrow(analysis_data)){
	analysis_data$county_index[i] = which(counties==analysis_data$county[i])
}

# CRASH RATES #
analysis_data_16_17 <- analysis_data[which(analysis_data$AGEGRP=="16-17"),]
sum(analysis_data_16_17$COUNT)/sum(analysis_data_16_17$LD_16_17)*1000


analysis_data_30_34 <- analysis_data[which(analysis_data$AGEGRP=="30-34"),]
sum(analysis_data_30_34$COUNT)/sum(analysis_data_30_34$LD_30_34)*1000


analysis_data_30_34 <- analysis_data[which(analysis_data$AGEGRP=="30-34"),]

overall_ctrl <- sum(analysis_data_30_34$COUNT)/sum(analysis_data_30_34$LD_30_34)*1000
pre_ctrl <- sum(analysis_data_30_34$COUNT[which(analysis_data_30_34$year < 2008)])/sum(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year < 2008)])*1000
post_ctrl <- sum(analysis_data_30_34$COUNT[which(analysis_data_30_34$year >= 2008)])/sum(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year >= 2008)])*1000

diff_ctrl <- sum(analysis_data_30_34$COUNT[which(analysis_data_30_34$year < 2008)])/sum(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year < 2008)])*1000-
sum(analysis_data_30_34$COUNT[which(analysis_data_30_34$year >= 2008)])/sum(analysis_data_30_34$LD_30_34[which(analysis_data_30_34$year >= 2008)])*1000

percent_ctrl <- diff_ctrl/pre_ctrl*100

##################
# START ANALYSIS #
##################

# MIXING PARAMETERS ARE NAMED rho_[LABEL]
# PRECISION PARAMETERS ARE NAMED tau_[LABEL]
# LINEAR COEFFICIENTS ARE NAMED beta_[LABEL]
# FOR BYM2, SPATIAL TERMS ARE NAMED phi_[LABEL], RANDOM NOISE TERMS ARE NAMED theta_[LABEL]

main_code <- 

nimbleCode({
	
	# NON-INFORMATIVE PRIOR FOR VARIANCE, INTERCEPT, GLOBAL MEAN PARAMETERS
	
	beta_0 ~ dnorm(0,tau=1/1000000)
	beta_1 ~ dnorm(0,tau=1/1000000)
	beta_2 ~ dnorm(0,tau=1/1000000)
	tau_beta_1 ~ dgamma(1,0.01)
	tau_beta_2 ~ dgamma(1,0.01)
	tau_t ~ dgamma(1,0.01)


	# SPATIOTEMPORAL RANDOM EFFECTS, AGE AND TREATMENT RANDOM EFFECTS #

	for(i in 1:ncounty){

		alpha_03[i] ~ dnorm(0,tau=tau_t)
		alpha_04[i] ~ dnorm(0,tau=tau_t)
		alpha_05[i] ~ dnorm(0,tau=tau_t)
		alpha_06[i] ~ dnorm(0,tau=tau_t)
		alpha_07[i] ~ dnorm(0,tau=tau_t)
		alpha_08[i] ~ dnorm(0,tau=tau_t)
		alpha_09[i] ~ dnorm(0,tau=tau_t)
		alpha_10[i] ~ dnorm(0,tau=tau_t)
		alpha_11[i] ~ dnorm(0,tau=tau_t)
		alpha_12[i] ~ dnorm(0,tau=tau_t)
		alpha_13[i] ~ dnorm(0,tau=tau_t)
		alpha_14[i] ~ dnorm(0,tau=tau_t)
		alpha_15[i] ~ dnorm(0,tau=tau_t)
		alpha_16[i] ~ dnorm(0,tau=tau_t)
		alpha_17[i] ~ dnorm(0,tau=tau_t)


		######

		# TREATMENT GROUP #
		beta_1i[i] ~ dnorm(beta_1,tau=tau_beta_1)
		
		# POLICY VARIABLE #
		beta_2i[i] ~ dnorm(beta_2,tau=tau_beta_2)
	}

	# DATA MODEL #

	for(i in 1:nobs){
		y[i] ~ dpois(Ey[i])
		log(Ey[i]) <- beta_0 + 
			  	  beta_1i[county_index[i]]*trt[i] + 
			  	  beta_2i[county_index[i]]*law[i] +
			  	  alpha_03[county_index[i]]*I03[i] +
			  	  alpha_04[county_index[i]]*I04[i] +
			  	  alpha_05[county_index[i]]*I05[i] +
			  	  alpha_06[county_index[i]]*I06[i] +
			  	  alpha_07[county_index[i]]*I07[i] +
			  	  alpha_08[county_index[i]]*I08[i] +
			  	  alpha_09[county_index[i]]*I09[i] +
			  	  alpha_10[county_index[i]]*I10[i] +
			  	  alpha_11[county_index[i]]*I11[i] +
			  	  alpha_12[county_index[i]]*I12[i] +
			  	  alpha_13[county_index[i]]*I13[i] +
			  	  alpha_14[county_index[i]]*I14[i] +
			  	  alpha_15[county_index[i]]*I15[i] +
			  	  alpha_16[county_index[i]]*I16[i] +
			  	  alpha_17[county_index[i]]*I17[i] + 
				  log(offset[i]) 
	}
})

ncounty <- length(counties)
constants <- list(nobs=nrow(analysis_data),county_index=analysis_data$county_index, ncounty = length(counties),
			L = length(adj_step2$weights),adj = adj_step2$adj, num = adj_step2$num, weights = adj_step2$weights)

# INITIAL VALUES #

# ROW 1: INTERCEPT AND GLOBAL MEANS FOR LINEAR COEFFICIENTS
# ROW 2: RANDOM LINEAR COEFFICIENTS
# ROW 3: ADDITIONAL PARAMETERS LINEAR COEFFICIENTS
# ROW 4: "DATA" PARAMETERS (EXPECTED VALUE)
# ROWS 5-7: SPATIO-TEMPORAL RANDOM EFFECTS

inits <- list(beta_0=0,beta_1=0,beta_2=0,
		  beta_1i=rep(0,ncounty),beta_2i=rep(0,ncounty),
		  tau_beta_1=0.001,tau_beta_2=0.001,tau_t=0.001,
		  Ey=rep(0,length(nimble_data$y)),
		  alpha_03=rep(0,ncounty),alpha_04=rep(0,ncounty),alpha_05=rep(0,ncounty),alpha_06=rep(0,ncounty),alpha_07=rep(0,ncounty),
		  alpha_08=rep(0,ncounty),alpha_09=rep(0,ncounty),alpha_10=rep(0,ncounty),alpha_11=rep(0,ncounty),alpha_12=rep(0,ncounty),
		  alpha_13=rep(0,ncounty),alpha_14=rep(0,ncounty),alpha_15=rep(0,ncounty),alpha_16=rep(0,ncounty),alpha_17=rep(0,ncounty))



monitor <- c("beta_0","beta_1","beta_2","beta_1i","beta_2i",
		 "alpha_03","alpha_04","alpha_05","alpha_06","alpha_07",
		 "alpha_08","alpha_09","alpha_10","alpha_11","alpha_12",
		 "alpha_13","alpha_14","alpha_15","alpha_16","alpha_17","Ey",
		 "tau_beta_1","tau_beta_2")

main_nimble <- nimbleModel(code=main_code, name = "simple_model", constants = constants,
				     data = nimble_data, inits = inits)

main_compile <- compileNimble(main_nimble)

main_MCMC <- nimbleMCMC(code=main_code, constants = constants,
				  data = nimble_data, inits = inits, monitors = monitor,
				  niter = 50000, nburnin = 10000, thin = 20, nchains = 1, progressBar = TRUE,WAIC=TRUE)


saveRDS(main_MCMC,"C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Output_evertyhing_independent_30_34.rds")


main_MCMC <- readRDS("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Output_evertyhing_independent_30_34.rds")



ncounty <- 88
point_est <- LL <- UL <- rep(0,ncounty)
for(i in 1:ncounty){
	#point_est[i] <- mean(c(main_MCMC$chain1[2000:3000,i],main_MCMC$chain2[,i]))
	#LL[i] <- quantile(c(main_MCMC$chain1[2000:3000,i],main_MCMC$chain2[,i]),probs = 0.025)
	#UL[i] <- quantile(c(main_MCMC$chain1[2000:3000,i],main_MCMC$chain2[,i]),probs = 0.975)

	point_est[i] <- mean(exp(main_MCMC$samples[,4050+i]))
	LL[i] <- quantile(exp(main_MCMC$samples[,4050+i]),probs = 0.025)
	UL[i] <- quantile(exp(main_MCMC$samples[,4050+i]),probs = 0.975)
}

plot(point_est,ylim=c(0.6,1.4),pch=18,ylab="DID Estimate",xlab = "Arbitraty County Order")

for(i in 1:length(counties)){
	segments(i,UL[i],i,LL[i])
	segments(i-0.2,LL[i],i+0.2,LL[i])
	segments(i-0.2,UL[i],i+0.2,UL[i])
}

abline(h=1)


plot(point_est,exp(point_est_F),xlab="Bayesian Estimate (Model 1)",
		ylab="Frequentist Estimate",pch=18,xlim=c(0.65,1.1))
abline(0,1)

fitted <- apply(main_MCMC$samples[,1:2639],2,mean)
plot(nimble_data$y,fitted)
abline(0,1)
plot(nimble_data$y-fitted)


plot(1/main_MCMC$samples[,3917],type='l')



## ORDER BY EFFECT SIZE ##

county_names <- rep("",ncounty)
for(i in 1:ncounty){
  tmp <- LD_data$CTYNAME[which(LD_data$county==counties[i])][1]
  county_names[i] <- substr(tmp,1,nchar(tmp)-7)
}
png("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Figures\\Figure S1a.png",height=12,width=6,unit="in",res=1000)

par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(point_est)][1:44],44:1,xlim=c(0.63,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 1:44){
  segments(LL[order(point_est)][i],45-i,UL[order(point_est)][i],45-i)
  segments(LL[order(point_est)][i],45-i-0.2,LL[order(point_est)][i],45-i+0.2)
  segments(UL[order(point_est)][i],45-i-0.2,UL[order(point_est)][i],45-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(point_est)][1:44])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")
dev.off()



png("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Figures\\Figure S1b.png",height=12,width=6,unit="in",res=1000)
par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(point_est)][45:88],44:1,xlim=c(0.63,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 45:88){
  segments(LL[order(point_est)][i],89-i,UL[order(point_est)][i],89-i)
  segments(LL[order(point_est)][i],89-i-0.2,LL[order(point_est)][i],89-i+0.2)
  segments(UL[order(point_est)][i],89-i-0.2,UL[order(point_est)][i],89-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(point_est)][45:88])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")
dev.off()



## ORDER BY POPULATION SIZE ##


counties_pop_order <- counties[order(subset_pop$TOT_POP)]

png("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Figures\\Figure S2a.png",height=12,width=6,unit="in",res=1000)
par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(subset_pop$TOT_POP)][1:44],44:1,xlim=c(0.63,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 1:44){
  segments(LL[order(subset_pop$TOT_POP)][i],45-i,UL[order(subset_pop$TOT_POP)][i],45-i)
  segments(LL[order(subset_pop$TOT_POP)][i],45-i-0.2,LL[order(subset_pop$TOT_POP)][i],45-i+0.2)
  segments(UL[order(subset_pop$TOT_POP)][i],45-i-0.2,UL[order(subset_pop$TOT_POP)][i],45-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(subset_pop$TOT_POP)][1:44])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")
dev.off()



png("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Figures\\Figure S2b.png",height=12,width=6,unit="in",res=1000)
par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(subset_pop$TOT_POP)][45:88],44:1,xlim=c(0.63,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 45:88){
  segments(LL[order(subset_pop$TOT_POP)][i],89-i,UL[order(subset_pop$TOT_POP)][i],89-i)
  segments(LL[order(subset_pop$TOT_POP)][i],89-i-0.2,LL[order(subset_pop$TOT_POP)][i],89-i+0.2)
  segments(UL[order(subset_pop$TOT_POP)][i],89-i-0.2,UL[order(subset_pop$TOT_POP)][i],89-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(subset_pop$TOT_POP)][45:88])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")
dev.off()



####################
# POLICE VARIABLES #
####################

plot((police_data$num_dept),point_est,pch=18,ylim=c(0.6,1.4),xlab="Number of Police Departments",ylab="DID Estimate")
for(i in 1:length(counties)){
	segments((police_data$num_dept[i]),LL[i],(police_data$num_dept[i]),UL[i])
	segments((police_data$num_dept[i])-0.2,LL[i],(police_data$num_dept[i])+0.2,LL[i])
	segments((police_data$num_dept[i])-0.2,UL[i],(police_data$num_dept[i])+0.2,UL[i])
}
abline(h=1)


plot((police_data$num_officer),point_est,pch=18,ylim=c(0.6,1.4),xlab="Number of Full Time Police Officers",ylab="DID Estimate")
for(i in 1:length(counties)){
	segments((police_data$num_officer[i]),LL[i],(police_data$num_officer[i]),UL[i])
	segments((police_data$num_officer[i])-30,LL[i],(police_data$num_officer[i])+30,LL[i])
	segments((police_data$num_officer[i])-30,UL[i],(police_data$num_officer[i])+30,UL[i])
}
abline(h=1)

plot((police_data$officers_per*1000),point_est,pch=18,ylim=c(0.6,1.4),xlab="Number of Full Time Police Officers per 1000 16-17 Uear-old Licensed Drivers",ylab="DID Estimate")
for(i in 1:length(counties)){
	segments((police_data$officers_per[i]*1000),LL[i],(police_data$officers_per[i]*1000),UL[i])
	segments((police_data$officers_per[i]*1000)-0.3,LL[i],(police_data$officers_per[i]*1000)+0.3,LL[i])
	segments((police_data$officers_per[i]*1000)-0.3,UL[i],(police_data$officers_per[i]*1000)+0.3,UL[i])
}
abline(h=1)


plot((police_data$budget/100000),point_est,pch=18,ylim=c(0.6,1.4),xlab="Total Police Budget (100k)",ylab="DID Estimate")
for(i in 1:length(counties)){
	segments((police_data$budget[i]/100000),LL[i],(police_data$budget[i]/100000),UL[i])
	segments((police_data$budget[i]/100000)-50,LL[i],(police_data$budget[i]/100000)+50,LL[i])
	segments((police_data$budget[i]/100000)-50,UL[i],(police_data$budget[i]/100000)+50,UL[i])
}
abline(h=1)


plot((police_data$budget_per),point_est,pch=18,ylim=c(0.6,1.4),xlab="Total Police Budget per 16-17 Uear-old Licensed Drivers",ylab="DID Estimate")
for(i in 1:length(counties)){
	segments((police_data$budget_per[i]),LL[i],(police_data$budget_per[i]),UL[i])
	segments((police_data$budget_per[i])-50,LL[i],(police_data$budget_per[i])+50,LL[i])
	segments((police_data$budget_per[i])-50,UL[i],(police_data$budget_per[i])+50,UL[i])
}
abline(h=1)


########
# MAPS #
########

require(RColorBrewer)
require(sf)



colors <- rev(brewer.pal(11,"RdYlBu"))[1:6]
colors[length(colors)] <- "#FFFFFF"
color_class <- cut(point_est,breaks=quantile(point_est,probs = c(0,0.1,0.25,0.5,0.75,0.942,1.0)))

#color_class <- cut(point_est,breaks=c(0.76,0.85,0.88,0.91,0.95,1.00,1.06))

levels(color_class)

my_colors <- colors[as.numeric(color_class)]

DID_data <- data.frame(counties,point_est,color_class,my_colors)
DID_data$COUNTYFP <- str_pad(DID_data$counties-39000,3,pad="0")

ohio_map2 <- merge(ohio_map,DID_data,by=c("COUNTYFP"),sort=TRUE)

legend_text <- rev(levels(color_class))
legend_text[1:2] <- c("(1.00,1.06]","(0.95,1.00]")

png("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Output\\ohio_map.PNG",height=650,width=650)
plot(ohio_map2[1],col=my_colors,main="")
legend("bottomright",legend=rev(levels(color_class)),fill=rev(colors),title="Point Estimate")
dev.off()



###############
# UPPER LIMIT #
###############

colors <- rev(brewer.pal(11,"RdYlBu"))[1:6]
colors[length(colors)] <- "#FFFFFF"
#color_class <- cut(UL,breaks=quantile(UL,probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0)))
#levels(color_class)

color_class <- cut(UL,breaks=c(0.85,0.88,0.91,0.94,0.98,1.00,1.23))


my_colors <- colors[as.numeric(color_class)]

DID_data <- data.frame(counties,UL,color_class,my_colors)
DID_data$COUNTYFP <- str_pad(DID_data$counties-39000,3,pad="0")

ohio_map2 <- merge(ohio_map,DID_data,by=c("COUNTYFP"),sort=TRUE)

legend_text <- rev(levels(color_class))
legend_text[1:2] <- c("(1.00,1.24]","(0.98,1.00]")

png("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Output\\UL_ohio_map.PNG",height=650,width=650)
plot(ohio_map2[1],col=my_colors,main="")
legend("bottomright",legend=legend_text,fill=rev(colors),title="Point Estimate")
dev.off()


################################
# HIGHLIGHT METROPOLITAN AREAS #
################################

st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y))
  do.call("c", lapply(x, f))
}



# THREE BIGGEST CITIES #

bold_map1 <- ohio_map2[which(ohio_map2$COUNTYFP %in% c("035","049","061")),]

plot(ohio_map2[1],col=my_colors,main="");par(new=TRUE)
plot(ohio_map2[1][which(ohio_map2$COUNTYFP %in% c("035","049","061")),],lwd=5,col=NA,main="",xlim=st_bbox(ohio_map2)[c(1,3)],ylim=st_bbox(ohio_map2)[c(2,4)])
legend("bottomright",legend=legend_text,fill=rev(rev(brewer.pal(11,"RdYlBu"))[1:6]),title="Point Estimate")


##########################
# LARGE COUNTIES VS OHIO #
##########################
png("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Figures\\Figure s3.PNG",height=400,width=400)
biggest <- c(39035,39049,39061,39153,39113,39095)
boxplot(point_est~as.numeric(counties %in% biggest),xaxt="n",xlab="",ylab=expression(paste("E(",exp(beta[2]),"|Y)")))
axis(1,at=c(1,2),labels=c("Rest of Ohio","Ohio's Six Largest Counties"))
dev.off()

########################
# URBAN INFLUENCE CODE #
########################

boxplot(point_est~as.numeric(UIC$UIC %in% c(1,2)),ylab = "DID Point Estimate",xlab="Metro vs. Non-metro (Metro = 1)",ylim=c(0.7,1.1))
summary(lm(point_est~as.numeric(UIC$UIC %in% c(1,2)),weight=subset_pop$TOT_POP))


UIC$UIC2 <- ifelse(UIC$UIC %in% c(1,2,3,5),UIC$UIC,6)
boxplot(point_est~UIC$UIC2,ylab = "DID Point Estimate",xlab="UIC",ylim=c(0.7,1.1),xaxt="n")
axis(1,at=c(1,2,3,4,5),labels = c("Large Metro","Small Metro","Micropolitan (large adj.)","Micropolitan (small adj.)","Non-core or non-metro adj"))

summary(lm(point_est~as.factor(UIC$UIC2),weight=subset_pop$TOT_POP))

UIC$UIC3 <- rep(0,nrow(UIC))
for (i in 1:nrow(UIC)){
	if(UIC$UIC[i] == 1){UIC$UIC3[i] <- "1: Large Metro"}
	if(UIC$UIC[i] == 2){UIC$UIC3[i] <- "2: Small Metro"}
	if(UIC$UIC[i] %in% c(3,4)){UIC$UIC3[i] <- "3: Metro Adjacent"}
	if(UIC$UIC[i] %in% c(5,6,7)){UIC$UIC3[i] <- "3: Metro Adjacent"}
	if(UIC$UIC[i] > 7){UIC$UIC3[i] <- "Not Metro Adjacent"}

}

png("C:\\Users\\bened\\Documents\\GDL Paper\\Code\\Upload to Github\\Output\\Figures\\Figure S4.png",height=6,width=8,unit="in",res=1000)
boxplot(point_est~UIC$UIC3,ylab = expression(paste("E(",exp(beta[2][i]),"|Y)")),xlab="Urban Influence Code",ylim=c(0.7,0.9))
dev.off()


UIC$UIC4 <- rep(0,nrow(UIC))
for (i in 1:nrow(UIC)){
	if(UIC$UIC[i] %in% c(1,2)){UIC$UIC4[i] <- "Metro"}
	if(UIC$UIC[i] %in% c(3,5)){UIC$UIC4[i] <- "Micropolitan Metro Adjacent"}
	if(UIC$UIC[i] %in% c(4,6,7)){UIC$UIC4[i] <- "Non-core Metro Adjacent"}
	if(UIC$UIC[i] > 7){UIC$UIC4[i] <- "Not Metro Adjacent"}

}

boxplot(point_est~UIC$UIC4,ylab = "DID Point Estimate",xlab="UIC",ylim=c(0.7,1.1))
summary(lm(point_est~as.factor(UIC$UIC4)))

tapply(point_est,UIC$UIC,summary)
tapply(point_est,as.numeric(UIC$UIC %in% c(1,2)),summary)
tapply(point_est,UIC$UIC3,summary)
tapply(point_est,UIC$UIC4,summary)

saveRDS(interaction_BYM_MCMC,"R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Output\\Output_1617_vs_1824_remove_b.rds")
