

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
basedata_trt <- read.csv("[PARENT FOLDER]\\Data\\OhioCrashes_1617.csv")
basedata_ctrl <- read.csv("[PARENT FOLDER]\\Data\\OhioCrashes_1824.csv")

LD_data <- read.csv("[PARENT FOLDER]\\Data\\Num_LD_Main_File.csv")

pop_00_09 <- read.csv("[PARENT FOLDER]\\Data\\age_grp_ohio_county_pop_00_09.csv")
pop_10_18 <- read.csv("[PARENT FOLDER]\\Data\\age_grp_ohio_county_pop_10_18.csv")

poverty_2008 <- read.csv("[PARENT FOLDER]\\Data\\est08all.csv")
UIC <- read.csv("[PARENT FOLDER]\\Data\\Ohio_UIC.csv")

county_map <- st_read("[PARENT FOLDER]\\Data\\shapefile\\cb_2018_us_county_500k.shp")
ohio_map <- county_map[which(county_map$STATEFP=="39"),]
rm(county_map)

police_data <- read.csv("[PARENT FOLDER]\\Data\\police_data.csv")

vmt <- read.csv("[PARENT FOLDER]\\Data\\VMT2008.csv")


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
nimble_data$offset <- ifelse(analysis_data$AGEGRP=="18-24",analysis_data$LD_18_24,analysis_data$LD_16_17)
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




##################
# START ANALYSIS #
##################


# MIXING PARAMETERS ARE NAMED rho_[LABEL]
# PRECISION PARAMETERS ARE NAMED tau_[LABEL]
# LINEAR COEFFICIENTS ARE NAMED beta_[LABEL]
# FOR BYM2, SPATIAL TERMS ARE NAMED phi_[LABEL], RANDOM NOISE TERMS ARE NAMED theta_[LABEL]

main_code <- 

nimbleCode({
	
	# NON-INFORMATIVE PRIOR FOR INTERCEPT, GLOBAL MEAN PARAMETERS
	
	beta_0 ~ dnorm(0,tau=1/1000000)
	beta_1 ~ dnorm(0,tau=1/1000000)
	beta_2 ~ dnorm(0,tau=1/1000000)
	tau_beta_1 ~ dgamma(1,0.01)


	# CAR, VARIANCE, MIXING COMPONENTS OF COMPONENT OF BYM2 PRIOR FOR POLICY EFFECT (beta_2i) #

	tau_theta_beta_2 ~ dgamma(1,0.01)
	tau_phi_beta_2 ~ dgamma(1,0.01)
	tau_beta_2 ~ dgamma(1,0.01)
	phi_beta_2[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_beta_2, zero_mean = 1)
	rho_beta_2 ~ dbeta(1,1)


	# CAR, VARIANCE, MIXING COMPONENTS OF BYM2 PRIORS FOR TIME EFFECTS (alpha_ti)#

	tau_theta_t ~ dgamma(1,0.01)
	tau_phi_t ~ dgamma(1,0.01)
	tau_t ~ dgamma(1,0.01)
	rho_t ~ dbeta(1,1)
	rho_alpha ~ dbeta(1,1)
	
	phi_03[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_04[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_05[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_06[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_07[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_08[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_09[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_10[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_11[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_12[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_13[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_14[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_15[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_16[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)
	phi_17[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 0)


	# SPATIOTEMPORAL RANDOM EFFECTS, AGE AND TREATMENT RANDOM EFFECTS #

	for(i in 1:ncounty){

		alpha_03[i] <- (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_03[i] + sqrt(rho_t/scale_parm)*phi_03[i])
		theta_03[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_04[i] <- rho_alpha*alpha_03[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_04[i] + sqrt(rho_t/scale_parm)*phi_04[i])
		theta_04[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_05[i] <- rho_alpha*alpha_04[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_05[i] + sqrt(rho_t/scale_parm)*phi_05[i])
		theta_05[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_06[i] <- rho_alpha*alpha_05[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_06[i] + sqrt(rho_t/scale_parm)*phi_06[i])
		theta_06[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_07[i] <- rho_alpha*alpha_06[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_07[i] + sqrt(rho_t/scale_parm)*phi_07[i])
		theta_07[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_08[i] <- rho_alpha*alpha_07[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_08[i] + sqrt(rho_t/scale_parm)*phi_08[i])
		theta_08[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_09[i] <- rho_alpha*alpha_08[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_09[i] + sqrt(rho_t/scale_parm)*phi_09[i])
		theta_09[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_10[i] <- rho_alpha*alpha_09[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_10[i] + sqrt(rho_t/scale_parm)*phi_10[i])
		theta_10[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_11[i] <- rho_alpha*alpha_10[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_11[i] + sqrt(rho_t/scale_parm)*phi_11[i])
		theta_11[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_12[i] <- rho_alpha*alpha_11[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_12[i] + sqrt(rho_t/scale_parm)*phi_12[i])
		theta_12[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_13[i] <- rho_alpha*alpha_12[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_13[i] + sqrt(rho_t/scale_parm)*phi_13[i])
		theta_13[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_14[i] <- rho_alpha*alpha_13[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_14[i] + sqrt(rho_t/scale_parm)*phi_14[i])
		theta_14[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_15[i] <- rho_alpha*alpha_14[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_15[i] + sqrt(rho_t/scale_parm)*phi_15[i])
		theta_15[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_16[i] <- rho_alpha*alpha_15[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_16[i] + sqrt(rho_t/scale_parm)*phi_16[i])
		theta_16[i] ~ dnorm(0,tau=tau_theta_t)

		alpha_17[i] <- rho_alpha*alpha_16[i] + (1/sqrt(tau_t))*(sqrt((1-rho_t))*theta_17[i] + sqrt(rho_t/scale_parm)*phi_17[i])
		theta_17[i] ~ dnorm(0,tau=tau_theta_t)



		######

		# TREATMENT GROUP #
		beta_1i[i] ~ dnorm(beta_1,tau=tau_beta_1)
		
		# POLICY VARIABLE
		beta_2i[i] <- beta_2 + (1/sqrt(tau_beta_2))*(sqrt((1-rho_beta_2))*theta_beta_2[i] + sqrt(rho_beta_2/scale_parm)*phi_beta_2[i])
		theta_beta_2[i] ~ dnorm(0,tau=tau_theta_beta_2)
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
			L = length(adj_step2$weights),adj = adj_step2$adj, num = adj_step2$num, weights = adj_step2$weights,scale_parm=scale_parm)

# INITIAL VALUES #

# ROW 1: INTERCEPT AND GLOBAL MEANS FOR LINEAR COEFFICIENTS
# ROW 2: RANDOM LINEAR COEFFICIENTS
# ROW 3: ADDITIONAL PARAMETERS FOR TREATMENT GROUP TERM (beta_1i)
# ROW 4: ADDITIONAL PARAMETERS FOR POLICY EFFECT TERM (beta_2i)
# ROW 5: "DATA" PARAMETERS (EXPECTED VALUE)
# ROWS 6-8: SPATIAL MIXTURE TERMS FOR SPATIO-TEMPORAL RANDOM EFFECTS
# ROWS 9-11: RANDOM NOISE TERMS FOR SPATIO-TEMPORAL RANDOM EFFECTS
# ROW 12: ADDITIONAL PARAMETERS FOR SPATIOTEMPORAL RANDOM EFFECTS 

inits <- list(beta_0=0,beta_1=0,beta_2=0,
		  beta_1i=rep(0,ncounty),beta_2i=rep(0,ncounty),
		  tau_beta_1=0.0001,
		  tau_theta_beta_2=0.0001,tau_phi_beta_2=0.0001,tau_beta_2=0.0001,rho_beta_2=0.5,phi_beta_2=rep(0,ncounty),theta_beta_2=rep(0,ncounty),
		  Ey=rep(0,length(nimble_data$y)),
		  phi_03=rep(0,ncounty),phi_04=rep(0,ncounty),phi_05=rep(0,ncounty),phi_06=rep(0,ncounty),phi_07=rep(0,ncounty),
		  phi_08=rep(0,ncounty),phi_09=rep(0,ncounty),phi_10=rep(0,ncounty),phi_11=rep(0,ncounty),phi_12=rep(0,ncounty),
		  phi_13=rep(0,ncounty),phi_14=rep(0,ncounty),phi_15=rep(0,ncounty),phi_16=rep(0,ncounty),phi_17=rep(0,ncounty),
		  theta_03=rep(0,ncounty),theta_04=rep(0,ncounty),theta_05=rep(0,ncounty),theta_06=rep(0,ncounty),theta_07=rep(0,ncounty),
		  theta_08=rep(0,ncounty),theta_09=rep(0,ncounty),theta_10=rep(0,ncounty),theta_11=rep(0,ncounty),theta_12=rep(0,ncounty),
		  theta_13=rep(0,ncounty),theta_14=rep(0,ncounty),theta_15=rep(0,ncounty),theta_16=rep(0,ncounty),theta_17=rep(0,ncounty),
		  tau_theta_t=0.0001,tau_phi_t=0.0001,tau_t=0.0001,rho_t=0.5,rho_alpha=0.5)



monitor <- c("beta_0","beta_1","beta_2","beta_1i","beta_2i","rho_beta_2",
		 "alpha_03","alpha_04","alpha_05","alpha_06","alpha_07",
		 "alpha_08","alpha_09","alpha_10","alpha_11","alpha_12",
		 "alpha_13","alpha_14","alpha_15","alpha_16","alpha_17","Ey")

main_nimble <- nimbleModel(code=main_code, name = "simple_model", constants = constants,
				     data = nimble_data, inits = inits)

main_compile <- compileNimble(main_nimble)


# RUN CODE
main_MCMC <- nimbleMCMC(code=main_code, constants = constants,
				  data = nimble_data, inits = inits, monitors = monitor,
				  niter = 50000, nburnin = 10000, thin = 20, nchains = 1, progressBar = TRUE,WAIC=TRUE)

# SAVE OUTPUT
main_MCMC <- saveRDS(main_MCMC,"[PARENT FOLDER]\\Output\\Output_beta_1_independent.rds")

# IF YOU WANT TO JUST READ IN THE EXISTING OUTPUT:
#main_MCMC <- readRDS("[PARENT FOLDER]\\Output\\Output_beta_1_independent.rds")

# DOUBLE CHECK INDICES. WE'RE LOOKING FOR THE TERMS LABELED "beta_2i[]"
#dimnames(main_MCMC$samples)


# CREATE POINT ESTIMATES AND 95% CREDIBLE INTERVALS
ncounty <- 88
point_est <- LL <- UL <- rep(0,ncounty)
for(i in 1:ncounty){
	#point_est[i] <- mean(c(main_MCMC$chain1[,i],main_MCMC$chain2[,i]))
	#LL[i] <- quantile(c(main_MCMC$chain1[,i],main_MCMC$chain2[,i]),probs = 0.025)
	#UL[i] <- quantile(c(main_MCMC$chain1[,i],main_MCMC$chain2[,i]),probs = 0.975)

	point_est[i] <- mean(exp(main_MCMC$samples[,4050+i]))
	LL[i] <- quantile(exp(main_MCMC$samples[,4050+i]),probs = 0.025)
	UL[i] <- quantile(exp(main_MCMC$samples[,4050+i]),probs = 1-0.025)
}


# PLOT EFFECT ESTIMATES AND 95% CREDIBLE INTERVALS
# ORDER BASED ON FIPS CODE

county_names <- rep("",ncounty)
for(i in 1:ncounty){
	tmp <- LD_data$CTYNAME[which(LD_data$county==counties[i])][1]
	county_names[i] <- substr(tmp,1,nchar(tmp)-7)
}

par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[1:44],44:1,xlim=c(0.78,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = '',yaxt='n')
for(i in 1:44){
	segments(UL[i],45-i,LL[i],45-i)
	segments(LL[i],45-i-0.2,LL[i],45-i+0.2)
	segments(UL[i],45-i-0.2,UL[i],45-i+0.2)
}

abline(v=1)
axis(side=2,at=44:1,labels=county_names[1:44])


par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[45:88],44:1,xlim=c(0.78,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = '',yaxt='n')
for(i in 45:88){
	segments(UL[i],89-i,LL[i],89-i)
	segments(LL[i],89-i-0.2,LL[i],89-i+0.2)
	segments(UL[i],89-i-0.2,UL[i],89-i+0.2)
}

abline(v=1)
axis(side=2,at=44:1,labels=county_names[45:88])



# TRACE PLOTS

par(mfrow=c(2,1))
plot(exp(main_MCMC$samples[,4051]),type='l',main="beta_2i Adams County", xlab="Iteration",ylab="beta_2i",ylim=c(0.8,1))
plot(exp(main_MCMC$samples[,4068]),type='l',main="beta_2i Cuyahoga County", xlab="Iteration",ylab="beta_2i",ylim=c(0.8,1))



## ORDER BY EFFECT SIZE ##
par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(point_est)][1:44],44:1,xlim=c(0.78,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 1:44){
	segments(LL[order(point_est)][i],45-i,UL[order(point_est)][i],45-i)
	segments(LL[order(point_est)][i],45-i-0.2,LL[order(point_est)][i],45-i+0.2)
	segments(UL[order(point_est)][i],45-i-0.2,UL[order(point_est)][i],45-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(point_est)][1:44])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")


par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(point_est)][45:88],44:1,xlim=c(0.78,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 45:88){
	segments(LL[order(point_est)][i],89-i,UL[order(point_est)][i],89-i)
	segments(LL[order(point_est)][i],89-i-0.2,LL[order(point_est)][i],89-i+0.2)
	segments(UL[order(point_est)][i],89-i-0.2,UL[order(point_est)][i],89-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(point_est)][45:88])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")
#dev.off()


# ORDER BY POP SIZE

pop <- subset_pop$TOT_POP

par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(pop)][1:44],44:1,xlim=c(0.78,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 1:44){
	segments(LL[order(pop)][i],45-i,UL[order(pop)][i],45-i)
	segments(LL[order(pop)][i],45-i-0.2,LL[order(pop)][i],45-i+0.2)
	segments(UL[order(pop)][i],45-i-0.2,UL[order(pop)][i],45-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(pop)][1:44])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")


par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(pop)][45:88],44:1,xlim=c(0.78,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 45:88){
	segments(LL[order(pop)][i],89-i,UL[order(pop)][i],89-i)
	segments(LL[order(pop)][i],89-i-0.2,LL[order(pop)][i],89-i+0.2)
	segments(UL[order(pop)][i],89-i-0.2,UL[order(pop)][i],89-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(pop)][45:88])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")




## ORDER BY VMT ##
par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(total)][1:44],44:1,xlim=c(0.78,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 1:44){
	segments(LL[order(total)][i],45-i,UL[order(total)][i],45-i)
	segments(LL[order(total)][i],45-i-0.2,LL[order(total)][i],45-i+0.2)
	segments(UL[order(total)][i],45-i-0.2,UL[order(total)][i],45-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(total)][1:44])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")



par(las=1)
par(mar=c(6,6,1,1))
plot(point_est[order(total)][45:88],44:1,xlim=c(0.78,1.0),pch=18,xlab=expression(paste("E(",exp(beta[2][i]),"|Y)")),ylab = "",yaxt='n')

for(i in 45:88){
	segments(LL[order(total)][i],89-i,UL[order(total)][i],89-i)
	segments(LL[order(total)][i],89-i-0.2,LL[order(total)][i],89-i+0.2)
	segments(UL[order(total)][i],89-i-0.2,UL[order(total)][i],89-i+0.2)
}
abline(v=1)
abline(v=mean(exp(main_MCMC$samples[,4050])),lty=2)
axis(side=2,at=44:1,labels=county_names[order(total)][45:88])
legend("topright",lty=c(2,1),legend=c(expression(paste("E(",exp(beta[2]),"|Y)")),"Null Value"),bg="white")



######### POPULATION BOXPLOTS ############

boxplot(point_est~cut(subset_pop$TOT_POP,breaks=c(0,4000,8000,30000,1000000)),ylab = expression(paste("E(",exp(beta[2][i]),"|Y)")),xaxt='n',xlab="2008 Populaion",ylim=c(0.82,0.93))
axis(1,at=c(1,2,3,5),labels=c("<40,000","40,000-79,999","80,000-299,999","Pop > 300,000"))


boxplot(point_est~cut(1/subset_pop$TOT_POP,breaks=c(1/10000000,1/30000,1)),ylab = expression(paste("E(",exp(beta[2][i]),"|Y)")),xaxt='n',xlab="",ylim=c(0.82,0.93))
axis(1,at=c(1,2),labels=c("Ohio's Six Largest Counties","Rest of Ohio"))



########
# MAPS #
########

colors <- rev(brewer.pal(11,"RdYlBu"))[1:6]

# MAKE THE LIGHTEST COLOR WHITE
colors[length(colors)] <- "#FFFFFF"

# COLOR BASED ON QUANTILES
color_class <- cut(point_est,breaks=c(0.83,0.85,0.87,0.89,0.91,0.93,0.95))

# CUSTOM COLOR CLASS
#color_class <- cut(point_est,breaks=c(FILL IN BEAK POINTS))

levels(color_class)

my_colors <- colors[as.numeric(color_class)]

DID_data <- data.frame(counties,point_est,color_class,my_colors)
DID_data$COUNTYFP <- str_pad(DID_data$counties-39000,3,pad="0")

ohio_map2 <- merge(ohio_map,DID_data,by=c("COUNTYFP"),sort=TRUE)

legend_text <- rev(levels(color_class))[2:6]


plot(ohio_map2[1],col=my_colors,main="")
legend("bottomright",legend=legend_text,fill=rev(colors)[2:6],title=expression(paste("E(",exp(beta[2][i]),"|Y)")))




########################
# URBAN INFLUENCE CODE #
########################

boxplot(point_est~as.numeric(UIC$UIC %in% c(1,2)),ylab = "DID Point Estimate",xlab="Metro vs. Non-metro (Metro = 1)",ylim=c(0.8,0.95))
summary(lm(point_est~as.numeric(UIC$UIC %in% c(1,2)),weight=subset_pop$TOT_POP))

UIC$UIC2 <- ifelse(UIC$UIC %in% c(1,2,3,5),UIC$UIC,6)
boxplot(point_est~UIC$UIC2,ylab = "DID Point Estimate",xlab="UIC",ylim=c(0.8,0.95),xaxt="n")
axis(1,at=c(1,2,3,4,5),labels = c("Large Metro","Small Metro","Micropolitan (large adj.)","Micropolitan (small adj.)","Non-core or non-metro adj"))

summary(lm(point_est~as.factor(UIC$UIC2),weight=subset_pop$TOT_POP))

UIC$UIC3 <- rep(0,nrow(UIC))
for (i in 1:nrow(UIC)){
	if(UIC$UIC[i] == 1){UIC$UIC3[i] <- 1}
	if(UIC$UIC[i] == 2){UIC$UIC3[i] <- 2}
	if(UIC$UIC[i] %in% c(3,4,5,6,7)){UIC$UIC3[i] <- 3}
	if(UIC$UIC[i] > 7){UIC$UIC3[i] <- 4}
}

boxplot(point_est~UIC$UIC3,ylab = "DID Point Estimate",xlab="Urban Influence Code",ylim=c(0.82,0.93),xaxt='n')
axis(1,at=c(1,2,3,4),labels = c("Large Metro","Small Metro","Metro Adjacent","Not Metro Adjacent"))



UIC$UIC4 <- rep(0,nrow(UIC))
for (i in 1:nrow(UIC)){
	if(UIC$UIC[i] %in% c(1,2)){UIC$UIC4[i] <- "Metro"}
	if(UIC$UIC[i] %in% c(3,5)){UIC$UIC4[i] <- "Micropolitan Metro Adjacent"}
	if(UIC$UIC[i] %in% c(4,6,7)){UIC$UIC4[i] <- "Non-core Metro Adjacent"}
	if(UIC$UIC[i] > 7){UIC$UIC4[i] <- "Not Metro Adjacent"}

}

boxplot(point_est~UIC$UIC4,ylab = "DID Point Estimate",xlab="UIC",ylim=c(0.8,0.95))
summary(lm(point_est~as.factor(UIC$UIC4)))

tapply(point_est,UIC$UIC,summary)
tapply(point_est,as.numeric(UIC$UIC %in% c(1,2)),summary)
tapply(point_est,UIC$UIC3,summary)
tapply(point_est,UIC$UIC4,summary)



#######
# VMT #
#######

plot(interstate,point_est,pch=18)
plot(total,point_est,pch=18,col=UIC$UIC2)
plot(total_per_pop,point_est,pch=18)
plot(pct_urban,point_est,pch=18)

plot(total[order(total)],point_est[order(total)],pch=18)
#quantile(total,probs=c(0,0.25,0.5,0.75,0.9,1))


