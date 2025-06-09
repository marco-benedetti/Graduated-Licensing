
# TO DO: UPDATE PHI VARIANCE AND OUTPUT FITTED VALUES
# CHECK: NO SPATIAL DEPENDENCE IN POLICY EFFECTS

###############################
# STEP 1: DATA PRE-PROCESSING #
###############################

# INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("did")
#install.packages("stringr")
#install.packages("tidycensus")
#install.packages("nimble")
#install.packages("sf")
#install.packages("INLA")

require(INLA)
require(sf)
require(spdep)
require(nimble)
require(did)
require(stringr)
require(tidycensus)

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

## PROCESS THE POPULATION DATA ##

# Remove extra columns
## As this program evolves, use extra care in this step. Confirm that you are removing the correct columns.
pop_00_09_step1 <- pop_00_09[,c(3,5,7,8,9)]
pop_10_18_step1 <- pop_10_18[,c(3,5,7,8,9)]


# Combine two data sets
pop_data_step1 <- rbind(pop_00_09_step1,pop_10_18_step1)

# Create county variable to merge later on
pop_data_step1$county <- paste("39",str_pad(pop_data_step1$COUNTY,3,pad="0"),sep="")

# Reformat year, pop, and age group
pop_data_step1$year <- as.numeric(pop_data_step1$year)
#pop_data_step1$TOT_POP <- as.numeric(pop_data_step1$TOT_POP)
pop_data_step1$AGEGRP <- as.numeric(pop_data_step1$AGEGRP)
pop_data_step1$TOT_POP <- ifelse(pop_data_step1$AGEGRP==4,as.numeric(pop_data_step1$TOT_POP)*0.4,as.numeric(pop_data_step1$TOT_POP))

# Subset to 16-17 and 18-24
pop_data_step2 <- pop_data_step1[which(pop_data_step1$AGEGRP %in% c(4,5)),]

# Sort
pop_data_step3 <- pop_data_step2[order(pop_data_step2$county,pop_data_step2$AGEGRP,pop_data_step2$year),]

# Remove extra columns
## As this program evolves, use extra care in this step. Confirm that you are removing the correct columns.

pop_data <- pop_data_step3[,c(2,3,4,5,6)]
pop_data$county <- as.numeric(pop_data$county)
names(pop_data)[3]="age"

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


## MERGE INJURY AND POPULATION DATA ##

# Merge
trt_data_step3 <- merge(trt_data_step2,pop_data[which(pop_data$age==4),],by = c("county","year"))
ctrl_data_step3 <- merge(ctrl_data_step2,pop_data[which(pop_data$age==5),],by = c("county","year"))


# Create some variables
trt_data_step3$AGEGRP = rep("16-17",nrow(trt_data_step3))
trt_data_step3$trt = rep(1,nrow(trt_data_step3))
trt_data_step3$period <- as.numeric(trt_data_step3$year > 2007)
trt_data_step3$injury_rate <- trt_data_step3$COUNT/trt_data_step3$TOT_POP*1000

ctrl_data_step3$AGEGRP = rep("18-24",nrow(ctrl_data_step3))
ctrl_data_step3$trt = rep(0,nrow(ctrl_data_step3))
ctrl_data_step3$period <- as.numeric(ctrl_data_step3$year > 2007)
ctrl_data_step3$injury_rate <- ctrl_data_step3$COUNT/ctrl_data_step3$TOT_POP*1000

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
nimble_data$offset <- analysis_data$TOT_POP
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
# SPAGHETTI PLOT #
##################


counties <- as.numeric(names(table(analysis_data$county)))
plot(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     analysis_data$injury_rate[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="16-17"))],
     type="l",ylim=c(0,215),xlab="Year",ylab="Injury Rate per Capita",col=2)
lines(analysis_data$year[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="18-24"))],
     analysis_data$injury_rate[which((analysis_data$county == counties[1]) & (analysis_data$AGEGRP=="18-24"))],col=4)

for(i in 1:length(counties)){

	tmp <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="16-17")),]
	lines(tmp$year,tmp$injury_rate,col=2)

	tmp2 <- analysis_data[which((analysis_data$county == counties[i]) & (analysis_data$AGEGRP=="18-24")),]
	lines(tmp2$year,tmp2$injury_rate,col=4)

}
abline(v=2007.5)



##################
# START ANALYSIS #
##################


# CRASH RATES #
# TREATED GROUP #
analysis_data_16_17 <- analysis_data[which(analysis_data$AGEGRP=="16-17"),]
overall_trt <- sum(analysis_data_16_17$COUNT)/sum(analysis_data_16_17$TOT_POP)*1000
pre_trt <- sum(analysis_data_16_17$COUNT[which(analysis_data_16_17$year < 2008)])/sum(analysis_data_16_17$TOT_POP[which(analysis_data_16_17$year < 2008)])*1000
post_trt <- sum(analysis_data_16_17$COUNT[which(analysis_data_16_17$year >= 2008)])/sum(analysis_data_16_17$TOT_POP[which(analysis_data_16_17$year >= 2008)])*1000

diff_trt <- sum(analysis_data_16_17$COUNT[which(analysis_data_16_17$year < 2008)])/sum(analysis_data_16_17$TOT_POP[which(analysis_data_16_17$year < 2008)])*1000-
sum(analysis_data_16_17$COUNT[which(analysis_data_16_17$year >= 2008)])/sum(analysis_data_16_17$TOT_POP[which(analysis_data_16_17$year >= 2008)])*1000

percent_trt <- diff_trt/pre_trt*100

overall_trt
pre_trt
post_trt
diff_trt
percent_trt


# CONTROL GROUP
analysis_data_18_24 <- analysis_data[which(analysis_data$AGEGRP=="18-24"),]

overall_ctrl <- sum(analysis_data_18_24$COUNT)/sum(analysis_data_18_24$LD_18_24)*1000
pre_ctrl <- sum(analysis_data_18_24$COUNT[which(analysis_data_18_24$year < 2008)])/sum(analysis_data_18_24$TOT_POP[which(analysis_data_18_24$year < 2008)])*1000
post_ctrl <- sum(analysis_data_18_24$COUNT[which(analysis_data_18_24$year >= 2008)])/sum(analysis_data_18_24$TOT_POP[which(analysis_data_18_24$year >= 2008)])*1000

diff_ctrl <- sum(analysis_data_18_24$COUNT[which(analysis_data_18_24$year < 2008)])/sum(analysis_data_18_24$TOT_POP[which(analysis_data_18_24$year < 2008)])*1000-
sum(analysis_data_18_24$COUNT[which(analysis_data_18_24$year >= 2008)])/sum(analysis_data_18_24$TOT_POP[which(analysis_data_18_24$year >= 2008)])*1000

percent_ctrl <- diff_ctrl/pre_ctrl*100


overall_ctrl
pre_ctrl
post_ctrl
diff_ctrl
percent_ctrl


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
	
	phi_03[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_04[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_05[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_06[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_07[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_08[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_09[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_10[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_11[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_12[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_13[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_14[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_15[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_16[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)
	phi_17[1:ncounty] ~ dcar_normal(adj[1:L], weights[1:L], num[1:ncounty], tau = tau_phi_t, zero_mean = 1)


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

main_MCMC <- nimbleMCMC(code=main_code, constants = constants,
				  data = nimble_data, inits = inits, monitors = monitor,
				  niter = 50000, nburnin = 10000, thin = 20, nchains = 1, progressBar = TRUE,WAIC=TRUE)

saveRDS(main_MCMC ,"R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Final Programs\\Output\\Output_pop_offset.rds")


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

plot(point_est,ylim=c(0.7,1.1),pch=18,ylab="DID Estimate",xlab = "Arbitraty County Order")

for(i in 1:length(counties)){
	segments(i,UL[i],i,LL[i])
	segments(i-0.2,LL[i],i+0.2,LL[i])
	segments(i-0.2,UL[i],i+0.2,UL[i])
}

abline(h=1)


# POSTERIOR MEANS
mean(exp(main_MCMC$samples[,4050]))
quantile(exp(main_MCMC$samples[,4050]),p=c(0.025,0.975))


fitted <- apply(main_MCMC$samples[,1:2639],2,mean)
plot(nimble_data$y,fitted)
abline(0,1)
plot(nimble_data$y-fitted)


## ORDER BY POPULATION SIZE ##
counties_pop_order <- counties[order(subset_pop$TOT_POP)]

plot(point_est[order(subset_pop$TOT_POP)],ylim=c(0.6,1.4),pch=18,ylab="DID Estimate",xlab = "Counties Ordered by Population")

for(i in 1:length(counties)){
	segments(i,LL[order(subset_pop$TOT_POP)][i],i,UL[order(subset_pop$TOT_POP)][i])
	segments(i-0.2,LL[order(subset_pop$TOT_POP)][i],i+0.2,LL[order(subset_pop$TOT_POP)][i])
	segments(i-0.2,UL[order(subset_pop$TOT_POP)][i],i+0.2,UL[order(subset_pop$TOT_POP)][i])
}
abline(h=1)


plot((subset_pop$TOT_POP),point_est,pch=18,ylim=c(0.6,1.4),xlab="Population",ylab="DID Estimate")
for(i in 1:length(counties)){
	segments((subset_pop$TOT_POP[i]),LL[i],(subset_pop$TOT_POP[i]),UL[i])
	segments((subset_pop$TOT_POP[i])-500,LL[i],(subset_pop$TOT_POP[i])+500,LL[i])
	segments((subset_pop$TOT_POP[i])-500,UL[i],(subset_pop$TOT_POP[i])+500,UL[i])
}
abline(h=1)


plot((subset_pop$TOT_POP),UL-LL,pch=18,xlab="Population",ylab="CI Width")



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
#color_class <- cut(point_est,breaks=quantile(point_est,probs = c(0,0.1,0.25,0.5,0.75,0.942,1.0)))

color_class <- cut(point_est,breaks=c(0.76,0.85,0.88,0.91,0.95,1.00,1.06))

levels(color_class)

my_colors <- colors[as.numeric(color_class)]

DID_data <- data.frame(counties,point_est,color_class,my_colors)
DID_data$COUNTYFP <- str_pad(DID_data$counties-39000,3,pad="0")

ohio_map2 <- merge(ohio_map,DID_data,by=c("COUNTYFP"),sort=TRUE)

legend_text <- rev(levels(color_class))
legend_text[1:2] <- c("(1.00,1.06]","(0.95,1.00]")

png("R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Output\\ohio_map.PNG",height=650,width=650)
plot(ohio_map2[1],col=my_colors,main="")
legend("bottomright",legend=legend_text,fill=rev(colors),title="Point Estimate")
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
	if(UIC$UIC[i] == 1){UIC$UIC3[i] <- "Large Metro"}
	if(UIC$UIC[i] == 2){UIC$UIC3[i] <- "Small Metro"}
	if(UIC$UIC[i] %in% c(3,4)){UIC$UIC3[i] <- "Large Metro Adjacent"}
	if(UIC$UIC[i] %in% c(5,6,7)){UIC$UIC3[i] <- "Small Metro Adjacent"}
	if(UIC$UIC[i] > 7){UIC$UIC3[i] <- "Not Metro Adjacent"}

}

boxplot(point_est~UIC$UIC3,ylab = "DID Point Estimate",xlab="UIC",ylim=c(0.7,1.1))



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

saveRDS(main_MCMC,"R:\\RESZhu\\Research_Marco\\Policy Effect Heterogeneity\\Code\\Output\\Output_pop_offset.rds")
