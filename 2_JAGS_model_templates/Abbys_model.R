##NOT READY FOR PRIMETIME - WILL NOT RUN!

model{

  for(k in 1:max(year_no)){

    for(j in 1:max(season_weeks)){
      #this fits the latent Gloeo to your observed Gloeo
      #run this on logged data
      y[k,j] ~ dnorm(mu[k,j],tau_obs)

    }

    #### Process Model

    for(j in 2:max(season_weeks)){

      #process model for tick life cycle
      mu[k,j]~dpois(tick_mod[k,j])
      tick_mod[k,j] = tick_mod[k,j-1] + growth_nymph[k,j] - death[k,j] - growth_adult[k,j]
      growth_nymph[k,j] = eggs_last_year[k] * eggs_to_nymphs[k,j]
      death = mortality * tick_mod[k,j-1]
      growth_adult = tick_mod[k,j-1]*nymph_to_adult[k,j]
      eggs_last_year[k] = egg_laying_rate * mu[k-1,max(season_weeks)]

      #process model for dependence of life cycle rates on temperature
      eggs_to_nymphs[k,j] = etn_beta1 + etn_beta2*eggs_to_nymphs[k,j-1] + etn_beta3*temp[k,j]
      nymphs_to_adults[k,j] = nta_beta1 + nta_beta2*nymphs_to_adults[k,j-1] + nta_beta3*temp[k,j]

    }

    #Loops through items in seasonal for-loop and defines initial conditions
    mu[k,1] ~ dpois(lambda_n0) #keep in mind you'll need to index like a matrix

  }
  #### Priors
  egg_laying_rate ~ dgamma(a_egg, r_egg)

  etn_beta1 ~ dnorm(beta.m1,beta.v1)
  etn_beta2 ~ dnorm(beta.m2,beta.v2)
  etn_beta3 ~ dnorm(beta.m2,beta.v2)
  nta_beta1 ~ dnorm(beta.m2,beta.v2)
  nta_beta2 ~ dnorm(beta.m2,beta.v2)
  nta_beta3 ~ dnorm(beta.m2,beta.v2)

  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1)
  beta2 ~ dnorm(beta.m2,beta.v2)
  tau_obs ~ dgamma(a_obs,r_obs)

}

#timestep: weekly
#spatial scale: one plot
#focal variable: tick nymphs

# Parameters
n0 <- 0                                                     ## initial population size
NT <- 54                                                    ## number of time steps to simulate (one year)
time <- 1:NT                                                ## vector of times
eggs_last_year = 1000                                       ## number of eggs laid last year (will ultimately be dependent
## on number of adults last year and temperature)
## rate at which eggs become nymphs (will ultimately be dependent on temperature)
eggs_to_nymphs = c(rep(0,13),sin(seq(0,pi, length.out = 12))/80,c(rep(0,28)))  # currently a sine function durring summer and zero otherwise
## rate at which nymphs become adults (will ultimately be dependent on temeprature)
nymph_to_adult = c(rep(0,13),sin(seq(0,pi, length.out = 12))/50,c(rep(0,28)))
mortality = .25                                             ## mortality rate

#Model
tick_mod <- rep(n0, NT)    ## vector to store results
for(t in 2:NT){
  growth_nymph = eggs_last_year * eggs_to_nymphs[t-1]
  growth_adult = tick_mod[t-1]*nymph_to_adult[t-1]
  death = tick_mod[t-1]*mortality
  tick_mod[t] = tick_mod[t - 1] + growth_nymph - death - growth_adult
}
