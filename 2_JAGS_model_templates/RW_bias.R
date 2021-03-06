model{

  for(k in 1:max(year_no)){

  for(j in 1:max(season_weeks)){
    #this fits the latent Gloeo to your observed Gloeo
    #run this on logged data
    y[k,j] ~ dnorm(mu[k,j],tau_obs)

  }

  #### Process Model

  for(j in 2:max(season_weeks)){

    #process model for Gloeo
    mu[k,j]~dnorm(lambda[k,j],tau_proc)
    lambda[k,j] <- beta1 + mu[k,j-1]

  }

    #Loops through items in seasonal for-loop and defines initial conditions
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix

  }
  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1)
  tau_obs ~ dgamma(a_obs,r_obs)

}
