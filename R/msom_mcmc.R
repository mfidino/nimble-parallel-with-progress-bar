# Read in the data
library(nimble)
library(doParallel)

source("./R/params.R")

 # start up the cluster
my_cluster <- makeCluster(
  n_chains,
  outfile = my_outfile
)

# site by species matrix, max number of surveys = 4
y <- read.csv("./data/species_data.R")

# design matrix
x <- read.csv("./data/design_matrix.R")

# query some values we'd need for the model
nsite <- nrow(y)
nspecies <- ncol(y)
nsurvey <- 4

# the data list for nimble
data_list <- list(
  y=y,
  design_matrix_psi = x,
  design_matrix_rho = x
)

# The constant list for nimble
constant_list <- list(
  # Number of sites
  nsite = nsite,
  # Number of repeat samples
  nsurvey = nsurvey,
  # number of species
  nspecies = nspecies,
  # design matrices for all models
  design_matrix_psi = x,
  design_matrix_rho = x,
  # number of parameters for each model
  npar_psi = ncol(x),
  npar_rho = ncol(x)
)

# the nimble model, written so that it can be run in parallel (i.e.,
#  the whole thing is written as one big function).
msom_mcmc <- function(seed, data_list, constant_list, niter = n_iterations){
  library(nimble)
  
  # The multi-species occupancy model
  msom_code <-nimble::nimbleCode(
    {
      for(site in 1:nsite){
        for(species in 1:nspecies){
          # logit linear predictor occupancy
          logit(psi[site,species]) <- inprod(
            beta_psi[species, 1:npar_psi],
            design_matrix_psi[site,1:npar_psi]
          )
          z[site,species] ~ dbern(
            psi[site,species]
          )
          # logit linear predictor detection
          logit(rho[site,species]) <- inprod(
            beta_rho[species,1:npar_rho],
            design_matrix_rho[site,1:npar_rho]
          )
          y[site,species] ~ dbin(
            rho[site,species] * z[site,species],
            nsurvey
          )
        }
      }
      # priors for occupancy
      for(psii in 1:npar_psi){
        beta_psi_mu[psii] ~ dlogis(0,1)
        tau_psi[psii] ~ dgamma(0.001,0.001)
        sd_psi[psii] <- 1 / sqrt(tau_psi[psii])
        for(species in 1:nspecies){
          beta_psi[species,psii] ~ dnorm(
            beta_psi_mu[psii],
            tau_psi[psii]
          )
        }
      }
      # priors for detection
      for(rhoi in 1:npar_rho){
        beta_rho_mu[rhoi] ~ dlogis(0,1)
        tau_rho[rhoi] ~ dgamma(0.001,0.001)
        sd_rho[rhoi] <- 1 / sqrt(tau_rho[rhoi])
        for(species in 1:nspecies){
          beta_rho[species,rhoi] ~ dnorm(
            beta_rho_mu[rhoi],
            tau_rho[rhoi]
          )
        }
      }
    }
  )
  
  # initial value function for the model
  my_inits <- function(){
    list(
      z = matrix(
        1,
        nrow = constant_list$nsite,
        ncol = constant_list$nspecies
      ),
      beta_psi = matrix(
        rnorm(constant_list$npar_psi * constant_list$nspecies),
        nrow  = constant_list$nspecies,
        ncol = constant_list$npar_psi),
      beta_rho = matrix(
        rnorm(constant_list$npar_psi * constant_list$nspecies),
        nrow  = constant_list$nspecies,
        ncol = constant_list$npar_psi)
    )
  }
  
  # set up the model
  msom <- nimble::nimbleModel(
    code = msom_code,
    name = "msom",
    constants = constant_list,
    data = data_list,
    inits = my_inits()
  )
  
  # you need to compile the model before
  # you modify the samplers.
  tmp_msom <- nimble::compileNimble(
    msom
  )
  # configure the model
  msom_configure <- nimble::configureMCMC(
    msom
  )
  # change the samplers
  msom_configure$addSampler(
    target = c("beta_psi_mu", "beta_psi"),
    type = "RW_block",
    control = list(adaptInterval = 100)
  )
  msom_configure$addSampler(
    target = c("beta_rho_mu", "beta_rho"),
    type = "RW_block",
    control = list(adaptInterval = 100)
  )
  # add monitors
  msom_configure$setMonitors(
    c(
      "beta_psi_mu", "beta_psi", "beta_rho_mu",
      "beta_rho", "sd_psi", "sd_rho"
    )
  )
  # going to sample the latent state as well,
  # but sample it less than the other parameters
  msom_configure$setMonitors2("z")
  msom_configure$setThin2(5)
  # build the model
  msom_build <- nimble::buildMCMC(
    msom_configure
  )
  # compile the model a second time,
  # reset functions because we added
  # different samplers.
  msom_compiled <- nimble::compileNimble(
    msom_build,
    project = msom,
    resetFunctions = TRUE
  )
  # fit the model
  results <- nimble::runMCMC(
    msom_compiled,
    niter = niter,
    setSeed = seed,
    inits = my_inits()
  )
  return(results)
}

# fit the model

chain_output <- parLapply(
  my_cluster,
  X = 1:n_chains,
  fun = msom_mcmc,
  data_list = data_list,
  constant_list = constant_list,
  niter = n_iterations
)
stopCluster(my_cluster)

saveRDS(
  chain_output,
  paste0(
    "./mcmc_output/model_fit_",Sys.Date(),".RDS"
  )
)