library(nimble)
library(doParallel)

# load the objects that need to be available to the cmd line
# and the R global environment.
source("./R/params.R")

# load the function to track progress. It opens up
#  the outfile, and looks for the progress bar that 
#  nimble creates in the file. 
source("./R/check_progress_mcmc.R")

if(
  file.exists(
    my_outfile
  )
){
  file.remove(my_outfile)
}

# Send the model to the console which will fit it,
#  and save the output.
shell(
  "Rscript ./R/msom_mcmc.R",
  wait = FALSE
)
# just wait a little bit for the cluster to get spun up
Sys.sleep(2)
# and check your progress!
check_progress_mcmc(
  outfile = my_outfile,
  nchain = n_chains
)

# remove the outfile when done
file.remove(my_outfile)

# read in the model
model <- readRDS(
  "./mcmc_output/model_fit_2021-11-11.RDS"
)

