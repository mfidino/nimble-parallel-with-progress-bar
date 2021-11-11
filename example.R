library(nimble)
library(doParallel)

# load the objects that need to be available to the cmd line
# and the R global environment.
source("./R/params.R")


# load the function to track progress. It opens up
#  the outfile, and looks for the progress bar that 
#  nimble creates in the file. 
check_progress_mcmc <- function(outfile, nchain, sleep_length = 10){
  # Have to fit 
  is_running <- TRUE
  iter = 1
  total_run <- (nchain * 57) - nchain * 2
  pb <- txtProgressBar(0, total_run)
  while(is_running){
    tmp <- readLines(outfile, warn = FALSE)
    error <- grep("Error", tmp)
    if(length(error)>0){
      stop("Model did not run, check outfile")
    }
    tmp <- tmp[grep("^\\|-",tmp)]
    if(length(tmp) == 0){
      cat("Nimble model getting set up. Checking again in 30 seconds...\n")
      Sys.sleep(30)
      next
    } else {
      nran <- nchar(tmp[length(tmp)])
      setTxtProgressBar(pb, nran)
      Sys.sleep(sleep_length)
      
      if(substr(tmp[length(tmp)],nran,nran) == "|"){
        is_running <- FALSE
        cat("\n Model running complete!\n")
      }
    }
  }
}


callr::r_bg(
  callr:::rscript(
    "./R/msom_mcmc.R"
  )
)
# Send the model to the console which will fit it,
#  and save the output.
rstudioapi::jobRunScript(
  "./R/msom_mcmc.R",
  workingDir = getwd()
)

# and check your progress
check_progress_mcmc(
  outfile = my_outfile,
  nchain = n_chains
)

file.remove(my_outfile)


