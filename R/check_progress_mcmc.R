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
