library(doParallel)

my_outfile <- paste0(
  getwd(),
  "/tmp_progress.txt"
)

my_cluster <- makeCluster(
  3,
  outfile = my_outfile
)
stopCluster(my_cluster)