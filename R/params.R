# some values that need to be set which
# need to be available to the global environment
# and the command line.

# some values that need to be set
n_chains <- 3
n_iterations <- 40000

# The temporary file you want to read.
my_outfile <- paste0(
  getwd(),
  "/tmp_progress.txt"
)
