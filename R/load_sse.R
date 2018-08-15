## Function to load simulation data from PsN SSE
## This facilitates extracting data needed for postprocessing of simulations
## poptentially helpfor after simulation with uncertainty (e.g. obtained through PsN bootstrap or impance sampling)

## main script here
load_sse <- function(x, parr=F, nodes=1) {
# Load SSE data -----------------------------------------------------------
## create list of paths for the tables
  require(data.table)
  require(dplyr)  
  # If you're not using NMprojects, you can skip this part
  dir <- "/m1" # Path to simulated data
  full_dir <- paste(x$run_dir, dir, sep = "")
  
  cmd <- as.vector(strsplit(x$cmd, " "), mode = 'list')
  
  if (!'sse' %in% cmd[[1]])
    stop("Object not an SSE PsN object")
 
  nruns <- as.numeric(strsplit(cmd[[1]][grep("-samples", cmd[[1]])],"=")[[1]][2])
  # If you're not using NMprojects, you can skip this part
  # provide full dir <- "/path"
  # provide nruns <- number of samples
  
  paths <- c()
  for (i in 1:nruns) {       ## create a vector of data set paths
    path <- paste(full_dir,"/mc-sim-",i,".dat",sep="")
    paths <- append(paths, path)
  }
  
  # Read in dataframes and d0 dataprocessing
  process1 <- function(data_path){
    d <- fread(data_path, skip = 1, header = T)
    # If you are working with large dataframes, you may be required to implement dataprocessing steps, depending on your capacity, check/set memory.limit()
    save(d, file = paste0(data_path,"processed.RData"))
  }
  
  #Apply the function to the paths using either lapply or a parallel version for computational demanding steps
  #if you are running this code, choose one of the options and specify the number of nodes if you wish to use multiple
  
  if (parr) {
    parallel::mclapply(paths, process1, mc.cores = nodes)
    } else { ## will run the processing in parallel
      lapply(paths, process1)
    }
  
  #load processed.Rdata into R environment and create list of dataframes
  get_processed <- function(data_path){
    load(paste0(data_path,"processed.RData"))
    d
  }
  
  # apply to paths
  d <- lapply(paths, get_processed)
  
  print("Only data from simulation step will be loaded, you may adapt this function to load data from estimation step, or to do both.")
  
  # rbind dataframes
  d <- bind_rows(d)  ## dplyr function
  
}
