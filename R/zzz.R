.onAttach <- function(libname = find.package("DataflowR2023"), pkgname = "DataflowR2023"){
  
  localpath <- system.file("localpath", package = "DataflowR2023")
  fdir <- matrix(utils::read.delim(localpath, header = FALSE, 
                                   stringsAsFactors = FALSE))[[1]][1]
  
  packageStartupMessage(paste("DataflowR2023 Data Directory:",
                              fdir, "\n", "To change, modify:", localpath))
                        
  options("fdir" = fdir)
}
