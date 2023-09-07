#Used instructions at https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/ for guidance


#Packages for creating new package
library(devtools)
#devtools::install_github("klutometis/roxygen")
library(roxygen2)

#setwd("\\\\ad.sfwmd.gov/dfsroot/userdata/kiallen/Docs/DataflowR2023")
setwd("C:/Documents/DataflowR2023")
# Run document() to create package documentation for different functions

#Set wd to Docs
setwd("..")
#Install new package
#install("DataflowR2023")
library(DataflowR2023)

#Test new functions
dt <- streamclean(yearmon = 201802, gps = "eu", eummin = 12, c6mmin = 12, tofile = F)

#Load older files that have undergone "hand cleaning"
dt <- streamparse(yearmon = 200301, tofile = T)


#QA cleaned streaming data
streamqa(yearmon = 201802, parset = names(streamget(201802))[c(4:12, 16:22)])

#Load previously cleaned streaming data
dt <- streamget(yearmon = 201402, qa = TRUE)

#Interpolate cleaned data files
names(dt)
streaminterp(streamget(yearmon = 201802, qa = TRUE), paramlist = c("salinity.pss"), 201802)
#Has to define new rraster object outside of function to get function to work

#Quick R plot of interpolated data
surfplot(rnge = c(201402), params = c("sal"))

#Clean grab sample records
grabclean(yearmon = 201410, tofile = FALSE)

#Load previously cleaned grab data
grabs <- grabget(rnge = c(201402, 201410))

#Calculate difference from average surface
avmap(yearmon = 201802, params = "salinity.pss", percentcov = 0.6, tolerance = 12)

#Fit grab sample and streaming averages
#Calculate coefficients
chlcoef(yearmon = 201308, remove.flags = TRUE, overwrite=F)
#Error about incorrect number of dimensions, but function does seem to work (at least somewhat)
#This error means there are missing/extra values cells in the dataset, function will still run but maybe doesn't incorporate data type where values are missing?

#Generate extracted chlorophyll surfaces
chlmap(yearmon = 201502)


######
# Resource for uploading/synching package to github:
# https://pjnewcombe.wordpress.com/2014/05/31/using-rstudio-to-create-and-maintain-an-r-package-on-github/#:~:text=Using%20RStudio%20to%20create%20and%20maintain%20an%20R,future%20additions%2Fedits%20to%20github%2C%20from%20within%20RStudio%20
# https://hansenjohnson.org/post/sync-github-repository-with-existing-r-project/


#need to install Git on computer in order to link R project with github repo



#################################################
#Testing dummy data

dt <- streamget(yearmon = 201802, qa = TRUE)

names(dt)
streaminterp(dt, paramlist=c("salinity.pss"), yearmon=201802)





