############################    ROC-TOC test   #######################################
################################  Generation of ROC/TOC curve and maps  #######################################
#This script processing examines the example maps for the ROC and TOC.
#AUTHORS: Ali Santa Cruz                                           
#DATE CREATED: 10/05/2014 
#DATE MODIFIED: 01/17/2015
#Version: 1
#PROJECT: ROC and TOC
#TO DO:
#change resolution by disagregating by 10 and 100 to check how the code is doing
#add random raster to test as well

#################################################################################################

#Installationg did not work....
#git  clone https://github.com/amsantac/TOC.git
library(devtools) 
install_github("amsantac/TOC")
###Loading R library and packages                                                      

library(sp)  #Spatial objects definition

library(spdep) #Spatial objects functions for analyses
library(rasterVis) #Raster visualization
library(raster) #Raster objects definition and function for analyses
library(rgdal) #GDAL binding for R

library(rgeos) #GEOS binding for R
library(gtools) #general additional tools
library(maptools) #mapping tools
library(colorRamps) #Palette/coloramp for display,contains matlab.like color palette
#Error: could not find function "as.bit"
library(bit)

###### Functions used in this script

create_dir_fun <- function(out_dir,out_suffix){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    out_dir <- file.path(out_dir,out_name)
  }
  #create if does not exists
  if(!file.exists(out_dir)){
    dir.create(out_dir)
  }
  return(out_dir)
}

load_obj <- function(f){
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

#setwd("D:/asantacruzdelgado/PhD/Fall 2014/RA/software/composite/toc")
#source("Rscripts/TOC.R")
#source("Rscripts/TOCplot.R")

function_TOC_creation <- "TOC.R"
#function_plot_TOC <- "TOCplot.R"
function_plot_TOC <- "plot.TOC.R"
#script_path <- "C:/Users/parmentier/Dropbox/Data/TOC/My Run" #path to script
#script_path <- "C:/Users/parmentier/Dropbox/Data/TOC/tocR/Rscripts"
script_path <- "/home/parmentier/Data/git_repo_projects/TOC"
source(file.path(script_path,function_TOC_creation)) #source all functions used in this script 1.
source(file.path(script_path,function_plot_TOC)) #source all functions used in this script 1.

#Using source, reads the script containing the functions, loads functions in the workspace/enviroment
#making them available for the user.

#####  Parameters and argument set up ###########

in_dir <- "/home/parmentier/Data/git_repo_projects/TOC/maps/"
out_dir <- "/home/parmentier/Data/git_repo_projects/TOC/maps/" #output will be created in the input dir, set 

out_dir <- in_dir #output will be created in the input dir
out_suffix_s <- "01192015" #can modify name of output suffix
create_out_dir_param <- FALSE

if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix_s)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

################ BEGIN SCRIPT################

###### READ IN RASTER DATASETS #######

#index <- raster(system.file("external/p_built01_suitability_1.rst", package="TOC"))#these are raster objects...
#boolean <- raster(system.file("external/BuiltGain1985_1999.rst", package="TOC"))
#mask <- raster(system.file("external/1985NonBuilt01.rst", package="TOC"))
# all unique values of the index map after applying the mask are used as thresholds (default option) 
#tocd <- TOC(index, boolean, mask, NAval=0, uncertainty=TRUE)
#tocd

r_index <- raster(file.path(in_dir,"p_built01_suitability_1.rst"))#, package="TOC"))#these are raster objects...
#boolean <- raster(file.path(in_dir,"BuiltGain1985_1999.rst")) #don't have this file...
r_boolean <- raster(file.path(in_dir,"1985-1999BuiltGain2.rst")) #this the reference map
r_mask <- raster(file.path(in_dir,"1985NonBuilt01.rst"))#, package="TOC")) #mask raster

unique(r_index) #19 values
r_stack <- stack(r_index,r_boolean,r_mask)
names(r_stack) <- c("index","boolean","mask")
plot(r_stack)
r_stack_m <- mask(r_stack,r_mask)

histogram(r_stack) #From rasterVis
histogram(r_index)
freq(r_index)

#### RUN THE TOC FUNCTIONS #######

# all unique values of the index map after applying the mask are used as thresholds (default option) tocd <- TOC(index, boolean, mask, NAval=0, uncertainty=TRUE)
debug(TOC) #degug function...
#problem so line .134 changed
#rerun
tocd <- TOC(r_index, r_boolean, r_mask, NAval=0, uncertainty=TRUE)

class(tocd)
names(tocd)

tocd$TOCtable #ok only 18 unique values...
plot.TOC(tocd, labelThres=FALSE) #Figure? numbers are e+08...

write.table(tocd$TOCtable,paste("TOC_table_",out_suffix_s,sep=""))

#### now create a random image

r_index
r_test <- aggregate(r_index,fact=10)
r_test2 <- r_test
setValues(r_test2)<- rnorm(ncell(r_test2))
  
############### END OF SCRIPT ###################


#commenting out previous code test from 10-14-2014
# ## Quick plot of Amin's data:
# 
# in_file_change <- file.path(in_dir,"Change_Map.asc")
# r_change <- raster(in_file_change)
# freq(r_change)
# dim(r_change) #1691x1351 image, missing ref projection
# levelplot(r_change,margin=F) #requires rasterVis
# 
# in_file_index <- file.path(in_dir,"Prob_Map.asc")
# r_index <- raster(in_file_index)
# 
# levelplot(r_index,margin=F)
# levelplot(stack(r_change,r_index),margin=F)
# 
# ### This is from Ali's
# ## AMIN DATA: 
# 
# input <- raster("C:/Users/parmentier/Dropbox/Data/TOC/tocR/maps/Prob_Map2.rst")
# ref <- raster("C:/Users/parmentier/Dropbox/Data/TOC/tocR/maps/Change_Map2b.rst")
# mask <- raster("C:/Users/parmentier/Dropbox/Data/TOC/tocR/maps/MASK3.rst")
# 
# system.time(tocd <- TOC(input, ref, mask, nthres=100, NAvalue=0)) #right this is rather slow
# #tocd is a data.frame
# 
# #ncol(ref)*nrow(ref)
# freq(ref)#,value=NA)
# #value   count
# #[1,]     0 1222042 #0 not read in as NAval!
# #[2,]     1  362200
# #[3,]     2  700299
# 
# ncells <- 1270089 #what is this number? is that the number of valid pixels, ok
# cellSize <- 1000
# population <- (ncells * cellSize^2)/1000^2
# 
# tocd1 <- TOCplot(tocd$truePositive, tocd$truePosRate, tocd$falsePositive, tocd$falsePosRate, population)
# #nice plot Ali!
# 
# ## PIE DATA
# 
# input <- raster("C:/Users/parmentier/Dropbox/Data/TOC/tocR/maps/p_built01_suitability_1.rst")
# ref <- raster("C:/Users/parmentier/Dropbox/Data/TOC/tocR/maps/1985-1999BuiltGain2.rst")
# mask <- raster("C:/Users/parmentier/Dropbox/Data/TOC/tocR/maps/1985NonBuilt01.rst")
# 
# #check dimension and visualize quickly data
# #dim(ref)
# levelplot(ref,margin=F) #MA PIE data ok!
# levelplot(input,margin=F)
# 
# system.time(tocd <- TOC(input, ref, mask, nthres=100, NAvalue=0))
# dim(mask)
# ncell(mask) #?
# freq(mask)
# ncells <- 1260141 # waht is this number?
# cellSize <- 30 #cellSize<-res(mask)
# population <- (ncells * cellSize^2)/1000^2
# tocd1 <- TOCplot(tocd$truePositive, tocd$truePosRate, tocd$falsePositive, tocd$falsePosRate, population)
# 
# #Add quick map generation saved in png?

##### End of script ####
