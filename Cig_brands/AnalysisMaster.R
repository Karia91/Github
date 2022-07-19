
################# analysis master file for Excise analysis ###########################


# 24/06/2022


# Written by: Kieran Byrne


user_num <- 2


if(user_num == 1) {
  
  # Theonille's filepath here
  
  dropbox <- 'C:/Users/kiera/Dropbox/Rwanda RRA/'
  
  
  github <-  'C:/Users/kiera/Documents/DIME/Github/'
  
}else{
  
  # Karia's DB details here
  
  dropbox <- 'C:/Users/Kalis/Dropbox/'
  
  # Karia's github details here
  
  github <- 'C:/Users/Kalis/Documents/GitHub/training/excise'
  
}

# packages

if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, arrow, lubridate, broom, fixest, lfe, viridis)



dropbox <- 'C:/Users/Kalis/Dropbox'



dropbox_data <- paste0(dropbox, '/202206Karia/Data/')


ebm_excise <- paste0(dropbox_data, '/202206Karia/Data/')



my.ggsave <- function(filename = default_name(plot),plot = plot, height= 3, width= 5, scale = 1.2, dpi= 300, ...) {
  ggsave(filename=filename, plot= plot, height=height, width=width,scale = scale, dpi=dpi, ...)
}


# folder for output

dropbox_pres <- paste0(dropbox, 'Excise_Tobacco_Pierre')
