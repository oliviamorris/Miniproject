###functions required####
fecundity # for the egg stages

##########################################
# egg stage 
# initial number here should be the number of eggs generated from returners
# parameters need to be defined
SR_egg <- 0.165                              #OM:sr monthly surivival -from table 1.egg survival rate from roberston 2005 
# the time of this stage is 1 month now. It can be adjusted to 2-6 weeks later
egg_matrix <- function(TC, global_min, global_max, matrix_size){
  fill <- rep(0, matrix_size) #repeat 0 the matrix size imes, i.e 8500
  min <- global_min
  max <- global_max
  for(i in min:max){
    fill[i] <- fecundity((2*i+1)/2*10,TC)*SR_egg              #OM: should this be calculated from eqn too?
  }
  return(fill)
}
#############################################################
#liv testing#
egg_matrix <- function(TC, global_min, global_max, matrix_size){
  fill <- rep(0, matrix_size) #repeat 0 the matrix size imes, i.e 8500
  min <- global_min
  max <- global_max
  for(i in min:max){
    fill[i] <- fecundity((2*i+1)/2*10,TC)*SR_egg              #OM: should this be calculated from eqn too?
  }
  return(fill)
}

fecundity((2*1+1)/2*10,TC)*SR_egg 

egg_matrix(TC = 20, global_min = 10, global_max = 30, matrix_size = 60)

##########################################
# eyed egg stage and alevin 
# initial number here should be the number of eggs generated from egg_matrix
# parameters need to be defined
SR_ea <- 0.8608                       #OM: from table 1 - eyed egg survival rate
# the time of the stage is 4.5 month now. It can be adjusted later

ea_m <- function(TC){ #it is not temperature relevant now
  final_ea_p <- SR_ea 
  return(final_ea_p)
}

#waste of effort?

##########################################

##########################################
# fry stage 
# initial vector here should be the number of eggs generated from fry stage
# Parameters need to be defined
SR_fry <- 0.8608                     #OM: from table 1 fry survival rate
# the total time of the stage is 1.5 month

# generate the initial fry. It is manually determined  
fry_May <- function(TC, matrix_size){#it is not temperature relevant now
  f <- rep(0, matrix_size)
  f[1] <- 1/2.5 * SR_fry
  f[2] <- 1/2.5 * SR_fry
  f[3] <- 0.5/2.5 * SR_fry
  return(f)
}

#OM:testing
f <- rep(0, 50)
f[1] <- 1/2.5 * SR_fry     #OM: why 1/2.5 etc for entry one two and three
f[2] <- 1/2.5 * SR_fry
f[3] <- 0.5/2.5 * SR_fry

# generate the matrix column
fry_matrix_column <- function(band_min, band_max, month, TC, matrix_size){
  fill <- rep(0, matrix_size)
  band_min_am <- mass_after_month(band_min, monthtime(month), TC)
  band_max_am <- mass_after_month(band_max, monthtime(month), TC)
  band_width <- band_max_am - band_min_am
  band_integer <- ceiling(band_min_am)- floor(band_max_am)
  band_top <- ceiling(band_min_am) - band_min_am
  band_bottom <-band_max_am - floor(band_max_am)
  band_top_p <- band_top/band_width
  band_bottom_p <- band_bottom/band_width
  band_middle_p <- 1/band_width   #remember to consider no middle one
  if (band_integer == 0) {
    fill[floor(band_min_am)] <- band_top_p * SR_fry  # check whether it is approprate to use the mass here to calculate survival rate
    fill[floor(band_max_am)] <- band_bottom_p * SR_fry
  } else if (band_integer == 1) {
    fill[floor(band_min_am)] <- band_top_p * SR_fry
    fill[floor(band_max_am)] <- band_bottom_p * SR_fry
    fill[(floor(band_min_am)+1)] <- band_middle_p * SR_fry
  } else {
    fill[floor(band_min_am)] <- band_top_p * SR_fry
    fill[floor(band_max_am)] <- band_bottom_p * SR_fry
    fill[(floor(band_min_am)+1):(floor(band_max_am)-1)] <- band_middle_p * SR_fry    
  }
  
  if(length(fill) > matrix_size){
    fill <- fill[1: matrix_size] 
  }
  
  if(sum(fill) < (SR_fry-0.1)){
    fill[matrix_size] <- SR_fry - sum(fill)
  }
  return(fill)
}

fry_matrix_column(band_min = 1, band_max = 10, )

# generate the matrix
fry_matrix <- function(month, TC, matrix_size){
  m <- matrix(0, matrix_size, matrix_size)
  for(i in 1:matrix_size){
    m[,i] <- fry_matrix_column(band_min = i, band_max = i+1, month, TC, matrix_size)
  }
  return(m)
}

