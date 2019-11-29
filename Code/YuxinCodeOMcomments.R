#######Parameters of metabolic theory#######

# 1.calculate the growth time 
growthtime <- function(m, TC){
  V <- -0.13* TC/(1+(TC/273))         #OM: from eqn, tc is temp in cels
  t <- (m ^(0.25))*exp(5.73)*exp(V)   #OM: whats e(5.73) 
  #t <- round(t, 2)                   #OM t - time
  return(t)
}

# 2.calculate the growth mass in day
growthmass <- function(t, TC){
  V <- - 0.13* TC/(1+(TC/273))
  m <- (exp(5.73)*exp(V)/t)^(-4) # the unit of t is day, *30 make it month
  #m <- round(m,2)
  return(m)
}

# 3.calculate the survival rate of a month. It was treated as 30 days a month
survival <- function(m, TC){  #m : mass, the unit is gram
  Temp = TC-5 +273.15                                        #OM: why -5
  E = 0.45                                                   #OM:activation energy in fish?(ref?)
  k = 8.61*(10^(-5))                                         #OM:boltzmann constant 8.617333262145×10−5 in eV⋅K−1
  V <- E/ (k*Temp)                                           #OM: exp
  Z <- (m ^(-0.23))*exp(19)/exp(V) # year survival rate      #OM: wtf is e(19)
  Zm <- (1-Z)^(1/12)  # /12 to calculate month survival rate
  return(Zm)                                                 #OM: returns mortality rate per month
}


# 4.calculate the fecundity.
fecundity <- function(m, TC){
  m <- m/1000                                          #OM: as the relationship is kg and m is in g
  egg_number <- (m^ 0.976) * exp(7.2924)               #OM: wo is e to the 7.2924 times mass to the 0.9
  egg_number <- round(egg_number)                      #OM: round it to whole number
  return(egg_number)                                   #OM: returns egg number
}


#########################################################

# decide the day of the month. Feb was treated all as 28
monthtime <- function(month){
  day30  <- c(4, 6, 9, 11)
  day31 <- c(1, 3, 5, 7, 8, 10, 12)
  day28 <- 2
  if(month %in% day30){
    return(30)
  }else if (month %in% day31){
    return(31)
  }else(
    return(28)
  )
}             #OM: if number of month is in any of these categories, return the number of day in that month

# the fianl mass after a month
mass_after_month <- function(initial_mass, month, TC){
  t <- growthtime(initial_mass, TC) + monthtime(month)        #OM: growthtime defined in eqn 1 plus monthtime: defined above gives t
  end_mass <- growthmass(t, TC)                               #OM: end mass is growth mass, from t
  return(end_mass)
}


# the final mass after a month with constant fixed. for the parr stage and the adult in marine stage
mass_after_month_fixed <- function(initial_mass, month, TC, c){     #OM: with a constant??
  t <- growthtime(initial_mass, TC) + monthtime(month)
  end_mass <- growthmass(t, TC)
  final_mass <- c *(end_mass -initial_mass) +initial_mass
  return(final_mass)
}


# the band width of parr0 
parr0_band<- function(start_weight, end_weight, TC, c){
  mv <-c(start_weight)
  m <- start_weight
  time <- c(7:12, 1:12)
  for(i in time){
    t <- growthtime(m, TC) + monthtime(i)
    end_mass <- growthmass(t, TC)
    m <- c *(end_mass -m) + m
    mv <- c(mv, m)
  }
  # to automatically add a year bandwidth to the vector 
  repeat{
    for(i in 1:12){
      t <- growthtime(m, TC) + monthtime(i)
      end_mass <- growthmass(t, TC)
      m <- c *(end_mass -m) + m
      mv <- c(mv, m)
    }
    if(! mv[length(mv)] < end_weight){ break } # Negation is crucial here!
  }
  return(mv)
}


# the right band number of the point, or the seqence number
right_point <- function(input_point, parr0_bandwidth){
  i <- 0
  repeat{
    i <- i+1
    point <- parr0_bandwidth[i]
    check1 <- point - input_point
    if(! check1 <0){ break } # Negation is crucial here!
  }
  return(i)
}


##########################################
# matrix of particular life stage 
##########################################

##########################################
# egg stage 
# initial number here should be the number of eggs generated from returners
# parameters need to be defined
SR_egg <- 0.165                     #OM: is sr monthly surivival - yes from table 1.egg survival rate from roberston 2005 
# the time of this stage is 1 month now. It can be adjusted to 2-6 weeks later
egg_matrix <- function(TC, global_min, global_max, matrix_size){
  fill <- rep(0, matrix_size)
  min <- global_min
  max <- global_max
  for(i in min:max){
    fill[i] <- fecundity((2*i+1)/2*10,TC)*SR_egg
  }
  return(fill)
}

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


# generate the matrix
fry_matrix <- function(month, TC, matrix_size){
  m <- matrix(0, matrix_size, matrix_size)
  for(i in 1:matrix_size){
    m[,i] <- fry_matrix_column(band_min = i, band_max = i+1, month, TC, matrix_size)
  }
  return(m)
}



#########################################################
# the transformation from fry to parr0
# transform the fry_matrix from 1 gram per band to another more narrow band
fry_parr0_column <- function(month, input_min, TC, parr0_bandwidth, matrix_size){ # need to add more detail, might not be correct in soem circumstance
  fill <- rep(0, matrix_size)
  band_min_am <- mass_after_month(input_min, monthtime(month), TC)
  band_max_am <- mass_after_month(input_min+1, monthtime(month), TC)
  band_width <- band_max_am - band_min_am
  min_right <-right_point(band_min_am, parr0_bandwidth)
  max_left <- right_point(band_max_am, parr0_bandwidth) -1
  band_top <- parr0_bandwidth[min_right] - band_min_am
  band_bottom <-band_max_am - parr0_bandwidth[max_left]
  band_top_p <- band_top/band_width
  band_bottom_p <- band_bottom/band_width
  if (max_left - min_right == 0) {
    fill[min_right-1] <- band_top_p * SR_parr0  # check whether it is approprate to use the mass here to calculate survival rate
    fill[min_right] <- band_bottom_p * SR_parr0
  } else if (max_left - min_right == 1) {
    fill[min_right-1] <- band_top_p * SR_parr0
    fill[max_left] <- band_bottom_p * SR_parr0
    fill[min_right] <- (parr0_bandwidth[max_left]- parr0_bandwidth[min_right])/band_width * SR_parr0
  } else if(max_left- min_right == -1){
    fill[max_left] <- SR_parr0
  }else {
    fill[min_right-1] <- band_top_p * SR_parr0
    fill[max_left] <- band_bottom_p * SR_parr0
    for(i in min_right: (max_left-1)) {
      fill[i] <- (parr0_bandwidth[i+1]- parr0_bandwidth[i])/band_width * SR_parr0
    }
  }
  return(fill)
}



fry_parr0_matrix <- function(month, TC, parr0_bandwidth, fry_matrix_size, parr_matrix_size){
  m <- matrix(0, nrow = parr_matrix_size, ncol = fry_matrix_size)
  for(i in 1:fry_matrix_size){
    m[,i] <- fry_parr0_column(month, input_min = i, TC, parr0_bandwidth, matrix_size = parr_matrix_size)
  }
  return(m)
}
##########################################
# parr0 stage 
# initial vector here should be the matrix output of fry stage
# the time of the stage is 6 month


# generate the matrix column
# SR_parr0 need to be defined, it can be changed later
SR_parr0 <- 0.8608
# the growth rate of this stage follows the metabolic theory * 0.01
parr0_matrix_column <- function(band_min, band_max, month, TC, matrix_size, parr0_bandwidth, c){
  fill <- rep(0, matrix_size)
  band_min_am <- mass_after_month_fixed(band_min, monthtime(month), TC, c)
  band_max_am <- mass_after_month_fixed(band_max, monthtime(month), TC, c)
  band_width <- band_max_am - band_min_am
  min_right <-right_point(band_min_am, parr0_bandwidth)
  max_left <- right_point(band_max_am, parr0_bandwidth) -1
  band_top <- parr0_bandwidth[min_right] - band_min_am
  band_bottom <-band_max_am - parr0_bandwidth[max_left]
  band_top_p <- band_top/band_width
  band_bottom_p <- band_bottom/band_width
  if (max_left - min_right == 0) {
    fill[min_right-1] <- band_top_p * SR_parr0  # check whether it is approprate to use the mass here to calculate survival rate
    fill[min_right] <- band_bottom_p * SR_parr0
  } else if (max_left - min_right == 1) {
    fill[min_right-1] <- band_top_p * SR_parr0
    fill[max_left] <- band_bottom_p * SR_parr0
    fill[min_right] <- (parr0_bandwidth[max_left]- parr0_bandwidth[min_right])/band_width * SR_parr0
  } else if(max_left- min_right == -1){
    fill[max_left] <- SR_parr0
  } else {
    fill[min_right-1] <- band_top_p * SR_parr0
    fill[max_left] <- band_bottom_p * SR_parr0
    for(i in min_right: (max_left-1)) {
      fill[i] <- (parr0_bandwidth[i+1]- parr0_bandwidth[i])/band_width * SR_parr0
    }
  }
  
  # to check whether the column length is out of the range of matrix
  if(length(fill) > matrix_size){
    fill <- fill[1: matrix_size] 
  }
  
  if(sum(fill) < (SR_parr0-0.1)){
    fill[matrix_size] <- SR_parr0 - sum(fill)
  }
  
  return(fill)
}

# generate the matrix
parr0_matrix <- function(month, TC, matrix_size, parr0_bandwidth, c){
  m <- matrix(0, matrix_size, matrix_size)
  for(i in 1: matrix_size){
    m[,i] <- parr0_matrix_column(band_min= parr0_bandwidth[i], band_max = parr0_bandwidth[i+1], month, TC, matrix_size, parr0_bandwidth, c)
  }
  return(m)
}


##########################################
# parr later check smolt 
# initial vector here should be the matrix output of parr0_matrix
# the time of the stage is 4 month, might change later
SR_p <- 0.9458

parr_matrix_column <- function(band_min, band_max, month, TC, matrix_size, parr0_bandwidth, c){
  fill <- rep(0, matrix_size)
  band_min_am <- mass_after_month_fixed(band_min, monthtime(month), TC, c)
  band_max_am <- mass_after_month_fixed(band_max, monthtime(month), TC, c)
  band_width <- band_max_am - band_min_am
  min_right <-right_point(band_min_am, parr0_bandwidth)
  max_left <- right_point(band_max_am, parr0_bandwidth) -1
  band_top <- parr0_bandwidth[min_right] - band_min_am
  band_bottom <-band_max_am - parr0_bandwidth[max_left]
  band_top_p <- band_top/band_width
  band_bottom_p <- band_bottom/band_width
  if (max_left - min_right == 0) {
    fill[min_right-1] <- band_top_p * survival((parr0_bandwidth[min_right-1]+parr0_bandwidth[min_right])/2, TC)  # check whether it is approprate to use the mass here to calculate survival rate
    fill[min_right] <- band_bottom_p * survival((parr0_bandwidth[min_right]+parr0_bandwidth[min_right+1])/2, TC)
  } else if (max_left - min_right == 1) {
    fill[min_right-1] <- band_top_p * survival((parr0_bandwidth[min_right-1]+parr0_bandwidth[min_right])/2, TC)
    fill[max_left] <- band_bottom_p * survival((parr0_bandwidth[max_left-1]+parr0_bandwidth[max_left])/2, TC)
    fill[min_right] <- (parr0_bandwidth[max_left]- parr0_bandwidth[min_right])/band_width * survival((parr0_bandwidth[min_right]+parr0_bandwidth[min_right+1])/2, TC)
  } else if(max_left- min_right == -1){
    fill[max_left] <-survival((parr0_bandwidth[max_left-1]+parr0_bandwidth[max_left])/2, TC)
  } else {
    fill[min_right-1] <- band_top_p * survival((parr0_bandwidth[min_right-1]+parr0_bandwidth[min_right])/2, TC)
    fill[max_left] <- band_bottom_p * survival((parr0_bandwidth[max_left-1]+parr0_bandwidth[max_left])/2, TC)
    for(i in min_right: (max_left-1)) {
      fill[i] <- (parr0_bandwidth[i+1]- parr0_bandwidth[i])/band_width * survival((parr0_bandwidth[i]+parr0_bandwidth[i+1])/2, TC)
    }
  }
  
  # to check whether the column length is out of the range of matrix
  if(length(fill) > matrix_size){
    fill <- fill[1: matrix_size] 
  }
  
  if(sum(fill) == 0){
    fill[matrix_size] <- survival((parr0_bandwidth[matrix_size]+parr0_bandwidth[matrix_size+1])/2, TC)
  }
  
  
  return(fill)
}


parr_matrix <- function(month, global_min, global_max, TC, matrix_size, parr0_bandwidth, c){
  m <- matrix(0, matrix_size, matrix_size)
  for(i in global_min: global_max){
    m[,i] <- parr_matrix_column(band_min= parr0_bandwidth[i], band_max = parr0_bandwidth[i+1], month, TC, matrix_size, parr0_bandwidth, c)
  }
  return(m)
}

##########################################
# smolt and post-smolt 
# the time of the stage is 8 month, might change later
SR_ps <- 0.8721                              #OM: survivial rate post smolts, from table
# generate the matrix column
# the growth rate of this stage follows the metabolic theory * 0.01
smolt_matrix_column <- function(band_min, band_max, month, TC, matrix_size, c){
  fill <- rep(0, matrix_size)
  band_min_am <- mass_after_month_fixed(band_min, monthtime(month), TC, c)
  band_max_am <- mass_after_month_fixed(band_max, monthtime(month), TC, c)
  band_width <- band_max_am - band_min_am
  band_integer <- floor(band_max_am)-ceiling(band_min_am)
  band_top <- ceiling(band_min_am) - band_min_am
  band_bottom <-band_max_am - floor(band_max_am)
  band_top_p <- band_top/band_width
  band_bottom_p <- band_bottom/band_width
  band_middle_p <- 1/band_width   #remember to consider no middle one
  if (band_integer == 0) {
    fill[floor(band_min_am)] <- band_top_p * SR_ps  # check whether it is approprate to use the mass here to calculate survival rate
    fill[floor(band_max_am)] <- band_bottom_p * SR_ps
  } else if (band_integer == 1) {
    fill[floor(band_min_am)] <- band_top_p * SR_ps
    fill[floor(band_max_am)] <- band_bottom_p * SR_ps
    fill[(floor(band_min_am)+1)] <- band_middle_p * SR_ps
  } else if(band_integer == -1){
    fill[floor(band_min_am)] <- SR_ps
  } else {
    fill[floor(band_min_am)] <- band_top_p * SR_ps
    fill[floor(band_max_am)] <- band_bottom_p * SR_ps
    fill[(floor(band_min_am)+1):(floor(band_max_am)-1)] <- band_middle_p *SR_ps  
  }
  # to check whether the column length is out of the range of matrix
  if(length(fill) > matrix_size){
    fill <- fill[1: matrix_size] 
  }
  
  if(sum(fill) < (SR_ps-0.1)){
    fill[matrix_size] <- SR_ps - sum(fill)
  }
  return(fill)
}

parr_smolt_matrix <- function(month, min_seq, max_seq, TC, parr_matrix_size, smolt_matrix_size, parr0_bandwidth,c){
  m <- matrix(0, ncol = parr_matrix_size, nrow = smolt_matrix_size)
  for(i in min_seq:max_seq){
    m[,i] <- smolt_matrix_column(band_min = parr0_bandwidth[i], band_max = parr0_bandwidth[i+1], month, TC, matrix_size = smolt_matrix_size, c)
  }
  return(m)
}

smolt_matrix <- function(month, global_min, global_max, TC, matrix_size,c){
  m <- matrix(0, ncol = matrix_size, nrow = matrix_size)
  for(i in global_min:global_max){
    m[,i] <- smolt_matrix_column(band_min = i, band_max = i+1, month, TC, matrix_size, c)
  }
  return(m)
}


smolt_Dec_matrix_column <- function(band_min, band_max, month, TC, marine_matrix_size, c){
  fill <- rep(0, marine_matrix_size)
  band_min_am <- mass_after_month_fixed(band_min, monthtime(month), TC, c)/10
  band_max_am <- mass_after_month_fixed(band_max, monthtime(month), TC, c)/10
  band_width <- band_max_am - band_min_am
  band_integer <- ceiling(band_min_am)- floor(band_max_am)
  band_top <- ceiling(band_min_am) - band_min_am
  band_bottom <-band_max_am - floor(band_max_am)
  band_top_p <- band_top/band_width
  band_bottom_p <- band_bottom/band_width
  if (band_integer == 0) {
    fill[floor(band_min_am)] <- band_top_p * SR_ps # check whether it is approprate to use the mass here to calculate survival rate
    fill[floor(band_max_am)] <- band_bottom_p * SR_ps
  } else{
    fill[floor(band_min_am)] <- SR_ps
  }
  if(length(fill) > matrix_size){
    fill <- fill[1: matrix_size] 
  }
  return(fill)
}


smolt_Dec_matrix <- function (month, smolt_matrix_size, marine_matrix_size, TC, c){
  m <- matrix(0, ncol = smolt_matrix_size, nrow = marine_matrix_size)
  for(i in 70:smolt_matrix_size){
    m[,i] <- smolt_Dec_matrix_column(band_min = i, band_max = i+1, month, TC, marine_matrix_size, c)    
  }
  return(m)
}


##########################################
# marine stage 
# initial vector here should be the matrix output of smolt
# the time of the stage is  month, might change later

# generate the matrix column
# the growth rate of this stage follows the metabolic theory * 0.01
marine_matrix_column <- function(band_min, band_max, month, TC, matrix_size, c){
  fill <- rep(0, matrix_size)
  band_min_am <- mass_after_month_fixed(band_min*10, monthtime(month), TC, c)/10
  band_max_am <- mass_after_month_fixed(band_max*10, monthtime(month), TC, c)/10
  band_width <- band_max_am - band_min_am
  band_integer <- ceiling(band_min_am)- floor(band_max_am)
  band_top <- ceiling(band_min_am) - band_min_am
  band_bottom <-band_max_am - floor(band_max_am)
  band_top_p <- band_top/band_width
  band_bottom_p <- band_bottom/band_width
  band_middle_p <- 1/band_width   #remember to consider no middle one
  if (band_integer == 0) {
    fill[floor(band_min_am)] <- band_top_p * survival((band_min*2+1)/2*10, TC)# check whether it is approprate to use the mass here to calculate survival rate
    fill[floor(band_max_am)] <- band_bottom_p * survival((band_min*2+1)/2*10, TC)
  } else if (band_integer == 1) {
    fill[floor(band_min_am)] <- band_top_p * survival((band_min*2+1)/2*10, TC)
    fill[floor(band_max_am)] <- band_bottom_p * survival((band_min*2+1)/2*10, TC)
    fill[(floor(band_min_am)+1)] <- band_middle_p * survival((band_min*2+1)/2*10, TC)
  } else if(band_integer == -1){
    fill[floor(band_min_am)] <- survival((band_min*2+1)/2*10, TC)
  } else {
    fill[floor(band_min_am)] <- band_top_p * survival((band_min*2+1)/2*10, TC)
    fill[floor(band_max_am)] <- band_bottom_p * survival((band_min*2+1)/2*10, TC)
    fill[(floor(band_min_am)+1):(floor(band_max_am)-1)] <- band_middle_p * survival((band_min*2+1)/2*10, TC)  
  }
  # to check whether the column length is out of the range of matrix
  if(length(fill) > matrix_size){
    final_fill <- fill[1: matrix_size] 
  }else{
    final_fill <- fill
  }
  return(final_fill)
}



marine_matrix <- function(month, global_min, global_max, TC, matrix_size, c){
  m <- matrix(0, matrix_size, matrix_size)
  global_width <- global_max - global_min
  for(i in global_min:global_max){
    m[,i] <- marine_matrix_column(band_min = i, band_max = i+1, month, TC, matrix_size, c)
  }
  return(m)
}

#################################
# returner stage 
# initial vector here should be the matrix output of smolt
# the time of the stage is  month, might change later
SR_return <- 0.8

returner_m <- function(TC, matrix_size){ #it is not temperature relevant now
  returner <- matrix(0, ncol = matrix_size, nrow = matrix_size)
  for(i in 1:matrix_size){
    returner[i,i] <- SR_return
  }
  return(returner)
}
##################################################

next_year_matrix<- function(fry_matrix_size, parr_matrix_size, smolt_matrix_size, matrix_size){
  f <- fry_matrix_size
  p <- parr_matrix_size
  s <- smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s + 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  z[3,2] <- 1                 # Eyed-egg: Dec      Eyed-egg: Dec
  z[4,4] <- 1                 # Alevin
  
  # fry
  for(i in 5:(4+f)){
    z[i, i] <- 1  
  }
  
  # 0+ parr
  for(i in (5+f): (4+f+p)){
    z[i+p, i] <- 1
  }
  
  # 1+ parr
  for(i in (5+f+p):(4+f+2*p)){
    z[i+p, i] <- 1
  }
  
  # 2+ parr
  for(i in (5+f+2*p): (4+f+3*p)){
    z[i+p, i] <- 1
  }
  
  # 3+ parr
  for(i in (5+f+3*p): (4+f+4*p)){
    z[i+p, i] <- 1
  }
  
  # 4+ parr
  for(i in (5+f+4*p): (4+f+5*p)){
    z[i+p, i] <- 1
  }
  
  # 5+ parr
  for(i in (5+f+5*p): (4+f+6*p)){
    z[i+p, i] <- 1
  }
  
  # 6+ parr
  for(i in (5+f+6*p): (4+f+7*p)){
    z[i+p, i] <- 1
  }
  
  
  # smolt
  for(i in (5+f+7*p+s):(4+f+7*p+s+m)){
    z[i+m, i] <- 1
  }
  
  # SW1
  for(i in (5+f+7*p+s+m): (4+f+7*p+s+2*m)){
    z[i + m, i] <- 1
  }
  
  # SW2
  for(i in (5+f+7*p+s+2*m): (4+f+7*p+s+3*m)){
    z[i+m, i] <- 1
  }
  
  # SW3
  for(i in (5+f+7*p+s+3*m): (4+f+7*p+s+4*m)){
    z[i, i] <- 1
  }
  
  return(z)
}



#year_book <- year_book_matrix(fry_matrix_size = 15, parr_matrix_size = 1002, smolt_matrix_size = 1000, marine_matrix_size = 850, returner_matrix_size = 85  )

m1 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 1
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  # eyed-egg
  z[3,3] <-ea_m(TC)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  
  return(z)
}

m2 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 2
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  # eyed-egg
  z[3,3] <-ea_m(TC)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  
  return(z)
}

m3 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 3
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  # eyed-egg
  z[3,3] <-ea_m(TC)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  
  return(z)
}

m4 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 4
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  # alevin
  z[4,3] <-ea_m(TC)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+p+cs-1)] <- parr[, 1:(cs-1)]
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+2*p+cs-1)] <- parr[, 1:(cs-1)] 
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+3*p+cs-1)] <- parr[, 1:(cs-1)]
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+4*p+cs-1)] <- parr[, 1:(cs-1)]
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+5*p+cs-1)] <- parr[, 1:(cs-1)]
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+6*p+cs-1)] <- parr[, 1:(cs-1)]
  # parr-smolt
  smolt <- parr_smolt_matrix(month, cs, end_point, TC, parr_matrix_size= p, smolt_matrix_size = s, parr0_bandwidth, c=parr_c)
  z[(5+f+7*p): (4+f+7*p+s), (5+f+p+cs):(4+f+2*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+2*p+cs):(4+f+3*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+3*p+cs):(4+f+4*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+4*p+cs):(4+f+5*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+5*p+cs):(4+f+6*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+6*p+cs):(4+f+7*p)] <- smolt[,cs:(p-1)]
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+m+ca-1), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine[1:(ca-1), ]
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+2*m+ca-1), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine[1:(ca-1), ]
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+3*m+ca-1), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine[1:(ca-1), ]
  #returner
  z[(5+f+7*p+s+4*m+ca): (4+f+7*p+s+4*m+r), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine[ca:(m-1), ]
  z[(5+f+7*p+s+4*m+r+ca): (4+f+7*p+s+4*m+2*r), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine[ca:(m-1), ]
  z[(5+f+7*p+s+4*m+2*r+ca): (4+f+7*p+s+4*m+3*r), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine[ca:(m-1), ]
  
  return(z)
}

m5 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 5
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  #fry 
  z[5:(4+f), 4] <- fry_May(TC, matrix_size = f)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # parr-smolt
  smolt <- parr_smolt_matrix(month, cs, end_point, TC, parr_matrix_size= p, smolt_matrix_size = s, parr0_bandwidth, c=parr_c)
  z[(5+f+7*p): (4+f+7*p+s), (5+f+p+cs):(4+f+2*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+2*p+cs):(4+f+3*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+3*p+cs):(4+f+4*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+4*p+cs):(4+f+5*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+5*p+cs):(4+f+6*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+6*p+cs):(4+f+7*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+7*p):(4+f+7*p+s)] <- smolt_matrix(month, global_min = 1, global_max= s-1, TC, matrix_size = s, c = marine_c)
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  #returner
  z[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r), (5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r), (5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r), (5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r)] <- returner_m(TC, matrix_size = r)
  
  return(z)
}

m6 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 6
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  #fry 
  z[5:(4+f), 5:(4+f)] <- fry_matrix(month, TC, matrix_size = f)
  # parr1-
  parr <-parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # parr-smolt
  smolt <- parr_smolt_matrix(month, cs, end_point, TC, parr_matrix_size= p, smolt_matrix_size = s, parr0_bandwidth, c=parr_c)
  z[(5+f+7*p): (4+f+7*p+s), (5+f+p+cs):(4+f+2*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+2*p+cs):(4+f+3*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+3*p+cs):(4+f+4*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+4*p+cs):(4+f+5*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+5*p+cs):(4+f+6*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+6*p+cs):(4+f+7*p)] <- smolt[,cs:(p-1)]
  z[(5+f+7*p): (4+f+7*p+s), (5+f+7*p):(4+f+7*p+s)] <- smolt_matrix(month, global_min = 1, global_max= s-1, TC, matrix_size = s, c = marine_c)
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  #returner
  z[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r), (5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r), (5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r), (5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r)] <- returner_m(TC, matrix_size = r)
  
  return(z)
}

m7 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 7
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  #fry- parr0
  z[(5+f):(4+f+p), 5:(4+f)]  <- fry_parr0_matrix(month, TC, parr0_bandwidth, fry_matrix_size = f, parr_matrix_size = p)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # smolt
  z[(5+f+7*p): (4+f+7*p+s), (5+f+7*p):(4+f+7*p+s)] <- smolt_matrix(month, global_min = 1, global_max= s, TC, matrix_size = s, c = marine_c)
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  #returner
  z[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r), (5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r), (5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r), (5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r)] <- returner_m(TC, matrix_size = r)
  
  return(z)
}


m8 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 8
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  #parr0
  z[(5+f): (4+f+p), (5+f):(4+f+p)] <- parr0_matrix(month, TC, matrix_size = p , parr0_bandwidth, c = parr_c)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # smolt
  z[(5+f+7*p): (4+f+7*p+s), (5+f+7*p):(4+f+7*p+s)] <- smolt_matrix(month, global_min = 1, global_max= s, TC, matrix_size = s, c = marine_c)
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  #returner
  z[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r), (5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r), (5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r), (5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r)] <- returner_m(TC, matrix_size = r)
  
  return(z)
}

m9 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 9
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  #parr0
  z[(5+f): (4+f+p), (5+f):(4+f+p)] <- parr0_matrix(month, TC, matrix_size = p , parr0_bandwidth, c = parr_c)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # smolt
  z[(5+f+7*p): (4+f+7*p+s), (5+f+7*p):(4+f+7*p+s)] <- smolt_matrix(month, global_min = 1, global_max= s, TC, matrix_size = s, c = marine_c)
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  #returner
  z[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r), (5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r), (5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r), (5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r)] <- returner_m(TC, matrix_size = r)
  
  return(z)
}

m10 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 10
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  #parr0
  z[(5+f): (4+f+p), (5+f):(4+f+p)] <- parr0_matrix(month, TC, matrix_size = p , parr0_bandwidth, c = parr_c)
  # parr1-
  parr <-parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # smolt
  z[(5+f+7*p): (4+f+7*p+s), (5+f+7*p):(4+f+7*p+s)] <- smolt_matrix(month, global_min = 1, global_max= s, TC, matrix_size = s, c = marine_c)
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  #returner
  z[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r), (5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r), (5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r)] <- returner_m(TC, matrix_size = r)
  z[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r), (5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r)] <- returner_m(TC, matrix_size = r)
  
  return(z)
}

m11 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 11
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10
  
  #egg
  egg <- egg_matrix(TC, global_min = 1, global_max = r, matrix_size = r)
  z[1,(5+f+7*p+s+4*m): (4+f+7*p+s+4*m+r)] <- egg
  z[1,(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m+2*r)] <- egg
  z[1,(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m+3*r)] <- egg
  #parr0
  z[(5+f): (4+f+p), (5+f):(4+f+p)] <- parr0_matrix(month, TC, matrix_size = p , parr0_bandwidth, c = parr_c)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # smolt
  z[(5+f+7*p): (4+f+7*p+s), (5+f+7*p):(4+f+7*p+s)] <- smolt_matrix(month, global_min = 1, global_max= s, TC, matrix_size = s, c = marine_c)
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  
  return(z)
}

#parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = 110, TC = 0, c = parr_c)
#mm11 <- m11(TC= 3, fry_matrix_size= 15, smolt_matrix_size= 1500, matrix_size= 8500, parr0_bandwidth, parr_c=0.2, marine_c=0.8, checkmass_smolt=70, checkmass_adult= 1500, parr_max_input=100)
#mm11[1, 5500:7000]


m12 <- function(TC, fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input){
  month <- 11
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size #smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  z <- matrix(0, nrow = size, ncol = size)
  
  cs <- right_point(checkmass_smolt, parr0_bandwidth)
  ca <- checkmass_adult/10  
  
  #eyed-egg
  z[2,1] <-ea_m(TC)
  #parr0
  z[(5+f): (4+f+p), (5+f):(4+f+p)] <- parr0_matrix(month, TC, matrix_size = p , parr0_bandwidth, c = parr_c)
  # parr1-
  parr <- parr_matrix(month, global_min= start_point, global_max = end_point, TC, matrix_size= p, parr0_bandwidth, c= parr_c)
  z[(5+f+p): (4+f+2*p), (5+f+p):(4+f+2*p)] <- parr 
  z[(5+f+2*p): (4+f+3*p), (5+f+2*p):(4+f+3*p)] <- parr
  z[(5+f+3*p): (4+f+4*p), (5+f+3*p):(4+f+4*p)] <- parr
  z[(5+f+4*p): (4+f+5*p), (5+f+4*p):(4+f+5*p)] <- parr
  z[(5+f+5*p): (4+f+6*p), (5+f+5*p):(4+f+6*p)] <- parr
  z[(5+f+6*p): (4+f+7*p), (5+f+6*p):(4+f+7*p)] <- parr
  # smolt-postsmolt
  z[(5+f+7*p+s): (4+f+7*p+s+m), (5+f+7*p):(4+f+7*p+s)] <- smolt_Dec_matrix(month, smolt_matrix_size = s, marine_matrix_size = m, TC, c=marine_c)
  
  #marine 
  marine <- marine_matrix(month, global_min = 10, global_max= m, TC, matrix_size = m, c = marine_c)
  z[(5+f+7*p+s+m): (4+f+7*p+s+2*m), (5+f+7*p+s+m):(4+f+7*p+s+2*m)] <- marine
  z[(5+f+7*p+s+2*m): (4+f+7*p+s+3*m), (5+f+7*p+s+2*m):(4+f+7*p+s+3*m)] <- marine
  z[(5+f+7*p+s+3*m): (4+f+7*p+s+4*m), (5+f+7*p+s+3*m):(4+f+7*p+s+4*m)] <- marine
  
  return(z)
}

###############################################

loop_year_returner <- function(whole_temp, year_min, year_max, fry_matrix_size, smolt_matrix_size, matrix_size, input_returner_sw1, input_returner_sw2, parr_c,marine_c, parr_max_input, parr_min_input, checkmass_smolt, checkmass_adult){
  use_temp <- subset(whole_temp, year > year_min -1 & year < year_max +1)
  
  parr0_bandwidth <- parr0_band(start_weight = 1, end_weight = parr_max_input+100, TC = 0, c = parr_c)
  end_point <- right_point(parr_max_input, parr0_bandwidth)-1
  start_point <- right_point(parr_min_input, parr0_bandwidth)-1
  f <- fry_matrix_size #fry_matrix_size
  p <- end_point # parr_matrix_size
  s <- smolt_matrix_size
  m <- matrix_size/10    #marine_matrix_size
  r <- matrix_size/10   #returner_matrix_size
  
  size <- 5 + f + 7 * p + s+ 4* m +  3 * r
  year_transition_matrix <- next_year_matrix(fry_matrix_size = f, parr_matrix_size = p, smolt_matrix_size = s, matrix_size= matrix_size)
  
  record_matrix <- matrix(0, ncol =(year_max-year_min+2), nrow = size)
  
  for(i in year_min:year_max){
    start_time <- Sys.time()
    year_count <- i+1-year_min
    print("year_count")
    print(year_count)
    
    TC_sheet <- subset(use_temp, year == i)
    TC_vector <- TC_sheet[,4]
    print(TC_vector)
    
    if(year_count ==1) {
      record_year_initial <- rep(0, size)  
    }  
    
    record <- matrix(0,nrow = size, ncol = 13)    
    record[, 1] <- record_year_initial
    mm1 <- m1(TC= TC_vector[1], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm2 <- m2(TC= TC_vector[2], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm3 <- m3(TC= TC_vector[3], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm4 <- m4(TC= TC_vector[4], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm5 <- m5(TC= TC_vector[5], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm6 <- m6(TC= TC_vector[6], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm7 <- m7(TC= TC_vector[7], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm8 <- m8(TC= TC_vector[8], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm9 <- m9(TC= TC_vector[9], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm10 <- m10(TC= TC_vector[10], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm11 <- m11(TC= TC_vector[11], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    mm12 <- m12(TC= TC_vector[12], fry_matrix_size, smolt_matrix_size, matrix_size, parr0_bandwidth, parr_c, marine_c, checkmass_smolt, checkmass_adult, parr_max_input, parr_min_input)
    
    record[, 2] <- round(mm1 %*% record[, 1], digits = 2) 
    record[, 3] <- round(mm2 %*% record[, 2], digits = 2) 
    record[, 4] <- round(mm3 %*% record[, 3], digits = 2) 
    record[, 5] <- round(mm4 %*% record[, 4], digits = 2) 
    record[, 6] <- round(mm5 %*% record[, 5], digits = 2) 
    record[, 7] <- round(mm6 %*% record[, 6], digits = 2) 
    record[, 8] <- round(mm7 %*% record[, 7], digits = 2) 
    record[, 9] <- round(mm8 %*% record[, 8], digits = 2) 
    record[, 10] <- round(mm9 %*% record[, 9], digits = 2) 
    record[, 11] <- round(mm10 %*% record[, 10], digits = 2) 
    
    
    
    if(year_count ==1) {
      record_year_initial <-record[,11]
      record_year_initial[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m +2*r)] <- returner_year_distribution(1800,2800, input_returner_sw1[1], matrix_size = r)
      record_year_initial[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m +3*r)] <- returner_year_distribution(4500,5500, input_returner_sw2[1], matrix_size = r)
      record[, 11] <- record_year_initial
    }else if(year_count ==2){
      record_year_initial <-record[,11]
      record_year_initial[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m +2*r)] <- returner_year_distribution(1800,2800, input_returner_sw1[2], matrix_size = r)
      record_year_initial[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m +3*r)] <- returner_year_distribution(4500,5500, input_returner_sw2[2], matrix_size = r)
      record[, 11] <- record_year_initial
    }else if(year_count ==3){
      record_year_initial <-record[,11]
      if(sum(record_year_initial[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m +3*r)])==0){
        record_year_initial[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m +2*r)] <- returner_year_distribution(1800,2800, input_returner_sw1[3], matrix_size = r)
        record_year_initial[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m +3*r)] <- returner_year_distribution(4500,5500, input_returner_sw2[3], matrix_size = r)
        record[, 11] <- record_year_initial
      }
    }else if(year_count ==4){
      record_year_initial <-record[,11]
      if(sum(record_year_initial[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m +3*r)])==0){
        record_year_initial[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m +2*r)] <- returner_year_distribution(1800,2800, input_returner_sw1[4], matrix_size = r)
        record_year_initial[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m +3*r)] <- returner_year_distribution(4500,5500, input_returner_sw2[4], matrix_size = r)
        record[, 11] <- record_year_initial
      }
    }else if(year_count ==5){
      record_year_initial <-record[,11]
      if(sum(record_year_initial[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m +3*r)])==0){
        record_year_initial[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m +2*r)] <- returner_year_distribution(1800,2800, input_returner_sw1[5], matrix_size = r)
        record_year_initial[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m +3*r)] <- returner_year_distribution(4500,5500, input_returner_sw2[5], matrix_size = r)
        record[, 11] <- record_year_initial
      }
    }else if(year_count ==6){
      record_year_initial <-record[,11]
      if(sum(record_year_initial[(5+f+7*p+s+4*m): (4+f+7*p+s+4*m +3*r)])==0){
        record_year_initial[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m +2*r)] <- returner_year_distribution(1800,2800, input_returner_sw1[6], matrix_size = r)
        record_year_initial[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m +3*r)] <- returner_year_distribution(4500,5500, input_returner_sw2[6], matrix_size = r)
        record[, 11] <- record_year_initial
      }
    }else if(year_count ==7){
      record_year_initial <-record[,11]
      if(sum(record_year_initial[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m +3*r)])==0){
        #record_year_initial[(5+f+7*p+s+4*m+r): (4+f+7*p+s+4*m +2*r)] <- returner_year_distribution(1500,2800, input_returner_sw1[7], matrix_size = r)
        record_year_initial[(5+f+7*p+s+4*m+2*r): (4+f+7*p+s+4*m +3*r)] <- returner_year_distribution(4500,5500, input_returner_sw2[7], matrix_size = r)
        record[, 11] <- record_year_initial
      }
    }  
    
    
    record[, 12] <- round(mm11 %*% record[, 11], digits = 2)
    record[, 13] <- round(mm12 %*% record[, 12], digits = 2)
    
    
    
    
    output <- paste("../Result/Use/iceland_predict/record",year_count,".csv")
    write.csv(record, output)
    
    record_matrix[,year_count] <- record[,13] 
    record_matrix[, year_count+1] <- round(year_transition_matrix %*% record_matrix[, year_count], digits = 2) 
    
    record_year_initial <- record_matrix[, year_count+1]
    
    print("finish 1 year")
    end_time <- Sys.time()
    print(end_time - start_time)
    
    
  }
  write.csv(record_matrix, "../Result/Use/iceland_predict/record_matrix.csv")
  print("finish for loop")
}



whole_temp <- read.csv("../Data/test_iceland_temp.csv")

input_data <-read.csv("../Vest_catch.csv")
input_data_year <- subset(input_data, year>2001)
initial_returner <- input_data_year[,3]

initial_returner_sw1 <- initial_returner*0.55
initial_returner_sw2 <- initial_returner*0.45

loop_year_returner(whole_temp, year_min = 2002, year_max = 2016, fry_matrix_size=15, smolt_matrix_size= 1500,matrix_size = 8500, input_returner_sw1= initial_returner_sw1, input_returner_sw2=initial_returner_sw2, parr_c = 0.11, marine_c = 0.64, parr_max_input = 200, parr_min_input = 20, checkmass_smolt = 70, checkmass_adult = 1500) 