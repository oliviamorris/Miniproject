##########Parr#########


##functions used here 
growthtime
monthtime
growthmass




# the band width of parr0 
parr0_band<- function(start_weight, end_weight, TC, c){
  mv <-c(start_weight)                           #OM: mv is start weight
  m <- start_weight                              #OM: as is m?
  time <- c(7:12, 1:12)                          #OM:months
  for(i in time){                                #OM: growth time and month time gives you the end mass?
    t <- growthtime(m, TC) + monthtime(i)
    end_mass <- growthmass(t, TC)
    m <- c *(end_mass -m) + m
    mv <- c(mv, m)
  }
  # to automatically add a year bandwidth to the vector 
  repeat{
    for(i in 1:12){
      t <- growthtime(m, TC) + monthtime(i)     #OM: t here is growth time and month time 
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
