###################YUXINS development equations######################

# 2. calculate the growth time 
growthtime <- function(m, TC){
  V <- -0.13* TC/(1+(TC/273))         #OM: from eqn, tc is temp in cels (e −0.13T c /(1+T c /273))
  t <- (m ^(0.25))*exp(5.73)*exp(V)   #OM: whats e(5.73) 
  #t <- round(t, 2)                   #OM m to the quarter, times e to the what, t - time
  return(t)                           #OM returns growth time to grow from 1g to M under Tc
}

# 3. calculate the growth mass in day
growthmass <- function(t, TC){        #OM: use t from above
  V <- - 0.13* TC/(1+(TC/273))
  m <- (exp(5.73)*exp(V)/t)^(-4) # the unit of t is day, *30 make it month . OM: she hasnt done this non?
  #m <- round(m,2)
  return(m)
}


############################################################################
#####Per month section#####

#4.monthtime decide the day of the month. Feb was treated all as 28
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


#5. mass after a month the fianl mass after a month
mass_after_month <- function(initial_mass, month, TC){
  t <- growthtime(initial_mass, TC) + monthtime(month)        #OM: growthtime defined in eqn 1 plus monthtime: defined above gives t
  end_mass <- growthmass(t, TC)                               #OM: end mass is growth mass, from t
  return(end_mass)
}


# the final mass after a month with constant fixed. for the parr stage and the adult in marine stage
mass_after_month_fixed <- function(initial_mass, month, TC, c){     #OM: with a constant?? food availability fa in thesis
  t <- growthtime(initial_mass, TC) + monthtime(month)
  end_mass <- growthmass(t, TC)
  final_mass <- c *(end_mass -initial_mass) +initial_mass
  return(final_mass)
}

################################ Livis version #################################################################################

#1. Development time (day)
growthtimeday <- function(m, TC, t0= 5.73, g=-0.13){
  t <- exp(t0) * m^0.25 * exp(g* TC/(1+(TC/273)) ) #values are taken from figure 1b         
  return(t)   #time in days
}


#2. Development time (month (/30))
growthtimemonth <- function(m, TC, t0= 5.73, g=-0.13){
  t <- exp(t0) * m^0.25 * exp(g* TC/(1+(TC/273)) ) #yuxins values are taken from figure 1b         
  return(t/30)  #per month                         
}


#3.Development mass (in a day?)
growthmassday <- function(t, TC, t0=5.73, g=-0.13){        #t: growthtime
  m <- (( exp(t0) * exp(g* TC/(1+(TC/273))))/t)^(-4)
  return(m)
}
# the unit of t is day, *30 make it month 


# Development mass (per month)
#growthmassmonth <- function(t, TC, t0=5.73, g=-0.13){       
#  m <- (( exp(t0) * exp(g* TC/(1+(TC/273))))/(t*30))^(-4)
#  return(m)
#}


#5.monthduration for how many days in each month (1-12). Feb was treated all as 28 
monthduration <- function(month){
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
} 

#6. the final mass after a month with constant c, fa (for the parr stage and the adult in marine stage)
# M2 = M1 + fa * (M2- M1)?
mass_after_month_fixed <- function(initial_mass, month, TC, c){    
  t <- growthtime(initial_mass, TC) + monthtime(month)
  end_mass <- growthmass(t, TC)
  final_mass <- c *(end_mass -initial_mass) +initial_mass
  return(final_mass)
}

######## plot of 1.##############################################################################
require(ggplot2)
set <- seq(0,10000, 0.1)

#plot of growth time per month#
part1 <- matrix(0, nrow = length(set), ncol = 3)
part1[,1] <- rep(-5, length(set)) 
part1[,2] <- set 
part1[,3] <- round(growthtimemonth(set,-5))

part2 <- matrix(0, nrow = length(set), ncol = 3)
part2[,1] <- rep(0, length(set)) 
part2[,2] <- set 
part2[,3] <- round(growthtimemonth(set,0))

part3 <- matrix(0, nrow = length(set), ncol = 3)
part3[,1] <- rep(5, length(set))
part3[,2] <- set 
part3[,3] <- round(growthtimemonth(set,5) , digits = 4)

part4 <- matrix(0, nrow = length(set), ncol = 3)
part4[,1] <- rep(10, length(set))
part4[,2] <- set 
part4[,3] <- round(growthtimemonth(set,10))

part5 <- matrix(0, nrow = length(set), ncol = 3)
part5[,1] <- rep(15, length(set)) 
part5[,2] <- set 
part5[,3] <- round(growthtimemonth(set,15))

part6 <- matrix(0, nrow = length(set), ncol = 3)
part6[,1] <- rep(20, length(set))
part6[,2] <- set 
part6[,3] <- round(growthtimemonth(set,20))


whole <- rbind(part1, part2, part3, part4,part5, part6)
colnames(whole) <-  c("Temperature","Mass","Growth.Time")
whole <- as.data.frame(whole)


ggplot(whole, aes(x= Mass, y= Growth.Time, group=Temperature, colour = Temperature))+
  geom_line(cex=0.5)+
  #stat_smooth( cex = 0.5)+
  scale_x_log10()+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="log(Mass)",y="Growth time (month)") +
  theme(axis.title  = element_text(size = 15))+
  theme(axis.text  = element_text(size = 12))+
  scale_color_gradient(low = "skyblue", high = "tomato")
#
  geom_vline(xintercept = 10, linetype="solid", color = "black", size=0.5) +
  geom_vline(xintercept = 70, linetype="solid", color = "black", size=0.5)+
  geom_vline(xintercept = 200, linetype="solid", color = "black", size=0.5) +
  geom_vline(xintercept = 1500, linetype="solid", color = "black", size=0.5) +
  annotate(geom="text", x=3, y=0, label="fry",color="black", size = 4) +
  annotate(geom="text", x=30, y=0, label="parr",color="black", size = 4) +
  annotate(geom="text", x=120, y=0, label="post-smolt",color="black", size = 3)+
  annotate(geom="text", x=600, y=0, label="marine salmon",color="black", size = 4)+
  annotate(geom="text", x=5000, y=0, label="adult",color="black", size = 4)


###########################################
  #plot of mass per day#
  part1 <- matrix(0, nrow = length(set), ncol = 3)
  part1[,1] <- rep(-5, length(set)) 
  part1[,2] <- set 
  part1[,3] <- round(growthmassday(set,-5))
  
  part2 <- matrix(0, nrow = length(set), ncol = 3)
  part2[,1] <- rep(0, length(set)) 
  part2[,2] <- set 
  part2[,3] <- round(growthmassday(set,0))
  
  part3 <- matrix(0, nrow = length(set), ncol = 3)
  part3[,1] <- rep(5, length(set))
  part3[,2] <- set 
  part3[,3] <- round(growthmassday(set,5) , digits = 4)
  
  part4 <- matrix(0, nrow = length(set), ncol = 3)
  part4[,1] <- rep(10, length(set))
  part4[,2] <- set 
  part4[,3] <- round(growthmassday(set,10))
  
  part5 <- matrix(0, nrow = length(set), ncol = 3)
  part5[,1] <- rep(15, length(set)) 
  part5[,2] <- set 
  part5[,3] <- round(growthmassday(set,15))
  
  part6 <- matrix(0, nrow = length(set), ncol = 3)
  part6[,1] <- rep(20, length(set))
  part6[,2] <- set 
  part6[,3] <- round(growthmassday(set,20))
  
  
  whole <- rbind(part1, part2, part3, part4,part5, part6)
  colnames(whole) <-  c("Temperature","Time","Mass")
  whole <- as.data.frame(whole)
  
  
  ggplot(whole, aes(x= Time, y= Mass, group=Temperature, colour = Temperature))+
    geom_line(cex=0.5)+
    #stat_smooth( cex = 0.5)+
    scale_y_log10()+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(x="Time",y="log mass") +
    theme(axis.title  = element_text(size = 15))+
    theme(axis.text  = element_text(size = 12))+
    scale_color_gradient(low = "skyblue", high = "tomato")
  
  