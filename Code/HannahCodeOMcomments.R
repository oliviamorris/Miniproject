#######My take on hannahs stuff######

#packages
library(popbio)

#set directory for data
setwd("Documents/PhD/Miniproject")


###define parameters####

#OM?i dont quite get where these paramters come from?+ why? i do a bit more now, theyre used in the builidng of the matrix but not hundy p, in order to know river phase time?, would chnage depending on number of stages you have inriver? and weve just said each one is a year, therefore six years?
# Maximum number of full years spent in the river before leaving.
# This includes all stages from egg to smolt (although smolt doesn't have an entry).
Npy_V = 6 #OM:the way it is used it for defining where to put things essentially and used to predicting like probability of lh stuff 

# Range size of parr years to allow e.g. Rpy = 2 if there is a main and alternative,
# Rpy = 3 if there is one main LH chanel and two others that are important
Rpy_V = 3

# Maximum number of winters spent at sea before maiden return
NSW_V = 2

# Range size of sea winters that returns happen over
# e.g. NSWR = 2 if have 1SW returners (females that return after 1 winter at sea) and 2SW returners,
# or NSWR = 2 if there are 2SW and 3SW females
# NSWR = 3 if there are 1SW, 2SW and 3SW females
NSWR_V = 2

#!!! Should do a check that NSWR<=NSW otherwise return an error #?why?

# Number of kelting events - i.e. how many repeat spawnings can take place after initial spawning (females only)
Nk_V = 1 # CURRENTLY ONLY CHECKED FOR 0 AND 1

# Number additional of winters spent at sea before returning again
# This does not include any winters spent at sea before the maiden return
# and does not include the winter spent as a kelt in the river before leaving
Nks_V = 1 # CHECK IF THIS CAN BE ZERO - i.e. they return after a few months at sea

# Range of additional winters spent at sea before returning again
NksR_V = 1

# Parr survival 
Spmat_V = matrix(0,Npy_V +1,1) #OM: Sp is montly survival during 1-6 stages and freshwater smolt phase, so create a mtrix for this section? why seven? is it smolt too?
Spmat_V[1] = 0.8608 # S_0p from model (use S_f from Maine system for now) #OM: Sf is monthly survival during fry stage
Spmat_V[2] = 0.9458 # S_1p from model (use S_p from Maine system for now) #OM: sp is monthly survival suring entire par stages 1-6
Spmat_V[3] = 0.9458 # S_2p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa #OM these are the same values no matter the age(?can imporve this?)
Spmat_V[4] = 0.9458 # S_3p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V[5] = 0.9458 # S_4p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V[6] = 0.9458 # S_5p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V[7] = 0.9458 # S_6p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
#Spmat_V[8] = 0.9458 # S_7p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V #OM: view it

Sps_V =  0.8721 # Monthly esturaine/initial marine survival #i.e. survival of post-smolts in marine phase
# This is from overall estuary mortality of 56% in river Elidar in Iceland. I.e. 0.44^(1/6)


pp1_V = 0.020 # The first entry of p_plus. The probability of smoltifying at the first opportunity.
pp2_V = 0.526  # The second entry of p_plus. The probability of smoltifying at the second opportunity given that the
# fish did not smoltify at the first opportunity. #OM calculated in SI of asgard report taken from robertson?

p_plus_V = matrix(0,Npy_V,1) #OM?same matrix produced as for smolts? why? cerates an empty m for something to do with smolting? different life history pasths i think
#p_plus_V[1] to p_plus_V[3] leave as 0 as can't smoltify at time t+1 if 0+, 1+ parr or 2+parr at time t (?is this iceland specific or fish specific?)
p_plus_V[4] = pp1_V # The first non-zero entry of p_plus. The probability of smoltifying at the first opportunity.
p_plus_V[5] = pp2_V # The second non-zero entry of p_plus. The probability of smoltifying at the second opportunity given that the
# fish did not smoltify at the first opportunity.
p_plus_V[6] = 1 # The third non-zero entry of p_plus. The probability of smoltifying at the third opportunity given that the
# fish did not smoltify at the first or second opportunity. ##OM?so you are one hundred percent going to smolt if you are this age?
p_plus_V

Sm_V = 0.97 #Monthly survival in the marine environment after first Jan 1st at sea (i.e. low mortality). Natural mortality. Value from Robertson (2005) based on ICES value
Sr_V = 0.8 # Monthly survival in the river after returning to spawn. No information #?wtf look back at maine system?#
#OM: I DONT KNOW WHAT THESE ARE?   #?could be Appendix A2:  Calculation of survival probabilities across different life-stages ???
R1SW_V = 0.221
R2SW_V = 1-R1SW_V   
p1SW_V = ((R1SW_V/R2SW_V)*(0.97)^12) / (1+((R1SW_V/R2SW_V)*(0.97^12))) #OM:huh 

p_SW_V = c(p1SW_V,1) # Made up for now. Entries are prob of female returning after 1SW, 
# prob of female returning after 2SW given she didn't return after 1 SW
Sk_V = 0.1 # A low value (guess - lower than Maine, USA) #Overall survival as a kelt in the river - Low percentage (Thordardottir & Guðbergsson, 2017)
Se_V = 0.165 # Maine, USA for now   #Survival from egg to beginning of fry stage
Sf_V = 0.8608 # Maine, USA for now  #Monthly survival during fry stage


# Fecundity data 
Fec_V = matrix(0,3,1) # three by one matrix for fecundity
Fec_V[1] = (2911/2) # Number of female eggs laid per 1SW female #Fecundity of female 1SW fish divided by two (i.e. number of female eggs) #?why? #Body size-fecundity relationship – 2kg fish (2016 statistics, Thordardottir & Guðbergsson, 2017)
Fec_V[2] = (7532/2) # Number of female eggs laid per 2SW female #Fecundity of female 2SW fish divided by two (i.e. number of female eggs) #Body size-fecundity relationship – 5kg fish (2016 statistics, Thordardottir & Guðbergsson, 2017)
Fec_V[3] = (13538/2) # Number of female eggs laid per MSW return spawning female #Second spawn fecundity. Body size-fecundity relationship – 8kg fish (assumption based on 2SW fish size and 78% of females being 2SW fish)
Fec_V

#####################################################################################################################
# Call the function to create the transition matrix etc.
testp_V = 2 #OM?what is this? #uncommented and not made a difference
#M_V = Vest_create_M(Npy_V, Rpy_V, NSW_V, NSWR_V, Nk_V, Nks_V, NksR_V, Sps_V, p_plus_V, Spmat_V, Sm_V, Sr_V, p_SW_V, Sk_V, Se_V, Sf_V, Fec_V, testp_V)


#Set transition matrix to be correct size (initialise with zeros) 
n_final = Npy_V + NSW_V + NSWR_V + Nk_V*(Nks_V + NksR_V) # Final dimension of matrix # OM: based on these things - is this so it can be edited depending on your number of states etc, is this a gernalising thing
n = Npy_V + (2*NSW_V) + Nk_V*(Nks_V + NksR_V) # Intermediate dimension until "unreachable" stages deleted
M = matrix(0,n,n) # OM:creates blank matrix of n rows and n columns based on number of states specified
M

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Create transition matrix !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! (and the parr LH vector if required) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Begin with parr stages &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#

# First do simple transition to next parr stage (i.e. probability of surviving to next stage) #this is just for parr stages
for (i in 1:Npy_V-1){ # 
  
  M[i+1,i] = (1-p_plus_V[i])*Spmat_V[i]^2 * Spmat_V[i+1]^10 # Don't smoltify ##OM: I think this probability of smolting (1 minus probability)(why?) times parr survival to the two time par survival to the 10(im not too sure why this happens)
  # Two months left in the ith stage in the river, and 10 months in (i+1)th #OM: due to durations?
  
  M[Npy_V+1,i] = p_plus_V[i] * (Spmat_V[i]^2) * (Spmat_V[i+1]^6) * (Sps_V^4) # Smoltify ##OM: 
  # Two months left in the ith stage in the river, and 6 months in (i+1)th 
  # then 4 as a post smolt at sea  #OM: same as above
  
}

#OM: look at M now
M

# Last parr stage is special case (no stay-in-river option)#OM I.e you have to smolt?
#OM: in m at row6+1,column6, insert the 6th row in pplus i.e probability of smolting is it? you have to at this point which is why one? times sixth row squared(why) in parr survival and seventh ^6(why) times post-smolt survival (esturay survival)
M[Npy_V+1,Npy_V] = p_plus_V[Npy_V] * (Spmat_V[Npy_V]^2) * (Spmat_V[Npy_V+1]^6) * (Sps_V^4) # p_plus[Npy] == 1
# Two months left as (Npy-1)+ parr (Npy th parr stage) in the river, and 6 months as Npy parr ((Npy+1)th parr stage) 
# then 4 as a post smolt at sea #OM why?
M

#OM:i think to understand this section i meed to understand why to the something is time?
#OM: and how you know that you need probability of survivng times survival here and survivial after?(i.e. p_plus_V[Npy_V] * (Spmat_V[Npy_V]^2) * (Spmat_V[Npy_V+1]^6) * (Sps_V^4))
#is that just what we have decided, is this subjective? never ending?
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#



#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Fish at sea and returning ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#

# Post smolt is special case as has high initial marine mortality
M[Npy_V+2, Npy_V+1] = p_SW_V[1]*(Sps_V^2) * (Sm_V^7) * (Sr_V^3) #OM:so at this time point[8,7]which is [1SWR,postsmolt] insert probability of female returning? times esturine survival to the two seven and three why?
M[Npy_V+3, Npy_V+1] = (1-p_SW_V[1])*(Sps_V^2) * (Sm_V^10) #OM at nxt row down(1sw fish is it?^ is a returner appaz?)#this is 0ne minus(why) probablility of female returningafter 1sw times monthly marine surivial(sps) in esturay times monthly surivial at sea, why? how do you know this again
M

if (Nk_V == 1){ #OM: Nk is number of kelting events(repeat spwanings) # if there is only one
  M[Npy_V + (2*NSW_V) + 1, Npy_V + 2] = Sk_V * (Sm_V^5.5) # Returning 1SW fish becomes a post-kelt if kelting occurs
} #OM  at end of parr/smolt and at two times number of sea winters possible (stage11) and stage end of smolt plus two (8), transition is kelt surivial esturay survival  -wait is it cos it is 11 times 8 essentially?
M #look

M[1, Npy_V + 2] = Fec_V[1]*Se_V*(Sf_V^1.5)*(Spmat_V[1]^4) # Returning fish lays eggs, which survive 1.5 month as fry and 4 month 0 parr #OM <-?survival times
M


# Create rows and columns for all potential at sea and returner stages and delete those that don't occur later

for (j in 2:(NSW_V)){ #no. sea winters
  i = (j*2) 
  M[Npy_V + i, Npy_V + (i-1)] = p_SW_V[j] * Sm_V^9 * Sr_V^3 # Returns #OM: prob of return * monthly surivival in marine * surviv in river once returned
  
  if (j<NSW_V){      # The final returning fish option doesn't have a stay at sea option
    M[Npy_V + i+1, Npy_V + (i-1)] = (1-p_SW_V[j]) * Sm_V^12 # Stays at sea #OM: not sure
  }
  if (Nk_V == 1){ #OM:If kelting is one (you spawned?)
    M[Npy_V + (2*NSW_V) + 1, Npy_V + i] = Sk_V * (Sm_V^5.5) # Returning fish becomes a post-kelt if kelting occurs
  }
  M[1, Npy_V + i] = Fec_V[j]*Se_V*(Sf_V^1.5)*(Spmat_V[1]^4) # Returning fish lays eggs
}

M
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#

#~~~~~~~~~~~~~~ Post kelts and repeat spawners ~~~~~~~~~~~~~~~~~~~~~~~~~~#
if (Nk_V == 1){ 
  M[Npy_V+(2*NSW_V)+2, Npy_V+(2*NSW_V)+1] = (Sm_V^9) * (Sr_V^3) # Post kelts at time t must become returners at t+1 (SIMPLIFYING ASSUMPTION)
  M[1, Npy_V + (2*NSW_V)+2] = Fec_V[NSW_V+1]*Se_V*(Sf_V^1.5)*(Spmat_V[1]^4) # Returning fish lays eggs
}

M
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

#[][][][][][][][][] Now find unreachable states and delete them [][][][][][][]#

# Any row that has all zero entries corresponds to an unreachable state
rowsums_M = rowSums(M)
isitzero = 0
for (i in 1:n){
  if (rowsums_M[i]==0){
    isitzero=isitzero+1
  }
  
}
if (isitzero > 0){
  unr = which(rowSums(M)==0)
  M = M[-unr,-unr]
}

#OM:dont have any in this example ^^

#OM:its called m not m_V now 
elast_a = elasticity(M) #OM: creates a matrix of elasticities of eigenvalues at each entry in matrix

rowcol = which(elast_a!=0,arr.ind = T) #OM:where are the results
e_values_a = elast_a[rowcol] #OM: e values at those^points

habitatcode = c(1,1,1,1,2,1,2,2,3,4,1,2,3,1,2,3,1) #OM:not sure who what or where this is
outy = cbind( e_values_a, habitatcode) 

outy2 = data.frame(outy)
rows1 = which(outy2$habitatcode ==1) #OM:where is habitat code and rename
rows2 = which(outy2$habitatcode ==2)
rows3 = which(outy2$habitatcode ==3)
rows4 = which(outy2$habitatcode ==4)

sum(outy2[rows1,1]) #OM:values of all elasticities at those habitats
sum(outy2[rows2,1])
sum(outy2[rows3,1])
sum(outy2[rows4,1])


#######################################################################################################################
mystuff = read.table("Data/Sensitivity_opt2_picklambda.csv", sep=",", header = T) #OM:wtf is this table

#OM: data contains the defined variables just in a table? - different values though for some not all?

cou=1 #OM: what is this
M_Vlt = Vest_create_M(mystuff$Npy_V[cou], mystuff$Rpy_V[cou], mystuff$NSW_V[cou], 
                      mystuff$NSWR_V[cou],mystuff$Nk_V[cou], mystuff$Nks_V[cou], mystuff$NksR_V[cou], 
                      mystuff$Sps_V[cou], 
                      rbind(0,0,0,mystuff$pp1_V[cou],mystuff$pp2_V[cou],1), 
                      rbind(mystuff$Spmat_V1[cou],mystuff$Spmat_V2[cou],mystuff$Spmat_V3[cou],mystuff$Spmat_V4[cou],mystuff$Spmat_V5[cou],mystuff$Spmat_V6[cou],mystuff$Spmat_V7[cou]), 
                      mystuff$Sm_V[cou], mystuff$Sr_V[cou], 
                      rbind(mystuff$p1SW_V[cou],1), 
                      mystuff$Sk_V[cou], mystuff$Se_V[cou], 
                      mystuff$Sf_V[cou], 
                      rbind(mystuff$Fec_V1[cou],mystuff$Fec_V2[cou],mystuff$Fec_V3[cou]), 
                      mystuff$testp_V[cou])

M_Vlt = Vest_create_M(Npy_V, Rpy_V, NSW_V, 
                      NSWR_V,Nk_V, Nks_V, NksR_V, 
                      Sps_V, 
                      rbind(0,0,0,pp1_V,pp2_V,1), 
                      rbind(Spmat_V1,Spmat_V2,Spmat_V3,Spmat_V4,Spmat_V5,Spmat_V6,Spmat_V7), 
                      Sm_V, Sr_V, 
                      rbind(p1SW_V,1), 
                      Sk_V, Se_V, 
                      Sf_V, 
                      rbind(Fec_V1,Fec_V2,Fec_V3), 
                      testp_V) #OM:creates a matrix inputting these values (esentially weve only given one which is why we have said one?)


#OM - spmat v1 is different here compared to sensitivity data file...?
#OM - fecV is also different?


n_V = Npy_V + (2*NSW_V) + Nk_V*(Nks_V + NksR_V) #OM same again not sure why/difference to below - 12

n_Vlt = Npy_V + (2*NSW_V) + Nk_V*(Nks_V + NksR_V) #OM:number of stages/years in this case? 
N1_Vlt = matrix(0,n_V,1) #OM:matrix created on number of stages? 
N1_Vlt #OM:12 by one matrix N1

# Doesn't matter what this is #OM: what is it based on? number of pops?
N1_Vlt[1]= 117025
N1_Vlt[2]= 49668
N1_Vlt[3]= 8441
N1_Vlt[4]= 6000
N1_Vlt[5]= 3400
N1_Vlt[6]= 1958
N1_Vlt[7]= 819
N1_Vlt[8]= 382
N1_Vlt[9]= 13
N1_Vlt[10]= 9
N1_Vlt[11]= 6
N1_Vlt[12]= 4

N1_Vlt

maxit = 10000  #OM:i.e. no. of years?
N_Vlt= matrix(0,n_V,maxit) #OM matrix of 0s, 12 by 10000 matrix
N_Vlt[,1] = N1_Vlt #OM: N input N1 initial adundances into 10000 matrix at beginning?

for (i in 2:maxit){
  N_Vlt[,i] = M %*% N_Vlt[,i-1]   #OM:same as MPMpractce, multiply initial matrix numbers thing by transition matrix #I removed the M_Vlt to be M 
  
}

prpss = matrix(0,n_V,maxit) #OM: matrix same as N_Vlt
sumss = colSums(N_Vlt) #OM 
for (i in 1:maxit){
  prpss[,i] = N_Vlt[,i]/sumss[i] #OM: divided by column sums, not too sure why
}

PRP = prpss[,maxit]
PRP[8]+PRP[10]+PRP[12]
#N1_Vbet = PRP*391/(PRP[8]+PRP[10]+PRP[12]) # For 1974

#maxit = 20
#N_Vltn= matrix(0,n_V,maxit)
#N_Vltn[,1] = round(N1_Vbet)

#for (i in 2:maxit){
#	N_Vltn[,i] = M_V_base %*% N_Vlt[,i-1]

#}

catch_V = read.table("Documents/PhD/MPM/Data/Vesturdalsa_catch.txt",sep="")
returners_n = N_Vltn[8,] + N_Vltn[10,] + N_Vltn[12,]
#OM: we dont have N_Vltn so ill try with N_Vlt
returners_n = N_Vlt[8,] + N_Vlt[10,] + N_Vlt[12,] #OM: these columns are the 1sw returners, 2sw returners and msw returner
#years2 = c(1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993)

#plot(years2[1:20],catch_V$V1[1:20],ylim=c(0,900),xlab="Year", ylab="Number of fish", col = "green", type="o")
#lines(years2[1:20],returners_n,type="o", col="black")
#legend(1970,-150,xpd=TRUE,legend=c("Model output - all returning females","Catch data"),col=c("black","green"), cex=0.8, pch=c(1,1) )

###################################
#############


## Vesturdalsa better starting point from data and assumptions
#OM:just trying different initial populations I presume
n_Vb = Npy_V + (2*NSW_V) + Nk_V*(Nks_V + NksR_V)
N1_Vb = matrix(0,n_Vb,1)
N1_Vb

#N1_Vb[1]= 26238
#N1_Vb[2]= 6997
#N1_Vb[3]= 1749
#N1_Vb[4]= 280
#N1_Vb[5]= 32
#N1_Vb[6]= 38
#N1_Vb[7]= 520
#N1_Vb[8]= 20
#N1_Vb[9]= 355
#N1_Vb[10]= 246
#N1_Vb[11]= 3
#N1_Vb[12]= 2


N1_Vb[1]= 11284#7011#11504#23194/2 #91 #67938
N1_Vb[2]= 16168#10046#16483#255134/2 #959 #747318
N1_Vb[3]=  8556#5316#8722#111331/2 #411 # 326102
N1_Vb[4]= 3006#1868#3065#74221/2 #274 # 217401
N1_Vb[5]= 204#408#254#416#114
N1_Vb[6]= 204#127#208
N1_Vb[7]= 569#(150+169)/(Sm^10)#520
N1_Vb[8]= 150#21
N1_Vb[9]= 169#117//(Sm^12)#355
N1_Vb[10]= 117#246
N1_Vb[11]= 3
N1_Vb[12]= 2

N1_Vb #OM:view 

maxit = 15 #OM: 15 iterations?
N_Vb = matrix(0,n_Vb,maxit) #OM:12 by 15 matrix
N_Vb[,1] = N1_Vb #OM: input inital values into matrix projections

N_Vb

for (i in 2:maxit){
  N_Vb[,i] = M %*% N_Vb[,i-1]   #OM: I have chnaged M_V to be just M
  
}

N_Vb #OM: shows the projections

#catch_V = read.table("Vesturdalsa_catch.txt",sep="")

returners = N_Vb[8,]+N_Vb[10,]+N_Vb[12,] #OM:8,10,12 are retruner columns, take the entire column and add them together to plot i presume
years=c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
plotout = cbind(returners,years,catch_V$V1[29:43])
plot(plotout[,2], plotout[,1], type="o", col="black", xlab="Years", 
     ylab = "Number", ylim=c(0,400))
lines(plotout[,2],plotout[,3], type="o", col="purple")
legend(1999, -70, xpd = TRUE, legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","purple"),cex=0.8,pch=c(1,1,1))

########## The long term proportions method of setting N1 ##############
n_Vb3 = Npy_V + (2*NSW_V) + Nk_V*(Nks_V + NksR_V)
N1_Vb3 = matrix(0,n_Vb3,1)

# Long term proportion from model
#Ltprp = propN_V_lt[,11] 

for (i in 1:12){
  N1_Vb3[i] = PRP[i] * 269/(PRP[8]+PRP[10]+PRP[12])
}

#for (i in 1:12){
#	N1_Vb3[i] = Ltprp[i] * 269/(0.0054+0.0008+0.0006)
#}

N1_Vb3 = round(N1_Vb3)
maxit = 15
N_Vb3 = matrix(0,n_Vb3,maxit)
N_Vb3[,1] = N1_Vb3

for (i in 2:maxit){
  N_Vb3[,i] = M_V %*% N_Vb3[,i-1]
  
}

returners3 = N_Vb3[8,]+N_Vb3[10,]+N_Vb3[12,]
years=c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
plotout3 = cbind(returners3,years,catch_V$V1[29:43])
plot(plotout3[,2], plotout3[,1], type="o", col="black", xlab="Year", 
     ylab = "Number", ylim=c(0,350))
lines(plotout3[,2],plotout3[,3], type="o", col="purple")
legend(1999, -55, xpd = TRUE, legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","purple"),cex=0.8,pch=c(1,1,1))

plotout3 = cbind(returners3,years,catch_V$V1[29:43])
yearsE = c(2002,2004,2006,2008,2010,2012,2014,2016)

# Composite figure
par(mfrow=c(2,1))
par(mai=c(0.4,0.8,0.4,0.1))
plot(plotout3[,2], plotout3[,1], type="o", col="black", xlab="", xaxt='n', 
     ylab = "Number", ylim=c(0,350))
axis(side = 1, at = yearsE, labels = FALSE, tck = -0.05)
lines(plotout3[,2],plotout3[,3], type="o", col="purple")
par(mai=c(0.9,0.8,0,0.1))
plot(plotout[,2], plotout[,1], type="o", col="black", xlab="Year", 
     ylab = "Number", ylim=c(0,350))
lines(plotout[,2],plotout[,3], type="o", col="purple")
legend(1999, -94, xpd = TRUE, bty = "n", legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","purple"),cex=0.8,pch=c(1,1,1))




#NAO = read.table("NAO.csv", sep=",", header=FALSE)

#par(mfrow=c(2,1))
#par(mai=c(0.1,0.8,0.5,0.1))
#plot(plotout3[,2], plotout3[,1], type="o", col="black", xlab="Year", 
#	ylab = "Number", ylim=c(0,350))
#lines(plotout3[,2],plotout3[,3], type="o", col="green")
#legend("topleft", legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","green"),cex=0.8,pch=c(1,1,1))
#barplot(NAO$V2, col="black", ylab = "NAO")

Mack = read.table("Makrel.csv",header=FALSE)

par(mfrow=c(4,1))
par(mai=c(0.1,0.6,0.1,0.1))
plot(plotout3[,2], plotout3[,1], type="o", col="black", xlab="Year", 
     ylab = "Number", ylim=c(0,350))
lines(plotout3[,2],plotout3[,3], type="o", col="green")
legend("topleft", legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","green"),cex=0.8,pch=c(1,1,1))
par(mai=c(0.1,0.6,0.2,0.1))
plot(plotout[,2], plotout[,1], type="o", col="black", xlab="Year",  
     ylab = "Number", ylim=c(0,350))
lines(plotout[,2],plotout[,3], type="o", col="green")
legend("topleft", legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","green"),cex=0.8,pch=c(1,1,1))
plot(years,Mack$V1,type="o", col="blue", xlab="Years", ylab="Mackerel catch")
par(mai=c(0.1,0.6,0.1,0.1))
barplot(NAO$V2, col="black", ylab = "NAO")



par(mfrow=c(4,1))
par(mai=c(0.1,0.6,0.4,0.1))
plot(plotout3[,2], plotout3[,1], type="o", col="black", xlab="Year", 
     ylab = "Number", ylim=c(0,350))
lines(plotout3[,2],plotout3[,3], type="o", col="green")
legend("topleft", legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","green"),cex=0.8,pch=c(1,1,1))
plot(years,Mack$V1,type="o", col="blue", xlab="Years", ylab="Mackerel catch")
barplot(NAO$V2, col="black", ylab = "NAO")


compareN1s = cbind(N1_Vb, N1_Vb3)

write.csv(propN_V_lt[,11],"model_longterm_prp.csv")


########## The long term proportions method of setting N1 from 2003 ##############
n_Vb4 = Npy_V + (2*NSW_V) + Nk_V*(Nks_V + NksR_V)
N1_Vb4 = matrix(0,n_Vb4,1)

# Long term proportion from model
#Ltprp = propN_V_lt[,11] 

for (i in 1:12){
  N1_Vb4[i] = PRP[i] * 175/(PRP[8]+PRP[10]+PRP[12])
}

#for (i in 1:12){
#	N1_Vb3[i] = Ltprp[i] * 175/(0.0054+0.0008+0.0006)
#}

N1_Vb4 = round(N1_Vb4)
maxit = 14
N_Vb4 = matrix(0,n_Vb4,maxit)
N_Vb4[,1] = N1_Vb4

for (i in 2:maxit){
  N_Vb4[,i] = M_V %*% N_Vb4[,i-1]
  
}

returners4 = N_Vb4[8,]+N_Vb4[10,]+N_Vb4[12,]
years=c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
plotout4 = cbind(returners4,years,catch_V$V1[30:43])
plot(plotout4[,2], plotout4[,1], type="o", col="black", xlab="Year", 
     ylab = "Number", ylim=c(0,350))
lines(plotout4[,2],plotout4[,3], type="o", col="purple")
legend(1999, -55, xpd = TRUE, legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","purple"),cex=0.8,pch=c(1,1,1))


## Vesturdalsa better starting point from data and assumptions 2003

n_Vb = Npy_V + (2*NSW_V) + Nk_V*(Nks_V + NksR_V)
N1_Vb = matrix(0,n_Vb,1)

#N1_Vb[1]= 26238
#N1_Vb[2]= 6997
#N1_Vb[3]= 1749
#N1_Vb[4]= 280
#N1_Vb[5]= 32
#N1_Vb[6]= 38
#N1_Vb[7]= 520
#N1_Vb[8]= 20
#N1_Vb[9]= 355
#N1_Vb[10]= 246
#N1_Vb[11]= 3
#N1_Vb[12]= 2


N1_Vb[1]= 46322
N1_Vb[2]= 12353
N1_Vb[3]= 3088
N1_Vb[4]= 494
N1_Vb[5]= 82
N1_Vb[6]= 41
N1_Vb[7]= 663
N1_Vb[8]= 13
N1_Vb[9]= 230
N1_Vb[10]= 160
N1_Vb[11]= 3
N1_Vb[12]= 2


maxit = 14
N_Vb = matrix(0,n_Vb,maxit)
N_Vb[,1] = N1_Vb

for (i in 2:maxit){
  N_Vb[,i] = M_V %*% N_Vb[,i-1]
  
}
#catch_V = read.table("Vesturdalsa_catch.txt",sep="")

returners = N_Vb[8,]+N_Vb[10,]+N_Vb[12,]
years=c( 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
plotout = cbind(returners,years,catch_V$V1[30:43])
plot(plotout[,2], plotout[,1], type="o", col="black", xlab="Years", 
     ylab = "Number", ylim=c(0,350))
lines(plotout[,2],plotout[,3], type="o", col="purple")
legend(1999, -55, xpd = TRUE, legend=c("Model - all female returners","Vesturdalsa catch data"), col=c("black","purple"),cex=0.8,pch=c(1,1,1))



#######OM: i dont know the difference between all these plots, I know what is going on just not why