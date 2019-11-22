#######My take on hannahs stuff######

#packages
library(popbio)

#set directory for data
setwd("Documents/PhD/Miniproject")


###define parameters####

#OM?i dont quite get where these paramters come from?+ why? i do a bit more now, theyre used in the builidng of the matrix but not hundy p, in order to know river phase time?, would chnage depending on number of stages you have inriver? and weve just said each one is a year, therefore six years?
# Maximum number of full years spent in the river before leaving.
# This includes all stages from egg to smolt (although smolt doesn't have an entry).
Npy_V = 6

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

#!!! Should do a check that NSWR<=NSW otherwise return an error #####?why?

# Number of kelting events - i.e. how many repeat spawnings can take place after initial spawning (females only)
Nk_V = 1 # CURRENTLY ONLY CHECKED FOR 0 AND 1

# Number additional of winters spent at sea before returning again
# This does not include any winters spent at sea before the maiden return
# and does not include the winter spent as a kelt in the river before leaving
Nks_V = 1 # CHECK IF THIS CAN BE ZERO - i.e. they return after a few months at sea

# Range of additional winters spent at sea before returning again
NksR_V = 1

# Parr survival 
Spmat_V = matrix(0,Npy_V +1,1) # is this number of par stages based upon all stages from egg to smolt (i.e. Npy)
Spmat_V[1] = 0.8608 # S_0p from model (use S_f from Maine system for now)
Spmat_V[2] = 0.9458 # S_1p from model (use S_p from Maine system for now)
Spmat_V[3] = 0.9458 # S_2p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V[4] = 0.9458 # S_3p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V[5] = 0.9458 # S_4p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V[6] = 0.9458 # S_5p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V[7] = 0.9458 # S_6p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
#Spmat_V[8] = 0.9458 # S_7p from model. Use Maine S_p for now. Needs to be recalculated for Iceland/Vesturdalsa
Spmat_V #OM: view it

Sps_V =  0.8721 # Monthly esturaine/initial marine survival i.e. survival of post-smolts
# This is from overall estuary mortality of 56% in river Elidar in Iceland. I.e. 0.44^(1/6)


pp1_V = 0.020 # The first entry of p_plus. The probability of smoltifying at the first opportunity.
pp2_V = 0.526  # The second entry of p_plus. The probability of smoltifying at the second opportunity given that the
# fish did not smoltify at the first opportunity.

p_plus_V = matrix(0,Npy_V,1) #OM?same matrix produced as for smolts? why? cerates an empty m for something to do with smolting?
#p_plus_V[1] to p_plus_V[3] leave as 0 as can't smoltify at time t+1 if 0+, 1+ parr or 2+parr at time t (?is this iceland specific or fish specific?)
p_plus_V[4] = pp1_V # The first non-zero entry of p_plus. The probability of smoltifying at the first opportunity.
p_plus_V[5] = pp2_V # The second non-zero entry of p_plus. The probability of smoltifying at the second opportunity given that the
# fish did not smoltify at the first opportunity.
p_plus_V[6] = 1 # The third non-zero entry of p_plus. The probability of smoltifying at the third opportunity given that the
# fish did not smoltify at the first or second opportunity. ##OM?so you are one hundred percent going to smolt if you are this age?
p_plus_V

Sm_V = 0.97 #Monthly survival in the marine environment after first Jan 1st at sea (i.e. low mortality). Natural mortality. Value from Robertson (2005) based on ICES value
Sr_V = 0.8 # Monthly survival in the river after returning to spawn. No information #?wtf look back at maine system?#
#I DONT KNOW WHAT THESE ARE?   #?could be Appendix A2:  Calculation of survival probabilities across different life-stages ???
R1SW_V = 0.221
R2SW_V = 1-R1SW_V   
p1SW_V = ((R1SW_V/R2SW_V)*(0.97)^12) / (1+((R1SW_V/R2SW_V)*(0.97^12))) #huh 

p_SW_V = c(p1SW_V,1) # Made up for now. Entries are prob of female returning after 1SW, 
# prob of female returning after 2SW given she didn't return after 1 SW
Sk_V = 0.1 # A low value (guess - lower than Maine, USA) Overall survival as a kelt in the river - Low percentage (Thordardottir & Guðbergsson, 2017)
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
testp_V = 2 #OM?what is this?
#M_V = Vest_create_M(Npy_V, Rpy_V, NSW_V, NSWR_V, Nk_V, Nks_V, NksR_V, Sps_V, p_plus_V, Spmat_V, Sm_V, Sr_V, p_SW_V, Sk_V, Se_V, Sf_V, Fec_V, testp_V)


#Set transition matrix to be correct size (initialise with zeros) #?what is a tranistion matrix?
n_final = Npy_V + NSW_V + NSWR_V + Nk_V*(Nks_V + NksR_V) # Final dimension of matrix
n = Npy_V + (2*NSW_V) + Nk_V*(Nks_V + NksR_V) # Intermediate dimension until "unreachable" stages deleted
M = matrix(0,n,n) # creates blank matrix of n rows and n columns


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

# Last parr stage is special case (no stay-in-river option)
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
M[Npy_V+3, Npy_V+1] = (1-p_SW_V[1])*(Sps_V^2) * (Sm_V^10) #OM at nxt row down(1sw fish is it?^ is a returner appaz?)

if (Nk_V == 1){ #OM: Nk is number of kelting events(repeat spwanings) # if there is only one
  M[Npy_V + (2*NSW_V) + 1, Npy_V + 2] = Sk_V * (Sm_V^5.5) # Returning 1SW fish becomes a post-kelt if kelting occurs
} #OM check why this is doing this liv

M[1, Npy_V + 2] = Fec_V[1]*Se_V*(Sf_V^1.5)*(Spmat_V[1]^4) # Returning fish lays eggs, which survive 1.5 month as fry and 4 month 0 parr 


# Create rows and columns for all potential at sea and returner stages and delete those that don't occur later

for (j in 2:(NSW)){
  i = (j*2)
  M[Npy + i, Npy + (i-1)] = p_SW[j] * Sm^9 * Sr^3 # Returns
  
  if (j<NSW){      # The final returning fish option doesn't have a stay at sea option
    M[Npy + i+1, Npy + (i-1)] = (1-p_SW[j]) * Sm^12 # Stays at sea
  }
  if (Nk == 1){
    M[Npy + (2*NSW) + 1, Npy + i] = Sk * (Sm^5.5) # Returning fish becomes a post-kelt if kelting occurs
  }
  M[1, Npy + i] = Fec[j]*Se*(Sf^1.5)*(Spmat[1]^4) # Returning fish lays eggs
}


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#

#~~~~~~~~~~~~~~ Post kelts and repeat spawners ~~~~~~~~~~~~~~~~~~~~~~~~~~#
if (Nk == 1){
  M[Npy+(2*NSW)+2, Npy+(2*NSW)+1] = (Sm^9) * (Sr^3) # Post kelts at time t must become returners at t+1 (SIMPLIFYING ASSUMPTION)
  M[1, Npy + (2*NSW)+2] = Fec[NSW+1]*Se*(Sf^1.5)*(Spmat[1]^4) # Returning fish lays eggs
}
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


#its called m not m_V i think#
elast_a = elasticity(M_V)
rowcol = which(elast_a!=0,arr.ind = T)
e_values_a = elast_a[rowcol]

habitatcode = c(1,1,1,1,2,1,2,2,3,4,1,2,3,1,2,3,1)
outy = cbind( e_values_a, habitatcode)