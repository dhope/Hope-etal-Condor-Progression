## ---- Control file for DSVM of Sandpiper Movement ------
## Imported 
## 
## Specifications for program

## Migration Variables

# food
0.01
# risk
-1
# outfile_name
countsTwoGroups.csv
# print_out
True
# obs_error
0.01
# finalDate
180
# baseline_arrival_date
106

## Movement Variables
# time_between_sites
1
# breedingsite
1
# totalbirds
2000


## Base variables
# A_arrival_time
4
# A_lengthofstay
5
# A_prob_skip
0. 0. 0.5 0.25 0.97 0.

# Proportion_A
1.0

# B_arrival_time
8
# B_lengthofstay
2
# B_prob_skip
0. 0. 0.6 0.3 0.99 0.

## Variables for non-breeding resident birds
# winteringSite
0
# proportionWinterResidents
0.
# dunlinMigrationarrivals
1 2 3 4
# winterProportions
np.arange(0, 1, 0.1)


## CRD Length of Stay Specific to this site
# separateCRD_LoS
False
# CRD_LOS
5



##------------ Not currently in use -----------##
# A_arrival_sigma
0.3
# B_arrival_sigma
0.2
# A_fuel_arrival
5
# A_fuel_sigma
0.5

# B_fuel_arrival
10
# B_fuel_sigma
0.5
# A_hideProbability
0.
# B_hideProbability
0.
##------------ Not currently in use -----------##


##------------ Sensitivity Variables -----------##
# num_cores
4
# Pop_Proportions
np.arange(0, 1.05, 0.05)
# seed_range
range(0,10)

## Variables for biasing Kachemak skipping probability
# biasKachemak
False
# critical_date
138

#!# Begin Dictionary - sensitivity_variables
# A_lengthofstay
1 2 3 4 5
# A_arrival_time
1 3 5 7 9
# time_between_sites
0.5 1 1.5 2 2.5 3 3.5 4
# skip_1_list
0.2 0.4 0.6
# skip_2_list
0.25 0.5 0.75
# skip_3_list
0.1 0.25 0.5
# skip_4_list
0.5 0.97 0.99
#!# End Dictionary


