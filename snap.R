# this will only be invoked if snap exists in the main program

# margin for error versus qualvar:
# toofast, veryfast: 10%
# fast, medium: 5%
# slow 3%
# slower 2%
# placebo 1%
# placebo+1 0.5%
# placebo+2 0.1%

#print(qualVar)

#print(exists(qualVar) == FALSE)

#if(!exists(qualVar)) {
#  qualVar <- "medium"
#}

#print(paste(qualVar) %in% c("medium", "slow", "slower", "placebo", "myheaterisbroken", "justfuckmyshitupfam"))

#if (qualVar == "medium" | qualVar == "slow") {
if (qualVar %in% c("medium", "slow", "slower", "placebo", "myheaterisbroken", "justfuckmyshitupfam")) {
  print("Running value correction appropriate for default or slower speeds...")
  # unrelated
  piHatTable[piHatTable < 0.0005] <- 0.0
  # this thingy
  piHatTable[piHatTable > 0.027 & piHatTable < 0.037] <- 0.03125
  # second cousins and equivalent
  piHatTable[piHatTable > 0.0550 & piHatTable < 0.0715] <- 0.0625
  # fist cousins
  piHatTable[piHatTable > 0.1197 & piHatTable <0.1303] <- 0.1250
  # 0.14065
  piHatTable[piHatTable > 0.137 & piHatTable < 0.1435] <- 0.14065
  # half sibs, uncle/aunt, double first cousins
  piHatTable[piHatTable > 0.2400 & piHatTable < 0.2600] <- 0.2500
  # 3/4 sibs or sib-cousins
  piHatTable[piHatTable > 0.3650 & piHatTable < 0.3850] <- 0.3750
  # siblings or parent/offspring
  piHatTable[piHatTable > 0.4895 & piHatTable <=0.5105] <- 0.5000
  # sibslings or parent/offspring where parents are uncle/aunt/niece/nephew or equivalent
  piHatTable[piHatTable > 0.6150 & piHatTable < 0.6350] <- 0.6250
  # siblings or parent/offspring where parents are first cousins
  piHatTable[piHatTable > 0.5490 & piHatTable < 0.5760] <- 0.5625
  # siblings or parent/offspring where parents are triple second cousins
  piHatTable[piHatTable > 0.54125 & piHatTable <= 0.5490] <- 0.5469
  # siblings or parent/offspring where parents are first cousins once removed
  piHatTable[piHatTable > 0.51750 & piHatTable <= 0.54125] <- 0.53125
  # siblings or parent/offspring where parents are second cousins
  piHatTable[piHatTable > 0.5105 & piHatTable <= 0.51750] <- 0.51565
  # same person
  piHatTable[piHatTable >= 0.9750 & piHatTable <= 1.075] <- 1.0000
} else if (qualVar %in% c("fast", "veryfast", "toofast", "superfast")) {
  print("Running value correction appropriate for fast speeds, only recommended for simple pedigrees...")
  # unrelated
  piHatTable[piHatTable < 0.01] <- 0.0
  # sibs & parents
  piHatTable[piHatTable > 0.35 & piHatTable < 0.65] <- 0.5
  # fist cousins
  piHatTable[piHatTable > 0.08 & piHatTable <=0.2] <- 0.1250
  # half sibs, uncle/aunt, double first cousins
  piHatTable[piHatTable > 0.2 & piHatTable <= 0.35] <- 0.25  
  # same person
  piHatTable[piHatTable >= 0.9 & piHatTable <= 1.1] <- 1.0  
}
  

