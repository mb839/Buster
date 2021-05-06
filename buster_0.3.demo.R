#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# wget https://cran.r-project.org/src/contrib/Archive/plyr/plyr_1.8.1.tar.gz
# install.packages("plyr_1.8.1.tar.gz",repos=NULL,type="source")

# note: tried foreach, parallel etc.
# just tends to give the wrong answer.
# maybe come back to it but just pasting loops into foreach %do% isn't worth it

# name idea: "Buster". Buster Keaton famous for wearing Pork Pie Hats,
# this "busts" i.e. brute-forces pihat values.

# current version 0.3 beta.
# previous versions: alpha 1, alpha 2, alpha 3, 0.1 beta, 0.2 beta

#0.x.1 versions are really WIPs of 0.x+1 beta.

# in the first instance there should be 3 arguments:
# input file, if missing deaults to the most recent .ped in the directory
# filtering, if missing defaults to strict
# speed, if missing defaults to "slow" 
# (perhaps recategorise some speeds since "toofast" vs "veryfast" isn't much different)

# todo:

# functionality to do the above for all pedigrees in a ped file then rbind, all non-matches = 0.

# notes on beget nPlaces:
# 1000 to 10000 seems to be a reasonable range for comprimise between 
# speed and accuracy. 100 only gives 1 decimal place accuracy 


# note: this is for alpha 2, alpha 3 increased speed by a factor of 3-5x.
# note: version 0.1 allows removal of non-study ID entries so speeds up by another ~3x.
# note: 
#
# for a family with 4 initialisations, 5 matings and 2 comparisons:
# (numbers in chrom = time to run once, time to run 250x, (comment))
# 100 = 0.33, 1min23s (but horribly innacurate)
# 1000 = 1.16, 4min50s (not massively accurate but with wide tolerances may be usable) LOW
# 5000 = 3.75, 15min38s (acceptable degree of accuracy for most uses) MEDIUM
# 10000 = 7.52, 31min20s (high degree of accuracy) HIGH
# 20000 = 17.58,  (high degree of accuracy, diminishing returns starting to kick in)
# 35000 = 40.59, (very high degree of accuracy)
# 50000 = 107.17, (see 35000)
# 100000 = 197.25, 13h 41m 53s (overnight run for silly accuracy)
# 440000 = 2764.59, 8 days (literally why)
#
#
# version 0.1 speeds
# pedigree NL01
# 2 initialisations, 3 matings,  comparisons

qualVar <- "fast"
inputFile <- "Adamneveit.ped"
outputFile <- "Adamneveit_tab.csv"
filterStrictnessChar <- "none"
filterStrictness <- 1
adjustVals <- "yes"
outputTabs <- "yes"
sinkLocation <- ""
specificPedigree <- 0

if (length(args)>=1) {
  for (i in 1:length(args)) {
    if (args[i] == "-s" | args[i] == "--speed") {
      qualVar <- args[i + 1]
    }else if (args[i] == "-i" | args[i] == "--input") {
      inputFile <- args[i + 1]
    } else if (args[i] == "-o" | args[i] == "--output") {
      outputFile <- args[i + 1]
    } else if (args[i] == "-f" | args[i] == "--filter") {
      filterStrictnessChar <- args[i + 1]
    } else if (args[i] == "-a" | args[i] == "--adjust") {
      adjustVals <- args[i + 1]
    } else if (args[i] == "-t" | args[i] == "--tabulate") {
      outputTabs <- args[i + 1]
    } else if (args[i] == "--help") {
      helpThenQuit <- 1
    }
  }
}

if (exists("helpThenQuit")) {
  if (helpThenQuit == 1) {
    print("Buster Pedigree PiHat Relatedness Coefficient Estimator")
    print("Arguments with valid options are: ")
    print("-s --speed: toofast, fast, medium(default), slow, tooslow, placebo")
    print("-i --input: input filename, .ped format")
    print("-o --output: output filename, .csv format")
    print("-f --filter: none, loose(default), strict")
    print("-a --adjust: no(default), yes")
    print("-t --tabulate: no(default), yes")
    print("--help (no options)")
    quit(save="no")
  }
}

platformName <- as.character(Sys.info()['sysname'])

filterStrictnessChar <- tolower(filterStrictnessChar)

if (platformName == "Linux") {
  sinkLocation <- "/dev/null"
} else if (platformName == "Windows") {
  sinklocation <- "nul:"
}

# 0 = no study ID filter, 1 = loose filter, 2 = strict filter
if (filterStrictnessChar == "loose") {
  filterStrictness <- 1
} else if (filterStrictnessChar == "strict") {
  filterStrictness <- 2
} else if (filterStrictnessChar == "none") {
  filterStrictness <- 0
}

if (filterStrictness != 0) {
  if (platformName == "Linux") {
    if (filterStrictness == 2) {
      source('StudyCheckSum.R')
    } else if (filterStrictness == 1) {
      source('StudyCheckSumLoose.R')
    }
  } else if (platformName == "Windows") {
    if (filterStrictness == 2) {
      source('C:/Users/chiwei/Dropbox/VirtualboxShare/Code/StudyCheckSum.R')
    } else if (filterStrictness == 1) {
      source('C:/Users/chiwei/Dropbox/VirtualboxShare/Code/StudyCheckSumLoose.R')
    }
  }
}

sinkportable <- function() {
  if (!exists("sinkLocation")) {
    sink(tempfile())
  } else if (sinkLocation == "") {
    sink(tempfile())
  } else if (platformName == "Linux") {
    sink("/dev/null")
  } else if (platformName == "Windows") {
    sink("nul:")
  }
}

gimme <- function(x) {
  return(eval(as.name(paste(x))))
}

gander <- function(x) {
  return(paste(print(as.name(paste(x)))))
}

beget <- function(qualVar="medium") {

  # this almost certainly isn't necessary
  qualVar <- as.character(qualVar)
  
  nPlaces <- 5000
  nPlaces[grepl("medium",qualVar)] <- 5000
  nPlaces[grepl("fast",qualVar)] <- 1000
  nPlaces[grepl("slow",qualVar)] <- 10000
  nPlaces[grepl("slower",qualVar)] <- 20000
  nPlaces[grepl("lacebo",qualVar)] <- 35000
  nPlaces[grepl("superfast",qualVar)] <- 500
  nPlaces[grepl("toofast",qualVar)] <- 100
  nPlaces[grepl("myheaterisbroken",qualVar)] <- 100000
  nPlaces[grepl("justfuckmyshitupfam", qualVar)] <- 1000000
  
  nameDNA <- data.frame(chrom=sample(1:1000000, nPlaces, replace=F))
  
  nameDNA$pos <- as.numeric(rownames(nameDNA))
  
  return(nameDNA)
  
}

mateXY <- function(mumsDNA, dadsDNA) {
  
  merge <- merge(mumsDNA, dadsDNA, 
                 by.x = "pos", by.y = "pos"
  )
  
  merge <- merge[with(merge, order(pos)), ]
  
  merge$test <- sample(0:1, nrow(merge), replace=TRUE)
  
  # (merge$test == 0) => (merge$test1 == 1) and vice-versa
  merge$test1 <- (1 - merge$test) %% 2 
    
  merge$chrom <- 0
  
  #would apply be quicker here?
  # tried this as a function & apply, didn't seem to be any quicker
  
  merge$chrom <- as.numeric(paste(gsub("^0", "", as.character(rep(merge$chrom.x * eval(merge$test)))),
                                  gsub("^0", "", as.character(rep(merge$chrom.y * eval(merge$test1)))),
                                  sep=""))
  
  kidsDNA <- merge[c(1,6)]
  
  return(kidsDNA)
  
}

comparePiHat <- function(person1, person2) {
  
  merge <- merge(person1, person2,
                 by = "pos"
                 )
  
  merge$match <- 0
  
  merge$match[merge$chrom.x == merge$chrom.y] <- 1
  
  #prevents getting the mean of all the large numbers
  
  merge <- merge[c(4)]
  
  piHat <- as.numeric(colMeans(merge, na.rm = FALSE, dims = 1))
  
  # is it worth having a bit here that accounts for ethnicity? Or in post-processing like snap?
  
  return(piHat)
}

# makes sure that the work space is clear and only contains the necessary functions
# it will only be non-clear if using an IDE so skips if running in Rscript

if (interactive()) {
  
  rm(list=setdiff(ls(), c("beget", "comparePiHat", "filterStrictness", "platformName", 
                          "gander", "gimme", "mateXY", "studyCheckSum", "qualVar",
                          "args", "filterStrictness", "inputFile", "outputFile", "cl",
                          "adjustVals", "outputTabs", "sinkLocation", "sinkportable")))
}

print("Running Buster PiHat Relatedness coefficient estimator...")
print("Run with --help to see full list of options.")
print(paste("Current speed is:", qualVar))

ptm <- proc.time()
  
if (platformName == "Linux") {
    
  ped <- read.csv(file=paste(inputFile), sep = "\t", 
                  header=FALSE, stringsAsFactors = FALSE)
    
} else if (platformName == "Windows") {
    
  ped <- read.csv(file=paste("C:/Users/chiwei/Dropbox/VirtualboxShare/Code/", inputFile, sep="")
                  , sep = "\t", header=FALSE, stringsAsFactors = FALSE)
    
}

# this gets the number of unique families in the ped file
# length(unique(ped$V1))

# maybe do something liek argument = pick = list of pedigrees to keep
# then if overall pedigrees > 1, loops over all with auto options enabled



ped$founder <- 0
  
ped$founder[ped$V3 == "0" & ped$V4 == "0"] <- 1
  
pedfounders <- ped[which(ped$founder == 1),]
pedchildren <- ped[which(ped$founder == 0),]

# since this is usually a low number, foreach actually slows the whole thing down a little

print(paste("Initialising", nrow(pedfounders), "founder members..."))

sinkportable()

for (i in 1:nrow(pedfounders)) {
  assign(paste(print(pedfounders$V2[i])), beget(qualVar))
}

sink()

print(paste("Generating", nrow(pedchildren), "child members..."))

sinkportable()
  
j <- 0

# note: because of the iterative nature of the i-j loop, cannot use *apply
  
while (j < nrow(pedchildren)) {
  
  for (i in 1:nrow(pedchildren)) {
 
    if (!exists(gander(pedchildren$V2[i])) &
         exists(gander(pedchildren$V3[i])) &
         exists(gander(pedchildren$V4[i]))) {
                
      assign(paste(print(pedchildren$V2[i])), 
             mateXY(gimme(gander(pedchildren$V3[i])), 
                    gimme(gander(pedchildren$V4[i]))))
        
      j <- j + 1
        
    }
      
  }
    
}

sink()

# could check for valid studyID, seqstat=6?
  
if (filterStrictness !=0) {
  
  ped$works <- lapply(ped$V2, studyCheckSum)

  ped <- ped[ which(ped$works == TRUE),]
  ped$works <- NULL
    
}

print(paste("Running pairwise matches for", nrow(ped), "pedigree members.",
            "This requires", ((nrow(ped))*(nrow(ped) -1))/2, "evaluations..."))

sinkportable()
  
piHatTable <- matrix(ncol=nrow(ped),nrow=nrow(ped), 1.5)
  
colnames(piHatTable) <- unlist(ped$V2)
rownames(piHatTable) <- unlist(ped$V2)

# is there a way of parallelising this? the foreach package sped it up
# but gave the wrong answers which is pretty useless

# current method ensures that (n(n-1))/2 comparisons are carried out.
# since the diagonal entries are = 1 and lower diag = upper diag.
# so for a 5x5 comparison table only 10 comparisons are actually required, rather than 25.

for (i in 1:nrow(ped)) {
  piHatTable[i, i] <- 1.0000
}

for (i in 1:nrow(ped)) {

  for (j in 1:nrow(ped)) {
      
    if (piHatTable[i, j] > 1.075 & (i != j)) {
      
      person1 <- gander(ped$V2[i])  
      person2 <- gander(ped$V2[j])
        
      piHatTable[i, j] <- comparePiHat(gimme(person1), gimme(person2))
      piHatTable[j, i] <- piHatTable[i, j]
        
    }
      
  }
    
}
 
sink()

# this tries to adjust values to sensible nearby ones
if (adjustVals == "yes") {
  if (platformName == "Linux") {
    source('snap.R')
  } else if (platformName == "Windows") {
    source('C:/Users/chiwei/Dropbox/VirtualboxShare/Code/snap.R')
  }
}
   
# OUTPUTS IN TAB FORMAT

if (outputTabs == "yes") {
  
  library(reshape2) 
  
  if (!exists("piHatDF")) {
    piHatDF <- data.frame(Person1=character(), 
                          Person2=character(), 
                          piHat=double(),
                          stringsAsFactors=FALSE)
  }
  
  piHatDataFrame <- as.data.frame(piHatTable)
  
  # assuming the distance follows a metric we avoid everything below and on the diagonal
  piHatDataFrame[lower.tri(piHatDataFrame, diag = TRUE )]  <- NA
  piHatDataFrame$Person1 <- rownames(piHatDataFrame)
  
  dm.molten2 <- melt(piHatDataFrame, na.rm= TRUE, id.vars="Person1",
                     value.name="piHat", variable.name="Person2")
  
  piHatDF <- rbind(piHatDF, dm.molten2)
  
  outputTabFileName <- gsub(".csv", "_tab.csv", outputFile)
  
  
  if (platformName == "Linux") {
    
    write.csv(piHatDF, file=paste(outputTabFileName))
    
  } else if (platformName == "Windows") {
    
    write.csv(piHatTable, file=paste("C:/Users/chiwei/Dropbox/VirtualboxShare/Code/", 
                                     outputTabFileName, sep=""))
    
  }
  
}

# prints the table to the console
print(piHatTable)    

# lets the user know how long it took
print("Time taken:")
proc.time() - ptm

#exports the matrix, perhaps change to _mat and allow for tab only?
if (platformName == "Linux") {
  
  write.csv(piHatTable, file = paste(outputFile))
  
} else if (platformName == "Windows") {
    
    write.csv(piHatTable, file=paste("C:/Users/chiwei/Dropbox/VirtualboxShare/Code/", 
                                     outputFile, sep=""))
    #stopCluster(cl)
}


