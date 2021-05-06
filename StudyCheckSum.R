# function to determine if a given BRIDGE studyID is valid
# written by Matt Brown 23/11/2015, based on code by Tony Attwood
# usage: studyCheckSum("ADD000123A")
# prints a message and returns boolean
studyCheckSum <- function(input, print=TRUE) {
  # checks the input length is 10. If not prints a message & returns FALSE
  if (nchar(input) != 10) {
    #print(paste(input, "is not the correct length, it is", nchar(input), 
    #            "characters, need 10!"))
    return(FALSE)
  }
  # sets up the base as 37, this can be changed if needed
  base <- 37
  # takes the first 9 characters of the input
  input2 <- substr(input, 1,9)
  # sets up the codePoint vector
  codePoint <- vector(mode = "numeric", length = 9)
  # sets up the code point list as a string whose characters correspond to
  # code point + 1
  cypher <- "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*"
  # for loop from 1 to 9, finds the numerical position in the code point of
  # the first instance of each character in the studyID. this is fine since
  # there are obviously no repeats in the code point
  for (i in 1:9) {
    codePoint[i] <- as.numeric(gregexpr((substr(input2,i,i)), 
                                        as.character(cypher))) - 1
  }
  # initialises the sum variable
  sum <- 0
  # working back from 9 to 1, multiplies the code point by 2^n where n is
  # the nth cycle in the iteration, i.e. 1st multiplies by 2, then 4 etc.
  # then adds the multiplied codepoint to the sum variable.
  for (i in 9:1) {
    codePoint[i] <- (codePoint[i]) * (2^(abs(i-9) + 1))
    sum = codePoint[i] + sum
  }
  # takes the remainder of sum when divided by base away from base + 1
  # then makes sure it is still in the correct base
  pointCode <- (((base + 1) - (sum %% base)) %% base)
  # determines the correct check digit as the pointcode + 1th digit in
  # the code point. This is because pointCode runs from 0 to 36 but
  # the code point has places 1 to 37.
  checkDigit <- substr(cypher, pointCode + 1, pointCode + 1)
  # checks that the computed check digit matches the provided one
  # if they match, prints that it is valid and returns TRUE
  # if they do not match, prints that it is not valid and returns FALSE
  if (checkDigit == substr(input, 10,10)) {
    #print(paste(input, "is a valid Study ID."))
    return(TRUE)
  } else {
    #print(paste(input, "is NOT a valid Study ID, check digit should be", 
    #            checkDigit))
    return(FALSE)
  }
}