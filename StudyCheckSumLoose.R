# function which gives a rough approximation of study ID checking
# this should be used where there may be typos, but anything
# obviously wrong should be removed.

# for example this will remove IV/1 but keep ADD00196P

studyCheckSum <- function(input, print=TRUE) {
  
  if ((nchar(input) < 7) | (nchar(input) > 13)) {
    
    return(FALSE)
  }
  
  if (grepl("^[[:alpha:]]{2,3}[0]{2,8}", input) == FALSE) {
    return(FALSE)
  }
  
  if (grepl(":", input) | grepl("/", input) |
      grepl("-", input) | grepl(" ", input)) {
    return(FALSE)
  }

  return(TRUE)
}
