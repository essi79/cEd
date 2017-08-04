
numbers2words <- function(x){
  # input: a numerical value between 1-999 or vector of such elements
  # output: the English spelling of the numeric value(s)
  # internal functions: helper and makenumber
  helper <- function(x){
    # recursive helper function to iterate through the digits of x and produce a string 
    # containing the English spelling of the numeric value in x
    if (x>999 || x<1) {
      stop("[numbers2words2]:argument out of range (1-999 supported)")
    }  
    # get the vector of digits in x in character format
    digits <- rev(strsplit(as.character(x), "")[[1]])
    # get the number of digits in x
    nDigits <- length(digits)
    # if x has one digit, return the value from the vector containing single-digit spellings
    if (nDigits == 1) as.vector(ones[digits])
    # if x has two digits, check to see if x is a teen value and return appropriate spelling
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    # if x has two digits, and is not a teen value, put the tens digit in the output string and call
    # this function recursively on the right-hand digit
    else paste(tens[digits[2]],
                    Recall(as.numeric(digits[1])))
    # if three digits, put the hundreds in the output and call the function recursively on the 2nd and 1st digits
    else if (nDigits == 3) paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1])))
  }
  # this function is used to convert characters back to string for when the helper function is called recursively   
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  #define lists of the English spellings 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  
  if (length(x) > 1) return(sapply(x, helper))
  helper(x)
}