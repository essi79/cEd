#The following function is developed in response to a programming question 
#The code with the latest changes can be found in the following GitHub repo:
#https://github.com/essi79/cEd
#For questions contact: essi.shams@gmail.com

createEdictionary <- function (a,b) {
    # input: two parameters (a & b)
    # output: a list of names and values (equal to a C# dictionary object) 
    # where names are the numeric values in the range from a to b (inclusive), #
    # and the values are the number of occurences of the letter 'e' in the 
    # English spelling of the corresponding numeric value
  
    # The original problem required that the return value be a dictionary.
    # lists with names elements are the closest data structure in R to a dictionary object in C#
    # Elements of the list can be referred to and extracted by their name by using the below convention:
    # ListName["MemberName"] which is equal to Listname[MemberIndex]
    # More info: http://www.r-tutor.com/r-introduction/list/named-list-members
    
    # This function uses the numbers2words function defined in the "n2w.R" source file in the same repo
    # the "n2w.R" file needs to be sourced with the R source command before sourcing this file. 
  
    # example: 
    # createEdictionary (6,10) returns:
    # a list as the following:
    #  $`6`
    #  [1] 0
     
    #  $`7`
    #  [1] 2
      
    #  $`8`
    #  [1] 1
      
    #  $`9`
    #  [1] 1
      
    #  $`10`
    #  [1] 1
    #
    
    #Check that input parameters are of the right class
    if (class(a)!="numeric" || class(b)!="numeric") {
      stop("[createEdictionary]: at least one invalid argument (not of class numeric)")
    }
    
    #define the output of type list
    outputlist <- list()
    #iterate through the range defined by input parameters
    for (i in (a:b)) {
      #get the English spelling of the number by using the numbers2words function
      englishspelling <- numbers2words(i)
      #count the number of time the letter e occurs in the English spelling
      countofoccurences <- sapply(regmatches(englishspelling, gregexpr("e", englishspelling)), length)
      #Advance to the next spot in the list which is currently NULL
      newindex = length(outputlist) +1
      #Assign the next spot in the list to the number of times e occurs in the English spelling
      outputlist[newindex] <- countofoccurences
      #Assign the list member a name equal to the number it is associated with
      names(outputlist)[newindex] <- i
    }
    #return the output list
    outputlist
}

