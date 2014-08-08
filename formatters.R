## Formatting functions

# Function to print numbers to decimal places by default
print2 <- function(x, decimals=2) noquote(format(x, nsmall=decimals))

# function to return the direction and magnitude of a change as a string
strChange <- function(x) {
    if (x == 0) return("no effect")
    if (x > 0) return(paste("increase of", x))
    if (x < 0) return(paste("decrease of", abs(x)))
}
