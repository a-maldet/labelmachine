# Function that checks if a variable name is syntactically valid
# This function was suggested by Hadley Wickham see [http://r.789695.n4.nabble.com/Syntactically-valid-names-td3636819.html]
is.syntactic <- function(x) x == make.names(x)

