# random other things that tend to be useful
options(digits=10) # Precision to 1e-8 makes lat/lon resolution accurate to ~1 meter

.c <- function(...)as.character(sys.call())[-1] # char strings from unquoted words
.q <- function(...) {s <- sys.call()[-1]; w <- as.character(s); n <- names(s); if(length(n)) names(w) <- n; w} # works for args
