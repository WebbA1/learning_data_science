# Looking at Parellelisation
library(parallel)
detectCores()
# We use 'mcapply' to distibute tasks to multiple processors.
# mclapply gathers up the responses from each of these function calls, 
# and returns a list of responses that is the same length as the list
# or vector of input data.