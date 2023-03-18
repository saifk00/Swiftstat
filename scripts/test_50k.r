library(bnlearn)
library(tictoc)
# set the file you want to test
load("models/bnlearn/munin.rda"); tic(); x=rbn(bn, 50000); toc()