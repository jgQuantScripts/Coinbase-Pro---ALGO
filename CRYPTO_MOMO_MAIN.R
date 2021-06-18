source("CRYPTO_MOMO_FUN.R")
### get Time Sequences for Algo
tmz = getTMZ(TF=60)
# ***********************************************************************************************
# THE FOLLOWING LINE ENSURES THAT THE ALGO START TIMES ARE GREATER THAN THE CURRENT TIME
# **** IF THE "SCAN" BLOCK STOP/FAILS - RE-RUN LINES: 12-25 *****
tmz <- tmz[tmz>Sys.time()]
# ***********************************************************************************************
#                               ALGO START
# ***********************************************************************************************
SLEEEP(1)
SCAN <- pblapply(as.list(2:length(tmz)), function(xx){
  source("CRYPTO_MOMO_FUN.R")
  # ***********************************************
  # get latest MOMO 
  OUT <- getMOMO(COINS=COINS)
  # send orders
  crypto2Trade(OUT=OUT,FIAT=FIAT,fiat2Invest = 50)
  # ***********************************************
  cat("\n")
  print(tail(OUT))
  cat("\n")
  # Sleep until the next bar
  SLEEEP(xx)
})

