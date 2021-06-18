require("rgdax");require("httr");require("dplyr");require("data.table")
require("lubridate");require("quantmod");require("pbapply");require("stringr")
# https://cran.r-project.org/web/packages/rgdax/rgdax.pdf
#*************************************************************************************
#                          # API KEYS / ACCOUNT ID
#*************************************************************************************
KEYS <- new.env()
assign("apikey","*********************************",envir = KEYS)
assign("PASS","*********************************",envir = KEYS)
assign("apiSecret","*********************************",
       envir = KEYS)
# FIAT ; can be USD, EUR, GBP
FIAT = "USD"
COINS <- paste0(c("LINK","ETC","XLM","ETH"),"-",FIAT)
# will list all account IDs for different cryptos
allAccts <- rgdax::accounts(api.key = KEYS$apikey,secret = KEYS$apiSecret, passphrase = KEYS$PASS)
# size limits
sizeLMT <- rgdax::public_info(product=TRUE)
# time difference between current time zone and UTC-time
tmDIFF = round(as.numeric(difftime(Sys.time(),
                                   lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                   units = "hours")),0)
#*************************************************************************************
#                          # get real-time bars
#*************************************************************************************
# granularity: limited to 300 bars
# 60   :  1 minute bars
# 300  :  5 minute bars 
# 900  : 15 minute bars 
# 3600 :  1   hour bars 
# 21600:  6   hour bars 
# 86400:  1    day bars 

# calculates START time in order to get max number of bars (300) 
# for each interval/granularity
get_START = function(NOW, Interval)
{
  if(Interval == 60)
  {
    START = as.character(strftime(NOW - minutes(300), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 300)
  {
    START = as.character(strftime(NOW - minutes(1500), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 900)
  {
    START = as.character(strftime(NOW - minutes(4500), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 3600)
  {
    START = as.character(strftime(NOW - hours(300), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 21600)
  {
    START = as.character(strftime(NOW - hours(1800), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  if(Interval == 86400)
  {
    START = as.character(strftime(NOW - days(300), "%Y-%m-%dT%H:%M:%S.500Z"))
  }
  
  return(START)
}
# wrapper to get most recent bars
get_latest_crypto_bars = function(KEYS,COIN, Interval)
{
  # Current time
  NOW = Sys.time()
  # Start Time
  START = get_START(NOW=NOW,Interval=Interval)
  # End Time
  END   = as.character(strftime(NOW , "%Y-%m-%dT%H:%M:%S.500Z"))
  
  # BUILD COINBASE PRO URL
  url <- paste0("https://api.pro.coinbase.com/products/",COIN,"/candles?start=",START,
                "&end=",END,"&granularity=",Interval)
  # GET REQUEST
  exp <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Key" = KEYS$apikey,
                         "Passphrase"= KEYS$PASS,
                         "Secret" = KEYS$apiSecret))
  # format request
  dta <- fromJSON(url,simplifyVector = TRUE)
  # rbind bars
  #dta <- do.call(rbind,dta) %>% as.data.frame()
  dta <- dta %>% as.data.frame()
  # format column names
  colnames(dta)<- c("Time","Low","High","Open","Close","Volume")
  # local timezone adjustment
  tmDIFF = round(as.numeric(difftime(Sys.time(),
                                     lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                     units = "hours")),0)
  if(Interval == 86400)
  {
    # format timeStamps
    Time <- as.Date(as.POSIXct(dta$Time, origin = "1970-01-01",tz = Sys.timezone()) - hours(tmDIFF))
  }else{
    # format timeStamps
    Time <- as.POSIXct(dta$Time, origin = "1970-01-01",tz = Sys.timezone()) - hours(tmDIFF)
  }
  
  # convert to xts
  dta <- as.xts(dta[,c("Open","High","Low","Close","Volume")], order.by = Time)
  colnames(dta) <- paste0(str_sub(COIN,1,-5),".",c("Open","High","Low","Close","Volume"))
  # return data
  dta
}
#*************************************************************************************
#                          # get Momentum
#*************************************************************************************
getMOMO = function(COINS){
# get latest bars
PRC <- do.call(merge,lapply(as.list(COINS), function(x)
{
  Cl(get_latest_crypto_bars(KEYS$KEYS, COIN=x, Interval = 900))
}))
colnames(PRC) <- gsub(".Close","",names(PRC))
# get momentum for every 60 bars , round to 4 decimal places
MOMO60 <- na.omit(round(ROC(PRC,n=60,"discrete"),4))

# sequence of hourly intervals... will be used to extract the asset with the best momentum at that time
indx <- seq.POSIXt(from = round.POSIXt(index(MOMO60)[1],units = "hours"), to = Sys.time(), by = "1 hour")
# use the index to subset the momentum & price series
SELECT <- MOMO60[indx]
PRC2 <- PRC[indx]

# Contains the Column index for the MAXIMUM VALUE
SEQ <- as.numeric(apply(SELECT,1,which.max))
# CALCULATE PRICE RETURN OVER HOLDING PERIOD
RETS <- ROC(PRC2,type = "discrete")

# APPLY STRATEGY FOR THE HOLDING PERIOD
ALL <- do.call(merge.xts,lapply(as.list(1:dim(RETS)[2]), function(x){
  reclass(ifelse(SEQ==x,1,0),match.to=SELECT)*RETS[,x]
}))

colnames(ALL) <- names(PRC)
ALL[is.na(ALL)]<-0

# RETURNS Chart for the strategy
EQT <- reclass(rowSums(ALL),match.to=ALL); EQT[is.na(EQT)]<-0
# combine Momentum, EQT Returns, and Sequence
OUT <- cbind(SELECT,EQT,SEQ)
# return xts
OUT
}
#*************************************************************************************
#                          # Make a trading decision
#*************************************************************************************
#OUT = getMOMO(COINS)
crypto2Trade = function(OUT,FIAT,fiat2Invest)
{
  # select last 2 elements
  SEQ <- tail(OUT,2)  
  if(coredata(SEQ$SEQ[1]) != coredata(SEQ$SEQ[2]))
  {
    # CRYPTO to TRADE
    crypto2open <- names(SEQ[,coredata(SEQ$SEQ[2])])
    crypto2close<- names(SEQ[,coredata(SEQ$SEQ[1])])
    # available monies
    monies <- subset(allAccts, allAccts$currency == FIAT)$available %>% as.numeric
    # type : Optional character value for the order type. The default is "limit"
    # stop : Possible values apart from default NULL are "loss" or "entry". 
    # side : The default is "b" which stands for buy must be one of either "b" (buy) or "s" (sell).
    # price: It can either be an integer or float. Float values rounded to 2 decimals
    # size : Mandatory numeric value. It can either be an integer or float. Float values will NOT be rounded
    if(fiat2Invest < monies)
    {
    # get current ASK price 2 open position
    cur_PRC = get_best_bidAsk(KEYS,COIN=paste0(crypto2open))$ask.Price %>% as.numeric
    # send order to open position
    msg <- rgdax::add_order(api.key=KEYS$apikey, secret=KEYS$apiSecret, passphrase=KEYS$PASS, 
                     product_id = paste0(crypto2open,"-USD"),type = "limit", stop = NULL, 
                     stop_price = NULL, side = "b",price = cur_PRC, size=signif(fiat2Invest/cur_PRC,3))
    cat(paste0("\n",msg))
    # get current BID price 2 close position
    cur_PRC = get_best_bidAsk(KEYS,COIN=paste0(crypto2close))$bid.Price %>% as.numeric
    # get crypto size in portfolio
    cur_SZE = subset(allAccts,allAccts$currency == crypto2close)$available %>% as.numeric
    SZE_LMT = subset(sizeLMT, sizeLMT$id == paste0(crypto2close,"-USD"))[,"base_min_size"] %>% as.numeric
    # close position if current size is greater than or equal to size limit
    if(cur_SZE >= SZE_LMT)
    {
    # send order to close position
    msg <- rgdax::add_order(api.key=KEYS$apikey, secret=KEYS$apiSecret, passphrase=KEYS$PASS, 
                     product_id = paste0(crypto2close,"-USD"),type = "limit", stop = NULL, 
                     stop_price = NULL, side = "s",price = cur_PRC, size=cur_SZE)
    cat(paste0("\n",msg))
    }
    
    # IF amount to invest is greater than available then we can't place orders
    # adjust the fiat2invest
    }
    if(SZE_LMT < signif(fiat2Invest/cur_PRC,3))
    {
      cat(paste0("\nNeed to increment Monies, \nbase size higher than trade size: ", signif(fiat2Invest/cur_PRC,3)))
    }
    
    if(fiat2Invest < monies)
    {
     cat(paste0("\nNot Enough Cash in your Account!\nCurrent Balance: ", monies))
    }
  }
  # if we have the same asset to trade check if we have open Positions
  if(coredata(SEQ$SEQ[1]) == coredata(SEQ$SEQ[2]))
  {
    crypto2open <- names(SEQ[,coredata(SEQ$SEQ[2])])
    # check latest trades for that asset
    FILLS <- rgdax::fills(api.key=KEYS$apikey, secret=KEYS$apiSecret, 
                    passphrase=KEYS$PASS, product_id = paste0(crypto2open,"-USD"))
    # get size limit for crypto
    SZE_LMT = subset(sizeLMT, sizeLMT$id == paste0(crypto2open,"-USD"))[,"base_min_size"] %>% as.numeric
    # get current position
    cur_POS = subset(allAccts, allAccts$currency == crypto2open)[,"balance"] %>% as.numeric
    # OPEN POSITION if it hasn't already
    if(nrow(FILLS)== 0 | (cur_POS < SZE_LMT))
    {
      # get current ASK price 2 open position
      cur_PRC = get_best_bidAsk(KEYS,COIN=paste0(crypto2open))$ask.Price %>% as.numeric
      # send order to open position
      msg <- rgdax::add_order(api.key=KEYS$apikey, secret=KEYS$apiSecret, passphrase=KEYS$PASS, 
                       product_id = paste0(crypto2open,"-USD"),type = "limit", stop = NULL, 
                       stop_price = NULL, side = "b",price = cur_PRC, size=signif(fiat2Invest/cur_PRC,3))
      cat(paste0("\n",msg))
    }
    
    if(cur_POS >= SZE_LMT)
    {
      # don't open position
      cat("\nPosition already exists!\n")
    }
    
    
  }
  
}
#*************************************************************************************
#                          # Helper Functions
#*************************************************************************************
## gets best bid/ask
get_best_bidAsk = function(KEYS,COIN)
{
  # BUILD COINBASE PRO URL
  url <- paste0("https://api.pro.coinbase.com/products/",COIN,"-USD/book")
  
  # GET REQUEST
  exp <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Key" = KEYS$apikey,
                         "Passphrase"= KEYS$PASS,
                         "Secret" = KEYS$apiSecret))
  # format request
  dta <- fromJSON(url,simplifyVector = TRUE)
  
  # get bid/ask
  bid = dta$bids %>% as.data.frame
  colnames(bid) <- c("bid.Price","bid.Size","bid.numOrders")
  bid <- bid[,c("bid.numOrders","bid.Size","bid.Price")]
  ask = dta$asks %>% as.data.frame
  colnames(ask) <- c("ask.Price","ask.Size","ask.numOrders")
  bidask = cbind(bid,ask)
  # change to numeric columns
  bidask$bid.numOrders <- as.numeric(bidask$bid.numOrders)
  bidask$bid.Size <- as.numeric(bidask$bid.Size)
  bidask$bid.Price <- as.numeric(bidask$bid.Price)
  bidask$ask.numOrders <- as.numeric(bidask$ask.numOrders)
  bidask$ask.Size <- as.numeric(bidask$ask.Size)
  bidask$ask.Price <- as.numeric(bidask$ask.Price)
  # Spread
  bidask$Spread = bidask$ask.Price - bidask$bid.Price
  # return data
  bidask
}
#bAsk = get_best_bidAsk(KEYS,COIN="ETH")
## Create sequence of times
getTMZ = function(TF)
{
  # Start/End Times
  START <- as.POSIXct(round_date(Sys.time(), paste0(TF," minutes")))
  START  <- START #- hours(tmDIFF)   
  END <- START + hours(24)
  # the following line will determine the start times for the algo
  tmz <- seq(START,END, by=paste0("",TF," min"))
  tmz
}
### SLEEP UNTIL MARKET OPENS 
SLEEEP = function(xx){
  ttt <- tmz[xx] - (Sys.time())
  HMS <- attr(ttt,"units")
  tt <- as.numeric(ttt)
  if(HMS == "hours")
  {
    print(paste0("Will now sleep for: ",tt , " hours"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt*60*60)
  }
  if(HMS == "mins")
  {
    print(paste0("Will now sleep for: ",tt , " minutes"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt*60)
  }
  if(HMS == "secs")
  {
    print(paste0("Will now sleep for: ",tt , " seconds"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt)
  }  
}


