### run code in parallel



cat("\014")  # clear console
rm(list=ls(all=TRUE))

set.seed( 17 )

library( parallel )

detectCores()


# library( quantreg )
# data( engel )
# 
# plot( foodexp ~ income, data = engel )
# 
# fitl = rq( foodexp ~ income, tau = 0.5, data = engel ); fitl$coefficients
# abline( fitl )
# 
# library( foreach )
# library( doParallel )
# cl = makeCluster(2)
# registerDoParallel(cl)  # 
# #res = foreach(i = 1:10, .packages = "quantreg" ) %dopar%{
# res = foreach(i = 1:10, .combine = "rbind", .packages = "quantreg" ) %dopar%{
#   
#   boot_dat = engel[ sample( 1:nrow( engel ), replace = T ), ]
#   fitl = rq( foodexp ~ income, tau = 0.5, data = boot_dat ); fitl$coefficients
#   
# }
# stopCluster(cl)
# 
# resdf = as.data.frame( res )
# quantile( resdf$income, probs = c(0.025, 0.975), type = 1 )

library( bayarx   )
library( bayarxsv )
library( readxl   )
library( magrittr )
library( tfplot   )
library( lubridate )

qoq     <- function( x ) ( ( x /stats::lag( x, k = -1 ) )^4 - 1 ) * 100
doArima <- function( tsy, p ) {
  ### try to estimate ARIMA( p,0,0 ): in case of nonstationarity => ARIMA( p-1,1,0 )
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      armod    <- arima(tsy, order=c( p, 0, 0 ) ); armod
      y_sigma2 <- armod$sigma2
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      # Choose a return value in case of error
      out01 <- tryCatch(
        {
          armod    <- arima(tsy, order=c( p - 1, 1, 0 ) ); armod
          armod$sigma2          
        },
        error=function(cond) {
          
          armod    <- arima(tsy, order=c( p - 2, 2, 0 ) ); armod
          armod$sigma2          
          return(armod$sigma2)
        },
        warning=function(cond) {
          # Choose a return value in case of warning
          return(NULL)
        },
        finally={
          
        }
      ) 
      
      return(out01)
    },
    warning=function(cond) {
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message("Some other message at the end")
    }
  )    
  return(out)
}  

path = list()
path$main_D = "c:\\Users\\BBB\\ARBEIT\\NCST-LV-PSEUDO\\"  # "D:\\LNB\\NCST-LV-PSEUDO\\"         # 
path$main_K = "c:\\Users\\BBB\\ARBEIT\\NCST-LV-PSEUDO\\"  # "K:\\NotesLNB\\NCST-LV-PSEUDO\\"    #  

path$data = paste0( path$main_D, "DATA\\")
path$vntg = paste0( path$data  , "NCST-VNTG\\")
path$code = paste0( path$main_K, "CODE\\")
path$plot = paste0( path$main_D, "PLOT\\")
path$resu = paste0( path$main_D, "RESU\\")


### outturns to compute forecast accuracy
# taken from Q:\CSP_MPP_DATI\IKP\IKP_IZL\2018\IEN_3
gdp_lv = paste0( path$data, "IKP_IZL_201710_201712.xlsx" )
db_xlsx = read_excel( gdp_lv, sheet = "0102Q_chain_season.adj." ); head( db_xlsx )

### determine rows with dates in the first column-Q
date_indx = grep( "^[0-9]{4}-Q[1-4]{1}$", db_xlsx[[1]] )
date_beg  = strsplit( db_xlsx[[1]][ date_indx[1] ], "-Q", fixed = T ) %>% unlist %>% as.numeric; date_beg

tsy_lvl = ts( db_xlsx[["X__1"]][ date_indx ] %>% as.numeric, start = date_beg, frequency = 4 )

tsy = qoq( tsy_lvl ); tsy
tfplot( tsy, lwd = 2 ); abline( h = 0 )

opt <- list()
opt$SV_switch    = "SV-ON" # SV_switch; # "SV-ON" # 
opt$model_choice = "AR0+incpt" # model_choice; # "AR2+incpt+survindu" # "AR2+incpt" #  
opt$fcst_orgn    = "fo4"
opt$fcst_trgt    = list( beg = as.Date("2004-03-01"), end = as.Date("2017-12-01") )
opt$lmbd_set     = "diffuse" # lmbd_set # "as_in_CCM2015" # 
opt$sy           = "y"


opt_mcmc             <- list()
opt_mcmc$draws       <- 50000
opt_mcmc$burnin      <- 10000
opt_mcmc$thinpara    <- 10
opt_mcmc$thinlatent  <- 10
opt_mcmc$priorphi_a0 <- 20
opt_mcmc$priorphi_b0 <- 1.5

###options for Bayesian regression: see p. 845
opt_prior <- list()
if( opt$lmbd_set == "diffuse")      { opt_prior$lmbd <- c( 1000000, 1000000, 1 ) }  # shrinkage hyperparameters
if( opt$lmbd_set == "as_in_CCM2015"){ opt_prior$lmbd <- c(     0.2,     0.2, 1 ) }  # shrinkage hyperparameters


###priors for volatility
opt_prior$variSigma0   = 4
opt_prior$muPhi        = 0.035; #0.035
opt_prior$priordfPhi   = 5
opt_prior$arForPriorPi = 4;  # AR(4) for prior on slope coefficients


### collect all options
opt$mcmc  = opt_mcmc
opt$prior = opt_prior


j_fo = opt$fcst_orgn    = "fo4"
if( j_fo == "fo1" | j_fo == "fo2" ) horz = 1  
if( j_fo == "fo3" | j_fo == "fo4" ) horz = 0


#cat("\nhorz ", horz )
if( grepl( "^AR0\\+incpt$", opt$model_choice ) ){
  
  tsx_name = "incpt"
  
}else{
  
  tsx_name = colnames( tsx_estn )
  
}
tsx_name

tsy_estn = tsy
tsx_estn = ts( 1, start = start( tsy ),end = end( tsy ), frequency = frequency( tsy) )
tsx_fcst = 1

### specify priors
nylag    <- length( grep( paste0(opt$sy,"_L"), tsx_name ) ); nylag
ncoef    <- ifelse( grepl( "^AR0", opt$model_choice ),       1, dim( tsx_estn )[2] ); ncoef # number of coefficients in the model



priorMean <- matrix( rep( 0, ncoef ), ncol = 1 )
priorVari <- diag( ncoef ) * 0.01

lmbd = opt_prior$lmbd; lmbd

if( nylag > 0 ) for( l in 1:nylag) priorVari[ l+1, l+1] <- ( lmbd[1] / ( l + horz )^(lmbd[3]) )^2

### build a prior: adjust prior for intercept ( the only scale-dependent coefficient in the AR model )
### ols estimation
y_sigma2 = doArima( tsy_estn, opt_prior$arForPriorPi )
priorVari[1,1] <- 1000^2 * y_sigma2; priorVari #very loose prior on incpt 

indx <- 1 + nylag; indx
### other variables
if( indx < ncoef ){
  for( l in (indx + 1) : ncoef ){ # l = 4
    
    sx <- tsx_name[ l ]; sx 
    # armod <- arima( tsx_estn[, sx ], order=c( 4, 0, 0 ) ); #cat('\n', sx ); print( armod ); 
    # x_sigma2 <- armod$sigma2
    x_sigma2 = doArima( tsx_estn[, sx ], 4 )
    
    ###determine whether it is a lagged variable or not
    if( length( grep( "_L " , sx) ) == 0 ) dLag = 0
    if( length( grep( "_L1$", sx) ) >  0 ) dLag = 1
    if( length( grep( "_L2$", sx) ) >  0 ) dLag = 2
    
    priorVari[ l, l ] = ( y_sigma2 / x_sigma2 ) * ( lmbd[2] * lmbd[1] / ( 1 + dLag + horz ) ^ (lmbd[3]) )^2
    
  }
}
rownames(priorVari) <- tsx_name; priorVari

# diag( priorVari )

vy <- matrix( tsy_estn, ncol = 1)
mx <- matrix( tsx_estn, nrow = nrow(vy) ); # cbind( vy, mx)

if( opt$SV_switch == "SV-OFF" ){
  
  draws <- bayarx( opt_mcmc$draws, opt_mcmc$burnin, opt_mcmc$thinpara, priorMean, priorVari, vy, mx )$mstore
 
  
  library( foreach )
  library( doParallel )
  cl = makeCluster(2)
  registerDoParallel(cl)  # .combine = "rbind"
  res = foreach(i = 1:5, .packages = "bayarx" ) %dopar%{
    
    
    draws <- bayarx( 100, opt_mcmc$burnin, opt_mcmc$thinpara, priorMean, priorVari, vy + i*100, mx )$mstore
    
  }
  stopCluster(cl)
  res  
   
} 

if( opt$SV_switch == "SV-ON" ){
  
  ###priors for volatility
  variSigma0  = opt_prior$variSigma0
  muPhi       = opt_prior$muPhi ; #0.035
  priordfPhi  = opt_prior$priordfPhi
  
  #use pre-sample to initialise period 0 volatiliy: here use the estimation sample
  armodpre <- arima( tsy, order=c( opt_prior$arForPriorPi, 0, 0 ) ); armodpre
  meanSigma0 = log( armodpre$sigma2 ); meanSigma0
  
  ptm <- proc.time()
  
  listMCMC <- bayarxsv( opt_mcmc$draws, opt_mcmc$burnin, opt_mcmc$thinpara, priorMean, priorVari, meanSigma0, variSigma0, muPhi, priordfPhi, vy, mx )
  str( listMCMC )
  proc.time() - ptm
  
  # str( listMCMC )
  
  mPi           <- listMCMC[["mPi"]]
  mLogSigma     <- listMCMC[["mLogSigma"]]
  vPhi          <- listMCMC[["vPhi"]]
  vLogSigmaPred <- listMCMC[["vLogSigmaPred"]]
  
  summary( vLogSigmaPred )
  summary( mLogSigma )  
  
  
  startSigma <- as.Date( sprintf("%i-%i-01",start( tsy )[1],start( tsy )[2] * 3 ) ) %m-% months(3); startSigma
  
  ###draw stochastic volatility
  sigmaQ50 <- ts( apply( exp( mLogSigma / 2 ), 2, median                 ), start = c( year(startSigma), quarter(startSigma) ), frequency = 4 )
  sigmaQ95 <- ts( apply( exp( mLogSigma / 2 ), 2, quantile, probs = 0.95 ), start = c( year(startSigma), quarter(startSigma) ), frequency = 4 )
  sigmaQ05 <- ts( apply( exp( mLogSigma / 2 ), 2, quantile, probs = 0.05 ), start = c( year(startSigma), quarter(startSigma) ), frequency = 4 )
  
  #  pdf( file = paste0(path$plot, "SVSAMPLE", sfile_ud_vint,".pdf"  ) )
  plot( sigmaQ50, col = "red", type = "l", lwd = 2, ylim =c( 0, max(sigmaQ95) ), main = "Stochastic Volatility (sd)" )
  lines(sigmaQ95, col = "blue")
  lines(sigmaQ05, col = "blue")
  
  
  
  ## one-step ahead log-volatility h_{T+1}
  predvol <- vLogSigmaPred; head( predvol )
  
  ## one-step ahead posterior predictive distribution
  preddraws <- rnorm( length(predvol), mPi %*% matrix( tsx_fcst, ncol = 1 ), exp( predvol / 2 ) ); head( preddraws )
  tfplot( )
  
  
  library( foreach )
  library( doParallel )
  
  ptm <- proc.time()
  cl = makeCluster(2)
  registerDoParallel(cl)  # .combine = "rbind" %do%
  res = foreach(i = 1:5, .packages = "bayarxsv" ) %dopar%{
    
    listMCMC <- bayarxsv( opt_mcmc$draws, opt_mcmc$burnin, opt_mcmc$thinpara, priorMean, priorVari, meanSigma0, variSigma0, muPhi, priordfPhi, vy, mx )
    
  }
  stopCluster(cl)
  proc.time() - ptm
  
  str( res )
  
} 


# setwd( "c:\\Users\\BBB\\ARBEIT\\BUILDNEWPACKAGE\\" )
# RcppArmadillo.package.skeleton( "bayarx" )
# setwd( "c:\\Users\\BBB\\ARBEIT\\BUILDNEWPACKAGE\\bayarx" )
# compileAttributes( verbose = T )

setwd( "c:\\Users\\BBB\\ARBEIT\\BUILDbayarxsv\\" )
RcppArmadillo.package.skeleton( "bayarxsv" )
setwd( "c:\\Users\\BBB\\ARBEIT\\BUILDbayarxsv\\bayarxsv" )
compileAttributes( verbose = T )

### in command prompt change to WD
### type Rcmd build bayarxsv
### ( in original paper KimMartinMcMurry "Instruction for creating your own R package"):
### type Rcmd build --binary bayarxsv
