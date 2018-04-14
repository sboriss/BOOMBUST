### run ncst with BMFM
### use own functions bayarx and bayarxsv
### parallel computation over fcst target 
### add vintages for indicators with lagged variables
### store forecasting results in PSQL

library( zoo        )
library( lubridate  ) 
library( tfplot     )
library( fanplot    ) 
library( XML        )
library( methods    )
library( xml2       )
library( readxl     )
library( magrittr   )
library( Rcpp       )
library( coda       )
library( sm         )
library( rlist      )
library( coda       )
library( scoringRules )
library( bayarx     )
library( bayarxsv   )
library( parallel   )
library( foreach    )
library( doParallel )
library( seasonal   )  
library( stringr    )
library( RPostgreSQL )

detach("package:dplyr", unload=TRUE)

cat("\014")  # clear console
rm(list=ls(all=TRUE))

diq     <- function( x ) diff( x, k = 1 )
diy     <- function( x ) diff( x, k = 4 )
qoq     <- function( x ) ( ( x /stats::lag( x, k = -1 ) )^4 - 1 ) * 100
yoy     <- function( x ) ( ( x /stats::lag( x, k = -4 ) )   - 1 ) * 100
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
          
          # armod    <- arima(tsy, order=c( p - 2, 2, 0 ) ); armod
          # armod$sigma2    
          
          return( var( tsy ) )
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
fnGetDateSQL <- function( x ){
  ### create dates in SQL(hstore) format  
  # input: a ts object x
  period <- "month"
  beg.x  <- start(x)
  if(frequency(x) == 4){ period <- "3 months"; beg.x[2] <- beg.x[2]*3;} #set to the last month in quarter
  return(seq(as.Date(sprintf("%i-%i-1", beg.x[1], beg.x[2])), by=period, length.out=length(x)))
}
fnGetHstore  <- function( x ){
  ### create hstore object for SQL  
  # input: a ts object x
  dates  <- fnGetDateSQL(x); dates
  hstore <- paste(dates, x, sep="=>", collapse=",")
  hstore <- gsub("NA","NULL",hstore); 
  return(hstore)
}
ts2hstore    <- function( x ){
  
  dates    <- fnGetDateSQL(x); dates
  x_hstore <- paste(dates, x, sep="=>", collapse=","); x_hstore
  x_hstore <- gsub("NA","NULL",x_hstore)  
  x_hstore
}


opt <- list()
opt$fcst_orgn      = c( "fo1","fo2","fo3","fo4" )
opt$fcst_trgt      = list( beg = as.Date("2004-03-01"), end = as.Date("2017-12-01") )
opt$sy             = "y";
opt$created        = "2018-04-13"
opt$fcst_eval_smpl = list( fullsample  = list( beg = c(2004,1), end = c(2017,4) ),
                           pre_crisis  = list( beg = c(2004,1), end = c(2007,3) ),
                           crisis      = list( beg = c(2007,4), end = c(2010,3) ),
                           post_crisis = list( beg = c(2010,4), end = c(2017,4) ) )
opt$metric         = c( "rmsfe", "mlogs", "mcrps" )

### prepare PSQL: ncstlvpseudo
if( FALSE ){
  
  ### name of the table to store forecasting results:
  psql_table2store_fcst = "ncstlvpseudo.resu"
  
  split_txt = strsplit( psql_table2store_fcst, ".", fixed = T) %>% unlist; split_txt
  dbExistsTable = dbGetQuery(conn, sprintf("SELECT EXISTS ( SELECT 1 FROM pg_tables WHERE schemaname = '%s' AND tablename = '%s' );", split_txt[1],  split_txt[2]) )
  dbExistsTable
  ### create a table if it does not exist
  if( !dbExistsTable ){
    
    sql_query = paste0( "CREATE TABLE ", psql_table2store_fcst,
                        "( created date not null, model varchar(256) not null, fcstorig varchar(3) not null, stochvol varchar(6) not null, lambdaset varchar(7) not null, 
                        otrn     hstore not null,
                        mean     hstore not null,
                        erro_q50 hstore not null,
                        logs     hstore not null,
                        crps     hstore not null,
                        q0p5pp   hstore not null,
                        q2p5pp   hstore not null,
                        q5pp     hstore not null,
                        q16p5pp  hstore not null, 
                        q25pp    hstore not null,
                        q33p3pp  hstore not null,
                        q50pp    hstore not null,
                        q66p6pp  hstore not null,
                        q75pp    hstore not null,
                        q83p5pp  hstore not null,
                        q95pp    hstore not null,
                        q97p5pp  hstore not null,
                        q99p95pp hstore not null, 
                        rmsfe    hstore not null,
                        mlogs    hstore not null,
                        mcrps    hstore not null, PRIMARY KEY (created, model, fcstorig, stochvol, lambdaset ) );" )
    
    gsub( "\n","", sql_query )
    # dbGetQuery(conn, sql_query)
    
  }
  
}


path = list()
path$main_D = "D:\\LNB\\NCST-LV-PSEUDO\\"         # "c:\\Users\\BBB\\ARBEIT\\NCST-LV-PSEUDO\\" # 
path$main_K = "K:\\NotesLNB\\NCST-LV-PSEUDO\\"    # "c:\\Users\\BBB\\ARBEIT\\NCST-LV-PSEUDO\\" #

path$data = paste0( path$main_D, "DATA\\")
path$vntg = paste0( path$data  , "NCST-VNTG\\")
path$code = paste0( path$main_K, "CODE\\")
path$plot = paste0( path$main_D, "PLOT\\")
path$resu = paste0( path$main_D, "RESU\\")
path$summ = paste0( path$main_D, "RESU_SUMM\\")

do_vint <- list()
do_vint$GDP = FALSE # TRUE # 
do_vint$TSX = FALSE # TRUE

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

### load and join indicators
if( FALSE ){ 
  
  dbx_all = read.csv( paste0( path$data, "mat_tsx_all.csv"   ), stringsAsFactors = F ); head( dbx_all )
  dbx_exd = read.csv( paste0( path$data, "mat_tsx_extd.csv"  ), stringsAsFactors = F ); head( dbx_exd )
  dbx_mon = read.csv( paste0( path$data, "mat_tsx_monagg.csv"), stringsAsFactors = F ); head( dbx_mon )
  
  names_all = colnames( dbx_all )[ -c(1,2)]; names_all # remove year,period columns
  names_exd = colnames( dbx_exd )[ -c(1,2)]; names_exd
  names_mon = colnames( dbx_mon )[ -c(1,2)]; names_mon
  
  tsdbx_all = ts( dbx_all[, setdiff( names_all, names_exd ) ], start = c( dbx_all$year[1], dbx_all$period[1] ), frequency = 12 )
  tsdbx_exd = ts( dbx_exd[,                     names_exd   ], start = c( dbx_exd$year[1], dbx_exd$period[1] ), frequency = 12 )
  tsdbx_mon = ts( dbx_mon[,                     names_mon   ], start = c( dbx_mon$year[1], dbx_mon$period[1] ), frequency = 12 )
  
  tsx_join = cbind( tsdbx_all, tsdbx_exd, tsdbx_mon )
  colnames( tsx_join ) = gsub( "tsdbx_all.","",colnames( tsx_join ), fixed = T )
  colnames( tsx_join ) = gsub( "tsdbx_exd.","",colnames( tsx_join ), fixed = T )
  colnames( tsx_join ) = gsub( "tsdbx_mon.","",colnames( tsx_join ), fixed = T )
  colnames( tsx_join )
  tsWrite( tsx_join, paste0( path$data, "mat_tsx_join.csv") )
  
}
### do seasonal adjustment
if( FALSE ){
  
  dbx = read.csv( paste0( path$data, "mat_tsx_join.csv"   ), stringsAsFactors = F ); head( dbx )
  colnames( dbx )
  
  # ### correct for outlier in cit in 2010M6: -103378.8 => take average from May and July
  # cit0 = dbx$LV_BUDGET.CIT; cit = cit0
  # indx_outl = which( cit0 < 0 )
  # cit[ indx_outl ] = mean( c( cit0[ indx_outl-1 ], cit0[ indx_outl+1 ] ) ) 
  # dbx$LV_BUDGET.CIT = cit
  
  ### do seasonal adjustment "LV_BUDGET.INCOME" -- TOO SHORT
  name_x2sa = c( "LV_HICP_xEF", "LV_VACANCIES", "LV_UPERS", "LV_EXPTOT", 
                 "LV_PORTCARGO_TOTAL", "LV_BUDGET.PIT", "LV_BUDGET.CIT", "LV_BUDGET.VAT")
  
  # collect data 
  list_tsx2sa = lapply( name_x2sa, function( sx ) ts(dbx[ , sx ], start = c( dbx$year[1], dbx$period[1] ), frequency = 12 ) )
  names( list_tsx2sa ) = name_x2sa 
  
  # loop over dta
  ll <- lapply(list_tsx2sa, function(e) try(seas(e, x11 = "")))
  
  # list failing models
  is.err <- sapply(ll, class) == "try-error"
  ll[is.err]
  
  # return final series of successful evaluations
  mat_tsx2sa_final = do.call( cbind, lapply(ll[!is.err], final) )
  colnames( mat_tsx2sa_final ) = paste0( name_x2sa, "_SA" )
  
  tsdbx = ts( dbx, start = c( dbx$year[1], dbx$period[1] ), frequency = 12)
  tsdbx_sa = cbind( tsdbx, mat_tsx2sa_final  )
  colnames(tsdbx_sa) = c( colnames(tsdbx), colnames( mat_tsx2sa_final ) )
  
  tsWrite( tsdbx_sa, paste0( path$data, "mat_tsx_join_sa.csv" ) )
  
}
dbx = read.csv( paste0( path$data, "mat_tsx_join_sa.csv"   ), stringsAsFactors = F ); head( dbx )
colnames( dbx )

if( do_vint$GDP ){
  
  list_trgt_date = seq( as.Date("2004-03-01"), as.Date( "2018-03-01" ), by = "3 months")
  
  ### create vintages: GDP 
  sapply( list_trgt_date, function( j_trgt_date ){  # j_trgt_date = as.Date( "2017-12-01" )
    
    ### prepare pseudo-vintages
    opt_time <- list()
    opt_time$trgt_date = j_trgt_date
    opt_time$trgt_q    = c( year(opt_time$trgt), quarter(opt_time$trgt) )
    opt_time$fcst_orgn = seq( opt_time$trgt_date + months( -2 ), opt_time$trgt_date + months( 1 ), by = "months" )
    names( opt_time$fcst_orgn ) = c( "fo1", "fo2", "fo3", "fo4" )
    opt_time
    
    ### create vintage
    sapply( names( opt_time$fcst_orgn), function(j_fo){# j_fo = "fo4"; 
      
      j_fo_date = opt_time$fcst_orgn[ j_fo ]; j_fo_date
      
      ### NB! timing of GDP releases ignores flash estimates available after one month
      ###     use only releases after two months of the end of the target quarter
      if( j_fo == "fo1" | j_fo == "fo2" ) fcst_hrzn = 2
      if( j_fo == "fo3" | j_fo == "fo4" ) fcst_hrzn = 1
      
      tsy_vntg_end = j_fo_date - months( 3 + fcst_hrzn ); tsy_vntg_end
      
      tsy_vntg = window( tsy, end = c( year(tsy_vntg_end), quarter(tsy_vntg_end) ) )
      
      
      mtsy = cbind( tsy_vntg, lag( tsy_vntg, k = -fcst_hrzn), lag( tsy_vntg, k = -fcst_hrzn-1) )
      colnames( mtsy ) = c( "y", paste0( "y_L", seq( fcst_hrzn, fcst_hrzn + 1) ) )
      
      tsWrite( mtsy, paste0(path$vntg, "tsy_",j_fo_date,"_",j_fo,".csv") )  
      
    }) 
    
  })
  
}

### create vintages of indicators:
if( do_vint$TSX){ 
  
  info_survindu_lvl  = c( sx = "INDU.LV.TOT.COF.BS.M", sxs = "survindu_lvl", publag = "1", transf = "m2q_lvl" )
  info_survindu_diq  = c( sx = "INDU.LV.TOT.COF.BS.M", sxs = "survindu_diq", publag = "1", transf = "m2q_diq" )
  info_survindu_diy  = c( sx = "INDU.LV.TOT.COF.BS.M", sxs = "survindu_diy", publag = "1", transf = "m2q_diy" )
  info_survbuid_lvl  = c( sx = "BUIL.LV.TOT.COF.BS.M", sxs = "survbuid_lvl", publag = "1", transf = "m2q_lvl" )
  info_survbuid_diq  = c( sx = "BUIL.LV.TOT.COF.BS.M", sxs = "survbuid_diq", publag = "1", transf = "m2q_diq" )
  info_survbuid_diy  = c( sx = "BUIL.LV.TOT.COF.BS.M", sxs = "survbuid_diy", publag = "1", transf = "m2q_diy" )
  info_survreta_lvl  = c( sx = "RETA.LV.TOT.COF.BS.M", sxs = "survreta_lvl", publag = "1", transf = "m2q_lvl" )
  info_survreta_diq  = c( sx = "RETA.LV.TOT.COF.BS.M", sxs = "survreta_diq", publag = "1", transf = "m2q_diq" )
  info_survreta_diy  = c( sx = "RETA.LV.TOT.COF.BS.M", sxs = "survreta_diy", publag = "1", transf = "m2q_diy" )
  info_retail_yoy    = c( sx = "LV_RETAIL"           , sxs = "retail_yoy"  , publag = "2", transf = "m2q_yoy" )    
  info_retail_qoq    = c( sx = "LV_RETAIL"           , sxs = "retail_qoq"  , publag = "2", transf = "m2q_qoq" )    
  info_incpt         = c( sx = "INCPT"               , sxs = "incpt"       , publag = "0", transf = "m2q_lvl" )
  info_vacancy_lvl   = c( sx = "LV_VACANCIES_SA"     , sxs = "vacancy_lvl" , publag = "1", transf = "m2q_lvl" )
  info_vacancy_qoq   = c( sx = "LV_VACANCIES_SA"     , sxs = "vacancy_qoq" , publag = "1", transf = "m2q_qoq" )
  info_vacancy_yoy   = c( sx = "LV_VACANCIES"        , sxs = "vacancy_yoy" , publag = "1", transf = "m2q_yoy" )
  info_upers_yoy     = c( sx = "LV_UPERS"            , sxs = "upers_yoy"   , publag = "1", transf = "m2q_yoy" )
  info_upers_qoq     = c( sx = "LV_UPERS_SA"         , sxs = "upers_qoq"   , publag = "1", transf = "m2q_qoq" )
  info_upers_lvl     = c( sx = "LV_UPERS_SA"         , sxs = "upers_lvl"   , publag = "1", transf = "m2q_lvl" )
  info_m3_yoy        = c( sx = "LV_M3_HIST"          , sxs = "m3_yoy"      , publag = "1", transf = "m2q_yoy" )
  info_m3_qoq        = c( sx = "LV_M3_HIST"          , sxs = "m3_qoq"      , publag = "1", transf = "m2q_qoq" )
  info_iip_qoq       = c( sx = "PROD_C_SCA_I15_LV"   , sxs = "iip_qoq"     , publag = "2", transf = "m2q_qoq" )   
  info_iip_yoy       = c( sx = "PROD_C_SCA_I15_LV"   , sxs = "iip_yoy"     , publag = "2", transf = "m2q_yoy" )   
  info_omxr_yoy      = c( sx = "LV_OMXR"             , sxs = "omxr_yoy"    , publag = "1", transf = "m2q_yoy" )
  info_omxr_qoq      = c( sx = "LV_OMXR"             , sxs = "omxr_qoq"    , publag = "1", transf = "m2q_qoq" )
  info_expt_yoy      = c( sx = "LV_EXPTOT"           , sxs = "expt_yoy"    , publag = "2", transf = "m2q_yoy" )
  info_expt_qoq      = c( sx = "LV_EXPTOT_SA"        , sxs = "expt_qoq"    , publag = "2", transf = "m2q_qoq" )
  info_portcargo_qoq = c( sx = "LV_PORTCARGO_TOTAL_SA", sxs = "portcargo_qoq", publag = "1", transf = "m2q_qoq" )
  info_portcargo_yoy = c( sx = "LV_PORTCARGO_TOTAL"   , sxs = "portcargo_yoy", publag = "1", transf = "m2q_yoy" )
  info_hicp_qoq      = c( sx = "LV_HICP_xEF_SA"       , sxs = "hicp_qoq"     , publag = "1", transf = "m2q_qoq" )
  info_hicp_yoy      = c( sx = "LV_HICP_xEF"          , sxs = "hicp_yoy"     , publag = "1", transf = "m2q_yoy" )
  info_vat_qoq      = c( sx = "LV_BUDGET.VAT_SA"      , sxs = "vat_qoq"      , publag = "1", transf = "m2q_qoq" )
  info_vat_yoy      = c( sx = "LV_BUDGET.VAT"        , sxs = "vat_yoy"      , publag = "1", transf = "m2q_yoy" )
  info_pit_qoq      = c( sx = "LV_BUDGET.PIT_SA"      , sxs = "pit_qoq"     , publag = "1", transf = "m2q_qoq" )
  info_pit_yoy      = c( sx = "LV_BUDGET.PIT"         , sxs = "pit_yoy"     , publag = "1", transf = "m2q_yoy" )
  # info_cit_qoq      = c( sx = "LV_BUDGET.CIT_SA"      , sxs = "cit_qoq"     , publag = "1", transf = "m2q_qoq" )
  # info_cit_yoy      = c( sx = "LV_BUDGET.CIT"         , sxs = "cit_yoy"     , publag = "1", transf = "m2q_yoy" )
  info_irate3m_lvl      = c( sx = "EURIBOR3MD_"       , sxs = "irate3m_lvl" , publag = "1", transf = "m2q_lvl" )
  info_irate3m_qoq      = c( sx = "EURIBOR3MD_"       , sxs = "irate3m_qoq" , publag = "1", transf = "m2q_diq" )
  info_irate3m_yoy      = c( sx = "EURIBOR3MD_"       , sxs = "irate3m_yoy" , publag = "1", transf = "m2q_diy" )
  info_loan2nfc_qoq      = c( sx = "LV_LOAN_NFC"      , sxs = "loan2nfc_qoq", publag = "1", transf = "m2q_qoq" )
  info_loan2nfc_yoy      = c( sx = "LV_LOAN_NFC"      , sxs = "loan2nfc_yoy", publag = "1", transf = "m2q_yoy" )
  info_loan2hh_qoq      = c( sx = "LV_LOAN_HH"        ,sxs = "loan2hh_qoq"  , publag = "1", transf = "m2q_qoq" )
  info_loan2hh_yoy      = c( sx = "LV_LOAN_HH"        ,sxs = "loan2hh_yoy"  , publag = "1", transf = "m2q_yoy" )
  
  list_info_tsx = list( 
    survindu_lvl  = info_survindu_lvl,
    survindu_diq  = info_survindu_diq,
    survindu_diy  = info_survindu_diy,
    survbuid_lvl  = info_survbuid_lvl,
    survbuid_diq  = info_survbuid_diq,
    survbuid_diy  = info_survbuid_diy,
    survreta_lvl  = info_survreta_lvl,
    survreta_diq  = info_survreta_diq,
    survreta_diy  = info_survreta_diy,
    retail_yoy    = info_retail_yoy,   
    retail_qoq    = info_retail_qoq,    
    incpt         = info_incpt,
    vacancy_lvl   = info_vacancy_lvl,
    vacancy_qoq   = info_vacancy_qoq,
    vacancy_yoy   = info_vacancy_yoy,
    upers_yoy     = info_upers_yoy,
    upers_qoq     = info_upers_qoq,
    upers_lvl     = info_upers_lvl,
    m3_yoy        = info_m3_yoy,
    m3_qoq        = info_m3_qoq,
    iip_qoq       = info_iip_qoq,   
    iip_yoy       = info_iip_yoy,
    omxr_yoy      = info_omxr_yoy,
    omxr_qoq      = info_omxr_qoq,
    expt_yoy      = info_expt_yoy,
    expt_qoq      = info_expt_qoq,
    portcargo_qoq = info_portcargo_qoq,
    portcargo_yoy = info_portcargo_yoy,
    hicp_qoq      = info_hicp_qoq,
    hicp_yoy      = info_hicp_yoy,
    vat_qoq       = info_vat_qoq,
    vat_yoy       = info_vat_yoy,
    pit_qoq       = info_pit_qoq,
    pit_yoy       = info_pit_yoy,
    # cit_qoq       = info_cit_qoq,
    # cit_yoy       = info_cit_yoy,
    irate3m_lvl   = info_irate3m_lvl,
    irate3m_qoq   = info_irate3m_qoq,
    irate3m_yoy   = info_irate3m_yoy,
    loan2nfc_qoq  = info_loan2nfc_qoq,
    loan2nfc_yoy  = info_loan2nfc_yoy,
    loan2hh_qoq   = info_loan2hh_qoq,
    loan2hh_yoy   = info_loan2hh_yoy)
  
  
  
  lapply( list_info_tsx, function(info_tsx){ # info_tsx = list_info_tsx[[10]]; info_tsx
    
    sx     = info_tsx[ "sx"     ] #"INDU.LV.TOT.COF.BS.M"
    publag = info_tsx[ "publag" ] # at the fcst origin
    transf = info_tsx[ "transf" ] 
    
    tsx_lvl = ts( dbx[ ,sx], start = c(dbx[ 1,"year"],dbx[ 1,"period"]), frequency = 12 )
    tfplot( tsx_lvl )
    
    
    if( grepl( "^LV_M3", sx ) ){
      
      list_fcst_orgn = seq( as.Date("2004-01-01"), as.Date( "2013-12-01" ), by = "1 months" )
      
    }else{
      
      list_fcst_orgn = seq( as.Date("2004-01-01"), as.Date( "2018-03-01" ), by = "1 months" )
      
    } 
    
    sapply(list_fcst_orgn, function(j_fcst_orgn){ # j_fcst_orgn = list_fcst_orgn[157]; j_fcst_orgn
      
      vntg_end = as.Date( j_fcst_orgn ) - months( as.numeric( publag ) ); vntg_end 
      
      tsx_lvl_vntg = window( tsx_lvl, end = c( year(vntg_end), month(vntg_end) ) )
      
      ###make sure that starts from january and end in december: otherwise aggregate does not work 
      tsx_lvl_vntg_extd = window( tsx_lvl_vntg, start = c( start(tsx_lvl_vntg)[1], 1), 
                                  end = c( end(tsx_lvl_vntg)[1], 12 ),extend = TRUE )
      
      # skip-sample to quarterly frequency
      tsx_lvl_vntg_m1  = aggregate(tsx_lvl_vntg_extd, nfrequency = 4,FUN = weighted.mean, w = c(1, 0, 0) )
      tsx_lvl_vntg_m2  = aggregate(tsx_lvl_vntg_extd, nfrequency = 4,FUN = weighted.mean, w = c(0, 1, 0) )
      tsx_lvl_vntg_m3  = aggregate(tsx_lvl_vntg_extd, nfrequency = 4,FUN = weighted.mean, w = c(0, 0, 1) )
      
      if( transf == "m2q_lvl" ){
        
        mtsx_vntg = cbind( tsx_lvl_vntg_m1, tsx_lvl_vntg_m2, tsx_lvl_vntg_m3 )
        colnames( mtsx_vntg ) = paste0( info_tsx[ "sxs" ], "_m", seq(1,3),"_lvl" )              
        
      }
      if( transf == "m2q_qoq" ){
        
        mtsx_vntg = cbind( qoq( tsx_lvl_vntg_m1), qoq( tsx_lvl_vntg_m2 ), qoq( tsx_lvl_vntg_m3 ) )
        colnames( mtsx_vntg ) = paste0( info_tsx[ "sxs" ], "_m", seq(1,3),"_qoq" )              
        tfplot( tsx_lvl_vntg_m1, tsx_lvl_vntg_m2, tsx_lvl_vntg_m3  )
        tfplot( qoq( tsx_lvl_vntg_m1), qoq( tsx_lvl_vntg_m2 ), qoq( tsx_lvl_vntg_m3 )  )
      }
      if( transf == "m2q_diq" ){
        
        mtsx_vntg = cbind( diq( tsx_lvl_vntg_m1), diq( tsx_lvl_vntg_m2 ), diq( tsx_lvl_vntg_m3 ) )
        colnames( mtsx_vntg ) = paste0( info_tsx[ "sxs" ], "_m", seq(1,3),"_diq" )              
        
      }
      if( transf == "m2q_diy" ){
        
        mtsx_vntg = cbind( diy( tsx_lvl_vntg_m1), diy( tsx_lvl_vntg_m2 ), diy( tsx_lvl_vntg_m3 ) )
        colnames( mtsx_vntg ) = paste0( info_tsx[ "sxs" ], "_m", seq(1,3),"_diy" )              
        
      }
      if( transf == "m2q_yoy" ){
        
        mtsx_vntg = cbind( yoy( tsx_lvl_vntg_m1), yoy( tsx_lvl_vntg_m2 ), yoy( tsx_lvl_vntg_m3 ) )
        colnames( mtsx_vntg ) = paste0( info_tsx[ "sxs" ], "_m", seq(1,3),"_yoy" )              
        
      }
      
      ### add lagged values
      mtsx_vntg_all = cbind( mtsx_vntg, lag( mtsx_vntg, k = -1 ) )
      colnames( mtsx_vntg_all ) = c( colnames(mtsx_vntg), paste0( colnames(mtsx_vntg), "_L1" ) )
      
      tfplot( mtsx_vntg_all[,1],mtsx_vntg_all[,2], mtsx_vntg_all[,3], title = info_tsx[ "sxs" ] );
      
      head( mtsx_vntg_all )
      
      mtsx_vntg_csv = paste0(path$vntg, "tsx_",info_tsx[ "sxs" ],"_",j_fcst_orgn,".csv")
      tsWrite( mtsx_vntg_all, mtsx_vntg_csv )
      
      if( sx == "INCPT" ){
        
        mtsx_incpt_vntg = read.csv( mtsx_vntg_csv )[,c(1,2,3)];
        colnames( mtsx_incpt_vntg ) = c( "year", "period", "incpt" )
        file.remove( mtsx_vntg_csv )
        write.csv( mtsx_incpt_vntg, mtsx_vntg_csv, row.names = FALSE )
        
      }
      
    })
    
  })
  
}

### do forecasts
if( FALSE ){  
  
  list_name_x = sapply( list_info_tsx, "[[", 2 );  attributes( list_name_x ) = NULL
  list_model_choice_tmp = c( "AR0+incpt", "AR2+incpt", paste( "AR2+incpt", list_name_x, sep = "+") )
  list_model_choice     = list_model_choice_tmp[ -which( list_model_choice_tmp == "AR2+incpt+incpt" ) ]
  
  list_model_choice     = c( list_model_choice, "AR2+incpt+vacancy_yoy+omxr_qoq",
                             "AR2+incpt+vacancy_yoy+omxr_qoq+retail_yoy",
                             "AR2+incpt+survreta_lvl+survind_lvl+survind_biud",
                             "AR2+incpt+vacancy_yoy+omxr_qoq+retail_yoy+survreta_lvl",
                             "AR2+incpt+vacancy_yoy+omxr_qoq+retail_yoy+survreta_lvl+survind_lvl",
                             "AR2+incpt+vacancy_yoy+omxr_qoq+retail_yoy+survreta_lvl+survind_lvl+survind_biud",
                             "AR2+incpt+vacancy_yoy+omxr_qoq+survreta_lvl",
                             "AR2+incpt+retail_yoy+survind_lvl" )
  
  #list_model_choice = c( "AR2+incpt+expt_yoy" ) #,"AR2+incpt+expt_qoq""AR2+incpt+vacancy_yoy+omxr_qoq","AR2+incpt+vacancy+survindu","AR2+incpt+vacancy+survbuid")
  #"AR2+incpt+iip_yoy","AR2+incpt+iip_qoq""AR2+incpt+m3_yoy","AR2+incpt+m3_qoq"# "AR2+incpt+vacancy_lvl","AR2+incpt+vacancy_yoy","AR2+incpt+vacancy_qoq"
  # c( "AR0+incpt", "AR2+incpt", "AR2+incpt+survindu", "AR2+incpt+vacancy", "AR2+incpt+upers",
  #   "AR2+incpt+survbuid", "AR2+incpt+survbuid_diq" )
  
  #    list_model_choice = c( "AR2+incpt+vacancy_yoy+omxr_qoq+survreta_lvl+retail_yoy+survind_lvl+survind_biud" ) 
  
  list_lmbd_set  = c( "diffuse" ,"ccm2015") #   
  list_SV_switch = c( "SV-OFF","SV-ON")
  
  
  ###loop over fcst horizons
  for( fo in seq_along(opt$fcst_orgn) ){ #fcst_orgn = opt$fcst_orgn[ 1 ]; fcst_orgn 
    
    fcst_orgn = opt$fcst_orgn[ fo ]  
    
    #loop over list_lmbd_set
    for( k in seq_along( list_lmbd_set ) ){
      
      lmbd_set = list_lmbd_set[ k ]; 
      
      #loop_SV_switch = lapply( list_SV_switch, function(SV_switch){
      for( sv in seq_along(list_SV_switch) ){
        
        # sv = 1;[1:2]
        SV_switch = list_SV_switch[ sv ]; SV_switch
        
        for(j_mdl in seq_along(list_model_choice) ){ # j_mdl = 1 
          
          model_choice = list_model_choice[ j_mdl ]; print( paste( fcst_orgn, lmbd_set, SV_switch, model_choice ) )
          
          fcst_orgn = "fo1"; lmbd_set = "diffuse"; SV_switch    = "SV-OFF"; model_choice = "AR2+incpt" #+retail_yoy+survindu_lvl
          
          list_fcst_trgt = seq( opt$fcst_trgt[["beg"]], opt$fcst_trgt[["end"]], by = "3 months")
          
          ### loop over fcst targets for the specific fcst origin
          list_mtsyx_fo = lapply( list_fcst_trgt, function(j_fcst_trgt_date){ # j_fcst_trgt_date = list_fcst_trgt[56]; j_fcst_trgt_date 
            
            
            j_fcst_trgt_yyqq = c( year(j_fcst_trgt_date), quarter(j_fcst_trgt_date) )      
            j_fcst_orgn_date = j_fcst_trgt_date + months( gsub( "fo","",fcst_orgn ) %>% as.integer - 3 ); j_fcst_orgn_date 
            
            y_vntg = list.files( path$vntg, pattern = paste(j_fcst_orgn_date, fcst_orgn, sep = "_" ) )
            x_vntg_all = list.files( path$vntg, pattern = paste0("^tsx.*",j_fcst_orgn_date ) ); x_vntg_all
            
            ### select indicators
            model_choice_all = strsplit( model_choice, "+", fixed = T ) %>% unlist
            model_choice_sel = sapply( model_choice_all, function( i_model ) grep( paste0(i_model,"_[0-9]{4}"), x_vntg_all ) ) %>% unlist; model_choice_sel
            x_vntg_all[ model_choice_sel ]
            
            
            list_dbyx = lapply( c( y_vntg, x_vntg_all[ model_choice_sel ] ), function(x_vntg){ # x_vntg = x_vntg_all[ model_choice_sel ][2]; x_vntg
              
              dbx_tmp = read.csv( paste0( path$vntg, x_vntg) )
              ### drop year, period columns
              dbx = ts( dbx_tmp, start = c(dbx_tmp$year[1],dbx_tmp$period[1]), frequency = 4 )[, -c(1,2) ]
              if( is.null(colnames( dbx ) ) ){
                
                if( grepl( "incpt", x_vntg ) ) dbx_name = "incpt"
                
              }else{
                
                dbx_name = colnames( dbx ) 
                
              } 
              
              list( dbx, dbx_name )
              
            }) 
            
            mtsyx_vntg = do.call( "cbind", lapply(list_dbyx, `[[`, 1) )
            colnames( mtsyx_vntg ) = lapply(list_dbyx, `[[`, 2 ) %>% unlist
            
            ### set model specification
            tsx_fcst_aux = window( mtsyx_vntg, start = j_fcst_trgt_yyqq, end = j_fcst_trgt_yyqq ); tsx_fcst_aux
            
            tsx_name = colnames(tsx_fcst_aux)[ which( !is.na(tsx_fcst_aux) ) ] 
            
            ### put incpt in the first column
            tsx_name_incpt_first = c( "incpt", tsx_name[ -which(tsx_name == "incpt") ] ); tsx_name_incpt_first
            
            if( grepl( "AR0", model_choice ) ){
              
              y_L_indx = grep( paste0( opt$sy,"_L" ), tsx_name_incpt_first )
              if( length( y_L_indx ) > 0 ) tsx_name_incpt_first = tsx_name_incpt_first[ -y_L_indx ] 
              
            } 
            
            tsx_name_incpt_first
            tsx_name_incpt_first_cln = tsx_name_incpt_first; 
            ### remove lagged indicators if contemporaneous values are available
            if( any( names( model_choice_sel ) != "incpt" ) ){
              
              names_not_incpt = names( model_choice_sel )[ -which( names( model_choice_sel ) == "incpt") ]; names_not_incpt
              
              list_indx = lapply( names_not_incpt, function( sx ){ # sx = names_not_incpt[1]; sx
                ##check if there are any contemporaneous values 
                indx_sx = grep( paste0( sx,".*[^L]{1}[^0-9]{1}$" ), tsx_name_incpt_first )
                if( length(indx_sx) > 0 ){
                  
                  indx_ud = grep( paste0( sx,".*[L]{1}[0-9]{1}$" ), tsx_name_incpt_first )
                  
                }else{
                  
                  indx_ud = integer(0)
                  
                } 
                indx_ud
              })
              if( length( list_indx %>% unlist ) > 0 ){
                
                tsx_name_incpt_first_cln = tsx_name_incpt_first[ -(list_indx %>% unlist) ]
                
              }
              
            }
            tsx_name_incpt_first_cln
            
            tsx_fcst = tsx_fcst_aux[ , tsx_name_incpt_first_cln, drop = F ]; tsx_fcst
            
            tsyx_estn = na.omit( mtsyx_vntg )
            
            tsy_estn = tsyx_estn[, opt$sy  , drop = F]
            tsx_estn = tsyx_estn[, tsx_name_incpt_first_cln, drop = F]
            
            # head( tsx_estn, drop = F )
            list( tsy_estn = tsy_estn, tsx_estn = tsx_estn, tsx_fcst = tsx_fcst )
          })
          names( list_mtsyx_fo ) = list_fcst_trgt
          
          #names( list_mtsyx_fo )
          ##list_mtsyx_fo[[ "2017-12-01" ]]
          #################################################################
          ###
          #################################################################
          
          # fcst_summ = sapply( names( list_mtsyx_fo ), function(otrn_date){ #  otrn_date = as.Date( "2009-12-01" )
          # 
          ptm <- proc.time()
          cl = makeCluster( detectCores() )
          registerDoParallel(cl)  # .combine = "rbind" %do% %dopar%
          fcst_summ = foreach( i = seq_along( names( list_mtsyx_fo ) ), .combine = "rbind", .inorder = T, 
                               .packages = c("bayarx", "bayarxsv", "lubridate","coda","scoringRules") ) %dopar% {
                                 #i = 56  
                                 otrn_date = names( list_mtsyx_fo )[i]; otrn_date
                                 
                                 
                                 mtsyx =list_mtsyx_fo[[ as.character( otrn_date ) ]]
                                 
                                 tsy_estn = mtsyx$tsy_estn
                                 tsy_otrn = window( tsy, start = c(year(otrn_date),quarter(otrn_date)), end = c(year(otrn_date),quarter(otrn_date)) )
                                 tsx_estn = mtsyx$tsx_estn
                                 tsx_fcst = mtsyx$tsx_fcst
                                 
                                 #summary( lm( tsy_estn ~ tsx_estn - 1 ) )
                                 #cbind( tsy_estn, tsx_estn )
                                 
                                 sfile_ud_vint = paste( model_choice, SV_switch, sep = "_"); sfile_ud_vint
                                 
                                 opt_mcmc             <- list()
                                 opt_mcmc$draws       <- 50000
                                 opt_mcmc$burnin      <- 10000
                                 opt_mcmc$thinpara    <- 10
                                 opt_mcmc$thinlatent  <- 10
                                 opt_mcmc$priorphi_a0 <- 20
                                 opt_mcmc$priorphi_b0 <- 1.5
                                 
                                 ###options for Bayesian regression: see p. 845
                                 opt_prior <- list()
                                 if( lmbd_set == "diffuse")      { lmbd <- c( 1000000, 1000000, 1 ) }  # shrinkage hyperparameters
                                 if( lmbd_set == "ccm2015"){ lmbd <- c(     0.2,     0.2, 1 ) }  # shrinkage hyperparameters
                                 
                                 
                                 ###priors for volatility
                                 opt_prior$variSigma0   = 4
                                 opt_prior$muPhi        = 0.035; #0.035
                                 opt_prior$priordfPhi   = 5
                                 opt_prior$arForPriorPi = 4;  # AR(4) for prior on slope coefficients
                                 
                                 ### collect all options
                                 opt$mcmc  = opt_mcmc
                                 opt$prior = opt_prior
                                 
                                 if( fcst_orgn == "fo1" | fcst_orgn == "fo2" ) horz = 1  
                                 if( fcst_orgn == "fo3" | fcst_orgn == "fo4" ) horz = 0
                                 
                                 
                                 #cat("\nhorz ", horz )
                                 
                                 if( grepl( "^AR0", model_choice ) ){
                                   
                                   tsx_name = "incpt"
                                   
                                 }else{
                                   
                                   tsx_name = colnames( tsx_estn )
                                   
                                 }
                                 tsx_name
                                 ### specify priors
                                 nylag    <- length( grep( paste0( opt$sy,"_L[1-9]{1}"), tsx_name ) ); nylag
                                 ncoef    <- ifelse( grepl( "^AR0", model_choice ),       1, dim( tsx_estn )[2] ); ncoef # number of coefficients in the model
                                 
                                 
                                 priorMean <- matrix( rep( 0, ncoef ), ncol = 1 )
                                 priorVari <- diag( ncoef ) * 0.01
                                 
                                 # lmbd = opt_prior$lmbd; lmbd
                                 
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
                                 
                                 if( SV_switch == "SV-OFF" ){
                                   
                                   print( cbind(vy,mx) )
                                   print( summary( lm(vy ~ mx ) ) )
                                   
                                   # draws <- RcppAR( opt_mcmc$draws, opt_mcmc$burnin, opt_mcmc$thinpara, priorMean, priorVari, vy, mx )$mstore
                                   draws <- bayarx( opt_mcmc$draws, opt_mcmc$burnin, opt_mcmc$thinpara, priorMean, priorVari, vy, mx )$mstore
                                   
                                   dim( draws ); str( draws )
                                   
                                   colnames( draws) <- c( "sigma", tsx_name ); head(draws) 
                                   
                                   res_coda  <- mcmc( draws )
                                   
                                   ###compute effective size
                                   ess_ud <- (opt_mcmc$draws / opt_mcmc$thinpara) / effectiveSize( res_coda ) 
                                   names( ess_ud ) <- paste( "ess", colnames(draws), sep = "_" ); ess_ud
                                   
                                   geweke_diag <- geweke.diag( res_coda , frac1=0.1, frac2=0.5)
                                   geweke_diag_ud <- c( geweke_diag[[2]], geweke_diag[[1]] )
                                   names( geweke_diag_ud ) <- paste0( "geweke_", c( "frac1", "frac2", colnames( draws ) ) )
                                   # geweke_diag_ud
                                   
                                   ### collect all diagnostics
                                   diag_ud <- c( ess_ud, geweke_diag_ud )
                                   
                                   ## one-step ahead volatility h_{T+1}
                                   predvol <- draws[,"sigma", drop = F] 
                                   
                                   ## one-step ahead posterior predictive distribution
                                   preddraws <- rnorm( nrow(predvol), draws[, tsx_name ] %*% matrix( tsx_fcst, ncol = 1 ), sqrt( predvol ) )
                                   summary( preddraws )
                                   
                                   # plot( density(draws[, "survindu_m3_lvl"] ) )
                                   # 
                                   # plot( density(draws[, "y_L2"] ) )
                                   # 
                                   # ols = lm( tsy ~ tsx - 1 ); summary( ols )
                                   # summary( draws[, "survindu_m3_lvl"] )  
                                   
                                   
                                 }
                                 if( SV_switch == "SV-ON" ){
                                   
                                   ###priors for volatility
                                   variSigma0  = opt_prior$variSigma0
                                   muPhi       = opt_prior$muPhi ; #0.035
                                   priordfPhi  = opt_prior$priordfPhi
                                   
                                   #use pre-sample to initialise period 0 volatiliy: here use the estimation sample
                                   meanSigma0 = log( y_sigma2 ); meanSigma0
                                   
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
                                   
                                   head( mLogSigma )
                                   
                                   rbind( mean = apply(  mPi , 2, mean ), sd = apply(  mPi, 2, sd ) )
                                   rbind( mean = apply(  vPhi, 2, mean ), sd = apply(  vPhi, 2, sd ) )
                                   
                                   startSigma <- as.Date( sprintf("%i-%i-01",start( tsy )[1],start( tsy )[2] * 3 ) ) %m-% months(3); startSigma
                                   
                                   ###draw stochastic volatility
                                   sigmaQ50 <- ts( apply( exp( mLogSigma / 2 ), 2, median                 ), start = c( year(startSigma), quarter(startSigma) ), frequency = 4 )
                                   sigmaQ95 <- ts( apply( exp( mLogSigma / 2 ), 2, quantile, probs = 0.95 ), start = c( year(startSigma), quarter(startSigma) ), frequency = 4 )
                                   sigmaQ05 <- ts( apply( exp( mLogSigma / 2 ), 2, quantile, probs = 0.05 ), start = c( year(startSigma), quarter(startSigma) ), frequency = 4 )
                                   
                                   #  pdf( file = paste0(path$plot, "SVSAMPLE", sfile_ud_vint,".pdf"  ) )
                                   # plot( sigmaQ50, col = "red", type = "l", lwd = 2, ylim =c( 0, max(sigmaQ95) ), main = "Stochastic Volatility (sd)" )
                                   # lines(sigmaQ95, col = "blue")
                                   # lines(sigmaQ05, col = "blue")
                                   #  dev.off()
                                   
                                   draws <- cbind(  vPhi , mPi )
                                   colnames( draws) <- c(  "phi", tsx_name ); head(draws) 
                                   
                                   res_coda  <- mcmc( draws )
                                   
                                   ###compute effective size
                                   ess_ud <- (opt_mcmc$draws / opt_mcmc$thinpara) / effectiveSize( res_coda ) 
                                   names( ess_ud ) <- paste( "ess", colnames(draws), sep = "_" )
                                   
                                   geweke_diag <- geweke.diag( res_coda , frac1=0.1, frac2=0.5)
                                   geweke_diag_ud <- c( geweke_diag[[2]], geweke_diag[[1]] )
                                   names( geweke_diag_ud ) <- paste0( "geweke_", c( "frac1", "frac2", "phi", tsx_name ) )
                                   geweke_diag_ud
                                   
                                   ### collect all diagnostics
                                   diag_ud <- c( ess_ud, geweke_diag_ud )
                                   
                                   ## one-step ahead log-volatility h_{T+1}
                                   predvol <- vLogSigmaPred; head( predvol )
                                   
                                   ## one-step ahead posterior predictive distribution
                                   preddraws <- rnorm( length(predvol), mPi %*% matrix( tsx_fcst, ncol = 1 ), exp( predvol / 2 ) ); head( preddraws )
                                   summary( preddraws )
                                   summary( exp( predvol / 2 ) )
                                   
                                 }
                                 
                                 crps = crps_sample( y = c( tsy_otrn ), dat = preddraws )
                                 logs = logs_sample( y = c( tsy_otrn ), dat = preddraws )
                                 pred_qntl = quantile( preddraws, probs = c(0.005, 0.025, 0.05, 0.165, 0.25, 0.333, 0.50, 0.666, 0.75, 0.835, 0.95, 0.975, 0.9995 ) )
                                 names( pred_qntl ) = paste0( "Q",names( pred_qntl ) ); pred_qntl
                                 
                                 # plot( density( preddraws )); abline( v = c( tsy_otrn ), lwd = 2, col = "red" )
                                 
                                 c( otrn = tsy_otrn, mean = mean(preddraws), erro = tsy_otrn - pred_qntl[ "Q50%"], logs = logs, crps = crps, pred_qntl )
                                 
                               } 
          stopCluster(cl)
          proc.time() - ptm
          
          fcst_sum_beg = as.Date( names( list_mtsyx_fo )[1] ); fcst_sum_beg
          
          tsfcst_summ = ts( fcst_summ, start = c( year(fcst_sum_beg), quarter(fcst_sum_beg) ), frequency = 4 )
          
          
          head( tsfcst_summ )
          
          fcst_title = paste0( model_choice,"_",lmbd_set,"_",SV_switch,"_", fcst_orgn ); fcst_title
          
          tsWrite( tsfcst_summ, paste0( path$resu, "fcst_summ_", fcst_title,".csv" ) )
          
          pdf( paste0( path$plot, "fcst_otrn_", fcst_title,".pdf"), width = 12, height = 7 )
          tfplot( ts(0, start = start( tsfcst_summ), end = end(tsfcst_summ), frequency = frequency( tsfcst_summ ) ), 
                  title = fcst_title, ylab = "", ylim = range( tsfcst_summ[ ,c( "otrn", "q16p5pp", "q83p5pp" ) ] ), col = "gray" )
          lines( tsfcst_summ[ ,"otrn"], lwd = 2, type = "b" )
          lines( tsfcst_summ[ ,"q50pp"], col = "red", lwd = 2, type = "b" )
          lines( tsfcst_summ[ ,"q16p5pp"], col = "blue", lwd = 2 )
          lines( tsfcst_summ[ ,"q83p5pp"], col = "blue", lwd = 2 )
          dev.off()
          
          
          tfplot( tsfcst_summ[ ,"crps"], lwd = 2, title = fcst_title, ylab = "crps" )
          tfplot( tsfcst_summ[ ,"logs"], lwd = 2, title = fcst_title, ylab = "logs" )
          
          rmsfe = sqrt( mean( fcst_summ[,"erro_q50pp"]^2 ) ); rmsfe
          mlogs = mean( fcst_summ[,"logs"] ); mlogs
          mcrps = mean( fcst_summ[,"crps"] ); mcrps
          
          c( rmsfe = rmsfe, mlogs = mlogs, mcrps = mcrps )
          
        }    
      }
    }   
  }
  
  
} 


list_resu_alle = list.files( path$resu )


### insert into PSQL
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, dbname="sboriss", host="localhost", user="postgres", password="postgres", port="5432")

dbGetQuery(conn, "SET search_path to sboriss")
dbGetQuery(conn, "CREATE EXTENSION IF NOT EXISTS hstore;") 

dbGetQuery(conn, "DELETE FROM ncstlvpseudo.resu;") 

sapply(list_resu_alle, function(file_name){ # file_name = list_resu_alle[151]; file_name
  
  dbx = read.csv( paste0( path$resu, file_name ), stringsAsFactors = F ); head( dbx )
  tsdbx = ts( dbx, start = c( dbx$year[1], dbx$period[1], frequency = 4 ) )[, -c(1,2)]
  
  file_name_aux = gsub( "as_in_CCM2015", "ccm2015", file_name     ); file_name_aux
  
  
  fcst_orgn = str_extract( file_name_aux,  "fo[0-9]{1}"    ); fcst_orgn
  SV_switch = str_extract( file_name_aux,  "SV-[A-Z]{2,3}" ); SV_switch
  lambdaset = ifelse( grepl( "diffuse", file_name_aux ), "diffuse", "ccm2015" ); lambdaset
  
  ### remove all attributes to get model specification
  file_name_aux = gsub( "fcst_summ_"           , "", file_name_aux ); file_name_aux
  file_name_aux = gsub( "\\.csv"               , "", file_name_aux ); file_name_aux
  file_name_aux = gsub( paste0( "_",fcst_orgn ), "", file_name_aux ); file_name_aux
  file_name_aux = gsub( paste0( "_",SV_switch ), "", file_name_aux ); file_name_aux
  model_choice  = gsub( paste0( "_",lambdaset ), "", file_name_aux ); model_choice
  
  
  ### upload to SQL 
  sql_query <- sprintf("INSERT INTO ncstlvpseudo.resu (created, model, fcstorig, stochvol, lambdaset ) VALUES ('%s','%s','%s','%s','%s')", 
                       opt$created, model_choice, fcst_orgn, SV_switch, lambdaset );    sql_query
  dbGetQuery(conn, sql_query)                      
  
  colnames( tsdbx )
  colnames( tsdbx ) = gsub( "erro.Q50.","erro_q50pp", colnames( tsdbx ), fixed = T )
  colnames( tsdbx ) = gsub( "\\.$","pp", colnames( tsdbx )  ); colnames( tsdbx )
  colnames( tsdbx ) = tolower( gsub( "\\.","p", colnames( tsdbx ) ) )
  colnames( tsdbx )
  
  ### put columns of tsdbx into PSQL
  sapply( colnames( tsdbx ), function( sx){ # sx = colnames( tsdbx )[ 2 ]; sx
    
    sql_query <- sprintf("UPDATE ncstlvpseudo.resu SET %s = '%s' WHERE created = '%s' AND model = '%s' AND fcstorig = '%s' AND stochvol = '%s' AND lambdaset = '%s'", 
                         sx, ts2hstore( tsdbx[, sx ] ), opt$created, model_choice, fcst_orgn, SV_switch, lambdaset );sql_query
    dbGetQuery(conn, sql_query)  
    
  })
  
  ### summmarize fcst accuracy
  sapply(opt$metric, function(i_metric){ # i_metric = opt$metric[1]; i_metric
    
    pool_subsamples = lapply( opt$fcst_eval_smpl, function( smpl ){ #smpl = fcst_eval_smpl[[3]]
      
      if( i_metric == "rmsfe" ) ud = window( tsdbx[, "erro_q50pp"], start = smpl$beg, end = smpl$end )**2 %>% mean %>% sqrt
      if( i_metric == "mcrps" ) ud = window( tsdbx[, "crps"      ], start = smpl$beg, end = smpl$end )    %>% mean 
      if( i_metric == "mlogs" ) ud = window( tsdbx[, "logs"      ], start = smpl$beg, end = smpl$end )    %>% mean
      ud
      
    })
    pool_metric = do.call( "cbind.data.frame", pool_subsamples )
    
    ### insert into PSQL
    hstore2sql = paste( colnames( pool_metric ), pool_metric, sep="=>", collapse=",")
    sql_query <- sprintf("UPDATE ncstlvpseudo.resu SET %s = '%s' WHERE created = '%s' AND model = '%s' AND fcstorig = '%s' AND stochvol = '%s' AND lambdaset = '%s'", 
                         i_metric, hstore2sql, opt$created, model_choice, fcst_orgn,SV_switch,lambdaset );sql_query
    dbGetQuery(conn, sql_query)  
    
  })
})

sql_query = sprintf(  "SELECT model, fcstorig FROM ncstlvpseudo.resu WHERE model = '%s' AND stochvol = '%s' AND lambdaset = '%s' ;", "AR2+incpt", "SV-ON", "diffuse" ); sql_query
dbGetQuery(conn, sql_query ) 

model = "AR2+incpt+vacancy_yoy+omxr_qoq+survreta_lvl+retail_yoy+survind_lvl+survind_biud"

sql_query = sprintf(  "SELECT fcstorig, stochvol, lambdaset, public.skeys(rmsfe), public.svals(rmsfe), model  FROM ncstlvpseudo.resu WHERE model = '%s' AND stochvol = '%s' AND lambdaset = '%s' ;", model, "SV-ON", "diffuse" ); sql_query
dbGetQuery(conn, sql_query ) 



# close the connection
dbDisconnect(conn)
dbUnloadDriver(drv)


















list_resu_db_alle = lapply( list_resu_alle, function( resu_file ){
  
  resu_mat = read.csv( paste0( path$resu, resu_file), stringsAsFactors = F ); head( resu_mat )
  ts_resu = ts( resu_mat, start = c( resu_mat$year[1], resu_mat$period[1] ), frequency = 4 )
  
})
names( list_resu_db_alle ) = list_resu_alle

choiceFO = "_fo4"
choiceSV = "_SV-OFF" #"_SV-ON" # 
choicePR = "_diffuse" # "_as_in_CCM2015" #

list_resu_db_fo       = list_resu_db_alle [ grep( choiceFO, names(list_resu_db_alle) ) ]
list_resu_db_fo_sv    = list_resu_db_fo   [ grep( choiceSV, names(list_resu_db_fo) ) ]
list_resu_db_fo_sv_pr = list_resu_db_fo_sv[ grep( choicePR, names(list_resu_db_fo_sv) ) ]

names( list_resu_db_fo_sv_pr )

fcst_eval_smpl = list( fullsample  = list( beg = c(2004,1), end = c(2017,4) ),
                       pre_crisis  = list( beg = c(2004,1), end = c(2007,3) ),
                       crisis      = list( beg = c(2007,4), end = c(2010,3) ),
                       post_crisis = list( beg = c(2010,4), end = c(2017,4) ) )

metric = c( "rmsfe", "mlogs", "mcrps" )

### loop over metrics
lapply(metric, function(i_metric){ # i_metric = metric[1]; i_metric
  
  ### loop over models
  pool_models = lapply( list_resu_db_fo_sv_pr, function( ts_resu){ 
    
    ### loop over sub-samples
    pool_subsamples = lapply( fcst_eval_smpl, function( smpl ){ #smpl = fcst_eval_smpl[[3]]
      
      if( i_metric == "rmsfe" ) ud = window( ts_resu[, "erro.Q50."], start = smpl$beg, end = smpl$end )**2 %>% mean %>% sqrt
      if( i_metric == "mcrps" ) ud = window( ts_resu[, "crps"     ], start = smpl$beg, end = smpl$end )    %>% mean 
      if( i_metric == "mlogs" ) ud = window( ts_resu[, "logs"     ], start = smpl$beg, end = smpl$end )    %>% mean
      ud
      
    })
    do.call( "cbind", pool_subsamples )
    
  })
  names( pool_models ) = gsub( "fcst_summ_", "", names( pool_models ) )
  names( pool_models ) = gsub( ".csv"      , "", names( pool_models ), fixed = T )
  pool_models_tbl = do.call( "rbind", pool_models )
  rownames( pool_models_tbl ) = names( pool_models ); pool_models_tbl
  
  apply( pool_models_tbl, 1, function( x ) x / pool_models_tbl[ 1, , drop = F] )
  
  pool_models_tbl_ud = cbind( pool_models_tbl, sweep( pool_models_tbl, 2, c( pool_models_tbl[ 1, ] ) , `/` ) )
  
  write.csv( pool_models_tbl_ud, paste0( path$summ, "resu_summ_subsample_", i_metric,choicePR,choiceSV,choiceFO,".csv" ) )
  
})


### Q1: does SV improve RMSFE
if( FALSE ){
  
  list_resu_summ_alle = list.files( path$summ ) 
  
  list_resu_summ_rmsfe_diffuse = list_resu_summ_alle[ grep( "_rmsfe_diffuse_", list_resu_summ_alle ) ]; list_resu_summ_rmsfe_diffuse
  
  
  col2compare = c( "fullsample", "pre_crisis", "crisis", "post_crisis" )
  list_resu_summ_rmsfe_diffuse_tbl = lapply( list_resu_summ_rmsfe_diffuse, function(sx){
    # sx = list_resu_summ_rmsfe_diffuse[1]
    dbx = read.csv( paste0( path$summ, sx ), stringsAsFactors = F, row.names = 1 )[, col2compare ]  
    
  })
  names( list_resu_summ_rmsfe_diffuse_tbl ) = list_resu_summ_rmsfe_diffuse
  
  rmsfe_onn_model = list_resu_summ_rmsfe_diffuse_tbl[[ "resu_summ_subsample_rmsfe_diffuse_SV-ON_fo4.csv"  ]][-c(1,2), ]
  rmsfe_off_model = list_resu_summ_rmsfe_diffuse_tbl[[ "resu_summ_subsample_rmsfe_diffuse_SV-OFF_fo4.csv" ]][-c(1,2), ]
  
  rownames( rmsfe_onn_model ) = gsub( "_SV-ON_fo4", "", rownames( rmsfe_onn_model ) )
  rownames( rmsfe_off_model ) = gsub( "_SV-OFF_fo4","", rownames( rmsfe_off_model ) )
  
  ratio_resu_summ_rmsfe_diffuse_tbl = ( rmsfe_onn_model / rmsfe_off_model - 1 ) * 100
  
  ### for all models
  apply( ratio_resu_summ_rmsfe_diffuse_tbl, 2, median )
  summary( ratio_resu_summ_rmsfe_diffuse_tbl )
  
  ### for all models| better than AR2+incpt SV-OFF (either SV-ON or SV-OFF )
  bnch = list_resu_summ_rmsfe_diffuse_tbl[[ "resu_summ_subsample_rmsfe_diffuse_SV-OFF_fo4.csv" ]][ "AR2+incpt_diffuse_SV-OFF_fo4", , drop = F]; bnch
  
  ### ratio wrt bnch
  rmsfe_off_model2bnch = sweep( rmsfe_off_model, 2, unlist( c( bnch ) ), `/`)
  rmsfe_onn_model2bnch = sweep( rmsfe_onn_model, 2, unlist( c( bnch ) ), `/`) 
  
  rmsfe_onn_model2bnch_log = rmsfe_onn_model2bnch < 1
  rmsfe_off_model2bnch_log = rmsfe_off_model2bnch < 1 
  
  indx_log = ifelse( rmsfe_onn_model2bnch_log + rmsfe_off_model2bnch_log > 0, 1, NA )
  
  indx_by_subsample = apply( indx_log, 2, function(x) c( which( !is.na( x ) ) ) )
  
  sapply( names( indx_by_subsample ), function( x ){
    
    indx = indx_by_subsample[[ x ]]; attributes( indx ) = NULL
    summary( ratio_resu_summ_rmsfe_diffuse_tbl[indx, x ] )
    
  })
  
}

fnGetDateSQL <- function(x){
  ### create dates in SQL(hstore) format  
  # input: a ts object x
  period <- "month"
  beg.x  <- start(x)
  if(frequency(x) == 4){ period <- "3 months"; beg.x[2] <- beg.x[2]*3;} #set to the last month in quarter
  return(seq(as.Date(sprintf("%i-%i-1", beg.x[1], beg.x[2])), by=period, length.out=length(x)))
}
fnGetHstore <- function(x){
  ### create hstore object for SQL  
  # input: a ts object x
  dates  <- fnGetDateSQL(x); dates
  hstore <- paste(dates, x, sep="=>", collapse=",")
  hstore <- gsub("NA","NULL",hstore); 
  return(hstore)
}


drv <- dbDriver("PostgreSQL")

conn <- dbConnect(drv, dbname="sboriss", host="localhost", user="postgres", password="postgres", port="5432")

dbGetQuery(conn, "SET search_path to sboriss")
dbGetQuery(conn, "CREATE EXTENSION IF NOT EXISTS hstore;")

dbGetQuery(conn, "CREATE EXTENSION hstore SCHEMA public;")

ts2hstore = function( x ){
  
  dates    <- fnGetDateSQL(x); dates
  x_hstore <- paste(dates, x, sep="=>", collapse=","); x_hstore
  x_hstore <- gsub("NA","NULL",x_hstore)  
  x_hstore
}

ts2hstore( tsy )

### upload to SQL 
sql_query <- sprintf("INSERT INTO public.books (title, attr) VALUES ('MY DATA','%s')", x_hstore ); sql_query
df = dbGetQuery(conn, sql_query)

str( df )
rownames( df )

df[ 1, ]

### get all values
sql_query <- "SELECT public.skeys(attr), public.svals(attr) FROM public.books WHERE id = 1;"
df = dbGetQuery(conn, sql_query)

rownames( df ) = df$skeys
df[,-1, drop = F] 

df[ "weight", ]

attributes( df )

sql_query <- "SELECT public.svals(attr) FROM public.books WHERE id = 1;"
df = dbGetQuery(conn, sql_query)


sql_query <- "SELECT attr -> 'weight' AS weight FROM public.books WHERE  id = 1;"
dbGetQuery(conn, sql_query)




sql_query <- "SELECT title, (EACH(attr) ).* FROM public.books;"
dbGetQuery(conn, sql_query)


#   query_i <- sprintf("SELECT public.skeys(data)::date AS date, public.svals(data)::double precision AS value
#                          FROM %s
#                  WHERE abbr='%s' AND vintage='%s' AND release='%s' ",
#                    sql$data_qsna, x["abbr"], x["vintage"], x["release"]); query_i
#   dfy <- dbGetQuery(conn, query_i)
#




list_pair_csv = c( "fcst_summ_AR2+incpt_diffuse_SV-ON.csv",
                   "fcst_summ_AR2+incpt+vacancy_yoy+omxr_qoq_diffuse_SV-ON.csv" )

list_pair_csv = c( "fcst_summ_AR0+incpt_diffuse_SV-OFF.csv",
                   "fcst_summ_AR2+incpt_diffuse_SV-ON.csv" )

list_pair_csv = c( "fcst_summ_AR0+incpt_diffuse_SV-ON.csv",
                   "fcst_summ_AR2+incpt_diffuse_SV-ON.csv" )

list_pair_csv = c( "fcst_summ_AR2+incpt_diffuse_SV-OFF.csv",
                   "fcst_summ_AR2+incpt_diffuse_SV-ON.csv" )

list_pair_csv = c( "fcst_summ_AR2+incpt_diffuse_SV-ON.csv",
                   "fcst_summ_AR2+incpt+vacancy_yoy+omxr_qoq_diffuse_SV-ON.csv" )

list_pair_csv = c( "fcst_summ_AR2+incpt_diffuse_SV-ON.csv",
                   "fcst_summ_AR2+incpt+expt_qoq_diffuse_SV-ON.csv" )

list_pair_csv = c( "fcst_summ_AR2+incpt_diffuse_SV-ON.csv",
                   "fcst_summ_AR2+incpt+expt_yoy_diffuse_SV-ON.csv" )
list_pair_model = sapply( list_pair_csv, function(x){  
  
  tmp = gsub( "fcst_summ_","", x)
  gsub( ".csv","", tmp, fixed = T )
  
})
attributes( list_pair_model ) <- NULL; list_pair_model

title4plot = paste( unlist( list_pair_model ), collapse = "\n" )

fGetFcstSumm = function( list_file, sx ){
  ud = lapply( list_file, function( sfile ){
    
    db = as.data.frame( read.csv( sfile ) ); print( colnames( db )  )
    ts( db[, sx ], start = c( db$year[1], quarter = db$period[1] ), frequency = 4 )
    
  })
  ud
}
erro = fGetFcstSumm( paste0( path$resu, list_pair_csv ), "erro.Q50." )
names( erro ) = list_pair_model; erro

erro2Sqr = ( do.call( "cbind", erro ) )**2
mat_cssfed = apply( sweep( erro2Sqr, 1, c( erro2Sqr[,1] ) , `-` ) * (-1), 2, cumsum )[ ,-1]
ts_cssfed  = ts( mat_cssfed, start = start( erro2Sqr ), frequency = frequency( erro2Sqr ) )
tfplot( ts_cssfed, title = paste("CSSFED: \n", title4plot ) )

fcst = fGetFcstSumm( paste0( path$resu, list_pair_csv ), "Q50." )
names( fcst ) = list_pair_model; fcst

tsy_fcst_sample = window( tsy, start = start( fcst[[1]] ), end = end( fcst[[1]] ) )
tfplot( tsy_fcst_sample, lwd = 2, title = paste("OUTTURN: \n", title4plot )  ); abline( h = 0 )
lines( fcst[[1]], type = "b", col = "blue")
lines( fcst[[2]], type = "b", col = "red")

logs = fGetFcstSumm( paste0( path$resu, list_pair_csv ), "logs" )
names( logs ) = list_pair_model; logs
mat_logs = ( do.call( "cbind", logs ) )
mat_cslogs = apply( sweep( mat_logs, 1, c( mat_logs[,1] ) , `-` ) * (-1), 2, cumsum )[ ,-1]
ts_cslogs  = ts( mat_cslogs, start = start( mat_logs ), frequency = frequency( mat_logs ) )
tfplot( ts_cslogs, title = paste("CSLOGS: \n", title4plot ) )

crps = fGetFcstSumm( paste0( path$resu, list_pair_csv ), "crps" )
names( crps ) = list_pair_model; crps
mat_crps = ( do.call( "cbind", crps ) )
mat_cscrps = apply( sweep( mat_crps, 1, c( mat_crps[,1] ) , `-` ) * (-1), 2, cumsum )[ ,-1]
ts_cscrps  = ts( mat_cscrps, start = start( mat_crps ), frequency = frequency( mat_crps ) )
tfplot( ts_cscrps, title = paste("CSCRPS: \n", title4plot ) )



