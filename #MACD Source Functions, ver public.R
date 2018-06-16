## Source FUNCTIONs

# RDS data.frame fields. Source of historical stock quotes is YAHOO FINANCE.
# [[1]] price.open
# [[2]] price.high
# [[3]] price.low
# [[4]] price.close
# [[5]] volume
# [[6]] price.adjusted
# [[7]] ref.date
# [[8]] ticker

## FUNCTION:  Calc_MACD
## Calculate EMA (Exponential Moving Average) for Long and Short waves
Calc_MACD <- function() {
  m[1, 1:2]  <<- IND[1, 4]
  m[1, 3:5]  <<- 0.01  
  sh_K <<- 2 / (sh[si] + 1)
  lo_K <<- 2 / (lo[li] + 1)
  for (r in 2:lastR) {
    m[r, 1] <<- IND[r, 4] * sh_K + m[r - 1, 1] * (1 - sh_K) # short wave
    m[r, 2] <<- IND[r, 4] * lo_K + m[r - 1, 2] * (1 - lo_K) # long wave
    m[r, 3] <<- m[r, 1] - m[r, 2]   # MACD = EMA_short - EMA_long
    }
} ## End of FUNCTION:  Calc_MACD


## FUNCTION:  Calc_Return
## Calculate Buy/Sell signal and Return baced on 4 days hold strategy (d=4)
## t1 - thresholds for normalized change of the price
Calc_Return <- function(d = 4, t1 = 0.002) {
  for (r in 5:(lastR - 2 - d)) {
    d12 <- (m[r, 3] - m[r - 1, 3]) / m[r, 3]
    d23 <- (m[r + 1, 3] - m[r, 3]) / m[r, 3]
    ## Buy signal
    if (d12 > t1 && d23 > t1 && IND[r + 2, 3] < IND[r + 1, 4]) {     
      m[r + 2, 4] <<- +1
      m[r + 2, 5] <<- (IND[r + 2 + d, 4] - IND[r + 2, 4]) / IND[r + 2, 4]   
    }
    ## Sell signal
    if (d12 < -t1 && d23 < -t1 && IND[r + 2, 2] > IND[r + 1, 4]) {   
      m[r + 2, 4] <<- -1
      m[r + 2, 5] <<- (IND[r + 2, 4] - IND[r + 2 + d, 4]) / IND[r + 2 + d, 4]     
    }
  }
}   ## End of FUNCTION: Calc_Return


## FUNCTION:  Save_INDm_file
## merge IND and "m" column-wise and write to disk the resulted file.csv with a time stamp
Save_INDm_file <- function() {
  INDm <- cbind(IND, m)
  write.csv(INDm, file = paste0(
    '#',
    gsub(pattern = "\\.csv$", "", flist[i]),
    ', ',
    format(Sys.time(), '%Hh%Mm%Ss'),
    '.csv'
  ))
} ## End of FUNCTION: Save_INDm_file










