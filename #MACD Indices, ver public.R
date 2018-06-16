##-------------------------------------------------------------------------------------
## PROJECT (Author: GitHub user "redsun123")
## Finding optimal parameters for MACD 
## (Moving Average Convergence-Divergence) stock trading indicator

## BACKGROUND
## MACD (Moving Average Convergence-Divergence) stock trading indicator 
## is one of the ## popular technical oscillators/indicators that is often employed 
## to enter market for trading securities. Regularly recommended settings 
## use subtraction of the 26-day exponential moving average (EMA) from the 
## 12-day EMA (https://www.investopedia.com/terms/m/macd.asp). 
##      In our R project, we studied a wide range of settings to evaluate financial 
## return of underlining security trading. We found that optimal settings vary 
## greatly between securities, and therefore length of the long and short EMA 
## waves need to be determined specifically for each security. When we model 
## international index trading, the cumulative financial return with optimal MACD
## parameters we found was always better than a conservative buy-and-hold 
## strategy.    
## 
## NOTE-1:  this code is designed and written by GitHub user "redsun123"
##                 exclusively for demonstrating of the programming automation
##                 capabilities.  Author of the code does not responsible for any 
##                 financial losses occurred, but is responsible solely for a profit.
##
## WORKFLOW
##   -- download international stock market indices
##   -- clean data
##   -- calculate exponential moving averages
##   -- calculate MACD and MACD-Histogram
##   -- generate signals to open position
##   -- emulate the closing of positions in several days and calculate return
##   -- vary length of the short (signal) and long waves
##   -- generate reports
##
## REPORTING and OUTPUT:
## REPORT-1: generate summary report (as a *.csv file) with all 
##                     analyzed indices and all returns for the entire time period 
##                     under investigation (approximately three calendar years)
## REPORT-2: generate heatmap (as a *.PDF file) for each index to 
##                     visualize the best combination of the short and long waves 
##                     to reach maximal return 
## REPORT-3: compare returns for each index assuming conservative 
##                     buy-and-hold strategy (single *.csv file) versus MACD with 
##                     optimal settings
##
## NOTE-1: Main code is in R language. The code invokes additional 
##                custom functions that are located in a source code in a 
##                separate file.
## NOTE-2: code requires configuration of working directory; 
##                code needs R environment; code.R file should be moved into 
##                working directory
## NOTE-3: REPORTs are located in the subdirectory "BGS_Cache" 
##-----------------------------------------------------------------------------------------

rm(list = ls(all = TRUE)) ## Remove all objects in workspace
# cat("\014") # clear screen

library(BatchGetSymbols)
library(ggplot2)
library(RColorBrewer)
library(reshape2)

# if you wish to use a specific working directory - entre its name below
# mypath <- c('C:/')
# setwd(mypath)
setwd("~/")
source("#MACD Source Functions, ver public.R")

# download indices historical quotes for about one year from Yahoo Finance web
tickers <- c('^BVSP', '^FCHI',  '^GDAXI',	'^GSPC',	'^HSI',  '^N225')
BatchGetSymbols(tickers, first.date = Sys.Date() - 360,
  last.date = Sys.Date(), thresh.bad.data = 0.75, bench.ticker = "^GSPC",
  type.return = "arit", do.complete.data = FALSE, do.cache = TRUE,
  cache.folder = "BGS_Cache")

## Optimize long (lo) and signal (sh, shorter) waves for MACD
lo <- seq(5, 50, by = 3)  # long wave
sh <- seq(3, 25, by = 2)   # shorter signal wave

# seed matrix for rbind reports and recursive reporting table
IND_rep <- matrix(0, nrow = 1, ncol = 1 + length(lo))
big_rep <- matrix(0, nrow = length(sh) + 3, ncol = 1 + length(lo))
rep <- matrix(0, nrow = length(sh), ncol = length(lo))

# flying file by file (i)
setwd(paste0(getwd(), '/BGS_Cache'))
flist <- list.files(pattern = "*.rds")
for (i in seq(length(flist))) { 
IND <- na.omit(readRDS(gzfile(flist[i])))
lastR <- dim(IND)[1]
message('Just loaded: ', flist[i])

## li - VARY length of the long wave
  for (li in seq(length(lo))) {
## si - VARY length of the short wave
    for (si in seq(length(sh))) {
      if (sh[si] >= lo[li]) {
        next
      }
      
      # Every time re-create m-matrix to match IND file and to erase 
      # previous calculations based on old "sh" and "lo" values
      m <- matrix(0, nrow = lastR, ncol = 5)
      colnames(m) <-  c("1.EMA(Short)", "2.EMA(Long)", "3.MACD=Sh-Lo", 
        "4.Buy_Sell", "5.Return")
 
      Calc_MACD()
      Calc_Return()       
      
      ## Calculate %(SUM) of returns for this IND
      rep[si, li] <- 100 * sum(m[5:(lastR - 2 - 4), 5])
 
    } # End si  short wave cycle
  } # End li  long wave cycle
  
  # Reporting into rep
  # rep_pt <- rep[3:(2 + length(sh)), 2:(1 + length(lo))]
  M <- round(max(rep), 1)
  rc <- which(rep == M, arr.ind = TRUE)
  big_rep[3:(2 + length(sh)), 2:(1 + length(lo))] <- rep
  big_rep[1, 1] <-
    substr(flist[i], 1, nchar(flist[i]) - 4) # IND name w/o ".csv"
  big_rep[1, 3] <- paste0("max = ", M)
  big_rep[2, 2:(1 + length(lo))] <- lo
  big_rep[3:(length(sh) + 2), 1] <- t(sh)
  
IND_rep <- rbind(IND_rep, big_rep)

## Create Heatmap for each Index based on saved *.csv file
data <- rep   
rownames(data) <- paste0("d", sh)
colnames(data) <- paste0("d", lo)

data[data == 0] <- NA
data_melt <- melt(data)
hm.palette <-
  colorRampPalette(rev(brewer.pal(11, 'Spectral')), space = 'Lab')

myPlot <-
  ggplot(data_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 1))) +
  xlab('long wave, days') +
  ylab('short wave, days') +
  ggtitle(big_rep[1, 1]) +
  scale_fill_gradientn(colours = hm.palette(100))

ggsave(filename = paste0("Heatmap_", big_rep[1, 1], ".pdf"), plot = myPlot, width = 10, height = 5, dpi = 150, units = "in")

  # Save_INDm_file()
  
} # end i  of  flying over files

write.csv(IND_rep, file = paste0('Indices_MACD', '-', format(Sys.time(), "%Y%m%d-%Hh%M'%S''"),'.csv'))




