#### carica librerie necessarie ####

rm(list=ls())
cat("\014")  
library(tidyverse)
library(tidyquant)
library(timetk)
library(quantmod)
library(xts)
library(QuantTools)
library(broom)
library(nlme)
library(ggplot2)
library(stargazer)

#### #####

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### carica i dati  ####

# Fama French 3 factors
dati = read_csv("C:/Users/longfils/Desktop/R_projects/F-F_Research_Data_Factors_daily.csv", skip=5, 
                col_names = c("data","MkrRF","SMB","HML","RF"),
                col_types = cols(
                  data = col_datetime(format="%Y%m%d"),
                  MkrRF = col_double(),
                  SMB = col_double(),
                  HML = col_double(),
                  RF = col_double()))


# scarica dati da Yahoo Finance
# stocks <- stockSymbols()
# 
# stocks <- stocks[complete.cases(stocks),]
# stocks <- stocks[stocks$LastSale>=50,]
# 
# fondi <- sample(stocks$Symbol,100)
fondi <- c('SPY','AGG','EFA')
prezzi <- getSymbols(fondi, from = "2016-12-31", to = "2018-12-31", src = "yahoo",
                     auto.assign = TRUE, warnings = FALSE) 

# considero solo i loro valori Adjusted (get + Ad)
prezzi <- prezzi %>% map(~Ad(get(.)))
# unisci i valori di SPY e AGG in un unico data frame
prezzi <- prezzi %>% reduce(merge)
# rinomina le colonne
colnames(prezzi) <- fondi


#### calcolo le log return mensili ####


# converti prezzi_daily in una tibble e rinomina index in "data"
prezzi_daily <- prezzi %>% tk_tbl(preserve_index = TRUE, rename_index = "data")
# cambia come i dati solo salvati nella tibble: la prima colonna indica il nome dell'asset,
# la seconda il corrispondente return 
prezzi_daily <- prezzi_daily %>% gather(asset, returns, -data)
# calcola la log(return)
prezzi_daily <- prezzi_daily %>% group_by(asset) %>% mutate(returns = (log(returns) - log(lag(returns))))
prezzi_daily <- prezzi_daily %>% na.omit()

  
#### costruisco un portfolio ####

# proporzione dei singoli fund all'interno del portfolio (tutti uguali al momento)
pesi <- c(rep(1/ncol(prezzi),ncol(prezzi)))
# calcolo la return totale del portfolio come somma pesata delle returns degli asset
portfolio_returns <- tq_portfolio(prezzi_daily, asset, returns, weights = pesi, rebalance_on = "days", col_rename = "returns")

#### preparo i fattori per il modello Fama French ####

# per prima cosa estraggo solo le colonne in "dati" che corrispondono allo 
# stesso periodo temporale per portfolio_returns
data_inizio <- first(portfolio_returns$data)
data_fine <- last(portfolio_returns$data)+months(1)
fattoriFF <- filter(dati, dati$data>=data_inizio & dati$data<=data_fine)
# unisci i fattori FF e portfolio returns
fattoriFF$data <- as.Date(fattoriFF$data)
fattoriFF <- left_join(portfolio_returns,fattoriFF, by = "data")
# crea una variabile excess return = portfolio return - RF
fattoriFF$ExReturn <- fattoriFF$returns - fattoriFF$RF
fattoriFF <- fattoriFF %>% na.omit()

#### fit il modello FF #####

model <- fattoriFF %>% lm(ExReturn ~ MkrRF + SMB + HML, data=.)
#converti modello in tibble e calcola intervallo di confidenza per i coefficienti
modello <- tidy(model, conf.int = T, conf.level = 0.95)

coeffplot <- ggplot(modello, aes(x=term, y=estimate)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1) +
  geom_point()+geom_hline(yintercept = 0, color= "red")
print(coeffplot)

fattoriFF$FitReturn <- model$fitted.values

pred_vs_real <- ggplot(filter(fattoriFF, fattoriFF$data>=data_fine-years(1)), aes(x=data)) + geom_line(aes(y=ExReturn, color="Return osservate"))  +
  geom_line(aes(y=FitReturn, color="Return prevista dal modello")) +ylab("Return")
print(pred_vs_real)

# il comando qui sotto produce la tabella con i risultati da copiare direttamente in Latex
# stargazer(model)

#### fit il modello FF su anni precedenti all'ultimo e valida sull'ultimo anno #####

fattoriFFold <- filter(fattoriFF, fattoriFF$data<=data_fine-years(1))
fattoriFFnew <- filter(fattoriFF, fattoriFF$data>data_fine-years(1))
model <- fattoriFFold %>% lm(ExReturn ~ MkrRF + SMB + HML, data=.)
newPred <- predict(model, fattoriFFnew)
fattoriFF$PredReturn <- c(model$fitted.values,newPred)

pred_vs_real <- ggplot(filter(fattoriFF, fattoriFF$data>=data_fine-years(1)), aes(x=data)) + geom_line(aes(y=ExReturn, color="Return osservate"))  +
  geom_line(aes(y=FitReturn, color="Return fit dal modello")) +
  geom_line(aes(y=PredReturn, color="Return prevista dal modello")) +
  ylab("Return")
print(pred_vs_real)

autocorrelation = acf(model$residuals)

p1 <- ggplot(fattoriFF, aes(x=data, y=MkrRF )) +
  geom_line() +
  ylab("MktRF")


p2 <- ggplot(fattoriFF, aes(x=data, y=HML )) +
  geom_line() +
  ylab("HML")

p3 <- ggplot(fattoriFF, aes(x=data, y=SMB )) +
  geom_line() +
  ylab("SMB") + xlab("data")

p4 <- ggplot(fattoriFF, aes(x=data, y=RF )) +
  geom_line() +
  ylab("RF") + xlab("data")

multiplot(p1, p2, p3, p4, cols=1)