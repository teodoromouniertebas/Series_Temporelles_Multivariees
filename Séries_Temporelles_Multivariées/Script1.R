###################################  Projet Multivar√© ###################################
# 2 . Explication d'une expension mon√©taire en √©conomie ouverte (interet/PIB/Taux de Change /BC/Domestic reterns/Investissement) :


# Import de la base
library(readxl)
library(zoo)
library(fUnitRoots)
library(urca)
library(vars)
library(MTS)
library(tsDyn)
library(Quandl)
library(tseries)
library(urca)
library(vars)
library(tsDyn)
library(ggplot2)

Quandl.api_key("xLQnFvg45KANa2ZjVjKU")

GDP <- Quandl("FRED/GDP",type="zoo",start_date="1950-01-01", end_date="2000-12-31") 
I <- Quandl("FRED/INTDSRUSM193N",type="zoo",collapse = "quarterly",start_date="1950-01-01", end_date="2000-12-31")


series_plot <- function(GDP, I){
  par(mfrow = c(1, 1))
  plot(GDP, main="Gross Domestic Product", xlab = "") 
  plot(I, main="Interest Rates", xlab = "")
} # graphique des sÈries


series_plot(GDP, I)


stat_tests <- function(series, seuil){
  adf <- adf.test(series)
  kpss <- kpss.test(series)
  ers <- ur.ers(series, type = "P-test", model = "trend")
  name <- deparse(substitute(series))
  crit <- seuil
  if (adf$p.value > crit){
    print(paste(name, "is non-stationary at a", crit, "confidence level (adf)"))}
  else if (adf$p.value < crit) {
    print(paste(name, "is stationary at a", crit, "confidence level (adf)"))
  }
  
  if (kpss$p.value > crit){
    print(paste(name, "is stationary at a", crit, "confidence level (kpss)"))}
  else if (kpss$p.value < crit) {
    print(paste(name, "is non-stationary at a", crit, "confidence level (kpss)"))
  }
  
  if (ers@teststat > ers@cval[2]){
    print(paste(name, "is non-stationary at a", crit, "confidence level (ers)"))}
  else if (ers@teststat < ers@cval[2]) {
    print(paste(name, "is stationary at a", crit, "confidence level (ers)"))
  }
} # ADF/GLS et KPSS


stat_tests(GDP, 0.05)
stat_tests(I, 0.05)

# Non stat. -> diff
GDP_diff <- diff(GDP, lag = 1) 
I_diff <- diff(I, lag = 1)
stat_tests(GDP_diff, 0.05)
stat_tests(I_diff, 0.05)
series_plot(GDP_diff, I_diff)

# toujours pas stationnaire en variance -> log
GDP_log_diff <- diff(log(GDP))
I_log_diff <- diff(log(I))
stat_tests(GDP_log_diff, 0.05)
stat_tests(I_log_diff, 0.05)
series_plot(GDP_log_diff, I_log_diff)

# On regroupe dans une matrice
GDP <- log(GDP)
I <- log(I)
y <- cbind(GDP, I)
plot(y)

# Nombre de retards ?
VARselect(y, lag.max=12) # -> 3

# specification de la cointegration ?
none <- VECM(y, lag = 3, estim = "ML", include = 'none')
const <- VECM(y, lag = 3, estim = "ML", LRinclude = 'const')
trend <- VECM(y, lag = 3, estim = "ML", include = 'trend')

BIC(none)
BIC(const)
BIC(trend)

AIC(none)
AIC(const)
AIC(trend) # On choisi la sp?cification de la relation de coint?gration (ici "const")

johansen <- ca.jo(y, type = "trace", ecdet = "const", K = 3, spec = "longrun")
summary(johansen) # il ya bien une relation de coint?gration
johansen <- ca.jo(y, type = "eigen", ecdet = "const", K = 3, spec = "longrun")
summary(johansen) # il ya bien une relation de coint?gration

#Restrictions "de base" a la Johansen sur Beta
vecm <- cajorls(johansen, r = 1)
summary(vecm$rlm)

# Fonctions d'impulsion
vecm_to_var <- vec2var(johansen, r = 1)

# diagnostic residus
serial.test(vecm_to_var)
normality.test(vecm_to_var)

resid_plots <- function(model){
  resid <- data.frame(GDP = model$resid[,1], I = model$resid[,2])
  resid$Index <- seq(from = 1, to = length(resid$GDP), by = 1)
  
  mean_GDP = mean(resid$GDP)
  sd_GDP = sd(resid$GDP)
  mean_I = mean(resid$I)
  sd_I = sd(resid$I)
  
  GDP <- ggplot(data = resid)+
    geom_density(aes(x = GDP), color = 'steelblue')+
    stat_function(fun = dnorm, args = list(mean = mean_GDP, sd = sd_GDP), color = 'red')+
    ggtitle("R?sidus de GDP")+
    theme_minimal()
  
  print(GDP)
  
  I <- ggplot(data = resid)+
    geom_density(aes(x = I), color = 'steelblue')+
    stat_function(fun = dnorm, args = list(mean = mean_I, sd = sd_I), color = 'red')+
    ggtitle("R?sidus de I")+
    theme_minimal()
  
  print(I)
  
  Time_GDP <- ggplot(data = resid)+
    geom_line(aes(x = Index, y = GDP), color = 'steelblue')+
    ggtitle("R?sidus de GDP")+
    theme_minimal()
  
  print(Time_GDP)
  
  Time_I <- ggplot(data = resid)+
    geom_line(aes(x = Index, y = I), color = 'steelblue')+
    ggtitle("R?sidus de I")+
    theme_minimal()
  
  print(Time_I)
}# graphiques des residus distrib et serie

resid_plots(vecm_to_var)

# nouveau mod?le avec trend
johansen <- ca.jo(y, type = "eigen", ecdet = "trend", K = 3, spec = "longrun")
vecm <- cajorls(johansen, r = 1)
summary(vecm$rlm)
coefA(vecm)
coefB(vecm)
vecm_to_var <- vec2var(johansen, r = 1)

serial.test(vecm_to_var)
normality.test(vecm_to_var)

# re estime
johansen <- ca.jo(y, type = "eigen", ecdet = "const", K = 3, spec = "longrun")
vecm <- cajorls(johansen, r = 1)
summary(vecm$rlm)
coefA(vecm)
coefB(vecm)
vecm_to_var <- vec2var(johansen, r = 1) # on re estime avec const on garde cette spÈcification

# predicted VS real values
values <- data.frame(GDP = y$GDP[- c(1:3)], I = y$I[- c(1:3)], GDP_fit = fitted(vecm_to_var)[, 1], I_fit = fitted(vecm_to_var)[, 2])
values$index <- seq(1, length(values$GDP), 1)

ggplot(data = values, aes(x = index))+
  geom_line(aes(y = GDP), color = 'steelblue')+
  geom_line(aes(y = GDP_fit), color = 'red')+
  theme_minimal()

ggplot(data = values, aes(x = index))+
  geom_line(aes(y = I), color = 'steelblue')+
  geom_line(aes(y = I_fit), color = 'red')+
  theme_minimal()

#IRFs
plot(irf(vecm_to_var, response = "GDP"))
plot(irf(vecm_to_var, response = "I"))

