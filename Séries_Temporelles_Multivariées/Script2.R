###################################  Projet Multivaré ###################################
# 2 . Explication d'une expension monétaire en économie ouverte (interet/PIB/Taux de Change /BC/Domestic reterns/Investissement) :


# Import de la base
library(readxl)
library(zoo)
library(fUnitRoots)
library(urca)
library(vars)
library(MTS)
library(tsDyn)
library(Quandl)

#df <- read_excel("Base_multivar.xls")


# L'objectif principal de cette application est de reproduire la figure 2 de Gali (1999) de l'AER (utilise VAR avec restrictions de LT, à la Blanchard et Quah). On va faire la même chose avec les données mises à jour (on ne s'arrete pas à 99).
    # 1) analyse de données
#     - Graphiques
#     - Tests de racines unitaires
#     - Transformation de données : les premières différences des données
    # 2) estimation du modèle VAR sous sa forme réduite
    # 3) imposition de restrictions de LT à la Blanchard et Quah pour obtenir un VAR structurel
    # 4) interprétation des résultats du SVAR obtenu dans la partie 3).
    # 5) Présentation des IRF cumulatifs du modèle SVAR dans la partie 3).
    # 6) Analyse de la FEVD Pour un examen plus approfondi de notre SVAR.

# Nous téléchargeons les données de FRED à l'aide de la fonction Quandl.
# Nous traçons ensuite les séries chronologiques de ces deux ensembles de données pour avoir une idée de ce à quoi ces ensembles de données ressemblent visuellement.


Quandl.api_key("xLQnFvg45KANa2ZjVjKU") # données trimestrielles

GDP <- Quandl("FRED/GDPC1",type="zoo",start_date="1950-01-01", end_date="2000-12-31") # GDP
I <- Quandl("FRED/INTDSRUSM193N",type="zoo",collapse = "quarterly",start_date="1950-01-01", end_date="2000-12-31") # Interest rate

    #,collapse='quaterly' : précise qu'il faut prendre données trim quand mens ET trim




    # 1) analyse de données



# Transformation en log des données en niveau
lGDP<- log(GDP)*100
lI <- log(I)*100

# Plot des variables avant et après log
par(mfrow=c(2,2),cex=.6,mar=c(4,4,2,1))
plot(GDP, main="Gross Domestic Product")
plot(I, main="Interest Rates")
plot(lGDP, main="Log (GDP)")
plot(lI, main="Log (I)")

# Test de stationnarité des variables
library(urca)
lGDP.urers1 <- ur.ers(lrLP,type="P-test",model="trend")
summary(lGDP.urers1)
lI.urers1 <- ur.ers(lI,type="P-test",model="trend")
summary(lI.urers1)   # séries non statio, accepte H0

# On différencie les séries
dGDP <- diff(GDP)
dI <- diff(I)
dlGDP <- diff(lGDP)
dlI <- diff(lI)

# On plot
par(mfrow=c(2,2), cex=.6, mar=c(4,4,2,1))
plot(lGDP, main="(log) GDP")
plot(lI, main= "(log) I")
plot(dlGDP, main="(diff) GDP")
plot(dlI, main= "(diff) I") #valeur aberrante à la fin

# On véririfie I(1)
dlGDP.urers1 <- ur.ers(dlGDP,type="P-test",model="trend")
summary(dlGDP.urers1)
dlI.urers1 <- ur.ers(dlI,type="P-test",model="trend")
summary(dlI.urers1)  # rejet H0




    # 2) Estimation d'un VAR stationnaire (Etapes : nb de retards, stabilité du modèle, tests des résidus)




# On combine les deux séries chronologiques en une seule matrice de données
GDPs <- dlGDP
Is <- dlI
y <- cbind(GDPs, Is)
plot(y)
# Gini (1999) : le modèle bivarié est estimé à l'aide de données trimestrielles américaines couvrant la période 1948:1-1994:4.


# On centre les données
#apply(y, 2, mean)
y <- sweep(y, 2, apply(y, 2, mean))
y <- window(y,start="1950 Q1", end="2000 Q4")
plot(y)

library(vars)
VARselect(y, lag.max=12)  #commencer par retard plus perit=parcimonie puis on augmente jusqu'à ce qu'il n'y ait plus de probleme lié aux résidus.

# VAR forme réduite 2 retards
m2=vars::VAR(y, p=2, type="const")  #2 retards
    # stabilité
roots(m2) # modèle stable
    # normalité
m2.norm <- normality.test(m2,multivariate.only=TRUE)
m2.norm$jb.mul
#plot(m2.norm)  # résidus normaux
    # Autocorrélation (H0 : absence autoco)
Serial1 <- serial.test(m2,lags.pt=5, type="PT.asymptotic")
Serial1
    # Hétéroscédasticité (H0 : rés homoscédastiques)
arch1 <- arch.test(m2,lags.multi = 5)
arch1$arch.mul  # tout est bon
#plot(arch1)


# VAR
myVAR <- vars::VAR(y, ic="HQ", lag.max=12, type="const")
summary(myVAR)


# Notre objectif est d'étudier les effets des chocs technologiques (offre) et des chocs de demande sur les heures travaillées. Choc de demande à CT (pas d'effets à LT) et Offre : LT.




    # 3) Imposition de restrictions
    # 4) interprétation des résultats du SVAR obtenu dans la partie 3).




# On utilise l'approche à la Blanchard et Quah pour obtenir un SVAR où nous imposons la condition qur les chocs de demande n'affectent pas la production réelle par heure à long terme.
    # ligne 1 colonne 2 (=choc de demande à long terme) de la matrice des effets cumulatifs sera imposé à 0 sur les variables. Pour que ça soit compatible avec théorie éco : les chocs d'offre càd tehcnologiques ont un effet à CT sur la productivité car les prix sont rigides mais à LT la monnaie est neutre et donc il n'y a pas d'effet à LT d'un choc d'offre sur la productivité.


mySVAR <- BQ(myVAR)  # on passe de forme réduite à modèle structurel
summary(mySVAR)

# -------- IRFs non cumulés (standards)
myIRF <- irf(mySVAR, n.ahead=12, ci=.9) # 12 : 3 ans d'horizon de prévision

# Normalisation : changer les signes des irf de choc non technologiques pour qu'ils montrent les effets d'un choc positif et non d'un choc négatif
myIRF$irf[[2]] <- myIRF$irf[[2]]
myIRF$Lower[[2]] <- myIRF$Lower[[2]]
myIRF$Upper[[2]] <- myIRF$Upper[[2]]





    # 5) Présentation des IRF cumulatifs du modèle SVAR dans la partie 3).






# --------  IRFs cumulés
myIRF.c <- irf(mySVAR, n.ahead=12, ci=.9, cumulative=TRUE) #ci=.9 : 90% intervalle de confiance

# Normalisation : changer les signes des IRF de choc non technologiques pour qu'ils montrent les effets d'un choc positif et non d'un choc négatif
myIRF.c$irf[[2]] <- myIRF.c$irf[[2]]
myIRF.c$Lower[[2]] <- myIRF.c$Lower[[2]]

myIRF.c$Upper[[2]] <- myIRF.c$Upper[[2]]

myIRF.c$irf[[1]] <- myIRF.c$irf[[1]]
myIRF.c$Lower[[1]] <- myIRF.c$Lower[[1]]
myIRF.c$Upper[[1]] <- myIRF.c$Upper[[1]]



# Obtenir un graphique avec IRF cumulées pour dlrLP mais un IRF standard pour TH
par(mfrow=c(2,2), cex=.6, mar=c(4,4,2,1))



source("plotIRF.r")
plotIRF(myIRF.c, lwd=2.5, ask=FALSE, vnames="GDPs", vlabels="GDP", slabels=c("technology shock","non-technology shock"))
plotIRF(myIRF.c, lwd=2.5, ask=FALSE, vnames="Is", vlabels="I", slabels=c("technology shock","non-technology shock"))

    # 1er choc : techno et 2è de demande
    # choc (positif) techno = augmente productivité, nb heures baisse, mais revient à la normale rapidement (effet de très CT, récation non sign sauf à l'impact)  -> quand productivité augmente on a besoin de travailler moins car plus efficace !! d'où les relations et les effets qu'on observe.
    # choc de demande = effet CT sur productivité et effet positif sur nb d'heures travaillées
    # donc réactions compatibles avec identification qu'on souhaite avoir dans modèle VAR càd distinguer entre choc d'offre et de demande. Aussi, compatible avec théorie économique.
    # à part graph(1,2) les autres ne sont pas contraints donc on laisse parler les données et c'est là qu'on voit si c'est cohérent avec théorie éco. Ici oui.
    # si théorie non validée (quand effet inverse mais si juste on observe un truc neutre on dit que c'est bon) : signifie que la restriction imposée ne marche pas.




    # 6) Analyse de la FEVD pour un examen plus approfondi de notre SVAR.




# FEVD
plot(fevd(mySVAR, n.ahead=40), addbars=4, col=c("red3","royalblue3"))
    # le nombre d'heures est expliqué principalement par le nb d'heures








##########################
########  Resctrictions à definir à la main
##########################

    # Données trim : PIB, inflation, taux d'interet
y <- ts(log(dat$RGDP[-1]), start = c(1990, 2), freq = 4)  # GDP
i.tmp <- (1 + (dat$repo/100))^0.25 - 1  # Interest Rate
i <- ts(i.tmp[-1] * 100, start = c(1990, 2), freq = 4)


data <- cbind(y,pi,i)
colnames(data) <- c("y", "pi", "i")  # y, Inflation, TxInt
plot.ts(data)

# sur cette période le log(PIB) peut contenir une tendance temporelle déterministe plutôt qu'une racine unitaire. On fait donc des tests :
lin.mod <- lm(y~time(y))
lin.trend <- lin.mod$fitted.values
linear <- ts(lin.trend, start = c(1990, 2), frequency = 4)
lin.cycle <- y - linear

adf.lin <- ur.df(lin.cycle, type = "none", selectlags = c("AIC"))
summary(adf.lin) # vble stationnaire à 5%

par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot.ts(lin.cycle)
    # dans le graphique on peut voir que le PIB a connu une baisse en 1992 et une section plate à la fin en 2008. On inclut une variable muette exogène pour ces périodes.
    # utiliser test qui prend en compte les points aberrants, atypiques et qui date les changements de régime (pas le cas de ADF) : test de Show idéal !
    # test ADF : détecte statio MALGRÉ les points abérrants.



##########################
########  Création de variables dummies
##########################



# 1 pour 92 et une pour 2008 :
dum92 <- rep(0, length(y))
dum92[11] <- 1
dum08 <- rep(0, length(y))
dum08[75] <- 1

dum92 <- ts(dum92, start = c(1990, 2), freq = 4)
dum08 <- ts(dum08, start = c(1990, 2), freq = 4)
dum <- cbind(dum92, dum08)
colnames(dum) <- c("dum92", "dum08")
    # 11è obs dum$dum92 : pic
    # 3 types de points abérrants : pic/blip? (monte puis revient à la normale), break.. voir cours de Darnée types d'outliers.

# Notez aussi qu'au lieu d'utiliser le cycle linéaire, nous allons utiliser... ça permet de montrer  [...]

dum92 <- rep(0, length(y))
dum92[11] <- 1
dum08 <- rep(0, length(y))
dum08[75] <- 1

dum92 <- ts(dum92, start = c(1990, 2), freq = 4)
dum08 <- ts(dum08, start = c(1990, 2), freq = 4)
dum <- cbind(dum92, dum08)
colnames(dum) <- c("dum92", "dum08")


# Nous incluons à la fois un trend et une constante
var.est1 <- VAR(data, p = 1, type = "both", season = NULL, exog = dum)
summary(var.est1)




##########################
########  Création de matrice Cholesky à la main
##########################



# Matrice A : matrice triangulaire (restrictions récursives)
    # mettre à 0 le coef sur lequel on met la restriction, ou alors on met 2.5 fin ce qu'on veut -> restrictions de CT
a.mat <- diag(3)
diag(a.mat) <- NA # pas de restrictions qd 'NA'
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[3, 2] <- NA
print(a.mat)

# Matrice B : matrice diagonale
b.mat <- diag(3)
diag(b.mat) <- NA
print(b.mat)

svar.one <- SVAR(var.est1,Amat=a.mat, Bmat=b.mat, max.iter=10000,hessian=TRUE)
svar.one  # on voit les relations po ou neg entre les différentes variables


# IRF
par(mfrow=c(3,3))
one.int <- irf(svar.one, response = "i", impulse = "i",
               n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(one.int)

one.gdp <- irf(svar.one, response = "y", impulse = "i",
               n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(one.gdp)

one.inf <- irf(svar.one, response = "pi", impulse = "i",
               n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(one.inf) # taux d'interet impacte par PIB et inflation, mais lui n'impacte pas PIB et infl avec retards. Donc choc de politique monétaire. On fait un choc sur tx int et on voit réactions des autres variables (choc tx int = baisse PIB et infl).


# personnaliser les graphiques, changer les paramètres des modèles etc.


# ----- DOSSIER : dernière semaine fevrier
    # DOSSIER : (environ 15-20 pages, max 25)
        #- tableaux propores, graphiques commentés, résultats pas beaux en annexe
        #- script R et données
    # ORAL : 5/10 mins (max 10') et 15' de discussion
        #- étape du dossier
        #- interprétations des résultats trèèès importantes
