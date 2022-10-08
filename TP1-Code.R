# Travaux pratiques numéro 1 rendu le samedi 29 janvier 2022 

# MOUDILA Marcel , MEUTCHOUNDJOU Thierry

# 2. DONNEES ET PREMIERES MANIPULATIONS

HTA <- read.delim("~/Bureau/statistiques inférentielles/HTA.txt", na.strings="")

View(HTA) 

# 2. 1 MANIPULATION DES VARIABLES

     # DECLARATION DES VARIABLES QUALITATIVES 

# 2)

HTA$HTACONNU <-as.factor(HTA$HTACONNU) # transforme en facteur
levels(HTA$HTACONNU) <- c("non", "oui") #  envoie la premiere modalite (0) vers non et la seconde vers oui


# 3) on transforme les variables SEXE (0,1) et ETNHIE (1,2,3,4) en leurs vrais significations 
#SEXE [Femme=0 , Homme=1] et ETNHIE [Hindou=1, Musulman=2 , Créole=3 , Chinois=4]

HTA$SEXE <- as.factor(HTA$SEXE)
levels(HTA$SEXE) <- c("Femme", "Homme")


HTA$ETHNIE <- as.factor(HTA$ETHNIE)
levels(HTA$ETHNIE) <- c("Hindou", "Musulman" , "Créole" , "Chinois")

table(HTA$ETHNIE)

# 4) creation TASM et TADM
HTA$TASM <- (HTA$TAS2+ HTA$TAS3)/2 #cree la variable TASM 
HTA$TADM <- (HTA$TAD2+ HTA$TAD3)/2


# 5) creation de HTAhnorm
HTA$HTAhnorm <- (HTA$TASM>140)*(HTA$TADM>90)*1

sum(HTA$HTAhnorm)

# 6) creation de HTA

HTA$HTA<- HTA$HTAhnorm | (HTA$HTACONNU=="oui")
HTA$HTA <- HTA$HTA*1
table(HTA$HTA)

table(HTA$HTA)

# 7) on cree les variables IMC et cIMC

HTA$IMC <- HTA$POIDS / (HTA$TAILLE/100)**2

head(HTA$IMC)

HTA$cIMC <- HTA$IMC 
HTA$cIMC <- ifelse(HTA$cIMC <25,"normal",ifelse(HTA$cIMC >=25 & HTA$cIMC < 30,"surpoids","obese"))
levels(HTA$cIMC) <- c("normal","surpoids","obese")
HTA$cIMC <- ordered(HTA$cIMC, levels=c("normal","surpoids","obese"))


# 3 ANALYSE CONJOINTE DE DEUX VARIABLES ET TESTS

# 3.1 CROISEMENT DE DEUX VARIABLES QUALITATIVES

# croisement ethnie et hta 

tab<-table(HTA$ETHNIE, HTA$HTA) # pour le tableau de contingence
addmargins(tab)                 # pour rajouter les effectifs marginaux
round(prop.table(tab,2)*100,1)  # pour les pourcentages arrondi

ETHNIE3<-HTA$ETHNIE[-which(HTA$ETHNIE== "Chinois")]
#levels(HTA$ETHNIE3) <- c("Hindou","Musulman","Creole")

ETHNIE3<-factor(ETHNIE3, labels=c('Hindou','Musulman','Creole'))

t<-table(ETHNIE3,HTA$HTA[-which(HTA$ETHNIE== "Chinois")])
addmargins(t)


summary(t)

# croisement sexe, hta

t1 <-table(HTA$SEXE,HTA$HTA)
round(prop.table(t1)*100,1)

summary(t1)


# 3.2 Comparaison de deux moyennes 

# 3. 2. 1. sexe , tasm

table(HTA$SEXE) # pour les effectifs des femmes et des hommes

boxplot(HTA$TASM~HTA$SEXE,col="red",main= "TASM en fonction du sexe") # description

t.test(HTA$TASM~HTA$SEXE) 

sd(HTA$TASM[which(HTA$SEXE=='Femme')]) # ecart-type
sd(HTA$TASM[which(HTA$SEXE=='Homme')])

# 3. 2. 2 tasm , hta

table(HTA$HTA)

boxplot(HTA$TASM~HTA$HTA,main="TASM selon HTA")

t.test(HTA$TASM~HTA$HTA)
sd(HTA$TASM[which(HTA$HTA==0)])
sd(HTA$TASM[which(HTA$HTA==1)])

# 3. 2. 3 tasm, cIMC

table(HTA$cIMC)

boxplot(HTA$TASM~HTA$cIMC, main="TASM selon cIMC")

cIMC2 <- HTA$cIMC

cIMC2 <- ifelse(cIMC2 == "normal","normal","autres")

levels(cIMC2) <- c("normal","autres")

cIMC2 <- ordered(cIMC2, levels=c("normal","autres"))

table(cIMC2)

boxplot(HTA$TASM~cIMC2, main= "TASM selon cIMC2")

t.test(HTA$TASM~cIMC2)
sd(HTA$TASM[which(cIMC2=="normal")])
sd(HTA$TASM[which(cIMC2=="autres")])

# 3. 3 COMPARAISON DE DEUX MOYENNES APPARIEES

mean(HTA$TAS1)
mean(HTA$TAS2)
t.test(HTA$TAS1, HTA$TAS2, paired=TRUE)

mean(HTA$TAD2)
mean(HTA$TAD3)

L <- HTA$TAD2 - HTA$TAD3
t.test(L)

# 4. ANALYSE DU FICHIER

   # 4. 1 description des variables IMC, groupe d'IMC, et ETHNIE

str(HTA$IMC)
str(HTA$cIMC)
str(HTA$ETHNIE)

summary(HTA$IMC)
table(HTA$cIMC)
table(HTA$ETHNIE)

tab1 <- table(HTA$cIMC)
tab2 <- table(HTA$ETHNIE)

round(prop.table(tab1)*100,1)
round(prop.table(tab2)*100,1)

 # 4. 2 cIMC, sexe

tab3 <- table(HTA$cIMC,HTA$SEXE)
addmargins(tab3)

round(prop.table(tab3)*100,1)

summary(tab3)

 # 4. 3 cIMC et SEDENT2

SEDENT2 <- HTA$SEDENT
# marche plus de 5 miles par semaine (1,oui), sinon (0,non)
SEDENT2 <- factor(SEDENT2,levels = c(0,1),labels = c("non","oui")) 
tab4 <- table(HTA$cIMC,SEDENT2)
addmargins(tab4)

round(prop.table(tab4)*100,0)

summary(tab4)
chisq.test(tab4)$expected


# moyenne IMC , sedent2

table(SEDENT2)

boxplot(HTA$IMC~SEDENT2,main =" IMC selon SEDENT")

t.test(HTA$IMC~SEDENT2)
sd(HTA$IMC[which(SEDENT2=="non")])
sd(HTA$IMC[which(SEDENT2=="oui")])


 # 4. 4 intervalles de confiance 

sum (HTA$HTAhnorm == 1)
prop.test(45,402,0.11)

 # 4. 5  hta, sedent=1

tab5 <- table(HTA$HTA,HTA$SEDENT)
addmargins(tab5)
round(prop.table(tab5)*100,0)
summary(tab5)

# 4. 6 age et tasm

summary(HTA$AGE)
summary(HTA$TASM)

plot(HTA$AGE,HTA$TASM)

# test de nullité de coefficient de corrélation de Pearson : car Chi-deux incorrect 
cor.test(HTA$AGE,HTA$TASM) 

# imc et tadm

plot(HTA$IMC,HTA$TADM)
cor.test(HTA$IMC,HTA$TADM)



