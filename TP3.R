poumon <- read.delim2("~/Bureau/TP3 statistiques inferentielles/poumon.txt", na.strings="")
# TP2 et TP3
# Auteur : MOUDILA Marcel

#2.2 Creation de IMC

poumon$IMC <- poumon$POIDS/((poumon$TAILLE_EN_M)**2)

# 3. 1 Visualisation des nuages de points

plot(poumon$AGE,poumon$TLCO)         
plot(poumon$TAILLE_EN_M,poumon$TLCO)
plot(poumon$POIDS,poumon$TLCO)
plot(poumon$IMC,poumon$TLCO)

#3. 2 Visul

poumon2 <- poumon[,c("TLCO","origine","AGE","TAILLE_EN_M","POIDS","IMC")]
pairs(poumon2)


# 3. 3

modelTLCO <- lm(TLCO~AGE, data=poumon)
summary(modelTLCO)
plot(poumon$AGE,poumon$TLCO)
abline(modelTLCO)

# COMMENTAIRE : beta(0) = 45.35 ; beta = (-0.30) ; quand l'age prend une unite, TLCO diminue de 0.30
# quand l age augmente d une unite , TLCO diminue de 0.30
# la p-value < 0.001 

# 4 

pred.frame<- data.frame(AGE=20:95)
pc<- predict(modelTLCO, interval="confidence",newdata=pred.frame)
pp<- predict(modelTLCO, interval="prediction",newdata=pred.frame)

# valeur de TLCO attendue pour une personne de 40 ans
pred.frame2<- data.frame(AGE=40)
TLCO_40<- predict(modelTLCO, interval="confidence",newdata=pred.frame2)
TLCO_40

# on pouvait aussi utiliser la commande > pc[21,]

plot(poumon$AGE,poumon$TLCO)
abline(modelTLCO)
# maintenant on trace l intervalle de confiance : c est l intervalle de confiance de la droite de regression
matlines(pred.frame, pc[,2:3], lty=c(2,2), col="blue")
# maintenant on trace l intervalle de prediction : c est l intervalle de prediction des nouvelles valeurs observees
matlines(pred.frame, pp[,2:3], lty=c(3,3), col="red")
legend("topleft",c("confiance","prediction"),lty=c(2,3), col=c("blue","red"))

pp2<- predict(modelTLCO, interval="prediction",newdata=pred.frame,level = 0.99)

matlines(pred.frame, pc[,2:3], lty=c(2,2), col="blue")
matlines(pred.frame, pp2[,2:3], lty=c(3,3), col="red")
legend("topleft",c("confiance","prediction"),lty=c(2,3), col=c("blue","red"))

# 5
s <- summary(modelTLCO)
s$coef
s$coef[1,1]
s$coef[2,1]

prev<- s$coef[1,1]+s$coef[2,1]*pred.frame$AGE


# 6

s <- summary(modelTLCO)
res <- s$res
plot(res,poumon$AGE)
plot(res,poumon$TLCO)
shapiro.test(res)
hist(res)

# 7
resstud <-rstudent(modelTLCO)
plot(poumon$AGE, resstud)
trop <- which(abs(resstud)>2.5)
trop

out<-poumon[trop,]
out[c(1:7,13)]

trop2 <- which(abs(resstud)>3)
trop2

# 8

tr <- sample(1:nrow(poumon),225)
train <- poumon[tr,]   #  echantillon d apprentissage
test <- poumon[-tr,]   # echantillon de validation

model<-lm(TLCO~AGE , data=train)
summary(model)
pp3<- predict(model, interval="prediction",newdata=test)


# 10



# 11

model3 <- lm(TLCO~Sexe, data = poumon)
summary(model3)
plot(poumon$Sexe,poumon$TLCO)
abline(model3)


# 12

poumon$cIMC <- ifelse(poumon$IMC < 25,"normal",ifelse(poumon$IMC >= 25 & poumon$IMC < 30 ,"surpoids","obese"))

model4 <- lm(TLCO~cIMC,data = poumon)
summary(model4)

# regression lineaire multiple

#4.1
poumon3 <- poumon[,c("TLCO","origine","Sexe","AGE","TAILLE_EN_M","POIDS","BMI")]
pairs(poumon3,pch = 3, col = "blue", main = "")

#4.2

test1 <-lm(TLCO~origine,data=poumon)
summary(test1) #  p-value: 1.149e-05 < 0.1
                  

test1 <-lm(TLCO~Sexe,data=poumon)
summary(test1) #  p-value: < 2.2e-16 < 0.1

test1 <-lm(TLCO~AGE,data=poumon)
summary(test1) # p-value: < 2.2e-16 < 0.1

test1 <-lm(TLCO~TAILLE_EN_M,data=poumon)
summary(test1) # p-value: < 2.2e-16 < 0.1

test1 <-lm(TLCO~POIDS,data=poumon)
summary(test1) #  p-value: 1.134e-11 < 0.1

test1 <-lm(TLCO~BMI,data=poumon)
summary(test1) #  p-value: 0.05203 < 0.1

#4.3

modelTLCO <- lm(TLCO~AGE+origine+Sexe+TAILLE_EN_M+POIDS+BMI, data=poumon)
summary(modelTLCO)

  # non les p-value ne correspondent pas avec celles obtenues en regression simple


# 4.4
 
# on enleve la variable POIDS car elle a p-value (> 0.05) plus grand
modelTLCO <- lm(TLCO~AGE+origine+Sexe+TAILLE_EN_M+BMI, data=poumon)
summary(modelTLCO)

# on enleve la variable BMI car elle a p-value (> 0.05) plus grand
modelTLCO <- lm(TLCO~AGE+origine+Sexe+TAILLE_EN_M, data=poumon)
summary(modelTLCO)

# exemple d interpretation pour age : quand l origine et sexe sont constants
# si on augmente d une unite l age, la TLCO diminue en moyenne de 0.21

# on enleve la variable origine 
modelTLCO <- lm(TLCO~AGE+Sexe+TAILLE_EN_M, data=poumon)
summary(modelTLCO)


#4.5 

 # les residus

s <- summary(modelTLCO)
res <- s$res
res

 # nuage des points
layout(matrix(1:4,2,2))
plot(modelTLCO)

modelTLCO <- lm(TLCO~AGE+Sexe+TAILLE_EN_M, data=poumon)
summary(modelTLCO)

poumon4 <- poumon[-217,] # j ai teste parmi les individus 137,225,306, 99, et 217
                        # il n y a pas signification de modification des 
                        # coefficients de la regression dans le modele qui suit

modelTLCO <- lm(TLCO~AGE+Sexe+TAILLE_EN_M, data=poumon4)
summary(modelTLCO)

# on garde toutes les variables 

modelTLCO <- lm(TLCO~AGE+Sexe+TAILLE_EN_M, data=poumon)
summary(modelTLCO)

#4.6 

#4.7

tr <- sample(1:nrow(poumon),225)
train <- poumon[tr,]
test <- poumon[-tr,]
model<-lm(TLCO~AGE+Sexe+TAILLE_EN_M, data=train)
pp<- predict(model, interval="prediction",newdata=test)
pp


good<-(pp[,2]<test$TLCO)*(pp[,3]>test$TLCO)
indic1<-sum(good)/length(good)
SCE<-sum((pp[,1]-mean(test$TLCO))^2)
SCT<-sum((mean(test$TLCO)-test$TLCO)^2)
SCR<-sum((pp[,1]-test$TLCO)^2)
indic2<-SCE/SCT

#4.8 
X <- 0
Y <- 0
for (i in 1:100) {
  tr <- sample(1:nrow(poumon),225)
  train <- poumon[tr,]
  test <- poumon[-tr,]
  model<-lm(TLCO~AGE+Sexe+TAILLE_EN_M, data=train)
  pp<- predict(model, interval="prediction",newdata=test)
  
  
  good<-(pp[,2]<test$TLCO)*(pp[,3]>test$TLCO)
  indic1<-sum(good)/length(good)
  SCE<-sum((pp[,1]-mean(test$TLCO))^2)
  SCT<-sum((mean(test$TLCO)-test$TLCO)^2)
  SCR<-sum((pp[,1]-test$TLCO)^2)
  indic2<-SCE/SCT
  
  v1 <- c(X,indic1)
  X<- v1
  v2 <- c(Y,indic2)
  Y <- v2
}
vect1 <- X[2:100]
vect1
vect2 <- Y[2:100]
vect2
boxplot(vect1,vect2,names = c("TLCO dans pp","R²"))


 
  

