install.packages("foreign")#egkasthistoume to paketo foreign 
library(foreign)#energopoioume to paketo foreign 

dietary<-read.spss(file.choose())#eisagoume to arxeio sthn R
dietary<-as.data.frame(dietary)# to metatrepoume se data frame
head(dietary)#elegxos tou arxeiou
dietary<-dietary[,-c(11,12)]#afairoume tis metablhtes 11,12 

str(dietary)#elegxoyme ton typo ton metablhton

dietary$SEX<-as.factor(dietary$SEX)#metstropi tou sex se factor 
dietary$SMOKSTAT<-as.factor(dietary$SMOKSTAT)# metatropi tou smokstat se factor 
dietary$VITUSE<-as.factor(dietary$VITUSE)#metstropi tou vituse se factor 


levels(dietary$SEX)<-c("Άνδρας","Γυναίκα")#metatropi ton levels tou sex
levels(dietary$SMOKSTAT)<-c("ποτέ","πρώην καπνιστ-ής/-ρια","καπνιστ-ής/-ρια")#metatropi ton levels tou smokstat 
levels(dietary$VITUSE)<-c("ναι-συχνά","ναι-όχι συχνά","όχι")#metatropi ton levels tou vituse




#1
#Kathgorikes
table(dietary$SEX)#vrsiskoune arithmo parathrhsewn sto sex
table(dietary$SMOKSTAT)#vrsiskoune arithmo parathrhsewn sto smokstat 
table(dietary$VITUSE)#vrsiskoune arithmo parathrhsewn sto vituse


prop.table(table(dietary$SEX))#vriskoume pososto parathrhsewn sto sex
prop.table(table(dietary$SMOKSTAT))#vriskoume pososto parathrhsewn sto smokstat
prop.table(table(dietary$VITUSE))#vriskoume pososto parathrhsewn sto vituse

#posotikes
descriptives<-function(x)
{  
  n<-length(x)  
  mu<-mean(x)  
  v<-var(x)  
  sd<-sqrt(v)  
  Q1<-quantile(x,0.25) 
  med<-median(x)  
  Q3<-quantile(x,0.75) 
  kurtosis<-sum((x-mu)^4)/(n*sd^4)-3  
  symmetry<-sum((x-mu)^3)/(n*sd^3) 
  cv<-sd(x)/mean(x)  
  d<-c(mu,sd,Q1,med,Q3,kurtosis,symmetry,cv)  
  names(d)<-c("mean","sd","Q1","median","Q3","kurtosis","symmetry","cv")  
  return(d) 
}#dhmiourgia synarthshs perigrafikon metron

descriptives(dietary$AGE)#ypologismos perigrafikon metron  age
descriptives(dietary$BMI)#ypologismos perigrafikon metron BMI 
descriptives(dietary$CALORIES)#ypologismos perigrafikon metron calories 
descriptives(dietary$FAT)#ypologismos perigrafikon metron fat
descriptives(dietary$FIBER)#ypologismos perigrafikon metron fiber
descriptives(dietary$ALCOHOL)#ypologismos perigrafikon metron alcohol 
descriptives(dietary$CHOLESTE)#ypologismos perigrafikon metron chokleste
descriptives(dietary$BETAPLAS)#ypologismos perigrafikon metron gia betaplas 
descriptives(dietary$RETPLAS)#ypologismos perigrafikon metron retplas




#ΑΝΑ 2


shapiro.test(dietary$AGE)#elegxos kanonikothtas metablhths age
shapiro.test(dietary$BMI)#elegxos kanonikothtas metablhths bmi
shapiro.test(dietary$CALORIES)#elegxos kanonikothtas metablhths calories
shapiro.test(dietary$FAT)#elegxos kanonikothtas metablhths fat
shapiro.test(dietary$FIBER)#elegxos kanonikothtas metablhths fiber
shapiro.test(dietary$ALCOHOL)#elegxos kanonikothtas metablhths alcohol 
shapiro.test(dietary$CHOLESTE)#elegxos kanonikothtas metablhths choleste 
shapiro.test(dietary$BETAPLAS)#elegxos kanonikothtas metablhths betaplas
shapiro.test(dietary$RETPLAS)#elegxos kanonikothtas metablhths retplas






install.packages("sjPlot")#egkasthistoume to paketo sjplot
library(sjPlot)#enrgopoioume paketo sjplot

tab_corr( dietary[,c(1,4,6:12)], corr.method = "spearman", p.numeric = F, triangle = "lower")#dhmiourgia pinaka sysxetisewn


#Provleptika h ermhneytika montela




###########################BETAPLAS#############################################
lmbetaplas <- lm(dietary$BETAPLAS ~ ., data=dietary[,-11])#dhniourgia gramikou monteloy opou h betaplas einai eksartimeni xwris then metablhth 11
summary(lmbetaplas )#dhmioyrgia summary lmbetaplas 




install.packages("car")#egkasthistoume to paketo car
library(car)#enrgopoiume to paketo car

#polisygramrikothta
vif(lmbetaplas)#ypologimsos polisygramikothtas


lmbetaplas <- lm(BETAPLAS ~ ., data = dietary[, -c(6)])#dhniourgia gramikou monteloy opou h betaplas einai eksartimeni xwris then metablhth 6
summary(lmbetaplas )#dhmioyrgia summary lmbetaplas 

par(mfrow = c(1, 2))  #emfanisi grafimatvm dipla dipla 

#normality
plot(lmbetaplas ,which=2)#dhmiourgia Q-Q grafhmatos
shapiro.test((lmbetaplas $residuals))#elgxos kanonikothtas

#omokedastikothta

plot(lmbetaplas$fitted.values, rstandard(lmbetaplas), cex=2)
abline( h=2, col='red',lwd=2)
abline( h=-2, col='red',lwd=2)
#dhmiourgia grafhmatos omoskedastikothas me kokkines grames 

residualPlots(lmbetaplas)#dhniourhia diagrammatvn tvn katalipon toy monteloy



###############################ME log###########################################
dietary1<-dietary[dietary$BETAPLAS>0,]
lmbetaplas <- lm(log(dietary1$BETAPLAS) ~ ., data = dietary1[ , -c(6,11)])#log metasximatismos metablhths 

summary(lmbetaplas )


par(mfrow = c(1, 2))  #emfanisi grafimatvm dipla dipla 

#normality
plot(lmbetaplas ,which=2)#dhmiourgia Q-Q grafhmatos
shapiro.test((lmbetaplas $residuals))#elegxos kanonikothtas 



#omokedastikothta

plot(lmbetaplas$fitted.values, rstandard(lmbetaplas), cex=2)
abline( h=2, col='red',lwd=2)
abline( h=-2, col='red',lwd=2)
#dhmiourgia grafhmatos omoskedastikothas me kokkines grames 

residualPlots(lmbetaplas)#dhniourhia diagrammatvn tvn katalipon toy monteloy


plot(lmbetaplas ,which=4)#dhmiourgia diagramatos cook



dietary1[61, ]#emfanisi xaraktiristikwn parathrhshs 61


dietary <- dietary[-61, ]#afairesh 61 parathrhshs


dietary1<-dietary[dietary$BETAPLAS>0,]
lmbetaplas <- lm(log(dietary1$BETAPLAS) ~ ., data = dietary1[ , -c(6,11)])
summary(lmbetaplas )




#normality
plot(lmbetaplas ,which=2)#exei anafertei panw
shapiro.test((lmbetaplas $residuals))#exei anafertei panw


#omokedastikothta

plot(lmbetaplas$fitted.values, rstandard(lmbetaplas), cex=2)
abline( h=2, col='red',lwd=2)
abline( h=-2, col='red',lwd=2)

#exei anafertei panw

residualPlots(lmbetaplas)#exei anafertei panw


#########RETPLAS################################################################

lmretplas <- lm(dietary$RETPLAS ~ ., data=dietary[,-12])#dhniourgia gramikou monteloy opou h retplas einai eksartimeni xwris then metablhth 12
summary(lmretplas)#exei anafertei panw


#polisygramrikothta
vif(lmretplas)

#exei anafertei panw

lmretplas <- lm(dietary$RETPLAS ~ ., data = dietary[, -c(6)])#afairoume thn 6h metablhth
summary(lmretplas )



par(mfrow = c(1, 2)) #exei anafertei panw
#normality
plot(lmretplas ,which=2)#exei anafertei panw
shapiro.test((lmretplas $residuals))#exei anafertei panw


#omokedastikothta

plot(lmretplas$fitted.values, rstandard(lmretplas), cex=2)
abline( h=2, col='red',lwd=2)
abline( h=-2, col='red',lwd=2)

#exei anafertei panw

residualPlots(lmretplas)#exei anafertei panw



plot(lmretplas ,which=4)#exei anafertei panw

dietary <- dietary[-61, ]#exei anafertei panw


lmretplas <- lm(dietary$RETPLAS ~ ., data=dietary[,-c(6,12)])#afairoume thn metablhth 6,12 efoson afairesame parathrhsh 61

#normality
plot(lmretplas ,which=2)#exei anafertei panw
shapiro.test((lmretplas $residuals))#exei anafertei panw


#omokedastikothta

plot(lmretplas$fitted.values, rstandard(lmretplas), cex=2)
abline( h=2, col='red',lwd=2)
abline( h=-2, col='red',lwd=2)

#exei anafertei panw

plot(lmretplas ,which=4)#exei anafertei panw



#2os asthenhs
dietary <- dietary[-19, ]#afairoyme parathrhsh 19
lmretplas <- lm(dietary$RETPLAS ~ ., data=dietary[,-c(6,12)])#afairoume thn metablhth 6,12 efoson afairesame parathrhsh 19
summary(lmretplas )#exei anafertei panw

#3os asthenhs
plot(lmretplas ,which=4)#exei anafertei panw
dietary <- dietary[-295, ]#afairoune parhrhsh 295
lmretplas <- lm(dietary$RETPLAS ~ ., data=dietary[,-c(6,12)])#afairoume thn metablhth 6,12 efoson afairesame parathrhsh 295

summary(lmretplas )#exei anafertei panw


#(παραρτημα)
#SEX
par(mfrow = c(2, 2))  

hist(dietary$BETAPLAS[dietary$SEX == "Άνδρας"],
     main = "BETAPLAS - Άνδρες", col = "blue")
#dhmiourgia istogrammatos


hist(dietary$BETAPLAS[dietary$SEX == "Γυναίκα"],
     main = "BETAPLAS - Γυναίκες", col = "pink")
#dhmiourgia istogrammatos

hist(dietary$RETPLAS[dietary$SEX == "Άνδρας"],
     main = "RETPLAS - Άνδρες", col = "blue")
#dhmiourgia istogrammatos

hist(dietary$RETPLAS[dietary$SEX == "Γυναίκα"],
     main = "RETPLAS - Γυναίκες", col = "pink")
#dhmiourgia istogrammatos

wilcox.test(dietary$RETPLAS~dietary$SEX)#mh parametrikos elgxos tou retplas stis 2 kathgories toy sex
wilcox.test(dietary$BETAPLAS~dietary$SEX)#mh parametrikos elgxos tou betaplas stis 2 kathgories toy sex


par(mfrow = c(1, 2))  
boxplot(dietary$RETPLAS ~ dietary$SEX,
        xlab = "Φύλο",
        ylab = "RETPLAS",
        col = c("blue", "pink"))

boxplot(dietary$BETAPLAS ~ dietary$SEX,
        xlab = "Φύλο",
        ylab = "BETAPLAS",
        col = c("blue", "pink"))

#dhmioyrgia boxplot gia to sex se betaplas kai retplas

#SMOKE

anovasmok1=aov(dietary$RETPLAS~dietary$SMOKSTAT)#elegxos anova tou retplas stis kathgories tou smokstat
anovasmok2=aov(dietary$BETAPLAS~dietary$SMOKSTAT)#elegxos anova tou betaplas stis kathgories tou smokstat

par(mfrow = c(1, 2))  
hist(anovasmok1$res,main = "RETPLAS ",col="orange")#dhmioyrgia istorgramataos
hist(anovasmok2$res,main = "BETAPLAS ",col="purple")#dhmioyrgia istorgramataos

kruskal.test(dietary$RETPLAS~dietary$SMOKSTAT)#elegxos kruskal tou retplas stis kathgories tou smokstat
kruskal.test(dietary$BETAPLAS~dietary$SMOKSTAT)#elegxos kruskal tou betplas stis kathgories tou smokstat

boxplot(dietary$RETPLAS ~ dietary$SMOKSTAT,
        xlab = "Καπνιστικές συνήθειες ",
        ylab = "RETPLAS",
        col = c("green", "yellow", "red"))


boxplot(dietary$BETAPLAS ~ dietary$SMOKSTAT,
        xlab = "Καπνιστικές συνήθειες ",
        ylab = "BETAPLAS",
        col = c("green", "yellow", "red"))

#dhmioyrgia boxplot gia to SMOKSTAT se betaplas kai retplas

#Vituse
anovavit1=aov(dietary$RETPLAS~dietary$VITUSE )#elegxos anova tou retplas stis kathgories tou vituse
anovavit2=aov(dietary$BETAPLAS~dietary$VITUSE )#elegxos anova tou betaplas stis kathgories tou vituse


par(mfrow = c(1, 2))  
hist(anovavit1$res,main = "RETPLAS ",col="orange")#dhmioyrgia istorgramataos
hist(anovavit2$res,main = "BETAPLAS ",col="purple")#dhmioyrgia istorgramataos

kruskal.test(dietary$RETPLAS~dietary$VITUSE)#elegxos kruskal tou retplas stis kathgories tou vituse
kruskal.test(dietary$BETAPLAS~dietary$VITUSE)#elegxos kruskal tou betaplas stis kathgories tou vituse

boxplot(dietary$BETAPLAS ~ dietary$VITUSE,
        xlab = "Χρήση βιταμινών ",
        ylab = "BETAPLAS",
        col = c("red", "yellow", "green"))

boxplot(dietary$RETPLAS ~ dietary$VITUSE,
        xlab = "Καπνιστικές συνήθειες ",
        ylab = "RETPLAS",
        col = c("red", "yellow", "green"))



#dhmioyrgia boxplot gia to vituse se betaplas kai retplas


