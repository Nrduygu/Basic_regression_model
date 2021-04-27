## Ekonometrik modeller kurulurken öncelikle her bir katsayýnýn ne olduðu iyi anlaþýlmalýdýr. Bu nedenle Beta parametrelerinin oluþturulmasý, hata terimlerine iliþkin kalýntýlarýn hesaplanmasý,
# regresyon modeli kurulduktan sonra standart hatalarýn, varyan-kovaryans matrisinin, t istatistik deðerlerinin ve olaýlýk deðerlerinin manuel olarak elde edilmesi aþaðýda sýrasýyla gösterilmiþtir. 
# Uygulama örneðidir. Kendi veri setinizi R programýnda yükleyerek basit regresyon modeline iliþkin bu aþamalarý hesaplayabilirsiniz.


y = c(24,25,20,14,18,15,11)
x2= c(14,15,14,12,14,16,14)
x3= c(25,36,25,14,12,47,17)
Xmat =cbind(rep(1, length(y)), x2, x3)
Xmat
Betalar = solve(t(Xmat)%*% Xmat)%*%t(Xmat)%*%y
Betalar
### regresyon tahmini  

lm(y~x2+x3)
summary(lm(y~x2+x3))
## kalýntýlarýn elde edilmesi

kalintilar = y-Xmat%*%Betalar
kalintilar
model_LM = lm(y~x2+x3)
model_LM
model_resid = resid(model_LM)
model_resid
model_resid==kalintilar   ### ayný gözlemler için kalýntýlarýn birbirine eþit olup olmadýðý kontrol edilir. 

## standart hata elde etmek

e = sum(kalintilar^2)
Se2 = e/(7-3)    ## e/(n-k)

## varyans kovaryans matrisi 

Var_cov = Se2*solve(t(Xmat)%*%Xmat)
Var_cov

Sbetalar = sqrt(diag(Var_cov))
Sbetalar

## t istatistik deðerleri

t_degerleri= Betalar/Sbetalar 
t_degerleri

## olasýlýk deðerleri tahmin etme

p.values = 2*pt(abs(t_degerleri),(length(y)-ncol(Xmat)), lower = FALSE)  
p.values


