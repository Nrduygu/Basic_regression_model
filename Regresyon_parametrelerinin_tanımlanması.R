## Ekonometrik modeller kurulurken �ncelikle her bir katsay�n�n ne oldu�u iyi anla��lmal�d�r. Bu nedenle Beta parametrelerinin olu�turulmas�, hata terimlerine ili�kin kal�nt�lar�n hesaplanmas�,
# regresyon modeli kurulduktan sonra standart hatalar�n, varyan-kovaryans matrisinin, t istatistik de�erlerinin ve ola�l�k de�erlerinin manuel olarak elde edilmesi a�a��da s�ras�yla g�sterilmi�tir. 
# Uygulama �rne�idir. Kendi veri setinizi R program�nda y�kleyerek basit regresyon modeline ili�kin bu a�amalar� hesaplayabilirsiniz.


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
## kal�nt�lar�n elde edilmesi

kalintilar = y-Xmat%*%Betalar
kalintilar
model_LM = lm(y~x2+x3)
model_LM
model_resid = resid(model_LM)
model_resid
model_resid==kalintilar   ### ayn� g�zlemler i�in kal�nt�lar�n birbirine e�it olup olmad��� kontrol edilir. 

## standart hata elde etmek

e = sum(kalintilar^2)
Se2 = e/(7-3)    ## e/(n-k)

## varyans kovaryans matrisi 

Var_cov = Se2*solve(t(Xmat)%*%Xmat)
Var_cov

Sbetalar = sqrt(diag(Var_cov))
Sbetalar

## t istatistik de�erleri

t_degerleri= Betalar/Sbetalar 
t_degerleri

## olas�l�k de�erleri tahmin etme

p.values = 2*pt(abs(t_degerleri),(length(y)-ncol(Xmat)), lower = FALSE)  
p.values


