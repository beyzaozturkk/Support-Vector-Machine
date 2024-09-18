#####SUPPORT VECTOR MACHINE####
cancer <- read.csv('breast_cancer.csv' , header = TRUE , sep = "," , dec = ".")
View(cancer)

#ID (Kimlik numarası): Hastanın kimlik numarası.
#Diagnosis (Teşhis): Tümör tipini belirtir (M = Malignant = Kötü huylu, B = Benign = İyi huylu).
#Aşağıdaki 10 temel özellik, her biri için ortalama, standart hata ve en kötü (maksimum) değer olmak üzere 3 farklı hesaplama içerir. Toplamda 30 değişken bu şekilde elde edilir:
  
#Radius (Yarıçap): Çevre uzunluğunun (perimeter) yarıçapı (ortalama yarıçap).
#Texture (Doku): Gri tonlarındaki varyasyonun standart sapması.
#Perimeter (Çevre): Hücre çekirdeği çevresi.
#Area (Alan): Hücre çekirdeğinin alanı.
#Smoothness (Pürüzsüzlük): Hücre sınırlarının pürüzsüzlüğü (standart sapması).
#Compactness (Sıkılık): Perimeter² / Alan - 1.0 formülü ile hesaplanır.
#Concavity (Çukurluk): Hücre sınırlarının çukur kısımlarının derecesi.
#Concave Points (Çukur Noktalar): Hücre sınırında bulunan çukur noktaların sayısı.
#Symmetry (Simetri): Hücrelerin simetrisi.
#Fractal Dimension (Fraktal Boyut): "Bölünme" boyutu (1'den az olan fraktal boyut ölçümü).
#Bu değişkenler, tümörlerin iyi huylu mu kötü huylu mu olduğunu tahmin etmek için kullanılır.

str(cancer) #veri türlerine bakmak için
# bağımlı değişken karakter olarak gelirse faktör kullanıldığından faktöre çevirmemiz gerekir

varNames <- names(cancer)
cancer<- cancer[, -which(varNames == 'X' | varNames == 'id')]
varNames <- names(cancer)

variables<- c("diagnosis" ,  "radius_mean" ,"texture_mean"  ,         
                  "perimeter_mean"  ,"area_mean" ,"smoothness_mean" ,       
                  "compactness_mean" ,"concavity_mean", "concave.points_mean" ,
                  "symmetry_mean" ,"fractal_dimension_mean")

selectedvariables <- cancer[variables]
View(selectedvariables)

##Görselleştirme 

# genel bir fikir edinmek adına birkaç değişken için bakmak yeterli
# verilerin dağılımına bakarsak kernel fonksiyonu olarak radial ya da linear kullanabiliriz

plot(selectedvariables$radius_mean , selectedvariables$texture_mean , pch = 19,
     col = c('blue' , 'orange')[as.factor(selectedvariables$diagnosis)] )


plot(selectedvariables$area_mean , selectedvariables$perimeter_mean , pch = 19,
     col = c('blue' , 'orange')[as.factor(selectedvariables$diagnosis)] )

plot(selectedvariables$smoothness_mean, selectedvariables$compactness_mean , pch = 19,
     col = c('blue' , 'orange')[as.factor(selectedvariables$diagnosis)] )

plot(selectedvariables$radius_mean, selectedvariables$compactness_mean , pch = 19,
     col = c('blue' , 'orange')[as.factor(selectedvariables$diagnosis)] )

#Görsellere bakarsak model kurarken linear ya da radial kullanabiliriz.

#### Model Oluşturma

library(e1071)


### Diagnosis faktöre çevrilmeli 

selectedvariables$diagnosis <- as.factor(selectedvariables$diagnosis)
class(selectedvariables$diagnosis)


table(selectedvariables$diagnosis) # B ve M huylu tümörlerin frekansları


#Veri setini trainset ve test set olarak bölmek

set.seed(125)
splited<- sample(1:nrow(selectedvariables) , size = 0.8*nrow(selectedvariables))

trainSet <- selectedvariables[ splited,]
testSet <- selectedvariables[-splited,]

table(trainSet$diagnosis) #trainSet için bağımlı değişken frekansları
table(testSet$diagnosis) #testSet için bağımlı değişken frekansları

head(trainSet)

model_Linear <- svm( diagnosis ~ . , data  = trainSet , kernel = 'linear' )
model_Radial <- svm( diagnosis ~ . , data  = trainSet , kernel = 'radial' )

model_Linear
summary(model_Linear)
model_Radial$coefs #coef olarak alınan değerler support vektor değerleridir
model_Linear$coefs
model_Linear$decision.values

## Modeller üzerinden Tahminler


predict_Linear <- predict(model_Linear , testSet)
predict_Radial <- predict(model_Radial , testSet)

library(caret)

# tahmin degerler ile gerçek değerler kullanılarak confusion matrix oluşturmak için
confusionMatrix(predict_Linear , testSet$diagnosis)
confusionMatrix(predict_Radial , testSet$diagnosis)

confusionMatrix(predict_Linear , testSet$diagnosis , mode = "prec_recall")
confusionMatrix(predict_Radial , testSet$diagnosis , mode = "prec_recall")



### Olasılık Turunden Tahmin Sonuçları

#diagnosis değiskeni içindeki M ve B değerleriden hangisinin olma olasılığı nedir?
#sınır genellikle 0.5'tir. optimum cutoff bulunarak bu sınır değiştirilebilir

model_Linear_Prob <- svm( diagnosis ~ . , data  = trainSet , kernel = 'linear' , probability = T )
model_Radial_Prob <- svm( diagnosis ~ . , data  = trainSet , kernel = 'radial' , probability = T )
predict_Linear_Prob <- predict(model_Linear_Prob , testSet , probability = T)
predict_Radial_Prob <- predict(model_Radial_Prob , testSet , probability = T)

predict_Linear_Prob

predLinearProbs <- attr(predict_Linear_Prob , 'probabilities')## probability değerlerini almak için
predLinearProbs
class(predLinearProbs)

predRadialProbs <- attr(predict_Radial_Prob , 'probabilities')## probability değerlerini almak için
class(predRadialProbs)

attr(predict_Linear_Prob , "probabilities")



## Sonuçların Görselleştirilmesi

##Linear için,

plot(model_Linear , trainSet , radius_mean ~ texture_mean)

##Çarpı noktalar support vectörlerdir
#bu support vektörler ikisinin çarpıştığı yerlerde de var bunlar miss(kayıp) class olur

plot(model_Linear , testSet , radius_mean ~ texture_mean)

plot(modelLinear , testSet , perimeter_mean ~ area_mean)

##Radial icin
plot(model_Radial , trainSet , radius_mean ~ texture_mean)
#çarpı noktalar daha fazla.çünkü verileri dağınık ayırır
plot(model_Radial , testSet , radius_mean ~ texture_mean)
plot(model_Radial , testSet , perimeter_mean ~ area_mean)


?plot.svm

?tune

?tune.control

?plot.tune

## Model Tune Etmek

library(e1071)

#cross ile kaç fold olacağını söyleriz
#gamma ve cost degerlerini kendimiz belirledik


model_Linear_Tune <- tune(svm , diagnosis ~ . , data = trainSet ,
                        kernel = "linear",
                        ranges = list(gamma = 2^(-2:2) , cost = 2^(-4:2)),
                        tunecontrol = tune.control(cross = 5)
)
model_Linear_Tune


model_Radial_Tune <- tune(svm , diagnosis ~ . , data = trainSet ,
                        kernel = "radial",
                        ranges = list(gamma = 2^(-2:2) , cost = 2^(-4:2)),
                        tunecontrol = tune.control(cross = 5)
)

model_Radial_Tune

model_Radial_Tune$performances # en iyi gamma ve cost değerlerine gore hataları verir


model_Linear_Tune$best.model # predict işlemi için best.model kullanırız
model_Radial_Tune$best.model


#Tahmin yaparsak,

#linear icin tahmin,
predict_Linear_Tune <- predict(model_Linear_Tune$best.model , testSet)

#radial icin tahmin,
predict_Radial_Tune <- predict(model_Radial_Tune$best.model , testSet)


#Confusion Matrix ile tahmin değerleri ve gerçek değerleri kıyaslarsak,

#confusion matrix ile çıktıların kalitesini ölçmek için farklı metrikler hesaplarız.
#


confusionMatrix(predict_Linear_Tune , testSet$diagnosis)
confusionMatrix(predict_Radial_Tune , testSet$diagnosis)

confusionMatrix(predict_Linear_Tune , testSet$diagnosis , mode = "prec_recall")
confusionMatrix(predict_Radial_Tune , testSet$diagnosis , mode = "prec_recall")


#####SONUÇ####


#B:İyi Huylu Tümör İçin,

confusionMatrix(predict_Linear , testSet$diagnosis)
confusionMatrix(predict_Radial , testSet$diagnosis)


confusionMatrix(predict_Linear_Tune , testSet$diagnosis)
confusionMatrix(predict_Radial_Tune , testSet$diagnosis)

confusionMatrix(predict_Linear , testSet$diagnosis , mode = "prec_recall")
confusionMatrix(predict_Radial , testSet$diagnosis , mode = "prec_recall")

confusionMatrix(predict_Linear_Tune , testSet$diagnosis , mode = "prec_recall")
confusionMatrix(predict_Radial_Tune , testSet$diagnosis , mode = "prec_recall")


#M:Kötü Huylu Tümör İçin,

confusionMatrix(predict_Linear , testSet$diagnosis, positive = "M")
confusionMatrix(predict_Radial , testSet$diagnosis, positive = "M")


confusionMatrix(predict_Linear_Tune , testSet$diagnosis, positive = "M")
confusionMatrix(predict_Radial_Tune , testSet$diagnosis, positive = "M")

confusionMatrix(predict_Linear , testSet$diagnosis , mode = "prec_recall", positive = "M")
confusionMatrix(predict_Radial , testSet$diagnosis , mode = "prec_recall", positive = "M")

confusionMatrix(predict_Linear_Tune , testSet$diagnosis , mode = "prec_recall", positive = "M")
confusionMatrix(predict_Radial_Tune , testSet$diagnosis , mode = "prec_recall", positive = "M")



##sonuçlara göre tune işlemi yapmadan aldığımız sonuçlar ile yaptıktan sonra aldığımız sonuçları kıyaslarsak,
#tune işlemi yapmadan da B ve M tümörlerini tahmin etmede gayet iyi sonuçlar almış bulunuyoruz.
