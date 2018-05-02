# 3 Variabel 
#library(ggplot2)
library(dplyr)

#load file .csv
df <- read.csv(file="datakotor.csv", header=TRUE, sep=",")

ptm <- proc.time()
#pembagian variabel
head(df)
print(head(df))
loc <- df[4:4][1:1] #daerah
x1 <- df[5:5][1:1] #luas tanah
x2 <- df[6:6][1:1] #luas bangunan
y <- df[2:2][1:1]  #harga

#x1 <- df[2:2][1:1] #luas tanah
#x2 <- df[3:3][1:1] #luas bangunan
#y <- df[7:7][1:1]  #harga

kotor <- data.frame(loc ,x1, x2, y)
#input <- tabel[,c("Luas.Tanah","Luas.Bangunan","Harga")]
print(head(kotor))
print(kotor)
# print(input)

#pemecahan berdasarkan lokasi
aggregate(kotor$Lokasi, by=list(Lokasi=kotor$Lokasi), FUN=length)
arcamanik <- filter(kotor, Lokasi == "Bandung, Arcamanik" )
print(head(arcamanik))

#pembersihan data

filter(arcamanik, is.na(Harga))
filter(arcamanik, is.na(Bangunan))
filter(arcamanik, is.na(Tanah))

sum(is.na(arcamanik))
complete.cases(arcamanik)
aArcamanik <- arcamanik[complete.cases(arcamanik), ]
#na.omit(arcamanik, cols="Tanah", "Bangunan", "Harga")

sum(is.na(aArcamanik))
complete.cases(aArcamanik)
print(head(aArcamanik))

noMArcamanik <- aArcamanik[-grep("M", aArcamanik$Harga) , ]
cleanArcamanik <- noMArcamanik[-grep(",", noMArcamanik$Harga) , ]
print(head(cleanArcamanik))

filter(cleanArcamanik, is.na(Harga))
filter(cleanArcamanik, is.na(Bangunan))
filter(cleanArcamanik, is.na(Tanah))


#set.seed(20)
#theCluster <- kmeans(cleanArcamanik[, 2:4], 3, nstart = 20)
#theCluster
#ggplot(cleanArcamanik, aes(Bangunan, Harga, color = theCluster$cluster)) + geom_point()


library(magrittr)
b <- cleanArcamanik[2:2][1:1]
a1 <- cleanArcamanik[3:3][1:1]
a2 <- cleanArcamanik[4:4][1:1]

b <- droplevels(b)

perhitungan <- data.frame(b, a1, a2)
print(head(perhitungan))
write.csv(perhitungan, file = "perhitungan.csv")
summary(perhitungan)
ph <- read.csv(file="perhitungan.csv", header=TRUE, sep=",")
x1 <- ph[2:2][1:1] #luas tanah
x2 <- ph[3:3][1:1] #luas bangunan
y <- ph[4:4][1:1]  #harga
perhitungan <- data.frame(x1,x2,y)
head(perhitungan)

#B/T ratio
perhitungan["ratio"] <- NA
perhitungan["cluster"] <- NA
perhitungan$ratio <- (perhitungan$Bangunan / perhitungan$Tanah)
perhitungan$cluster <- ifelse (perhitungan$ratio < 0.2, 1, ifelse (perhitungan$ratio < 0.4, 2, ifelse (perhitungan$ratio < 0.6, 3, ifelse (perhitungan$ratio < 0.8, 4, ifelse (perhitungan$ratio < 1, 5, ifelse (perhitungan$ratio > 1, 6, 0))))))
print(perhitungan)

aggregate(perhitungan$cluster, by=list(cluster=perhitungan$cluster), FUN=length)
cluster1 <- filter(perhitungan, cluster == 1 )
cluster2 <- filter(perhitungan, cluster == 2 )
cluster3 <- filter(perhitungan, cluster == 3 )
cluster4 <- filter(perhitungan, cluster == 4 )
cluster5 <- filter(perhitungan, cluster == 5 ) 
                   
cluster1
cluster1["cluster1Tanah"] <- NA
cluster1["cluster1Bangunan"] <- NA
cluster1["cluster1Intercept"] <- NA
model <- lm(Harga~Bangunan+Tanah, data=cluster1)
model
cluster1$cluster1Tanah <- model[["coefficients"]][["Tanah"]]
cluster1$cluster1Bangunan <- model[["coefficients"]][["Bangunan"]]
cluster1$cluster1Intercept <- model[["coefficients"]][["(Intercept)"]]
cluster1["Prediksi"] <- NA
cluster1$Prediksi <- (((cluster1$Tanah * cluster1$cluster1Tanah) + (cluster1$Bangunan * cluster1$cluster2Bangunan)) + cluster1$cluster2Intercept)
cluster1["Error"] <- NA
cluster1$Error <- ((cluster1$Prediksi - cluster1$Harga) / cluster1$Harga)
cluster1["AbsE"] <- NA
cluster1$AbsE <- abs(cluster1$Error)

cluster2
cluster2["cluster2Tanah"] <- NA
cluster2["cluster2Bangunan"] <- NA
cluster2["cluster2Intercept"] <- NA
model <- lm(Harga~Bangunan+Tanah, data=cluster2)
model
cluster2$cluster2Tanah <- model[["coefficients"]][["Tanah"]]
cluster2$cluster2Bangunan <- model[["coefficients"]][["Bangunan"]]
cluster2$cluster2Intercept <- model[["coefficients"]][["(Intercept)"]]
cluster2["Prediksi"] <- NA
cluster2$Prediksi <- (((cluster2$Tanah * cluster2$cluster2Tanah) + (cluster2$Bangunan * cluster2$cluster2Bangunan)) + cluster2$cluster2Intercept)
cluster2["Error"] <- NA
cluster2$Error <- ((cluster2$Prediksi - cluster2$Harga) / cluster2$Harga)
cluster2["AbsE"] <- NA
cluster2$AbsE <- abs(cluster2$Error)

cluster3
cluster3["cluster3Tanah"] <- NA
cluster3["cluster3Bangunan"] <- NA
cluster3["cluster3Intercept"] <- NA
model <- lm(Harga~Bangunan+Tanah, data=cluster3)
model
cluster3$cluster3Tanah <- model[["coefficients"]][["Tanah"]]
cluster3$cluster3Bangunan <- model[["coefficients"]][["Bangunan"]]
cluster3$cluster3Intercept <- model[["coefficients"]][["(Intercept)"]]
cluster3["Prediksi"] <- NA
cluster3$Prediksi <- (((cluster3$Tanah * cluster3$cluster3Tanah) + (cluster3$Bangunan * cluster3$cluster3Bangunan)) + cluster3$cluster3Intercept)
cluster3["Error"] <- NA
cluster3$Error <- ((cluster3$Prediksi - cluster3$Harga) / cluster3$Harga)
cluster3["AbsE"] <- NA
cluster3$AbsE <- abs(cluster3$Error)

cluster4
cluster4["cluster4Tanah"] <- NA
cluster4["cluster4Bangunan"] <- NA
cluster4["cluster4Intercept"] <- NA
model <- lm(Harga~Bangunan+Tanah, data=cluster4)
model
cluster4$cluster4Tanah <- model[["coefficients"]][["Tanah"]]
cluster4$cluster4Bangunan <- model[["coefficients"]][["Bangunan"]]
cluster4$cluster4Intercept <- model[["coefficients"]][["(Intercept)"]]
cluster4["Prediksi"] <- NA
cluster4$Prediksi <- (((cluster4$Tanah * cluster4$cluster4Tanah) + (cluster4$Bangunan * cluster4$cluster4Bangunan)) + cluster4$cluster4Intercept)
cluster4["Error"] <- NA
cluster4$Error <- ((cluster4$Prediksi - cluster4$Harga) / cluster4$Harga)
cluster4["AbsE"] <- NA
cluster4$AbsE <- abs(cluster4$Error)

cluster5
cluster5["cluster2Tanah"] <- NA
cluster5["cluster2Bangunan"] <- NA
cluster5["cluster2Intercept"] <- NA
model <- lm(Harga~Bangunan+Tanah, data=cluster5)
model
cluster5$cluster5Tanah <- model[["coefficients"]][["Tanah"]]
cluster5$cluster5Bangunan <- model[["coefficients"]][["Bangunan"]]
cluster5$cluster5Intercept <- model[["coefficients"]][["(Intercept)"]]
cluster5["Prediksi"] <- NA
cluster5$Prediksi <- (((cluster5$Tanah * cluster5$cluster5Tanah) + (cluster5$Bangunan * cluster5$cluster5Bangunan)) + cluster5$cluster5Intercept)
cluster5["Error"] <- NA
cluster5$Error <- ((cluster5$Prediksi - cluster5$Harga) / cluster5$Harga)
cluster5["AbsE"] <- NA
cluster5$AbsE <- abs(cluster1$Error)

#double standartd regression 
phT <- read.csv(file="tanah.csv", header=TRUE, sep=",")
x1 <- phT[2:2][1:1] #luas tanah
y <- phT[1:1][1:1]  #harga
locTanah <- phT[3:3][1:1]
perhitunganTanah <- data.frame(x1,y,locTanah)
head(perhitunganTanah)

aggregate(perhitunganTanah$Daerah, by=list(Daerah=perhitunganTanah$Daerah), FUN=length)
arcamanikT <- filter(perhitunganTanah, Daerah == "Bandung, Arcamanik" )
arcamanikT
model <- lm(Harga~Luas, data=arcamanikT)
model
perhitunganDS <- perhitungan
perhitunganDS["HargaT"] <- NA
perhitunganDS["IntT"] <- NA
perhitunganDS$HargaT <- model[["coefficients"]][["Luas"]]
perhitunganDS$IntT <- model[["coefficients"]][["(Intercept)"]]
perhitunganDS["HargaBangunan"] <- NA
perhitunganDS$HargaBangunan <- perhitunganDS$Harga - ((perhitunganDS$Tanah * perhitunganDS$HargaT) + perhitunganDS$IntT)
model2 <- lm(perhitunganDS$HargaBangunan ~ Bangunan, data=perhitunganDS)
perhitunganDS["HargaB"] <- NA
perhitunganDS$HargaB <- model2[["coefficients"]][["Bangunan"]]
perhitunganDS["IntB"] <- NA
perhitunganDS$IntB <- model2[["coefficients"]][["(Intercept)"]]
perhitunganDS["Prediksi"] <- NA
perhitunganDS$Prediksi <- ((perhitunganDS$Tanah * perhitunganDS$HargaT) + perhitunganDS$IntT) + ((perhitunganDS$Bangunan * perhitunganDS$HargaB) + perhitunganDS$IntB)
perhitunganDS["Error"] <- NA
perhitunganDS$Error <- ((perhitunganDS$Harga - perhitunganDS$Prediksi)/perhitunganDS$Prediksi)*100
perhitunganDS["AbsE"] <- NA
perhitunganDS$AbsE <- abs(perhitunganDS$Error)

#double linear regression 
perhitunganDL <- perhitungan
relation <- lm(Harga ~ Tanah + Bangunan, data=perhitunganDL)
relation

perhitunganDL["HargaB"] <- NA
perhitunganDL$HargaB <- relation[["coefficients"]][["Bangunan"]]
perhitunganDL["HargaT"] <- NA
perhitunganDL$HargaT <- relation[["coefficients"]][["Tanah"]]
perhitunganDL["Int"] <- NA
perhitunganDL$Int <- relation[["coefficients"]][["(Intercept)"]]
perhitunganDL["Prediksi"] <- NA
perhitunganDL$Prediksi <- ((perhitunganDL$Tanah * perhitunganDL$HargaT) + perhitunganDL$Bangunan * perhitunganDL$HargaB) + perhitunganDL$Int
perhitunganDL["Error"] <- NA
perhitunganDL$Error <- ((perhitunganDL$Prediksi - perhitunganDL$Harga) / perhitunganDL$Harga)*100
perhitunganDL["ErrorCluster"] <- NA
perhitunganDL$ErrorCluster <- ifelse (perhitunganDL$Error > 0 & perhitunganDL$Error < 15 , 1, 0)


cleanData <- filter(perhitunganDL, ErrorCluster == 1 )

relation <- lm(Harga ~ Tanah + Bangunan, data=cleanData)
relation

cleanData["HargaB"] <- NA
cleanData$HargaB <- relation[["coefficients"]][["Bangunan"]]
cleanData["HargaT"] <- NA
cleanData$HargaT <- relation[["coefficients"]][["Tanah"]]
cleanData["Int"] <- NA
cleanData$Int <- relation[["coefficients"]][["(Intercept)"]]
cleanData["Prediksi"] <- NA
cleanData$Prediksi <- ((cleanData$Tanah * cleanData$HargaT) + cleanData$Bangunan * cleanData$HargaB) + cleanData$Int
cleanData["Error"] <- NA
cleanData$Error <- ((cleanData$Prediksi - cleanData$Harga) / cleanData$Harga)*100

proc.time() - ptm
