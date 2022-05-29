#1
#a
sebelumAktivitasA <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
setelahAktivitasA <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)
tabelPengamatanOksigen <- data.frame (
  x = sebelumAktivitasA,
  y = setelahAktivitasA
)

tabelPengamatanOksigen

perbedaanSesudahDanSebelum <- tabelPengamatanOksigen[["y"]] - tabelPengamatanOksigen[["x"]]
n <- length(perbedaanSesudahDanSebelum)
rerata <- mean(perbedaanSesudahDanSebelum)

variance <- 0

for (i in perbedaanSesudahDanSebelum) {
  variance <- variance + (i - rerata)^2 
}
variance <- variance / (n - 1)
standardDeviasi <- variance^(0.5)

standardDeviasi

#b

t.test(
  x = perbedaanSesudahDanSebelum, alternative="two.sided",
  mu=0, var.equal=TRUE
)

#c
# H0: tidak ada pengaruh yang signifikan secara statistika dalam hal 
# kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas A
# H0: mean perubahan = 0
# H1: mean perubahan != 0
meanPerubahan = 0
significantLevel <- 0.05
t <- (rerata - meanPerubahan) * (n^(0.5)) / standardDeviasi
if (2 * dt(t, n - 1) <= significantLevel) {
  print("Menolak H0, terdapat pengaruh")
} else {
  print("Tidak menolak H0, tidak terdapat pengaruh")
}

#2
library(BSDA)

zsum.test(
  mean.x=23500, sigma.x = 3900, n.x = 100,  
  alternative = "two.sided", mu = 20000,
  conf.level = 0.95
)
# a
# confidence level yang digunakan adalah 95%
# Tidak setuju pada klaim tersebut

# b
# berdasarkan output, nilai p-value < 2.2e-16 dan
# estimasi rata-ratanya adalah 22735.61 < mean < 24264.39

# c
# karena nilai p-value lebih kecil dari significant level,
# maka null hipotesis ditolak

# 3
# a
# H0 : rata-rata Bandung - rata-rata Bali = 0
# H1 : rata-rata Bandung - rata-rata Bali != 0

# b
confidenceLevel <- 0.95
n1 = 19
mean1 = 3.64
sigma1 = 1.67

n2 = 27
mean2 = 2.79
sigma2 = 1.32

sp <- (((n1 - 1)*(sigma1^2) + (n2 - 1)*(sigma2^2)) / (n1 + n2 - 2))^(0.5)

dof <- n1 + n2 - 2
paste("s-pool: ", sp)

# c
tsum.test(
  mean.x=mean1, s.x = sigma1, n.x = n1, 
  mean.y=mean2, s.y = sigma2, n.y = n2, 
  alternative = "two.sided", 
  mu = 0, var.equal = TRUE,
  conf.level = confidenceLevel
)

# d
# nilai kritikal adalah 0.05 dan p-value adalah 0.06049

# e
# keputusan: Terima null hipotesa

# f
# kesimpulan: Rata-rata kedua populasi sama, tidak cukup bukti untuk
# menyatakan terdapat perbedaan pada rata-rata kedua populasi 

# 4
dataSetKucing <- read.delim(file.choose())
dataSetKucing$Group <- as.factor(dataSetKucing$Group)
library(ggpubr)

# a
qplot(dataSetKucing$Group, dataSetKucing$Length, xlab="Group", ylab="Length")

# b
# significant level = 0.05
bartlett.test(Length ~ Group, data = dataSetKucing)
# H0 : variansi ketiga populasi sama
# H1 : variansi ketiga populasi berbeda
# p-value = 0.8054
# Null hipotesis diterima karena nilai p-value lebih besar dari 0.05
# Variansi ketiga populasi sama

# c
model1 <- aov(Length ~ Group, data = dataSetKucing)
summary(model1)

# d
summary(model1)
# H0 : mean1 = mean2 = mean3
# H1 : mean1 != mean2 != mean3
# p-value = 0.0013
# Keputusan: Tolak H0, karena p-value < 0.05
# Kesimpulan: rata-rata ketiga populasi berbeda

# e
TukeyHSD(model1)
# terdapat perbedaan pada 
# kucing grup 1 dengan grup 2 dan kucing grup 2 dengan grup 3
# sedangkan grup 1 dengan grup 3 sama, kucing grup 2 adalah
# yang terpendek

#f
ggboxplot(
  dataSetKucing, x = "Group", y = "Length",
  color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  order = c("1", "2", "3"),
  ylab = "Weight", xlab = "Treatment"
)

# 5
library(readr)
library(dplyr)
library(multcompView)
dataCahaya <- read.csv(file.choose())
dataCahaya$Temp <- as.factor(dataCahaya$Temp)

# a
qplot(dataCahaya$Glass, dataCahaya$Light, xlab="Glass", ylab="Light")
qplot(dataCahaya$Temp, dataCahaya$Light, xlab="Temp", ylab="Light")
ggline(
  dataCahaya, x = "Temp", y = "Light", color = "Glass",
  add = c("dotplot"), palette = c("#00AFBB", "#E7B800", "#46eb34")
)

# b
hasilANOVACahaya <- aov(Light ~ Glass + Temp + Glass:Temp, dataCahaya)
summary(hasilANOVACahaya)

# c
group_by(dataCahaya, Glass, Temp) %>% 
summarise(mean=mean(Light), sd=sd(Light))

# d
hasilTukeyCahaya <- TukeyHSD(hasilANOVACahaya)
hasilTukeyCahaya

# e
multcompLetters4(hasilANOVACahaya, hasilTukeyCahaya)