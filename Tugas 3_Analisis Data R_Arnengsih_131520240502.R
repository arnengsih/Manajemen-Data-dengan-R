# skript untuk kelas data manajemen dan analisis
# contoh skript R sederhana
#kode operasi matematik penjumlahan
1 + 3

#perkalian
2 * 3

#pembagian
10 / 2

#pengurangan
7 -2

#Aktivasi package dplyr + paket lainnya yg tergabung dalam grup tdyverse
library(tidyverse)

#membaca data dari URL
#menggunakan library readr
library(readr)
#membaca data crime(kejahatan) di kampus2 
c_data <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/c_data.csv")

#menggunakan library data.table
library(data.table)
#membaca data hasil pengukuran PEFR IFLS wave 5
pef <- fread("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")

#membaca data morbiditas dan kondisi penyakit IFLS wave 5
w5 <- fread("https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv")

#membaca data sensor polusi udara
library(readr)
d_1_14 <- read_csv("2025-08-20.csv")
library(readr)
d_1_14_21 <- read_csv("2025-08-21.csv")

#mempelajari struktur data
names(d_1_14)
names(d_1_14_21)

#mempelajari struktur data
names(pef)
str(pef)
glimpse(pef)
summary(pef)

names(w5)
str(w5)
glimpse(w5)
summary(w5)

#memvisualisasi distribusi data umur berupa numerik dengan histogram dari variabel age pada object dataset pef
hist(pef$age)

#memvisualisasi distribusi data umur berupa numerik dengan boxplot pada object dataset pef
boxplot(pef$age)

#memvisualisasi frekuensi dari variable type pada object dataset c_data
table(c_data$type)

#memvisualisasi frekuensi dari variable region pada object dataset c_data
table(c_data$region)

#subsetting data set
#subsetting variables
#memilih berdasarkan nama variables
pef1  = select(pef, pidlink, age, pef)

#memilih berdasarkan huruf yg terkandung dalam variables
pef2 = select(d_1_14, contains("pm"))
pef2 = select(d_1_14_21, contains("pm"))

#memilih dengan mendrop/mendelete nama variables
pef3 = select(pef, -pidlink)

#memilih range variable yang berada diantara dua variables
pef4 = select(pef, height:us09c)

newd_1_14 = select(d_1_14, id:pm10)
names(newd_1_14)

newd_1_14_21 = select(d_1_14_21, id:pm10)
names(newd_1_14_21)

#membuat dataset baru untuk subject 1-5 dgn variabel terpilih
newd_1_5 = select(d_1_14, id:pm10)

#membuat dataset baru untuk subject 1-28 dgn variabel terpilih
newd_1_28 = select(d_1_5, id:pm10)

names(newd_1_5)
class(newd_1_5$time)

#memberikan id kepada observasi > jam 09:00:00
newd_1_5$id_subject  = NA
newd_1_14$id_subject  = NA
newd_1_14_21$id_subject  = NA

#kode di bawah tidak berhasil
newd_1_5$id_subject [newd_1_5$time > "09:00:00"] = "1-5"
newd_1_14$id_subject [newd_1_14$time > "09:00:00"] = "1-9"

#memilih berdasarkan range urutan observasi/baris
#1-5
newd_1_5 = slice(newd_1_5, 2160:5744)

#1-9
newd_1_14 = slice(newd_1_14, 2160:5744)
#memberikan id kepada observasi < jam 09:00:00
newd_1_14$id_subject  = "1-9-28"

#1-28
newd_1_28 = slice(newd_1_28, 1:2159)
#memberikan id kepada observasi < jam 09:00:00
newd_1_28$id_subject  = "1-28"

#1-9-29
newd_1_14_21 = slice(newd_1_14_21, 1:2159)
#memberikan id kepada observasi < jam 09:00:00
newd_1_14_21$id_subject  = "1-9_29"

#1-5
newd_1_5 = slice(newd_1_14, 1:2159)
#memberikan id kepada observasi < jam 09:00:00
newd_1_5$id_subject  = "1-5"


#menyatukan dua data set dengan jumlah & jenis variable yang sama
newd = rbind(newd_1_5,newd_1_14)
#menyatukan dua data set dengan jumlah & jenis variable yang sama
newd2 = rbind(newd_1_14,newd_1_14_21)

#export ke csv
write.csv(newd,"tugas2.csv")
#export ke csv
write.csv(newd2,"tugas2.csv")

#mencek working directory
getwd()

library(ggplot2)
#menampilkan daftar variabel dari dataset yg kita akan kerjakan
names(newd2)
ggplot(data = newd2 , aes(x = time, y = pm1)) +
  geom_point(size=0.7,color="purple") +
  labs(x = "Time", y = "PM 1.0") +
  ggtitle("24 hrs PM 1.0 Exposure")

#subsetting observations
#with operator >, <, >=, <=, ==, !=, 
#memilih berdasarkan kriteria kuantitatif tertentu
pef5 = filter(pef, pef > 250)

#memilih berdasarkan range urutan observasi/baris
pef6 = slice(pef, 1:10)

#memilih secara random (random sampling) sejumlah yg ditentukan
pef7 = sample_n(pef, 15, replace = TRUE)

##memilih secara random (random sampling) dengan prosentase
pef8 = sample_frac(pef, 0.5, replace = TRUE)

#memilih berdasarkan kriteria pef & heighttidak missing(NA)
pef_c = filter(pef,!is.na(pef)&!is.na(height))

summary(pef_c)
rm(pef_c)
summary(w5)

#combining dataset (menggabungkan data)
# menggabungkan variables dengan common variable
w5pef = left_join(w5, pef, by = "pidlink")
rm(w5_pef_c_lj)
names(w5pef)
w5_pef_c_rj = right_join(w5, pef, by = "pidlink")
w5_pef_c_ij = inner_join(w5, pef, by = "pidlink")
w5_pef_c_fj = full_join(w5, pef, by = "pidlink")

summary(w5_pef_c_ij)
summary(w5_pef_c_fj)

#menggabungkan observation dengan struktur variable yg sama
#contoh, misalkan data multicenter (lokasi penelitian di berbagai tempat)
# dataDKI
pef_dki <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef_dki.csv")

# data Jabar
pef_jabar <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef_jabar.csv")

#menggabungkan observasi dari dua object dataset
pef_dki_jabar = rbind(pef_dki,pef_jabar)                  
                  
#remove objects dataset yang tidak terpakai (membersihkan workspace ~ mengurangi penggunaan memori)
rm(w5_pef_c_fj,w5_pef_c_ij,w5_pef_c_lj,w5_pef_c_rj)

#exploring the data
# check jumlah obs pada object dataset pef
length(pef$pidlink)

# check jumlah obs pidlink yang unik
n_distinct(pef$pidlink)

length(w5$pidlink)
n_distinct(w5$pidlink)

# Find duplicated pidlink
pef %>% 
  count(pidlink) %>% 
  filter(n > 1)

w5 %>% 
  count(pidlink) %>% 
  filter(n > 1)



# Remove duplicated rows based on pidlink
w5 = w5 %>% distinct(pidlink, .keep_all = TRUE)

pef = pef %>% distinct(pidlink, .keep_all = TRUE)


# Count the number of full duplicates
sum(duplicated(pef))
sum(duplicated(w5))

str(pef)
str(w5)
str(all_wave5)
class(w5$pidlink)

# Convert character to number: pidlink
w5 <- w5 %>%
  mutate(pidlink_num = as.numeric(pidlink))

w5 <- w5 %>%
  mutate(pidlink_num = as.integer(pidlink))

#mengcopy paste variable pidlink asli
w5$pidlink_chr = w5$pidlink

#mereplace pidlink dengan versi int(num)
w5$pidlink = as.numeric(w5$pidlink)
class(w5$pidlink)
class(pef$pidlink)
glimpse(w5)

#menampilkan frekuensi variable sex
table (pef_w5$sex)
#menampilkan frekuensi variable asthma
table(pef_w5$Asthma)
#menampilkan descriptive statistic variable age (numeric)
summary(pef_w5$age)

#menampilkan histogram variable age (numeric)
hist(pef_w5$age)

#check normality data
#histogram
hist(pef_w5$age)
hist(pef_w5$height)

#grafik normality
qqnorm(pef_w5$age); qqline(pef_w5$age)
qqnorm(pef_w5$height); qqline(pef_w5$height)

# normality test for small sample (3-5000)
shapiro.test(pef_w5$age)

#for big sample
library(nortest)
lillie.test(pef_w5$age)
lillie.test(pef_w5$height)
lillie.test(pef_w5$pef)

#Outlier test for detection
library(outliers)
#small sample (<30)
dixon.test(pef_w5$age)
dixon.test(pef_w5$height)
dixon.test(pef_w5$pef)

#Big sample
#test nilai outlier tinggi
chisq.out.test(w5pef$age)
max(pef_w5$age)
chisq.out.test(w5pef$height)
chisq.out.test(pef_w5$pef)

#test nilai outlier rendah
chisq.out.test(pef_w5$age,opposite = TRUE)
min(pef_w5$age)

grubbs.test(pef_w5$age)
grubbs.test(pef_w5$age,opposite = TRUE)

#identifikasi outlier menggunakan boxplot
boxplot(pef_w5$age)
boxplot(pef_w5$height)
boxplot(pef_w5$pef)
#memvisualisasi nilai2 outlier sesuai boxplot
boxplot(pef_w5$age,plot = FALSE)$out
boxplot(pef_w5$height,plot = FALSE)$out

#mengidentifikasi nilai cut offs
min(boxplot(pef_w5$age,plot = FALSE)$out)
#variabel usia
outliers <- boxplot(pef_w5$age,plot = FALSE)$out
stem(outliers)
#variable height
outliers <- boxplot(pef_w5$height,plot = FALSE)$out
stem(outliers)

#boxplot
boxplot(w5_pef$age~w5_pef$sex,xlab = "Jenis Kelamin",ylab ="Usia(tahun)",
        col = c("red","blue"), 
        main="Distribusi Usia berdasarkan Jenis Kelamin")
boxplot(w5_pef$pef)
boxplot(w5_pef$pef, plot = FALSE)$out
min(boxplot(w5_pef$pef, plot = FALSE)$out)
outliers <- boxplot(w5_pef$pef, plot = FALSE)$out
stem(outliers)
#membuat dataset baru yg berisikan non outlier
data_no_outlier = subset(w5pef, w5pef$age <998 ) 
data_no_outlier = subset(w5pef,w5pef$pef >20 & w5pef$pef <710 ) 
boxplot(data_no_outlier$pef)
boxplot(data_no_outlier$pef, plot = FALSE)$out
length(data_no_outlier)

grubbs.test(data_no_outlier$pef)
grubbs.test(data_no_outlier$pef,opposite = TRUE)
lillie.test(data_no_outlier$pef)
qqnorm(data_no_outlier$pef); qqline(data_no_outlier$pef)

#Visualisasi hubungan antara variable dengan grafik scatter plot
plot(w5pef$age,w5pef$pef)
plot(pef_w5$height,pef_w5$pef)
table(w5pef$sex)

#cleaning data
#Exclusion of age 9yo
pefc = pef_w5%>% filter(age>9)
#Eklusi nilai pef < 300 dan >600 dari objek/dataset w5pef
w5pef_1 = w5pef %>% filter(pef>=300 & pef<=600)
hist(w5pef_1$pef)
hist(w5pef_1$age)
#ekslusi nilai usia <18
w5pef_2 = w5pef_1 %>% filter(age>18)

#scatter plot usia dan pef
plot(w5pef_2$age,w5pef_2$pef)
names(w5pef_2)

hist(w5pef_2$height)
summary(w5pef_2$height)

#exclusion tinggi <130
w5pef_2 = w5pef_2 %>% filter(height>130)

#scatter plot tinggi dgn pef
plot(w5pef_2$height,w5pef_2$pef)
#exclussion asthma missing
table(pefc$Asthma)
pefc = pefc %>% filter (Asthma=='Yes-Asthma'|Asthma=='No-Asthma')
table(pefc$sex)
#membuat data khusus laki2
pefm = pefc %>% filter (sex=='Male')

#membuat data khusus perempuan
peff = pefc %>% filter (sex=='Female')

#visualisasi plot khusus laki2
plot(pefm$age,pefm$pef)

#visualisasi plot khusus perempuan
plot(peff$age,peff$pef)

plot(pefc$age,pefc$pef)
#Visualisasi hubungan antara variable kategori (sex) dengan pef
boxplot(pefc$pef~pefc$sex,xlab = "Jenis Kelamin",ylab ="PEF",
        col = c("red","blue"), 
        main="Distribusi PEF berdasarkan Jenis Kelamin")
#Visualisasi hubungan antara variable kategori (Asthma) dengan pef
boxplot(pefc$pef~pefc$Asthma,xlab = "Asthma",ylab ="PEF",
        col = c("red","blue"), 
        main="Distribusi PEF berdasarkan Status Asthma")
#Visualisasi hubungan antara variable kategori (merokok) dengan pef
boxplot(pefc$pef~pefc$smokingn,xlab = "Jenis Kelamin",ylab ="PEF",
        col = c("red","blue"), 
        main="Distribusi PEF berdasarkan Status Merokok")

#subset Yes-Asthma
boxplot(pef~smokingn,xlab = "Status Merokok",ylab ="PEF",
        col = c("red","blue"), data = pefc,
        subset= Asthma=="Yes-Asthma",
        main="Distribusi PEF berdasarkan Status Merokok pada subyek Asthma")

#subset No-Asthma
boxplot(pef~smokingn,xlab = "Status Merokok",ylab ="PEF",
        col = c("red","blue"), data = pefc,
        subset= Asthma=="No-Asthma",
        main="Distribusi PEF berdasarkan Status Merokok pada subyek Non-Asthma")

#Visualisasi hubungan antara variable kategori (Asthma) dengan crp
boxplot(crp_plas_equi~Asthma,xlab = "Asthma",ylab ="PEF",
        col = c("red","blue"), data =pefc)

names(w5pef_2)
#membuat tabel frek jenis kelamin
table(w5pef_2$sex)

#membuat studi tentang hubungan sex, tb, usia dengan fungsi pernafasan (pef)
#membuat boxplot perbandingan pef antar sex
boxplot(pef~sex,xlab = "sex",ylab ="PEF",
        col = c("red","blue"), data =w5pef_2)
#uji dua rata-rata (t.test)
#opsi kode 1
t.test(w5pef_2$pef~w5pef_2$sex)
#opsi kode 2
t.test(pef~sex,data=w5pef_2)

# membandingkan rata-rata lebih dari 3 grup (analysis of variance)
res.aov <- aov(pef ~ smokingn, data = pefc)
# Summary of the analysis
summary(res.aov)

#uji korelasi antara pef dengan age dengan person & spearman
#Pearson
cor.test(w5pef_2$age,w5pef_2$pef)
cor.test(w5pef_2$height,w5pef_2$pef)
#Spearman
cor.test(w5pef_2$age,w5pef_2$pef,method = 'spearman')
cor.test(w5pef_2$height,w5pef_2$pef,method = 'spearman')

#single linier regression y=pef, x=age, y~x1
smod1 = lm (pef~age , data=w5pef_2)
summary(smod1)
plot(w5pef_2$age,w5pef_2$pef)
AIC(smod1)

smod2 = lm (pef~height , data = w5pef_2)
summary(smod2)
AIC(smod2)

smod3 = lm (pef~sex , data = w5pef_2)
summary(smod3)
AIC(smod3)

#multiple linear regression
mmod = lm(pef~age+sex+height, data=w5pef_2)
summary(mmod)
AIC(mmod)

#melakukan pemodelan dengan stepwise forward & backward
#persiapan min dan max model
#model min
modmin = lm(pef~1,data=pefc)
#model max/full model
modmax = lm(pef~age + sex + height + lncrp_plas_equi + 
              A + B + C + D +E + F +G + H+I+J+M+O+P+Q+R, data=pefc)

#forward
step(modmin, direction="forward",
     scope =list(lower=modmin, upper=modmax))

#backward
step(modmax, direction="backward",
     scope =list(lower=modmin, upper=modmax))