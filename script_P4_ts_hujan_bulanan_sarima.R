#Skrip by: supmawatimeysi@gmail.com
#Skrip bertujuan untuk mengubah ch harian menjadi CH bulanan
#Skrip juga mengisi nilai CH bulanan yang kosong dengan Nilai Klimatologi
#Skrip ini menampilkan plot time series dan klimatologi, serta melakukan dekomposisi
#skrip ini menggunakan Fungsi "day2mon_ch.R"
rm(list = ls())
graphics.off()

#library yang diperlukan
library(TSA)
library(ggplot2)
library(scales)
library(tseries)
library(urca)
library(forecast)
library(lmtest)

#******** SESUAIKAN BAGIAN BERIKUT INI *******************************************************
setwd("C:/Users/lenio/Downloads/P3/") #set working direktory
rm(list = ls()) #hapus semua variable di environmen

dirData    <- "C:/Users/lenio/Downloads/P1/data_precipitation_daily/"
dirInfo    <- "C:/Users/lenio/Downloads/P1/data_precipitation_daily/"
dirFungsi  <- "C:/Users/lenio/Downloads/P1/add_function/"
dirHasil   <- "C:/Users/lenio/Downloads/P4/hasil/"
dir.create(dirHasil,recursive = T,showWarnings=F)

fileData <- paste(dirInfo,"out_Data_CH_daily.csv",sep="")
DATA     <- read.csv(fileData,header=F)
fileInfo <- paste(dirInfo,"out_Data_CH_daily_infosta.csv",sep="")
INFO     <- read.csv(fileInfo,header=T)

startYr <- 1996
endYr   <- 2023
nosta   <- 96291 #lihat pilihan stasiun pada file "out_Data_CH_daily_infosta.csv"

#***************** Tahap 1 ***************
DATA    <- DATA[which(DATA[,1]>=startYr &DATA[,1]<=endYr),] 
idxData <- which(INFO$NoSTA==nosta)
data    <- DATA[,c(1,2,3,idxData+3)]

#fungsi untuk menghitung ch bulanan
source(paste(dirFungsi,"day2mon_precip.R",sep=""))
ch_mon <- day2mon_precip(data)

#Tambal jika ada NaN
source(paste(dirFungsi,"fill_with_clim.R",sep=""))
ch_mon_ori     <- ch_mon$original
ch_mon_fillmis <- fill_with_clim(ch_mon_ori)
ch_mon_fill    <- ch_mon_fillmis$originalfill
ch_mon_clim    <- ch_mon_fillmis$climatology

# *************** Tahap 1: Siapkan data deret waktu 
source(paste(dirFungsi,"ori2tsmon.R",sep=""))
df_ch_mon <- ori2tsmon(ch_mon_fill)

#Tampilkan plot time series - sepanjang waktu
library(ggplot2)
library(scales)
chmon_line <- ggplot(df_ch_mon, aes(TglBln, VAL)) +
  geom_line(na.rm=TRUE) +  
  ggtitle(paste("Curah Hujan Bulanan\n", "Stasiun :",as.character(nosta),sep="") ) +
  xlab("Bulanan") + ylab("Curah Hujan (mm)") +
  scale_x_date(breaks = date_breaks("2 year"),labels = date_format("%b %y")) +
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 15)) +
  theme(text = element_text(size=10))
chmon_line

#DiKonversi ke format "ts" 
hujan_ts <- ts(data = df_ch_mon$VAL, frequency = 12)

#Selection data
hujan_train <- df_ch_mon[df_ch_mon$TglBln >="1996-01-01" & df_ch_mon$TglBln <="2020-12-01",]
hujan_train <- ts(data = hujan_train$VAL,frequency = 12)
hujan_test <- df_ch_mon[df_ch_mon$TglBln >= "2021-01-01" & df_ch_mon$TglBln <= "2023-12-01",]

#Cek kekuatan sinyal musiman
decomp <- stl(hujan_ts, s.window = 'periodic')
#Plot decomposition
autoplot(decomp, range.bars=F) + theme_bw()
Tt <- trendcycle(decomp)
St <- seasonal(decomp)
Rt <- remainder(decomp)
#Trend strenght calculation
Ft <- round(max(0,1 - (var(Rt)/var(Tt +Rt))),1)
#Seasonal strenght calculation
Fs <- round(max(0,1 - (var(Rt)/var(St +Rt))),1)
data.frame('Trend Strenght'=Ft, 'SeasonalStrenght' = Fs)
#Jika nilai Fs semakin dekat ke 1 maka sinyal seasonal makin kuat

#***************** Tahap 2 : uji stasioneritas ***************
#bentuk plot acf untuk series non-musiman dari data original kode "00"
acf00 <- acf(hujan_train, main=paste("Plot ACF non-musiman CH bulanan : ", 
                                     as.character(nosta), sep=""), lag.max = 48, xaxt="n")
axis(1, at=0:48/12, labels=0:48)

#bentukplot acf untuk series musiman dari data original kde "01"
acf00$lag <- acf00$lag * 12
acf01 <- as.data.frame(cbind("acf" = acf00$acf, "lag" = acf00$lag))
acf01 <- acf01[acf01$lag %% 12 == 0, ]
barplot(height = acf01$acf, names.arg = acf01$lag, ylab = "ACF", xlab = "Lag",
        main = paste("Plot ACF musiman CH bulanan :", as.character(nosta), sep=""))

#***************** Tahap 3A : Diferencing data ori pada komponen musiman, lag=12
D1 <- diff(hujan_train, lag=12)
ts.plot(D1, type="l", ylab="CH (mm)", col="black",
        main=paste("Diff12 - curah hujan bulanan\n", "Stasiun :", as.character(nosta),sep=""))

# Bentuk plot ACF untuk series Non-Musiman dari data D1, kode "02"
acf02 <- acf(D1, main=paste("Plot ACF-D1 Non-Musiman CH bulanan", as.character(nosta), sep=""))

# Bentuk plot acf untuk series musiman dari data original kode "01"
acf02$lag <- acf02$lag * 12
acf03 <- as.data.frame(cbind(acf02$acf, acf02$lag))  # Membuat data frame
colnames(acf03) <- c("acf", "lag")  # Menetapkan nama kolom
acf03 <- acf03[acf03$lag %% 12 == 0, ]  # Memilih lag yang musiman

# Membuat barplot
barplot(height = acf03$acf, names.arg = acf03$lag, ylab = "ACF", xlab = "Lag",
        main=paste("Plot ACF-D1 musiman CH bulanan : ", as.character(nosta), sep=""))

#***************** Tahap 3B : Diferencing data D1 pada komponen musiman
D1d1 <- diff(D1, lag=1)
ts.plot(D1d1, type="l", ylab="CH (mm)", col="black",
        main=paste("Diff12-Diff1 - curah hujan bulanan\n", "Stasiun :", as.character(nosta),sep=""))

#bentuk plot ACF untuk series Non-Musiman dari data D1, kode "02"
acf02 <- acf(D1, main=paste("Plot ACF-D1 Non-Musiman CH bulanan", as.character(nosta),sep=""), lag.max = 48, xaxt="n")
axis(1, at=0:48/12, labels=0:48)

#bentukplot acf untuk series musiman dari data original kde "01"
# Ambil lag dalam satuan bulan
acf02$lag <- acf02$lag * 12
# Membuat data frame dari ACF dan lag
acf03 <- as.data.frame(cbind("acf" = acf02$acf, "lag" = acf02$lag))
# Filter untuk lag yang merupakan kelipatan 12 (musiman)
acf03 <- acf03[acf03$lag %% 12 == 0, ]
# Plot ACF dengan barplot
barplot(height =acf03$acf, names.arg = acf03$lag, ylab = "ACF", xlab = "Lag",
        main = paste("Plot ACF-D1 musiman CH bulanan : ", as.character(nosta), sep=""))

#***************** Tahap 4 : Penentuan kandidat model
#hitung EACF
eacf(D1d1)

#manmpilkan acf dan pacf sekaligus dengan library(astsa)
library(astsa)
astsa::acf2(D1d1,48)

#***************** Tahap 5 :Penentuan model terbaik
sarima1 <- forecast::Arima(hujan_train, order = c(0,1,1), seasonal = c(0,1,1))
sarima2 <- forecast::Arima(hujan_train, order = c(0,1,0), seasonal = c(0,1,1))
sarima3 <- forecast::Arima(hujan_train, order = c(0,1,2), seasonal = c(0,1,1))
sarima4 <- forecast::Arima(hujan_train, order = c(1,1,1), seasonal = c(0,1,1))
sarima5 <- forecast::Arima(hujan_train, order = c(1,1,2), seasonal = c(0,1,1))
sarima6 <- forecast::auto.arima(hujan_train)

#ringkasan nilai aic model
data.frame('Model-1'=sarima1$aic, 'Model-2'=sarima2$aic,
           'Model-3'=sarima3$aic, 'Model-4'=sarima4$aic,
           'Model-5'=sarima5$aic, 'Auto'=sarima6$aic,
           row.names = "AIC Value")

#***************** Tahap 6 :Diagnostik model
#diagnosis 1 : melakukan uji signifikansi parameter p dan q dari model terpilih
#fungsi untuk menghitung signifikansi koefisien SARIMA "printstatarima.R"
#cara 1
source(paste(dirFungsi, "printstatarima.R", sep=""))
printstatarima(sarima5)
#cara 2
library(lmtest)
coeftest <- lmtest::coeftest(sarima5)
coeftest

#diagnosis 2a : cek autokorelasi pada residual atau sisaan secara visual melalui bentuk plot acf
#cara 1
forecast::checkresiduals(sarima5)
#cara 2
sisaan <- sarima5$residuals
stats::tsdiag(sarima5)
#acf yang diharapkan adalah tidak ada spike pada tiap lag yang melebihi batas signifikansi
#kecuali spike pada lag-0

#diagnosis 2b : cek autokorelasi pada residual atau sisaan
#residual mdel arima yang dibentuk bersifat "white noise"
stats::Box.test(sisaan, type = "Ljung")
#hipotesis 0 : tidak ada autokorelasi pada sisaan/sisaan saling bebas
#hipotesis 1 : ada autokorelasi pada sisaan
#hasil yang diharapkan adalah pvalue>=0.05 yang berarti H0 tidak ditolak

#diagnosis 3 : cek apakah residuals punya "zer mean"
stats::t.test(sisaan, mu=0, conf.level = 0.95)
#hipotesis 0 : nilai tengah sisaan sama dengan nol
#hipotesis 1 : nilai tengah sisaan tidak sama dengan nol
#hasil yang diharapkan adalah pvalue>=0.05 yang berarti H0 tidak ditolak

#diagnosis 4 : cek apakah residuals terdistribusi normal
stats::shapiro.test(sisaan)
#hipotesis 0 : residual menyebar normal
#hipotesis 1 : residual tidak menyebar normal
#hasil yang diharapkan adalah pvalue>=0.05 (alpha), residual menyebar normal

#cara 3 : menggunakan library(astsa) untuk menduga model sarima
sarima5_fit <- astsa::sarima(hujan_train,1,1,2,0,1,1,12)
#nilai AIC dari library(astsa) berbeda library(forecast)
#perbedaan terletak pada dilakukan normalisasi nilai atau tidak
#namun antar kedua AIC pada library tersebut tidak bisa diperbandingkan

#***************** Tahap 7a :Forecast sebanyak n bulan kedepan
pred_sarima <- forecast(sarima5, 36) #karena sisaa data 3 tahun
#tampilkan plot prediksi sarima 
plot(pred_sarima)

#tampilkan plot prediksi sarima dan data asli
fit_sarima <- c(as.vector(pred_sarima$fitted), as.vector(pred_sarima$mean))
df_ch_mon_pred <- cbind(df_ch_mon, fit_sarima)

chmon_line <- ggplot() +
  geom_line(df_ch_mon_pred, mapping=aes(x=TglBln, y=VAL), na.rm = TRUE, color="black") +
  geom_line(df_ch_mon_pred, mapping=aes(x=TglBln, y=fit_sarima), na.rm = TRUE, color="blue") +
  ggtitle(paste("Curah hujan bulanan\n", "Statiun :", as.character(nosta), sep="")) +
  xlab("Bulanan") +ylab("Curah hujan (mm)") +
  scale_x_date(breaks = date_breaks("2 year"), labels= date_format("%b %y")) +
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 15)) +
  theme(text = element_text(size=10)) + theme(legend.position = c(0.87, 0.25))
chmon_line

#***************** Tahap 7b : validasi mdel
accuracy(pred_sarima,hujan_test[, "VAL"])




