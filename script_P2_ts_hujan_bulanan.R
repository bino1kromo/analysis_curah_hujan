#Skrip supmawatimeysi@gmail.com
#Skrip bertujuan untuk mengubah ch harian menjadi CH bulanan
#Skrip juga mengisi nilai CH bulanan yang kosong dengan Nilai Klimatologi
#Skrip ini menampilkan plot time series dan klimatologi, serta melakukan dekomposisi
#skrip ini menggunakan Fungsi "day2mon_ch.R"
rm(list = ls())
graphics.off()

#******** SESUAIKAN BAGIAN BERIKUT INI *******************************************************
setwd("C:/Users/lenio/Downloads/P1/") #set working direktory
rm(list = ls()) #hapus semua variable di environmen

dirData    <- "C:/Users/lenio/Downloads/P1/data_precipitation_daily/"
dirInfo    <- "C:/Users/lenio/Downloads/P1/data_precipitation_daily/"
dirFungsi  <- "C:/Users/lenio/Downloads/P1/add_function/"
dirHasil   <- "C:/Users/lenio/Downloads/P2/hasil/"
dir.create(dirHasil,recursive = T,showWarnings=F)

fileData <- paste(dirInfo,"out_Data_CH_daily.csv",sep="")
DATA     <- read.csv(fileData,header=F)
fileInfo <- paste(dirInfo,"out_Data_CH_daily_infosta.csv",sep="")
INFO     <- read.csv(fileInfo,header=T)

startYr <- 1991
endYr   <- 2023
nosta   <- 96167 #lihat pilihan stasiun pada file "out_Data_CH_daily_infosta.csv"

#***************** Mulai dari sini tidak perlu diganti ***************
DATA    <- DATA[which(DATA[,1]>=startYr &DATA[,1]<=endYr),] 
idxData <- which(INFO$NoSTA==nosta)
data    <- DATA[,c(1,2,3,idxData+3)]

#fungsi untuk menghitung ch bulanan
source(paste(dirFungsi,"day2mon_precip.R",sep=""))
ch_mon<-day2mon_precip(data)
 
# tambal jika ada NaN
source(paste(dirFungsi,"fill_with_clim.R",sep=""))
ch_mon_ori     <- ch_mon$original
ch_mon_fillmis <- fill_with_clim(ch_mon_ori)
ch_mon_fill    <- ch_mon_fillmis$originalfill
ch_mon_clim    <- ch_mon_fillmis$climatology

# LANGKAH 1: Siapkan data deret waktu yang akan di cek stasioneritas- nya
source(paste(dirFungsi,"ori2tsmon.R",sep=""))
ts_ch_mon <- ori2tsmon(ch_mon_fill)

#plot time series
library(ggplot2)
library(scales)
chmon_line <- ggplot(ts_ch_mon, aes(TglBln, VAL)) +
  geom_line(na.rm=TRUE) +  
  ggtitle(paste("Curah Hujan Bulanan\n", "Stasiun :",as.character(nosta),sep="") ) +
  xlab("Bulanan") + ylab("Curah Hujan (mm)") +
  scale_x_date(breaks = date_breaks("2 year"),labels = date_format("%b %y")) +
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 15)) +
  theme(text = element_text(size=10))
chmon_line

#LANGKAH2: Uji stasioneritas dengan bentuk plot ACF
library(tseries)
ts_ch_mon <-ts(ts_ch_mon$VAL)
head(ts_ch_mon)
acfout<-acf(ts_ch_mon, main= paste("Plot ACF CH bulanan :", as.character(nosta),sep=""))
acfout
# acfout$acf

#LANGKAH3: Uji stasioneritas untuk uji ADF dengan  "tseries" library
adfout1<-tseries::adf.test(ts_ch_mon)
adfout1

#LANGKAH4: Uji stasioneritas untuk uji KPSS  dengan  "tseries" library
#Uji KPSS "Trend"
kpssout1a<-tseries::kpss.test(ts_ch_mon,null="Trend")
kpssout1a

#Uji KPSS "Level"
kpssout1b<-tseries::kpss.test(ts_ch_mon,null="Level")
kpssout1b

#LANGKAH5: Uji akar unit dengan library "ucra" untuk ur.kpss
library(urca)
urkpsstest1a <- ur.kpss(ts_ch_mon, type="mu", lags = "nil")
summary(urkpsstest1a)

urkpsstest1b <- ur.kpss(ts_ch_mon, type="tau", lags = "short")
summary(urkpsstest1b)
