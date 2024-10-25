#Skrip by: supmawatimeysi@gmail.com
#Skrip bertujuan untuk mengubah ch harian menjadi CH bulanan
#Skrip juga mengisi nilai CH bulanan yang kosong dengan Nilai Klimatologi
#Skrip ini menampilkan plot time series dan klimatologi, serta melakukan dekomposisi
#skrip ini menggunakan Fungsi "day2mon_ch.R"

#******** SESUAIKAN BAGIAN BERIKUT INI *******************************************************
setwd("C:/Users/lenio/Downloads/P1/") #set working direktory
rm(list = ls()) #hapus semua variable di environmen

dirData    <- "C:/Users/lenio/Downloads/P1/data_precipitation_daily/"
dirInfo    <- "C:/Users/lenio/Downloads/P1/data_precipitation_daily/"
dirFungsi  <- "C:/Users/lenio/Downloads/P1/add_function/"
dirHasil   <- "C:/Users/lenio/Downloads/P1/hasil/"
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
#simpan nilai ch setelah ditambal
fileOut <- paste(dirHasil,"CHori_bulanan_",as.character(nosta),".csv",sep="")
write.csv(ch_mon_ori, file=fileOut, row.names=F)
fileOut <- paste(dirHasil,"CHfill_bulanan_",as.character(nosta),".csv",sep="")
write.csv(ch_mon_fill, file=fileOut, row.names=F)

library(ggplot2)
#tampilkan plot klimatologi
hedmon <- c("JAN","FEB","MAR","APR","MEI","JUN","JUL","AGT","SEP","OKT","NOV","DES")

ch_clim   <- data.frame(y=ch_mon_clim, x=hedmon)
ch_clim$x <- as.character(ch_clim$x)
ch_clim$x <- factor(ch_clim$x,levels=unique(ch_clim$x))
ggplot(ch_clim,aes(x,y)) + 
  geom_bar(stat="identity",fill="orange",width=0.4) + theme_bw()+
  labs(title = paste("Rata-rata Ch bulanan: ",nosta,sep=""),x = "Bulan", y = "CH (mm)")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

# buat series dari data yang sudah di "tambal"
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

#dekompoisii time-series
#terkebih dahulu install library "ggplottimeseries"
devtools:: install_github("brisneve/ggplottimeseries")
library(ggplottimeseries)
library(tidyr)
x  <- ts_ch_mon$TglBln
y  <- ts_ch_mon$VAL
z  <- 12 #number of month in a year
df <- dts1(x,y,z, type = "additive")
head(df)
#plots decomposed time series into one figure
ggdecompose(df)+
  xlab("Bulanan")+
  ylab("CH")+
  ggtitle(paste("Dekomposisi data CH ",as.character(nosta),sep=""))
#ambil data seasonal
df_seas <- df$seasonal[1:12]
#Bandingkan seasonal dengan klimatologi
ch_clim_seas <- data.frame(y=ch_mon_clim, z=df_seas,x=hedmon)
#plot perbandingan nilai CH selama periode testing
par(mar = c(5, 5, 3, 5))
plot(ch_clim_seas[,"y"], type ="l", ylab = "CH (mm)",
     main = "Perbandingan: Klimatologi vs Seasonality", xlab = "Bulanan",
     col = "blue",xaxt="n")
par(new = TRUE)
plot(ch_clim_seas[,"z"], type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("Seasonality", side = 4, line = 3)
legend("bottomright", c("KLIM", "SEAS"),col = c("blue", "red"), lty = c(1, 2),cex=0.5)
ticks <- seq(1,12,1)
axis(side = 1, at = ticks,labels = hedmon)

#*********************** SELESAI****************************************
#*
#*
#
# #fungsi untuk menghitung ch bulanan
# source(paste(dirFungsi,"dec2mon_precip.R",sep=""))
# data<- ch_dec$original
# ch_mon<-dec2mon_precip(data)

# # fungsi untuk menghitung ch bulanan
# source(paste(dirFungsi,"day2mon_precip.R",sep=""))
# ch_mon<-day2mon_precip(data)
