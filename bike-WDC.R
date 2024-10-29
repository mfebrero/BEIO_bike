
# Instalar mejor desde GitHub: remotes::install_github("moviedo5/fda.usc")
library(fda.usc)

# Lectura de datos
data <- read.csv("./hour.csv")
l <- which(data$weekday == 6)
datan <- data[l, ]
tab <- table(datan$dteday, datan$hr)
dayf <- unique(rownames(which(tab == 0, arr.ind = TRUE)))
l <- which(data$weekday == 6 & !(data$dteday %in% dayf))
datan <- data[l, ]
ddays <- unique(datan$dteday)
tmin <- -8
tmax <- 39
atmin <- -16
atmax <- 50
fhum <- 100
fws <- 67

# Conversión a datos funcionales
tj <- seq(0.5, 23.5)
y0 <- fdata(mdata = matrix(datan$casual, ncol = 24, byrow = TRUE),
            argvals = tj, rangeval = c(0, 24),
            names = list(main = "NBCR", xlab = "Hora", ylab = "NBCR"))
nbcr <- apply(y0$data, 1, sum)
Temp <- fdata(mdata = matrix(tmin + datan$temp * (tmax - tmin), ncol = 24,
                             byrow = TRUE),
              argvals = tj, rangeval = c(0, 24),
              names = list(main = "Temperatura", xlab = "Hora", ylab = "ºC"))
Hum <- fdata(mdata = matrix(fhum * datan$hum, ncol = 24, byrow = TRUE),
             argvals = tj, rangeval = c(0, 24),
             names = list(main = "Humedad", xlab = "Hora", ylab = ""))
WS <- fdata(mdata = matrix(fws * datan$windspeed, ncol = 24, byrow = TRUE),
            argvals = tj, rangeval = c(0, 24),
            names = list(main = "Vel. Viento", xlab = "Hora", ylab = ""))
FT <- fdata(mdata = matrix(atmin + datan$atemp * (atmax - atmin), ncol = 24,
                           byrow = TRUE), argvals = tj, rangeval = c(0, 24),
            names = list(main = "Sens. térmica", xlab = "Hora",
                         ylab = "ºC"))
ldatm <- ldata(df = data.frame(nbcr = nbcr, lnbcr = log(nbcr)), y = y0,
               Temp = Temp, Hum = Hum, WS = WS, FT = FT)

# Exploración datos (Figura 1)
source("color.bar.R")
pdf(file = "bikerental.pdf", width = 10.67, height = 6)
ncolor <- cut(log(nbcr), 5)
cols <- colorRampPalette(c("blue", "orange"))(length(levels(ncolor)))
par(mfrow = c(1, 2))
plot(Hum, col = cols, ylim = c(0, 105), lwd = 2)
color.bar(cols, min(log(nbcr)), max(log(nbcr)), xpos = 0)
plot(FT, col = cols, ylim = c(-10, 50), lwd = 2)
color.bar(cols, min(log(nbcr)), max(log(nbcr)))
dev.off()

# Base de componentes principales (truncamiento elegido según SICc,
# ver test.Hum$p y test.FT$p)
b.x <- list(
  Temp = create.pc.basis(fdataobj = ldatm$Temp, l = 1:6),
  Hum = create.pc.basis(fdataobj = ldatm$Hum, l = 1:3),
  WS = create.pc.basis(fdataobj = ldatm$WS, l = 1:6),
  FT = create.pc.basis(fdataobj = ldatm$FT, l = 1:6)
)

# Regresiones y diagnósticos
rHum <- fregre.lm(lnbcr ~ Hum, data = ldatm, basis.x = b.x)
rFT <- fregre.lm(lnbcr ~ FT, data = ldatm, basis.x = b.x)
summary(rHum)
summary(rFT)

pdf(file = "FTregre.pdf", width = 10.67, height = 6)
par(mfrow = c(1, 2))
plot(rHum, which = 1, main = "Humedad")
plot(rFT, which = 1, main = "Sens. térmica")
dev.off()

# Tests
test.Hum <- rp.flm.test(Hum, log(nbcr), B = 2000)
test.Hum$p.values.fdr
test.Hum$p
test.FT <- rp.flm.test(FT, log(nbcr), B = 2000)
test.FT$p.values.fdr
test.FT$p

# CvM test Hum
par(mfrow = c(2, 5))
for (i in 1:10) {
  rr <- range(c(test.Hum$boot.proj.statistics[i, 1, ],
                test.Hum$proj.statistics[i, 1]))
  plot(density(test.Hum$boot.proj.statistics[i, 1, ]),
       main = paste0("Hum/CvM - R.P.:", i), xlim = rr)
  abline(v = test.Hum$proj.statistics[i, 1], col = "red")
}

# KS test Hum
par(mfrow = c(2, 5))
for (i in 1:10) {
  rr <- range(c(test.Hum$boot.proj.statistics[i, 2, ],
                test.Hum$proj.statistics[i, 2]))
  plot(density(test.Hum$boot.proj.statistics[i, 2, ]),
       main = paste0("Hum/KS - R.P.:", i), xlim = rr)
  abline(v = test.Hum$proj.statistics[i, 2], col = "red")
}

# CvM test FT
par(mfrow = c(2, 5))
for (i in 1:10) {
  rr <- range(c(test.FT$boot.proj.statistics[i, 1, ],
                test.FT$proj.statistics[i, 1]))
  plot(density(test.FT$boot.proj.statistics[i, 1, ]),
       main = paste0("FT/CvM - R.P.:", i), xlim = rr)
  abline(v = test.FT$proj.statistics[i, 1], col = "red")
}

# KS test FT
par(mfrow = c(2, 5))
for (i in 1:10) {
  rr <- range(c(test.FT$boot.proj.statistics[i, 2, ],
                test.FT$proj.statistics[i, 2]))
  plot(density(test.FT$boot.proj.statistics[i, 2, ]),
       main = paste0("FT/KS - R.P.:", i), xlim = rr)
  abline(v = test.FT$proj.statistics[i, 2], col = "red")
}
