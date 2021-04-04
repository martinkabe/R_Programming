# Simulation of non-covered mu --------------------------------------------
par(mfrow=c(1,1)) # one chart in one screen
vect <- replicate(200, NA)
for (j in 1:200) {
  heights <- replicate(100, rnorm(100, mean = 69, sd = 3))
  tint <- matrix(NA, nrow = dim(heights)[2], ncol = 2)
  for (i in 1:dim(heights)[2]) {
    temp <- t.test(heights[, i], conf.level = 0.90)
    tint[i, ] <- temp$conf.int
  }
  colnames(tint) <- c("lcl", "ucl")
  tint <- data.frame(tint)
  indx <- (tint$lcl <= 69) & (tint$ucl >= 69)
  vect[j]  <- sum(indx)
}

hist(vect, freq = FALSE, breaks = "sturges", main = paste("Histogram pro frekvence IS nepokryvajicich mu", "\n","(v prumeru: )", mean(vect), sep=" "),
     xlab = "IS", ylab = "frekvence", col = "lightgreen", xlim=c(80,100))
curve(dnorm(x, mean=mean(vect), sd=sd(vect)), 0, 100, add=TRUE, col="darkblue", lwd=2) 
abline(v = mean(vect), col = "red", lwd = 3)




# Confidence intervals ----------------------------------------------------
par(mfrow=c(1,2)) # two charts in one screen

# Generate random sample
set.seed(32)
values <- replicate(100, rnorm(100, mean = 69, sd = 3))

tint = as.data.frame(do.call(rbind,
        lapply(1:dim(values)[2], function(x) {
          t.test(values[, x], conf.level = 0.90)$conf.int
        })))

colnames(tint) <- c("lcl", "ucl")

indx <- (tint$lcl <= 69) & (tint$ucl >= 69)
sum(indx)

# empty plot
plot(NA, xlim=c(0,dim(values)[2]), ylim=c(min(min(tint$lcl), min(tint$ucl)), max(max(tint$lcl), max(tint$ucl))), 
     xlab="Poradi IS", 
     ylab="Hodnota merene veliciny",
     main=paste("Intervalu pokryvajicich mu:",  sum(indx), " \n a intervalu nepokryvajicich mu:", (dim(values)[2])-sum(indx), sep=" "))

for (i in 1:dim(values)[2]) {
  if ((tint$lcl[i] <= 69) & (tint$ucl[i] >= 69)) {
    segments(i, tint$lcl[i], i, tint$ucl[i], col= 'blue')
  }
  else {
    segments(i, tint$lcl[i], i, tint$ucl[i], col= 'blue', lwd = 3)
  }
}
abline(h = 69, col = "red", lwd = 3)

# Simulation of IS covering mu --------------------------------------------

vect <- replicate(100, NA)
for (j in 1:length(vect)) {
  values <- replicate(100, rnorm(100, mean = 69, sd = 3))
  tint <- matrix(NA, nrow = dim(values)[2], ncol = 2)
  for (i in 1:dim(values)[2]) {
    temp <- t.test(values[, i], conf.level = 0.90)
    tint[i, ] <- temp$conf.int
  }
  colnames(tint) <- c("lcl", "ucl")
  tint <- data.frame(tint)
  indx <- (tint$lcl <= 69) & (tint$ucl >= 69)
  vect[j]  <- sum(indx)
}

hist(vect, freq = FALSE, breaks = "sturges", main = paste("Histogram pro frekvence IS pokryvajicich mu", "\n","(v prumeru:", mean(vect), ")", sep=" "),
     xlab = "Intervaly IS", ylab = "Pravdepodobnost", col = "lightgreen", xlim=c(80,100))
curve(dnorm(x, mean=mean(vect), sd=sd(vect)), 80, 100, add=TRUE, col="darkblue", lwd=2) 
abline(v = mean(vect), col = "red", lwd = 3)
