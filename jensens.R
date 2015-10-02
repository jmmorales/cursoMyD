
a <- 1
b <- 0.15

ag <- 2
bg <- 5

curve(a*x/(b+x), xaxt = 'n', yaxt = 'n', xlab = "", ylab = "", lwd=3, bty = "l", xlim = c(0,1.2), xaxs = "i", yaxs = "i")
#abline(.2,1, lwd = 3, col = 2)
lines(seq(from=0,to=1.5,by=0.01), dgamma(seq(from=0,to=1.5,by=0.01),ag,bg)*0.1, lty=3, lwd=2)
#lines(rep(0.05,4),seq(0,0.3, length.out = 4), lty = 2, lwd=2)
#lines(rep(1,4),seq(0,0.9, length.out = 4), lty = 2, lwd=2)
#lines(seq(0.05,1, length.out = 5), 0.25+seq(0.1,1, length.out = 5)*.65, lty=2, lwd=2)
lines(rep(ag/bg, 4),seq(0, a*(ag/bg)/(b+(ag/bg)), length.out = 4), col="grey", lwd=3)
lines(seq(0, ag/bg, length.out = 4), rep(a*(ag/bg)/(b+(ag/bg)), 4), col="grey", lwd=3)

xs <- rgamma(100000, ag,bg)
ys <- a*xs/(b+xs)
aca = density(ys)
lines(aca$y *0.05, aca$x, lwd=2, lty=3)
arrows(-0.1, mean(ys), 0, mean(ys), angle=20, length=0.1, xpd = TRUE, lwd=2)


