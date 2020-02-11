#################################
#### Lecture 3
#################################

## plot of p.d.f (standard normal)
mu = 0
sigma = 1
n = 1000
x = seq(from = mu - 3*sigma, to = mu+3*sigma,length.out = n)
y = dnorm(x,mu,sigma)
plot(x,y,type="l",xlab="",ylab="density")

## plot of c.d.f (binomial)
total = 5
prob = 0.5
x = c(0:total)
y = pbinom(c(0:10),size=total,prob)
plot(0:(total),(0:(total))/total,type="n",xaxt="n",bty="n",xlab=expression(x),ylab="c.d.f",yaxt="n")
axis(1,at=c(0:(total)),pos=0)
axis(2,at=(0:total)/total,pos=0)
for (i in 0:(total)){
  segments(x0=i,y0=y[i+1],x1=i+1,y1=y[i+1])     
  points(i,y[i+1],pch=16,col="black")
  points(i+1,y[i+1],pch=21,bg="white")
}

## plot of random numbers (exponential)
lambda = 1
n = 2000
x = rexp(n,lambda)
hist(x,freq=FALSE)
lines(density(x),col="red")
