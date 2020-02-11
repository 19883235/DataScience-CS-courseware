#### Example Seven: Test for mu1-mu2 (Paired t test)
x = c(0.133,0.141,0.134,0.137,0.136,0.137)
y = c(0.141,0.147,0.144,0.143,0.142,0.146)
alpha=0.05
m = length(x)
n = length(y)
bar.x = mean(x)
bar.y = mean(y)
sd.x = sd(x)
sd.y = sd(y)
sw2 = 1/(m+n-2)*((m-1)*sd.x^2+(n-1)*sd.y^2)
sw = sqrt(sw2)
## Using independent t test
## Method One
t0 = (bar.x-bar.y)/sw/sqrt(1/m+1/n)
t = qt(1-alpha/2,m+n-2)
I(abs(t0)>abs(t))
## Method Two
p = 2*(1-pt(abs(t0),m+n-2))
I(p<alpha)
## Method Three
t.test(x,y,var.equal = TRUE)

