############################################################
############### Code for Hypothesis Testing ################
############################################################
#### Part I: One Sample with Normal Distribution N(mu,sigma^2)
#### Example One: Test for mu if sigma^2 is known
mu0 = 8
x = c(8.05,8.15,8.2,8.1,8.25)
n = length(x)
sigma = 0.2
bar.x = mean(x) ## calculate the mean of sample
## Method One
z = (bar.x-mu0)/sigma*sqrt(n)
## Method Two
p.value = (1-pnorm(z))*2
## Method Three
library(BSDA)
z.test(x = x, # one sample
       alternative="two.sided", # "greater","less","two.sided"(default) 
       mu = mu0, # the null hypothesis
       sigma.x=0.2) # the pre-determined population sd

#### Example Two: Test for mu if sigma^2 is unknown
mu0 = 240
x = c(239.7,239.6,239,240,239.2)
n = length(x)
bar.x = mean(x) ## calculate the mean of sample
sd.x = sd(x) ## calculate the standard deviation of sample
## Method One
t = (bar.x-mu0)/sd.x*sqrt(n)
## Method Two
p.value = (1-pt(abs(t),df=n-1))*2
## Method Three
library(BSDA)
z.test(x = x, # one sample
       alternative="two.sided", # "greater","less","two.sided"(default) 
       mu = mu0, # the null hypothesis
       sigma.x=0.2) # the pre-determined population sd

#### Example Three: Test for sigma^2
sigma2 = 0.016
n = 25
var.x = 0.025
alpha = 0.05
## Method One
chisq2 = (n-1)*var.x/sigma2
chisq2>=qchisq(1-alpha,n-1) 
# Result: (1) TRUE=>Reject H_0 (2) FALSE=>Not reject H_0
## Method Two
p.value = 1-pchisq(chisq2,n-1)
p.value < alpha
# Result: (1) TRUE=>Reject H_0 (2) FALSE=>Not reject H_0
## Method Three
## var.test(x) # If we have real data, we could use this function!!!

#### Part II: Two Samples with Normal Distribution N(mu1,sigma12) and N(mu2,sigma22)
#### Example Four: Test for mu1-mu2
x = c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04,
      79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
y = c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)
sd.x = 0.025
sd.y = 0.030
m = length(x)
n = length(y)
bar.x = mean(x)
bar.y = mean(y)
## Method One
z = (bar.x-bar.y)/sqrt(sd.x^2/m+sd.y^2/n)
## Method Two
p = (1-pnorm(z))*2
## Method Three
library(BSDA)
z.test(x,y,sigma.x=sd.x,sigma.y =sd.y)

#### Example Five: Test for mu1-mu2
x = c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04,
      79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
y = c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)
alpha=0.05
m = length(x)
n = length(y)
bar.x = mean(x)
bar.y = mean(y)
sd.x = sd(x)
sd.y = sd(y)
sw2 = (sd.x^2*(m-1) + sd.y^2*(n-1))/(m+n-2)
sw = sqrt(sw2)
## Method One
t0 = (bar.x-bar.y)/sw/sqrt(1/m+1/n)
t = qt(1-alpha/2,m+n-2)
I(t0>t)
# Result: (1) TRUE=>Reject H_0 (2) FALSE=>Not reject H_0
## Method Two
p = (1-pt(t0,m+n-2))*2
I(p<alpha) 
# Result: (1) TRUE=>Reject H_0 (2) FALSE=>Not reject H_0
## Method Three
t.test(x,y,var.equal = TRUE)


#### Example Six: Test for mu1-mu2 (Independent t test)
x = c(76.43, 76.21, 73.58, 69.69, 65.29,
      70.83, 82.75, 72.34)
y = c(73.66, 64.27, 69.34, 71.37, 69.77,
      68.12, 67.27, 68.07, 62.61)
m = length(x)
n = length(y)
bar.x = mean(x)
bar.y = mean(y)
sd.x = sd(x)
sd.y = sd(y)
sw2 = 1/(m+n-2)*((m-1)*sd.x^2+(n-1)*sd.y^2)  
sw = sqrt(sw2)
## Method One
t0 = (bar.x-bar.y)/sw/sqrt(1/m+1/n)
t = qt(1-alpha,m+n-2)
I(t0>t)
## Method Two
p = (1-pt(t0,m+n-2))
I(p<alpha)
## Method Three
t.test(x,y,var.equal = TRUE,alternative = "greater")


#### Example Seven: Test for mu1-mu2 (Paired t test)
x = c(23,35,29,42,39,29,37,34,35,28)
y = c(30,39,35,40,38,34,36,33,41,31)
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
## Using paired t test
alpha=0.05
d = x-y
bar.d = mean(d)
sd.d = sd(d)
n = length(d)
## Method One
t0 = bar.d/sd.d*sqrt(n)
t = qt(1-alpha/2,n-1)
I(abs(t0)>abs(t))
## Method Two
p = 2*(1-pt(abs(t0),n-1))
I(p<alpha)
## Method Three
t.test(x,y,paired=TRUE)

#### Example Eight: Test for sigma12/sigma22
x = c(16.2, 16.4, 15.8, 15.5, 16.7, 15.6, 15.8)
y = c(15.9, 16.0, 16.4, 16.1, 16.5, 15.8, 15.7, 15.0)
m = length(x)
n = length(y)
var.x = var(x)
var.y = var(y)
alpha=0.05
## Method One
F0 = var.x/var.y
I(F0<qf(alpha/2,m-1,n-1) | F0>qf(1-alpha/2,m-1,n-1))
## Method Two
p = 2*min(pf(F0,m-1,n-1),1-pf(F0,m-1,n-1))
I(p<alpha)
## Method.Three
var.test(x,y)

#### Part III: One Sample with Other Distributions 
#### Example Nine: Test for theta (Exponential distribution Exp(1/theta))
x = c(395, 4094, 119, 11572, 6133)
n = length(x)
bar.x = mean(x)
theta0 = 6000
alpha=0.05
## Method One
chi02 = 2*n*bar.x/theta0
chi2q = qchisq(alpha,2*n)
## Method Two
p = pchisq(chi02,2*n)
