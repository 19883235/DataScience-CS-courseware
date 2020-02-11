#生成随机数有两个函数runif()和rnorm(),其中r表示的是random随机的意思，unif表示的是均匀分布，而norm表示的是正态分布。


# T4 
# 第一小问：
# 随机投点法求平均值

#先生成2n个随机数，n个数对
# n <- 1005001000
n = 5
# 为了使得每次的随机结果相同，要用到set.seed()
# 在设定种子的前提下生成10个随机数, 创建(x,y)向量
set.seed(1001)
x = runif (n, 0, 1)
y = runif (n, 0, 1)

g <- function(x) {exp(x)-1}/{exp(1)-1}
integrate(g,0,1)

n1 <- 0
for (i in 1:n)
{
   g <- function(x) {exp(x)-1}/{exp(1)-1}
   f1 = integrate(g(x[i]),0,1)
   if (y[i] <= f1)
     {
        n1 <- n1 + 1 
    }
}

J1 <- n1/n
print("采用随机投点法定积分J1的估计值为 ：", J1)


