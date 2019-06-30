
library(tidyverse)
library(purrr)

n=50
sim_data = tibble(
  x = rnorm(n,10,1),
  y = rnorm(n,1,1)+x*rnorm(n,0,1)
)

list = c(1:n)

generate.theta = function(data,theta0){
  
#   sim_data = tibble(
#               x = rnorm(n),
#               y = rnorm(n)+x*rnorm(n,theta0,100)
# )
 l =  sample(list,replace = TRUE)
 x = data[l,1]
 y = data[l,2]
 
tibble(
theta = as.numeric(cov(x,y)/var(x)),
theta0,
diff = sqrt(n)*(theta-theta0)
)
}

res = vector("list",10000)

for (i in 1:800) {
  res = rbind(res,generate.theta(sim_data,theta0 = (i-400)/100))
}
res = bind_rows(res)

ggplot(data = res,aes(x = diff))+geom_density()



generate.theta(sim_data,theta0 = 6)

















