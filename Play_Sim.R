

## Optimizing for parallel processing 
library(parallel)
library(doSNOW)
library(foreach)
library(plyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

# simulation function:
regression_sim = function(simNum, n, b0, b1, b2, b3, x1mean=0, x1_sd=1, err_mean=0, err_sd=1){
  x1 = rnorm(n, mean = x1_sd)
  x2 = sample(0:1, n, replace = TRUE)
  
  y = b0 + (b1* x1) + (b2 * x2) + (b3 * x1 * x2) + rnorm(n, mean = err_mean, sd = err_sd)
  
  model = lm(y~ x1 * x2)
  summary(model)
  #Pull info
  output = summary(model)$coefficients
  coefs = output[,1]
  ps = output[,4]
  Rsq = summary(model)$r.squared
  
  results = c(coefs, ps, Rsq)
  names(results) = c('b0_coef', 'b1_coef', 'b2_coef', 'b3_coef',
                     'b0_p', 'b1_p', 'b2_p', 'b3_p', 'rsq')
  return(results)
}

plot_finish = theme(plot.title = element_text(color= 'black',face='bold',size=18))+
  theme(axis.title.x = element_text(color= 'black',face='bold',size=18),
        axis.text.x = element_text(color= 'black',face='bold',size=18))+
  theme(axis.title.y = element_text(color= 'black',face='bold',size=18),
        axis.text.y = element_text(color= 'black',face='bold',size=18))+
  theme(# adjust X-axis labels; also adjust their position using margin (acts like a bounding box)
    # using margin was needed because of the inwards placement of ticks
    # http://stackoverflow.com/questions/26367296/how-do-i-make-my-axis-ticks-face-inwards-in-ggplot2
    axis.text.x = element_text( margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
    # adjust Y-axis labels
    axis.text.y = element_text( margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
    # length of tick marks - negative sign places ticks inwards
    axis.ticks.length = unit(-1.4, "mm"),
    # width of tick marks in mm
    axis.ticks = element_line(size = .8))

# prep for parallel processing:
ncpus = detectCores()  # number of cores to use, use -1 if you want to still use the PC for other things
cl = makeCluster(ncpus, type = 'SOCK')
registerDoSNOW(cl)

# Setup grid and run sim:
grid_input = expand.grid(n= c(50, 75, 100, 150, 200, 250), b1=c(.2,.5,.8), b2=c(.2,.5,.8),b3=c(.2,.5,.8))
results = NULL
num_sims = 1000
system.time(
  for(row in 1:nrow(grid_input)){
    sims = ldply(1:num_sims, regression_sim, n=grid_input[row, 'n'], b0=0, b1 = .3, b2=.2, b3 = grid_input[row,'b3'], .parallel = TRUE)
    sims$n = grid_input[row, 'n']
    sims$b1 = grid_input[row, 'b1']
    sims$b2 = grid_input[row, 'b2']
    sims$b3 = grid_input[row, 'b3']
    results = rbind(results, sims)
    

  })
power_est = NULL ##!!!!!!!!! Fix here... loop index does not work for group_by or summarize
for (this.effect.i in c('b1', 'b2', 'b3')) {
  power_est2 = results %>%
    group_by(n, assign(this.effect.i))%>%
    summarize(power = sum(assign(this.effect.i) < .05)/n())
  power_est2$main.effect = this.effect.i
  power_est = rbind(power_est, power_est2)

}
power_ests2 = results %>%
  group_by(n, b3)%>%
  summarize(power = sum(b3_p < .05)/n())

power_ests2$b3 = factor(power_ests2$b3)
# Get power and n then plot: $$ Need to make a main.effect column, n, power
power_est = pivot_longer(results[6:13],
                         cols = b1:b3, 
                         names_to = 'main.effect.size',
                         values_to = 'effect.size')

power_est = pivot_longer(power_est, 
                         cols = b1_p:b3_p,
                         names_to = 'main.effect.p',
                         values_to = 'p.value')
power_est = power_est[!duplicated(power_est$p.value),]


power_ests2 = power_est %>%
  group_by(n, b3)%>%
  summarize(power = sum(b3_p < .05)/n())


power_ests2long = gather(power_ests2, main_effect, b1:b3, factor_key = TRUE)

power_ests2$b1 = factor(power_ests2$b1)
power_ests2$b2 = factor(power_ests2$b2)
power_ests2$b3 = factor(power_ests2$b3)

ggplot(power_ests2, aes(x=n, y=power, group = b3, color= b3))+
  geom_point(size = 3.5)+
  geom_line(size = 1.3)+
  # stat_smooth(method = 'loess',  se = TRUE, aes(fill = b3), alpha = 0.3)+
  ylim(c(0,1))+
  geom_hline(yintercept =  .8, size = 1)+
  labs(title = paste("Estimated Power by N"))+
  xlab("Sample Size (N)")+ ylab("Estimated Power")+
  # theme_classic()+
  #coord_cartesian(ylim=c(-2, .5))+
  plot_finish


