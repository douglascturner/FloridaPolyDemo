rm(list = ls())
set.seed(100)
library(strucchange)
library(ggplot2)
library(car)
theme_set(theme_bw())

# Generate Data
N = 100
t = c(1:N)
e = rnorm(N,0,2)
y = 1 + 2*(t>50) + e
dummy_var = t>50
df = data.frame(y=y,t=t, dummy_var = dummy_var)

# Plot Data
gg <- ggplot(df, aes(x=t, y=y)) +
  geom_point() + 
  xlim(c(1, N)) + 
  ylim(c(min(df$y), max(df$y))) + 
  labs(y="Y", 
       x="Time", 
       title="Quandt Likelihood Ratio Example")

plot(gg)

full_F = Fstats(y~1, from =2, to = 98, data = df, vcov. = NULL)
df["F_stats"] = c(0,full_F$Fstats,0,0)
crit_value = boundary(full_F, alpha = .01)[1]
df["crit_val01"] = rep(crit_value,100)
crit_value = boundary(full_F, alpha = .05)[1]
df["crit_val05"] = rep(crit_value,100)
   
   
gg <- ggplot(df) +
  geom_line(aes(x=t, y=F_stats)) + 
  geom_line(aes(x=t, y=crit_val01), color = "red", linetype="dashed") + 
  geom_line(aes(x=t, y=crit_val05), color = "red") + 
  annotate("text", x=4, y=df$crit_val01[1]+.75, label= ".01 Level") +
  annotate("text", x=4, y=df$crit_val05[1]+.75, label= ".05 Level") +
  xlim(c(1, N)) + 
  ylim(c(0, max(df$F_stats))) + 
  labs(y="Y", 
       x="Time", 
       title="Quandt Likelihood Ratio Example")

plot(gg)


