rm(list = ls())
library(strucchange)
library(ggplot2)
library(car)
theme_set(theme_bw())

data("Nile")
Nile
# Generate Data
N = 100
t = c(1871:1970)
df = data.frame(y=Nile,t=t)

# Plot Data
gg <- ggplot(df, aes(x=t, y=y)) +
  geom_line() + 
  xlim(c(min(df$t), max(df$t))) + 
  ylim(c(min(df$y), max(df$y))) + 
  labs(y=bquote("Flow of Nile ("~10^8~"Cubic Meters)"), 
       x="Year", 
       title="Nile River Flows")

plot(gg)

full_F = Fstats(y~1, from =2, to = 98, data = df, vcov. = NULL)
df["F_stats"] = c(0,full_F$Fstats,0,0)
crit_value = boundary(full_F, alpha = .01)[1]
df["crit_val01"] = rep(crit_value,100)
crit_value = boundary(full_F, alpha = .001)[1]
df["crit_val001"] = rep(crit_value,100)


gg <- ggplot(df) +
  geom_line(aes(x=t, y=F_stats)) + 
  geom_line(aes(x=t, y=crit_val01), color = "red", linetype="dashed") + 
  geom_line(aes(x=t, y=crit_val001), color = "red") + 
  annotate("text", x=1965, y=df$crit_val01[1]+2, label= ".01 Level", size = 3) +
  annotate("text", x=1965, y=df$crit_val001[1]+2, label= ".001 Level", size = 3) +
  xlim(c(min(df$t), max(df$t))) + 
  ylim(c(min(df$F_stats), max(df$F_stats))) + 
  labs(y="F Stat", 
       x="Year", 
       title="Quandt Likelihood Ratio Example: River Nile Flows")

plot(gg)



