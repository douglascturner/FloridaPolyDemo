rm(list = ls())
set.seed(100)
library(strucchange)
library(ggplot2)
library(car)
theme_set(theme_bw())

# Generate Data
N = 100
t = c(1:N)
e = rnorm(N,0,40)
y = 1 + 5*t + (10*t)*(t>50) + e
dummy_var = t>50
df = data.frame(y=y,t=t, dummy_var = dummy_var)

# Plot Data
gg <- ggplot(df, aes(x=t, y=y)) +
  geom_point() + 
  xlim(c(1, N)) + 
  ylim(c(0, 2500)) + 
  labs(y="Y", 
       x="Time")

plot(gg)

# Method 1: Test of Split Regressions vs. Pooled
# Full Model
full_model = lm(y~t, data = df)
SSR_full = sum(full_model$residuals^2)
df_full = 2
gg <- ggplot(df, aes(x=t, y=y)) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) + 
  xlim(c(1, N)) + 
  ylim(c(-400, 2500)) +
  labs(y="Y", 
       x="Time")
plot(gg)

# Restricted Model
# Model One (t<50)
Restricted_model_one = lm(y~t,data =df, subset=(t<=50))
SSR_res1 = sum(Restricted_model_one$residuals^2)
df_res1 = 2
# Model Two (t<50)
Restricted_model_two = lm(y~t,data =df, subset=(t>50))
SSR_res2 = sum(Restricted_model_two$residuals^2)
df_res2 = 2

gg <- ggplot(df, aes(x=t, y=y, color = dummy_var)) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) +  theme(legend.position="none") + 
  xlim(c(1, N)) + 
  ylim(c(-400, 2500)) + 
  labs(y="Y", 
       x="Time")
plot(gg)

gg <- ggplot(df,aes(x=t, y=y)) +
  geom_point() + 
  geom_smooth(data = subset(df, dummy_var==1), formula=y~x, method="lm", se=FALSE) + 
  geom_smooth(data = subset(df, dummy_var==0), formula=y~x, method="lm", se=FALSE) + 
  xlim(c(1, N)) + 
  ylim(c(-400, 2500)) + 
  labs(y="Time", x="Y")
plot(gg)


F_stat = (SSR_full - (SSR_res1 + SSR_res2))/(SSR_res1 + SSR_res2) * ((N-2*df_full)/df_full)
F_stat

# Method 2: Using strucchange package
sctest(y~t, data = df, type = "Chow", point = 50)

# Method 3: Dummary Variable Approach: Using Joint F test of Coefficients 
dummy_var = (t>50)
dummy_var_t = (t>50)*t
dummy_model = lm(y~t+dummy_var+dummy_var_t, data = df)
#summary(dummy_model)
#F_stat
linearHypothesis(dummy_model, c("dummy_varTRUE=0", "dummy_var_t=0")) #from car package