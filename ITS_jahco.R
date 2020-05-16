# Data load ---------------------------------------------------------------
nile <- read_csv("~/Desktop/MyProject/nile.csv")


# Libraries ---------------------------------------------------------------
p_load(car, nlme)


# Plot --------------------------------------------------------------------
plot(nile$time, 
     nile$flow,
     ylab = "Water Flow",
     ylim = c(0, 4500),
     xlab = "Year",
     type = "l",
     col = "red",
     xaxt = "n")
axis(1, at = 1:60, labels = nile$year)
points(nile$time,
       nile$flow,
       col = "red",
       pch = 20)
abline(v = 27.5, lty = 2)

model_ols <- lm(flow ~ time + level + trend, data=nile)
summary(model_ols)
dwt(model_ols, max.lag = 12, alternative = "two.sided")

par(mfrow=c(2,1))
acf(residuals(model_ols))
acf(residuals(model_ols), type = "partial")

model_p10 <- gls(flow ~ time + level + trend, 
                 data = nile, 
                 correlation = corARMA(p = 10, form = ~time), 
                 method = "ML")

summary(model_p10)
summary(model_ols)
 View(nile)

 
nilehuron <- read_csv("https://courses.edx.org/assets/courseware/v1/f3a6d640dae70ea8db7b24202a561baa/asset-v1:UBCx+ITSx+1T2017+type@asset+block/nilehuron.csv") 
nilehuron <- nilehuron %>% 
       group_by(nile) %>% 
       mutate(time = row_number(),
              level = ifelse(time < 28, 0, 1),
              trend = ifelse(level == 1, cumsum(level), 0)) %>% 
       ungroup() %>% 
       mutate(niletime = ifelse(nile == 1, time, 0),
              nilelevel = ifelse(nile == 1, level, 0),
              niletrend = ifelse(nile == 1, trend, 0))


par(mfrow=c(1,1))
#Adding control
plot(nilehuron$time[1:60], nilehuron$flow[1:60], 
     ylab = "Water Flow",
     ylim = c(0, 4500),
     xlab = "Year",
     type = "l",
     col = "red",
     xaxt = "n")
points(nilehuron$time[61:120], nilehuron$flow[61:120],
      type = "l",
      col = "blue")
axis(1, at = 1:60, labels = nilehuron$year[1:60])
points(nilehuron$time[1:60], nilehuron$flow[1:60],
       pch = 20,
       col = "red")
points(nilehuron$time[61:120], nilehuron$flow[61:120],
       pch = 20,
       col = "blue")
abline(v = 27.5, lty = 2)
legend(x = 3, y = 1000,
      legend = c("Nile", "Huron"),
      col = c("red", "blue"), pch = 20)

#Simple ols (not accounting for autocorrelation)
model <- lm(flow ~ time + nile + niletime + level + trend + nilelevel + niletrend, data = nilehuron)
summary(model)
model2 <- lm(flow ~ time + nile + nile*time + level + trend + nile*level + nile*trend, data = nilehuron)
summary(model2)
confint(model)
par(mfrow = c(2, 1))
acf(residuals(model))
acf(residuals(model), type = "partial")
#There is a spike in 10 lag in the partial and exponential decay y de regular. This indicates autoregressive p = 10 (not detected was moving average)

#To correct for autocorrelation again use gls() from nlme
model_p10 <- gls(
  model = flow ~ time + nile + niletime + level + trend + nilelevel + niletrend,
  data = nilehuron,
  correlation = corARMA(p = 10, form = ~time | nile),
  method = "ML"
)
summary(model_p10)
summary(model)
confint(model_p10)




outome ~ time + dummyGrupo + time*dummyGrupo + dummyAPartirTiempo0 + TrendAPartirTiempo0 + DummyGrupo*dummyAPartirTiempo0 + DummyGrupo*TrendAPartirTiempo0
  
  
sjPlot::plot_model(model_p10, type = "pred")

model3 <- lm(flow ~ nile + time + time*nile, data = nilehuron)
sjPlot::plot_model(model3, type = "int")
model3



model_p10_2 <- gls(
  model = flow ~ nile + time + time*nile + level + trend + level*nile + trend*nile,
  data = nilehuron,
  correlation = corARMA(p = 10, form = ~time | nile),
  method = "ML"
)

sjPlot::plot_model(model_p10_2, type = "pred")

summary(model_p10_2)


nile <- nile %>% 
        mutate(.data =., 
               drought = ifelse(time == 43, 1, 0))

model_drought <- lm(flow ~ time + level + trend + drought,
                    data = nile)
summary(model_drought)
summary(model_ols)

model_p10_drouhgt <- gls(flow ~ time + level + trend + drought,
                         data = nile,
                         correlation = corARMA(
                           p = 10, form = ~time
                           ),
                         method = "ML")