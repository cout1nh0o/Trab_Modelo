# Gráfico de série temporal
ggplot(mar, aes(x = DT_INTER, y = mean_umid)) +
  geom_line(color = "steelblue", size = .5) +          # Linha da série temporal
  geom_point(color = "steelblue", size = 1) +         # Pontos na série
  labs(title = "Série Temporal",           # Título
       x = "Data", 
       y = "Valor") +
  scale_x_date(date_labels = "%b %Y",                 # Formato do eixo X (meses/anos)
               date_breaks = "1 month") +             # Quebra do eixo X (1 mês)
  theme_minimal()                                     # Tema minimalista


temp_ts <- ts(mar$mean_temp, start = min(mar$DT_INTER), frequency = 12)
umid_ts <- ts(mar$mean_umid, start = min(mar$DT_INTER), frequency = 12)

decomp_temp <- decompose(temp_ts)
plot(decomp_temp)

temp_ts2 <- temp_ts - decomp_temp$seasonal
plot(temp_ts2)

decomp_umid <- decompose(umid_ts)
plot(decomp_umid)

umid_ts2 <- temp_ts - decomp_umid$seasonal
plot(umid_ts2)

x <- as.double(umid_ts2)
y <- as.double(temp_ts2)
modelo <- lm(y ~ x, data = mar)

summary(modelo)
# performance::check_model(modelo)


(residuos <-  y - modelo$fitted.values) #residuals(ajuste)
(plot(round(residuos - modelo$residuals, 9)))


hist(residuos,
     freq = F,
     main = "Histograma",
     ylab = "densidade")
curve(dnorm(x, 0, sqrt(sigma2chapeu)),
      add = T,
      lwd = 2,
      col = "red")

qqnorm(residuos,
       xlab = "quatis teóricos",
       ylab = "quantis amostrais",
       bty = "n")
qqline(residuos, col = "red", lwd = 2)

shapiro.test(residuos)

ks.test(residuos, "pnorm", 0, sqrt(sigma2chapeu))

ks.test(residuos, "pnorm", mean(residuos), sd(residuos))

par(mfrow = c(2, 2))
plot(x, residuos)
plot(y, residuos)
plot(residuos)
