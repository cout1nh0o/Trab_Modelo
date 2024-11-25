library(tidyverse)
library(patchwork)



# === Passos prévios ====

MARANHAO <- readxl::read_excel("MARANHAO.xlsx")

mar <- MARANHAO  %>%
  mutate(DT_INTER = as.Date(datahora, format = "%Y-%m-%d")) %>%
  select(-c(mun_geocod, mun_nome, datahora, mun_uf_nome)) %>%
  filter(DT_INTER >= "2018-01-01" &  DT_INTER <= '2018-12-31') %>%
  group_by(DT_INTER) %>%
  summarise(
    mean_temp = mean(temperatura_c, na.rm = TRUE),
    mean_umid = mean(umidade_relativa_percentual, na.rm = TRUE),
    median_temp = median(temperatura_c, na.rm = TRUE),
    median_umid = median(umidade_relativa_percentual, na.rm = TRUE),
    mean_pm2.5 = mean(pm25_ugm3, na.rm = TRUE),
    mean_co = mean(co_ppb, na.rm = TRUE),
    mean_no2 = mean(no2_ppb, na.rm = TRUE),
    mean_o3 = mean(o3_ppb, na.rm = TRUE),
    mean_so2 = mean(so2_ugm3, na.rm = TRUE),
    focos_queimada = sum(focos_queimada, na.rm = TRUE)
  )



# ==== Análise descritiva ====

# Histograma baseado na densidade para mean_umid
ggplot(mar, aes(x = mean_umid)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 fill = "cornflowerblue",
                 color = "black",
                 alpha = .6) +
  labs(x = "Umidade", y = "Densidade") +
  scale_x_continuous(limits = c(85, 100), 
                     breaks = seq(85, 100, by = 5)) +
  theme_minimal() -> h1

# Histograma baseado na densidade para mean_temp
ggplot(mar, aes(x = mean_temp)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 fill = "tomato",
                 color = "black",
                 alpha = .6) +
  labs(x = "Temperatura", y = "Densidade") +
  scale_x_continuous(limits = c(20, 35), 
                     breaks = seq(20, 35, by = 5)) +
  theme_minimal() -> h2

# Plotando um abaixo do outro (o patchwork precisa estar carregado)
h1 / h2

# Teste de normalidade para a variável resposta
shapiro.test(mar$mean_temp) # Não-normal
ks.test(mar$mean_temp, "pnorm", mean(mar$mean_temp), sd(mar$mean_temp)) # Normal
nortest::ad.test(mar$mean_temp) # Não-normal
nortest::lillie.test(mar$mean_temp) # Não-normal
tseries::jarque.bera.test(mar$mean_temp) # Não-normal


# Dispersão da temperatura por umidade
ggplot(mar, aes(x = mean_umid, y = mean_temp)) +
  geom_point(color = "cornflowerblue",
             size = 2,
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Umidade", y = "Temperatura") +
  scale_y_continuous(limits = c(20, 35),
                     breaks = seq(20, 35, by = 5)) +
  scale_x_continuous(limits = c(85, 100),
                     breaks = seq(85, 100, by = 5)) +
  theme_minimal() -> d1
d1

# Para plotar junto
# par(mfrow = c(2, 2))

# Série temporal da temperatura média
mt <- ggplot(mar, aes(x = DT_INTER, y = mean_temp)) +
  geom_line(color = "steelblue", size = .5) +
  geom_point(color = "cornflowerblue", size = 1) +
  labs(title = "Evolução da média da temperatura no tempo",
       x = "Data", 
       y = "Temperatura média") +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "1 month") +
  theme_minimal()

# Série temporal da umidade média
mu <- ggplot(mar, aes(x = DT_INTER, y = mean_umid)) +
  geom_line(color = "steelblue", size = .5) +
  geom_point(color = "cornflowerblue", size = 1) +
  labs(title = "Evolução da média da umidade no tempo",
       x = "Data", 
       y = "Umidade média") +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "1 month") +
  theme_minimal()

# Plotando um abaixo do outro (lembrar de carregar o patchwork)
mt / mu


# Correlação entre temperatura e umidade
cor(x = mar$mean_temp, y = mar$mean_umid) # -0.8504322


# Achados:

# 1. Há uma correlação linear negativa da ordem de 85%, indicando que uma modelagem
# linear entre essas duas variáveis é uma boa opção.
# 2. Essa relação pode ser vista graficamente, tanto na bi-dispersão, quanto na
# evolução temporal, onde se torna nítido o comportamento antagonista entre as
# variáveis.
# 3. O uso da mediana em vez da média não se mostrou interessante, pois ela não
# gera dados contínuos, apesar de acusar mais normalidade que a média (considerando
# o Shapiro-Wilk). A menos do teste de Kolmogorov-Smirnov, todos os outros
# recusaram normalidade da temperatura média. Assim, torna-se útil e inteligente
# utilizar o resultado do teste de K.S.



# === Modelo linear simples ====

# Modelagem
x <- mar$mean_umid
y <- mar$mean_temp

modelo <- lm(y ~ x, data = mar)

# Estatísticas e outras informações
summary(modelo)

# Diagnóstico visual
performance::check_model(modelo) # Lembrar de aumentar a região de plotagem

# Normalidade dos resíduos
ks.test(modelo$residuals, "pnorm", mean(modelo$residuals), sd(modelo$residuals)) # Normal

# Independência dos resíduos
lmtest::dwtest(modelo)

# Homocedasticidade
lmtest::bptest(modelo)

# anova(modelo) # Já sai no summary do modelo

# Coeficientes
coefficients(modelo)
confint.lm(modelo)

# Valores ajustados
ajustados <- modelo$fitted.values
ajustados_IC <- predict(modelo, interval = "confidence") # Pegar colunas 2 e 3



# Daqui pra baixo tem que avaliar a necessidade

### estimado sigma^2
(sigma2chapeu <- summary(modelo)$sigma ^ 2)


# Intervalo de predicao para y_novo
#usando uma funcao do R

(xnovo <-  x)
(pred_y <-  predict(modelo,
                    newdata = mar,
                    interval = "prediction",
                    level = gama))


# Gráfico de dispersão com reta ajustada e intervalos de confiança
plot(
  x,
  y,
  main = "Grafico de Dispersao com Intervalos de Confianca",
  xlab = "x",
  ylab = "y",
  bty = "n",
  ylim = c(min(y, pred_y), max(y, pred_y))
)
ordem = order(x)
# Adicionando o sombreado dos intervalos de confianca de y
polygon(c(x[ordem], rev(x[ordem])),
        c(pred_y[ordem, 2], rev(pred_y[ordem, 3])),
        col = "darkgray",
        border = NA)
# Adicionando o sombreado dos intervalos de confianca de mu
polygon(c(x[ordem], rev(x[ordem])),
        c(ICy_i_c[ordem, 2], rev(ICy_i_c[ordem, 3])),
        col = "blue",
        border = NA)
#adicionando a reta ajustada
abline(modelo, col = "red", lwd = 2)
#adicionando os valores observados
points(x, y)


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

### o grafico de y com os residos me pareceu estranho.
