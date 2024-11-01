library(dplyr)
library(ggplot2)
MARANHAO <- readxl::read_excel("MARANHAO.xlsx")

mar <- MARANHAO  %>%   
  mutate(DT_INTER = as.Date(datahora, format="%Y-%m-%d"))%>%
  select(-c(mun_geocod,mun_nome,datahora,mun_uf_nome))%>%
  filter(DT_INTER >= "2018-01-01" &  DT_INTER <= '2018-12-31')%>%
  group_by(DT_INTER)%>%
  summarise(max_temp = max(temperatura_c,na.rm = TRUE), 
            min_umid = min(umidade_relativa_percentual,na.rm = TRUE),
            max_pm2.5 = max(pm25_ugm3,na.rm = TRUE),
            md_co= mean(co_ppb,na.rm=TRUE),
            md_no2= mean(no2_ppb,na.rm=TRUE),
            md_o3= mean(o3_ppb,na.rm=TRUE),
            md_so2= mean(so2_ugm3,na.rm=TRUE),
            focos_queimada=sum(focos_queimada,na.rm = TRUE))

#temperatura X umidade
ggplot(mar, 
       aes(x = max_temp, 
           y = min_umid)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Temperatura",
       y = "Umidade") +
theme_classic()


#### Temperatura por umidade parece ter uma relação linear negativa e bem ajustada. 
#### Dentre todos gráficos que Amanda mandou, esse parece ser o mais interessante.

y <- mar$min_umid
x <- mar$max_temp
modelo <- lm(min_umid ~ max_temp, data = mar)

b0chap <- coef(modelo)[1]
###B0 estimado = 159.8799 

b1chap <- coef(modelo)[2]
###B1 estimado = -2.324915  

### calculando y_ajustado
y_ajustado <- predict(modelo, mar)

### estimado sigma^2
sigma2chapeu <- summary(modelo)$sigma^2
# ou 
sum((mar$min_umid - y_ajustado)^2)/363

### somatórios
sxy <- sum((x - mean(x)) * (y - mean(y)))
sxx <- sum((x - mean(x))^2)
syy <- sum((y - mean(y))^2)

### IC de 95% de confiança para B0 
ICb0 <- confint(modelo)[1,]

### IC de 95% de confiança para B0 
ICb1 <- confint(modelo)[2,]

### IC para y_ajustado
ICy_i_c <-  predict(modelo, interval = "confidence")
ICy_i_c[,2:3]
###### Teste de hipótese para B0
tb0 <- b0chap/sqrt(sigma2chapeu * (1/length(x) + mean(x)^2/sxx))
### p-valor para B0
#p0 <- 2*pt(-abs(tb0), df = summary(modelo)$df[1])
p01 <- 2 * (1 - pt(abs(tb0), df=summary(modelo)$df[2]))
p01<0.005
t <- dt(0.975, 363)

tb0 > t
### Rejeita h0, logo há evidencias para acreditar que b0 é diferente de 0 

##### Teste de hipótese para B1
t_b1 <- b1chap/sqrt(sigma2chapeu/sxx)
### p-valor para B1
 # p1 <- 2*pt(-abs(t_b1), df = summary(modelo)$df[1])
  p12 <- 2 * (1 - pt(abs(t_b1),df = summary(modelo)$df[2] ))
t <- dt(0.975, 363)

abs(t_b1) > t

p12 < 0.005
#Em geral vamos usar o pvalor deste teste para tomar a decis˜ao. Ent˜ao H0 ser´a rejeitada
#se o p-valor do teste for pequeno e aceita caso contr´ario
### Pelo p-valor rejeita h0, mas pela estatistica de teste rejeita h0, logo há evidencias para acreditar que b0 é diferente de 0 



###### 

# Intervalo de predicao para y_novo
#usando uma funcao do R
xnovo <-  x
pred_y <-  predict(modelo, newdata=mar, interval = "prediction", level = gama)


# Gráfico de dispersão com reta ajustada e intervalos de confiança
plot(x, y, main = "Grafico de Dispersao com Intervalos de Confianca",
     xlab = "x", ylab = "y", bty="n", ylim=c(min(y, pred_y), max(y, pred_y)))
ordem = order(x)
# Adicionando o sombreado dos intervalos de confianca de y
polygon(c(x[ordem], rev(x[ordem])), 
        c(pred_y[ordem,2], rev(pred_y[ordem,3])), 
        col = "darkgray", border = NA)
# Adicionando o sombreado dos intervalos de confianca de mu
polygon(c(x[ordem], rev(x[ordem])), 
        c(ICy_i_c[ordem,2], rev(ICy_i_c[ordem,3])), 
        col = "blue", border = NA)
#adicionando a reta ajustada
abline(ajuste, col = "red", lwd = 2)
#adicionando os valores observados
points(x,y)



##### ANOVA 

tabAnova <- anova(modelo)


residuos <-  y - modelo$fitted.values #residuals(ajuste)
plot(round(residuos-modelo$residuals,9))


hist(residuos, freq=F, main="Histograma", ylab="densidade")
curve(dnorm(x, 0, sqrt(sigma2chapeu)), add=T, lwd=2, col="red")

qqnorm(residuos, xlab="quatis teóricos", ylab="quantis amostrais", bty="n")
qqline(residuos, col="red", lwd=2)

shapiro.test(residuos)

ks.test(residuos,"pnorm",0, sqrt(sigma2chapeu))

ks.test(residuos,"pnorm",mean(residuos), sd(residuos))

par(mfrow=c(2,2))
plot(x, residuos)
plot(y, residuos)
plot(residuos)

### o grafico de y com os residos me pareceu estranho. 
