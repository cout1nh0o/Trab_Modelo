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

modelo <- function(tipo = "max", dt_min, dt_max){
  
  if(tipo == "media"){
    mar <- MARANHAO  %>%   
      mutate(DT_INTER = as.Date(datahora, format="%Y-%m-%d"))%>%
      select(-c(mun_geocod,mun_nome,datahora,mun_uf_nome))%>%
      filter(DT_INTER >= dt_min &  DT_INTER <= dt_max)%>%
      group_by(DT_INTER)%>%
      summarise(temp = mean(temperatura_c,na.rm = TRUE), 
                umid = mean(umidade_relativa_percentual,na.rm = TRUE),
                max_pm2.5 = max(pm25_ugm3,na.rm = TRUE),
                md_co= mean(co_ppb,na.rm=TRUE),
                md_no2= mean(no2_ppb,na.rm=TRUE),
                md_o3= mean(o3_ppb,na.rm=TRUE),
                md_so2= mean(so2_ugm3,na.rm=TRUE),
                focos_queimada=sum(focos_queimada,na.rm = TRUE))
  }else if ( tipo == 'mediana'){
    mar <- MARANHAO  %>%   
      mutate(DT_INTER = as.Date(datahora, format="%Y-%m-%d"))%>%
      select(-c(mun_geocod,mun_nome,datahora,mun_uf_nome))%>%
      filter(DT_INTER >= dt_min &  DT_INTER <= dt_max)%>%
      group_by(DT_INTER)%>%
      summarise(temp = median(temperatura_c,na.rm = TRUE), 
                umid = median(umidade_relativa_percentual,na.rm = TRUE),
                max_pm2.5 = max(pm25_ugm3,na.rm = TRUE),
                md_co= mean(co_ppb,na.rm=TRUE),
                md_no2= mean(no2_ppb,na.rm=TRUE),
                md_o3= mean(o3_ppb,na.rm=TRUE),
                md_so2= mean(so2_ugm3,na.rm=TRUE),
                focos_queimada=sum(focos_queimada,na.rm = TRUE))
  }else{
    mar <- MARANHAO  %>%   
      mutate(DT_INTER = as.Date(datahora, format="%Y-%m-%d"))%>%
      select(-c(mun_geocod,mun_nome,datahora,mun_uf_nome))%>%
      filter(DT_INTER >= dt_min &  DT_INTER <= dt_max)%>%
      group_by(DT_INTER)%>%
      summarise(temp = max(temperatura_c,na.rm = TRUE), 
                umid = min(umidade_relativa_percentual,na.rm = TRUE),
                max_pm2.5 = max(pm25_ugm3,na.rm = TRUE),
                md_co= mean(co_ppb,na.rm=TRUE),
                md_no2= mean(no2_ppb,na.rm=TRUE),
                md_o3= mean(o3_ppb,na.rm=TRUE),
                md_so2= mean(so2_ugm3,na.rm=TRUE),
                focos_queimada=sum(focos_queimada,na.rm = TRUE))
  }
  
  
  #temperatura X umidade
 dispersao <- ggplot(mar, 
         aes(y = temp, 
             x = umid)) +
    geom_point(color="#1b6ca8", 
               size = 2, 
               alpha = .6) +
    geom_smooth(method = "lm",
                color = "#3aaf85",
                se = FALSE) +
    labs(y = "Temperatura",
         x = "Umidade") +
    theme_classic()
 

 
 y <- mar$temp
 x <- mar$umid
 n <- length(y)
 modelo <- lm(y ~ x, data = mar)
 
 b0chap <- coef(modelo)[1]
 ###B0 estimado = 159.8799 
 
 b1chap <- coef(modelo)[2]
 ###B1 estimado = -2.324915  
 
 ### calculando y_ajustado
 y_ajustado <- predict(modelo, mar)
 
 ### estimado sigma^2
 sigma2chapeu <- summary(modelo)$sigma^2
 
 
 ### somatórios
 sxy <- sum((x - mean(x)) * (y - mean(y)))
 sxx <- sum((x - mean(x))^2)
 syy <- sum((y - mean(y))^2)
 
 
 
 ### IC de 95% de confiança para B0 
 ICb0 <- confint(modelo)[1,]
 
 ### IC de 95% de confiança para B0 
 ICb1 <- confint(modelo)[2,]
 
 ### IC para y_ajustado
 
 # Grafico de dispersao com reta ajustada e intervalos de confianc

 ###### Teste de hipótese para B0
 tb0 <- b0chap/sqrt(sigma2chapeu * (1/length(x) + mean(x)^2/sxx))
 ### p-valor para B0
 #p0 <- 2*pt(-abs(tb0), df = summary(modelo)$df[1])
 p01 <- 2 * (1 - pt(abs(tb0), df=summary(modelo)$df[2]))
 p01<0.005
 t <- dt(0.975, n-2)
 
 tb0 > t
 ### Rejeita h0, logo há evidencias para acreditar que b0 é diferente de 0 
 
 ##### Teste de hipótese para B1
 t_b1 <- b1chap/sqrt(sigma2chapeu/sxx)
 ### p-valor para B1
 # p1 <- 2*pt(-abs(t_b1), df = summary(modelo)$df[1])
 p12 <- 2 * (1 - pt(abs(t_b1),df = summary(modelo)$df[2] ))
 t <- dt(0.975, n-2)
 
 abs(t_b1) > t
 
 p12 < 0.005
 #Em geral vamos usar o pvalor deste teste para tomar a decis˜ao. Ent˜ao H0 ser´a rejeitada
 #se o p-valor do teste for pequeno e aceita caso contr´ario
 ### Pelo p-valor rejeita h0, mas pela estatistica de teste rejeita h0, logo há evidencias para acreditar que b0 é diferente de 0 
 ICy_i_c <-  predict(modelo, interval = "confidence")
 ICy_i_c[,2:3]
 
 dispersao_ic <- ggplot(mar, aes(x = x, y = y)) +
   geom_point(color="#1b6ca8") +  # Adiciona os pontos do gráfico de dispersão
   geom_smooth(method = "lm", color = "#3aaf85", fill = "darkgray", alpha = 0.2) +  # Adiciona a reta de regressão e o intervalo de confiança
   labs(title = "Gráfico de Dispersão com Intervalos de Confiança para as médias",
        x = "Temperatura",
        y = "Umidade") +
   theme_minimal()
 
 ##
 ##### ANOVA 
 
 tabAnova <- anova(modelo)
 f0 <- tabAnova$`F value`[1]
 f <- qf(0.95, 1, n-2)
 f0 > f
 
 residuos <-  y - modelo$fitted.values #residuals(ajuste)
 
 shapiro.test(residuos)
 
 ks.test(residuos,"pnorm",0, sqrt(sigma2chapeu))
 
 ks.test(residuos,"pnorm",mean(residuos), sd(residuos))
 
 gg_qqplot <- performance::check_model(modelo, check ='qq')
 
gg_hist_residuos <-  ggplot(data = data.frame(x = residuos), aes(x)) +
   geom_histogram(aes(y = ..density..),bins = 15, color = "gray", fill = "#1b6ca8") +
   stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt(sigma2chapeu)),
                 color = "#3aaf85", size = 1) +
   labs(title = "Histograma dos resíduos com Curva Normal",
        x = "Valores",
        y = "Frequência") +
   theme_minimal()
 
 gg_linear <- performance::check_model(modelo, check ='linearity')
 gg_homo <- performance::check_model(modelo, check ='homogeneity')
 
  retorno <- list(gg_dispersao = dispersao,
                  correlacao = cor(x,y),
                 modelo = modelo,
                 b0chap = b0chap,
                 b1chap = b1chap,
                 ICb0 = ICb0,
                 test_b0 = tb0,
                 test_b1 = t_b1,
                 ICb1 = ICb1,
                 gg_dispersao_ic = dispersao_ic,
                 tabela_Anova = tabAnova,
                 sp = shapiro.test(residuos),
                 ks1 = ks.test(residuos,"pnorm",0, sqrt(sigma2chapeu)),
                 ks2 = ks.test(residuos,"pnorm",mean(residuos), sd(residuos)),
                 gg_qqplot = gg_qqplot,
                 gg_histogram_residuo = gg_hist_residuos,
                 gg_dispersao_residuo = gg_linear,
                 gg_homocestacieda = gg_homo,
                 r2 = summary(modelo)$r.squared
                 )
  
  return(retorno)
}


model <- modelo(dt_min = "2018-01-01", dt_max = "2018-12-31", tipo = "media")


model$gg_dispersao
model$correlacao
model$b0chap
model$b1chap
model$ICb0
model$ICb1
model$test_b0
model$test_b0>dt(0.975, 363)
model$test_b1
abs(model$test_b1)>dt(0.975, 363)
model$gg_dispersao_ic
model$tabela_Anova
model$sp
model$ks1
model$ks2
model$gg_qqplot
model$gg_histogram_residuo
model$gg_dispersao_residuo
model$gg_homocestacieda
model$r2
