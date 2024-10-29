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


#temperatura X pm2.5

ggplot(mar, 
       aes(x = max_temp, 
           y = max_pm2.5)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Temperatura",
       y = "PM2.5") +
  theme_classic()

#temperatura X co2
ggplot(mar, 
       aes(x = max_temp, 
           y = md_co)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Temperatura",
       y = "CO2") +
  theme_classic()

#temperatura X no2
ggplot(mar, 
       aes(x = max_temp, 
           y = md_no2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Temperatura",
       y = "NO2") +
  theme_classic()

#temperatura X o3
ggplot(mar, 
       aes(x = max_temp, 
           y = md_o3)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Temperatura",
       y = "O3") +
  theme_classic()

#temperatura X so2
ggplot(mar, 
       aes(x = max_temp, 
           y = md_so2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Temperatura",
       y = "SO2") +
  theme_classic()

#temperatura X queimadas
ggplot(mar, 
       aes(x = max_temp, 
           y = focos_queimada)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Temperatura",
       y = "Queimadas") +
  theme_classic()


#umidade X pm2.5
ggplot(mar, 
       aes(x = min_umid, 
           y = max_pm2.5)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Umidade",
       y = "PM2.5") +
  theme_classic()

#umidade X co2
ggplot(mar, 
       aes(x = min_umid, 
           y = md_co)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Umidade",
       y = "CO2") +
  theme_classic()

#umidade X no2
ggplot(mar, 
       aes(x = min_umid, 
           y = md_no2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Umidade",
       y = "NO2") +
  theme_classic()
#umidade X o3
ggplot(mar, 
       aes(x = min_umid, 
           y = md_o3)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Umidade",
       y = "O3") +
  theme_classic()

#umidade X so2
ggplot(mar, 
       aes(x = min_umid, 
           y = md_so2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Umidade",
       y = "SO2") +
  theme_classic()

#umidade X queimadas
ggplot(mar, 
       aes(x = min_umid, 
           y = focos_queimada)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "Umidade",
       y = "Queimadas") +
  theme_classic()

#pm2.5 X co2
ggplot(mar, 
       aes(x = max_pm2.5, 
           y = md_co)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "PM2.5",
       y = "CO2") +
  theme_classic()

#pm2.5 X no2
ggplot(mar, 
       aes(x = max_pm2.5, 
           y = md_no2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "PM2.5",
       y = "NO2") +
  theme_classic()
#pm2.5 X o3
ggplot(mar, 
       aes(x = max_pm2.5, 
           y = md_o3)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "PM2.5",
       y = "O3") +
  theme_classic()

#pm2.5 X so2
ggplot(mar, 
       aes(x = max_pm2.5, 
           y = md_so2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "PM2.5",
       y = "SO2") +
  theme_classic()

#pm2.5 X queimadas
ggplot(mar, 
       aes(x = max_pm2.5, 
           y = focos_queimada)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "PM2.5",
       y = "Queimadas") +
  theme_classic()

#co2 X no2
ggplot(mar, 
       aes(x = md_co, 
           y = md_no2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "CO2",
       y = "NO2") +
  theme_classic()


#co2 X o3
ggplot(mar, 
       aes(x = md_co, 
           y = md_o3)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "CO2",
       y = "O3") +
  theme_classic()

#co2 X so2
ggplot(mar, 
       aes(x = md_co, 
           y = md_so2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "CO2",
       y = "SO2") +
  theme_classic()

#co2 X queimadas
ggplot(mar, 
       aes(x = md_co, 
           y = focos_queimada)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "CO2",
       y = "Queimadas") +
  theme_classic()


#no2 X o3
ggplot(mar, 
       aes(x = md_no2, 
           y = md_o3)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "NO2",
       y = "O3") +
  theme_classic()

#no2 X so2
ggplot(mar, 
       aes(x = md_no2, 
           y = md_so2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "NO2",
       y = "SO2") +
  theme_classic()

#no2 X queimadas
ggplot(mar, 
       aes(x = md_no2, 
           y = focos_queimada)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "NO2",
       y = "Queimadas") +
  theme_classic()

#o3 X so2
ggplot(mar, 
       aes(x = md_o3, 
           y = md_so2)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "O3",
       y = "SO2") +
  theme_classic()

#o3 X queimadas
ggplot(mar, 
       aes(x = md_o3, 
           y = focos_queimada)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "O3",
       y = "Queimadas") +
  theme_classic()


#so2 X queimadas
ggplot(mar, 
       aes(x = md_so2, 
           y = focos_queimada)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(method = "lm",
              color = "tomato",
              se = FALSE) +
  labs(x = "SO2",
       y = "Queimadas") +
  theme_classic()

