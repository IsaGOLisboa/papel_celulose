install.packages('forecast', 'dplyr', 'zoo')
library(forecast)
library (dplyr)
library(zoo)

world_production<- read.csv("world_production.csv")
colnames(world_production)

world_production <- world_production %>%
  rename(`Production (million of tons)` = Production..million.of.tons.) %>%  # Renomear a coluna
  select(Date, `Production (million of tons)`)  # Selecionar as colunas necessárias

# Verificando o resultado
head(world_production)



# Converter a coluna Date para o formato de data e definir como série temporal
world_production$Date <- as.numeric(as.character(world_production$Date))

# Criar uma série temporal para dados anuais
ts_data <- ts(world_production$`Production (million of tons)`, start = min(world_production$Date), frequency = 1)

# Calcular a média móvel para identificar a tendência
trend <- rollmean(ts_data, k = 5, fill = NA)

# Calcular os resíduos (observado - tendência)
residuals <- ts_data - trend

# Plotar a série original, tendência e resíduos
par(mfrow = c(3, 1))
plot(ts_data, main = "Série Original", ylab = "Produção (milhões t)")
plot(trend, main = "Tendência (Média Móvel)", ylab = "Produção (milhões t)")
plot(residuals, main = "Resíduos", ylab = "Produção (milhões t)")
