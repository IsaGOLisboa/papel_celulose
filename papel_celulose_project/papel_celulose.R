################################Dados Brasil#################################

# Séries e códigos disponíveis ipea
series_ipeadata <- ipeadatar::available_series()
series_ipeadata
# Filtrar séries com o termo "caged"
dplyr::filter(
  series_ipeadata,
  stringr::str_detect(source, stringr::regex("caged", ignore_case = TRUE))
)

############################Séries
#FUNCEX12_XQPAP2N122 - Exportações - celulose, papel e produtos de papel - quantum indice(média 2018=100)
exportacoes<- ipeadatar::ipeadata("FUNCEX12_XQPAP2N12",language=c("en","br"),quiet=FALSE)
exportacoes<- exportacoes%>%
  filter(date >= as.Date("2014-08-01"))%>%
  select(2,3)%>%
  rename(Data = date,
         exportacoes = value)%>%
  mutate(Data = format(Data,"%m/%Y"))


#PIMPFN12_QIIGNN812 - Produção industrial - celulose, papel e produtos de papel - quantum índice(média 2022=100) 
producao<- ipeadatar::ipeadata("PIMPFN12_QIIGNN812",language=c("en","br"),quiet=FALSE)
producao<- producao%>%
  filter(date >= as.Date("2014-08-01"))%>%
  select(2,3)%>%
  rename(Data = date,
         producao = value)%>%
  mutate(Data = format(Data,"%m/%Y"))


#	GAC12_CAPAPEL12 - Consumo aparente - celulose, papel e produtos de papel - quantum índice (média 2012=100) 
consumo_aparente<- ipeadatar::ipeadata("GAC12_CAPAPEL12",language=c("en","br"),quiet=FALSE)
consumo_aparente<- consumo_aparente%>%
  filter(date >= as.Date("2014-08-01"))%>%
  select(2,3)%>%
  rename(Data = date,
         consumo_aparente = value)%>%
  mutate(Data = format(Data,"%m/%Y"))

#	PIMCELUPAPELN - Produção industrial - celulose, papel e produtos de papel  - quantum índice (média 2022=100) 
producao_regional<- ipeadatar::ipeadata("PIMCELUPAPELN",language=c("en","br"),quiet=FALSE)
producao_regional<- producao_regional%>%
  filter(date >= as.Date("2014-08-01"))%>%
  select(2,3,5)%>%
  rename(Data = date,
         producao_regional = value)%>%
  mutate(Data = format(Data,"%m/%Y"))



regioes<- ipeadatar::available_territories(language = c("en", "br"))


# Realizando o merge para adicionar a coluna tname ao próprio producao_regional
producao_regional <- merge(producao_regional, regioes, by = "tcode")



papel_celulose<-exportacoes %>%
  left_join(producao, by = "Data") %>%
  left_join(consumo_aparente, by = "Data") 




###############################Dados do Mundo
dados_FAOSTAT<- read.csv("Forestry_E_All_Data.csv")
unique(dados_FAOSTAT$Item)  

# Filtrando os dados para "Pulp for paper"
dados_pulp_for_paper <- dados_FAOSTAT %>%
  filter(Item == "Pulp for paper")

#limpando o df:
# Selecionando apenas as colunas cujo nome NÃO termina com 'N' ou 'F'
colunas_para_manter <- grep("^(?!.*[NF]$)", names(dados_pulp_for_paper), value = TRUE, perl = TRUE)

# Criando um novo DataFrame com as colunas filtradas
dados_pulp_for_paper <-dados_pulp_for_paper[, colunas_para_manter]


head(dados_pulp_for_paper)

#Removendo caracteres 
colnames(dados_pulp_for_paper)<-gsub("^Y", "", colnames(dados_pulp_for_paper))
head(dados_pulp_for_paper)


#Dados Produção
dados_pulp_for_paper_prod<- dados_pulp_for_paper %>%
  filter (Element == 'Production')
producao_filtrados<-dados_pulp_for_paper_prod[order(-dados_pulp_for_paper_prod$'2022'), ]
head(producao_filtrados)


dados_world <- subset(producao_filtrados, Area == "World")

write.csv(dados_world, "C:\\Users\\User\\Desktop\\papel_celulose\\producao_mundial.csv")

# Transpondo os dados 
dados_world_long <- data.frame(
  Ano = as.numeric(colnames(dados_world)[-1]),
  Valores = as.numeric(dados_world[1, -1])
)

# Criando gráfico de linhas
fig <- plot_ly(dados_world_long, x = ~Ano, y = ~Valores, type = 'scatter', mode = 'lines+markers',
               line = list(color = 'blue')) %>%
  layout(title = 'Produção mundial de papel',
         xaxis = list(title = 'Ano'),
         yaxis = list(title = 'Produção (t)'),
         template = 'plotly_white') 

# Exibindo o gráfico interativo
fig

# Filtrando apenas as áreas desejadas
areas_desejadas <- c("Americas", "Asia", "Europe", "Africa", "Oceania")

# Criando um novo DataFrame com as áreas selecionadas
producao_continentes <- subset(producao_filtrados, Area %in% areas_desejadas)

# Verificando e convertendo as colunas para numérico (exceto a primeira coluna 'Area')
producao_continentes[, 9:ncol(producao_continentes)] <- lapply(producao_continentes[, 9:ncol(producao_continentes)], function(x) as.numeric(as.character(x)))

# Calculando a soma dos continentes a partir da coluna 9
dados_continentes_soma <- colSums(producao_continentes[, 9:ncol(producao_continentes)], na.rm = TRUE)

# Selecionando os valores de 'World' a partir da coluna 9 e converter para numérico
dados_world_valores <- as.numeric(dados_world[1, 9:ncol(dados_world)])

# Verificação se as somas dos continentes são iguais aos valores de "World"
comparacao <- dados_continentes_soma == dados_world_valores

print(comparacao)


##################################Adicionar a coluna world para traçar o gráfico de colunas 
producao_continentes_world = rbind(producao_continentes, dados_world)


#Grafico comparativo entre produções
producao_long <- melt(producao_continentes_world, id.vars = "Area", 
                      measure.vars = colnames(producao_continentes_world)[9:ncol(producao_continentes_world)],
                      variable.name = "Ano", value.name = "Producao")

# Convertendo a coluna 'Ano' para numérico
producao_long$Ano <- as.numeric(as.character(producao_long$Ano))

# Criando o gráfico de barras com ggplot2
p <- ggplot(producao_long, aes(x = Ano, y = Producao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Produção por Continentes e World ao longo dos Anos",
       x = "Ano", y = "Produção") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)

############# análise gráfica para exportação
# Filtrando apenas as áreas desejadas
areas_desejadas <- c("United States of America", "China", "Brazil", "Canada", "Sweden", "Finland", "	
Indonesia", "Russian Federation", "Japan", "India")

# Criando um novo DataFrame com as áreas selecionadas
producao_paises <- subset(producao_filtrados, Area %in% areas_desejadas)

# Verificando e convertendo as colunas para numérico (exceto a primeira coluna 'Area')
producao_paises[, 9:ncol(producao_paises)] <- lapply(producao_paises[, 9:ncol(producao_paises)], function(x) as.numeric(as.character(x)))

producao_paises<- rbind(producao_paises, dados_world)
colnames(producao_paises)

# Selecionando as colunas 
colunas_selecionadas <- c("1961", "1970", "1980", "1990", "2000", "2010", "2020","2021", "2022")

# Derretendo (melt) os dados para o formato longo, com as colunas selecionadas
producao_long <- melt(producao_paises[,  c("Area", colunas_selecionadas)], 
                      id.vars = "Area", 
                      variable.name = "Ano", 
                      value.name = "Producao")

# Convertendo a coluna 'Ano' para numérico, se necessário
producao_long$Ano <- as.numeric(as.character(producao_long$Ano))

# Filtrando os dados para remover a observação 'World'
producao_long_sem_world <- subset(producao_long, Area != "World")

# Reordenando as áreas com base na produção (maior para menor)
producao_long_sem_world <- producao_long_sem_world %>%
  group_by(Ano) %>%
  arrange(desc(Producao), .by_group = TRUE)

# Criando o gráfico de barras 

p <- ggplot(producao_long_sem_world, aes(x = Ano, y = Producao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +  
  labs(title = "Produção por país",
       x = "Ano", y = "Produção (t)") +
  scale_x_continuous(breaks = as.numeric(colunas_selecionadas)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")  

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)



# Dividir o gráfico em várias facetas (um para cada país)
p <- ggplot(producao_long_sem_world, aes(x = Ano, y = Producao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Produção por País ao Longo dos Anos", x = "Ano", y = "Produção (t)") +
  facet_wrap(~ Area, scales = "free_y") +  # Dividir em facetas, um gráfico para cada país
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)




#######################Trabalhando com exportação
#Dados exportação
dados_pulp_for_paper_exp<- dados_pulp_for_paper %>%
  filter (Element == 'Export Quantity')
exp_filtrados<-dados_pulp_for_paper_exp[order(-dados_pulp_for_paper_exp$'2022'), ]
head(exp_filtrados)

dados_world_exp <- subset(exp_filtrados, Area == "World")

# Transpondo os dados 
dados_world_exp_long <- data.frame(
  Ano = as.numeric(colnames(dados_world_exp)[-1]),
  Valores = as.numeric(dados_world_exp[1, -1])
)

# Criar gráfico de linhas
ggplot(dados_world_exp_long, aes(x = Ano, y = Valores)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Exportações totais no mundo",
       x = "Ano", y = "Exportações (t)") +
  theme_minimal()

# Filtrando apenas as áreas desejadas
areas_desejadas <- c("Americas", "Asia", "Europe", "Africa", "Oceania")

# Criando um novo DataFrame com as áreas selecionadas
exp_continentes <- subset(exp_filtrados, Area %in% areas_desejadas)

# Verificando e convertendo as colunas para numérico (exceto a primeira coluna 'Area')
exp_continentes[, 9:ncol(exp_continentes)] <- lapply(exp_continentes[, 9:ncol(exp_continentes)], function(x) as.numeric(as.character(x)))

# Calculando a soma dos continentes a partir da coluna 9
dados_exp_soma <- colSums(exp_continentes[, 9:ncol(exp_continentes)], na.rm = TRUE)

# Selecionando os valores de 'World' a partir da coluna 9 e converter para numérico
dados_world_exp_valores <- as.numeric(dados_world_exp[1, 9:ncol(dados_world_exp)])

# Verificação se as somas dos continentes são iguais aos valores de "World"
comparacao <- dados_exp_soma == dados_world_exp_valores

print(comparacao)


# Verificando valores ausentes ou valores fora da escala
problemas_valores <- subset(dados_world_exp_long, is.na(Valores) | Valores <= 0)

# Exibindo os valores problemáticos
print(problemas_valores)

##################################Adicionar a coluna world para traçar o gráfico de colunas 
exp_continentes_world = rbind(exp_continentes, dados_world_exp)


#Grafico comparativo entre produções
exp_long <- melt(exp_continentes_world, id.vars = "Area", 
                      measure.vars = colnames(exp_continentes_world)[9:ncol(exp_continentes_world)],
                      variable.name = "Ano", value.name = "Exportacao")

# Convertendo a coluna 'Ano' para numérico
exp_long$Ano <- as.numeric(as.character(exp_long$Ano))

# Criando o gráfico de barras com ggplot2
p <- ggplot(exp_long, aes(x = Ano, y = Exportacao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Exportações por Continentes",
       x = "Ano", y = "Exprtações (t)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)

############# análise gráfica para exportação
# Filtrando apenas as áreas desejadas
areas_desejadas <- c("United States of America", "China", "Brazil", "Canada", "Sweden", "Finland", "	
Indonesia", "Russian Federation", "Japan", "India")

# Criando um novo DataFrame com as áreas selecionadas
exp_paises <- subset(exp_filtrados, Area %in% areas_desejadas)

# Verificando e converter as colunas para numérico (exceto a primeira coluna 'Area')
exp_paises[, 9:ncol(exp_paises)] <- lapply(exp_paises[, 9:ncol(exp_paises)], function(x) as.numeric(as.character(x)))

exp_paises<- rbind(exp_paises, dados_world_exp)
colnames(exp_paises)

# Selecionando as colunas 
colunas_selecionadas <- c("1961", "1970", "1980", "1990", "2000", "2010", "2020","2021", "2022")

# Derretendo (melt) os dados para o formato longo, com as colunas selecionadas
exp_long <- melt(exp_paises[,  c("Area", colunas_selecionadas)], 
                      id.vars = "Area", 
                      variable.name = "Ano", 
                      value.name = "Exportacao")

# Convertendo a coluna 'Ano' para numérico, se necessário
exp_long$Ano <- as.numeric(as.character(exp_long$Ano))

# Filtrando os dados para remover a observação 'World'
exp_long_sem_world <- subset(exp_long, Area != "World")

# Reordenando as áreas com base na produção (maior para menor)
exp_long_sem_world <- exp_long_sem_world %>%
  group_by(Ano) %>%
  arrange(desc(Exportacao), .by_group = TRUE)

# Criando o gráfico de barras com barras mais largas e ordenadas

p <- ggplot(exp_long_sem_world, aes(x = Ano, y = Exportacao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +  
  labs(title = "Exportações por país",
       x = "Ano", y = "Exportações (t)") +
  scale_x_continuous(breaks = as.numeric(colunas_selecionadas)) +  # Incluir os anos selecionados no eixo X
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")  # Usando uma paleta com mais cores

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)

# Dividindo o gráfico em várias facetas (um para cada país)
p <- ggplot(exp_long_sem_world, aes(x = Ano, y = Exportacao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Exportações por País", x = "Ano", y = "Exportações (t)") +
  facet_wrap(~ Area, scales = "free_y") +  # Dividir em facetas, um gráfico para cada país
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)


###################Dados importações
#Dados importação
dados_pulp_for_paper_imp<- dados_pulp_for_paper %>%
  filter (Element == 'Import Quantity')
imp_filtrados<-dados_pulp_for_paper_imp[order(-dados_pulp_for_paper_imp$'2022'), ]
head(imp_filtrados)

dados_world_imp <- subset(imp_filtrados, Area == "World")

# Transpondo os dados 
dados_world_imp_long <- data.frame(
  Ano = as.numeric(colnames(dados_world_imp)[-1]),
  Valores = as.numeric(dados_world_imp[1, -1])
)

# Criando gráfico de linhas
ggplot(dados_world_imp_long, aes(x = Ano, y = Valores)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Importações totais no mundo",
       x = "Ano", y = "Importações (t)") +
  theme_minimal()

# Filtrando apenas as áreas desejadas
areas_desejadas <- c("Americas", "Asia", "Europe", "Africa", "Oceania")

# Criando um novo DataFrame com as áreas selecionadas
imp_continentes <- subset(imp_filtrados, Area %in% areas_desejadas)

# Verificando e convertendo as colunas para numérico (exceto a primeira coluna 'Area')
imp_continentes[, 9:ncol(imp_continentes)] <- lapply(imp_continentes[, 9:ncol(imp_continentes)], function(x) as.numeric(as.character(x)))

# Calculando a soma dos continentes a partir da coluna 9
dados_imp_soma <- colSums(imp_continentes[, 9:ncol(imp_continentes)], na.rm = TRUE)

# Selecionando os valores de 'World' a partir da coluna 9 e converter para numérico
dados_world_imp_valores <- as.numeric(dados_world_imp[1, 9:ncol(dados_world_imp)])

# Verificação se as somas dos continentes são iguais aos valores de "World"
comparacao <- dados_imp_soma == dados_world_imp_valores

print(comparacao)


##################################Adicionar a coluna world para traçar o gráfico de colunas 
imp_continentes_world = rbind(imp_continentes, dados_world_imp)


#Grafico comparativo entre produções
imp_long <- melt(imp_continentes_world, id.vars = "Area", 
                 measure.vars = colnames(imp_continentes_world)[9:ncol(imp_continentes_world)],
                 variable.name = "Ano", value.name = "Importacao")

# Convertendo a coluna 'Ano' para numérico
imp_long$Ano <- as.numeric(as.character(imp_long$Ano))


# Criando o gráfico de barras com ggplot2
p <- ggplot(imp_long, aes(x = Ano, y = Importacao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Importações por Continentes",
       x = "Ano", y = "Imprtações (t)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)

############# análise gráfica para importação
# Filtrando apenas as áreas desejadas
areas_desejadas <- c("United States of America", "China", "Brazil", "Canada", "Sweden", "Finland", "	
Indonesia", "Russian Federation", "Japan", "India")

# Criando um novo DataFrame com as áreas selecionadas
imp_paises <- subset(imp_filtrados, Area %in% areas_desejadas)

# Verificando e convertendo as colunas para numérico (exceto a primeira coluna 'Area')
imp_paises[, 9:ncol(imp_paises)] <- lapply(imp_paises[, 9:ncol(imp_paises)], function(x) as.numeric(as.character(x)))

imp_paises<- rbind(imp_paises, dados_world_imp)
colnames(imp_paises)

# Selecionando as colunas 
colunas_selecionadas <- c("1961", "1970", "1980", "1990", "2000", "2010", "2020","2021", "2022")

# Derretendo (melt) os dados para o formato longo, com as colunas selecionadas
imp_long <- melt(imp_paises[,  c("Area", colunas_selecionadas)], 
                 id.vars = "Area", 
                 variable.name = "Ano", 
                 value.name = "Importacao")

# Convertendo a coluna 'Ano' para numérico
imp_long$Ano <- as.numeric(as.character(imp_long$Ano))

# Filtrando os dados para remover a observação 'World'
imp_long_sem_world <- subset(imp_long, Area != "World")

# Reordenando as áreas com base na produção (maior para menor)
imp_long_sem_world <- imp_long_sem_world %>%
  group_by(Ano) %>%
  arrange(desc(Importacao), .by_group = TRUE)

# Criando o gráfico de barras com barras 

p <- ggplot(imp_long_sem_world, aes(x = Ano, y = Importacao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +  
  labs(title = "Importações por país",
       x = "Ano", y = "Importações (t)") +
  scale_x_continuous(breaks = as.numeric(colunas_selecionadas)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")  # Usando uma paleta com mais cores

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)

# Dividindo o gráfico em várias facetas (um para cada país)
p <- ggplot(imp_long_sem_world, aes(x = Ano, y = Importacao, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Importações por País", x = "Ano", y = "Importações") +
  facet_wrap(~ Area, scales = "free_y") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transformando o gráfico em interativo com ggplotly
ggplotly(p)


#####################balança comercial de papel e celulose
# Fazendo o merge dos dois DataFrames com base na coluna 'Area'
dados_combinados <- merge(exp_filtrados, imp_filtrados, by = "Area", suffixes = c("_exp", "_imp"))

# Identificando as colunas a partir da posição 9 (supondo que sejam as mesmas em ambos os DataFrames)
colunas_selecionadas <- colnames(exp_filtrados)[9:ncol(exp_filtrados)]

# Criando o DataFrame balanca_comercial e manter todas as colunas do merge
balanca_comercial <- dados_combinados

# Calculando a diferença para as colunas a partir da posição 9
for (col in colunas_selecionadas) {
  col_exp <- paste0(col, "_exp")
  col_imp <- paste0(col, "_imp")
  
  # Verificando se as colunas existem no DataFrame combinado e se ambas têm o mesmo número de valores
  if (col_exp %in% colnames(dados_combinados) && col_imp %in% colnames(dados_combinados)) {
    # Garantindo que os valores são numéricos e realizar a subtração
    balanca_comercial[[col]] <- as.numeric(dados_combinados[[col_exp]]) - as.numeric(dados_combinados[[col_imp]])
  }
}

head(balanca_comercial)

# Selecionando a coluna 'Area' e as colunas que possuem apenas números (sem sufixos)
balanca_comercial_limpo <- balanca_comercial[, c("Area", grep("^[0-9]+$", colnames(balanca_comercial), value = TRUE))]

# Exibindo as primeiras linhas do DataFrame limpo
head(balanca_comercial_limpo)


# Filtrando os dados para os países de 'areas_desejadas'
dados_filtrados_balanca <- balanca_comercial_limpo[balanca_comercial_limpo$Area %in% areas_desejadas, ]

dados_long <- melt(dados_filtrados_balanca, id.vars = "Area", variable.name = "Ano", value.name = "Balanca")

# Criando o gráfico de linhas interativo usando plotly
fig <- plot_ly(dados_long, x = ~Ano, y = ~Balanca , color = ~Area, type = 'scatter', mode = 'lines')

# Exibindo o gráfico
fig

###########################Tipos de papel
unique(dados_FAOSTAT$Item)
# Definindo os itens desejados em inglês 
itens <- c("Recovered paper", 
                     "Paper and paperboard", 
                     "Graphic papers", 
                     "Newsprint", 
                     "Paper and paperboard, excluding newsprint", 
                     "Printing and writing papers", 
                     "Printing and writing papers, uncoated, mechanical", 
                     "Printing and writing papers, uncoated, wood free", 
                     "Printing and writing papers, coated", 
                     "Other paper and paperboard", 
                     "Household and sanitary papers", 
                     "Packaging paper and paperboard", 
                     "Case materials", 
                     "Cartonboard", 
                     "Wrapping papers", 
                     "Other papers mainly for packaging", 
                     "Other paper and paperboard n.e.s. (not elsewhere specified)")

# Filtrando o DataFrame 'dados_FOASTAT' 
df_papel <- subset(dados_FAOSTAT, Item %in% itens)

head(df_papel)

papel_world<- df_papel%>%
  filter(Area == "World" & Element == "Production")

colunas_para_manter <- grep("^(?!.*[NF]$)", names(papel_world), value = TRUE, perl = TRUE)

# Criando um novo DataFrame com as colunas filtradas
papel_world <-papel_world[, colunas_para_manter]


colnames(papel_world)<-gsub("^Y", "", colnames(papel_world))


# Filtrando os dados para as observações especificadas
itens_desejados <- c("Graphic papers", "Newsprint",
                     "Printing and writing papers", "Other paper and paperboard", 
                     "Household and sanitary papers", "Packaging paper and paperboard", 
                     "Case materials", "Cartonboard", "Wrapping papers", 
                     "Other papers mainly for packaging", "Other paper and paperboard n.e.s.")

df_filtrado <- subset(papel_world, Item %in% itens_desejados)

# Selecionando as colunas a partir da posição 9 (anos de 1961 a 2022)
colunas_anos <- colnames(df_filtrado)[9:ncol(df_filtrado)]

# Transformando o DataFrame para o formato longo (long format)
df_long <- melt(df_filtrado, id.vars = "Item", measure.vars = colunas_anos, 
                variable.name = "Ano", value.name = "Valor")

# Convertendo a coluna 'Ano' para numérico
df_long$Ano <- as.numeric(as.character(df_long$Ano))
df_long<-df_long[df_long$Ano>=2000, ]

# Criando o gráfico de pizza animado
fig <- plot_ly(df_long, labels = ~Item, values = ~Valor, type = 'pie', frame = ~Ano,
               textinfo = 'label+percent', hoverinfo = 'label+value') %>%
  layout(title = 'Distribuição de Papéis e Papelões ao longo dos anos',
         updatemenus = list(
           list(type = "buttons",
                buttons = list(
                  list(method = "animate",
                       args = list(NULL, list(frame = list(duration = 1000, redraw = TRUE), 
                                              transition = list(duration = 500))),
                       label = "Play")
                )
           )
         ))

# Exibindo o gráfico
fig

paper_for_type_22<- subset(df_long,Ano == "2022" )
paper_for_type<-df_long
paper_for_type


##############consumo de papel por paises
dados_pulp_for_paper_prod
dados_pulp_for_paper_exp
dados_pulp_for_paper_imp
# consumo = Produção + importação - exportação

# Selecionando os dados de interesse
dados_pulp_for_paper_prod_limpo <- dados_pulp_for_paper_prod[dados_pulp_for_paper_prod$Element == "Production", -c(1, 2, 4, 5, 6)]
dados_pulp_for_paper_imp_limpo <- dados_pulp_for_paper_imp[dados_pulp_for_paper_imp$Element == "Import Quantity", -c(1, 2, 4, 5, 6)]
dados_pulp_for_paper_exp_limpo <- dados_pulp_for_paper_exp[dados_pulp_for_paper_exp$Element == "Export Quantity", -c(1, 2, 4, 5, 6)]
print(dim(dados_pulp_for_paper_exp_limpo))  # Exportação
print(dim(dados_pulp_for_paper_imp_limpo))  # Importação
print(dim(dados_pulp_for_paper_prod_limpo)) # Produção


# Realizando o merge utilizando a coluna 'Area' como chave, garantindo que todos os dados sejam combinados corretamente
dados_merged <- merge(dados_pulp_for_paper_prod_limpo, dados_pulp_for_paper_imp_limpo, by = "Area", all = TRUE, suffixes = c("_prod", "_imp"))
dados_merged <- merge(dados_merged, dados_pulp_for_paper_exp_limpo, by = "Area", all = TRUE, suffixes = c("", "_exp"))

# Substituindo os valores NA por 0 para as colunas de produção, importação e exportação
anos <- paste0(1961:2022)

for (ano in anos) {
  dados_merged[[paste0(ano, "_prod")]] <- ifelse(is.na(dados_merged[[paste0(ano, "_prod")]]), 0, dados_merged[[paste0(ano, "_prod")]])
  dados_merged[[paste0(ano, "_imp")]] <- ifelse(is.na(dados_merged[[paste0(ano, "_imp")]]), 0, dados_merged[[paste0(ano, "_imp")]])
  dados_merged[[ano]] <- ifelse(is.na(dados_merged[[ano]]), 0, dados_merged[[ano]])  
}

# Criando o DataFrame final 'consumo' apenas com a coluna 'Area' e os resultados calculados para cada ano
consumo <- dados_merged[, "Area", drop = FALSE] 

# Adicionando colunas de cálculo para cada ano
for (ano in anos) {
  consumo[[ano]] <- dados_merged[[paste0(ano, "_prod")]] + dados_merged[[paste0(ano, "_imp")]] - dados_merged[[ano]]
}


write.csv(consumo, "C:\\Users\\User\\Desktop\\papel_celulose\\dfs\\consumo_polpa_celulose_por_pais.csv")


# Filtrando apenas as linhas onde Area é igual a 'China'
consumo_china <- subset(consumo, Area == "China")
prod_china<- subset(dados_pulp_for_paper_prod_limpo, Area =="China")
import_china <- subset(dados_pulp_for_paper_imp_limpo, Area =="China")
export_china <- subset(dados_pulp_for_paper_exp_limpo, Area =="China")


print(consumo_china)
print(prod_china)
print(import_china)
print(export_china)


##Consumo mundial
consumo_world<- subset (consumo, Area == "World")
print(consumo_world)
colnames(consumo_world)

# Transformando o DataFrame consumo_world em longo
consumo_world_long <- consumo_world %>%
    pivot_longer(cols = -Area,               
               names_to = "Ano",           
               values_to = "(t)")  

print(consumo_world_long)

# Removendo a primeira coluna 
consumo_world_long <- consumo_world_long[, -1]
consumo_world_long$Ano<- as.numeric(consumo_world_long$Ano)

head(consumo_world_long)


#####Calculando o consumo per capto

populacao_por_pais<- read.csv("C:\\Users\\User\\Desktop\\papel_celulose\\dfs\\populacao_por_pais.csv")

# Removendo o prefixo "X" dos nomes das colunas de populacao_por_pais
colnames(populacao_por_pais) <- gsub("^X", "", colnames(populacao_por_pais))

# Removendo a primeira coluna de populacao_por_pais
populacao_por_pais <- populacao_por_pais[, -1]

colnames(populacao_por_pais)

# Verificando os tipos de dados das colunas no DataFrame populacao_por_pais
str(populacao_por_pais)
populacao_por_pais[colunas_anos] <- lapply(populacao_por_pais[colunas_anos], function(x) as.numeric(x))

# Verificando se a conversão foi bem-sucedida
str(populacao_por_pais)

str(consumo)

# Substituindo valores 0 por NaN no DataFrame consumo (exceto na coluna 'Area')
consumo[, -1] <- lapply(consumo[, -1], function(x) ifelse(x == 0, NaN, x))

# Corrigindo manualmente nomes de países no DataFrame consumo$Area
consumo$Area[consumo$Area == "United States of America"] <- "United States"
consumo$Area[consumo$Area == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
consumo$Area[consumo$Area == "T\xfcrkiye"] <- "Turkey"
consumo$Area[consumo$Area == "R\xe9union"] <- "Reunion"
consumo$Area[consumo$Area == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
consumo$Area[consumo$Area == "Russian Federation"] <- "Russia"
consumo$Area[consumo$Area == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
consumo$Area[consumo$Area == "Iran (Islamic Republic of)"] <- "Iran"
consumo$Area[consumo$Area == "Bolivia (Plurinational State of)"] <- "Bolivia"
consumo$Area[consumo$Area == "Democratic People's Republic of Korea"] <- "North Korea"
consumo$Area[consumo$Area == "Republic of Korea"] <- "South Korea"
consumo$Area[consumo$Area == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
consumo$Area[consumo$Area == "Congo"] <- "Congo, Rep."
print(unique(consumo$Area))

# Adicionando o sufixo _pop às colunas de anos a partir da segunda coluna em populacao_por_pais
colnames(populacao_por_pais)[-1] <- paste0(colnames(populacao_por_pais)[-1], "_pop")


# Realizando o merge utilizando 'Area' de consumo e 'Pais' de populacao_por_pais como chaves
consumo_populacao <- merge(consumo, populacao_por_pais, by.x = "Area", by.y = "Pais", all.x = TRUE)

print(consumo_populacao)

# Removendo as linhas onde todas as colunas (exceto a coluna 'Area') possuem valores NaN
consumo_populacao <- consumo_populacao[!apply(consumo_populacao[, -1], 1, function(row) all(is.na(row))), ]

print(consumo_populacao)
colnames(consumo_populacao)


# Lista de anos de consumo e suas correspondentes colunas de população
anos <- 1961:2022  
colunas_consumo <- as.character(anos)  
colunas_populacao <- paste0(as.character(anos), "_pop")  

# Calculando o consumo per capita para cada ano
for (ano in colunas_consumo) {
  
  # Verificando se as colunas de consumo e população existem e não estão vazias
  if (!is.null(consumo_populacao[[ano]]) && !is.null(consumo_populacao[[paste0(ano, "_pop")]])) {
    
    # Fazendo a divisão entre consumo e população, verificando se os valores são válidos
    consumo_populacao[[paste0(ano, "_per_capita")]] <- ifelse(
      is.na(consumo_populacao[[ano]]) | is.na(consumo_populacao[[paste0(ano, "_pop")]]) |
        consumo_populacao[[ano]] == 0 | consumo_populacao[[paste0(ano, "_pop")]] == 0,
      NaN, 
      consumo_populacao[[ano]] / consumo_populacao[[paste0(ano, "_pop")]]
    )
  }
}

# Removendo colunas de população e consumo, mantendo apenas 'Area' e o resultado per capita
consumo_per_capita <- consumo_populacao[, c("Area", paste0(anos, "_per_capita"))]

print(consumo_per_capita)

# Ordenando o DataFrame com base no ano 2022 per capita, do maior para o menor
consumo_per_capita <- consumo_per_capita[order(-consumo_per_capita[["2022_per_capita"]]), ]

print(consumo_per_capita)

# Removendo o sufixo '_per_capita' dos nomes das colunas
colnames(consumo_per_capita) <- gsub("_per_capita", "", colnames(consumo_per_capita))


print(consumo_per_capita)

# Filtrando apenas as linhas onde Area é igual a 'China'
consumo_per_capta_china <- subset(consumo_per_capita,Area == "China" )
cons_china<- subset(consumo, Area =="Finland")
pop_china <- subset(populacao_por_pais, Pais =="Finland")


print(consumo_per_capta_china)
print(cons_china)
print(pop_china)
print(unique(consumo$Area))


# Multiplicando as colunas de consumo per capita por 1000, a partir da segunda coluna
consumo_per_capita[, 2:ncol(consumo_per_capita)] <- consumo_per_capita[, 2:ncol(consumo_per_capita)] * 1000


print(consumo_per_capita)
write.csv(consumo_per_capita, "C:\\Users\\User\\Desktop\\papel_celulose\\dfs\\consumo_per_capta.csv")


consumo_per_capta_world<- subset(consumo_per_capita, Area == "World")
consumo_per_capta_world

##### Visualização gráfica

# Inicializando uma lista para armazenar os 10 maiores valores por ano
top10_lista <- list()

# Loop para encontrar os 10 maiores valores de consumo per capita em cada ano
for (ano in anos) {
  # Obtendo o nome da coluna correspondente ao ano
  coluna_ano <- as.character(ano)
  
  # Ordenando os dados pelo valor do ano em ordem decrescente e pegar os 10 maiores
  top10 <- consumo_per_capita[order(-consumo_per_capita[[coluna_ano]]), c("Area", coluna_ano)][1:10, ]
  top10
  
  
  # Adicionando a coluna 'Ano'
  top10$Ano <- ano
  
  # Renomeando a coluna de consumo per capita para um nome comum, como 'Consumo_per_capita'
  colnames(top10)[2] <- "Consumo_per_capita"
  
  # Reordenando os países por consumo per capita 
  top10$Area <- reorder(top10$Area, -top10$Consumo_per_capita)
  
  # Adicionando os dados do ano na lista
  top10_lista[[coluna_ano]] <- top10
}

# Combinando todas as observações em um único data frame
top10_df <- do.call(rbind, top10_lista)

# Removendo observações com consumo per capita igual a 0 ou NA
top10_df <- top10_df %>% filter(!is.na(Consumo_per_capita) & Consumo_per_capita != 0)


print(unique(top10_df$Area))


#traduzido paises para Português
# Criar o mapa de tradução como uma lista
mapa_paises <- c(
  "Finland" = "Finlândia",
  "Sweden" = "Suécia",
  "Canada" = "Canadá",
  "Norway" = "Noruega",
  "United States" = "Estados Unidos",
  "New Zealand" = "Nova Zelândia",
  "Austria" = "Áustria",
  "Switzerland" = "Suíça",
  "Japan" = "Japão",
  "Australia" = "Austrália",
  "Eswatini" = "Essuatíni",
  "France" = "França",
  "Germany" = "Alemanha",
  "Slovenia" = "Eslovênia",
  "Portugal" = "Portugal",
  "Cayman Islands" = "Ilhas Cayman",
  "Czechia" = "Tchéquia"
)

# Aplicando a função case_when para traduzir os valores com base no mapa
top10_df <- top10_df %>%
  mutate(Area = case_when(
    Area %in% names(mapa_paises) ~ mapa_paises[Area],
    TRUE ~ Area
  ))


print(unique(top10_df$Area))

######################Grafico interativo
# Ordenando os dados de forma decrescente
top10_df <- top10_df[order(-top10_df$Consumo_per_capita),]

# Criando o gráfico 
p <- plot_ly(top10_df, 
             x = ~Area, 
             y = ~Consumo_per_capita, 
             frame = ~Ano,  
             color = ~Area, 
             type = "bar", 
             text = ~round(Consumo_per_capita, 1),  
             textposition = "outside",  
             hoverinfo = "text",  
             name = "Consumo per Capita")  

# Ajustando layout 
p <- p %>% layout(
  title = "Evolução do Consumo per Capita de Polpa para Pepel",
  xaxis = list(title = "País", categoryorder = "total descending"),  
  yaxis = list(title = "Consumo per Capita (kg/pessoa)", side = "left"),  
  yaxis2 = list(title = "Produção Mundial (milhões t)", overlaying = "y", side = "right"),  
  showlegend = TRUE,  
  bargap = 0.1  
)

# Exibindo o gráfico interativo
p


################################grafico cosumo per capta com dados de world
consumo_per_capta_world

# Transformando o DataFrame para o formato longo
consumo_per_capta_world_long <- consumo_per_capta_world %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),  
               names_to = "Ano",  
               values_to = "World")  

consumo_per_capta_world_long<- consumo_per_capta_world_long[, -1]

head(consumo_per_capta_world_long)

# Convertendo 'Ano' para numérico 
consumo_per_capta_world_long$Ano <- as.numeric(as.character(consumo_per_capta_world_long$Ano))

# Criando o gráfico de linhas interativo com animação
p <- plot_ly() 

# Adicionando os dados ano a ano acumulando a linha
for(i in seq_len(nrow(consumo_per_capta_world_long))) {
  p <- p %>%
    add_trace(x = consumo_per_capta_world_long$Ano[1:i], 
              y = consumo_per_capta_world_long$World[1:i], 
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = 'blue'),  
              marker = list(color = 'blue'),  
              name = paste("Ano", consumo_per_capta_world_long$Ano[i]),  
              frame = consumo_per_capta_world_long$Ano[i],  
              text = paste("Ano:", consumo_per_capta_world_long$Ano[1:i], "<br>Produção:", consumo_world_long$World[1:i]),
              hoverinfo = "text")
}

# Ajustando layout do gráfico com animação
p <- p %>% layout(title = "Evolução do Consumo per Capta Mundial ao Longo dos Anos",
                  xaxis = list(title = "Ano"),
                  yaxis = list(title = "Consumo per capta mundial (kg/pessoa)"),
                  showlegend = FALSE,  # Não exibir a legenda
                  updatemenus = list(
                    list(type = "buttons", 
                         buttons = list(
                           list(label = "Play",
                                method = "animate",
                                args = list(NULL, list(frame = list(duration = 500, redraw = TRUE), 
                                                       fromcurrent = TRUE, mode = "immediate"))),
                           list(label = "Pause",
                                method = "animate",
                                args = list(NULL, list(frame = list(duration = 0, redraw = FALSE), mode = "immediate")))
                         ))))

# Exibindo o gráfico interativo com animação
p


# Salvando o gráfico em um arquivo HTML
htmlwidgets::saveWidget(p, "C:/Users/User/Desktop/papel_celulose/graficos/interactive_graph_consumption_country.html")
getwd()


#####Gráfico produção mundial
world_production <- read.csv("C:\\Users\\User\\Desktop\\papel_celulose\\producao_mundial.csv")
world_production<- world_production[, -c(1,2,3,5,6,7,8,9)]
colnames(world_production)<- gsub("^X", "", colnames(world_production))
colnames(world_production)
world_production <- as.data.frame(t(world_production))
world_production
colnames(world_production)

colnames(world_production)[1]<- "Production (million of tons)"
world_production$Date <- rownames(world_production)
#removendo o index
rownames(world_production) <- NULL

# Reordenando as colunas 
world_production <- world_production[, c("Date", colnames(world_production)[-ncol(world_production)])]
world_production<- world_production[-1, ]
world_production[, 2] <- as.numeric(world_production[, 2])/1000000

world_production
summary(world_production)



#############################Comparativo de China, maior impoprtador e Brasil, maior exportador
importacao_china<- import_china[, -c(2,3)]
importacao_china<-as.data.frame(t(importacao_china))
colnames(importacao_china)[1] <- "Importações (milhões t)"
importacao_china$Date <- rownames(importacao_china)
rownames (importacao_china)<- NULL
importacao_china

# Reordenando as colunas 
importacao_china <-importacao_china[, c("Date", colnames(importacao_china)[-ncol(importacao_china)])]
importacao_china<- importacao_china[-1, ]
importacao_china[, 2] <- as.numeric(importacao_china[, 2])/1000000

importacao_china
summary(importacao_china)

#filtrando dados de exportação do Brasil
dados_pulp_for_paper_exp_limpo
world_exp<-subset(dados_pulp_for_paper_exp_limpo, Area == "World")
world_exp
brasil_exp<- subset(dados_pulp_for_paper_exp_limpo, Area == "Brazil")
brasil_exp


world_exp<- world_exp[, -c(2,3)]
world_exp<- as.data.frame(t(world_exp))

world_exp$Date <- rownames(world_exp)

#removendo o index
rownames(world_exp) <- NULL

# Reordenando as colunas 
world_exp <- world_exp[, c("Date", colnames(world_exp)[-ncol(world_exp)])]
world_exp[, 2] <- as.numeric(world_exp[, 2])/1000000
colnames(world_exp)[2]<- "Export (millions of tons)"
world_exp <- world_exp[-1, ]
world_exp


brasil_exp <- brasil_exp[, -c(2,3)]
brasil_exp <- as.data.frame(t(brasil_exp))

brasil_exp$Date <- rownames(brasil_exp)
# Removendo o index
rownames(brasil_exp) <- NULL

# Reordenando as colunas
brasil_exp <- brasil_exp[, c("Date", colnames(brasil_exp)[-ncol(brasil_exp)])]
brasil_exp[, 2] <- as.numeric(brasil_exp[, 2])/1000000
colnames(brasil_exp)[2] <- "Export (millions of tons)"
brasil_exp <- brasil_exp[-1, ]

# Exibindo o resultado
brasil_exp

#percentual de exportação do Brasil em 2022
19.150764/ 64.07008*100

# Ordenando o DataFrame pelo valor da coluna 2022, do maior para o menor
dados_pulp_for_paper_exp_limpo <- dados_pulp_for_paper_exp_limpo[order(-dados_pulp_for_paper_exp_limpo$`2022`), ]
dados_pulp_for_paper_exp_limpo
#percentual do Canadá
7.821620/64.07008*100
usa_exp<- subset(dados_pulp_for_paper_exp_limpo, Area == "United States of America")
usa_exp
#percentual USA
7.222960/64.07008*100
#percentual Indonésia
4.455051/64.07008*100
#percentual Finlandia
3.957278/64.07008*100

str(dados_pulp_for_paper_exp_limpo$`2022`)

# Corrigindo a codificação da coluna 'Area'
dados_pulp_for_paper_exp_limpo$Area <- iconv(dados_pulp_for_paper_exp_limpo$Area, from = "latin1", to = "UTF-8", sub = "")

# Utilizando a função trimws() apos correção
dados_pulp_for_paper_exp_limpo$Area <- trimws(dados_pulp_for_paper_exp_limpo$Area)
dados_pulp_for_paper_exp_limpo$Area <- trimws(dados_pulp_for_paper_exp_limpo$Area)
dados_pulp_for_paper_exp_limpo$Area <- iconv(dados_pulp_for_paper_exp_limpo$Area, from = "latin1", to = "UTF-8")
dados_pulp_for_paper_exp_limpo <- dados_pulp_for_paper_exp_limpo[order(-dados_pulp_for_paper_exp_limpo$`2022`), ]
dados_pulp_for_paper_exp_limpo


#####Gráfico de linhas
# Renomeando colunas 
colnames(importacao_china)[2] <- "China_Import (million of tons)"
colnames(world_production)[2] <- "World_Production (million of tons)"
colnames(brasil_exp)[2] <- "Brazil_Export (million of tons)"

# Fazendo o merge dos três data frames usando a coluna 'Date'
combined_data <- merge(importacao_china[, c("Date", "China_Import (million of tons)")], 
                       world_production[, c("Date", "World_Production (million of tons)")], 
                       by = "Date", all = TRUE)

combined_data <- merge(combined_data, brasil_exp[, c("Date", "Brazil_Export (million of tons)")], 
  
                                            by = "Date", all = TRUE)


# Garantindo que a coluna 'Date' está numérica
combined_data$Date <- as.numeric(combined_data$Date)
combined_data <- combined_data[-nrow(combined_data), ]

head(combined_data)
colnames((combined_data))

#Traduzindo para o Português
# Corrigir possíveis problemas de codificação nas colunas
combined_data$`China_Import (million of tons)` <- iconv(combined_data$`China_Import (million of tons)`, from = "UTF-8", to = "UTF-8", sub = "")
combined_data$`World_Production (million of tons)` <- iconv(combined_data$`World_Production (million of tons)`, from = "UTF-8", to = "UTF-8", sub = "")
combined_data$`Brazil_Export (million of tons)` <- iconv(combined_data$`Brazil_Export (million of tons)`, from = "UTF-8", to = "UTF-8", sub = "")

# Renomeando as colunas para português (exceto a coluna "Date")
colnames(combined_data) <- c("Date", 
                             "Importação da China (milhões de toneladas)", 
                             "Produção Mundial (milhões de toneladas)", 
                             "Exportação do Brasil (milhões de toneladas)")



print(colnames(combined_data))

# Verificando se as colunas estão sendo interpretadas corretamente como numéricas
combined_data$`Importação da China (milhões de toneladas)` <- as.numeric(combined_data$`Importação da China (milhões de toneladas)`)
combined_data$`Exportação do Brasil (milhões de toneladas)` <- as.numeric(combined_data$`Exportação do Brasil (milhões de toneladas)`)

# Verificando se há valores estranhos ou ausentes
summary(combined_data)
combined_data

# Criar o gráfico 
p <- ggplot(combined_data, aes(x = Date)) + 
  geom_line(aes(y = `Importação da China (milhões de toneladas)`, color = "Importação da China (milhões de toneladas)"), size = 1) +
  geom_line(aes(y = `Exportação do Brasil (milhões de toneladas)`, color = "Exportação do Brasil (milhões de toneladas)"), size = 1) +
  labs(title = "Maior Importador (China) e Maior Exportador (Brasil) de polpa de papel em 2022",
       x = "Ano", y = "Milhões t") +
  scale_x_continuous(breaks = c(1961, 1970, 1980, 1990, 2000, 2010, 2020, 2022)) + 
  scale_color_manual(values = c("Importação da China (milhões de toneladas)" = "purple",  "Exportação do Brasil (milhões de toneladas)" = "blue")) +
  theme_minimal()

# Exibindo o gráfico
p



###Grafico de linhas para world_production
world_production
world_production<- world_production[-1, ]
world_prod_grafic <- world_production
world_prod_grafic$Date <- as.numeric(as.character(world_prod_grafic$Date))

world_prod_grafic
colnames(world_prod_grafic)
write.csv(world_prod_grafic, "C:\\Users\\User\\Desktop\\papel_celulose\\papel_celulose_project\\world_production.csv")

# Gerando o gráfico
p_world_production <- ggplot(world_prod_grafic, aes(x = Date, y = `World_Production (million of tons)`)) + 
  geom_line(color = "black", size = 1) +
  labs(title = "Produção mundial de polpa para papel",
       x = "Ano", y = "Produção (milhões t)") +
  scale_x_continuous(breaks = c(1961, 1970, 1980, 1990, 2000, 2010, 2020, 2022)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exibindo o gráfico
p_world_production

str(world_prod_grafic$Date)

#####Gráfico de pizza por tipo em 2022
paper_for_type_22

# Convertendo os dados em formato apropriado para ggplot
paper_for_type_22_pizza <- paper_for_type_22[, c(1, 3)]  

# Renomeando as colunas para algo mais significativo (opcional)
colnames(paper_for_type_22_pizza) <- c("Type", "Value")

# Calculando os percentuais
paper_for_type_22_pizza$Percent <- paper_for_type_22_pizza$Value / sum(paper_for_type_22_pizza$Value) * 100

paper_for_type_22_pizza

# Garantindo que a coluna Type não tenha espaços ou problemas de codificação
paper_for_type_22_pizza$Type <- trimws(paper_for_type_22_pizza$Type)
paper_for_type_22_pizza$Type <- iconv(paper_for_type_22_pizza$Type, from = "UTF-8", to = "UTF-8", sub = '')

# Traduzindo para português com case_when
paper_for_type_22_pizza <- paper_for_type_22_pizza %>%
  mutate(Type = case_when(
    Type == "Graphic papers" ~ "Papéis gráficos",
    Type == "Newsprint" ~ "Papel jornal",
    Type == "Printing and writing papers" ~ "Papéis para impressão e escrita",
    Type == "Other paper and paperboard" ~ "Outros papéis e papelão",
    Type == "Household and sanitary papers" ~ "Papéis domésticos e sanitários",
    Type == "Packaging paper and paperboard" ~ "Papéis e papelões para embalagem",
    Type == "Case materials" ~ "Materiais de caixa",
    Type == "Cartonboard" ~ "Cartonagem",
    Type == "Wrapping papers" ~ "Papéis para embrulho",
    Type == "Other papers mainly for packaging" ~ "Outros papéis principalmente para embalagem",
    Type == "Other paper and paperboard n.e.s." ~ "Outros papéis e papelões n.e.s.",
    TRUE ~ Type  # Mantém os valores que não precisam ser traduzidos
  ))

# Verificando o resultado
print(unique(paper_for_type_22_pizza$Type))

#Criando o gráfico interativo
fig <- plot_ly(paper_for_type_22_pizza, labels = ~Type, values = ~Value, type = 'pie', 
               textinfo = 'label+percent', hoverinfo = 'label+value',
               marker = list(line = list(color = '#FFFFFF', width = 2))) %>%
  layout(title = "Produção mundial por tipo de papel (2022)",
         showlegend = TRUE)

# Exibindo
fig

print(unique(dados_FAOSTAT$Item))
##################Paper for type
print(unique(paper_for_type$Item))
paper_for_type
paper_for_type_grafic<- paper_for_type%>%
  mutate(Item = case_when(
    Item == "Graphic papers" ~ "Papéis gráficos",
    Item == "Newsprint" ~ "Papel jornal",
    Item == "Printing and writing papers" ~ "Papéis para impressão e escrita",
    Item == "Other paper and paperboard" ~ "Outros papéis e papelão",
    Item == "Household and sanitary papers" ~ "Papéis domésticos e sanitários",
    Item == "Packaging paper and paperboard" ~ "Papéis e papelões para embalagem",
    Item == "Case materials" ~ "Materiais de caixa",
    Item == "Cartonboard" ~ "Cartonagem",
    Item == "Wrapping papers" ~ "Papéis para embrulho",
    Item == "Other papers mainly for packaging" ~ "Outros papéis principalmente para embalagem",
    Item == "Other paper and paperboard n.e.s." ~ "Outros papéis e papelões n.e.s.",
    TRUE ~ Item  
  ))
print(unique(paper_for_type_grafic$Item))

colnames(paper_for_type_grafic)
#Grafico por tipo de papel ao longo dos anos

# Criando o gráfico com todos os tipos de papel em um único gráfico
p <- plot_ly()

# Loop para adicionar cada tipo de papel no gráfico
for (tipo in unique(paper_for_type_grafic$Item)) {
  
  # Filtrando os dados para o tipo de papel atual
  df_tipo <- paper_for_type_grafic %>% filter(Item == tipo)
  
  # Adicionando o tipo de papel ao gráfico
  p <- p %>%
    add_trace(x = df_tipo$Ano, 
              y = df_tipo$Valor, 
              type = 'scatter', 
              mode = 'lines+markers',
              name = tipo)
}

# Ajustando o layout do gráfico
p <- p %>%
  layout(
    title = "Evolução de diferentes tipos de papel ao longo dos anos",
    xaxis = list(title = "Ano"),
    yaxis = list(title = "Valor (produção ou consumo)"),
    showlegend = TRUE
  )

# Exibindo o gráfico
p



#Gráfico de barras


# Paleta de cores
cores <- c("#76C7A0", "#4D4D4D", "#9FC69F", "#B0B0B0", "#5E9C76", "#7F7F7F", "#A3D3A3", "#595959", "#8CBE8C", "#333333")

# Criando o gráfico com todos os tipos de papel em barras
p <- plot_ly()

# Loop 
for (i in seq_along(unique(paper_for_type_grafic$Item))) {
  
  tipo <- unique(paper_for_type_grafic$Item)[i]
  df_tipo <- paper_for_type_grafic %>% filter(Item == tipo)
  
  p <- p %>%
    add_trace(x = df_tipo$Ano, 
              y = df_tipo$Valor, 
              type = 'bar', 
              name = tipo,
              marker = list(color = cores[i]))  # Definindo a cor das barras
}

# layout do gráfico
p <- p %>%
  layout(
    title = "Evolução de diferentes tipos de papel ao longo dos anos",
    xaxis = list(title = "Ano"),
    yaxis = list(title = "Valor (produção ou consumo)"),
    barmode = 'group',  # Para barras agrupadas (pode usar 'stack' para barras empilhadas)
    showlegend = TRUE,
    plot_bgcolor = "#f0f0f0",  # Cor de fundo do gráfico
    paper_bgcolor = "#f0f0f0"  # Cor de fundo da área do gráfico
  )


p

###Gráfico de pizza
# Agrupando os dados por tipo de papel 
dados_pizza <- paper_for_type_grafic %>%
  group_by(Item) %>%
  summarise(total_valor = sum(Valor, na.rm = TRUE))

# Criando o gráfico de pizza
p <- plot_ly(dados_pizza, labels = ~Item, values = ~total_valor, type = 'pie',
             textinfo = 'label+percent', insidetextorientation = 'radial',
             marker = list(colors = c("#76C7A0", "#4D4D4D", "#9FC69F", "#B0B0B0", "#5E9C76", "#7F7F7F", "#A3D3A3", "#595959", "#8CBE8C", "#333333")),
             hoverinfo = 'label+percent+value')

# Ajustando o layout
p <- p %>%
  layout(
    title = "Distribuição dos Tipos de Papel",
    showlegend = TRUE
  )

p

print(unique(paper_for_type_grafic$Item))
paper_for_type_grafic$Ano
# Agrupar em 3 categorias
paper_for_type_grafic <- paper_for_type_grafic %>%
  mutate(Grupo = case_when(
    Item %in% c("Papéis gráficos", "Papel jornal", "Papéis para impressão e escrita") ~ "Papéis para Impressão, Jornais e Revistas",
    Item %in% c("Materiais de caixa", "Cartonagem", "Papéis para embrulho", "Papéis e papelões para embalagem", "Outros papéis principalmente para embalagem") ~ "Papéis e Papelões para Embalagem",
    Item %in% c("Papéis domésticos e sanitários") ~ "Papéis de Uso Doméstico",
    Item %in% c("Outros papéis e papelão") ~"Outros papéis e papelão",
    TRUE ~ "Outros"  
  ))

# Visualizando o resultado
head(paper_for_type_grafic)

# Agregando os valores por Ano e Grupo
dados_agrupados <- paper_for_type_grafic %>%
  group_by(Ano, Grupo) %>%
  summarise(Valor = sum(Valor, na.rm = TRUE))  

# Verificando se há categorias não esperadas
print(unique(paper_for_type_grafic$Grupo))

# Verificando quais itens estão na categoria "Outros"
paper_for_type_grafic %>% filter(Grupo == "Outros") %>% select(Item) %>% unique()

# Criando o gráfico com 3 curvas, uma para cada categoria
ggplot(dados_agrupados, aes(x = Ano, y = Valor, color = Grupo)) +
  geom_line(size = 1.2) +  
  geom_point(size = 2) +   
  labs(title = "Produção de Papéis por Categoria ao Longo dos Anos",
       x = "Ano",
       y = "Valor (em unidades)") +
  scale_color_manual(values = c("Papéis para Impressão, Jornais e Revistas" = "darkgreen",
                                "Papéis e Papelões para Embalagem" = "#4D4D4D",
                                "Papéis de Uso Doméstico" = "#76C7A0",
                                "Outros papéis e papelão" = "lightgray")) +  
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5),  
        legend.title = element_blank(),  
        legend.position = "right")  

# Definindo as cores
color_scale <- c("Papéis para Impressão, Jornais e Revistas" = "darkgreen",
                 "Papéis e Papelões para Embalagem" = "#4D4D4D",
                 "Papéis de Uso Doméstico" = "#76C7A0",
                 "Outros papéis e papelão" = "lightgray")
############################
# Criando gráfico animado
p <- plot_ly()
x_range<-c(2000, 2022)
# Definindo a paleta de cores
color_scale <- c("Papéis para Impressão, Jornais e Revistas" = "darkgreen",
                 "Papéis e Papelões para Embalagem" = "#4D4D4D",
                 "Papéis de Uso Doméstico" = "#76C7A0",
                 "Outros papéis e papelão" = "lightgray")

# Adicionando as curvas para cada grupo
categorias <- unique(dados_agrupados$Grupo)

for (grupo in categorias) {
  grupo_dados <- dados_agrupados %>% filter(Grupo == grupo)
  
  # Adicionando as linhas e pontos ano a ano
  for (i in seq_len(nrow(grupo_dados))) {
    p <- p %>%
      add_trace(x = grupo_dados$Ano[1:i], 
                y = grupo_dados$Valor[1:i], 
                type = "scatter", 
                mode = "lines+markers",
                line = list(width = 2, color = color_scale[[grupo]]),  
                marker = list(size = 6, color = color_scale[[grupo]]), 
                name = grupo,
                frame = as.factor(grupo_dados$Ano[i]),  
                text = paste("Ano:", grupo_dados$Ano[1:i], "<br>Valor:", grupo_dados$Valor[1:i]),
                hoverinfo = "text")
  }
}

# Ajustando o layout do gráfico
p <- p %>% layout(
  title = "Produção de Papéis por Categoria ao Longo dos Anos",
  xaxis = list(title = "Ano"),
  yaxis = list(title = "Valor (em unidades)"),
  showlegend = TRUE,  
  legend =list(orientation = "v",  
               x = 1.02, y = 1,    
               xanchor = "left",  
               yanchor = "top"),
  plot_bgcolor = "#f0f0f0",  
  paper_bgcolor = "#f0f0f0",  
  updatemenus = list(
    list(type = "buttons", 
         buttons = list(
           list(label = "Play",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 500, redraw = TRUE), 
                                       fromcurrent = TRUE, mode = "immediate"))),
           list(label = "Pause",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 0, redraw = FALSE), mode = "immediate")))
         ))))

# Exibindo o gráfico
p


dados_agrupados$Ano

####################################
# Definindo o intervalo de valores no eixo x e y
x_range <- c(min(dados_agrupados$Ano), max(dados_agrupados$Ano)) 
y_range <- c(0, max(dados_agrupados$Valor, na.rm = TRUE))  # Ajustando dinamicamente para o valor máximo

# Criando o diretório onde os arquivos PNG serão salvos
dir.create("papel_por_tipo", showWarnings = FALSE)

# Loop para salvar cada frame como PNG
for (i in seq_len(nrow(dados_agrupados))) {
  p_frame <- plot_ly()  # Resetando o gráfico para cada frame
  
  # Filtrando os dados até o ano atual do loop
  dados_ate_ano <- dados_agrupados %>% filter(Ano <= dados_agrupados$Ano[i])  # Acumulando os anos
  
  # Adicionando as curvas para cada grupo
  for (grupo in categorias) {
    grupo_dados <- dados_ate_ano %>% filter(Grupo == grupo)
    
    if (nrow(grupo_dados) > 0) {  # Garantindo que há dados no grupo
      p_frame <- p_frame %>%
        add_trace(x = grupo_dados$Ano, 
                  y = grupo_dados$Valor, 
                  type = "scatter", 
                  mode = "lines+markers",
                  line = list(width = 2, color = color_scale[[grupo]]),  
                  marker = list(size = 6, color = color_scale[[grupo]]), 
                  name = grupo,
                  text = paste("Ano:", grupo_dados$Ano, "<br>Valor:", grupo_dados$Valor),
                  hoverinfo = "text")
    }
  }
  
  # Layout do gráfico
  p_frame <- p_frame %>%
    layout(
      title = paste("Produção de Papéis por Categoria ao Longo dos Anos:", dados_agrupados$Ano[i]),
      xaxis = list(title = "Ano", range = x_range),
      yaxis = list(title = "Valor (em unidades)", range = y_range),
      showlegend = TRUE,
      legend = list(orientation = "v",  
                    x = 1.02, y = 1,    
                    xanchor = "left",  
                    yanchor = "top"),
      plot_bgcolor = "#f0f0f0"
    )
  
  # Nome do arquivo PNG com zeros à esquerda
  png_filename <- sprintf("papel_por_tipo/frame_%03d.png", i)
  
  # Salvando o gráfico como PNG usando Kaleido
  save_image(p_frame, file = png_filename, engine = "kaleido")
}

# Criando o GIF a partir dos frames
frames <- list.files("papel_por_tipo", pattern = "*.png", full.names = TRUE)
frames <- sort(frames)

# Carregando os frames como imagens
imgs <- image_read(frames)

# Adicionando repetição do último frame para "pausar" no final
last_frame <- imgs[length(imgs)]
extra_frames <- rep(last_frame, 30)  
imgs_with_pause <- c(imgs, extra_frames)

# Criando o GIF com animação lenta e pausando no final
animation <- image_animate(imgs_with_pause, fps = 2)

# Salvando o GIF
image_write(animation, "papel_por_tipo.gif")

cat("GIF criado com sucesso e salvo como 'papel_por_tipo.gif'.\n")


####Gráfico de pizza animado
# Definindo cores para os grupos
color_scale <- c("Papéis para Impressão, Jornais e Revistas" = "darkgreen",
                 "Papéis e Papelões para Embalagem" = "#4D4D4D",
                 "Papéis de Uso Doméstico" = "#76C7A0",
                 "Outros papéis e papelão" = "lightgray")

# Diretório onde os arquivos PNG serão salvos
dir.create("papel_por_tipo_pizza", showWarnings = FALSE)

# Loop para salvar cada frame como PNG
for (i in seq_len(nrow(dados_agrupados))) {
  p_frame <- plot_ly()
  
  # Filtrar dados do ano corrente
  dados_ano_atual <- dados_agrupados %>% filter(Ano == dados_agrupados$Ano[i])
  
  # Criando o gráfico de pizza para o ano corrente
  p_frame <- p_frame %>%
    add_pie(labels = dados_ano_atual$Grupo, 
            values = dados_ano_atual$Valor, 
            textinfo = 'label+percent', 
            insidetextorientation = 'radial',
            hoverinfo = 'label+percent+value', 
            marker = list(colors = unname(color_scale[dados_ano_atual$Grupo]))) %>%
    layout(
      title = paste("Distribuição da Produção de Papéis por Categoria - Ano:", dados_agrupados$Ano[i]),
      showlegend = TRUE,
      plot_bgcolor = "#f0f0f0"  
      )
  
  # Nome do arquivo PNG com zeros à esquerda
  png_filename <- sprintf("papel_por_tipo_pizza/frame_%03d.png", i)
  
  # Salvando o gráfico como PNG usando Kaleido
  save_image(p_frame, file = png_filename, engine = "kaleido")
}

# Criando o GIF a partir dos frames
frames <- list.files("papel_por_tipo_pizza", pattern = "*.png", full.names = TRUE)
frames <- sort(frames)

# Carregando os frames como imagens
imgs <- image_read(frames)

# Adicionando repetição do último frame para "pausar" no final
last_frame <- imgs[length(imgs)]
extra_frames <- rep(last_frame, 30)  
imgs_with_pause <- c(imgs, extra_frames)

# Criando o GIF com animação lenta e pausando no final
animation <- image_animate(imgs_with_pause, fps = 2)

# Salvando o GIF
image_write(animation, "papel_por_tipo_pizza.gif")

cat("GIF criado com sucesso e salvo como 'papel_por_tipo_pizza.gif'.\n")

####Gráfico consumo_world
colnames(consumo_world_long)
consumo_world_long_grafic<- consumo_world_long
consumo_world_long_grafic$`(t)`<-consumo_world_long_grafic$`(t)`/1000000
p <- ggplot(consumo_world_long_grafic, aes(x = Ano, y = `(t)`)) + 
  geom_line(size = 1, color = "blue") +  # Definir cor da linha
  labs(title = "Evolução do consumo de polpa para papel no mundo ao longo dos anos",
       x = "Ano", y = "Consumo (milhões t)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transformando o gráfico em interativo com plotly
p_interativo <- ggplotly(p)

# Exibindo o gráfico interativo
p_interativo


#####Gráfico consumo per capta versus produção mundial
pop_world<-subset(consumo_populacao, Area == "World")
pop_world <- pop_world[, grep("_pop$", colnames(pop_world))]
colnames(pop_world) <- gsub("_pop$", "", colnames(pop_world))
colnames(pop_world)
pop_world <- t(pop_world)
pop_world<- as.data.frame(pop_world)
pop_world$Ano <- rownames(pop_world)
pop_world <- pop_world[, c("Ano", "247")]
colnames(pop_world)[2] <- "pop_world"
rownames(pop_world) <- NULL
pop_world$pop_world<-pop_world$pop_world/(10**6)
pop_world$Ano<- as.numeric(pop_world$Ano)

consumo_per_capta_world_long
world_prod_grafic
print(subset(dados_pulp_for_paper_prod_limpo, Area == "World"))
new_row <- data.frame(Date = 1961, `World_Production..million.of.tons.` = 61.88)
colnames(new_row) <- colnames(world_prod_grafic)
world_prod_grafic <- rbind(world_prod_grafic, new_row)
world_prod_grafic<-world_prod_grafic[order(world_prod_grafic$Date), ]
world_prod_grafic

# Combinando os dois DataFrames em um único DataFrame com o mesmo frame (Ano)
combined_data <- merge(consumo_per_capta_world_long, world_prod_grafic, by.x = "Ano", by.y = "Date")

# Combinando os dois DataFrames em um único DataFrame com o mesmo frame (Ano)
combined_data <- merge(consumo_per_capta_world_long, world_prod_grafic, by.x = "Ano", by.y = "Date")



# Convertendo 'Ano' para numérico 
consumo_per_capta_world_long$Ano <- as.numeric(as.character(consumo_per_capta_world_long$Ano))

# Criando o gráfico de linhas interativo com animação
p <- plot_ly() 


# Adicionando os dados ano a ano acumulando a linha para Consumo per Capta Mundial
for(i in seq_len(nrow(consumo_per_capta_world_long))) {
  p <- p %>%
    add_trace(x = consumo_per_capta_world_long$Ano[1:i], 
              y = consumo_per_capta_world_long$World[1:i], 
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#4D4D4D"),  
              marker = list(color = "#4D4D4D"),  
              name = "Consumo per Capta Mundial",  
              frame = consumo_per_capta_world_long$Ano[i],  
              text = paste("Ano:", consumo_per_capta_world_long$Ano[1:i], "<br>Consumo:", consumo_per_capta_world_long$World[1:i]),
              hoverinfo = "text")
}
#Adicionand os dados ano a ano acumulando a linha para Produção Mundial de Polpa (eixo y secundário)
for(i in seq_len(nrow(world_prod_grafic))) {
  p <- p %>%
    add_trace(x = world_prod_grafic$Date[1:i], 
              y = world_prod_grafic$`World_Production (million of tons)`[1:i], 
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#76C7A0"),  
              marker = list(color = "#76C7A0"),  
              name = "Produção Mundial de Polpa",  
              frame = world_prod_grafic$Date[i],  
              text = paste("Ano:", world_prod_grafic$Date[1:i], "<br>Produção:", world_prod_grafic$`World_Production (million of tons)`[1:i]),
              hoverinfo = "text",
              yaxis = "y2")  
}

# Ajustando layout do gráfico com animação
p <- p %>% layout(title = "Evolução do Consumo per Capta e Produção Mundial ao Longo dos Anos",
                  xaxis = list(title = "Ano"),
                  yaxis = list(title = "Consumo per capta mundial (kg/pessoa)", range = c(0, 100)),
                  yaxis2 = list(title = "Produção Mundial de Polpa (milhões t)", overlaying = "y", side = "right", range = c(0, 250)),
                  showlegend = TRUE,  # Exibir a legenda
                  updatemenus = list(
                    list(type = "buttons", 
                         buttons = list(
                           list(label = "Play",
                                method = "animate",
                                args = list(NULL, list(frame = list(duration = 500, redraw = TRUE), 
                                                       fromcurrent = TRUE, mode = "immediate"))),
                           list(label = "Pause",
                                method = "animate",
                                args = list(NULL, list(frame = list(duration = 0, redraw = FALSE), mode = "immediate")))
                         ))))

# Exibindo o gráfico interativo com animação
p



#Salvando gráfico

# Criando o diretório para salvar os frames
if (!dir.exists("consumo_per_capta")) {
  dir.create("consumo_per_capta")
}

# Encontrando o intervalo de anos completo para usar no ajuste dinâmico do eixo X
ano_min <- min(consumo_per_capta_world_long$Ano)
ano_max <- max(consumo_per_capta_world_long$Ano)

# Loop para salvar cada frame como PNG
for (i in seq_len(nrow(consumo_per_capta_world_long))) {
  p <- plot_ly() %>%
    add_trace(x = consumo_per_capta_world_long$Ano[1:i], 
              y = consumo_per_capta_world_long$World[1:i], 
              type = "scatter", mode = "lines+markers",
              line = list(color = "#4D4D4D"),
              marker = list(color = "#4D4D4D"),
              name = "Consumo per Capta Mundial",
              text = paste("Ano:", consumo_per_capta_world_long$Ano[1:i], "<br>Consumo:", consumo_per_capta_world_long$World[1:i]),
              hoverinfo = "text") %>%
    add_trace(x = world_prod_grafic$Date[1:i], 
              y = world_prod_grafic$`World_Production (million of tons)`[1:i], 
              type = "scatter", mode = "lines+markers",
              line = list(color = "#76C7A0"),
              marker = list(color = "#76C7A0"),
              name = "Produção Mundial de Polpa",
              text = paste("Ano:", world_prod_grafic$Date[1:i], "<br>Produção:", world_prod_grafic$`World_Production (million of tons)`[1:i]),
              hoverinfo = "text",
              yaxis = "y2") %>%
    layout(title = paste("Evolução do Consumo per Capta Mundial e Produção Mundial de Polpa para Papel:", consumo_per_capta_world_long$Ano[i]),
           xaxis = list(title = "Ano", range = c(ano_min, consumo_per_capta_world_long$Ano[i])),  
           yaxis = list(title = "Consumo per capta mundial de polpa para papel (kg/pessoa)", range = c(0, 100)),
           yaxis2 = list(title = "Produção mundial de polpa para papel (milhões t)", overlaying = "y", side = "right", range = c(0, 250)),
           showlegend = TRUE,
           width = 1000,  
           height = 600)
  
  # Nome do arquivo PNG com zeros à esquerda
  png_filename <- sprintf("consumo_per_capta/frame_%03d.png", i)  
  
  # Salvando o gráfico como PNG usando Kaleido
  save_image(p, file = png_filename, engine = "kaleido")
}


#################################
# Criando o diretório para salvar os frames
if (!dir.exists("pop_production")) {
  dir.create("pop_production")
}

# Encontrando o intervalo de anos completo para usar no ajuste dinâmico do eixo X
ano_min <- min(pop_world$Ano)
ano_max <- max(pop_world$Ano)

# Criando o gráfico de linhas interativo com animação
p <- plot_ly()

# Adicionando os dados ano a ano acumulando a linha para População Mundial
for(i in seq_len(nrow(pop_world))) {
  p <- p %>%
    add_trace(x = pop_world$Ano[1:i], 
              y = pop_world$pop_world[1:i], 
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#4D4D4D"),  
              marker = list(color = "#4D4D4D"),  
              name = "População Mundial (milhões de pessoas)",  
              frame = as.factor(pop_world$Ano[i]),  # Assegure-se de que o frame é um fator de anos
              text = paste("Ano:", pop_world$Ano[1:i], "<br>População:", pop_world$pop_world[1:i]),
              hoverinfo = "text")
}

# Adicionando os dados ano a ano acumulando a linha para Produção Mundial de Polpa (eixo y secundário)
for(i in seq_len(nrow(world_prod_grafic))) {
  p <- p %>%
    add_trace(x = world_prod_grafic$Date[1:i], 
              y = world_prod_grafic$`World_Production (million of tons)`[1:i], 
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#76C7A0"),  
              marker = list(color = "#76C7A0"),  
              name = "Produção Mundial de Polpa",  
              frame = as.factor(world_prod_grafic$Date[i]),  # Assegure-se de que o frame é um fator de anos
              text = paste("Ano:", world_prod_grafic$Date[1:i], "<br>Produção:", world_prod_grafic$`World_Production (million of tons)`[1:i]),
              hoverinfo = "text",
              yaxis = "y2")
}

# Ajustando layout do gráfico com animação
p <- p %>% layout(
  title = "Evolução da população mundial e produção mundial de polpa para papel",
  xaxis = list(title = "Ano", range = list(min(pop_world$Ano), max(pop_world$Ano[1:i]))),  # Ajustando dinamicamente o eixo X
  yaxis = list(title = "População Mundial (milhões de pessoas)", range = c(min(pop_world$pop_world), max(pop_world$pop_world))),  # Ajuste dinâmico do range
  yaxis2 = list(title = "Produção Mundial de Polpa (milhões t)", overlaying = "y", side = "right", range = c(0, max(world_prod_grafic$`World_Production (million of tons)`))),
  showlegend = TRUE,  # Exibir a legenda
  updatemenus = list(
    list(type = "buttons", 
         buttons = list(
           list(label = "Play",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 500, redraw = TRUE), 
                                       fromcurrent = TRUE, mode = "immediate"))),
           list(label = "Pause",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 0, redraw = FALSE), mode = "immediate")))
         ))))

# Exibindo o gráfico interativo com animação
p


# Loop para salvar cada frame como PNG
for (i in seq_len(nrow(pop_world))) {
  p_frame <- plot_ly() %>%
    add_trace(x = pop_world$Ano[1:i], 
              y = pop_world$pop_world[1:i], 
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#4D4D4D"),  
              marker = list(color = "#4D4D4D"),  
              name = "População Mundial (milhões de pessoas)",  
              text = paste("Ano:", pop_world$Ano[1:i], "<br>População:", pop_world$pop_world[1:i]),
              hoverinfo = "text") %>%
    add_trace(x = world_prod_grafic$Date[1:i], 
              y = world_prod_grafic$`World_Production (million of tons)`[1:i], 
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#76C7A0"),  
              marker = list(color = "#76C7A0"),  
              name = "Produção Mundial de Polpa",  
              text = paste("Ano:", world_prod_grafic$Date[1:i], "<br>Produção:", world_prod_grafic$`World_Production (million of tons)`[1:i]),
              hoverinfo = "text",
              yaxis = "y2") %>%
    layout(
      title = paste("Evolução da população mundial e da produção mundial de polpa para papel até o ano", pop_world$Ano[i]),  # Corrigido o título
      xaxis = list(title = "Ano", range = c(min(pop_world$Ano), max(pop_world$Ano[1:i]))),  # Ajustando dinamicamente o eixo X
      yaxis = list(title = "População Mundial (milhões de pessoas)", range = c(0, pop_world$pop_world)),
      yaxis2 = list(title = "Produção Mundial de Polpa (milhões t)", overlaying = "y", side = "right", range = c(0, max(world_prod_grafic$`World_Production (million of tons)`))),
      showlegend = TRUE
    )
  
  # Nome do arquivo PNG com zeros à esquerda
  png_filename <- sprintf("pop_production/frame_%03d.png", i)  
  
  # Salvando o gráfico como PNG usando Kaleido
  save_image(p_frame, file = png_filename, engine = "kaleido") 
}
##################Dados normalizados
# Função para normalizar os dados
normalize_data <- function(df, column) {
  df[[paste0(column, '_norm')]] <- (df[[column]] - min(df[[column]])) / (max(df[[column]]) - min(df[[column]]))
  return(df)
}

# Normalizando os dados de população mundial e produção mundial de polpa
pop_world <- normalize_data(pop_world, 'pop_world')
world_prod_grafic <- normalize_data(world_prod_grafic, 'World_Production (million of tons)')

# Criando o gráfico de linhas interativo com animação
p <- plot_ly()

# Adicionando os dados ano a ano acumulando a linha para População Mundial (normalizada)
for(i in seq_len(nrow(pop_world))) {
  p <- p %>%
    add_trace(x = pop_world$Ano[1:i], 
              y = pop_world$pop_world_norm[1:i],  # Usando a coluna normalizada
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#4D4D4D"),  
              marker = list(color = "#4D4D4D"),  
              name = "População Mundial (normalizada)",  
              frame = as.factor(pop_world$Ano[i]),  
              text = paste("Ano:", pop_world$Ano[1:i], "<br>População:", pop_world$pop_world_norm[1:i]),
              hoverinfo = "text")
}

# Adicionando os dados ano a ano acumulando a linha para Produção Mundial de Polpa (eixo y secundário, normalizada)
for(i in seq_len(nrow(world_prod_grafic))) {
  p <- p %>%
    add_trace(x = world_prod_grafic$Date[1:i], 
              y = world_prod_grafic$`World_Production (million of tons)_norm`[1:i],  # Usando a coluna normalizada
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#76C7A0"),  
              marker = list(color = "#76C7A0"),  
              name = "Produção Mundial de Polpa (normalizada)",  
              frame = as.factor(world_prod_grafic$Date[i]),  
              text = paste("Ano:", world_prod_grafic$Date[1:i], "<br>Produção:", world_prod_grafic$`World_Production (million of tons)_norm`[1:i]),
              hoverinfo = "text",
              yaxis = "y2")
}

# Ajustando layout do gráfico com animação
p <- p %>% layout(
  title = "Evolução normalizada da população mundial e produção mundial de polpa para papel",
  xaxis = list(title = "Ano", range = list(min(pop_world$Ano), max(pop_world$Ano[1:i]))),  # Ajustando dinamicamente o eixo X
  yaxis = list(title = "População Mundial (normalizada)", range = c(0, 1)),  # Ajuste dinâmico do range para a normalização
  yaxis2 = list(title = "Produção Mundial de Polpa (normalizada)", overlaying = "y", side = "right", range = c(0, 1)),
  showlegend = TRUE, 
  legend = list(
    x = 0,  
    y = 1,  
    xanchor = "left",  
    yanchor = "top" ),
  plot_bgcolor = "#f0f0f0",  
  paper_bgcolor = "#f0f0f0",
  margin = list(l = 50, r = 100, t = 50, b = 50),
  updatemenus = list(
    list(type = "buttons", 
         buttons = list(
           list(label = "Play",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 500, redraw = TRUE), 
                                       fromcurrent = TRUE, mode = "immediate"))),
           list(label = "Pause",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 0, redraw = FALSE), mode = "immediate")))
         ))))

# Exibindo o gráfico interativo com animação
p


# Loop para salvar cada frame como PNG
x_range <- c(min(pop_world$Ano), max(pop_world$Ano)) 
for (i in seq_len(nrow(pop_world))) {
  p_frame <- plot_ly() %>%
    add_trace(x = pop_world$Ano[1:i], 
              y = pop_world$pop_world_norm[1:i],  # Usando a coluna normalizada
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#4D4D4D"),  
              marker = list(color = "#4D4D4D"),  
              name = "População Mundial (normalizada)",  
              text = paste("Ano:", pop_world$Ano[1:i], "<br>População:", pop_world$pop_world_norm[1:i]),
              hoverinfo = "text") %>%
    add_trace(x = world_prod_grafic$Date[1:i], 
              y = world_prod_grafic$`World_Production (million of tons)_norm`[1:i],  # Usando a coluna normalizada
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#76C7A0"),  
              marker = list(color = "#76C7A0"),  
              name = "Produção Mundial de Polpa (normalizada)",  
              text = paste("Ano:", world_prod_grafic$Date[1:i], "<br>Produção:", world_prod_grafic$`World_Production (million of tons)_norm`[1:i]),
              hoverinfo = "text",
              yaxis = "y2") %>%
    layout(
      title = paste("Evolução da população mundial e da produção mundial de polpa para papel:", pop_world$Ano[i]),  
      xaxis = list(title = "Ano", range = x_range),  
      yaxis = list(title = "População Mundial (normalizada)", range = c(0, 1)),
      yaxis2 = list(title = "Produção Mundial de Polpa (normalizada)", overlaying = "y", side = "right", range = c(0, 1)),
      showlegend = TRUE,
      legend = list(
        x = 0,  
        y = 1,  
        xanchor = "left",  
        yanchor = "top"),
      plot_bgcolor = "#f0f0f0",  
      paper_bgcolor = "#f0f0f0",
      margin = list(l = 50, r = 100, t = 50, b = 50)  
    )
  
  # Nome do arquivo PNG com zeros à esquerda
  png_filename <- sprintf("pop_production/frame_%03d.png", i)  
  
  # Salvando o gráfico como PNG usando Kaleido
  save_image(p_frame, file = png_filename, engine = "kaleido") 
} 

# Criando o GIF a partir dos frames
frames <- list.files("pop_production", pattern = "*.png", full.names = TRUE)
frames <- sort(frames)  

# Carregando os frames como imagens
imgs <- image_read(frames)

# Adicionando repetição do último frame para "pausar" no final
last_frame <- imgs[length(imgs)]
extra_frames <- rep(last_frame, 30)  
imgs_with_pause <- c(imgs, extra_frames)

# Criando o GIF com animação lenta e pausando no final
animation <- image_animate(imgs_with_pause, fps = 2)

# Salvando o GIF
image_write(animation, "pop_production.gif")

cat("GIF criado com sucesso e salvo como 'pop_production.gif'.\n")


#####

#Gerar o gráfico apenas para consumo per capto
##############################################################
consumo_per_capta_world_long
# Criando o diretório para salvar os frames
if (!dir.exists("consumo_per_capta_world")) {
  dir.create("consumo_per_capta_world")
}

# Criando o gráfico de linhas interativo para o Consumo per Capta Mundial
p <- plot_ly()

# Adicionando os dados ano a ano acumulando a linha para Consumo per Capta Mundial
for(i in seq_len(nrow(consumo_per_capta_world_long))) {
  p <- p %>%
    add_trace(x = consumo_per_capta_world_long$Ano[1:i], 
              y = consumo_per_capta_world_long$World[1:i],  # Usando a coluna com os valores do consumo
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#616A6B"),  
              marker = list(color = "#616A6B"),  
              name = "Consumo per Capta Mundial (kg/pessoa)",  
              frame = as.factor(consumo_per_capta_world_long$Ano[i]),  # Cada frame corresponde a um ano
              text = paste("Ano:", consumo_per_capta_world_long$Ano[1:i], "<br>Consumo:", consumo_per_capta_world_long$World[1:i]),
              hoverinfo = "text")
}

# Ajustando layout do gráfico com animação e cor de fundo
p <- p %>% layout(
  title = "Evolução do Consumo per Capta Mundial de Polpa de Papel",
  xaxis = list(title = "Ano", range = c(min(consumo_per_capta_world_long$Ano), max(consumo_per_capta_world_long$Ano))),  # Manter o eixo X estático
  yaxis = list(title = "Consumo per Capta (kg/pessoa)", range = c(15, 45)),  # Ajuste dinâmico do range para o consumo
  showlegend = TRUE,
  legend = list(
    x = 0,  
    y = 1,  
    xanchor = "left",  
    yanchor = "top" ),
  plot_bgcolor = "#f0f0f0",  
  paper_bgcolor = "#f0f0f0",  
  updatemenus = list(
    list(type = "buttons", 
         buttons = list(
           list(label = "Play",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 500, redraw = TRUE), 
                                       fromcurrent = TRUE, mode = "immediate"))),
           list(label = "Pause",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 0, redraw = FALSE), mode = "immediate")))
         ))))

# Exibindo o gráfico interativo com animação
p


# Loop para salvar cada frame como PNG
x_range = c(min(consumo_per_capta_world_long$Ano), max(consumo_per_capta_world_long$Ano))
for (i in seq_len(nrow(consumo_per_capta_world_long))) {
  p_frame <- plot_ly() %>%
    add_trace(x = consumo_per_capta_world_long$Ano[1:i], 
              y = consumo_per_capta_world_long$World[1:i],  # Usando os valores de consumo
              type = "scatter", 
              mode = "lines+markers",  
              line = list(color = "#616A6B"),  
              marker = list(color = "#616A6B"),  
              name = "Consumo per Capta Mundial (kg de polpa/pessoa)",  
              text = paste("Ano:", consumo_per_capta_world_long$Ano[1:i], "<br>Consumo:", consumo_per_capta_world_long$World[1:i]),
              hoverinfo = "text") %>%
    layout(
      title = paste("Evolução do Consumo per Capta Mundial de polpa para papel:", consumo_per_capta_world_long$Ano[i]),  # Corrigido o título
      xaxis = list(title = "Ano", range = x_range),  # Eixo X permanece estático
      yaxis = list(title = "Consumo per Capta (kg/pessoa)", range = c(15, 45)),
      showlegend = TRUE,
      legend = list(
        x = 0,  
        y = 1,  
        xanchor = "left",  
        yanchor = "top" ),
      plot_bgcolor = "#f0f0f0","#f0f0f0",
      paper_bgcolor = "#f0f0f0"
    )
  
  # Nome do arquivo PNG com zeros à esquerda
  png_filename <- sprintf("consumo_per_capta_world/frame_%03d.png", i)  
  
  # Salvando o gráfico como PNG usando Kaleido
  save_image(p_frame, file = png_filename, engine = "kaleido") 
}



# Criando o GIF a partir dos frames (opcional)
frames <- list.files("consumo_per_capta_world", pattern = "*.png", full.names = TRUE)
frames <- sort(frames)  

# Carregando os frames como imagens
imgs <- image_read(frames)

# Adicionando repetição do último frame para "pausar" no final
last_frame <- imgs[length(imgs)]
extra_frames <- rep(last_frame, 30)  
imgs_with_pause <- c(imgs, extra_frames)

# Criando o GIF com animação lenta e pausando no final
animation <- image_animate(imgs_with_pause, fps = 2)

# Salvando o GIF
image_write(animation, "consumo_per_capta_world.gif")

cat("GIF criado com sucesso e salvo como 'consumo_per_capta_world.gif'.\n")



#####
#Salvando gráfico como GIT para gráfico de barras interativo de consumo per capta top10

print(unique(top10_df$Area))

####Criando o diretório para salvar os frames
if (!dir.exists("consumo_per_capta_barras")) {
  dir.create("consumo_per_capta_barras")
}

# Definindo os anos únicos para iterar
anos <- sort(unique(top10_df$Ano))

# Definir um mapeamento fixo de cores em tons pastéis para os países
color_map <- c(
  "Finlândia" = "#A9DFBF",     # Verde claro
  "Suécia" = "#76C7A0",        # Verde médio
  "Canadá" = "#D5DBDB",        # Cinza claro
  "Noruega" = "#AAB7B8",       # Cinza médio
  "Ilhas Cayman" = "#1D8348",  # Verde escuro
  "Áustria" = "#616A6B",       # Cinza escuro
  "Estados Unidos" = "#A9DFBF", # Verde claro
  "Nova Zelândia" = "#76C7A0",  # Verde médio
  "Essuatíni" = "#D5DBDB",     # Cinza claro
  "Eslovênia" = "#AAB7B8",     # Cinza médio
  "Portugal" = "#1D8348",      # Verde escuro
  "Japão" = "#616A6B",         # Cinza escuro
  "Tchéquia" = "#A9DFBF",      # Verde claro
  "Suíça" = "#76C7A0",         # Verde médio
  "Austrália" = "#D5DBDB",     # Cinza claro
  "Alemanha" = "#1D8348",      # Verde escuro
  "França" = "#AAB7B8"         # Cinza médio
)

# Loop para salvar cada frame como PNG
for (i in seq_along(anos)) {
  # Filtrando os dados para o ano atual
  dados_ano <- top10_df[top10_df$Ano == anos[i], ]
  
  # Criando o gráfico de barras
  p <- plot_ly(dados_ano, 
               x = ~Area, 
               y = ~Consumo_per_capita, 
               color = ~Area, 
               colors = color_map,  
               type = "bar", 
               text = ~round(Consumo_per_capita, 1),  
               textposition = "outside",  
               hoverinfo = "text", 
               name = "Consumo per Capita") %>%  
    layout(
      title = paste("Evolução do Consumo per Capita de Polpa para Papel (Top 10 Países no Ano ) - Ano:", anos[i]),
      xaxis = list(title = "País", categoryorder = "total descending"),  
      yaxis = list(title = "Consumo per Capita (kg/pessoa)", side = "left"),  
      yaxis2 = list(title = "Produção Mundial (milhões t)", overlaying = "y", side = "right"),  
      showlegend = FALSE,
      bargap = 0.1,
      width = 1000,  
      height = 600
    )
  
  # Nome do arquivo PNG com zeros à esquerda
  png_filename <- sprintf("consumo_per_capta_barras/frame_%03d.png", i)
  
  # Salvando o gráfico como PNG usando Kaleido
  save_image(p, file = png_filename, engine = "kaleido")
}

cat("Frames salvos com sucesso na pasta 'consumo_per_capta_barras'.\n")

# Listando todos os frames da pasta consumo_per_capta_barras
frames <- list.files("consumo_per_capta_barras", pattern = "*.png", full.names = TRUE)
frames <- sort(frames)  

# Carregando os frames como imagens
imgs <- image_read(frames)

# Adicionando repetição do último frame para "pausar" no final
last_frame <- imgs[length(imgs)]
extra_frames <- rep(last_frame, 30)  
imgs_with_pause <- c(imgs, extra_frames)

# Criar o GIF com animação lenta e pausando no final
animation <- image_animate(imgs_with_pause, fps = 2)

# Salvar o GIF
image_write(animation, "consumo_per_capta_barras.gif")

cat("GIF criado com sucesso e salvo como 'consumo_per_capta_barras.gif'.\n")

