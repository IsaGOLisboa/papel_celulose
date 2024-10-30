#Bibliotecas
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
import scipy.stats as stats

#dados
###################produção de papel no mundo

producao_world_base = pd.read_csv(r"C:\Users\User\Desktop\papel_celulose\producao_mundial.csv")
print(producao_world_base)

# Filtrando colunas com anos iguais ou maiores a 2000
colunas_filtradas = ['Item'] + [col for col in producao_world_base.columns if col.isdigit() and int(col)>2000]
producao_world = producao_world_base[colunas_filtradas]
producao_world.insert(1, "Unidade", "t")
producao_world.head()

#Ajustando os dados para o gráfico populacao x produção mundial
colunas_filtradas = ['Item'] + [col for col in producao_world_base.columns if col.isdigit()]
producao_world_2 = producao_world_base[colunas_filtradas]
producao_world_2.insert(1, "Unidade", "t")
producao_world_2.head()
# Converter as colunas numéricas, ignorando erros
producao_world_2.iloc[:, 1:] = producao_world_2.iloc[:, 1:].apply(pd.to_numeric, errors = 'coerce')

producao_world_tot = producao_world_2.T
producao_world_tot = producao_world_tot.drop(producao_world_tot.index[1])
# Se a segunda linha contém os nomes das colunas corretos, podemos redefini-las
producao_world_tot.columns = producao_world_tot.iloc[0]  # Usar a segunda linha (index 0) como nomes das colunas
producao_world_tot = producao_world_tot.drop(producao_world_tot.index[0])  # Remover a primeira linha de dados
producao_world_tot['Pulp for paper'] = pd.to_numeric(producao_world_tot['Pulp for paper'], errors='coerce')


##################PIB China e EUA
GDP = pd.read_csv(r"C:\Users\User\Desktop\papel_celulose\P_Data_Extract_From_World_Development_Indicators (2)\c167bd50-a437-47e1-a731-f520e6f3f113_Series - Metadata.csv")
GDP = GDP.drop(GDP.columns[[0,1,3]], axis=1)
GDP.columns = GDP.columns.str.replace(r'\s*\[.*?\]', '', regex=True)
GDP.rename(columns = {'Country Name' : 'Item'}, inplace = True)
GDP.insert(1, "Unidade", "current US$")
GDP=GDP.iloc[0:2] 
GDP
###############População mundial
populacao_world = pd.read_csv(r"C:\Users\User\Desktop\papel_celulose\P_Popular Indicators\48c8c916-0ced-4f96-a8d0-14eee63a8cff_Series - Metadata.csv", sep=",", encoding='ISO-8859-1')
populacao_world = populacao_world.drop(populacao_world.columns[[0,1,3]], axis = 1)
populacao_world.columns = populacao_world.columns.str.replace(r'\s*\[.*?\]', '', regex=True)
populacao_world.columns


populacao_world.iloc[:, 1:] =populacao_world.iloc[:, 1:].apply(pd.to_numeric, errors = 'coerce')
populacao_world_tot = populacao_world.drop(columns='Country Name').sum()
populacao_world_tot = pd.DataFrame(populacao_world_tot)
populacao_world_tot = populacao_world_tot.T

populacao_world_tot.insert(0, "Unidade", "bilhoes de pessoas")
populacao_world_tot.insert(0, "Item", "populacao_mundial")
populacao_world_tot.iloc[:, 2:] = populacao_world_tot.iloc[:, 2:].div(10**9)
populacao_world_tot




populacao_world_2 = pd.read_csv(r"C:\Users\User\Desktop\papel_celulose\P_Data_Extract_From_Population_estimates_and_projections\d36a2f7c-0bca-4f88-acff-3308979dc0a1_Series - Metadata.csv", encoding = "utf-8")


print(populacao_world_2)
#ajustando df 
populacao_world_2 = populacao_world_2.drop(populacao_world_2.columns[[1,2,3]], axis=1)
populacao_world_2 = populacao_world_2.loc[:, :'2022 [YR2022]']
populacao_world_2.columns = populacao_world_2.columns.str.replace(r'\s*\[.*?\]', '', regex=True)
populacao_world_2.columns
print(populacao_world_2['Country Name'].unique())

pop_world_2 = populacao_world_2[populacao_world_2['Country Name'] == "World"]
pop_world_2
pop_world_2_tot = pd.DataFrame(pop_world_2)
populacao_world_2_tot = pop_world_2_tot.T
populacao_world_2_tot


populacao_world_2_tot.iloc[:, 2:] = populacao_world_2_tot.iloc[:, 2:]

populacao_world_2_tot.columns
populacao_world_2_tot.columns = populacao_world_2_tot.iloc[0]
populacao_world_2_tot = populacao_world_2_tot.drop(populacao_world_2_tot.index[0])
populacao_world_2_tot = populacao_world_2_tot.reset_index()
populacao_world_2_tot = populacao_world_2_tot.rename(columns={'index': 'Ano', 'World': 'População (bilhões de pessoas)'})
populacao_world_2_tot.reset_index(drop = True)
populacao_world_2_tot.index.name = None

populacao_world_2_tot['População (bilhões de pessoas)'] = pd.to_numeric(populacao_world_2_tot['População (bilhões de pessoas)'])
populacao_world_2_tot.to_csv(r"C:\Users\User\Desktop\papel_celulose\dfs\populacao_mundial.csv")
####Gráfico
# Plotando o gráfico
plt.figure(figsize=(10, 6))
plt.plot(populacao_world_2_tot['Ano'], populacao_world_2_tot['População (bilhões de pessoas)'], marker='o', color='b')

# Definindo título e rótulos
plt.title('Evolução da População Mundial ao Longo dos Anos')
plt.xlabel('Ano')
plt.ylabel('População (bilhões de pessoas)')

# Exibindo o gráfico
plt.grid(True)
plt.show()



populacao_por_pais = pd.read_csv(r'C:\Users\User\Desktop\papel_celulose\P_Data_Extract_From_Population_estimates_and_projections\ec16d308-757f-4cf5-987a-b23a01d971cf_Series - Metadata.csv')
populacao_por_pais = populacao_por_pais.drop(populacao_por_pais.columns[[1,2,3]], axis = 1)
populacao_por_pais.columns = populacao_por_pais.columns.str.replace(r'\s*\[.*?\]', '', regex=True)
populacao_por_pais= populacao_por_pais.drop(populacao_por_pais.columns[[63, 64, 65, 66, 67]], axis = 1)
populacao_por_pais.rename(columns={"Country Name": "Pais"}, inplace = True)
populacao_por_pais.to_csv(r"C:\Users\User\Desktop\papel_celulose\dfs\populacao_por_pais.csv")


#####################E-commerce nos USA

e_commerce = pd.read_csv(r"C:\Users\User\Desktop\papel_celulose\ECOMSA.csv")
e_commerce.columns
e_commerce['DATE'] = pd.to_datetime(e_commerce['DATE'])
e_commerce['Year'] = e_commerce['DATE'].dt.year
e_commerce = e_commerce.groupby('Year')['ECOMSA'].sum().reset_index()
e_commerce.columns = [None] * len(e_commerce.columns)
e_commerce = e_commerce.T
e_commerce.columns = [None] * len(e_commerce.columns)
e_commerce.columns = e_commerce.iloc[0]
e_commerce.columns = e_commerce.columns.astype(int)
e_commerce = e_commerce.drop(e_commerce.columns[[0,25]], axis=1)
e_commerce = e_commerce.reset_index(drop=True)
e_commerce = e_commerce.drop([0], axis=0).reset_index(drop=True)
e_commerce.insert(0, "Unidade", "milhoes US$")
e_commerce.insert(0, "Item", "e_commerce")
print(e_commerce)


##############merge dos dfs
# Reindexando todos os dfs
# Função para garantir que as colunas de anos sejam inteiras
def padronizar_colunas_anos(df):
    # Converter as colunas de anos para int (se possível)
    df.columns = [int(col) if str(col).isdigit() else col for col in df.columns]
    return df

# Aplicando a função para padronizar os anos como inteiros 
producao_world = padronizar_colunas_anos(producao_world)
GDP = padronizar_colunas_anos(GDP)
populacao_world_tot = padronizar_colunas_anos(populacao_world_tot)
e_commerce = padronizar_colunas_anos(e_commerce)


# Encontrando as colunas comuns (Item, Unidade e anos)
anos_comuns = list(set(producao_world.columns) & set(GDP.columns) & set(populacao_world_tot.columns) & set(e_commerce.columns))

# Separando as colunas de anos (inteiros) das colunas de strings (como 'Item' e 'Unidade')
colunas_anos = [col for col in anos_comuns if isinstance(col, int)]
colunas_fixas = ['Item', 'Unidade']

# Reindexando todos os DataFrames 
producao_world = producao_world.reindex(columns=colunas_fixas + sorted(colunas_anos))
GDP = GDP.reindex(columns=colunas_fixas + sorted(colunas_anos))
populacao_world_tot = populacao_world_tot.reindex(columns=colunas_fixas + sorted(colunas_anos))
e_commerce = e_commerce.reindex(columns=colunas_fixas + sorted(colunas_anos))

# Concatenando 
dados_papel_macro = pd.concat([producao_world, GDP, populacao_world_tot, e_commerce], ignore_index=True)


print(dados_papel_macro)

dados_papel_macro.to_csv(r"C:\Users\User\Desktop\papel_celulose\dados_papel_macro.csv")


###################Análise de dados

# Garantindo que os anos sejam convertidos em colunas numéricas
colunas_anos = [col for col in dados_papel_macro.columns if str(col).isdigit()]

# Alongando o DataFrame com as colunas de anos e variáveis
dados_long = pd.melt(dados_papel_macro, id_vars=['Item', 'Unidade'], value_vars=colunas_anos, var_name='Ano', value_name='Valor')

# Certificando - se que os valores estão em formato numérico
dados_long['Valor'] = pd.to_numeric(dados_long['Valor'], errors='coerce')

# Criando um DataFrame pivotado para ter os itens como colunas e os anos como índice
dados_pivot = dados_long.pivot_table(index='Ano', columns='Item', values='Valor')

# Gerando a matriz de correlação para as variáveis (colunas como produção, PIB, etc.)
matriz_correlacao = dados_pivot.corr()




print(dados_pivot)

dados_pivot.describe()
dados_pivot_reset = dados_pivot.reset_index()


dados_papel_macro = dados_pivot_reset
dados_papel_macro.columns
######################Regressão multipla 
#Definindo var dependente
Y = dados_papel_macro['Pulp for paper']

#Definindo vars explicativas
X = dados_papel_macro.drop(columns=['Pulp for paper', 'Ano', "United States"])
# Adicionando uma constante ao modelo (necessário para a regressão)
X = sm.add_constant(X)

# Ajustando o modelo de regressão múltipla
modelo = sm.OLS(Y, X).fit()

# Exibindo o resumo dos resultados da regressão
print(modelo.summary())

###Step wise
def stepwise_selection(X, y, 
                       initial_list=[], 
                       threshold_in=0.01, 
                       threshold_out=0.05, 
                       verbose=True):
    """ 
    Stepwise selection based on p-value from statsmodels.api.OLS
    Arguments:
        X - pandas DataFrame with candidate features
        y - list-like with the target variable
        initial_list - list of features to start with (default is empty)
        threshold_in - include a feature if its p-value < threshold_in
        threshold_out - exclude a feature if its p-value > threshold_out
        verbose - whether to print the progress
    Returns:
        list of selected features 
    """
    included = list(initial_list)
    while True:
        changed = False
        # forward step
        excluded = list(set(X.columns) - set(included))
        new_pval = pd.Series(index=excluded)
        for new_column in excluded:
            model = sm.OLS(y, sm.add_constant(pd.DataFrame(X[included + [new_column]]))).fit()
            new_pval[new_column] = model.pvalues[new_column]
        best_pval = new_pval.min()
        if best_pval < threshold_in:
            best_feature = new_pval.idxmin()
            included.append(best_feature)
            changed = True
            if verbose:
                print(f'Add  {best_feature} with p-value {best_pval}')
        
        # backward step
        model = sm.OLS(y, sm.add_constant(pd.DataFrame(X[included]))).fit()
        pvalues = model.pvalues.iloc[1:]  # all except intercept
        worst_pval = pvalues.max()  # null if pvalues is empty
        if worst_pval > threshold_out:
            worst_feature = pvalues.idxmax()
            included.remove(worst_feature)
            changed = True
            if verbose:
                print(f'Remove {worst_feature} with p-value {worst_pval}')
        
        if not changed:
            break

    return included

# Aplicando a seleção stepwise
result = stepwise_selection(X, Y)

# Imprimindo as variáveis selecionadas
print('Variáveis selecionadas:', result)

# Ajustando o modelo final com as variáveis selecionadas
X_selected = sm.add_constant(X[result])
modelo_final = sm.OLS(Y, X_selected).fit()

# Exibindo o resumo do modelo final
print(modelo_final.summary())


residuos = modelo.resid
# Teste de normalidade Shapiro-Wilk (substituindo o Shapiro-Francia)
stat, p_value = stats.shapiro(residuos)

print(f'Estatística do teste: {stat}')
print(f'Valor p: {p_value}')

# Interpretação
if p_value > 0.05:
    print("Os resíduos parecem seguir uma distribuição normal (falha ao rejeitar H0).")
else:
    print("Os resíduos não seguem uma distribuição normal (rejeita-se H0).")

###########Transformação de box_cox
# Verificando se todos os valores de Y são positivos
if np.any(Y <= 0):
    print("A transformação de Box-Cox só pode ser aplicada a dados positivos.")
else:
    # Aplicando a transformação de Box-Cox
    Y_boxcox, lambda_bc = stats.boxcox(Y)
    
    print(f"Lambda da transformação de Box-Cox: {lambda_bc}")

    # Exibir os valores transformados de Y
    print("Valores de Y transformados (Box-Cox):")
    print(Y_boxcox)

    
   
############Reaplicando o modelo com box_cox
###Step wise
def stepwise_selection(X, y, 
                       initial_list=[], 
                       threshold_in=0.01, 
                       threshold_out=0.05, 
                       verbose=True):
    """ 
    Stepwise selection based on p-value from statsmodels.api.OLS
    Arguments:
        X - pandas DataFrame with candidate features
        y - list-like with the target variable
        initial_list - list of features to start with (default is empty)
        threshold_in - include a feature if its p-value < threshold_in
        threshold_out - exclude a feature if its p-value > threshold_out
        verbose - whether to print the progress
    Returns:
        list of selected features 
    """
    included = list(initial_list)
    while True:
        changed = False
        # forward step
        excluded = list(set(X.columns) - set(included))
        new_pval = pd.Series(index=excluded)
        for new_column in excluded:
            model = sm.OLS(Y_boxcox, sm.add_constant(pd.DataFrame(X[included + [new_column]]))).fit()
            new_pval[new_column] = model.pvalues[new_column]
        best_pval = new_pval.min()
        if best_pval < threshold_in:
            best_feature = new_pval.idxmin()
            included.append(best_feature)
            changed = True
            if verbose:
                print(f'Add  {best_feature} with p-value {best_pval}')
        
        # backward step
        model = sm.OLS(Y_boxcox, sm.add_constant(pd.DataFrame(X[included]))).fit()
        pvalues = model.pvalues.iloc[1:]  # all except intercept
        worst_pval = pvalues.max()  # null if pvalues is empty
        if worst_pval > threshold_out:
            worst_feature = pvalues.idxmax()
            included.remove(worst_feature)
            changed = True
            if verbose:
                print(f'Remove {worst_feature} with p-value {worst_pval}')
        
        if not changed:
            break

    return included

# Aplicando a seleção stepwise
result_transformado = stepwise_selection(X, Y_boxcox)

# Imprimindo as variáveis selecionadas
print('Variáveis selecionadas:', result_transformado)

# Ajustando o modelo final com as variáveis selecionadas
X_selected = sm.add_constant(X[result_transformado])
modelo_final_transformado = sm.OLS(Y_boxcox, X_selected).fit()

# Exibindo o resumo do modelo final
print(modelo_final_transformado.summary())


residuos_transformado = modelo_final_transformado.resid
# Teste de normalidade Shapiro-Wilk (substituindo o Shapiro-Francia)
stat, p_value = stats.shapiro(residuos_transformado)

print(f'Estatística do teste: {stat}')
print(f'Valor p: {p_value}')

# Interpretação
if p_value > 0.05:
    print("Os resíduos parecem seguir uma distribuição normal (falha ao rejeitar H0).")
else:
    print("Os resíduos não seguem uma distribuição normal (rejeita-se H0).")



# Plotar um histograma dos resíduos transformados
import matplotlib.pyplot as plt
plt.hist(residuos_transformado, bins=30, edgecolor='black')
plt.title('Histograma dos Resíduos (Modelo com Y Transformado)')
plt.show()

# Teste de normalidade dos resíduos após a transformação
stat, p_value = stats.shapiro(residuos_transformado)
print(f'Estatística do teste de Shapiro-Wilk: {stat}')
print(f'Valor p: {p_value}')

if p_value > 0.05:
    print("Os resíduos do modelo transformado seguem uma distribuição normal (falha ao rejeitar H0).")
else:
    print("Os resíduos do modelo transformado não seguem uma distribuição normal (rejeita-se H0).")


# Criando o gráfico Q-Q
stats.probplot(residuos_transformado, dist="norm", plot=plt)
plt.title('Gráfico Q-Q dos Resíduos')
plt.show()


######################Random Forest
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, r2_score
import matplotlib.pyplot as plt
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import cross_val_score

# Separar a variável dependente (y) - produção de papel
y = dados_papel_macro['Pulp for paper']

# Separar as variáveis explicativas (X) - as demais variáveis
X = dados_papel_macro.drop(columns=['Pulp for paper', 'Ano', 'United States'])

# Dividir os dados em conjunto de treino e teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Inicializar o modelo de Random Forest
random_forest = RandomForestRegressor(n_estimators=100, random_state=42)

# Treinar o modelo nos dados de treino
random_forest.fit(X_train, y_train)

# Fazer previsões no conjunto de teste
y_pred = random_forest.predict(X_test)

# Avaliar o modelo
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

print(f"Mean Squared Error: {mse}")
print(f"R-squared: {r2}")

# Visualizar a importância das variáveis
importances = random_forest.feature_importances_
indices = np.argsort(importances)[::-1]

# Plotar a importância das variáveis
plt.figure(figsize=(10, 6))
plt.title('Importância das Variáveis')
plt.bar(range(X.shape[1]), importances[indices], align='center')
plt.xticks(range(X.shape[1]), [X.columns[i] for i in indices], rotation=90)
plt.tight_layout()
plt.show()

print("Média dos valores reais: ", np.mean(y_test))
# Calcular o Root Mean Squared Error (RMSE)
rmse = np.sqrt(mse)
print(f"Root Mean Squared Error: {rmse}")


###########Tunning do modelo

# Definir os hiperparâmetros que serão ajustados
param_grid = {
    'n_estimators': [100, 200, 300],         # Número de árvores na floresta
    'max_depth': [None, 10, 20, 30],         # Profundidade máxima das árvores
    'min_samples_split': [2, 5, 10],         # Número mínimo de amostras para dividir um nó
    'min_samples_leaf': [1, 2, 4],           # Número mínimo de amostras em uma folha
    'max_features': ['auto', 'sqrt', 'log2'] # Número máximo de variáveis consideradas para a melhor divisão
}

# Inicializar o modelo Random Forest
rf = RandomForestRegressor(random_state=42)

# Inicializar o GridSearchCV
grid_search = GridSearchCV(estimator=rf, param_grid=param_grid, 
                           cv=5, n_jobs=-1, verbose=2, scoring='neg_mean_squared_error')

# Ajustar o GridSearchCV aos dados de treino
grid_search.fit(X_train, y_train)

# Exibir os melhores hiperparâmetros encontrados
print(f"Melhores hiperparâmetros: {grid_search.best_params_}")

# Ajustar o modelo final com os melhores hiperparâmetros
best_rf = grid_search.best_estimator_

# Fazer previsões no conjunto de teste
y_pred_tuned = best_rf.predict(X_test)

# Avaliar o modelo ajustado
mse_tuned = mean_squared_error(y_test, y_pred_tuned)
r2_tuned = r2_score(y_test, y_pred_tuned)

print(f"Mean Squared Error após o tuning: {mse_tuned}")
print(f"R-squared após o tuning: {r2_tuned}")


# Avaliar o desempenho do modelo nos dados de treino
y_train_pred = best_rf.predict(X_train)
mse_train = mean_squared_error(y_train, y_train_pred)
r2_train = r2_score(y_train, y_train_pred)

print(f"Mean Squared Error no treino: {mse_train}")
print(f"R-squared no treino: {r2_train}")

# Avaliar o desempenho do modelo nos dados de teste (já feito no código anterior)
mse_test = mean_squared_error(y_test, y_pred_tuned)
r2_test = r2_score(y_test, y_pred_tuned)

print(f"Mean Squared Error no teste: {mse_test}")
print(f"R-squared no teste: {r2_test}")



####Utilizand validação cruzada
# Ajustar o modelo de Random Forest com os hiperparâmetros tunados
rf_tuned = RandomForestRegressor(
    n_estimators=500,           # Aumentar o número de árvores
    max_depth=10,               # Permitir maior profundidade das árvores
    min_samples_split=2,        # Reduzir a exigência de divisões para aumentar a complexidade
    min_samples_leaf=1,         # Reduzir o número mínimo de folhas
    random_state=42
)

# Realizar a validação cruzada com 5 folds
cv_scores = cross_val_score(rf_tuned, X, y, cv=5, scoring='neg_mean_squared_error')

# Converter os escores para valores positivos
mse_cv_scores = -cv_scores

# Calcular a média e o desvio padrão do MSE da validação cruzada
mean_mse_cv = mse_cv_scores.mean()
std_mse_cv = mse_cv_scores.std()

print(f"Mean Squared Error médio na validação cruzada: {mean_mse_cv}")
print(f"Desvio padrão do MSE na validação cruzada: {std_mse_cv}")

# Treinar o modelo de Random Forest com os dados completos
rf_tuned.fit(X_train, y_train)

# Avaliar o modelo nos dados de treino e teste
y_train_pred_tuned = rf_tuned.predict(X_train)
y_test_pred_tuned = rf_tuned.predict(X_test)

# Calcular o MSE e o R² no treino e teste
mse_train_tuned = mean_squared_error(y_train, y_train_pred_tuned)
r2_train_tuned = r2_score(y_train, y_train_pred_tuned)
mse_test_tuned = mean_squared_error(y_test, y_test_pred_tuned)
r2_test_tuned = r2_score(y_test, y_test_pred_tuned)

print(f"Mean Squared Error no treino (ajustado): {mse_train_tuned}")
print(f"R-squared no treino (ajustado): {r2_train_tuned}")
print(f"Mean Squared Error no teste (ajustado): {mse_test_tuned}")
print(f"R-squared no teste (ajustado): {r2_test_tuned}")


################Gradiente boosting
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import train_test_split

# Dividir os dados em treino e teste (se não tiver feito antes)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Inicializar o modelo de Gradient Boosting
gb = GradientBoostingRegressor(
    n_estimators=100,      # Número de árvores
    learning_rate=0.1,     # Taxa de aprendizado
    max_depth=3,           # Profundidade máxima das árvores
    min_samples_split=2,   # Número mínimo de amostras para dividir um nó
    min_samples_leaf=1,    # Número mínimo de amostras por folha
    random_state=42
)

# Treinar o modelo nos dados de treino
gb.fit(X_train, y_train)

# Fazer previsões no conjunto de teste
y_pred_gb = gb.predict(X_test)

# Avaliar o modelo
mse_gb = mean_squared_error(y_test, y_pred_gb)
r2_gb = r2_score(y_test, y_pred_gb)

print(f"Mean Squared Error (MSE): {mse_gb}")
print(f"R-squared (R²): {r2_gb}")

# Avaliar o desempenho nos dados de treino (para verificar overfitting)
y_train_pred_gb = gb.predict(X_train)
mse_train_gb = mean_squared_error(y_train, y_train_pred_gb)
r2_train_gb = r2_score(y_train, y_train_pred_gb)

print(f"Mean Squared Error no treino: {mse_train_gb}")
print(f"R-squared no treino: {r2_train_gb}")





#####Tuning
# Tentar um ajuste mais equilibrado nos hiperparâmetros
gb_tuned = GradientBoostingRegressor(
    n_estimators=1000,          # Número de árvores maior que antes para aumentar a complexidade
    learning_rate=0.01,        # Taxa de aprendizado mais baixa para um ajuste mais suave
    max_depth=7,               # Aumentar um pouco a profundidade para melhorar a complexidade
    min_samples_split=9,       # Reduzir a restrição de amostras para split
    min_samples_leaf=2,        # Permitir folhas menores
    subsample=0.9,             # Subamostrar um pouco mais para permitir mais ajuste
    max_features='sqrt',       # Manter a limitação de features
    random_state=42
)


# Exibindo a média e o desvio padrão do MSE
print(f"MSE médio na validação cruzada: {np.mean(mse_scores)}")
print(f"Desvio padrão do MSE na validação cruzada: {np.std(mse_scores)}")

# Treinar o modelo ajustado
gb_tuned.fit(X_train, y_train)

# Fazer previsões no conjunto de teste
y_pred_gb_tuned = gb_tuned.predict(X_test)

# Avaliar o modelo ajustado
mse_gb_tuned = mean_squared_error(y_test, y_pred_gb_tuned)
r2_gb_tuned = r2_score(y_test, y_pred_gb_tuned)

print(f"Mean Squared Error (MSE) após reajuste: {mse_gb_tuned}")
print(f"R-squared (R²) após reajuste: {r2_gb_tuned}")


###Overfiting
# Avaliar o desempenho nos dados de treino
y_train_pred_gb_tuned = gb_tuned.predict(X_train)
mse_train_gb_tuned = mean_squared_error(y_train, y_train_pred_gb_tuned)
r2_train_gb_tuned = r2_score(y_train, y_train_pred_gb_tuned)

print(f"Mean Squared Error no treino (ajustado): {mse_train_gb_tuned}")
print(f"R-squared no treino (ajustado): {r2_train_gb_tuned}")

# Comparar com os resultados no conjunto de teste (já calculado)
print(f"Mean Squared Error no teste (ajustado): {mse_gb_tuned}")
print(f"R-squared no teste (ajustado): {r2_gb_tuned}")


plt.figure(figsize=(10, 6))
sns.scatterplot(x=y_pred_gb_tuned, y=y_test)
plt.plot([min(y_test), max(y_test)], [min(y_test), max(y_test)], color='red', linestyle='--')  # Linha de 45º
plt.title('Valores Reais vs. Valores Ajustados (Fitted)')
plt.xlabel('Valores Ajustados (Fitted)')
plt.ylabel('Valores Reais (Y)')
plt.grid(True)
plt.show()