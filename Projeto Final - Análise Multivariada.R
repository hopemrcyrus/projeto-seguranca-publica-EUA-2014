install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(ggplot2)
data <- readxl::read_excel("C:/Users/suhayla cassaro/Downloads/novadatabase.xlsx")
str(data)
summary(as.numeric(data$`Victim Age`))
summary(as.numeric(data$`Perpetrator Age`))
data_clean <- data %>%
  filter(as.numeric(`Victim Age`) > 0 & as.numeric(`Victim Age`) <= 100) %>%
  filter(as.numeric(`Perpetrator Age`) > 0 & as.numeric(`Perpetrator Age`) <= 100)
summary(as.numeric(data_clean$`Victim Age`))
# Acima foi realizada a limpeza dos dados e correções nas idades, ois havia erros de digitação
# Abaixo realizaremos uma Análise Exploratória (EDA) para compreender os padrões nos dados, como armas mais usadas, idade média de vítimas, etc. 
# Distribuição da idade das vítimas
ggplot(data_clean, aes(x = as.numeric(`Victim Age`))) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribuição das Idades das Vítimas", x = "Idade", y = "Frequência")
# Armas mais Utilizadas 
data_clean %>%
  count(Weapon, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(Weapon, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Armas Utilizadas", x = "Arma", y = "Frequência")
# Explorando a relação entre a Idade da Vítima e o autor (utilizaremos regressão linear)
data_clean$`Victim Age` <- as.numeric(data_clean$`Victim Age`)
data_clean$`Perpetrator Age` <- as.numeric(data_clean$`Perpetrator Age`)
modelo_rl <- lm(`Victim Age` ~ `Perpetrator Age`, data = data_clean)
summary(modelo_rl)
ggplot(data_clean, aes(x = `Perpetrator Age`, y = `Victim Age`)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regressão Linear entre Idade da Vítima e do Perpetrador")
# Classificar o sexo do perpetrador com base em variáveis como idade, arma utilizada, etc.
# Utilizaremos Análise Discriminante (LDA)
install.packages("MASS")
library(MASS)
install.packages("dplyr")  
library(dplyr)  
install.packages("dplyr")
library(dplyr)
# Preparar dados: remover NAs e selecionar colunas relevantes
lda_data <- data_clean %>%
  filter(`Perpetrator Sex` %in% c("Male", "Female")) %>%
  mutate(`Perpetrator Sex` = factor(`Perpetrator Sex`)) %>%
  select(`Perpetrator Sex`, `Perpetrator Age`, Weapon)
# Converter para fatores
lda_data$Weapon <- factor(lda_data$Weapon)
lda_data$`Perpetrator Age` <- as.numeric(lda_data$`Perpetrator Age`)
# LDA
modelo_lda <- lda(`Perpetrator Sex` ~ `Perpetrator Age` + Weapon, data = lda_data)
print(modelo_lda) # Está apresentando um erro relacionado ao pacote MASS
install.packages("MASS")
library(MASS)
# LDA
modelo_lda <- lda(`Perpetrator Sex` ~ `Perpetrator Age` + Weapon, data = lda_data)
print(modelo_lda)
# Previsões
pred <- predict(modelo_lda)
table(Predito = pred$class, Real = lda_data$`Perpetrator Sex`) 

# Visualizando a Análise Discriminante em um gráfico
library(MASS)
library(ggplot2)
# Treinar o modelo LDA 
modelo_lda <- lda(`Perpetrator Sex` ~ `Perpetrator Age` + Weapon, data = lda_data)
# Obter os escores discriminantes (valores projetados no eixo LDA)
lda_values <- predict(modelo_lda)
# Gráfico da separação por LD1
ggplot(lda_data, aes(x = LD1, fill = `Perpetrator Sex`)) +
  geom_density(alpha = 0.5) +
  labs(title = "Separação entre Sexos pelo Primeiro Discriminante Linear (LD1)",
       x = "LD1", y = "Densidade") +
  theme_minimal() # Ocorreu um erro pois LD1 ainda não foi criada no  lda_data. Após ajustar o modelo com lda(), precisa extrair os escores discriminantes (valores de LD1) e adicioná-los ao lda_data antes de fazer o gráfico.
modelo_lda <- lda(`Perpetrator Sex` ~ `Perpetrator Age` + Weapon, data = lda_data)
lda_values <- predict(modelo_lda)
lda_data$LD1 <- lda_values$x[,1]
library(ggplot2)

ggplot(lda_data, aes(x = LD1, fill = `Perpetrator Sex`)) +
  geom_density(alpha = 0.5) +
  labs(title = "Separação entre Sexos pelo Primeiro Discriminante Linear (LD1)",
       x = "LD1", y = "Densidade") +
  theme_minimal()