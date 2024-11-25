# -----------------------------------------
# ETAPA INICIAL: Carga
# -----------------------------------------

# Carregar o pacote necessário
library(tidyverse)

# Configurar o diretório de trabalho
setwd("C:/Users/giovanni.ornellas/Desktop/UFRJ")

# Caminho do dataset
caminho_datasetBA <- "perfil_eleitor_secao_ATUAL_BA.csv"

# Carregar o dataset
dataset_BA <- read_delim(
  file = caminho_datasetBA,
  delim = ";",
  quote = "\"",
  locale = locale(encoding = "ISO-8859-1")
)

# Exibir uma amostra do dataset
print(head(dataset_BA))

# -----------------------------------------
# ETAPA 1: Preparação e Limpeza dos Dados
# -----------------------------------------

# Dimensões do dataset antes da projeção
linhas_antes <- nrow(dataset_BA)
colunas_antes <- ncol(dataset_BA)

# Selecionar colunas de interesse
colunas_interesse <- c(
  "NM_MUNICIPIO", "CD_GENERO", "DS_GENERO",
  "CD_ESTADO_CIVIL", "DS_ESTADO_CIVIL",
  "CD_FAIXA_ETARIA", "DS_FAIXA_ETARIA",
  "CD_GRAU_ESCOLARIDADE", "DS_GRAU_ESCOLARIDADE",
  "QT_ELEITORES_PERFIL"
)

dataset_reduzido_BA <- dataset_BA %>%
  select(all_of(colunas_interesse))

# Remover linhas com dados inválidos
dataset_final_BA <- dataset_reduzido_BA %>%
  filter(
    DS_FAIXA_ETARIA != "Inválido",
    !if_any(everything(), ~ . == "NÃO INFORMADO")
  )

# Dimensões finais do dataset
linhas_finais <- nrow(dataset_final_BA)
colunas_finais <- ncol(dataset_final_BA)

# Total geral de eleitores
total_geral <- sum(dataset_final_BA$QT_ELEITORES_PERFIL, na.rm = TRUE)

# ----------------------------------------
# ETAPA 2: Análise por Critérios Específicos
# ----------------------------------------

# Critérios específicos
criterios <- list(
  "Homens, 35-39 anos, casados, ensino médio completo" = dataset_final_BA %>%
    filter(
      DS_GENERO == "MASCULINO",
      DS_FAIXA_ETARIA == "35 a 39 anos",
      DS_ESTADO_CIVIL == "CASADO",
      DS_GRAU_ESCOLARIDADE == "ENSINO MÉDIO COMPLETO"
    ),
  "Mulheres, 45-49 anos, solteiras, ensino superior completo" = dataset_final_BA %>%
    filter(
      DS_GENERO == "FEMININO",
      DS_FAIXA_ETARIA == "45 a 49 anos",
      DS_ESTADO_CIVIL == "SOLTEIRO",
      DS_GRAU_ESCOLARIDADE == "SUPERIOR COMPLETO"
    ),
  "Homens, 25-29 anos, divorciados, ensino fundamental incompleto" = dataset_final_BA %>%
    filter(
      DS_GENERO == "MASCULINO",
      DS_FAIXA_ETARIA == "25 a 29 anos",
      DS_ESTADO_CIVIL == "DIVORCIADO",
      DS_GRAU_ESCOLARIDADE == "ENSINO FUNDAMENTAL INCOMPLETO"
    ),
  "Ambos os gêneros, 60-64 anos, viúvos, lê e escreve" = dataset_final_BA %>%
    filter(
      DS_FAIXA_ETARIA == "60 a 64 anos",
      DS_ESTADO_CIVIL == "VIÚVO",
      DS_GRAU_ESCOLARIDADE == "LÊ E ESCREVE"
    ),
  "Ambos os gêneros, 21-24 anos, analfabeto" = dataset_final_BA %>%
    filter(
      DS_FAIXA_ETARIA == "21 a 24 anos",
      DS_GRAU_ESCOLARIDADE == "ANALFABETO"
    )
)

# Calcular os totais e porcentagens para cada critério
resultados_criterios <- map_dfr(
  criterios,
  ~ data.frame(
    Total = sum(.x$QT_ELEITORES_PERFIL, na.rm = TRUE),
    Porcentagem = sum(.x$QT_ELEITORES_PERFIL, na.rm = TRUE) / total_geral * 100
  ),
  .id = "Critério"
)

# ---------------------------------------
# ETAPA 3: Contagens e Estatísticas
# ---------------------------------------

# Contagem de eleitores por gênero
contagem_por_sexo <- dataset_final_BA %>%
  group_by(DS_GENERO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Gráfico: Total de eleitores por gênero
ggplot(data = contagem_por_sexo, aes(x = DS_GENERO, y = Total_Eleitores, fill = DS_GENERO)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribuição de Eleitores por Gênero",
    x = "Gênero",
    y = "Total de Eleitores"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Contagem de eleitores por faixa etária
contagem_por_idade <- dataset_final_BA %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Gráfico: Total de eleitores por faixa etária
ggplot(data = contagem_por_idade, aes(x = reorder(DS_FAIXA_ETARIA, -Total_Eleitores), y = Total_Eleitores)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Distribuição de Eleitores por Faixa Etária",
    x = "Faixa Etária",
    y = "Total de Eleitores"
  ) +
  theme_minimal() +
  coord_flip()


# Contagem de eleitores por estado civil
contagem_por_estado_civil <- dataset_final_BA %>%
  group_by(DS_ESTADO_CIVIL) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Gráfico: Total de eleitores por estado civil
ggplot(data = contagem_por_estado_civil, aes(x = reorder(DS_ESTADO_CIVIL, -Total_Eleitores), y = Total_Eleitores)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(
    title = "Distribuição de Eleitores por Estado Civil",
    x = "Estado Civil",
    y = "Total de Eleitores"
  ) +
  theme_minimal() +
  coord_flip()

# Contagem de eleitores por grau de escolaridade
contagem_por_escolaridade <- dataset_final_BA %>%
  group_by(DS_GRAU_ESCOLARIDADE) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Gráfico: Total de eleitores por grau de escolaridade
ggplot(data = contagem_por_escolaridade, aes(x = reorder(DS_GRAU_ESCOLARIDADE, -Total_Eleitores), y = Total_Eleitores)) +
  geom_bar(stat = "identity", fill = "seagreen3") +
  labs(
    title = "Distribuição de Eleitores por Grau de Escolaridade",
    x = "Grau de Escolaridade",
    y = "Total de Eleitores"
  ) +
  theme_minimal() +
  coord_flip()

# Contagem de eleitores por município
contagem_por_municipio <- dataset_final_BA %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Top 20 municípios com mais eleitores
top_municipios <- contagem_por_municipio %>%
  arrange(desc(Total_Eleitores)) %>%
  slice(1:20)

# Gráfico: Top 20 municípios por total de eleitores
ggplot(data = top_municipios, aes(x = reorder(NM_MUNICIPIO, -Total_Eleitores), y = Total_Eleitores)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(
    title = "Top 20 Municípios por Total de Eleitores",
    x = "Município",
    y = "Total de Eleitores"
  ) +
  theme_minimal() +
  coord_flip()


# Exibir as tabelas
contagem_por_sexo
contagem_por_idade
contagem_por_estado_civil
contagem_por_escolaridade
contagem_por_municipio


# Contagem de eleitores por genero e idade
contagem_sexo_idade <- dataset_final_BA %>%
  group_by(DS_GENERO, DS_FAIXA_ETARIA) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Gráfico: Gênero por faixa etária
ggplot(data = contagem_sexo_idade, aes(x = DS_FAIXA_ETARIA, y = Total_Eleitores, fill = DS_GENERO)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribuição de Gênero por Faixa Etária",
    x = "Faixa Etária",
    y = "Total de Eleitores"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  coord_flip()

# Contagem de eleitores por sexo e município
contagem_sexo_municipio <- dataset_final_BA %>%
  group_by(DS_GENERO, NM_MUNICIPIO) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Contagem de eleitores por grau de escolaridade e estado civil
contagem_escolaridade_estado <- dataset_final_BA %>%
  group_by(DS_GRAU_ESCOLARIDADE, DS_ESTADO_CIVIL) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE))

# Gráfico: Porcentagem por estado civil
ggplot(data = contagem_por_estado_civil, aes(x = "", y = Total_Eleitores, fill = DS_ESTADO_CIVIL)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribuição Percentual por Estado Civil",
    fill = "Estado Civil"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),       # Remove os números ao redor
    axis.ticks = element_blank(),      # Remove os ticks dos eixos
    panel.grid = element_blank()       # Remove as grades de fundo
  )

# Exibir as tabelas
contagem_sexo_idade
contagem_sexo_municipio
contagem_escolaridade_estado

# Heatmap

# Agregando os dados por faixa etária, estado civil e gênero
heatmap_data <- dataset_final_BA %>%
  group_by(DS_GENERO, DS_FAIXA_ETARIA, DS_ESTADO_CIVIL) %>%
  summarise(Total_Eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%
  ungroup()

# Filtrar dados para homens
heatmap_homens <- heatmap_data %>%
  filter(DS_GENERO == "MASCULINO")

# Criar o mapa de calor
ggplot(heatmap_homens, aes(x = DS_FAIXA_ETARIA, y = DS_ESTADO_CIVIL, fill = Total_Eleitores)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightpink", high = "darkred", name = "Total Eleitores") +
  labs(
    title = "Mapa de Calor: Faixa Etária vs Estado Civil (Homens)",
    x = "Faixa Etária",
    y = "Estado Civil"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Filtrar dados para mulheres
heatmap_mulheres <- heatmap_data %>%
  filter(DS_GENERO == "FEMININO")

# Criar o mapa de calor
ggplot(heatmap_mulheres, aes(x = DS_FAIXA_ETARIA, y = DS_ESTADO_CIVIL, fill = Total_Eleitores)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Total Eleitores") +
  labs(
    title = "Mapa de Calor: Faixa Etária vs Estado Civil (Mulheres)",
    x = "Faixa Etária",
    y = "Estado Civil"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## Estatiticas por agrupamento

# Função para calcular estatísticas básicas
calcular_estatisticas <- function(data, coluna) {
  
  estatisticas <- data %>%
    summarise(
      Media = mean(.data[[coluna]], na.rm = TRUE),
      Mediana = median(.data[[coluna]], na.rm = TRUE),
      Moda = as.numeric(names(sort(table(.data[[coluna]]), decreasing = TRUE)[1])),
      Desvio_Padrao = sd(.data[[coluna]], na.rm = TRUE),
      Variancia = var(.data[[coluna]], na.rm = TRUE),
      Minimo = min(.data[[coluna]], na.rm = TRUE),
      Maximo = max(.data[[coluna]], na.rm = TRUE),
      Intervalo = Maximo - Minimo
    )
  
  return(estatisticas)
}

estatisticas_sexo_idade <- calcular_estatisticas(contagem_sexo_idade, "Total_Eleitores")
estatisticas_sexo_municipio <- calcular_estatisticas(contagem_sexo_municipio, "Total_Eleitores")
estatisticas_escolaridade_estado <- calcular_estatisticas(contagem_escolaridade_estado, "Total_Eleitores")

estatisticas_sexo_municipio
estatisticas_sexo_idade
estatisticas_escolaridade_estado



# ------------------------------------
# ETAPA 4: Insights e Conclusões
# ------------------------------------


# Calcular porcentagens por grupo
porcentagens <- list(
  "Por Sexo" = contagem_por_sexo %>%
    mutate(Porcentagem = (Total_Eleitores / total_geral) * 100),
  
  "Por Município" = contagem_por_municipio %>%
    mutate(Porcentagem = (Total_Eleitores / total_geral) * 100),
  
  "Por Faixa Etária" = contagem_por_idade %>%
    mutate(Porcentagem = (Total_Eleitores / total_geral) * 100),
  
  "Por Estado Civil" = contagem_por_estado_civil %>%
    mutate(Porcentagem = (Total_Eleitores / total_geral) * 100),
  
  "Por Escolaridade" = contagem_por_escolaridade %>%
    mutate(Porcentagem = (Total_Eleitores / total_geral) * 100)
)

# Município com menos eleitores e porcentagem
municipio_menos <- porcentagens[["Por Município"]] %>%
  arrange(Total_Eleitores) %>%
  slice(1)

# Município com mais eleitores (exceto a capital) e porcentagem
municipio_mais <- porcentagens[["Por Município"]] %>%
  arrange(desc(Total_Eleitores)) %>%
  filter(NM_MUNICIPIO != "SALVADOR") %>%
  slice(1)

# Características dominantes com porcentagens
estado_civil_dominante <- porcentagens[["Por Estado Civil"]] %>%
  arrange(desc(Total_Eleitores)) %>%
  slice(1)

genero_dominante <- porcentagens[["Por Sexo"]] %>%
  arrange(desc(Total_Eleitores)) %>%
  slice(1)

faixa_etaria_dominante <- porcentagens[["Por Faixa Etária"]] %>%
  arrange(desc(Total_Eleitores)) %>%
  slice(1)

escolaridade_dominante <- porcentagens[["Por Escolaridade"]] %>%
  arrange(desc(Total_Eleitores)) %>%
  slice(1)

# Exibir Conclusões com Porcentagens
list(
  Municipio_Menos = municipio_menos,
  Municipio_Mais = municipio_mais,
  Estado_Civil_Dominante = estado_civil_dominante,
  Genero_Dominante = genero_dominante,
  Faixa_Etaria_Dominante = faixa_etaria_dominante,
  Escolaridade_Dominante = escolaridade_dominante
)