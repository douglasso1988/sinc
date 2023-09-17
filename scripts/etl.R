#---------------------------------------------------------------------------
#----------------- BIBLIOTECAS E CONFIGURAÇÕES DE AMBIENTE -----------------
#---------------------------------------------------------------------------

# Nessa seção, configuramos o ambiente do RStudio e invocamos as bibliotecas necessárias para leitura e
# manipulação dos dados.

# Configurações do ambiente do RStudio
options(scipen = 999, digits = 5)


# Configurando uma seed para a geração de números aleatórios
set.seed(1234)

# Bibliotecas necessárias para a execução do script
library(tidyverse)
library(openxlsx)
library(lubridate)
library(jsonlite)



#---------------------------------------------------------------------------
#---------------------------- LEITURA DOS DADOS ----------------------------
#---------------------------------------------------------------------------

# Nessa seção, realizamos a leitura dos dados que serão utilizados nesse algoritmo.

# Dados de pessoas físicas gerados aleatóriamente
# FONTE: 
dados_pf = read.xlsx("C:\\Users\\dougl\\OneDrive\\Desktop\\Hackathon Algar\\datasets\\pf_dataset.xlsx")



#------------------------------------------------------------------------------
#---------------------------- TRATAMENTO DOS DADOS ----------------------------
#------------------------------------------------------------------------------

# Tratamento dos dados de PF
dados_pf_trat = dados_pf %>%
  # Selecionanddo aleatóriamente 30% dos dados
  slice_sample(prop = 0.1) %>%
  # Modificando e criando nomas variáveis
  # - Birthday: transformação para o tipo DATE
  # - num_idade: cálculo da idade do indivíduo
  # - Gender: tradução do gênero
  mutate(Birthday = as.Date(Birthday, "%m/%d/%Y"),
         num_idade = floor(as.numeric(difftime(Sys.Date(), Birthday))/365),
         Gender = ifelse(Gender == "male", "Masculino", "Feminino"),
         nm_endereco_completo = paste0(StreetAddress, ", ", City, " - ", State, " - ", ZipCode)) %>%
  # Selecionando as variáveis de interesse e renomeando os mesmos
  select(c("fiscalDocument" = "NationalID",
           "nm_primeiro_nome" = "GivenName",
           "nm_inicial_segundo_nome" = "MiddleInitial",
           "nm_ultimo_nome" = "Surname",
           "nm_genero" = "Gender",
           "dt_nascimento" = "Birthday",
           "num_idade" = "num_idade",
           "nm_endereco" = "StreetAddress",
           "nm_cidade" = "City",
           "nm_uf" = "State",
           "nm_cep" = "ZipCode",
           "nm_endereco_completo" = "nm_endereco_completo",
           "nm_email" = "EmailAddress",
           "nm_telefone" = "TelephoneNumber"))

# Criando a lista de CPFs
lista_cpf = unique(dados_pf_trat$fiscalDocument)
# Criando a lista de datas
lista_datas = as.Date(seq(from = as.Date("2010-01-01"), to = Sys.Date(), by = "month"))
# Criando uma tabela vazia para adicionar os dados transacionais
dados_transacionais_pf = tibble()
# Contador para o Loop
contador = 1

# Loop para gerar a tabela de
for(cpf in lista_cpf){
  
  # Visualização da "completude" da geração dos dados transacionais
  cat("\f")
  cat(paste(100*round(contador/length(lista_cpf), 4), "%"))
  contador = contador + 1
  
  # Gerando um número de "repetiçoes" aleatória
  num_rep = round(runif(n = 1, min = 1, max = length(lista_datas)), 0)
  
  # Gerando as listas auxiliares
  lista_aux_cpf = rep(cpf, num_rep)
  lista_aux_datas = sort(sample(x = lista_datas, size = num_rep))
  lista_aux_valor = round(runif(n = num_rep, min = 100, max = 600), 2)
  lista_aux_status_cobranca = sample(x = c("PAGO", "NÃO PAGO"), size = num_rep, prob = c(0.8, 0.2), replace = TRUE)
  
  # Criando tabela auxiliar baseaddo nas listas acima
  tbl_aux = tibble(fiscalDocument = lista_aux_cpf,
                   dt_transacao = lista_aux_datas,
                   num_valor_cobranca = lista_aux_valor,
                   nm_status_cobranca = lista_aux_status_cobranca)
  
  # Concatenando a tabela auxiliar com a tabela transacional completa
  dados_transacionais_pf = bind_rows(dados_transacionais_pf, tbl_aux)
  
} # Fim do "for"

# Criando o id_transacao
dados_transacionais_pf = dados_transacionais_pf %>%
  rownames_to_column(var = "id_transacao")

# Limpando a memória
rm(tbl_aux, contador, lista_cpf, lista_datas, lista_aux_cpf, lista_aux_datas, lista_aux_valor, lista_aux_status_cobranca, num_rep, cpf)



#-----------------------------------------------------------------------------
#------------------------ SUMARIZAÇÃO E CLUSTERIZAÇÃO ------------------------
#-----------------------------------------------------------------------------

# Sumarização dos dados transacionais
dados_transacionais_sumarizados = dados_transacionais_pf %>%
  # Agrupamento por CPF
  group_by(id_cpf) %>%
  # Cálculos:
  # - num_transacoes: total de transações que o cliente possui
  # - num_transacoes_paga: total de transações pagas que o cliente possui
  # - num_valor_pago: total pago pelo cliente
  # - num_transacoes_nao_paga: total de transações não pagas que o cliente possui
  # - num_valor_nao_pago: total não pago pelo cliente
  summarise(num_transacoes = n(),
            num_transacoes_paga = sum(ifelse(nm_status_cobranca == "PAGO", 1, 0)),
            num_valor_pago = sum(ifelse(nm_status_cobranca == "PAGO", num_valor_cobranca, 0)),
            num_transacoes_nao_paga = sum(ifelse(nm_status_cobranca == "NÃO PAGO", 1, 0)),
            num_valor_nao_pago = sum(ifelse(nm_status_cobranca == "NÃO PAGO", num_valor_cobranca, 0))) %>%
  # Criação de clusters baseado no algoritmo k-means
  # Foi criado uma variável de cluster para cada indicador criado acima
  mutate(nm_classe_transacoes = kmeans(x = num_transacoes, centers = 3)$cluster,
         nm_classe_transacoes_paga = kmeans(x = num_transacoes_paga, centers = 3)$cluster,
         nm_classe_valor_pago = kmeans(x = num_valor_pago, centers = 3)$cluster,
         nm_classe_transacoes_nao_paga = kmeans(x = num_transacoes_nao_paga, centers = 3)$cluster,
         nm_classe_valor_nao_pago = kmeans(x = num_valor_nao_pago, centers = 3)$cluster)



#----------------------------------------------------------------------------------------
#------------------------ CLASSIFICAÇÃO E PONTUAÇÃO DOS CLUSTERS ------------------------
#----------------------------------------------------------------------------------------

# Nos trechos de códigos abaixo, temos a definição da pontuação de cada cluster

# TRANSAÇÕES
tbl_decisao_classe_transacoes = dados_transacionais_sumarizados %>%
  group_by(nm_classe_transacoes) %>%
  summarise(num_media = mean(num_transacoes)) %>%
  arrange(desc(num_media)) %>%
  mutate(nm_segmentacao_transacoes = c("OURO","PRATA","BRONZE")) %>%
  select(-c("num_media"))

# TRANSAÇÕES PAGAS
tbl_decisao_classe_transacoes_paga = dados_transacionais_sumarizados %>%
  group_by(nm_classe_transacoes_paga) %>%
  summarise(num_media = mean(num_transacoes_paga)) %>%
  arrange(desc(num_media)) %>%
  mutate(nm_segmentacao_transacoes_paga = c("OURO","PRATA","BRONZE")) %>%
  select(-c("num_media"))

# VALOR PAGO
tbl_decisao_classe_valor_pago = dados_transacionais_sumarizados %>%
  group_by(nm_classe_valor_pago) %>%
  summarise(num_media = mean(num_valor_pago)) %>%
  arrange(desc(num_media)) %>%
  mutate(nm_segmentacao_valor_pago = c("OURO","PRATA","BRONZE")) %>%
  select(-c("num_media"))

# TRANSAÇÕES NÃO PAGAS
tbl_decisao_classe_transacoes_nao_paga = dados_transacionais_sumarizados %>%
  group_by(nm_classe_transacoes_nao_paga) %>%
  summarise(num_media = mean(num_transacoes_nao_paga)) %>%
  arrange(num_media) %>%
  mutate(nm_segmentacao_transacoes_nao_paga = c("OURO","PRATA","BRONZE")) %>%
  select(-c("num_media"))

# VALOR NÃO PAGO
tbl_decisao_classe_valor_nao_pago = dados_transacionais_sumarizados %>%
  group_by(nm_classe_valor_nao_pago) %>%
  summarise(num_media = mean(num_valor_nao_pago)) %>%
  arrange(num_media) %>%
  mutate(nm_segmentacao_valor_nao_pago = c("OURO","PRATA","BRONZE")) %>%
  select(-c("num_media"))



#--------------------------------------------------------------------------------------
#------------------------ CONSOLIDAÇÃO DOS DADOS TRANSACIONAIS ------------------------
#--------------------------------------------------------------------------------------

# Agregando as classificações e pontuações por cluster
dados_transacionais_sumarizados = dados_transacionais_sumarizados %>%
  left_join(tbl_decisao_classe_transacoes) %>%
  left_join(tbl_decisao_classe_transacoes_paga) %>%
  left_join(tbl_decisao_classe_valor_pago) %>%
  left_join(tbl_decisao_classe_transacoes_nao_paga) %>%
  left_join(tbl_decisao_classe_valor_nao_pago) %>%
  select(-c("nm_classe_transacoes","nm_classe_transacoes_paga","nm_classe_valor_pago",
            "nm_classe_transacoes_nao_paga","nm_classe_valor_nao_pago")) %>%
  # Cálculo das pontuações
  mutate(num_pontos_transacoes = (num_transacoes - min(num_transacoes))/(max(num_transacoes) - min(num_transacoes)),
         num_pontos_transacoes_paga = (num_transacoes_paga - min(num_transacoes_paga))/(max(num_transacoes_paga) - min(num_transacoes_paga)),
         num_pontos_valor_pago = (num_valor_pago - min(num_valor_pago))/(max(num_valor_pago) - min(num_valor_pago)),
         num_pontos_transacoes_nao_paga = (max(num_transacoes_nao_paga) - num_transacoes_nao_paga)/(max(num_transacoes_nao_paga) - min(num_transacoes_nao_paga)),
         num_pontos_valor_nao_pago = (max(num_valor_nao_pago) - num_valor_nao_pago)/(max(num_valor_nao_pago) - min(num_valor_nao_pago)),
         num_pontos_totais = num_pontos_transacoes + num_pontos_transacoes_paga + num_pontos_valor_pago + num_pontos_transacoes_nao_paga + num_pontos_valor_nao_pago,
         num_score = (num_pontos_totais - min(num_pontos_totais))/(max(num_pontos_totais) - min(num_pontos_totais)))

# Limpando a memória
rm(tbl_decisao_classe_transacoes, tbl_decisao_classe_transacoes_paga, tbl_decisao_classe_valor_pago,
   tbl_decisao_classe_transacoes_nao_paga, tbl_decisao_classe_valor_nao_pago)



#----------------------------------------------------------------------
#------------------------ EXPORTAÇÃO DOS DADOS ------------------------
#----------------------------------------------------------------------

write_json(dados_pf_trat, "C:\\Users\\dougl\\OneDrive\\Desktop\\Hackathon Algar\\outputs\\tbl_pf.json")
write_json(dados_transacionais_pf, "C:\\Users\\dougl\\OneDrive\\Desktop\\Hackathon Algar\\outputs\\tbl_pf_transacional.json")
write_json(dados_transacionais_sumarizados, "C:\\Users\\dougl\\OneDrive\\Desktop\\Hackathon Algar\\outputs\\tbl_pf_transacional_sumarizados.json")
