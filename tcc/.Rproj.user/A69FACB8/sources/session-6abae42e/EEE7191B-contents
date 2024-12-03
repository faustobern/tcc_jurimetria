install.packages("remotes")
remotes::install_github("jjesusfilho/tjsp")
install.packages("tidyverse")

library(tjsp)
library(tidyverse)

#cria tabela para preencher com os dados do e-saj, cada pagina possuí 10 documentos, assim declarando 100 páginas

cjpg <- list(
  processo = "",
  pagina = "",
  hora_coletada = "",
  duplicado = "",
  classe = "",
  assunto = "4829",
  magistrado = "",
  comarca = "",
  foro = "",
  vara = "",
  inicio = "",
  fim = "",
  diretorio = "cjpg",
  paginas = 1:100
)

#ler os processos do portal do tjsp

cjpg <- tjsp_ler_cjpg()


#------------------------------------------------------------------------------------#

# tabela de processos com julgados procedentes, improcedentes ou parcialmente procedentes


#cria nova tabela "procedente" a partir da tabela principal "cjpg" com os julgados que possuem a string "PROCEDENTE" e em seguida substitui os valores da coluna julgado com "PROCEDENTE" para melhor visualização e criação de gráfico
procedente <-cjpg|>
  filter(str_detect(julgado, "PROCEDENTE"))
procedente%julgado <- "PROCEDENTE"


#cria nova tabela "improcedente" a partir da tabela principal "cjpg" com os julgados que possuem a string "IMPROCEDENTE" e em seguida substitui os valores da coluna julgado com "IMPROCEDENTE" para melhor visualização e criação de gráfico
improcedente <-cjpg|>
  filter(str_detect(julgado, "IMPROCEDENTE"))
improcedente%julgado <- "IMPROCEDENTE"


#cria nova tabela "parcial" a partir da tabela principal "cjpg" com os julgados que possuem a string "PARCIALMENTE PROCEDENTE" e em seguida substitui os valores da coluna julgado com "PARCIALMENTE PROCEDENTE" para melhor visualização e criação de gráfico
parcial <-cjpg|>
  filter(str_detect(julgado, "PARCIALMENTE PROCEDENTE"))
parcial%julgado <- "PARCIALMENTE PROCEDENTE"

#------------------------------------------------------------------------------------#

# tabela com os 10 juizes com maior numero de julgados = PROCENDENTE, IMPROCEDENTE OU PARCIALMENTE IMPROCEDENTE

top_juizes_procedente <- procedente %>%
  group_by(magistrado) %>%
  summarise(julgado = n()) %>%
  arrange(desc(julgado)) %>%
  slice_head(n = 10)

top_juizes_improcedente <- improcedente %>%
  group_by(magistrado) %>%
  summarise(julgado = n()) %>%
  arrange(desc(julgado)) %>%
  slice_head(n = 10)

top_juizes_parcial <- parcial %>%
  group_by(magistrado) %>%
  summarise(julgado = n()) %>%
  arrange(desc(julgado)) %>%
  slice_head(n = 10)

#------------------------------------------------------------------------------------#

# Cria gráfico com relação aos 10 maiores magistrados em quantidade de julgados


# Julgados Procedentes
ggplot(top_juizes_procedente, aes(x = reorder(magistrado, julgado), y = julgado)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total de Julgados Procedentes",
    x = "magistrado",
    y = "julgado"
  ) +
  theme_minimal()


# Julgados Improcedentes
ggplot(top_juizes_improcedente, aes(x = reorder(magistrado, julgado), y = julgado)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total de Julgados Improcedentes",
    x = "magistrado",
    y = "julgado"
  ) +
  theme_minimal()


# Julgados Parcialmente Procedentes
ggplot(top_juizes_parcial, aes(x = reorder(magistrado, julgado), y = julgado)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Total de Julgados Parcialmente Procedentes",
    x = "magistrado",
    y = "julgado"
  ) +
  theme_minimal()