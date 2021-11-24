#-----------------------------------------------------------------------------------
# Título: Visualização de dados com ggplot2 e extensões
#-----------------------------------------------------------------------------------
# Autor: Prof. Dr. Deivison Venicio Souza
# Instituto: Universidade Federal do Pará (UFPA)
#-----------------------------------------------------------------------------------

###########################################################
# Parte 1 - Visualização de dados com ggplot2 e extensões
###########################################################

#----------------------------------
# 1 - Motivação
#----------------------------------

# Dados constituem uma representação usada para gerar informações
# sobre um fenômeno.
# Os gráficos são representações visuais úteis para exibir
# informações ocultas nos dados.
# Representações visuais são melhor interpretadas pelo cerébro
# humano.
# A natureza da variável e o objetivo da análise, em geral,
# orientam a melhor representação visual.
# O R-base possui diversas funções para visualização de dados.
# Por exemplo, **plot()**, **hist()**, **barplot()**,
# **boxplot()**, entre outras.
# O tidyverse incorpora o pacote **ggplot2**, um framework
# sensacional para construção de gráficos elegantes.

#----------------------------------
# 2 - O pacote ggplot2
#----------------------------------

# O ggplot2 é um pacote para .blue[visualização de dados] disponível no R.
# O pacote foi desenvolvido por .blue[Hadley Wickham], inspirado pelo livro .blue[**The Grammar of graphics**] (A gramática dos gráficos) de .blue[Leland Wilkinson].
# A essência do ggplot2 é a construção de gráficos ".blue[camada por camada]".
# O ggplot2 possui .blue[vantagens] frente ao R-base:
  ## É altamente customizável;
  ## É mais elegante (bonito);
  ## É muito intuitivo, devido a filosofia de camadas.

# Instale o ggplot2

install.packages("ggplot2")   # Visualizar dados
install.packages("dplyr")     # Manipulação de dados
install.packages("ggpmisc")   # Adicionar equações e estatísticas
install.packages("patchwork") # Combinar gráficos ggplot
#install.packages("tidyverse")
# instala diversos pacotes, inclusive ggplot2

# Carrega pacotes
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(patchwork)

# Para tornar todas as funções do pacote disponíveis para uso
# na sessão corrente do R.

#--------------------------------------------
# 3 - Conjunto de dados - IF100%
#--------------------------------------------

data <- readr::read_csv(file="Slides/data/UPA07DVS.csv") %>%
  mutate(DAP = round(CAP/pi, 2),
         V = round(((DAP^2*pi)/40000)*HC*0.7, 4))

# Para fins didáticos, vamos usar uma amostra de 4 espécies
# florestais de um IF100% para construir os gráficos...

#--------------------------------------------
# Amostra 1 - 4 espécies
#--------------------------------------------

set.seed(1000)

data_sample <- data %>%
  select(Nome_Especie, DAP, CAP, HC, QF, Selecao, V) %>%
  filter(Nome_Especie %in% c('Acapu', 'Andiroba',
                             'Maçaranduba', 'Goiabão')) %>%
  group_by(Nome_Especie) %>%
  sample_n(50, replace = F)

#--------------------------------------------
# 4 - Sintaxe Básica - ggplot2
#--------------------------------------------

# A estrutura de dados deve ser um data.frame.
# Não admite matrizes, vetores, e outros!
# A função principal é ggplot().
# Nesta função os argumentos básicos são:
  ## **data**: argumento que recebe os dados no formato de
  ## data.frame.
  ## **mapping**: argumento que recebe informações das variáveis
  ## (x e y) a serem usadas na plotagem. Esses argumentos são
  ## passados dentro da função aes().

# Obs.: Se mapping não for especificado na função ggplot(),
# deve ser fornecido em cada camada adicionada ao gráfico.

# Existem 3 formas de invocar a função ggplot():

# 1) ggplot(df, aes(x, y, outras estéticas))

  ## Método é recomendado quando dados de **um único data.frame**
  ## e também as **mesmas estéticas** são usadas nas camadas
  ## adicionadas.

# 2) ggplot(df)

  ## Método é recomendado quando dados de **um único data.frame**
  ## é usado pelas camadas adicionadas, mas as **estéticas
  ## variam de uma camada para outra**.

# 3) ggplot()

  ## Método é recomendado quando **vários data.frames precisam
  ## ser usados para produzir diferentes camadas** no gráfico.
  ## É usual para gráficos complexos.

#--------------------------------------------
# 5 - Camada base - ggplot2
#--------------------------------------------

# A execução da função ggplot() (sem argumentos) ou com apenas
# o argumento data especificado ggplot(data = data_sample) gera
# simplesmente a base do gráfico (sem elementos estéticos e
# geométricos).

ggplot(data = data_sample)    # sem pipe

data_sample %>% ggplot()      # com pipe (%>%)

# É comum usar o operador pipe (%>%) para enviar o conjunto
# de dados para a função ggplot().

#-------------------------------------------
# 5.1 - Camada base - Especificando x e y

# - Adicionamos "x" = DAP e "y" = HC dentro da função aes()
# do argumento mapping.
# - Isso informar que as variáveis DAP e HC devem ser usadas
# nos eixos x e y, respectivamente, do gráfico.
# - Apesar disso, ainda não é possível visualizar um gráfico
# com elementos geométricos representativos dos dados.
# - Então, é necessário definir o **tipo de gráfico** que
# deseja-se construir.

ggplot(data = data_sample,
       mapping = aes(x = DAP, y = HC)) # 1ª camada

#-------------------------------------------
# 6 - Gráfico de pontos (dispersão)
#-------------------------------------------
# Útil para exibir a relação entre duas variáveis contínuas.

ggplot(data = data_sample,
       mapping = aes(x = DAP, y = V)) + # 1ª camada
  geom_point()                          # 2ª camada

#-----------------------------
# 2ª camada:
# - A função geom_point() foi usada para adicionar uma camada
# de pontos no gráfico. Tem-se um gráfico de dispersão.
# - Importante: Para adicionar camadas usa-se o operador + (adição).
#-----------------------------

# 6.1 - Gráfico de pontos - Adicione linha de regressão
#-----------------------------------------------------

ggplot(data = data_sample,
       mapping = aes(x = DAP, y = V)) +       # 1ª camada
  geom_point() +                              # 2ª camada
  geom_smooth(method='lm', formula=y~x, se=F) # 3ª camada

# 3ª camada:
# - A função geom_smooth() foi usada para adicionar uma linha
# de regressão linear (y~x = V~DAP).

# 6.2 - Gráfico de pontos - polinômio de 2 grau
#-----------------------------------------------------
ggplot(data = data_sample,
       mapping = aes(x = DAP, y = V)) +          # 1ª camada
  geom_point() +                                 # 2ª camada
  geom_smooth(method='lm', formula=y~x, se=F) +  # 3ª camada
  geom_smooth(method='lm', formula=y~poly(x,2),
              se=F, color = "red")               # 4ª camada

# 4ª camada:
# - A função geom_smooth() foi usada para adicionar outra
# linha de regressão, porém de um polinômio de segundo
# grau (y~x^2 = V~DAP^2).

# 6.3 - Gráfico de pontos - polinômio de 10 grau
#-----------------------------------------------------

ggplot(data = data_sample,
       mapping = aes(x = DAP, y = V)) +          # 1ª camada
  geom_point() +                                 # 2ª camada
  geom_smooth(method='lm', formula=y~x, se=F) +  # 3ª camada
  geom_smooth(method='lm', formula=y~poly(x,2),
              se=F, color = "red") +             # 4ª camada
  geom_smooth(method='lm', formula=y~poly(x,10),
              se=F, color = "green")             # 5ª camada

# 5ª camada:
# - A função geom_smooth() foi usada para adicionar outra
# linha de regressão, porém de um polinômio de grau 10
# (y~x^10 = V~DAP^10).

# 6.4 - Gráfico de pontos - Modifique a cor dos pontos
#-----------------------------------------------------
# A ideia é identificar os pontos pertencentes a cada
# espécie.

ggplot(data = data_sample,
       mapping = aes(x = DAP, y = V)) +           # 1ª camada
  geom_point(mapping = aes(colour=Nome_Especie))  # 2ª camada


# 2ª camada:
# A função geom_point() também possui argumentos para
# customização de atributos estéticos. (?geom_point).
  ## mapping: pode receber argumentos por meio da função aes():
  ### x, y, alpha, **colour**, fill, group, shape, size, stroke

# O argumento **colour** recebeu a coluna "Nome_Especie" e
# mapeou diferentes cores para as categorias: Acapu, Andiroba,
# Goiabão, Maçaranduba.

# 6.5 - Gráfico de pontos - Modifique a forma dos pontos
#-----------------------------------------------------

ggplot(data = data_sample,
       mapping = aes(x = DAP, y = V)) +           # 1ª camada
  geom_point(mapping = aes(colour=Nome_Especie,
                           shape=Selecao))        # 2ª camada

# 2ª camada:
# O argumento **shape** recebeu a coluna "Selecao" e mapeou
# diferentes formas para as categorias: Explorar e Remanescente.


# 6.6 - Gráfico de pontos - Tamanho proporcional
#---------------------------------------------------
# - **Bubblechart** (gráfico de bolhas): É um gráfico de
# dispersão, em que uma terceira dimensão é adicionada.

# Vamos usar apenas os dados da "Maçaranduba" para melhor
# visualização dos efeitos. Mas, para isso é necessário
# filtrar apenas as linhas de dados da espécie.

ggplot(data = data_sample %>%
         filter(Nome_Especie == 'Maçaranduba'),
       mapping = aes(x = DAP, y = V)) +           # 1ª camada
  geom_point(mapping = aes(colour = Selecao,
                           size = HC))            # 2ª camada

# Ou de outro modo...

data_sample %>%
  filter(Nome_Especie == 'Maçaranduba') %>%
  ggplot(mapping = aes(x = DAP, y = V)) +         # 1ª camada
  geom_point(mapping = aes(colour = Selecao,
                           size = HC))            # 2ª camada

# 2ª camada:

# - O argumento **size** (dentro de aes()) recebeu a coluna "HC"
# e mapeou tamanhos de pontos proporcionais aos valores de HC.
# - Portanto, a altura de cada árvore é representada pelo
# tamanho do círculo.

# 6.7 - Gráfico de pontos - Modifique tamanho dos pontos
#---------------------------------------------------

# Vamos usar apenas os dados de "Maçaranduba" e "Acapu" para
# melhor visualização dos efeitos. Mas, para isso é necessário
# filtrar apenas as linhas de dados da espécie.

ggplot(data = data_sample %>%
         filter(Nome_Especie %in% c('Acapu', 'Maçaranduba')),
       mapping = aes(x = DAP, y = V)) +           # 1ª camada
  geom_point(mapping = aes(colour = Nome_Especie,
                           shape = Selecao),
             size = 4)                            # 2ª camada

# Ou de outro modo...

data_sample %>%
  filter(Nome_Especie %in% c('Acapu', 'Maçaranduba')) %>%
  ggplot(mapping = aes(x = DAP, y = V)) +        # 1ª camada
  geom_point(mapping =
               aes(colour = Nome_Especie,
                           shape = Selecao
                   ),
             size = 4)                            # 2ª camada

# 2ª camada:

# - Isso é feito com argumento **size**, porém fora de aes().
# - Especifique um valor inteiro para o argumento.


# 6.8 - Gráfico de pontos - Personalize a forma e a cor dos pontos
#----------------------------------------------------------------------

ggplot(data = data_sample %>%
         filter(Nome_Especie %in% c('Andiroba', 'Maçaranduba')),
       mapping = aes(x = DAP, y = V)) +               # 1ª camada
  geom_point(mapping = aes(colour = Nome_Especie,
                           shape = Selecao),
             size = 4) +                              # 2ª camada
  scale_shape_manual(values = c(3, 5)) +              # 3ª camada
  scale_color_manual(values = c('#F1C40F','#DE3163')) # 4ª camada

# Ou de outro modo...

data_sample %>%
  filter(Nome_Especie %in% c('Andiroba', 'Maçaranduba')) %>%
  ggplot(mapping = aes(x = DAP, y = V)) +             # 1ª camada
  geom_point(mapping =
               aes(colour = Nome_Especie,
                           shape = Selecao
                   ),
             size = 4) +                              # 2ª camada
  scale_shape_manual(values = c(3, 5)) +              # 3ª camada
  scale_color_manual(values = c('#F1C40F','#DE3163')) # 4ª camada

# 3ª e 4ª camadas:

# - Use scale_shape_manual() e scale_color_manual() para
# definir a forma e cor desejada para os pontos (e muito mais),
# respectivamente.


# 6.9 - Gráfico de pontos - Modifique legendas
#---------------------------------------------

ggplot(data = data_sample %>%
         filter(Nome_Especie %in% c('Andiroba', 'Maçaranduba')),
       mapping = aes(x = DAP, y = V)) +                 # 1ª camada
  geom_point(mapping = aes(colour = Nome_Especie,
                           shape = Selecao),
             size = 4) +                                # 2ª camada
  scale_shape_manual(values = c(3, 5),
                     labels = c("Árvore para explorar",
                                "Árvore remanescente"),
                     name = "Seleção") +                # 3ª camada
  scale_color_manual(values = c('#999999','#E69F00'),
                     labels = c("Carapa guianensis",
                                "Manilkara elata"),
                     name = "Espécie")                  # 4ª camada

# Ou de outro modo...

data_sample %>%
  filter(Nome_Especie %in% c('Andiroba', 'Maçaranduba')) %>%
ggplot(mapping = aes(x = DAP, y = V)) +                 # 1ª camada
  geom_point(mapping = aes(colour = Nome_Especie,
                           shape = Selecao),
             size = 4) +                                # 2ª camada
  scale_shape_manual(values = c(3, 5),
                     labels = c("Árvore para explorar",
                                "Árvore remanescente"),
                     name = "Seleção") +                # 3ª camada
  scale_color_manual(values = c('#999999','#E69F00'),
                     labels = c("Carapa guianensis",
                                "Manilkara elata"),
                     name = "Espécie")                  # 4ª camada

# 3ª e 4ª camadas:

# - Use scale_shape_manual() e scale_color_manual() para
# modificar as legendas atribuídas a partir das variáveis
# mapeadas em "shape" e "color" de geom_point(), respectivamente.
# - Explore os argumentos "labels" e "name".


# 6.10 - Gráfico de pontos - Modifique títulos e escalas
#------------------------------------------------------

data_sample %>%
  filter(Nome_Especie %in% c('Andiroba', 'Maçaranduba')) %>%
  ggplot(mapping = aes(x = DAP, y = V)) +             # 1ª camada
  geom_point(mapping = aes(colour = Nome_Especie,
                           shape = Selecao),
             size = 4
             ) +                                       # 2ª camada
  scale_shape_manual(values = c(3, 5),
                     labels = c("Árvore para explorar",
                                "Árvore remanescente"),
                     name = "Seleção") +               # 3ª camada
  scale_color_manual(values = c('#999999','#E69F00'),
                     labels = c("Carapa guianensis",
                                "Manilkara elata"),
                     name = "Espécie") +                # 4ª camada
  scale_x_continuous(name = "Diâmetro (cm)",
                     limits = c(35, 115),
                     breaks = seq(35, 115, 10))         # 5ª camada


# 5ª camada:

# - Argumentos "name", "limits" e "breaks" na camada de
# scale_x_continuous().
# - Experimente: scale_y_continuous().


# 6.11 - Gráfico de pontos - Adicione títulos,
# subtítulos, tags...
#-----------------------------------------------------

data_sample %>%
  filter(Nome_Especie %in% c('Andiroba', 'Maçaranduba')) %>%
  ggplot(mapping = aes(x = DAP, y = V)) +             # 1ª camada
  geom_point(mapping = aes(colour = Nome_Especie,
                           shape = Selecao),
             size = 4) +                              # 2ª camada
  scale_shape_manual(values = c(3, 5),
                     labels = c("Árvore para explorar",
                                "Árvore remanescente"),
                     name = "Seleção") +               # 3ª camada
  scale_color_manual(values = c('#999999','#E69F00'),
                     labels = c("Carapa guianensis",
                                "Manilkara elata"),
                     name = "Espécie") +               # 4ª camada
  labs(title = "Relação Volume-Diâmetro",
       subtitle = "(IF100% - Amazônia)",
       tag = "A", x = "Diâmetro (cm)",
       y = "Volume (m³)")                              # 5ª camada

# 5ª camada:
# - Experimente a função labs() para adicionar títulos.


# 6.12 - Gráfico de pontos - Uma equação e suas estatísticas
#-----------------------------------------------------

# Uma maneira prática (não a única!) de adicionar um modelo
# ajustado (e suas estatísticas de ajuste) em um gráfico
# criado com ggplot2 é usar funções do pacote "ggpmisc".

# Vamos ajustar uma RL usando apenas dados da 'Maçaranduba'.

data_sample %>%
  filter(Nome_Especie %in% 'Maçaranduba') %>%
  ggplot(mapping = aes(x = DAP, y = V)) +       # 1ª camada
  geom_point() +                                # 2ª camada
  geom_smooth(method='lm', formula=y~x, se=F) + # 3ª camada
  ggpmisc::stat_poly_eq(formula = y~x,
                        eq.with.lhs = "italic(hat(V))~`=`~",
                        aes(label =
                              paste(..eq.label..,
                                    ..adj.rr.label..,
                                    ..AIC.label..,
                                    sep = "*plain(\",\")~")),
                        parse = TRUE)            # 4ª camada

# 4ª camada:
# - Use a função stat_poly_eq() do pacote ggpmisc para adicionar
#  informações de ajuste de um modelo linear.


# 6.13 - Gráfico de pontos - Uma equação e suas estatísticas
#-----------------------------------------------------

# Adicione um polinômio de grau 2...

data_sample %>%
  filter(Nome_Especie %in% 'Maçaranduba') %>%
  ggplot(mapping = aes(x = DAP, y = V)) +       # 1ª camada
  geom_point() +                                # 2ª camada
  geom_smooth(method='lm',
              formula=y~poly(x,2), se=F) +      # 3ª camada
  ggpmisc::stat_poly_eq(formula = y~poly(x,2),
                        eq.with.lhs = "italic(hat(V))~`=`~",
                        aes(label =
                              paste(..eq.label..,
                                    ..adj.rr.label..,
                                    ..AIC.label..,
                                    sep = "*plain(\",\")~")),
                        parse = TRUE)            # 4ª camada

# 4ª camada:
# - Use a função stat_poly_eq() do pacote **ggpmisc** para
# adicionar informações de ajuste de um modelo linear.
# - Use poly() no argumento formula para modificar o grau do
# polinômio.

# 6.14 - Gráfico de pontos - Duas equações e suas estatísticas
#-----------------------------------------------------

# Adicione duas equações no gráfico...

data_sample %>%
  filter(Nome_Especie %in% c('Andiroba', 'Maçaranduba')) %>%
  ggplot(mapping =
           aes(x = DAP, y = V,
               linetype = Nome_Especie,
               colour = Nome_Especie)) +           # 1ª camada
  geom_point() +                                   # 2ª camada
  geom_smooth(method='lm',
              formula=y~poly(x,2), se=F) +         # 3ª camada
  ggpmisc::stat_poly_eq(formula = y~poly(x,2),
                        eq.with.lhs = "italic(hat(V))~`=`~",
                        aes(label =
                              paste(..eq.label..,
                                    ..adj.rr.label..,
                                    sep = "*plain(\",\")~")),
                        parse = TRUE) +             # 4ª camada
  scale_color_manual(values = c("red", "black"))    # 5ª camada


#--------------------------------------------
# 7 - Gráfico de barras (ou colunas)
#--------------------------------------------

# - Existem 2 tipos de gráficos de barra:
# geom_bar() e geom_col()

# Vamos experimentar 2 abordagens usando geom_bar() para
# construir gráficos de barras com contagens por categorias...

# - Abordagem 1: A partir de um quadro com todos os dados.
# - Abordagem 2: A partir de um quadro com os dados resumidos.

# Para facilitar a compreensão, vamos usar apenas dados das
# 3 espécies mais frequentes.

# Filtra os dados de 3 espécies mais frequentes
data_sample2 <- data %>%
  filter(Nome_Especie %in% c("Acapu", "Casca seca", "Timborana"))

#----------------------------------------------------------
# Abordagem 1: Gráfico de contagem por categoria
# Criando gráfico a partir de um quadro com todos os dados.
#----------------------------------------------------------

data_sample2 %>%
  ggplot() +                           # 1ª camada
  geom_bar(aes(x = Nome_Especie),
           stat = "count")             # 2ª camada

# 2ª camada
# - Use stat = "count" na camada geom_bar() para fazer a
# contagem de casos por categoria em "x".
# - Neste caso, o argumento "y" é dispensado.


#----------------------------------------------------------
# Abordagem 2: Gráfico de contagem por categoria
# Criando gráfico a partir de um quadro com os dados resumidos.
#----------------------------------------------------------

# Neste caso, primeiro cria-se um quadro de contagens e depois
# usa-se os dados deste quadro para construir o gráfico...

# Uma tabela de contagem das 3 espécies mais frequentes

(data_top <- data %>%
  filter(Nome_Especie %in% c("Acapu", "Casca seca", "Timborana")) %>%
  group_by(Nome_Especie) %>%
  summarise(media_HC = mean(HC), n = n()))

# Obs.: Adicione **arrange(desc(n))** se desejar imprimir a
# tabela ordenada por "n".

# Cria o gráfico de contagem

data_top %>%
  ggplot() +                              # 1ª camada
  geom_bar(aes(x = Nome_Especie, y = n),
           stat = "identity")             # 2ª camada

# 2ª camada
# Use stat = "identity" na camada geom_bar(), indicando que
# os dados devem ser usados como estão.
# Neste caso, o argumento "y" com os valores de contagem
# precisa ser informado.

# Veja que dessa vez não mapeamos x e y na função ggplot().
# Você entenderá o motivo adiante!

#----------------------------------------------------------
# Abordagem 1: Gráfico de média por categoria
# Criando gráfico a partir de um quadro com todos os dados.
#----------------------------------------------------------

data_sample2 %>%
  ggplot() +                                # 1ª camada
  geom_bar(aes(x = Nome_Especie, y = HC),
           stat = "summary",
           fun = "mean")                    # 2ª camada

# 2ª camada
# - Use stat = "summary" e fun = "mean" na camada
# geom_bar().
# - Ao usar stat = "summary" é possível especificar a
# estatística de resumo no argumento "fun".
# - Experimente**: Use fun = "sum" e y = V, para obter
# o volume total das 3 espécies.


#----------------------------------------------------------
# Abordagem 2: Gráfico de média por categoria
# Criando gráfico a partir de um quadro com os dados resumidos.
#----------------------------------------------------------

data_top %>%
  ggplot() +                                      # 1ª camada
  geom_bar(aes(x = Nome_Especie, y = media_HC),
           stat = "identity")                     # 2ª camada

# 2ª camada
# - Use stat = "identity" na camada geom_bar(), indicando
# que os dados devem ser usados como estão. Fazendo isso,
# stat_identity() será invocada.
# Neste caso, o argumento "y" (Média) precisa ser informado.


# 7.1 - Gráfico de barras - Modificando cores das barras
#-----------------------------------------------------

data_top %>%
  ggplot() +                                   # 1ª camada
  geom_bar(mapping = aes(x = Nome_Especie,
                         y = n,
                         fill = Nome_Especie),
           stat = "identity", show.legend = F)  # 2ª camada

# 2ª camada:

# - Em geom_bar() pode-se adicionar elementos estéticos.
# - Por exemplo, o argumento "fill" recebeu a coluna
# "Nome_Especie" e mapeou diferentes cores para as categorias.

# 7.2 - Gráfico de barras - Reordenando as barras
#-----------------------------------------------------

data_top %>%
  mutate(Nome_Especie =
           forcats::fct_reorder(Nome_Especie, n, .desc = F)
  ) %>%
  ggplot() +                                        # 1ª camada
  geom_bar(mapping = aes(x = Nome_Especie,
                         y = n,
                         fill = Nome_Especie),
           stat = "identity", show.legend = F)      # 2ª camada

# - Use a função fct_reorder() do pacote **forcats** para
# reordenar os níveis de fator da variável "Nome_Especie".
# Repasse o comando dentro da função mutate() do **dplyr**.
# Faça: as.factor(data_top$Nome_Especie) e veja que os níveis
# de fator, por padrão, são ordenados por ordem alfabética.


# 7.3 - Gráfico de barras - Adicionando rótulos nas barras
#-----------------------------------------------------

data_top %>%
  mutate(Nome_Especie =
           forcats::fct_reorder(
             Nome_Especie, n, .desc = F)) %>%
  ggplot() +                                      # 1ª camada
  geom_bar(mapping = aes(
    x = Nome_Especie, y = n, fill = Nome_Especie),
    stat = "identity", show.legend = F) +         # 2ª camada
  geom_label(aes(x = Nome_Especie,
                 y = n/2,
                 label = n),
             size = 3)                            # 3ª camada

# 3ª camada:
# - Use a função geom_label() para reordenar adicionar rótulos
# com os valores de contagem por espécie a cada barra.
# - **Importante**: perceba que para geom_label() mapeou-se
# novos pares de valores (x e y), que indicam a posição de
# impressão dos rótulos no gráfico.
# - Este é o motivo para não especificar x e y em ggplot()!


# 7.4 - Gráfico de barras - Representando duas (ou mais)
# variáveis
#-----------------------------------------------------

data_sample %>%
  filter(Nome_Especie != "Acapu") %>%
  ggplot() +                                    # 1ª camada
  geom_bar(aes(x = Nome_Especie, fill = Selecao),
           position = "dodge",
           stat = "count")                      # 2ª camada

# 2ª camada:
# - O gráfico mostra a contagem de árvores remanescente e
# explorar por espécie.
# - Use "fill" para mapear diferentes cores para as categorias
# de "Selecao".
# - Use position = "dodge" para que as barras sejam
# posicionadas lado a lado. (Padrão é "stack" = empilhadas)


# 7.5 - Gráfico de barras - Represente duas (ou mais)
# variáveis quantitativas
#-----------------------------------------------------

data_sample %>%
  select(c(Nome_Especie, HC, DAP)) %>%
  tidyr::pivot_longer(cols = !Nome_Especie,
                      names_to = "Variavel",
                      values_to = "Valor") %>%    # pivot = "mudar" ou "girar"
  ggplot() +                                      # 1ª camada
  geom_bar(mapping = aes(x = Nome_Especie,
                         y = Valor,
                         fill = Variavel),
           position = "dodge",
           stat = "summary",
           fun = "mean")                          # 2ª camada

# - Inicialmente, transforme o quadro de dados para o formato
# longo. Use a função pivot_longer() do pacote **tidyr**.
# - Use "cols" para informar as colunas que devem sofrer
# pivotagem.
# - Use names_to para informar o **nome da coluna** a ser
# criada a partir dos **nomes das colunas pivotadas**.
# - Use "values_to" para informar o **nome da coluna** a ser
# criada a a partir dos **dados das células**.


#-----------------------------------------------------------
# 8 - Histogramas de frequências - Distribuição diamétrica
#-----------------------------------------------------------

data %>%
  ggplot() +                                 # 1ª camada
  geom_histogram(mapping = aes(x = DAP))     # 2ª camada

# 2ª camada:
# - Gráfico mostra a distribuição de frequência de uma única
# variável contínua (DAP).
# - Use geom_histogram() para criar histogramas.
# - É necessário apenas especificar para "x" ou "y" a
# variável contínua.


# 8.1 - Histogramas de frequências - Controlando a
# largura das barras
#-----------------------------------------------------

data %>%
  ggplot() +                                 # 1ª camada
  geom_histogram(mapping = aes(x = DAP),
                 binwidth = 10)              # 2ª camada

# 2ª camada:
# - Use "binwidth" para controlar a largura das barras.
# - Explore "binwidth" para encontrar a melhor representação
# dos dados.


#-----------------------------------------------------
# 9 - Polígono de frequências - Compare distribuições
# entre categorias
#-----------------------------------------------------

data_sample2 %>%
  ggplot() +                                 # 1ª camada
  geom_freqpoly(mapping = aes(x = DAP,
                              colour = Nome_Especie),
                binwidth = 10)              # 2ª camada

# 2ª camada:
# - Use geom_freqpoly() para criar polígonos de frequências.
# - Esse tipo de gráfico é recomendado quando deseja-se
# comparar a distribuição entre os níveis de uma variável
# categórica.

#-----------------------------------------------------------
# 10 - BoxPlots - Compare distribuições
#-----------------------------------------------------------

data_sample2 %>%
  ggplot() +                                 # 1ª camada
  geom_boxplot(aes(x = Nome_Especie,
                   y = DAP))                 # 2ª camada

# 2ª camada:

# - São adequados para comparar distribuições.
# - Use geom_boxplot() para criar BoxPlots.
# - Mapeie os valores de "x" e "y" na camada.
# - No exemplo, os BoxPlots dos DAPs de cada categoria de
# "Nome_Especie" são mapeados.
# - É possível visualizar um BoxPlot geral
# (sem considerar categorias) apenas modificando para "x" = 1.


# 10.1 - BoxPlots - Reordenando os BoxPlots
#-----------------------------------------------------------

data_sample2 %>%
  mutate(Nome_Especie =
           forcats::fct_reorder(
             Nome_Especie,
             DAP,
             .desc = T)
  ) %>%
  ggplot() +                                 # 1ª camada
  geom_boxplot(aes(x = Nome_Especie,
                   y = DAP))                 # 2ª camada


# 2ª camada:
# - Use fct_reorder() do pacote **forcats** para reordenar os
# níveis de fator da variável "Nome_Especie".
# - Repasse o comando dentro da função mutate() do **dplyr**.


# 10.2 - BoxPlots - Adicione símbolo para representar a média
#-----------------------------------------------------------

data_sample2 %>%
  ggplot(aes(x = Nome_Especie,
             y = DAP)) +             # 1ª camada
  geom_boxplot() +                   # 2ª camada
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 4,
               color = "red"
  )                                   # 3ª camada

# 2ª camada:
# - Use stat_summary() para adicionar pontos representativos
# da média para cada categoria de "Nome_Especie".
# - Use "fun" para informar a estatística desejada.


# 10.3 - BoxPlots - Represente vários grupos
#-----------------------------------------------------------

data_sample %>%
  ggplot(aes(x = Nome_Especie,
             y = DAP,
             color = Selecao)) +      # 1ª camada
  geom_boxplot()                      # 2ª camada

# 1ª camada:
# - Use "color" = Selecao para criar BoxPlots de cada categoria
# de "Selecao" para cada espécie.
# - Os BoxPlots das categorias serão diferenciados por cores.


# 10.4 - BoxPlots - Adicione pontos representativos dos dados
#-----------------------------------------------------------

data_sample %>%
  ggplot(aes(x = Nome_Especie,
             y = DAP)) +                        # 1ª camada
  geom_boxplot() +                              # 2ª camada
  geom_jitter(width = 0.1)                      # 3ª camada

# 3ª camada:
# - Use geom_jitter() para adicionar pontos representativos dos
# dados.
# - A geom_jitter() adiciona uma pequena variação aleatória à
# localização de cada ponto, evitando *overplotting*
# (dados sobrepostos).


# 10.5 - BoxPlots - Adicione cores aos pontos por categoria
#-----------------------------------------------------------

data_sample %>%
  ggplot(aes(x = Nome_Especie,
             y = DAP,
             color = Selecao)) +                # 1ª camada
  geom_boxplot(color="black",
               outlier.shape = NA) +            # 2ª camada
  geom_jitter(width = 0.1)                      # 3ª camada

# 3ª camada:
# - Use color = Selecao em ggplot() para mapear cores para os
# pontos representativos das categorias de "Selecao".
# - Use color = "black" em geom_boxplot.
# - Use outlier.shape = NA em geom_boxplot para ignorar os
# outliers.


#-------------------------------------------
# 11 - Facetas - Matriz de painéis (facet)
#-------------------------------------------

# - Primeiro, vamos resgatar um gráfico de pontos que
# fizemos anteriormente.
# - Neste gráfico, a estética de cor (color) foi usada para
# diferenciar as categorias da variável "Nome_Especie".
# - Mas, veja que existe uma razoável sobreposição de grupos.
# Isso dificulta uma clara e fácil separação destes.
# - **Alternativa**: Use facet_grid() ou facet_wrap() para
# diferenciar grupos.

data_sample %>%
  ggplot() +                    # 1ª camada
  geom_point(
    mapping = aes(
      x = DAP,
      y = V,
      color = Nome_Especie))    # 2ª camada

#-------------------------------------------------------
# 11.1 - Facetas - Represente categorias de uma varíavel
#-------------------------------------------------------

data_sample %>%
  ggplot(aes(x = DAP, y = V)) +       # 1ª camada
  geom_point() +                      # 2ª camada
  facet_grid(. ~ Nome_Especie)        # 3ª camada

# 3ª camada:
# - Use facet_grid() para diferenciar grupos de uma (ou mais)
# variáveis.
# - Forneça uma fórmula para facet_grid(), como:
# **vertical ~ horizontal**

# Experimente:
# - **Modifique**: facet_grid(Nome_Especie ~ .)
# - **Adicione**: geom_smooth()

data_sample %>%
  ggplot(aes(x = DAP, y = V)) +                # 1ª camada
  geom_point() +                               # 2ª camada
  facet_grid(. ~ Nome_Especie) +               # 3ª camada
  geom_smooth(method='lm', formula=y~x, se=F)  # 4ª camada

data_sample %>%
  ggplot(aes(x = DAP, y = V)) +                # 1ª camada
  geom_point() +                               # 2ª camada
  facet_grid(. ~ Nome_Especie) +               # 3ª camada
  geom_smooth(method='lm', color = "red",
              formula=y~poly(x,2), se=F)       # 4ª camada

data_sample %>%
  ggplot(aes(x = DAP, y = V)) +                # 1ª camada
  geom_point() +                               # 2ª camada
  facet_grid(. ~ Nome_Especie) +               # 3ª camada
  geom_smooth(method='lm', color = "red",
              formula=y~poly(x,2), se=F) +     # 4ª camada
  ggpmisc::stat_poly_eq(
    aes(label = paste(..eq.label.., sep = "~~~")),
               label.x.npc = "left",
               label.y.npc = .95,
               eq.with.lhs = "italic(hat(y))~`=`~",
               eq.x.rhs = "~italic(x)",
               formula = y~poly(x,2),
               parse = TRUE,
               size = 4) +
  ggpmisc::stat_poly_eq(
    aes(label = paste(..rr.label.., sep = "~~~")),
    label.x.npc = "left",
    label.y.npc = .9,
    formula = y~poly(x,2),
    parse = TRUE,
    size = 4)


#-------------------------------------------------------
# 11.2 - Facetas - Represente categorias de duas (ou mais)
# variáveis
#-------------------------------------------------------

data_sample %>%
  ggplot(aes(x = DAP, y = V)) +       # 1ª camada
  geom_point() +                      # 2ª camada
  facet_grid(Selecao ~ Nome_Especie)  # 3ª camada

# 3ª camada:
# - Adicione em facet_grid() mais uma variável para mapear.
# - Um matriz de subgráficos é criada com dados dos
# níveis/categorias das variáveis “Selecao” e “Nome_Especie”

# Experimente:
# - **Modifique**: facet_grid(Nome_Especie ~ Selecao)
# - **Adicione**: geom_smooth()

data_sample %>%
  ggplot(aes(x = DAP, y = V)) +                # 1ª camada
  geom_point() +                               # 2ª camada
  facet_grid(Selecao ~ Nome_Especie) +         # 3ª camada
  geom_smooth(method='lm', color = "red",
              formula=y~poly(x,2), se=F)       # 4ª camada

#-------------------------------------------------------
# 11.3 - Facetas - Escalas dos eixos livres
#-------------------------------------------------------

data_sample %>%
  ggplot(aes(x = DAP, y = V)) +       # 1ª camada
  geom_point() +                      # 2ª camada
  facet_grid(Selecao ~ Nome_Especie,
             scales="free")           # 3ª camada

# - Por padrão, facet_grid() usa scales="fixed".
# Isso significa que as escalas dos eixos (x e y)
# são constantes em todos os subgráficos.
# - Use scales="free" para permitir que as escalas
# sejam independentes, ou seja, possam variar para
# cada subgráfico.

# Experimente
# - **Modifique**: scales="free_x" e scales="free_y".
# O que acontece?


#----------------------------------------------------------
# 11.4 - Facetas - Modificando o texto do rótulo da faceta
#----------------------------------------------------------

labels1 <- c(Remanescente = "Rem.", Explorar = "Exp.")
labels2 <- c(Acapu = "Vouacapoua", Andiroba = "Carapa",
             `Goiabão` = "Pouteria", `Maçaranduba` = "Manilkara")

data_sample %>%
  ggplot(aes(x = DAP, y = V)) +                  # 1ª camada
  geom_point() +                                 # 2ª camada
  facet_grid(Selecao ~ Nome_Especie,
             scales = "free",
             labeller =
               labeller(Selecao = labels1,
                        Nome_Especie = labels2)) # 3ª camada

# 3ª camada:
# - Use labeller para modificar os rótulos das facetas.


#----------------------------------------------------------
# 12 - Salve um ggplot - Função ggsave()
#----------------------------------------------------------

# Um gráfico produzido com ggplot2 pode ser salvo em diferentes
# extensões e resoluções usando a função ggsave().
# Por padrão, o último gráfico produzido e exibido é salvo
# (last_plot).
# O gráfico é salvo no diretório corrente, se não houver
# especificação do diretório.
# É possível salvar em várias extensões, como: .png, .jpg,
# .tiff, .pdf

# Salva último gráfico exibido no diretório corrente
ggsave(filename = "facet.png")
ggsave(filename = "facet.pdf")

# Salva último gráfico exibido em outro diretório
ggsave("facet.png", path = "Slides/fig/part3")

# Salva especificando resolução (dpi)
ggsave("facet.png", path = "Slides/fig/part3",
       dpi = 600)

# Salva especificando resolução, largura e altura
ggsave("facet.png", path = "Slides/fig/part3",
       width = 8, height = 8, dpi = 600, units = "cm")

###########################################################
# Parte 2 - Combinação de gráficos ggplot com Patchwork
###########################################################

# - O pacote **ggplot2** não possui funções para combinar gráficos.
# - O pacote **patchwork** permite uma composição (simples e complexa)
# de gráficos.
# - O pacote é muito intuitivo, pois usa do auxílio de operadores
# matemáticos (+, /) para combinar gráficos, além de possuir funções específicas.
# - Existem outros pacotes empenhados nesta tarefa, por exemplo, gridExtra e cowplot.

# Primeiro, os gráficos devem ser construídos com ggplot e
# atribuídos a algum objeto no R...
# Vamos resgatar 4 gráficos elaborados anteriormente...

# Gráfico 1
# -----------------------
(g1 <- data_sample %>%
  filter(Nome_Especie %in% 'Maçaranduba') %>%
  ggplot(mapping = aes(x = DAP, y = V)) +       # 1ª camada
  geom_point() +                                # 2ª camada
  geom_smooth(method='lm', formula=y~x, se=F) + # 3ª camada
  ggpmisc::stat_poly_eq(formula = y~x,
                        eq.with.lhs = "italic(hat(V))~`=`~",
                        aes(label =
                              paste(..eq.label..,
                                    ..adj.rr.label..,
                                    sep = "*plain(\",\")~")),
                        parse = TRUE)+ # 4ª camada
   ggtitle('g1'))

# Gráfico 2
# -----------------------
(g2 <- data_top %>%
  mutate(Nome_Especie =
           forcats::fct_reorder(
             Nome_Especie, n, .desc = F)) %>%
  ggplot() +                                      # 1ª camada
  geom_bar(mapping = aes(
    x = Nome_Especie, y = n, fill = Nome_Especie),
    stat = "identity", show.legend = F) +         # 2ª camada
  geom_label(aes(x = Nome_Especie,
                 y = n/2,
                 label = n),
             size = 3)+
   ggtitle('g2'))

# Gráfico 3
# -----------------------
(g3 <- data_sample2 %>%
   ggplot() +                                 # 1ª camada
   geom_freqpoly(mapping = aes(x = DAP,
                               colour = Nome_Especie),
                 binwidth = 10) +             # 2ª camada
   theme(legend.position = c(.7,.5),
         legend.direction="vertical",
         legend.key.size = unit(0.2, "cm"),
         legend.key = element_rect(colour = NA, fill = NA),
         legend.title = element_blank())+
   ggtitle('g3'))

# Gráfico 4
# -----------------------

(g4 <- data_sample %>%
    filter(Nome_Especie != "Acapu") %>%
    ggplot(aes(x = Nome_Especie,
               y = DAP,
               color = Selecao)) +                # 1ª camada
    geom_boxplot(color="black",
                 outlier.shape = NA) +            # 2ª camada
    geom_jitter(width = 0.1) +                    # 3ª camada
    theme(legend.position = c(.3,.8),
          legend.direction = "vertical",
          legend.key.size = unit(0.2, "cm"),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.title = element_blank())+
    ggtitle('g4'))


# 1 - Uso básico - Operador adição (+)
#--------------------------------
# A abordagem mais simples é usar o operador + (adição)
# para combinar gráficos.

g1 + g2

# No Patchwork, por padrão, busca-se criar uma matriz quadrada,
# cujos subgráficos são alocados na ordem das linhas...

g1 + g2 + g3 + g4

# 1 - Uso básico - Operador divisão (/)
#--------------------------------
# O operador **/** pode ser usado para empilhar gráficos

g2/g4

g2/g4/g3

# 1 - Uso básico - Operador barra vertical (|)
#--------------------------------

# O operador **|** pode ser usado para dispor os gráficos lado
# a lado.

g2 | (g4/g3)

(g3/g1) | (g4/g2)

(g1 | g2 | g3)/g4

(g4 / g2 /g3) | g1

# 2 - Disposição e área ocupada pelos subgráficos - plot_layout()
#-----------------------------------

# Se desejar mais controle sobre a disposição e o espaço ocupado por
# cada subgráficos use a função plot_layout().
?plot_layout

# Especificando o número de linhas e a ordenação dos subgráficos
g1 + g2 + g3 + g4 + plot_layout(nrow = 3, byrow = F)

# Especificando uma largura relativa para os subgráficos
# No exemplo, a área destinada aos subgráficos da segunda coluna é o dobro
# da primeira coluna.
g2 + g1 + g3 + g4 + plot_layout(widths = c(1, 2))

# Especificando uma altura relativa para os subgráficos
g2 + g1 + g3 + g4 + plot_layout(heights = c(1, 2))

# Especificando uma largura e altura relativas para os subgráficos
g1 + g2 + g3 + g4 + plot_layout(widths = c(1, 2), heights = c(1, 2))

# Posição de legendas no painel
# A função plot_layout() possui o argumento "guide" para tratar da disposição
# das legendas no painel de subgráficos. Este argumento também ajuda a remover
# legendas duplicadas.

# Legendas posicionadas no lado direito

g3 + g4                               # Sem especificar "guides"

g3 + g4 +
  plot_layout(guides = 'collect')     # guides = 'collect'

# Removendo legendas duplicadas

g3 + g3

g3 + g3 +
  plot_layout(guides = 'collect')

# Modifica posição e direção da legenda

g3 + g1 + g3 + g4 +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom',
        legend.direction = "horizontal")


# 3 - Gráfico dentro de outro gráfico - inset_element()
#-----------------------------------
# A função inset_element() permite inserir um gráfico dentro de um
# um gráfico anteriormente especificado. A localização de inserção
# é dada por coordenadas das bordas esquerda, inferior, direita e superior.

g3 + inset_element(g2,
                   left = 0.4, bottom = 0.3,
                   right = 0.95, top = 0.9)


# 4 - Adicionando textos, títulos, tags... - plot_annotation()
#-----------------------------------
# - Use a função plot_annotation() para adicionar textos descritivos
# ao painel de gráficos.
# - No exemplo, são usados os argumentos: title, subtitle,
# caption e tag_levels (etiquetas de subgráficos).
# - Modifique tag_levels: '1', 'a', 'I', 'i'

g3 + g1 +
  plot_layout(guides = 'collect') +
  plot_annotation(
  title = 'Inventário Florestal 100%',
  subtitle = 'Explorando dados de algumas espécies',
  caption = 'Fonte: O Autor',
  tag_levels = 'A'
)

# Adicionando prefixo e sufixo na etiqueta (tag)
g2 + g1 + g3 + g4 +
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = 'Inventário Florestal 100%',
    subtitle = 'Explorando dados de algumas espécies',
    caption = 'Fonte: O Autor',
    tag_levels = 'A',
    tag_prefix = 'Fig. ',
    tag_suffix = ':'
  )

# Têm muito mais...:)

# Fim--------------------
