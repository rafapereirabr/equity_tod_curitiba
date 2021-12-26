
library(janitor)
library(data.table)

df <- fread('./data-raw/cb_2016/deslocamento_arq._.csv.csv')
head(df)

df <- janitor::clean_names(df)
head(df)

###### tempos de viagem -------------------------------

# select intra cur, only public transport trips
df2 <- subset(df, municipio_de_destino=='CURITIBA' & municipio_origem=='CURITIBA')
df2 <- subset(df2, tipo_meio=='COLETIVO')


df2[, atividade := fcase(tipo_destino %like% 'saúde', 'saude',
                         tipo_destino %like% 'Trabalho', 'trabalho')]
table(df2$atividade)
table(df2$tipo_destino)

# average travel time
setDT(df2)
# r <- df2[tipo_origem=='ResidÃªncia' , .(avg_travel_time = mean(tempo)), by=atividade]
a <- df2[ , .(avg_travel_time = mean(tempo)), by=atividade]
head(a)
# ddd <- merge(a,r, by='atividade')



########### divisao modal -------------------------       
# nao tem info especifica sobre BRT

# select intra cur, only public transport trips
df3 <- subset(df, municipio_de_destino=='CURITIBA' & municipio_origem=='CURITIBA')

table(df3$tipo_meio)

11601 / sum(11601          ,25033          ,12038            ,238)

in Curitiba according to the latest 2016 household travel survey (IPPUC, 2017),
23.7% transit


table(df3$meio)
