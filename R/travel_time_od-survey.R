
library(janitor)
library(data.table)

df <- fread('./data-raw/cb_2016/deslocamento_arq._.csv.csv')
head(df)

df <- janitor::clean_names(df)
head(df)

# select intra cur, only public transport trips
df2 <- subset(df, municipio_de_destino=='CURITIBA' & municipio_origem=='CURITIBA')
df2 <- subset(df2, tipo_meio=='COLETIVO')


df2[, atividade := fcase(tipo_destino %like% 'saúde', 'saude',
                         tipo_destino %like% 'Trabalho', 'trabalho')]
table(df2$atividade)
table(df2$tipo_destino)

# average travel time
setDT(df2)
# r <- df2[tipo_origem=='Residência' , .(avg_travel_time = mean(tempo)), by=atividade]
a <- df2[ , .(avg_travel_time = mean(tempo)), by=atividade]
head(a)
# ddd <- merge(a,r, by='atividade')
       

