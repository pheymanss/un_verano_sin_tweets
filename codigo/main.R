# header bonito
source('codigo/scraping_letra.R')
source('codigo/buscar_tweets.R')

urls_canciones <- obtener_urls_canciones('https://www.musica.com/letras.asp?letra=2572619#album')

un_verano <- data.table::data.table(cancion = seq_along(urls_canciones),
                                    urls = urls_canciones
                                    )[, .(verso = obtener_letra(urls)), by = cancion
                                    ][, n_verso = 1:.N]




un_verano <- data.table::fread('un_verano.csv')
while(as.integer(readr::read_lines('proximo_verso.txt')) <= 1238){
  agregar_tweets(un_verano)
}


tweets <- data.table::fread('tweets.csv')
sum(is.na(tweets$tweet))/nrow(tweets)
