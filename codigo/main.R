# header bonito
source('codigo/scraping_letra.R')
source('codigo/buscar_tweets.R')

urls_canciones <- obtener_urls_canciones('https://www.musica.com/letras.asp?letra=2572619#album')

un_verano <- data.table::data.table(cancion = seq_along(urls_canciones),
                                    urls = urls_canciones
                                    )[, .(verso = obtener_letra(urls)), by = cancion
                                    ][, n_verso = 1:.N]

# dado que se llega rápidamente al límite del API, en vez de correr una sola
# llamada vamos a tener que ir recorriendo las letras con un contador persistente
readr::write_lines(x = 1, file = 'proximo_verso.txt')

# que correrá siempre que no haya terminado de correr
while(as.integer(readr::read_lines('proximo_verso.txt')) <= nrow(un_verano)){
  agregar_tweets(un_verano)
}

# tasa de cobertura
tweets <- data.table::fread('tweets.csv')
sum(nrow(tweets[tweet=='']))/nrow(tweets)
