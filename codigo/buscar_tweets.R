library(twitteR)

# credenciales para acceder al API de twitter
consumer_key <- readLines('consumer_key.txt')
consumer_secret <- readLines('consumer_secret.txt')
access_token <- readLines('access_token.txt')
access_secret <- readLines('access_secret.txt')
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


buscar_verso <- function(verso,
                         fecha = '2022-05-05' # el lanzamiento de Un Verano Sin Ti
){

  #recoger tweets que contengan el verso
  tweets <- searchTwitter(searchString = verso,
                          n = 5,
                          since = fecha)

  if (length(tweets) == 0) {
    message('verso no encontrado: ', verso)
    tweet_verso <- NA_character_
  } else {
    tweets <- tweets |>
      twListToDF() |>
      data.table::data.table()

    # limpiar texto
    tweets[, text := tolower(text) |>
             stringr::str_remove('[:punct:]') |>
             stringi::stri_trans_general('Latin-ASCII')
    ]

    # elegir un tweet que solamente contenga el verso
    match_verso <- tweets[verso == text
                        ][order(-favoriteCount)
                        ][isRetweet == FALSE]

    # si no hay un match exacto, elegir el que tenga menos caracteres
    if(nrow(match_verso) == 0){
      message('no hay match perfecto: ', verso)
      match_verso <- tweets[nchar(text) == min(nchar(text))][1]
    }else{
      # si hay varios, elegir el que tenga mas likes
      match_verso <- match_verso[favoriteCount == max(favoriteCount)][1]
    }

    tweet_verso <- paste('https://twitter.com', match_verso$screenName, 'status', match_verso$id, sep = '/')

  }
}

agregar_tweets <- function(versos){

  i <- as.integer(readr::read_lines('proximo_verso.txt'))
  message('buscando verso: ', versos$verso[i])
  nuevo_verso <- versos[i][, tweet := buscar_verso(verso), by = verso]

  data.table::fwrite(x = nuevo_verso, file = 'tweets.csv', append = TRUE, sep = '|')

  readr::write_lines(x = i+1, file = 'proximo_verso.txt')
}



