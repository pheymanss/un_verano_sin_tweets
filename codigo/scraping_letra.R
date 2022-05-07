library(rvest)
library(xml2)
library(stringr)
library(data.table)

# Este script nos permite recoger las letras directamente de una página web

# Las letras de las canciones se van a recopilar de musica.com
# vamos a recoger las urls de cada letra desde la url del album completo

# espera un periodo entre 0,2 y 0.5 segundos entre cada llamada a una url
# para evitar un exceso de requests
esperar_leer_html <- function(url, verbose=FALSE){
  if(verbose){message('Esperando para hacer el request')}
  Sys.sleep(runif(n = 1, min = 0.01, max = 0.2))
  return(read_html(url))
}


#' Obtener las urls de cada canción individualmente
#'
#' @param url_album la url de musica.com del album a estudiar
#'
#' @return Un vector de caracteres con las urls a las letras de cada canción.
#' @export
#'
#' @examples
#' obtener_urls_canciones('https://www.musica.com/letras.asp?letra=2572619#album')
obtener_urls_canciones <- function(url_album){
  url_album |>
    esperar_leer_html() |> #
    rvest::html_elements('main article div div div nav ol li a') |> # navegar hasta los nodos correctos
    rvest::html_attr('href') # extraer las urls
}


#' Obtener la letra de una canción de Musica.com
#'
#' @param url_cancion
#'
#' @return Un vector de caracteres con la letra de la canción del url correspondiente
#' @export
#'
#' @examples
obtener_letra <- function(url_cancion){
  letra <-
    esperar_leer_html(url_cancion) |>
    rvest::html_nodes(xpath = "//*[@id='letra']") |> # navegar al nodo correcto
    rvest::html_nodes(xpath = "//*[@id='letra']") |> # una vez mas
    rvest::html_elements('p')

  # hack truculento para conservar los separadores cuando se recoja el texto
  xml2::xml_find_all(letra, ".//br") |> xml2::xml_add_sibling("p", "\n")
  xml2::xml_find_all(letra, ".//br") |> xml2::xml_remove()

  letra <- letra |>
    #recoger todo el texto de cada nodo:
    rvest::html_text() |>
    # eliminar los nodos que sean sólo espacios:
    purrr::discard(~.x == '\n') |>
    # separar las estrofas en versos:
    stringr::str_split(pattern = '\n') |>
    # aplanar: (no nos interesan las estrofas)
    unlist() |>
    # eliminar las lineas vacias:
    purrr::discard( ~.x == '') |>
    # eliminar la puntuacion
    stringr::str_remove_all('[:punct:]') |>
    # pasar todo a minusculas
    tolower()|>
    # reemplazar caracteres en espanol (para homogenizar):
    stringi::stri_trans_general('Latin-ASCII')
  return(letra)
}
