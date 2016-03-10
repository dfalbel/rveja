# busca

#' Define a url da busca
#'
#'
url_busca <- function(){
  "http://veja.abril.com.br/busca?qu="
}

#' Query
#'
#' Cria a query
#'
#' @param p palavras chave separadas por espaço
#' @param sd data de inicio da busca
#' @param ed data de fim da busca
#'
#' @note as datas devem ser passadas no formato dia-mes-ano
#'
#' @export
query <- function(p, sd = "", ed = ""){
  p <- stringr::str_split(p, " ") %>% unlist
  paste0(
    url_busca(),
    paste(p, collapse = "+")
  )
}

#' Quantidade de resultados
#'
#' @param p pagina recebida pelo httr::GET
#'
#' @return o número de resultados encontrados na busca
#'
qtd_resultados <- function(p){
  p %>% httr::content() %>%
    rvest::html_nodes(".resultsExib") %>%
    rvest::html_text() %>%
    stringr::str_extract(" [0-9]*\\)") %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric()
}

#' Baixar páginas da busca
#'
#' Baixa todas as páginas que contém as listas de resultados da sua busca.
#'
#' @note É importante baixar os dados html para se precisar rodar novamente o
#' código, você não ter que baixá-los novamente.
#'
#' @export
baixar_pag_busca <- function(q, dir, t = 1){

  p <- httr::GET(q)
  n <- qtd_resultados(p)

  paginas <- seq(from = 1, to = 1639, by = 25)
  message(paste(length(paginas), "páginas serão baixadas."))

  for(i in 1:length(paginas)){
    p <- paste0(q, "&sr=", paginas[i])
    fn <- paste0(dir, "pagina", i, ".html")
    httr::GET(p, write_disk(path = fn))
    Sys.sleep(t)
  }

  return(dir)
}

#' Tabelar página
#'
#' @param a arquivo html com uma página baixada
#'
#' @export
tabelar_pagina <- function(a){
  li <- xml2::read_html(a) %>%
    rvest::html_nodes(".search-results-list") %>%
    rvest::html_nodes("li")

  url <- li %>%
    rvest::html_nodes(".search-results-title") %>%
    rvest::html_node("a") %>%
    rvest::html_attr("href")

  titulo <- li %>%
    rvest::html_nodes(".search-results-title") %>%
    rvest::html_node("a") %>%
    rvest::html_text()

  trecho <- li %>%
    rvest::html_nodes(".content") %>%
    rvest::html_text()

  data.frame(
    url = url,
    titulo = titulo,
    trecho = trecho,
    stringsAsFactors = F
  )
}



#' Tabelar págians de busca
#'
#' @param dir diretório com os arquivos das páginas de buscas baixadas.
#'
#' @export
tabelar_busca <- function(dir){
  a <- list.files(dir, full.names = T)
  df <- plyr::ldply(a, tabelar_pagina)
  return(df)
}

