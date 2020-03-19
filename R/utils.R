#' Pipe
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Dispor Números em Formato Amigável
#'
#' @param x Número que será formatado
#'
#' @return Uma string com o número formatado
#' @export
#'
#' @examples
#' formatar_numero(4321) # "4,32 mil"
#' formatar_numero(1e9) # "1 bilhões"
formatar_numero <- function(x){
  stopifnot(is.numeric(x))
  ordem <- c(1, 1e3, 1e6, 1e9)
  ordem_nomes <- c("", "mil", "milhões", "bilhões")

  i <- suppressWarnings(max(which(abs(x) >= ordem)))
  if (is.infinite(i)) i <- 1
  num_formatado <- format(round(x/ordem[i], 2),
                          decimal.mark = ",",big.mark = ".")
  paste(num_formatado, ordem_nomes[i])
}

#' Dessazonalizar uma serie
#'
#' @param x Série para dessazonalizar
#' @param start Início da série
#' @param freq Frequência da série
#'
#' @return \code{x} sem sazonalidade
#' @export
desazonalizar <- function(x, start, freq) {
  stats::ts(x, start = start, frequency = freq) %>%
    seasonal::seas() %>%
    seasonal::final()
}

#' Deflacionar serie de valores em reais
#'
#' @param x Valores
#' @param ... Outros argumentos passados para os métodos
#' @param periodo Período de referência dos valores de \code{x}
#' @param base Período base dos valores finais
#' @param indice Índice usado para deflacionar
#'
#' @return Um vetor com valores deflacionados
#' @export
#' @name deflacionar
#'
#' @examples
#' serie_dbl <- c(100, 105)
#' datas <- seq(as.Date("2019-01-01"), by = "1 month", along.with = serie_dbl)
#' deflacionar(serie_dbl, datas, "2019-01-01")
deflacionar <- function(x, ...) {
  UseMethod("deflacionar")
}

#' @rdname deflacionar
#' @export
deflacionar.default <- function(x, periodo, base, indice = "ipca", ...) {
  indice <- "PRECOS12_IPCA12"
  ipca <- ipeadatar::ipeadata(indice) %>%
    dplyr::select(date, value)

  vlr_base <- ipca %>%
    dplyr::filter(date == base) %>%
    dplyr::pull(value) %>%
    `attributes<-`(c(value = NULL))

  df_deflator <- ipca %>%
    dplyr::mutate(deflator = value/vlr_base)

  tibble::tibble(
    date = periodo,
    valor = x
  ) %>%
    dplyr::left_join(df_deflator, "date") %>%
    dplyr::mutate(valor = valor / deflator) %>%
    dplyr::pull(valor) %>%
    `attributes<-`(c(value = NULL))
}


#' Calcular partipacao com base em quantidades
#'
#' @param x vetor de quantidades
#'
#' @return um vetor de participacoes
#' @export
#'
#' @examples
#' participacao(c(25, 25))
participacao <- function(x) {
  x / sum(x)
}


#' Calcular HHI
#'
#' @param x Um vetor de quantidades ou participacoes
#'
#' @return O indice HH (HHI)
#' @export
#'
#' @examples
#' hhi(c(10, 25))
hhi <- function(x) {
  sum(participacao(x) ^ 2)
}


#' Criar indice de base fixa
#'
#' @param x Valores
#' @param base Base para qual alterar o centro/início da série
#'
#' @return Um vetor
#' @name indice
#' @export
#'
#' @examples
#' indice(c(25, 30, 35))
indice <- function(x) {
  x * 100 / x[1]
}

#' @rdname indice
#' @export
indice_movel <- function(x) {
  difs <- c(NA, x[-length(x)])
  100 * x / difs
}

#' @rdname indice
#' @export
mudar_base <- function(x, base) {
  x * 100 / x[base]
}


