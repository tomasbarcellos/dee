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
