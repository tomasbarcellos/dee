context("Numeros")

test_that("Formatar numeros", {
  expect_equal(formatar_numero(3000), "3 mil")
  expect_equal(formatar_numero(3e6), "3 milhões")
  expect_equal(formatar_numero(3.4 * 1000^3), "3,4 bilhões")
})

context("Indices")

test_that("indices", {
  expect_equal(indice(c(1, 2, 3)), c(100, 200, 300))
  expect_equal(indice(c(100, 98, 97)), c(100, 98, 97))

  expect_equal(valores <- indice(c(30, 33, 42)), c(100, 110, 140))
  expect_equal(indice(valores), valores)

  expect_equal(
    mudar_base(valores, 3), c(100*100/140, 100*110/140, 100)
  )

  expect_equal(indice_movel(c(100, 110, 121)),  c(NA, 110, 110))
})

context("HHI")

test_that("Participação e concentração", {
  expect_equal(participacao(c(100, 100)), c(0.5, 0.5))
  expect_equal(part <- participacao(c(100, 300)), c(0.25, 0.75))
  expect_equal(participacao(part), part)

  expect_equal(hhi(c(100, 100)), 0.5)

  quantidades <- c(100, 80, 75)
  expect_equal(hhi(participacao(c(quantidades))), hhi(quantidades))
})

context("Deflacionar")

test_that("Deflacionar", {
  serie_dbl <- c(100, 105)
  datas <- seq(as.Date("2019-01-01"), by = "1 month", along.with = serie_dbl)

  serie_ts <- ts(serie_dbl, start = c(2019, 1), frequency = 12)

  expect_equal(deflacionar(serie_dbl, datas, "2019-01-01"), c(100, 104.55049))

  # Funciona com indices diferentes
  expect_silent(deflacionar(serie_dbl, datas, "2019-01-01", "inpc"))
})
