# Importar banco
db_instrument <- readxl::read_excel("data-raw/df_instrument.xlsx")

# salvar para uso
usethis::use_data(
  db_instrument,
  overwrite = TRUE
)
