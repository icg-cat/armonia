# tests feedback alejandro

# A. Raw Wave 1 UK
raw_w1_uk <- tibble::tibble(
  timestamp = (Sys.time()),
  email = paste0("user", 1:10, "_uk@mail.com"),
  Age = sample(20:60, 10),
  # Energy = sample(1:10, 10)
  `Diabetic Status` = sample(c("Yes", "No", "Pre-diabetic"), 10, replace = TRUE),
  # Diet = random_diet_sentences(n = 10, language = "en", seed = 42),
  Energy = paste(
    sample(c("Mostly", "Usually", "Currently", "Very", "To be honest,"), 10, TRUE),
    sample(c("high", "moderate", "low", "average", "drained", "intense"), 10, TRUE))
)

# B. Raw Wave 1 SP
raw_w1_sp <- tibble::tibble(
  `Marca temporal` = (Sys.time()),
  `Correo electrónico` = paste0("user", 1:8, "_sp@mail.com"),
  Edad = sample(20:60, 8),
  # Energía = sample(1:10, 8)
  `Tiene diabetes` = sample(c("Sí", "No"), 8, replace = TRUE)
  # Dieta = random_diet_sentences(n = 8, language = "es", seed = 123)
)


dict_init(data_list = list(UK = raw_w1_uk, SP = raw_w1_sp),
          save_path = "debug/260314_tests_alejandro.xlsx",
          match_by = "position")

dict_validate(path = "debug/260314_tests_alejandro.xlsx")



emails1 <- c("marilo.barranco@gmail.com", "marilocharo@gmail.com",  "fulanitadetal@gmail.com", "janedoe@gmail.com")
emails2 <- c("marilo.barranco@gmail.com", "MARILO.BARRANCO@gmail,com", "MARILOBARRANCO@gamil.com", "marilocharo@gmail.con", "marilo.charo@gmail.com", "fulanitadetal@gmail.com")

.check_email(emails2)

check_id_audit(
  emails1, emails2,
  "debug/260314_tests_alejandro.xlsx",
  case.sensitive = F, verbose = T)
