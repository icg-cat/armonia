# compara
A <- "R/dict_init.R"
B <- "R/dict_init copy.R"

diffr::diffr(
  A, B,
  contextSize = 0,
  minJumpSize = 500)
