# Maynard Smith, John. Evolutionary Genetics (2nd ed.)
# Chapter 1 Computer Project: METHINKSITISAWEASEL

library(purrr)

string_distance <- \(s, target) {
  # s:  string (or string vector)
  
  s <- strsplit(s, "")
  target <- strsplit(target, "")[[1]]
  
  s |> map_dbl(\(x) mean(x != target))
  
}

mutate_string <- \(s, prob) {
  # s:  character scalar
  
  s = strsplit(s, "")[[1]]
  for (i in seq_along(s)) {
    if (runif(1) < prob) {
      s[i] = mutate_letter(s[i])
    }
  }
  paste(s, collapse="")
}

mutate_letter <- function(x) {
  new_letters = setdiff(LETTERS, x)
  idx <- sample.int(length(new_letters), 1)
  new_letters[idx]
}

run_simulation <- function(target = "METHINKSITISAWEASEL", M=1000, N=10, p=0.01) {
  # M: number of generations
  # N: population size
  # p: mutation rate
  
  # generate initial population
  population = 
    rep(".", nchar(target)) |> 
    paste(collapse = "") |> 
    rep(N) |> 
    map_chr(mutate_string, prob = 1)
  
  fitness_values = list()
  for (i in 1:M) {
    fitness = 1 - string_distance(population, target)
    fitness_values[[i]] = max(fitness)
    new_parent = population[which.max(fitness)]
    population = rep(new_parent, N) |> map_chr(mutate_string, prob = p)
    checkpoints[[i]] = population
  }
  
  unlist(fitness_values)
  
}

x = sapply(1:100, \(x) run_simulation())
matplot(x, type="l", col=rgb(0,0,0,0.3), lty=1)

xx = c(0.25, 0.5, 0.75, 1) |> sapply(\(u) apply(x, 1, \(v) mean(v >= u)))
matplot(xx, type="l", lty=1)
