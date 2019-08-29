tabulate_dataset <- function(x, ...){
  UseMethod("tabulate_dataset", x)
}


tabulate_dataset.apde_hys <- function(x, ...){
 x * unlist(...)
}

tabulate_dataset.apde_birth <- function(x){
  x /2
}


tabulate_dataset(structure(10, class = c('numeric', 'apde_hys')), 10)
tabulate_dataset(structure(10, class = c('numeric', 'apde_birth')))

