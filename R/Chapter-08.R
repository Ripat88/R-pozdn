pow <- function(x, p=2) {
  power <- x ^ p
  return(power)
}
pow(3,3)

imt <- function(weight, height) weight / height ^ 2

w <- c(60,80,120)
h <- c(1.6, 1.7, 1.8)
imt(weight = w, height = h)

imt <- function(weight, height) {
  if (any(weight <= 0 | height <= 0)) stop("Индекс массы тела не может быть посчитан для отрицательных значений")
  if (any(height > 3)) warning("Рост в аргументе height больше 3: возможно, указан рост в сантиметрах, а не в метрах\n")
  weight / height ^ 2
}
imt(-78, 167)

sampdist <- replicate(1000, mean(rlnorm(30)))
hist(sampdist)