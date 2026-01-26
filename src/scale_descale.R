scale_by <- function(x, center, scale) {
  (x - center) / scale
}

descale_by <- function(x, center, scale) {
  x * scale + center
}