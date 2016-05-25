library(ggplot2)
library(purrr)
library(rootSolve)

dfdx <- function(f) {
  g <- function(x) {}
  body(g) <- D(body(f), "x")
  g
}

equation <- function(f, prime = 0) {
  b <- body(f)
  prims <- rep("'", prime) %>% paste(collapse = "")
  paste0("f ", prims, " (x) = ", deparse(b))
}

take_derivs <- function(f) {
  funs <- list(f)
  while(body(f) %>% as.character() %>% map_lgl(~grepl("x", .x)) %>% any()) {
    f <- dfdx(f)
    funs <- c(funs, f)
  }
  funs
}

make_plot <- function(x, y) {
  ggplot(mapping = aes(x = x, y = y)) +
    geom_hline(yintercept = 0,
               color = "grey70",
               linetype = "dashed") +
    geom_vline(xintercept = 0,
               color = "grey70",
               linetype = "dashed") +
    coord_cartesian(range(x), range(y), FALSE) +
    theme_light(9) +
    theme(
      text = element_text(color = "grey50"),
      axis.text = element_text(color = "grey50"),
      panel.grid = element_blank(),
      axis.ticks = element_line(color = "grey50"),
      legend.text = element_text(size = rel(1.1)),
      legend.key = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(size = 1),
                                reverse = TRUE)) +
    labs(y = "f(x)", color = NULL)
}

geom_fun <- function(f, prim) {
  stat_function(fun = f, mapping = aes_(color = equation(f, prim)),
                size = 1.2, alpha = 0.5)
}

add_funs <- function(p, f) {
  list(p, f) %>% reduce(`%+%`)
}

f <- function(x) x^3 + 3 * x^2 - 10 * x - 15
funs <- f %>% take_derivs() %>% map2(seq_along(.) - 1, geom_fun)

x <- seq(-10, 10, length.out = 100)
y <- seq(-100, 100, length.out = 100)
p <- make_plot(x, y)
pp <- add_funs(p, funs)
pp

r <- multiroot(dfdx(f), c(-5, 5))

pp +
  geom_point(aes(x = r$root, y = r$f.root)) +
  geom_point(aes(x = r$root, y = f(r$root)))
