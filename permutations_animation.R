library(tidyverse)
library(ap)
m28 <- readRDS("013_moving_28.rds")
usf_accent <- c("#DBE442","#9CCB3B","#009374","#29AFCE","#80B0A6","#7396A0","#006484","#466069")

left_date = "2017-10-01"
right_date = "2017-12-15"

jitter <- function(df, var) {
  var <- enquo(var)
  jit <- runif(length(df[[rlang::quo_text(var)]]), -0.125, 0.125)
  jit[which(df$iter == 0)] <- 0
  mutate(df, end = end + jit)
}

filter_and_jitter <- function(df, date) {
  df %>%
    filter(end == lazydate(date)) %>%
    mutate(end = as.integer(strftime(end, "%j"))) %>%
    jitter(end)
}

plot_iters <- function(df, i, lab.x = "Normal", lab.y = "KLD", add_boxplot = FALSE) {
  xmin <- min(df$end)
  xmax <- max(df$end)
  df <- filter(df, iter >= 1000 - i)
  g <- ggplot(df) +
    aes(y = dist, x = end, group = end)
  if (add_boxplot) g <- g + geom_boxplot()
  g +
    geom_point(data = filter(df, iter != 0), alpha = 0.25, color = usf_accent[6], shape = 16) +
    geom_point(data = filter(df, iter == 0), color = '#BB1847', size = 4, shape = 16) +
    theme_minimal(14, "Roboto") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    labs(x = lab.x, y = lab.y) +
    scale_x_continuous(limits = c(xmin - 0.15, xmax + 0.15),
                       expand = c(0,0), position = "top") +
    ylim(.25, .4)
}

set.seed(42)
m28_left <- filter_and_jitter(m28, left_date)
m28_right <- filter_and_jitter(m28, right_date)

single_plot <- function(i, ...) {
  cowplot::plot_grid(
    plot_iters(m28_left, i, "Normal", ...),
    plot_iters(m28_right, i, "Abnormal", ...),
    ncol = 2,
    align = 'h'
  )
}

# library(animation)
# ani.options(ani.height = 750, ani.width = 1000, interval = 0.05, imgdir = "ani", loop = 1)
# saveGIF(for (i in 0:1000) print(single_plot(i)),
#         movie.name = "permutations.gif")


library(parallel)
tmpdir <- tempfile("", ".")
dir.create(tmpdir)
# mclapply(seq(0, 1000, 10), function(i) {
#   gg <- single_plot(i)
#   ggsave(sprintf("%s/permutation_%04d.png", tmpdir, i), gg, height = 7.5/2, width = 10/2)
# }, mc.cores = 4)
ix <- seq(0, 1000, 5)
p <- progress_estimated(length(ix))
cat("Writing to:", tmpdir, "\n")
for(i in ix) {
  gg <- single_plot(i)
  ggsave(sprintf("%s/permutation_%04d.png", tmpdir, i), gg, height = 7.5/2, width = 10/2)
  p$tick()$print()
}
p$stop()$print()
cat("\nSaved to:", tmpdir)
