## code to prepare `fig.mac` dataset goes here

fig.mac <- data.frame(wd.cm = 25.6,
                      ht.cm = 16,
                      cm.to.inch = 0.393700787) %>%
  dplyr::mutate(wd = wd.cm * cm.to.inch,
                ht = ht.cm * cm.to.inch)

usethis::use_data(fig.mac, overwrite = TRUE)
