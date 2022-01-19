doses <- data.frame(
  case = c(
    "M4F1: M", "M4F1: F", "F4M1: F", "F4M1: M", "M6F2: M", "M6F2: F",
    "F6M2: F", "F6M2: M", "M17F2: M", "M17F2: F", "F17M2: F", "F17M2: M"
  ),
  lower = c(
    2.6, -1.32, 2.93, -1.13, 3.37,
    0.89, 4.16, 0.63, 7.82, 1.14, 7.99, 1.08
  ),
  estimate = c(
    3.37, 0.07, 3.79, 0.23, 4.63,
    1.52, 5.17, 1.17, 10.51, 1.4, 11.19, 1.34
  ),
  upper = c(
    4.13, 1.45, 4.65, 1.6, 5.89,
    2.15, 6.18, 1.71, 13.2, 1.66, 14.38, 1.6
  )
)

doses %>%
  dplyr::mutate(
    error = (upper - estimate) / 1.96,
    upper = estimate + 2 * error,
    lower = estimate - 2 * error,
    dplyr::across(
      c(lower, estimate, upper),
      .fns = biodosetools:::correct_negative_vals
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    case_single = factor(
      stringr::str_split(case, ":") %>% unlist() %>% .[[1]],
      levels = c("M4F1", "F4M1", "M6F2", "F6M2", "M17F2", "F17M2")
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = case_single, y = estimate, ymin = lower, ymax = upper) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(width = 0.2) +
  ggplot2::scale_y_continuous(limits = c(0, 20), breaks = 0:20) +
  ggplot2::theme_light()


round(results_hetero$est_yields$yield1, 3) %>% clipr::write_clip()
c(0.714, 1.210, 1.705)
#== c(0.957, 1.210, 1.462)))
round(results_hetero$est_yields$yield2, 3) %>% clipr::write_clip()
c(0, 0.010, 0.092)
#== c(0, 0.010, 0.052)))
round(results_hetero$est_doses$dose1, 3)   %>% clipr::write_clip()
c(3.295, 4.215, 5.134)
#== c(3.547, 4.215, 4.942)))
round(results_hetero$est_doses$dose2, 3)   %>% clipr::write_clip()
c(0, 0.241, 1.835)
#== c(0, 0.241, 0.795)))
round(results_hetero$est_frac$estimate, 3) %>% clipr::write_clip()
c(0.578, 0.422)
#== c(0.578, 0.422)))
round(results_hetero$est_frac$std_err, 3)  %>% clipr::write_clip()
c(0.092, 0.092)
#== c(0.092, 0.092)))
