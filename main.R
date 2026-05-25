library(tidyverse)

all <- read_delim("All_lending.csv", delim = ";", show_col_types = FALSE,
                  locale = locale(decimal_mark = ".", grouping_mark = "", encoding = "UTF-8"))
fx  <- read_delim("FX_lending.csv",  delim = ";", show_col_types = FALSE,
                  locale = locale(decimal_mark = ".", grouping_mark = "", encoding = "UTF-8"))
dif <- read_delim("diferencial.csv", delim = ";", show_col_types = FALSE,
                  locale = locale(decimal_mark = ".", grouping_mark = "", encoding = "UTF-8")) |>
  select(1, 2) |>
  rename(date = 1, spread = 2) |>
  mutate(date = as.Date(date)) |>
  filter(!is.na(spread))

pivot_long <- function(df) {
  df |>
    rename(category = Ukazatel) |>
    mutate(category = str_trim(category)) |>
    pivot_longer(-category, names_to = "date", values_to = "value") |>
    mutate(date = as.Date(date))
}

all_long <- pivot_long(all)
fx_long  <- pivot_long(fx)

# Combine Domácnosti + NISH in All_lending to match the FX grouping
domacnosti_all <- all_long |>
  filter(category %in% c(
    "Domácnosti",
    "Neziskové instituce sloužící domácnostem"
  )) |>
  group_by(date) |>
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  mutate(category = "Domácnosti")

all_sel <- bind_rows(
  all_long |> filter(category %in% c(
    "Celkem", "Nefinanční podniky", "Finanční instituce", "Nerezidenti"
  )),
  domacnosti_all
)

fx_sel <- fx_long |>
  filter(category %in% c(
    "Celkem",
    "Nefinanční podniky",
    "Finanční instituce",
    "Domácnosti + Neziskové instituce sloužící domácnostem",
    "Nerezidenti"
  )) |>
  mutate(category = if_else(
    str_starts(category, "Domácnosti"), "Domácnosti", category
  ))

df <- inner_join(
  all_sel |> rename(total   = value),
  fx_sel  |> rename(foreign = value),
  by = c("category", "date")
) |>
  filter(!is.na(total), !is.na(foreign), total > 0) |>
  mutate(ratio = foreign / total)

sec_br <- seq(-2, 8, by = 2)

make_chart <- function(cat, width, height, base_size, large = FALSE, pri_limit = 1.0) {
  df_cat    <- filter(df, category == cat)
  scale_fac <- pri_limit / 10        # secondary range is always 10 (-2 to 8)
  offset    <- -(-2) * scale_fac     # maps -2 p.b. → 0%
  pri_br    <- seq(0, pri_limit, by = 0.1)
  dif_scaled <- mutate(dif, spread_scaled = spread * scale_fac + offset)

  p <- ggplot(df_cat, aes(x = date, y = ratio)) +
    geom_hline(yintercept = offset, linetype = "dotdash", color = "black", linewidth = 0.45) +
    geom_line(aes(color = "Podíl devizových úvěrů na celkovém objemu"), linewidth = 0.8) +
    geom_line(
      data = dif_scaled,
      aes(x = date, y = spread_scaled, color = "Úrokový diferenciál 3M PRIBOR – 3M EURIBOR"),
      linewidth = 0.7, linetype = "dashed"
    ) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(
      name   = "Podíl FX (v %)",
      breaks = pri_br,
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, pri_limit),
      expand = expansion(0),
      sec.axis = sec_axis(
        transform = ~ (. - offset) / scale_fac,
        name      = "Úrokový diferenciál (p. b.)",
        breaks    = sec_br,
        labels    = function(x) paste0(x, " p.b.")
      )
    ) +
    scale_color_manual(values = c(
      "Podíl devizových úvěrů na celkovém objemu" = "#153081",
      "Úrokový diferenciál 3M PRIBOR – 3M EURIBOR" = "#FF4B00"
    )) +
    labs(
      #title    = if (large) paste("Podíl FX úvěrů na celkovém objemu úvěrů") else cat,
      #subtitle = "ARAD (Sestava 1053)",
      x        = NULL,
      color    = NULL
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title          = element_text(face = "bold"),
      panel.grid.minor    = element_blank(),
      legend.position     = "bottom"#,
      #axis.title.y.right  = element_text(color = "#FF4B00"),
      #axis.text.y.right   = element_text(color = "#FF4B00")
    )

  if (!large) {
    p <- p +
      guides(color = guide_legend(nrow = 2)) +
      theme(
        legend.text     = element_text(size = rel(0.8)),
        legend.key.size = unit(0.4, "cm")
      )
  }

  filename <- paste0("fx_ratio_", str_replace_all(cat, "[^[:alnum:]]", "_"), ".pdf")
  ggsave(filename, plot = p, width = width, height = height, device = cairo_pdf)
  message("Saved: ", filename)
}

# Small charts: sized to fit 2 side-by-side on A4
small_cats <- c("Nefinanční podniky", "Finanční instituce", "Domácnosti", "Nerezidenti")
walk(small_cats, ~ make_chart(.x, width = 3.1, height = 2.8, base_size = 8))

# Celkem: full-width, own row
make_chart("Celkem", width = 6.5, height = 3.2, base_size = 9, large = TRUE, pri_limit = 0.5)
