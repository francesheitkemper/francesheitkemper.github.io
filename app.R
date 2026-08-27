library(shiny)
library(tidyverse)
library(bslib)

gold <- readr::read_csv(
 'https://raw.githubusercontent.com/datasets/gold-prices/refs/heads/main/data/monthly-processed.csv') |>
    rename(`Gold Price` = Price)

oil <- readr::read_csv('https://raw.githubusercontent.com/datasets/oil-prices/refs/heads/main/data/wti-monthly.csv') |>
    rename(`Oil Price` = Price)

nat_gas <- readr::read_csv('https://raw.githubusercontent.com/datasets/natural-gas/refs/heads/main/data/monthly-processed.csv') |>
    rename(`Nautral Gas Price` = Price)

vix <- readr::read_csv('https://raw.githubusercontent.com/datasets/finance-vix/refs/heads/main/data/vix-monthly.csv') |>
    rename(`Volatility Index` = Close)  # volatility index

bond <- readr::read_csv('https://raw.githubusercontent.com/datasets/bond-yields-us-10y/refs/heads/main/data/monthly.csv') |>
    rename(`Bond Rate` = Rate) # bond yields us 10y

temp <- readr::read_csv('https://raw.githubusercontent.com/datasets/global-temp/refs/heads/main/data/monthly.csv') |>
    mutate(Date = lubridate::as_date(paste0(Year, "-01"))) |>
    rename(`Tempature Difference` = Mean)|>
    select(-Year, -Source)

sp <- readr::read_csv('https://raw.githubusercontent.com/datasets/s-and-p-500/refs/heads/main/data/data.csv') |>
    select(Date, SP500)

full_data <- gold |>
    full_join(oil)|>
    full_join(nat_gas)|>
    full_join(vix)|>
    full_join(bond)|>
    full_join(bond)|>
    full_join(temp)|>
    full_join(sp)

y_choices <- setdiff(names(full_data), "Date")

plot_ts <- function(variable, log = TRUE, date_range) {
  p <- full_data |>
    select(Date, .data[[variable]]) |>
    filter(between(Date, as.Date(date_range[1]), as.Date(date_range[2]))) |>
    drop_na() |>
    ggplot(aes(x = Date, y = .data[[variable]])) +
    geom_line(color = "pink") +
    theme_minimal() +
    labs(x = "date", y = variable)

  if (log) {
    p <- p + scale_y_log10()
  }
  p
}

ui <- page_sidebar(
  title = "Market Indicators Explorer",
  sidebar = sidebar(
    selectInput("rainbow", "Variable to plot:", choices = y_choices),
    checkboxInput("unicorn", "Log scale (y-axis)", value = TRUE),
    dateRangeInput("butterfly",
      label = "Date Range", 
      start = "1833-01-01", 
      end = "2026-08-25",
      min = "1833-01-01", 
      max = "2026-08-25",
      format = "yyyy-mm-dd"
  )),
  plotOutput("the_plot")
)

server <- function(input, output) {
  output$the_plot <- renderPlot({
    plot_ts(input$rainbow, log = input$unicorn, date_range = input$butterfly)
  })
}

shinyApp(ui, server)