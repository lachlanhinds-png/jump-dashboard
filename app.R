### Beach Volleyball Jump Time Dashboard ###
### Pulls from Catapult Connect API v6 and shows ONE athlete at a time: their ###
### average and max jump time over time, as a line chart, with a table below. ###
### Jump data is stored permanently in a Supabase (Postgres) database, so     ###
### opening the app shows everything already pulled with NO API call - "Fetch ###
### latest data" only needs clicking when you actually want new data added.   ###
###
### SET UP (one-time):
### 1. install.packages(c("shiny","bslib","plotly","DT","httr","jsonlite","dplyr","tidyr","DBI","RPostgres"))
### 2. Put your Catapult token AND your Supabase DB credentials in .Renviron
###    (never in this file):
###      usethis::edit_r_environ()   # or just open ~/.Renviron in a text editor
###    add these lines (get the Supabase values from Project Settings > Database):
###      CATAPULT_TOKEN=your_token_here
###      SUPABASE_HOST=db.xxxxxxxxxxxx.supabase.co
###      SUPABASE_PORT=5432
###      SUPABASE_DB=postgres
###      SUPABASE_USER=postgres
###      SUPABASE_PASSWORD=your_db_password_here
###    save, then RESTART R (Session > Restart R in RStudio) so it's picked up.
### 3. In Supabase's SQL Editor, run the CREATE TABLE statement once - see the
###    bottom of this comment block, or ask Claude for it again if you lost it.
### 4. Open this file in RStudio and click "Run App".
###
### CREATE TABLE jumps (
###   activity_id text not null,
###   activity_name text,
###   athlete_id text not null,
###   athlete_name text,
###   start_time bigint not null,
###   end_time bigint,
###   jump_attribute double precision,
###   basketball_load double precision,
###   period_name text,
###   fetched_at timestamptz default now(),
###   unique (activity_id, athlete_id, start_time, end_time)
### );

list.of.packages <- c("shiny", "bslib", "plotly", "DT", "httr", "jsonlite", "dplyr", "tidyr", "DBI", "RPostgres")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(DBI)
library(RPostgres)

BASE_URL <- "https://connect-au.catapultsports.com/api/v6"

DB_COLUMNS <- c("activity_id", "activity_name", "athlete_id", "athlete_name",
                "start_time", "end_time", "jump_attribute", "basketball_load", "period_name")

get_db_con <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("SUPABASE_HOST"),
    port = as.integer(Sys.getenv("SUPABASE_PORT", "5432")),
    dbname = Sys.getenv("SUPABASE_DB", "postgres"),
    user = Sys.getenv("SUPABASE_USER", "postgres"),
    password = Sys.getenv("SUPABASE_PASSWORD")
  )
}

# Reads everything already stored - this is what makes opening the app not
# require an API call. Returns NULL (not an error) if the DB isn't reachable
# or isn't configured yet, so the rest of the app can degrade gracefully.
load_stored_data <- function() {
  if (!nzchar(Sys.getenv("SUPABASE_HOST"))) return(NULL)
  con <- tryCatch(get_db_con(), error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(DBI::dbDisconnect(con))
  d <- tryCatch(DBI::dbGetQuery(con, "SELECT * FROM jumps"), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d$start_time <- as.numeric(d$start_time)
  d$end_time <- as.numeric(d$end_time)
  as_tibble(d)
}

# Adds newly-fetched rows to the permanent store. Safe to call repeatedly
# with overlapping date ranges - the unique constraint on
# (activity_id, athlete_id, start_time, end_time) means re-fetching the same
# jump twice never creates a duplicate row.
upsert_jumps <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(invisible(0))
  for (col in setdiff(DB_COLUMNS, colnames(df))) df[[col]] <- NA
  df <- df[, DB_COLUMNS, drop = FALSE]

  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "jumps_staging", df, temporary = TRUE, overwrite = TRUE)
  DBI::dbExecute(con, paste0(
    "INSERT INTO jumps (", paste(DB_COLUMNS, collapse = ", "), ") ",
    "SELECT ", paste(DB_COLUMNS, collapse = ", "), " FROM jumps_staging ",
    "ON CONFLICT (activity_id, athlete_id, start_time, end_time) DO NOTHING"
  ))
  invisible(nrow(df))
}

# Catapult doesn't have a dedicated volleyball jump classifier - "basketball" is
# the closest jump-detection model for indoor/court jump sports. Confirm with
# your Catapult rep this is right for your account, or swap in "ima_jump".
JUMP_EVENT_TYPES <- "basketball"
JUMP_TIME_COL <- "jump_attribute"  # raw flight-time field, in centiseconds

# ---------- API helpers ----------

catapult_get <- function(url, token, query = list()) {
  response <- VERB("GET", url, query = query,
                    add_headers("authorization" = paste("Bearer", token)),
                    content_type("application/octet-stream"),
                    accept("application/json"))
  if (httr::status_code(response) >= 300) {
    stop(paste0("Request failed (", httr::status_code(response), "): ", url,
                " - ", rawToChar(response$content)))
  }
  fromJSON(rawToChar(response$content))
}

# nrow() is only well-behaved on a real data.frame - depending on the account,
# Catapult can return a shape jsonlite doesn't auto-simplify into one, and
# nrow() on that can come back NULL/NA instead of a clean 0. This never does.
safe_nrow <- function(x) {
  if (is.null(x)) return(0L)
  if (is.data.frame(x)) return(nrow(x))
  if (is.list(x)) return(length(x))
  length(x)
}

fetch_jump_data <- function(token, start_date, end_date, on_progress = function(msg, amount = 0) NULL) {
  on_progress("Fetching activity list...", 0.05)
  activities <- catapult_get(paste0(BASE_URL, "/activities"), token)
  if (safe_nrow(activities) == 0) stop("No activities returned - check your token and region.")

  activities <- activities %>%
    mutate(date = as.Date(as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC"))) %>%
    filter(date >= as.Date(start_date), date <= as.Date(end_date))

  if (safe_nrow(activities) == 0) stop("No activities in that date range.")

  all_data <- tibble()
  n <- nrow(activities)

  for (j in seq_len(n)) {
    activity_id <- activities$id[j]
    activity_name <- activities$name[j]
    on_progress(paste0("Activity ", j, "/", n, ": ", activity_name), 0.05 + 0.9 * (j - 1) / n)

    athletes <- tryCatch(
      catapult_get(paste0(BASE_URL, "/activities/", activity_id, "/athletes"), token),
      error = function(e) NULL
    )
    if (!is.null(athletes) && !is.data.frame(athletes)) {
      athletes <- tryCatch(as_tibble(athletes), error = function(e) NULL)
    }
    if (safe_nrow(athletes) == 0) next

    # Periods (e.g. Set 1/Set 2/Set 3) - fetched once per activity, not per
    # athlete, since they're a property of the activity itself. Each jump
    # event's own start_time is matched against these period time windows
    # further down, rather than needing a separate per-period events call.
    periods <- tryCatch(
      catapult_get(paste0(BASE_URL, "/activities/", activity_id, "/periods"), token),
      error = function(e) NULL
    )
    if (!is.null(periods) && !is.data.frame(periods)) {
      periods <- tryCatch(as_tibble(periods), error = function(e) NULL)
    }
    if (safe_nrow(periods) == 0 || !all(c("name", "start_time", "end_time") %in% colnames(periods))) {
      periods <- NULL
    }

    session_rows <- list()

    for (i in seq_len(nrow(athletes))) {
      athlete_id <- athletes$id[i]
      athlete_name <- if ("name" %in% colnames(athletes)) {
        athletes$name[i]
      } else if (all(c("first_name", "last_name") %in% colnames(athletes))) {
        paste(athletes$first_name[i], athletes$last_name[i])
      } else {
        as.character(athlete_id)
      }

      jump_url <- paste0(BASE_URL, "/activities/", activity_id, "/athletes/", athlete_id, "/events")
      player_data <- tryCatch(
        catapult_get(jump_url, token, query = list(event_types = JUMP_EVENT_TYPES)),
        error = function(e) NULL
      )

      if (!is.null(player_data) && !is.null(player_data$data) && length(player_data$data) > 0) {
        player_data <- as_tibble(player_data)
        player_data <- player_data %>% mutate(
          activity_id = activity_id, activity_name = activity_name,
          athlete_id = athlete_id, athlete_name = athlete_name
        )
        player_data <- player_data %>% select(activity_name, activity_id, athlete_name, athlete_id, everything())

        if ("data" %in% colnames(player_data)) player_data <- player_data %>% unnest(cols = c(`data`))
        if ("basketball" %in% colnames(player_data)) player_data <- player_data %>% unnest(cols = c(`basketball`))
        if ("jump_attribute" %in% colnames(player_data)) player_data <- player_data %>% filter(jump_attribute > 0)

        player_data$period_name <- if (!is.null(periods) && "start_time" %in% colnames(player_data)) {
          vapply(player_data$start_time, function(t) {
            idx <- which(t >= periods$start_time & t <= periods$end_time)
            if (length(idx) == 0) NA_character_ else periods$name[idx[1]]
          }, character(1))
        } else {
          NA_character_
        }

        session_rows[[length(session_rows) + 1]] <- player_data
      }
    }

    if (length(session_rows) > 0) all_data <- bind_rows(all_data, bind_rows(session_rows))
  }

  on_progress("Done", 1)
  all_data
}

# ---------- UI ----------

theme <- bs_theme(
  version = 5, bootswatch = "darkly",
  bg = "#0E1A22", fg = "#EDE3CE",
  primary = "#E3B23C", success = "#4FB3A9",
  base_font = font_google("Inter"),
  heading_font = font_google("Barlow Condensed")
)

ui <- page_sidebar(
  title = "Jump Time Dashboard",
  theme = theme,
  sidebar = sidebar(
    width = 300,
    dateRangeInput("dates", "Date range (shows this window)", start = Sys.Date() - 27, end = Sys.Date()),
    layout_columns(
      col_widths = c(6, 6),
      actionButton("preset_28", "Last 28 days", class = "btn-sm btn-outline-secondary w-100"),
      actionButton("preset_365", "Last 12 months", class = "btn-sm btn-outline-secondary w-100")
    ),
    tags$div(style = "margin-top:10px;"),
    actionButton("fetch", "Fetch latest data", class = "btn-primary w-100"),
    tags$small("Fetch pulls new data from Catapult for the dates above."),
    tags$div(style = "margin-top:10px;"),
    textOutput("status"),
    hr(),
    uiOutput("athlete_ui"),
    downloadButton("download_csv", "Export athlete's data (CSV)", class = "w-100"),
    hr(),
    tags$strong("Average = top N jumps per session"),
    numericInput("top_n", NULL, value = 20, min = 1, max = 50),
    hr(),
    tags$strong("High jump cut-off (centiseconds)"),
    numericInput("high_cut", NULL, value = 80, min = 1, max = 300),
    tags$small("Matches OpenField: Settings > Bands > Indoor Jumps > High. A jump counts as high at or above this.")
  ),
  card(
    card_header("Overview"),
    textOutput("overview_jumps"),
    textOutput("overview_avg"),
    textOutput("overview_max")
  ),
  card(
    card_header(textOutput("trend_header")),
    plotlyOutput("trend_plot", height = 750)
  ),
  card(
    card_header("Daily Breakdown"),
    DTOutput("daily_table")
  )
)

# ---------- server ----------

server <- function(input, output, session) {
  token <- Sys.getenv("CATAPULT_TOKEN")
  raw_data <- reactiveVal(NULL)
  status_msg <- reactiveVal("Loading stored data...")

  # Load whatever's already in Supabase the moment the app opens - no API
  # call needed just to see existing data.
  stored_on_start <- tryCatch(load_stored_data(), error = function(e) NULL)
  if (!is.null(stored_on_start)) {
    raw_data(stored_on_start)
    status_msg(paste0("Loaded ", nrow(stored_on_start), " stored jump events. Fetch only pulls what's new."))
  } else if (!nzchar(Sys.getenv("SUPABASE_HOST"))) {
    status_msg("Database not configured (see setup notes at the top of this file) - Fetch will pull from Catapult but won't be saved.")
  } else {
    status_msg("No stored data yet. Set a date range and click Fetch latest data.")
  }

  do_fetch <- function() {
    if (!nzchar(token)) {
      status_msg("No token found. Add CATAPULT_TOKEN to your .Renviron and restart R.")
      return(invisible())
    }
    withProgress(message = "Pulling from Catapult...", value = 0, {
      last_amount <- 0
      d <- tryCatch(
        fetch_jump_data(
          token, input$dates[1], input$dates[2],
          on_progress = function(msg, amount) {
            incProgress(amount - last_amount, detail = msg)
            last_amount <<- amount
          }
        ),
        error = function(e) {
          status_msg(paste("Fetch failed:", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(d)) {
        save_result <- tryCatch({ upsert_jumps(d); "ok" }, error = function(e) paste("save failed:", conditionMessage(e)))
        merged <- tryCatch(load_stored_data(), error = function(e) NULL)
        if (!is.null(merged)) {
          raw_data(merged)
          status_msg(paste0("Fetched ", nrow(d), " events from Catapult. Now showing ", nrow(merged), " total stored events."))
        } else {
          raw_data(d)
          note <- if (identical(save_result, "ok")) "but couldn't read the database back" else save_result
          status_msg(paste0("Fetched ", nrow(d), " events from Catapult (", note, ") - showing this session's data only."))
        }
      }
    })
  }

  # The UI is built once when the app starts, so set the rolling 28-day
  # window fresh for every visitor (otherwise it goes stale on a long-running app).
  session$onFlushed(function() {
    updateDateRangeInput(session, "dates", start = Sys.Date() - 27, end = Sys.Date())
  }, once = TRUE)

  observeEvent(input$fetch, { do_fetch() })
  observeEvent(input$preset_28, {
    updateDateRangeInput(session, "dates", start = Sys.Date() - 27, end = Sys.Date())
  })
  observeEvent(input$preset_365, {
    updateDateRangeInput(session, "dates", start = Sys.Date() - 364, end = Sys.Date())
  })
  output$status <- renderText(status_msg())

  # Shaped data: one row per jump, with date and jump time (seconds) attached.
  shaped <- reactive({
    req(raw_data())
    d <- raw_data()
    d$.date <- as.Date(as.POSIXct(d$start_time, origin = "1970-01-01", tz = "UTC"))
    d$.jump_cs <- if (JUMP_TIME_COL %in% colnames(d)) {
      suppressWarnings(as.numeric(d[[JUMP_TIME_COL]]))   # raw centiseconds, used for the high-jump cut-off
    } else {
      NA_real_
    }
    d$.jump_time_s <- d$.jump_cs * 0.01
    d
  })

  # High-jump cut-off in centiseconds (default 80, same as the OpenField High band).
  high_cut_val <- reactive({
    if (is.null(input$high_cut) || is.na(input$high_cut)) 80 else input$high_cut
  })

  output$athlete_ui <- renderUI({
    req(shaped())
    athletes <- sort(unique(shaped()$athlete_name))
    req(length(athletes) > 0)
    selectInput("athlete", "Athlete", choices = athletes, selected = athletes[1])
  })

  # All of this athlete's stored jumps (full history). The chronic line is
  # worked out from this, so it is correct even at the start of the window.
  athlete_all <- reactive({
    req(shaped(), input$athlete)
    shaped() %>% filter(athlete_name == input$athlete, !is.na(.jump_time_s))
  })

  # Just this athlete's jumps inside the chosen date window (default: last 28 days).
  athlete_data <- reactive({
    d <- athlete_all()
    req(input$dates)
    d %>% filter(.date >= input$dates[1], .date <= input$dates[2])
  })

  # Chronic high-jump load: for every calendar day, the average number of
  # high jumps per day over the last 28 days (that day and the 27 before it).
  # Rest days count as zero. Needs 28 days of history, so the first 27 days
  # after an athlete's first stored session are left blank rather than shown low.
  chronic_daily <- reactive({
    d <- athlete_all()
    req(nrow(d) > 0)
    cut <- high_cut_val()
    daily <- d %>%
      group_by(.date) %>%
      summarise(high = sum(.jump_cs >= cut, na.rm = TRUE), .groups = "drop")
    days <- seq(min(daily$.date), max(Sys.Date(), max(daily$.date)), by = "day")
    full <- data.frame(date = days) %>%
      left_join(daily, by = c("date" = ".date")) %>%
      mutate(high = ifelse(is.na(high), 0, high))
    csum <- cumsum(full$high)
    lagged <- c(rep(0, 28), csum)[seq_along(csum)]
    full$chronic <- (csum - lagged) / 28
    full$chronic[seq_len(min(27, nrow(full)))] <- NA_real_
    full
  })

  session_summary <- reactive({
    d <- athlete_data()
    req(nrow(d) > 0)
    n <- if (is.null(input$top_n) || is.na(input$top_n)) 20 else input$top_n
    cut <- high_cut_val()

    totals <- d %>%
      group_by(activity_id, activity_name) %>%
      summarise(date = min(.date, na.rm = TRUE), jumps = n(),
                high_jumps = sum(.jump_cs >= cut, na.rm = TRUE),
                max_time_s = round(max(.jump_time_s), 2), .groups = "drop")

    top_avgs <- d %>%
      group_by(activity_id) %>%
      arrange(desc(.jump_time_s), .by_group = TRUE) %>%
      slice_head(n = n) %>%
      summarise(avg_time_s = round(mean(.jump_time_s), 2), n_used = n(), .groups = "drop")

    totals %>%
      left_join(top_avgs, by = "activity_id") %>%
      arrange(date, activity_name) %>%
      mutate(session_label = paste0(activity_name, " (", format(date, "%b %d"), ")"))
  })

  output$overview_jumps <- renderText({
    d <- athlete_data()
    paste0(input$athlete, ": ", nrow(d), " jumps across ", length(unique(d$activity_name)), " session(s)")
  })
  output$overview_avg <- renderText({
    d <- athlete_data()
    req(nrow(d) > 0)
    paste0("Average jump time (all jumps): ", round(mean(d$.jump_time_s), 2), " s")
  })
  output$overview_max <- renderText({
    d <- athlete_data()
    req(nrow(d) > 0)
    paste0("Max jump time: ", round(max(d$.jump_time_s), 2), " s")
  })

  output$trend_header <- renderText({
    n <- if (is.null(input$top_n) || is.na(input$top_n)) 20 else input$top_n
    paste0("Jump Time Trend (max, average of top ", n, " jumps) with total and high (>= ", high_cut_val(),
           ") jump counts per session, and chronic high jumps (28-day average per day, shaded)")
  })

  output$trend_plot <- renderPlotly({
    agg <- session_summary() %>%
      left_join(chronic_daily() %>% select(date, chronic), by = "date")
    req(nrow(agg) > 0)
    agg$chronic <- as.numeric(agg$chronic)
    p <- plot_ly(agg, x = ~session_label)
    # Chronic: rolling 28-day average of high jumps per day (rest days = 0),
    # drawn first so it sits behind everything as a light shaded area.
    # Skipped when there isn't 28 days of history yet (all values blank).
    if (any(!is.na(agg$chronic))) {
      p <- p %>% add_trace(y = ~chronic, name = "Chronic high jumps (28-day avg/day)", type = "scatter", mode = "lines",
                yaxis = "y2", fill = "tozeroy", connectgaps = FALSE,
                line = list(color = "rgba(209,96,77,0.9)", width = 2, shape = "spline"),
                fillcolor = "rgba(209,96,77,0.18)")
    }
    p %>%
      # Columns: total and high jump counts per session, on their own
      # (right-hand) axis since counts and seconds are on different scales.
      # Semi-transparent so the time lines stay readable through them.
      add_trace(y = ~jumps, name = "Total jumps", type = "bar", yaxis = "y2",
                marker = list(color = "rgba(111,140,163,0.45)")) %>%
      add_trace(y = ~high_jumps, name = "High jumps", type = "bar", yaxis = "y2",
                marker = list(color = "rgba(209,96,77,0.55)")) %>%
      # Lines: jump time (left axis).
      add_trace(y = ~max_time_s, name = "Max", type = "scatter", mode = "lines+markers",
                line = list(color = "#E3B23C"), marker = list(color = "#E3B23C")) %>%
      add_trace(y = ~avg_time_s, name = "Average (top N)", type = "scatter", mode = "lines+markers",
                line = list(color = "#4FB3A9"), marker = list(color = "#4FB3A9")) %>%
      layout(
        barmode = "group", bargap = 0.35,
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font = list(color = "#EDE3CE", size = 10),
        xaxis = list(title = "", showgrid = FALSE, automargin = TRUE, tickangle = -35, tickfont = list(size = 9),
                     categoryorder = "array", categoryarray = agg$session_label),
        yaxis = list(title = "Jump Time (s)", showgrid = FALSE, zeroline = FALSE, automargin = TRUE, dtick = 0.05,
                     titlefont = list(size = 11), tickfont = list(size = 9)),
        yaxis2 = list(title = "# Jumps", overlaying = "y", side = "right", showgrid = FALSE, zeroline = FALSE,
                      automargin = TRUE, rangemode = "tozero", tickformat = "d",
                      titlefont = list(size = 11, color = "#8FA9BD"), tickfont = list(size = 9, color = "#8FA9BD")),
        # (barmode / bargap are set above)
        legend = list(font = list(size = 10))
      )
  })

  # Same metric (max, and average of top N), computed one level finer - per
  # period within a session - so a session's breakdown can sit under it.
  period_summary <- reactive({
    d <- athlete_data()
    req(nrow(d) > 0)
    n <- if (is.null(input$top_n) || is.na(input$top_n)) 20 else input$top_n
    cut <- high_cut_val()
    d %>%
      filter(!is.na(period_name)) %>%
      group_by(activity_id, activity_name, period_name) %>%
      summarise(
        date = min(.date, na.rm = TRUE),
        jumps = n(),
        high_jumps = sum(.jump_cs >= cut, na.rm = TRUE),
        max_time_s = round(max(.jump_time_s), 2),
        avg_time_s = round(mean(sort(.jump_time_s, decreasing = TRUE)[seq_len(min(n(), n))]), 2),
        n_used = min(n(), n),
        .groups = "drop"
      )
  })

  output$daily_table <- renderDT({
    sess <- session_summary()
    per <- period_summary()
    n_periods_by_activity <- per %>% count(activity_id, name = "n_periods")

    main <- sess %>%
      left_join(n_periods_by_activity, by = "activity_id") %>%
      mutate(n_periods = ifelse(is.na(n_periods), 0, n_periods)) %>%
      arrange(desc(date), activity_name)

    main_display <- main %>%
      transmute(
        ` ` = ifelse(n_periods > 1, "\u25b6", ""),
        Session = activity_name,
        Date = date,
        Jumps = jumps,
        `High Jumps` = high_jumps,
        `Avg Time, Top N (s)` = avg_time_s,
        `N Used` = n_used,
        `Max Time (s)` = max_time_s,
        activity_id = activity_id
      )

    # Warm Up first, then Set 1/2/3... by number, anything unrecognised last.
    period_rank <- function(names_vec) {
      vapply(names_vec, function(nm) {
        nm_l <- tolower(nm)
        if (grepl("warm", nm_l)) return(0)
        m <- regmatches(nm_l, regexpr("[0-9]+", nm_l))
        if (length(m) == 1 && nzchar(m)) return(as.numeric(m))
        99
      }, numeric(1))
    }

    by_activity <- split(per, per$activity_id)
    child_html <- lapply(by_activity, function(p) {
      p <- p[order(period_rank(p$period_name)), , drop = FALSE]
      rows <- paste0(
        "<tr><td style='padding:4px 10px;color:#B7ADA0;'>", p$period_name, "</td>",
        "<td style='padding:4px 10px;'>", p$jumps, "</td>",
        "<td style='padding:4px 10px;'>", p$high_jumps, "</td>",
        "<td style='padding:4px 10px;'>", p$avg_time_s, "</td>",
        "<td style='padding:4px 10px;'>", p$n_used, "</td>",
        "<td style='padding:4px 10px;'>", p$max_time_s, "</td></tr>",
        collapse = ""
      )
      paste0(
        "<table style='width:100%;border-collapse:collapse;'>",
        "<tr style='color:#7C93A2;font-size:11px;'>",
        "<td style='padding:4px 10px;'>Period</td><td style='padding:4px 10px;'>Jumps</td>",
        "<td style='padding:4px 10px;'>High Jumps</td>",
        "<td style='padding:4px 10px;'>Avg Time, Top N (s)</td><td style='padding:4px 10px;'>N Used</td>",
        "<td style='padding:4px 10px;'>Max Time (s)</td></tr>",
        rows, "</table>"
      )
    })

    activity_id_col <- which(colnames(main_display) == "activity_id") - 1  # 0-indexed for JS

    datatable(
      main_display,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        columnDefs = list(
          list(targets = activity_id_col, visible = FALSE),
          list(targets = 0, orderable = FALSE, className = "details-control", width = "18px")
        )
      ),
      callback = JS(sprintf("
        var childData = %s;
        table.on('click', 'td.details-control', function() {
          var td = $(this);
          var tr = td.closest('tr');
          var row = table.row(tr);
          var data = row.data();
          var actId = data[%d];
          if (!childData.hasOwnProperty(actId)) { return; }
          if (row.child.isShown()) {
            row.child.hide();
            td.html('&#9654;');
          } else {
            row.child(childData[actId]).show();
            td.html('&#9660;');
          }
        });
      ", jsonlite::toJSON(child_html, auto_unbox = TRUE), activity_id_col))
    ) %>%
      formatStyle(columns = setdiff(colnames(main_display), "activity_id"),
                  color = "#EDE3CE", backgroundColor = "#122029")
  }, server = FALSE)

  output$download_csv <- downloadHandler(
    filename = function() paste0(gsub(" ", "_", req(input$athlete)), "_jump_data_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- athlete_data() %>% select(athlete_name, .date, activity_name, .jump_time_s)
      names(d) <- c("athlete", "date", "session", "jump_time_s")
      write.csv(d, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
