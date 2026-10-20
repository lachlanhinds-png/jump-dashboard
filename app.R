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
# Sessions are dated, and days/weeks are cut, in Adelaide time (not UTC) so an
# early-morning session lands on the right day, and Monday starts the week locally.
LOCAL_TZ <- "Australia/Adelaide"
today_local <- function() as.Date(format(Sys.time(), "%Y-%m-%d", tz = LOCAL_TZ))
to_local_date <- function(epoch_seconds) as.Date(format(as.POSIXct(epoch_seconds, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d", tz = LOCAL_TZ))

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

# Syncs the store with what Catapult currently has for a date window:
#  1. adds new jumps and refreshes jumps already stored (names, coding, values);
#  2. removes stored jumps Catapult no longer has - athletes taken out of an
#     activity, jumps that were re-processed, and activities that were deleted
#     or replaced (e.g. an auto-named "Activity 2026..." copy left behind when a
#     session is renamed/recreated in OpenField).
# Safeguards: an activity where ANY Catapult call failed is skipped entirely
# (never deleted from, never overwritten) and retried next time; and if a
# cleanup would remove more than half the rows in the window it is NOT done
# and a note is returned instead. All writes happen in one transaction.
KEY_COLUMNS <- c("activity_id", "athlete_id", "start_time", "end_time")

sync_jumps <- function(d, start_date, end_date) {
  res <- list(upserted = 0, removed_stale = 0, removed_orphan = 0, detail = "",
              failed = length(attr(d, "failed_ids")), skipped_cleanup = FALSE, note = "")
  has_rows <- !is.null(d) && nrow(d) > 0
  complete_ids <- attr(d, "complete_ids")
  all_ids <- attr(d, "all_listed_ids")
  if (!has_rows && is.null(complete_ids) && is.null(all_ids)) return(res)

  if (has_rows) {
    for (col in setdiff(DB_COLUMNS, colnames(d))) d[[col]] <- NA
    d <- d[, DB_COLUMNS, drop = FALSE]
    # start/end times are stored as whole seconds (bigint), so round them here the
    # same way - otherwise fractional times never match the stored copy and every
    # stored row would look "missing from Catapult".
    d$start_time <- round(as.numeric(d$start_time))
    d$end_time <- round(as.numeric(d$end_time))
    d <- d[!duplicated(d[, KEY_COLUMNS, drop = FALSE]), , drop = FALSE]
  }

  con <- get_db_con()
  on.exit(DBI::dbDisconnect(con))
  qlist <- function(ids) paste(DBI::dbQuoteLiteral(con, as.character(ids)), collapse = ", ")
  num <- function(x) format(x, scientific = FALSE)
  s_epoch <- num(as.numeric(as.POSIXct(format(as.Date(start_date)), tz = LOCAL_TZ)))
  e_epoch <- num(as.numeric(as.POSIXct(format(as.Date(end_date)), tz = LOCAL_TZ)) + 86400)

  if (has_rows) DBI::dbWriteTable(con, "jumps_staging", d, temporary = TRUE, overwrite = TRUE)

  # Rows inside activities Catapult returned in full, that Catapult no longer has.
  where_stale <- NULL
  if (length(complete_ids) > 0) {
    where_stale <- paste0("activity_id IN (", qlist(complete_ids), ")")
    if (has_rows) {
      where_stale <- paste0(where_stale, " AND NOT EXISTS (SELECT 1 FROM jumps_staging s WHERE ",
        "s.activity_id = jumps.activity_id AND s.athlete_id = jumps.athlete_id AND ",
        "s.start_time = jumps.start_time AND s.end_time = jumps.end_time)")
    }
  }
  # Stored activities in this window that no longer exist in Catapult at all.
  where_orphan <- NULL
  if (length(all_ids) > 0) {
    where_orphan <- paste0(
      "activity_id IN (SELECT activity_id FROM jumps GROUP BY activity_id HAVING min(start_time) >= ",
      s_epoch, " AND min(start_time) < ", e_epoch, ") AND activity_id NOT IN (", qlist(all_ids), ")")
  }

  # (counts come back from Postgres as 64-bit integers, which break ordinary
  # arithmetic like 0.5 * n - so convert to plain numbers)
  count_where <- function(w) if (is.null(w)) 0 else
    as.numeric(DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM jumps WHERE ", w))$n)
  window_rows <- as.numeric(DBI::dbGetQuery(con, paste0(
    "SELECT count(*) AS n FROM jumps WHERE start_time >= ", s_epoch, " AND start_time < ", e_epoch))$n)
  # Which sessions the rows to remove belong to, so the result can say what changed.
  describe_where <- function(w) {
    if (is.null(w)) return(character())
    x <- DBI::dbGetQuery(con, paste0(
      "SELECT activity_name, count(*) AS n FROM jumps WHERE ", w,
      " GROUP BY activity_name ORDER BY count(*) DESC LIMIT 8"))
    if (nrow(x) == 0) return(character())
    paste0(x$activity_name, " (", as.numeric(x$n), ")")
  }
  n_stale <- count_where(where_stale)
  n_orphan <- count_where(where_orphan)
  do_cleanup <- TRUE
  if (window_rows > 200 && (n_stale + n_orphan) > 0.5 * window_rows) {
    do_cleanup <- FALSE
    res$skipped_cleanup <- TRUE
    res$note <- paste0("Cleanup skipped: it would have removed ", n_stale + n_orphan, " of ", window_rows,
                       " stored rows in this window, which looks wrong. Nothing was deleted.")
  }

  if (do_cleanup && (n_stale + n_orphan) > 0) {
    res$detail <- paste(unique(c(describe_where(where_stale), describe_where(where_orphan))), collapse = "; ")
  }

  DBI::dbWithTransaction(con, {
    if (has_rows) {
      update_cols <- setdiff(DB_COLUMNS, KEY_COLUMNS)
      DBI::dbExecute(con, paste0(
        "INSERT INTO jumps (", paste(DB_COLUMNS, collapse = ", "), ") ",
        "SELECT ", paste(DB_COLUMNS, collapse = ", "), " FROM jumps_staging ",
        "ON CONFLICT (", paste(KEY_COLUMNS, collapse = ", "), ") DO UPDATE SET ",
        paste0(update_cols, " = EXCLUDED.", update_cols, collapse = ", ")))
      res$upserted <- nrow(d)
    }
    if (do_cleanup && !is.null(where_stale) && n_stale > 0)
      res$removed_stale <- DBI::dbExecute(con, paste0("DELETE FROM jumps WHERE ", where_stale))
    if (do_cleanup && !is.null(where_orphan) && n_orphan > 0)
      res$removed_orphan <- DBI::dbExecute(con, paste0("DELETE FROM jumps WHERE ", where_orphan))
  })
  res
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
  all_listed_ids <- as.character(activities$id)   # every activity Catapult lists, before the date filter

  activities <- activities %>%
    mutate(date = to_local_date(start_time)) %>%
    filter(date >= as.Date(start_date), date <= as.Date(end_date))

  if (safe_nrow(activities) == 0) stop("No activities in that date range.")

  all_data <- tibble()
  n <- nrow(activities)
  complete_ids <- character()   # activities where every Catapult call succeeded
  failed_ids <- character()     # activities where a call failed - left untouched

  for (j in seq_len(n)) {
    activity_id <- activities$id[j]
    activity_name <- activities$name[j]
    on_progress(paste0("Activity ", j, "/", n, ": ", activity_name), 0.05 + 0.9 * (j - 1) / n)
    activity_ok <- TRUE

    athletes <- tryCatch(
      catapult_get(paste0(BASE_URL, "/activities/", activity_id, "/athletes"), token),
      error = function(e) { activity_ok <<- FALSE; NULL }
    )
    if (!is.null(athletes) && !is.data.frame(athletes)) {
      athletes <- tryCatch(as_tibble(athletes), error = function(e) NULL)
    }
    if (safe_nrow(athletes) == 0) {
      if (activity_ok) complete_ids <- c(complete_ids, activity_id) else failed_ids <- c(failed_ids, activity_id)
      next
    }

    # Periods (e.g. Set 1/Set 2/Set 3) - fetched once per activity, not per
    # athlete, since they're a property of the activity itself. Each jump
    # event's own start_time is matched against these period time windows
    # further down, rather than needing a separate per-period events call.
    periods <- tryCatch(
      catapult_get(paste0(BASE_URL, "/activities/", activity_id, "/periods"), token),
      error = function(e) { activity_ok <<- FALSE; NULL }
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
        error = function(e) { activity_ok <<- FALSE; NULL }
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

    if (activity_ok) {
      complete_ids <- c(complete_ids, activity_id)
      if (length(session_rows) > 0) all_data <- bind_rows(all_data, bind_rows(session_rows))
    } else {
      failed_ids <- c(failed_ids, activity_id)   # don't use partial data from this activity
    }
  }

  on_progress("Done", 1)
  attr(all_data, "all_listed_ids") <- all_listed_ids
  attr(all_data, "complete_ids") <- complete_ids
  attr(all_data, "failed_ids") <- failed_ids
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

# Monday of the week a date falls in (Mon-Sun weeks, locale independent).
week_start <- function(x) x - (as.integer(format(x, "%u")) - 1L)

# One row per session: jump counts, max, and the average of the top-N jumps.
session_stats <- function(d, topn, cut) {
  totals <- d %>%
    group_by(activity_id, activity_name) %>%
    summarise(date = min(.date, na.rm = TRUE), jumps = n(),
              high_jumps = sum(.jump_cs >= cut, na.rm = TRUE),
              max_time_s = round(max(.jump_time_s), 2), .groups = "drop")
  top_avgs <- d %>%
    group_by(activity_id) %>%
    arrange(desc(.jump_time_s), .by_group = TRUE) %>%
    slice_head(n = topn) %>%
    summarise(avg_time_s = round(mean(.jump_time_s), 2), n_used = n(), .groups = "drop")
  totals %>%
    left_join(top_avgs, by = "activity_id") %>%
    arrange(date, activity_name) %>%
    mutate(session_label = paste0(activity_name, " (", format(date, "%b %d"), ")"))
}

ui <- page_sidebar(
  title = "Jump Time Dashboard",
  theme = theme,
  fillable = FALSE,   # let the whole page scroll instead of squeezing every card into one screen
  tags$head(tags$style(".view-mode .form-group, .view-mode .shiny-input-container { margin-bottom: 0; }")),
  sidebar = sidebar(
    width = 300,
    dateRangeInput("dates", "Show dates", start = today_local() - 27, end = today_local()),
    layout_columns(
      col_widths = c(6, 6),
      actionButton("preset_28", "Last 28 days", class = "btn-sm btn-outline-secondary w-100"),
      actionButton("preset_365", "Last 12 months", class = "btn-sm btn-outline-secondary w-100")
    ),
    hr(),
    tags$strong("Refresh from Catapult"),
    dateRangeInput("sync_dates", NULL, start = today_local() - 6, end = today_local()),
    actionButton("fetch", "Refresh these dates", class = "btn-primary w-100"),
    tags$small("Re-pulls every session in these dates and updates the saved copy, so sessions you've renamed or coded since they uploaded are corrected. The last 7 days are also refreshed automatically every 12 hours. For a long range (more than a few weeks) use the GitHub 'Run workflow' button instead."),
    tags$div(style = "margin-top:10px;"),
    textOutput("status"),
    hr(),
    uiOutput("athlete_ui"),
    downloadButton("download_csv", "Export athlete's data (CSV)", class = "w-100"),
    hr(),
    tags$strong("Average = top N jumps (per session, or per week in week view)"),
    numericInput("top_n", NULL, value = 10, min = 1, max = 50),
    hr(),
    tags$strong("High jump cut-off (centiseconds)"),
    numericInput("high_cut", NULL, value = 82, min = 1, max = 300),
    tags$small("A jump counts as high at or above this. OpenField lists the High band as 80, but 82 reproduces its session counts exactly (checked on two sessions), so 82 is the default.")
  ),
  card(
    card_header("Overview"),
    textOutput("overview_jumps"),
    textOutput("overview_avg"),
    textOutput("overview_max")
  ),
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-start gap-3",
          div(style = "flex:1;", textOutput("trend_header")),
          div(class = "view-mode", style = "width:210px; flex:none;",
              selectInput("view_mode", NULL, width = "100%",
                          choices = c("Rolling day by day" = "day", "Week view (Mon\u2013Sun)" = "week"),
                          selected = "day")))
    ),
    plotlyOutput("trend_plot", height = 680)
  ),
  card(
    card_header(textOutput("table_header")),
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
    status_msg(paste0("Loaded ", nrow(stored_on_start), " stored jump events."))
  } else if (!nzchar(Sys.getenv("SUPABASE_HOST"))) {
    status_msg("Database not configured (see setup notes at the top of this file) - Fetch will pull from Catapult but won't be saved.")
  } else {
    status_msg("No stored data yet. Set the refresh dates and click Refresh these dates.")
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
          token, input$sync_dates[1], input$sync_dates[2] + 1,  # +1 so sessions starting late on the last day (UTC) are included
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
        sync_info <- NULL
        save_result <- tryCatch({
          sync_info <- sync_jumps(d, input$sync_dates[1], input$sync_dates[2] + 1)
          "ok"
        }, error = function(e) paste("save failed:", conditionMessage(e)))
        clean_txt <- ""
        if (!is.null(sync_info)) {
          removed <- sync_info$removed_stale + sync_info$removed_orphan
          if (removed > 0) clean_txt <- paste0(clean_txt, "Removed ", removed, " rows Catapult no longer has (", sync_info$detail, "). ")
          if (sync_info$failed > 0) clean_txt <- paste0(clean_txt, sync_info$failed, " activities were skipped (Catapult errors) - refresh again. ")
          if (nzchar(sync_info$note)) clean_txt <- paste0(clean_txt, sync_info$note, " ")
        }
        merged <- tryCatch(load_stored_data(), error = function(e) NULL)
        if (!is.null(merged)) {
          raw_data(merged)
          status_msg(paste0("Refreshed ", nrow(d), " events from Catapult. ", clean_txt, "Now showing ", nrow(merged), " total stored events."))
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
    updateDateRangeInput(session, "dates", start = today_local() - 27, end = today_local())
    updateDateRangeInput(session, "sync_dates", start = today_local() - 6, end = today_local())
  }, once = TRUE)

  observeEvent(input$fetch, { do_fetch() })
  observeEvent(input$preset_28, {
    updateDateRangeInput(session, "dates", start = today_local() - 27, end = today_local())
  })
  observeEvent(input$preset_365, {
    updateDateRangeInput(session, "dates", start = today_local() - 364, end = today_local())
  })
  output$status <- renderText(status_msg())

  # Shaped data: one row per jump, with date and jump time (seconds) attached.
  shaped <- reactive({
    req(raw_data())
    d <- raw_data()
    d$.date <- to_local_date(d$start_time)
    d$.jump_cs <- if (JUMP_TIME_COL %in% colnames(d)) {
      suppressWarnings(as.numeric(d[[JUMP_TIME_COL]]))   # raw centiseconds, used for the high-jump cut-off
    } else {
      NA_real_
    }
    d$.jump_time_s <- d$.jump_cs * 0.01
    d
  })

  # High-jump cut-off in centiseconds (default 82, which reproduces OpenField's High counts).
  high_cut_val <- reactive({
    if (is.null(input$high_cut) || is.na(input$high_cut)) 82 else input$high_cut
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
  # high jumps per day over the 28 days BEFORE it (the day itself is excluded).
  # Rest days count as zero. Needs 28 days of history, so the first 28 days
  # after an athlete's first stored session are left blank rather than shown low.
  chronic_daily <- reactive({
    d <- athlete_all()
    req(nrow(d) > 0)
    cut <- high_cut_val()
    daily <- d %>%
      group_by(.date) %>%
      summarise(high = sum(.jump_cs >= cut, na.rm = TRUE), .groups = "drop")
    days <- seq(min(daily$.date), max(today_local(), max(daily$.date)), by = "day")
    full <- data.frame(date = days) %>%
      left_join(daily, by = c("date" = ".date")) %>%
      mutate(high = ifelse(is.na(high), 0, high))
    # For each day: the 28 days BEFORE it (today is excluded, it hasn't finished).
    csum <- cumsum(full$high)
    pad <- c(rep(0, 29), csum)
    full$chronic <- (pad[seq_along(csum) + 28] - pad[seq_along(csum)]) / 28
    full$chronic[seq_len(min(28, nrow(full)))] <- NA_real_
    full
  })

  top_n_val <- reactive({
    if (is.null(input$top_n) || is.na(input$top_n)) 10 else input$top_n
  })
  is_week <- reactive(identical(input$view_mode, "week"))

  session_summary <- reactive({
    d <- athlete_data()
    req(nrow(d) > 0)
    session_stats(d, top_n_val(), high_cut_val())
  })

  # ---- Week view (Mon-Sun) ----
  # Whole weeks that overlap the chosen date window, so a week is never shown
  # half-counted just because the window starts mid-week. (The current week is
  # naturally "to date".)
  week_data <- reactive({
    d <- athlete_all()
    req(input$dates)
    d$.week <- week_start(d$.date)
    d %>% filter(.week >= week_start(input$dates[1]), .week <= week_start(input$dates[2]))
  })

  # One row per week: jumps and high jumps summed over the week's days, max of
  # the week, and the average of the top-N jumps across the whole week.
  week_summary <- reactive({
    d <- week_data()
    req(nrow(d) > 0)
    topn <- top_n_val()
    cut <- high_cut_val()
    totals <- d %>%
      group_by(.week) %>%
      summarise(sessions = n_distinct(activity_id), jumps = n(),
                high_jumps = sum(.jump_cs >= cut, na.rm = TRUE),
                max_time_s = round(max(.jump_time_s), 2), .groups = "drop")
    top_avgs <- d %>%
      group_by(.week) %>%
      arrange(desc(.jump_time_s), .by_group = TRUE) %>%
      slice_head(n = topn) %>%
      summarise(avg_time_s = round(mean(.jump_time_s), 2), n_used = n(), .groups = "drop")
    totals %>%
      left_join(top_avgs, by = ".week") %>%
      rename(week = .week) %>%
      arrange(week) %>%
      mutate(week_label = paste0("Wk ", format(week, "%b %d")),
             week_range = paste0(format(week, "%d %b"), " – ", format(week + 6, "%d %b")))
  })

  # Chronic high-jump load, week version: for every Mon-Sun week, the average
  # number of high jumps per week over the 4 full weeks before it.
  # Weeks with no sessions count as zero. Blank until 4 weeks of history exist.
  chronic_weekly <- reactive({
    d <- athlete_all()
    req(nrow(d) > 0)
    cut <- high_cut_val()
    d$.week <- week_start(d$.date)
    wk <- d %>%
      group_by(.week) %>%
      summarise(high = sum(.jump_cs >= cut, na.rm = TRUE), .groups = "drop")
    weeks <- seq(min(wk$.week), max(week_start(today_local()), max(wk$.week)), by = "week")
    full <- data.frame(week = weeks) %>%
      left_join(wk, by = c("week" = ".week")) %>%
      mutate(high = ifelse(is.na(high), 0, high))
    # For each week: the 4 full weeks BEFORE it (the current week is excluded, it hasn't finished).
    csum <- cumsum(full$high)
    pad <- c(rep(0, 5), csum)
    full$chronic <- (pad[seq_along(csum) + 4] - pad[seq_along(csum)]) / 4
    full$chronic[seq_len(min(4, nrow(full)))] <- NA_real_
    full
  })

  # What the graph plots, in either view.
  plot_data <- reactive({
    if (is_week()) {
      w <- week_summary() %>%
        left_join(chronic_weekly() %>% select(week, chronic), by = "week")
      data.frame(label = w$week_label, hover = paste0("Week ", w$week_range, " (", w$sessions, " sessions)"),
                 jumps = w$jumps, high_jumps = w$high_jumps,
                 max_time_s = w$max_time_s, avg_time_s = w$avg_time_s,
                 chronic = as.numeric(w$chronic), stringsAsFactors = FALSE)
    } else {
      s <- session_summary() %>%
        left_join(chronic_daily() %>% select(date, chronic), by = "date")
      # Short x labels ("Tue 13 Oct") keep the chart tall; the full session name
      # is in the hover. Two sessions on one day get (1), (2).
      lab <- format(s$date, "%a %d %b")
      idx <- ave(seq_along(lab), lab, FUN = seq_along)
      cnt <- ave(seq_along(lab), lab, FUN = length)
      lab <- ifelse(cnt > 1, paste0(lab, " (", idx, ")"), lab)
      data.frame(label = lab, hover = paste0(s$activity_name, " - ", format(s$date, "%a %d %b")),
                 jumps = s$jumps, high_jumps = s$high_jumps,
                 max_time_s = s$max_time_s, avg_time_s = s$avg_time_s,
                 chronic = as.numeric(s$chronic), stringsAsFactors = FALSE)
    }
  })

  output$trend_header <- renderText({
    n <- top_n_val()
    if (is_week()) {
      paste0("Weekly Jump Time Trend (Mon–Sun): max and average of the top ", n, " jumps in the week, with total and high (>= ",
             high_cut_val(), ") jump counts per week, and chronic high jumps (average per week over the previous 4 full weeks, shaded)")
    } else {
      paste0("Jump Time Trend (max, average of top ", n, " jumps) with total and high (>= ", high_cut_val(),
             ") jump counts per session, and chronic high jumps (average per day over the previous 28 days, shaded)")
    }
  })

  output$table_header <- renderText({
    if (is_week()) "Weekly Breakdown (Mon–Sun)" else "Daily Breakdown"
  })

  output$trend_plot <- renderPlotly({
    agg <- plot_data()
    req(nrow(agg) > 0)
    chronic_name <- if (is_week()) "Chronic high jumps (previous 4 weeks, avg/week)" else "Chronic high jumps (previous 28 days, avg/day)"
    p <- plot_ly(agg, x = ~label)
    # Chronic: rolling average of high jumps (rest days/weeks = 0), drawn first
    # so it sits behind everything as a light shaded area. Skipped when there
    # isn't enough history yet (all values blank).
    if (any(!is.na(agg$chronic))) {
      p <- p %>% add_trace(y = ~chronic, name = chronic_name, type = "scatter", mode = "lines",
                yaxis = "y2", fill = "tozeroy", connectgaps = FALSE,
                text = ~hover, hovertemplate = "<b>%{text}</b><br>Chronic high jumps: %{y:.1f}<extra></extra>",
                line = list(color = "rgb(232,110,90)", width = 3.5, shape = "spline"),
                fillcolor = "rgba(232,110,90,0.42)")
    }
    p %>%
      # Columns: total and high jump counts, on their own (right-hand) axis
      # since counts and seconds are on different scales. Semi-transparent so
      # the time lines stay readable through them.
      add_trace(y = ~jumps, name = "Total jumps", type = "bar", yaxis = "y2",
                text = ~hover, hovertemplate = "<b>%{text}</b><br>Total jumps: %{y}<extra></extra>",
                marker = list(color = "rgba(111,140,163,0.45)")) %>%
      add_trace(y = ~high_jumps, name = "High jumps", type = "bar", yaxis = "y2",
                text = ~hover, hovertemplate = "<b>%{text}</b><br>High jumps: %{y}<extra></extra>",
                marker = list(color = "rgba(209,96,77,0.55)")) %>%
      # Lines: jump time (left axis).
      add_trace(y = ~max_time_s, name = "Max", type = "scatter", mode = "lines+markers",
                text = ~hover, hovertemplate = "<b>%{text}</b><br>Max: %{y:.2f} s<extra></extra>",
                line = list(color = "#E3B23C"), marker = list(color = "#E3B23C")) %>%
      add_trace(y = ~avg_time_s, name = "Average (top N)", type = "scatter", mode = "lines+markers",
                text = ~hover, hovertemplate = "<b>%{text}</b><br>Average (top N): %{y:.2f} s<extra></extra>",
                line = list(color = "#4FB3A9"), marker = list(color = "#4FB3A9")) %>%
      layout(
        barmode = "group", bargap = 0.35,
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font = list(color = "#EDE3CE", size = 10),
        xaxis = list(title = "", showgrid = FALSE, automargin = TRUE, tickangle = -45, tickfont = list(size = 10),
                     categoryorder = "array", categoryarray = agg$label),
        yaxis = list(title = "Jump Time (s)", showgrid = FALSE, zeroline = FALSE, automargin = TRUE, dtick = 0.05,
                     titlefont = list(size = 11), tickfont = list(size = 9)),
        yaxis2 = list(title = "# Jumps", overlaying = "y", side = "right", showgrid = FALSE, zeroline = FALSE,
                      automargin = TRUE, rangemode = "tozero", tickformat = "d",
                      titlefont = list(size = 11, color = "#8FA9BD"), tickfont = list(size = 9, color = "#8FA9BD")),
        # Legend along the top (not down the right-hand side) so the plot gets the full width.
        legend = list(orientation = "h", x = 0, y = 1.1, font = list(size = 11)),
        margin = list(t = 50)
      )
  })

  # Same metric (max, and average of top N), computed one level finer - per
  # period within a session - so a session's breakdown can sit under it.
  period_summary <- reactive({
    d <- athlete_data()
    req(nrow(d) > 0)
    n <- top_n_val()
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
    cell <- "padding:4px 10px;"
    child_table <- function(first_header, body_rows) {
      paste0(
        "<table style='width:100%;border-collapse:collapse;'>",
        "<tr style='color:#7C93A2;font-size:11px;'>",
        "<td style='", cell, "'>", first_header, "</td>",
        "<td style='", cell, "'>Jumps</td><td style='", cell, "'>High Jumps</td>",
        "<td style='", cell, "'>Avg Time, Top N (s)</td><td style='", cell, "'>N Used</td>",
        "<td style='", cell, "'>Max Time (s)</td></tr>",
        body_rows, "</table>"
      )
    }
    child_rows <- function(label, p) {
      paste0(
        "<tr><td style='", cell, "color:#B7ADA0;'>", label, "</td>",
        "<td style='", cell, "'>", p$jumps, "</td>",
        "<td style='", cell, "'>", p$high_jumps, "</td>",
        "<td style='", cell, "'>", p$avg_time_s, "</td>",
        "<td style='", cell, "'>", p$n_used, "</td>",
        "<td style='", cell, "'>", p$max_time_s, "</td></tr>",
        collapse = ""
      )
    }

    if (is_week()) {
      # One row per Mon-Sun week (jumps summed, top-N average over the whole
      # week); click the arrow to see that week's sessions.
      wk <- week_summary() %>% arrange(desc(week))
      main_display <- wk %>%
        transmute(
          ` ` = ifelse(sessions > 0, "▶", ""),
          Week = week_range,
          Sessions = sessions,
          Jumps = jumps,
          `High Jumps` = high_jumps,
          `Avg Time, Top N (s)` = avg_time_s,
          `N Used` = n_used,
          `Max Time (s)` = max_time_s,
          key = as.character(week)
        )
      topn <- top_n_val()
      cut <- high_cut_val()
      wd <- week_data()
      by_week <- split(wd, as.character(wd$.week))
      child_html <- lapply(by_week, function(dw) {
        ss <- session_stats(dw, topn, cut) %>% arrange(date, activity_name)
        child_table("Session", child_rows(paste0(ss$activity_name, " (", format(ss$date, "%a %d %b"), ")"), ss))
      })
    } else {
      sess <- session_summary()
      per <- period_summary()
      n_periods_by_activity <- per %>% count(activity_id, name = "n_periods")

      main <- sess %>%
        left_join(n_periods_by_activity, by = "activity_id") %>%
        mutate(n_periods = ifelse(is.na(n_periods), 0, n_periods)) %>%
        arrange(desc(date), activity_name)

      main_display <- main %>%
        transmute(
          ` ` = ifelse(n_periods > 1, "▶", ""),
          Session = activity_name,
          Date = date,
          Jumps = jumps,
          `High Jumps` = high_jumps,
          `Avg Time, Top N (s)` = avg_time_s,
          `N Used` = n_used,
          `Max Time (s)` = max_time_s,
          key = as.character(activity_id)
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
        child_table("Period", child_rows(p$period_name, p))
      })
    }

    key_col <- which(colnames(main_display) == "key") - 1  # 0-indexed for JS

    datatable(
      main_display,
      escape = FALSE,
      rownames = FALSE,
      fillContainer = FALSE,
      options = list(
        paging = FALSE,            # one long list that scrolls inside its own box...
        scrollY = "520px",         # ...while the whole page also scrolls
        scrollCollapse = TRUE,
        dom = "fti",
        columnDefs = list(
          list(targets = key_col, visible = FALSE),
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
      ", jsonlite::toJSON(child_html, auto_unbox = TRUE), key_col))
    ) %>%
      formatStyle(columns = setdiff(colnames(main_display), "key"),
                  color = "#EDE3CE", backgroundColor = "#122029")
  }, server = FALSE)

  output$download_csv <- downloadHandler(
    filename = function() paste0(gsub(" ", "_", req(input$athlete)), "_jump_data_", today_local(), ".csv"),
    content = function(file) {
      d <- athlete_data() %>% select(athlete_name, .date, activity_name, .jump_time_s)
      names(d) <- c("athlete", "date", "session", "jump_time_s")
      write.csv(d, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
