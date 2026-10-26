
# ============================================================
# TEST: PANAMA CND / ETESA LIVE DATA SOURCE
# Read-only diagnostic; no database or model changes.
# ============================================================

if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")
}
if (!requireNamespace("xml2", quietly = TRUE)) {
  install.packages("xml2")
}

library(httr)
library(xml2)

url <- "https://sitr.cnd.com.pa/m/pub/sin.html"

cat("\n========================================\n")
cat("PANAMA LIVE SOURCE CONNECTIVITY TEST\n")
cat("========================================\n")
cat("URL:", url, "\n")

response <- tryCatch(
  GET(
    url,
    timeout(25),
    user_agent("PanamaElectricityForecasting/1.0")
  ),
  error = function(e) {
    cat("\n[FAIL] HTTP request failed:", conditionMessage(e), "\n")
    NULL
  }
)

if (is.null(response)) {
  stop("Could not retrieve the SITR page. Check connectivity and retry.")
}

status <- status_code(response)
content_type <- headers(response)[["content-type"]]

cat("\nHTTP status:", status, "\n")
cat("Content type:", content_type, "\n")
cat("Downloaded bytes:", length(content(response, "raw")), "\n")

if (status != 200) {
  stop("The source did not return HTTP 200.")
}

html <- content(response, as = "text", encoding = "UTF-8")
doc <- read_html(html)
page_text <- xml_text(doc, trim = TRUE)

cat("\n--- Live data page content ---\n")

for (label in c(
  "Generación total",
  "Demanda Total",
  "Intercambio neto",
  "Intercambio programado",
  "Reserva rodante",
  "Generación vs. Demanda Real vs. Demanda Pronosticada"
)) {
  position <- regexpr(label, page_text, ignore.case = TRUE)[1]
  
  if (position > 0) {
    start <- max(1, position - 30)
    end <- min(nchar(page_text), position + 180)
    
    cat("\n[FOUND]", label, "\n")
    cat(substr(page_text, start, end), "\n")
  } else {
    cat("\n[NOT FOUND]", label, "\n")
  }
}

# ------------------------------------------------------------
# Inspect scripts that may load data dynamically
# ------------------------------------------------------------

script_nodes <- xml_find_all(doc, ".//script[@src]")
script_urls <- xml_attr(script_nodes, "src")
script_urls <- unique(script_urls[!is.na(script_urls)])

cat("\n--- JavaScript files referenced by the page ---\n")
cat("Script files found:", length(script_urls), "\n")

if (length(script_urls) > 0) {
  full_script_urls <- vapply(
    script_urls,
    function(x) url_absolute(x, url),
    character(1)
  )
  
  print(full_script_urls)
} else {
  full_script_urls <- character()
  cat("No external scripts found in the returned HTML.\n")
}

# ------------------------------------------------------------
# Look for clues to an underlying data endpoint
# ------------------------------------------------------------

patterns <- c(
  "api", "json", "ajax", "fetch\\(",
  "XMLHttpRequest", "demanda", "pronostic",
  "websocket", "\\.php", "\\.ashx"
)

cat("\n--- Endpoint clues in returned HTML ---\n")

for (pattern in patterns) {
  found <- grepl(pattern, html, ignore.case = TRUE, perl = TRUE)
  cat(if (found) "[FOUND] " else "[NOT FOUND] ",
      pattern, "\n")
}

# Search the first 20 external scripts for endpoint clues.
# This is discovery only; it does not call any discovered endpoint.

if (length(full_script_urls) > 0) {
  cat("\n--- Inspecting JavaScript references ---\n")
  
  for (script_url in head(full_script_urls, 20)) {
    script_response <- tryCatch(
      GET(script_url, timeout(15)),
      error = function(e) NULL
    )
    
    if (is.null(script_response) ||
        status_code(script_response) != 200) {
      cat("[SKIP] Could not retrieve:", script_url, "\n")
      next
    }
    
    js <- content(
      script_response,
      as = "text",
      encoding = "UTF-8"
    )
    
    clues <- grepl(
      "api|json|ajax|fetch\\(|XMLHttpRequest|websocket",
      js,
      ignore.case = TRUE,
      perl = TRUE
    )
    
    if (clues) {
      cat("[CLUE] Relevant script:", script_url, "\n")
      
      # Print a few endpoint-like string literals, if present.
      matches <- gregexpr(
        "['\"][^'\"]{1,180}(/[^'\"]+|api[^'\"]*|\\.json[^'\"]*)['\"]",
        js,
        ignore.case = TRUE,
        perl = TRUE
      )
      
      candidates <- regmatches(js, matches)[[1]]
      
      if (length(candidates) > 0) {
        print(head(unique(candidates), 12))
      } else {
        cat("  No obvious endpoint strings extracted.\n")
      }
    }
  }
}

cat("\n========================================\n")
cat("CONNECTIVITY TEST COMPLETE\n")
cat("No database or model files were modified.\n")
cat("========================================\n")
