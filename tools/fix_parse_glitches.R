# tools/fix_parse_glitches.R
stopifnot(file.exists("DESCRIPTION"))
stopifnot(file.exists("R/surroNMA.R"))

backup <- sprintf("R/surroNMA.R.bak-%s", format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy("R/surroNMA.R", backup, overwrite = TRUE)
message("Backup written: ", backup)

read_lines  <- function() readLines("R/surroNMA.R", warn = FALSE)
write_lines <- function(x) writeLines(x, "R/surroNMA.R", useBytes = TRUE)

x <- read_lines()

# 1) Join broken assignments: a bare name line followed by a line that starts with "<-"
is_name_only <- function(s) grepl("^\\s*[A-Za-z.][A-Za-z0-9._]*\\s*$", s)
is_arrow_next <- function(s) grepl("^\\s*<-", s)

i <- 1L
joined <- 0L
while (i < length(x)) {
  if (is_name_only(x[i]) && is_arrow_next(x[i+1])) {
    lhs <- sub("^\\s*([A-Za-z.][A-Za-z0-9._]*)\\s*$", "\\1", x[i])
    rhs <- sub("^\\s*<-\\s*", "<- ", x[i+1])
    x[i]   <- paste0(lhs, " ", rhs)
    x[i+1] <- "# AUTO-FIX: joined broken assignment from previous line"
    joined <- joined + 1L
    i <- i + 2L
  } else {
    i <- i + 1L
  }
}
if (joined) message("Joined ", joined, " broken assignment line(s).")

# 2) (Optional) If you still have the lone brace at 1000, comment it
if (length(x) >= 1000 && grepl("^\\s*\\}\\s*$", x[1000])) {
  x[1000] <- "# AUTO-FIX removed stray '}' here"
  message("Commented lone '}' at line 1000.")
}

write_lines(x)

# 3) Parse check + helpful message
pe <- try(parse("R/surroNMA.R", keep.source = TRUE), silent = TRUE)
if (!inherits(pe, "try-error")) {
  message("✓ File parses cleanly after fixes.")
} else {
  msg <- conditionMessage(attr(pe, "condition"))
  message("✗ Still has a parse error:\n", msg)
  message("Tip: if it says unexpected '}', run your brace fixer again near the reported line.")
}
