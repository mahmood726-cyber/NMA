# tools/fix_specific_braces.R
# Applies two surgical fixes:
#   (a) remove extra '}' at line 599
#   (b) fix early closing brace inside export_cinema()

stopifnot(file.exists("DESCRIPTION"))
stopifnot(file.exists("R/surroNMA.R"))

backup <- sprintf("R/surroNMA.R.backup-%s", format(Sys.time(), "%Y%m%d-%H%M%S"))
file.copy("R/surroNMA.R", backup, overwrite = TRUE)
message("Backup: ", backup)

read_lines  <- function() readLines("R/surroNMA.R", warn = FALSE)
write_lines <- function(x) writeLines(x, "R/surroNMA.R", useBytes = TRUE)

comment_out_brace_line <- function(x, ln) {
  if (ln >= 1 && ln <= length(x)) {
    if (grepl("^\\s*\\}\\s*$", x[ln])) {
      x[ln] <- "# AUTO-FIX removed stray '}' that was here"
    } else {
      x[ln] <- sub("\\}", " # AUTO-FIX removed one '}' ", x[ln])
    }
  }
  x
}

# (a) remove extra } at line 599 (from your parser error)
x <- read_lines()
if (length(x) >= 599 && grepl("\\}", x[599])) {
  x <- comment_out_brace_line(x, 599)
  message("Applied: removed stray '}' at line 599")
} else {
  message("Note: no '}' detected at line 599 (skipped).")
}

# (b) fix export_cinema(): delete early '}' that appears right after pw <- pw[pw$t1 != pw$t2,]
#    and before the lines that define `get <-`, `pw$diff_mean <-`, `write.csv`, `invisible(pw)`
start <- grep("^\\s*export_cinema\\s*<-\\s*function\\s*\\(", x)
if (length(start)) {
  i0 <- start[1]
  # search a small window after the start for the pattern and early brace
  win <- i0:min(length(x), i0 + 60)
  i_pw  <- grep("^\\s*pw\\s*<-\\s*pw\\s*\\[.*\\]$", x[win])
  i_get <- grep("^\\s*get\\s*<-\\s*function\\s*\\(", x[win])
  i_inv <- grep("^\\s*invisible\\s*\\(pw\\)\\s*$", x[win])
  i_br  <- grep("^\\s*\\}\\s*$", x[win])  # any standalone brace in window

  if (length(i_pw) && length(i_br)) {
    # absolute indices
    ipw <- win[ i_pw[1] ]
    # any brace between ipw and either get/invisible?
    # locate the first brace after ipw
    ibr_after <- i_br[which(win[i_br] > ipw)]
    if (length(ibr_after)) {
      ibr_abs <- win[ibr_after[1]]
      # if that brace appears BEFORE the get/invisible lines, it's premature -> delete it
      boundary <- min(if (length(i_get)) win[i_get[1]] else Inf,
                      if (length(i_inv)) win[i_inv[1]] else Inf)
      if (!is.infinite(boundary) && ibr_abs < boundary) {
        x[ibr_abs] <- "# AUTO-FIX removed early '}' inside export_cinema()"
        message("Applied: removed early '}' in export_cinema() at line ", ibr_abs)
      }
    }
  }
} else {
  message("Note: export_cinema() not found (skipped).")
}

write_lines(x)

# Parse check and brief guidance
ok <- !inherits(try(parse("R/surroNMA.R", keep.source = TRUE), silent = TRUE), "try-error")
if (ok) {
  message("✓ R/surroNMA.R parses after targeted fixes.")
} else {
  p <- try(parse("R/surroNMA.R", keep.source = TRUE), silent = TRUE)
  msg <- if (inherits(p, "try-error")) conditionMessage(attr(p, "condition")) else "OK"
  message("ℹ Still parse error: ", msg)
  message("Tip: re-run your diagnostics (tools/diagnose_and_patch.R) or share the new line number.")
}

# Optional: quick square bracket hotspot finder
x <- read_lines()
strip <- function(xx){
  xx <- gsub('"(?:[^"\\\\]|\\\\.)*"', '""', xx, perl=TRUE)
  xx <- gsub("'(?:[^'\\\\]|\\\\.)*'", "''",  xx, perl=TRUE)
  sub("#.*$", "", xx, perl=TRUE)
}
s <- strip(x)
count <- function(p) vapply(gregexpr(p, s, perl=TRUE), function(m) ifelse(m[1]==-1,0L,length(m)), 0L)
openB  <- count("\\["); closeB <- count("\\]"); cumB <- cumsum(openB - closeB)
if (tail(cumB, 1) != 0) {
  hot <- which(cumB > 0)[1]
  rng <- pmax(1, hot-15):pmin(length(x), hot+25)
  message("Bracket imbalance persists. First hotspot around line ", hot, ". Inspect:")
  cat(paste(sprintf("%5d | %s", rng, x[rng]), collapse = "\n"), "\n", sep = "")
}
