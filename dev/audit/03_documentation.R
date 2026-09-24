# ============================================================
# 03_documentation.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-24
#
# Checks the documentation of every exported function: that each one
# says what it returns, that the returned value is not copied from
# another function, that the arguments of the documentation and of the
# function are the same, that the cross-references point to topics that
# exist, and that every DOI resolves and belongs to the reference that
# cites it. Needs an internet connection for the DOIs.
#
# Output (dev/audit/):
#   documentation.csv    (one row per problem found)
# ============================================================

rm(list = ls())

library(here)
library(tools)

pkgload::load_all(here::here(), quiet = TRUE)

problems <- data.frame(
  topic = character(0), check = character(0), detail = character(0),
  stringsAsFactors = FALSE
)
add <- function(topic, check, detail) {
  problems <<- rbind(problems, data.frame(
    topic = topic, check = check, detail = detail,
    stringsAsFactors = FALSE
  ))
}

files <- list.files(here::here("man"), pattern = "\\.Rd$", full.names = TRUE)
rd <- lapply(files, tools::parse_Rd)
names(rd) <- basename(files)

# A dataset documents its format, not a returned value or an example
is_data <- function(x) {
  any(vapply(x, function(el) identical(attr(el, "Rd_tag"), "\\docType"), logical(1))) &&
    grepl("data", paste(unlist(x), collapse = " "))
}

section <- function(x, tag) {
  out <- character(0)
  for (el in x) {
    if (identical(attr(el, "Rd_tag"), tag)) {
      out <- c(out, paste(unlist(el), collapse = ""))
    }
  }
  out
}

exported <- sort(getNamespaceExports("netmem"))
aliases <- unlist(lapply(rd, section, "\\alias"))
topics <- unlist(lapply(rd, function(x) section(x, "\\name")))

#### Every exported function is documented, says what it returns and has an example ####

for (f in exported) {
  if (!any(trimws(aliases) == f)) {
    add(f, "not documented", "no Rd file has this alias")
  }
}

values <- character(0)
for (nm in names(rd)) {
  topic <- trimws(section(rd[[nm]], "\\name"))
  value <- trimws(paste(section(rd[[nm]], "\\value"), collapse = " "))
  if (is_data(rd[[nm]])) next
  if (identical(value, "") && length(section(rd[[nm]], "\\usage")) > 0) {
    add(topic, "no value", "the documentation does not say what the function returns")
  }
  if (!identical(value, "")) values[topic] <- value
  if (length(section(rd[[nm]], "\\examples")) == 0 && length(section(rd[[nm]], "\\usage")) > 0) {
    add(topic, "no example", "the documentation has no example")
  }
}

# The same sentence in two functions is a sign of a copied block
repeated <- values[duplicated(values) | duplicated(values, fromLast = TRUE)]
for (topic in names(repeated)) {
  twins <- setdiff(names(values)[values == repeated[[topic]]], topic)
  add(topic, "repeated value", paste("the same as", paste(twins, collapse = ", ")))
}

#### The arguments of the documentation and of the function are the same ####

for (f in exported) {
  file <- names(rd)[vapply(rd, function(x) any(trimws(section(x, "\\alias")) == f), logical(1))]
  if (length(file) == 0) next
  block <- rd[[file[1]]]
  # Several functions can share a help page, which documents the arguments of
  # all of them
  shared <- intersect(trimws(section(block, "\\alias")), exported)
  args_fun <- unique(unlist(lapply(shared, function(g) names(formals(get(g, envir = asNamespace("netmem")))))))
  documented <- character(0)
  for (el in block) {
    if (identical(attr(el, "Rd_tag"), "\\arguments")) {
      for (item in el) {
        if (identical(attr(item, "Rd_tag"), "\\item")) {
          documented <- c(documented, trimws(paste(unlist(item[[1]]), collapse = "")))
        }
      }
    }
  }
  documented <- unlist(strsplit(documented, ",\\s*"))
  missing <- setdiff(setdiff(args_fun, "..."), documented)
  extra <- setdiff(documented, c(args_fun, "..."))
  if (length(missing) > 0) {
    add(f, "argument not documented", paste(missing, collapse = ", "))
  }
  if (length(extra) > 0) {
    add(f, "argument documented but not in the function", paste(extra, collapse = ", "))
  }
}

#### The cross-references point to topics that exist ####

for (nm in names(rd)) {
  topic <- trimws(section(rd[[nm]], "\\name"))
  text <- paste(unlist(rd[[nm]]), collapse = " ")
  links <- regmatches(text, gregexpr("\\\\link\\{[^}]+\\}|\\\\code\\{\\\\link\\{[^}]+\\}\\}", text))[[1]]
  links <- gsub(".*\\\\link\\{|\\}.*", "", links)
  for (target in unique(links)) {
    if (!(target %in% trimws(aliases)) && !(target %in% trimws(topics))) {
      add(topic, "broken cross-reference", target)
    }
  }
}

#### Every DOI resolves, and its title matches the reference that cites it ####

text_all <- lapply(rd, function(x) paste(unlist(x), collapse = " "))
dois <- list()
for (nm in names(rd)) {
  found <- regmatches(text_all[[nm]], gregexpr("\\\\doi\\{[^}]+\\}", text_all[[nm]]))[[1]]
  found <- gsub("\\\\doi\\{|\\}", "", found)
  for (d in found) dois[[d]] <- c(dois[[d]], trimws(section(rd[[nm]], "\\name")))
}

# The references of each Rd, to compare the title of the DOI with the citation
references <- lapply(rd, function(x) paste(section(x, "\\references"), collapse = " "))

common <- c(
  "the", "and", "of", "in", "for", "with", "from", "network", "networks",
  "social", "analysis", "a", "an", "on", "to", "by", "its", "their"
)

for (d in names(dois)) {
  url <- paste0("https://api.crossref.org/works/", utils::URLencode(d, reserved = TRUE))
  answer <- try(suppressWarnings(readLines(url, warn = FALSE)), silent = TRUE)
  if (inherits(answer, "try-error")) {
    add(paste(dois[[d]], collapse = ", "), "doi does not resolve", d)
    next
  }
  title <- regmatches(answer, regexpr("\"title\":\\[\"[^\"]*\"", answer))
  if (length(title) == 0) next
  title <- tolower(gsub("\"title\":\\[\"|\"$", "", title))
  words <- setdiff(unlist(strsplit(gsub("[^a-z ]", " ", title), "\\s+")), c(common, ""))
  words <- words[nchar(words) > 3]
  for (topic in unique(dois[[d]])) {
    file <- names(rd)[trimws(unlist(lapply(rd, function(x) section(x, "\\name")))) == topic]
    cited <- tolower(references[[file[1]]])
    hits <- sum(vapply(words, function(w) grepl(w, cited, fixed = TRUE), logical(1)))
    # A citation that shares almost no word with the title of the DOI is
    # probably pointing to another article
    if (length(words) > 3 && hits / length(words) < 0.4) {
      add(topic, "doi and citation disagree", paste0(d, " is '", title, "'"))
    }
  }
}

#### Record ####

write.csv(problems, here::here("dev", "audit", "documentation.csv"), row.names = FALSE)

problems
