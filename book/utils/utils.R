###### Custom script:
######
###### - get the Rd files, convert them to markdown in "docs/docs/reference" and
######   put them in "mkdocs.yaml"
###### - run the examples in the "Reference" files
library(yaml)
library(here)
library(marginaleffects)


deploy_book <- function() {
  pkg_ver <- packageVersion("marginaleffects")
  num_periods <- length(gregexpr("\\.", pkg_ver)[[1]])
  if (num_periods == 2) {
    system("make deploy")
  } else {
    system("make deploydev")
  }
}


link_function_docs = function() {
  quarto_files = c(Sys.glob("book/*.qmd"), Sys.glob("book/articles/*.qmd"))
  for (i in seq_along(quarto_files)) {
    qf = readLines(quarto_files[i], warn = FALSE)
    fun_names = c(
      "avg_predictions", "avg_comparisons", "avg_slopes", "predictions", "comparisons", "slopes", "marginal_means",
      "hypotheses", "datagrid", "datagridcf", "inferences", "plot_predictions",
      "plot_comparisons", "plot_slopes", "posterior_draws")
    for (fn in fun_names) {
      ln = gsub("avg_", "", fn)
      tar = sprintf(" [`%s()`](reference/%s.html) ", fn, ln)
      src = sprintf("\\s`%s`\\s", fn)
      qf = gsub(src, tar, qf)
      src = sprintf("\\s`%s\\(\\)`\\s", fn)
      qf = gsub(src, tar, qf)
    }
    writeLines(qf, quarto_files[i])
  }
}


get_quarto_yaml = function(pdf = FALSE, dev = FALSE) {
  yml = read_yaml(here::here("book/utils/_quarto.yml"))
  if (isTRUE(dev)) {
    yml$book[["title"]] = paste0("The Marginal Effects Zoo (", packageVersion("marginaleffects"), ")")
  } else {
    yml$book[["title"]] = paste0(
      "The Marginal Effects Zoo (",
      gsub("^(\\d+\\.\\d+\\.\\d+).*", "\\1", packageVersion("marginaleffects")),
      ")")
  }
  if (isTRUE(pdf)) {
    idx = sapply(yml$book$chapters, function(x) x$part)
    idx = grep("functions.qmd", idx)
    yml$book$chapters[[idx]] = NULL
  }
  write_yaml(yml, here::here("book/_quarto.yml"))
  yml = readLines(here::here("book/_quarto.yml"))
  yml = gsub("@", "\\@", yml)
  yml = gsub(": yes$", ": true", yml)
  writeLines(yml, here::here("book/_quarto.yml"))
}


get_news = function() {
  tmp = tempfile()
  url = "https://raw.githubusercontent.com/vincentarelbundock/marginaleffects/main/NEWS.md"
  download.file(url, tmp, quiet = TRUE)
  new = readLines(tmp)
  new = gsub("\\@([\\w|-]+)", "[\\\\@\\1](https://github.com/\\1)", new, perl = TRUE)
  new = gsub("#(\\d+)", "[#\\1](https://github.com/vincentarelbundock/marginaleffects/issues/\\1)", new, perl = TRUE)
  new = gsub("(^## .*)", "\\1 {.unnumbered}", new)
  new = gsub("^## marginaleffects", "## ", new)
  writeLines(new, here::here("book/articles/NEWS.qmd"))
  unlink(tmp)
}


rd2qmd = function(src) {
  # Rd -> html
  rd = tools::parse_Rd(here(src))
  rd = gsub("\\eqn\\{.*\\}\\{.*\\}", "\\$\\1\\$", rd)
  tmp_html = paste0(tempfile(), ".html")
  tools::Rd2HTML(rd, out = tmp_html)

  # superfluous header and footer
  tmp = readLines(tmp_html)
  tmp = tmp[(grep("</table>$", tmp)[1] + 1):length(tmp)]
  tmp = tmp[seq_len(which("</div>" == tmp) - 3)]

  # first column (odd entries) of table in Arguments should not be wrapped
  idx = grep("<td>", tmp)
  idx = idx[seq_along(idx) %% 2 == 1]
  tmp[idx] = sub("<td>", '<td style = "white-space: nowrap; font-family: monospace;>"', tmp[idx])

  # math in Equivalence section
  idx = grepl("<.code", tmp)

  # examples: evaluate code blocks (assume examples are always last)
  idx = which(tmp == "<h3>Examples</h3>")
  if (length(idx) == 1) {
    ex = tmp[(idx + 1):length(tmp)]
    ex = gsub("<.*>", "", ex)
    ex = gsub("&lt;", "<", ex)
    ex = gsub("&gt;", ">", ex)
    ex = gsub("&gt;", ">", ex)
    ex = ex[!grepl("## Not run:", ex)]
    ex = ex[!grepl("## End", ex)]
    tmp = c(tmp[2:idx], "```{r}", "library(marginaleffects)", ex, "```")
  }

  # title
  funname = tools::file_path_sans_ext(basename(src))
  if (!is.null(title)) {
    tmp = tmp[!grepl("h1", tmp)]
    tmp = c(paste("#", funname, "{.unnumbered}\n"), tmp)
  }

  # Fix title level (use ## and not <h2> so that the TOC can be generated by
  # mkdocs)
  tmp = gsub("<h2[^>]*>", "", tmp, perl = TRUE)
  tmp = gsub("<.h2>", "", tmp)
  tmp = gsub("<h3>", "## ", tmp)
  tmp = gsub("</h3>", "", tmp)

  # write to file
  fn = file.path(here("book/reference"), sub("Rd$", "qmd", basename(src)))
  writeLines(tmp, con = fn)
}






# get_title <- function(file_path) {
#   text <- readLines(file_path)
#   title_line <- grep("\\\\title\\{.*\\}", text)
#   if (length(title_line) == 0) return(NULL)
#   title_text <- sub("\\\\title\\{(.*?)\\}", "\\1", text[title_line])
#   return(title_text)
# }


# # Run all
# message("Converting Rd files to markdown...\n")
# funs <- c("predictions", "comparisons", "slopes", "marginal_means", "hypotheses",
#           "datagrid", "datagridcf",
#           "inferences", "plot_predictions", "plot_comparisons", "plot_slopes", "posterior_draws")
# for (f in funs) {
#   r <- paste0("~/repos/marginaleffects/man/", f, ".Rd")
#   try(rd2qmd(r), silent = TRUE)
# }
