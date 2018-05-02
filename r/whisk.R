#' Produce stargazer-like tables from tidied models
#'
#' Takes a data.frame/tibble with at least a list-column named "model," where
#' each row represents one model and returns a stargazer like table.
#' Additional columns are added to the base of the table.
#'
#' @param x a data.frame containing at least list-column named "model" which
#' should be the result of \code{broom::tidy}.
#' @param col.names Character vector of column names. If empty, no column names.
#' @param model.numbers Logical. Add model numbers? Default T,
#' @param format Format of table: `markdown`, `latex`, `html`. Default markdown.
#' @param align How should cells be aligned, `l`, `c`, `r`. Default `l`
#' @param booktabs Use booktabs? Default T, they look nicer.
#' @param digits Same as \code{knitr::kable}, maximum number of digits for
#' numeric columns, passed to \code{base::round()}. However,\code{whisk} rounds
#' prior to passing to kable.
#' @param ... Extra arguments to be passed to kable.
#'
#' @return A kable table that can be augmented using kableExtra.
#'
#' @importFrom kableExtra kable `%>%`
#' @importFrom purrr map
#'
#' @export
whisk <- function(m, col.names = NULL, model.numbers = T, format = "markdown",
                  align = "l", booktabs = T, digits = 2, ...) {
  m %>%
    whip(digits, format) %>%
    {if(format == 'latex') mutate_all(., linebreak) else .} %>%
    kable(format = format, booktabs = booktabs, digits = digits, escape = F,
          col.names = c("", {if(model.numbers) paste0("(", 1:nrow(m), ")") else rep("", nrow(m))}),
          align = align, ...) %>%
    {if(!is.null(col.names) & format == "latex") add_header_above(., col.names, align = align, line = F) else .} %>%
    {if(format == 'latex') row_spec(., length(unique(unlist(map(m$model, ~ .x$term)))), hline_after = T) else .}
}

#' Produce stargazer-like data.frame from tidied models
#'
#' Internal function to mutate a data.frame (as specified in
#' \code{whisk::whisk}) into a dataframe shaped like a stargazer table.
whip <- function(m, digits, format) {
  m %>%
    # round all numeric columns that are not included in model
    mutate_if(is.numeric, funs(round(., digits = digits))) %>%
    mutate(
      model_id = 1:n(),
      model = map(model, ~ mutate(.x,
                      estimate = round(estimate, digits),
                      std.error = round(std.error, digits),
                      p.value = p_to_stars(p.value),
                      estimate = paste0(estimate, p.value,
                                        ifelse(format == "latex", "\n", " "),
                                        "(", std.error, ")")
                    ) %>%
                    select(term, estimate) %>%
                    gather(stat, val, -term) %>%
                    select(-stat) %>%
                    spread(term, val))
    ) %>%
    unnest() %>%
    gather(term, val, -model_id) %>%
    spread(model_id, val) %>%
    left_join(
      data_frame(
        term = c(unique(unlist(map(m$model, ~ .x$term))), colnames(m))
      ) %>%
        mutate(order = 1:n())
    ) %>%
    arrange(order) %>%
    select(-order)
}

#' Convert p-value to stars.
#'
#' @param p a numeric vector representing the p-value
#' @param levels decreasing significance levels; default:
#' \code{c(0.01, 0.05, 0.1)}
#' @param stars significance indicators for each level, default:
#' \code{c("***", "**", "*")}
#'
#' @return A character vector.
#'
#' @importFrom dplyr case_when
#'
#' @export
p_to_stars <- function(p, levels = c(0.01, 0.05, 0.1),
                       stars = c("***", "**", "*")) {
  case_when(
    p < levels[1] ~ stars[1],
    p < levels[2] ~ stars[2],
    p < levels[3] ~ stars[3],
    p >= levels[3] ~ ""
  )
}

