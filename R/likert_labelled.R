

likert_labelled <- function(ds, mid_level, ranked_levels, group,
                            label_minimum = 0.05, group_arrange = 'positive',
                            cat_neutral = TRUE) {

  #' Produces likert plots with labels
  #'
  #' The main function for the \code{\link{precisionlikert}} package.
  #'
  #' @param ds a datafarme with groups as rows and levels as columns
  #' @param ranked_levels an character vector of Likert levels, in desired order
  #' @param mid_level the neutral or middle category of the scale
  #' @param group a character giving name of grouping variable (y-axis of likert plot)
  #' @param label_minimum the minimum proportion for which likert plot will be labelled
  #' @param group_arrange a string, either "positive" (default) or "negative" to indicate vertical arrangement of groups within the plot
  #' @param cat_neutral logical; default TRUE. If FALSE then there is no true neutral category; mid_level specifies where alignment will happen
  #' @return None
  #' @examples
  #' data("country_outlook")
  #' ranked_levels <- factor(x = c('Much.worse', 'Somewhat.worse', 'Almost.the.same', 'Somewhat.better', 'Much.better'), levels = c('Much.worse', 'Somewhat.worse', 'Almost.the.same', 'Somewhat.better', 'Much.better'))
  #' mid_level <- 'Almost.the.same'
  #' group <- "Country"
  #' likert_labelled(ds, mid_level, ranked_levels, group)
  #' @export


  #---------- Separates the positive and negative responses based on mid-level category ----------#

  if (cat_neutral) {
    ds[ , mid_level] <- ds[ , mid_level]/2

  }

  upper_levels_indices <- match(mid_level, colnames(ds)) : match(last(ranked_levels), colnames(ds))

  lower_levels_indices <- if (cat_neutral) {
    match(first(ranked_levels), colnames(ds)) : match(mid_level, colnames(ds))
  } else {
    match(first(ranked_levels), colnames(ds)) : (match(mid_level, colnames(ds)) - 1)
  }

  #---------- Orders by group and by categories ----------#

  ds_rank_columns <- switch(group_arrange,
                            positive = upper_levels_indices,
                            negative = lower_levels_indices
  )

  ds$sum_for_rank <- rowSums(ds[ ,ds_rank_columns])

  group_order <- ds %>% arrange(sum_for_rank) %>% pull(!!group)

  ds_upper <- ds %>% select(!!group, upper_levels_indices) %>%
    gather(variable, value, -!!group, factor_key = TRUE) %>%
    mutate(variable = fct_rev(variable))

  ds_lower <- ds %>% select(!!group, lower_levels_indices) %>%
    gather(variable, value, -!!group, factor_key = TRUE)

  ds_upper[ , group] <- factor(ds_upper[ , group], levels = group_order)
  ds_lower[ , group] <- factor(ds_lower[ , group], levels = group_order)


  #---------- Calculates the relative label positions ----------#

  n_lags <- 1:floor(length(ranked_levels)/2)
  max_digits <- nchar(max(n_lags))
  lag_names <- sprintf("lag%0*d", max_digits, n_lags)
  lag_functions <- setNames(sprintf("dplyr::lag(., %d, default = 0)", n_lags), lag_names)


  ds_labels_upper <- ds_upper %>%
    group_by(!!sym(group)) %>%
    mutate_at(vars(value), funs_(lag_functions)) %>%
    ungroup() %>%
    mutate(pos  = value/2 + rowSums(select(., contains("lag"))))

  ds_labels_lower <-  ds_lower %>%
    arrange(desc(variable)) %>%
    group_by(!!sym(group))  %>%
    mutate_at(vars(value), funs_(lag_functions)) %>%
    ungroup() %>%
    mutate(pos  = 0 - (value/2 + rowSums(select(., contains("lag")))))

  if (cat_neutral) {

    ds_labels_upper <- ds_labels_upper %>%
      mutate(pos = ifelse(variable == mid_level, 0, pos),
             value = ifelse(variable == mid_level, value*2, value))

    ds_labels_lower <-  ds_labels_lower %>%
      mutate(pos = ifelse(variable == mid_level, 0, pos),
             value = ifelse(variable == mid_level, value*2, value)) %>%
      arrange(variable)

    ds_labels <- rbind(ds_labels_lower, ds_labels_upper) %>%
      distinct() %>%
      filter(value >= label_minimum)
  }

  if (!cat_neutral) {

    ds_labels <- rbind(ds_labels_lower, ds_labels_upper) %>%
      distinct() %>%
      filter(value >= label_minimum)
  }

  #---------- Define colour palette ----------#

  N <- length(ranked_levels)

  likert_pal <- colorRampPalette(c('#D40A42', '#D9D9D9', '#370AD4'), alpha = TRUE)(N)
  names(likert_pal) <- ranked_levels

  #---------- Produce the plot ----------#

      g <- ggplot() +
      geom_bar(data = ds_lower, aes(x = get(group), y = -value, fill = variable),
               position = "stack", stat = "identity") +
      geom_bar(data = ds_upper, aes(x = get(group), y = value, fill = variable),
               position = "stack", stat = "identity") +
      coord_flip() +
      geom_hline(yintercept = 0, color = c("white"))  +
      scale_fill_manual(values = likert_pal, breaks = ranked_levels, name = '') +
      geom_text(data = ds_labels, aes(label =  sprintf("%.2f", value), x = get(group), y = pos)) +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(x = "", y = "")

  return(g)

}






