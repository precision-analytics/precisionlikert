
likert_labelled <- function(ds, mid_level, ranked_levels, group,
                           label_minimum = 0.05, group_arrange = 'positive') {

  #' Produces likert plots with labels
  #'
  #' The one main function for the \code{\link{precisionlikert}} package.
  #'
  #' @param ds a datafarme with groups as rows and levels as columns
  #' @param ranked_levels an ordered factor of all levels
  #' @param mid_ldevel the neutral or middle category
  #' @param group a factor or character vector of all groups (y-axis of likert plot)
  #' @param label_minimum the proportion for which likert plot will be labelled
  #' @param group_arrange a character of positive (default) or negative
  #' @return None
  #' @examples
  #' data("country_outlook")
  #' ranked_levels <- factor(x = c('Much.worse', 'Somewhat.worse', 'Almost.the.same', 'Somewhat.better', 'Much.better'), levels = c('Much.worse', 'Somewhat.worse', 'Almost.the.same', 'Somewhat.better', 'Much.better'))
  #' mid_level <- 'Almost.the.same'
  #' group <- 'Country'
  #' likert_labeled(ds, mid_level, ranked_levels, group)
  #' @export


  #---------- Separates the positive and negative responses from the mid-level ----------#

  ds[ , mid_level] <- ds[ , mid_level]/2
  upper_levels <- which(colnames(ds)==mid_level):which(colnames(ds)==last(ranked_levels) )
  lower_levels <- which(colnames(ds)==first(ranked_levels)):which(colnames(ds)==mid_level)

  ds_upper <- ds %>% select(Country, upper_levels) %>%
    melt(id.var='Country') %>% mutate(value = round(value,2))

  ds_lower <- ds %>% select(Country, lower_levels) %>%
    melt(id.var='Country') %>% mutate(value = round(value,2))

  ds_upper$variable <- fct_rev(ds_upper$variable)

  if(group_arrange == "positive") {
    ds$sum_for_rank <- rowSums(ds[ ,upper_levels])
  }
  if(group_arrange == "negative") {
    ds$sum_for_rank <- rowSums(ds[ ,lower_levels])
  }

  group_order <- ds %>% arrange(sum_for_rank) %>% .$Country
  ds_upper[ , group] = factor(ds_upper[ , group], levels = group_order)
  ds_lower[ , group] = factor(ds_lower[ , group], levels = group_order)

  names_lower_levels <- colnames(ds[ ,lower_levels])
  names_upper_levels <- colnames(ds[ ,upper_levels])


  #---------- Position the labels ----------#

  n_lags <- 1:floor(length(ranked_levels)/2)

  lag_names <- paste("lag", formatC(n_lags, width = nchar(max(n_lags)), flag = "0"),
                     sep = "")

  lag_functions <- setNames(paste("dplyr::lag(., ", n_lags, ', default=0',")"), lag_names)


  ds_labels_upper <- ds_upper %>% group_by(Country)  %>%
    mutate_at(vars(value), funs_(lag_functions)) %>%
    ungroup() %>%
    mutate(pos  = value/2 + rowSums(select(., contains("lag")))) %>%
    mutate(pos = ifelse(variable == 'Almost.the.same', 0, pos))


  ds_labels_lower <-  ds_lower %>% arrange(desc(variable)) %>% group_by(Country)  %>%
    mutate_at(vars(value), funs_(lag_functions)) %>%
    ungroup() %>%
    mutate(pos  = 0 - (value/2 + rowSums(select(., contains("lag"))))) %>%
    mutate(pos = ifelse(variable == 'Almost.the.same', 0, pos))


  ds_labels <- rbind(ds_labels_lower, ds_labels_upper) %>% distinct() %>%
    filter(value >= label_minimum)


  #---------- Define colour palette ----------#

  N <- length(ranked_levels)
  likert_pal <- colorRampPalette(c('#D40A42', '#D9D9D9', '#370AD4'), alpha = TRUE)(N)
  names(likert_pal) <- ranked_levels


  #---------- Produce the plot ----------#

  g <- ggplot() +
    geom_bar(data = ds_lower, aes(x = Country, y = -value, fill = variable),
             position="stack", stat="identity") +
    geom_bar(data = ds_upper, aes(x = Country, y = value, fill = variable),
            position="stack", stat="identity") +
    coord_flip() + theme_fivethirtyeight() +
    geom_hline(yintercept = 0, color =c("white"))  +
    scale_fill_manual(values = likert_pal,
                      breaks = ranked_levels,
                      name = '') +
    geom_text(aes(label =  sprintf("%.2f", value), x = Country, y = pos), data = ds_labels)

return(g)
}






