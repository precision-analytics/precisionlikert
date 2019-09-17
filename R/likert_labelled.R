

likert_labelled <- function(ds, mid_level, ranked_levels, group,
                            label_minimum = 0.05, group_arrange = 'positive',
                            cat_neutral = TRUE) {

  #' Produces likert plots with labels
  #'
  #' The main function for the \code{\link{precisionlikert}} package.
  #'
  #' @param ds a datafarme with groups as rows and levels as columns
  #' @param ranked_levels an ordered factor of all levels
  #' @param mid_ldevel the neutral or middle category
  #' @param group a factor or character vector of all groups (y-axis of likert plot)
  #' @param label_minimum the proportion for which likert plot will be labelled
  #' @param group_arrange a character of positive (default) or negative
  #' @param cat_neutral default TRUE but if FALSE then there is no true neutral category; mid_level specifies where alignment will happen
  #' @return None
  #' @examples
  #' data("country_outlook")
  #' ranked_levels <- factor(x = c('Much.worse', 'Somewhat.worse', 'Almost.the.same', 'Somewhat.better', 'Much.better'), levels = c('Much.worse', 'Somewhat.worse', 'Almost.the.same', 'Somewhat.better', 'Much.better'))
  #' mid_level <- 'Almost.the.same'
  #' group <- "Country"
  #' likert_labelled(ds, mid_level, ranked_levels, group)
  #' @export


  #---------- Separates the positive and negative responses based on mid-level category ----------#

  if(cat_neutral) {
    ds[ , mid_level] <- ds[ , mid_level]/2
    upper_levels <- which(colnames(ds)==mid_level):which(colnames(ds)==last(ranked_levels) )
    lower_levels <- which(colnames(ds)==first(ranked_levels)):which(colnames(ds)==mid_level)
  }

  if(!cat_neutral) {
    #ds[ , mid_level] <- ds[ , mid_level]/2
    upper_levels <- which(colnames(ds)==mid_level):which(colnames(ds)==last(ranked_levels) )
    lower_levels <- which(colnames(ds)==first(ranked_levels)):(which(colnames(ds)==mid_level) - 1)
  }

  #---------- Orders by group and by categories ----------#

  ds_upper <- ds %>% select(!!group, upper_levels) %>%
    gather(variable, value, -!!group, factor_key = TRUE)

  ds_lower <- ds %>% select(!!group, lower_levels) %>%
    gather(variable, value, -!!group, factor_key = TRUE)

  ds_upper$variable <- fct_rev(ds_upper$variable)

  if(group_arrange == "positive") {
    ds$sum_for_rank <- rowSums(ds[ ,upper_levels])
  }
  if(group_arrange == "negative") {
    ds$sum_for_rank <- rowSums(ds[ ,lower_levels])
  }

  group_order <- ds %>% arrange(sum_for_rank) %>% pull(!!group)
  ds_upper[ , group] <- factor(ds_upper[ , group], levels = group_order)
  ds_lower[ , group] <- factor(ds_lower[ , group], levels = group_order)

  names_lower_levels <- colnames(ds[ ,lower_levels])
  names_upper_levels <- colnames(ds[ ,upper_levels])



  #---------- Calculates the relative label positions ----------#

  n_lags <- 1:floor(length(ranked_levels)/2)

  lag_names <- paste("lag", formatC(n_lags, width = nchar(max(n_lags)),
                                    flag = "0"), sep = "")

  lag_functions <- setNames(paste("dplyr::lag(., ", n_lags, ', default=0',")"), lag_names)

  if(cat_neutral) {

    ds_labels_upper <- ds_upper %>% group_by(!!sym(group)) %>%
      mutate_at(vars(value), funs_(lag_functions)) %>%
      ungroup() %>%
      mutate(pos  = value/2 + rowSums(select(., contains("lag")))) %>%
      mutate(pos = ifelse(variable == mid_level, 0, pos))

    ds_labels_lower <-  ds_lower %>% arrange(desc(variable)) %>% group_by(!!sym(group))  %>%
      mutate_at(vars(value), funs_(lag_functions)) %>%
      ungroup() %>%
      mutate(pos  = 0 - (value/2 + rowSums(select(., contains("lag"))))) %>%
      mutate(pos = ifelse(variable == mid_level, 0, pos))

    ds_labels <- rbind(ds_labels_lower, ds_labels_upper) %>% distinct() %>%
      mutate(value = ifelse(variable == mid_level, value*2, value)) %>%
      filter(value >= label_minimum)
  }


  if(!cat_neutral) {

    ds_labels_upper <- ds_upper %>% group_by(!!sym(group)) %>%
      mutate_at(vars(value), funs_(lag_functions)) %>%
      ungroup() %>%
      mutate(pos  = value/2 + rowSums(select(., contains("lag")))) #%>%
    #mutate(pos = ifelse(variable == mid_level, 0, pos))

    ds_labels_lower <-  ds_lower %>% arrange(desc(variable)) %>% group_by(!!sym(group))  %>%
      mutate_at(vars(value), funs_(lag_functions)) %>%
      ungroup() %>%
      mutate(pos  = 0 - (value/2 + rowSums(select(., contains("lag"))))) #%>%
    #mutate(pos = ifelse(variable == mid_level, 0, pos))

    ds_labels <- rbind(ds_labels_lower, ds_labels_upper) %>% distinct() %>%
      #mutate(value = ifelse(variable == mid_level, value*2, value)) %>%
      filter(value >= label_minimum)
  }

  #---------- Define colour palette ----------#

  N <- length(ranked_levels)

  likert_pal <- colorRampPalette(c('#D40A42', '#D9D9D9', '#370AD4'), alpha = TRUE)(N)
  names(likert_pal) <- ranked_levels

  #---------- Produce the plot ----------#

  g <- ggplot() +
    geom_bar(data = ds_lower, aes(x = !!sym(group), y = -value, fill = variable),
             position="stack", stat="identity") +
    geom_bar(data = ds_upper, aes(x = !!sym(group), y = value, fill = variable),
             position="stack", stat="identity") +
    coord_flip() + theme_fivethirtyeight() +
    geom_hline(yintercept = 0, color =c("white"))  +
    scale_fill_manual(values = likert_pal,
                      breaks = ranked_levels,
                      name = '') +
    geom_text(aes(label =  sprintf("%.2f", value), x = !!sym(group), y = pos), data = ds_labels)

  return(g)
}






