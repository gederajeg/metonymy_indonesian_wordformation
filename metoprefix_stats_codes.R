# load the dataset
load("gpwrajeg_2013_metoprefix.RData")

# load the require package
require(dplyr) # if not installed yet, then install `dplyr` by itself or together with the other tidyverse packages

# edit the zero prefix to be represented as unicode character
names(num.meto) <- gsub("\xd8", paste("\U00D8"), names(num.meto), perl = TRUE)
dframestrip$prefixes <- gsub("\xd8", paste("\U00D8"), dframestrip$prefixes, perl = TRUE)
prefixes <- gsub("\xd8", paste("\U00D8"), prefixes, perl = TRUE)
sourcetarget$prefix <- gsub("\xd8", paste("\U00D8"), sourcetarget$prefix, perl = TRUE)

# 1. R codes to perform the Shapiro-Wilk test for Normality (Primahadi Wijaya R, 2013, pp. 72-73)-------

## For the number of metonymy patterns for each prefix------

### perform the Shapiro-Wilk test
shapiro_meto <- shapiro.test(sourcetarget$m.pattern)

### get the W statistic
round(shapiro_meto$statistic, 2)

### get the p-value
round(shapiro_meto$p.value, 2)

## For the number of wordclass patterns for each prefix------

#### perform the Shapiro-Wilk test
shapiro_wclass <- shapiro.test(sourcetarget$wc.pattern)

#### get the W statistic
round(shapiro_wclass$statistic, 2)

#### get the p-value
round(shapiro_wclass$p.value, 2)

# 2. R codes to perform the descriptive statistics (mean, median, standard deviation, and interquartile range) (Primahadi Wijaya R, 2013, p. 73)-----

## For the number of metonymy patterns for each prefix-------

### mean
round(mean_meto <- mean(sourcetarget$m.pattern), 2)
### standard deviation (SD)
round(sd_meto <- sd(sourcetarget$m.pattern), 2)
### get the range values of the 1 SD above and below the mean
round(sd_1_above_meto <- mean_meto + sd_meto, 2)
round(sd_1_below_meto <- mean_meto - sd_meto, 2)

### median
round(median_meto <- median(sourcetarget$m.pattern), 2)
### IQR
round(iqr_meto <- IQR(sourcetarget$m.pattern), 2)

## For the number of wordclass patterns for each prefix-------

### mean
round(mean_wclass <- mean(sourcetarget$wc.pattern), 2)
### standard deviation (SD)
round(sd_wclass <- sd(sourcetarget$wc.pattern), 2)
### get the range values of the 1 SD above and below the mean
round(sd_1_above_wclass <- mean_wclass + sd_wclass, 2)
round(sd_1_below_wclass <- mean_wclass - sd_wclass, 2)

### median
round(median_wclass <- median(sourcetarget$wc.pattern), 2)
### IQR
round(iqr_wclass <- IQR(sourcetarget$wc.pattern), 2)

# 3. R codes to generate data for the specificity of the prefixes in terms of their target (Primahadi Wijaya R, 2013, pp. 75-76)----------

specificity_dbase <- dbase_type %>% 
  group_by(prefix) %>% 
  summarise(# for the metonymy pattern data
            n_meto_target = n_distinct(metonymy_target), 
            n_meto_source = n_distinct(metonymy_source), 
            n_meto_target_over_1 = if_else(n_meto_target > 1, TRUE, FALSE), 
            pref_with_meto_target_exceeds_source = if_else(n_meto_source < n_meto_target, TRUE, FALSE),
            # for the wordclass pattern data
            n_wclass_target = n_distinct(word_class_target), 
            n_wclass_source = n_distinct(word_class_source), 
            n_wclass_target_over_1 = if_else(n_wclass_target > 1, TRUE, FALSE), 
            pref_with_wclass_target_exceeds_source = if_else(n_wclass_source < n_wclass_target, TRUE, FALSE))

specificity_metonymy_target <- specificity_dbase %>% 
  count(n_meto_target_over_1) %>% 
  mutate(perc = round(n/sum(n) * 100, 2)) %>% 
  rename(n_of_pref = n)
specificity_metonymy_target # print the table
# the data from the `specificity_metonymy_target` table above constitutes the first two bars in Figure 3 for the `Metonymy` data.

specificity_meto_target_exceeds_source <- specificity_dbase %>% 
  count(pref_with_meto_target_exceeds_source) %>% 
  mutate(perc = round(n/sum(n) * 100, 2)) %>% 
  rename(n_of_pref = n)
specificity_meto_target_exceeds_source # print the table
# the second row (i.e., the `TRUE` data point of the `25`%) from the `specificity_meto_target_exceeds_source` table above constitutes the third bar in Figure 3 for the `Metonymy` data.
# that is, it represents the `Prefixes with Target > Sources` category in the legend of Figure 3

specificity_wclass_target <- specificity_dbase %>% 
  count(n_wclass_target_over_1) %>% 
  mutate(perc = round(n/sum(n) * 100, 2)) %>% 
  rename(n_of_pref = n)
specificity_wclass_target # print the table
# the data from the `specificity_wclass_target` table above constitutes the first two bars in Figure 3 for the `WordClass` data.

specificity_wclass_target_exceeds_source <- specificity_dbase %>% 
  count(pref_with_wclass_target_exceeds_source) %>% 
  mutate(perc = round(n/sum(n) * 100, 2)) %>% 
  rename(n_of_pref = n)
specificity_wclass_target_exceeds_source # print the table
# the second row (i.e., the `TRUE` data point of the `12.5`%) from the `specificity_wclass_target_exceeds_source` table above constitutes the third bar in Figure 3 for the `WordClass` data.
# that is, it represents the `Prefixes with Target > Sources` category in the legend of Figure 3

# 4. R codes for the directionality of the metonymy (Primahadi Wijaya R, 2013, pp. 77-79)---------

## unidirectional metonymy vs bidirectional metonymy
bidirectional <- metonymy_directionality %>% 
  filter(directionality == "bi-directional") %>% # it gives 20 rows, but in fact there are only 10 metonymy relationships because the items can switch role from source to target, and vice versa.
  mutate(relationship = c("abstraction-state",
                          "action-agent",
                          "action-characteristic",
                          "action-instrument",
                          "action-patient",
                          "action-state",
                          "action-agent",
                          "action-characteristic",
                          "characteristic-entity",
                          "characteristic-entity",
                          "entity-state",
                          "group-quantity",
                          "action-instrument",
                          "action-patient",
                          "group-quantity",
                          "quantity-state",
                          "abstraction-state",
                          "action-state",
                          "entity-state",
                          "quantity-state"))
n_bidirectional <- length(unique(bidirectional$relationship))
n_unidirectional <- dim(metonymy_directionality %>% filter(directionality == "uni-directional"))[1]
(perc_for_unidirectional_pattern <- round((n_unidirectional/sum(n_bidirectional, n_unidirectional)) * 100, 2))
# the percentage is presented in Primahadi Wijaya R (2013, p. 77)


## percentage of the uni-directional metonymies that are associated with either one or more prefix (Primahadi Wijaya R, 2013, pp. 77-78)

### get the relevant data first
unidirectional_pattern <- metonymy_directionality %>% 
  filter(directionality == "uni-directional") %>% 
  left_join(select(dbase_type, metonymy_pattern, prefix), by = "metonymy_pattern") %>% # get the prefix associated with the uni-directional metonymy patterns
  group_by(metonymy_pattern) %>% 
  summarise(n_pref = n_distinct(prefix)) %>% # get the number of prefix associated with the unidirectional metonymy patterns
  arrange(desc(n_pref)) %>% 
  mutate(unidirectional_meto_with_1_pref = if_else(n_pref == 1, TRUE, FALSE)) # flag with patterns are associated only with one prefix
unidirectional_pattern # print the table; the first nine rows are use for data in Primahadi Wijaya R (2013, p. 78, Table 6)

### calculate the percentage of patterns that are only associated with one or more prefix
perc_uni_patterns_with_1_pref <- unidirectional_pattern %>% 
  count(unidirectional_meto_with_1_pref) %>% 
  mutate(perc = round(n/sum(n) * 100, 2))
perc_uni_patterns_with_1_pref # the percentages appear in the body text of the last paragraph in Primahadi Wijaya R (2013, p. 77).
# That is, 31% of the patterns are associated by at least 2 prefixes (i.e., `FALSE`); 69% of the patterns are only associated with one prefix (`TRUE`)

## skewedness of the source and target elements in the bidirectional metonymy patterns
bidirectional_skewedness <- bidirectional %>% 
  left_join(select(dbase_type, metonymy_pattern, prefix), by = "metonymy_pattern") %>% 
  distinct() # get the unique combination of metonymy source, target, metonymy pattern, relationship, and prefix

### require the `tidyr` package that is part of the `tidyverse` package
library(tidyr)

### generate Table 7 in Primahadi Wijaya R (2013, p. 79)
bidirectional_skewedness_df <- bidirectional_skewedness %>% 
  group_by(relationship, metonymy_source) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(terms_id = rep(c("term_A_as_source", "term_B_as_source"), 10)) %>% 
  select(-metonymy_source) %>% 
  tidyr::spread(terms_id, n) %>% 
  arrange(term_A_as_source, term_B_as_source) %>% 
  left_join(bidirectional_skewedness %>% 
              group_by(relationship) %>% 
              summarise(n_pref = n_distinct(prefix)),
            by = "relationship") %>% # join the prefix summary data for each relationship
  mutate(is_skewed = if_else(term_A_as_source == term_B_as_source, 
                             FALSE, # balanced use of the terms A and B as both source and target
                             TRUE # one of the terms in the relationship has more number of occurrence as the source of the bidirectional metonymies
                             )) %>% 
  arrange(is_skewed, desc(n_pref), relationship)

bidirectional_skewedness_df # print the table for Table 7

### get the percentage of the skewed relationship of the bidirectional metonymy patterns
### the percentages appear in the body-text above Table 6 (Primahadi Wijaya R, 2013, p. 78)
perc_skewed_bidirectional <- bidirectional_skewedness_df %>% 
  count(is_skewed) %>% 
  mutate(perc = round(n/sum(n) * 100, 2))

