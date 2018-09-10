#==============================================================================
# 00_functions.R
# Purpose: utils for the project on hitchhiker bills and bill editing in the US
#          Congress.
# Article: "More Effective Than We Thought: Accounting for Legislative 
#           Hitchhikers Reveals a More Inclusive and Productive Lawmaking 
#           Process."
# Journal: American Journal of Political Science
# Year: 2018
# Authors: Andreu Casas, Matt Denny, and John
#==============================================================================

# Some pairwise comparisons show up in 2 different rows but flipped. Merging
#   them into only 1 row
delete_duplicate_comp <- function(dataset) { 
  done <- NULL
  data <- dataset[1,]
  for (i in unique(dataset$version_a)) {
    for (j in unique(dataset$version_b)) {
      i <- as.character(i)
      j <- as.character(j)
      pair1 <- paste0(i,"-",j)
      pair2 <- paste0(j, "-", i)
      if (!(pair1 %in% done)) {
        done <- c(done, pair1, pair2)
        obs1 <- dataset[(dataset$version_a == i & dataset$version_b == j),]
        obs2 <- dataset[(dataset$version_a == j & dataset$version_b == i),]
        if (nrow(obs1) != 0 & nrow(obs2) != 0) {
          i_in_j <- ((obs1$n * obs1$a_in_b) + (obs2$n * obs2$b_in_a)) / (obs1$n + obs2$n)
          j_in_i <- ((obs1$n * obs1$b_in_a) + (obs2$n * obs2$a_in_b)) / (obs1$n + obs2$n)
          new_row <- data.frame(version_a = i, version_b = j, a_in_b = i_in_j,
                                b_in_a = j_in_i, n = (obs1$n + obs2$n))
          
        } else {
          new_row <- rbind(obs1, obs2)
        }
        data <- rbind(data, new_row)
      }
    }
  }
  data <- data[-1, ]
  return(data)
}

#===============================================================================
# COLOR paletter for plots: blue and orange
orange <- rgb(red = 253, green = 174, blue = 97, alpha = 200, maxColorValue = 255)
blue <- rgb(red = 43, green = 131, blue = 186, alpha = 200, maxColorValue = 255)

orange1 <- rgb(red = 253, green = 174, blue = 97, alpha = 100, maxColorValue = 255)
orange2 <- rgb(red = 253, green = 174, blue = 97, alpha = 255, maxColorValue = 255)

#===============================================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#===============================================================================
# Function that process the output of wdiff and returns the essential 
# information we need
get_wdiff_data <- function(wdiff_otuput){
  wdiff_output <- gsub("\\={2,}", "=", wdiff_output)
  wdiff_changes <- strsplit(wdiff_output, "\\=")[[1]][-1]
  wdiff_info <- wdiff_changes[length(wdiff_changes)]
  wdiff_words <- gregexpr("[0-9]{1,} words", wdiff_info)
  wdiff_words_i <- as.numeric(wdiff_words[[1]])
  wdiff_words_n <- attr(wdiff_words[[1]], "match.length")
  wdiff_words_final <- as.numeric(gsub(" [a-z]{1,} ", "", 
                                       substring(wdiff_info, wdiff_words_i, 
                                                 wdiff_words_i + wdiff_words_n)))
  wdiff_common <- gregexpr("[0-9]{1,} [0-9]{1,}% common", wdiff_info)
  wdiff_common_i <- as.numeric(wdiff_common[[1]])
  wdiff_common_n <- attr(wdiff_common[[1]], "match.length")
  wdiff_common_final_pre <- gsub(" [a-z]{1,} ", "", 
                                 substring(wdiff_info, wdiff_common_i,
                                           wdiff_common_i + wdiff_common_n))
  wdiff_common_words_final <- strsplit(wdiff_common_final_pre, " ")[[1]][1]
  wdiff_common_percs <- c(strsplit(wdiff_common_final_pre, " ")[[1]][2],
                          strsplit(wdiff_common_final_pre, " ")[[2]][2])
  wdiff_common_percs_final <- as.numeric(gsub("\\%", "", wdiff_common_percs)) / 100
  wdiff_changes <- wdiff_changes[-length(wdiff_changes)]
  comp_output <- data.frame(
    words_a = as.numeric(gsub(" [a-z]{1,} ", "", 
                              substring(wdiff_info, wdiff_words_i[1], 
                                        wdiff_words_i[1] + wdiff_words_n[1]))),
    words_b = as.numeric(gsub(" [a-z]{1,} ", "", 
                              substring(wdiff_info, wdiff_words_i[2], 
                                        wdiff_words_i[2] + wdiff_words_n[2]))),
    words_in_common = wdiff_common_words_final,
    wdiff_a_in_b = wdiff_common_percs_final[1],
    wdiff_b_in_a = wdiff_common_percs_final[2]
  )
}

#===============================================================================
# Function from the congressReuse package to clean the top and bottom 
#   procedural stuff from the bills text
get_bill_core_text <- function(bill_full_text) {
  #sec1_ind <- as.numeric(regexec("SECTION 1.", bill_full_text))
  lower_text <- tolower(bill_full_text)
  front_cut <- as.numeric(regexec("be it enacted by", lower_text))
  #if (grepl("AN ACT", bill_full_text)) {
  #  front_cut <- as.numeric(regexec("AN ACT[[:space:]]+", bill_full_text)) +
  #    attr(regexec("AN ACT[[:space:]]+", bill_full_text)[[1]], "match.length")
  #} else {
  #  front_cut <- as.numeric(regexec("A BILL", bill_full_text)) +
  #    attr(regexec("A BILL[[:space:]]+", bill_full_text)[[1]], "match.length")
  #}
  bill_no_front <- substring(bill_full_text, front_cut, nchar(bill_full_text))
  end_cut_marker <- names(which(sapply(bill_endings,
                                       function(x) grepl(x, bill_no_front)) == TRUE))
  if (length(end_cut_marker) == 0) {
    end_cut <- nchar(bill_no_front)
  } else{
    if (length(end_cut_marker > 1)) {
      matches <- sapply(end_cut_marker, function(x) regexec(x, bill_no_front))
      matches_index <- as.numeric(matches)
      end_cut <- min(matches_index) - 1
    } else {
      end_cut <- as.numeric(regexec(end_cut_marker[1], bill_no_front)) - 1
    }
  }
  bill_core_text <- substring(bill_no_front, 1, end_cut)
  bill_core_text <- gsub("\n{2,}", "\n", bill_core_text)
  bill_core_text <- gsub("\n\\s+", "\n", bill_core_text)
  #bill_core_text <- gsub("([\\()])|[[:punct:]]", "\\1", bill_core_text)
  bill_core_text <- qdap::clean(gsub("[[:punct:]]", " ", bill_core_text))
  bill_core_text <- tolower(bill_core_text)
  return(bill_core_text)
}

bill_endings <- c("<all>", "(?:Union)? Calendar No.", "Passed the [House|Senate]",
                  "Speaker of the House of Representatives[.]", "Attest:")
months <- c("January", "February", "March", "April", "May", "June", "July",
            "August", "September", "November", "December")


#===============================================================================
get_unigrams <- function(string, rm_stopw = TRUE, stopw_list = NULL,
                         rm_num = TRUE, rm_punct = TRUE, rm_shorter_than = 1,
                         unique = FALSE) {
  # This function takes a string and returns a vector of unigrams.
  if (rm_punct) {
    string <- gsub("[[:punct:]]", "", string)
  }
  if (rm_num) {
    string <- gsub("[0-9]", "", string)
  }
  unigrams <- strsplit(string, split = " ")[[1]]
  if (rm_stopw) {
    unigrams <- unigrams[which(!(unigrams %in% stopw_list))]
  }
  unigrams <- unigrams[which(nchar(unigrams) > 1)]
  if (unique) {
    return(unique(unigrams))  
  } else {
    return(unigrams)
  }
  
}

#===============================================================================
# Pulling sections from raw bill text
get_section_titles <- function(text) {
  first_mark <- gregexpr(
    "SECTION 1\\. (([A-Z]* ){1,10}|([A-Z]*. ){1,10})", text
  )
  if (as.numeric(first_mark[[1]][1]) < 0) {
    first_mark <- gregexpr(
      "Section 1\\. [A-Z]", text
    )
  }
  marks <- gregexpr("SEC\\. [0-9]{1,}\\. (([A-Z]* ){1,10}|([A-Z]*. ){1,10})", text)
  if (as.numeric(marks[[1]][1]) < 0) {
    marks <- gregexpr("Sec\\. [0-9]{1,}\\. [A-Z]", text)
  }
  if (as.numeric(marks[[1]][1]) != -1) {
    sec_title_start <- c(as.numeric(first_mark[[1]])[1], as.numeric(marks[[1]]))
    sec_title_len <- c(attr(first_mark[[1]], "match.length")[1],
                       attr(marks[[1]], "match.length"))
  } else {
    sec_title_start <- as.numeric(first_mark[[1]])
    sec_title_len <- attr(first_mark[[1]], "match.length")
  }
  sec_title_end <- sec_title_start + sec_title_len
  sec_titles <- as.character(sapply(1:length(sec_title_start), function(x)
    substring(text, sec_title_start[x], sec_title_end[x])))
  if (length(sec_title_start) > 1) {
    sec_ends <- sec_title_start[2] - 1
    j <- 2
    while (sec_ends < sec_title_start[1]) {
      j <- j + 1
      sec_ends <- sec_title_start[j] - 1
    }
    for (i in 2:length(sec_title_start)) {
      if (length(sec_title_start) > i) {
        new_sec_ends <- sec_title_start[i + 1] - 1
        z <- i + 1
        while (new_sec_ends < sec_title_start[i]) {
          z <- z + 1
          new_sec_ends <- sec_title_start[z] - 1 
        }
        sec_ends <- c(sec_ends, new_sec_ends)
      } else {
        sec_ends <- c(sec_ends, nchar(text))
      }
    }
  } else {
    sec_ends <- nchar(text)
  }
  output <- data.frame(
    title = sec_titles,
    start_i = sec_title_start,
    end_i = sec_ends,
    title_end_i = (sec_title_end - 3),
    pre_sec = substring(text, sec_title_start - 2, sec_title_start - 1)
  )
  output$delete <- ifelse(grepl("``", output$pre_sec), 1, 0)
  output <- output[which(output$delete == 0),]
  return(output)
}

#===============================================================================
rm_amendments <- function(text) {
  bill_core_text <- text
  if (grepl("&lt;DELETED&gt;", bill_core_text)) {
    opening_deleted_indeces <- gregexpr("&lt;DELETED&gt;", bill_core_text)
    opening_deleted_length <- attr(opening_deleted_indeces[[1]], "match.length")[1]
    opening_deleted_indeces <- as.numeric(opening_deleted_indeces[[1]])
    closing_deleted_indeces <- gregexpr("&lt;/DELETED&gt;", bill_core_text)
    closing_deleted_length <- attr(closing_deleted_indeces[[1]], "match.length")[1]
    closing_deleted_indeces <- as.numeric(closing_deleted_indeces[[1]])
    # Sometimes more opening <DELETED> than closing ones
    # When that it's the case, we add closing ones right before the following
    #   opening. 
    if (!(length(opening_deleted_indeces) == length(closing_deleted_indeces))) {
      for (i in 1:(length(opening_deleted_indeces) - 1)) {
        open_ind <- opening_deleted_indeces[i]
        next_open_ind <- opening_deleted_indeces[i + 1]
        closest_close_ind <- closing_deleted_indeces[i]
        if (next_open_ind < closest_close_ind) {
          new_closing_ind <- next_open_ind - 1
          # Inserting an extra closing </deleted> index
          closing_first_part <- closing_deleted_indeces[0:(i - 1)]
          closing_second_part <- closing_deleted_indeces[i:length(closing_deleted_indeces)]
          closing_deleted_indeces <- c(closing_first_part, new_closing_ind,
                                       closing_second_part)
        }
      }
    }
    # I find one case (111-S-1733) for which after running the previous loop there
    #   are still more openings <deleted> than closing ones. I checked and it
    #   is because the last opening <deleted> is at the end of the bill and it
    #   doesn't have a closing one, implying that they want to delete until the 
    #   end of the bill. I write code below to do that for these cases.
    if (length(opening_deleted_indeces) > length(closing_deleted_indeces)){
      closing_deleted_indeces <- c(closing_deleted_indeces, 
                                   nchar(bill_core_text))
    }
    # If still not same number of opening <deleted> than closing </deleted>,
    #   that means that there are more </deleted>. I checked manually (e.g. bills
    #   111-S-1017-RS and 111-S-1635-RS) and they actually want to delete the
    #   whole thing in between when there are two consecutive </deleted>.
    if (length(opening_deleted_indeces) < length(closing_deleted_indeces)) {
      for (i in 2:(length(closing_deleted_indeces) - 1)) {
        closing_ind <- closing_deleted_indeces[i]
        previous_closing_ind <- closing_deleted_indeces[i -1]
        closest_opening_ind <- opening_deleted_indeces[i]
        if (closing_ind < closest_opening_ind) {
          new_opening_ind <- previous_closing_ind + 16
          # Inserting an extra opening <deleted> index
          opening_first_part <- opening_deleted_indeces[0:(i - 1)]
          opening_second_part <- opening_deleted_indeces[i:length(opening_deleted_indeces)]
          opening_deleted_indeces <- c(opening_first_part, new_opening_ind,
                                       opening_second_part)
        }
      }
    }
    # If still more closings than openings (e.g. 111-S-3903-RS), adding a 
    #   an extra opening right after the second from the last closing
    if (length(opening_deleted_indeces) < length(closing_deleted_indeces)) {
      y <- closing_deleted_indeces[length(closing_deleted_indeces) - 1]
      opening_deleted_indeces <- c(opening_deleted_indeces, y + 16)
    }
    # If the number of opening and closing <deleted> are still different, just
    #   get rid of everything before the last <deleted>
    if (length(opening_deleted_indeces) == length(closing_deleted_indeces)){
      deleting <- data.frame(open = opening_deleted_indeces, 
                             close = closing_deleted_indeces) 
      clean_text <- substring(bill_core_text, 1, opening_deleted_indeces[1] - 1)
      for (i in 1:nrow(deleting)) {
        closing_ind <- deleting$close[i]
        if (i < nrow(deleting)) {
          next_opening <- deleting$open[i + 1] - 1
          text_to_add <- substring(bill_core_text, closing_ind, next_opening)
          text_to_add <- gsub("&lt;/DELETED&gt;", "", text_to_add)
          text_to_add <- gsub("&lt;DELETED&gt;", "", text_to_add)
        } else {
          text_to_add <- substring(bill_core_text, closing_ind, nchar(bill_core_text))
          text_to_add <- gsub("&lt;/DELETED&gt;", "", text_to_add)
          text_to_add <- gsub("&lt;DELETED&gt;", "", text_to_add)
        }
        if (!is.na(text_to_add)) {
          clean_text <- paste(clean_text, text_to_add)
        }
      }
    } else{
      last_del_tag <- max(c(opening_deleted_indeces, closing_deleted_indeces))
      clean_text <- substring(bill_core_text, last_del_tag, nchar(bill_core_text))
      clean_text <- gsub("&lt;/DELETED&gt;", "", clean_text)
      clean_text <- gsub("&lt;DELETED&gt;", "", clean_text)
    }
    
    # Now removing the stuff/text between <deleted></deleted> tags
    # initializing the new clean text with the beginning of the bill
    
    bill_core_text <- clean_text
  }
  bill_core_text <- gsub("&lt;all&gt;", "", bill_core_text)
  return(bill_core_text)
}

#===============================================================================
# LIST OF STOPWORDS:
#   + The default stopwords in the 'quanteda' pacakge
#   + Some extra stopwords: out of the 50 most frequent words in a random 
#       sample of 500 bills, the words we thought to be irrelevant. See the full
#       list of 50 and which ones we considered to be stopwords (== YES).
#     (you can find the file with the top 100 in the data directory:
#      "./data/top_100_features_in_random_500bills.csv")


#1            shall     33975 YES
#2          section     29942 YES
#3              act     23032 YES
#4        secretary     17741 YES
#5       subsection     12674 YES 
#6              may     12288 YES
#7              sec     11481 YES
#8           states     11350 YES
#9         provided     11092 YES
#10           state     10244 NO
#11       available     10208 NO
#12          united     10098 NO
#13            year      9684 NO
#14         program      9651 NO
#15       paragraph      9502 YES
#16           title      9333 YES
#17         federal      9131 NO
#18          health      8737 NO
#19           funds      8503 NO
#20         general      8225 NO
#21       including      8049 NO
#22              ii      7311 YES
#23        national      7268 NO
#24          public      7149 NO
#25         amended      6490 YES
#26            made      6417 YES
#27        services      6355 NO
#28            date      6074 YES
#29       inserting      5929 YES
#30            term      5920 NO
#31          fiscal      5879 NO
#32     information      5565 NO
#33      assistance      5542 NO
#34             law      5532 YES
#35       following      5352 NO
#36          agency      5244 YES
#37         service      5235 NO
#38        striking      5038 YES
#39          amount      5012 YES
#40      activities      4905 YES
#41        programs      4734 YES
#42      authorized      4684 YES
#43       described      4681 NO
#44             use      4679 NO
#45          report      4645 YES
#46    subparagraph      4547 YES
#47        purposes      4320 NO
#48            plan      4177 NO
#49        security      4156 NO
#50         provide      4041 NO

basic_stopw <- quanteda::stopwords("english")

extra_stopw <- c("shall", "section", "act", "secretary", "subsection", "may", 
                 "sec", "states", "provided", "paragraph", "title", "ii", 
                 "amended", "made", "date", "insterting", "law", "agency", 
                 "striking", "amount", "activities", "programs", "authorized",
                 "report", "subparagraph")

stopw <- c(basic_stopw, extra_stopw)

#===============================================================================
# Getting all 1-to-n variable combinations 
get_vars_comb <- function(x_vars, n) {
  all_combs <- NULL
  for (i in 1:n) {
    combs_raw <- combn(x_vars, i)
    combs_clean <- sapply(1:ncol(combs_raw), function(x)
      paste0(combs_raw[,x], collapse = ", "))
    all_combs <- c(all_combs, combs_clean)
  }
  return(all_combs)
}

#===============================================================================
# Function for training-testing models in Stage-1 of the 3-stage bill-law
#   matching method
train_test_models <- function(labs_db, all_combs, sim){
  results <- NULL
  true_obs <- which(labs_db$true_match_num2 == 1)
  false_obs <- which(labs_db$true_match_num2 == 0)
  train_perc <- 0.8 # percentage of cases to use for training 
  y <- labs_db$true_match_num2
  iterations <- sim
  counter <- 1
  total <- length(all_combs)
  print(paste0("Training-testing ", total, " models"))
  for (i in 1:length(all_combs)) {
    all_vars <- strsplit(as.character(all_combs[i]), split = ", ")[[1]]
    x <- cbind(sapply(all_vars, function(x) labs_db[,x]))
    model_formula <- paste0("y ~ ", paste0(colnames(x), collapse = " + "))
    model_accuracy <- NULL
    model_precision <- NULL
    model_recall <- NULL
    model_fscore <- NULL
    print(paste0("[", counter, "/", total, "] ..."))
    for (j in 1:iterations) {
      train_size_true <- round(length(true_obs) * 0.8, 0)
      test_size_true <- length(true_obs) - train_size_true
      train_size_false <- round(length(false_obs) * 0.8, 0)
      test_size_false <- length(false_obs) - train_size_false
      true_train <- sample(x = true_obs, size = train_size_true, replace = FALSE)
      true_test <- true_obs[which(!(true_obs %in% true_train))]
      false_train <- sample(x = false_obs, size = train_size_false, replace = FALSE)
      false_test <- false_obs[which(!(false_obs %in% false_train))]
      train_obs <- c(true_train, false_train)
      test_obs <- c(true_test, false_test)
      train_data <- as.data.frame(cbind(y[train_obs], x[train_obs,]))
      colnames(train_data) <- c("y", all_vars)
      test_data <- as.data.frame(cbind(y[test_obs], x[test_obs,]))
      colnames(test_data) <- c("y", all_vars)
      model_fit <- glm(model_formula, data = train_data, family = "binomial")
      actual_values <- test_data$y
      model_pred <- predict(model_fit, newdata = test_data, type = "response")
      model_binary_pred <- ifelse(as.numeric(model_pred) > 0.5, 1, 0)
      gold_fit_df <- as.data.frame(
        cbind(actual_values, model_binary_pred)
      )
      gold_fit_df$same <- gold_fit_df$actual_values == gold_fit_df$model_binary_pred
      recall <- round(nrow(gold_fit_df[which(gold_fit_df$actual_values == 1 &
                                               gold_fit_df$same == TRUE),]) / test_size_true, 3)
      precision <- round(nrow(gold_fit_df[which(gold_fit_df$model_binary_pred == 1 &
                                                  gold_fit_df$same == TRUE),]) /
                           length(which(gold_fit_df$model_binary_pred == 1)), 3)
      fscore <- 2 * ((precision * recall) / (precision + recall))
      model_precision <- c(model_precision, precision)
      model_recall <- c(model_recall, recall)
      model_fscore <- c(model_fscore, fscore)
    }
    model_results <- data.frame(
      model = model_formula,
      # point estimates
      precision_pe = round(mean(model_precision), 3),
      recall_pe = round(mean(model_recall, na.rm = T), 3),
      fscore_pe = round(mean(model_fscore, na.rm = T), 3),
      # CIs
      precision_lwr = round(quantile(model_precision, probs = 0.025),2),
      precision_upr = round(quantile(model_precision, probs = 0.975),2),
      recall_lwr = round(quantile(model_recall, probs = 0.025, na.rm = T),2),
      recall_upr = round(quantile(model_recall, probs = 0.975, na.rm = T),2),
      fscore_lwr = round(quantile(model_fscore, probs = 0.025, na.rm = T),2),
      fscore_upr = round(quantile(model_fscore, probs = 0.975, na.rm = T),2)
    )
    print(paste0("      ... precision = ", model_results$precision_pe))
    print(paste0("      ... recall = ", model_results$recall_pe))
    row.names(model_results) <- NULL
    results <- rbind(results, model_results)
    counter <- counter + 1
  }
  results$mlabel <- paste0("nM", 1:nrow(results))
  return(results)
}

#===============================================================================
# Function for training-testing models in Stage-1 of the 3-stage bill-law
predict_bill_law_matches <- function(best_models, labs_db, potential_matches_db) {
  db <- potential_matches_db
  train_db <- labs_db
  train_db$y <- train_db$true_match_num2
  bill_law_pred_mat <- data.frame(
    version_a = db$version_a,
    version_b = db$version_b
  )
  best_models$bill_law_matches <- NA
  best_models$bill_mult_match <- NA
  for (i in 1:nrow(best_models)) {
    print(paste0("[", i, "/", nrow(best_models), "] ..."))
    model_formula <- as.character(best_models$model[i])
    model <- glm(model_formula, data = train_db, family = "binomial")
    pred <- predict(model, newdata = db, type = "response", se.fit = TRUE)
    pred_bin <- ifelse(pred$fit - (2 * pred$se.fit) > 0.5, 1, 0)
    bill_law_pred_mat <- cbind(bill_law_pred_mat, pred_bin)
    colnames(bill_law_pred_mat)[i + 2] <- as.character(best_models$mlabel[i])
    bill_law_matches <- length(which(pred_bin == 1))
    out <- data.frame(
      version_a = db$version_a,
      version_b = db$version_b,
      pred_bin = pred_bin
    )
    duplicate_bill_law <- out %>%
      filter(pred_bin == 1) %>%
      group_by(version_a) %>%
      summarize(n = n()) %>%
      arrange(desc(n))
    bill_mult_match <- length(which(duplicate_bill_law$n > 1))
    best_models$bill_law_matches[i] <- bill_law_matches
    best_models$bill_mult_match[i] <- bill_mult_match
  }
  return(bill_law_pred_mat)
}

#===============================================================================
# A function to get the coefficients from multinomial models from the 
#   'nnet' package
coef.multinom <- function(x) {
  nlevel <- length(x$lev)
  ncoef <- length(x$coefnames)
  coef <- x$wts[(ncoef+2):length(x$wts)]
  coef[-((0:(nlevel-2))*(ncoef+1) + 1)]
}

#===============================================================================
# A function to calculate marginal effects for logistic regressions
get_marfx_logistic <- function(model, model_dataset, type = "likelihood",
                               cat_variables = NULL) {
  library(MASS)
  library(nnet)
  library(dplyr)
  
  # - pulling key coefficients and data
  pe <- coef(model) # point estimates
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  
  # - simulate Betas
  sims <- 10000
  simbetas <- MASS::mvrnorm(sims, pe, vc) 
  
  # - initializing the results dataset
  marf_res <- NULL
  
  # - pulling the outcome and explanatory variables from the model formula. 
  #   Detecting any interactions and saving the names as well
  yname <- strsplit(as.character(model$formula), split = "~")[[2]]
  predictors <- unique(strsplit(
    strsplit(as.character(model$formula), split = "~")[[3]], 
    split = " \\+ | \\* "
  )[[1]])
  interactions_pre <- strsplit(
    strsplit(as.character(model$formula), split = "~")[[3]], 
    split = " \\+ "
  )[[1]]
  interactions <- interactions_pre[which(grepl(" * ", interactions_pre))]
  inter_variables <- strsplit(interactions, split = " \\* ")
  
  # - pulling the right side of the model formula to create model matrices 
  #   in the future (know which categorical variables need to be transformed to
  #   dummies)
  right_side <- paste0("~",
                       strsplit(as.character(model$formula), split = "~")[[3]])
  
  # - a version of the datset only with the variables of interest (and removing
  #   any rows that have an NA for any of those variables). Pulling the actual
  #   outcome values as well.
  full_data <- na.omit(model_dataset[,c(yname, predictors)])
  X <- full_data[,predictors]
  Y <- full_data[,yname]
  
  # - create a generic scenario (all variables at their minimum value)
  gen_scen <- NULL
  for (j in 1:ncol(X)) {
    if (colnames(X[j]) %in% cat_variables) {
      # - if it's a factor: giving it an NA for now
      gen_scen <- c(gen_scen, NA)
    } else {
      gen_scen <- c(gen_scen, as.matrix(min(X[,j])))
    }
  }
  gen_scen <- t(as.data.frame(gen_scen))
  gen_scen <- data.frame(gen_scen)
  colnames(gen_scen) <- colnames(X)
  rownames(gen_scen) <- NULL
  
  # - a list of variables for which to calculate the marginal effects in a first
  #   step: excluding factor variables and interactions
  variables <- predictors[which(!(predictors %in% 
                                    c(cat_variables)))]
  
  # - iterating through the selected predictors and calculating the marginal 
  #   effect on the outcome of going from minimum to maximum value
  #   -- NUMERIC VARIABLES --
  for (v in variables) {
    # - creating a minimum (generic) and maximum scenario
    gen_scen <- as.data.frame(gen_scen)
    scen0 <- scen1 <- gen_scen
    scen1[,v] <- max(X[,v])
    
    # - transforming the scenario to model.matrix format (transforming 
    #   categorical variables into dummies)
    for (cat_v in cat_variables) {
      scen0[,cat_v] <- factor(X[,cat_v][1], 
                              levels = as.character(unique(X[,cat_v])))
      scen1[,cat_v] <- scen0[,cat_v]
    }
    
    for (var in variables) {
      if (!(var %in% cat_variables)) {
        scen0[,var] <- as.numeric(as.character(scen0[,var]))
        scen1[,var] <- as.numeric(as.character(scen1[,var]))
      }
    }
    
    mmatrix0 <- model.matrix(formula(right_side), scen0)
    mmatrix1 <- model.matrix(formula(right_side), scen1)
    
    # - N predicted values for each scenario, and then taking the average 
    #   likelihood or the average differendce, and 95% CIs.
    
    yhats0 <- exp(mmatrix0 %*% t(as.matrix(simbetas)))
    yhats1 <- exp(mmatrix1 %*% t(as.matrix(simbetas)))
    
    if (type == "likelihood") {
      pred_values <- yhats1 / yhats0
    } else if (type == "difference") {
      pred_values <- yhats1 - yhats0
    }
    
    pe <- round(mean(pred_values), 4)
    lwr <- round(quantile(pred_values, probs = 0.025), 4)
    upr <- round(quantile(pred_values, probs = 0.975), 4)
    
    # - adding the marginal effect for this variable to the results dataset
    new_row <- data.frame(v, pe, lwr, upr)
    marf_res <- rbind(marf_res, new_row)
  }
  
  #   -- CATEGORICAL VARIABLES --
  for (v in cat_variables) {
    # - pull the reference class for this cat var
    full_data[,v] <- as.factor(full_data[,v])
    ref_class <- factor(levels(full_data[,v])[1], 
                        levels = levels(full_data[,v]))
    scen0 <- gen_scen
    
    # - set up 'random' values for the other categorical variables
    for (cat_v in cat_variables) {
      if (cat_v != v) {
        scen0[,cat_v] <- factor(X[,cat_v][1], 
                                levels = as.character(unique(X[,cat_v])))
      }
    }
    scen0[,v] <- ref_class
    
    # - calculate Pr baseline ref class
    mmatrix0 <- model.matrix(formula(right_side), scen0)
    yhats0 <- exp(mmatrix0 %*% t(as.matrix(simbetas)))
    
    # - calculate now Pr and marginal effect for the other levels
    other_levels <- levels(full_data[,v])[2:length(levels(full_data[,v]))]
    for (lev in other_levels) {
      scen1 <- scen0
      scen1[,v] <- factor(lev, levels = levels(scen0[,v]))
      mmatrix1 <- model.matrix(formula(right_side), scen1)
      yhats1 <- exp(mmatrix1 %*% t(as.matrix(simbetas)))
      
      if (type == "likelihood") {
        pred_values <- yhats1 / yhats0
      } else if (type == "difference") {
        pred_values <- yhats1 - yhats0
      }
      
      pe <- round(mean(pred_values), 4)
      lwr <- round(quantile(pred_values, probs = 0.025), 4)
      upr <- round(quantile(pred_values, probs = 0.975), 4)
      
      # - adding the marginal effect for this variable to the results dataset
      new_row <- data.frame(v = paste0(v, ":", lev), pe, lwr, upr)
      marf_res <- rbind(marf_res, new_row)
    }
  }
  
  # - finally, calculating the marginal effects for interacted variables
  genscen <- scen0
  
  for (inter in inter_variables) {
    if ("MRef" %in% inter) {
      inter <- c("Majority", "MRef")
    }
    v1 <- inter[1]
    v2 <- inter[2]
    # - for each value of the first dummy
    for (i in 0:1) {
      scen_i <- genscen
      # - calculate marginal effect of the second dummy going form 0 to 1
      scen_i0 <- scen_i1 <- scen_i
      scen_i0[,v1] <- scen_i1[,v1] <-  i
      scen_i1[,v2] <- 1
      
      mmatrix_i0 <- model.matrix(formula(right_side), scen_i0)
      mmatrix_i1 <- model.matrix(formula(right_side), scen_i1)      
      
      yhats0 <- exp(mmatrix_i0 %*% t(as.matrix(simbetas)))
      yhats1 <- exp(mmatrix_i1 %*% t(as.matrix(simbetas)))
      
      if (type == "likelihood") {
        pred_values <- yhats1 / yhats0
      } else if (type == "difference") {
        pred_values <- yhats1 - yhats0
      }
      
      pe <- round(mean(pred_values), 4)
      lwr <- round(quantile(pred_values, probs = 0.025), 4)
      upr <- round(quantile(pred_values, probs = 0.975), 4)
      
      # - adding the marginal effect for this variable to the results dataset
      new_row <- data.frame(v = paste0(v1, i, " (", v2, ")"), pe, lwr, upr)
      marf_res <- rbind(marf_res, new_row)
    }
    
  }
  return(marf_res)
}

#===============================================================================
# A function to calculate predicted probabilities and marginal effects for dummy
#   variables in multinomial logit models
get_pr_marf_dummy_multinom <- function(model, scenarios, 
                                       type = "percentage",
                                       maj_levels = NULL) {
  library(MASS)
  library(nnet)
  library(dplyr)
  # - pulling the values of interest for the marginal effect
  #values <- scenarios$values
  #scenarios <- scenarios %>%
  #  dplyr::select(-values)
  
  # - number of classes of the outcome variable
  outcomes_n <- length(model$lev)
  # - class labels for the outcome variable
  outcomes_labs <- model$lev
  
  # - pulling key coefficients and data
  pe <- coef(model) # point estimates
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  
  # - simulate Betas
  sims <- 10000
  simbetas <- MASS::mvrnorm(sims, pe, vc)       # draw parameters, using MASS::mvrnorm
  Bn <- (dim(simbetas)[2]/(outcomes_n - 1))
  simB <- array(NA, dim = c(sims, Bn, (outcomes_n - 1)))  # re-arrange simulates to array format
  for (i in 1:(outcomes_n - 1)) {
    if (i == 1) {
      simB[,,i] <- simbetas[,i:Bn]
    } else {
      simB[,,i] <- simbetas[,((Bn * (i - 1)) + 1):(Bn * i)]
    }
  }
  
  # - initializing the results dataset
  marf_res <- NULL
  for (i in 1:nrow(scenarios)) {
    # - pulling one scenario
    sc <- scenarios[i,]
    # - pulling the key variable in that scenario (e.g. "Majority")
    sc_var <- rownames(sc)
    # - creating a Pos and Neg scenarios dataframes
    sc1 <- sc0 <- sc
    sc1[,sc_var] <- 1
    sc0[,sc_var] <- 0
    # - if the variable of interest is in an interaction, setting the interacted
    #     variable to 0
    in_interaction <- ifelse(length(which(grepl(sc_var, interactions))) > 0,
                             1, 0)
    if (in_interaction) {
      interaction <- interactions[which(grepl(sc_var, interactions))]
      inter_vars <- strsplit(interaction, split = ":")[[1]]
      other_var <- inter_vars[which(!(grepl(sc_var, inter_vars)))]
      sc0[,other_var] <- 0
      sc1[,other_var] <- 0
    }
    
    # - if the model has "Major" as variable, making sure R knows it is a factor
    #     variables with multiple levels, not only the one in the generic scenario
    if (!(is.null(maj_levels))) {
      sc0$Major <- factor(sc0$Major, levels = maj_levels)
      sc1$Major <- factor(sc1$Major, levels = maj_levels)
    }
    
    # - transforming the scenario into model matrix format
    mm0 <- model.matrix(as.formula(paste0("~", right_side)), sc0)
    mm1 <- model.matrix(as.formula(paste0("~", right_side)), sc1)
    
    # - tranform the scenarios dataframes into matrices
    x1 <- as.matrix(as.numeric(mm1))
    x0 <- as.matrix(as.numeric(mm0))
    
    # - calculating (1,000) Pr of each outcome class when dummy == 1
    # ... a) calculating Y without the non-linear transf.
    Y1 <- sapply(1:(outcomes_n - 1), function(z)
      exp(simB[,,z] %*% x1))
    Y1denom <- rowSums(Y1) + 1
    # ... b) applying the non-linear transf.
    Pr1 <- sapply(1:(outcomes_n - 1), function(z)
      Y1[,z]/Y1denom)
    # ...c) adding the probabilities for the reference class
    Pr1_ref <- 1 - rowSums(Pr1)
    Pr1_final <- cbind(Pr1_ref, Pr1)
    Pr1_estimate <- sapply(1:ncol(Pr1_final), function(z)
      round(mean(Pr1_final[,z]), 3))
    
    # - calculating (1,000) Pr of each outcome class when dummy == 0
    Y0 <- sapply(1:(outcomes_n - 1), function(z)
      exp(simB[,,z] %*% x0))
    Y0denom <- rowSums(Y0) + 1
    # ... b) applying the non-linear transf.
    Pr0 <- sapply(1:(outcomes_n - 1), function(z)
      Y0[,z]/Y0denom)
    # ...c) adding the probabilities for the reference class
    Pr0_ref <- 1 - rowSums(Pr0)
    Pr0_final <- cbind(Pr0_ref, Pr0)
    Pr0_estimate <- sapply(1:ncol(Pr0_final), function(z)
      round(mean(Pr0_final[,z]), 3))
    
    # - calculating marginal effect of going from 0 to 1
    if (type == "percentage") {
      # ... marginal effect expressed in percentage points difference
      marf_mat <- Pr1_final - Pr0_final
    } else if (type == "likelihood") {
      # .. marginal effect expressed in likelihood difference
      marf_mat <- Pr1_final / Pr0_final
    } else {
      print("Error: specify either type == 'percentage' or type == 'likelihood'")
      break
    }
    marf_pe <- sapply(1:ncol(marf_mat), function(z)
      round(mean(marf_mat[,z]), 3))
    marf_lwr <- as.numeric(sapply(1:ncol(marf_mat), function(z)
      round(quantile(marf_mat[,z], probs = 0.025), 3)))
    marf_upr <- as.numeric(sapply(1:ncol(marf_mat), function(z)
      round(quantile(marf_mat[,z], probs = 0.975), 3)))
    
    # - a list with all relevant estimates
    all_estimates <- as.data.frame(
      rbind(Pr1_estimate, Pr0_estimate,
            marf_pe, marf_lwr, marf_upr))
    colnames(all_estimates) <- outcomes_labs
    all_estimates$var <- sc_var
    all_estimates$estimate <- rownames(all_estimates)
    rownames(all_estimates) <- NULL
    
    # -adding results to mar_res2
    marf_res <- rbind(marf_res, all_estimates)
  }
  return(marf_res)
}

#===============================================================================
# A function to calculate predicted probabilities and marginal effects for dummy
#   variables in multinomial logit models
get_std_coef_multinom <- function(model, model_data,
                                  scenarios, type = "percentage",
                                  maj_levels = NULL, cong_levels = NULL) {
  library(MASS)
  library(nnet)
  library(dplyr)
  # - pulling the values of interest for the marginal effect
  #values <- scenarios$values
  #scenarios <- scenarios %>%
  #  dplyr::select(-values)
  
  # - number of classes of the outcome variable
  outcomes_n <- length(model$lev)
  # - class labels for the outcome variable
  outcomes_labs <- model$lev
  # - checking for interaction variables 
  # - pulling model covariates from model formula
  mformula <- as.character(model$call)[2]
  right_side <- strsplit(mformula, split = "~")[[1]][2]
  vars <- gsub(" ", "", unique(strsplit(right_side, "[+|*]")[[1]]))
  interactions <- colnames(model.matrix(model))[which(grepl(
    ":", colnames(model.matrix(model))))]
  
  # - pulling key coefficients and data
  pe <- coef(model) # point estimates
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  
  # - simulate Betas
  sims <- 10000
  simbetas <- MASS::mvrnorm(sims, pe, vc)       # draw parameters, using MASS::mvrnorm
  Bn <- (dim(simbetas)[2]/(outcomes_n - 1))
  simB <- array(NA, dim = c(sims, Bn, (outcomes_n - 1)))  # re-arrange simulates to array format
  for (i in 1:(outcomes_n - 1)) {
    if (i == 1) {
      simB[,,i] <- simbetas[,i:Bn]
    } else {
      simB[,,i] <- simbetas[,((Bn * (i - 1)) + 1):(Bn * i)]
    }
  }
  
  # - initializing the results dataset
  marf_res <- NULL
  for (i in 1:nrow(scenarios)) {
    # - pulling one scenario
    sc <- scenarios[i,]
    # - pulling the key variable in that scenario (e.g. "Majority")
    sc_var_raw <- rownames(sc)
    sc_var <- gsub('[0-9]', '', sc_var_raw)
    # - creating a Min and a Max scenario
    sc1 <- sc0 <- sc
    sc0[,sc_var] <- min(model_data[, sc_var], na.rm = TRUE)
    sc1[,sc_var] <- max(model_data[, sc_var], na.rm = TRUE)
    # - if the variable of interest is in an interaction, setting the interacted
    #     variable to 0
    in_interaction <- ifelse(length(which(grepl(sc_var, interactions))) > 0,
                             1, 0)
    if (in_interaction) {
      interaction <- interactions[which(grepl(sc_var, interactions))]
      inter_vars <- strsplit(interaction, split = ":")[[1]]
      other_var <- inter_vars[which(!(grepl(sc_var, inter_vars)))]
      sc0[,other_var] <- 0
      sc1[,other_var] <- 0
    }
    
    # - if the model has "Major" as variable, making sure R knows it is a factor
    #     variables with multiple levels, not only the one in the generic scenario
    if (!(is.null(maj_levels))) {
      sc0$Major <- factor(sc0$Major, levels = maj_levels)
      sc1$Major <- factor(sc1$Major, levels = maj_levels)
    }
    # - the same for the categorical variable "Cong"
    if (!(is.null(cong_levels))) {
      sc0$Cong <- factor(sc0$Cong, levels = cong_levels)
      sc1$Cong <- factor(sc1$Cong, levels = cong_levels)
    }
    
    # - transforming the scenario into model matrix format
    mm0 <- model.matrix(as.formula(paste0("~", right_side)), sc0)
    mm1 <- model.matrix(as.formula(paste0("~", right_side)), sc1)
    
    # - tranform the scenarios dataframes into matrices
    x1 <- as.matrix(as.numeric(mm1))
    x0 <- as.matrix(as.numeric(mm0))
    
    # - calculating (1,000) Pr of each outcome class when dummy == 1
    # ... a) calculating Y without the non-linear transf.
    Y1 <- sapply(1:(outcomes_n - 1), function(z)
      exp(simB[,,z] %*% x1))
    Y1denom <- rowSums(Y1) + 1
    # ... b) applying the non-linear transf.
    Pr1 <- sapply(1:(outcomes_n - 1), function(z)
      Y1[,z]/Y1denom)
    # ...c) adding the probabilities for the reference class
    Pr1_ref <- 1 - rowSums(Pr1)
    Pr1_final <- cbind(Pr1_ref, Pr1)
    Pr1_estimate <- sapply(1:ncol(Pr1_final), function(z)
      round(mean(Pr1_final[,z]), 3))
    
    # - calculating (1,000) Pr of each outcome class when dummy == 0
    Y0 <- sapply(1:(outcomes_n - 1), function(z)
      exp(simB[,,z] %*% x0))
    Y0denom <- rowSums(Y0) + 1
    # ... b) applying the non-linear transf.
    Pr0 <- sapply(1:(outcomes_n - 1), function(z)
      Y0[,z]/Y0denom)
    # ...c) adding the probabilities for the reference class
    Pr0_ref <- 1 - rowSums(Pr0)
    Pr0_final <- cbind(Pr0_ref, Pr0)
    Pr0_estimate <- sapply(1:ncol(Pr0_final), function(z)
      round(mean(Pr0_final[,z]), 3))
    
    # - calculating marginal effect of going from 0 to 1
    if (type == "percentage") {
      # ... marginal effect expressed in percentage points difference
      marf_mat <- Pr1_final - Pr0_final
    } else if (type == "likelihood") {
      # .. marginal effect expressed in likelihood difference
      marf_mat <- Pr1_final / Pr0_final
    } else {
      print("Error: specify either type == 'percentage' or type == 'likelihood'")
      break
    }
    marf_pe <- sapply(1:ncol(marf_mat), function(z)
      round(mean(marf_mat[,z]), 3))
    marf_lwr <- as.numeric(sapply(1:ncol(marf_mat), function(z)
      round(quantile(marf_mat[,z], probs = 0.025), 3)))
    marf_upr <- as.numeric(sapply(1:ncol(marf_mat), function(z)
      round(quantile(marf_mat[,z], probs = 0.975), 3)))
    
    # - a list with all relevant estimates
    all_estimates <- as.data.frame(
      rbind(Pr1_estimate, Pr0_estimate,
            marf_pe, marf_lwr, marf_upr))
    colnames(all_estimates) <- outcomes_labs
    all_estimates$var <- sc_var_raw
    all_estimates$estimate <- rownames(all_estimates)
    rownames(all_estimates) <- NULL
    
    # -adding results to mar_res2
    marf_res <- rbind(marf_res, all_estimates)
  }
  return(marf_res)
}

#===============================================================================
# A function that takes a vector of numeric state codes and returns their 
#   alphabetical abbreviations
get_state_abbr <- function(num_states) {
  abbr <-  c('MI', 'NE', 'MN', 'CO', 'NJ', 'WI', 'GA', 'FL', 'MD', 'VA', 'TX',
             'NY', 'SC', 'IL', 'PA', 'LA', 'AK', 'CA', 'TN', 'RI', 'OK', 'HI',
             'NC', 'OH', 'IA', 'KY', 'UT', 'MA', 'ID', 'KS', 'AR', 'OR', 'CT',
             'MO', 'MS', 'AZ', 'NH', 'NV', 'WA', 'ME', 'SD', 'AL', 'IN',  'VT',
             'WV', 'NM', 'MT', 'DE', 'ND', 'WY', 'territory')
  numcode <-  c(23, 35, 33, 62, 12, 25, 44, 43, 52, 40, 49, 13, 48, 21, 14, 
                45, 81, 71, 54, 5, 53, 82, 47, 24, 31, 51, 67, 3, 63, 32, 
                42, 72, 1, 34, 46, 61, 4, 65, 73, 2, 37, 41, 22, 6, 56, 
                66, 64, 11, 36, 68, 0)
  out <- as.character(sapply(num_states, function(x) abbr[which(numcode == x)]))
  return(out)
}


#===============================================================================
# A function that takes a vector of numeric state codes and returns their 
#   alphabetical abbreviations
get_state_name <- function(state_abbr) {
  abbr <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
            "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
            "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
            "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
            "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY", "territory")
  
  full_name <- c("Alaska", "Alabama", "Arkansas", "Arizona", "California", 
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Iowa", "Idaho", "Illinois", "Indiana", "Kansas",
                 "Kentucky", "Louisiana", "Massachusetts", "Maryland", "Maine",
                 "Michigan", "Minnesota", "Missouri", "Mississippi", "Montana",
                 "North Carolina", "North Dakota", "Nebraska", "New Hampshire", 
                 "New Jersey", "New Mexico", "Nevada", "New York", "Ohio",
                 "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                 "South Carolina", "South Dakota", "Tennessee", "Texas",
                 "Utah", "Virginia", "Vermont", "Washington", "Wisconsin",
                 "West Virginia", "Wyoming", "territory")
  out <- as.character(sapply(state_abbr, function(x) 
    full_name[which(abbr == x)]))
  return(out)
}

#===============================================================================
# A function that takes a vector of state FIPS (01-78) codes and returns the 
#   State name or abbreviation.
get_state_name_from_fips <- function(state_fips_codes) {
  fips_codes <- as.character(state_fips_codes)
  fips_codes <- c( "02", "01", "05", "04", "06", "08", "09", "10", 
                   "12", "13", "15", "19", "16", "17", "18", "20", "21", "22", 
                   "25", "24", "23", "26", "27", "29", "28", "30", "37", "38", 
                   "31", "33", "34", "35", "32", "36", "39", "40", "41", "42", 
                   "44", "45", "46", "47", "48", "49", "51", "50",
                   "53", "55", "54", "56", "60", "11", "66", "72", "78")
  full_name <- c("Alaska", "Alabama", "Arkansas", "Arizona", "California", 
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Iowa", "Idaho", "Illinois", "Indiana", "Kansas",
                 "Kentucky", "Louisiana", "Massachusetts", "Maryland", "Maine",
                 "Michigan", "Minnesota", "Missouri", "Mississippi", "Montana",
                 "North Carolina", "North Dakota", "Nebraska", "New Hampshire", 
                 "New Jersey", "New Mexico", "Nevada", "New York", "Ohio",
                 "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                 "South Carolina", "South Dakota", "Tennessee", "Texas",
                 "Utah", "Virginia", "Vermont", "Washington", "Wisconsin",
                 "West Virginia", "Wyoming", "American Samoa", 
                 "District of Columbia", "Guam", "Puerto Rico", "Virgin Islands")
  out <- as.character(sapply(state_fips_codes, function(x) 
    full_name[which(fips_codes == x)]))
  return(out)
}

