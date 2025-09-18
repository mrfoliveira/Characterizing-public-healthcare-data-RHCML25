#' Outlier score
#'
#' Computes the percentage of outliers for each column in the dataset
#' 
#' @param data data frame
#' @param z_thr Z-score threshold for value to be considered an outlier
#' @param iqr_fact factor for boxplot rule for outlier detection that
#' considers an outlier if the value is either below Q1 - iqr_fact \* IQR 
#' or above Q3 + iqr_fact \* IQR, where Q1 and Q3 are the first and third 
#' quartile and IQR the inter-quartile range.
#'
#' @returns percentage of outliers according to both rules for each numeric
#' attribute; average and standard deviation across attributes
#' @export
#'
#' @examples
OutlierScore <- function(data, z_thr = 3, iqr_fact = 1.5, freq_thr = 0.05){
  
  num_atts <- data[, which(sapply(data, is.numeric)), drop = F]
  if(ncol(num_atts) > 0 & nrow(num_atts) > 0){
    perc_out <- lapply(num_atts, function(x){
      z_scores <- abs((x - mean(x, na.rm = T)) / sd(x, na.rm = T))
      z_nout <- length(which(z_scores > z_thr))
      z_perc <- 100 * z_nout / length(x)
      
      qs <- quantile(x, probs = c(0.25, 0.75), na.rm = T) 
      iqr_nout <- length(which( x < qs[1] - iqr_fact*IQR(x, na.rm =T) |  
                                  x > qs[2] + iqr_fact*IQR(x, na.rm=T)))
      iqr_perc <- 100 * iqr_nout / length(x)
      
      data.frame(measure = c("z_perc", "iqr_perc"), 
                 value = c(z_perc, iqr_perc))
    })
    names(perc_out) <- colnames(num_atts)
    perc_out <- dplyr::bind_rows(perc_out, .id = "variable") 
    
  }else
    perc_out <- data.frame(variable = c(NA, NA),
                           measure = c("z_perc", "iqr_perc"),
                           value = c(NA, NA))
  
  cat_atts <- data[, which(!sapply(data, is.numeric)), drop = F]
  if(ncol(cat_atts) > 0 & nrow(cat_atts) > 0){
    cat_perc_out <- lapply(cat_atts, function(x){
      rare_test <- data.frame(rare = table(x) / length(x) < freq_thr)
      rare_cat_5perc <- rownames(rare_test)[which(rare_test$rare)]
      
      data.frame(measure = "rare_cat", 
                 value = 100 * length(which(x %in% rare_cat_5perc)) / length(x))
    })
    names(cat_perc_out) <- colnames(cat_atts)
    cat_perc_out <- dplyr::bind_rows(cat_perc_out, .id = "variable") 
  }else
    cat_perc_out <- data.frame(variable = NA,
                               measure = c("rare_cat"),
                               value = NA)
  
  
  perc_out <- rbind(perc_out, cat_perc_out) %>% 
    tidyr::pivot_wider(names_from="measure", values_from="value")
  
  scores <- summarize_all(perc_out[,-1], list(avg = mean, sd = sd), na.rm = T)
  
  
  list(perc_out = perc_out, 
       scores = scores)
}

#' Missing Score
#' 
#' Computes the percentage of missing values per column in the dataset and
#' provides summary statistics for missing rates across samples and columns.
#'
#' @param data data frame
#' @param row_thr
#' @param col_thr 
#'
#' @returns
#' @export
#'
#' @examples
MissingScore <- function(data, row_thr = 0.1, col_thr = 0.1){#, tgt = NULL){
  # if(!is.numeric(tgt)) tgt <- which(tgt == colnames(data))
  # data <- data[,-tgt]
  
  missRows_perc <- apply(data, 1, function(x) 100 * length(which(is.na(x))) / length(x))
  missCols_perc <- apply(data, 2, function(x) 100 * length(which(is.na(x))) / length(x))
  
  list(missRows_perc = missRows_perc,
       missCols_perc = missCols_perc,
       scores = data.frame(missing_perc = 100 * sum(is.na(data)) / (ncol(data)*nrow(data)),
                           anyNA_perRow = 100* (nrow(data) - length(which(complete.cases(data)))) / nrow(data),
                           manyMissPerRow_perc = 100 * length(which(missRows_perc>100*row_thr)) / nrow(data),
                           missPerRow_perc_avg = mean(missRows_perc),
                           missPerRow_perc_max = max(missRows_perc),
                           anyNA_perCol = 100* length(which(sapply(data, anyNA))) / ncol(data),
                           missPerCol_perc_avg = mean(missCols_perc),
                           missPerCol_perc_max = max(missCols_perc),
                           manyMissPerCol_perc = 100 * length(which(missCols_perc>100*col_thr)) / nrow(data)))
}

#' Uniqueness score
#' 
#' Computes the percentage of duplicate records in a dataset, providing a 
#' measure of the dataset's uniqueness. 
#' 
#' The uniqueness score is calculated by determining theproportion of duplicate 
#' rows in the dataset. 
#'
#' A higher percentage indicates more duplicates, while a lower 
#' percentage indicates higher uniqueness.
#'
#' @param dataframe 
#'
#' @returns
#' @export
#'
#' @examples
UniquenessScore <- function(data){
  list(scores = data.frame(UniqScore =100 * length(which(duplicated(data))) / nrow(data)))
}

#' Correlation Score
#'
#'Computes the correlation matrix for the input data and 
#'determines the percentage of attribute pairs that are 
#'moderately or strongly correlated with each other and 
#'the average correlation values.
#'
#' @param data dataframe
#' @param tgt name or index position of target variable
#' @param c_thr strong correlation threshold
#' @param p_thr significance p-value threshold
#'
#' @returns 
#' @export
#'
#' @examples
CorrelationScore <- function(data, tgt, c_thr = 0.5, p_thr = 0.05){
  cols <- colnames(data)
  if(!is.numeric(tgt)) tgt <- which(tgt == cols)
  
  num_atts <- data[, setdiff(which(sapply(data, is.numeric)), tgt), drop = F]
  
  if(ncol(num_atts) < 2){
    warning("Only one or less numeric features means empty correlation scores.")
    pcorr = NULL
    scores = data.frame(perc_corr=NA, perc_corr_signif=NA,
                        corr_avg=NA, corr_abs_avg=NA, corr_std=NA)
  }else{
    pcorr <- Hmisc::rcorr(as.matrix(num_atts), type="pearson")
    upper_corr <- pcorr$r[upper.tri(pcorr$r, diag = F)]
    upper_pval <- pcorr$P[upper.tri(pcorr$P, diag = F)]
    
    strong_corr <- upper_corr[which(abs(upper_corr) > c_thr)]
    signif_corr <- upper_corr[which(abs(upper_corr) > c_thr & upper_pval <= p_thr)]
    
    scores <- data.frame(perc_corr = 100 * length(strong_corr) / length(upper_corr), 
                         perc_corr_signif = 100 * length(signif_corr) / length(upper_corr),
                         corr_avg = mean(upper_corr),
                         corr_abs_avg = mean(abs(upper_corr)),
                         corr_std = sd(upper_corr))
  }
  
  list(corr = pcorr, 
       scores = scores)
}

#' Complexity Score
#'
#' @param data data frame
#' @param tgt target variable name
#' @param groups "all" or a subset of "overlapping", "dimensionality", "balance",
#' "neighborhood", "linearity", and "network"
#'
#' @returns
#' @export
#'
#' @examples
ComplexityScore <- function(data, tgt, groups = "all", na_thr_col = 0.2, nclass_thr = 50){
  if(is.numeric(tgt)) tgt <- colnames(data)[tgt]

  # remove bad columns
  excl1 <- which(sapply(data, function(x) ( length(which(is.na(x))) / nrow(data) ) > na_thr_col))
  excl2 <- which(sapply(data, function(x) !is.numeric(x) & length(unique(x)) > 50))
  excl <- c(excl1, excl2)
  if(length(excl) > 0){
    if(length(excl1 > 0)) 
       warning(paste0("Dropped ", length(excl1), " columns that had more than ",
                   100*na_thr_col, "% missing values."))
    if(length(excl2 > 0)) 
      warning(paste0("Dropped ", length(excl2), " columns that had more than ", nclass_thr, " nominal values."))
    excl_data <- data[,-excl]
  }else
    excl_data <- data

  
  complete_data <- excl_data[complete.cases(excl_data),]
  c_names <- colnames(complete_data)
  if(nrow(complete_data) < 50)
     warning("Complete cases fewer than 50 points.")
  if(nrow(complete_data) < 0.8*nrow(data)) 
     warning("Complete cases fewer than 80% of original data.")
  
    if(nrow(complete_data) > 10000){
      warning("Data set too large for neighborhood, linearity, and network measures")
      if(groups == "all")
        groups <- c("overlapping", "dimensionality", "balance")
      else
        group <- setdiff(groups, c("neighborhood", "linearity", "network"))
    }
    tgt <- which(colnames(complete_data) == tgt)
    scores <- as.data.frame(t(ECoL::complexity(x = complete_data[,-tgt], 
                                             y = complete_data[,tgt, drop =F], 
                                             groups = groups)))
    rownames(scores) <- NULL
  
  list(scores = scores)
}
