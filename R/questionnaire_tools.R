#' Get survey data
#'
#' @param surveysDF A data.frame as returned by the \code{\link{get_surveys}}.
#' @param pid_col A character with the column name specifying user ID.
#' @param ... Not used.
#'
#' @return Returns a long format data.frame of survey data with names "SID variable name", "item", "value", "survey_name"
#' @import dplyr
#' @import tidyr
#' @export
get_survey_data<-function(surveysDF, pid_col='ID', ...){
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('tidyr', quietly = TRUE)
  
  survey_data <- surveysDF %>%
    dplyr::group_by(id) %>%
    dplyr::do(
      survey_name = .$name[[1]],
      survey_data = get_survey_responses(
        surveyID = .$id[[1]])
    )
  long_survey_data <- survey_data %>%
    dplyr::filter(dim(survey_data)[1]>0) %>%
    dplyr::do({
      gather_cols <- names(.$survey_data)[!grepl(pid_col, names(.$survey_data))]
      aDF <- tidyr::gather(.$survey_data,
                            item,
                            value,
                            all_of(gather_cols))
      aDF$survey_name <- .$survey_name
      aDF
    })
  return(long_survey_data)
}

#' Get rubrics
#'
#' @param rubric_filenames Data frame with column of file paths named "file".
#' @param type "scoring" for special handling of scoring rubrics, or "recoding"
#' for special handling of recoding rubrics.
#' @param source Unused, default's to 'csv' for now.
#'
#' @return If \code{type='scoring'}, returns a long data frame of rubrics with names:
#' "file"           "data_file_name" "scale_name"     "column_name"
#' "reverse"        "transform"      "scored_scale"   "include"
#' "min"            "max". Otherwise, it returns the transforming rubric with names:
#' "file"           "data_file_name" "scale_name"     "column_name"    "answer"
#' "response"       "score"
#' @import dplyr
#' @import tidyr
#' @export
get_rubrics <- function (rubric_filenames, type = 'scoring', source = "csv")
{
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('tidyr', quietly = TRUE)
  if(! type %in% c('scoring', 'recoding')){
    stop('Option `type` must be either "scoring" or "recoding"')
  }
  
  csv_rubrics <- rubric_filenames %>%
    mutate(file = as.character(file)) %>%
    group_by(file) %>% do({
      data_frame(rubric = list(read.csv(.$file[[1]], header = T,
                                        stringsAsFactors = F)))
    })
  
  rubric_data_long <- csv_rubrics %>%
    group_by(file) %>%
    do({
      thisDF <- .$rubric[[1]]
      
      names(thisDF) <- tolower(gsub(" ",
                                    "_",
                                    gsub("\\.",
                                         "_", names(thisDF))))
      if(type == 'scoring'){
        aDF <- gather(thisDF,
                      scored_scale,
                      include,
                      -one_of("data_file_name",
                              "scale_name", "column_name", "reverse", "transform",
                              "min", "max")) %>%
          mutate_all(funs(as.character))
      } else if (type == 'recoding') {
        
        aDF <- thisDF %>%
          mutate_all(funs(as.character))
      }
      aDF
    })
  
  return(rubric_data_long)
}

#' get_uncoercibles returns values that should be kept as characters.
#'
#' @param dataDF 
#'
#' @import data.table
#' @export
get_uncoercibles <- function(dataDF){
  requireNamespace('data.table', quietly = TRUE)
  dataDT <- as.data.table(dataDF)
  dataDT <- dataDT[, checkcol := class(type.convert(value, as.is=T)) == 'character', by = .(item,value)]
  dataDT <- dataDT[checkcol == T]
  return(dataDT[,checkcol := NULL])
}

#' Get items from data file that exist in a rubric
#'
#' @param dataDF A long-form data.frame as would be returned by \code{\link{get_survey_data}}
#' @param rubricDF A rubric data.frame as would be returned by \code{\link{get_rubrics}}
#'
#' @return returns a long-format data.frame with just items appearing in \code{rubricDF}
#' @import data.table
#' @export
get_items_in_rubric <- function(dataDF, rubricDF){
  requireNamespace('data.table', quietly = TRUE)
  dataDT <- as.data.table(dataDF)
  rubricCols <- rubricDF$column_name[rubricDF$include %in% c(1, "1", "sum", "prod", "I")]
  smallDF <- as.data.frame(dataDT[item %in% rubricCols])
  return(smallDF)
}

#' Score items
#'
#' @param item_values item_values
#' @param scoring_methods scoring_methods
#' @param na.rm na.rm
#' @param mean.na.rm mean.na.rm
#' @param scale_name scale_name
#' @param scored_scale scored_scale
score_items<-function(item_values,scoring_methods,na.rm=F,mean.na.rm=T,scale_name='', scored_scale='', SID=''){
  # item_values should be a vector of numbers
  # scoring_methods should be a function that takes a vector, or '1'
  #check that all elements in `scoring` are the same

  if(!all(scoring_methods[1]==scoring_methods))
    stop(paste0('Scoring methods not all the same. Check that you\'re\n',
                'grouping rows correctly, and that the rubric is correct.\n',
                '(scale name is ',scale_name,', scored scale is ',scored_scale,')\n',
                paste(scoring_methods, collapse='\n')))
  
  length_vars <- lapply(list(item_values, scoring_methods), length)
  detail_vars <- list(SID = SID, scale_name = scale_name, scored_scale = scale_name)
  length_details <- lapply(detail_vars, length)
  if(any(!length_vars > 0)){
      message('Empty values or scoring method vector...')
      if(any(length_details > 0)){
          details <- detail_vars[which(length_details > 0)]
          message('Details: ', 
                  paste(paste(names(details), details, sep = ': '), collapse = ', '),
                  '.')
      } else {
          message('No scale or subscale info.')
      }
      return(NA)
  }
  
  scoring_method <- unique(scoring_methods)
  scoring_is_mean <- try(scoring_method == 1)
  
  if (!inherits(scoring_is_mean, what = 'try-error') && scoring_is_mean){
    scoring_func<-mean
    na.rm=mean.na.rm
  } else {
    scoring_func<-try(get(scoring_method))
  }
  if (inherits(scoring_func, what = 'try-error')){
      if(any(length_details > 0)){
          details <- detail_vars[which(length_details > 0)]
          message('Details: ', 
                  paste(paste(names(details), details, sep = ': '), collapse = ', '),
                  '.')
      } else {
          message('No scale or subscale info.')
      }  
    stop(paste0('Scoring method "',scoring_method,'" not found.'))
  }
  if(na.rm) {
    do_for_na<-na.exclude
  } else {
    do_for_na<-na.pass
  }
  return(scoring_func(do_for_na(item_values)))
}

#' Reverse score
#'
#' @param item_values item_values
#' @param min min
#' @param max max
reverse_score<-function(
  item_values,
  min=stop('Must specify min and max score.'),
  max=stop('Must specify min and max score.')){
  rev_items<-max+min-item_values
  rev_items
}

#' RPI flip
#'
#' @param item_values item_values
rpi_flip<-function(item_values){
  one_indexes<-item_values==1
  two_indexes<-item_values==2
  item_values[one_indexes]<-2
  item_values[two_indexes]<-1
  item_values
}

#' transform scores
#'
#' @param item_values item_values
#' @param transformation transformation
#' @param min min
#' @param max max
transform_scores<-function(item_values,transformation,min=NA,max=NA){
  #`transformation` should be a function defined in terms of x that will
  #	be applied to the vector of item_values.
  #Conditional logic will be extracted for subsetting, also in terms of x.
  #`min` and `max` will be passed to `reverse_score` and `pomp_score`.
  #check that all elements in `transformation` are the same
  if(!all(transformation[1]==transformation))
    stop('Scoring methods not all the same. Check that you\'re
         grouping rows correctly, and that the rubric is correct.')
  transformation<-unique(transformation)
  if(regexpr('^if *(\\(.*\\))( | *\\{).*',transformation) != -1){
    logic<-sub('^if *(\\(.*\\))( | *\\{).*','\\1',transformation)
    aFunction<-sub('^if *(\\(.*\\))( | *\\{)(.*)','\\3',transformation)
  }
  else {
    logic<-NULL
    aFunction<-transformation
  }
  if(grepl('reverse_score$',aFunction)){
    aFunction<-paste(aFunction,'(x,min,max)')
  }
  if(!grepl('^function(x)',aFunction)){
    aFunction<-paste('function(x) ',aFunction)
  }
  aFunction<-eval(parse(text=aFunction))
  x<-item_values
  if(is.null(logic)){
    aFunction(x)
  }
  else {
    ifelse(
      eval(parse(text=logic)),
      aFunction(x),
      x)
  }
}

#' Score questionnaire
#'
#' @param dataDF A data.frame as returned by \code{\link{get_survey_data}}
#' @param rubricsDF A data.frame as returned by \code{\link{get_rubrics}}
#' @param psych A flag specifying whether \code{\link[psych]{psych-package}} should be used for scoring. Default is \code{FALSE} but you should probably set it to \code{TRUE} unless you're using this for a DSN lab project
#' @param ... Other arguments passed to one of the scoring engines.
#'
#' @return A long-form data.frame with scale scores.
#' @export
score_questionnaire<-function(dataDF, rubricsDF, psych = FALSE, ...){
  if(psych){
    return(score_questionnaire_psych(dataDF, rubricsDF, ...))
  } else {
    return(score_questionnaire_dsn(dataDF, rubricsDF, ...))
  }
}

#' Score questionnaire dsn
#'
#' @param dataDF dataDF
#' @param rubricsDF rubricsDF
#'
#' @import dplyr
#' @import tidyr
score_questionnaire_dsn <- function(dataDF,rubricsDF){
  #Takes long-form question data and rubric data, and a logical
  # value specifying whether to use _part2 rubrics.
  #Returns scored scales.
  if(any(dim(dataDF)[1] < 1, dim(rubricsDF)[1] < 1)){
      return(data.frame())
  }
  scores_with_scoring_params<-rubricsDF  %>%
    left_join(
      dataDF,
      by=c(
        #"data_file_name" = "survey_name",
        "column_name" = "item")) %>%
    filter(!(include %in% c(0,NA,'0','NA',''))) # this filters the rubrics

  nonNumeric_items <- scores_with_scoring_params %>% filter(include %in% 'I')
  
  if(!is.na(dim(nonNumeric_items)[1]) && dim(nonNumeric_items)[1] > 0){
      non_numeric <- nonNumeric_items %>%
          mutate(na.rm=F) %>%
          group_by(survey_name,scale_name,scored_scale,SID) %>%
          summarise(
              score=scorequaltrics:::score_items(
                  value,
                  include,
                  scale_name=scale_name[[1]],
                  scored_scale=scored_scale[[1]],
                  SID = SID[[1]]),
              n_items=sum(!is.na(value)),
              n_missing=sum(is.na(value)),
              method=unique(include))
  } else {
      non_numeric <- data.frame()
  }

  transform_scored<-
    scores_with_scoring_params %>%
    group_by(column_name) %>%
    mutate(
      value=as.numeric(
        ifelse(
          !(transform %in% c(0,NA,'')),
          scorequaltrics:::transform_scores(
            value,
            transform,
            min=as.numeric(min[[1]]),
            max=as.numeric(max[[1]])),
          value)))

  reverse_scored<-
    transform_scored %>%
    group_by(column_name) %>%
    mutate(
      value=as.numeric(
        ifelse(
          reverse %in% 1,
          scorequaltrics:::reverse_score(
            value,
            min=as.numeric(min[[1]]),
            max=as.numeric(max[[1]])),
          value))) %>%
    filter(!include %in% 'I')

  scored<-reverse_scored %>%
    mutate(na.rm=F) %>%
    group_by(survey_name,scale_name,scored_scale,SID) %>%
    summarise(
      score=scorequaltrics:::score_items(
          value,
          include,
          scale_name=scale_name[[1]],
          scored_scale=scored_scale[[1]],
          SID = SID[[1]]),
      n_items=sum(!is.na(value)),
      n_missing=sum(is.na(value)),
      method=unique(include)) %>%
    mutate(
      score=as.character(score)) %>%
    bind_rows(non_numeric,.)
  return(scored)
}

#' Score step one and two
#'
#' @param dataDF A data.frame as returned by \code{\link{get_survey_data}}
#' @param rubricsDF A data.frame as returned by \code{\link{get_rubrics}}
#' @return A long-form data.frame with scale scores.
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
score_step_one_and_two<-function(dataDF,rubricsDF){
  #Do step one of the scoring...
  rubricsDF_part1<-rubricsDF %>% filter(!grepl('part2',scale_name))
  scored<-score_questionnaire(dataDF,rubricsDF_part1)
  #To do step 2, we need to get our names right, and make sure we
  # are only using the necessary rows.
  rubricsDF_part2<-rubricsDF %>% ungroup %>%
    filter(grepl('part2',scale_name)) %>%
    mutate(column_name=tolower(column_name)) %>%
    dplyr::select(-file,-data_file_name) %>%
    mutate(data_file_name=scale_name) %>%
    mutate(data_file_name=sub('(.*)_part2','\\1',data_file_name))
  part2_cols<-unique(rubricsDF_part2$column_name)
  scored.part1<-scored %>% filter(!scored_scale %in% part2_cols)
  dataDF.part2<-scored %>%
    filter(scored_scale %in% part2_cols) %>%
    rename(survey_name=scale_name,item=scored_scale,value=score)
  scored.part2<-score_questionnaire(dataDF.part2,rubricsDF_part2)
  all_scored<-bind_rows(scored.part1,scored.part2)
  all_scored
}

#' Clean duplicates
#'
#' @param longDF longDF
#' @param pid_col pid_col
#' @param keep_text keep_text?
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
clean_dupes <- function(longDF, pid_col = 'ID', keep_text = FALSE){
    cleanedDF <- longDF %>% ungroup() %>%
        group_by_at(.vars = c(pid_col, 'item')) %>% #group by rows with the same item name and SID
        do({ #for each group
            if(keep_text){
                values <- na.exclude(ifelse(.$value == '', NA, .$value))
            } else {
                values <- na.exclude(as.numeric(.$value)) #get values in `values` column, make numeric (which yields NA if value==''), and exclude NA (no info, and no possible conflict)
            }
            dropped <- FALSE #Keep track of whether we have to drop this observation due to conflicts
            if(length(values>0) && all(values==values[[1]])){ #if there are multiple values, but they agree (all values are equal to the first value)
                bestValue <- values[[1]] #then just take the first value
            } else {
                #If after excluding missing values, the values are not all the same, this means
                #there must be more than one value, and there are differences between them.
                #There's not a good heuristic here -- this means that either they took the
                #questionnaire twice and answered differently (in which case, which is the 'right' answer),
                #or there is a problem with the SID column, and we need to figure out if
                #there is a miscoding of SID. Either way, there is some manual intervention needed so
                #we set the value to NA, and flag this observation as dropped.
                bestValue <- NA
                dropped <- TRUE
            }
            adf <- .[1,] #retain all the info from the first row of this group
            adf$old.value <- list(values) #wrap up all the values from the group, and save them to aid error checking
            adf$value <- bestValue #set the value to what we decided above
            adf$dropped <- dropped #set the dropped flag
            adf #return the new data frame, which should now be 1 row per item per SID.
        })
}

#' Create \code{psych} key from rubric
#'
#' @param rubricsDF rubricsDF
#' @param scale_name scale_name
#'
#' @return a key list for use in the \code{psych} package.
#' @export
#'
#' @import dplyr
#' @import tidyr
create_key_from_rubric <- function(rubricsDF, scale_name = NULL){
    if(!is.null(scale_name)){
        rubricsDF <- filter(rubricsDF, scale_name == scale_name)
    }

    if('include' %in% names(rubricsDF)){
        rubricsDF <- ungroup(filter(rubricsDF, include == 1))
    }
    if('reverse' %in% names(rubricsDF)){
        rubricsDF <- ungroup(mutate(rubricsDF,
                                    reverse = ifelse(is.na(reverse), 0, reverse),
                                    rscore_col_name = paste0(ifelse(reverse == 1, '-', ''), column_name)))
    } else {
        warning('No reverse-keyed items.')
        rubricsDF <- ungroup(mutate(rubricsDF,
                                    rscore_col_name = column_name))
    }

    keys_list_l <- select(rubricsDF, rscore_col_name, scored_scale)

    scored_scale_names <- unique(keys_list_l$scored_scale)
    key_list <- lapply(scored_scale_names, function(x){
        keys_list_l$rscore_col_name[keys_list_l$scored_scale == x]
    })
    names(key_list) <- scored_scale_names
    return(key_list)
}

#' score questionnaire psych
#'
#' @param dataDF dataDF
#' @param rubricsDF rubricsDF
#' @param scale_name scale_name
#' @param return_with_data return_with_data
#'
#' @import psych
#' @import dplyr
#' @import tidyr
score_questionnaire_psych <- function(dataDF, rubricsDF, scale_name = NULL, return_with_data = FALSE){
    require(psych)

    key_list <- create_key_from_rubric(rubricsDF = rubricsDF, scale_name = scale_name)

    dataDF_w <- spread(select(dataDF, SID, item, value),
                       item, value)
    scored_scales <- scoreItems(key_list, dataDF_w)
    if(return_with_data){
        scored_scales$input_data <- dataDF_w
    }
    rownames(scored_scales$scores) <- dataDF_w$SID
    return(scored_scales)
}


#' Recode repsonses
#'
#' Recodes responses according to a rubric.
#'
#' @param dataDF A long data frame.
#' @param recoding_rubric A recoding rubric with columns `column_name`,
#'   `response`, and `score`
#'
#' @return the data frame passed to it, with each value in `value` replaced with
#'   the recoded value in `score` from the template.
#' @export
#' @import data.table
recode_responses <- function(dataDF, recoding_rubric){
    recoding_rubric_reduced <- select(ungroup(recoding_rubric),
                                      column_name, response, score)

    dataDF_recoding <- dataDF %>%
        left_join(recoding_rubric_reduced,
                  by = c("item" = "column_name", "value" = "response")) %>%
        as.data.table

    to_recode <- dataDF_recoding %>%
        ungroup() %>%
        filter(!is.na(score),
               score != value) %>%
        summarize(N_recoded = n())

    dataDF_recoding[, value := ifelse(!is.na(score),
                                      score,
                                      value)]
    dataDF_recoding[, score := NULL]

    message('A total of ', to_recode$N_recoded, ' items recoded.')

    return(as.data.frame(dataDF_recoding))
}

#' Widen Qualtrics Long
#'
#' @param dataDF a scored data frame
#' @param scale_names a vector of scale names to be matched exactly
#'
#' @return a list with \code{scores}, a wide format data frame with scale scores,
#' and \code{data_quality}, a wide format data frame with number if missing and used items
#' for each scale.
#' @export
#' @import tidyr
#' @import dplyr
widen_qualtrics_long <- function(dataDF, scale_names){
    dataDF_scores <- dataDF %>% dplyr::ungroup() %>%
        dplyr::filter(scale_name %in% scale_names) %>%
        dplyr::select(SID, score, scored_scale) %>%
        tidyr::spread(scored_scale, score)

    dataDF_data_quality <- dataDF %>% dplyr::ungroup() %>%
        dplyr::filter(scale_name %in% scale_names) %>%
        dplyr::select(SID, scored_scale, n_items, n_missing) %>%
        tidyr::gather(attribute, value, n_items, n_missing) %>%
        tidyr::unite(scored_scale_attribute, scored_scale, attribute) %>%
        tidyr::spread(scored_scale_attribute, value)

    return(list(scores = dataDF_scores, data_quality = dataDF_data_quality))
}


#' Plot Scored Scale
#'
#' @param aDF a scored data frame in long format with columns \code{scale_name},
#' \code{scored_scale}, and \code{score}
#' @param scale_regx a regular expression that selects scales from column \code{scale_name} in \code{aDF}
#' @param type Can be 'score', 'n_missing' (aDF must have column \code{n_missing}), or 'p_missing'
#' (aDF must have columns \code{n_missing} and \code{n_items}).
#' @param by_gender logical flag to facet by gender.
#' @param gender_var name of the column that contains gender information for faceting.
#'
#' @return a gggplot
#' @export
#' @import ggplot2
#' @import dplyr
#'
plot_scored_scale <- function(aDF, scale_regx = '.*', type = 'score', by_gender = FALSE, gender_var = NA){
    numeric_cols <- c('score', 'n_missing', 'n_items')
    numeric_cols <- numeric_cols[numeric_cols %in% names(aDF)]
    aDF <- aDF %>%
        filter(grepl(scale_regx, scale_name)) %>%
        mutate_at(numeric_cols, as.numeric)

    if(length(unique(aDF$scale_name)) > 1){
        warning('Matched multiple scales: "', paste(unique(aDF$scale_name), collapse = '", "'), '".')
    }

    if (type == 'score'){
        colname <- 'score'
        ylab <- 'Scale Score'
    } else if (type == 'n_missing') {
        colname <- 'n_missing'
        ylab <- 'Number of missing responses'
    } else if (type == 'p_missing') {
        aDF$p_missing <- aDF$n_missing/(aDF$n_items + aDF$n_missing)
        colname <- 'p_missing'
        ylab <- 'Proportion of missing responses'
    }

    p <- ggplot(aDF, aes_string(y = colname, x = 'scored_scale')) +
        geom_violin(fill = 'black', alpha = .25, color = 'gray') +
        geom_boxplot(alpha = .5, width = .25, color = '#555555') +
        geom_point(position = position_jitter(w = .125, h = .05),
                   alpha = .3, color = 'blue', size = .75) +
        labs(y = ylab, x = 'Scale name') +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 360-70, hjust = 0))

    if(by_gender){
        p <- p + facet_grid(reformulate(gender_var, '.'))
    }
    return(p)
}


#' Longen a wide psych-scored scale
#'
#' @param psychMat a matrix from the \code{$scores} element of a \code{psych} object.
#' @param scale_name use to specify the name of the scale.
#' @param id_colname use to set the column name of for the ids (taken from the
#' rownames of the \code{psychMat}).
#'
#' @return a long data frame with an id column defined by id_colname, 'scale_name',
#' 'scored_scale', and 'score'
#' @export
#'
#' @import dplyr
#' @import tidyr
longen_psych_wide <- function(psychMat, scale_name = 'scale', id_colname = 'id'){
    psychDF <- as.data.frame(psychMat)
    psychDF$id <- rownames(psychDF)
    names(psychDF)[length(names(psychDF))] <- id_colname
    psychDF$scale_name <- scale_name
    psychDF_long <- psychDF %>%
        tidyr::gather(key = "scored_scale", value = "score",
                      -one_of(c('scale_name', id_colname)))
    return(psychDF_long)
}

#' Make nice scale filename
#'
#' @param scale_name Name of the scale (a character)
#'
#' @return A character that would be nice to compose into a filename.
#' @export
make_nice_scale_fname <- function(scale_name){
    scale_fname <- gsub('\\.',
                        '_',
                        make.names(scale_name))
    return(scale_fname)
}

#' Write widened scored scale
#'
#' @param dataDF A long data frame.
#' @param scale_names A character vector of scale names to widen and write.
#' @param dir_name The output directory to save csv files.
#' @param file_name A custom file name.
#' @param metadata List of key-value pairs that will be added as
#' column name - column values to the returned data frames.
#' @import tibble
#' @import rlang
#'
#' @export
write_widened_scored_scale <- function(dataDF, scale_names = NULL,
                                       dir_name = NULL, file_name = NULL,
                                       metadata = NULL){
    if(is.null(scale_names)){
        scale_names <- unique(dataDF$scale_name)
    }
    if(is.null(file_name)){
        if(length(scale_names) == 1){
            file_name <- paste0(make_nice_scale_fname(scale_names), '.csv')
        } else {
            file_name <- 'scored_scales_wide.csv'
        }
    }
    if(is.null(dir_name)){
        dir_name <- getwd()
    }
    wide_data_frame <- scorequaltrics::widen_qualtrics_long(dataDF,
                                                            scale_names = scale_names)
    if(!is.null(metadata)){
        wide_data_frame <- lapply(wide_data_frame, tibble::add_column, rlang::UQS(metadata))
    }
    full_file_name <- file.path(dir_name, file_name)
    message('Writing to ', full_file_name)
    write.csv(wide_data_frame, file = full_file_name, row.names = F)
}
