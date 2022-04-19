# prepare_data() creates sourcedata object correctly

    Code
      testsourcedata$validation_warnings
    Output
                  fieldname                                            message
       1:     col_timepoint        Missing or invalid value in Timepoint field
       2:     col_timepoint          expected valid date, but got '2021-06-31'
       3:     col_timepoint expected valid date, but got '2021-06-01 33:00:00'
       4:     col_timepoint expected valid date, but got '2021-06-31 02:00:00'
       5:     col_date_time expected valid date, but got '2021-06-31 02:00:00'
       6:     col_date_time expected valid date, but got '2021-06-01 33:00:00'
       7:     col_date_time expected valid date, but got '2021-06-02 24:00:00'
       8:     col_date_time    expected date like , but got '2021-06 05:00:00'
       9:     col_date_time   expected date like , but got '21-06-03 06:00:00'
      10:     col_date_time  expected date like , but got '2021-06-3 07:00:00'
      11:     col_date_only          expected valid date, but got '2021-06-31'
      12:     col_date_only             expected date like , but got '2021-06'
      13:     col_date_only          expected date like , but got '02/06/2021'
      14:     col_date_only           expected date like , but got '2021-06-2'
      15:     col_date_only            expected date like , but got '21-06-03'
      16:       col_date_uk          expected valid date, but got '31/06/2021'
      17:       col_date_uk    expected date like %d/%m/%Y, but got '02/06/21'
      18:       col_date_uk  expected date like %d/%m/%Y, but got '2021-06-03'
      19: col_numeric_dirty   expected no trailing characters, but got '3.02*'
      20: col_numeric_dirty                 expected a double, but got 'other'
      21: col_numeric_dirty    expected no trailing characters, but got '5,04'
      22: col_numeric_dirty   expected no trailing characters, but got '6.05g'
      23: col_numeric_dirty                     expected a double, but got '*'
      24: col_numeric_dirty                expected a double, but got '"9.08"'
      25: col_numeric_dirty               expected a double, but got ''10.09''
      26: col_numeric_dirty   expected no trailing characters, but got '11.1''
      27: col_numeric_dirty               expected a double, but got '`12.11''
                  fieldname                                            message
          instances
       1:         6
       2:         1
       3:         1
       4:         1
       5:         1
       6:         1
       7:         1
       8:         1
       9:         1
      10:         1
      11:         1
      12:         1
      13:         1
      14:         1
      15:         1
      16:         1
      17:         1
      18:         1
      19:         2
      20:         1
      21:         1
      22:         1
      23:         1
      24:         1
      25:         1
      26:         1
      27:         1
          instances

# sourcedata object prints to console ok

    Class: sourcedata
    Dataset: completetestset 
    
    Overall:
    Columns in source: 24 
    Columns imported: 12 
    Rows in source: 900 
    Duplicate rows removed: 4 
    Rows imported: 890 
    Column used for timepoint: col_timepoint 
    Min timepoint value: 2021-06-01 03:00:00 
    Max timepoint value: 2022-03-26 
    Rows missing timepoint values removed: 6 
    Strings interpreted as missing values: "","NA","NULL" 
    Total validation warnings: 33 
    
    Datafields:
                                              fieldname        fieldtype  datatype
    col_timepoint_err                 col_timepoint_err           ignore        NA
    col_timepoint                         col_timepoint        timepoint    double
    col_date_time_err                 col_date_time_err           ignore        NA
    col_date_time                         col_date_time         datetime    double
    col_date_only_err                 col_date_only_err           ignore        NA
    col_date_only                         col_date_only         datetime    double
    col_date_uk_err                     col_date_uk_err           ignore        NA
    col_date_uk                             col_date_uk         datetime    double
    col_id_num_err                       col_id_num_err           ignore        NA
    col_id_num                               col_id_num uniqueidentifier character
    col_id_string_err                 col_id_string_err           ignore        NA
    col_id_string                         col_id_string uniqueidentifier character
    col_numeric_clean_err         col_numeric_clean_err           ignore        NA
    col_numeric_clean                 col_numeric_clean          numeric    double
    col_numeric_dirty_err         col_numeric_dirty_err           ignore        NA
    col_numeric_dirty                 col_numeric_dirty          numeric    double
    col_categorical_small_err col_categorical_small_err           ignore        NA
    col_categorical_small         col_categorical_small      categorical character
    col_categorical_large_err col_categorical_large_err           ignore        NA
    col_categorical_large         col_categorical_large      categorical character
    col_freetext_err                   col_freetext_err           ignore        NA
    col_freetext                           col_freetext         freetext character
    col_simple_err                       col_simple_err           ignore        NA
    col_simple                               col_simple           simple character
                              count    missing                 min
    col_timepoint_err            NA         NA                  NA
    col_timepoint               890     0 (0%) 2021-06-01 03:00:00
    col_date_time_err            NA         NA                  NA
    col_date_time               878 12 (1.35%) 2021-06-03 08:00:00
    col_date_only_err            NA         NA                  NA
    col_date_only               879 11 (1.24%)          2021-06-03
    col_date_uk_err              NA         NA                  NA
    col_date_uk                 881  9 (1.01%)          2021-06-02
    col_id_num_err               NA         NA                  NA
    col_id_num                  883 7 (0.787%)                  10
    col_id_string_err            NA         NA                  NA
    col_id_string               883 7 (0.787%)               A0126
    col_numeric_clean_err        NA         NA                  NA
    col_numeric_clean           883 7 (0.787%)                   0
    col_numeric_dirty_err        NA         NA                  NA
    col_numeric_dirty           874  16 (1.8%)                   0
    col_categorical_small_err    NA         NA                  NA
    col_categorical_small       883 7 (0.787%)                cat1
    col_categorical_large_err    NA         NA                  NA
    col_categorical_large       883 7 (0.787%)                cat1
    col_freetext_err             NA         NA                  NA
    col_freetext                883 7 (0.787%)                   "
    col_simple_err               NA         NA                  NA
    col_simple                  883 7 (0.787%)           some text
                                              max validation_warnings
    col_timepoint_err                          NA                  NA
    col_timepoint                      2022-03-26                   9
    col_date_time_err                          NA                  NA
    col_date_time             2022-05-02 08:00:00                   6
    col_date_only_err                          NA                  NA
    col_date_only                      2022-03-26                   5
    col_date_uk_err                            NA                  NA
    col_date_uk                        2022-03-26                   3
    col_id_num_err                             NA                  NA
    col_id_num                                 99                   0
    col_id_string_err                          NA                  NA
    col_id_string                           A1019                   0
    col_numeric_clean_err                      NA                  NA
    col_numeric_clean                       30.23                   0
    col_numeric_dirty_err                      NA                  NA
    col_numeric_dirty                       15.14                  10
    col_categorical_small_err                  NA                  NA
    col_categorical_small                    cat7                   0
    col_categorical_large_err                  NA                  NA
    col_categorical_large                    cat9                   0
    col_freetext_err                           NA                  NA
    col_freetext                       some, text                   0
    col_simple_err                             NA                  NA
    col_simple                          some text                   0
    
    Validation warnings:
    
                fieldname                                            message
     1:     col_timepoint        Missing or invalid value in Timepoint field
     2:     col_timepoint          expected valid date, but got '2021-06-31'
     3:     col_timepoint expected valid date, but got '2021-06-01 33:00:00'
     4:     col_timepoint expected valid date, but got '2021-06-31 02:00:00'
     5:     col_date_time expected valid date, but got '2021-06-31 02:00:00'
     6:     col_date_time expected valid date, but got '2021-06-01 33:00:00'
     7:     col_date_time expected valid date, but got '2021-06-02 24:00:00'
     8:     col_date_time    expected date like , but got '2021-06 05:00:00'
     9:     col_date_time   expected date like , but got '21-06-03 06:00:00'
    10:     col_date_time  expected date like , but got '2021-06-3 07:00:00'
    11:     col_date_only          expected valid date, but got '2021-06-31'
    12:     col_date_only             expected date like , but got '2021-06'
    13:     col_date_only          expected date like , but got '02/06/2021'
    14:     col_date_only           expected date like , but got '2021-06-2'
    15:     col_date_only            expected date like , but got '21-06-03'
    16:       col_date_uk          expected valid date, but got '31/06/2021'
    17:       col_date_uk    expected date like %d/%m/%Y, but got '02/06/21'
    18:       col_date_uk  expected date like %d/%m/%Y, but got '2021-06-03'
    19: col_numeric_dirty   expected no trailing characters, but got '3.02*'
    20: col_numeric_dirty                 expected a double, but got 'other'
    21: col_numeric_dirty    expected no trailing characters, but got '5,04'
    22: col_numeric_dirty   expected no trailing characters, but got '6.05g'
    23: col_numeric_dirty                     expected a double, but got '*'
    24: col_numeric_dirty                expected a double, but got '"9.08"'
    25: col_numeric_dirty               expected a double, but got ''10.09''
    26: col_numeric_dirty   expected no trailing characters, but got '11.1''
    27: col_numeric_dirty               expected a double, but got '`12.11''
                fieldname                                            message
        instances
     1:         6
     2:         1
     3:         1
     4:         1
     5:         1
     6:         1
     7:         1
     8:         1
     9:         1
    10:         1
    11:         1
    12:         1
    13:         1
    14:         1
    15:         1
    16:         1
    17:         1
    18:         1
    19:         2
    20:         1
    21:         1
    22:         1
    23:         1
    24:         1
    25:         1
    26:         1
    27:         1
        instances

