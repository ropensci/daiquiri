# prepare_data() creates source_data object correctly

    Code
      testsource_data$validation_warnings
    Output
                 field_name                                            message
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
                 field_name                                            message
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

# source_data object prints to console ok

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
                      field_name       field_type  datatype count  missing
    1  col_timepoint_err         ignore           NA           NA NA      
    2  col_timepoint             timepoint        double      890 0 (0%)  
    3  col_date_time_err         ignore           NA           NA NA      
    4  col_date_time             datetime         double      878 12 (1%) 
    5  col_date_only_err         ignore           NA           NA NA      
    6  col_date_only             datetime         double      879 11 (1%) 
    7  col_date_uk_err           ignore           NA           NA NA      
    8  col_date_uk               datetime         double      881 9 (1%)  
    9  col_id_num_err            ignore           NA           NA NA      
    10 col_id_num                uniqueidentifier character   883 7 (0.8%)
    11 col_id_string_err         ignore           NA           NA NA      
    12 col_id_string             uniqueidentifier character   883 7 (0.8%)
    13 col_numeric_clean_err     ignore           NA           NA NA      
    14 col_numeric_clean         numeric          double      883 7 (0.8%)
    15 col_numeric_dirty_err     ignore           NA           NA NA      
    16 col_numeric_dirty         numeric          double      874 16 (2%) 
    17 col_categorical_small_err ignore           NA           NA NA      
    18 col_categorical_small     categorical      character   883 7 (0.8%)
    19 col_categorical_large_err ignore           NA           NA NA      
    20 col_categorical_large     categorical      character   883 7 (0.8%)
    21 col_freetext_err          ignore           NA           NA NA      
    22 col_freetext              freetext         character   883 7 (0.8%)
    23 col_simple_err            ignore           NA           NA NA      
    24 col_simple                simple           character   883 7 (0.8%)
                       min                 max validation_warnings
    1                   NA                  NA                  NA
    2  2021-06-01 03:00:00          2022-03-26                   9
    3                   NA                  NA                  NA
    4  2021-06-03 08:00:00 2022-05-02 08:00:00                   6
    5                   NA                  NA                  NA
    6           2021-06-03          2022-03-26                   5
    7                   NA                  NA                  NA
    8           2021-06-02          2022-03-26                   3
    9                   NA                  NA                  NA
    10                  10                  99                   0
    11                  NA                  NA                  NA
    12               A0126               A1019                   0
    13                  NA                  NA                  NA
    14                   0               30.23                   0
    15                  NA                  NA                  NA
    16                   0               15.14                  10
    17                  NA                  NA                  NA
    18                cat1                cat7                   0
    19                  NA                  NA                  NA
    20                cat1                cat9                   0
    21                  NA                  NA                  NA
    22                   "          some, text                   0
    23                  NA                  NA                  NA
    24           some text           some text                   0
    
    Validation warnings:
    
               field_name                                            message
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
               field_name                                            message
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

