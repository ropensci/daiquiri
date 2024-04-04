# prepare_data() creates source_data object correctly

    Code
      source_data$validation_warnings
    Output
                   field_name                                            message
                       <char>                                             <char>
       1:       col_timepoint        Missing or invalid value in Timepoint field
       2:       col_timepoint          expected valid date, but got '2021-06-31'
       3:       col_timepoint expected valid date, but got '2021-06-01 33:00:00'
       4:       col_timepoint expected valid date, but got '2021-06-31 02:00:00'
       5:       col_date_time expected valid date, but got '2021-06-31 02:00:00'
       6:       col_date_time expected valid date, but got '2021-06-01 33:00:00'
       7:       col_date_time expected valid date, but got '2021-06-02 24:00:00'
       8:       col_date_time    expected date like , but got '2021-06 05:00:00'
       9:       col_date_time   expected date like , but got '21-06-03 06:00:00'
      10:       col_date_time  expected date like , but got '2021-06-3 07:00:00'
      11:       col_date_only          expected valid date, but got '2021-06-31'
      12:       col_date_only             expected date like , but got '2021-06'
      13:       col_date_only          expected date like , but got '02/06/2021'
      14:       col_date_only           expected date like , but got '2021-06-2'
      15:       col_date_only            expected date like , but got '21-06-03'
      16:         col_date_uk          expected valid date, but got '31/06/2021'
      17:         col_date_uk    expected date like %d/%m/%Y, but got '02/06/21'
      18:         col_date_uk  expected date like %d/%m/%Y, but got '2021-06-03'
      19:   col_numeric_dirty   expected no trailing characters, but got '3.02*'
      20:   col_numeric_dirty                 expected a double, but got 'other'
      21:   col_numeric_dirty    expected no trailing characters, but got '5,04'
      22:   col_numeric_dirty   expected no trailing characters, but got '6.05g'
      23:   col_numeric_dirty                     expected a double, but got '*'
      24:   col_numeric_dirty                expected a double, but got '"9.08"'
      25:   col_numeric_dirty               expected a double, but got ''10.09''
      26:   col_numeric_dirty   expected no trailing characters, but got '11.1''
      27:   col_numeric_dirty               expected a double, but got '`12.11''
      28: col_numeric_missing          expected a double, but got 'not a number'
                   field_name                                            message
          instances
              <int>
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
      28:         4
          instances

# source_data object prints to console ok

    Dataset: completetestset 
    
    Overall:
    Columns in source: 26 
    Columns imported: 13 
    Rows in source: 900 
    Duplicate rows removed: 4 
    Rows imported: 890 
    Column used for timepoint: col_timepoint 
    Min timepoint value: 2021-06-01 03:00:00 
    Max timepoint value: 2022-03-26 
    Rows missing timepoint values removed: 6 
    Strings interpreted as missing values: "","NA","NULL" 
    Total validation warnings: 37 
    
    Datafields:
                      field_name       field_type  datatype count    missing
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
    25 col_numeric_missing_err   ignore           NA           NA NA        
    26 col_numeric_missing       numeric          double        0 890 (100%)
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
    25                  NA                  NA                  NA
    26                  NA                  NA                   4
    
    Validation warnings:
    
                 field_name                                            message
                     <char>                                             <char>
     1:       col_timepoint        Missing or invalid value in Timepoint field
     2:       col_timepoint          expected valid date, but got '2021-06-31'
     3:       col_timepoint expected valid date, but got '2021-06-01 33:00:00'
     4:       col_timepoint expected valid date, but got '2021-06-31 02:00:00'
     5:       col_date_time expected valid date, but got '2021-06-31 02:00:00'
     6:       col_date_time expected valid date, but got '2021-06-01 33:00:00'
     7:       col_date_time expected valid date, but got '2021-06-02 24:00:00'
     8:       col_date_time    expected date like , but got '2021-06 05:00:00'
     9:       col_date_time   expected date like , but got '21-06-03 06:00:00'
    10:       col_date_time  expected date like , but got '2021-06-3 07:00:00'
    11:       col_date_only          expected valid date, but got '2021-06-31'
    12:       col_date_only             expected date like , but got '2021-06'
    13:       col_date_only          expected date like , but got '02/06/2021'
    14:       col_date_only           expected date like , but got '2021-06-2'
    15:       col_date_only            expected date like , but got '21-06-03'
    16:         col_date_uk          expected valid date, but got '31/06/2021'
    17:         col_date_uk    expected date like %d/%m/%Y, but got '02/06/21'
    18:         col_date_uk  expected date like %d/%m/%Y, but got '2021-06-03'
    19:   col_numeric_dirty   expected no trailing characters, but got '3.02*'
    20:   col_numeric_dirty                 expected a double, but got 'other'
    21:   col_numeric_dirty    expected no trailing characters, but got '5,04'
    22:   col_numeric_dirty   expected no trailing characters, but got '6.05g'
    23:   col_numeric_dirty                     expected a double, but got '*'
    24:   col_numeric_dirty                expected a double, but got '"9.08"'
    25:   col_numeric_dirty               expected a double, but got ''10.09''
    26:   col_numeric_dirty   expected no trailing characters, but got '11.1''
    27:   col_numeric_dirty               expected a double, but got '`12.11''
    28: col_numeric_missing          expected a double, but got 'not a number'
                 field_name                                            message
        instances
            <int>
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
    28:         4
        instances

# source_data object prints to console ok when there is a strata field

    Dataset: df 
    
    Overall:
    Columns in source: 7 
    Columns imported: 7 
    Rows in source: 20 
    Duplicate rows removed: 0 
    Rows imported: 20 
    Column used for timepoint: col_timepoint 
    Min timepoint value: 2022-01-01 
    Max timepoint value: 2022-01-05 
    Rows missing timepoint values removed: 0 
    Column used for strata: col_stratify 
    Strata values: SITE1, SITE2, NA    
    Strings interpreted as missing values: "","NULL" 
    Total validation warnings: NA 
    
    Datafields:
                field_name       field_type  datatype count  missing        min
    1 col_timepoint        timepoint        double       20 0 (0%)   2022-01-01
    2 col_numeric          numeric          double       20 0 (0%)            2
    3 col_datetime         datetime         double        9 11 (55%) 2022-01-11
    4 col_uniqueidentifier uniqueidentifier character    20 0 (0%)            1
    5 col_categorical      categorical      character    20 0 (0%)            a
    6 col_simple           simple           character    10 10 (50%)          a
    7 col_stratify         strata           character    18 2 (10%)       SITE1
             max validation_warnings
    1 2022-01-05                   0
    2          3                   1
    3 2022-01-19                   0
    4          9                   1
    5          b                   0
    6          a                   0
    7      SITE2                   0
    
    Validation warnings:
    
                 field_name
                     <char>
    1:          col_numeric
    2: col_uniqueidentifier
                                                                                           message
                                                                                            <char>
    1:  Data supplied as double instead of character, non-conformant values will not be identified
    2: Data supplied as integer instead of character, non-conformant values will not be identified
       instances
           <int>
    1:        NA
    2:        NA

