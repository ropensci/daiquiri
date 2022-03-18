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

