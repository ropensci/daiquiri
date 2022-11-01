# template_field_types() generates template field_types output

    field_types(
      "col1" = ft_ignore(),
      "col2" = ft_ignore(),
      "col3" = ft_ignore()
    )

# field_types object prints to console ok

    Col_tp	<timepoint>	options: includes_time
    Col_uid	<uniqueidentifier>
    Col_cat	<categorical>
    Col_cat2	<categorical>	options: aggregate_by_each_category
    Col_num	<numeric>
    Col_dt	<datetime>	options: includes_time
    Col_dt2	<datetime>
    Col_ft	<freetext>
    Col_sim	<simple>
    Col_ign	<ignore>

