# print_field_types_template() generates template field_types output

    field_types( "col1" = ft_ignore(),
    	"col2" = ft_ignore(),
    	"col3" = ft_ignore() )

# field_types object prints to console ok

    Col_tp	<field_type_timepoint>	options: includes_time
    Col_uid	<field_type_uniqueidentifier>
    Col_cat	<field_type_categorical>
    Col_cat2	<field_type_categorical>	options: aggregate_by_each_category
    Col_num	<field_type_numeric>
    Col_dt	<field_type_datetime>	options: includes_time
    Col_dt2	<field_type_datetime>
    Col_ft	<field_type_freetext>
    Col_sim	<field_type_simple>
    Col_ign	<field_type_ignore>

