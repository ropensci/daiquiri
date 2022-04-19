# fieldtypes_template() generates template fieldtypes output

    fieldtypes( "col1" = ft_ignore(),
    	"col2" = ft_ignore(),
    	"col3" = ft_ignore() )

# fieldtypes object prints to console ok

    Col_tp	<fieldtype_timepoint>	options: includes_time
    Col_uid	<fieldtype_uniqueidentifier>
    Col_cat	<fieldtype_categorical>
    Col_cat2	<fieldtype_categorical>	options: aggregate_by_each_category
    Col_num	<fieldtype_numeric>
    Col_dt	<fieldtype_datetime>	options: includes_time
    Col_dt2	<fieldtype_datetime>
    Col_ft	<fieldtype_freetext>
    Col_sim	<fieldtype_simple>
    Col_ign	<fieldtype_ignore>

