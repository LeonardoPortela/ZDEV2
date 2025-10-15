*&---------------------------------------------------------------------*
*& Report ZTEST_WSB_003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report ztest_wsb_003.

parameters p_valeur type string.

data(mon_singleton) = zcl_temp_singleton_001=>get_instance(  ).
mon_singleton->set_data( p_valeur ).

perform p_display_parameter in program ztest_wsb_003b.
