*&---------------------------------------------------------------------*
*& Report ZTEST_WSB_003B
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report ztest_wsb_003b.

form p_display_parameter.
  data(mon_singleton) = zcl_temp_singleton_001=>get_instance(  ).
  write /1 mon_singleton->get_data( ).
endform.
