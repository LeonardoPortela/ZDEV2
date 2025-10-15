class zcl_temp_singleton_001 definition
  public
  final
  create private .


  public section.
    class-methods get_instance
      returning
        value(ro_instance) type ref to zcl_temp_singleton_001.
    methods set_data
      importing
        iv_valeur type string.
    methods get_data
      returning
        value(rv_valeur) type string.


  private section.
    class-data go_instance type ref to zcl_temp_singleton_001.
    data gv_valeur type string.

ENDCLASS.



CLASS ZCL_TEMP_SINGLETON_001 IMPLEMENTATION.


  method get_data.
    rv_valeur = gv_valeur.
  endmethod.


  method get_instance.
    ro_instance = cond #( when go_instance is bound
                            then go_instance
                          else new zcl_temp_singleton_001(  )  ).
    go_instance = ro_instance.
  endmethod.


  method set_data.
    gv_valeur = iv_valeur.
  endmethod.
ENDCLASS.
