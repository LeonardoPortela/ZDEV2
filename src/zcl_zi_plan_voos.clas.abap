class ZCL_ZI_PLAN_VOOS definition
  public
  final
  create public .

public section.

  class-methods SAVE_DADOS
    importing
      value(I_DADOS) type ZI_PLAN_VOOS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZI_PLAN_VOOS IMPLEMENTATION.


  method save_dados.

    check i_dados is not initial.


    if sy-subrc eq 0.

    endif.



  endmethod.
ENDCLASS.
