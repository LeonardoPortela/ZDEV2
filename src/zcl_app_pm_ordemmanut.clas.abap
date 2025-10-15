class ZCL_APP_PM_ORDEMMANUT definition
  public
  final
  create public .

public section.

  class-methods CREATE_DADOS_ORDEM
    importing
      !I_DADOS type ZPMT0082_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APP_PM_ORDEMMANUT IMPLEMENTATION.


  method create_dados_ordem.
    data: gv_wait     type abap_bool value abap_true.

    check i_dados is not initial.


    receive results from function 'ZFU_UPDATE_ORD_MANUT'
      tables
        t_dados_ordem = i_dados.  " Dados programação ordem manutenção


endmethod.
ENDCLASS.
