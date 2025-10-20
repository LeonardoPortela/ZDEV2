class ZCL_IM_ENH_COT_DEL_HEADER definition
  public
  final
  create public .

public section.

  interfaces IF_EX_LE_SHP_TAB_CUST_HEAD .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ENH_COT_DEL_HEADER IMPLEMENTATION.


  METHOD if_ex_le_shp_tab_cust_head~activate_tab_page.

    DATA:  lv_mostrar  TYPE char1.

    CALL FUNCTION 'ZDELIV_HEAD_SCREEN_VAL_ENABLE'
      EXPORTING
        im_lfart   = is_likp-lfart
        im_vstel   = is_likp-vstel
        im_werks   = is_likp-werks
      IMPORTING
        ex_mostrar = lv_mostrar.

    CHECK lv_mostrar IS NOT INITIAL.

    ef_caption = 'Datos Remito'.
    ef_position = '13'.
    ef_program = 'SAPLZFG_COT_DELIVERY_ENHC'.
    ef_dynpro = '5555'.
    cs_v50agl_cust = 'X'.

    CALL FUNCTION 'ZSET_DELIVERY_HEAD_SCREEN_VAL'
      EXPORTING
        im_likp       = is_likp.




  ENDMETHOD.


  METHOD if_ex_le_shp_tab_cust_head~pass_fcode_to_subscreen.



  ENDMETHOD.


  METHOD if_ex_le_shp_tab_cust_head~transfer_data_from_subscreen.

    CALL FUNCTION 'ZGET_DELIVERY_HEAD_SCREEN_VAL'
      IMPORTING
        ex_likp = cs_likp.

  ENDMETHOD.


  METHOD if_ex_le_shp_tab_cust_head~transfer_data_to_subscreen.

*    CALL FUNCTION 'ZSET_DELIVERY_HEAD_SCREEN_VAL'
*      EXPORTING
*        im_likp = is_likp.


  ENDMETHOD.
ENDCLASS.
