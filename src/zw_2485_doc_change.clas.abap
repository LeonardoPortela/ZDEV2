class ZW_2485_DOC_CHANGE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_IMP_AEI_WS_2485_DOC_CHANGE .
protected section.
private section.
ENDCLASS.



CLASS ZW_2485_DOC_CHANGE IMPLEMENTATION.


  METHOD if_imp_aei_ws_2485_doc_change~change_data.
  ENDMETHOD.


method IF_IMP_AEI_WS_2485_DOC_CHANGE~CHANGE_DOC.



endmethod.


  METHOD if_imp_aei_ws_2485_doc_change~change_per_asoc.

    DATA: lv_auart    TYPE vbak-auart.
    DATA: lv_days     TYPE string.
    DATA: lv_fchdesde TYPE string.
    DATA: lv_fchhasta TYPE string.
    DATA: lv_year(4)  TYPE c.
    DATA: lv_month(2) TYPE c.
    DATA: lv_day(2)   TYPE c.
    DATA: lv_days_f   TYPE t5a4a-dlydy.
    DATA: lv_date     TYPE p0001-begda.
    DATA: ls_vbfa TYPE vbfa.
    DATA: ls_vbrk TYPE vbrk.

    CONSTANTS lc_tvarv_perio TYPE c LENGTH 22 VALUE 'ZFACTURA_ELE_AR_FECHAS'.


    IF is_cae_det-cbteasoc IS INITIAL AND ( is_j_1acae EQ 'C' OR is_j_1acae EQ 'D' ) .


*      READ TABLE  lt_vbfa_sales INTO ls_vbfa INDEX 1.
*      IF sy-subrc IS INITIAL.
*        SELECT SINGLE auart FROM vbak INTO lv_auart WHERE vbeln = ls_vbfa-vbelv.
*        IF sy-subrc IS INITIAL.
*          SELECT SINGLE high FROM tvarvc INTO lv_days WHERE name = lc_tvarv_perio AND low = lv_auart.
*          IF sy-subrc IS INITIAL.
*            READ TABLE lt_vbrk INTO ls_vbrk WITH KEY vbeln = ls_vbfa-vbeln.
*            IF sy-subrc IS INITIAL.
*              lv_days_f = lv_days.
*              CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*                EXPORTING
*                  date      = ls_vbrk-fkdat
*                  days      = lv_days_f
*                  months    = 0
*                  signum    = '-'
*                  years     = 0
*                IMPORTING
*                  calc_date = lv_date.
*              lv_fchdesde = lv_date.
**          lv_fchdesde = ls_vbrk-fkdat - lv_days.
*              lv_year  = ls_vbrk-fkdat+0(4).
*              lv_month = ls_vbrk-fkdat+4(2).
*              lv_day   = ls_vbrk-fkdat+6(2).
*              CONCATENATE lv_year lv_month lv_day INTO lv_fchhasta."wa_periodo_asoc-fchhasta."wa_periodo_asoc-fchdesde
*              lv_year  = lv_fchdesde+0(4).
*              lv_month = lv_fchdesde+4(2).
*              lv_day   = lv_fchdesde+6(2).
*              CLEAR lv_fchdesde.
*              CONCATENATE lv_year lv_month lv_day INTO lv_fchdesde."wa_periodo_asoc-fchhasta."wa_periodo_asoc-fchdesde
*
*              wa_periodo_asoc-fchhasta = lv_fchhasta.
*              wa_periodo_asoc-fchdesde = lv_fchdesde.
*              IF wa_periodo_asoc-fchhasta IS NOT INITIAL.
*                gv_flag_periodo = 'X'.
*              ENDIF.
**          APPEND wa_periodo_asoc TO lt_periodo_asoc.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*


    ENDIF.

  ENDMETHOD.


method IF_IMP_AEI_WS_2485_DOC_CHANGE~CHANGE_REJ_DOC.
endmethod.
ENDCLASS.
