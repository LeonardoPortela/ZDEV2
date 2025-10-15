*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT097_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zgl015_dre_ccust_exit.

FORM f_exit_ZGL015_DRE_CCUST_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_ZGL015_DRE_CCUST TYPE ZGL015_DRE_CCUST.

  CLEAR: wl_ZGL015_DRE_CCUST.

  wl_ZGL015_DRE_CCUST-usnam     = sy-uname.
  wl_ZGL015_DRE_CCUST-zdt_atual = sy-datum.
  wl_ZGL015_DRE_CCUST-zhr_atual = sy-uzeit.

  MOVE-CORRESPONDING wl_ZGL015_DRE_CCUST TO p_registro_manter.

ENDFORM.

FORM f_exit_ZGL015_DRE_CCUST_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.


ENDFORM.

FORM f_exit_ZGL015_DRE_CCUST_0003 CHANGING p_registro_manter TYPE any.

*  DATA: wl_zglt097 TYPE zglt097.
*
*  CLEAR: wl_zglt097.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zglt097.
*
*  wl_zglt097-usnam = sy-uname.
*  wl_zglt097-zdt_atual = sy-datum.
*  wl_zglt097-zhr_atual = sy-uzeit.
*
*  MOVE-CORRESPONDING wl_zglt097 TO p_registro_manter.

ENDFORM.

FORM f_exit_ZGL015_DRE_CCUST_0004 CHANGING p_saida TYPE any.

*  DATA: wl_zglt097_out TYPE zglt097_out.
*
*  CLEAR: wl_zglt097_out.
*
*  MOVE-CORRESPONDING p_saida TO wl_zglt097_out.
*
*  MOVE-CORRESPONDING wl_zglt097_out TO p_saida.

ENDFORM.
FORM f_exit_ZGL015_DRE_CCUST_0013 TABLES p_table.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZGL015_DRE_CCUST'
      tabfirst = 'X'.

ENDFORM.
