*&---------------------------------------------------------------------*
*& Report  ZRD_ZGLT0102_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt100_exit.

FORM f_exit_zglt100_0001 CHANGING p_registro_manter TYPE any.

*  DATA: wl_ZGLT0102 TYPE ZGLT0102.
*
*  CLEAR: wl_ZGLT0102.
*
*
*  MOVE-CORRESPONDING wl_ZGLT0102 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt100_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.


  DATA: wl_zglt100 TYPE zglt100.
  DATA: lv_num    TYPE cms_dte_seqno,
        lv_arr(2) TYPE n,
        lv_lifnr  TYPE lfa1-lifnr,
        lv_n(10)  TYPE n,
        lv_kunnr  TYPE kna1-kunnr.

  FREE: wl_zglt100.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt100.
*  lv_arr = wl_zglt100-tp_arrendamento.
*  wl_zglt100-tp_arrendamento = lv_arr.

  CONCATENATE wl_zglt100-mes_ano_comp+3(4) wl_zglt100-mes_ano_comp(2) '01' INTO wl_zglt100-competencia.
  MOVE-CORRESPONDING wl_zglt100 TO p_registro_manter.

*
**number_get_next
*  IF sy-ucomm EQ 'NOVO' AND wl_zglt100-cod_contrato IS NOT INITIAL.
*
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr = '01'
*        object      = 'ZGLE0038'
**       QUANTITY    = '1'
**       SUBOBJECT   = ' '
**       TOYEAR      = '0000'
**       IGNORE_BUFFER                 = ' '
*      IMPORTING
*        number      = lv_num
**       QUANTITY    =
**       RETURNCODE  =
** EXCEPTIONS
**       INTERVAL_NOT_FOUND            = 1
**       NUMBER_RANGE_NOT_INTERN       = 2
**       OBJECT_NOT_FOUND              = 3
**       QUANTITY_IS_0                 = 4
**       QUANTITY_IS_NOT_1             = 5
**       INTERVAL_OVERFLOW             = 6
**       BUFFER_OVERFLOW               = 7
**       OTHERS      = 8
*      .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*
*    ELSE.
*      IF lv_num IS NOT INITIAL.
*        wl_zglt100-serial_no = lv_num.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IF ( sy-ucomm EQ 'NOVO' OR sy-ucomm EQ 'CHANGE' ) AND wl_zglt100-cod_contrato IS NOT INITIAL.
*
*    IF wl_zglt100-fornecedor IS NOT INITIAL.
*
*      lv_n = wl_zglt100-fornecedor.
*      lv_lifnr = lv_n.
*
*      SELECT SINGLE name1
*        FROM lfa1
*        INTO wl_zglt100-name1
*      WHERE lifnr EQ lv_lifnr.
*    ENDIF.
*
*    IF wl_zglt100-cliente IS NOT INITIAL.
*
*      lv_n = wl_zglt100-cliente.
*      lv_kunnr = lv_n.
*
*      SELECT SINGLE name1
*        FROM kna1
*        INTO wl_zglt100-name1
*      WHERE kunnr EQ lv_kunnr.
*    ENDIF.
*
*    IF wl_zglt100-name1 IS NOT INITIAL.
*      MOVE-CORRESPONDING wl_zglt100 TO p_registro_manter.
*    ENDIF.
*
*  ENDIF.
*
**  DATA: wl_ZGLT0102 TYPE ZGLT0102.
**  CLEAR: wl_ZGLT0102.
**  MOVE-CORRESPONDING p_registro_manter TO wl_ZGLT0102.
**
**if wl_ZGLT0102-tp_imposto is not INITIAL.
**select single DESC_IMPOST into wl_ZGLT0102-desc_tp_imposto from ZGLT0100 where tp_imposto = wl_ZGLT0102-tp_imposto.
**
**ENDIF.
**  MOVE-CORRESPONDING wl_ZGLT0102 TO p_registro_manter.
**
**
**  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt100_0003 CHANGING p_saida TYPE any.

*  DATA: wl_ZGLT0102 TYPE ZGLT0102.
*
*  CLEAR: wl_ZGLT0102.
*
*  MOVE-CORRESPONDING p_saida TO wl_ZGLT0102.
*
*  MOVE-CORRESPONDING wl_ZGLT0102 TO p_saida.


ENDFORM.

FORM f_exit_zglt100_0004 CHANGING p_saida TYPE any.

*  DATA: wl_ZGLT0102_out TYPE ZGLT0102_out.
*
*  CLEAR: wl_ZGLT0102_out.
*
*  MOVE-CORRESPONDING p_saida TO wl_ZGLT0102_out.
*  CLEAR p_saida.
*if wl_ZGLT0102_out-tp_imposto is NOT INITIAL.
*  select single DESC_IMPOST into wl_ZGLT0102_out-desc_tp_imposto from ZGLT0100 where tp_imposto = wl_ZGLT0102_out-tp_imposto.
*ENDIF.
*
* CASE wl_ZGLT0102_out-saldo.
*      WHEN '1'.
*        wl_ZGLT0102_out-dsaldo = 'Positivo'.
*      WHEN '2'.
*        wl_zglt0102_out-dsaldo = 'Negativo'.
*      WHEN OTHERS.
*        WRITE ' '.
*    ENDCASE.
*
*    MOVE-CORRESPONDING wl_ZGLT0102_out TO p_saida.
ENDFORM.

FORM f_exit_zglt100_0005 CHANGING p_registro_manter TYPE any.

*  DATA: wl_ZGLT0102_out TYPE ZGLT0102_out.
*  DATA: wl_t001 TYPE  t001,
*        wl_lfa1 TYPE  lfa1.
*
*  CLEAR: wl_ZGLT0102_out, wl_t001, wl_lfa1.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_ZGLT0102_out.
*
*
*  MOVE-CORRESPONDING wl_ZGLT0102_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zglt100_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

*  IF p_db_tab = 'ZGLT0102'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
ENDFORM.
