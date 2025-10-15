*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0250_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0250_exit .

FORM f_exit_zsdt0250_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0250 TYPE zsdt0250.

  CLEAR: wl_zsdt0250.

  MOVE-CORRESPONDING wl_zsdt0250 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0250_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0250 TYPE zsdt0250.

  CLEAR: wl_zsdt0250.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0250.

  CLEAR: p_error.

*  IF wl_zsdt0250-bukrs IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Empresa é campo obritório!' TYPE 'S'.
*    EXIT.
*  ENDIF.


ENDFORM.

FORM f_exit_zsdt0250_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0250 TYPE zsdt0250.

  CLEAR: wl_zsdt0250.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0250.

*  wl_zsdt0250-us_registro = sy-uname.
*  wl_zsdt0250-dt_registro = sy-datum.
*  wl_zsdt0250-hr_registro = sy-uzeit.
*


  MOVE-CORRESPONDING wl_zsdt0250 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0250_0004 CHANGING p_saida TYPE any.


  DATA: w_zsdt0250 TYPE zsdt0250.
  DATA: gt_ztipo TYPE TABLE OF dd07v.
  MOVE-CORRESPONDING p_saida  TO w_zsdt0250.

  IF w_zsdt0250-matnr IS NOT INITIAL. "AND w_zsdt0250-desc_mat IS INITIAL. 110816 Corrigir função modificar material ZSDT0169 - PSA
    SELECT SINGLE maktx
             FROM makt
             INTO w_zsdt0250-desc_mat
            WHERE matnr = w_zsdt0250-matnr.


      MODIFY zsdt0250 FROM w_zsdt0250.
  ELSEIF w_zsdt0250-matnr IS INITIAL.
    CLEAR w_zsdt0250-desc_mat .
  ENDIF.
  IF w_zsdt0250-tp_produto_producao IS NOT INITIAL. "AND w_zsdt0250-desc_tp_pprod IS INITIAL. 110816 Corrigir função modificar material ZSDT0169 - PSA
** Get Descrição
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZDM_TP_PRODUTO_PRODUCAO'
        text            = abap_true
      TABLES
        values_tab      = gt_ztipo
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    IF gt_ztipo[] IS NOT INITIAL.

      READ TABLE gt_ztipo INTO DATA(w_ztipo) WITH KEY domvalue_l = w_zsdt0250-tp_produto_producao.
      IF sy-subrc = 0.
        w_zsdt0250-desc_tp_pprod = w_ztipo-ddtext.

      MODIFY zsdt0250 FROM w_zsdt0250.
      ENDIF.
    ENDIF.

  ELSEIF w_zsdt0250-tp_produto_producao IS INITIAL.
    CLEAR w_zsdt0250-desc_tp_pprod.
  ENDIF.


  MOVE-CORRESPONDING w_zsdt0250 TO p_saida.

ENDFORM.

FORM f_exit_zsdt0250_0005 CHANGING p_registro_manter TYPE any.

  DATA: w_zsdt0250 TYPE zsdt0250.
  DATA: gt_ztipo TYPE TABLE OF dd07v.
  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0250.

  IF w_zsdt0250-matnr IS NOT INITIAL AND w_zsdt0250-desc_mat IS INITIAL.
    SELECT SINGLE maktx
             FROM makt
             INTO w_zsdt0250-desc_mat
            WHERE matnr = w_zsdt0250-matnr.
  ELSEIF w_zsdt0250-matnr IS INITIAL.
    CLEAR w_zsdt0250-desc_mat .
  ENDIF.
  IF w_zsdt0250-tp_produto_producao IS NOT INITIAL. "AND w_zsdt0250-desc_tp_pprod IS INITIAL. 110816 Corrigir função modificar material ZSDT0169 - PSA
** Get Descrição
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZDM_TP_PRODUTO_PRODUCAO'
        text            = abap_true
      TABLES
        values_tab      = gt_ztipo
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    IF gt_ztipo[] IS NOT INITIAL.

      READ TABLE gt_ztipo INTO DATA(w_ztipo) WITH KEY domvalue_l = w_zsdt0250-tp_produto_producao.
      IF sy-subrc = 0.
        w_zsdt0250-desc_tp_pprod = w_ztipo-ddtext.
      ENDIF.
    ENDIF.
    ELSEIF w_zsdt0250-tp_produto_producao IS INITIAL.
    CLEAR w_zsdt0250-desc_tp_pprod.
  ENDIF.

  MOVE-CORRESPONDING w_zsdt0250 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0250_0013 TABLES p_table.


ENDFORM.
