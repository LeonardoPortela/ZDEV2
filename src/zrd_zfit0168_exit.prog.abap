*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0168_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0168_exit.

FORM f_exit_zfit0168_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0168 TYPE zfit0168.

  CLEAR: wl_zfit0168.

  wl_zfit0168-dt_registro = sy-datum.
  wl_zfit0168-hr_registro = sy-uzeit.
  wl_zfit0168-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zfit0168 TO p_registro_manter.

ENDFORM.
FORM f_exit_zfit0168_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfit0168 TYPE zfit0168.

  CLEAR: wl_zfit0168.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0168.

  CLEAR: p_error.

  IF wl_zfit0168-tp_comunica IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Tipo de Comunicação Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zfit0168-end_arquivo IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Endereço Arquivo Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zfit0168-status IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Status Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zfit0168-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Empresa Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF  wl_zfit0168-hbkid IS NOT INITIAL.

    DATA: lit_t012 TYPE TABLE OF t012 WITH HEADER LINE.

    SELECT *
      FROM t012 INTO TABLE lit_t012
     WHERE bukrs EQ wl_zfit0168-bukrs
          AND hbkid EQ wl_zfit0168-hbkid.

    IF lit_t012[] IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Banco Empresa não Cadastrado!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0168_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0168 TYPE zfit0168.

  CLEAR: wl_zfit0168.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0168.

  wl_zfit0168-dt_registro = sy-datum.
  wl_zfit0168-hr_registro = sy-uzeit.
  wl_zfit0168-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zfit0168 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0168_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfit0168_out TYPE zfit0168_out.
  DATA: lit_values_tab LIKE TABLE OF dd07v WITH HEADER LINE.

  CLEAR: wl_zfit0168_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0168_out.

*  IF wl_zfit0168_out-tp_comunica IS NOT INITIAL.
*
*    CALL FUNCTION 'GET_DOMAIN_VALUES'
*      EXPORTING
*        domname    = 'ZDO_TPCOM'
*      TABLES
*        values_tab = lit_values_tab.
*
*    READ TABLE lit_values_tab WITH KEY domvalue_l = wl_zfit0168_out-tp_comunica.
*    wl_zfit0168_out-tp_comunica = lit_values_tab-ddtext.  "<- remeber it
*
*  ENDIF.
*
*
*  IF wl_zfit0168_out-status IS NOT INITIAL.
*
*    CALL FUNCTION 'GET_DOMAIN_VALUES'
*      EXPORTING
*        domname    = 'ZDO_STATUS'
*      TABLES
*        values_tab = lit_values_tab.
*
*    READ TABLE lit_values_tab WITH KEY domvalue_l = wl_zfit0168_out-status.
*    wl_zfit0168_out-status = lit_values_tab-ddtext.  "<- remeber it
*
*  ENDIF.
*
*  IF wl_zfit0168_out-baixa_arq IS NOT INITIAL.
*
*    IF wl_zfit0168_out-baixa_arq = '01'.
*      MOVE icon_locked TO wl_zfit0168_out-baixa_arq.
*    ELSE.
*      MOVE icon_unlocked TO wl_zfit0168_out-baixa_arq.
*    ENDIF.
*
*  ENDIF.

  MOVE-CORRESPONDING wl_zfit0168_out TO p_saida.

ENDFORM.

FORM f_exit_zfit0168_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zfit0168_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.
ENDFORM.

FORM f_exit_zfit0168_0007 TABLES p_table.

ENDFORM.

FORM f_exit_zfit0168_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

  IF p_ref_tabname = 'ZFIT0168_OUT' AND
     p_field       = 'TP_COMUNICA'.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0168_OUT' AND
     p_field       = 'STATUS'.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0168_OUT' AND
   p_field       = 'BAIXA_ARQ'.
  ENDIF.
ENDFORM.


FORM f_exit_zfit0168_0009  TABLES it_excl_toolbar
                           USING p_db_tab.

ENDFORM.
