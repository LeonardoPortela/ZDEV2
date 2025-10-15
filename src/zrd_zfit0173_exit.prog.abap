*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0173_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0173_exit.

FORM f_exit_zfit0173_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0173 TYPE zfit0173.

  CLEAR: wl_zfit0173.

  wl_zfit0173-dt_atual = sy-datum.
  wl_zfit0173-hr_atual = sy-uzeit.
  wl_zfit0173-usnam    = sy-uname.

  MOVE-CORRESPONDING wl_zfit0173 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0173_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfit0173 TYPE zfit0173.

  CLEAR: wl_zfit0173.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0173.

  CLEAR: p_error.

  IF wl_zfit0173-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Código Empresa Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.


  IF wl_zfit0173-dn_empresa IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Distinct Name Empresa Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zfit0173-bc_empresa IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Bit Code Empresa Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.
  IF wl_zfit0173-dn_banco IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Distinct Name Banco Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.
  IF wl_zfit0173-bc_banco IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Bit Code Banco Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.


  IF wl_zfit0173-hmt101 IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Header HMT101 Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.
  IF wl_zfit0173-tmt101 IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Trailer MT101 Obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.



  IF  wl_zfit0173-hbkid IS NOT INITIAL.

    DATA: lit_t012 TYPE TABLE OF t012 WITH HEADER LINE.

    SELECT *
      FROM t012 INTO TABLE lit_t012
     WHERE bukrs EQ wl_zfit0173-bukrs
          AND hbkid EQ wl_zfit0173-hbkid.

    IF lit_t012[] IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Banco Empresa não Cadastrado!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_exit_zfit0173_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0173 TYPE zfit0173.

  CLEAR: wl_zfit0173.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0173.

  wl_zfit0173-dt_atual = sy-datum.
  wl_zfit0173-hr_atual = sy-uzeit.
  wl_zfit0173-usnam = sy-uname.

  MOVE-CORRESPONDING wl_zfit0173 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0173_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfit0173_out TYPE zfit0173_out.

  CLEAR: wl_zfit0173_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0173_out.

  MOVE-CORRESPONDING wl_zfit0173_out TO p_saida.

ENDFORM.
FORM f_exit_zfit0173_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zfit0173_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.
ENDFORM.

FORM f_exit_zfit0173_0007 TABLES p_table.

ENDFORM.

FORM f_exit_zfit0173_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'zfit0173_OUT' AND
     p_field       = 'TP_COMUNICA'.
  ENDIF.

  IF p_ref_tabname = 'zfit0173_OUT' AND
     p_field       = 'STATUS'.
  ENDIF.

  IF p_ref_tabname = 'zfit0173_OUT' AND
   p_field       = 'BAIXA_ARQ'.
  ENDIF.
ENDFORM.


FORM f_exit_zfit0173_0009  TABLES it_excl_toolbar
                           USING p_db_tab.

ENDFORM.
