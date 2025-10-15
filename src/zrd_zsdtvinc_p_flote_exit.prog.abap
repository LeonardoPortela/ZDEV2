*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDTPROD_FLOTE_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdtvinc_p_flote_exit.


FORM f_exit_zsdtvinc_p_flote_0001 USING p_registro_manter TYPE any.


  DATA: wl_zsdtvinc_p_flote TYPE zsdtvinc_p_flote.

  CLEAR: wl_zsdtvinc_p_flote.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdtvinc_p_flote.

  IF wl_zsdtvinc_p_flote-us_criacao IS INITIAL.
    wl_zsdtvinc_p_flote-dt_criacao      = sy-datum.
    wl_zsdtvinc_p_flote-hr_criacao      = sy-uzeit.
    wl_zsdtvinc_p_flote-us_criacao      = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdtvinc_p_flote TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdtvinc_p_flote_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdtvinc_p_flote TYPE zsdtvinc_p_flote.

  CLEAR: wl_zsdtvinc_p_flote.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdtvinc_p_flote.

*  IF wl_zsdtvinc_p_flote-grup_comp_sap IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Grupo de compradores SAP é obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.

*  IF wl_zsdtvinc_p_flote-grup_comp_coupa IS INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Grupo de compradores Coupa é obrigatório!' TYPE 'S'.
*    EXIT.
*  ENDIF.



  MOVE-CORRESPONDING wl_zsdtvinc_p_flote TO p_registro_manter.


  CLEAR: p_error .

ENDFORM.


FORM f_exit_zsdtvinc_p_flote_0005 CHANGING p_saida TYPE any.

*  DATA: wl_zsdtvinc_p_flote TYPE zsdtvinc_p_flote.
*
*  CLEAR: wl_zsdtvinc_p_flote.
*
*  MOVE-CORRESPONDING p_saida TO wl_zsdtvinc_p_flote.
*
*  IF wl_zsdtvinc_p_flote-us_criacao IS INITIAL.
*    wl_zsdtvinc_p_flote-dt_criacao      = sy-datum.
*    wl_zsdtvinc_p_flote-hr_criacao      = sy-uzeit.
*    wl_zsdtvinc_p_flote-us_criacao      = sy-uname.
*  ELSE.
*    wl_zsdtvinc_p_flote-dt_modif      = sy-datum.
*    wl_zsdtvinc_p_flote-hr_modif      = sy-uzeit.
*    wl_zsdtvinc_p_flote-us_modif      = sy-uname.
*  ENDIF.
*
*  MOVE-CORRESPONDING wl_zsdtvinc_p_flote TO p_saida.


ENDFORM.

FORM f_exit_zsdtvinc_p_flote_0004 USING p_registro_manter TYPE any.


  DATA: wl_zsdtvinc_p_flote TYPE zsdtvinc_p_flote.

  CLEAR: wl_zsdtvinc_p_flote.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdtvinc_p_flote.

  IF wl_zsdtvinc_p_flote-us_criacao IS INITIAL.
    wl_zsdtvinc_p_flote-dt_criacao      = sy-datum.
    wl_zsdtvinc_p_flote-hr_criacao      = sy-uzeit.
    wl_zsdtvinc_p_flote-us_criacao      = sy-uname.
*  ELSE.
*    wl_zsdtvinc_p_flote-dt_modif      = sy-datum.
*    wl_zsdtvinc_p_flote-hr_modif      = sy-uzeit.
*    wl_zsdtvinc_p_flote-us_modif      = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdtvinc_p_flote TO p_registro_manter.

ENDFORM.

FORM  f_exit_zsdtvinc_p_flote_0009 TABLES pt_excl_toolbar
                           USING p_db_tab.
*
  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  FREE: it_excl_toolbar.


  wa_excl_toolbar-code = 'Modificar'.
  APPEND wa_excl_toolbar  TO it_excl_toolbar.

  pt_excl_toolbar[] = it_excl_toolbar[].
ENDFORM.
