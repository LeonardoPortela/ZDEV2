*&---------------------------------------------------------------------*
*& Report  ZRD_zfit0179_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0179_exit.

FORM f_exit_zfit0179_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0179 TYPE zfit0179.

  CLEAR: wl_zfit0179.
  wl_zfit0179-us_registro = sy-uname.
  wl_zfit0179-dt_registro    = sy-datum.
  wl_zfit0179-hr_registro    = sy-uzeit.

  MOVE-CORRESPONDING wl_zfit0179 TO p_registro_manter.

ENDFORM.


FORM f_exit_zfit0179_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfit0179 TYPE zfit0179.

  CLEAR: wl_zfit0179.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0179.

  SELECT SINGLE bname FROM usr02 INTO @DATA(user_sap) WHERE bname =  @wl_zfit0179-usname_acesso.

  IF sy-subrc <> 0.
    p_error = abap_true.
    MESSAGE 'Usuário não está cadastrado no SAP' TYPE 'E'.
    EXIT.
  ENDIF.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zfit0179_0003 CHANGING p_saida TYPE any.

  DATA: wl_zfit0179_out TYPE zfit0179.

  CLEAR: wl_zfit0179_out.

  MOVE-CORRESPONDING p_saida TO wl_zfit0179_out.

  MOVE-CORRESPONDING wl_zfit0179_out TO p_saida.


ENDFORM.


FORM f_exit_zfit0179_0004 CHANGING p_saida TYPE any.

ENDFORM.


FORM f_exit_zfit0179_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.


FORM  f_exit_zfit0179_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  IF p_db_tab = 'ZFIT0179'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
