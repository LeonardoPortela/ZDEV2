*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0286_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0286_exit.

FORM f_exit_zsdt0286_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0286 TYPE zsdt0286.

  CLEAR: wl_zsdt0286.
  wl_zsdt0286-usname = sy-uname.
  wl_zsdt0286-data    = sy-datum.
  wl_zsdt0286-hora    = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0286 TO p_registro_manter.

ENDFORM.


FORM f_exit_zsdt0286_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zsdt0286 TYPE zsdt0286.

  CLEAR: wl_zsdt0286.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0286.

  SELECT SINGLE bname FROM usr02 INTO @DATA(user_sap) WHERE bname =  @wl_zsdt0286-usname_acesso.

    IF sy-subrc <> 0.
      p_error = abap_true.
      MESSAGE 'Usuário não está cadastrado no SAP' TYPE 'E'.
      EXIT.
    ENDIF.


    CLEAR: p_error.

ENDFORM.


FORM f_exit_zsdt0286_0003 CHANGING p_saida TYPE any.

  DATA: wl_zsdt0286_out TYPE zsdt0286_out.

  CLEAR: wl_zsdt0286_out.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0286_out.

  MOVE-CORRESPONDING wl_zsdt0286_out TO p_saida.


ENDFORM.


FORM f_exit_zsdt0286_0004 CHANGING p_saida TYPE any.

ENDFORM.


FORM f_exit_zsdt0286_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.


FORM  f_exit_zsdt0286_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

IF p_db_tab = 'ZSDT0286'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
