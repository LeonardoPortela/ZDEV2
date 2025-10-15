*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0226_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zlest0226_exit.

FORM f_exit_zlest0226_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zlest0226 TYPE zlest0226.

  CLEAR: wl_zlest0226.

  MOVE-CORRESPONDING p_registro_manter TO wl_zlest0226.

  IF sy-ucomm = 'NOVO'.
    wl_zlest0226-dt_criacao = sy-datum.
    wl_zlest0226-hr_criacao = sy-uzeit.
    wl_zlest0226-usuario = sy-uname.
  ELSEIF sy-ucomm = 'CHANGE'.
    wl_zlest0226-dt_modificacao = sy-datum.
    wl_zlest0226-hr_modificacao = sy-uzeit.
    wl_zlest0226-usuario_mod = sy-uname.
  ENDIF.



  MOVE-CORRESPONDING wl_zlest0226 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0226_0009 TABLES pt_excl_toolbar
                            USING p_db_tab.
  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: _param TYPE ustyp_t_parameters.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  FREE: it_excl_toolbar.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = _param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  READ TABLE _param WITH KEY parid =  'ZLES0209_B'
  TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    wa_excl_toolbar-code = 'Modificar'.
    APPEND wa_excl_toolbar  TO it_excl_toolbar.

    wa_excl_toolbar-code = 'Novo'.
    APPEND wa_excl_toolbar  TO it_excl_toolbar.

    wa_excl_toolbar-code = 'Deletar'.
    APPEND wa_excl_toolbar  TO it_excl_toolbar.

    pt_excl_toolbar[] = it_excl_toolbar[].
  ENDIF.

ENDFORM.
