*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0381_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0383_exit.

FORM f_exit_zsdt0383_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0383 TYPE zsdt0383.

  CLEAR: wl_zsdt0383.

  wl_zsdt0383-user_create  = sy-uname.
  wl_zsdt0383-date_create  = sy-datum.
  wl_zsdt0383-time_create  = sy-uzeit.

  MOVE-CORRESPONDING wl_zsdt0383 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0383_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zsdt0383 TYPE zsdt0383.
  CLEAR: wa_zsdt0383.

  DATA: var_answer TYPE c.

  DATA: lit_zsdt0383 TYPE TABLE OF zsdt0383.

  MOVE-CORRESPONDING p_registro_manter TO wa_zsdt0383.

  CLEAR: p_error.

  IF wa_zsdt0383-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Empresa campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zsdt0383-hbkid IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Banco campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zsdt0383-meio_pgto IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Meio de pagto campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zsdt0383-desc_meio_pgto IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Descrição Meio de pagto campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0383_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zsdt0383 TYPE zsdt0383.
  MOVE-CORRESPONDING p_registro_manter TO wa_zsdt0383.

  wa_zsdt0383-user_create  = sy-uname.
  wa_zsdt0383-date_create  = sy-datum.
  wa_zsdt0383-time_create  = sy-uzeit.

  MOVE-CORRESPONDING wa_zsdt0383 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0383_0008  CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0383_OUT' AND
     p_field       = 'BUKRS'.
    p_scrtext_l    = 'Empresa'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0383_OUT' AND
     p_field       = 'HBKID'.
    p_scrtext_l    = 'Banco'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0383_OUT' AND
     p_field       = 'MEIO_PGTO'.
    p_scrtext_l    = 'Meio de pagamento'.
    p_outputlen    = 2.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0383_OUT' AND
     p_field       = 'DESC_MEIO_PGTO'.
    p_scrtext_l    = 'Descrição meio de pagamento'.
    p_outputlen    = 10.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0383_0013  TABLES p_tables.


ENDFORM.

FORM f_exit_zsdt0383_0017 USING p_tipo.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

  IF p_tipo = '0001'.
    PERFORM f4_val_meio USING '<FS_WA_REGISTRO_MANTER>-MEIO_PGTO'.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
FORM f4_val_meio  USING p_cod TYPE help_info-dynprofld.
*                              p_desc TYPE help_info-dynprofld.

*====>  Tabelas internas
  DATA: BEGIN OF t_val OCCURS 0,
          key  TYPE ilm_para_value,
          text TYPE eaml_descr,
        END OF t_val.

  DATA: t_return  TYPE STANDARD TABLE OF ddshretval.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area

  t_val-text = 'Boleto Bancário'.
  t_val-key  = 'D'.
  APPEND t_val.

  t_val-text = 'Depósito em Conta'.
  t_val-key  = 'U'.
  APPEND t_val.

  t_val-text = 'PIX/Bolepix'.
  t_val-key  = 'X'.
  APPEND t_val.

  t_val-text = 'Acerto'.
  t_val-key  = 'Y'.
  APPEND t_val.

  t_val-text = 'Troca'.
  t_val-key  = 'T'.
  APPEND t_val.

  t_val-text = 'PIX'.
  t_val-key  = 'X'.
  APPEND t_val.

  t_val-text = 'Financiamento'.
  t_val-key  = 'F'.
  APPEND t_val.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KEY'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Meio Pagto'
      value_org       = 'S'
    TABLES
      value_tab       = t_val
      return_tab      = t_return
      dynpfld_mapping = t_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENTER' "ENTER
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.



ENDFORM.
