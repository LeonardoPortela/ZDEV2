*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIT0150_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0190_exit.

FORM f_exit_zfit0190_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0190 TYPE zfit0190.

  wl_zfit0190-usuario  = sy-uname.
  wl_zfit0190-data     = sy-datum.
  wl_zfit0190-hora     = sy-uzeit.

  MOVE-CORRESPONDING wl_zfit0190 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0190_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfit0190 TYPE zfit0190,
        vl_message  TYPE c LENGTH 200,
        lv_cpf      TYPE zde_stcd2,
        lv_answer.

  CLEAR: wl_zfit0190.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0190.

  IF wl_zfit0190-bukrs   IS INITIAL.
    MESSAGE 'Informar Empresa' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = abap_true.
    EXIT.
  ENDIF.

*------------------------------
*---- valida empresa
*------------------------------
  SELECT bukrs
    INTO @DATA(l_bukrs)
    FROM t001
      UP TO 1 ROWS
   WHERE bukrs = @wl_zfit0190-bukrs.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE 'Empresa Informada está Incorreta.' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = abap_true.
    EXIT.
  ENDIF.

*------------------------------
*---- valida cpf
*------------------------------
  IF ( wl_zfit0190-cpf       IS     INITIAL   AND
       wl_zfit0190-cnpj_raiz IS     INITIAL ) OR
     ( wl_zfit0190-cpf       IS NOT INITIAL   AND
       wl_zfit0190-cnpj_raiz IS NOT INITIAL ).
    MESSAGE 'Informar CPF ou CNPJ Raiz!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = abap_true.
    EXIT.

  ELSEIF wl_zfit0190-cpf IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_INPUT'
      EXPORTING
        input     = wl_zfit0190-cpf
      IMPORTING
        output    = lv_cpf
      EXCEPTIONS
        not_valid = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE 'CPF Informado está Incorreto.' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zfit0190-akont IS INITIAL.
    MESSAGE 'Informar Conta Conciliação!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = abap_true.
    EXIT.
  ELSE.
    SELECT SINGLE *
      INTO @DATA(w_ska1)
      FROM ska1
     WHERE saknr = @wl_zfit0190-akont.

    IF sy-subrc <> 0.
      MESSAGE 'Conta informada está incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0190_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfit0190 TYPE zfit0190,
        vl_message  TYPE c LENGTH 200.

  CLEAR: wl_zfit0190.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfit0190.

  wl_zfit0190-usuario  = sy-uname.
  wl_zfit0190-data     = sy-datum.
  wl_zfit0190-hora     = sy-uzeit.

  MOVE-CORRESPONDING wl_zfit0190 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0190_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfit0190 TYPE zfit0190.

  CLEAR: wl_zfit0190.
  MOVE-CORRESPONDING p_saida TO wl_zfit0190.

  IF wl_zfit0190-cpf = '00000000000'.
    CLEAR wl_zfit0190-cpf.
    MOVE-CORRESPONDING wl_zfit0190 TO p_saida.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0190_0005 CHANGING p_saida TYPE any.

  LOOP AT SCREEN.

    IF ( sy-ucomm EQ 'CHANGE' AND ( screen-group1 EQ 'GNE' OR screen-group1 EQ 'GNS' ) ) OR
       ( sy-ucomm EQ 'NOVO'   AND ( screen-group1 EQ 'GNS' ) ).
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM f_exit_zfit0190_0014 USING p_saida TYPE any CHANGING p_break.

ENDFORM.

FORM f_exit_zfit0190_0016 USING p_ucomm
                          CHANGING p_registro_manter p_saida.

ENDFORM.

FORM f_edit_text USING wl_thead TYPE thead.
ENDFORM.

FORM f_create_text USING wl_thead.
ENDFORM.

FORM f_exit_zfit0190_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZFIT0190' AND
     p_field       = 'BUKRS'.
    p_scrtext_l    = 'Empresa'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0190' AND
     p_field       = 'CPF'.
    p_scrtext_l    = 'CPF'.
    p_outputlen    = 12.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0190' AND
     p_field       = 'CNPJ_RAIZ'.
    p_scrtext_l    = 'CNPJ Raiz'.
    p_outputlen    = 10.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0190' AND
     p_field       = 'AKONT'.
    p_scrtext_l    = 'Conta Conciliação'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0190' AND
     p_field       = 'USUARIO'.
    p_scrtext_l    = 'Usuário'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0190' AND
     p_field       = 'DATA'.
    p_scrtext_l    = 'Data'.
    p_outputlen    = 15.
  ENDIF.

  IF p_ref_tabname = 'ZFIT0190' AND
     p_field       = 'HORA'.
    p_scrtext_l    = 'Hora'.
    p_outputlen    = 15.
  ENDIF.

ENDFORM.
