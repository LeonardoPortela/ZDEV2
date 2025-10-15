*&---------------------------------------------------------------------*
*& Report  ZRD_zFIWRT0025_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfiwrt0025_exit.

FORM f_exit_zfiwrt0025_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zfiwrt0025 TYPE zfiwrt0025.
  CLEAR:  wa_zfiwrt0025.

  MOVE-CORRESPONDING p_registro_manter TO wa_zfiwrt0025.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zfiwrt0025-bukrs
    IMPORTING
      output = wa_zfiwrt0025-bukrs.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zfiwrt0025-loc_carrega
    IMPORTING
      output = wa_zfiwrt0025-loc_carrega.


  CLEAR: p_error.
  IF p_error IS INITIAL.
    IF wa_zfiwrt0025-operacao IS INITIAL OR wa_zfiwrt0025-operacao = 0.
      p_error = abap_true.
      MESSAGE 'Campo Código Operação Obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_zfiwrt0025-bukrs IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Empresa Obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_zfiwrt0025-loc_carrega IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Local Carregamento Obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF wa_zfiwrt0025-cfop IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo CFOP Obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_exit_zfiwrt0025_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zfiwrt0025 TYPE zfiwrt0025.
  MOVE-CORRESPONDING p_registro_manter TO wa_zfiwrt0025.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zfiwrt0025-bukrs
    IMPORTING
      output = wa_zfiwrt0025-bukrs.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zfiwrt0025-loc_carrega
    IMPORTING
      output = wa_zfiwrt0025-loc_carrega.

  MOVE-CORRESPONDING wa_zfiwrt0025 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfiwrt0025_0004 CHANGING p_saida TYPE any.

  DATA: wa_zfiwrt0025_out TYPE zfiwrt0025_out.

  CLEAR wa_zfiwrt0025_out .

  MOVE-CORRESPONDING p_saida TO wa_zfiwrt0025_out.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zfiwrt0025_out-loc_carrega
    IMPORTING
      output = wa_zfiwrt0025_out-loc_carrega.


  SELECT SINGLE lifnr, name1 FROM lfa1 INTO @DATA(lwa_lfa1) WHERE lifnr = @wa_zfiwrt0025_out-loc_carrega.
  IF sy-subrc EQ 0.
    wa_zfiwrt0025_out-name1  = lwa_lfa1-name1.
  ENDIF.

  MOVE-CORRESPONDING wa_zfiwrt0025_out TO p_saida.

ENDFORM.


FORM f_exit_zfiwrt0025_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_zfiwrt0025 TYPE zfiwrt0025.
  MOVE-CORRESPONDING p_registro_manter TO wa_zfiwrt0025.
  MOVE-CORRESPONDING wa_zfiwrt0025 TO p_registro_manter.

ENDFORM.
