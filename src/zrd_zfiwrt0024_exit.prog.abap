*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIWRT0024_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfiwrt0024_exit.


FORM f_exit_zfiwrt0024_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfiwrt0024 TYPE zfiwrt0024,
        gv_kunnr      TYPE kna1-kunnr,
        gv_lifnr      TYPE lfa1-lifnr.

  CLEAR: wl_zfiwrt0024.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfiwrt0024.

  CLEAR: p_error, gv_kunnr, gv_lifnr.


  IF wl_zfiwrt0024-cod_lr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Local de Entrega é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ELSE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_zfiwrt0024-cod_lr   " C field
      IMPORTING
        output = gv_kunnr.


    SELECT SINGLE * FROM kna1 INTO @DATA(wl_kna1)
      WHERE kunnr EQ @gv_kunnr.

    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Local de Entrega informado não existe!' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.


  IF wl_zfiwrt0024-cod_pc IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Local de Entrega é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ELSE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_zfiwrt0024-cod_pc   " C field
      IMPORTING
        output = gv_lifnr.

    SELECT SINGLE * FROM lfa1 INTO @DATA(wl_lfa1)
      WHERE lifnr EQ @gv_lifnr.

    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Ponto de Coleta informado não existe!' TYPE 'S'.
      EXIT.
    ENDIF.

  ENDIF.


  IF wl_zfiwrt0024-cod_sb IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Agente de Frete é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ELSE.
    CLEAR: wl_lfa1, gv_lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_zfiwrt0024-cod_sb   " C field
      IMPORTING
        output = gv_lifnr.


    SELECT SINGLE * FROM lfa1 INTO wl_lfa1
      WHERE lifnr EQ gv_lifnr.

    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Agente de Frete informado não existe!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.
