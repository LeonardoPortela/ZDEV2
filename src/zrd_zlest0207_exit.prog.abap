*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0207_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zlest0207_exit.

*-CS2022000236 - 25.02.2022 - JT - inicio
FORM f_exit_zlest0207_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0207 TYPE zlest0207.

  CLEAR: wl_zlest0207.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0207-tdlnr
    IMPORTING
      output = wl_zlest0207-tdlnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0207-tdlnr_frota
    IMPORTING
      output = wl_zlest0207-tdlnr_frota.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0207-tdlnr_sub
    IMPORTING
      output = wl_zlest0207-tdlnr_sub.

  wl_zlest0207-usnam               = sy-uname.
  wl_zlest0207-zdt_atual           = sy-datum.
  wl_zlest0207-zhr_atual           = sy-uzeit.

  MOVE-CORRESPONDING wl_zlest0207 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0207_0002    USING p_registro_manter TYPE any
                           CHANGING p_error.

  DATA: wa_zlest0207 TYPE zlest0207.

  MOVE-CORRESPONDING p_registro_manter TO wa_zlest0207.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zlest0207-tdlnr
    IMPORTING
      output = wa_zlest0207-tdlnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zlest0207-tdlnr_frota
    IMPORTING
      output = wa_zlest0207-tdlnr_frota.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zlest0207-tdlnr_sub
    IMPORTING
      output = wa_zlest0207-tdlnr_sub.

*--------------------------------------
* Forn.Servico
*--------------------------------------
  IF wa_zlest0207-tdlnr IS NOT INITIAL.
    SELECT dlgrp
      INTO @DATA(l_dlgrp)
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr = @wa_zlest0207-tdlnr.
    ENDSELECT.

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH 'Fornecedor não Cadastrado!'
                            DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ELSEIF l_dlgrp <> '0001'.
      MESSAGE s024(sd) WITH 'Fornecedor '  wa_zlest0207-tdlnr
                            ' não configurado em GrupEsqFornServ = "0001"'
                            DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

*--------------------------------------
* Forn.Servico Frota
*--------------------------------------
  IF wa_zlest0207-tdlnr_frota IS NOT INITIAL.
    SELECT dlgrp
      INTO l_dlgrp
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr = wa_zlest0207-tdlnr_frota.
    ENDSELECT.

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH 'Fornecedor Serv.Frota não Cadastrado!'
                            DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ELSEIF l_dlgrp <> '0001'.
      MESSAGE s024(sd) WITH 'Fornecedor Serv.Frota '  wa_zlest0207-tdlnr
                            ' não configurado em GrupEsqFornServ = "0001"'
                            DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

*--------------------------------------
* Forn.Servico Subcontratacao
*--------------------------------------
  IF wa_zlest0207-tdlnr_sub IS NOT INITIAL.
    SELECT dlgrp
      INTO l_dlgrp
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr = wa_zlest0207-tdlnr_sub.
    ENDSELECT.

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH 'Fornecedor Serv.Subcontratação não Cadastrado!'
                            DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ELSEIF l_dlgrp <> '0001'.
      MESSAGE s024(sd) WITH 'Fornecedor Serv.Subcontratação '  wa_zlest0207-tdlnr
                            ' não configurado em GrupEsqFornServ = "0001"'
                            DISPLAY LIKE 'E'.
      p_error = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  IF wa_zlest0207-tdlnr       IS INITIAL AND
     wa_zlest0207-tdlnr_frota IS INITIAL AND
     wa_zlest0207-tdlnr_sub   IS INITIAL.
    MESSAGE s024(sd) WITH 'Informar algum Fornecedor!'  wa_zlest0207-tdlnr
                          DISPLAY LIKE 'E'.
    p_error = abap_true.
    EXIT.
  ENDIF.

ENDFORM.
*-CS2022000236 - 25.02.2022 - JT - fim

FORM f_exit_zlest0207_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zlest0207 TYPE zlest0207.

  MOVE-CORRESPONDING p_registro_manter TO wa_zlest0207.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zlest0207-tdlnr
    IMPORTING
      output = wa_zlest0207-tdlnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zlest0207-tdlnr_frota
    IMPORTING
      output = wa_zlest0207-tdlnr_frota.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zlest0207-tdlnr_sub
    IMPORTING
      output = wa_zlest0207-tdlnr_sub.

  wa_zlest0207-usnam               = sy-uname.
  wa_zlest0207-zdt_atual           = sy-datum.
  wa_zlest0207-zhr_atual           = sy-uzeit.
  MOVE-CORRESPONDING wa_zlest0207 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0207_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZLEST0207' AND
     p_field       = 'TDLNR'.
    p_scrtext_l    = 'Fornecedor Serviços'.
    p_outputlen    = 25.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0207' AND
     p_field       = 'TDLNR_FROTA'.
    p_scrtext_l    = 'Fornecedor Serv.Frota'.
    p_outputlen    = 25.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0207' AND
     p_field       = 'TDLNR_SUB'.
    p_scrtext_l    = 'Fornecedor Serv.Subcontratação'.
    p_outputlen    = 30.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.
