*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0217_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zlest0217_exit.

FORM f_exit_zlest0217_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0217 TYPE zlest0217.

  CLEAR: wl_zlest0217.

  wl_zlest0217-dt_registro = sy-datum.
  wl_zlest0217-hr_registro = sy-uzeit.
  wl_zlest0217-us_registro = sy-uname.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0217-kostl
    IMPORTING
      output = wl_zlest0217-kostl.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0217-prctr
    IMPORTING
      output = wl_zlest0217-prctr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = wl_zlest0217-matnr_serv
    IMPORTING
      output       = wl_zlest0217-matnr_serv
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = wl_zlest0217-matnr
    IMPORTING
      output       = wl_zlest0217-matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  MOVE-CORRESPONDING wl_zlest0217 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0217_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zlest0217 TYPE zlest0217_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0217.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = w_zlest0217-kostl
    IMPORTING
      output = w_zlest0217-kostl.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = w_zlest0217-prctr
    IMPORTING
      output = w_zlest0217-prctr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = w_zlest0217-matnr_serv
    IMPORTING
      output       = w_zlest0217-matnr_serv
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = w_zlest0217-matnr
    IMPORTING
      output       = w_zlest0217-matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  IF w_zlest0217-kostl IS INITIAL AND w_zlest0217-prctr IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Centro de Custo ou Centro de Lucro.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  IF w_zlest0217-kostl IS NOT INITIAL AND w_zlest0217-prctr IS NOT INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Centro de Custo ou Centro de Lucro.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zlest0217-kostl IS NOT INITIAL AND w_zlest0217-matnr_serv IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Material de Serviço.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* IF w_zlest0217-kostl IS NOT INITIAL AND w_zlest0217-matnr IS INITIAL.
  IF w_zlest0217-matnr IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Material.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zlest0217-kostl IS INITIAL AND ( w_zlest0217-matnr_serv IS NOT INITIAL ).
*                                  OR   w_zlest0217-matnr      IS NOT INITIAL ).
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Não informar Material de serviço.'
*                         'e nem Material.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zlest0217-kostl IS NOT INITIAL.
    SELECT SINGLE *
             FROM csks
             INTO @DATA(w_csks)
            WHERE kostl  = @w_zlest0217-kostl
              AND datbi >= @sy-datum.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Centro de Custo informado está incorreto.'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF w_zlest0217-prctr IS NOT INITIAL.
    SELECT SINGLE *
             FROM cepc
             INTO @DATA(w_cepc)
            WHERE prctr  = @w_zlest0217-prctr
              AND datbi >= @sy-datum.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Centro de Lucro informado está incorreto.'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF w_zlest0217-matnr_serv IS NOT INITIAL.
    SELECT SINGLE *
             FROM mara
             INTO @DATA(w_mara)
            WHERE matnr     = @w_zlest0217-matnr_serv.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Material de Serviço informado está incorreto.'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF w_zlest0217-matnr IS NOT INITIAL.
    SELECT SINGLE *
             FROM mara
             INTO @DATA(w_mara2)
            WHERE matnr     = @w_zlest0217-matnr.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Material informado está incorreto.'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0217_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0217    TYPE zlest0217.

  CLEAR: wl_zlest0217.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0217.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0217-kostl
    IMPORTING
      output = wl_zlest0217-kostl.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0217-prctr
    IMPORTING
      output = wl_zlest0217-prctr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = wl_zlest0217-matnr_serv
    IMPORTING
      output       = wl_zlest0217-matnr_serv
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = wl_zlest0217-matnr
    IMPORTING
      output       = wl_zlest0217-matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  wl_zlest0217-dt_registro = sy-datum.
  wl_zlest0217-hr_registro = sy-uzeit.
  wl_zlest0217-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zlest0217 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0217_0004 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zlest0217_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zlest0217_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.
ENDFORM.

FORM f_exit_zlest0217_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZLEST0217_OUT' AND
     p_field       = 'KOSTL'.
    p_scrtext_l = 'Centro de Custo'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0217_OUT' AND
     p_field       = 'PRCTR'.
    p_scrtext_l = 'Centro de Lucro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0217_OUT' AND
     p_field       = 'MATNR_SERV'.
    p_scrtext_l = 'Material de Serviço'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0217_OUT' AND
     p_field       = 'MATNR'.
    p_scrtext_l = 'Material'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0217_OUT' AND
     p_field       = 'US_REGISTRO'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0217_OUT' AND
     p_field       = 'DT_REGISTRO'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0217_OUT' AND
     p_field       = 'HR_REGISTRO'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0217_0009  TABLES pt_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
