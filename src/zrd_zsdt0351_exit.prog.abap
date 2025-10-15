*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0351_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0351_exit.

FORM f_exit_zsdt0351_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zsdt0351 TYPE zsdt0351_out.

  CLEAR: w_zsdt0351.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0351.
  w_zsdt0351-user_create    =  sy-uname.
  w_zsdt0351-date_create    = sy-datum.
  w_zsdt0351-time_create    = sy-uzeit.


  MOVE-CORRESPONDING w_zsdt0351 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0351_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0351 TYPE zsdt0351_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0351.

  IF w_zsdt0351-lifnr IS INITIAL AND w_zsdt0351-kunnr IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Terminal Porto ou Local Entrega'
                     DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF w_zsdt0351-lifnr IS NOT INITIAL AND w_zsdt0351-kunnr IS NOT INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar somente Terminal Porto ou Local Entrega'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zsdt0351-recebimento_eudr IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar o Recebimento EUDR'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0351_0003 CHANGING p_registro_manter TYPE any.

  DATA: w_zsdt0351 TYPE zsdt0351_out.

  CLEAR: w_zsdt0351.
  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0351.
  w_zsdt0351-user_change    =  sy-uname.
  w_zsdt0351-date_change    = sy-datum.
  w_zsdt0351-time_change    = sy-uzeit.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = w_zsdt0351-lifnr
    IMPORTING
      output = w_zsdt0351-lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = w_zsdt0351-kunnr
    IMPORTING
      output = w_zsdt0351-kunnr.


  MOVE-CORRESPONDING w_zsdt0351 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0351_0004 CHANGING p_registro_manter TYPE any.
  DATA: w_zsdt0351 TYPE zsdt0351_out.
  CLEAR w_zsdt0351.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0351.
  CASE w_zsdt0351-recebimento_eudr .
    WHEN 1.
      w_zsdt0351-desc_rec_eudr = 'Recebimento Híbrido'.
    WHEN 2.
      w_zsdt0351-desc_rec_eudr = 'Recebimento 100% EUDR'.
    WHEN OTHERS.
      w_zsdt0351-desc_rec_eudr = 'Recebimento não EUDR'.
  ENDCASE.

  MOVE-CORRESPONDING w_zsdt0351 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0351_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.

FORM f_exit_zsdt0351_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zsdt0351_0008 CHANGING p_col_pos
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


  IF p_ref_tabname = 'ZSDT0351_OUT' AND
   p_field       = 'LIFNR'.
    p_scrtext_l = 'Z1 - Terminal Porto'.
    p_outputlen = 20.
  ENDIF.
  IF p_ref_tabname = 'ZSDT0351_OUT' AND
   p_field       = 'KUNNR'.
    p_scrtext_l = 'LR - Local Entrega'.
    p_outputlen = 20.
  ENDIF.
  IF p_ref_tabname = 'ZSDT0351_OUT' AND
  p_field       = 'DESC_REC_EUDR'.
    p_scrtext_l = 'Desc. Receb. EUDR'.
    p_outputlen = 20.
  ENDIF.


ENDFORM.

FORM f_exit_zsdt0351_0009  TABLES it_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
