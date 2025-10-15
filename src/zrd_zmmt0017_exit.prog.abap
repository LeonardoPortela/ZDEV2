*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0017_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0017_exit.

FORM f_exit_zmmt0017_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zmmt0017 TYPE zmmt0017_out.

  CLEAR: w_zmmt0017.

  MOVE-CORRESPONDING p_registro_manter  TO w_zmmt0017.
*  w_ZMMT0017-user_create    =  sy-uname.
*  w_ZMMT0017-date_create    = sy-datum.
*  w_ZMMT0017-time_create    = sy-uzeit.


  MOVE-CORRESPONDING w_zmmt0017 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0017_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zmmt0017 TYPE zmmt0017_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zmmt0017.

  IF w_zmmt0017-matnr IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Material' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zmmt0017-centro_a_fixar IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Centro Afixar' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zmmt0017-centro_fixo IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Centro Fixo' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zmmt0017-lgort IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Deposito' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.




ENDFORM.

FORM f_exit_zmmt0017_0003 CHANGING p_registro_manter TYPE any.

  DATA: w_zmmt0017 TYPE zmmt0017_out.

  CLEAR: w_zmmt0017.
  MOVE-CORRESPONDING p_registro_manter  TO w_zmmt0017.
*  w_ZMMT0017-user_change    =  sy-uname.
*  w_ZMMT0017-date_change    = sy-datum.
*  w_ZMMT0017-time_change    = sy-uzeit.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = w_zmmt0017-matnr
    IMPORTING
      output = w_zmmt0017-matnr.


  MOVE-CORRESPONDING w_zmmt0017 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0017_0004 CHANGING p_registro_manter TYPE any.
  DATA: w_zmmt0017 TYPE zmmt0017_out.
  CLEAR w_zmmt0017.

  MOVE-CORRESPONDING p_registro_manter  TO w_zmmt0017.
  CASE w_zmmt0017-tp_produto .
    WHEN 'CO'.
      w_zmmt0017-desc_tp_prod = 'Convencional'.
    WHEN 'RR'.
      w_zmmt0017-desc_tp_prod = 'Transgênico'.
    WHEN OTHERS.
      w_zmmt0017-desc_tp_prod = 'Não Informado'.
  ENDCASE.

  CASE w_zmmt0017-eudr .
    WHEN 'S'.
      w_zmmt0017-desc_eudr = 'EUDR'.
    WHEN 'N'.
      w_zmmt0017-desc_eudr = 'Não EUDR'.
    WHEN OTHERS.
      w_zmmt0017-desc_eudr = 'Não Se Aplica'.
  ENDCASE.

  MOVE-CORRESPONDING w_zmmt0017 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0017_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.

FORM f_exit_zmmt0017_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zmmt0017_0008 CHANGING p_col_pos
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


  IF p_ref_tabname = 'ZMMT0017_OUT' AND
   p_field       = 'CENTRO_A_FIXAR'.
    p_scrtext_l = 'Centro A Fixar'.
    p_outputlen = 15.
  ENDIF.
  IF p_ref_tabname = 'ZMMT0017_OUT' AND
   p_field       = 'CENTRO_FIXO'.
    p_scrtext_l = 'Centro Fixo'.
    p_outputlen = 12.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0017_OUT' AND
  p_field       = 'EUDR'.
    p_scrtext_l = 'Atende EUDR'.
    p_outputlen = 10.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0017_OUT' AND
  p_field       = 'DESC_EUDR'.
    p_scrtext_l = 'Desc. EUDR'.
    p_outputlen = 10.
  ENDIF.
  IF p_ref_tabname = 'ZMMT0017_OUT' AND
p_field       = 'DESC_TP_PROD'.
    p_scrtext_l = 'Desc. Tp. Material'.
    p_outputlen = 20.
  ENDIF.


ENDFORM.

FORM f_exit_zmmt0017_0009  TABLES it_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
