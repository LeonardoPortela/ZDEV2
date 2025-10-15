*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT_DEPARA_DEPO_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt_depara_depo_exit.

FORM f_exit_zsdt_depara_depo_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zsdt_depara_depo TYPE zsdt_depara_depo_out.

  CLEAR: w_zsdt_depara_depo.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt_depara_depo.
*  w_ZSDT_DEPARA_DEPO-user_create    =  sy-uname.
*  w_ZSDT_DEPARA_DEPO-date_create    = sy-datum.
*  w_ZSDT_DEPARA_DEPO-time_create    = sy-uzeit.


  MOVE-CORRESPONDING w_zsdt_depara_depo TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt_depara_depo_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt_depara_depo TYPE zsdt_depara_depo.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt_depara_depo.

  IF w_zsdt_depara_depo-werks IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Centro' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE bukrs, branch
     FROM j_1bbranch INTO @DATA(lwa_branch)
    WHERE branch EQ @w_zsdt_depara_depo-werks.

  IF sy-subrc NE 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar um centro fixo' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zsdt_depara_depo-lifnr IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Fornecedor' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zsdt_depara_depo-operacao IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Operação' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zsdt_depara_depo-werks_v IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Centro Virtual' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zsdt_depara_depo-lgort IS INITIAL AND w_zsdt_depara_depo-lgort_t IS INITIAL AND w_zsdt_depara_depo-lgort_f IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar pelo menos um deposito' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


ENDFORM.

FORM f_exit_zsdt_depara_depo_0003 CHANGING p_registro_manter TYPE any.

  DATA: w_zsdt_depara_depo TYPE zsdt_depara_depo_out.

  CLEAR: w_zsdt_depara_depo.
  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt_depara_depo.
*
*  w_ZSDT_DEPARA_DEPO-user_change    =  sy-uname.
*  w_ZSDT_DEPARA_DEPO-date_change    = sy-datum.
*  w_ZSDT_DEPARA_DEPO-time_change    = sy-uzeit.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = w_zsdt_depara_depo-lifnr
    IMPORTING
      output = w_zsdt_depara_depo-lifnr.


  MOVE-CORRESPONDING w_zsdt_depara_depo TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt_depara_depo_0004 CHANGING p_registro_manter TYPE any.
  DATA: w_zsdt_depara_depo TYPE zsdt_depara_depo_out.
  CLEAR w_zsdt_depara_depo.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt_depara_depo.
  CASE w_zsdt_depara_depo-eudr .
    WHEN 'S'.
      w_zsdt_depara_depo-desc_eudr = 'EUDR'.
    WHEN 'N'.
      w_zsdt_depara_depo-desc_eudr = 'Não EUDR'.
    WHEN OTHERS.
      w_zsdt_depara_depo-desc_eudr = 'Não se aplica'.
  ENDCASE.

  MOVE-CORRESPONDING w_zsdt_depara_depo TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt_depara_depo_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.

FORM f_exit_zsdt_depara_depo_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zsdt_depara_depo_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT_DEPARA_DEPO_OUT' AND
    p_field       = 'WERKS'.
    p_scrtext_l = 'Centro Fixo'.
    p_outputlen = 12.
  ENDIF.

  IF p_ref_tabname = 'ZSDT_DEPARA_DEPO_OUT' AND
    p_field       = 'LIFNR'.
    p_scrtext_l = 'Terminal - Z1'.
    p_outputlen = 20.
  ENDIF.
  IF p_ref_tabname = 'ZSDT_DEPARA_DEPO_OUT' AND
    p_field       = 'OPERACAO'.
    p_scrtext_l = 'Operação'.
    p_outputlen = 20.
  ENDIF.
  IF p_ref_tabname = 'ZSDT_DEPARA_DEPO_OUT' AND
     p_field       = 'EUDR'.
    p_scrtext_l = 'EUDR'.
    p_outputlen = 5.
  ENDIF.
  IF p_ref_tabname = 'ZSDT_DEPARA_DEPO_OUT' AND
    p_field       = 'WERKS_V'.
    p_scrtext_l = 'C. Virtural'.
    p_outputlen = 12.
  ENDIF.
  IF p_ref_tabname = 'ZSDT_DEPARA_DEPO_OUT' AND
    p_field       = 'LGORT_T'.
    p_scrtext_l = 'Dep. Conv'.
    p_outputlen = 10.
  ENDIF.
*  IF p_ref_tabname = 'ZSDT_DEPARA_DEPO_OUT' AND
*    p_field       = 'LGORT_F'.
*    p_scrtext_l = 'Dep. Fábrica'.
*    p_outputlen = 5.
*  ENDIF.
  IF p_ref_tabname = 'ZSDT_DEPARA_DEPO_OUT' AND
    p_field       = 'DESC_EUDR'.
    p_scrtext_l = 'Desc. Receb. EUDR'.
    p_outputlen = 20.
  ENDIF.


ENDFORM.

FORM f_exit_zsdt_depara_depo_0009  TABLES it_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
