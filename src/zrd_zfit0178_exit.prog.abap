*&---------------------------------------------------------------------*
*& Report  ZRD_zfit0178_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfit0178_exit.

FORM f_exit_zfit0178_0001 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zfit0178_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zfit0178 TYPE zfit0178_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zfit0178.

  IF w_zfit0178-pais IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Código do País.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zfit0178-data_feriado IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Data Feriado.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  IF w_zfit0178-desc_feriado IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Descrição do Feriado.'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0178_0004 CHANGING p_registro_manter TYPE any.

  DATA: w_zfit0178 TYPE zfit0178_out.
  CLEAR w_zfit0178.

  MOVE-CORRESPONDING p_registro_manter  TO w_zfit0178.

  SELECT SINGLE *
         FROM t005t
         INTO @DATA(w_t005)
        WHERE land1 = @w_zfit0178-pais
    AND SPRAS EQ 'P'.
  IF sy-subrc = 0.
    w_zfit0178-desc_pais = w_t005-landx.
  ENDIF.
  MOVE-CORRESPONDING w_zfit0178 TO p_registro_manter.
ENDFORM.

FORM f_exit_zfit0178_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.

FORM f_exit_zfit0178_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.
ENDFORM.

FORM f_exit_zfit0178_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZFIT0178_OUT' AND
   p_field       = 'DESC_PAIS'.
    p_scrtext_l = 'Descrição País'.
    p_outputlen = 60.
  ENDIF.

ENDFORM.

FORM f_exit_zfit0178_0009  TABLES it_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
