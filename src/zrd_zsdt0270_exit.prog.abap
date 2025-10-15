*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0270_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0270_exit.

FORM f_exit_zsdt0270_0001 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0270_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0270 TYPE zsdt0270_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0270.

  IF w_zsdt0270-cod_regional IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Código da Regional'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zsdt0270-regional IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar o Nome da Regional'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0270_0004 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0270_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

  DATA: w_zsdt0270 TYPE zsdt0270_out,
        l_filial   TYPE string.

  FREE: l_filial.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0270.

  SELECT filial
    INTO TABLE @DATA(t_filial)
    FROM zsdt0271
   WHERE cod_regional = @w_zsdt0270-cod_regional.

  IF t_filial[] IS NOT INITIAL.
    LOOP AT t_filial INTO DATA(w_filial).
      IF sy-tabix = 1.
        l_filial = w_filial.
      ELSE.
        CONCATENATE l_filial ',' w_filial
               INTO l_filial.
      ENDIF.
    ENDLOOP.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Regional Contém filial: ' l_filial ' vinculada(s).'
                     DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0270_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0270_OUT' AND
     p_field       = 'REGIONAL'.
    p_outputlen    = 165.
    p_scrtext_l    = 'Descrição Regional'.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0270_0009  TABLES it_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
