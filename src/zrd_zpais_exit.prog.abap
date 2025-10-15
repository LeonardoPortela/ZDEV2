*&---------------------------------------------------------------------*
*& Report  ZRD_ZPAIS_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zpais_exit.

FORM f_exit_zpais_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zpais TYPE zpais.

  CLEAR: wl_zpais.

  MOVE-CORRESPONDING wl_zpais TO p_registro_manter.

ENDFORM.

FORM f_exit_zpais_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zpais TYPE zpais_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zpais.

ENDFORM.

FORM f_exit_zpais_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zpais    TYPE zpais.

  CLEAR: wl_zpais.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zpais.

  MOVE-CORRESPONDING wl_zpais TO p_registro_manter.

ENDFORM.

FORM f_exit_zpais_0004 CHANGING p_registro_manter TYPE any.

  DATA: w_zpais TYPE zpais_out.
  DATA: lt_dd07v TYPE TABLE OF dd07v.

  MOVE-CORRESPONDING p_registro_manter  TO w_zpais.

  SELECT SINGLE landx
    FROM t005t
    INTO w_zpais-landx
    WHERE spras EQ sy-langu
     AND land1 EQ w_zpais-land1.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name      = 'ZDO_CONTINENTE'
      langu     = sy-langu
    TABLES
      dd07v_tab = lt_dd07v.

  READ TABLE lt_dd07v INTO DATA(ls_dd07v) WITH KEY domvalue_l = w_zpais-continente.
  IF sy-subrc IS INITIAL.
    w_zpais-continente_x = ls_dd07v-ddtext.
  ENDIF.


  MOVE-CORRESPONDING w_zpais TO p_registro_manter.

ENDFORM.

FORM f_exit_zpais_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zpais_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.
ENDFORM.

FORM f_exit_zpais_0008 CHANGING p_col_pos
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

*  IF p_ref_tabname = 'ZPAIS_OUT' AND
*     p_field       = 'USNAM'.
*    p_scrtext_l = 'Usuário'.
*    p_outputlen = 15.
*    p_f4           = abap_true.
*  ENDIF.
*
*  IF p_ref_tabname = 'ZPAIS_OUT' AND
*     p_field       = 'DATUM'.
*    p_scrtext_l = 'Data Registro'.
*    p_outputlen = 15.
*    p_f4           = abap_true.
*  ENDIF.
*
*  IF p_ref_tabname = 'ZPAIS_OUT' AND
*     p_field       = 'UZEIT'.
*    p_scrtext_l = 'Hora Registro'.
*    p_outputlen = 15.
*    p_f4           = abap_true.
*  ENDIF.
*
*  IF p_ref_tabname = 'ZPAIS_OUT' AND
*     p_field       = 'INCIDE'.
*    p_scrtext_l = 'Incidir?'.
*    p_check     = abap_true.
*    p_outputlen = 10.
*    p_f4           = abap_true.
*  ENDIF.

ENDFORM.

FORM f_exit_zpais_0009  TABLES pt_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
