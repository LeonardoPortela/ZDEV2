*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0419_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0419_exit.

FORM f_exit_zsdt0419_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zsdt0419 TYPE zsdt0419.

  CLEAR: w_zsdt0419.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0419.
*  w_zsdt0419-user_create    =  sy-uname.
*  w_zsdt0419-date_create    = sy-datum.
*  w_zsdt0419-time_create    = sy-uzeit.

  w_zsdt0419-kunnr = |{ w_zsdt0419-kunnr ALPHA = IN }|.

  MOVE-CORRESPONDING w_zsdt0419 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0419_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0419 TYPE zsdt0419.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0419.

*  IF w_zsdt0419-matkl IS INITIAL.
*
*    p_erro = abap_true.
*
*    MESSAGE s024(sd) WITH 'Informar Grupo de Mercadoria'
*                     DISPLAY LIKE 'E'.
*
*    EXIT.
*
*  ENDIF.

ENDFORM.

FORM f_exit_zsdt0419_0003 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0419 TYPE zsdt0419.
  DATA lv_matnr TYPE matnr18.

  CLEAR: w_zsdt0419.
  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0419.
*  w_zsdt0419-user_change    =  sy-uname.
*  w_zsdt0419-date_change    = sy-datum.
*  w_zsdt0419-time_change    = sy-uzeit.

*  IF w_zsdt0419-matkl IS NOT INITIAL.
*
*    SELECT SINGLE wgbez FROM t023t
*      INTO w_zsdt0419-wgbez
*        WHERE spras = sy-langu
*          AND matkl = w_zsdt0419-matkl.
*
*  ENDIF.
*
*  IF w_zsdt0419-matkl IS NOT INITIAL.
*
*    SELECT SINGLE msehl FROM t006a
*      INTO w_zsdt0419-msehl
*        WHERE spras = sy-langu
*          AND msehi = w_zsdt0419-mseh3.
*
*  ENDIF.

  MOVE-CORRESPONDING w_zsdt0419 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0419_0004 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0419 TYPE zsdt0419.

  CLEAR w_zsdt0419.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0419.

*  IF w_zsdt0419-matkl IS NOT INITIAL.
*
*    SELECT SINGLE wgbez FROM t023t
*      INTO w_zsdt0419-wgbez
*        WHERE spras = sy-langu
*          AND matkl = w_zsdt0419-matkl.
*
*  ENDIF.
*
*  IF w_zsdt0419-matkl IS NOT INITIAL.
*
*    DATA lv_meins TYPE meins.
*
*    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*      EXPORTING
*        input          = w_zsdt0419-mseh3
*      IMPORTING
*        output         = lv_meins
*      EXCEPTIONS
*        unit_not_found = 1
*        OTHERS         = 2.
*
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    SELECT SINGLE msehl FROM t006a
*      INTO w_zsdt0419-msehl
*        WHERE spras = sy-langu
*          AND msehi = lv_meins.
*
*  ENDIF.

  MOVE-CORRESPONDING w_zsdt0419 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0419_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0419_out TYPE zsdt0419.

  CLEAR: wl_zsdt0419_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0419_out.

  MOVE-CORRESPONDING wl_zsdt0419_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0419_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zsdt0419_0008 CHANGING p_col_pos
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

  IF p_field = 'MSEH3'.

    p_scrtext_l = 'Unidade'.

  ENDIF.

ENDFORM.

FORM f_exit_zsdt0419_0009  TABLES it_excl_toolbar
                            USING p_db_tab.

*  IF p_db_tab = 'ZSDT0419'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.


ENDFORM.
