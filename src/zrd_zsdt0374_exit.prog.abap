*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0374_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0374_exit.

FORM f_exit_zsdt0374_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zsdt0374 TYPE zsdt0374.

  CLEAR: w_zsdt0374.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0374.
  w_zsdt0374-user_create    =  sy-uname.
  w_zsdt0374-date_create    = sy-datum.
  w_zsdt0374-time_create    = sy-uzeit.


  MOVE-CORRESPONDING w_zsdt0374 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0374_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0374 TYPE zsdt0374.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0374.

  IF w_zsdt0374-email IS INITIAL.

    p_erro = abap_true.

    MESSAGE s024(sd) WITH 'Informar e-mail'
                     DISPLAY LIKE 'E'.

    EXIT.

  ENDIF.

*  IF w_zsdt0374-guid IS INITIAL.
*
*    CALL FUNCTION 'GUID_CREATE'
*      IMPORTING
*        ev_guid_32 = w_zsdt0374-guid.
*
*  ENDIF.

ENDFORM.

FORM f_exit_zsdt0374_0003 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0374 TYPE zsdt0374.
  DATA lv_matnr TYPE matnr18.

  CLEAR: w_zsdt0374.
  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0374.
  w_zsdt0374-user_change    =  sy-uname.
  w_zsdt0374-date_change    = sy-datum.
  w_zsdt0374-time_change    = sy-uzeit.

  MOVE-CORRESPONDING w_zsdt0374 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0374_0004 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0374 TYPE zsdt0374.

  CLEAR w_zsdt0374.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0374.

*  IF w_zsdt0374-guid IS INITIAL.
*
*    CALL FUNCTION 'GUID_CREATE'
*      IMPORTING
*        ev_guid_32 = w_zsdt0374-guid.
*
*  ENDIF.

  MOVE-CORRESPONDING w_zsdt0374 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0374_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0374_out TYPE zsdt0374.

  CLEAR: wl_zsdt0374_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0374_out.

*  IF wl_zsdt0374_out-guid IS INITIAL.
*
*    CALL FUNCTION 'GUID_CREATE'
*      IMPORTING
*        ev_guid_32 = wl_zsdt0374_out-guid.
*
*  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0374_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0374_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zsdt0374_0008 CHANGING p_col_pos
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

  IF p_field = 'GUID'.

    p_scrtext_l = 'Chave'.

  ENDIF.

ENDFORM.

FORM f_exit_zsdt0374_0009  TABLES it_excl_toolbar
                            USING p_db_tab.

*  IF p_db_tab = 'ZSDT0374'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.


ENDFORM.
