*&---------------------------------------------------------------------*
*& Report  ZRD_zsdt0378_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0378_exit.

FORM f_exit_zsdt0378_0001 CHANGING p_registro_manter TYPE any.


  DATA: w_zsdt0378 TYPE zsdt0378.

  CLEAR: w_zsdt0378.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0378.

  w_zsdt0378-user_create    =  sy-uname.
  w_zsdt0378-date_create    = sy-datum.
  w_zsdt0378-time_create    = sy-uzeit.


  MOVE-CORRESPONDING w_zsdt0378 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0378_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zsdt0378 TYPE zsdt0378.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0378.

  IF w_zsdt0378-pergunta IS INITIAL.
    p_erro = abap_true.

    MESSAGE s024(sd) WITH 'Informar Pergunta'
                 DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.

FORM f_exit_zsdt0378_0003 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0378 TYPE zsdt0378.

  CLEAR: w_zsdt0378.
  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0378.

  IF w_zsdt0378-checkid IS INITIAL.


    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSDCHECKID'
      IMPORTING
        number                  = w_zsdt0378-checkid
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE e016(ds) WITH 'Cadastrar intervalo(SNRO)' 'ZSDCHECKID'.
    ENDIF.


  ENDIF.

  w_zsdt0378-user_change    =  sy-uname.
  w_zsdt0378-date_change    = sy-datum.
  w_zsdt0378-time_change    = sy-uzeit.

  MOVE-CORRESPONDING w_zsdt0378 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0378_0004 CHANGING p_registro_manter TYPE any.

  DATA w_zsdt0378 TYPE zsdt0378.

  CLEAR w_zsdt0378.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0378.

  IF w_zsdt0378-deleted = abap_true.
    CLEAR w_zsdt0378.
  ENDIF.

  MOVE-CORRESPONDING w_zsdt0378 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0378_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0378_out TYPE zsdt0378.

  CLEAR: wl_zsdt0378_out.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0378_out.

  MOVE-CORRESPONDING wl_zsdt0378_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0378_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.
" delete
FORM f_exit_zsdt0378_0014 USING p_saida TYPE zsdt0378 CHANGING cv_break TYPE c.


  p_saida-deleted = abap_true.
  cv_break = abap_true.

  p_saida-user_change    = sy-uname.
  p_saida-date_change    = sy-datum.
  p_saida-time_change    = sy-uzeit.

  MODIFY zsdt0378 FROM p_saida.

  COMMIT WORK AND WAIT.


ENDFORM.
FORM f_exit_zsdt0378_0008 CHANGING p_col_pos
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

  IF p_field = 'SIM_NAO_INCONFOR'.

    p_outputlen = 21.
    p_scrtext_l = 'SIM = Inconformidade?'.
    p_check = abap_true.

  ENDIF.


  IF p_field = 'INATIVAR'.

    p_scrtext_l = 'Inativar Pergunta'.
    p_check = abap_true.

  ENDIF.

  IF p_field = 'DELETED'.

    p_scrtext_l = 'Excluido'.
    p_check = abap_true.

  ENDIF.


ENDFORM.

FORM f_exit_zsdt0378_0009  TABLES it_excl_toolbar
                            USING p_db_tab.

  IF p_db_tab = 'ZSDT0378'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.


ENDFORM.
