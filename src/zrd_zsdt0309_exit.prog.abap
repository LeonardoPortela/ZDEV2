*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0309_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0309_exit.

FORM f_exit_zsdt0309_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0309 TYPE zsdt0309.

  wl_zsdt0309-data  = sy-datum.
  wl_zsdt0309-hora  = sy-uzeit.
  wl_zsdt0309-usnam = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0309 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0309_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zsdt0309 TYPE zsdt0309_out.

  MOVE-CORRESPONDING p_registro_manter  TO w_zsdt0309.

  IF w_zsdt0309-vkbur IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Escritório de Vendas!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    SELECT SINGLE *
               FROM tvkbz
               INTO @DATA(w_tvkbz)
              WHERE vkorg = @w_zsdt0309-vkorg
                AND vkbur = @w_zsdt0309-vkbur.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Escritório de Vendas não Pertence a '
                            'Organização de Vendas informada no Filtro.'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0309_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zsdt0309 TYPE zsdt0309,
        l_id        TYPE zsdt0309-id.

  CLEAR: wl_zsdt0309.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zsdt0309.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZSD_SEQ_CE'
    IMPORTING
      number                  = l_id
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  wl_zsdt0309-id    = l_id.
  wl_zsdt0309-data  = sy-datum.
  wl_zsdt0309-hora  = sy-uzeit.
  wl_zsdt0309-usnam = sy-uname.

  MOVE-CORRESPONDING wl_zsdt0309 TO p_registro_manter.

ENDFORM.

FORM f_exit_zsdt0309_0004 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0309_0005 CHANGING p_registro_manter TYPE any.
ENDFORM.

FORM f_exit_zsdt0309_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.
ENDFORM.

FORM f_exit_zsdt0309_0008 CHANGING p_col_pos
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

  IF p_ref_tabname = 'ZSDT0309_OUT' AND
     p_field       = 'VKORG'.
    p_scrtext_l = 'Organização de vendas'.
    p_outputlen = 21.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0309_OUT' AND
     p_field       = 'VKBUR'.
    p_scrtext_l = 'Escritório de vendas'.
    p_outputlen = 21.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0309_OUT' AND
     p_field       = 'USNAM'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0309_OUT' AND
     p_field       = 'DATA'.
    p_scrtext_l = 'Data Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZSDT0309_OUT' AND
     p_field       = 'HORA'.
    p_scrtext_l = 'Hora Registro'.
    p_outputlen = 15.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zsdt0309_0009  TABLES pt_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
