*&---------------------------------------------------------------------*
*&  Include           ZFI_ACT_PADRON_AFIP_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM f_alv .

* El alv se muestra si el proceso no fué ejecutado de fondo
* Modificado en 06.12.2010 - Diego...
* check sy-batch IS INITIAL.
  if sy-batch is initial.

    CLEAR: t_fieldcat,
            layout,
            t_fieldcat,
            t_events,
            gt_list_top_of_page,
            gt_list_end_of_list.

    REFRESH: t_fieldcat,
             t_fieldcat,
             t_events,
             gt_list_top_of_page,
             gt_list_end_of_list.

    v_repid = sy-repid.

    PERFORM init_fieldcat USING t_fieldcat[].
    PERFORM init_eventos.
    PERFORM init_layout.
* Muestro la informacion en el alv
    PERFORM f_visualiza_alv.

* ..............................................
  else.

    loop at t_mensaje_e into st_mensaje_e.

      Write: /01 st_mensaje_e-BUKRS,
                 st_mensaje_e-LIFNR,
                 st_mensaje_e-FECHA,
                 st_mensaje_e-HORA,
                 st_mensaje_e-INDICADOR,
                 st_mensaje_e-TEXTO,
                 st_mensaje_e-TIPO,
                 st_mensaje_e-STCD1.

    endloop.

  endif.
* Fin 06.12.2010 ...............................

ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
FORM init_fieldcat USING t_fieldcat TYPE slis_t_fieldcat_alv.

  PERFORM f_ls_fieldcat USING 'Sociedad'         'BUKRS'       '6'  ' ' '01'.
  PERFORM f_ls_fieldcat USING 'Nro Provedor'     'LIFNR'       '15' ' ' '02'.
  PERFORM f_ls_fieldcat USING 'Razón Social'     'NAME1'       '40' ' ' '03'.
  PERFORM f_ls_fieldcat USING 'CUIT'             'STCD1'       '11' ' ' '04'.
  PERFORM f_ls_fieldcat USING 'Fecha ejecución'  'FECHA'       '10' ' ' '05'.
  PERFORM f_ls_fieldcat USING 'Hora ejecución'   'HORA'        '10' ' ' '06'.
  PERFORM f_ls_fieldcat USING 'Indicador'        'INDICADOR'   '10' ' ' '07'.
  PERFORM f_ls_fieldcat USING 'Error'            'TEXTO'       '40' ' ' '08'.

ENDFORM.                    " INIT_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  INIT_EVENTOS
*&---------------------------------------------------------------------*
FORM init_eventos.
  REFRESH t_events.
  CLEAR   t_events.
  DATA e_events TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      et_events       = t_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CONSTANTS: c_end_of_list TYPE slis_formname VALUE 'END_OF_LIST'.

  LOOP AT t_events INTO e_events.
    CASE e_events-name.
      WHEN slis_ev_top_of_page.
        e_events-form = 'TOP_OF_PAGE'. "Nombre del FORM
      WHEN c_end_of_list.
        e_events-form = 'END_OF_LIST'. "Nombre del FORM
    ENDCASE.
    MODIFY t_events FROM e_events.
  ENDLOOP.



ENDFORM.                    " INIT_EVENTOS


*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
FORM init_layout.

  layout-zebra               = 'X'.

ENDFORM.                    " INIT_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  end_of_list
*&---------------------------------------------------------------------*
FORM end_of_list.

ENDFORM.                    "END_OF_LIST


*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  CLEAR gt_list_top_of_page.
  PERFORM construir_cabecera_alv USING gt_list_top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.
ENDFORM.                    "TOP_OF_PAGE


*&---------------------------------------------------------------------*
*&      Form  CONSTRUIR_CABECERA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM construir_cabecera_alv USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line TYPE slis_listheader.

  IF rb_002 EQ 'X'.
    ls_line-typ  = 'H'.
    ls_line-info =  'Registro procesado con errores'.
    APPEND ls_line TO lt_top_of_page. CLEAR ls_line.
  ELSE.
    ls_line-typ  = 'H'.
    ls_line-info =  'Informe de Proceso'.
    APPEND ls_line TO lt_top_of_page. CLEAR ls_line.
  ENDIF.

  ls_line-typ  = 'S'.
  CONCATENATE sy-datum+6(2)'/' sy-datum+4(2)'/'sy-datum(4)
  INTO  ls_line-info.
  APPEND ls_line TO lt_top_of_page. CLEAR ls_line.

  ls_line-typ  = 'S'.
  ls_line-info =  sy-uname.
  APPEND ls_line TO lt_top_of_page.
  CLEAR ls_line.



ENDFORM.                    " CONSTRUIR_CABECERA_ALV


*&---------------------------------------------------------------------*
*&      Form  F_VISUALIZA_ALV
*&---------------------------------------------------------------------*
FORM f_visualiza_alv.

  CONSTANTS: c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE'.
  SORT t_mensaje_e BY tipo lifnr indicador.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_top_of_page = c_top_of_page
      i_callback_program     = v_repid
      is_layout              = layout
      it_fieldcat            = t_fieldcat[]
      i_save                 = 'X'
      it_events              = t_events[]
    TABLES
      t_outtab               = t_mensaje_e
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_VISUALIZA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_LS_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_ls_fieldcat  USING    pi_corto
                             pi_campo
                             pi_long
                             pi_visible
                             pi_col.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
*  Sociedad
  ls_fieldcat-outputlen = pi_long.
  ls_fieldcat-seltext_s = pi_corto.
  ls_fieldcat-seltext_m = pi_corto.
  ls_fieldcat-seltext_l = pi_corto.
  ls_fieldcat-fieldname = pi_campo.
  ls_fieldcat-no_out    = pi_visible.
  ls_fieldcat-col_pos   = pi_col.

  APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
  CLEAR ls_fieldcat.
ENDFORM.                    " F_LS_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  CONSTRUIR_FIN_PAG_ALV
*&---------------------------------------------------------------------*
FORM construir_fin_pag_alv  USING  lt_end_of_page TYPE slis_t_listheader.

ENDFORM.                    " CONSTRUIR_FIN_PAG_ALV
