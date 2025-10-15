*&---------------------------------------------------------------------*
*&  Include           ZFIY0002_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
 FORM init_fieldcat USING t_fieldcat TYPE slis_t_fieldcat_alv.

 DATA: ls_fieldcat TYPE slis_fieldcat_alv.

*  Sociedad
   ls_fieldcat-seltext_s = 'Sociedad'.
   ls_fieldcat-seltext_m = 'Sociedad'.
   ls_fieldcat-seltext_l = 'Sociedad'.
   ls_fieldcat-fieldname = 'BUKRS'.
   ls_fieldcat-outputlen = '6'.
   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
   CLEAR ls_fieldcat.


*  Nro Proveedor
   ls_fieldcat-seltext_s = 'Nro Proveedor'.
   ls_fieldcat-seltext_m = 'Nro Proveedor'.
   ls_fieldcat-seltext_l = 'Nro Proveedor'.
   ls_fieldcat-fieldname = 'LIFNR'.
   ls_fieldcat-outputlen = '15'.
   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
   CLEAR ls_fieldcat.


*  Nro de cuit
   ls_fieldcat-seltext_s = 'Nro de Cuit'.
   ls_fieldcat-seltext_m = 'Nro de Cuit'.
   ls_fieldcat-seltext_l = 'Nro de Cuit'.
   ls_fieldcat-fieldname = 'STCD1'.
   ls_fieldcat-outputlen = '18'.
   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
   CLEAR ls_fieldcat.

*  Modo
   ls_fieldcat-seltext_s = 'Modo'.
   ls_fieldcat-seltext_m = 'Modo'.
   ls_fieldcat-seltext_l = 'Modo'.
   ls_fieldcat-fieldname = 'MODO'.
   ls_fieldcat-outputlen = '30'.
   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
   CLEAR ls_fieldcat.


*  SEMAFORO
   ls_fieldcat-seltext_s = ''.
   ls_fieldcat-seltext_m = ''.
   ls_fieldcat-seltext_l = ''.
   ls_fieldcat-fieldname = 'SEMAFORO'.
   ls_fieldcat-outputlen = '6'.
   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
   CLEAR ls_fieldcat.


*  Texto
   ls_fieldcat-seltext_s = 'Informe'.
   ls_fieldcat-seltext_m = 'Informe'.
   ls_fieldcat-seltext_l = 'Informe'.
   ls_fieldcat-fieldname = 'TEXT'.
   ls_fieldcat-no_out    = 'X'.
   ls_fieldcat-outputlen = '40'.
   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
   CLEAR ls_fieldcat.

*
   ls_fieldcat-seltext_s = 'Indicador A'.
   ls_fieldcat-seltext_m = 'Indicador A'.
   ls_fieldcat-seltext_l = 'Indicador A'.
   ls_fieldcat-fieldname = 'RETA'.
   ls_fieldcat-outputlen = '10'.
   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
   CLEAR ls_fieldcat.

   ls_fieldcat-seltext_s = 'Indicador D'.
   ls_fieldcat-seltext_m = 'Indicador D'.
   ls_fieldcat-seltext_l = 'Indicador D'.
   ls_fieldcat-fieldname = 'RETD'.
   ls_fieldcat-outputlen = '10'.
   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.
   CLEAR ls_fieldcat.

 ENDFORM.                    " INIT_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  INIT_EVENTOS
*&---------------------------------------------------------------------*
 FORM init_eventos.
refresh t_events.
clear t_events.
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

   LOOP AT t_events INTO e_events.
     CASE e_events-name.

       WHEN slis_ev_top_of_page.
         e_events-form = 'TOP_OF_PAGE'. "Nombre del FORM
         MODIFY t_events FROM e_events.

     ENDCASE.
   ENDLOOP.

 ENDFORM.                    " INIT_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
 FORM init_layout.

   layout-zebra               = 'X'.

 ENDFORM.                    " INIT_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
 FORM top_of_page.
   clear gt_list_top_of_page.
    perFORM construir_cabecera_alv using gt_list_top_of_page.
   CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
     EXPORTING
       it_list_commentary = gt_list_top_of_page.

 ENDFORM.                    "TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  CONSTRUIR_CABECERA_ALV
*&---------------------------------------------------------------------*
 FORM construir_cabecera_alv USING lt_top_of_page TYPE slis_t_listheader.

   DATA: ls_line TYPE slis_listheader.

* Centro
   ls_line-typ  = 'H'.
   ls_line-info =  'Informe de Proceso'.
   APPEND ls_line TO lt_top_of_page. CLEAR ls_line.

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
Sort t_mes_sal by Bukrs LIFNR.
   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
       i_callback_top_of_page = c_top_of_page
       i_callback_program     = v_repid
       is_layout              = layout
       it_fieldcat            = t_fieldcat[]
       i_save                 = 'X'
       it_events              = t_events[]
     TABLES
       t_outtab               = t_mes_sal
     EXCEPTIONS
       program_error          = 1
       OTHERS                 = 2.
   IF sy-subrc <> 0.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
   ENDIF.

 ENDFORM.                    " F_VISUALIZA_ALV
