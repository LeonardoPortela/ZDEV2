*&---------------------------------------------------------------------*
*&  Include           ZFI_R_002_ALV
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*

 FORM init_fieldcat USING t_fieldcat TYPE slis_t_fieldcat_alv.
   CLEAR t_fieldcat.

   PERFORM ls_fieldcat USING 'Sociedad'
                              'BUKRS'
                               '5'
                               '1'.

   PERFORM ls_fieldcat USING 'Documento Sap'
                               'BELNR'
                                '10'
                                '2'.

   PERFORM ls_fieldcat USING 'Ejercicio'
                               'GJAHR'
                                '5'
                                '3'.


* Inicio GB - 01/09/2010

   PERFORM ls_fieldcat USING 'Clase de documento'
                               'BLART'
                                '5'
                                '4'.

* Fin GB - 01/09/2010


   PERFORM ls_fieldcat USING 'C.U.I.T.'
                              'CUIT'
                              '13'      "Modificado 25.01.2011 - Diego
                              '5'.

   PERFORM ls_fieldcat USING 'Razón Social'
                             'NAME1'
                             '11'
                             '6'.

   PERFORM ls_fieldcat USING  'Clase de condición'
                              'KSCHL'
                              '4'
                              '7'.


   PERFORM ls_fieldcat USING 'Importe del impuesto'
                              'HWSTE'  " Modificado en 25.01.2011 - Diego
                              '15'
                              '8'.

   PERFORM ls_fieldcat USING 'Base impuesto'
                             'HWBAS'  " Modificado en 25.01.2011 - Diego
                             '15'
                             '9'.

   PERFORM ls_fieldcat USING 'Tipo impositivo'
                             'KBETR'
                             '16'
                             '10'.

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
*      -->P_GT_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*

 FORM construir_cabecera_alv USING lt_top_of_page TYPE slis_t_listheader.

   DATA: ls_line TYPE slis_listheader,
         vl_txt  TYPE char50         ,
         vl_butxt  TYPE char50       ,
         vl_fecha  TYPE sy-datum     ,
         vl_fecha2 TYPE sy-datum     ,
         vl_txt1   TYPE char10       ,
         vl_txt2   TYPE char10       .


* Centro

   ls_line-typ  = 'H'.
   ls_line-info =  'Informe de percepción de iva '.
   APPEND ls_line TO lt_top_of_page. CLEAR ls_line.

   SELECT SINGLE  butxt
   FROM t001
   INTO vl_butxt
   WHERE bukrs EQ s_bukrs.

   CONCATENATE 'Sociedad:'
               s_bukrs
               vl_txt
       INTO vl_txt SEPARATED BY space.

   ls_line-typ  = 'S'.
   ls_line-info =  vl_txt.
   APPEND ls_line TO lt_top_of_page.
   CLEAR ls_line.


   vl_fecha  = s_bldat-low.
   vl_fecha2 = s_bldat-high.

   ls_line-typ  = 'S'.



*   Fecha low

   CONCATENATE  vl_fecha+6(2)'/' vl_fecha+4(2)'/'vl_fecha(4)
  INTO  vl_txt1.


*   Fecha high

   CONCATENATE vl_fecha2+6(2)'/' vl_fecha2+4(2)'/'vl_fecha2(4)
   INTO  vl_txt2.


*   Junto las 2 Fechas

   CONCATENATE 'Fecha Ejecución:'
                vl_txt1
                'AL'
                vl_txt2
          INTO ls_line-info SEPARATED BY space.

   APPEND ls_line TO lt_top_of_page. CLEAR ls_line.

   ls_line-typ  = 'S'.
   CONCATENATE 'Usuario:' sy-uname INTO ls_line-info SEPARATED BY space.
   APPEND ls_line TO lt_top_of_page.

   CLEAR ls_line.

 ENDFORM.                    " CONSTRUIR_CABECERA_ALV

*&---------------------------------------------------------------------*
*&      Form  F_VISUALIZA_ALV
*&---------------------------------------------------------------------*

 FORM f_visualiza_alv.

   CONSTANTS: c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE'.

   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
       i_callback_top_of_page  = c_top_of_page

*       i_callback_user_command = 'USER_COMMAND_CUS'

       i_callback_program      = v_repid
       is_layout               = layout
       it_fieldcat             = t_fieldcat[]
       i_save                  = 'X'
       it_events               = t_events[]
     TABLES
       t_outtab                = t_salida
     EXCEPTIONS
       program_error           = 1
       OTHERS                  = 2.
   IF sy-subrc <> 0.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
   ENDIF.

 ENDFORM.                    " F_VISUALIZA_ALV

*&---------------------------------------------------------------------*
*&      Form  LS_FIELDCAT
*&---------------------------------------------------------------------*

 FORM ls_fieldcat  USING  pi_seltext
                          pi_fieldname
                          pi_outputlen
                          pi_col_pos.

   DATA: ls_fieldcat TYPE slis_fieldcat_alv.
   CLEAR ls_fieldcat.
   ls_fieldcat-col_pos    = pi_col_pos.
   ls_fieldcat-seltext_s  = pi_seltext.
   ls_fieldcat-seltext_m  = pi_seltext.
   ls_fieldcat-seltext_l  = pi_seltext.
   ls_fieldcat-fieldname  = pi_fieldname.
   ls_fieldcat-outputlen  = pi_outputlen.

*   ls_fieldcat-colddictxt = 'L'.

   APPEND ls_fieldcat TO t_fieldcat. CLEAR ls_fieldcat.


 ENDFORM.                    " LS_FIELDCAT


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       Se ejecutan los eventos producidos por el usuario
*----------------------------------------------------------------------*

 FORM user_command_cus USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.


*   DATA: x_salida TYPE ty_salida.
*
*   CASE r_ucomm.
*     WHEN '&IC1'.
*       READ TABLE t_salida INTO x_salida INDEX rs_selfield-tabindex.
*       IF x_salida-tipo = 'SD'.
*         SET PARAMETER ID: 'VF' FIELD x_salida-vbeln.
*         CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
*       ELSE.
*         SET PARAMETER ID: 'BLN' FIELD x_salida-vbeln,
*                           'BUK' FIELD p_bukrs,
*                           'GJR' FIELD x_salida-gjahr.
*         CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*       ENDIF.
*   ENDCASE.


 ENDFORM.                    "user_command
