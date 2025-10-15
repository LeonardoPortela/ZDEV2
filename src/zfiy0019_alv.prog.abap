*&---------------------------------------------------------------------*
*&  Include           ZFIY0019_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM F_ALV .

* El alv se muestra si el proceso no fué ejecutado de fondo
* Modificado en 06.12.2010 - Diego...
* check sy-batch IS INITIAL.
  IF SY-BATCH IS INITIAL.

    CLEAR: T_FIELDCAT,
            LAYOUT,
            T_FIELDCAT,
            T_EVENTS,
            GT_LIST_TOP_OF_PAGE,
            GT_LIST_END_OF_LIST.

    REFRESH: T_FIELDCAT,
             T_FIELDCAT,
             T_EVENTS,
             GT_LIST_TOP_OF_PAGE,
             GT_LIST_END_OF_LIST.

    V_REPID = SY-REPID.

    PERFORM INIT_FIELDCAT USING T_FIELDCAT[].
    PERFORM INIT_EVENTOS.
    PERFORM INIT_LAYOUT.

* Muestro la informacion en el alv
    PERFORM F_VISUALIZA_ALV.

* ..............................................
  ELSE.

    LOOP AT t_output INTO St_output.

      WRITE: /01 St_output-LIFNR,
                 St_output-NAME1,
                 St_output-STCD1,
                 St_output-KTOKK,
                 St_output-BRSCH,
                 St_output-CATEGORIA,
                 St_output-SITUACION,
                 St_output-WITHT,
                 St_output-WT_WITHCD.

    ENDLOOP.

  ENDIF.
* Fin 06.12.2010 ...............................

ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
FORM INIT_FIELDCAT USING T_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  PERFORM F_LS_FIELDCAT USING 'Nro Provedor'     'LIFNR'       '15' ' ' '01'.
  PERFORM F_LS_FIELDCAT USING 'Razón Social'     'NAME1'       '40' ' ' '02'.
  PERFORM F_LS_FIELDCAT USING 'CUIT'             'STCD1'       '11' ' ' '03'.
  PERFORM F_LS_FIELDCAT USING 'Grupo de Cuentas' 'KTOKK'       '04' ' ' '04'.
  PERFORM F_LS_FIELDCAT USING 'Ramo'             'BRSCH'       '04' ' ' '05'.
  PERFORM F_LS_FIELDCAT USING 'Categoria'        'CATEGORIA'   '20' ' ' '06'.
  PERFORM F_LS_FIELDCAT USING 'Situacion'        'SITUACION'   '20' ' ' '07'.
  PERFORM F_LS_FIELDCAT USING 'Tipo Retencion'   'WITHT'       '02' ' ' '08'.
  PERFORM F_LS_FIELDCAT USING 'Indicador'        'WT_WITHCD'   '02' ' ' '09'.

ENDFORM.                    " INIT_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  INIT_EVENTOS
*&---------------------------------------------------------------------*
FORM INIT_EVENTOS.
  REFRESH T_EVENTS.
  CLEAR   T_EVENTS.
  DATA E_EVENTS TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      ET_EVENTS       = T_EVENTS
    EXCEPTIONS
      LIST_TYPE_WRONG = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CONSTANTS: C_END_OF_LIST TYPE SLIS_FORMNAME VALUE 'END_OF_LIST'.

  LOOP AT T_EVENTS INTO E_EVENTS.
    CASE E_EVENTS-NAME.
      WHEN SLIS_EV_TOP_OF_PAGE.
        E_EVENTS-FORM = 'TOP_OF_PAGE'. "Nombre del FORM
      WHEN C_END_OF_LIST.
        E_EVENTS-FORM = 'END_OF_LIST'. "Nombre del FORM
    ENDCASE.
    MODIFY T_EVENTS FROM E_EVENTS.
  ENDLOOP.



ENDFORM.                    " INIT_EVENTOS


*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
FORM INIT_LAYOUT.

  LAYOUT-ZEBRA               = 'X'.

ENDFORM.                    " INIT_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  end_of_list
*&---------------------------------------------------------------------*
FORM END_OF_LIST.

ENDFORM.                    "END_OF_LIST


*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CLEAR GT_LIST_TOP_OF_PAGE.
  PERFORM CONSTRUIR_CABECERA_ALV USING GT_LIST_TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.
ENDFORM.                    "TOP_OF_PAGE


*&---------------------------------------------------------------------*
*&      Form  CONSTRUIR_CABECERA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CONSTRUIR_CABECERA_ALV USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.


  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO =  'Informe de Proceso'.
  APPEND LS_LINE TO LT_TOP_OF_PAGE. CLEAR LS_LINE.

  LS_LINE-TYP  = 'S'.
  CONCATENATE SY-DATUM+6(2)'/' SY-DATUM+4(2)'/'SY-DATUM(4)
  INTO  LS_LINE-INFO.
  APPEND LS_LINE TO LT_TOP_OF_PAGE. CLEAR LS_LINE.

  LS_LINE-TYP  = 'S'.
  LS_LINE-INFO =  SY-UNAME.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
  CLEAR LS_LINE.



ENDFORM.                    " CONSTRUIR_CABECERA_ALV


*&---------------------------------------------------------------------*
*&      Form  F_VISUALIZA_ALV
*&---------------------------------------------------------------------*
FORM F_VISUALIZA_ALV.

  CONSTANTS: C_TOP_OF_PAGE  TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
  SORT t_output BY LIFNR KTOKK BRSCH WITHT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_TOP_OF_PAGE = C_TOP_OF_PAGE
      I_CALLBACK_PROGRAM     = V_REPID
      IS_LAYOUT              = LAYOUT
      IT_FIELDCAT            = T_FIELDCAT[]
      I_SAVE                 = 'X'
      IT_EVENTS              = T_EVENTS[]
    TABLES
      T_OUTTAB               = t_output
    EXCEPTIONS
      PROGRAM_ERROR          = 1
      OTHERS                 = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_VISUALIZA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_LS_FIELDCAT
*&---------------------------------------------------------------------*
FORM F_LS_FIELDCAT  USING    PI_CORTO
                             PI_CAMPO
                             PI_LONG
                             PI_VISIBLE
                             PI_COL.
  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
*  Sociedad
  LS_FIELDCAT-OUTPUTLEN = PI_LONG.
  LS_FIELDCAT-SELTEXT_S = PI_CORTO.
  LS_FIELDCAT-SELTEXT_M = PI_CORTO.
  LS_FIELDCAT-SELTEXT_L = PI_CORTO.
  LS_FIELDCAT-FIELDNAME = PI_CAMPO.
  LS_FIELDCAT-NO_OUT    = PI_VISIBLE.
  LS_FIELDCAT-COL_POS   = PI_COL.

  APPEND LS_FIELDCAT TO T_FIELDCAT. CLEAR LS_FIELDCAT.
  CLEAR LS_FIELDCAT.
ENDFORM.                    " F_LS_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  CONSTRUIR_FIN_PAG_ALV
*&---------------------------------------------------------------------*
FORM CONSTRUIR_FIN_PAG_ALV  USING  LT_END_OF_PAGE TYPE SLIS_T_LISTHEADER.

ENDFORM.                    " CONSTRUIR_FIN_PAG_ALV
