*&---------------------------------------------------------------------*
*& Report  ZPMR0053
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZPMR0053.

TABLES: ZPMT0033.

TYPES: BEGIN OF TY_SAIDA,
         IWERK   TYPE QMIH-IWERK,
         EQART   TYPE ITOB-EQART,
         MTTR    TYPE ZPMT0033-MTTR,
         MTBF    TYPE ZPMT0033-MTBF,
         INDIS   TYPE ZPMT0033-INDIS,
         CONF    TYPE ZPMT0033-CONF,
         EARTX   TYPE T370K_T-EARTX,
         CELLTAB TYPE LVC_T_STYL,
       END OF TY_SAIDA.

DATA: IT_SAIDA    TYPE TABLE OF TY_SAIDA,
      WA_SAIDA    TYPE TY_SAIDA,
      IT_ZPMT0033 TYPE TABLE OF ZPMT0033,
      WA_ZPMT0033 TYPE  ZPMT0033,
      IT_T370K_T  TYPE TABLE OF T370K_T,
      WA_T370K_T  TYPE T370K_T.


DATA: G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_GRID             TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT            TYPE LVC_T_FCAT,
      WA_FCAT            TYPE LVC_S_FCAT,
      TL_FUNCTION        TYPE UI_FUNCTIONS,
      WL_FUNCTION        LIKE TL_FUNCTION WITH HEADER LINE,
      IT_SELECTEDROW     TYPE LVC_T_ROW,
      WA_SELECTEDROW     TYPE LVC_S_ROW,
      WA_LAYOUT          TYPE LVC_S_LAYO,
      WA_VARIANT         TYPE DISVARIANT,
      WA_ESTILO          TYPE LVC_T_STYL,
      WA_STABLE          TYPE LVC_S_STBL VALUE 'XX'.


START-OF-SELECTION.


  PERFORM BUSCA_DADOS.

  CALL SCREEN 0100.


FORM BUSCA_DADOS.

  SELECT * FROM ZPMT0033 INTO TABLE IT_ZPMT0033.

  CHECK   IT_ZPMT0033 IS NOT INITIAL.

  SELECT * FROM T370K_T INTO TABLE IT_T370K_T
    FOR ALL ENTRIES IN IT_ZPMT0033
   WHERE SPRAS EQ SY-LANGU
    AND  EQART EQ IT_ZPMT0033-EQART.


  LOOP AT IT_ZPMT0033 INTO WA_ZPMT0033.

    READ TABLE IT_T370K_T INTO WA_T370K_T WITH KEY EQART = WA_ZPMT0033-EQART.

    WA_SAIDA-IWERK      =  WA_ZPMT0033-IWERK.
    WA_SAIDA-EQART      =  WA_ZPMT0033-EQART.
    WA_SAIDA-MTTR       =  WA_ZPMT0033-MTTR.
    WA_SAIDA-MTBF       =  WA_ZPMT0033-MTBF.
    WA_SAIDA-INDIS      =  WA_ZPMT0033-INDIS.
    WA_SAIDA-CONF       =  WA_ZPMT0033-CONF.
    WA_SAIDA-EARTX      =  WA_T370K_T-EARTX.

    FREE WA_SAIDA-CELLTAB.
    WA_ESTILO =  VALUE #( ( FIELDNAME = 'IWERK' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED  )
                          ( FIELDNAME = 'EQART' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED  )
                          ( FIELDNAME = 'MTTR'  STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED  )
                          ( FIELDNAME = 'MTBF'  STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED  )
                          ( FIELDNAME = 'INDIS' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED  )
                          ( FIELDNAME = 'CONF'  STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED  )   ).
    INSERT LINES OF WA_ESTILO INTO TABLE WA_SAIDA-CELLTAB.

    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR: WA_SAIDA, WA_ZPMT0033, WA_T370K_T.
  ENDLOOP.

ENDFORM.


FORM ALV.
  REFRESH IT_FCAT.
  PERFORM PREENCHE_CAT USING:
          'IWERK'         'Centro'         '07'     ''     ''     ''     ''   'X'     'QMIH'    'IWERK'    '',
          'EQART'         'Categoria'      '12'     ''     ''     ''     ''   'X'     'T370K_T' 'EQART'    '',
          'EARTX'         'Descrição'      '20'     ''     ''     ''     ''   ' '     ''        ''         '',
          'MTTR'          'MTTR'           '10'     ''     ''     ''     ''   'X'     ''        ''         '',
          'MTBF'          'MTBF'           '10'     ''     ''     ''     ''   'X'     ''        ''         '',
          'INDIS'         'INDIS'          '10'     ''     ''     ''     ''   'X'     ''        ''         '',
          'CONF'          'CONF'           '10'     ''     ''     ''     ''   'X'     ''        ''         ''.
ENDFORM.


FORM PREENCHE_CAT USING VALUE(P_CAMPO)
                        VALUE(P_DESC)
                        VALUE(P_TAM)
                        VALUE(P_ZERO)
                        VALUE(P_HOT)
                        VALUE(P_SUM)
                        VALUE(P_JUST)
                        VALUE(P_EDIT)
                        VALUE(P_TABLE)
                        VALUE(P_FIELDNAME)
                        VALUE(P_F4).

  WA_FCAT-FIELDNAME   = P_CAMPO.
  WA_FCAT-COLTEXT     = P_DESC.
  WA_FCAT-SCRTEXT_L   = P_DESC.
  WA_FCAT-SCRTEXT_M   = P_DESC.
  WA_FCAT-SCRTEXT_S   = P_DESC.
  WA_FCAT-OUTPUTLEN   = P_TAM.
  WA_FCAT-HOTSPOT     = P_HOT.
  WA_FCAT-NO_ZERO     = P_ZERO.
  WA_FCAT-DO_SUM      = P_SUM.
  WA_FCAT-JUST        = P_JUST.
  WA_FCAT-EDIT        = P_EDIT.
  WA_FCAT-REF_TABLE   = P_TABLE.
  WA_FCAT-REF_FIELD   = P_FIELDNAME.
  WA_FCAT-F4AVAILABL  = P_F4.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM ALV.

  IF G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    IF G_GRID IS INITIAL AND G_CUSTOM_CONTAINER IS NOT  INITIAL.

      CREATE OBJECT G_GRID
        EXPORTING
          I_PARENT          = G_CUSTOM_CONTAINER
        EXCEPTIONS
          ERROR_CNTL_CREATE = 1
          ERROR_CNTL_INIT   = 2
          ERROR_CNTL_LINK   = 3
          ERROR_DP_CREATE   = 4
          OTHERS            = 5.
    ENDIF.

    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION  = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    WA_LAYOUT-STYLEFNAME = 'CELLTAB'.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT           = WA_VARIANT
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
        IT_OUTTAB            = IT_SAIDA
        IT_FIELDCATALOG      = IT_FCAT.


    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

  ELSE.

    CALL METHOD G_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = IT_FCAT.


    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  DATA: WA_SAVE TYPE ZPMT0033.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      LOOP AT IT_SAIDA INTO WA_SAIDA.
        WA_SAVE-MANDT       =   SY-MANDT.
        MOVE-CORRESPONDING WA_SAIDA TO WA_SAVE.

        MODIFY ZPMT0033 FROM WA_SAVE.
        COMMIT WORK.

        CLEAR: WA_SAVE, WA_SAIDA.
      ENDLOOP.

      MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.
      REFRESH IT_SAIDA.

      PERFORM BUSCA_DADOS.

    WHEN '&INS'.
      APPEND INITIAL LINE TO IT_SAIDA.

    WHEN '&DEL'.

      CALL METHOD G_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECTEDROW.

      IF IT_SELECTEDROW[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
        EXIT.
      ELSE.

        LOOP AT IT_SELECTEDROW INTO WA_SELECTEDROW.

          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SELECTEDROW-INDEX.

          DELETE FROM ZPMT0033 WHERE IWERK  EQ WA_SAIDA-IWERK
                                AND  EQART  EQ WA_SAIDA-EQART.

          CLEAR: WA_SAIDA, WA_SELECTEDROW.
        ENDLOOP.

        REFRESH IT_SAIDA.

        PERFORM BUSCA_DADOS.

        CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
          EXPORTING
            IS_STABLE = WA_STABLE.
      ENDIF.
  ENDCASE.
ENDMODULE.
