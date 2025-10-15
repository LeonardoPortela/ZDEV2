**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Paulo Quevedo ( paulo.quevedo@amaggi.com.br )                        |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Limite de Desconto/Acrescimo                                              |*
**/===========================================================================\*

REPORT ZSDR0106.

TABLES: VBAK, VBAP.

TYPES: BEGIN OF TY_SAIDA.
        INCLUDE TYPE ZSDT0195.
TYPES: ESTILO TYPE LVC_T_STYL,
       END OF TY_SAIDA,

       TY_T_SAIDA TYPE TABLE OF TY_SAIDA WITH DEFAULT KEY.

DATA: GT_OUTTAB     TYPE TABLE OF TY_SAIDA,
      GT_HIST       TYPE TABLE OF TY_SAIDA,
      WA_0195       TYPE ZSDT0195,
      _STABLE       TYPE LVC_S_STBL VALUE 'XX',
      WA_LAYOUT     TYPE LVC_S_LAYO,
      GT_ESTILO     TYPE LVC_T_STYL,
      TL_UCOMM      TYPE TABLE OF SY-UCOMM,

      "//Objects
      GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_H        TYPE REF TO CL_GUI_ALV_GRID,
      CUSTOM_GRID   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CUSTOM_GRID_H TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      IT_EXCLUDE    TYPE UI_FUNCTIONS,
      FCAT          TYPE LVC_T_FCAT.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
S_VKORG FOR VBAK-VKORG NO-EXTENSION NO INTERVALS OBLIGATORY,
S_SPART FOR VBAK-SPART,
S_AUART FOR VBAK-AUART.
SELECTION-SCREEN END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM SELECT.
  CALL SCREEN 0001.


MODULE MAIN_PBO OUTPUT.

  FREE TL_UCOMM.

  IF SY-UCOMM EQ 'INSR'.
    TL_UCOMM = VALUE #( ( 'HIST' ) ).
  ENDIF.

  SET TITLEBAR 'MAIN_TITLE'.
  SET PF-STATUS 'MAIN_STATUS' EXCLUDING TL_UCOMM.

  WA_LAYOUT = VALUE #( STYLEFNAME = 'ESTILO' ).

  IF CUSTOM_GRID_H IS NOT INITIAL.
    CALL METHOD CUSTOM_GRID_H->FREE.
    IF CUSTOM_GRID_H IS NOT INITIAL.
      FREE CUSTOM_GRID_H.
    ENDIF.
    CALL METHOD CL_GUI_CFW=>FLUSH.
  ENDIF.

  FCAT = VALUE LVC_T_FCAT(
                 ( REF_TABLE = 'VBAK'     REF_FIELD = 'VKORG' FIELDNAME = 'VKORG'  COLTEXT = 'Empresa'    OUTPUTLEN = 10 )
                 ( REF_TABLE = 'ZSDT0041' REF_FIELD = 'SPART' FIELDNAME = 'SPART'  COLTEXT = 'St. Ativ'   OUTPUTLEN = 18 )
                 ( REF_TABLE = 'VBAK'     REF_FIELD = 'AUART' FIELDNAME = 'AUART'  COLTEXT = 'Tipo de OV' OUTPUTLEN = 08 )
                 ( REF_TABLE = 'VBAK'     REF_FIELD = 'WAERK' FIELDNAME = 'WAERK'  COLTEXT = 'Moeda'      OUTPUTLEN = 08 )
                 ( REF_TABLE = 'VBAK'     REF_FIELD = 'NETWR' FIELDNAME = 'NETWR'  COLTEXT = 'Limite'     OUTPUTLEN = 20 )
                 ( REF_TABLE = ''         REF_FIELD = ''      FIELDNAME = 'USNAM'  COLTEXT = 'Usuário'    OUTPUTLEN = 20 NO_OUT = ABAP_TRUE )
                 ( REF_TABLE = ''         REF_FIELD = ''      FIELDNAME = 'DATA'   COLTEXT = 'Data'       OUTPUTLEN = 10 NO_OUT = ABAP_TRUE )
                 ( REF_TABLE = ''         REF_FIELD = ''      FIELDNAME = 'HORA'   COLTEXT = 'Hora'       OUTPUTLEN = 10 NO_OUT = ABAP_TRUE )
                ).

  LOOP AT GT_OUTTAB ASSIGNING FIELD-SYMBOL(<F_OUTTAB>).
    FREE <F_OUTTAB>-ESTILO.
    IF <F_OUTTAB>-STATUS EQ 'I'.
      GT_ESTILO = VALUE #(
                            ( FIELDNAME = 'VKORG' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                            ( FIELDNAME = 'SPART' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                            ( FIELDNAME = 'AUART' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                            ( FIELDNAME = 'WAERK' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                            ( FIELDNAME = 'NETWR' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                         ).

      IF <F_OUTTAB>-VKORG IS NOT INITIAL.
        DELETE GT_ESTILO WHERE FIELDNAME = 'VKORG'.
      ENDIF.

      IF <F_OUTTAB>-SPART IS NOT INITIAL.
        DELETE GT_ESTILO WHERE FIELDNAME = 'SPART'.
      ENDIF.

      IF <F_OUTTAB>-AUART IS NOT INITIAL.
        DELETE GT_ESTILO WHERE FIELDNAME = 'AUART'.
      ENDIF.

      INSERT LINES OF GT_ESTILO INTO TABLE <F_OUTTAB>-ESTILO.
    ENDIF.
  ENDLOOP.

  IT_EXCLUDE = VALUE #(
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW      )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      )
                        ( CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           )
                        ( CL_GUI_ALV_GRID=>MC_FC_REFRESH           )
                      ).

  IF CUSTOM_GRID IS INITIAL.

    CREATE OBJECT CUSTOM_GRID
      EXPORTING
        CONTAINER_NAME = 'CC_01'.

    CREATE OBJECT GRID
      EXPORTING
        I_PARENT = CUSTOM_GRID.

    CALL METHOD GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = WA_LAYOUT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
      CHANGING
        IT_OUTTAB            = GT_OUTTAB
        IT_FIELDCATALOG      = FCAT.

    GRID->REGISTER_EDIT_EVENT( CL_GUI_ALV_GRID=>MC_EVT_MODIFIED ).
    GRID->REGISTER_EDIT_EVENT( CL_GUI_ALV_GRID=>MC_EVT_ENTER ).
    GRID->SET_READY_FOR_INPUT( 1 ).

  ELSE.
    GRID->REFRESH_TABLE_DISPLAY( ).
  ENDIF.

  LOOP AT FCAT ASSIGNING FIELD-SYMBOL(<FCAT>).
    <FCAT>-NO_OUT = ABAP_FALSE.
    <FCAT>-COL_OPT = ABAP_TRUE.
  ENDLOOP.

  CHECK SY-UCOMM EQ 'HIST'.

  CREATE OBJECT CUSTOM_GRID_H
    EXPORTING
      CONTAINER_NAME = 'CC_02'.

  CREATE OBJECT GRID_H
    EXPORTING
      I_PARENT = CUSTOM_GRID_H.

  CALL METHOD GRID_H->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_LAYOUT
      IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
    CHANGING
      IT_OUTTAB            = GT_HIST
      IT_FIELDCATALOG      = FCAT.

ENDMODULE.

MODULE MAIN_PAI INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'INSR'.
      PERFORM ADD_LINE.
    WHEN 'SAVE'.
      PERFORM SAVE.
    WHEN 'CANCELAR'.
      PERFORM SELECT.
    WHEN 'HIST'.
      PERFORM SELECT_HIST.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ADD_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_LINE.

  DELETE GT_OUTTAB WHERE STATUS NE 'I'.

  APPEND VALUE #(
                    VKORG = S_VKORG-LOW
                    SPART = S_SPART-LOW
                    AUART = S_AUART-LOW
                    STATUS = 'I' "// Status de Incluir "Somente em tempo de Execução"
                    USNAM  = SY-UNAME
                    DATA   = SY-DATUM
                    HORA   = SY-UZEIT
                 ) TO GT_OUTTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE.

  LOOP AT GT_OUTTAB INTO DATA(WA_OUTTAB).

    IF WA_OUTTAB-VKORG IS INITIAL OR
       WA_OUTTAB-SPART IS INITIAL OR
       WA_OUTTAB-AUART IS INITIAL OR
       WA_OUTTAB-WAERK IS INITIAL OR
       WA_OUTTAB-NETWR IS INITIAL .

*      MESSAGE 'Existe Campo Obrigátorio sem Informação!' TYPE 'E' DISPLAY LIKE 'S'.
      CONTINUE.

    ENDIF.

    SELECT SINGLE *
      FROM ZSDT0195
      INTO @DATA(WA_CHECK)
      WHERE VKORG  EQ @WA_OUTTAB-VKORG
        AND SPART  EQ @WA_OUTTAB-SPART
        AND AUART  EQ @WA_OUTTAB-AUART
        AND WAERK  EQ @WA_OUTTAB-WAERK
        AND STATUS EQ @ABAP_FALSE.

    IF SY-SUBRC IS INITIAL.
      UPDATE ZSDT0195 SET STATUS = ABAP_TRUE
      WHERE VKORG  EQ WA_OUTTAB-VKORG
        AND SPART  EQ WA_OUTTAB-SPART
        AND AUART  EQ WA_OUTTAB-AUART
        AND WAERK  EQ WA_OUTTAB-WAERK.
      COMMIT WORK.
    ENDIF.

    CLEAR WA_0195.
    WA_OUTTAB-STATUS = ABAP_FALSE. "// Status de Ativo
    MOVE-CORRESPONDING WA_OUTTAB TO WA_0195.

    MODIFY ZSDT0195 FROM WA_0195.
    COMMIT WORK.

  ENDLOOP.

  PERFORM SELECT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT .

  SELECT *
    FROM ZSDT0195
    INTO CORRESPONDING FIELDS OF TABLE GT_OUTTAB
    WHERE VKORG IN S_VKORG
      AND SPART IN S_SPART
      AND AUART IN S_AUART
      AND STATUS EQ ABAP_FALSE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECT_HIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_HIST .

  SELECT *
    FROM ZSDT0195
    INTO CORRESPONDING FIELDS OF TABLE GT_HIST
    WHERE VKORG IN S_VKORG
      AND SPART IN S_SPART
      AND AUART IN S_AUART
      AND STATUS EQ ABAP_TRUE.

ENDFORM.
