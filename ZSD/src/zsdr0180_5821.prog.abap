*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_5821
*&---------------------------------------------------------------------*

TYPES: BEGIN OF TY_ROMANEIOS_5821,
         ICONE TYPE CHAR6.
        INCLUDE STRUCTURE ZSDT0001.
TYPES: END OF TY_ROMANEIOS_5821.

DATA:  G_CUSTOM_CONTAINER_POP_5821 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       CTL_ALV1_POP_5821           TYPE REF TO CL_GUI_ALV_GRID,
       GS_LAYOUT_POP_5821          TYPE LVC_S_LAYO,
       IT_FIELDCATALOG_POP_5821    TYPE LVC_T_FCAT.

DATA: IT_ZSDT0001_5821 TYPE STANDARD TABLE OF TY_ROMANEIOS_5821,
      IT_ZSDT0134_5821 TYPE STANDARD TABLE OF ZSDT0134.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER_5821 DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      TOOLBAR_5821 FOR EVENT TOOLBAR OF  CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      USER_COMMAND_5821 FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_5821 IMPLEMENTATION.

  METHOD TOOLBAR_5821.

    DATA WA_TOOL TYPE STB_BUTTON.

    MOVE 3 TO WA_TOOL-BUTN_TYPE.
    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
    CLEAR WA_TOOL.

    WA_TOOL-FUNCTION = 'ESTORNAROM'.
    WA_TOOL-ICON     = '@VI@'.
    WA_TOOL-QUICKINFO = 'Estornar Todos Romaneios'.
    APPEND WA_TOOL TO E_OBJECT->MT_TOOLBAR.
    CLEAR WA_TOOL.

  ENDMETHOD.             "DISPLAY

  METHOD USER_COMMAND_5821.

    DATA:WA_ZSDT0001   TYPE TY_ROMANEIOS_5821,
         WA_ZSDT0139   TYPE ZSDT0139,
         WA_ZSDT0134   TYPE ZSDT0134,
         WA_CARGA_5820 TYPE TY_CARGA_5820,
         VL_CHECK      TYPE CHAR1.


    IF E_UCOMM = 'ESTORNAROM'.

      LOOP AT IT_ZSDT0001_5821 INTO WA_ZSDT0001.
        IF WA_ZSDT0001-DOC_REM IS NOT INITIAL.
          VL_CHECK = ABAP_TRUE.
        ENDIF.
      ENDLOOP.

      IF VL_CHECK IS INITIAL.

        CREATE OBJECT ZCL_ROMANEIO.

        LOOP AT IT_ZSDT0001_5821 INTO WA_ZSDT0001.

          TRY.
              CALL METHOD ZCL_ROMANEIO->SET_REGISTRO
                EXPORTING
                  I_ID_REGISTRO = WA_ZSDT0001-CH_REFERENCIA.
            CATCH ZCX_CADASTRO INTO ZCX_CADASTRO.
              ZCX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
          ENDTRY.

          CALL METHOD ZCL_ROMANEIO->EXCLUIR_REGISTRO.

        ENDLOOP.

        LOOP AT IT_ZSDT0134_5821 INTO WA_ZSDT0134.
          CLEAR: WA_ZSDT0134-CH_REFERENCIA.
          MODIFY ZSDT0134 FROM WA_ZSDT0134.
        ENDLOOP.

        SELECT SINGLE *
          FROM ZSDT0139
          INTO WA_ZSDT0139
          WHERE NRO_CGD EQ VG_CG_PARA_ROM.

        WA_ZSDT0139-STATUS = WA_ZSDT0139-STATUS - 1.
        MODIFY ZSDT0139 FROM WA_ZSDT0139 .
        MESSAGE TEXT-059 TYPE 'S'.

        READ TABLE IT_CARGA_5820 INTO WA_CARGA_5820 WITH KEY NRO_CGD = VG_CG_PARA_ROM.
        IF SY-SUBRC IS INITIAL.
          WA_CARGA_5820-STATUS = 1.
          WA_CARGA_5820-ICONE  = '@5B@'.
          MODIFY IT_CARGA_5820 FROM WA_CARGA_5820 INDEX SY-TABIX.
        ENDIF.

      ELSE.
        MESSAGE TEXT-060 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      VG_TELA_5821 = ABAP_TRUE.

      CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
        EXPORTING
          CHAVE = VG_CG_PARA_ROM.



      CALL METHOD CTL_ALV1_5820->GET_FRONTEND_LAYOUT
        IMPORTING
          ES_LAYOUT = GS_LAYOUT_5820_ALV1.

      GS_LAYOUT_5820_ALV1-CWIDTH_OPT = ABAP_TRUE.

      CALL METHOD CTL_ALV1_5820->SET_FRONTEND_LAYOUT
        EXPORTING
          IS_LAYOUT = GS_LAYOUT_5820_ALV1.

      CALL METHOD CTL_ALV1_5820->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.

      LEAVE TO SCREEN 0.

    ENDIF.

  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5821_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5821_EXIT INPUT.

  CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
    EXPORTING
      CHAVE = VG_CG_PARA_ROM.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5821  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_5821 OUTPUT.

  SET PF-STATUS 'PF5131'.
  SET TITLEBAR  'T5821'.

  PERFORM PESQUISA_POP_5821.
  PERFORM MOSTRA_POP_5821.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5821  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_5821 INPUT.

  CASE SY-UCOMM.
    WHEN 'SALVAR'.

      CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
        EXPORTING
          CHAVE = VG_CG_PARA_ROM.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.

      CALL FUNCTION 'ZDENQUEUE_SD_CARGAD_INSUMOS'
        EXPORTING
          CHAVE = VG_CG_PARA_ROM.

      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_5821
*&---------------------------------------------------------------------*
FORM MOSTRA_POP_5821 .

  IF G_CUSTOM_CONTAINER_POP_5821 IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER_POP_5821
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER5821'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    PERFORM FILL_IT_FIELDCATALOG TABLES IT_FIELDCATALOG_POP_5821 USING:
          01 'NRO_CG'          ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
          01 'CH_REFERENCIA'   ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ref.',
          01 'NR_ROMANEIO'     ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Romaneio',
          01 'VBELN'           ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
          01 'ICONE'           ''         ' '  ' ' 'X'  ' '   'X'   ' '   ' '   ' '   'Finalizado'.

    GS_LAYOUT_POP_5821-SEL_MODE   = 'A'.
    GS_LAYOUT_POP_5821-CWIDTH_OPT = 'X'.

    CREATE OBJECT CTL_ALV1_POP_5821
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER_POP_5821.           "ALV Lote

    SET HANDLER:
      LCL_EVENT_HANDLER_5821=>TOOLBAR_5821 FOR CTL_ALV1_POP_5821,
      LCL_EVENT_HANDLER_5821=>USER_COMMAND_5821 FOR CTL_ALV1_POP_5821.

    CALL METHOD CTL_ALV1_POP_5821->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT_POP_5821
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_POP_5821
        IT_OUTTAB       = IT_ZSDT0001_5821.

  ELSE.
    CALL METHOD CTL_ALV1_POP_5821->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_5821
*&---------------------------------------------------------------------*
FORM PESQUISA_POP_5821 .

  DATA: IT_ROM_5820 TYPE STANDARD TABLE OF TY_ORDEM_5820,
        IT_ZSDT0134 TYPE STANDARD TABLE OF ZSDT0134.

  DATA: WA_ZSDT0001 TYPE TY_ROMANEIOS_5821.

  SELECT *
    FROM ZSDT0140
    INNER JOIN ZSDT0082 ON ZSDT0140~NRO_SOL = ZSDT0082~NRO_SOL AND
                           ZSDT0140~SEQ   = ZSDT0082~SEQ
    INTO CORRESPONDING FIELDS OF TABLE IT_ROM_5820
    WHERE ZSDT0140~STATUS  NE 'X'
      AND ZSDT0140~NRO_CGD EQ VG_CG_PARA_ROM.

  SORT IT_ROM_5820 BY VBELN SEQ.
  DELETE ADJACENT DUPLICATES FROM IT_ROM_5820 COMPARING VBELN SEQ.

  SELECT *
    FROM ZSDT0134
    INTO TABLE IT_ZSDT0134_5821
    FOR ALL ENTRIES IN IT_ROM_5820
    WHERE VBELN EQ IT_ROM_5820-VBELN
      AND NRO_CG EQ VG_CG_PARA_ROM
      AND STATUS NE 'X'.

  SELECT *
    FROM ZSDT0001
    INTO CORRESPONDING FIELDS OF TABLE IT_ZSDT0001_5821
    FOR ALL ENTRIES IN IT_ROM_5820
    WHERE NRO_CG EQ VG_CG_PARA_ROM
      AND VBELN EQ IT_ROM_5820-VBELN.

  LOOP AT IT_ZSDT0001_5821 INTO WA_ZSDT0001.
    IF WA_ZSDT0001-ST_PROC EQ '99'.
      WA_ZSDT0001-ICONE = '@01@'.
    ENDIF.
    MODIFY IT_ZSDT0001_5821 FROM WA_ZSDT0001 INDEX SY-TABIX.
  ENDLOOP.


ENDFORM.
