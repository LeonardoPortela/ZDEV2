*&---------------------------------------------------------------------*
*& Report  ZMMR150
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR150.

TABLES: ZMMT0102.

TYPES: BEGIN OF TY_SAIDA,
         EBELN     TYPE ZMMT0102-EBELN,
         EBELP     TYPE ZMMT0102-EBELP,
         MBLNR     TYPE ZMMT0102-MBLNR,
         MJAHR     TYPE ZMMT0102-MJAHR,
         CHARG     TYPE ZMMT0102-CHARG,
         NR_FASE   TYPE ZMMT0102-NR_FASE,
         MATNR     TYPE ZMMT0102-MATNR,
         WERKS     TYPE ZMMT0102-WERKS,
         LGORT     TYPE ZMMT0102-LGORT,
         CATEGORIA TYPE ZMMT0102-CATEGORIA,
         MENGE     TYPE ZMMT0102-MENGE,
         MOD,
         CELLTAB   TYPE LVC_T_STYL,
       END OF TY_SAIDA.

DATA: IT_SAIDA    TYPE TABLE OF TY_SAIDA,
      WA_SAIDA    TYPE TY_SAIDA,
      IT_ZMMT0102 TYPE TABLE OF ZMMT0102,
      WA_ZMMT0102 TYPE ZMMT0102,
      WA_COPY     TYPE TY_SAIDA.

DATA: TELA(4)      TYPE C VALUE '0101',
      V_PEDIDO(10) TYPE C VALUE '9999999999'.

CLASS:  LCL_ALV_TOOLBAR    DEFINITION DEFERRED.
DATA: G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_GRID               TYPE REF TO CL_GUI_ALV_GRID,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      IT_FIELDCATALOG      TYPE LVC_T_FCAT,
      WA_FIELDCATALOG      TYPE LVC_S_FCAT,
      TL_FUNCTION          TYPE UI_FUNCTIONS,
      WL_FUNCTION          LIKE TL_FUNCTION WITH HEADER LINE,
      TY_TOOLBAR           TYPE STB_BUTTON,
      GS_LAYOUT            TYPE LVC_S_LAYO,
      GS_VARIANT           TYPE DISVARIANT,
      GT_ESTILO            TYPE LVC_T_STYL,
      WA_STABLE            TYPE LVC_S_STBL VALUE 'XX'.

DATA: TG_SELECTEDROW TYPE LVC_T_ROW,
      WG_SELECTEDROW TYPE LVC_S_ROW.


SELECTION-SCREEN BEGIN OF SCREEN 0101 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK B1.
SELECT-OPTIONS: P_EBELN  FOR ZMMT0102-EBELN,
                P_MATNR  FOR ZMMT0102-MATNR,
                P_CHARG  FOR ZMMT0102-CHARG.
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN END OF SCREEN 0101.


INITIALIZATION.

CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM ,

      ON_F4  FOR EVENT ONF4   OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME  ES_ROW_NO   ER_EVENT_DATA  ET_BAD_CELLS E_DISPLAY.

ENDCLASS.

CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.

    METHODS: CONSTRUCTOR
      IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,

      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_INTERACTIVE E_OBJECT SENDER,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.

ENDCLASS.

CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD ON_DATA_CHANGED.

    DATA: VMATNR TYPE MSEG-MATNR.


    LOOP AT ER_DATA_CHANGED->MT_MOD_CELLS INTO DATA(WA_MOD_CELLS)
        WHERE FIELDNAME = 'EBELN' OR FIELDNAME = 'EBELP'
         OR   FIELDNAME = 'MBLNR' OR FIELDNAME = 'MATNR'.

      LOOP AT IT_SAIDA INTO WA_SAIDA.

        CHECK WA_MOD_CELLS-ROW_ID EQ SY-TABIX.

        CASE WA_MOD_CELLS-FIELDNAME.
          WHEN 'EBELN'.

            IF WA_MOD_CELLS-VALUE NE V_PEDIDO.

              SELECT SINGLE * FROM EKPO INTO @DATA(WA_EKPO)
                WHERE EBELN EQ @WA_MOD_CELLS-VALUE.

              IF SY-SUBRC = 0.
                MOVE  WA_MOD_CELLS-VALUE TO WA_SAIDA-EBELN.
                MODIFY IT_SAIDA FROM WA_SAIDA INDEX WA_MOD_CELLS-ROW_ID.
              ELSE.
                MESSAGE 'Pedido informado não existe!' TYPE 'I'.
                EXIT.
              ENDIF.

            ELSE.
              MOVE  WA_MOD_CELLS-VALUE TO WA_SAIDA-EBELN.
              WA_SAIDA-EBELP = '00001'.
              MODIFY IT_SAIDA FROM WA_SAIDA INDEX WA_MOD_CELLS-ROW_ID.
            ENDIF.

          WHEN 'EBELP'.
            IF WA_SAIDA-EBELN NE V_PEDIDO.

              SELECT * FROM EKPO INTO TABLE @DATA(IT_EKPO)
              WHERE EBELN EQ @WA_SAIDA-EBELN
                AND EBELP EQ @WA_MOD_CELLS-VALUE.

              READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_SAIDA-EBELN
                                                       EBELP = WA_MOD_CELLS-VALUE.
              IF SY-SUBRC = 0.
                MOVE WA_EKPO-EBELP    TO WA_SAIDA-EBELP.
                MOVE WA_EKPO-LGORT    TO WA_SAIDA-LGORT.
                MOVE WA_EKPO-MATNR    TO WA_SAIDA-MATNR.
                MOVE WA_EKPO-WERKS    TO WA_SAIDA-WERKS.
                MODIFY IT_SAIDA FROM WA_SAIDA INDEX WA_MOD_CELLS-ROW_ID.
              ELSE.
                MESSAGE 'Item do Pedido informado não existe!' TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.


          WHEN 'MBLNR'.

            IF WA_SAIDA-EBELN NE V_PEDIDO.

              IF WA_SAIDA-EBELN IS INITIAL AND WA_SAIDA-EBELP IS INITIAL.
                MESSAGE 'Favor informe o numero do pedido e item !' TYPE 'S'.
                EXIT.
              ELSE.

                SELECT SINGLE * FROM EKBE INTO  @DATA(WA_EKBE)
                  WHERE EBELN EQ @WA_SAIDA-EBELN
                  AND   EBELP EQ @WA_SAIDA-EBELP
                  AND   VGABE EQ '1'.

                IF SY-SUBRC = 0.
                  MOVE: WA_EKBE-BELNR   TO WA_SAIDA-MBLNR.
                  MOVE: WA_EKBE-GJAHR   TO WA_SAIDA-MJAHR.
                  MODIFY IT_SAIDA FROM WA_SAIDA INDEX WA_MOD_CELLS-ROW_ID.
                ELSE.
                  MESSAGE 'Migo não existe para pedido informado!'  TYPE 'S'.
                  EXIT.
                ENDIF.

                SELECT SINGLE * FROM MSEG INTO @DATA(WA_MSEG)
                  WHERE MBLNR EQ @WA_SAIDA-MBLNR
                  AND   MJAHR EQ @WA_SAIDA-MJAHR.

                IF SY-SUBRC <> 0.
                  MESSAGE 'Movimento de mercadoria não existe!'  TYPE 'S'.
                  EXIT.
                ENDIF.
              ENDIF.

            ELSE.

              SELECT SINGLE * FROM MSEG INTO WA_MSEG
                WHERE MBLNR EQ WA_MOD_CELLS-VALUE.

              IF SY-SUBRC = 0.
                MOVE: WA_MSEG-MBLNR TO WA_SAIDA-MBLNR,
                      WA_MSEG-MJAHR TO WA_SAIDA-MJAHR.
                MODIFY IT_SAIDA FROM WA_SAIDA INDEX WA_MOD_CELLS-ROW_ID.
              ELSE.
                MESSAGE 'Movimento de mercadoria não existe!'  TYPE 'S'.
                EXIT.
              ENDIF.
            ENDIF.

          WHEN 'MATNR'.

            CLEAR VMATNR.

            IF WA_SAIDA-EBELN EQ V_PEDIDO.

              VMATNR = WA_MOD_CELLS-VALUE.
              VMATNR = |{ VMATNR ALPHA = IN  }|.

              SELECT SINGLE * FROM MSEG INTO WA_MSEG
               WHERE  MBLNR EQ WA_SAIDA-MBLNR
                 AND  MJAHR EQ WA_SAIDA-MJAHR
                 AND  MATNR EQ VMATNR.

              IF SY-SUBRC = 0.
                IF WA_MSEG-BWART NE '101'.
                  MESSAGE 'Doc. de Material informado não é de entrada!'  TYPE 'S'.
                  EXIT.
                ELSE.
                  MOVE: WA_MSEG-MATNR TO WA_SAIDA-MATNR,
                        WA_MSEG-WERKS TO WA_SAIDA-WERKS,
                        WA_MSEG-LGORT TO WA_SAIDA-LGORT.
                  MODIFY IT_SAIDA FROM WA_SAIDA INDEX WA_MOD_CELLS-ROW_ID.
                ENDIF.
              ELSE.
                MESSAGE 'Material informado não existe para esse movimento mercadoria!'  TYPE 'S'.
                EXIT.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.


  METHOD ON_F4.

    TYPES: BEGIN OF TY_FIELD,
             TABNAME   TYPE DD03L-TABNAME,
             FIELDNAME TYPE  DD03L-FIELDNAME,
             S(1)      TYPE C,
           END OF TY_FIELD.

    TYPES: BEGIN OF TY_VALUE,
             TABNAME     TYPE DD03L-TABNAME,    "Nome da tabela
             FIELDNAME   TYPE DD03L-FIELDNAME,    "Nome de campo
             CHAR79(100) TYPE C,
           END OF TY_VALUE.

    DATA: BEGIN OF WL_DOCUMENTO,
            FIELD(50),
          END OF  WL_DOCUMENTO.

    DATA: BEGIN OF WL_PEDIDO,
            FIELD(50),
          END OF  WL_PEDIDO.



    DATA: TL_DOCUMENTO LIKE TABLE OF WL_DOCUMENTO,
          TL_PEDIDO    LIKE TABLE OF WL_PEDIDO,
          TL_FIELD     TYPE TABLE OF TY_FIELD,
          WL_FIELD     TYPE TY_FIELD,
          TL_VALUE     TYPE TABLE OF TY_VALUE,
          WL_VALUE     TYPE TY_VALUE,
          WL_CHAR(20),
          WL_INDEX     TYPE SY-TABIX.


    IF E_FIELDNAME = 'MBLNR'.

      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX ES_ROW_NO-ROW_ID.

      IF WA_SAIDA-EBELN IS INITIAL AND WA_SAIDA-EBELP IS INITIAL.
        MESSAGE 'Favor informe o numero do pedido e item !' TYPE 'S'.
        EXIT.
      ELSE.

        SELECT * FROM EKBE INTO TABLE @DATA(IT_EKBE)
          WHERE EBELN EQ @WA_SAIDA-EBELN
          AND   EBELP EQ @WA_SAIDA-EBELP
          AND   VGABE EQ '1'.


        LOOP AT IT_EKBE INTO DATA(WA_EKBE).

          MOVE: WA_EKBE-BELNR TO WL_DOCUMENTO-FIELD.
          APPEND WL_DOCUMENTO TO TL_DOCUMENTO.

          MOVE: WA_EKBE-GJAHR TO WL_DOCUMENTO-FIELD.
          APPEND WL_DOCUMENTO TO TL_DOCUMENTO.

        ENDLOOP.

        WL_FIELD-TABNAME   = 'EKBE'.
        WL_FIELD-FIELDNAME = 'BELNR'.
        WL_FIELD-S = 'X'.
        APPEND WL_FIELD TO TL_FIELD.

        WL_FIELD-TABNAME   = 'EKBE'.
        WL_FIELD-FIELDNAME = 'GJAHR'.
        WL_FIELD-S = ' '.
        APPEND WL_FIELD TO TL_FIELD.

        CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
          EXPORTING
            FIELDNAME                 = 'BELNR'
            TABNAME                   = 'EKBE'
          IMPORTING
            INDEX                     = WL_INDEX
            SELECT_VALUE              = WL_CHAR
          TABLES
            FIELDS                    = TL_FIELD
            SELECT_VALUES             = TL_VALUE
            VALUETAB                  = TL_DOCUMENTO
          EXCEPTIONS
            FIELD_NOT_IN_DDIC         = 001
            MORE_THEN_ONE_SELECTFIELD = 002
            NO_SELECTFIELD            = 003.


        IF SY-SUBRC IS INITIAL.

          READ TABLE IT_EKBE INTO WA_EKBE INDEX WL_INDEX.
          IF ES_ROW_NO-ROW_ID GT 0.

            READ TABLE IT_SAIDA INTO WA_SAIDA INDEX ES_ROW_NO-ROW_ID.
            IF SY-SUBRC IS INITIAL.

              MOVE: WA_EKBE-BELNR   TO WA_SAIDA-MBLNR.
              MOVE: WA_EKBE-GJAHR   TO WA_SAIDA-MJAHR.
              MODIFY IT_SAIDA FROM WA_SAIDA INDEX ES_ROW_NO-ROW_ID.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.
ENDCLASS.


CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.

  METHOD CONSTRUCTOR.

    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.

  ENDMETHOD.

  METHOD ON_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_INSERT_ROW.
    TY_TOOLBAR-FUNCTION  = 'ADD'.
    TY_TOOLBAR-TEXT      = 'Inserir'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = 'DEL'.
    TY_TOOLBAR-TEXT      = 'Excluir'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_CHANGE.
    TY_TOOLBAR-FUNCTION  = 'EDIT'.
    TY_TOOLBAR-TEXT      = 'Modificar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-ICON      = ICON_SYSTEM_COPY.
    TY_TOOLBAR-FUNCTION  = 'COPY'.
    TY_TOOLBAR-TEXT      = 'Copiar'.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.

  ENDMETHOD.

  METHOD HANDLE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN 'ADD'.
        CLEAR WA_SAIDA.

        WA_SAIDA-MOD = 'X'.
        CLEAR WA_SAIDA-CELLTAB.
        GT_ESTILO = VALUE #(  ( FIELDNAME = 'EBELN'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                              ( FIELDNAME = 'EBELP'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                              ( FIELDNAME = 'MBLNR'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                              ( FIELDNAME = 'MATNR'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                              ( FIELDNAME = 'CHARG'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                              ( FIELDNAME = 'NR_FASE'   STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                              ( FIELDNAME = 'CATEGORIA' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                              ( FIELDNAME = 'MENGE'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED ) ).
        INSERT LINES OF GT_ESTILO INTO TABLE WA_SAIDA-CELLTAB.

        APPEND WA_SAIDA TO IT_SAIDA.

        "APPEND INITIAL LINE TO IT_SAIDA.
      WHEN 'DEL'.

        CALL METHOD G_GRID->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TG_SELECTEDROW.

        IF TG_SELECTEDROW IS INITIAL.
          MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
          EXIT.
        ELSE.
          LOOP AT TG_SELECTEDROW INTO WG_SELECTEDROW.
            READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WG_SELECTEDROW-INDEX.

            DELETE FROM ZMMT0102 WHERE EBELN   EQ  WA_SAIDA-EBELN
                                   AND EBELP   EQ WA_SAIDA-EBELP
                                   AND MBLNR   EQ WA_SAIDA-MBLNR
                                   AND MJAHR   EQ WA_SAIDA-MJAHR
                                   AND CHARG   EQ WA_SAIDA-CHARG
                                   AND NR_FASE EQ WA_SAIDA-NR_FASE.
          ENDLOOP.

          CLEAR WA_SAIDA.
          PERFORM Z_BUSCA_DADOS.
        ENDIF.

      WHEN 'EDIT'.

        DATA WA_MODIF TYPE TY_SAIDA.

        CALL METHOD G_GRID->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TG_SELECTEDROW.

        IF TG_SELECTEDROW IS INITIAL.
          MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
          EXIT.
        ELSE.
          CLEAR WA_MODIF.

          READ TABLE TG_SELECTEDROW INTO WG_SELECTEDROW INDEX 1.

          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WG_SELECTEDROW-INDEX.

          MOVE WA_SAIDA TO WA_MODIF.
          WA_MODIF-MOD = 'X'.

          CLEAR WA_MODIF-CELLTAB.
          GT_ESTILO = VALUE #(  ( FIELDNAME = 'CATEGORIA' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED )
                                ( FIELDNAME = 'MENGE'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED ) ).

          INSERT LINES OF GT_ESTILO INTO TABLE WA_MODIF-CELLTAB.
          MODIFY IT_SAIDA FROM WA_MODIF INDEX WG_SELECTEDROW-INDEX.
        ENDIF.

      WHEN 'COPY'.

        CALL METHOD G_GRID->GET_SELECTED_ROWS
          IMPORTING
            ET_INDEX_ROWS = TG_SELECTEDROW.


        IF TG_SELECTEDROW IS INITIAL.
          MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
          EXIT.
        ELSE.
          READ TABLE TG_SELECTEDROW INTO WG_SELECTEDROW INDEX 1.

          READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WG_SELECTEDROW-INDEX.

          MOVE WA_SAIDA TO WA_COPY.
          WA_COPY-MOD = 'X'.

          APPEND WA_COPY TO IT_SAIDA.

          CLEAR: WA_COPY, WA_SAIDA.
        ENDIF.
    ENDCASE.

    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.

  CALL SCREEN 0100.

FORM Z_BUSCA_DADOS.

  REFRESH :  IT_SAIDA, IT_ZMMT0102.

  SELECT *
    FROM ZMMT0102 INTO TABLE IT_ZMMT0102
   WHERE EBELN IN P_EBELN
    AND  MATNR IN P_MATNR
    AND  CHARG IN P_CHARG
    AND  TCODE EQ SY-TCODE.

  CHECK IT_ZMMT0102 IS NOT INITIAL.

  LOOP AT IT_ZMMT0102 INTO WA_ZMMT0102.

    WA_SAIDA-EBELN       =  WA_ZMMT0102-EBELN.
    WA_SAIDA-EBELP       =  WA_ZMMT0102-EBELP.
    WA_SAIDA-MBLNR       =  WA_ZMMT0102-MBLNR.
    WA_SAIDA-MJAHR       =  WA_ZMMT0102-MJAHR.
    WA_SAIDA-CHARG       =  WA_ZMMT0102-CHARG.
    WA_SAIDA-NR_FASE     =  WA_ZMMT0102-NR_FASE.
    WA_SAIDA-MATNR       =  WA_ZMMT0102-MATNR.
    WA_SAIDA-WERKS       =  WA_ZMMT0102-WERKS.
    WA_SAIDA-LGORT       =  WA_ZMMT0102-LGORT.
    WA_SAIDA-CATEGORIA   =  WA_ZMMT0102-CATEGORIA.
    WA_SAIDA-MENGE       =  WA_ZMMT0102-MENGE.

    CLEAR WA_SAIDA-CELLTAB.
    GT_ESTILO = VALUE #(  ( FIELDNAME = 'EBELN'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                          ( FIELDNAME = 'EBELP'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                          ( FIELDNAME = 'MBLNR'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                          ( FIELDNAME = 'MATNR'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                          ( FIELDNAME = 'CHARG'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                          ( FIELDNAME = 'NR_FASE'   STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                          ( FIELDNAME = 'CATEGORIA' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED )
                          ( FIELDNAME = 'MENGE'     STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED ) ).
    INSERT LINES OF GT_ESTILO INTO TABLE WA_SAIDA-CELLTAB.

    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR: WA_SAIDA, WA_ZMMT0102.

  ENDLOOP.

ENDFORM.

FORM Z_CRIA_ALV.

  REFRESH IT_FIELDCATALOG.
  PERFORM PREENCHE_CAT USING:
        'EBELN'         'Pedido'           '10'     ''     ''     ''     ''   ''      ''            ''            ' ',
        'EBELP'         'Item'             '06'     ''     ''     ''     ''   ''      ''            ''            ' ',
        'MBLNR'         'Nº documento'     '13'     ''     ''     ''     ''   ''      ''            ''            'X',
        'MJAHR'         'Ano'              '04'     ''     ''     ''     ''   ''      ''            ''            ' ',
        'CHARG'         'Lote'             '10'     ''     ''     ''     ''   ''      ''            ''            ' ',
        'MATNR'         'Material'         '18'     'X'    ''     ''     ''   ''      ''            ''            ' ',
        'WERKS'         'Filial'           '07'     ''     ''     ''     ''   ''      ''            ''            ' ',
        'LGORT'         'Deposito'         '10'     ''     ''     ''     ''   ''      ''            ''            ' ',
        'NR_FASE'       'Nº Fase'          '20'     ''     ''     ''     ''   ''      ''            ''            ' ',
        'CATEGORIA'     'Categoria'        '08'     ''     ''     ''     ''   ''      'ZMMT0102'    'CATEGORIA'   ' ',
        'MENGE '        'Quantidade'       '13'     ''     ''     ''     ''   ''      ''            ''            ' '.

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

  WA_FIELDCATALOG-FIELDNAME   = P_CAMPO.
  WA_FIELDCATALOG-COLTEXT     = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_L   = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_M   = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_S   = P_DESC.
  WA_FIELDCATALOG-OUTPUTLEN   = P_TAM.
  WA_FIELDCATALOG-HOTSPOT     = P_HOT.
  WA_FIELDCATALOG-NO_ZERO     = P_ZERO.
  WA_FIELDCATALOG-DO_SUM      = P_SUM.
  WA_FIELDCATALOG-JUST        = P_JUST.
  WA_FIELDCATALOG-EDIT        = P_EDIT.
  WA_FIELDCATALOG-REF_TABLE   = P_TABLE.
  WA_FIELDCATALOG-REF_FIELD   = P_FIELDNAME.
  WA_FIELDCATALOG-F4AVAILABL  = P_F4.

  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: LT_F401 TYPE LVC_T_F4,
        WL_F401 TYPE LVC_S_F4.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM Z_CRIA_ALV.

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

    GS_LAYOUT-STYLEFNAME = 'CELLTAB'.

    WL_F401-FIELDNAME  = 'MBLNR'.
    WL_F401-REGISTER   = 'X'.
    WL_F401-GETBEFORE  = 'X'.
    APPEND WL_F401 TO  LT_F401.
    CLEAR WL_F401.


    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = G_GRID.

    SET HANDLER: OBG_TOOLBAR->ON_TOOLBAR FOR G_GRID,
                 OBG_TOOLBAR->HANDLE_USER_COMMAND FOR G_GRID.


    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT           = GS_VARIANT
        IS_LAYOUT            = GS_LAYOUT
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
      CHANGING
        IT_OUTTAB            = IT_SAIDA
        IT_FIELDCATALOG      = IT_FIELDCATALOG.


    SET HANDLER: LCL_EVENT_HANDLER=>ON_F4 FOR G_GRID,
                 LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR G_GRID.


    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD G_GRID->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F401[].

  ELSE.

    CALL METHOD G_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = IT_FIELDCATALOG.

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
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM Z_SAVE.
    WHEN '&PESQ'.
      PERFORM Z_BUSCA_DADOS.
  ENDCASE.
ENDMODULE.

FORM Z_SAVE.
  DATA WA_SALVAR TYPE ZMMT0102.
  DATA VERRO.

  LOOP AT IT_SAIDA INTO WA_SAIDA
    WHERE MOD = 'X'.

    IF WA_SAIDA-EBELN IS INITIAL.
      VERRO = ABAP_TRUE.
      MESSAGE 'Favor informar o numero do Pedido!' TYPE 'S'.
      EXIT.
    ELSEIF WA_SAIDA-EBELP IS INITIAL.
      VERRO = ABAP_TRUE.
      MESSAGE 'Favor informar o item do Pedido!' TYPE 'S'.
      EXIT.
    ELSEIF WA_SAIDA-MBLNR IS INITIAL.
      VERRO = ABAP_TRUE.
      MESSAGE 'Favor informar Nº Documento !'  TYPE 'S'.
      EXIT.
    ELSEIF WA_SAIDA-CHARG IS INITIAL.
      VERRO = ABAP_TRUE.
      MESSAGE 'Favor informar numero do Lote !'  TYPE 'S'.
      EXIT.
    ELSEIF WA_SAIDA-NR_FASE IS INITIAL.
      VERRO = ABAP_TRUE.
      MESSAGE 'Favor informar numero Fase !'  TYPE 'S'.
      EXIT.
    ELSEIF WA_SAIDA-CATEGORIA IS INITIAL.
      VERRO = ABAP_TRUE.
      MESSAGE 'Favor informar Categoria !'  TYPE 'S'.
      EXIT.
    ELSEIF WA_SAIDA-MENGE IS INITIAL .
      VERRO = ABAP_TRUE.
      MESSAGE 'Favor informar Quantidade !'  TYPE 'S'.
      EXIT.
    ELSE.
      WA_SALVAR-MANDT       = SY-MANDT.
      WA_SALVAR-EBELN       = WA_SAIDA-EBELN.
      WA_SALVAR-EBELP       = WA_SAIDA-EBELP.
      WA_SALVAR-MBLNR       = WA_SAIDA-MBLNR.
      WA_SALVAR-MJAHR       = WA_SAIDA-MJAHR.
      WA_SALVAR-CHARG       = WA_SAIDA-CHARG.
      WA_SALVAR-NR_FASE     = WA_SAIDA-NR_FASE.
      WA_SALVAR-MATNR       = WA_SAIDA-MATNR.
      WA_SALVAR-WERKS       = WA_SAIDA-WERKS.
      WA_SALVAR-LGORT       = WA_SAIDA-LGORT.
      WA_SALVAR-CATEGORIA   = WA_SAIDA-CATEGORIA.
      WA_SALVAR-MENGE       = WA_SAIDA-MENGE.
      WA_SALVAR-TCODE       = SY-TCODE.

      MODIFY ZMMT0102 FROM WA_SALVAR.
      CLEAR: WA_SALVAR, WA_SAIDA.

    ENDIF.
  ENDLOOP.

  IF  VERRO = ABAP_FALSE.
    PERFORM Z_BUSCA_DADOS.
  ENDIF.
ENDFORM.
