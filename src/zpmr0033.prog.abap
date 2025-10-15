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
**|    + Cleudo Ferreira( cleudo.ferreira@amaggi.com.br )                     |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Cadastro de Materias SAAF                                                 |*
**/===========================================================================\*

REPORT ZPMR0033 MESSAGE-ID ZPMMSG.

TYPES: BEGIN OF TY_004.
        INCLUDE TYPE ZPMT004.
TYPES: MAKTX       TYPE MAKTX,
       IWERK       TYPE IWERK,
       ID_DEPOSITO TYPE CHAR10,
       ID_TQ       TYPE CHAR10,
       MARC        TYPE C.
TYPES END OF TY_004.
*
*TYPES: BEGIN OF TY_006,
*         MATNR   TYPE MSEG-MATNR,
*         STATUS  TYPE C,
*         ERNAM   TYPE AUFERFNAM,
*         ERDAT   TYPE AUFERFDAT,
*         ERFZEIT TYPE CO_INS_TIME,
*         AENAM   TYPE AUFAENAM,
*         WERKS   TYPE IWERK,
*         ID_TQ   TYPE P DECIMALS 1,
*       END OF TY_006.

DATA: WA_ZPMT004     TYPE TY_004.
DATA: _ZPMT004       TYPE ZPMT004.
DATA: _ZPMT006       TYPE ZPMT006.
DATA: IT_SELECT_ROWS TYPE LVC_T_ROW.
DATA: WA_SELECT_ROWS TYPE LVC_S_ROW.
DATA: WA_FCAT        TYPE LVC_S_FCAT.


DATA: IT_ZPMT004 TYPE TABLE OF TY_004.
DATA: IT_ZPMT006 TYPE TABLE OF TY_004.

DATA:
  OBJ_ALV  TYPE REF TO CL_GUI_ALV_GRID,
  IT_EXC   TYPE UI_FUNCTIONS,
  VARIANT  TYPE DISVARIANT,
  LAYOUT   TYPE LVC_S_LAYO,
  ESTILO   TYPE LVC_T_STYL,
  STABLE   TYPE LVC_S_STBL,
  OBJ_CONT TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  IT_FCAT  TYPE LVC_T_FCAT,
  STR      TYPE REF TO DATA.


CLASS ZCL_EVENTS DEFINITION.
  PUBLIC SECTION.
    METHODS: HANDLE_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN SENDER.
ENDCLASS.

DATA OBJ_EVENTS TYPE REF TO ZCL_EVENTS.

CLASS ZCL_MAIN DEFINITION.

  PUBLIC SECTION.

    METHODS:
      SCREEN,
      GET_DADOS,
      CREATE_ALV,
      CHECK_MATNR IMPORTING INPUT TYPE MATNR,
      GET_MAKTX IMPORTING INPUT         TYPE MATNR
                RETURNING VALUE(RETURN) TYPE MAKTX,
      ADD,
      DEL,
      SET_LAYOUT.

ENDCLASS.

DATA(OBJ_MAIN) = NEW ZCL_MAIN( ).

CLASS ZCL_EVENTS IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.

    CASE E_COLUMN-FIELDNAME.
      WHEN 'MATNR'.

        CLEAR _ZPMT004.

        TRY.
            READ TABLE IT_ZPMT004 INTO WA_ZPMT004 INDEX E_ROW.
            _ZPMT004-MATNR = WA_ZPMT004-MATNR."IT_ZPMT004-MATNR[ E_ROW-INDEX ].
            OBJ_MAIN->CHECK_MATNR( _ZPMT004-MATNR ).
            MESSAGE |Para Deletar o Material { _ZPMT004-MATNR } Clique em Remover! | TYPE 'I'.
          CATCH CX_SY_ITAB_LINE_NOT_FOUND.
        ENDTRY.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.

CLASS ZCL_MAIN IMPLEMENTATION.

  METHOD SCREEN.
    ME->GET_DADOS( ).
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD GET_DADOS.
*    SELECT A~MATNR B~MAKTX A~STATUS A~ERNAM A~ERDAT A~ERFZEIT A~AENAM
*      FROM ZPMT004 AS A
*      INNER JOIN MAKT AS B ON B~MATNR EQ A~MATNR
*      INTO CORRESPONDING FIELDS OF TABLE IT_ZPMT004
*      WHERE SPRAS EQ SY-LANGU.

    SELECT A~MATNR B~MAKTX A~ERNAM A~ERDAT A~ERFZEIT A~AENAM A~IWERK A~ID_DEPOSITO A~ID_TQ
    FROM ZPMT006 AS A
        INNER JOIN MAKT AS B ON B~MATNR EQ A~MATNR
        INTO CORRESPONDING FIELDS OF TABLE IT_ZPMT004
        WHERE SPRAS EQ SY-LANGU.

    LOOP AT IT_ZPMT004 ASSIGNING FIELD-SYMBOL(<I_ZPMT004>).
      <I_ZPMT004>-MATNR = |{ <I_ZPMT004>-MATNR ALPHA = OUT }|.
      <I_ZPMT004>-ID_TQ = |{ <I_ZPMT004>-ID_TQ ALPHA = OUT }|.
      <I_ZPMT004>-ID_DEPOSITO = |{ <I_ZPMT004>-ID_DEPOSITO ALPHA = OUT }|.

    ENDLOOP.



  ENDMETHOD.

  METHOD CREATE_ALV.
    FREE IT_FCAT.

    ASSIGN 'TY_004' TO FIELD-SYMBOL(<FS_STR>).
    CREATE DATA STR TYPE (<FS_STR>).

*    IT_FCAT = CORRESPONDING LVC_T_FCAT( CL_SALV_DATA_DESCR=>READ_STRUCTDESCR( CAST CL_ABAP_STRUCTDESCR( CL_ABAP_STRUCTDESCR=>DESCRIBE_BY_DATA_REF( STR ) ) ) ).


*    LOOP AT IT_FCAT ASSIGNING FIELD-SYMBOL(<FCAT>).
*      CASE <FCAT>-FIELDNAME.
*        WHEN 'SEQ'.
*          <FCAT>-F4AVAILABL = ABAP_FALSE.
*      ENDCASE.
*    ENDLOOP.

    PERFORM FCAT.

    IF OBJ_CONT IS INITIAL.

      CREATE OBJECT OBJ_CONT
        EXPORTING
          CONTAINER_NAME = 'CC_CONTAINER'.

      CREATE OBJECT OBJ_ALV
        EXPORTING
          I_SHELLSTYLE    = 0
          I_PARENT        = OBJ_CONT
          I_APPL_EVENTS   = ABAP_FALSE
          I_FCAT_COMPLETE = ABAP_FALSE.

      LAYOUT = VALUE #( SEL_MODE = 'A'
                      CWIDTH_OPT = ABAP_TRUE
                           ZEBRA = ABAP_TRUE
                      NO_ROWINS  = ABAP_TRUE ).

      VARIANT = VALUE #( REPORT = SY-REPID ).
      STABLE  = VALUE #( ROW = ABAP_TRUE COL = ABAP_TRUE ).
      IT_EXC  = VALUE #( ( CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL ) ).

      CREATE OBJECT OBJ_EVENTS.

      SET HANDLER: OBJ_EVENTS->HANDLE_DOUBLE_CLICK FOR OBJ_ALV.

      OBJ_ALV->SET_TABLE_FOR_FIRST_DISPLAY(
        EXPORTING
          IS_LAYOUT                     = LAYOUT
          IS_VARIANT                    = VARIANT
          I_SAVE                        = 'A'
          IT_TOOLBAR_EXCLUDING          = IT_EXC
        CHANGING
          IT_OUTTAB                     = IT_ZPMT004
          IT_FIELDCATALOG               = IT_FCAT[]
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4 ).

    ELSE.
      OBJ_ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = STABLE ).
    ENDIF.


  ENDMETHOD.

  METHOD CHECK_MATNR.
    SELECT COUNT(*) FROM MARA WHERE MATNR EQ INPUT.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE 'Material não existe!' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      ME->GET_MAKTX( INPUT ).
    ENDIF.
  ENDMETHOD.

  METHOD GET_MAKTX.
    CLEAR WA_ZPMT004-MAKTX.
    SELECT SINGLE MAKTX FROM MAKT INTO WA_ZPMT004-MAKTX WHERE MATNR EQ INPUT.
  ENDMETHOD.

  METHOD ADD.

    SELECT *
    FROM ZPMT006
    INTO TABLE @DATA(T_ZPMT004)
      WHERE MATNR EQ @WA_ZPMT004-MATNR.

    IF T_ZPMT004 IS NOT INITIAL.
      MESSAGE TEXT-001 TYPE 'I'.
    ELSE.
*      MOVE WA_ZPMT004 TO _ZPMT004.
      _ZPMT004-MATNR   = WA_ZPMT004-MATNR.
      _ZPMT004-ERNAM   = SY-UNAME.
      _ZPMT004-ERDAT   = SY-DATUM.
      _ZPMT004-ERFZEIT = SY-UZEIT.

      _ZPMT006-MATNR   = WA_ZPMT004-MATNR.
      _ZPMT006-ERNAM   = SY-UNAME.
      _ZPMT006-ERDAT   = SY-DATUM.
      _ZPMT006-ERFZEIT = SY-UZEIT.
      _ZPMT006-IWERK   = WA_ZPMT004-IWERK.
      _ZPMT006-ID_DEPOSITO   = WA_ZPMT004-ID_DEPOSITO.
      _ZPMT006-ID_TQ   = WA_ZPMT004-ID_TQ.
      MODIFY ZPMT004 FROM _ZPMT004.
      MODIFY ZPMT006 FROM _ZPMT006.
      CLEAR: WA_ZPMT004, _ZPMT004, _ZPMT006.
    ENDIF.
  ENDMETHOD.

  METHOD DEL.

    DATA: P_RESP.
    DATA: LV_MSG TYPE BAPI_MSG.

    CLEAR: IT_SELECT_ROWS[], WA_SELECT_ROWS. "W_OPERA.

    IF IT_ZPMT004 IS NOT INITIAL.
      CALL METHOD OBJ_ALV->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECT_ROWS.


      IF IT_SELECT_ROWS[] IS NOT INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING        "TITLEBAR = 'Confirmar'
            TEXT_QUESTION         = 'Deseja realmente excluir a linha?'
            TEXT_BUTTON_1         = 'Sim'
            TEXT_BUTTON_2         = 'Não'
            DISPLAY_CANCEL_BUTTON = ' '
          IMPORTING
            ANSWER                = P_RESP.

        IF P_RESP = '1'.

          LOOP AT IT_SELECT_ROWS INTO WA_SELECT_ROWS.
            LOOP AT IT_ZPMT004 ASSIGNING FIELD-SYMBOL(<W_ZPMT004>).
              IF SY-TABIX = WA_SELECT_ROWS-INDEX.
                <W_ZPMT004>-MARC = 'X'.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

          DELETE IT_ZPMT004 WHERE MARC NE 'X'.

          LOOP AT IT_ZPMT004 INTO _ZPMT004.

            _ZPMT004-MATNR = |{ _ZPMT004-MATNR ALPHA = IN }|.
            DELETE FROM ZPMT004 WHERE MATNR = _ZPMT004-MATNR
                                AND ERNAM   = _ZPMT004-ERNAM
                                AND ERDAT   = _ZPMT004-ERDAT
                                AND ERFZEIT = _ZPMT004-ERFZEIT.


            DELETE FROM ZPMT006 WHERE MATNR = _ZPMT004-MATNR
                                AND ERNAM   = _ZPMT004-ERNAM
                                AND ERDAT   = _ZPMT004-ERDAT
                                AND ERFZEIT = _ZPMT004-ERFZEIT.


            CLEAR: WA_ZPMT004, _ZPMT004.
          ENDLOOP.

          IF SY-SUBRC = 0.
            MESSAGE S000(O0) WITH 'Informação excluida com sucesso' DISPLAY LIKE 'S'.
          ENDIF.

          OBJ_ALV->REFRESH_TABLE_DISPLAY( EXPORTING IS_STABLE = STABLE ).

        ENDIF.

      ELSE.
        MESSAGE I026(SV)." WITH 'Selecione uma linha para excluir'.
      ENDIF.
    ENDIF.
*
*    CHECK _ZPMT004-MATNR IS NOT INITIAL.
*    DELETE FROM ZPMT004 WHERE MATNR EQ _ZPMT004-MATNR.
*    CLEAR: WA_ZPMT004, _ZPMT004.

  ENDMETHOD.

  METHOD SET_LAYOUT.
    FREE: LAYOUT, VARIANT, STABLE, IT_EXC.
    LAYOUT  = VALUE #( ZEBRA = ABAP_TRUE NO_ROWINS  = ABAP_TRUE CWIDTH_OPT = ABAP_TRUE ).
    VARIANT = VALUE #( REPORT = SY-REPID HANDLE = '0100' ).
    STABLE  = VALUE #( ROW = ABAP_TRUE COL = ABAP_TRUE ).
    IT_EXC  = VALUE #( ( CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL ) ).
  ENDMETHOD.

ENDCLASS.

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PBO100'.
  SET TITLEBAR 'PAI100'.
  OBJ_MAIN->CREATE_ALV( ).
ENDMODULE.

MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN  0.
    WHEN 'ADD'.
      OBJ_MAIN->ADD( ).
    WHEN 'DEL'.
      OBJ_MAIN->DEL( ).
  ENDCASE.

  OBJ_MAIN->GET_DADOS( ).

ENDMODULE.

MODULE CHECK_MATNR INPUT.
  OBJ_MAIN->CHECK_MATNR( WA_ZPMT004-MATNR ).
ENDMODULE.

START-OF-SELECTION.
  OBJ_MAIN->SCREEN( ).
*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FCAT .
  PERFORM Z_FEED_FIELDCAT USING:
       1  'MATNR   '  'IT_ZPMT004'  '30'   ' '  ' '  ' '  'Nº do material         '  ''  ' '  'ZPMT004'  ' ',
       2  'STATUS  '  'IT_ZPMT004'  '30'   ' '  ' '  ' '  'Status                 '  ''  ' '  'ZPMT004'  ' ',
       3  'ERNAM   '  'IT_ZPMT004'  '15'   ' '  ' '  ' '  'Criado por             '  ''  ' '  'ZPMT004'  ' ',
       4  'ERDAT   '  'IT_ZPMT004'  '30'   ' '  ' '  ' '  'Data de entrada        '  ''  ' '  'ZPMT004'  ' ',
       5  'ERFZEIT '  'IT_ZPMT004'  '10'   ' '  ' '  ' '  'Hora de entrada        '  ''  ' '  'ZPMT004'  ' ',
       6  'IWERK   '  'IT_ZPMT004'  '30'   ' '  ' '  ' '  'Centro                 '  ''  ' '  'ZPMT006'  ' ',
       7  'ID_DEPOSITO   '  'IT_ZPMT004'  '10'   ' '  ' '  ' '  'Deposito         '  ''  ' '  'ZPMT006'  ' ',
       8  'Id_TQ   '  'IT_ZPMT004'  '10'   ' '  ' '  ' '  'Id do Tq               '  ''  ' '  'ZPMT006'  ' '.
*       6  'AENAM   '  'IT_ZPMT004'  '10'   ' '  ' '  ' '  'Última modificação por '  ''  ' '  'ZPMT004'  ' '.
ENDFORM.                    " Z_FILDCAT
*&---------------------------------------------------------------------*
*&      Form  Z_FEED_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0687   text
*      -->P_0688   text
*      -->P_0689   text
*      -->P_0690   text
*      -->P_0691   text
*      -->P_0692   text
*      -->P_0693   text
*      -->P_0694   text
*      -->P_0695   text
*      -->P_0696   text
*      -->P_0697   text
*----------------------------------------------------------------------*
FORM Z_FEED_FIELDCAT  USING       VALUE(P_COLNUM)
                                  VALUE(P_FIELDNAME)
                                  VALUE(P_TABNAME)
                                  VALUE(P_LEN)
                                  VALUE(P_EDIT)
                                  VALUE(P_ICON)
                                  VALUE(P_DO_SUM)
                                  VALUE(P_HEADER)
                                  VALUE(P_EMPHASIZE)
                                  VALUE(P_HOTSPOT)
                                  VALUE(P_REF_TABLE)
                                  VALUE(P_REF_FIELD).


  WA_FCAT-COL_POS     = P_COLNUM.
  WA_FCAT-FIELDNAME   = P_FIELDNAME.
  WA_FCAT-TABNAME     = P_TABNAME.
  WA_FCAT-OUTPUTLEN   = P_LEN.
  WA_FCAT-EDIT        = P_EDIT.
  WA_FCAT-ICON        = P_ICON.
  WA_FCAT-DO_SUM      = P_DO_SUM.
  WA_FCAT-COLTEXT     = P_HEADER.
  WA_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WA_FCAT-HOTSPOT     = P_HOTSPOT.
  WA_FCAT-REF_TABLE   = P_REF_TABLE.
  WA_FCAT-REF_TABLE   = P_REF_FIELD.

*  WA_LAYOUT-EXCP_CONDS    = 'X'.
*  WA_LAYOUT-ZEBRA         = 'X'.
*  WA_LAYOUT-SEL_MODE      = 'A'.
*  WA_LAYOUT-CWIDTH_OPT    = 'X'.     "  Otimizar colunas na tela
*  WA_LAYOUT-TOTALS_BEF    = ' '.

  APPEND WA_FCAT TO IT_FCAT.
ENDFORM.
