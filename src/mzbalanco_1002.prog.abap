*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1002 .
*----------------------------------------------------------------------*

TYPES: TY_LINE(500).

TYPES BEGIN OF TY_SPLIT.
TYPES: VALOR TYPE CHAR50.
TYPES END OF TY_SPLIT.

*---------- Definition -----------------------------------------------*
CLASS LCL_BALANCO_EVENT_1002 DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_ALV_1002
      FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                E_COLUMN_ID
                ES_ROW_NO.
ENDCLASS.                    "lcl_event_est_handler DEFINITION

*---------- Implementation -------------------------------------------*
CLASS LCL_BALANCO_EVENT_1002 IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_ALV_1002.
    PERFORM HOTSPOT_ALV_1002 USING E_ROW_ID  E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "HANDLE_HOTSPOT_ALV_1002
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_ALV_1002
*&---------------------------------------------------------------------*
*       Performe para ações de HiperLink da ALV Tela 1002
*----------------------------------------------------------------------*
FORM HOTSPOT_ALV_1002  USING  E_ROW_ID    TYPE LVC_S_ROW
                              E_COLUMN_ID TYPE LVC_S_COL
                              ES_ROW_NO   TYPE LVC_S_ROID.

  READ TABLE IT_ZGLT046_ALV INDEX ES_ROW_NO-ROW_ID.
  CASE E_COLUMN_ID-FIELDNAME.
    WHEN OK_ESTRUT.
      "Montar Estrutura de Balanço Patrimonial
      TL_1001_01 = TL_1012.
      LEAVE TO SCREEN 1001.
    WHEN OK_EDITAR.
      "Editar Cadastro de Balanço Patrimonial
      CK_EDITAR = ABAP_TRUE.
      MOVE-CORRESPONDING IT_ZGLT046_ALV TO IT_ZGLT046.
      CALL SCREEN 1003 STARTING AT 10 10.
  ENDCASE.
ENDFORM.                    " HOTSPOT_ALV_1002

DATA: BALANCO_CONTAINER_1002 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      BALANCO_ALV_1002       TYPE REF TO CL_GUI_ALV_GRID,
      BALANCO_CATALOGO_1002  TYPE LVC_T_FCAT,
      BALANCO_GS_LAYOUT_1002 TYPE LVC_S_LAYO,
      BALANCO_EVENT_ALV_1002 TYPE REF TO LCL_BALANCO_EVENT_1002,
      WA_SCROLL_COL_1002     TYPE LVC_S_COL,
      WA_SCROLL_ROW_1002     TYPE LVC_S_ROID.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1002 OUTPUT.

  DATA: TEXT_E001 TYPE C LENGTH 50 VALUE 'Versão',
        TEXT_E002 TYPE C LENGTH 50 VALUE 'Desc. da Versão'.

  IF PRIM_BALANCO_ALV EQ ABAP_OFF.

    CLEAR: BALANCO_CATALOGO_1002.

    CREATE OBJECT BALANCO_CONTAINER_1002
      EXPORTING
        CONTAINER_NAME = 'ALV_1002'.

    CREATE OBJECT BALANCO_ALV_1002
      EXPORTING
        I_PARENT = BALANCO_CONTAINER_1002.

    PERFORM Z_ESTRUTURA_FIELDCAT TABLES BALANCO_CATALOGO_1002 USING:
        'IT_ZGLT046_ALV' 'EDITAR' ' '       'X' 01 04 SPACE SPACE SPACE 'X'   SPACE SPACE SPACE SPACE ,
        'IT_ZGLT046_ALV' 'ESTRUT' ' '       'X' 02 04 SPACE SPACE SPACE 'X'   SPACE SPACE SPACE SPACE ,
        'IT_ZGLT046_ALV' 'VERSN'  TEXT_E001 ' ' 03 05 SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE ,
        'IT_ZGLT046_ALV' 'VSTXT'  TEXT_E002 ' ' 04 60 SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE .

    CLEAR: BALANCO_GS_LAYOUT_1002.
    BALANCO_GS_LAYOUT_1002-ZEBRA    = ABAP_TRUE.
    BALANCO_GS_LAYOUT_1002-SEL_MODE = 'A'.

    CALL METHOD BALANCO_ALV_1002->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_DEFAULT       = SPACE
        IS_LAYOUT       = BALANCO_GS_LAYOUT_1002
      CHANGING
        IT_FIELDCATALOG = BALANCO_CATALOGO_1002
        IT_OUTTAB       = IT_ZGLT046_ALV[].

    CREATE OBJECT BALANCO_EVENT_ALV_1002.
    SET HANDLER BALANCO_EVENT_ALV_1002->HANDLE_HOTSPOT_ALV_1002 FOR BALANCO_ALV_1002.

    PRIM_BALANCO_ALV = ABAP_ON.
  ENDIF.

  CALL METHOD BALANCO_ALV_1002->REFRESH_TABLE_DISPLAY.

  CALL METHOD BALANCO_ALV_1002->SET_SCROLL_INFO_VIA_ID
    EXPORTING
      IS_COL_INFO = WA_SCROLL_COL_1002
      IS_ROW_NO   = WA_SCROLL_ROW_1002.

ENDMODULE.                 " STATUS_1002  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ALV_ESTRUTURAS
*&---------------------------------------------------------------------*
*       Atualiza estrutura alv apresentada na tela 1002
*       Apresenta todas as estruturas do Balanço Patrimonial
*----------------------------------------------------------------------*
FORM ATUALIZA_ALV_ESTRUTURAS .

  SELECT * INTO TABLE IT_ZGLT046 FROM ZGLT046.

  CLEAR: IT_ZGLT046_ALV[].

  LOOP AT IT_ZGLT046.
    CLEAR: IT_ZGLT046_ALV.
    MOVE-CORRESPONDING IT_ZGLT046 TO IT_ZGLT046_ALV.
    IT_ZGLT046_ALV-EDITAR = ICON_CHANGE.
    IT_ZGLT046_ALV-ESTRUT = ICON_DISPLAY_TREE.
    APPEND IT_ZGLT046_ALV.
  ENDLOOP.

ENDFORM.                    " ATUALIZA_ALV_ESTRUTURAS

*&---------------------------------------------------------------------*
*&      Form  NOVA_ESTRUTURA_BALANCO
*&---------------------------------------------------------------------*
FORM NOVA_ESTRUTURA_BALANCO .
  CLEAR: IT_ZGLT046.
  CALL SCREEN 1003 STARTING AT 10 10.
  "Estrutura Lançada
  IF IT_ZGLT046 IS NOT INITIAL.

  ENDIF.
ENDFORM.                    " NOVA_ESTRUTURA_BALANCO

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ESTRUTURA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CARREGA_ESTRUTURA_ARQUIVO .

  DATA: FILE_TABLE    TYPE FILETABLE,
        WA_FILE       TYPE FILE_TABLE,
        LC_FILE       TYPE STRING,
        CK_FILE       TYPE C LENGTH 1,
        IT_LINES      TYPE TY_LINE OCCURS 0 WITH HEADER LINE,
        IT_SPLIT      TYPE TABLE OF TY_SPLIT WITH HEADER LINE,
        WA_ZGLT047_LC TYPE ZGLT047,
        WA_ZGLT047_SM TYPE ZGLT047,
        WA_ZGLT048_LC TYPE ZGLT048,
        ET_INDEX_ROWS	TYPE LVC_T_ROW,
        WA_INDEX_ROWS TYPE LVC_S_ROW,
        CK_FILHO_SOMA TYPE C LENGTH 1,
        RC  TYPE I.

  CALL METHOD BALANCO_ALV_1002->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = ET_INDEX_ROWS.

  IF ET_INDEX_ROWS IS NOT INITIAL.

    READ TABLE ET_INDEX_ROWS  INTO WA_INDEX_ROWS INDEX 1.
    READ TABLE IT_ZGLT046_ALV INDEX WA_INDEX_ROWS-INDEX.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
      CHANGING
        FILE_TABLE              = FILE_TABLE
        RC                      = RC
      EXCEPTIONS
        FILE_OPEN_DIALOG_FAILED = 1
        CNTL_ERROR              = 2
        ERROR_NO_GUI            = 3
        NOT_SUPPORTED_BY_GUI    = 4
        OTHERS                  = 5.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF FILE_TABLE IS NOT INITIAL.
      READ TABLE FILE_TABLE INTO WA_FILE INDEX 1.

      LC_FILE = WA_FILE-FILENAME.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
        EXPORTING
          FILE                 = LC_FILE
        RECEIVING
          RESULT               = CK_FILE
        EXCEPTIONS
          CNTL_ERROR           = 1
          ERROR_NO_GUI         = 2
          WRONG_PARAMETER      = 3
          NOT_SUPPORTED_BY_GUI = 4
          OTHERS               = 5.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF CK_FILE EQ ABAP_TRUE.

        CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
            FILENAME                = LC_FILE
          TABLES
            DATA_TAB                = IT_LINES
          EXCEPTIONS
            FILE_OPEN_ERROR         = 1
            FILE_READ_ERROR         = 2
            NO_BATCH                = 3
            GUI_REFUSE_FILETRANSFER = 4
            INVALID_TYPE            = 5
            NO_AUTHORITY            = 6
            UNKNOWN_ERROR           = 7
            BAD_DATA_FORMAT         = 8
            HEADER_NOT_ALLOWED      = 9
            SEPARATOR_NOT_ALLOWED   = 10
            HEADER_TOO_LONG         = 11
            UNKNOWN_DP_ERROR        = 12
            ACCESS_DENIED           = 13
            DP_OUT_OF_MEMORY        = 14
            DISK_FULL               = 15
            DP_TIMEOUT              = 16
            OTHERS                  = 17.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.

          LOOP AT IT_LINES.
            CHECK SY-TABIX NE 1.
            CLEAR: WA_ZGLT047_LC.
            WA_ZGLT047_LC-VERSN = IT_ZGLT046_ALV-VERSN.

            SPLIT IT_LINES AT ';' INTO TABLE IT_SPLIT.
            LOOP AT IT_SPLIT.
              CASE SY-TABIX.
                  "Nível
                WHEN 1.
                  MOVE IT_SPLIT-VALOR TO WA_ZGLT047_LC-NIVEL.
                  "Descrição
                WHEN 2.
                  MOVE IT_SPLIT-VALOR TO WA_ZGLT047_LC-DESNVL.
                  "Sequência
                WHEN 3.
                  MOVE IT_SPLIT-VALOR TO WA_ZGLT047_LC-SQNIVEL.
                  "Nível Pai
                WHEN 4.
                  MOVE IT_SPLIT-VALOR TO WA_ZGLT047_LC-NIVELPAI.
                  "Nível Somatorio
                WHEN 5.
                  MOVE IT_SPLIT-VALOR TO WA_ZGLT047_LC-NIVELSUM.
              ENDCASE.
            ENDLOOP.

            CK_FILHO_SOMA = ABAP_FALSE.

            SELECT SINGLE *
              INTO WA_ZGLT047_SM
              FROM ZGLT047
             WHERE VERSN    EQ WA_ZGLT047_LC-VERSN
               AND NIVEL    EQ WA_ZGLT047_LC-NIVELPAI
               AND NIVELSUM EQ ABAP_TRUE.

            IF SY-SUBRC IS INITIAL.
              CK_FILHO_SOMA = ABAP_TRUE.
            ENDIF.

            CASE CK_FILHO_SOMA.
              WHEN ABAP_FALSE.
                PERFORM AJUSTA_NIVEL USING WA_ZGLT047_LC.
                MODIFY ZGLT047 FROM WA_ZGLT047_LC.
                SELECT * INTO TABLE IT_ZGLT041
                  FROM ZGLT041
                 WHERE COD_CLAS_BAL EQ WA_ZGLT047_LC-NIVEL
                 ORDER BY COD_CLAS_BAL COD_CLAS_NOT2.
                IF SY-SUBRC IS INITIAL.
                  PERFORM INCLUIR_NOTAS TABLES IT_ZGLT041 USING WA_ZGLT047_LC.
                ENDIF.
              WHEN ABAP_TRUE.
                CLEAR: WA_ZGLT048_LC.
                WA_ZGLT048_LC-VERSN    = WA_ZGLT047_LC-VERSN.
                WA_ZGLT048_LC-NIVEL    = WA_ZGLT047_SM-NIVEL.
                WA_ZGLT048_LC-NIVELSUM = WA_ZGLT047_LC-NIVEL.
                MODIFY ZGLT048 FROM WA_ZGLT048_LC.
            ENDCASE.

          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE S010.
  ENDIF.

ENDFORM.                    " CARREGA_ESTRUTURA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  APAGAR_ESTRUTURA_BALANCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APAGAR_ESTRUTURA_BALANCO .

  DATA: ET_INDEX_ROWS	TYPE LVC_T_ROW,
        WA_INDEX_ROWS TYPE LVC_S_ROW,
        ANSWER     TYPE C LENGTH 1.

  CALL METHOD BALANCO_ALV_1002->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = ET_INDEX_ROWS.

  IF ET_INDEX_ROWS IS NOT INITIAL.
    READ TABLE ET_INDEX_ROWS  INTO WA_INDEX_ROWS INDEX 1.
    READ TABLE IT_ZGLT046_ALV INDEX WA_INDEX_ROWS-INDEX.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        TITEL     = 'Atenção!'
        TEXTLINE1 = 'A Estrutura será Excluida'
        TEXTLINE2 = 'Deseja Excluir a Estrutura?'
      IMPORTING
        ANSWER    = ANSWER.

    IF ANSWER = 'J'.
      DELETE FROM ZGLT049 WHERE VERSN EQ IT_ZGLT046_ALV-VERSN.
      DELETE FROM ZGLT048 WHERE VERSN EQ IT_ZGLT046_ALV-VERSN.
      DELETE FROM ZGLT047 WHERE VERSN EQ IT_ZGLT046_ALV-VERSN.
      DELETE FROM ZGLT046 WHERE VERSN EQ IT_ZGLT046_ALV-VERSN.
      COMMIT WORK.
      PERFORM ATUALIZA_ALV_ESTRUTURAS.
      MESSAGE S012.
    ENDIF.
  ELSE.
    MESSAGE S011.
  ENDIF.

ENDFORM.                    " APAGAR_ESTRUTURA_BALANCO
