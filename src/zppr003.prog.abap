
*&---------------------------------------------------------------------*
*& Report  ZPPR003
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZPPR003.

TYPES BEGIN OF TY_SAIDA.
        INCLUDE TYPE ZPPT0012.
TYPES: MAKTX TYPE MAKT-MAKTX,
       MARK  TYPE C.
TYPES END OF TY_SAIDA.

TYPES:
  BEGIN OF TY_FIELDS,
    GROUP TYPE CHAR3,
    VALUE TYPE NUM1,
  END OF TY_FIELDS.

DATA SOLICITACAO TYPE ZPPT0012.
DATA FIELDS      TYPE TABLE OF TY_FIELDS.

DATA: GT_SAIDA TYPE TABLE OF TY_SAIDA,
      GW_SAIDA TYPE TY_SAIDA.

DATA BATCH_DESTINATION TYPE UMCHA.

"//Classes definitions;
CLASS CX_LOCAL_EXCEPTION DEFINITION INHERITING FROM CX_STATIC_CHECK.
  PUBLIC SECTION.
    METHODS GET_TEXT REDEFINITION.

    METHODS GET_TEXTS
      RETURNING VALUE(RETURN) TYPE BAPIRET2_T.

    METHODS CONSTRUCTOR
      IMPORTING
        TEXT  TYPE ITEX132    OPTIONAL
        TEXTS TYPE BAPIRET2_T OPTIONAL.

    METHODS MESSAGE
      IMPORTING
        TYPE    TYPE C
        DISPLAY TYPE C OPTIONAL.

    METHODS DISPLAY.

    DATA TEXT   TYPE ITEX132.
    DATA RETURN TYPE BAPIRET2_T.
ENDCLASS.

CLASS CX_LOCAL_EXCEPTION IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    CALL METHOD SUPER->CONSTRUCTOR.
    ME->TEXT   = TEXT.
    ME->RETURN = TEXTS.
  ENDMETHOD.

  METHOD GET_TEXT.
    MOVE ME->TEXT TO RESULT.
  ENDMETHOD.

  METHOD GET_TEXTS.
    MOVE ME->RETURN TO RETURN.
  ENDMETHOD.

  METHOD DISPLAY.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        IT_MESSAGE = ME->RETURN.
  ENDMETHOD.

  METHOD MESSAGE.
    MESSAGE ME->GET_TEXT( ) TYPE TYPE DISPLAY LIKE DISPLAY.
  ENDMETHOD.
ENDCLASS.

CLASS CL_MAIN DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS RUN.

    METHODS SET_TITLE.
    METHODS SET_STATUS.
    METHODS SET_INITIAL_DATA.
    METHODS SET_SCREEN_FIELDS.
    METHODS USER_COMMAND IMPORTING UCOMM TYPE SY-UCOMM.
    METHODS SELECT_DATA.

    METHODS SAVE
      RETURNING VALUE(SUCCESS) TYPE ABAP_BOOL.

    METHODS GET_DESTINATION_BATCH
      IMPORTING
        DEPOSITO     TYPE ZPPT0012-LGORT
      RETURNING
        VALUE(BATCH) TYPE UMCHA.

    METHODS ALERT_BEFORE_EXIT
      EXCEPTIONS
        CANNOT_EXIT.

    METHODS GET_MATERIAL_TEXT
      IMPORTING
        MATERIAL    TYPE ZPPT0012-MATNR
      RETURNING
        VALUE(TEXT) TYPE MAKTX.

    METHODS GET_NEXT_ID
      IMPORTING
        TEMP         TYPE ABAP_BOOL OPTIONAL
      RETURNING
        VALUE(VALUE) TYPE CHAR10.

ENDCLASS.

DATA REF_MAIN TYPE REF TO CL_MAIN.

CLASS CL_MAIN IMPLEMENTATION.
  METHOD RUN.
    CREATE OBJECT REF_MAIN.
    CALL SCREEN 0001.
  ENDMETHOD.

  METHOD SET_TITLE.
    SET TITLEBAR 'MAIN_TITLE'.
  ENDMETHOD.

  METHOD SET_STATUS.
    SET PF-STATUS 'MAIN_STATUS'.
  ENDMETHOD.

  METHOD SET_INITIAL_DATA.
    CHECK SY-UCOMM <> 'BTN_SEARCH'.

    IF ( SOLICITACAO-SOLDF(1) = '$' OR SOLICITACAO-SOLDF IS INITIAL ).

      FIELDS = VALUE #( ( GROUP = 'GR1' VALUE = 1 )
                        ( GROUP = 'GR2' VALUE = 0 )
                      ).

      SOLICITACAO-SOLDF = ME->GET_NEXT_ID( TEMP = ABAP_TRUE ).
      SOLICITACAO-CREATED_BY = SY-UNAME.
      SOLICITACAO-CREATED_AT = SY-DATUM.
*    ELSE.

    ENDIF.
  ENDMETHOD.

  METHOD SET_SCREEN_FIELDS.
    LOOP AT FIELDS INTO DATA(_FIELD).
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = _FIELD-GROUP.
          SCREEN-INPUT = _FIELD-VALUE.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD USER_COMMAND.
    CASE UCOMM.
      WHEN 'CANCEL' OR 'EXIT' OR 'BACK'.

        ME->ALERT_BEFORE_EXIT( EXCEPTIONS CANNOT_EXIT = 4 ).

        IF SY-SUBRC = 0.
          LEAVE PROGRAM.
        ENDIF.

      WHEN 'SAVE'.
        ME->SAVE( ).

      WHEN 'BTN_SEARCH'.
        CLEAR: SOLICITACAO-SOLDF, SOLICITACAO-UMLGO, GT_SAIDA.
        FIELDS = VALUE #( ( GROUP = 'GR1' VALUE = 0 ) ( GROUP = 'GR2' VALUE = 1 ) ).

      WHEN 'BTN_NEW'.
        CLEAR: GT_SAIDA, SOLICITACAO-UMLGO.

        SOLICITACAO-SOLDF = ME->GET_NEXT_ID( TEMP = ABAP_TRUE ).
        FIELDS = VALUE #( ( GROUP = 'GR1' VALUE = 1 ) ( GROUP = 'GR2' VALUE = 0 ) ).

      WHEN 'ENTER'.
        ME->SELECT_DATA( ).
    ENDCASE.
  ENDMETHOD.

  METHOD SAVE.
    SUCCESS = ABAP_TRUE.

    IF LINES( GT_SAIDA ) = 0.
      SUCCESS = ABAP_FALSE.
      MESSAGE TEXT-E03 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      DATA ITEM_TABLE TYPE ZPPT0012.
      DATA ITEMS_TABLE TYPE TABLE OF ZPPT0012.

      SOLICITACAO-SOLDF = ME->GET_NEXT_ID( ).

      TRY.
          IF SOLICITACAO-UMLGO IS INITIAL.
            RAISE EXCEPTION TYPE CX_LOCAL_EXCEPTION
              EXPORTING
                TEXT = TEXT-E04.
          ENDIF.

          LOOP AT GT_SAIDA INTO GW_SAIDA.
            MOVE-CORRESPONDING GW_SAIDA TO ITEM_TABLE.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = ITEM_TABLE-MATNR
              IMPORTING
                OUTPUT = ITEM_TABLE-MATNR.

            ITEM_TABLE-SOLDF         = SOLICITACAO-SOLDF.
            ITEM_TABLE-CREATED_AT = SOLICITACAO-CREATED_AT.
            ITEM_TABLE-CREATED_BY = SOLICITACAO-CREATED_BY.
            ITEM_TABLE-UMLGO      = SOLICITACAO-UMLGO.

            IF GW_SAIDA-MAKTX IS INITIAL.
              RAISE EXCEPTION TYPE CX_LOCAL_EXCEPTION
                EXPORTING
                  TEXT = |{ TEXT-E01 } (Linha { SY-TABIX })|.
            ENDIF.

            IF GW_SAIDA-LGORT IS INITIAL OR GW_SAIDA-BDMNG IS INITIAL.
              RAISE EXCEPTION TYPE CX_LOCAL_EXCEPTION
                EXPORTING
                  TEXT = |{ TEXT-E05 } (Linha { SY-TABIX })|.
            ENDIF.

            APPEND ITEM_TABLE TO ITEMS_TABLE.
          ENDLOOP.

          MODIFY ZPPT0012 FROM TABLE ITEMS_TABLE.
          COMMIT WORK.

          MESSAGE |Solicitação gerada sob. nº { SOLICITACAO-SOLDF }| TYPE 'S'.
          FIELDS = VALUE #( ( GROUP = 'GR1' VALUE = 0 ) ( GROUP = 'GR2' VALUE = 0 ) ).

        CATCH CX_LOCAL_EXCEPTION INTO DATA(_CX).
          SUCCESS = ABAP_FALSE.
          SOLICITACAO-SOLDF = ME->GET_NEXT_ID( TEMP = ABAP_TRUE ).

          _CX->MESSAGE(
            EXPORTING
              TYPE    = 'S'
              DISPLAY = 'E'
          ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD GET_DESTINATION_BATCH.
    DATA _DEPOSITO TYPE NUM8.
    _DEPOSITO = DEPOSITO+2.

    BATCH = 'EQ' && _DEPOSITO.
  ENDMETHOD.

  METHOD ALERT_BEFORE_EXIT.
    DATA ANSWER TYPE C.

    CHECK SOLICITACAO-SOLDF(1) = '$'.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR       = 'Registros não foram salvos'
*       DIAGNOSE_OBJECT       = ' '
        TEXT_QUESTION  = 'Deseja salvar os dados antes de sair?'
        TEXT_BUTTON_1  = 'Sim'
*       ICON_BUTTON_1  = ' '
        TEXT_BUTTON_2  = 'Não'
*       ICON_BUTTON_2  = ' '
        DEFAULT_BUTTON = '1'
*       DISPLAY_CANCEL_BUTTON = 'X'
*       USERDEFINED_F1_HELP   = ' '
*       START_COLUMN   = 25
*       START_ROW      = 6
*       POPUP_TYPE     =
*       IV_QUICKINFO_BUTTON_1 = ' '
*       IV_QUICKINFO_BUTTON_2 = ' '
      IMPORTING
        ANSWER         = ANSWER.

    IF ANSWER = '1'.
      IF ME->SAVE( ) = ABAP_FALSE.
        RAISE CANNOT_EXIT.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD SELECT_DATA.
    IF SOLICITACAO-SOLDF    IS NOT INITIAL
   AND SOLICITACAO-SOLDF(1) NE '$'.
      DATA(_ID) = CONV NUMC10( SOLICITACAO-SOLDF ).

      SELECT *
        FROM ZPPT0012
        INTO TABLE @DATA(_ITEMS)
       WHERE SOLDF = @_ID.

      IF _ITEMS IS INITIAL.
        MESSAGE TEXT-E02 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        CLEAR GT_SAIDA.

        LOOP AT _ITEMS INTO DATA(_ITEM).
          APPEND VALUE #( MATNR = _ITEM-MATNR WERKS = _ITEM-WERKS LGORT = _ITEM-LGORT BDMNG = _ITEM-BDMNG MAKTX = REF_MAIN->GET_MATERIAL_TEXT( _ITEM-MATNR ) ) TO GT_SAIDA.

          SOLICITACAO-SOLDF         = _ITEM-SOLDF.
          SOLICITACAO-CREATED_AT = _ITEM-CREATED_AT.
          SOLICITACAO-CREATED_BY = _ITEM-CREATED_BY.
          SOLICITACAO-UMLGO      = _ITEM-UMLGO.
        ENDLOOP.

        FIELDS = VALUE #( ( GROUP = 'GR1' VALUE = 0 ) ( GROUP = 'GR2' VALUE = 0 ) ).
      ENDIF.
    ENDIF.

    IF SOLICITACAO-UMLGO IS NOT INITIAL AND BATCH_DESTINATION IS INITIAL.
      BATCH_DESTINATION = ME->GET_DESTINATION_BATCH( SOLICITACAO-UMLGO ).
    ENDIF.

  ENDMETHOD.

  METHOD GET_MATERIAL_TEXT.
    DATA(_MATNR) = MATERIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = _MATNR
      IMPORTING
        OUTPUT = _MATNR.

    SELECT SINGLE MAKTX
      FROM MAKT
      INTO TEXT
     WHERE MATNR = _MATNR.
  ENDMETHOD.

  METHOD GET_NEXT_ID.
*    DATA _ID TYPE NUMC10.
    SELECT MAX( SOLDF ) FROM ZPPT0012 INTO VALUE.
    DATA(_ID) = CONV NUMC10( VALUE ).

    IF _ID IS INITIAL.
      _ID = 0000000001.
    ELSE.
      ADD 1 TO _ID.
    ENDIF.

    IF TEMP = ABAP_TRUE.
      VALUE = '$' && _ID+1.
    ELSE.
      VALUE = _ID.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*CALL SCREEN 0001.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MAIN_PBO OUTPUT.
  IF REF_MAIN IS NOT BOUND.
    CREATE OBJECT REF_MAIN.
  ENDIF.

  REF_MAIN->SET_STATUS( ).
  REF_MAIN->SET_TITLE( ).

  REF_MAIN->SET_INITIAL_DATA( ).
  REF_MAIN->SET_SCREEN_FIELDS( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MAIN_PAI INPUT.
  REF_MAIN->USER_COMMAND( SY-UCOMM ).
ENDMODULE.


INITIALIZATION.
  CL_MAIN=>RUN( ).

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TBCONTROL' ITSELF
  CONTROLS: TBCONTROL TYPE TABLEVIEW USING SCREEN 0001.

*&SPWIZARD: LINES OF TABLECONTROL 'TBCONTROL'
  DATA:     G_TBCONTROL_LINES  LIKE SY-LOOPC.

  DATA:     OK_CODE LIKE SY-UCOMM.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TBCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE TBCONTROL_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE GT_SAIDA LINES TBCONTROL-LINES.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TBCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE TBCONTROL_GET_LINES OUTPUT.
  G_TBCONTROL_LINES = SY-LOOPC.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TBCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE TBCONTROL_MODIFY INPUT.

  GW_SAIDA-MAKTX = REF_MAIN->GET_MATERIAL_TEXT( GW_SAIDA-MATNR ).

  IF GW_SAIDA-MAKTX IS INITIAL.
    MESSAGE TEXT-E01 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  IF GW_SAIDA-LGORT IS INITIAL OR GW_SAIDA-BDMNG IS INITIAL.
    MESSAGE TEXT-E05 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  IF GT_SAIDA IS INITIAL.
    APPEND GW_SAIDA TO GT_SAIDA.
  ELSE.
    MODIFY GT_SAIDA
      FROM GW_SAIDA
      INDEX TBCONTROL-CURRENT_LINE.
  ENDIF.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TBCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE TBCONTROL_MARK INPUT.
  DATA: G_TBCONTROL_WA2 LIKE LINE OF GT_SAIDA.
  IF TBCONTROL-LINE_SEL_MODE = 1
  AND GW_SAIDA-MARK = 'X'.
    LOOP AT GT_SAIDA INTO G_TBCONTROL_WA2
      WHERE MARK = 'X'.
      G_TBCONTROL_WA2-MARK = ''.
      MODIFY GT_SAIDA
        FROM G_TBCONTROL_WA2
        TRANSPORTING MARK.
    ENDLOOP.
  ENDIF.
  MODIFY GT_SAIDA
    FROM GW_SAIDA
    INDEX TBCONTROL-CURRENT_LINE
    TRANSPORTING MARK.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TBCONTROL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE TBCONTROL_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'TBCONTROL'
                              'GT_SAIDA'
                              'MARK'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                         P_TABLE_NAME
                         P_MARK_NAME
                CHANGING P_OK      LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: L_OK     TYPE SY-UCOMM,
        L_OFFSET TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH P_OK FOR P_TC_NAME.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  L_OFFSET = STRLEN( P_TC_NAME ) + 1.
  L_OK = P_OK+L_OFFSET.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE L_OK.
    WHEN 'INSR'.                      "insert row
      PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                        P_TABLE_NAME.
      CLEAR P_OK.

    WHEN 'DELE'.                      "delete row
      PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                        P_TABLE_NAME
                                        P_MARK_NAME.
      CLEAR P_OK.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                            L_OK.
      CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                        P_TABLE_NAME
                                        P_MARK_NAME   .
      CLEAR P_OK.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                          P_TABLE_NAME
                                          P_MARK_NAME .
      CLEAR P_OK.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM FCODE_INSERT_ROW
              USING    P_TC_NAME           TYPE DYNFNAM
                       P_TABLE_NAME             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_LINES_NAME       LIKE FELD-NAME.
  DATA L_SELLINE          LIKE SY-STEPL.
  DATA L_LASTLINE         TYPE I.
  DATA L_LINE             TYPE I.
  DATA L_TABLE_NAME       LIKE FELD-NAME.
  FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <LINES>              TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
  ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE L_SELLINE.
  IF SY-SUBRC <> 0.                   " append line to table
    L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
    IF L_SELLINE > <LINES>.
      <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
    ELSE.
      <TC>-TOP_LINE = 1.
    ENDIF.
  ELSE.                               " insert line into table
    L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
    L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
  <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE L_LINE.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM FCODE_DELETE_ROW
              USING    P_TC_NAME           TYPE DYNFNAM
                       P_TABLE_NAME
                       P_MARK_NAME   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    IF <MARK_FIELD> = 'X'.
      DELETE <TABLE> INDEX SYST-TABIX.
      IF SY-SUBRC = 0.
        <TC>-LINES = <TC>-LINES - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                      P_OK.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TC_NEW_TOP_LINE     TYPE I.
  DATA L_TC_NAME             LIKE FELD-NAME.
  DATA L_TC_LINES_NAME       LIKE FELD-NAME.
  DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
  ASSIGN (L_TC_LINES_NAME) TO <LINES>.


*&SPWIZARD: is no line filled?                                         *
  IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
    L_TC_NEW_TOP_LINE = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        ENTRY_ACT      = <TC>-TOP_LINE
        ENTRY_FROM     = 1
        ENTRY_TO       = <TC>-LINES
        LAST_PAGE_FULL = 'X'
        LOOPS          = <LINES>
        OK_CODE        = P_OK
        OVERLAPPING    = 'X'
      IMPORTING
        ENTRY_NEW      = L_TC_NEW_TOP_LINE
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD L_TC_FIELD_NAME
             AREA  L_TC_NAME.

  IF SYST-SUBRC = 0.
    IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
  ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

    <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  HELP_ID  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ID INPUT.
  SELECT *
    FROM ZPPT0012
    INTO TABLE @DATA(_ITEMS).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD    = 'SOLDF'
      DYNPPROG    = SY-REPID
      DYNPNR      = SY-DYNNR
      DYNPROFIELD = 'SOLICITACAO-SOLDF'
      VALUE_ORG   = 'S'
    TABLES
      VALUE_TAB   = _ITEMS.
ENDMODULE.
