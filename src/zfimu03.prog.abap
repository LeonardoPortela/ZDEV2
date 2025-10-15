*&--------------------------------------------------------------------&*
*&                        FI                                         &*
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Antonio Rodrigues                                       &*
*& Data.....: 07/04/2016                                              &*
*& Descrição: Contratos de Mútuos                                      &*
*& Transação: ZFIMU07                                                 &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Request DEVK956038     Data 01/04/2016  IR IR98191                 &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
PROGRAM ZFIMU03.


TYPES: BEGIN OF TY_ZFITAXCTR.
         INCLUDE STRUCTURE ZFITAXCTR.
         TYPES:  STYLE         TYPE LVC_T_STYL,
         NAME1_F       TYPE LFA1-NAME1,
         NAME1_C       TYPE KNA1-NAME1,
         MARK(1),
         NOVO(1),
         LINE_COLOR(4) TYPE C, "Used to store row color attributes
       END OF TY_ZFITAXCTR,

       BEGIN OF TY_FIELDS,
         CAMPO(30) TYPE C,
         GROUP1(5) TYPE C,
         VALUE     TYPE SY-TABIX,
         INVISIBLE TYPE SY-TABIX,
       END OF TY_FIELDS.

DATA: IT_ZFITAXCTR_AUX TYPE TABLE OF ZFITAXCTR,
      IT_ZFITAXCTR     TYPE TABLE OF ZFITAXCTR,
      TG_FIELDS        TYPE TABLE OF TY_FIELDS   WITH HEADER LINE,
      WA_ZFITAXCTR     TYPE ZFITAXCTR,
      WA_ZFITAXCTR_AUX TYPE ZFITAXCTR,
      IT_SELECT_ROWS   TYPE LVC_T_ROW,
      WA_SELECT_ROWS   TYPE LVC_S_ROW.

DATA: OK_CODE            LIKE SY-UCOMM,
      SAVE_OK            LIKE SY-UCOMM,
      WG_ACAO(30),
      WG_ZFITAXCTR       TYPE ZFITAXCTR,
      WG_MENSAGEM(30),
      WL_ERRO(1),

      G_CONTAINER        TYPE SCRFNAME VALUE 'BCALV_GRID_DEMO_0100_CONT1',
      G_GRID             TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRID1              TYPE REF TO CL_GUI_ALV_GRID,
      GS_LAYOUT          TYPE LVC_S_LAYO.

DATA: GT_OUTTAB   TYPE TABLE OF TY_ZFITAXCTR,

      IT_LFA1     TYPE TABLE OF LFA1,
      IT_KNA1     TYPE TABLE OF KNA1,
      WA_LFA1     TYPE LFA1,
      WA_KNA1     TYPE KNA1,
      WA_OUTTAB   TYPE TY_ZFITAXCTR,
      WA_STYLE    TYPE LVC_S_STYL,
      WA_STABLE   TYPE LVC_S_STBL,
      STYLE       TYPE LVC_T_STYL   WITH HEADER LINE,
      TG_MSG_RET  TYPE TABLE OF ZFIWRS0002  WITH HEADER LINE,
      X_FIELD(30).

DATA: T_FIELDCATALOG TYPE LVC_T_FCAT,
      W_FIELDCATALOG TYPE LVC_S_FCAT.

CONSTANTS: C_SHOW_MSGRE(10) TYPE C VALUE 'SHOW_MSGRE',
           C_X              TYPE C VALUE 'X',
           C_0              TYPE C VALUE '0',
           C_1              TYPE C VALUE '1'.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
        IMPORTING ER_DATA_CHANGED E_ONF4 E_ONF4_BEFORE E_ONF4_AFTER E_UCOMM .
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_DATA_CHANGED.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       MAIN                                                          *
*---------------------------------------------------------------------*
*CALL SCREEN 100.


*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE PBO OUTPUT.

  DATA: TL_FUNCTION TYPE UI_FUNCTIONS,
        WL_FUNCTION LIKE TL_FUNCTION WITH HEADER LINE.

  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'MAIN100'.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.
    CREATE OBJECT G_GRID
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER.

    GS_LAYOUT-EDIT = ''.
    GS_LAYOUT-STYLEFNAME = 'STYLE'.
    GS_LAYOUT-BOX_FNAME   = 'MARK'.
    GS_LAYOUT-INFO_FNAME  = 'LINE_COLOR'.
    GS_LAYOUT-CWIDTH_OPT  = C_X.
    GS_LAYOUT-ZEBRA       = C_X.
    GS_LAYOUT-NO_ROWMARK  = ''.
    GS_LAYOUT-SEL_MODE    = 'A'.

    WA_STABLE-ROW         = C_X.
    WA_STABLE-COL         = C_X.

    PERFORM F_ATUALIZAR.

    PERFORM MONTAR_LAYOUT.

    REFRESH: TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = GS_LAYOUT
      CHANGING
        IT_FIELDCATALOG      = T_FIELDCATALOG
        IT_OUTTAB            = GT_OUTTAB.

    CALL METHOD G_GRID->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 0.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD G_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

  ELSE.
    PERFORM F_ATUALIZAR.
    PERFORM MONTAR_LAYOUT.

    CALL METHOD G_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = T_FIELDCATALOG[].

    CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.                    "pbo OUTPUT
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE PAI INPUT.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  DATA: L_VALID(1) TYPE C.

  CASE SAVE_OK.
    WHEN 'NOVO'.
      WG_ACAO = 'ADD'.
      CLEAR: WG_ZFITAXCTR,  WA_OUTTAB.
      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR1'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      CALL SCREEN 0200   STARTING AT 050 3
                         ENDING   AT 202 22.
    WHEN 'EXIT'.
      PERFORM EXIT_PROGRAM.
    WHEN 'SWITCH'.
      CLEAR: WG_ZFITAXCTR,  WA_OUTTAB.
      WG_ACAO = 'UPD'.
      CALL METHOD G_GRID->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_SELECT_ROWS.

      LOOP AT IT_SELECT_ROWS INTO WA_SELECT_ROWS.
        READ TABLE GT_OUTTAB INTO WA_OUTTAB INDEX WA_SELECT_ROWS-INDEX.
        EXIT.
      ENDLOOP.

      IF WA_OUTTAB IS INITIAL.
        MESSAGE 'Selecione uma linha' TYPE 'I'.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING WA_OUTTAB TO WG_ZFITAXCTR.
      REFRESH: TG_FIELDS.
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR2'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR1'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      CALL SCREEN 0200   STARTING AT 050 3
                         ENDING   AT 202 22.
    WHEN 'EXCLUIR'.
      PERFORM  EXCLUIR.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                    "pai INPUT
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM EXIT_PROGRAM.
  LEAVE PROGRAM.
ENDFORM.                    "exit_program

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR
*&---------------------------------------------------------------------*
FORM EXCLUIR .
  DATA WA_ZFISOLCTR TYPE ZFISOLCTR.


  CALL METHOD G_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECT_ROWS.


  LOOP AT IT_SELECT_ROWS INTO WA_SELECT_ROWS.
    READ TABLE GT_OUTTAB INTO WA_OUTTAB INDEX WA_SELECT_ROWS-INDEX.
    IF WA_OUTTAB-ZID_CONTR IS NOT INITIAL.
      SELECT SINGLE *
        FROM ZFISOLCTR
        INTO WA_ZFISOLCTR
        WHERE ZID_CONTR =  WA_OUTTAB-ZID_CONTR.
      IF SY-SUBRC = 0.
        MESSAGE S836(SD) DISPLAY LIKE 'I' WITH 'Existe Solicitação de contrato'
                                               WA_ZFITAXCTR-ZID_CONTR.
        CONTINUE.
      ENDIF.
      DELETE FROM ZFITAXCTR
      WHERE ZID_CONTR = WA_OUTTAB-ZID_CONTR.
    ENDIF.
    DELETE GT_OUTTAB  INDEX WA_SELECT_ROWS-INDEX.
  ENDLOOP.

ENDFORM.                    " EXCLUIR
**&---------------------------------------------------------------------*
**&      Form  INCLUIR
**&---------------------------------------------------------------------*
FORM F_ATUALIZAR .

  SELECT * FROM ZFITAXCTR
      INTO CORRESPONDING FIELDS OF TABLE GT_OUTTAB.

  IF GT_OUTTAB[] IS NOT  INITIAL.
    SELECT *
      FROM LFA1
      INTO TABLE IT_LFA1
      FOR ALL ENTRIES IN GT_OUTTAB
      WHERE LIFNR = GT_OUTTAB-LIFNR.

    SORT IT_LFA1 BY LIFNR.

    SELECT *
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN GT_OUTTAB
      WHERE KUNNR = GT_OUTTAB-KUNNR.

    SORT IT_KNA1 BY KUNNR.
  ENDIF.

  LOOP AT GT_OUTTAB INTO WA_OUTTAB.
    CLEAR WA_LFA1.
    READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_OUTTAB-LIFNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_OUTTAB-NAME1_F = WA_LFA1-NAME1.
    ENDIF.
    "
    CLEAR WA_KNA1.
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_OUTTAB-KUNNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_OUTTAB-NAME1_C = WA_KNA1-NAME1.
    ENDIF.
*    REFRESH  STYLE.
*    WA_STYLE-FIELDNAME = 'ZID_CONTR'.
*    WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT  WA_STYLE INTO TABLE STYLE .
*
*    WA_STYLE-FIELDNAME = 'DT_INI'.
*    WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT  WA_STYLE INTO TABLE STYLE .
*
*    INSERT LINES OF STYLE INTO TABLE  WA_OUTTAB-STYLE.
    IF WA_OUTTAB-DT_FIM LT SY-DATUM.
      WA_OUTTAB-LINE_COLOR = 'C610'.
    ENDIF.
    MODIFY GT_OUTTAB FROM WA_OUTTAB TRANSPORTING STYLE NAME1_F NAME1_C LINE_COLOR.
  ENDLOOP.

ENDFORM.                    " INCLUIR
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS .

  DATA: WL_LINHA(6),
        WL_T001       TYPE T001,
        WL_J_1BBRANCH TYPE J_1BBRANCH,
        WL_TGSB       TYPE TGSB,
        WL_LFA1       TYPE LFA1,
        WL_KNA1       TYPE KNA1,
        WL_SKA1       TYPE SKA1.

  CLEAR:    TG_MSG_RET.
  REFRESH:  TG_MSG_RET.

  MOVE-CORRESPONDING WG_ZFITAXCTR TO WA_OUTTAB.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WA_OUTTAB-ZID_CONTR
    IMPORTING
      OUTPUT = WA_OUTTAB-ZID_CONTR.
  IF WA_OUTTAB-ZID_CONTR IS INITIAL OR WA_OUTTAB-ZID_CONTR = '000000'.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-ZID_CONTR'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Informe o contrato' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSEIF WG_ACAO = 'ADD'.
    SELECT SINGLE * FROM ZFITAXCTR INTO WA_ZFITAXCTR WHERE ZID_CONTR = WA_OUTTAB-ZID_CONTR.
    IF SY-SUBRC = 0.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-ZID_CONTR'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Existe contrato' WA_OUTTAB-ZID_CONTR WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WA_OUTTAB-NOM_CONTR IS INITIAL.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-NOM_CONTR'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Informe A DESCRIÇÃO' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.
*
  "Sempre obrigatorio
  IF WA_OUTTAB-KUNNR IS INITIAL.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-KUNNR'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Informe o cliente' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_OUTTAB-KUNNR
      IMPORTING
        OUTPUT = WA_OUTTAB-KUNNR.
    SELECT SINGLE *
          FROM KNA1
          INTO WL_KNA1
          WHERE KUNNR = WA_OUTTAB-KUNNR.

    IF SY-SUBRC NE 0.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-KUNNR'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Cliente não cadastrado' WA_OUTTAB-KUNNR WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WA_OUTTAB-BUKRS_C NE '9999'.
    IF WA_OUTTAB-BUKRS_C IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-BUKRS_C'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe a empresa cliente' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
            FROM T001
            INTO WL_T001
            WHERE BUKRS = WA_OUTTAB-BUKRS_C.

      IF SY-SUBRC NE 0.
        CLEAR TG_MSG_RET-ABA.
        MOVE: 'WG_ZFITAXCTR-BUKRS_C'   TO TG_MSG_RET-FIELD.
        CONCATENATE 'Empresa cliente não cadastrada' WA_OUTTAB-BUKRS_C WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
        FROM T001
        INTO WL_T001
        WHERE BUKRS = WA_OUTTAB-BUKRS_C.
    IF WL_T001-LAND1 = 'BR'.
      SELECT SINGLE *
        FROM J_1BBRANCH
        INTO WL_J_1BBRANCH
        WHERE BUKRS  = WA_OUTTAB-BUKRS_C
        AND   BRANCH = WA_OUTTAB-GSBER_C.
    ELSE.
      SELECT SINGLE * FROM TGSB
        INTO  WL_TGSB
      WHERE GSBER EQ WA_OUTTAB-GSBER_C.
    ENDIF.
    IF SY-SUBRC NE 0.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-GSBER_C'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Divisão do cliente inválida' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.


    IF WA_OUTTAB-HKONT_C IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-HKONT_C'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe a conta cliente' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_OUTTAB-HKONT_C
        IMPORTING
          OUTPUT = WA_OUTTAB-HKONT_C.
      SELECT SINGLE * "#EC CI_DB_OPERATION_OK[2431747]
            FROM SKA1 "#EC CI_DB_OPERATION_OK[2389136]
            INTO WL_SKA1
            WHERE KTOPL = '0050'
            AND   SAKNR = WA_OUTTAB-HKONT_C.

      IF SY-SUBRC NE 0.
        CLEAR TG_MSG_RET-ABA.
        MOVE: 'WG_ZFITAXCTR-KUNNR'   TO TG_MSG_RET-FIELD.
        CONCATENATE 'Conta cliente não cadastrada' WA_OUTTAB-HKONT_C WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.


    IF WA_OUTTAB-UMSKZ_C IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-UMSKZ'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'rz.especial Clien' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WA_OUTTAB-TP_CAPI_C  IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-TP_CAPI_C'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe o tipo capitalização cliente' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WA_OUTTAB-TP_AMOR_C  IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-TP_AMOR_C'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe o tipo amortização cliente' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WA_OUTTAB-TP_PROV_C  IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-TP_PROV_C'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe o tipo provisão cliente' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  "sempre obrigatorio
  IF WA_OUTTAB-LIFNR IS INITIAL.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-LIFNR'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Informe o fornecedor' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_OUTTAB-LIFNR
      IMPORTING
        OUTPUT = WA_OUTTAB-LIFNR.
    SELECT SINGLE *
          FROM LFA1
          INTO WL_LFA1
          WHERE LIFNR = WA_OUTTAB-LIFNR.

    IF SY-SUBRC NE 0.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-LIFNR'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Fornecedor não cadastrado' WA_OUTTAB-LIFNR WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WA_OUTTAB-BUKRS_F NE '9999'.
    IF WA_OUTTAB-BUKRS_F IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-BUKRS_F'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe a empresa fornecedora' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
        FROM T001
        INTO WL_T001
        WHERE BUKRS = WA_OUTTAB-BUKRS_F.

      IF SY-SUBRC NE 0.
        CLEAR TG_MSG_RET-ABA.
        MOVE: 'WG_ZFITAXCTR-BUKRS_F'   TO TG_MSG_RET-FIELD.
        CONCATENATE 'Empresa fornecedora não cadastrada' WA_OUTTAB-BUKRS_F WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    "divisão fornecedor
    SELECT SINGLE *
         FROM T001
         INTO WL_T001
         WHERE BUKRS =  WA_OUTTAB-BUKRS_F.
    IF WL_T001-LAND1 = 'BR'.
      SELECT SINGLE *
        FROM J_1BBRANCH
        INTO WL_J_1BBRANCH
        WHERE BUKRS  = WA_OUTTAB-BUKRS_F
        AND   BRANCH = WA_OUTTAB-GSBER_F.
    ELSE.
      SELECT SINGLE * FROM TGSB
        INTO  WL_TGSB
      WHERE GSBER EQ WA_OUTTAB-GSBER_F.
    ENDIF.
    IF SY-SUBRC NE 0.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-GSBER_F'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Divisão do fornecedor inválida' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WA_OUTTAB-HKONT_F IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      CONCATENATE 'Informe a conta fornecedor' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_OUTTAB-HKONT_F
        IMPORTING
          OUTPUT = WA_OUTTAB-HKONT_F.
      SELECT SINGLE * "#EC CI_DB_OPERATION_OK[2389136]
            FROM SKA1 "#EC CI_DB_OPERATION_OK[2431747]
            INTO WL_SKA1
            WHERE KTOPL = '0050'
            AND   SAKNR = WA_OUTTAB-HKONT_F.

      IF SY-SUBRC NE 0.
        CLEAR TG_MSG_RET-ABA.
        MOVE: 'WG_ZFITAXCTR-HKONT_F'   TO TG_MSG_RET-FIELD.
        CONCATENATE 'Conta fornecedor não cadastrada' WA_OUTTAB-HKONT_F WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
        APPEND TG_MSG_RET.
        CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WA_OUTTAB-UMSKZ_F IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-UMSKZ_F'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'rz.especial Forn' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WA_OUTTAB-TP_CAPI_F  IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-TP_CAPI_F'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe o tipo capitalização fornecedor' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WA_OUTTAB-TP_AMOR_F  IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-TP_AMOR_F'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe o tipo amortização fornecedor' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.

    IF WA_OUTTAB-TP_PROV_F  IS INITIAL.
      CLEAR TG_MSG_RET-ABA.
      MOVE: 'WG_ZFITAXCTR-TP_PROV_F'   TO TG_MSG_RET-FIELD.
      CONCATENATE 'Informe o tipo provisão fornecedor' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
      APPEND TG_MSG_RET.
      CLEAR: TG_MSG_RET.
    ENDIF.
  ENDIF.

  IF WA_OUTTAB-DT_INI  IS INITIAL.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-DT_INI'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Informe a data inicial' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WA_OUTTAB-DT_FIM  IS INITIAL.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-DT_FIM'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Informe a data final' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WA_OUTTAB-DT_INI GE WA_OUTTAB-DT_FIM.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-DT_INI'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Data inicial inválida' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WA_OUTTAB-VLR_CTR  IS INITIAL.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-VLR_CTR'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Informe o valor inicial do contrato' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WA_OUTTAB-TAXA  IS INITIAL.
*      CLEAR TG_MSG_RET-ABA.
*      CONCATENATE 'Informe a taxa' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*  ELSEIF WA_OUTTAB-TAXA  GT 100 OR WA_OUTTAB-TAXA LT 0.
  ELSEIF WA_OUTTAB-TAXA LT 0.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-TAXA'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Taxa inválida' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

  IF WA_OUTTAB-WAERS  IS INITIAL.
    CLEAR TG_MSG_RET-ABA.
    MOVE: 'WG_ZFITAXCTR-WAERS'   TO TG_MSG_RET-FIELD.
    CONCATENATE 'Informe a moeda' WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
    APPEND TG_MSG_RET.
    CLEAR: TG_MSG_RET.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .
  REFRESH T_FIELDCATALOG.
  PERFORM MONTAR_ESTRUTURA USING:
        1 ' '                        ' '               'GT_OUTTAB' 'ZID_CONTR'         'Contrato '           '10' ' ' ' ' ' ',
        2 ' '                        ' '               'GT_OUTTAB' 'DT_INI'            'Dt.Inicio'           '10' ' ' ' ' ' ',
        3 ' '                        ' '               'GT_OUTTAB' 'DT_FIM'            'Dt. Fim '            '10' ' ' ' ' ' ',
        4 ' '                        ' '               'GT_OUTTAB' 'TAXA'              'Taxa'                '08' ' ' ' ' ' ',
        5 ' '                        ' '               'GT_OUTTAB' 'WAERS'             'Moeda'               '08' ' ' ' ' ' ',
        6 ' '                        ' '               'GT_OUTTAB' 'VLR_CTR'           'Valor Inicial'       '15' ' ' ' ' ' ',
        6 ' '                        ' '               'GT_OUTTAB' 'NOM_CONTR'         'Descr.Contrato'      '20' ' ' ' ' ' ',

        7 'T001'                     'BUKRS'           'GT_OUTTAB' 'BUKRS_F'           'Emp.01'              '06' ' ' ' ' ' ',
        8 ' '                        ' '               'GT_OUTTAB' 'GSBER_F'           'Div.Emp.01'          '10' ' ' ' ' ' ',
       09 'LFA1'                     'LIFNR'           'GT_OUTTAB' 'LIFNR'             'Fornec'              '10' ' ' ' ' ' ',
       09 'LFA1'                     'NAME1'           'GT_OUTTAB' 'NAME1_F'           'Descrição'           '20' ' ' ' ' ' ',
       10 'SKA1'                     'SAKNR'           'GT_OUTTAB' 'HKONT_F'           'Conta Razão Forn'    '15' ' ' ' ' ' ',
       11 ' '                        ' '               'GT_OUTTAB' 'UMSKZ_F'           'Razão Esp. Forn'     '15' ' ' ' ' ' ',
       12 ' '                        ' '               'GT_OUTTAB' 'TP_CAPI_F'         'Tp. Cap. Forn'       '10' ' ' ' ' ' ',
       13 ' '                        ' '               'GT_OUTTAB' 'TP_AMOR_F'         'Tp. Amor. Forn'      '10' ' ' ' ' ' ',
       14 ' '                        ' '               'GT_OUTTAB' 'TP_PROV_F'         'Tp. Prov. Forn'      '10' ' ' ' ' ' ',

       15 'T001'                     'BUKRS'           'GT_OUTTAB' 'BUKRS_C'           'Emp.02'              '06' ' ' ' ' ' ',
       16 ' '                        ' '               'GT_OUTTAB' 'GSBER_C'           'Div.Emp.02'          '10' ' ' ' ' ' ',
       17 'KNA1'                     'KUNNR'           'GT_OUTTAB' 'KUNNR'             'Cliente'             '10' ' ' ' ' ' ',
       17 'KNA1'                     'NAME1'           'GT_OUTTAB' 'NAME1_C'           'Descrição'           '20' ' ' ' ' ' ',
       18 'SKA1'                     'SAKNR'           'GT_OUTTAB' 'HKONT_C'           'Conta Razão Cli'     '15' ' ' ' ' ' ',
       19 ' '                        ' '               'GT_OUTTAB' 'UMSKZ_C'           'Razão Esp. Cli'      '15' ' ' ' ' ' ',
       20 ' '                        ' '               'GT_OUTTAB' 'TP_CAPI_C'         'Tp. Cap. Cli'        '10' ' ' ' ' ' ',
       21 ' '                        ' '               'GT_OUTTAB' 'TP_AMOR_C'         'Tp. Amor. Cli'       '10' ' ' ' ' ' ',
       22 ' '                        ' '               'GT_OUTTAB' 'TP_PROV_C'         'Tp. Prov. Cli'       '10' ' ' ' ' ' ',

       23 ' '                        ' '               'GT_OUTTAB' 'DATA_ATUAL'        'Data'                '08' ' ' ' ' ' ',
       24 ' '                        ' '               'GT_OUTTAB' 'HORA_ATUAL'        'Hora'                '08' ' ' ' ' ' ',
       25 ' '                        ' '               'GT_OUTTAB' 'USUARIO'           'Usuario'             '15' ' ' ' ' ' '.
ENDFORM.

FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN)
                            VALUE(P_EDIT)
                            VALUE(P_SUM)
                            VALUE(P_EMPHASIZE).

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.
  W_FIELDCATALOG-KEY           = ' '.
  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS         = P_COL_POS.
  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN      = P_OUTPUTLEN.
  ENDIF.
  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  IF P_FIELD EQ 'CHECKBOX'.
    W_FIELDCATALOG-CHECKBOX = C_X.
  ENDIF.

  IF P_FIELD EQ 'KOSTL_C'.
    W_FIELDCATALOG-CHECKBOX = C_X.
  ENDIF.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE SY-UCOMM.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.

    WHEN 'CHECK_PER'.
      "Marcar o campo perc taxa CDI.
*      CLEAR: WG_ZFITAXCTR-PERC_TAXA_CDI.
*      IF WG_ZFITAXCTR-JROS_CDI_TX IS NOT INITIAL.
*        LOOP AT SCREEN.
*          IF SCREEN-NAME EQ 'WG_ZFITAXCTR-PERC_TAXA_CDI'.
*            SCREEN-INPUT = '1'.
*            SCREEN-INVISIBLE = 1.
*            SCREEN-ACTIVE = 0.
*            SCREEN-REQUIRED = 0.
*          ENDIF.
*          MODIFY SCREEN.
*        ENDLOOP.
*      ENDIF.

    WHEN 'GRAVAR'.
      PERFORM F_VERIFICA_ERROS.
      IF TG_MSG_RET[] IS INITIAL.
        CLEAR WG_ACAO.
        WG_ZFITAXCTR-DATA_ATUAL  = SY-DATUM.
        WG_ZFITAXCTR-HORA_ATUAL  = SY-UZEIT.
        WG_ZFITAXCTR-USUARIO     = SY-UNAME.
        MODIFY ZFITAXCTR FROM WG_ZFITAXCTR.
        COMMIT WORK.
      ELSE.
        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH TEXT-E59.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            I_SCREEN      = '200'
            I_SHOW        = SPACE   "c_x
            I_REPID       = SY-REPID
            I_PRESSED_TAB = ''
            I_SET_FIELD   = 'X_FIELD'
          IMPORTING
            E_MESSAGEM    = WG_MENSAGEM
          TABLES
            IT_MSGS       = TG_MSG_RET.
      ENDIF.
    WHEN C_SHOW_MSGRE.
      PERFORM F_VERIFICA_ERROS.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          I_SCREEN      = '0200'
          I_SHOW        = C_X
          I_REPID       = SY-REPID
          I_POPUP       = 0
          I_PRESSED_TAB = ''
          I_SET_FIELD   = 'X_FIELD'
          "I_SET_CELL    = 'WG_CELL'
          "I_SET_OBJ     = 'WG_ZFITAXCTR'
        IMPORTING
          E_MESSAGEM    = WG_MENSAGEM
        TABLES
          IT_MSGS       = TG_MSG_RET.
  ENDCASE.
ENDMODULE.

FORM F_TRATA_CAMPOS  USING P_FIELD P_GROUP1 P_VALUE P_INVISIBLE.
  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.
ENDFORM.                    " F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRATA_FIELDS OUTPUT.
  LOOP AT TG_FIELDS.
    LOOP AT SCREEN.
      IF    SCREEN-NAME   EQ TG_FIELDS-CAMPO
        OR  SCREEN-GROUP1 EQ TG_FIELDS-GROUP1.

        SCREEN-INPUT     = TG_FIELDS-VALUE.
        SCREEN-INVISIBLE = TG_FIELDS-INVISIBLE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  IF X_FIELD IS NOT INITIAL.
    SET CURSOR FIELD X_FIELD.
  ENDIF.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'WG_ZFITAXCTR-PERC_CDI'.
      IF WG_ZFITAXCTR-JROS_CDI_TX IS INITIAL.
        SCREEN-INPUT  = 0.
      ELSE.
        SCREEN-INPUT  = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
