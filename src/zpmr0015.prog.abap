************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 08.05.2015                                          *
* Objetivo    ...: Atribuir materiais auxiliares x Tipo consumo        *
* Transação   ...: ZPM0004                                             *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 02.06.2015  Welgem Barbosa   Criação      08:21          DEVK947503  *
************************************************************************


REPORT  ZPMR0015.
TABLES: ZTFTPM_LUBRI, MAKT, T370FLD_T, DD07T.

TYPES:
       BEGIN OF TY_SAIDA,
          CODE_MAT  TYPE T370FLD_T-FLUID_TYPE,
          ID_MAT    TYPE MAKT-MATNR,
          MATERIAL  TYPE MAKT-MAKTX,
          CATEGORIA TYPE ZTFTPM_LUBRI-CATEGORIA,
          CAT_DESC  TYPE DD07T-DDTEXT,
       END OF TY_SAIDA.

DATA: IT_ZTFTPM_LUBRI TYPE TABLE OF ZTFTPM_LUBRI,
      IT_MAKT         TYPE TABLE OF MAKT,
      IT_MAKT1         TYPE TABLE OF MAKT,
      IT_DD07T        TYPE TABLE OF DD07T,
      IT_SAIDA        TYPE TABLE OF TY_SAIDA,
      IT_FCAT         TYPE TABLE OF LVC_S_FCAT,
      WA_CONT         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV          TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT       TYPE LVC_S_LAYO,
      WA_FCAT         TYPE LVC_S_FCAT,
      IT_SELECT_ROWS  TYPE LVC_T_ROW,
      WA_SELECT_ROWS  TYPE LVC_S_ROW,
      WA_ZTFTPM_LUBRI TYPE ZTFTPM_LUBRI,
      LW_ZTFTP_EDIT   TYPE ZTFTPM_LUBRI,
      WA_MAKT         TYPE MAKT,
      WA_MAKT1         TYPE MAKT,
      WA_DD07T        TYPE DD07T,
      WA_SAIDA        TYPE TY_SAIDA,
      C_MAT           TYPE T370FLD_T-FLUID_TYPE,
      C_CON           TYPE MAKT-MATNR,
      C_CAT           TYPE ZTFTPM_LUBRI-CATEGORIA,
      VAR_SY          TYPE C,
      CONTINUA        TYPE I.


SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_MAT  FOR T370FLD_T-FLUID_TYPE NO-EXTENSION,
                P_CONJ FOR MAKT-MATNR,
                P_CAT  FOR ZTFTPM_LUBRI-CATEGORIA NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM: SELECIONA_DADOS, Z_FIELDCAT.
  CALL SCREEN 0100.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS.

  REFRESH IT_SAIDA.

  SELECT * FROM ZTFTPM_LUBRI INTO TABLE IT_ZTFTPM_LUBRI
    WHERE CODE_MAT  IN P_MAT
    AND   CONJUNTO  IN P_CONJ
    AND   CATEGORIA IN P_CAT.

  SELECT * FROM MAKT INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_ZTFTPM_LUBRI
    WHERE SPRAS EQ SY-LANGU
    AND   MATNR EQ IT_ZTFTPM_LUBRI-CONJUNTO.

  SELECT * FROM DD07T INTO TABLE IT_DD07T
    WHERE DOMNAME EQ 'ZDOFT_CATG'
      AND DDLANGUAGE EQ SY-LANGU.

  LOOP AT IT_ZTFTPM_LUBRI INTO WA_ZTFTPM_LUBRI.

    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZTFTPM_LUBRI-CONJUNTO.
    READ TABLE IT_DD07T INTO WA_DD07T WITH KEY DOMVALUE_L = WA_ZTFTPM_LUBRI-CATEGORIA.

    WA_SAIDA-CODE_MAT  = WA_ZTFTPM_LUBRI-CODE_MAT.
    WA_SAIDA-ID_MAT    = WA_MAKT-MATNR.
    WA_SAIDA-MATERIAL  = WA_MAKT-MAKTX.
    WA_SAIDA-CATEGORIA = WA_ZTFTPM_LUBRI-CATEGORIA.
    WA_SAIDA-CAT_DESC  = WA_DD07T-DDTEXT.

    APPEND WA_SAIDA TO IT_SAIDA.

  ENDLOOP.



ENDFORM.                    " SELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  Z_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM Z_FIELDCAT .
  PERFORM Z_FEED_FIELDCAT USING:

        'CODE_MAT'   'Mat. Auxiliar' 'X' '' ''  '' '',
        'ID_MAT'     'Cod. Mat'      'X' '' 'X' '' '',
        'MATERIAL'   'Material'      'X' '' ''  '' '',
        'CATEGORIA'  'Cod. Cat'      'X' '' ''  '' '',
        'CAT_DESC'   'Categoria'     'X' '' ''  '' ''.
ENDFORM.                    " Z_FILDCAT


*&---------------------------------------------------------------------*
*&      Form  Z_FEED_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CAMPO    text
*      -->P_DESC     text
*      -->P_TAM      text
*      -->P_HOT      text
*      -->P_ZERO     text
*      -->P_SUM      text
*      -->P_EDIT     text
*----------------------------------------------------------------------*
FORM Z_FEED_FIELDCAT  USING    P_CAMPO TYPE C
                               P_DESC  TYPE C
                               P_OPT   TYPE C
                               P_HOT   TYPE C
                               P_ZERO  TYPE C
                               P_SUM   TYPE C
                               P_EDIT  TYPE C
                               .
  WA_FCAT-FIELDNAME = P_CAMPO.
  WA_FCAT-SCRTEXT_M = P_DESC.
  WA_FCAT-COL_OPT   = P_OPT.
  WA_FCAT-HOTSPOT   = P_HOT.
  WA_FCAT-NO_ZERO   = P_ZERO.
  WA_FCAT-DO_SUM    = P_SUM.
  WA_FCAT-EDIT      = P_EDIT.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " Z_FEED_FIELDCAT


*----------------------------------------------------------------------*
*  MODULE B_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE B_0100 OUTPUT.
  SET PF-STATUS 'S_0100'.
  SET TITLEBAR  'T_0100'.

  IF WA_CONT IS INITIAL.
    CREATE OBJECT WA_CONT
      EXPORTING
        CONTAINER_NAME              = 'T_0100'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    CREATE OBJECT WA_ALV
      EXPORTING
        I_PARENT          = WA_CONT
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.


    CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = WA_LAYOUT
      CHANGING
        IT_OUTTAB                     = IT_SAIDA
        IT_FIELDCATALOG               = IT_FCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC NE 0 .
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.


ENDMODULE.                 " B_0100  OUTPUT

*----------------------------------------------------------------------*
*  MODULE A_0100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE A_0100 INPUT.
  VAR_SY = SY-UCOMM.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'DEL'.
      PERFORM DELETE_DADOS.
    WHEN 'NEW'.
      PERFORM INSERIR_DADOS.
    WHEN 'CANCEL'.
      "      LEAVE TO SCREEN 0.
      LEAVE PROGRAM.
    WHEN 'EDIT'.
      PERFORM EDITAR_DADOS.
    WHEN 'COPY'.
      PERFORM COPIAR_DADOS.
    WHEN 'REF'.
      PERFORM SELECIONA_DADOS.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " A_0100  INPUT


*&---------------------------------------------------------------------*
*&      Form  INSERIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INSERIR_DADOS.
  DATA: LW_ZTFTP_INPUT TYPE ZTFTPM_LUBRI,
        VALID1 TYPE I,
        VALID2 TYPE I.

  IF CONTINUA NE 1.
    CALL SCREEN 0200 STARTING AT 10 10.
  ELSE.
    IF C_MAT IS NOT INITIAL AND C_CON IS NOT INITIAL AND C_CAT IS NOT INITIAL.
      SELECT MATNR FROM MAKT INTO TABLE IT_MAKT1 WHERE MATNR EQ C_CON.
      IF IT_MAKT1 IS NOT INITIAL.
        READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY ID_MAT = C_CON.
        VALID2 = SY-SUBRC.
        CLEAR SY-SUBRC.
        IF VALID2 NE 0.
          MOVE: C_MAT TO LW_ZTFTP_INPUT-CODE_MAT,
                C_CON TO LW_ZTFTP_INPUT-CONJUNTO,
                C_CAT TO LW_ZTFTP_INPUT-CATEGORIA.

          INSERT INTO ZTFTPM_LUBRI VALUES LW_ZTFTP_INPUT.

          COMMIT WORK.
          PERFORM: SELECIONA_DADOS, Z_FIELDCAT.
          CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
        ELSE.
          CLEAR SY-SUBRC.
          MESSAGE 'Material inválido ou já atribuído a outro material, favor verificar!' TYPE 'I'.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'Preencha todos os campos!' TYPE 'I'.
    ENDIF.
  ENDIF.
ENDFORM.                    " EDIT_LINHA



*----------------------------------------------------------------------*
*  MODULE B_0200 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE B_0200 OUTPUT.
  SET PF-STATUS 'S_0200'.
  SET TITLEBAR  'T_0200'.

  LOOP AT SCREEN.
    CASE SY-UCOMM.
      WHEN 'EDIT'.
        CASE SCREEN-NAME.
          WHEN 'C_CON' OR 'C_MAT' OR 'C_CAT'.
            SCREEN-INPUT = 0.
            MODIFY SCREEN.
        ENDCASE.
    ENDCASE.
  ENDLOOP.
ENDMODULE.                 " B_0200  OUTPUT


*----------------------------------------------------------------------*
*  MODULE A_0200 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE A_0200 INPUT.
  DATA: POP TYPE C LENGTH 150.
  DATA: TEMP TYPE MAKT-MATNR.
  DATA: CONT_C TYPE N LENGTH 3,
        CONT_F TYPE N LENGTH 3,
        CONT_O TYPE N LENGTH 3.

  CONT_C = 0.
  CONT_F = 0.
  CONT_O = 0.

  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE' OR 'COPY' OR 'NEW' OR ' '.
      CASE VAR_SY.
        WHEN 'N' OR 'C'. "N - NOVO, C - COPY
          CONTINUA = 1.
          IF C_MAT IS NOT INITIAL AND C_CON IS NOT INITIAL AND C_CAT IS NOT INITIAL.
            IF IT_SAIDA IS NOT INITIAL.
              READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY  CODE_MAT = C_MAT. "VERIFICA SE EXISTE O MATERIAL AUXILIAR NA TABELA
              IF SY-SUBRC NE 0.
                SELECT MATNR FROM MAKT INTO TABLE IT_MAKT1 WHERE MATNR EQ C_CON.
                IF IT_MAKT1 IS NOT INITIAL.
                  READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY ID_MAT = C_CON.
                  IF SY-SUBRC EQ 0.
                    TEMP = C_CON.
                    SHIFT TEMP LEFT DELETING LEADING '0'.
                    CONCATENATE 'O conjunto ' TEMP ' já esta sendo Utilizado!' INTO POP SEPARATED BY SPACE.
                    MESSAGE POP TYPE 'I'.
                    CLEAR: C_MAT, C_CON, C_CAT, CONTINUA.
                    LEAVE TO SCREEN 0.
                  ELSE.
                    PERFORM INSERIR_DADOS.
                    CLEAR: C_MAT, C_CON, C_CAT, CONTINUA.
                    LEAVE TO SCREEN 0.
                  ENDIF.
                ELSE.
                  MESSAGE 'Material INCORRETO ou NÃO cadastrado' TYPE 'I'.
                  CLEAR: C_MAT, C_CON, C_CAT, CONTINUA.
                  LEAVE TO SCREEN 0.
                ENDIF.
              ELSE.
                LOOP AT IT_SAIDA INTO WA_SAIDA.
                  IF WA_SAIDA-CODE_MAT EQ C_MAT.
                    IF WA_SAIDA-CATEGORIA EQ 'C'." Combustivel
                      CONT_C = CONT_C + 1.
                    ENDIF.
                    IF WA_SAIDA-CATEGORIA EQ 'F'. " Filtro
                      CONT_F = CONT_F + 1.
                    ENDIF.
                    IF WA_SAIDA-CATEGORIA EQ 'O'. " Óleo
                      CONT_O = CONT_O + 1.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
                CASE C_CAT.
                  WHEN 'C'.

                    IF CONT_F EQ 0 AND CONT_O EQ 0.
                      SELECT MATNR FROM MAKT INTO TABLE IT_MAKT1 WHERE MATNR EQ C_CON.
                      IF IT_MAKT1 IS NOT INITIAL.
                        READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY ID_MAT = C_CON.
                        IF SY-SUBRC EQ 0.
                          TEMP = C_CON.
                          SHIFT TEMP LEFT DELETING LEADING '0'.
                          CONCATENATE 'O conjunto ' TEMP ' já esta sendo Utilizado!' INTO POP SEPARATED BY SPACE.
                          MESSAGE POP TYPE 'I'.
                          CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                          LEAVE TO SCREEN 0.
                        ELSE.
                          PERFORM INSERIR_DADOS.
                          CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                          LEAVE TO SCREEN 0.
                        ENDIF.
                      ELSE.
                        MESSAGE 'Material não existe na Tabela MAKT' TYPE 'I'.
                        CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                        LEAVE TO SCREEN 0.
                      ENDIF.
                    ELSE.
                      MESSAGE 'O material auxiliar já atribuído a outra categoria, verificar ou corrija para inserir um novo item.' TYPE 'I'.
                    ENDIF.

                  WHEN 'F'.
                    IF CONT_C EQ 0 AND CONT_O EQ 0.
                      SELECT MATNR FROM MAKT INTO TABLE IT_MAKT1 WHERE MATNR EQ C_CON.
                      IF IT_MAKT1 IS NOT INITIAL.
                        READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY ID_MAT = C_CON.
                        IF SY-SUBRC EQ 0.
                          TEMP = C_CON.
                          SHIFT TEMP LEFT DELETING LEADING '0'.
                          CONCATENATE 'O conjunto ' TEMP ' já esta sendo Utilizado!' INTO POP SEPARATED BY SPACE.
                          MESSAGE POP TYPE 'I'.
                          CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                          LEAVE TO SCREEN 0.
                        ELSE.
                          PERFORM INSERIR_DADOS.
                          CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                          LEAVE TO SCREEN 0.
                        ENDIF.
                      ELSE.
                        MESSAGE 'Material não existe na Tabela MAKT' TYPE 'I'.
                        CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                        LEAVE TO SCREEN 0.
                      ENDIF.
                    ELSE.
                      MESSAGE 'O material auxiliar já atribuído a outra categoria, verificar ou corrija para inserir um novo item.' TYPE 'I'.
                    ENDIF.

                  WHEN 'O'.

                    IF CONT_C EQ 0 AND CONT_F EQ 0.
                      SELECT MATNR FROM MAKT INTO TABLE IT_MAKT1 WHERE MATNR EQ C_CON.
                      IF IT_MAKT1 IS NOT INITIAL.
                        READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY ID_MAT = C_CON.
                        IF SY-SUBRC EQ 0.
                          TEMP = C_CON.
                          SHIFT TEMP LEFT DELETING LEADING '0'.
                          CONCATENATE 'O conjunto ' TEMP ' já esta sendo Utilizado!' INTO POP SEPARATED BY SPACE.
                          MESSAGE POP TYPE 'I'.
                          CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                          LEAVE TO SCREEN 0.
                        ELSE.
                          PERFORM INSERIR_DADOS.
                          CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                          LEAVE TO SCREEN 0.
                        ENDIF.
                      ELSE.
                        MESSAGE 'Material não existe na Tabela MAKT' TYPE 'I'.
                        CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                        LEAVE TO SCREEN 0.
                      ENDIF.
                    ELSE.
                      MESSAGE 'O material auxiliar já atribuído a outra categoria, verificar ou corrija para inserir um novo item.' TYPE 'I'.
                    ENDIF.

                ENDCASE.

                CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
                LEAVE TO SCREEN 0.
              ENDIF.
            ELSE.
              PERFORM INSERIR_DADOS.
              CLEAR: C_MAT, C_CON, C_CAT, CONTINUA.
              LEAVE TO SCREEN 0.
            ENDIF.
          ELSE.
            MESSAGE 'Preencha todos os campos!' TYPE 'I'.
            CLEAR: C_MAT, C_CON, C_CAT, CONTINUA, CONT_C, CONT_F, CONT_O.
            LEAVE TO SCREEN 0.
          ENDIF.
          CLEAR VAR_SY.
        WHEN 'E'.
          UPDATE  ZTFTPM_LUBRI  SET CATEGORIA = C_CAT
                              WHERE CODE_MAT EQ C_MAT
                                AND CONJUNTO EQ C_CON.
          CLEAR: C_MAT, C_CON, C_CAT, CONTINUA.
          LEAVE TO SCREEN 0.
          CLEAR VAR_SY.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " A_0200  INPUT



*&---------------------------------------------------------------------*
*&      Form  DELETE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DELETE_DADOS .
  DATA: P_RESP,
          LV_MSG TYPE BAPI_MSG.

  CLEAR: IT_SELECT_ROWS[], WA_SELECT_ROWS.

  CALL METHOD WA_ALV->GET_SELECTED_ROWS
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

    IF P_RESP = 1.
      LOOP AT IT_SELECT_ROWS INTO WA_SELECT_ROWS.
        READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SELECT_ROWS-INDEX.
        DELETE FROM ZTFTPM_LUBRI
        WHERE CONJUNTO = WA_SAIDA-ID_MAT.

        REFRESH IT_SAIDA.

      ENDLOOP.
    ENDIF.
    PERFORM: SELECIONA_DADOS, Z_FIELDCAT.
  ELSE.

    MESSAGE 'Selecione uma linha para excluir' TYPE 'I'.
    PERFORM: SELECIONA_DADOS, Z_FIELDCAT.
  ENDIF.

ENDFORM.                    " DELETE_DADOS
*&---------------------------------------------------------------------*
*&      Form  EDITAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EDITAR_DADOS.


  CLEAR: IT_SELECT_ROWS[], WA_SELECT_ROWS.

  CALL METHOD WA_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECT_ROWS.
  IF IT_SELECT_ROWS[] IS NOT INITIAL.
    LOOP AT IT_SELECT_ROWS INTO WA_SELECT_ROWS.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SELECT_ROWS-INDEX.
      C_MAT = WA_SAIDA-CODE_MAT.
      C_CON = WA_SAIDA-ID_MAT.
      C_CAT = WA_SAIDA-CATEGORIA.

      CALL SCREEN 0200 STARTING AT 10 10.

    ENDLOOP.
    CLEAR: C_MAT, C_CON, C_CAT, CONTINUA.
    PERFORM: SELECIONA_DADOS, Z_FIELDCAT.
  ELSE.
    MESSAGE 'Selecione uma linha para editar!' TYPE 'I'.
  ENDIF.
ENDFORM.                    "EDITAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  COPIAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM COPIAR_DADOS .
  CLEAR: IT_SELECT_ROWS[], WA_SELECT_ROWS.

  CALL METHOD WA_ALV->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_SELECT_ROWS.
  IF IT_SELECT_ROWS[] IS NOT INITIAL.
    LOOP AT IT_SELECT_ROWS INTO WA_SELECT_ROWS.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WA_SELECT_ROWS-INDEX.
      C_MAT = WA_SAIDA-CODE_MAT.
      C_CON = WA_SAIDA-ID_MAT.
      C_CAT = WA_SAIDA-CATEGORIA.
      CALL SCREEN 0200 STARTING AT 10 10.
    ENDLOOP.
    CLEAR: C_MAT, C_CON, C_CAT, CONTINUA.
    PERFORM: SELECIONA_DADOS, Z_FIELDCAT.

  ELSE.
    MESSAGE 'Selecione uma linha para copiar!' TYPE 'I'.
  ENDIF.
ENDFORM.                    " COPIAR_DADOS
