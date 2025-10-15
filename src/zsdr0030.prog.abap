*&---------------------------------------------------------------------*
*& Report  ZSDR0030
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZSDR0030.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS.
TYPES: BEGIN OF TY_SAIDA,
        MARK,
        COD_FP TYPE ZSDT0056-COD_FP,
        BEZEI  TYPE ZSDT0056-BEZEI,
        WAERS  TYPE ZSDT0056-WAERS,
        OCBOT  TYPE ZSDT0056-OCBOT,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.
*----------------------------------------------------------------------*
* Taleba interna / Variaveis
*----------------------------------------------------------------------*
DATA: TG_0056 TYPE TABLE OF ZSDT0056 WITH HEADER LINE,
      TG_0070 TYPE TABLE OF ZSDT0070 WITH HEADER LINE,
      TG_0058 TYPE TABLE OF ZSDT0058 WITH HEADER LINE,
      TG_0059 TYPE TABLE OF ZSDT0059 WITH HEADER LINE,
      TG_SAIDA TYPE TABLE OF TY_SAIDA WITH HEADER LINE.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_COD_FP FOR TG_0056-COD_FP.
SELECTION-SCREEN: END OF BLOCK B1.
*----------------------------------------------------------------------*
* start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM SELECIONA_DADOS.
  PERFORM ORGANIZAR_DADOS.
  PERFORM INICIAR_VARIAVEIS.
  PERFORM IMPRIMIR_DADOS.
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
  WL_LAYOUT-BOX_TABNAME = 'TG_SAIDA'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = V_REPORT
      I_CALLBACK_PF_STATUS_SET = 'X_PF_STATUS_SET'
      I_CALLBACK_USER_COMMAND  = 'XUSER_COMMAND' "sem 2º click
      IT_FIELDCAT              = ESTRUTURA[]
      IS_LAYOUT                = WL_LAYOUT
      I_SAVE                   = 'A'
      IT_EVENTS                = EVENTS
      IS_PRINT                 = T_PRINT
    TABLES
      T_OUTTAB                 = TG_SAIDA.



ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.

  PERFORM F_CARREGAR_EVENTOS USING:
* para tira duplo click          SLIS_EV_USER_COMMAND 'XUSER_COMMAND',
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.


ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.

  PERFORM MONTAR_ESTRUTURA USING:
        1  'ZSDT0056'   'COD_FP'     'TG_SAIDA' 'COD_FP'  ' '  ' ' ,
        2  'ZSDT0056'   'BEZEI'      'TG_SAIDA' 'BEZEI'   ' '  ' ' ,
        3  'ZSDT0056'   'WAERS'      'TG_SAIDA' 'WAERS'   ' '  ' ' ,
        4  'ZSDT0056'   'OCBOT'      'TG_SAIDA' 'OCBOT'   ' '  ' ' .


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR WA_ESTRUTURA.

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*---------------------------------------------------------------------*
*       FORM X_PF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM X_PF_STATUS_SET USING UCOMM .                          "#EC CALLED

  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "X_TOP_PAGE
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING E_UCOMM
                         E_STABLE.                          "#EC CALLED

  DATA: WL_INPUT_0070 TYPE ZSDT0070.

  CASE E_UCOMM.
    WHEN 'SAVE'.
      LOOP AT TG_SAIDA
        WHERE MARK IS NOT INITIAL.
        READ TABLE TG_0056
          WITH KEY COD_FP = TG_SAIDA-COD_FP.
        IF SY-SUBRC IS INITIAL.
          MOVE: TG_0056-PRECO      TO WL_INPUT_0070-PRECO,
                TG_0056-TIPO_CALC  TO WL_INPUT_0070-TIPO_CALC,
                TG_0056-C_DECIMAIS TO WL_INPUT_0070-C_DECIMAIS,
                TG_0056-VLR_FIXO   TO WL_INPUT_0070-VLR_FIXO,
                TG_0056-cod_fp     TO WL_INPUT_0070-cod_fp,
                'PRECO'            TO WL_INPUT_0070-FIELD.

          MODIFY ZSDT0070 FROM WL_INPUT_0070.

          UPDATE ZSDT0058 SET FIELD     = WL_INPUT_0070-FIELD
                 WHERE COD_FP = TG_0056-COD_FP.

          UPDATE ZSDT0059 SET FIELD = WL_INPUT_0070-FIELD
                 WHERE COD_FP = TG_0056-COD_FP.
        ENDIF.

      ENDLOOP.
      COMMIT WORK AND WAIT.
  ENDCASE.

ENDFORM. "XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.

  V_REPORT = SY-REPID.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  SELECT *
    FROM ZSDT0056
    INTO TABLE TG_0056
      WHERE COD_FP IN S_COD_FP.
  IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM ZSDT0070
      INTO TABLE TG_0070
       FOR ALL ENTRIES IN TG_0056
        WHERE COD_FP EQ TG_0056-COD_FP.

    LOOP AT TG_0070.
      DELETE TG_0056 WHERE COD_FP EQ TG_0070-COD_FP.

    ENDLOOP.
    IF TG_0056[] IS NOT INITIAL.
      SELECT *
        FROM ZSDT0058
        INTO TABLE TG_0058
         FOR ALL ENTRIES IN TG_0056
         WHERE COD_FP EQ TG_0056-COD_FP.

      SELECT *
        FROM ZSDT0059
        INTO TABLE TG_0059
         FOR ALL ENTRIES IN TG_0056
          WHERE COD_FP EQ TG_0056-COD_FP.

    ENDIF.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZAR_DADOS .

  LOOP AT TG_0056.
    MOVE-CORRESPONDING: TG_0056 TO TG_SAIDA.
    APPEND TG_SAIDA.
    CLEAR: TG_SAIDA.
  ENDLOOP.
ENDFORM.                    " ORGANIZAR_DADOS
