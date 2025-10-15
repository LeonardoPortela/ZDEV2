*---------------------------------------------------------------------*
*                          O P E N S I S                              *
*  T E C H N O L O G Y   &   I N F O R M A T I O N    S E C U R I T Y *
*---------------------------------------------------------------------*
* Autor........: Issac Nolis Ohasi                                    *
* An.Funcional.: Carla Vacarrelli                                     *
* Arq.Sistema..: Issac Nolis Ohasi                                    *
* Data.........: 27.06.2007                                           *
* Versão.......: 1.0                                                  *
* Transação....: ZOPNSS_SURVEY                                        *
* Função.......: Listagem de Perfis e Transações por Usuário          *
*---------------------------------------------------------------------*
*                  Descrição das alterações                           *
*---------------------------------------------------------------------*
* Vers |  Data      | Descrição                             | Autor   *
*---------------------------------------------------------------------*
* 1.0  | 18.06.2007 | Inicial                               | Issac   *
*---------------------------------------------------------------------*
REPORT  ZOPNSS_REL_SURVEY
  NO STANDARD PAGE HEADING
  LINE-SIZE 170
  LINE-COUNT 58 .

*---------------------------------------------------------------------*
* Definição das tabelas transparentes --------------------------------*
*---------------------------------------------------------------------*
TABLES: UST04,
        USR02,
        USR03,
        UST10S,
        UST12,
        TSTCT,
        AGR_1016,
        AGR_1251,
        AGR_1252,
        ADRP,
        ADCP,
        USR21,
        CSKS,
        CSKT,
        TSTCA.

*---------------------------------------------------------------------*
* Definição das tabelas internas -------------------------------------*
*---------------------------------------------------------------------*
DATA: TI_UST04       LIKE UST04     OCCURS  0 WITH HEADER LINE.
DATA: TI_UST10S      LIKE UST10S    OCCURS  0 WITH HEADER LINE.
DATA: TI_UST10C      LIKE UST10C    OCCURS  0 WITH HEADER LINE.
DATA: TI_UST12       LIKE UST12     OCCURS  0 WITH HEADER LINE.
DATA: TI_UST12_AUX   LIKE UST12     OCCURS  0 WITH HEADER LINE.
DATA: TI_TSTCT       LIKE TSTCT     OCCURS  0 WITH HEADER LINE.
DATA: TI_TSTCA       LIKE TSTCA     OCCURS  0 WITH HEADER LINE.
DATA: TI_AGR_1016    LIKE AGR_1016  OCCURS  0 WITH HEADER LINE.
DATA: TI_AGR_1251    LIKE AGR_1251  OCCURS  0 WITH HEADER LINE.
DATA: TI_AGR_1252    LIKE AGR_1252  OCCURS  0 WITH HEADER LINE.
DATA: TI_USR03       LIKE USR03      OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF TI_USOBT_C OCCURS  0.
        INCLUDE STRUCTURE USOBT_C.
        DATA:   VALUE LIKE TSTCA-VALUE,
      END OF TI_USOBT_C.

DATA: TI_USOBT_C_AUX LIKE TI_USOBT_C OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF WT_SAIDA OCCURS 0,
        BNAME        LIKE TI_UST04-BNAME,
        TCODE        LIKE TI_UST12-VON,
        DESCT        LIKE TI_TSTCT-TTEXT,
        OBJCT        LIKE TI_TSTCA-OBJCT,
        FIELD        LIKE TI_TSTCA-FIELD,
        VALUE        LIKE TI_TSTCA-VALUE,
        ZIND_MANTER  LIKE ZTOPENSIS_008-ZIND_MANTER,
        ZIND_REMOVER LIKE ZTOPENSIS_008-ZIND_REMOVER,
        REMOVER_ORIG LIKE ZTOPENSIS_008-ZIND_MANTER,
        MANTER_ORIG  LIKE ZTOPENSIS_008-ZIND_REMOVER,
      END OF  WT_SAIDA.

DATA: BEGIN OF WT_SAIDA_TCODE OCCURS 0,
        BNAME LIKE TI_UST04-BNAME,
        TCODE LIKE TI_UST12-VON,
      END OF  WT_SAIDA_TCODE.

DATA: BEGIN OF WT_SAIDA_TCODE2 OCCURS 0,
        BNAME LIKE TI_UST04-BNAME,
        TCODE LIKE TI_UST12-VON,
      END OF  WT_SAIDA_TCODE2.

DATA: BEGIN OF TI_USR02  OCCURS  0,
        BNAME LIKE USR02-BNAME,
        CLASS LIKE USR02-CLASS,
      END OF TI_USR02.

DATA: TI_008_ORIGEM LIKE ZTOPENSIS_008 OCCURS 0 WITH HEADER LINE,
      TI_008        LIKE ZTOPENSIS_008 OCCURS 0 WITH HEADER LINE.


*---------------------------------------------------------------------*
* Definição de Variáveis ---------------------------------------------*
*---------------------------------------------------------------------*
DATA: WV_TCODES LIKE SY-TABIX.

DATA: W_TABIX  LIKE SY-TABIX,
      W_TABIX2 LIKE SY-TABIX,
      W_TCODE  LIKE TI_UST12-VON,
      W_LEN    TYPE I,
      W_LEN2   TYPE I,
      W_TOTAL  TYPE I,
      W_LINHA  LIKE TI_TSTCA-VALUE,
      W_VALUE  LIKE TI_TSTCA-VALUE,
      W_VALUE2 LIKE TI_TSTCA-VALUE,

      GC_LINES TYPE C LENGTH 08,
      VI_LINES TYPE I,
      GC_ERRO.

DATA: T_FIELDCAT     TYPE LVC_T_FCAT,
      T_FIELDCAT_BKP TYPE LVC_T_FCAT.

"Variáveis para o ALV
DATA: W_FIELDCAT   TYPE        LVC_S_FCAT,
      W_VARIANT    TYPE        DISVARIANT,
      CONTAINER_R1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRID_R1      TYPE REF TO CL_GUI_ALV_GRID,
      G_CONTAINER1 TYPE        SCRFNAME VALUE 'CONTAINER1',
      G_LAYOUT1    TYPE        LVC_S_LAYO,
      GS_F4        TYPE        LVC_S_F4                    , "F4
      GT_F4        TYPE        LVC_T_F4                    . "F4

"The Below Definitions Must.....
DATA: DG_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,            "Reference to document
      DG_SPLITTER    TYPE REF TO CL_GUI_SPLITTER_CONTAINER, "Reference to split container
      DG_PARENT_GRID TYPE REF TO CL_GUI_CONTAINER,          "Reference to grid container
      DG_HTML_CNTRL  TYPE REF TO CL_GUI_HTML_VIEWER,        "Reference to html container
      DG_PARENT_HTML TYPE REF TO CL_GUI_CONTAINER.          "Reference to html container
"up to here

*======================================================================*
*        C  L  A  S  S                                                 *
*======================================================================*
*======================================================================*
*  CLASS lcl_event_handler IMPLEMENTATION                              *
*======================================================================*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:
      "Overrinding Standard Functions.
      HANDLE_BEFORE_USER_COMMAND   FOR EVENT BEFORE_USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

      "Overrinding Standard Functions.
      HANDLE_AFTER_USER_COMMAND    FOR EVENT AFTER_USER_COMMAND  OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,

      TOP_OF_PAGE                  FOR EVENT TOP_OF_PAGE         OF CL_GUI_ALV_GRID
        IMPORTING E_DYNDOC_ID,

      "Barra de Ferramentas
      HANDLE_TOOLBAR1      FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID IMPORTING E_OBJECT.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_BEFORE_USER_COMMAND.

    CASE E_UCOMM.
      WHEN '&XXL' OR '&PC'.

        T_FIELDCAT_BKP[] = T_FIELDCAT[].

        LOOP AT T_FIELDCAT INTO W_FIELDCAT.
          W_FIELDCAT-CHECKBOX = ''.
          MODIFY T_FIELDCAT FROM W_FIELDCAT INDEX SY-TABIX.
        ENDLOOP.

        CALL METHOD GRID_R1->SET_FRONTEND_FIELDCATALOG
          EXPORTING
            IT_FIELDCATALOG = T_FIELDCAT.

        CALL METHOD GRID_R1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.
    ENDCASE.

  ENDMETHOD.                    "handle_before_user_command

  METHOD HANDLE_AFTER_USER_COMMAND.

    CASE E_UCOMM.
      WHEN '&XXL' OR '&PC'.
        T_FIELDCAT[] = T_FIELDCAT_BKP[].

        CALL METHOD GRID_R1->SET_FRONTEND_FIELDCATALOG
          EXPORTING
            IT_FIELDCATALOG = T_FIELDCAT[].

        CALL METHOD GRID_R1->REFRESH_TABLE_DISPLAY
          EXPORTING
            I_SOFT_REFRESH = 'X'.

    ENDCASE.

  ENDMETHOD.                    "handle_after_user_command

  METHOD TOP_OF_PAGE.                   "implementation

*    PERFORM EVENT_TOP_OF_PAGE USING DG_DYNDOC_ID.

  ENDMETHOD.                            "top_of_page

  "Chamada da barra de ferramentas (Criação)
  METHOD HANDLE_TOOLBAR1.

    PERFORM ZF_ADICIONAR_EXCLUIR_BOTOES CHANGING E_OBJECT.

  ENDMETHOD.                    "handle_toolbar

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: EVENT_RECEIVER       TYPE REF TO LCL_EVENT_RECEIVER.

*======================================================================*
*        A L V                                                         *
*======================================================================*
DATA: O_COLU        TYPE REF TO CL_SALV_COLUMN,
      O_ALV         TYPE REF TO CL_SALV_TABLE,
      O_COLUMNS     TYPE REF TO CL_SALV_COLUMNS_TABLE,
      O_COLUMN      TYPE REF TO CL_SALV_COLUMN_TABLE,
      GR_SORTS      TYPE REF TO CL_SALV_SORTS,
      GR_DISPLAY    TYPE REF TO CL_SALV_DISPLAY_SETTINGS,
      O_GRID        TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
      O_GRID_END    TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
      O_CONTENT     TYPE REF TO CL_SALV_FORM_ELEMENT,
      O_CONTENT_END TYPE REF TO CL_SALV_FORM_ELEMENT.

DATA: T_SORT  TYPE LVC_T_SORT,
      FS_SORT TYPE LVC_S_SORT.

*---------------------------------------------------------------------*
* Select-Options e Parameters ----------------------------------------*
*---------------------------------------------------------------------*
SELECT-OPTIONS: S_NAME  FOR  UST04-BNAME.
SELECT-OPTIONS: S_CLASS FOR  USR02-CLASS.
SELECT-OPTIONS: S_KOSTL FOR  USR03-KOSTL.
SELECT-OPTIONS: S_OBJCT FOR  TSTCA-OBJCT.

* FPO - 08.09.2019 - Inicio
PARAMETERS: P_WRT TYPE C RADIOBUTTON GROUP G1 DEFAULT 'X',
            P_ALV TYPE C RADIOBUTTON GROUP G1.
* FPO - 08.09.2019 - Fim

*---------------------------------------------------------------------*
* Eventos ------------------------------------------------------------*
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM IMPRIME_CABECALHO.

INITIALIZATION.

  SELECT *
    FROM ZTOPENSIS_008
    INTO TABLE TI_008_ORIGEM.

  SORT TI_008_ORIGEM BY BNAME TCODE DATA_AVALIACAO DESCENDING HORA_AVALIACAO DESCENDING.

*----------------------------------------------------------------------*
* Lógica Principal do programa ----------------------------------------*
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM SELECOES_GERAIS.

  PERFORM IMPRIME_RELATORIO.

  IF GC_ERRO IS NOT INITIAL.
    WRITE 'Não foram encontradas dados para esta selação'.
    EXIT.
  ELSE.
    DELETE WT_SAIDA WHERE NOT OBJCT IN S_OBJCT.
  ENDIF.

  PERFORM ZV_VERIFICAR_JUSTIFICATIVA.

  IF P_ALV IS NOT INITIAL.
    PERFORM ZV_LISTAR_RELAT_ALV.
  ELSE.
    PERFORM ZV_LISTAR_RELAT.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  imprime_cabecalho
*&---------------------------------------------------------------------*
*       Cabeçalho do Programa
*----------------------------------------------------------------------*
FORM IMPRIME_CABECALHO .

  CHECK WT_SAIDA[] IS NOT INITIAL AND
        P_WRT IS NOT INITIAL.

  ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: / 'Usuário:',   WT_SAIDA-BNAME, '-', ADRP-NAME_TEXT,
         AT SY-LINSZ SPACE,
         / 'Gestor.:', CSKS-VERAK, 090 'Nro Transações:', WV_TCODES,
                       AT SY-LINSZ SPACE,
         / 'CC ....:', USR02-ACCNT, '-', CSKT-LTEXT, AT SY-LINSZ SPACE,
         / 'Função.:', ADCP-FUNCTION, AT SY-LINSZ SPACE,
         / 'Depto..:', ADCP-DEPARTMENT, AT SY-LINSZ SPACE,
         / 'Tel....:', ADCP-TEL_NUMBER, AT SY-LINSZ SPACE.

  FORMAT RESET.
  ULINE.
  WRITE: " 002 'Transação',
         /123 ' Utilização  '.

  WRITE:/002 'Transação',
         023 'Descrição da Transação',
         060 'Objeto',
         071 'Campo',
         082 'Valor',
         123 ' Sim  -  Não ',
         140 'Observação'.
  WRITE:/001 SY-ULINE(170).
ENDFORM.                    " imprime_cabecalho

*&---------------------------------------------------------------------*
*&      Form  selecoes_gerais
*&---------------------------------------------------------------------*
*       Seleciona dados para o relatório
*----------------------------------------------------------------------*
FORM SELECOES_GERAIS .
  SELECT * INTO TABLE TI_UST04 FROM UST04
      WHERE BNAME IN S_NAME.


  IF SY-SUBRC IS INITIAL.

    IF NOT S_CLASS[] IS INITIAL.
      SELECT BNAME CLASS INTO TABLE TI_USR02 FROM USR02
        WHERE BNAME IN S_NAME
          AND CLASS IN S_CLASS.

      SORT TI_USR02 BY BNAME.

      LOOP AT TI_UST04.
        READ TABLE TI_USR02 WITH KEY BNAME = TI_UST04-BNAME
                   BINARY SEARCH.
        IF NOT SY-SUBRC IS INITIAL.
          DELETE TI_UST04.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT S_KOSTL[] IS INITIAL.
      SELECT * INTO TABLE TI_USR03 FROM USR03
        WHERE BNAME IN S_NAME
          AND KOSTL IN S_KOSTL.

      SORT TI_USR03 BY BNAME.

      LOOP AT TI_UST04.
        READ TABLE TI_USR03 WITH KEY BNAME = TI_UST04-BNAME
                   BINARY SEARCH.
        IF NOT SY-SUBRC IS INITIAL.
          DELETE TI_UST04.
        ENDIF.
      ENDLOOP.
    ENDIF.

*--Recupera todos os profiles do SAL ALL
    SELECT * INTO TABLE TI_UST10C FROM UST10C
      FOR ALL ENTRIES IN TI_UST04
        WHERE  PROFN EQ TI_UST04-PROFILE .

    LOOP AT TI_UST10C.
      LOOP AT TI_UST04 WHERE PROFILE = TI_UST10C-PROFN.
        MOVE: TI_UST10C-SUBPROF TO  TI_UST04-PROFILE.
        APPEND TI_UST04.
      ENDLOOP.
    ENDLOOP.

    SELECT * INTO TABLE TI_UST10S FROM UST10S
      FOR ALL ENTRIES IN TI_UST04
        WHERE  PROFN EQ TI_UST04-PROFILE AND
               OBJCT EQ 'S_TCODE'.
  ELSE.
    GC_ERRO = 'X'.
  ENDIF.


  IF NOT TI_UST10S[] IS INITIAL.
    SELECT * INTO TABLE TI_UST12 FROM UST12
      FOR ALL ENTRIES IN TI_UST10S
        WHERE AUTH  EQ TI_UST10S-AUTH AND
              OBJCT EQ 'S_TCODE'.
  ELSE.
    GC_ERRO = 'X'.
    EXIT.
  ENDIF.

  IF GC_ERRO IS NOT INITIAL.
    EXIT.
  ENDIF.

*--Descrição das Transações

  TI_UST12_AUX[] = TI_UST12[].

  SORT TI_UST12_AUX BY VON.


  DELETE ADJACENT DUPLICATES FROM TI_UST12_AUX COMPARING VON.

  LOOP AT TI_UST12_AUX.

    SELECT * APPENDING TABLE TI_TSTCT  FROM TSTCT
        WHERE SPRSL  EQ 'PT'              AND
              TCODE  EQ TI_UST12_AUX-VON.

    SELECT * APPENDING TABLE TI_TSTCA  FROM TSTCA
        WHERE TCODE  EQ TI_UST12_AUX-VON.

    SELECT * APPENDING TABLE TI_USOBT_C FROM USOBT_C
        WHERE NAME = TI_UST12_AUX-VON
          AND TYPE IN ('TR','T').

  ENDLOOP.

  SORT TI_TSTCA BY TCODE.
  SORT TI_USOBT_C BY NAME OBJECT FIELD.

  LOOP AT TI_USOBT_C.
    ON CHANGE OF TI_USOBT_C-NAME OR
                 TI_USOBT_C-OBJECT OR
                 TI_USOBT_C-FIELD.
      IF SY-TABIX > 1.
        APPEND TI_USOBT_C_AUX.
      ENDIF.

      CLEAR: TI_USOBT_C_AUX.
      TI_USOBT_C_AUX-NAME = TI_USOBT_C-NAME.
      TI_USOBT_C_AUX-OBJECT = TI_USOBT_C-OBJECT.
      TI_USOBT_C_AUX-FIELD  = TI_USOBT_C-FIELD.
    ENDON.
    CLEAR: W_VALUE.
    IF NOT TI_USOBT_C-HIGH IS INITIAL.
      CONCATENATE TI_USOBT_C-LOW '-' TI_USOBT_C-HIGH
                INTO W_VALUE SEPARATED BY SPACE.
    ELSE.
      W_VALUE = TI_USOBT_C-LOW.
    ENDIF.

    IF NOT TI_USOBT_C_AUX-VALUE IS INITIAL.
      CONCATENATE TI_USOBT_C_AUX-VALUE ';' W_VALUE
                  INTO TI_USOBT_C_AUX-VALUE.
    ELSE.
      TI_USOBT_C_AUX-VALUE = W_VALUE.
    ENDIF.
  ENDLOOP.
  IF SY-SUBRC IS INITIAL.
    APPEND TI_USOBT_C_AUX.
  ENDIF.

  TI_USOBT_C[] = TI_USOBT_C_AUX[].

  LOOP AT TI_UST04.
    SELECT * APPENDING TABLE TI_AGR_1016
      FROM AGR_1016
     WHERE PROFILE = TI_UST04-PROFILE.
  ENDLOOP.

  IF NOT TI_AGR_1016[] IS INITIAL.
    SORT TI_AGR_1016 BY AGR_NAME PROFILE.

    LOOP AT TI_AGR_1016.
      ON CHANGE OF TI_AGR_1016-AGR_NAME.
        SELECT * APPENDING TABLE TI_AGR_1251
          FROM AGR_1251
         WHERE AGR_NAME = TI_AGR_1016-AGR_NAME.

        SELECT * APPENDING TABLE TI_AGR_1252
          FROM AGR_1252
         WHERE AGR_NAME = TI_AGR_1016-AGR_NAME.
      ENDON.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " selecoes_gerais
*&---------------------------------------------------------------------*
*&      Form  imprime_relatorio
*&---------------------------------------------------------------------*
*       Montagem do relatório
*----------------------------------------------------------------------*
FORM IMPRIME_RELATORIO .

  SORT  TI_UST04 BY BNAME PROFILE.
  SORT  TI_AGR_1016 BY PROFILE.
  SORT  TI_AGR_1251 BY AGR_NAME OBJECT AUTH FIELD LOW HIGH.
  SORT  TI_AGR_1252 BY AGR_NAME VARBL LOW HIGH.
  SORT  TI_TSTCT BY TCODE.

  LOOP AT TI_UST04.
    LOOP AT TI_UST10S WHERE PROFN EQ TI_UST04-PROFILE.
      LOOP AT TI_UST12 WHERE AUTH  EQ TI_UST10S-AUTH.

        CLEAR TI_TSTCT.
        READ TABLE TI_TSTCT WITH KEY TCODE = TI_UST12-VON BINARY SEARCH.
        READ TABLE TI_USOBT_C WITH KEY NAME = TI_UST12-VON
                              BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          LOOP AT TI_USOBT_C FROM SY-TABIX.

            IF TI_USOBT_C-NAME NE TI_UST12-VON.
              EXIT.
            ENDIF.

            MOVE: TI_UST04-BNAME TO WT_SAIDA-BNAME,
                  TI_UST12-VON   TO WT_SAIDA-TCODE,
                  TI_TSTCT-TTEXT TO WT_SAIDA-DESCT,
                  TI_USOBT_C-OBJECT TO WT_SAIDA-OBJCT,
                  TI_USOBT_C-FIELD TO WT_SAIDA-FIELD.

            CLEAR: WT_SAIDA-VALUE.
            READ TABLE TI_AGR_1016
                       WITH KEY PROFILE = TI_UST04-PROFILE
                                BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              READ TABLE TI_AGR_1251
                         WITH KEY AGR_NAME = TI_AGR_1016-AGR_NAME
                                  OBJECT = TI_USOBT_C-OBJECT
                                  AUTH = TI_UST12-AUTH
                                  FIELD = TI_USOBT_C-FIELD
                                  BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                W_TABIX = SY-TABIX.
                LOOP AT TI_AGR_1251 FROM SY-TABIX.
                  IF ( TI_AGR_1251-AGR_NAME NE TI_AGR_1016-AGR_NAME ) OR
                     ( TI_AGR_1251-OBJECT NE TI_USOBT_C-OBJECT ) OR
                     ( TI_AGR_1251-AUTH NE TI_UST12-AUTH ) OR
                     ( TI_AGR_1251-FIELD NE TI_USOBT_C-FIELD ).
                    EXIT.
                  ENDIF.

                  CLEAR: W_VALUE2.
                  IF TI_AGR_1251-LOW(1) = '$'.

                    READ TABLE TI_AGR_1252
                               WITH KEY AGR_NAME = TI_AGR_1251-AGR_NAME
                                        VARBL = TI_AGR_1251-LOW
                                        BINARY SEARCH.
                    IF SY-SUBRC IS INITIAL.
                      W_TABIX2 = SY-TABIX.
                      LOOP AT TI_AGR_1252 FROM W_TABIX2.
                        IF ( TI_AGR_1252-AGR_NAME NE
                             TI_AGR_1251-AGR_NAME ) OR
                           ( TI_AGR_1252-VARBL NE
                             TI_AGR_1251-LOW ).
                          EXIT.
                        ENDIF.

                        IF NOT TI_AGR_1252-HIGH IS INITIAL.
                          CONCATENATE TI_AGR_1252-LOW '-'
                                      TI_AGR_1252-HIGH
                                      INTO W_LINHA SEPARATED BY SPACE.
                        ELSE.
                          W_LINHA = TI_AGR_1252-LOW.
                        ENDIF.

                        IF W_VALUE2 IS INITIAL.
                          W_VALUE2 = W_LINHA.
                        ELSE.
                          CONCATENATE W_VALUE2 ';' W_LINHA
                                      INTO W_VALUE2.
                        ENDIF.
                      ENDLOOP.
                    ENDIF.
                  ENDIF.

                  IF W_VALUE2 IS INITIAL.
                    IF NOT TI_AGR_1251-HIGH IS INITIAL.
                      CONCATENATE TI_AGR_1251-LOW '-' TI_AGR_1251-HIGH
                                  INTO W_LINHA SEPARATED BY SPACE.
                    ELSE.
                      W_LINHA = TI_AGR_1251-LOW.
                    ENDIF.
                    W_VALUE = W_LINHA.
                  ELSE.
                    W_VALUE = W_VALUE2.
                  ENDIF.

                  IF WT_SAIDA-VALUE IS INITIAL.
                    WT_SAIDA-VALUE = W_VALUE.
                  ELSE.
                    W_LEN = STRLEN( WT_SAIDA-VALUE ).
                    W_LEN2 = STRLEN( W_VALUE ).
                    W_TOTAL = W_LEN + W_LEN2 + 1.
                    IF W_TOTAL > 40.
                      COLLECT WT_SAIDA.
                      CLEAR: WT_SAIDA-VALUE.
                    ENDIF.
                    CONCATENATE WT_SAIDA-VALUE ';' W_VALUE
                                INTO WT_SAIDA-VALUE.

                  ENDIF.
                ENDLOOP.
                IF NOT WT_SAIDA-VALUE IS INITIAL.
                  COLLECT WT_SAIDA.
                ENDIF.
              ELSE.
                IF TI_USOBT_C-VALUE(1) = '$'.
                  CLEAR: W_VALUE2.
                  LOOP AT TI_AGR_1252
                       WHERE AGR_NAME = TI_AGR_1016-AGR_NAME
                         AND VARBL = TI_USOBT_C-VALUE.

                    CLEAR: W_VALUE.
                    IF NOT TI_AGR_1252-HIGH IS INITIAL.
                      CONCATENATE TI_AGR_1252-LOW '-' TI_AGR_1252-HIGH
                                  INTO W_VALUE SEPARATED BY SPACE.
                    ELSE.
                      W_VALUE = TI_AGR_1252-LOW.
                    ENDIF.

                    IF NOT W_VALUE2 IS INITIAL.
                      CONCATENATE W_VALUE2 ';' W_VALUE
                                  INTO W_VALUE2.
                    ELSE.
                      W_VALUE2 = W_VALUE.
                    ENDIF.
                  ENDLOOP.
                  MOVE: W_VALUE2 TO WT_SAIDA-VALUE.
                  COLLECT WT_SAIDA.
                ELSE.
                  MOVE: TI_USOBT_C-VALUE TO WT_SAIDA-VALUE.
                  COLLECT WT_SAIDA.
                ENDIF.
              ENDIF.
            ELSE.
              MOVE: TI_USOBT_C-VALUE TO WT_SAIDA-VALUE.
              COLLECT WT_SAIDA.
            ENDIF.

            MOVE: TI_UST04-BNAME TO WT_SAIDA_TCODE-BNAME,
                  TI_UST12-VON   TO WT_SAIDA_TCODE-TCODE.
            COLLECT WT_SAIDA_TCODE.
          ENDLOOP.
        ELSE.
          CLEAR: WT_SAIDA.
          MOVE: TI_UST04-BNAME TO WT_SAIDA-BNAME,
                TI_UST12-VON   TO WT_SAIDA-TCODE,
                TI_TSTCT-TTEXT TO WT_SAIDA-DESCT.
          COLLECT WT_SAIDA.

          MOVE: TI_UST04-BNAME TO WT_SAIDA_TCODE-BNAME,
                TI_UST12-VON   TO WT_SAIDA_TCODE-TCODE.
          COLLECT WT_SAIDA_TCODE.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " imprime_relatorio
*&---------------------------------------------------------------------*
*&      Form  zv_listar_relat
*&---------------------------------------------------------------------*
*       Lista o relatório
*----------------------------------------------------------------------*
FORM ZV_LISTAR_RELAT .
  DATA: LV_VERAK LIKE CSKS-VERAK.
  DATA: LV_KOSTL LIKE CSKS-KOSTL.

  SORT WT_SAIDA BY BNAME TCODE OBJCT.

  LOOP AT WT_SAIDA.
    ON CHANGE OF WT_SAIDA-BNAME
              OR WT_SAIDA-TCODE.
      IF SY-TABIX > 1.
        ULINE.
      ENDIF.
    ENDON.

    AT NEW BNAME.
      SELECT SINGLE * FROM USR21 WHERE BNAME = WT_SAIDA-BNAME.
      SELECT SINGLE * FROM USR02 WHERE BNAME = WT_SAIDA-BNAME.

      SELECT NAME_TEXT INTO ADRP-NAME_TEXT
             FROM ADRP UP TO 1 ROWS
             WHERE PERSNUMBER = USR21-PERSNUMBER.
      ENDSELECT.

      SELECT * INTO ADCP
             FROM ADCP UP TO 1 ROWS
             WHERE PERSNUMBER EQ USR21-PERSNUMBER
               AND ADDRNUMBER EQ USR21-ADDRNUMBER
               AND DATE_FROM  LE SY-DATUM
               AND DATE_TO    GT SY-DATUM.
      ENDSELECT.


      IF SY-SUBRC NE 0.
        CLEAR ADCP.
      ENDIF.

*   Obter descrição do CC

      LV_KOSTL = USR02-ACCNT.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LV_KOSTL
        IMPORTING
          OUTPUT = LV_KOSTL.


      SELECT * INTO CSKS
             FROM CSKS
             WHERE KOKRS EQ '0050'
               AND KOSTL EQ LV_KOSTL
               AND DATBI GE SY-DATUM
               AND DATAB LE SY-DATUM.
      ENDSELECT.
      IF SY-SUBRC EQ 0.
        SELECT SINGLE * INTO CSKT
               FROM CSKT
               WHERE SPRAS = SY-LANGU
                 AND KOKRS = CSKS-KOKRS
                 AND KOSTL = CSKS-KOSTL
                 AND DATBI = CSKS-DATBI.
      ELSE.
        CLEAR: CSKS, CSKT.
      ENDIF.

      REFRESH WT_SAIDA_TCODE2.
      CLEAR WV_TCODES.
      WT_SAIDA_TCODE2[] = WT_SAIDA_TCODE[].
      DELETE WT_SAIDA_TCODE2 WHERE BNAME NE WT_SAIDA-BNAME.
      DESCRIBE TABLE WT_SAIDA_TCODE2 LINES WV_TCODES.
      NEW-PAGE.
    ENDAT.

* Busca Profiles do usuário

    IF WT_SAIDA-TCODE NE W_TCODE.
      WRITE:/002 WT_SAIDA-TCODE,
             023 WT_SAIDA-DESCT.
    ELSE.
      WRITE:/002 SPACE.
    ENDIF.
    WRITE: 060 WT_SAIDA-OBJCT,
           071 WT_SAIDA-FIELD,
           082 WT_SAIDA-VALUE,
*           123 '(   )   (   )',
           125 WT_SAIDA-ZIND_MANTER,
           133 WT_SAIDA-ZIND_REMOVER,
           140 SY-ULINE(30).
    W_TCODE = WT_SAIDA-TCODE.
  ENDLOOP. " wt-saida.

ENDFORM.                    " zv_listar_relat
*&---------------------------------------------------------------------*
*&      Form  ZV_LISTAR_RELAT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZV_LISTAR_RELAT_ALV .

  CALL SCREEN 9001.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CUSTOM_ALV                                            *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM ZF_CUSTOM_ALV .

  PERFORM ZF_APPEND_FIELDCAT USING:
   "01   02         03         04  05 06 07 08  09 10  11   12  13  14  15
 'BNAME'        'WT_SAIDA' TEXT-C01 '15' '' '' '' 'L' '' ''  '05'  ' ' ' ' ' ' '' , "Usuário
 'TCODE'        'WT_SAIDA' TEXT-C02 '15' '' '' '' 'L' '' ''  '05'  ' ' ' ' ' ' '' , "Transação
 'DESCT'        'WT_SAIDA' TEXT-C03 '30' '' '' '' 'L' '' ''  '06'  ' ' ' ' ' ' '' , "Descrição Transação
 'OBJCT'        'WT_SAIDA' TEXT-C04 '10' '' '' '' 'C' '' ''  '07'  ' ' ' ' ' ' '' , "Objeto
 'FIELD'        'WT_SAIDA' TEXT-C05 '10' '' '' '' 'C' '' ''  '08'  ' ' ' ' ' ' '' , "Campo
 'VALUE'        'WT_SAIDA' TEXT-C06 '30' '' '' '' 'L' '' ''  '09'  ' ' ' ' ' ' '' , "Valor
 'ZIND_MANTER'  'WT_SAIDA' TEXT-C07 '10' '' '' '' 'C' '' ''  '09'  ' ' ' ' 'X' 'X', "Justificavel
 'ZIND_REMOVER' 'WT_SAIDA' TEXT-C08 '10' '' '' '' 'C' '' ''  '09'  ' ' ' ' 'X' 'X'. "Não Justificavel

ENDFORM.                    " ZF_CUSTOM_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.

  SET PF-STATUS '9001'.
  SET TITLEBAR  '9001'.

  PERFORM ZF_CUSTOM_ALV.
  PERFORM ZF_SORT_ALV.
  PERFORM ZF_DISPLAY_ALV.

ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.

  DATA: LC_VALID.

  CALL METHOD GRID_R1->CHECK_CHANGED_DATA.

  CASE SY-UCOMM.
    WHEN  'BACK' OR 'EXIT' OR 'CANC'.

*      PERFORM ZF_VERIFICA_SE_HOUVE_ALTERACAO CHANGING LC_VALID.

      IF LC_VALID IS NOT INITIAL.
        CLEAR LC_VALID.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            TITLEBAR              = TEXT-I01 "Log
            TEXT_QUESTION         = TEXT-I02 "Deseja realmente sair sem salvar os dados?
            TEXT_BUTTON_1         = 'Sim'
            TEXT_BUTTON_2         = 'Não'
            DEFAULT_BUTTON        = '2'
            DISPLAY_CANCEL_BUTTON = SPACE
          IMPORTING
            ANSWER                = LC_VALID.
        CHECK LC_VALID = '1'.
      ENDIF.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.

      PERFORM ZF_VALIDA_DADOS  CHANGING LC_VALID .
      IF LC_VALID IS INITIAL.
        PERFORM ZF_SALVA_ALTERACAO.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9001  INPUT
*  &---------------------------------------------------------------------*
*  &      Form  ZF_APPEND_FIELDCAT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
FORM ZF_APPEND_FIELDCAT USING P_FIELDNAME  "01-Nome do Campo
                              P_TABNAME    "02-Nome da Tabela Interna
                              P_COLTEXT    "03-Texto Coluna
                              P_OUTPUTLEN  "04-Largura da Coluna
                              P_ROLLNAME   "05-Elemento de Dados
                              P_INTTYPE    "06-Tipo de Dados
                              P_F4         "07-Ação do F4
                              P_JUST       "08-Alinhar Coluna
                              P_HOTSPOT    "09-Ação do Click
                              P_NO_OUT     "10-Não Saida no Relatório
                              P_POSITION   "11-Posição da Coluna
                              P_REF_FIELD  "12-Referencia Standard Campo
                              P_REF_TABLE  "13-Referencia Standard Tabela
                              P_CHECKBOX   "14-Checkbox
                              P_EDIT   .   "15-Edit

  CLEAR W_FIELDCAT.
  W_FIELDCAT-FIELDNAME        = P_FIELDNAME. "01-Nome do Campo
  W_FIELDCAT-TABNAME          = P_TABNAME  . "02-Nome da Tabela Interna
  W_FIELDCAT-COLTEXT          = P_COLTEXT  . "03-Texto Coluna
  W_FIELDCAT-OUTPUTLEN        = P_OUTPUTLEN. "04-Largura da Coluna
  W_FIELDCAT-ROLLNAME         = P_ROLLNAME . "05-Elemento de Dados
  W_FIELDCAT-INTTYPE          = P_INTTYPE  . "06-Typo de Dados
  W_FIELDCAT-F4AVAILABL       = P_F4       . "07-Ação do F4
  W_FIELDCAT-JUST             = P_JUST     . "08-Alinhar Coluna
  W_FIELDCAT-HOTSPOT          = P_HOTSPOT  . "09-Ação do Click
  W_FIELDCAT-NO_OUT           = P_NO_OUT   . "10-Não Saida no Relatório
  W_FIELDCAT-COL_POS          = P_POSITION . "11-Posição da Coluna
  W_FIELDCAT-REF_FIELD        = P_REF_FIELD. "12-Referencia Standard Campo
  W_FIELDCAT-REF_TABLE        = P_REF_TABLE. "13-Referencia Standard Tabela
  W_FIELDCAT-CHECKBOX         = P_CHECKBOX . "14-Checkbox
  W_FIELDCAT-EDIT             = P_EDIT.      "15-Edit

  APPEND W_FIELDCAT TO T_FIELDCAT    .

ENDFORM.                    " ZF_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONAR_EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_ADICIONAR_EXCLUIR_BOTOES CHANGING E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.

  DATA: LS_TOOLBAR  TYPE STB_BUTTON.

  DEFINE DEL_BUTTON.
    clear ls_toolbar.
    ls_toolbar-function  = &1.
    ls_toolbar-icon      = &2.
    ls_toolbar-text      = &3.
    ls_toolbar-quickinfo = &4.
    ls_toolbar-disabled  = space.
    delete e_object->mt_toolbar where function = ls_toolbar-function.
  END-OF-DEFINITION.

  DEL_BUTTON '&CHECK'             SPACE SPACE SPACE.
  DEL_BUTTON '&REFRESH'           SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&CUT'         SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&COPY'        SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&PASTE'       SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&UNDO'        SPACE SPACE SPACE.
  DEL_BUTTON '&&SEP00'            SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&APPEND'      SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&INSERT_ROW'  SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&DELETE_ROW'  SPACE SPACE SPACE.
  DEL_BUTTON '&LOCAL&COPY_ROW'    SPACE SPACE SPACE.
  DEL_BUTTON '&&SEP02'            SPACE SPACE SPACE.
  DEL_BUTTON '&PRINT_BACK'        SPACE SPACE SPACE.
  DEL_BUTTON '&DETAIL'            SPACE SPACE SPACE.
  DEL_BUTTON '&PC'                SPACE SPACE SPACE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SORT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_SORT_ALV .

  FS_SORT-SPOS      = '1'.
  FS_SORT-FIELDNAME = 'TCODE'.
  FS_SORT-UP        = 'X'.
  APPEND FS_SORT TO T_SORT.
  CLEAR FS_SORT.

*  FS_SORT-SPOS      = '2'.
*  FS_SORT-FIELDNAME = 'ZCOD_TCODE'.
*  FS_SORT-UP        = 'X'.
*  APPEND FS_SORT TO T_SORT.
*  CLEAR FS_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_DISPLAY_ALV .

  IF CONTAINER_R1 IS INITIAL.

* create a custom container control for our ALV Control
    CREATE OBJECT CONTAINER_R1
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER1'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.


**   Create object for ALV grid inside container
    CREATE OBJECT GRID_R1
      EXPORTING
        I_PARENT = CONTAINER_R1.

    G_LAYOUT1-ZEBRA       = 'X'    .
    G_LAYOUT1-SEL_MODE    = 'X'    .
    G_LAYOUT1-NO_F4       = SPACE  .
    G_LAYOUT1-STYLEFNAME  = 'CELLTAB'.
    DESCRIBE TABLE WT_SAIDA LINES VI_LINES.
    GC_LINES = VI_LINES.
    CONDENSE GC_LINES NO-GAPS.
    CONCATENATE 'Total de'
                'Registros Encontrados:'
                GC_LINES
                INTO G_LAYOUT1-GRID_TITLE  SEPARATED BY ' '.

    W_VARIANT-REPORT = SY-REPID.

    CREATE OBJECT EVENT_RECEIVER.
*    SET HANDLER EVENT_RECEIVER->HANDLE_BEFORE_USER_COMMAND FOR grid_r1. "EXCEL
*    SET HANDLER EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND  FOR grid_r1. "EXCEL
**    SET HANDLER EVENT_RECEIVER->TOP_OF_PAGE                FOR grid_r1.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR1            FOR GRID_R1.

    CALL METHOD GRID_R1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_SAVE          = 'A'
        IS_LAYOUT       = G_LAYOUT1
        IS_VARIANT      = W_VARIANT
      CHANGING
        IT_FIELDCATALOG = T_FIELDCAT[]
        IT_OUTTAB       = WT_SAIDA[]
        IT_SORT         = T_SORT.

    "Initializing document
*    CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT.

    "Set editable cells to ready for input initially
    CALL METHOD GRID_R1->SET_READY_FOR_INPUT
      EXPORTING
        I_READY_FOR_INPUT = 1.

    CALL METHOD GRID_R1->SET_TOOLBAR_INTERACTIVE.

    CALL METHOD GRID_R1->REFRESH_TABLE_DISPLAY .

  ENDIF.

ENDFORM.                    " ZF_DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_VALIDA_DADOS  CHANGING PC_VALID.

  DATA: LC_MSG TYPE C LENGTH 60.

  LOOP AT WT_SAIDA .

    CLEAR TI_008_ORIGEM.
    READ TABLE TI_008_ORIGEM WITH KEY BNAME = WT_SAIDA-BNAME
                                      TCODE = WT_SAIDA-TCODE
                                      OBJCT = WT_SAIDA-OBJCT
                                      FIELD = WT_SAIDA-FIELD.

    IF WT_SAIDA-ZIND_MANTER  IS NOT INITIAL AND
       WT_SAIDA-ZIND_REMOVER IS NOT INITIAL.
      "Não é possivel marcar Justificavel e Não Justificavel.
      LC_MSG   = TEXT-E01.
      PC_VALID = 'E'.
      EXIT.
    ELSEIF ( ( WT_SAIDA-ZIND_MANTER   IS INITIAL AND
               WT_SAIDA-ZIND_REMOVER  IS NOT INITIAL ) OR
             ( WT_SAIDA-ZIND_MANTER   IS NOT INITIAL AND
               WT_SAIDA-ZIND_REMOVER  IS INITIAL ) OR
             ( WT_SAIDA-ZIND_MANTER   IS INITIAL AND
               WT_SAIDA-MANTER_ORIG   IS NOT INITIAL ) OR
             ( WT_SAIDA-ZIND_REMOVER  IS INITIAL AND
               WT_SAIDA-REMOVER_ORIG  IS NOT INITIAL ) ) AND
           ( WT_SAIDA-ZIND_MANTER  <> WT_SAIDA-MANTER_ORIG OR
             WT_SAIDA-ZIND_REMOVER <> WT_SAIDA-REMOVER_ORIG ).
      PC_VALID = 'S'.

      MOVE-CORRESPONDING WT_SAIDA TO TI_008.
      TI_008-ZIND_MANTER    = WT_SAIDA-ZIND_MANTER .
      TI_008-ZIND_REMOVER   = WT_SAIDA-ZIND_REMOVER.
      TI_008-AVALIADOR      = SY-UNAME.
      TI_008-DATA_AVALIACAO = SY-DATUM.
      TI_008-HORA_AVALIACAO = SY-UZEIT.
      APPEND TI_008.
    ENDIF.

  ENDLOOP.

  IF PC_VALID IS INITIAL.
    "Não existe ajuste para ser salvo.
    LC_MSG   = TEXT-E02.
    PC_VALID = 'E'.
  ELSEIF PC_VALID = 'S'.
    CLEAR PC_VALID.
    EXIT.
  ENDIF.

  MESSAGE LC_MSG TYPE 'E' DISPLAY LIKE 'I'.
  FREE: TI_008[].
  CLEAR TI_008.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SALVA_ALTERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_SALVA_ALTERACAO .

  MODIFY ZTOPENSIS_008 FROM TABLE TI_008.
  COMMIT WORK AND WAIT.
  MESSAGE 'Registros salvo com sucesso' TYPE 'S'.
  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZV_VERIFICAR_JUSTIFICATIVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZV_VERIFICAR_JUSTIFICATIVA .

  DATA: LI_INDEX TYPE I.

  LOOP AT WT_SAIDA.
    LI_INDEX = SY-TABIX.
    READ TABLE TI_008_ORIGEM WITH KEY BNAME = WT_SAIDA-BNAME
                                      TCODE = WT_SAIDA-TCODE
                                      OBJCT = WT_SAIDA-OBJCT
                                      FIELD = WT_SAIDA-FIELD.
    IF SY-SUBRC = 0.
      WT_SAIDA-ZIND_MANTER  = TI_008_ORIGEM-ZIND_MANTER.
      WT_SAIDA-ZIND_REMOVER = TI_008_ORIGEM-ZIND_REMOVER.
      WT_SAIDA-MANTER_ORIG  = TI_008_ORIGEM-ZIND_MANTER.
      WT_SAIDA-REMOVER_ORIG = TI_008_ORIGEM-ZIND_REMOVER.
      MODIFY WT_SAIDA INDEX LI_INDEX.
    ENDIF.
  ENDLOOP.

ENDFORM.
