*&---------------------------------------------------------------------*
*& Report  ZPMR0017
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZPMR0017.

TABLES: CAUFVD, RKPF, RESB.
*&---------------------------------------------------------------------*
*&      TYPES
*&---------------------------------------------------------------------*

TYPES: BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX.
TYPES: END OF TY_MAKT.

TYPES: BEGIN OF TY_ESTOQUE_MARD,
         MATNR TYPE MAKT-MATNR,
         WERKS TYPE RESB-WERKS,
         LGORT TYPE RESB-LGORT,
         LABST TYPE MARD-LABST,
         LGPBE TYPE MARD-LGPBE.
TYPES: END OF TY_ESTOQUE_MARD.

TYPES: BEGIN OF TY_ESTOQUE_MCHB,
         MATNR TYPE MCHB-MATNR,
         WERKS TYPE MCHB-WERKS,
         LGORT TYPE MCHB-LGORT,
         CHARG TYPE MCHB-CHARG,
         CLABS TYPE MCHB-CLABS.
TYPES:END OF TY_ESTOQUE_MCHB.

TYPES: BEGIN OF TY_LOCALIZACAO,
         MATNR TYPE LQUA-MATNR,
         WERKS TYPE LQUA-WERKS,
         LGORT TYPE LQUA-LGORT,
         LGPLA TYPE LQUA-LGPLA.
TYPES:END OF TY_LOCALIZACAO.

TYPES: BEGIN OF TY_LOCALIZACAO_FIXA,
         MATNR TYPE MLGT-MATNR,        " Nº do material
         LGNUM TYPE MLGT-LGNUM,        " Nºdepósito/complexo de depósito
         LGTYP TYPE MLGT-LGTYP,        " Tipo de depósito
         LVORM TYPE MLGT-LVORM,        " Marcação p/eliminar todos os dados mat.de um tipo depósito
         LGPLA TYPE MLGT-LGPLA.        " Posição no depósito
TYPES:END OF TY_LOCALIZACAO_FIXA.

TYPES: BEGIN OF TY_T320,
         WERKS TYPE T320-WERKS,
         LGORT TYPE T320-LGORT,
         LGNUM TYPE T320-LGNUM.
TYPES:END OF TY_T320.

TYPES: BEGIN OF TY_AFVC,
         AUFNR TYPE AFKO-AUFNR,
         AUFPL TYPE AFVC-AUFPL,
         VORNR TYPE AFVC-VORNR,
         LTXA1 TYPE AFVC-LTXA1.
TYPES:END OF TY_AFVC.

TYPES: BEGIN OF TY_RESB,
         AUFNR TYPE CAUFV-AUFNR,
         AUTYP TYPE CAUFV-AUTYP,
         OBJNR TYPE CAUFV-OBJNR,
         VORNR TYPE RESB-VORNR,
         RSNUM TYPE RESB-RSNUM,
         BDMNG TYPE RESB-BDMNG,
         ENMNG TYPE RESB-ENMNG,
         MATNR TYPE RESB-MATNR,
         WERKS TYPE RESB-WERKS,
         KZEAR TYPE RESB-KZEAR,
       END OF TY_RESB.

TYPES: BEGIN OF TY_RKPF,
         AUFNR TYPE RKPF-AUFNR,
         RSNUM TYPE RKPF-RSNUM,
         RSPOS TYPE RESB-RSPOS,
         WERKS TYPE RESB-WERKS,
       END OF TY_RKPF.

TYPES: BEGIN OF TY_JEST ,             " Status da Ordem
         OBJNR TYPE JEST-OBJNR,   " Objeto
         STAT  TYPE JEST-STAT,    " Nº do objeto
         TXT04 TYPE TJ02T-TXT04,  " Descrição dos status
       END OF TY_JEST.

TYPES: BEGIN OF TY_DOCS,
         RSNUM   TYPE ZMMT0096-RSNUM,
         RSPOS   TYPE ZMMT0096-RSPOS,
         PERNR   TYPE ZMMT0096-PERNR,
         DATA    TYPE ZMMT0096-DATA,
         HORA    TYPE ZMMT0096-HORA,
         QTDSOL  TYPE ZMMT0096-QTDSOL,
         USUARIO TYPE ZMMT0096-USUARIO,
       END OF TY_DOCS.

DATA: WA_FCAT_LVC   TYPE LVC_S_FCAT,
      LT_FCAT_LVC   TYPE LVC_T_FCAT,
      LT_FCAT_LVC2  TYPE LVC_T_FCAT,
      T_DATA        TYPE REF TO DATA,
      T_DATA2       TYPE REF TO DATA,
      T_DATA3       TYPE REF TO DATA,
      WA_CELLCOLORS TYPE LINE OF LVC_T_SCOL.

CONSTANTS C_X               TYPE C VALUE 'X'.

*Class definition for ALV toolbar
CLASS:  LCL_ALV_TOOLBAR     DEFINITION DEFERRED.
*Declaration for toolbar buttons
DATA: TY_TOOLBAR TYPE STB_BUTTON.

DATA: WA_LAYOUT TYPE LVC_S_LAYO,
      WA_STABLE TYPE LVC_S_STBL.

DATA: G_CONTAINER          TYPE SCRFNAME VALUE 'CC_REL',
      G_CONTAINER2         TYPE SCRFNAME VALUE 'CC_IMPRESSO',
      "
      CL_CONTAINER_95      TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
      EDITCONTAINER        TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,

      G_CUSTOM_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CUSTOM_CONTA0200   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_1          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 1
      CONTAINER_2          TYPE REF TO CL_GUI_CONTAINER,       "splitter conteiner 2
      SPLITTER             TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GRID1                TYPE REF TO CL_GUI_ALV_GRID,
      GRID2                TYPE REF TO CL_GUI_ALV_GRID,
      OBG_TOOLBAR          TYPE REF TO LCL_ALV_TOOLBAR,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      GS_VARIANT_C         TYPE DISVARIANT,
      GS_VARIANT_2         TYPE DISVARIANT.

DATA: EVENT           TYPE CNTL_SIMPLE_EVENT,
      EVENTS          TYPE CNTL_SIMPLE_EVENTS,
      TL_FILTER       TYPE LVC_T_FILT,
      WL_FILTER       TYPE LVC_S_FILT,
      TG_SELECTEDCELL TYPE LVC_T_CELL,
      WG_SELECTEDCELL TYPE LVC_S_CELL,
      TL_FUNCTION     TYPE UI_FUNCTIONS,
      WL_FUNCTION     LIKE TL_FUNCTION  WITH HEADER LINE.

*--------------------------------------------------------------------
* C L A S S E S
*--------------------------------------------------------------------
CLASS LCL_ALV_TOOLBAR DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      CONSTRUCTOR
        IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
      ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION


CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS:
      HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING
          E_OBJECT E_INTERACTIVE.

    METHODS HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING
        E_UCOMM.

    METHODS : HANDLE_DATA_CHANGED FOR EVENT DATA_CHANGED OF  CL_GUI_ALV_GRID
      IMPORTING
        ER_DATA_CHANGED.

    METHODS : ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW E_COLUMN.

    METHODS:
      ON_ONF4 FOR EVENT ONF4 OF CL_GUI_ALV_GRID
        IMPORTING E_FIELDNAME E_FIELDVALUE ES_ROW_NO ER_EVENT_DATA ET_BAD_CELLS E_DISPLAY.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*&---------------------------------------------------------------------*
*&      TABELAS & WORKAREAS
*&---------------------------------------------------------------------*
DATA:
  IT_RKPF             TYPE TABLE OF TY_RKPF,
  IT_AFVC             TYPE TABLE OF TY_AFVC,
  IT_AFKO             TYPE TABLE OF AFKO,
  WA_AFVC             TYPE TY_AFVC,
  IT_RESB             TYPE TABLE OF TY_RESB WITH HEADER LINE,
  WA_RESB             TYPE TY_RESB,
  T_JEST              TYPE TABLE OF TY_JEST WITH HEADER LINE,
  IT_AFIH             TYPE TABLE OF AFIH,
  WA_AFIH             TYPE AFIH,
  WA_AUFK             TYPE AUFK,
  WA_RKPF             TYPE RKPF,
  WA_EQKT             TYPE EQKT,
  WA_VIAUFKS          TYPE VIAUFKS,
  WA_IFLOTX           TYPE IFLOTX,
  IT_SAIDA            TYPE STANDARD TABLE OF ZPME_MATERIAIS_IMPRESSAO,
  IT_SAIDA_TELA       TYPE STANDARD TABLE OF ZPME_MATERIAIS_IMPRESSAO,
  WA_SAIDA            TYPE ZPME_MATERIAIS_IMPRESSAO, "TY_SAIDA,
  IT_MAKT             TYPE STANDARD TABLE OF TY_MAKT,
  WA_MAKT             TYPE TY_MAKT,
  IT_ESTOQUE_MARD     TYPE STANDARD TABLE OF TY_ESTOQUE_MARD,
  WA_ESTOQUE_MARD     TYPE TY_ESTOQUE_MARD,
  IT_ESTOQUE_MCHB     TYPE STANDARD TABLE OF TY_ESTOQUE_MCHB,
  WA_ESTOQUE_MCHB     TYPE TY_ESTOQUE_MCHB,
  IT_LOCALIZACAO      TYPE STANDARD TABLE OF TY_LOCALIZACAO,
  WA_LOCALIZACAO      TYPE TY_LOCALIZACAO,
  IT_LOCALIZACAO_FIXA TYPE STANDARD TABLE OF TY_LOCALIZACAO_FIXA,
  WA_LOCALIZACAO_FIXA TYPE TY_LOCALIZACAO_FIXA,
  IT_T320             TYPE STANDARD TABLE OF TY_T320,
  WA_T320             TYPE TY_T320,
  WA_HEADER           TYPE ZPME_MATERIAIS_IMPRESSAO_HDR,
  IT_ZMMT0096         TYPE TABLE OF ZMMT0096,
  WA_ZMMT0096         TYPE ZMMT0096,
  WA_DOCS             TYPE TY_DOCS,
  IT_DOCS_ALV         TYPE TABLE OF TY_DOCS.

DATA: GR_ALVGRID             TYPE REF TO CL_GUI_ALV_GRID,                     "Grid
      GC_CUSTOM_CONTROL_NAME TYPE SCRFNAME VALUE 'CC_ALV',                    "Nome Custom Container
      GR_CCONTAINER          TYPE REF TO CL_GUI_CUSTOM_CONTAINER,             "Custom Container
      GT_FIELDCAT            TYPE LVC_T_FCAT,                                 "Fieldcat
      GS_LAYOUT              TYPE LVC_S_LAYO,                                 "Layout
      IT_EXCLUDE             TYPE UI_FUNCTIONS,                               "Botões(Excluir)
      LS_EXCLUDE             TYPE UI_FUNC,                                    "Botões(Excluir)
      O_EVENT_RECEIVER       TYPE REF TO LCL_EVENT_RECEIVER.                  "Eventos

DATA: OK_UCOMM TYPE SY-UCOMM,
      C_FORM   TYPE RS38L_FNAM VALUE 'ZPMR0017',                             "Nome do Formulário
      V_TABIX  TYPE SY-TABIX. " guardar o indice


*&---------------------------------------------------------------------*
*&      TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_WERKS FOR RESB-WERKS NO INTERVALS NO-EXTENSION.

PARAMETERS: P_RSNUM TYPE RKPF-RSNUM,
            P_AUFNR TYPE RKPF-AUFNR.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-004.
PARAMETERS: RB_IMP RADIOBUTTON GROUP G1 DEFAULT 'X',
            RB_COM RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B2.



*&---------------------------------------------------------------------*
*&      TELA DE SELEÇÃO_ORDEM
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_AUFNR.
  IF P_WERKS IS INITIAL.
    MESSAGE 'Informar o centro' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM SELECAO_ORDEM.
  ENDIF.
*&---------------------------------------------------------------------*
*&      TELA DE SELEÇÃO_RESERVA
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_RSNUM.
  IF P_WERKS IS INITIAL.
    MESSAGE 'Informar o centro' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM SELECAO_RESERVA.
  ENDIF.

*&---------------------------------------------------------------------*
*&      SELEÇÃO
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF P_RSNUM IS INITIAL AND P_AUFNR IS INITIAL.
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM BUSCA_DADOS.
  PERFORM MONTA_SAIDA.

  CALL SCREEN 1001.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_RESERVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_DADOS.
  REFRESH: IT_SAIDA_TELA,
            IT_MAKT,
            IT_ESTOQUE_MARD,
            IT_ESTOQUE_MCHB,
            IT_LOCALIZACAO,
            IT_LOCALIZACAO_FIXA,
            IT_T320,
            IT_ZMMT0096.

  IF P_RSNUM IS INITIAL.
    SELECT SINGLE *
      INTO WA_RKPF
      FROM RKPF
      WHERE AUFNR EQ P_AUFNR.
  ELSE.
    SELECT SINGLE *
        INTO WA_RKPF
        FROM RKPF
        WHERE RSNUM EQ P_RSNUM.
  ENDIF.

  IF WA_RKPF IS INITIAL .
    MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ELSE .
    " Busca itens da reserva
    SELECT  *
      INTO CORRESPONDING FIELDS OF TABLE IT_SAIDA_TELA
      FROM  RESB
      WHERE RSNUM EQ WA_RKPF-RSNUM
      AND XLOEK EQ SPACE
      AND XWAOK EQ 'X'.

    SORT IT_SAIDA_TELA ASCENDING BY AUFNR VORNR.

    IF IT_SAIDA_TELA IS INITIAL.
      MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  " Busca descrição dos materiais
  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_SAIDA_TELA
    WHERE MATNR EQ IT_SAIDA_TELA-MATNR
    AND SPRAS EQ SY-LANGU.

  "Busca saldo de estoque (MARD)
  SELECT MATNR WERKS LGORT LABST LGPBE
    FROM MARD
    INTO TABLE IT_ESTOQUE_MARD
    FOR ALL ENTRIES IN IT_SAIDA_TELA
    WHERE MATNR EQ IT_SAIDA_TELA-MATNR
    AND WERKS EQ IT_SAIDA_TELA-WERKS
    AND LGORT EQ IT_SAIDA_TELA-LGORT.

  "Busca saldo de estoque (MCHB)
  SELECT MATNR WERKS LGORT CHARG CLABS
    FROM MCHB
    INTO TABLE IT_ESTOQUE_MCHB
    FOR ALL ENTRIES IN IT_SAIDA_TELA
    WHERE   MATNR EQ IT_SAIDA_TELA-MATNR
    AND     WERKS EQ IT_SAIDA_TELA-WERKS
    AND     LGORT EQ IT_SAIDA_TELA-LGORT
    AND     CHARG EQ IT_SAIDA_TELA-CHARG.

  "Busca localização dos materiais
  SELECT  MATNR WERKS LGORT LGPLA
    FROM    LQUA
    INTO TABLE IT_LOCALIZACAO
    FOR ALL ENTRIES IN IT_SAIDA_TELA
    WHERE   MATNR EQ IT_SAIDA_TELA-MATNR
    AND     WERKS EQ IT_SAIDA_TELA-WERKS
    AND     LGORT EQ IT_SAIDA_TELA-LGORT
    AND     LETYP NE SPACE.

  "Busca localização dos materiais - fixa
  SELECT MATNR LGNUM LGTYP LVORM LGPLA
    FROM MLGT
    INTO TABLE IT_LOCALIZACAO_FIXA
    FOR ALL ENTRIES IN IT_SAIDA_TELA
    WHERE MATNR EQ IT_SAIDA_TELA-MATNR
    AND   LVORM NE ABAP_TRUE.

  "Depósito WM
  SELECT WERKS LGORT LGNUM
    FROM T320
    INTO TABLE IT_T320
    FOR ALL ENTRIES IN IT_SAIDA_TELA
    WHERE LGORT EQ IT_SAIDA_TELA-LGORT.

  SELECT A~AUFNR B~AUFPL B~VORNR B~LTXA1
  FROM AFKO AS A
  INNER JOIN AFVC AS B ON B~AUFPL = A~AUFPL
  INTO TABLE IT_AFVC
  FOR ALL ENTRIES IN IT_SAIDA_TELA
  WHERE A~AUFNR EQ IT_SAIDA_TELA-AUFNR
    AND B~VORNR EQ IT_SAIDA_TELA-VORNR.

  "impressão autorizados
  SELECT *
    FROM ZMMT0096
    INTO TABLE IT_ZMMT0096
    FOR ALL ENTRIES IN IT_SAIDA_TELA
    WHERE RSNUM = IT_SAIDA_TELA-RSNUM
    AND   RSPOS = IT_SAIDA_TELA-RSPOS
    AND   MBLNR = ''.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_SAIDA .

  DATA: INDEX_LOOP TYPE I.
  SORT IT_ZMMT0096 BY RSNUM RSPOS DATA HORA.
  LOOP AT IT_SAIDA_TELA INTO WA_SAIDA.
    INDEX_LOOP = SY-TABIX.

    LOOP AT IT_ZMMT0096 INTO WA_ZMMT0096 WHERE RSNUM = WA_SAIDA-RSNUM
                                         AND   RSPOS = WA_SAIDA-RSPOS.
      ADD WA_ZMMT0096-QTDSOL TO WA_SAIDA-QTDIMP.
    ENDLOOP.

    WA_SAIDA-DIFMG = WA_SAIDA-BDMNG - WA_SAIDA-ENMNG.
    "Adiciona descrição dos materiais na tabela de saida
    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_SAIDA-MATNR.
    IF SY-SUBRC IS INITIAL.
      MOVE WA_MAKT-MAKTX TO WA_SAIDA-MAKTX.
    ENDIF.
    "Adiciona estoque dos materiais na tabela de saida
    READ TABLE IT_ESTOQUE_MCHB INTO WA_ESTOQUE_MCHB WITH KEY MATNR = WA_SAIDA-MATNR
                                                             WERKS = WA_SAIDA-WERKS
                                                             LGORT = WA_SAIDA-LGORT
                                                             CHARG = WA_SAIDA-CHARG.
    IF SY-SUBRC IS INITIAL.
      MOVE WA_ESTOQUE_MCHB-CLABS TO WA_SAIDA-ESTOQUE.
    ELSE.
      READ TABLE IT_ESTOQUE_MARD INTO WA_ESTOQUE_MARD WITH KEY MATNR = WA_SAIDA-MATNR
                                                               WERKS = WA_SAIDA-WERKS
                                                               LGORT = WA_SAIDA-LGORT.
      IF SY-SUBRC IS INITIAL.
        MOVE WA_ESTOQUE_MARD-LABST TO WA_SAIDA-ESTOQUE.
      ELSE.
        MOVE 0 TO WA_SAIDA-ESTOQUE.
      ENDIF.
    ENDIF.
    "Adiciona localização dos materiais na tabela de saida
    "Busca WM
    READ TABLE IT_T320 INTO WA_T320 WITH KEY WERKS = WA_SAIDA-WERKS
                                             LGORT = WA_SAIDA-LGORT.
    IF SY-SUBRC IS INITIAL.
      "Encontrando WM, busca a posição fixa.
      READ TABLE IT_LOCALIZACAO_FIXA INTO WA_LOCALIZACAO_FIXA WITH KEY MATNR = WA_SAIDA-MATNR
                                                                       LGNUM = WA_T320-LGNUM.
      IF SY-SUBRC IS INITIAL.
        CONCATENATE WA_SAIDA-MAT_LOC WA_LOCALIZACAO_FIXA-LGPLA INTO WA_SAIDA-MAT_LOC SEPARATED BY SPACE .
      ENDIF.
    ENDIF.
    "Caso não encontrar a localização em WM
    IF WA_SAIDA-MAT_LOC IS INITIAL.
      CLEAR WA_ESTOQUE_MARD.
      READ TABLE IT_ESTOQUE_MARD INTO WA_ESTOQUE_MARD WITH KEY MATNR = WA_SAIDA-MATNR
                                                               WERKS = WA_SAIDA-WERKS
                                                               LGORT = WA_SAIDA-LGORT.
      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-MAT_LOC = WA_ESTOQUE_MARD-LGPBE.
      ENDIF.
    ENDIF.

    READ TABLE IT_AFVC INTO WA_AFVC WITH KEY AUFNR = WA_SAIDA-AUFNR
                                             VORNR = WA_SAIDA-VORNR.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-VORNR = WA_AFVC-VORNR.
      WA_SAIDA-LTXA1 = WA_AFVC-LTXA1.
    ENDIF.

    MODIFY IT_SAIDA_TELA FROM WA_SAIDA INDEX INDEX_LOOP.
    CLEAR WA_AFVC.
  ENDLOOP.

  "Seleciona que tabela manter dependendo da seleção do radiobutton
  IF RB_IMP EQ ABAP_TRUE.
    DELETE IT_SAIDA_TELA WHERE DIFMG EQ 0.
  ELSE.
    "DO NOTHING
  ENDIF.

  "Monta Cabeçalho
  WA_HEADER-BWART = WA_RKPF-BWART.              "Tipo de Movimento
  WA_HEADER-DTEMI = SY-DATUM.                   "Data de Emissão
  WA_HEADER-HREMI = SY-UZEIT.                   "Hora de Emissão
  WA_HEADER-AUFNR = WA_RKPF-AUFNR.              "Ordem

  IF WA_HEADER-BWART EQ '261'.                  "Se a Reserva veio de Ordem

    SELECT SINGLE *
      FROM AFIH
      INTO WA_AFIH
      WHERE AUFNR EQ WA_RKPF-AUFNR.

    IF WA_AFIH IS NOT INITIAL.

      WA_HEADER-EQUNR = WA_AFIH-EQUNR.        "Equipamento

      SELECT SINGLE *
        FROM EQKT
        INTO WA_EQKT
        WHERE EQUNR EQ WA_AFIH-EQUNR.

      IF WA_EQKT IS NOT INITIAL.
        WA_HEADER-EQKTX = WA_EQKT-EQKTX.     "Descrição do Equipamento
      ENDIF.

    ENDIF.

    SELECT SINGLE *
        FROM AUFK
        INTO WA_AUFK
        WHERE AUFNR EQ WA_RKPF-AUFNR.

    IF WA_AUFK IS NOT INITIAL.
      WA_HEADER-KOSTL = WA_AUFK-KOSTL.        "Centro de custo
    ENDIF.

    SELECT SINGLE *
      FROM VIAUFKS
      INTO WA_VIAUFKS
      WHERE AUFNR EQ WA_RKPF-AUFNR.

    IF WA_VIAUFKS IS NOT INITIAL.

      SELECT SINGLE *
        FROM IFLOTX
        INTO WA_IFLOTX
        WHERE TPLNR EQ WA_VIAUFKS-TPLNR.

      IF WA_IFLOTX IS NOT INITIAL.
        WA_HEADER-PLTXT = WA_IFLOTX-PLTXT.        "Local de Instalação
      ENDIF.

    ENDIF.

  ELSE.
    WA_HEADER-KOSTL = WA_RKPF-KOSTL.
  ENDIF.

  WA_HEADER-ZUSER = SY-UNAME.
  WA_HEADER-RSNUM = WA_RKPF-RSNUM.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1001 OUTPUT.

  SET PF-STATUS 'ZIMPRIMIRREC'.
  IF RB_IMP EQ ABAP_TRUE.
    SET TITLEBAR 'T001'.
  ELSE.
    SET TITLEBAR 'T002'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001 INPUT.

  CALL METHOD GR_ALVGRID->CHECK_CHANGED_DATA.

  CASE OK_UCOMM.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      "DO NOTHING
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  DATA LT_F4       TYPE LVC_T_F4     WITH HEADER LINE.

  IF GR_ALVGRID IS INITIAL .

    CREATE OBJECT GR_CCONTAINER
      EXPORTING
        CONTAINER_NAME              = GC_CUSTOM_CONTROL_NAME
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    IF SY-SUBRC <> 0.
      "Exception
    ENDIF.

    CREATE OBJECT GR_ALVGRID
      EXPORTING
        I_PARENT          = GR_CCONTAINER
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    IF SY-SUBRC <> 0.
      "Exception
    ENDIF.

    CREATE OBJECT O_EVENT_RECEIVER.
    SET HANDLER O_EVENT_RECEIVER->HANDLE_USER_COMMAND FOR GR_ALVGRID.
    IF RB_IMP EQ ABAP_TRUE.
      SET HANDLER O_EVENT_RECEIVER->HANDLE_TOOLBAR  FOR GR_ALVGRID.
      SET HANDLER O_EVENT_RECEIVER->ON_DOUBLE_CLICK FOR GR_ALVGRID.
    ENDIF.
    SET HANDLER O_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR GR_ALVGRID.

    PERFORM PREPARE_FIELD_CATALOG.
    PERFORM PREPARE_LAYOUT.
    PERFORM EXCLUIR_BOTOES.

    CALL METHOD GR_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = GS_LAYOUT
        IT_TOOLBAR_EXCLUDING          = IT_EXCLUDE
      CHANGING
        IT_OUTTAB                     = IT_SAIDA_TELA
        IT_FIELDCATALOG               = GT_FIELDCAT
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC <> 0.
      "Exception
    ENDIF.

    CALL METHOD GR_ALVGRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.


    LT_F4-FIELDNAME = 'WEMPF'.
    LT_F4-REGISTER = 'X'.
    LT_F4-GETBEFORE = 'X'.
    LT_F4-CHNGEAFTER ='X'.
    APPEND LT_F4.

    CALL METHOD GR_ALVGRID->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = LT_F4[].

    SET HANDLER: O_EVENT_RECEIVER->ON_ONF4 FOR GR_ALVGRID.
  ELSE .
    CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
      EXCEPTIONS
        FINISHED = 1
        OTHERS   = 2.

    IF SY-SUBRC <> 0.
      "Exception
    ENDIF.

  ENDIF .

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM PREPARE_FIELD_CATALOG.

  IF RB_IMP EQ ABAP_TRUE.
    PERFORM MONTAR_FIELDCAT USING:
              '01'      'IMPRE'     'Imprimir'                   'X' ' '   ''.
  ENDIF.

  PERFORM MONTAR_FIELDCAT USING:
          '02'      'RSNUM'     'Reserva'                    ''  ''    '',
          '03'      'RSPOS'     'Item'                       ''  ''    '',
          '04'      'MATNR'     'Material'                   ''  ''    '',
          '05'      'MAKTX'     'Descrição do Material'      ''  ''    '',
          '06'      'ERFME'     'UMB'                        ''  ''    '',
          '07'      'ERFMG'     'Qtde Requisição'            ''  ''    '',
          '08'      'DIFMG'     'Qtde Pendente'              ''  ''    ''.

  IF RB_IMP EQ ABAP_TRUE.
    PERFORM MONTAR_FIELDCAT USING:
          '09'      'QTDSOL'    'Qtde Solicitada'            ''  'X'   'ZPME_MATERIAIS_IMPRESSAO',
          '10'      'QTDIMP'    'Qtde Impressa'              ''  ' '   'ZPME_MATERIAIS_IMPRESSAO'.
  ENDIF.

  PERFORM MONTAR_FIELDCAT USING:
          '11'      'ESTOQUE'   'Estoque'                    ''  ''    '',
          '12'      'WERKS'     'Centro'                     ''  ''    '',
          '13'      'LGORT'     'Depósito'                   ''  ''    '',
          '14'      'BDTER'     'Necessidade'                ''  ''    '',
          '15'      'WEMPF'     'Recebedor'                  ''  'X'   '',
          '16'      'VORNR'     'Operação '                  ''  ''    '',
          '17'      'LTXA1'     'Txt Operação'               ''  ''    ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM MONTAR_FIELDCAT  USING VALUE(P_POS)
                            VALUE(P_FNAME)
                            VALUE(P_DESC)
                            VALUE(P_CHECK)
                            VALUE(P_EDIT)
                            VALUE(P_REF_TABLE).

  DATA LS_FCAT TYPE LVC_S_FCAT.

  MOVE P_POS        TO LS_FCAT-COL_POS.
  MOVE P_FNAME      TO LS_FCAT-FIELDNAME.
  MOVE P_DESC       TO LS_FCAT-COLTEXT.
  MOVE P_DESC       TO LS_FCAT-SCRTEXT_L.
  MOVE P_DESC       TO LS_FCAT-SCRTEXT_M.
  MOVE P_DESC       TO LS_FCAT-SCRTEXT_S.
  MOVE P_CHECK      TO LS_FCAT-CHECKBOX.
  MOVE P_EDIT       TO LS_FCAT-EDIT.
  MOVE P_REF_TABLE  TO LS_FCAT-REF_TABLE.

  IF P_FNAME = 'QTDIMP'.
    LS_FCAT-HOTSPOT = C_X.
  ENDIF.

  APPEND LS_FCAT TO GT_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARE_LAYOUT .

  MOVE ABAP_TRUE TO GS_LAYOUT-ZEBRA.
  MOVE ABAP_TRUE TO GS_LAYOUT-CWIDTH_OPT.
  "gs_layout-grid_title = 'Impressão' .
  "gs_layout-smalltitle = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUIR_BOTOES .

  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_PRINT.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.
  CLEAR LS_EXCLUDE.

ENDFORM.

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
  METHOD CONSTRUCTOR.
*   Create ALV toolbar manager instance
    CREATE OBJECT C_ALV_TOOLBARMANAGER
      EXPORTING
        IO_ALV_GRID = IO_ALV_GRID.
  ENDMETHOD.                    "constructor

  METHOD ON_TOOLBAR.
    DATA: WL_DESACTIVE.

    WL_DESACTIVE = SPACE.

    TY_TOOLBAR-ICON      = ICON_DELETE_ROW.
    TY_TOOLBAR-FUNCTION  = 'DEL'.
    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

    TY_TOOLBAR-BUTN_TYPE = 5.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*   variable for Toolbar Button
    TY_TOOLBAR-ICON      = ICON_VIEW_CLOSE.
    TY_TOOLBAR-FUNCTION  = 'CLOS_MSG'.
    TY_TOOLBAR-DISABLED  = SPACE.
    TY_TOOLBAR-BUTN_TYPE = 0.
    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
    CLEAR TY_TOOLBAR.

*    CALL REORGANIZE METHOD OF TOOLBAR MANAGER TO
*    DISPLAY THE TOOLBAR
    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
      EXPORTING
        IO_ALV_TOOLBAR = E_OBJECT.
  ENDMETHOD.                    "on_toolbar

  METHOD HANDLE_USER_COMMAND.
    CASE E_UCOMM.
      WHEN 'DEL'.
        CALL METHOD GRID2->GET_SELECTED_CELLS
          IMPORTING
            ET_CELL = TG_SELECTEDCELL.

        LOOP AT TG_SELECTEDCELL INTO WG_SELECTEDCELL.
          READ TABLE IT_DOCS_ALV INTO WA_DOCS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
          DELETE FROM ZMMT0096 WHERE RSNUM = WA_DOCS-RSNUM
                               AND   RSPOS = WA_DOCS-RSPOS
                               AND   PERNR = WA_DOCS-PERNR
                               AND   DATA  = WA_DOCS-DATA
                               AND   HORA  = WA_DOCS-HORA.
          COMMIT WORK.
          DELETE IT_DOCS_ALV INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
        ENDLOOP.
        PERFORM BUSCA_DADOS.
        PERFORM MONTA_SAIDA.
    ENDCASE.

*** Método de atualização de dados na Tela
    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_TOOLBAR.

    CONSTANTS:
      "Tipos de Botão
      C_BUTTON_NORMAL           TYPE I VALUE 0,
      C_MENU_AND_DEFAULT_BUTTON TYPE I VALUE 1,
      C_MENU                    TYPE I VALUE 2,
      C_SEPARATOR               TYPE I VALUE 3,
      C_RADIO_BUTTON            TYPE I VALUE 4,
      C_CHECKBOX                TYPE I VALUE 5,
      C_MENU_ENTRY              TYPE I VALUE 6.

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.

    CLEAR LS_TOOLBAR.
    MOVE C_SEPARATOR TO LS_TOOLBAR-BUTN_TYPE.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'MARCAR'          TO LS_TOOLBAR-FUNCTION.
    MOVE  ICON_SELECT_ALL  TO LS_TOOLBAR-ICON.
    MOVE 'Marcar todos'    TO LS_TOOLBAR-QUICKINFO.
    "MOVE 'Marcar'       TO LS_TOOLBAR-TEXT.
    MOVE ' '               TO LS_TOOLBAR-DISABLED.
    APPEND LS_TOOLBAR      TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'DESMARCAR'        TO LS_TOOLBAR-FUNCTION.
    MOVE  ICON_DESELECT_ALL TO LS_TOOLBAR-ICON.
    MOVE 'Desmarcar todos'  TO LS_TOOLBAR-QUICKINFO.
    "MOVE 'Desmarcar'        TO LS_TOOLBAR-TEXT.
    MOVE ' '                TO LS_TOOLBAR-DISABLED.
    APPEND LS_TOOLBAR       TO E_OBJECT->MT_TOOLBAR.

    CLEAR LS_TOOLBAR.
    MOVE 'IMPRIMIR'     TO LS_TOOLBAR-FUNCTION.
    MOVE  ICON_PRINT    TO LS_TOOLBAR-ICON.
    MOVE 'Imprimir'     TO LS_TOOLBAR-QUICKINFO.
    "MOVE 'Imprimir'        TO LS_TOOLBAR-TEXT.
    MOVE ' '            TO LS_TOOLBAR-DISABLED.
    APPEND LS_TOOLBAR   TO E_OBJECT->MT_TOOLBAR.

  ENDMETHOD.                    "handle_toolbar

  METHOD HANDLE_USER_COMMAND.
    DATA: WR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.
    DATA: LT_ROWS  TYPE LVC_T_ROW,
          LT_INDEX TYPE  LVC_S_ROW-INDEX.

    CASE E_UCOMM.
      WHEN 'IMPRIMIR'.
        PERFORM PREPARA_SELECAO.
        PERFORM INICIA_SMARTFORM.
      WHEN 'MARCAR'.
        LOOP AT IT_SAIDA_TELA INTO WA_SAIDA.
          IF WA_SAIDA-IMPRE NE ABAP_TRUE.
            MOVE ABAP_TRUE TO WA_SAIDA-IMPRE.
          ENDIF.
          MODIFY IT_SAIDA_TELA FROM WA_SAIDA INDEX SY-TABIX.
        ENDLOOP.
        CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY.
      WHEN 'DESMARCAR'.
        LOOP AT IT_SAIDA_TELA INTO WA_SAIDA.
          IF WA_SAIDA-IMPRE EQ ABAP_TRUE.
            MOVE ABAP_FALSE TO WA_SAIDA-IMPRE.
          ENDIF.
          MODIFY IT_SAIDA_TELA FROM WA_SAIDA INDEX SY-TABIX.
        ENDLOOP.
        CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY.
    ENDCASE.
  ENDMETHOD.                    "handle_user_command

  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    "handle_data_changed

  METHOD ON_DOUBLE_CLICK.
    REFRESH IT_DOCS_ALV.
    IF E_ROW-INDEX GT 0.
      READ TABLE IT_SAIDA_TELA INTO WA_SAIDA INDEX E_ROW-INDEX.
      LOOP AT IT_ZMMT0096 INTO WA_ZMMT0096 WHERE RSNUM = WA_SAIDA-RSNUM
                                           AND   RSPOS = WA_SAIDA-RSPOS.
        MOVE-CORRESPONDING WA_ZMMT0096 TO WA_DOCS.
        APPEND WA_DOCS TO IT_DOCS_ALV.
      ENDLOOP.
      CALL SCREEN 1100 STARTING AT 030 3
                       ENDING   AT 125 21.
    ENDIF.
  ENDMETHOD.

  METHOD ON_ONF4.

  ENDMETHOD.

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM DATA_CHANGED USING P_ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  "Verifica se a quantidade solicitada é maior que a quantidade Pendente.
  DATA : LS_MOD_CELLS TYPE LVC_S_MODI,
         VL_QTDSOL    TYPE ERFMG,
         VL_DIFMG     TYPE BDMNG,
         VL_WEMPF     TYPE WEMPF,
         VL_PERNR     TYPE PERSNO.

  LOOP AT P_ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    CASE LS_MOD_CELLS-FIELDNAME.
      WHEN 'WEMPF'.
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = 'WEMPF'
          IMPORTING
            E_VALUE     = VL_WEMPF.
        IF VL_WEMPF+0(1) = '7'.
          VL_PERNR = VL_WEMPF.
          SELECT SINGLE *
            FROM PA0465
            INTO @DATA(W_PA0465)
            WHERE PERNR = @VL_PERNR.
          IF SY-SUBRC NE 0.
            CALL METHOD P_ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
              EXPORTING
                I_MSGID     = 'Z01'
                I_MSGNO     = '000'
                I_MSGTY     = 'E'
                I_MSGV1     = 'Empregado não cadastrado!'
                I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
                I_ROW_ID    = LS_MOD_CELLS-ROW_ID.
            EXIT.
          ELSE.
            VL_WEMPF = |{ VL_PERNR }|.

            CALL METHOD P_ER_DATA_CHANGED->MODIFY_CELL
              EXPORTING
                I_ROW_ID    = LS_MOD_CELLS-ROW_ID
                I_FIELDNAME = 'WEMPF'
                I_VALUE     = VL_WEMPF.
          ENDIF.
        ELSE.
          CALL METHOD P_ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
            EXPORTING
              I_MSGID     = 'Z01'
              I_MSGNO     = '000'
              I_MSGTY     = 'E'
              I_MSGV1     = 'Informe uma matricula válida!'
              I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
              I_ROW_ID    = LS_MOD_CELLS-ROW_ID.
          EXIT.
        ENDIF.

      WHEN 'QTDSOL'.
        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = 'QTDSOL'
          IMPORTING
            E_VALUE     = VL_QTDSOL.

        CALL METHOD P_ER_DATA_CHANGED->GET_CELL_VALUE
          EXPORTING
            I_ROW_ID    = LS_MOD_CELLS-ROW_ID
            I_FIELDNAME = 'DIFMG'
          IMPORTING
            E_VALUE     = VL_DIFMG.

        READ TABLE IT_SAIDA_TELA INTO WA_SAIDA INDEX LS_MOD_CELLS-ROW_ID.
        IF VL_QTDSOL = '0' OR VL_QTDSOL = ''.
          MOVE ABAP_FALSE TO WA_SAIDA-IMPRE.

        ELSEIF VL_QTDSOL > '0' OR VL_QTDSOL > ''.
          MOVE ABAP_TRUE TO WA_SAIDA-IMPRE.
        ENDIF.
        MODIFY IT_SAIDA_TELA FROM WA_SAIDA INDEX SY-TABIX.

        IF VL_QTDSOL GT VL_DIFMG.

          CLEAR WA_SAIDA.
          READ TABLE IT_SAIDA_TELA INTO WA_SAIDA INDEX LS_MOD_CELLS-ROW_ID.
          CLEAR WA_SAIDA-QTDSOL.
          MODIFY IT_SAIDA_TELA FROM WA_SAIDA INDEX LS_MOD_CELLS-ROW_ID.

          CALL METHOD P_ER_DATA_CHANGED->ADD_PROTOCOL_ENTRY
            EXPORTING
              I_MSGID     = 'Z01'
              I_MSGNO     = '000'
              I_MSGTY     = 'E'
              I_MSGV1     = 'Quantidade Solicitada maior que o permitido'
              I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
              I_ROW_ID    = LS_MOD_CELLS-ROW_ID.
          EXIT.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INICIA_SMARTFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIA_SMARTFORM .

  DATA:  VL_FM_NAME TYPE RS38L_FNAM.
  DATA   W_ANSWER(1).

  DATA   VL_WEMPF    TYPE WEMPF.
  DATA   VL_PERNR    TYPE PERSNO.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = C_FORM
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      FM_NAME            = VL_FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  CALL FUNCTION VL_FM_NAME
    EXPORTING
*     ARCHIVE_INDEX    =
*     ARCHIVE_INDEX_TAB          =
*     ARCHIVE_PARAMETERS         =
*     CONTROL_PARAMETERS         =
*     MAIL_APPL_OBJ    =
*     MAIL_RECIPIENT   =
*     MAIL_SENDER      =
*     OUTPUT_OPTIONS   =
*     USER_SETTINGS    = 'X'
      WA_HEADER        = WA_HEADER
*   IMPORTING
*     DOCUMENT_OUTPUT_INFO       =
*     JOB_OUTPUT_INFO  =
*     JOB_OUTPUT_OPTIONS         =
    TABLES
      IT_SAIDA         = IT_SAIDA
    EXCEPTIONS
      FORMATTING_ERROR = 1
      INTERNAL_ERROR   = 2
      SEND_ERROR       = 3
      USER_CANCELED    = 4
      OTHERS           = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = 'Grava essa matricula para retirada?'
      TEXT_BUTTON_1         = 'Sim'(100)
      ICON_BUTTON_1         = 'ICON_OKAY '
      TEXT_BUTTON_2         = 'Não'(101)
      ICON_BUTTON_2         = 'ICON_CANCEL'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ' '
      START_COLUMN          = 25
      START_ROW             = 6
    IMPORTING
      ANSWER                = W_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.

  IF W_ANSWER = '1'. "sim
    REFRESH IT_ZMMT0096.
    LOOP AT IT_SAIDA INTO WA_SAIDA.
*      IF WA_SAIDA-ERFMG GT WA_SAIDA-QTDIMP.
*        CONTINUE.
*      ENDIF.
      IF WA_SAIDA-WEMPF+0(1) = '7'.
        VL_PERNR = WA_SAIDA-WEMPF.
        SELECT SINGLE *
          FROM PA0465
          INTO @DATA(W_PA0465)
          WHERE PERNR = @VL_PERNR.
        IF SY-SUBRC NE 0.
          MESSAGE 'Empregado não cadastrado!' TYPE 'I'.
          REFRESH IT_ZMMT0096.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE 'Informe uma matricula válida!' TYPE 'I'.
        REFRESH IT_ZMMT0096.
        EXIT.
      ENDIF.
      WA_ZMMT0096-RSNUM   = WA_SAIDA-RSNUM.
      WA_ZMMT0096-RSPOS   = WA_SAIDA-RSPOS.
      WA_ZMMT0096-PERNR   = VL_PERNR.
      WA_ZMMT0096-DATA    = SY-DATUM.
      WA_ZMMT0096-HORA    = SY-UZEIT.
      WA_ZMMT0096-QTDSOL  = WA_SAIDA-QTDSOL.
      WA_ZMMT0096-USUARIO = SY-UNAME.
      APPEND WA_ZMMT0096 TO IT_ZMMT0096.
    ENDLOOP.
    IF IT_ZMMT0096[] IS NOT INITIAL.
      MODIFY ZMMT0096 FROM TABLE IT_ZMMT0096.
      COMMIT WORK.
    ENDIF.
    PERFORM BUSCA_DADOS.
    PERFORM MONTA_SAIDA.
    CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
      EXCEPTIONS
        FINISHED = 1
        OTHERS   = 2.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARA_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARA_SELECAO .

  IT_SAIDA = IT_SAIDA_TELA.
  DELETE IT_SAIDA
    WHERE IMPRE   NE ABAP_TRUE
       OR QTDSOL  IS INITIAL.

  IF IT_SAIDA IS INITIAL.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 1001.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECAO_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECAO_ORDEM.

  SELECT A~AUFNR A~AUTYP A~OBJNR B~VORNR B~RSNUM B~BDMNG B~ENMNG B~MATNR B~WERKS B~KZEAR
   FROM CAUFV AS A
   INNER JOIN RESB AS B ON B~AUFNR = A~AUFNR
   INNER JOIN RKPF AS C ON C~RSNUM = B~RSNUM
   APPENDING TABLE IT_RESB
   WHERE B~WERKS IN P_WERKS
    AND  B~KZEAR EQ ABAP_FALSE
     AND B~XLOEK EQ ABAP_FALSE
     AND B~BDMNG > '0'
     AND A~AUTYP EQ '30'
     AND C~KZVER EQ 'F '
     AND A~IDAT2 EQ '00000000'.


  SORT IT_RESB ASCENDING BY AUFNR.

  CHECK IT_RESB[] IS NOT  INITIAL.

  SELECT A~AUFNR A~RSNUM B~RSPOS B~WERKS
    FROM RKPF AS A
    INNER JOIN RESB AS B ON B~RSNUM = A~RSNUM
    INTO TABLE IT_RKPF
    FOR ALL ENTRIES IN IT_RESB
    WHERE A~RSNUM EQ IT_RESB-RSNUM
      AND A~KZVER EQ 'F '.

  SORT IT_RKPF ASCENDING BY AUFNR RSNUM RSPOS.

  CHECK IT_RKPF IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD    = 'AUFNR'
      DYNPPROG    = SY-REPID
      DYNPNR      = SY-DYNNR
      DYNPROFIELD = 'P_AUFNR'
      VALUE_ORG   = 'S'
    TABLES
      VALUE_TAB   = IT_RKPF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECAO_RESERVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECAO_RESERVA .

  SELECT A~AUFNR A~RSNUM B~RSPOS B~WERKS
    FROM RKPF AS A
    INNER JOIN RESB AS B ON B~RSNUM = A~RSNUM
    INTO TABLE IT_RKPF
   WHERE B~WERKS IN P_WERKS
     AND B~KZEAR EQ ABAP_FALSE
     AND B~XLOEK EQ ABAP_FALSE
     AND B~BDMNG > '0'.

  SORT IT_RKPF ASCENDING BY RSNUM RSPOS.

  CHECK IT_RKPF  IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD    = 'RSNUM'
      DYNPPROG    = SY-REPID
      DYNPNR      = SY-DYNNR
      DYNPROFIELD = 'P_RSNUM'
      VALUE_ORG   = 'S'
    TABLES
      VALUE_TAB   = IT_RKPF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1100 OUTPUT.
  SET PF-STATUS '1100'.
  SET TITLEBAR  '1100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_OBJETOS_1100 OUTPUT.
  REFRESH : EVENTS,
            TL_FILTER,
            TL_FUNCTION.

  CLEAR: WL_FUNCTION,
         WL_FILTER  ,
         EVENT    .

  WA_STABLE-ROW = 'X'.
  WA_STABLE-COL = 'X'.

  IF G_CUSTOM_CONTA0200 IS INITIAL.
    WA_LAYOUT-CWIDTH_OPT = C_X.
    WA_LAYOUT-ZEBRA       = C_X.
    WA_LAYOUT-NO_ROWMARK  = SPACE.
    WA_LAYOUT-SEL_MODE    = 'B'.
    WA_LAYOUT-BOX_FNAME   = ''.

    CREATE OBJECT G_CUSTOM_CONTA0200
      EXPORTING
        CONTAINER_NAME = G_CONTAINER2.

    CREATE OBJECT SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTA0200
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = CONTAINER_2.

    CREATE OBJECT GRID2
      EXPORTING
        I_PARENT = CONTAINER_2.

    CREATE OBJECT OBG_TOOLBAR
      EXPORTING
        IO_ALV_GRID = GRID2.

** Register event handler
    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID2.
    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID2.

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
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
    APPEND WL_FUNCTION TO TL_FUNCTION.
    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
    APPEND WL_FUNCTION TO TL_FUNCTION.

    CLEAR WA_LAYOUT-CWIDTH_OPT.
    GS_VARIANT_2-REPORT = SY-REPID.

    PERFORM F_MONTA_LAYOUT2.

    CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT           = GS_VARIANT_2
        IT_TOOLBAR_EXCLUDING = TL_FUNCTION
        IS_LAYOUT            = WA_LAYOUT
        I_DEFAULT            = 'X'
      CHANGING
        IT_FIELDCATALOG      = LT_FCAT_LVC2[]
        IT_OUTTAB            = IT_DOCS_ALV[].

    CALL METHOD GRID2->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

    CALL METHOD GRID2->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

*    SET HANDLER:
*              LCL_EVENT_HANDLER=>CATCH_HOTSPOT FOR GRID2.

*    posiciona spliter na altura x
    CALL METHOD SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 100.

  ELSE.

    CALL METHOD GRID2->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = LT_FCAT_LVC2[].

    CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MONTA_LAYOUT2 .
  REFRESH LT_FCAT_LVC2.
  PERFORM MONTA_FIELDCAT2 USING:
         'RSNUM'      'ZMMT0096'      'Reserva'   '10'       'RSNUM',
         'RSPOS'      'ZMMT0096'      'Item'      '05'       'RSPOS',
         'PERNR'      'ZMMT0096'      'Matricula' '10'       'PERNR',
         'DATA'       'ZMMT0096'      'Data'      '08'       'DATA',
         'HORA'       'ZMMT0096'      'Hora'      '08'       'HORA',
         'QTDSOL'     'ZMMT0096'      'Qtde'      '10'       'QTDSOL',
         'USUARIO'    'ZMMT0096'      'Usuário'   '10'       'USUARIO'.

ENDFORM.

FORM MONTA_FIELDCAT2 USING P_FIELD
                          P_TABREF
                          P_TEXT
                          P_OUT
                          P_REF_FIELD.
  CLEAR:  WA_FCAT_LVC.
  WA_FCAT_LVC-FIELDNAME   = P_FIELD.
  WA_FCAT_LVC-TABNAME     = '<FS_DATA>'.
  WA_FCAT_LVC-REF_TABLE   = P_TABREF.
  WA_FCAT_LVC-SELTEXT     = P_TEXT.

  WA_FCAT_LVC-SCRTEXT_M  = P_TEXT.
  WA_FCAT_LVC-SCRTEXT_L   = P_TEXT.
  WA_FCAT_LVC-SCRTEXT_S   = P_TEXT.

  WA_FCAT_LVC-OUTPUTLEN   = P_OUT.
  WA_FCAT_LVC-REF_FIELD   = P_REF_FIELD.

*inclui dados da work-área p/ tabela sem cab.
  APPEND WA_FCAT_LVC TO LT_FCAT_LVC2.

ENDFORM.                    " monta_fieldcat
