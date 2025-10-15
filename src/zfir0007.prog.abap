*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZFIR0007                                                                             *
*& Chamado        : USER STORY 140931                                                                    *
*& Data           : 24/12/2024                                                                           *
*& Especificado   : Antonio Rodrigues                                                                    *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 24/12/2024  |DEVK9A2C12  |NSEGATIN       |Desenvilvimento inicial. Chamado: 140931.                   *
*--------------------------------------------------------------------------------------------------------*
REPORT ZFIR0007.

TABLES: ZFIT0007.

*--------------------------------------------------------------------*
* T Y P E S                                                          *
*--------------------------------------------------------------------*
TYPES: BEGIN OF TY_UP_TBL,
         TABIX  TYPE SYTABIX,
         MATNR  TYPE MATNR,
         FLGMAT TYPE C,
         MWSKZ  TYPE MWSKZ,
         FLGMKZ TYPE C,
         TABIX2 TYPE SYTABIX,
         ERRO   TYPE C,
       END   OF TY_UP_TBL,

       BEGIN OF TY_0007,
         STATUS      TYPE ICON_D,
         STTS_PEDIDO TYPE STRING, "Melhorias automação Lanç Fatura de Energia #166647 - BG
         docnum      type J_1BDOCNUM. "Melhorias automação Lanç Fatura de Energia #166647 - BG
         INCLUDE STRUCTURE ZFIT0007.
TYPES: ESTILO      TYPE LVC_T_STYL,
       END   OF TY_0007,
       "Melhorias automação Lanç Fatura de Energia #166647 - BG -- INICIO
       BEGIN OF TY_EKKO,
         EBELN TYPE EBELN,
         FRGKE TYPE FRGKE,
       END OF TY_EKKO.
"Melhorias automação Lanç Fatura de Energia #166647 - BG -- FIM

*--------------------------------------------------------------------*
* I N T E R N A L  T A B L E S                                       *
*--------------------------------------------------------------------*
DATA: TG_0007   TYPE TABLE OF TY_0007,
      TG_0013   TYPE TABLE OF ZFIT0013,
      TG_UP_TBL TYPE TABLE OF TY_UP_TBL,
      TG_EKKO   TYPE TABLE OF TY_EKKO.

*--------------------------------------------------------------------*
* C L A S S   D E F I N I T I O N                                    *
*--------------------------------------------------------------------*
CLASS CLG_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
* To implement the call routine to data change event.
    METHODS: ZM_DATA_CHANGED FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
      IMPORTING ER_DATA_CHANGED
                E_UCOMM.
* To add a link type hotspot in to the ALV Grid
    METHODS: ZM_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                E_COLUMN_ID
                ES_ROW_NO.

  PRIVATE SECTION.

ENDCLASS.
*--------------------------------------------------------------------*
* C L A S S   I M P L E M E N T A T I O N S                          *
*--------------------------------------------------------------------*
CLASS CLG_EVENT_HANDLER IMPLEMENTATION.
* Handle Data Changed
  METHOD ZM_DATA_CHANGED.
* Processamento de linhas que sofreram alteração na edição.
    PERFORM ZF_DATA_CHANGED USING ER_DATA_CHANGED
                                  E_UCOMM.

  ENDMETHOD.                   "ZM_DATA_CHANGED
* Handle Toolbar
  METHOD ZM_HOTSPOT_CLICK.
* Processamento após ação do usuários nos botões.
    PERFORM ZF_HOTSPOT_CLICK USING E_ROW_ID
                                   E_COLUMN_ID
                                   ES_ROW_NO.

  ENDMETHOD.                   "ZM_TOOLBAR

ENDCLASS.                    "CLG_EVENT_HANDLER IMPLEMENTATION

* Declarações do ALV das Subtelase ALV OO.
*** Custom container instance reference
DATA: CUST_CTRL   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CLG_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.

*--------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N                                    *
*--------------------------------------------------------------------*
* Tela de Seleção
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_WERKS FOR ZFIT0007-WERKS         OBLIGATORY NO INTERVALS,
                  S_REFDT FOR ZFIT0007-REFERENCEDATE OBLIGATORY NO-EXTENSION,
                  S_DUEDT FOR ZFIT0007-DUEDATE       NO-EXTENSION. "Melhorias automação Lanç Fatura de Energia #166647 - BG

  SELECTION-SCREEN SKIP.
* Informação de Status do Documento
  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: RB_TODOS RADIOBUTTON GROUP GRP DEFAULT 'X',
                RB_PENDE RADIOBUTTON GROUP GRP,
                RB_FINAL RADIOBUTTON GROUP GRP.

  SELECTION-SCREEN END OF BLOCK B2.
SELECTION-SCREEN END OF BLOCK B1.
*--------------------------------------------------------------------*
* I N I T I A L I Z A T I O N                                        *
*--------------------------------------------------------------------*
INITIALIZATION.
* Restringe as opções da tela de seleção
  PERFORM ZF_LIMIT_SELECT_OPTION.
*--------------------------------------------------------------------*
* S T A R T - O F - S E L E C T I O N                                *
*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM: ZF_SELECT_DATA, "Selecionar dados para o processamento
           ZF_SHOW_DATA.   "Exibe os dados selecionados

*&----------------------------------------------------------------------------------*
*&      Form  ZF_LIMIT_SELECT_OPTION
*&----------------------------------------------------------------------------------*
*       Restringe as opções da tela de seleção
*-----------------------------------------------------------------------------------*
FORM ZF_LIMIT_SELECT_OPTION.

  TYPE-POOLS SSCR. "Tipo da tela de selação
* Restringe os dados do parâmetro da tela de seleção
  DATA: TL_SCREEN TYPE SSCR_RESTRICT. "Tabelda tela de seleção
* Estruturas para preencher a tab. t_screen
  DATA: EL_OPTS  TYPE SSCR_OPT_LIST, "Estrutura da restrição da lista de opções
        EL_ASSOC TYPE SSCR_ASS.      "Estrutura da lista do nome da variável restringida
  CONSTANTS: CL_OBJECTKEY1(10) TYPE C VALUE 'OBJECTKEY1'.

* Restringe o campo "Modificado em"  selection para somente EQ.
* Filial
  EL_OPTS-NAME       = CL_OBJECTKEY1.
  EL_OPTS-OPTIONS-EQ = ABAP_ON.
  EL_OPTS-OPTIONS-NE = ABAP_ON.
  EL_OPTS-OPTIONS-CP = ABAP_ON.
  APPEND EL_OPTS TO TL_SCREEN-OPT_LIST_TAB.
  EL_ASSOC-KIND      = SY-ABCDE+18(1). "S
  EL_ASSOC-NAME      = 'S_WERKS'.
  EL_ASSOC-SG_MAIN   = SY-ABCDE+8(1).  "I
  EL_ASSOC-SG_ADDY   = SPACE.
  EL_ASSOC-OP_MAIN   = CL_OBJECTKEY1.
  APPEND EL_ASSOC TO TL_SCREEN-ASS_TAB.
* Função para restringir Selection  Option
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      RESTRICTION            = TL_SCREEN
    EXCEPTIONS
      TOO_LATE               = 1
      REPEATED               = 2
      SELOPT_WITHOUT_OPTIONS = 3
      SELOPT_WITHOUT_SIGNS   = 4
      INVALID_SIGN           = 5
      EMPTY_OPTION_LIST      = 6
      INVALID_KIND           = 7
      REPEATED_KIND_A        = 8
      OTHERS                 = 9.
* Verifica de função executou com erro.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_DATA
*&---------------------------------------------------------------------*
*       Selecionar dados para o processamento
*----------------------------------------------------------------------*
FORM ZF_SELECT_DATA.

  DATA: TL_ESTILO TYPE LVC_T_STYL.

  DATA: EL_ESTILO TYPE LVC_S_STYL,
        EL_0007   TYPE TY_0007.

  SELECT * FROM ZFIT0007
    INTO CORRESPONDING FIELDS OF TABLE TG_0007
  WHERE WERKS         IN S_WERKS
    AND DUEDATE       IN S_DUEDT
    AND REFERENCEDATE IN S_REFDT.

  IF SY-SUBRC IS INITIAL.
    CASE ABAP_ON.
      WHEN RB_PENDE. "Pendentes
        DELETE TG_0007 WHERE BELNR IS NOT INITIAL.

      WHEN RB_FINAL. "Finalizados
        DELETE TG_0007 WHERE BELNR IS INITIAL.

      WHEN OTHERS.
*     Do nothing
    ENDCASE.

    IF TG_0007[] IS INITIAL.
* Não encontrados dados para esta seleção
      MESSAGE S114(PT) DISPLAY LIKE SY-ABCDE+4(1). "E
      LEAVE LIST-PROCESSING.

    ELSE.
""Melhorias automação Lanç Fatura de Energia #166647 - BG -- INICIO
      SELECT EBELN FRGKE
      FROM EKKO
      INTO TABLE TG_EKKO FOR ALL ENTRIES IN TG_0007
      WHERE EBELN EQ  TG_0007-EBELN.

      LOOP AT TG_0007 ASSIGNING FIELD-SYMBOL(<FS_0007>).
        READ TABLE TG_EKKO INTO DATA(WA_EKKO) WITH KEY EBELN = <FS_0007>-EBELN.
        IF SY-SUBRC IS INITIAL.
          IF WA_EKKO-FRGKE EQ '2'.
            <FS_0007>-STTS_PEDIDO = 'Liberado'.
          ELSE.
            <FS_0007>-STTS_PEDIDO = 'Bloqueado'.
          ENDIF.
        ENDIF.
      ENDLOOP.
""Melhorias automação Lanç Fatura de Energia #166647 - BG -- FIM

*** Definição dos campos que não serão editáveis.
* Material.
      EL_ESTILO-FIELDNAME = 'MATNR'.
      EL_ESTILO-STYLE  = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      EL_ESTILO-STYLE2 = ABAP_FALSE.
      EL_ESTILO-STYLE3 = ABAP_FALSE.
      EL_ESTILO-STYLE4 = ABAP_FALSE.
      INSERT EL_ESTILO INTO TABLE TL_ESTILO.
* Código do imposto.
      EL_ESTILO-FIELDNAME = 'MWSKZ'.
      INSERT EL_ESTILO INTO TABLE TL_ESTILO.

      APPEND LINES OF TL_ESTILO TO EL_0007-ESTILO.
      MODIFY TG_0007 FROM EL_0007 TRANSPORTING ESTILO WHERE EBELN IS NOT INITIAL.

      CLEAR: TL_ESTILO, EL_0007-ESTILO.
*** Definição dos campos que serão editáveis.
* Material.
      EL_ESTILO-FIELDNAME = 'MATNR'.
      EL_ESTILO-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      INSERT EL_ESTILO INTO TABLE TL_ESTILO.
* Código do imposto.
      EL_ESTILO-FIELDNAME = 'MWSKZ'.
      INSERT EL_ESTILO INTO TABLE TL_ESTILO.
      CLEAR EL_ESTILO.

      APPEND LINES OF TL_ESTILO TO EL_0007-ESTILO.
      MODIFY TG_0007 FROM EL_0007 TRANSPORTING ESTILO WHERE EBELN IS INITIAL.
* Verifica Status de processamento do registros das faturas.
      PERFORM ZF_CHECK_STATUS_INVOICE.

    ENDIF.

  ELSE.
* Não encontrados dados para esta seleção
    MESSAGE S114(PT) DISPLAY LIKE SY-ABCDE+4(1). "E
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SHOW_DATA
*&---------------------------------------------------------------------*
*       Exibe os dados selecionados
*----------------------------------------------------------------------*
FORM ZF_SHOW_DATA.

* Exibe dados processados - Relatório ALV
  CALL SCREEN '2000'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SHOW_DATA_ALVOO
*&---------------------------------------------------------------------*
*       Exibe os dados selecionados
*----------------------------------------------------------------------*
FORM ZF_SHOW_DATA_ALVOO.

  DATA: CLL_EVENT_HANDLER TYPE REF TO CLG_EVENT_HANDLER.

  DATA: TL_EXCLUDE TYPE UI_FUNCTIONS,
        TL_FCAT    TYPE LVC_T_FCAT.

  DATA: EL_LAYOUT  TYPE LVC_S_LAYO,
        EL_VARIANT TYPE DISVARIANT.

  CONSTANTS: CL_MATNR	 TYPE LVC_FNAME VALUE 'MATNR',
             CL_MWSKZ	 TYPE LVC_FNAME VALUE 'MWSKZ',
             CL_BUKRS	 TYPE LVC_FNAME VALUE 'BUKRS',
             CL_WERKS	 TYPE LVC_FNAME VALUE 'WERKS',
             CL_INVOI	 TYPE LVC_FNAME VALUE 'INVOICEID',
             CL_INVFL	 TYPE LVC_FNAME VALUE 'INVOICEFILES',
             CL_EBELN	 TYPE LVC_FNAME VALUE 'EBELN',
             CL_MBLNR	 TYPE LVC_FNAME VALUE 'MBLNR',
             CL_BELNR	 TYPE LVC_FNAME VALUE 'BELNR',
             CL_MANDT	 TYPE LVC_FNAME VALUE 'MANDT',
             CL_STATUS TYPE LVC_FNAME VALUE 'STATUS',
             CL_USER   TYPE LVC_FNAME VALUE 'US_CRIACAO',
             CL_DATA   TYPE LVC_FNAME VALUE 'DT_CRIACAO',
             CL_HORA   TYPE LVC_FNAME VALUE 'HR_CRIACAO',
             LC_MARA   TYPE TABNAME   VALUE 'MARA',
             LC_T007A	 TYPE TABNAME   VALUE 'T007A',
             CL_stt_ped TYPE LVC_FNAME VALUE 'STTS_PEDIDO'.



  IF ZCL_SCREEN=>ZIF_SCREEN~SET_CRIAR_TELA_PADRAO_REPORT(
    EXPORTING
       I_TITULO  = 'Relatório de Faturas de Energia'
     CHANGING
       ALV = CLG_ALVGRID
     ) EQ ABAP_TRUE.
* Configura o Layout de saída da GRID do ALV.
    EL_LAYOUT-ZEBRA      = ABAP_ON.
    EL_LAYOUT-CWIDTH_OPT = ABAP_ON.
    EL_LAYOUT-COL_OPT    = ABAP_ON.
    EL_LAYOUT-SEL_MODE   = SY-ABCDE(1). "A - Seleção Múltiplas de Linha e Coluna do ALV
    EL_LAYOUT-STYLEFNAME = 'ESTILO'.
* Configura a Variante de exibição das modificações de layout do ALV Grid
    EL_VARIANT-REPORT = SY-REPID.
* Marca os botões da Grid do ALV para serem excluídos.
    APPEND CL_GUI_ALV_GRID=>MC_FC_MAXIMUM           TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_MINIMUM           TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_AVERAGE           TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_PRINT             TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_MB_VIEW              TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_GRAPH             TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_INFO              TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_DETAIL            TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK             TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO TL_EXCLUDE.
    APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO TL_EXCLUDE.
* Configura o ALV Grid para considerar o enter.
    CALL METHOD CLG_ALVGRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
* Configura o ALV Grid para editar.
    CLG_ALVGRID->SET_READY_FOR_INPUT( 1 ). "Aberto para edição
*--Creating an instance for the event handler
    CREATE OBJECT CLL_EVENT_HANDLER.
*--Registering handler methods to handle ALV Grid events
    SET HANDLER CLL_EVENT_HANDLER->ZM_DATA_CHANGED  FOR CLG_ALVGRID.
    SET HANDLER CLL_EVENT_HANDLER->ZM_HOTSPOT_CLICK FOR CLG_ALVGRID.
* Monta o Catalogo de Campos do ALV.
    CLEAR TL_FCAT.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        I_STRUCTURE_NAME       = 'ZFIT0007'
        I_INTERNAL_TABNAME     = 'TG_0007'
      CHANGING
        CT_FIELDCAT            = TL_FCAT[]
      EXCEPTIONS
        INCONSISTENT_INTERFACE = 1
        PROGRAM_ERROR          = 2
        OTHERS                 = 3.

    READ TABLE TL_FCAT INTO DATA(EL_FCAT) INDEX 1.
    EL_FCAT-FIELDNAME = CL_STATUS.
    EL_FCAT-DATATYPE  = 'CHAR'.
    EL_FCAT-INTTYPE   = 'C'.
    EL_FCAT-INTLEN    = 4.
    EL_FCAT-DOMNAME   = 'ICON'.
    EL_FCAT-REF_TABLE = 'ICON'.
    EL_FCAT-DD_OUTLEN = 4.
    EL_FCAT-TECH      = ABAP_OFF.
    EL_FCAT-SCRTEXT_S = EL_FCAT-SCRTEXT_M = EL_FCAT-SCRTEXT_L = EL_FCAT-REPTEXT = 'Status'.
    APPEND EL_FCAT TO TL_FCAT.

    READ TABLE TL_FCAT INTO EL_FCAT INDEX 26.
    EL_FCAT-FIELDNAME = CL_STT_PED.
    EL_FCAT-DATATYPE  = 'CHAR'.
    EL_FCAT-INTTYPE   = 'C'.
    EL_FCAT-INTLEN    = 5.
    "EL_FCAT-DOMNAME   = 'ICON'.
    "EL_FCAT-REF_TABLE = 'ICON'.
    EL_FCAT-DD_OUTLEN = 10.
    EL_FCAT-TECH      = ABAP_OFF.
    EL_FCAT-SCRTEXT_S = EL_FCAT-SCRTEXT_M = EL_FCAT-SCRTEXT_L = EL_FCAT-REPTEXT = 'Stts Pedido'.
    APPEND EL_FCAT TO TL_FCAT.
    SORT TL_FCAT BY COL_POS.
* Ajusta o Catalogo de Campos do Relatório do ALV.
    LOOP AT TL_FCAT INTO EL_FCAT WHERE FIELDNAME EQ CL_MANDT
                                    OR FIELDNAME EQ CL_MATNR
                                    OR FIELDNAME EQ CL_MWSKZ
                                    OR FIELDNAME EQ CL_BUKRS
                                    OR FIELDNAME EQ CL_WERKS
                                    OR FIELDNAME EQ CL_INVOI
                                    OR FIELDNAME EQ CL_EBELN
                                    OR FIELDNAME EQ CL_MBLNR
                                    OR FIELDNAME EQ CL_BELNR
                                    OR FIELDNAME EQ CL_INVFL
                                    OR FIELDNAME EQ CL_STATUS
                                    OR FIELDNAME EQ CL_USER
                                    OR FIELDNAME EQ CL_DATA
                                    OR FIELDNAME EQ CL_HORA
                                    OR FIELDNAME EQ CL_stt_ped.
* Verifica o nome do campo do relatório ALV.
      CASE EL_FCAT-FIELDNAME.
        WHEN CL_MATNR OR CL_MWSKZ.
          EL_FCAT-F4AVAILABL = ABAP_ON.
          CASE EL_FCAT-FIELDNAME.
            WHEN CL_MATNR.
              EL_FCAT-CHECKTABLE = LC_MARA.

            WHEN CL_MWSKZ.
              EL_FCAT-CHECKTABLE = LC_T007A.

            WHEN OTHERS.
*           Do nothing
          ENDCASE.

          MODIFY TL_FCAT FROM EL_FCAT INDEX SY-TABIX TRANSPORTING CHECKTABLE F4AVAILABL.

        WHEN CL_BUKRS OR CL_WERKS OR CL_INVOI OR CL_STATUS.
          EL_FCAT-KEY_SEL = ABAP_ON.

          IF EL_FCAT-FIELDNAME EQ CL_INVOI  OR
             EL_FCAT-FIELDNAME EQ CL_STATUS.
            EL_FCAT-HOTSPOT = ABAP_ON.

            IF EL_FCAT-FIELDNAME EQ CL_STATUS.
              EL_FCAT-ICON = ABAP_ON.
              MODIFY TL_FCAT FROM EL_FCAT INDEX SY-TABIX TRANSPORTING KEY_SEL HOTSPOT ICON.

            ELSE.
              MODIFY TL_FCAT FROM EL_FCAT INDEX SY-TABIX TRANSPORTING KEY_SEL HOTSPOT.

            ENDIF.

          ELSE.
            MODIFY TL_FCAT FROM EL_FCAT INDEX SY-TABIX TRANSPORTING KEY_SEL.

          ENDIF.

        WHEN CL_EBELN OR CL_MBLNR OR CL_BELNR.
          EL_FCAT-HOTSPOT = ABAP_ON.
          MODIFY TL_FCAT FROM EL_FCAT INDEX SY-TABIX TRANSPORTING HOTSPOT.

        WHEN CL_MANDT OR CL_INVFL.
          EL_FCAT-NO_OUT = ABAP_ON.
          EL_FCAT-TECH   = ABAP_ON.
          MODIFY TL_FCAT FROM EL_FCAT INDEX SY-TABIX TRANSPORTING NO_OUT TECH.

        WHEN CL_USER OR CL_DATA OR CL_HORA.
          EL_FCAT-NO_OUT = ABAP_ON.
          MODIFY TL_FCAT FROM EL_FCAT INDEX SY-TABIX TRANSPORTING NO_OUT.

        WHEN OTHERS.
*     Do nothing
      ENDCASE.

    ENDLOOP.
* Exibição da Grid do ALV.
    CALL METHOD CLG_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = EL_LAYOUT
        IS_VARIANT           = EL_VARIANT
        I_SAVE               = SY-ABCDE(1) "A - Ambos o Layout (Global e Definido pelo usuário)
        IT_TOOLBAR_EXCLUDING = TL_EXCLUDE
      CHANGING
        IT_OUTTAB            = TG_0007[]
        IT_FIELDCATALOG      = TL_FCAT.

  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*      FORM ZF_DATA_CHANGE2                                           *
*---------------------------------------------------------------------*
*      Processamento de linhas que sofreram alteração na edição       *
*---------------------------------------------------------------------*
*      -->UCL_DATA_CHANGED - Clase relacionada ao evento de alteração
*      -->UV_UCOMM         - Ação acionada pelo usuário
*---------------------------------------------------------------------*
FORM ZF_DATA_CHANGED USING UCL_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                           UV_UCOMM         TYPE        SYUCOMM.

  DATA: EL_UP_TBL TYPE TY_UP_TBL.

  DATA: VL_VALUE TYPE LVC_VALUE.

  CONSTANTS: CL_MATNR	TYPE LVC_FNAME VALUE 'MATNR',
             CL_MWSKZ	TYPE LVC_FNAME VALUE 'MWSKZ'.

  DATA(LT_GOOD_CELLS) = UCL_DATA_CHANGED->MT_GOOD_CELLS.

  LOOP AT LT_GOOD_CELLS INTO DATA(EL_GOOD_CELLS).
    READ TABLE TG_0007 INTO DATA(EL_0007) INDEX EL_GOOD_CELLS-ROW_ID.
    EL_UP_TBL-TABIX  = EL_GOOD_CELLS-ROW_ID.
    EL_UP_TBL-TABIX2 = EL_GOOD_CELLS-TABIX.
* Verifica o nome técnico do campo editável.
    CASE EL_GOOD_CELLS-FIELDNAME.
      WHEN 'MATNR'. "Material
        CHECK EL_0007-MATNR NE EL_GOOD_CELLS-VALUE(18).
        EL_UP_TBL-MATNR  = EL_GOOD_CELLS-VALUE.
        EL_UP_TBL-FLGMAT = ABAP_ON.

      WHEN 'MWSKZ'. "Código do imposto
        CHECK EL_0007-MWSKZ NE EL_GOOD_CELLS-VALUE(2).
        EL_UP_TBL-MWSKZ  = EL_GOOD_CELLS-VALUE.
        EL_UP_TBL-FLGMKZ = ABAP_ON.

      WHEN OTHERS.
*       Do nothing
    ENDCASE.

    READ TABLE TG_UP_TBL TRANSPORTING NO FIELDS WITH KEY TABIX = EL_UP_TBL-TABIX.

    IF SY-SUBRC IS INITIAL.
      MODIFY TG_UP_TBL FROM EL_UP_TBL INDEX SY-TABIX TRANSPORTING MWSKZ FLGMKZ.

    ELSE.
      APPEND EL_UP_TBL TO TG_UP_TBL.

    ENDIF.

    CLEAR EL_UP_TBL.

  ENDLOOP.

  IF NOT TG_UP_TBL[] IS INITIAL.
* Verifica se o Código do Imposto é Válido.
    PERFORM ZF_TAX_INDICATOR_CHECK TABLES TG_UP_TBL
                                    USING UCL_DATA_CHANGED
                                          EL_0007.

  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*      FORM ZF_AFTER_USER_COMMAND                                     *
*---------------------------------------------------------------------*
*      Processamento após ação do usuários nos botões                 *
*---------------------------------------------------------------------*
*      -->UV_ROW_ID    - Identificação da Linha                       *
*      -->UV_COLUMN_ID - Identificação da Coluna                      *
*      -->UE_ROW_NO    - Estrutura de identificação da Linha e Coluna *
*---------------------------------------------------------------------*
FORM ZF_HOTSPOT_CLICK USING UV_ROW_ID    TYPE LVC_S_ROW
                            UV_COLUMN_ID TYPE LVC_S_COL
                            US_ROW_NO    TYPE LVC_S_ROID.

  DATA: TL_FATURA_ENERGIA_INVOIFILE TYPE ZFICT_FATURA_ENERGIA_INVOIFILE,
        TL_RETURN                   TYPE BAPIRET2_T.

  DATA: VL_XSTRING TYPE XSTRING.

  CONSTANTS: CL_INVOI	 TYPE LVC_FNAME VALUE 'INVOICEID',
             CL_EBELN  TYPE LVC_FNAME VALUE 'EBELN',
             CL_MBLNR  TYPE LVC_FNAME VALUE 'MBLNR',
             CL_BELNR  TYPE LVC_FNAME VALUE 'BELNR',
             CL_STATUS TYPE LVC_FNAME VALUE 'STATUS',
             CL_MIR4   TYPE TCODE     VALUE 'MIR4',
             CL_RBN    TYPE CHAR3     VALUE 'RBN',
             CL_GJR    TYPE CHAR3     VALUE 'GJR'.

  READ TABLE TG_0007 INTO DATA(EL_0007) INDEX UV_ROW_ID.

  CASE UV_COLUMN_ID.
    WHEN CL_INVOI. "Indentificação da Fatura

      IF SY-SUBRC IS INITIAL.
        APPEND EL_0007-INVOICEFILES TO TL_FATURA_ENERGIA_INVOIFILE.

        TRY.
            ZCL_INT_OB_FI_FAT_ENER_DWLD=>ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE( )->EXECUTE_REQUEST(
                 EXPORTING
                   I_INFO_REQUEST  = TL_FATURA_ENERGIA_INVOIFILE
                 IMPORTING
                   E_ID_INTEGRACAO = DATA(RESUL_ID)
                   E_INTEGRACAO    = DATA(RESULT_JSON)
                 ).

          CATCH ZCX_ERROR INTO DATA(ZCX_ERROR).
            MESSAGE ID ZCX_ERROR->MSGID TYPE 'E' NUMBER ZCX_ERROR->MSGNO WITH ZCX_ERROR->MSGV1 ZCX_ERROR->MSGV2 ZCX_ERROR->MSGV3 ZCX_ERROR->MSGV4.
            RETURN.

        ENDTRY.
*Displays PDF
        CALL FUNCTION 'ZSMARTFORMS_PDF_FILE_PREVIEW'
          EXPORTING
            PDF_DATA = RESULT_JSON-DS_DATA_XSTRING.

      ENDIF.

    WHEN CL_EBELN . "Nº do documento de compras
      CHECK NOT EL_0007-EBELN IS INITIAL.
      CALL FUNCTION 'MEPO_CALL'
        EXPORTING
          IM_EBELN                 = EL_0007-EBELN
          IM_TRTYP                 = SY-ABCDE(1) "A - Exibir
        EXCEPTIONS
          INVALID_TRANSACTION_TYPE = 1
          INVALID_CALL             = 2
          NO_AUTHORITY             = 3
          OTHERS                   = 4.

      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ENDIF.

    WHEN CL_MBLNR . "Número do documento do material
      IF SY-SUBRC IS INITIAL.
        CHECK NOT EL_0007-MBLNR IS INITIAL AND
              NOT EL_0007-MJAHR IS INITIAL.
        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            I_MBLNR             = EL_0007-MBLNR
            I_MJAHR             = EL_0007-MJAHR
          EXCEPTIONS
            ILLEGAL_COMBINATION = 1
            OTHERS              = 2.

        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

        ENDIF.

      ENDIF.

    WHEN CL_BELNR . "Nº de um documento de faturamento
      CHECK NOT EL_0007-BELNR IS INITIAL AND
            NOT EL_0007-GJAHR IS INITIAL.
      SET PARAMETER ID: CL_RBN FIELD EL_0007-BELNR,
                        CL_GJR FIELD EL_0007-GJAHR.
      CALL TRANSACTION CL_MIR4 AND SKIP FIRST SCREEN.
      SET PARAMETER ID: CL_RBN FIELD SPACE,
                        CL_GJR FIELD SPACE.

    WHEN CL_STATUS. "Status de processamento de documentos Pedido/MIGO/MIRO
* Verifica se o semáforo do status é vermelho.
      CHECK EL_0007-STATUS EQ '@0A@'.
      LOOP AT TG_0013 INTO DATA(EL_0013) WHERE BUKRS     EQ EL_0007-BUKRS
                                           AND WERKS     EQ EL_0007-WERKS
                                           AND INVOICEID EQ EL_0007-INVOICEID.

        APPEND INITIAL LINE TO TL_RETURN ASSIGNING FIELD-SYMBOL(<FS_RETURN>).

        IF <FS_RETURN> IS ASSIGNED.
          MOVE-CORRESPONDING EL_0013 TO <FS_RETURN>.
          MOVE: EL_0013-ZNUMBER      TO <FS_RETURN>-NUMBER,
                EL_0013-ZPARAMETER   TO <FS_RETURN>-PARAMETER,
                EL_0013-ZROW         TO <FS_RETURN>-ROW,
                EL_0013-ZSYSTEM      TO <FS_RETURN>-SYSTEM.

        ENDIF.

      ENDLOOP.
* Verifica se há mensagens a serem exibidas.
      IF NOT TL_RETURN[] IS INITIAL.
* Exibe mensagens de processamentos.
        CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
          EXPORTING
            IT_MESSAGE = TL_RETURN[].

      ENDIF.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module ZM_STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*& Chamada da Barra de Ferramentas e Título da tela 2000
*&---------------------------------------------------------------------*
MODULE ZM_STATUS_2000 OUTPUT.

  DATA TL_FCODE TYPE TABLE OF SY-UCOMM.

  DATA: GV_TITLE TYPE SYTITLE.

  APPEND 'ONLI' TO TL_FCODE.
  APPEND 'GET'  TO TL_FCODE.
  APPEND 'ALLS' TO TL_FCODE.
  APPEND 'FEWS' TO TL_FCODE.
  APPEND 'DOCU' TO TL_FCODE.

  IF GV_TITLE IS INITIAL.
    GV_TITLE = SY-TITLE.

  ENDIF.

  SET PF-STATUS 'ZFI_AUTOM_ENERG' EXCLUDING TL_FCODE.
  SET TITLEBAR 'ZFI_TIT_AUTM_ENERG' WITH GV_TITLE.

  PERFORM ZF_SHOW_DATA_ALVOO.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_2000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       Ação do usuário. Botões barra de ferramentada tela 2000 - Saída
*----------------------------------------------------------------------*
MODULE ZM_USER_COMMAND_2000_EXIT INPUT.

  CALL METHOD CLG_ALVGRID->CHECK_CHANGED_DATA.

  DATA(TG_UP_TBL2) = TG_UP_TBL.
  DELETE TG_UP_TBL2 WHERE ERRO IS NOT INITIAL.

  IF NOT TG_UP_TBL2[] IS INITIAL.
* Verifica e salva dados do ALV.
    PERFORM ZF_SAVAR_DADOS USING SY-UCOMM.

  ENDIF.

  CASE SY-UCOMM.
    WHEN 'ENDE' OR 'ECAN'.
* Inicializa Classes de Objetos dos ALV OO.
      PERFORM ZF_INIT_OBJ_CLASS_ALVOO.
      LEAVE PROGRAM.

    WHEN 'E'.
* Inicializa Classes de Objetos dos ALV OO.
      PERFORM ZF_INIT_OBJ_CLASS_ALVOO.
      SET SCREEN 0.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
* Do nothing
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       Ação do usuário. Botões barra de ferramentada tela 2000
*----------------------------------------------------------------------*
MODULE ZM_USER_COMMAND_2000 INPUT.

  CASE SY-UCOMM.
    WHEN 'SPOS'.
* Verifica e salva dados do ALV.
      PERFORM ZF_SAVAR_DADOS USING SY-UCOMM.
      CALL METHOD CLG_ALVGRID->REFRESH_TABLE_DISPLAY.

    WHEN OTHERS.
* Do nothing
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_INIT_OBJ_CLASS_ALVOO
*&---------------------------------------------------------------------*
*       Inicializa Classes de Objetos dos ALV OO
*----------------------------------------------------------------------*
FORM ZF_INIT_OBJ_CLASS_ALVOO.

  IF NOT CUST_CTRL IS INITIAL.
    FREE: CUST_CTRL.

  ENDIF.

  IF NOT CLG_ALVGRID IS INITIAL.
    FREE: CLG_ALVGRID.

  ENDIF.

  CLEAR TL_FCODE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_savar_dados
*&---------------------------------------------------------------------*
*& Verifica e salva dados do ALV
*&---------------------------------------------------------------------*
*& UV_UCOMM - Valor da função acionada pelo usuário.
*&---------------------------------------------------------------------*
FORM ZF_SAVAR_DADOS USING UV_UCOMM TYPE SYUCOMM.

  DATA: EL_FATURA_ENERGIA_UPST TYPE ZFIE_FATURA_ENERGIA_UPST,
        EL_RETURN_ERRO         TYPE ZSTRUCT_RETURN_API_EUDR,
        EL_0007                TYPE TY_0007.

  DATA LV_RESULT.

  IF UV_UCOMM EQ 'SPOS'.
    LV_RESULT = 1.
    CALL METHOD CLG_ALVGRID->CHECK_CHANGED_DATA.

  ELSE.
    DATA(LV_UCOMM) = UV_UCOMM.
* Há dados para serem salvos. Deseja salvar?
* Mensagem de confirmação para cadastro novo de contrato
    PERFORM ZF_POPUP_TO_CONFIRM USING    TEXT-012
                                         1
                                CHANGING LV_RESULT.
    UV_UCOMM = LV_UCOMM.

  ENDIF.

  IF LV_RESULT = 1.
    IF TG_UP_TBL[] IS INITIAL.
* Não há dados para serem salvos.
      MESSAGE S398(00) WITH TEXT-013 DISPLAY LIKE 'W'.

    ELSE.
      DATA(VL_FLAG) = SY-ABCDE+4(1). "E - Erro

      LOOP AT TG_UP_TBL INTO DATA(LE_UP_TBL) WHERE ERRO IS INITIAL.
        IF VL_FLAG EQ SY-ABCDE+4(1). "E - Erro
          CLEAR VL_FLAG.

        ENDIF.

        READ TABLE TG_0007 INTO EL_0007 INDEX LE_UP_TBL-TABIX.

        IF SY-SUBRC IS INITIAL.
          IF NOT LE_UP_TBL-FLGMAT IS INITIAL      AND
                 LE_UP_TBL-MATNR  EQ EL_0007-MATNR.
            EL_0007-MATNR = LE_UP_TBL-MATNR.

          ENDIF.

          IF NOT LE_UP_TBL-FLGMKZ IS INITIAL      AND
                 LE_UP_TBL-MWSKZ  EQ EL_0007-MWSKZ.
            EL_0007-MWSKZ = LE_UP_TBL-MWSKZ.

          ENDIF.

          MODIFY TG_0007 FROM EL_0007 INDEX LE_UP_TBL-TABIX TRANSPORTING MATNR MWSKZ.

          UPDATE ZFIT0007
             SET MATNR = EL_0007-MATNR
                 MWSKZ = EL_0007-MWSKZ
          WHERE BUKRS     EQ EL_0007-BUKRS
            AND WERKS     EQ EL_0007-WERKS
            AND INVOICEID EQ EL_0007-INVOICEID.

          IF         SY-SUBRC       IS INITIAL    AND
             ( ( NOT EL_0007-MATNR IS INITIAL    OR
                 NOT EL_0007-MWSKZ IS INITIAL )  OR
               (     EL_0007-MATNR IS INITIAL    OR
                     EL_0007-MWSKZ IS INITIAL ) ).
            COMMIT WORK.

            IF ( NOT EL_0007-MATNR IS INITIAL   AND
                     EL_0007-MWSKZ IS INITIAL ) OR
               (     EL_0007-MATNR IS INITIAL   AND
                 NOT EL_0007-MWSKZ IS INITIAL ) OR
               (     EL_0007-MATNR IS INITIAL   AND
                     EL_0007-MWSKZ IS INITIAL ).
              EL_FATURA_ENERGIA_UPST-LAUNCHPROTOCOL  = |Salvo na Base SAP|.

            ELSE.
              EL_FATURA_ENERGIA_UPST-LAUNCHPROTOCOL  = |Salvo na Base SAP com Info. para lançar|.

            ENDIF.

            EL_FATURA_ENERGIA_UPST-INVOICEID       = EL_0007-INVOICEID.
            EL_FATURA_ENERGIA_UPST-PAYMENTPROTOCOL = SPACE.
            EL_FATURA_ENERGIA_UPST-INVOICESTATUS   = '3'. "Integrada
            EL_FATURA_ENERGIA_UPST-RELEASEDATE     = SY-DATLO(4) && SY-ULINE(1) && SY-DATLO+4(2) && SY-ULINE(1) && SY-DATLO+6(2).
            EL_FATURA_ENERGIA_UPST-PAYMENTDATE     = 'null'.
* Chama API Autom. Fatura Energia - Atual. Status.
            TRY.
                ZCL_INT_OB_FI_FAT_ENER_UPST=>ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE( )->EXECUTE_REQUEST(
                     EXPORTING
                       I_INFO_REQUEST           = EL_FATURA_ENERGIA_UPST
                     IMPORTING
                       E_ID_INTEGRACAO          = DATA(RESUL_ID)
                       E_INTEGRACAO             = DATA(RESULT_JSON)
                     ).
              CATCH ZCX_INTEGRACAO INTO DATA(ZCX_INTEGRACAO).
                MESSAGE ID ZCX_INTEGRACAO->MSGID TYPE 'E' NUMBER ZCX_INTEGRACAO->MSGNO WITH ZCX_INTEGRACAO->MSGV1 ZCX_INTEGRACAO->MSGV2 ZCX_INTEGRACAO->MSGV3 ZCX_INTEGRACAO->MSGV4.
                RETURN.

              CATCH ZCX_ERROR INTO DATA(ZCX_ERROR).
                IF RESUL_ID IS NOT INITIAL.
                  SELECT SINGLE DS_DATA_RETORNO
                    FROM ZINTEGRACAO_LOG INTO @DATA(VL_DATA_RETURN_ERRO)
                    WHERE ID_INTEGRACAO EQ @RESUL_ID.

                  IF     SY-SUBRC            IS INITIAL  AND
                     NOT VL_DATA_RETURN_ERRO IS INITIAL.
                    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = VL_DATA_RETURN_ERRO CHANGING DATA = EL_RETURN_ERRO ).

                    IF NOT EL_RETURN_ERRO-DATA IS INITIAL.
                      MESSAGE |(WSC Atualiza status Automação Energia) { EL_RETURN_ERRO-DATA } | TYPE 'S' DISPLAY LIKE 'E'.

                    ENDIF.

                  ENDIF.

                ENDIF.

                MESSAGE ID ZCX_ERROR->MSGID TYPE 'S' NUMBER ZCX_ERROR->MSGNO WITH ZCX_ERROR->MSGV1 ZCX_ERROR->MSGV2 ZCX_ERROR->MSGV3 ZCX_ERROR->MSGV4 DISPLAY LIKE 'E'.

            ENDTRY.

          ELSE.
            ROLLBACK WORK.
            VL_FLAG = ABAP_ON.

          ENDIF.

        ENDIF.

      ENDLOOP.

      CHECK VL_FLAG NE SY-ABCDE+4(1). "E - Erro
      IF VL_FLAG IS INITIAL.
* Dados foram salvos com sucesso.
        MESSAGE S104(ZEHS).

      ELSE.
* Nem todos os dados foram salvos com sucesso.
        MESSAGE S398(00) WITH TEXT-014 DISPLAY LIKE 'W'.

      ENDIF.

      CLEAR TG_UP_TBL.
* Verifica Status de processamento do registros das faturas.
      PERFORM ZF_CHECK_STATUS_INVOICE.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*       Mensagem de confirmação da geração e reprocessamento de arquivos
*----------------------------------------------------------------------*
*      -->P_TEXT_QUESTION  Texto da pergunta na caixa de diálogo
*      -->P_TIPO           Tipo de Pop-up 1 = "SIM/NÃO" 2 = "Escolha 3"
*      <--P_RESULT         Retorno da escolha do Pop-up.
*----------------------------------------------------------------------*
FORM ZF_POPUP_TO_CONFIRM USING    P_TEXT_QUESTION TYPE CHAR256
                                  P_TIPO          TYPE I
                         CHANGING P_RESULT.
*
* Declaração de Constantes Locais
  CONSTANTS: C_1        TYPE C VALUE '1'.
*
* Declaração de Variáveis Locais
* Declaração das variávis do método de criação do arquivo.
  DATA: LV_WINDOW_TITLE TYPE STRING .
*
* Acerto o texto da Barra de Título da Caixa de Diálogo do Windows
  LV_WINDOW_TITLE = SY-TITLE.

  CASE P_TIPO.
    WHEN 1.
* Função de mensagem de confirmação da geração e reprocessamento de
* arquivos.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = LV_WINDOW_TITLE
          TEXT_QUESTION         = P_TEXT_QUESTION
          TEXT_BUTTON_1         = TEXT-004 "Sim
          ICON_BUTTON_1         = TEXT-005 "ICON_OKAY
          TEXT_BUTTON_2         = TEXT-006 "Não
          ICON_BUTTON_2         = TEXT-007 "ICON_CANCEL
          DEFAULT_BUTTON        = C_1
          DISPLAY_CANCEL_BUTTON = ABAP_OFF
        IMPORTING
          ANSWER                = P_RESULT
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
* Verifica de função executou com erro.
      IF NOT SY-SUBRC IS INITIAL.                           "#EC NEEDED
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN 2.
* Função de mensagem de confirmação da geração e reprocessamento de
* arquivos.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = LV_WINDOW_TITLE
          TEXT_QUESTION         = P_TEXT_QUESTION
          TEXT_BUTTON_1         = TEXT-008 "Pasta
          ICON_BUTTON_1         = TEXT-009 "ICON_OPEN_FOLDER
          TEXT_BUTTON_2         = TEXT-010 "Arquivo
          ICON_BUTTON_2         = TEXT-011 "ICON_DISPLAY_TEXT
          DEFAULT_BUTTON        = C_1
          DISPLAY_CANCEL_BUTTON = ABAP_ON
        IMPORTING
          ANSWER                = P_RESULT
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
* Verifica de função executou com erro.
      IF NOT SY-SUBRC IS INITIAL.                           "#EC NEEDED
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    WHEN OTHERS.
* Do nothing.
  ENDCASE.
*
ENDFORM.                    " ZF_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& Form ZF_TAX_INDICATOR_CHECK
*&---------------------------------------------------------------------*
*& Verifica se o Código do Imposto é Válido
*&---------------------------------------------------------------------*
*&      -->PT_UP_TBL        - TI de valores alterado da Grid ALV
*&      -->UCL_DATA_CHANGED - Clase relacionada ao evento de alteração
*&      -->UE_0007          - WA da Tabela Faturas NexInvoice
*&---------------------------------------------------------------------*
FORM ZF_TAX_INDICATOR_CHECK TABLES PT_UP_TBL        LIKE        TG_UP_TBL
                             USING UCL_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                   UE_0007          LIKE LINE OF TG_0007.

  TABLES: T007A, EKPO.

  LOOP AT PT_UP_TBL.
    DATA(VL_INDEX) = SY-TABIX.
    SELECT SINGLE LAND1 FROM T001 INTO @DATA(VL_LAND1) WHERE BUKRS EQ @UE_0007-BUKRS.
* Verifica se o Código do Imposto é Válido.
    CALL FUNCTION 'TAX_INDICATOR_CHECK'
      EXPORTING
        LAND              = VL_LAND1
        STEUERKENNZEICHEN = PT_UP_TBL-MWSKZ
      IMPORTING
        T007A             = T007A
      EXCEPTIONS
        KALSM_NOT_VALID   = 1
        NOT_VALID         = 2
        PARAMETER_ERROR   = 3
        OTHERS            = 4.

    IF SY-SUBRC IS INITIAL.
      IF NOT PT_UP_TBL-MATNR IS INITIAL.
        DATA(VL_MATNR) = PT_UP_TBL-MATNR.

      ELSE.
        CHECK NOT UE_0007-MATNR IS INITIAL.
        VL_MATNR = UE_0007-MATNR.

      ENDIF.

      IF NOT PT_UP_TBL-MWSKZ IS INITIAL.
        DATA(VL_MWSKZ) = PT_UP_TBL-MWSKZ.

      ELSE.
        CHECK NOT UE_0007-MWSKZ IS INITIAL.
        VL_MWSKZ = UE_0007-MWSKZ.

      ENDIF.

      EKPO-KNTTP = 'K'.
      EKPO-MATNR = VL_MATNR.
      SELECT SINGLE * FROM MARA INTO @DATA(EL_MARA) WHERE MATNR EQ @UE_0007-MATNR.
      EKPO-MEINS = EL_MARA-MEINS.
      EKPO-MWSKZ = VL_MWSKZ.
      EKPO-WERKS = UE_0007-WERKS.

      SELECT SINGLE BWKEY FROM T001W INTO @DATA(LV_BWKEY) WHERE WERKS EQ @UE_0007-WERKS.
* Verifica se o Código do Imposto é Válido para o Material conforme o seu tipo.
      CALL FUNCTION 'J_1B_NF_PO_CONSUMPTION_TAX'
        EXPORTING
          I_EKPO                = EKPO
          I_T007A               = T007A
          I_BWKEY               = LV_BWKEY
        EXCEPTIONS
          NOT_CONSUMPTION       = 1
          NOT_INDUSTRIALIZATION = 2
          OTHERS                = 3.

      IF NOT SY-SUBRC IS INITIAL.
* Mensagem de erro da validação do código do imposto.
        CALL METHOD UCL_DATA_CHANGED->ADD_PROTOCOL_ENTRY
          EXPORTING
            I_MSGID     = SY-MSGID
            I_MSGTY     = SY-MSGTY
            I_MSGNO     = SY-MSGNO
            I_MSGV1     = SY-MSGV1
            I_MSGV2     = SY-MSGV2
            I_FIELDNAME = 'MWSKZ'
            I_ROW_ID    = PT_UP_TBL-TABIX
            I_TABIX     = PT_UP_TBL-TABIX.
* Marca registro como erro.
        PT_UP_TBL-ERRO = ABAP_ON.
        MODIFY PT_UP_TBL INDEX VL_INDEX TRANSPORTING ERRO.

      ELSE.
* Marca registro como não erro.
        PT_UP_TBL-ERRO = ABAP_OFF.
        MODIFY PT_UP_TBL INDEX VL_INDEX TRANSPORTING ERRO.

      ENDIF.

    ELSE.
* Mensagem de erro da validação do código do imposto.
      CALL METHOD UCL_DATA_CHANGED->ADD_PROTOCOL_ENTRY
        EXPORTING
          I_MSGID     = SY-MSGID
          I_MSGTY     = SY-MSGTY
          I_MSGNO     = SY-MSGNO
          I_MSGV1     = SY-MSGV1
          I_MSGV2     = SY-MSGV2
          I_FIELDNAME = 'MWSKZ'
          I_ROW_ID    = PT_UP_TBL-TABIX
          I_TABIX     = PT_UP_TBL-TABIX2.
* Marca registro como erro.
      PT_UP_TBL-ERRO = ABAP_ON.
      MODIFY PT_UP_TBL INDEX VL_INDEX TRANSPORTING ERRO.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_check_status_invoice
*&---------------------------------------------------------------------*
*& Verifica Status de processamento do registros das faturas
*&---------------------------------------------------------------------*
FORM ZF_CHECK_STATUS_INVOICE.

  DATA: EL_0007 TYPE TY_0007.

  SELECT * FROM ZFIT0013
    INTO TABLE TG_0013
    FOR ALL ENTRIES IN TG_0007
  WHERE BUKRS     EQ TG_0007-BUKRS
    AND WERKS     EQ TG_0007-WERKS
    AND INVOICEID EQ TG_0007-INVOICEID.

  IF SY-SUBRC IS INITIAL.
*** Semáforo Vermelho - Erro no processamento de criação do Documentos Pedido/MIGO/MIRO.
    LOOP AT TG_0013 INTO DATA(EL_0013).
      EL_0007-STATUS = '@0A@'. "Vermelho
      MODIFY TG_0007 FROM EL_0007 TRANSPORTING STATUS WHERE BUKRS     EQ EL_0013-BUKRS
                                                        AND WERKS     EQ EL_0013-WERKS
                                                        AND INVOICEID EQ EL_0013-INVOICEID.

    ENDLOOP.

  ENDIF.
*** Semáforo Cinza - Registro carregado se informação em um dos campos de Material e Código de imposto.
  EL_0007-STATUS = '@EB@'. "Cinza
  MODIFY TG_0007 FROM EL_0007 TRANSPORTING STATUS WHERE   STATUS NE '@0A@'      "Vermelho
                                                    AND ( MATNR  IS INITIAL
                                                     OR   MWSKZ  IS INITIAL ).
*** Semáforo Amarelo - Registro em processamento de criação do Documentos Pedido/MIGO/MIRO.
  EL_0007-STATUS = '@09@'. "Amarelo
  MODIFY TG_0007 FROM EL_0007 TRANSPORTING STATUS WHERE   STATUS NE '@0A@'      "Vermelho
                                                    AND   MATNR  IS NOT INITIAL
                                                    AND   MWSKZ  IS NOT INITIAL
                                                    AND ( EBELN  IS INITIAL
                                                     OR   MBLNR  IS INITIAL
                                                     OR   BELNR  IS INITIAL ).
*** Semáforo Verde - Registro em processamento de criação do Documentos Pedido/MIGO/MIRO.
  EL_0007-STATUS = '@08@'. "Verde
  MODIFY TG_0007 FROM EL_0007 TRANSPORTING STATUS WHERE STATUS NE '@0A@'        "Vermelho
                                                    AND MATNR  IS NOT INITIAL
                                                    AND MWSKZ  IS NOT INITIAL
                                                    AND EBELN  IS NOT INITIAL
                                                    AND MBLNR  IS NOT INITIAL
                                                    AND BELNR  IS NOT INITIAL.

ENDFORM.
