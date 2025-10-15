*&---------------------------------------------------------------------*
*&  Include           ZFIR060_TOP
*&---------------------------------------------------------------------*

REPORT ZFIR060.

TABLES: J_1BNFDOC, MARA, T001W.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES:
      BEGIN OF TY_J_1BNFDOC,
        DOCNUM           TYPE J_1BNFDOC-DOCNUM,
        BUKRS            TYPE J_1BNFDOC-BUKRS,
        DIRECT           TYPE J_1BNFDOC-DIRECT,
        AUTOM_INCOMING   TYPE J_1BNFDOC-AUTOM_INCOMING,
        NFENUM           TYPE J_1BNFDOC-NFENUM,
        SERIES           TYPE J_1BNFDOC-SERIES,
        CANDAT           TYPE J_1BNFDOC-CANDAT,
        PSTDAT           TYPE J_1BNFDOC-PSTDAT,
        DOCDAT           TYPE J_1BNFDOC-DOCDAT,
        DOCTYP           TYPE J_1BNFDOC-DOCTYP,
      END   OF TY_J_1BNFDOC,

      BEGIN OF TY_J_1BNFE_ACTIVE,
        DOCNUM     TYPE J_1BNFE_ACTIVE-DOCNUM,
        DOCSTA     TYPE J_1BNFE_ACTIVE-DOCSTA,
        CANCEL     TYPE J_1BNFE_ACTIVE-CANCEL,
        REGIO      TYPE J_1BNFE_ACTIVE-REGIO,
        NFYEAR     TYPE J_1BNFE_ACTIVE-NFYEAR,
        NFMONTH    TYPE J_1BNFE_ACTIVE-NFMONTH,
        STCD1      TYPE J_1BNFE_ACTIVE-STCD1,
        MODEL      TYPE J_1BNFE_ACTIVE-MODEL,
        SERIE      TYPE J_1BNFE_ACTIVE-SERIE,
        NFNUM9     TYPE J_1BNFE_ACTIVE-NFNUM9,
        DOCNUM9    TYPE J_1BNFE_ACTIVE-DOCNUM9,
        CDV        TYPE J_1BNFE_ACTIVE-CDV,
      END   OF TY_J_1BNFE_ACTIVE,

      BEGIN OF TY_J_1BNFLIN,
        DOCNUM    TYPE J_1BNFLIN-DOCNUM,
        ITMNUM    TYPE J_1BNFLIN-ITMNUM,
        CFOP      TYPE J_1BNFLIN-CFOP,
        MENGE     TYPE J_1BNFLIN-MENGE,
        MEINS     TYPE J_1BNFLIN-MEINS,
        NETWRT    TYPE J_1BNFLIN-NETWRT,
        MATNR     TYPE J_1BNFLIN-MATNR,
        REFKEY    TYPE J_1BNFLIN-REFKEY,
        NBM       TYPE J_1BNFLIN-NBM,
        REFITM    TYPE J_1BNFLIN-REFITM,
      END   OF TY_J_1BNFLIN,

      BEGIN OF TY_VBRK,
        VBELN  LIKE VBAK-VBELN,
        FKART  LIKE VBRK-FKART,
        AUBEL  LIKE VBRP-AUBEL,
        VGBEL  LIKE VBRP-VGBEL,
        WERKS  LIKE VBRP-WERKS,
      END OF TY_VBRK,

      BEGIN OF TY_KONV,
        KNUMV  LIKE KONV-KNUMV,
        KWERT  LIKE KONV-KWERT,
        KBETR  LIKE KONV-KBETR,
        KSCHL  LIKE KONV-KSCHL,
      END   OF TY_KONV,

      BEGIN OF TY_LFA1,
        LIFNR     TYPE LFA1-LIFNR,
        NAME1     TYPE LFA1-NAME1,
      END   OF TY_LFA1,

      BEGIN OF TY_J_1BNFNAD,
        DOCNUM   TYPE J_1BNFNAD-DOCNUM,
        PARVW    TYPE J_1BNFNAD-PARVW,
        PARID    TYPE J_1BNFNAD-PARID,
        PARTYP   TYPE J_1BNFNAD-PARTYP,
      END   OF TY_J_1BNFNAD,

      BEGIN OF TY_MAKT,
        MATNR TYPE MAKT-MATNR,
        MAKTX TYPE MAKT-MAKTX,
      END OF TY_MAKT,

      BEGIN OF TY_ZFIWRT0001,
        OPERACAO TYPE ZFIWRT0001-OPERACAO,
      END OF TY_ZFIWRT0001,

      BEGIN OF TY_SAIDA_0100,
        SEL,
        WERKS       TYPE VBRP-WERKS,       "Centro
        UF_FILIAL   TYPE LFA1-REGIO,       "UF FILIAL
        CFOP        TYPE J_1BNFLIN-CFOP,   "CFOP
        NFENUM      TYPE J_1BNFDOC-NFENUM, "Nº Nota
        DOCNUM      TYPE J_1BNFDOC-DOCNUM, "Nº Documento
        SERIES      TYPE J_1BNFDOC-SERIES, "Série
        ST_SAP      TYPE C LENGTH 30,      "Status SAP
        ST_NFE      TYPE C LENGTH 30,      "Status NFE
        MEINS       TYPE J_1BNFLIN-MEINS,  "Unid.
        MATNR       TYPE J_1BNFLIN-MATNR,  "Material
        MAKTX       TYPE MAKT-MAKTX,       "Ds. Material
        PSTDAT      TYPE J_1BNFDOC-PSTDAT, "Data Lançamento
        DOCDAT      TYPE J_1BNFDOC-DOCDAT, "Data Documento
        MENGE       TYPE J_1BNFLIN-MENGE,  "Quantidade
        NETWRT      TYPE J_1BNFLIN-NETWRT, "Valor Total
        VLR_TRIBUTO TYPE J_1BNFLIN-NETWRT, "Valor Tributo
        ORDEM       TYPE LIPS-VGBEL,       "Ordem
        REMESSA     TYPE LIPS-VBELN,       "Remessa
        FATURA      TYPE VBRK-VBELN,       "Fatura
        POSNR       TYPE VBRP-POSNR,       "Item Fatura
        BELNR       TYPE BSIS-BELNR,       "Doc.Contábil
        ST_CTB      TYPE C LENGTH 4,       "St Contábil
        NBM         TYPE J_1BNFLIN-NBM,    "NCM
        CHAVE_NFE   TYPE C LENGTH 44,      "Chave NFe
        DS_TERMINAL TYPE C LENGTH 45,       "Nome Cliente Forne
        UF          TYPE LFA1-REGIO,       "UF
        TIPO_ORDEM  TYPE VBRK-FKART,       "Tp.Ordem
        OBJ_KEY     TYPE ZIB_CONTABIL-OBJ_KEY,
        BUKRS       TYPE J_1BNFDOC-BUKRS,
        SEQ_LCTO   TYPE ZSDT0156-SEQ_LCTO,
        ITMNUM     TYPE ZSDT0156-ITMNUM,
      END  OF TY_SAIDA_0100.



*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_0100 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: CONSTRUCTOR
                IMPORTING IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
*Event for toolbar
    ON_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
               IMPORTING  E_OBJECT,

    HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
               IMPORTING E_UCOMM.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
* Estruturas ALV
*----------------------------------------------------------------------*

DATA: OBJ_ALV_0100         TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_0100   TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

* ALV field catalogs
DATA: IT_FCAT    TYPE LVC_T_FCAT,
      WA_FCAT    TYPE LVC_S_FCAT.

* ALV excluded functions
DATA: IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE.

* Alv Styles
DATA: LS_EDIT         TYPE LVC_S_STYL,
      LT_EDIT         TYPE LVC_T_STYL.

* ALV layout variant
DATA: GS_VARIANT   TYPE DISVARIANT,
      VARIANTE     LIKE DISVARIANT,
      GS_VARIANT_C TYPE DISVARIANT.

* ALV layout
DATA: GS_LAYOUT        TYPE SLIS_LAYOUT_ALV.

* ALV Stable
DATA: WA_STABLE        TYPE LVC_S_STBL.

DATA: IT_SEL_ROWS         TYPE LVC_T_ROW,
      WA_SEL_ROWS         TYPE LVC_S_ROW.

DATA: GT_ESTILO   TYPE LVC_T_STYL WITH HEADER LINE,
      WL_ESTILO   TYPE LVC_S_STYL.

DATA: GT_F4  TYPE LVC_T_F4 WITH HEADER LINE.

DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.

* Objetos
DATA: C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TY_TOOLBAR TYPE STB_BUTTON.

DATA: WA_ESTRUTURA       TYPE TY_ESTRUTURA,
      IT_ESTRUTURA       TYPE TABLE OF TY_ESTRUTURA.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: IT_SAIDA_0100       TYPE TABLE OF TY_SAIDA_0100,
      WA_SAIDA_0100       TYPE TY_SAIDA_0100,
      TG_J_1BNFDOC        TYPE TABLE OF TY_J_1BNFDOC        WITH HEADER LINE,
      TG_J_1BNFE_ACTIVE   TYPE TABLE OF TY_J_1BNFE_ACTIVE   WITH HEADER LINE,
      TG_J_1BNFSTX        TYPE TABLE OF J_1BNFSTX           WITH HEADER LINE,
      TG_J_1BNFLIN        TYPE TABLE OF J_1BNFLIN           WITH HEADER LINE,
      TG_J_1BNFLIN_NFW    TYPE TABLE OF J_1BNFLIN           WITH HEADER LINE,
      TG_MAKT             TYPE TABLE OF TY_MAKT             WITH HEADER LINE,
      TG_J_1BNFNAD        TYPE TABLE OF TY_J_1BNFNAD        WITH HEADER LINE,
      TG_0155             TYPE TABLE OF ZSDT0155            WITH HEADER LINE,
      TG_0156             TYPE TABLE OF ZSDT0156            WITH HEADER LINE,
      TG_0156_AUX         TYPE TABLE OF ZSDT0156            WITH HEADER LINE,
      TG_BKPF             TYPE TABLE OF BKPF                WITH HEADER LINE,
      TG_0157             TYPE TABLE OF ZSDT0157            WITH HEADER LINE,
      TG_LFA1             TYPE TABLE OF TY_LFA1             WITH HEADER LINE,
      TG_KONV             TYPE TABLE OF TY_KONV             WITH HEADER LINE,
      TG_VBRK             TYPE TABLE OF TY_VBRK             WITH HEADER LINE,
      TG_ZFIWRT0008       TYPE TABLE OF ZFIWRT0008          WITH HEADER LINE,
      TG_ZFIWRT0001       TYPE TABLE OF TY_ZFIWRT0001       WITH HEADER LINE,
      TG_ZIB_ERR          TYPE TABLE OF ZIB_CONTABIL_ERR.


*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*

RANGES: R_TP_TRIB FOR ZSDT0155-TP_TRIB.

*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
 DATA: VG_DTINI_PROC TYPE SY-DATUM,
       VAR_ANSWER    TYPE C.

*----------------------------------------------------------------------*
* tela de Seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETER: P_BUKRS  TYPE TY_J_1BNFDOC-BUKRS OBLIGATORY.


  SELECT-OPTIONS: P_BRANCH FOR J_1BNFDOC-BRANCH OBLIGATORY,
                  P_REGIO  FOR T001W-REGIO OBLIGATORY NO INTERVALS, "USER STORY 159931 - MMSILVA - 17.12.2024
                  P_DOCDAT FOR J_1BNFDOC-DOCDAT OBLIGATORY NO-EXTENSION,
                  P_DOCNUM FOR J_1BNFDOC-DOCNUM NO INTERVALS NO-EXTENSION ,
                  P_MATKL  FOR MARA-MATKL.

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  PARAMETER: P_ZFES   TYPE J_1BNFDOC-CANCEL, " 01 - Fethab Soja
             P_ZFESA  TYPE J_1BNFDOC-CANCEL, " 02 - Fethab Soja Adicional
             P_ZFEA   TYPE J_1BNFDOC-CANCEL, " 03 - Fethab Algoão
             P_ZFEAA  TYPE J_1BNFDOC-CANCEL, " 04 - Fethab Algoão Adicional
             P_ZIMA   TYPE J_1BNFDOC-CANCEL, " 05 - IMA
             P_ZIAGRO TYPE J_1BNFDOC-CANCEL, " 06 - IAGRO
             P_ZFEMI  TYPE J_1BNFDOC-CANCEL, " 07 - FETHAB MILHO
             P_ZFETTO  TYPE J_1BNFDOC-CANCEL. " 07 - FET TOCANTINS
SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.

PARAMETERS: P_ATV RADIOBUTTON GROUP RB1, " Ativa
            P_EST RADIOBUTTON GROUP RB1, " Estornada
            P_AMB RADIOBUTTON GROUP RB1. " Ambos

SELECTION-SCREEN: END OF BLOCK B3.

SELECTION-SCREEN: BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-002.
PARAMETER: P_VARIA TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN: END OF BLOCK B5.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*

DATA: VG_REPID          LIKE SY-REPID,
      VG_VARIANT        TYPE DISVARIANT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARIA.

  VG_REPID        = SY-REPID.
  VARIANTE-REPORT = VG_REPID.

  IF ( P_VARIA IS NOT INITIAL ).
    VG_VARIANT-VARIANT = P_VARIA.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = VARIANTE
      I_SAVE        = 'A'
    IMPORTING
      ES_VARIANT    = VARIANTE
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( SY-SUBRC NE 0 ).
    MESSAGE S000(Z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE VARIANTE-VARIANT TO P_VARIA.
    MOVE VARIANTE-VARIANT TO GS_VARIANT_C-VARIANT.
  ENDIF.

START-OF-SELECTION.

  PERFORM F_SELECIONAR_DADOS.
  PERFORM F_PROCESSAR_DADOS.
  PERFORM F_IMPRIMIR_DADOS.
