*&---------------------------------------------------------------------*
*&  Include           ZSDR0051_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS ICON.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*


TYPES: BEGIN OF TY_PARAM,
         P_BUKRS      TYPE ZIB_NFE_FORN-BUKRS,
         P_BRANCH     TYPE ZIB_NFE_FORN-BRANCH,
         P_CNPJ       TYPE ZIB_NFE_FORN-NU_CHAVE_CNPJ,
         P_MODELO     TYPE ZIB_NFE_FORN-NU_CHAVE_MODELO,
         P_DT_EMISSAO TYPE ZIB_NFE_FORN-DT_EMISSAO,
         P_NUMERO     TYPE ZIB_NFE_FORN-NU_CHAVE_NUMERO,
       END OF TY_PARAM,

       BEGIN OF TY_PATH,
         P_INPUT(100) TYPE C,
         P_IMP_TODOS  TYPE C,
       END OF TY_PATH,

       BEGIN OF TY_CHAVES_ERRO,
         CHAVE  TYPE C LENGTH 44,
         MSG    TYPE C LENGTH 200,
       END OF TY_CHAVES_ERRO,

       BEGIN OF TY_J_1BBRANCH,
         BRANCH      TYPE J_1BBRANCH-BRANCH,
         STATE_INSC  TYPE J_1BBRANCH-STATE_INSC,
         BUKRS       TYPE J_1BBRANCH-BUKRS,
         STCD1       TYPE J_1BBRANCH-STCD1,
       END OF TY_J_1BBRANCH,

       BEGIN OF TY_DOC_ELET,
         BUKRS              TYPE ZIB_NFE_FORN-BUKRS,
         BRANCH             TYPE ZIB_NFE_FORN-BRANCH,
         NU_CHAVE_CNPJ      TYPE ZIB_NFE_FORN-NU_CHAVE_CNPJ,
         NU_CHAVE_MODELO    TYPE ZIB_NFE_FORN-NU_CHAVE_MODELO,
         NU_CHAVE_SERIE     TYPE ZIB_NFE_FORN-NU_CHAVE_SERIE,
         DT_EMISSAO         TYPE ZIB_NFE_FORN-DT_EMISSAO,
         HR_EMISSAO         TYPE ZIB_NFE_DIST_TER-HR_EMISSAO,
         NU_CHAVE_NUMERO    TYPE ZIB_NFE_FORN-NU_CHAVE_NUMERO,
         NU_CHAVE           TYPE ZIB_NFE_FORN-NU_CHAVE,
         NU_CNPJ_DEST       TYPE J_1BSTCD1,
         NU_IE              TYPE ZIB_NFE_FORN-NU_IE,
         ST_NOTA            TYPE ZIB_NFE_FORN-ST_NOTA,
         NU_CHAVE_REGIAO    TYPE ZIB_NFE_FORN-NU_CHAVE_REGIAO,
         NU_CHAVE_ANO       TYPE ZIB_NFE_FORN-NU_CHAVE_ANO,
         NU_CHAVE_MES       TYPE ZIB_NFE_FORN-NU_CHAVE_MES,
         NU_CHAVE_ALEATOR   TYPE ZIB_NFE_FORN-NU_CHAVE_ALEATOR,
         NU_CHAVE_DV        TYPE ZIB_NFE_FORN-NU_CHAVE_DV,
         NU_CODE            TYPE ZIB_NFE_FORN-NU_CODE,
         ATUALIZADO         TYPE ZIB_NFE_FORN-ATUALIZADO,
         "Adicionais
         UF_DESTINO         TYPE C LENGTH 2,
         DESTINO_IE         TYPE ZIB_NFE_DIST_TER-DESTINO_IE,
         FORNE_CPF          TYPE ZIB_NFE_DIST_TER-FORNE_CPF,
         FORNE_IE           TYPE ZIB_NFE_DIST_TER-FORNE_IE,
         FORNE_RAZAO        TYPE ZIB_NFE_DIST_TER-FORNE_RAZAO,
         DS_NAT_OPERACAO    TYPE ZIB_NFE_DIST_TER-DS_NAT_OPERACAO,
         CD_FORM_PAG        TYPE ZIB_NFE_DIST_TER-CD_FORM_PAG,
         CD_TIPO_DOC        TYPE ZIB_NFE_DIST_TER-CD_TIPO_DOC,
         CD_FORM_EMISSAO    TYPE ZIB_NFE_DIST_TER-CD_FORM_EMISSAO,
         DT_SAIDA           TYPE ZIB_NFE_DIST_TER-DT_SAIDA,
         HR_SAIDA           TYPE ZIB_NFE_DIST_TER-HR_SAIDA,
         CD_FINA_EMISSAO    TYPE ZIB_NFE_DIST_TER-CD_FINA_EMISSAO,
         NR_PED_COMPRA      TYPE ZIB_NFE_DIST_TER-NR_PED_COMPRA,
         NR_CTR_COMPRA      TYPE ZIB_NFE_DIST_TER-NR_CTR_COMPRA,
         NR_FATURA          TYPE ZIB_NFE_DIST_TER-NR_FATURA,
         VL_TOTAL_FATURA    TYPE ZIB_NFE_DIST_TER-VL_TOTAL_FATURA,
         VL_DESCO_FATURA    TYPE ZIB_NFE_DIST_TER-VL_DESCO_FATURA,
         VL_LIQUIDO         TYPE ZIB_NFE_DIST_TER-VL_LIQUIDO,
         VL_ICMS_BASE       TYPE ZIB_NFE_DIST_TER-VL_ICMS_BASE,
         VL_ICMS_TOTAL      TYPE ZIB_NFE_DIST_TER-VL_ICMS_TOTAL,
         VL_ICMS_ST_BASE    TYPE ZIB_NFE_DIST_TER-VL_ICMS_ST_BASE,
         VL_ICMS_ST_TOTAL   TYPE ZIB_NFE_DIST_TER-VL_ICMS_ST_TOTAL,
         VL_PRODUTOS        TYPE ZIB_NFE_DIST_TER-VL_PRODUTOS,
         VL_FRETE           TYPE ZIB_NFE_DIST_TER-VL_FRETE,
         VL_SEGURO          TYPE ZIB_NFE_DIST_TER-VL_SEGURO,
         VL_DESCONTO        TYPE ZIB_NFE_DIST_TER-VL_DESCONTO,
         VL_II_TOTAL        TYPE ZIB_NFE_DIST_TER-VL_II_TOTAL,
         VL_IPI_TOTAL       TYPE ZIB_NFE_DIST_TER-VL_IPI_TOTAL,
         VL_PIS_TOTAL       TYPE ZIB_NFE_DIST_TER-VL_PIS_TOTAL,
         VL_COF_TOTAL       TYPE ZIB_NFE_DIST_TER-VL_COF_TOTAL,
         VL_DESPESAS        TYPE ZIB_NFE_DIST_TER-VL_DESPESAS,
         VL_TOTAL           TYPE ZIB_NFE_DIST_TER-VL_TOTAL,
         VL_ICMS_DESONERADO TYPE ZIB_NFE_DIST_TER-VL_ICMS_DESONERADO,
       END OF TY_DOC_ELET,

       BEGIN OF TY_DOC_ELET_ITM.
         INCLUDE STRUCTURE ZIB_NFE_DIST_ITM.
TYPES: END OF TY_DOC_ELET_ITM.

TYPES: BEGIN OF TY_SAIDA_XML.
         INCLUDE TYPE ZIB_NFE_FORN.
TYPES: END OF TY_SAIDA_XML.

*---------------------------------------------------------------------*
*  Inicio Implementação Classes
*---------------------------------------------------------------------*

CLASS LCL_ALV_TOOLBAR_XML DEFINITION.
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

* Objetos
DATA: C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      TY_TOOLBAR TYPE STB_BUTTON.

DATA: OBJ_ALV_XML          TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER_XML    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: GT_CATALOG_XML       TYPE LVC_T_FCAT,
      GW_CATALOG_XML       TYPE LVC_S_FCAT.

DATA: IT_SELECTED_ROWS     TYPE LVC_T_ROW,
      WA_SELECTED_ROWS     TYPE LVC_S_ROW.

DATA: OBJ_TOOLBAR_XML      TYPE REF TO LCL_ALV_TOOLBAR_XML.

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
DATA: GS_VARIANT       TYPE DISVARIANT.

* ALV layout
DATA: GS_LAYOUT        TYPE LVC_S_LAYO.

* ALV Stable
DATA: WA_STABLE        TYPE LVC_S_STBL.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: IT_SAIDA_XML        TYPE TABLE OF TY_SAIDA_XML,
      WA_SAIDA_XML        TYPE TY_SAIDA_XML,
      IT_ZIB_NFE_FORN     TYPE TABLE OF ZIB_NFE_FORN,
      WA_ZIB_NFE_FORN     TYPE ZIB_NFE_FORN,
      IT_ZIB_NFE_DIST_TER TYPE TABLE OF ZIB_NFE_DIST_TER,
      WA_ZIB_NFE_DIST_TER TYPE ZIB_NFE_DIST_TER,
      IT_J_1BBRANCH       TYPE TABLE OF TY_J_1BBRANCH WITH HEADER LINE,
      WA_J_1BBRANCH       TYPE TY_J_1BBRANCH,
      TG_CHAVES_ERRO      TYPE TABLE OF TY_CHAVES_ERRO WITH HEADER LINE,
      WA_PARAM            TYPE TY_PARAM,
      WA_PATH             TYPE TY_PATH,
      WA_DOC_ELET         TYPE TY_DOC_ELET,
      IT_DOC_ELET_ITM     TYPE TABLE OF TY_DOC_ELET_ITM,
      WA_DOC_ELET_ITM     TYPE TY_DOC_ELET_ITM,
      WA_FILES_UNIX       TYPE EPSFILI,
      WA_FILES_DOC        TYPE SDOKPATH.

*-------------------------------------------------------------------
* Variaveis
*-------------------------------------------------------------------
DATA: V_PREFIX_ENT    TYPE ZPREFIX,
      V_MENSAGEM      TYPE BAPI_MSG,
      T_DIR_LOC_F     TYPE TABLE OF SDOKPATH,
      T_DIR_LOCAL     TYPE TABLE OF SDOKPATH,
      T_DIR_UNIX      TYPE TABLE OF EPSFILI,
      V_FILE_AUX      TYPE DRAW-FILEP,
      V_FILE_AUX2     TYPE DRAW-FILEP,
      IT_XML_FORN     TYPE TABLE OF ZXML,
      WA_XML_FORN     TYPE ZXML,
      VG_BLOQ_FILIAL  TYPE C.

*-------------------------------------------------------------------
* Radio Buttons
*-------------------------------------------------------------------

DATA: R_LOCAL TYPE C VALUE 'X',
      R_UNIX  TYPE C.


RANGES: P_BUKRS      FOR ZIB_NFE_FORN-BUKRS,
        P_BRANCH     FOR ZIB_NFE_FORN-BRANCH,
        P_CNPJ       FOR ZIB_NFE_FORN-NU_CHAVE_CNPJ,
        P_MODELO     FOR ZIB_NFE_FORN-NU_CHAVE_MODELO,
        P_DT_EMISSAO FOR ZIB_NFE_FORN-DT_EMISSAO,
        P_NUMERO     FOR ZIB_NFE_FORN-NU_CHAVE_NUMERO.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: C_X            TYPE C VALUE 'X',
           C_LOG(10)      TYPE C VALUE 'LOG',
           C_PROC(10)     TYPE C VALUE 'PROC',
           C_ENT(10)      TYPE C VALUE 'ENT',
           C_ALL(3)       TYPE C VALUE 'ALL',
           C_ASC(10)      TYPE C VALUE 'ASC',
           C_MASK_LOC(6)  TYPE C VALUE '*.xml',
           C_MASK_UNIX(6) TYPE C VALUE '*.xml',
           C_U            TYPE C VALUE 'U',
           C_W            TYPE C VALUE 'W',
           C_L            TYPE C VALUE 'L',
           C_E            TYPE C VALUE 'E',
           C_S            TYPE C VALUE 'S'.
