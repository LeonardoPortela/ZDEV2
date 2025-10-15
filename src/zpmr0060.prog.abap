******************************************************************************
***                         AMAGGI IMPORTAÇÃO                              ***
******************************************************************************
*** Programa  : ZPMR0003                                                   ***
***                                                                        ***
*** Descrição : Acompanhamento de desvios dos planos de manutenção         ***
**              Especificação feita pelo analista Cleudo                   ***
**                                                                         ***
*** Objetivo  : Controlar execução de planos                               ***
***                                                                        ***
*** Versão    Autor               Data          Observações                ***
*** ------    ----------          ----------    --------------             ***
***                                                                        ***
******************************************************************************

REPORT  ZPMR0060.

TABLES: AUFK, SSCRFIELDS, EQUZ, MMPT, ITOB, *MMPT, T399W, *T399W, MPLA, *MPLA, MHIO, *MHIO, MPOS, IFLO.

*** Macros
***********************************************************
DEFINE MC_PREENCHE_FIELDCAT.

  CLEAR GV_FCAT.
  GV_FCAT-FIELDNAME     = &1.
  GV_FCAT-DATATYPE      = &2.
  GV_FCAT-COLTEXT       = &3.
  GV_FCAT-JUST          = &4.
  GV_FCAT-KEY           = &5.
  GV_FCAT-JUST          = &6.
  GV_FCAT-EDIT_MASK     = &7.
  GV_FCAT-TABNAME       = &8.
  INSERT GV_FCAT INTO TABLE GT_FIELDCATALOG.



END-OF-DEFINITION.

DEFINE MC_PREENCHE_CLASS.

  VG_I = VG_I + 1.
  CLEAR IT_SORT.
  IT_SORT-SPOS      = VG_I.
  IT_SORT-FIELDNAME = &1.
  IT_SORT-GROUP     = &2.
  IT_SORT-UP        = &3.
  IT_SORT-SUBTOT    = &4.
  APPEND IT_SORT.

END-OF-DEFINITION.
* Definições para ALV
TYPE-POOLS: KKBLO.  "Tipos globais para ALV

*----------------------------------------------------------*
*           Definição de variáveis globais                 *
*----------------------------------------------------------*

DATA: V_TABIX     TYPE SY-TABIX        ,  " guardar o indice
      V_PERMISSAO TYPE C,
      V_BUKRS(50) TYPE C,
      V_WERKS(50) TYPE C.

*---------------------------------------------------------*
*               Types                                     *
*---------------------------------------------------------*
TYPES:
  BEGIN OF TY_EQUI,            " equi: Equipamento dados mestres
    PLTXT TYPE IFLO-PLTXT,     " Descrição do local
    EQUNR TYPE EQUI-EQUNR,     " Nº equipamento
    AEDAT TYPE EQUI-AEDAT,     " Data da última modificação
    EQTYP TYPE EQUI-EQTYP,     " Categoria de equipamento
    EQART TYPE EQUI-EQART,     " Tipo do objeto técnico
    OBJNR TYPE EQUI-OBJNR,     " Nº objeto
    DATBI TYPE EQUZ-DATBI,     " Data de validade final
    IWERK TYPE EQUZ-IWERK,     " Centro de planejamento de manutenção
    TIDNR TYPE EQUZ-TIDNR,     " Nº identificação técnica
    EARTX TYPE T370K_T-EARTX,  " Descrição do tipo equipamento
  END OF TY_EQUI,

  BEGIN OF TY_EQUZ,  " equz: Intervalo de tempo equipamento
    EQUNR TYPE EQUZ-EQUNR,  " Nº equipamento
    DATBI TYPE EQUZ-DATBI,  " Data de validade final
    IWERK TYPE EQUZ-IWERK,  " Centro de planejamento de manutenção
    TIDNR TYPE EQUZ-TIDNR,  " Nº identificação técnica
  END OF TY_EQUZ,

  BEGIN OF TY_T370K_T,       " Descrição do tipo do equipamento
    EQART TYPE T370K_T-EQART ,   " código
    EARTX TYPE T370K_T-EARTX ,   " descrição
  END OF TY_T370K_T,

  BEGIN OF TY_RELATORIO,      " Dados do relatorio
    IWERK      TYPE EQUZ-IWERK,    " Centro
    EQUNR      TYPE EQUZ-EQUNR ,   " Codigo do equipamento
    SHTXT      TYPE ITOB-SHTXT ,   " Denominação do objeto técnico
    EARTX      TYPE T370K_T-EARTX, " Tipo de veículo
    WARPL      TYPE MPOS-WARPL,    " Plano de manutenção
    ZYKL1(8)   TYPE C,             " Ciclo / offset de um pacote de manutenção
    PAK_TEXT   TYPE MMPT-PAK_TEXT, " Texto p/o pacote ou ciclo de manutenção (tempo/rendimento)
    WPTXT      TYPE MPLA-WPTXT ,   " Descrição
    SZAEH(8)   TYPE C,             " Posição Contador
    RZAEH(8)   TYPE C,             " Posição contador conclusão
    RZAEH_     TYPE CAUFV-IDAT2,    " Posição contador conclusão
    SZAEH_     TYPE CAUFV-IDAT2,   " Posição Contador
    NZAEH_     TYPE CAUFV-IDAT2,   " Posição contador conclusão
    TOTAC(8)   TYPE C,             " Posição Total Contador
    NZAEH(8)   TYPE C,             " Próxima leitura planejada de contador
    USO(8)     TYPE C,             " Utilização
    DESVIO(8)  TYPE C,             " Desvio
    PER_DESV   TYPE I,             " Desvio
    IDAT2      TYPE CAUFV-IDAT2,   " Data Ultima_troca
    IDAT3      TYPE CAUFV-IDAT2,
    LRMDT      TYPE MHIS-LRMDT,
    BAUTL      TYPE MPOS-BAUTL,    " Sistema
    CELL_COLOR TYPE LVC_T_SCOL,    " Cor da Célula
    ZEIEH      TYPE MMPT-ZEIEH,
    PLTXT      TYPE IFLO-PLTXT,
    EQKTX      TYPE EQKT-EQKTX,
    ORDEM(4)   TYPE C,
    W_ORDEM    TYPE AUFNR,
    DESV_PL(8) TYPE C,
  END OF TY_RELATORIO .

TYPES: BEGIN OF TY_CALC,
         ABNUM     TYPE MHIS-ABNUM,
         WARPL     TYPE MPLA-WARPL,
         NPLDA     TYPE MHIS-NPLDA,
         STADT     TYPE MHIS-STADT,
         LRMDT     TYPE MHIS-LRMDT,
         MPTYP     TYPE MPLA-MPTYP,
         ZEIEH     TYPE MMPT-ZEIEH,
         ZYKL1     TYPE MMPT-ZYKL1,
         ZYKL2(8)  TYPE C,
         USO(8)    TYPE C,
         DESVIO(8) TYPE C,
         PER_DESV  TYPE I,
       END OF TY_CALC.

TYPES: BEGIN OF TY_VIAUFKST,
         BUKRS  TYPE VIAUFKST-BUKRS,
         WERKS  TYPE VIAUFKST-WERKS,
         WARPL  TYPE VIAUFKST-WARPL,
         AUFNR  TYPE VIAUFKST-AUFNR,
         QMNUM  TYPE VIAUFKST-QMNUM,
         ERDAT  TYPE VIAUFKST-ERDAT,
         OBJNR  TYPE CAUFV-OBJNR,
         STTXT  TYPE CAUFVD-STTXT,
         ASTTX  TYPE CAUFVD-ASTTX,
         STATUS TYPE CHAR10.
TYPES: END OF TY_VIAUFKST.


TYPES: BEGIN OF TY_VIQMEL.
         INCLUDE STRUCTURE VIQMEL.
         TYPES: STTXT  TYPE CAUFVD-STTXT,
         ASTTX  TYPE CAUFVD-ASTTX,
         STATUS TYPE CHAR10.
TYPES: END OF TY_VIQMEL.


DATA: IT_CALC  TYPE TABLE OF TY_CALC WITH HEADER LINE.
DATA: IT_CALC_AUX  TYPE TABLE OF TY_CALC WITH HEADER LINE.

DATA: CLICKS TYPE SY-TABIX.

*----------------------------------------------------*
*                Tabelas Internas                    *
*----------------------------------------------------*
DATA:
  T_EQUI       TYPE TABLE OF TY_EQUI       WITH HEADER LINE,
  T_EQUZ       TYPE TABLE OF TY_EQUZ       WITH HEADER LINE,
  T_T370K_T    TYPE TABLE OF TY_T370K_T    WITH HEADER LINE,
  T_RELATORIO  TYPE TABLE OF TY_RELATORIO,
  T_RELATORIO1 TYPE TABLE OF TY_RELATORIO,
  T_PLANOS     TYPE TABLE OF TY_RELATORIO,
  W_RELATORIO  TYPE TY_RELATORIO,
  T_ZEIEH      TYPE RANGE OF MMPT-ZEIEH.

DATA: T_ORDEM  TYPE TABLE OF TY_VIAUFKST.
DATA: T_VIQMEL TYPE TABLE OF TY_VIQMEL.
DATA: LINHA_SELECIONADA TYPE SLIS_SELFIELD.
DATA: _EXIT             TYPE C.

CONSTANTS: YX  VALUE 'X'.              "Flag X

*** Declaração de constantes
***********************************************************
CONSTANTS: CC_A        TYPE C VALUE 'A',
           CC_X        TYPE C VALUE 'X',
           CC_I        TYPE C VALUE 'I',
           CC_1        TYPE C VALUE '1',
           CC_2        TYPE C VALUE '2',
           CC_SPRAS(2) TYPE C VALUE 'PT',
           CC_M        TYPE C VALUE 'M',
           WC_UPD      TYPE C VALUE 'U',    "indicator: Update
           WC_DELE     TYPE C VALUE 'L'.    "indicator: delete

DATA: TI_LINECOLOR  TYPE SLIS_SPECIALCOL_ALV   OCCURS 0 WITH HEADER LINE,
      TI_LISTHEADER TYPE SLIS_T_LISTHEADER,
      TI_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV            WITH HEADER LINE,
      TI_SORT       TYPE SLIS_SORTINFO_ALV     OCCURS 0 WITH HEADER LINE.

DATA: OK_CODE     TYPE          SY-UCOMM,
      RETURN_CODE TYPE          SY-SUBRC,
      GS_LAYOUT   TYPE          LVC_S_LAYO,
      WA_COLOR    TYPE          LVC_S_SCOL,  " Cor para célula
      IT_COLOR    TYPE TABLE OF LVC_S_SCOL,  " Cor para célula
      IT_S_FCAT   TYPE LVC_T_FCAT,
      PT_EXCLUDE  TYPE          UI_FUNCTIONS. " internal table declaration to be passed.

DATA: L_FLSTR             LIKE RIHIMRG-DESIC   .

DATA: T_EQUI_TABIX LIKE SY-INDEX,
      T_EQUI_MAX   LIKE SY-INDEX,
      T_MPOS_MAX   LIKE SY-INDEX,
      T_EQUZ_MAX   LIKE SY-INDEX,
      T_JEST_MAX   LIKE SY-INDEX,
      MSG(60)      TYPE C.

*--- strategy header
TYPES: BEGIN OF TY_T351.
         INCLUDE STRUCTURE T351.
         TYPES:  KTEXT LIKE T351T-KTEXT.
TYPES: END OF TY_T351.
TYPES: WC_T_T351 TYPE TY_T351 OCCURS 0.
DATA: G_W_T351        TYPE TY_T351.

*--- data of packages (strategy)
TYPES: BEGIN OF TY_PACKAGE_DATA.
         INCLUDE STRUCTURE T351P AS T351P RENAMING WITH SUFFIX T351P.
         INCLUDE STRUCTURE T351X AS T351X RENAMING WITH SUFFIX T351X.
       TYPES: END OF TY_PACKAGE_DATA.
DATA: G_T_PACKAGE_DATA
      TYPE TABLE OF TY_PACKAGE_DATA WITH HEADER LINE.

*--- data (task list and strategy) of packages
TYPES: BEGIN OF TY_PACKAGE_DATA_ST_TL.
         INCLUDE STRUCTURE PLWP AS PLWP RENAMING WITH SUFFIX PLWP.
         INCLUDE STRUCTURE T351P AS T351P RENAMING WITH SUFFIX STRAT.
         TYPES:   PLAN         LIKE MPOS-WARPL,
         ITEM         LIKE MPOS-WAPOS,
         WPPOS        LIKE MPOS-WPPOS,
         IND_VALID(1) TYPE C.
TYPES: END OF TY_PACKAGE_DATA_ST_TL.

**--- package (task list and strategie) data
DATA: G_T_PACKAGE_DATA_ST_TL
      TYPE TABLE  OF TY_PACKAGE_DATA_ST_TL WITH HEADER LINE.

*---  Cycle definitions and MeasPoints (MMPT)
TYPES: BEGIN OF TY_WMMPT.
         INCLUDE STRUCTURE MMPT.
         INCLUDE STRUCTURE MMPT_ADDITION.
       TYPES: END   OF TY_WMMPT.
TYPES: WC_T_WMMPT TYPE TY_WMMPT OCCURS 0.
DATA:  MMPT_TAB       TYPE TY_WMMPT OCCURS 0 WITH HEADER LINE.

*--- maintenance item (MPOS)
TYPES: BEGIN OF TY_WMPOS.
         INCLUDE STRUCTURE MPOS.
         INCLUDE STRUCTURE MPOS_ADDITION.
       TYPES: END   OF TY_WMPOS.
TYPES: WC_T_WMPOS TYPE TY_WMPOS OCCURS 0.
DATA: IMPOS TYPE TY_WMPOS OCCURS 1 WITH HEADER LINE.    "table


DATA: BEGIN OF INDEX_MMPT OCCURS 20.
        INCLUDE STRUCTURE INXX.        "Manutenção Lista de Itens
      DATA: END OF INDEX_MMPT.

TYPES: BEGIN OF TY_MPOS.
         INCLUDE STRUCTURE MPOS.
         TYPES: PLTXT TYPE IFLO-PLTXT.
TYPES: END   OF TY_MPOS.
DATA: T_MPOS TYPE TABLE OF TY_MPOS WITH HEADER LINE.
DATA: T_EQKT       TYPE TABLE OF EQKT WITH HEADER LINE.

TYPES: BEGIN OF TY_VIMHIS.
         INCLUDE STRUCTURE VIMHIS.
         TYPES: READG LIKE  IMRG-READG.
TYPES: END   OF TY_VIMHIS.
DATA: T_VIMHIS TYPE TABLE OF TY_VIMHIS WITH HEADER LINE.

* Status
TYPES: BEGIN OF TY_JEST.          " Status da Ordem
         INCLUDE STRUCTURE JEST.
       TYPES: END OF TY_JEST.
DATA: T_JEST TYPE TABLE OF TY_JEST  WITH HEADER LINE.

DATA: BEGIN OF WA_IMPT.
        INCLUDE STRUCTURE IMPT.
      DATA: END OF WA_IMPT.

DATA: BEGIN OF WA_IMRG.
        INCLUDE STRUCTURE IMRG.
      DATA: END OF WA_IMRG.

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
      DATA: END OF IT_MSG.

DATA: FLTP_CHAR     TYPE IMRC_TOTAC,
      POINT_TXT(40) TYPE C,
      ANZ_VERPAK    LIKE T351P-ZAEHL,
      STTAG         TYPE SY-DATUM,
      MMPT_MAX      LIKE SY-INDEX,
      GV_RC(2)      TYPE C,
      GV_USO        TYPE IMRC_READG,
      GV_DESVIO     TYPE IMRC_READG.

* Fieldcatalog
DATA: GT_FIELDCATALOG TYPE LVC_T_FCAT,
      GV_FCAT         LIKE LINE OF GT_FIELDCATALOG,
      LS_STABLE       TYPE LVC_S_STBL.

*----------------------------------------------------*
*                ALV GRID                            *
*----------------------------------------------------*
* Control
DATA: G_CONTAINER        TYPE SCRFNAME VALUE 'ALV_CONTAINER',
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      R_GRID             TYPE REF TO CL_GUI_ALV_GRID.

DATA: OBJ_CUSTOM_0110 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      OBJ_ALV_0110    TYPE REF TO CL_GUI_ALV_GRID.

* Codigos para chamada da transação IP10.
CLASS LCL_EVENTS_HANDLER DEFINITION DEFERRED.

DATA: R_EVENT_HANDLER TYPE REF TO LCL_EVENTS_HANDLER,
      I_SELECTED_ROWS TYPE LVC_T_ROW,                "Linhas selecionadas
      W_SELECTED_ROWS TYPE LVC_S_ROW.                "Colunas Selecionadas

DATA: P_RESP, CHECK, P_ERRO(1).
DATA: TI_BDCDATA TYPE STANDARD TABLE OF BDCDATA,
      WA_BDCDATA LIKE LINE OF TI_BDCDATA.

DATA:ET_RETURN TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
     LS_RETURN TYPE BAPIRET2.

DATA: ORD_ATV TYPE CHAR1.

*---------------------------------------------------------------------*
*       CLASS lcl_events_handler DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_EVENTS_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS
      HANDLE_DOUBLE_CLICK
                  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.
ENDCLASS.                    "lcl_events_handler DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_events_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENTS_HANDLER IMPLEMENTATION.
*---Metodo para double_click.

  METHOD HANDLE_DOUBLE_CLICK.

    CHECK E_ROW-ROWTYPE(1) EQ SPACE.
    PERFORM SEL_ORDEM  USING E_ROW E_COLUMN-FIELDNAME.

  ENDMETHOD. " HANDLE_DOUBLE_CLICK
ENDCLASS.                    "lcl_events_handler IMPLEMENTATION



*----------------------------------------------------*
*                Parâmetros de Seleção               *
*----------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.

PARAMETER: P_HO RADIOBUTTON GROUP B1.
SELECTION-SCREEN COMMENT 2(7) TEXT-005 FOR FIELD P_HO.
SELECTION-SCREEN POSITION 11.

PARAMETER: P_DT RADIOBUTTON GROUP B1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 14(10) TEXT-006 FOR FIELD P_DT.
SELECTION-SCREEN POSITION 25.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  S_MPTYP FOR MPLA-MPTYP,
  S_EQUNR FOR ITOB-EQUNR,
  S_TPLNR FOR IFLO-TPLNR,
  S_EQART FOR ITOB-EQART,
  S_BAUTL FOR MPOS-BAUTL,
  S_TIPOO FOR MPOS-AUART.
SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
S_IWERK FOR ITOB-IWERK OBLIGATORY DEFAULT '*',
S_KOSTL FOR ITOB-KOSTL,
S_WARPL FOR MPOS-WARPL.
SELECTION-SCREEN: END OF BLOCK B3.

SELECTION-SCREEN: BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
S_AUFNR FOR AUFK-AUFNR.
SELECTION-SCREEN: END OF BLOCK B4.

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.
*  SSCRFIELDS-FUNCTXT_01 = 'Listar ordem'.

AT SELECTION-SCREEN. "PAI
  CASE SSCRFIELDS-UCOMM. "pushbutton pressed
    WHEN 'FC01'.
      CLEAR: ORD_ATV.
      LOOP AT SCREEN.
        CASE SCREEN-NAME.
          WHEN 'S_AUFNR-LOW' OR 'S_AUFNR-HIGH'.
            SCREEN-ACTIVE = '1'.         "Hide parameters     "n921165
            ORD_ATV = '1'.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
  ENDCASE.

*Classe para executar a lista de ordem.
CLASS ZCL_LIS_ORDEM DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: ZLIST_ORDEM.

    CLASS-METHODS: ZSHDB_ORDEM IMPORTING AUFNR TYPE AUFNR.

    CLASS-METHODS: SELEC_ORDEM IMPORTING WARPL TYPE WARPL
                               EXPORTING AUFNR TYPE AUFNR.



ENDCLASS.

CLASS ZCL_LIS_ORDEM IMPLEMENTATION.

  METHOD SELEC_ORDEM.
    DATA(W_WARPL) = WARPL.
    CLEAR: AUFNR.

    SELECT *
    FROM AFIH
    INTO TABLE @DATA(_AFIH)
      WHERE WARPL EQ @W_WARPL.


    CHECK _AFIH IS NOT INITIAL.
    SORT _AFIH DESCENDING BY AUFNR.

    READ TABLE _AFIH INTO DATA(W__AFIH) INDEX 1.
    IF SY-SUBRC EQ 0.
      AUFNR = |{ W__AFIH-AUFNR ALPHA = OUT }|.
    ENDIF.

    CLEAR: W__AFIH.
    FREE: _AFIH.
  ENDMETHOD.

  METHOD ZLIST_ORDEM.

    SELECT *
    FROM AUFK
    INTO TABLE @DATA(T_AUFNR)
      WHERE AUFNR IN @S_AUFNR.

    CHECK T_AUFNR IS NOT INITIAL.

    LOOP AT T_AUFNR ASSIGNING FIELD-SYMBOL(<_AUFNR>).
      CALL METHOD ZSHDB_ORDEM
        EXPORTING
          AUFNR = <_AUFNR>-AUFNR.
    ENDLOOP.

  ENDMETHOD.

  METHOD ZSHDB_ORDEM.

*  7007660, 61, 62

    FREE TI_BDCDATA[].
    FREE IT_MSG[].

    PERFORM F_BDC_DATA USING:
  '        '  '    '  'T'     'IW32      '    '                                                               ',
  'SAPLCOIH'  '0101'  'X'     '            '    '                                                               ',
  '        '  '    '  ' '    	'BDC_CURSOR	 '    'CAUFVD-AUFNR                                                   ',
  '        '  '    '  ' '    	'BDC_OKCODE	 '    '/00                                                            ',
  '        '  '    '  ' '     'CAUFVD-AUFNR'    AUFNR,
  'SAPLCOIH'  '3000'  'X'     '            '    '                                                               ',
  '        '  '    '  ' '    	'BDC_OKCODE	 '    '=BABL                                                          ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                3001SUB_ALL            ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                1100SUB_LEVEL          ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                1102SUB_KOPF           ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                1105SUB_BTN            ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                1104SUB_TEXT           ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                1120SUB_AUFTRAG        ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLIPAR                                0415SUB_ADRESSE        ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLIPAR                                0415SUB_ADDR_PM        ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                0154HEADER             ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                0153MAINORDER          ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                0157PARTNER            ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                7400SUB_PM_ADDR        ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                7402SUB_PM_ADDR_BTN    ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                7300TERM               ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                7301SUB_BTN            ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                7310SUB_ADD            ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                7100OBJECT             ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                0815NOTIFICATION_DATA  ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOI0                                0310AVO                ',
  '        '  '    '  ' '    	'BDC_SUBSCR	 '    'SAPLCOIH                                0153SUB_SERVICE        ',
  'SAPLSPO1'  '0100'  'X'     '            '    '                                                               ' ,
  '        '  '    '  ' '     'BDC_OKCODE  '    '=YES                                 '.

    CLEAR P_ERRO.
    PERFORM ZF_CALL_TRANSACTION USING 'IW32' CHANGING P_ERRO.

    IF P_ERRO IS INITIAL.
      CLEAR LS_RETURN.
      LS_RETURN-MESSAGE = AUFNR && ' - executado com sucesso'.
    ELSE.
      CLEAR LS_RETURN.
      LS_RETURN-MESSAGE = 'Erro ao executar a ordem' && '-' && AUFNR.
    ENDIF.

    APPEND LS_RETURN TO ET_RETURN.
  ENDMETHOD.
ENDCLASS.


*--------------------------------------------------------------------
* s t a r t - o f - s e l e c t i o n.
*--------------------------------------------------------------------
START-OF-SELECTION.

  IF S_AUFNR IS NOT INITIAL.
    CALL METHOD ZCL_LIS_ORDEM=>ZLIST_ORDEM.
  ELSE.

    PERFORM SELECIONA_DADOS.
    IF T_RELATORIO IS NOT INITIAL.
      CALL SCREEN 100.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BAUTL-LOW.
  PERFORM BUSCAR_SISTEMA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BAUTL-HIGH.
  PERFORM BUSCAR_SISTEMA.

AT SELECTION-SCREEN OUTPUT. "PAI

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN '%B002023_BLOCK_1000' OR '%_S_AUFNR_%_APP_%-TEXT'
        OR '%_S_AUFNR_%_APP_%-OPTI_PUSH' OR 'S_AUFNR-LOW'
        OR '%_S_AUFNR_%_APP_%-TO_TEXT' OR 'S_AUFNR-HIGH'
        OR '%_S_AUFNR_%_APP_%-VALU_PUSH'.


        IF ORD_ATV EQ 1.
          SCREEN-ACTIVE = '1'.         "Hide parameters     "n921165
        ELSE.
          SCREEN-ACTIVE = '0'.         "Hide parameters     "n921165
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.


*&---------------------------------------------------------------------*
*&      Form  buscar_sistema
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUSCAR_SISTEMA.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_TEMP OCCURS 0,
          BAUTL TYPE MPOS-BAUTL,
          PSTXT TYPE MPOS-PSTXT,
        END OF TL_TEMP.

  SELECT DISTINCT *
    FROM MPOS
    INTO CORRESPONDING FIELDS OF TABLE TL_TEMP
  WHERE EQUNR IN S_EQUNR.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'BAUTL'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'MPOS-BAUTL'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TEMP
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDFORM.                    "buscar_sistema

*&---------------------------------------------------------------------*
*&      module  user_command_0100  input
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT'.
*      CALL METHOD G_CUSTOM_CONTAINER->FREE.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
*      CALL METHOD G_CUSTOM_CONTAINER->FREE.
      LEAVE PROGRAM .
    WHEN 'REFRESH'.

*      PERFORM SELECIONA_DADOS.

      CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = LS_STABLE
        EXCEPTIONS
          FINISHED  = 1
          OTHERS    = 2.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      module  status_0100  output
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'S100'.
  SET TITLEBAR 'T100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECTS OUTPUT.
** create objects

  IF OBJ_CUSTOM_0110 IS INITIAL.


*    MC_PREENCHE_FIELDCAT:

    PERFORM IT_CATALOG USING:


*  1  'TIDNR'     ' '  'C'  'CHAR'  ' '  ' ' 'Id.Tec'         'X'  ' '  ' '  ' '  ' '  ' ',
   2  'IWERK'     ' '  'C'  'CHAR'  ' '  ' ' 'Centro'         'X'  ' '  ' '  ' '  ' '  ' ',
   3  'PLTXT'     ' '  'C'  'CHAR'  ' '  ' ' 'Local Instal'   ' '  ' '  ' '  ' '  ' '  ' ',
   4  'EQUNR'     ' '  'C'  'CHAR'  ' '  ' ' 'Equipamento'    ' '  ' '  ' '  ' '  'X'  ' ',
   5  'SHTXT'     ' '  'C'  'CHAR'  ' '  ' ' 'Denominação'    ' '  ' '  ' '  ' '  ' '  ' ',
   6  'EARTX'     ' '  'C'  'CHAR'  ' '  ' ' 'Tipo'           ' '  ' '  ' '  ' '  ' '  ' ',
   7  'WARPL'     ' '  'C'  'CHAR'  ' '  ' ' 'Plano'          ' '  ' '  ' '  ' '  'X'  ' ',
   8  'BAUTL'     ' '  'C'  'CHAR'  ' '  ' ' 'Sistema'        ' '  ' '  ' '  ' '  ' '  ' ',
   9  'WPTXT'     ' '  'C'  'CHAR'  ' '  ' ' 'Texto do plano' ' '  ' '  ' '  ' '  ' '  ' ',
   10 'PAK_TEXT'  ' '  'C'  'CHAR'  ' '  ' ' 'Texto do Ciclo' ' '  ' '  ' '  ' '  ' '  ' ',
   11 'ZYKL1'     ' '  '15' 'CURR'  ' '  ' ' 'Ciclo'          ' '  ' '  ' '  ' '  ' '  ' ',
   12 'USO'       ' '  '15' 'CURR'  ' '  ' ' 'Uso'            ' '  ' '  ' '  ' '  ' '  ' ',
   13 'DESVIO'    ' '  '15' 'CURR'  ' '  ' ' 'Desvio'         ' '  ' '  ' '  ' '  ' '  ' ',
   14 'W_ORDEM'   ' '  '08' 'CHAR'  ' '  ' ' 'Ordem atual'          ' '  ' '  ' '  ' '  ' '  ' ',
   15 'ORDEM'     ' '  '05' 'CHAR'  ' '  ' ' 'Hist.ordem'     ' '  ' '  ' '  ' '  'X'  ' ',
   16 'PER_DESV'  ' '  '15' 'CURR'  ' '  ' ' '% Desvio'       ' '  ' '  ' '  ' '  ' '  ' ',
   17 'RZAEH'     ' '  'C'  'CHAR'  ' '  ' ' 'Ult.Exec.Pl.'   ' '  ' '  ' '  ' '  ' '  ' '.


    IF P_DT IS NOT INITIAL.
      PERFORM  IT_CATALOG  USING:
      17 'NZAEH_'   ' '   'C' 'DATS'  ' '  ' ' 'Proxima'     ' '  ' '  ' '  ' ' ' '  ' '.
    ELSE.
      PERFORM  IT_CATALOG  USING:
      18 'NZAEH_'   ' '   'C' 'CHAR'  ' '  ' ' 'Proxima'     ' '  ' '  ' '  ' ' ' '  ' ',
      19 'TOTAC'    ' '   'C' 'CHAR'  ' '  ' ' 'Pos.Total'   ' '  ' '  ' '  ' ' ' '  ' '.
    ENDIF.

    PERFORM  IT_CATALOG  USING:
    20 'IDAT2'    ' '   'C' 'DATS'  ' '  ' ' 'Conclusão Atual'     ' '  ' '  ' '  ' ' ' '  ' ',
    20 'IDAT3'    ' '   'C' 'DATS'  ' '  ' ' 'Conclusão Anterior'  ' '  ' '  ' '  ' ' ' '  ' ',
    20 'DESV_PL'    ' '   'C' 'DATS'  ' '  ' ' 'Desvio'              ' '  ' '  ' '  ' ' ' '  ' '.

    IF P_DT IS NOT INITIAL.
      PERFORM  IT_CATALOG  USING:
      21 'SZAEH_'   ' '   'C' 'DATS'  ' '  ' ' 'Reinicio Pl' ' ' ' '  ' '  ' '  ' ' ' '.
    ELSE.
      PERFORM  IT_CATALOG  USING:
      22 'SZAEH'    ' '   'C' 'CHAR'  ' '  ' ' 'Reinicio Pl' ' ' ' '  ' '  ' ' ' '  ' '.
    ENDIF.

    GS_LAYOUT-CTAB_FNAME     = 'CELL_COLOR'.
    GS_LAYOUT-ZEBRA          = 'X'.
    GS_LAYOUT-SEL_MODE       = 'A'.
    GS_LAYOUT-CWIDTH_OPT     = 'X'.     "  Otimizar colunas na tela

    PERFORM EXCLUI_TB_FUNCTIONS CHANGING PT_EXCLUDE.

    CREATE OBJECT OBJ_CUSTOM_0110
      EXPORTING
        CONTAINER_NAME              = 'ALV_CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    CREATE OBJECT OBJ_ALV_0110
      EXPORTING
        I_PARENT = OBJ_CUSTOM_0110.


* Link used Events and Event Handler Methods
*    CREATE OBJECT R_EVENT_HANDLER.
    SET HANDLER: LCL_EVENTS_HANDLER=>HANDLE_DOUBLE_CLICK FOR OBJ_ALV_0110.

* load data into the grid and display them
    CALL METHOD OBJ_ALV_0110->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IT_TOOLBAR_EXCLUDING = PT_EXCLUDE        "excluding toolbar functions
      CHANGING
        IT_FIELDCATALOG      = IT_S_FCAT
        IT_OUTTAB            = T_RELATORIO.
  ENDIF.

  CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*&      form  load_dados_na_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOAD_DADOS_NA_GRID.

  PERFORM PREPARA_RELATORIO.
  PERFORM POPULA_FIELDCATALOG.

  GS_LAYOUT-CTAB_FNAME     = 'CELL_COLOR'.
  GS_LAYOUT-ZEBRA          = 'X'.
  GS_LAYOUT-SEL_MODE       = 'A'.
  GS_LAYOUT-CWIDTH_OPT     = 'X'.     "  Otimizar colunas na tela

  PERFORM EXCLUI_TB_FUNCTIONS CHANGING PT_EXCLUDE.

* Link used Events and Event Handler Methods
  CREATE OBJECT R_EVENT_HANDLER.
  SET HANDLER R_EVENT_HANDLER->HANDLE_DOUBLE_CLICK FOR R_GRID.

* load data into the grid and display them
  CALL METHOD R_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = GS_LAYOUT
      IT_TOOLBAR_EXCLUDING = PT_EXCLUDE        "excluding toolbar functions
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCATALOG
      IT_OUTTAB            = T_RELATORIO.

ENDFORM.                    " load_dados_na_grid

*&---------------------------------------------------------------------*
*&      Form  exclui_tb_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_EXCLUDE text
*----------------------------------------------------------------------*
FORM EXCLUI_TB_FUNCTIONS CHANGING PT_EXCLUDE TYPE UI_FUNCTIONS.

  APPEND CL_GUI_ALV_GRID=>MC_FC_SUBTOT      TO PT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_SUM         TO PT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_MB_SUBTOT      TO PT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_MB_SUM         TO PT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_INFO        TO PT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_GRAPH       TO PT_EXCLUDE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_PRINT       TO PT_EXCLUDE.


ENDFORM.                    "exclui_tb_functions

*&---------------------------------------------------------------------*
*&      Form  popula_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POPULA_FIELDCATALOG.

  MC_PREENCHE_FIELDCAT:

*      1           2         3             4    5    6    7    8
*   'TIDNR'       'CHAR' 'Id.Tec'         'C'  'X'  ' '  ' '  ' ',
    'IWERK'       'CHAR' 'Centro'         'C'  'X'  ' '  ' '  ' ',
    'PLTXT'       'CHAR' 'Local Instal'   'C'  ' '  ' '  ' '  ' ',
    'EQUNR'       'CHAR' 'Equipamento'    'C'  ' '  ' '  ' '  ' ',
    'SHTXT'       'CHAR' 'Denominação'    'C'  ' '  'L'  ' '  ' ',
    'EARTX'       'CHAR' 'Tipo'           'C'  ' '  'L'  ' '  ' ',
    'WARPL'       'CHAR' 'Plano'          'C'  ' '  ' '  ' '  ' ',
    'BAUTL'       'CHAR' 'Sistema'        'C'  ' '  'L'  ' '  ' ',
    'WPTXT'       'CHAR' 'Texto do plano' 'C'  ' '  'L'  ' '  ' ',
    'PAK_TEXT'    'CHAR' 'Texto do Ciclo' 'C'  ' '  'L'  ' '  ' ',
    'ZYKL1'       'CURR' 'Ciclo'          '15' ' '  ' '  ' '  ' ',
    'USO'         'CURR' 'Uso'            '15' ' '  ' '  ' '  ' ',
    'DESVIO'      'CURR' 'Desvio'         '15' ' '  ' '  ' '  ' ',
    'ORDEM'       'CHAR' 'Ordem     '     '05' ' '  ' '  ' '  ' ',
    'PER_DESV'    'CURR' '% Desvio'       '15' ' '  ' '  ' '  ' ',
    'RZAEH'       'CHAR' 'Ult.Exec.Pl.'   'C'  ' '  'L'  ' '  ' '.


  IF P_DT IS NOT INITIAL.
*    MC_PREENCHE_FIELDCAT: 'NZAEH'       'CHAR' 'Proxima'        'C'  ' '  ' '  '__/__/____' ' '.
    MC_PREENCHE_FIELDCAT: 'NZAEH_'       'DATS' 'Proxima'        'C'  ' '  ' '  ' ' ' '.
  ELSE.
    MC_PREENCHE_FIELDCAT: 'NZAEH'        'CHAR' 'Proxima'        'C'  ' '  ' '  ' ' ' ',
                          'TOTAC'        'CHAR' 'Pos.Total'      'C'  ' '  ' '  ' ' ' '.
  ENDIF.

  MC_PREENCHE_FIELDCAT:
*      'IDAT2'       'DATS' 'Conclusão'      'C'  ' '  ' '  '__/__/____' ' '.
       'IDAT2'       'DATS' 'Conclusão'      'C'  ' '  ' '  ' ' ' '.

  IF P_DT IS NOT INITIAL.
*   MC_PREENCHE_FIELDCAT:'SZAEH'        'CHAR' 'Reinicio Pl.'   'C'  ' '  ' '  '__/__/____' ' '.
    MC_PREENCHE_FIELDCAT:'SZAEH_'       'DATS' 'Reinicio Pl.'   'C'  ' '  ' '  ' ' ' '.
  ELSE.
    MC_PREENCHE_FIELDCAT:'SZAEH'       'CHAR' 'Reinicio Pl.'   'C'  ' '  ' '  ' ' ' '.
  ENDIF.

  READ TABLE GT_FIELDCATALOG WITH KEY FIELDNAME = 'EQUNR' INTO GV_FCAT.
  GV_FCAT-NO_ZERO = 'X'.
  MODIFY  GT_FIELDCATALOG FROM GV_FCAT INDEX SY-TABIX TRANSPORTING NO_ZERO.
  READ TABLE GT_FIELDCATALOG WITH KEY FIELDNAME = 'WARPL' INTO GV_FCAT.
  GV_FCAT-NO_ZERO = 'X'.
  MODIFY  GT_FIELDCATALOG FROM GV_FCAT INDEX SY-TABIX TRANSPORTING NO_ZERO.

ENDFORM.                    "popula_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  DATA: IT_ABASTEC TYPE RANGE OF ZTPARAM-ZVAL,
        WA_ABASTEC LIKE LINE OF IT_ABASTEC.
  DATA: IT_ZTPARAM TYPE STANDARD TABLE OF ZTPARAM,
        WA_ZTPARAM TYPE ZTPARAM.

  REFRESH: T_EQUI ,
           T_EQUZ ,
           T_RELATORIO.
  CLEAR:   W_RELATORIO, RETURN_CODE.

  IF S_EQUNR IS INITIAL AND
     S_EQART IS INITIAL AND
     S_IWERK IS INITIAL AND
     S_TIPOO IS INITIAL.
    MESSAGE 'Nenhum Parametro de seleção foi informado.' TYPE 'I'.
    RETURN_CODE = 4.
    EXIT.
  ENDIF.

  T_ZEIEH = VALUE #(
                      ( SIGN = 'I' OPTION = 'EQ' LOW = 'H'  )
                      ( SIGN = 'I' OPTION = 'EQ' LOW = 'KM' )
                    ).

  " Se equipamento ou tipo do objeto técnico for preenchido.

  SELECT C~BUKRS A~IWERK F~PLTXT A~WARPL D~WPTXT A~PSTXT A~BAUTL A~ILOAN A~EQUNR A~PLNNR A~PLNAL A~INACT A~ERSDT A~STATUS D~MPTYP D~OBJNR A~WSTRA B~NAME1"E~EQKTX" B~NAME1
     FROM MPLA AS D
     INNER JOIN MPOS  AS A ON A~WARPL = D~WARPL
     INNER JOIN T001W AS B ON B~WERKS = A~IWERK
     INNER JOIN ILOA  AS C ON C~ILOAN = A~ILOAN
     INNER JOIN IFLO  AS F ON F~TPLNR = C~TPLNR
     INTO CORRESPONDING FIELDS OF TABLE T_MPOS
 WHERE BAUTL IN S_BAUTL
 AND A~IWERK IN S_IWERK
 AND A~EQUNR IN S_EQUNR
 AND A~WARPL IN S_WARPL
 AND C~KOSTL IN S_KOSTL
 AND F~TPLNR IN S_TPLNR
 AND A~AUART IN S_TIPOO
 AND D~MPTYP IN S_MPTYP
 AND A~INACT  EQ ABAP_FALSE.

  SORT T_MPOS ASCENDING BY WARPL IWERK.

  LOOP AT T_MPOS.
    IF T_MPOS-WARPL IS NOT INITIAL AND T_MPOS-WSTRA IS NOT INITIAL.
      DELETE T_MPOS INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.

  SELECT *
  FROM EQKT
  INTO TABLE T_EQKT
  FOR ALL ENTRIES IN T_MPOS
  WHERE EQUNR EQ T_MPOS-EQUNR.

  SELECT EQUI~EQUNR EQUI~AEDAT EQUI~EQTYP EQUI~EQART EQUI~OBJNR
  FROM EQUI AS EQUI
  INNER JOIN V_EQUI AS V_EQUI ON V_EQUI~EQUNR EQ EQUI~EQUNR
  INTO CORRESPONDING FIELDS OF TABLE T_EQUI
    FOR ALL ENTRIES IN T_MPOS
  WHERE EQUI~EQUNR EQ T_MPOS-EQUNR
  AND  EQUI~EQART IN S_EQART
  AND  V_EQUI~SWERK IN S_IWERK
  AND  V_EQUI~DATBI = '99991231'.

  SELECT *
    FROM ZTPARAM
    INTO TABLE IT_ZTPARAM
    WHERE PARAM EQ 'TP_OBJ'
  AND ABASTEC EQ 'X'.

  IF IT_ZTPARAM IS NOT INITIAL.
    LOOP AT IT_ZTPARAM INTO WA_ZTPARAM.
      WA_ABASTEC-SIGN = 'I'.
      WA_ABASTEC-OPTION = 'EQ'.
      WA_ABASTEC-LOW = WA_ZTPARAM-ZVAL.
      APPEND WA_ABASTEC TO IT_ABASTEC.
    ENDLOOP.
  ENDIF.

  IF T_EQUI IS NOT INITIAL.
    DELETE T_EQUI WHERE EQTYP NOT IN IT_ABASTEC.



    SELECT V_EQUI~EQUNR V_EQUI~DATBI V_EQUI~IWERK V_EQUI~TIDNR
      INTO CORRESPONDING FIELDS OF TABLE T_EQUZ
      FROM V_EQUI
      FOR ALL ENTRIES IN T_EQUI
      WHERE   EQUNR       EQ T_EQUI-EQUNR
    AND     V_EQUI~DATBI = '99991231'.

*     deleta equipamentos duplicados
    SORT T_EQUZ BY IWERK EQUNR ASCENDING.

    DELETE ADJACENT DUPLICATES FROM T_EQUZ COMPARING EQUNR.
  ENDIF.


  IF P_DT IS NOT INITIAL.

    SELECT *
        FROM MPLA AS A
          INNER JOIN MPOS AS B ON A~WARPL	=	B~WARPL
          INNER JOIN MMPT AS M ON A~WARPL	=	M~WARPL
          INNER JOIN ILOA AS C ON C~ILOAN	=	B~ILOAN
          INNER JOIN MHIS AS D ON A~WARPL	=	D~WARPL
      INTO CORRESPONDING FIELDS OF TABLE IT_CALC
      FOR ALL ENTRIES IN T_MPOS
        WHERE A~WARPL EQ T_MPOS-WARPL
        AND   D~NPLDA > SY-DATUM
        AND   D~ABNUM EQ ( SELECT MIN( ABNUM ) FROM MHIS WHERE WARPL EQ D~WARPL AND NPLDA > SY-DATUM )
        AND   B~EQUNR IN S_EQUNR
        AND   C~TPLNR IN S_TPLNR
        AND   A~MPTYP IN S_MPTYP
        AND   C~KOSTL IN S_KOSTL.
    SORT IT_CALC BY NPLDA NPLDA.
  ENDIF.

  DESCRIBE TABLE T_MPOS LINES T_MPOS_MAX.
  IF T_MPOS_MAX = 0.
    MESSAGE 'Nenhum equipamento foi selecionado, com o criterio informado.' TYPE 'I'.
    RETURN_CODE = 4.
    EXIT.
  ENDIF.


  " Tipo de equipamento
  IF T_EQUI IS NOT INITIAL.
    SELECT EQART EARTX
      INTO TABLE T_T370K_T
      FROM T370K_T
      FOR ALL ENTRIES IN T_EQUI
      WHERE EQART  =  T_EQUI-EQART
    AND   SPRAS  =  CC_SPRAS .

    LOOP AT T_EQUI.
      T_EQUI_TABIX = SY-TABIX .
      LOOP AT T_EQUZ WHERE EQUNR = T_EQUI-EQUNR.
        READ TABLE T_T370K_T WITH KEY EQART = T_EQUI-EQART.
        T_EQUI-EARTX =  T_T370K_T-EARTX. " Tipo do objeto técnico
        T_EQUI-DATBI =  T_EQUZ-DATBI.
        T_EQUI-IWERK =  T_EQUZ-IWERK.
        T_EQUI-TIDNR =  T_EQUZ-TIDNR.
        MODIFY T_EQUI INDEX T_EQUI_TABIX.
      ENDLOOP.
    ENDLOOP.

*   Busca os status
    " I0076 - MREL --> marcado p/eliminação
    " I0320 - INAT --> oBJETO INATIVO
    SELECT * FROM JEST
      INTO TABLE T_JEST
      FOR ALL ENTRIES IN T_EQUI
      WHERE JEST~OBJNR  EQ T_EQUI-OBJNR
       AND  INACT       NE CC_X
    AND  JEST~STAT   IN ('I0076','I0320').

    DESCRIBE TABLE T_JEST LINES T_JEST_MAX.
    IF T_JEST_MAX NE 0.
      PERFORM EXCLUI_INATIVO.

      DESCRIBE TABLE T_EQUI LINES T_EQUI_MAX.
      IF T_EQUI_MAX = 0.
        MESSAGE 'Nenhum equipamento foi selecionado, com o criterio informado.' TYPE 'I'.
        RETURN_CODE = 4.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM PREPARA_RELATORIO.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  PREPARA_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARA_RELATORIO.

  DATA: V_KTEX1             TYPE T351X-KTEX1,
        LV_CYCLE_VAL        TYPE F,
        LV_TABIX_INDEX_MMPT TYPE SY-TABIX,
        LV_TABIX_MPOS       TYPE SY-TABIX.

  CLEAR: W_RELATORIO .
  FREE: T_RELATORIO.


  LOOP AT T_MPOS.
    W_RELATORIO-IWERK  = T_MPOS-IWERK.
    W_RELATORIO-PLTXT  = T_MPOS-PLTXT.
    W_RELATORIO-EQUNR  = T_MPOS-EQUNR.
    W_RELATORIO-WARPL  = T_MPOS-WARPL.
    W_RELATORIO-WPTXT  = T_MPOS-PSTXT.
    W_RELATORIO-BAUTL  = T_MPOS-BAUTL.
    W_RELATORIO-ORDEM  = ICON_ORDER.

    CALL METHOD ZCL_LIS_ORDEM=>SELEC_ORDEM
      EXPORTING
        WARPL = W_RELATORIO-WARPL
      IMPORTING
        AUFNR = W_RELATORIO-W_ORDEM.

    IF T_MPOS-EQUNR IS NOT INITIAL.

      READ TABLE T_EQKT WITH KEY EQUNR = T_MPOS-EQUNR.
      IF SY-SUBRC = 0.
        W_RELATORIO-EQKTX = T_EQKT-EQKTX.
      ENDIF.

      READ TABLE T_EQUI WITH KEY EQUNR = T_MPOS-EQUNR.
      IF SY-SUBRC = 0.


        READ TABLE T_T370K_T WITH KEY EQART = T_EQUI-EQART.
        IF SY-SUBRC = 0.
          W_RELATORIO-EARTX   = T_EQUI-EARTX.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT SINGLE EQKTX FROM EQKT
      INTO  (W_RELATORIO-SHTXT)
      WHERE EQUNR = T_EQUI-EQUNR
    AND   SPRAS = CC_SPRAS.


    IF S_BAUTL-LOW IS NOT INITIAL AND
       T_MPOS-BAUTL <> S_BAUTL-LOW.
      CONTINUE.
    ENDIF.

    SELECT SINGLE * FROM MPLA
    WHERE WARPL = T_MPOS-WARPL.

    PERFORM STATUS_CHECK_PLANO USING MPLA-OBJNR RETURN_CODE.
    IF RETURN_CODE <> 0 AND  " ativos
       RETURN_CODE <> 3   .  " inativo MREL
      CONTINUE.
    ENDIF.

    SELECT * FROM MMPT
      WHERE WARPL EQ T_MPOS-WARPL.
    ENDSELECT.
    IF SY-SUBRC EQ 0.

      W_RELATORIO-ZEIEH = MMPT-ZEIEH.
      IF P_HO IS NOT INITIAL.
        CHECK W_RELATORIO-ZEIEH IN T_ZEIEH.
      ELSE .
        CHECK W_RELATORIO-ZEIEH NOT IN T_ZEIEH.
      ENDIF.

      PERFORM OBTEM_POS_TOT_CONTADOR USING MMPT-POINT.
      IF T_MPOS-WSTRA IS INITIAL.
        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR MMPT-ZYKL1 MMPT-ZEIEH.
        W_RELATORIO-ZYKL1    =  FLTP_CHAR+14(8).
        CONDENSE W_RELATORIO-ZYKL1.
        W_RELATORIO-PAK_TEXT = MMPT-PAK_TEXT.

        PERFORM OBTEM_CONTADORES USING T_MPOS-WARPL MMPT-ZEIEH.

      ENDIF.
    ELSE.
      W_RELATORIO-ZYKL1      = ' '.
      W_RELATORIO-PAK_TEXT   = ' '.
    ENDIF.
    CLEAR T_MPOS.
  ENDLOOP.

ENDFORM.                    " PREPARA_RELATORIO

*&---------------------------------------------------------------------*
*&      Form  obtem_contadores
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WARPL    text
*----------------------------------------------------------------------*
FORM  OBTEM_CONTADORES USING P_WARPL P_ZEIEH.
  DATA: CONT_DATA TYPE P.

  CLEAR: T_VIMHIS, W_RELATORIO-IDAT2.

  SELECT * FROM VIMHIS
    INTO  TABLE T_VIMHIS
  WHERE WARPL = P_WARPL.

  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.
  DATA(TL_VIMHIS) = T_VIMHIS[].

  SORT TL_VIMHIS DESCENDING BY LRMDT.
  CLEAR CONT_DATA.
  CONT_DATA = 1.

  LOOP AT T_VIMHIS WHERE WARPL EQ P_WARPL.
    LOOP AT TL_VIMHIS ASSIGNING FIELD-SYMBOL(<W_VIMHIS>) WHERE WARPL EQ P_WARPL.
      IF <W_VIMHIS>-TSABR NE SPACE AND <W_VIMHIS>-LRMDT NE 0.  " Concluido
        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-RZAEH P_ZEIEH.
*
***   Pegar valor da ultima conclução.
        IF CONT_DATA <= 2.
          W_RELATORIO-RZAEH = FLTP_CHAR+14(8).           " Posição da última execução.
          CONDENSE W_RELATORIO-RZAEH.
          IF CONT_DATA NE 2.
            W_RELATORIO-IDAT2 = <W_VIMHIS>-LRMDT.            " Data de Conclusão Atual
          ENDIF.
*
          IF CONT_DATA EQ 2.
            W_RELATORIO-IDAT3 = <W_VIMHIS>-LRMDT.            " Data de Conclusão Anterior
          ENDIF.
*
          ADD 1 TO CONT_DATA.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SELECT SINGLE *
      FROM MHIO
      WHERE WARPL EQ T_VIMHIS-WARPL
    AND  ABNUM EQ T_VIMHIS-ABNUM.

    IF T_VIMHIS-TSABR NE SPACE AND MHIO-ADDAT NE 0.  " Concluido
      CONTINUE.
    ENDIF.

    IF T_VIMHIS-TSTAT = CC_X.                        " Ignorado.
      CONTINUE.
    ENDIF.

    IF T_VIMHIS-TSABR NE SPACE.                      " Solicitado.
      PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-SZAEH P_ZEIEH.
      W_RELATORIO-SZAEH  =  FLTP_CHAR+14(8).         " Posição do contador.
      CONDENSE  W_RELATORIO-SZAEH.

      PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-NZAEH P_ZEIEH.
      W_RELATORIO-NZAEH  =  FLTP_CHAR+14(8).       " Proxima leitura programa
      CONDENSE W_RELATORIO-NZAEH.

      IF T_MPOS-WSTRA IS INITIAL.
        GV_USO    = WA_IMRG-READG - T_VIMHIS-NZAEH + MMPT-ZYKL1.
        GV_DESVIO = GV_USO - MMPT-ZYKL1.
      ELSE.
        GV_USO    = WA_IMRG-READG - T_VIMHIS-NZAEH + MMPT_TAB-ZYKL1.
        GV_DESVIO = GV_USO - MMPT_TAB-ZYKL1.
      ENDIF.

      PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR GV_USO P_ZEIEH.
      W_RELATORIO-USO    =  FLTP_CHAR+14(8).       " Uso
      CONDENSE W_RELATORIO-USO.

      PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR GV_DESVIO P_ZEIEH.
      W_RELATORIO-DESVIO =  FLTP_CHAR+14(8).       " Desvio
      CONDENSE W_RELATORIO-DESVIO.

      IF W_RELATORIO-ZYKL1 > 0.
        W_RELATORIO-PER_DESV = ( W_RELATORIO-DESVIO / W_RELATORIO-ZYKL1 ) * 100.
      ELSE.
        W_RELATORIO-PER_DESV = 0.
      ENDIF.

      PERFORM AGRUPA_CALC USING P_WARPL W_RELATORIO-IDAT2 W_RELATORIO-IDAT3.
      PERFORM CALC USING P_DT P_WARPL.

      IF P_DT IS NOT INITIAL.
*          Mudando a cor dos ponto em atraso
        IF W_RELATORIO-DESVIO > 0.
          CLEAR WA_COLOR.
          MOVE 'DESVIO'   TO WA_COLOR-FNAME.
          MOVE '6'        TO WA_COLOR-COLOR-COL.
          MOVE '1'        TO WA_COLOR-COLOR-INT.
          MOVE '1'        TO WA_COLOR-COLOR-INV.
          APPEND WA_COLOR TO IT_COLOR.
          W_RELATORIO-CELL_COLOR[] = IT_COLOR[].
        ENDIF.


        IF W_RELATORIO-SZAEH_ <> W_RELATORIO-RZAEH.
          CLEAR WA_COLOR.
          MOVE 'SZAEH_'    TO WA_COLOR-FNAME.
          MOVE '3'        TO WA_COLOR-COLOR-COL.
          MOVE '1'        TO WA_COLOR-COLOR-INT.
          MOVE '1'        TO WA_COLOR-COLOR-INV.
          APPEND WA_COLOR TO IT_COLOR.

          CLEAR WA_COLOR.
          MOVE 'RZAEH'    TO WA_COLOR-FNAME.
          MOVE '3'        TO WA_COLOR-COLOR-COL.
          MOVE '1'        TO WA_COLOR-COLOR-INT.
          MOVE '1'        TO WA_COLOR-COLOR-INV.
          APPEND WA_COLOR TO IT_COLOR.
        ENDIF.

      ELSE.
*         Mudando a cor dos ponto em atraso
        IF W_RELATORIO-DESVIO > 0.
          CLEAR WA_COLOR.
          MOVE 'DESVIO'   TO WA_COLOR-FNAME.
          MOVE '6'        TO WA_COLOR-COLOR-COL.
          MOVE '1'        TO WA_COLOR-COLOR-INT.
          MOVE '1'        TO WA_COLOR-COLOR-INV.
          APPEND WA_COLOR TO IT_COLOR.
          W_RELATORIO-CELL_COLOR[] = IT_COLOR[].
        ENDIF.

        IF W_RELATORIO-SZAEH <> W_RELATORIO-RZAEH.
          CLEAR WA_COLOR.
          MOVE 'SZAEH'    TO WA_COLOR-FNAME.
          MOVE '3'        TO WA_COLOR-COLOR-COL.
          MOVE '1'        TO WA_COLOR-COLOR-INT.
          MOVE '1'        TO WA_COLOR-COLOR-INV.
          APPEND WA_COLOR TO IT_COLOR.

          CLEAR WA_COLOR.
          MOVE 'RZAEH'    TO WA_COLOR-FNAME.
          MOVE '3'        TO WA_COLOR-COLOR-COL.
          MOVE '1'        TO WA_COLOR-COLOR-INT.
          MOVE '1'        TO WA_COLOR-COLOR-INV.
          APPEND WA_COLOR TO IT_COLOR.

        ENDIF.

        W_RELATORIO-CELL_COLOR[] = IT_COLOR[].


        APPEND W_RELATORIO TO T_RELATORIO.

        CLEAR: "W_RELATORIO-TIDNR,
               W_RELATORIO-EQUNR,
               W_RELATORIO-DESV_PL,
               W_RELATORIO-IDAT2,
               W_RELATORIO-IDAT3,
               W_RELATORIO-PLTXT,
               W_RELATORIO-EQKTX,
               W_RELATORIO-ORDEM,
               W_RELATORIO-SHTXT,
               W_RELATORIO-EARTX,
               W_RELATORIO-CELL_COLOR[],
               T_VIMHIS,
               W_RELATORIO-CELL_COLOR,
               IT_COLOR[].
        EXIT.
      ENDIF.
    ENDIF.

    IF T_VIMHIS-TSVBT NE SPACE AND T_VIMHIS-TSABR EQ SPACE.
      IF T_VIMHIS-TSENQ NE SPACE.                      "Espera.
        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-SZAEH P_ZEIEH.
        W_RELATORIO-SZAEH  =  FLTP_CHAR+14(8).       " Posição do contador.

        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR T_VIMHIS-NZAEH P_ZEIEH.
        W_RELATORIO-NZAEH  =  FLTP_CHAR+14(8).       " Proxima leitura programa
        CONDENSE W_RELATORIO-NZAEH.

        IF T_MPOS-WSTRA IS INITIAL.
          GV_USO    = WA_IMRG-READG - T_VIMHIS-NZAEH + MMPT-ZYKL1.
          GV_DESVIO = GV_USO - MMPT-ZYKL1.
        ELSE.
          GV_USO    = WA_IMRG-READG - T_VIMHIS-NZAEH + MMPT_TAB-ZYKL1.
          GV_DESVIO = GV_USO - MMPT_TAB-ZYKL1.
        ENDIF.

        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR GV_USO P_ZEIEH.
        W_RELATORIO-USO    =  FLTP_CHAR+14(8).       " Uso
        CONDENSE W_RELATORIO-USO.

        PERFORM FLTP_CHAR_CONVERSION_PAK_F40 USING FLTP_CHAR GV_DESVIO P_ZEIEH.
        W_RELATORIO-DESVIO =  FLTP_CHAR+14(8).       " Uso
        CONDENSE W_RELATORIO-DESVIO.

        IF W_RELATORIO-ZYKL1 > 0.
          W_RELATORIO-PER_DESV = ( W_RELATORIO-DESVIO / W_RELATORIO-ZYKL1 ) * 100.
        ELSE.
          W_RELATORIO-PER_DESV = 0.
        ENDIF.


        PERFORM AGRUPA_CALC USING P_WARPL W_RELATORIO-IDAT2 W_RELATORIO-IDAT3.
        PERFORM CALC USING P_DT P_WARPL.



        IF P_DT IS NOT INITIAL.
*          Mudando a cor dos ponto em atraso
          IF W_RELATORIO-DESVIO > 0.
            CLEAR WA_COLOR.
            MOVE 'DESVIO'   TO WA_COLOR-FNAME.
            MOVE '6'        TO WA_COLOR-COLOR-COL.
            MOVE '1'        TO WA_COLOR-COLOR-INT.
            MOVE '1'        TO WA_COLOR-COLOR-INV.
            APPEND WA_COLOR TO IT_COLOR.
            W_RELATORIO-CELL_COLOR[] = IT_COLOR[].
          ENDIF.

          IF W_RELATORIO-SZAEH_ <> W_RELATORIO-RZAEH.
            CLEAR WA_COLOR.
            MOVE 'SZAEH_'    TO WA_COLOR-FNAME.
            MOVE '3'        TO WA_COLOR-COLOR-COL.
            MOVE '1'        TO WA_COLOR-COLOR-INT.
            MOVE '1'        TO WA_COLOR-COLOR-INV.
            APPEND WA_COLOR TO IT_COLOR.

            CLEAR WA_COLOR.
            MOVE 'RZAEH'    TO WA_COLOR-FNAME.
            MOVE '3'        TO WA_COLOR-COLOR-COL.
            MOVE '1'        TO WA_COLOR-COLOR-INT.
            MOVE '1'        TO WA_COLOR-COLOR-INV.
            APPEND WA_COLOR TO IT_COLOR.
          ENDIF.

        ELSE.
*         Mudando a cor dos ponto em atraso
          IF W_RELATORIO-DESVIO > 0.
            CLEAR WA_COLOR.
            MOVE 'DESVIO'   TO WA_COLOR-FNAME.
            MOVE '6'        TO WA_COLOR-COLOR-COL.
            MOVE '1'        TO WA_COLOR-COLOR-INT.
            MOVE '1'        TO WA_COLOR-COLOR-INV.
            APPEND WA_COLOR TO IT_COLOR.
            W_RELATORIO-CELL_COLOR[] = IT_COLOR[].
          ENDIF.

          IF W_RELATORIO-SZAEH <> W_RELATORIO-RZAEH.
            CLEAR WA_COLOR.
            MOVE 'SZAEH'    TO WA_COLOR-FNAME.
            MOVE '3'        TO WA_COLOR-COLOR-COL.
            MOVE '1'        TO WA_COLOR-COLOR-INT.
            MOVE '1'        TO WA_COLOR-COLOR-INV.
            APPEND WA_COLOR TO IT_COLOR.

            CLEAR WA_COLOR.
            MOVE 'RZAEH'    TO WA_COLOR-FNAME.
            MOVE '3'        TO WA_COLOR-COLOR-COL.
            MOVE '1'        TO WA_COLOR-COLOR-INT.
            MOVE '1'        TO WA_COLOR-COLOR-INV.
            APPEND WA_COLOR TO IT_COLOR.
          ENDIF.
        ENDIF.



        W_RELATORIO-CELL_COLOR[] = IT_COLOR[].
*
        APPEND W_RELATORIO TO T_RELATORIO.
        CLEAR:
               W_RELATORIO-EQUNR,
               W_RELATORIO-PLTXT,
               W_RELATORIO-EQKTX,
               W_RELATORIO-ORDEM,
               W_RELATORIO-SHTXT,
               W_RELATORIO-EARTX,
               W_RELATORIO-DESV_PL,
               W_RELATORIO-IDAT2,
               W_RELATORIO-IDAT3,
               W_RELATORIO-CELL_COLOR[],
               T_VIMHIS,
               W_RELATORIO-CELL_COLOR,
               IT_COLOR[].
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM T_RELATORIO COMPARING IWERK EARTX WARPL ZYKL1(8) PAK_TEXT WPTXT SZAEH.

ENDFORM.                    "obtem_contadores

*&---------------------------------------------------------------------*
*&      Form  fltp_char_conversion_pak_f40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CHAR_WERT  text
*      -->FLTP_WERT  text
*      -->EINHEIT    text
*----------------------------------------------------------------------*
FORM FLTP_CHAR_CONVERSION_PAK_F40 USING CHAR_WERT
                                        FLTP_WERT
                                        EINHEIT.
  CLEAR CHAR_WERT.
  CHECK NOT EINHEIT IS INITIAL.

  CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
    EXPORTING
      CHAR_UNIT       = EINHEIT
      DECIMALS        = 0
      EXPONENT        = 0
      FLTP_VALUE_SI   = FLTP_WERT
      INDICATOR_VALUE = CC_X
      MASC_SYMBOL     = ' '
    IMPORTING
      CHAR_VALUE      = CHAR_WERT.

ENDFORM.                    "fltp_char_conversion_pak_f40

*&---------------------------------------------------------------------*
*&      Form  exclui_inativo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EXCLUI_INATIVO.
  DATA: LV_TABIX      TYPE SY-TABIX.
* delete duplicate indices
  SORT T_JEST BY OBJNR.

  DELETE ADJACENT DUPLICATES FROM T_JEST COMPARING OBJNR.

  LOOP AT T_JEST.
    DELETE T_EQUI WHERE OBJNR = T_JEST-OBJNR.
  ENDLOOP.

ENDFORM.                    "exclui_inativo

*&---------------------------------------------------------------------*
*&      Form  Obtem_Pos_Tot_contador
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->POINT      text
*----------------------------------------------------------------------*
FORM OBTEM_POS_TOT_CONTADOR USING POINT.

  DATA : NO_BELEG LIKE SY-SUBRC,
         FLTP     TYPE IMRC_TOTAC.

  CALL FUNCTION 'MEASUREM_POINT_LAST_VALUE'
    EXPORTING
      I_POINT           = POINT
    IMPORTING
      E_WA_POINT        = WA_IMPT
      E_POINT_TXT       = POINT_TXT
      E_WA_VALUE        = WA_IMRG
      E_NO_VALUE        = NO_BELEG
    EXCEPTIONS
      POINTER_NOT_FOUND = 01.

  CASE SY-SUBRC.
    WHEN 0.
      IF NO_BELEG IS INITIAL.
        IF NOT WA_IMRG-READG IS INITIAL.
          PERFORM FLTP_CHAR_CONVERSION_PAK_F40
             USING FLTP_CHAR
                   WA_IMRG-READG
                   WA_IMPT-MSEHI.
          W_RELATORIO-TOTAC  =  FLTP_CHAR+14(8).
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      CLEAR: W_RELATORIO-TOTAC.
  ENDCASE.

ENDFORM.                    "Obtem_Pos_Tot_contador


*&---------------------------------------------------------------------*
*&      Form  obtem_ciclos_strat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STRAT    text
*      -->P_WARPL    text
*----------------------------------------------------------------------*
FORM OBTEM_CICLOS_STRAT USING P_STRAT P_WARPL.

  CALL FUNCTION 'IWP1_STRATEGY_READ'
    EXPORTING
      I_STRAT                   = P_STRAT
      I_MULTICOUNTERPLAN        = ' '
    TABLES
      T_PACKAGE_DATA            = G_T_PACKAGE_DATA
    CHANGING
      C_T351                    = G_W_T351
    EXCEPTIONS
      STRATEGY_IS_INITIAL       = 1
      STRATEGY_NOT_FOUND        = 2
      STRATEGY_WITHOUT_PACKAGES = 3
      NO_CYCLE_SET              = 4
      NO_STRATEGY               = 5
      OTHERS                    = 6.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  CLEAR    G_T_PACKAGE_DATA_ST_TL.
  REFRESH  G_T_PACKAGE_DATA_ST_TL.
  CLEAR IMPOS.    REFRESH IMPOS.
  MOVE-CORRESPONDING T_MPOS TO IMPOS.
  MOVE T_MPOS-WPPOS         TO IMPOS-POSNR.
  MOVE CC_X                 TO IMPOS-DBKNZ.
  APPEND IMPOS.
  SORT IMPOS BY POSNR.

*--- determine the used packages in plan/item for
*    strategy releated scheduling
  CHECK NOT P_STRAT IS INITIAL.

  IF IMPOS-PLNAL NE SPACE AND
     IMPOS-PLNNR NE SPACE.
*--- update table with package data
    PERFORM UPDATE_PACKAGE_DATA_ST_TL USING IMPOS
                                             ' '  " t399w-call_type
                                             ' '
                                             '19000101'.
  ENDIF.

*--- sort table
  SORT G_T_PACKAGE_DATA_ST_TL ASCENDING BY ZAEHLSTRAT
                                           DATUVPLWP.

*--- determine the number of used packages
  DESCRIBE TABLE G_T_PACKAGE_DATA_ST_TL LINES ANZ_VERPAK.


ENDFORM.                    "obtem_ciclos_strat

*&---------------------------------------------------------------------*
*&      Form  update_package_data_st_tl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITEM         text
*      -->P_CALL_OBJECT  text
*      -->P_ACTIVITY     text
*      -->P_DATE_BEGIN   text
*----------------------------------------------------------------------*
FORM UPDATE_PACKAGE_DATA_ST_TL USING P_ITEM        TYPE TY_WMPOS
                                     P_CALL_OBJECT LIKE T399W-CALL_TYPE
                                     P_ACTIVITY    TYPE C
                                     P_DATE_BEGIN  LIKE SY-DATUM.

  DATA: L_T_PLWP LIKE PLWP OCCURS 0 WITH HEADER LINE.

  IF NOT P_ACTIVITY = 1.
    CALL FUNCTION 'CI03_READ_PLWP_TIME_INTERVAL'
      EXPORTING
        I_PLNTY      = P_ITEM-PLNTY
        I_PLNNR      = P_ITEM-PLNNR
        I_PLNAL      = P_ITEM-PLNAL
        I_DATE_BEGIN = P_DATE_BEGIN
        I_DATE_END   = '99991231'
      IMPORTING
        E_PLWP_TAB   = L_T_PLWP[].
  ENDIF.


  CALL FUNCTION 'Z_PACKAGES_STRATEGY_TL'
    EXPORTING
      I_ACTIVITY               = P_ACTIVITY
      I_ITEM                   = P_ITEM
      I_CALL_OBJECT            = P_CALL_OBJECT
    TABLES
      T_PACKAGE_DATA_ST_TL     = G_T_PACKAGE_DATA_ST_TL
      T_PACKAGE_DATA_TASK_LIST = L_T_PLWP
      T_PACKAGE_DATA_STRATEGY  = G_T_PACKAGE_DATA.
  PERFORM WSTRA_MMPT_F10.

ENDFORM.                    "update_package_data_st_tl

*&---------------------------------------------------------------------*
*&      Form  wstra_mmpt_f10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM WSTRA_MMPT_F10.

  CLEAR MMPT_TAB.
  REFRESH MMPT_TAB.


  LOOP AT G_T_PACKAGE_DATA.
    CLEAR MMPT_TAB.
    MOVE: T_MPOS-WARPL                TO MMPT_TAB-WARPL,
          G_T_PACKAGE_DATA-PAKETT351X TO MMPT_TAB-NUMMER,
          G_T_PACKAGE_DATA-ZEIEHT351P TO MMPT_TAB-ZEIEH,
          G_T_PACKAGE_DATA-ZYKZTT351P TO MMPT_TAB-ZYKL1,
          G_T_PACKAGE_DATA-OFFZTT351P TO MMPT_TAB-OFFSET,
          G_T_PACKAGE_DATA-KTEX1T351X TO MMPT_TAB-PAK_TEXT,
          G_T_PACKAGE_DATA-KOFF1T351X TO MMPT_TAB-KOFF1,
          G_T_PACKAGE_DATA-KZYK1T351X TO MMPT_TAB-KZYK1.

    APPEND MMPT_TAB.

  ENDLOOP.
ENDFORM.                               " WSTRA_MMPT_F10

*&---------------------------------------------------------------------*
*&      Form  index_mmpt_used_packages
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WPPOS    text
*----------------------------------------------------------------------*
FORM INDEX_MMPT_USED_PACKAGES USING P_WPPOS LIKE MPOS-WPPOS.


  DATA:   SAVE_TABIX LIKE SY-TABIX.
  RANGES: R_WPPOS    FOR G_T_PACKAGE_DATA_ST_TL-WPPOS.



  IF NOT P_WPPOS IS INITIAL.
    R_WPPOS-SIGN   = 'I'.
    R_WPPOS-OPTION = 'EQ'.
    R_WPPOS-LOW    = P_WPPOS.
    APPEND R_WPPOS.
  ENDIF.
  STTAG = SY-DATUM.
  CALL FUNCTION 'CI03_READ_PLKO'
    EXPORTING
      I_PLNTY         = T_MPOS-PLNTY
      I_PLNNR         = T_MPOS-PLNNR
      I_PLNAL         = T_MPOS-PLNAL
      I_DATE          = STTAG
    EXCEPTIONS
      TL_NOT_EXISTENT = 1
      TL_NOT_VALID    = 2.

  CASE SY-SUBRC.
    WHEN 1.
      CONCATENATE 'A lista de tarefa' T_MPOS-PLNTY T_MPOS-PLNNR T_MPOS-PLNAL 'não existe.' INTO MSG.
      MESSAGE MSG TYPE 'E'.
      EXIT.
    WHEN 2.
      CONCATENATE 'Não existe lista de tarefas'  T_MPOS-PLNTY T_MPOS-PLNNR T_MPOS-PLNAL 'na data fixada' INTO MSG.
      MESSAGE MSG TYPE 'E'.
      EXIT.
  ENDCASE.

*--- init data
  CLEAR SAVE_TABIX.
  CLEAR INDEX_MMPT.
  REFRESH INDEX_MMPT.

  SELECT SINGLE * FROM  T399W
  WHERE  MPTYP       = T_MPOS-MITYP.


  CALL FUNCTION 'Z_PACKAGES_SET_IND_VALIDITY'
    EXPORTING
      I_DATE               = STTAG
      I_CALL_OBJECT        = T399W-CALL_TYPE
      I_ITEM_ROW_NO        = P_WPPOS
    TABLES
      T_PACKAGE_DATA_ST_TL = G_T_PACKAGE_DATA_ST_TL.

  LOOP AT MMPT_TAB
    WHERE UPD_KNZ NE WC_DELE.
    SAVE_TABIX = SY-TABIX.
    LOOP AT G_T_PACKAGE_DATA_ST_TL
      WHERE WPPOS      IN R_WPPOS           AND
            ZAEHLSTRAT =  MMPT_TAB-NUMMER.
      IF NOT G_T_PACKAGE_DATA_ST_TL-IND_VALID IS INITIAL.
        MOVE SAVE_TABIX TO INDEX_MMPT-TABPT.
        APPEND INDEX_MMPT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  SORT INDEX_MMPT BY TABPT.
  DELETE ADJACENT DUPLICATES FROM INDEX_MMPT COMPARING TABPT.

  DESCRIBE TABLE INDEX_MMPT LINES MMPT_MAX.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  STATUS_CHECK_PLANO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->MPLA_OBJNR   text
*      -->RETURN_CODE  text
*----------------------------------------------------------------------*
FORM STATUS_CHECK_PLANO USING MPLA_OBJNR RETURN_CODE.

  CONSTANTS: Y_I0013_LOKZ LIKE JEST-STAT  VALUE 'I0013',
             Y_I0076_LOVM LIKE JEST-STAT  VALUE 'I0076',
             Y_I0320_INAK LIKE JEST-STAT  VALUE 'I0320'.

  DATA: BEGIN OF IJSTAT OCCURS   0.
          INCLUDE STRUCTURE JSTAT.
        DATA: END OF IJSTAT.

  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      CLIENT      = SY-MANDT
      OBJNR       = MPLA_OBJNR
      ONLY_ACTIVE = 'X'
    TABLES
      STATUS      = IJSTAT.

  LOOP AT IJSTAT.

    CASE IJSTAT-STAT.
      WHEN Y_I0013_LOKZ.
        RETURN_CODE = 1.
      WHEN Y_I0076_LOVM.
        RETURN_CODE = 2.
        EXIT.
      WHEN Y_I0320_INAK.               "Inativo
        RETURN_CODE = 3.
      WHEN OTHERS.
        RETURN_CODE = 0.
    ENDCASE.

  ENDLOOP.
ENDFORM.                    "STATUS_CHECK_PLANO
*&---------------------------------------------------------------------*
*&      Form  CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DT  text
*      -->P_P_WARPL  text
*----------------------------------------------------------------------*
FORM CALC  USING    V_DT
                    V_WARPL.


  IF V_DT IS NOT INITIAL.
    TRY .
        DATA(WA) = IT_CALC[ WARPL = V_WARPL ].
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
        CLEAR WA.
    ENDTRY.

    W_RELATORIO-USO       = WA-USO.
    W_RELATORIO-DESVIO    = WA-DESVIO.
    W_RELATORIO-PER_DESV  = WA-PER_DESV.
    W_RELATORIO-SZAEH_    = WA-STADT.
    W_RELATORIO-NZAEH_    = WA-NPLDA.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AGRUPA_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AGRUPA_CALC USING V_WARPL V_IDAT2 V_IDAT3.


  IF P_DT IS NOT INITIAL.

    DATA: CONVERT      TYPE I,
          DATA_PROXIMA TYPE MHIS-LRMDT.

    SORT IT_CALC BY WARPL LRMDT DESCENDING.

*    LOOP AT IT_CALC INTO DATA(WA_CALC)
*      WHERE WARPL EQ V_WARPL.
*
*      IF SY-TABIX > 2.
*        EXIT.
*      ELSE.
*        READ TABLE IT_CALC INTO DATA(WCALC) INDEX SY-TABIX.
*        IF SY-TABIX EQ 1.
*          DATA(VDATA1) = V_IDAT2.
*        ENDIF.
*
*        IF SY-TABIX EQ 2.
*          DATA(VDATA2) = V_IDAT3.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

    IF V_IDAT2 IS NOT INITIAL AND V_IDAT3 IS NOT INITIAL.
      W_RELATORIO-DESV_PL = ( V_IDAT2 - V_IDAT3 ).
    ENDIF.

    SORT IT_CALC BY  WARPL LRMDT.
    LOOP AT IT_CALC ASSIGNING FIELD-SYMBOL(<CALC>) WHERE WARPL EQ V_WARPL.

      <CALC>-LRMDT = V_IDAT2.

      PERFORM FLTP_CHAR_CONVERSION_PAK_F40
          USING FLTP_CHAR
                <CALC>-ZYKL1
                <CALC>-ZEIEH.

      CONDENSE FLTP_CHAR NO-GAPS.

      <CALC>-ZYKL2 = FLTP_CHAR. "CICLO

      IF <CALC>-STADT IS NOT INITIAL AND <CALC>-LRMDT IS NOT INITIAL.
        DATA(DT_MENOR1) = SY-DATUM - <CALC>-STADT.
        DATA(DT_MENOR2) = SY-DATUM - <CALC>-LRMDT.
        IF DT_MENOR1 < DT_MENOR2.
          DATA_PROXIMA = <CALC>-STADT.
        ELSE.
          DATA_PROXIMA = <CALC>-LRMDT.
        ENDIF.
      ELSEIF <CALC>-STADT IS NOT INITIAL AND <CALC>-LRMDT IS INITIAL.
        DATA_PROXIMA = <CALC>-STADT.
      ELSEIF <CALC>-STADT IS INITIAL AND <CALC>-LRMDT IS NOT INITIAL.
        DATA_PROXIMA = <CALC>-LRMDT.
      ENDIF.

      CASE <CALC>-ZEIEH.
        WHEN 'WCH'. "Semanas
          CONVERT =  ( ( SY-DATUM - DATA_PROXIMA ) / 7 ).  "CALC USO SEMANAS
        WHEN 'MON'. "Meses
          CONVERT =  ( ( SY-DATUM - DATA_PROXIMA ) / 30 ). "CALC USO MESES
        WHEN 'TAG'. "Dias
          CONVERT =  (   SY-DATUM - DATA_PROXIMA ).          "CALC USO DIAS
      ENDCASE.

      <CALC>-USO =  CONVERT. "CONVERT USO

      CONVERT =  <CALC>-USO - <CALC>-ZYKL2. " CALC DESVIO
      <CALC>-DESVIO =  CONVERT. " CONVERT DESVIO

      CONVERT = CONVERT * -1.
      <CALC>-PER_DESV =  ( ( CONVERT / <CALC>-ZYKL2 ) * 100 ). " PORCENTAGEM DO DESVIO
      "      W_RELATORIO-LRMDT = <CALC>-LRMDT.

    ENDLOOP.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEL_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN_FIELDNAME  text
*----------------------------------------------------------------------*
FORM SEL_ORDEM  USING P_ROW P_COLUMN_FIELDNAME.

  FREE CLICKS.
  ADD 1 TO CLICKS.

  FREE T_PLANOS.
  MOVE-CORRESPONDING T_RELATORIO TO T_PLANOS.
  TRY .
      DATA(WA_PLANOS) = T_PLANOS[ P_ROW ].
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
  ENDTRY.

  CASE P_COLUMN_FIELDNAME.

    WHEN 'ORDEM'.
      WA_PLANOS-WARPL = |{ WA_PLANOS-WARPL ALPHA = IN }|.

      SELECT *
      FROM VIAUFKST
      INTO CORRESPONDING FIELDS OF TABLE T_ORDEM
      WHERE WARPL EQ WA_PLANOS-WARPL.
      SORT T_ORDEM ASCENDING BY ERDAT.

      SELECT *
      FROM VIQMEL
      INTO CORRESPONDING FIELDS OF TABLE T_VIQMEL
      WHERE IWERK EQ WA_PLANOS-IWERK
      AND WARPL EQ WA_PLANOS-WARPL.
      SORT T_VIQMEL ASCENDING BY QMDAT.

      IF NOT T_ORDEM IS INITIAL.
        FREE LINHA_SELECIONADA.
        FREE _EXIT.
        LOOP AT T_ORDEM ASSIGNING FIELD-SYMBOL(<W_ORDEM>).

          IF <W_ORDEM>-OBJNR IS NOT INITIAL.
            CALL FUNCTION 'STATUS_TEXT_EDIT'
              EXPORTING
                FLG_USER_STAT    = YX
                OBJNR            = <W_ORDEM>-OBJNR             "1695763
                SPRAS            = SY-LANGU
              IMPORTING
                LINE             = <W_ORDEM>-STTXT             "1695763
                USER_LINE        = <W_ORDEM>-ASTTX             "1695763
              EXCEPTIONS
                OBJECT_NOT_FOUND = 1
                OTHERS           = 2.

            IF SY-SUBRC = 0.
              <W_ORDEM>-STATUS = <W_ORDEM>-STTXT.
              <W_ORDEM>-STATUS = <W_ORDEM>-STATUS(4).
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF ( T_ORDEM IS NOT INITIAL ).

          DATA(TL_FIELDCAT) = VALUE SLIS_T_FIELDCAT_ALV(

          ( FIELDNAME = 'BUKRS     '        SELTEXT_M = 'Empresa '  OUTPUTLEN = '07' )
          ( FIELDNAME = 'WERKS     '        SELTEXT_M = 'Centro  '  OUTPUTLEN = '04' )
          ( FIELDNAME = 'WARPL     '        SELTEXT_M = 'Plano   '  OUTPUTLEN = '10' )
          ( FIELDNAME = 'AUFNR     '        SELTEXT_M = 'Ordem   '  OUTPUTLEN = '10' )
          ( FIELDNAME = 'STATUS    '        SELTEXT_M = 'Status  '  OUTPUTLEN = '10' )
          ( FIELDNAME = 'ERDAT     '        SELTEXT_M = 'Data    '  OUTPUTLEN = '10' ) ).

          CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
            EXPORTING
              I_TITLE     = 'Selecionar ordem de plano'
              I_SELECTION = 'X'
              I_TABNAME   = 'T_ORDEM'
              I_ZEBRA     = 'X'
              IT_FIELDCAT = TL_FIELDCAT
            IMPORTING
              ES_SELFIELD = LINHA_SELECIONADA
              E_EXIT      = _EXIT
            TABLES
              T_OUTTAB    = T_ORDEM.

        ELSE.
          FREE LINHA_SELECIONADA.
          FREE _EXIT.
          CHECK ( T_VIQMEL IS NOT INITIAL ).
          LOOP AT T_VIQMEL ASSIGNING FIELD-SYMBOL(<W_VIQMEL>).
            IF <W_VIQMEL>-OBJNR IS NOT INITIAL.
              CALL FUNCTION 'STATUS_TEXT_EDIT'
                EXPORTING
                  FLG_USER_STAT    = YX
                  OBJNR            = <W_VIQMEL>-OBJNR             "1695763
                  SPRAS            = SY-LANGU
                IMPORTING
                  LINE             = <W_VIQMEL>-STTXT             "1695763
                  USER_LINE        = <W_VIQMEL>-ASTTX             "1695763
                EXCEPTIONS
                  OBJECT_NOT_FOUND = 1
                  OTHERS           = 2.

              IF SY-SUBRC = 0.
                <W_VIQMEL>-STATUS = <W_VIQMEL>-STTXT.
                <W_VIQMEL>-STATUS = <W_VIQMEL>-STATUS(4).
              ENDIF.
            ENDIF.
          ENDLOOP.

          DATA(_TL_FIELDCAT) = VALUE SLIS_T_FIELDCAT_ALV(

          ( FIELDNAME = 'BUKRS     '        SELTEXT_M = 'Empresa       '  OUTPUTLEN = '07' )
          ( FIELDNAME = 'IWERK     '        SELTEXT_M = 'Centro        '  OUTPUTLEN = '04' )
          ( FIELDNAME = 'WARPL     '        SELTEXT_M = 'Plano         '  OUTPUTLEN = '10' )
          ( FIELDNAME = 'QMNUM     '        SELTEXT_M = 'Nota          '  OUTPUTLEN = '10' )
          ( FIELDNAME = 'STATUS    '        SELTEXT_M = 'Status        '  OUTPUTLEN = '10' )
          ( FIELDNAME = 'QMDAT     '        SELTEXT_M = 'Data da nota  '  OUTPUTLEN = '10' ) ).

          CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
            EXPORTING
              I_TITLE     = 'Selecionar notas de plano'
              I_SELECTION = 'X'
              I_TABNAME   = 'T_VIQMEL'
              I_ZEBRA     = 'X'
              IT_FIELDCAT = _TL_FIELDCAT
            IMPORTING
              ES_SELFIELD = LINHA_SELECIONADA
              E_EXIT      = _EXIT
            TABLES
              T_OUTTAB    = T_VIQMEL.

        ENDIF.
      ELSE.
        MESSAGE 'Não existe ordem para o plano selecionado' TYPE 'I' DISPLAY LIKE 'I'.
        EXIT.
      ENDIF.

      CASE LINHA_SELECIONADA-FIELDNAME.
        WHEN 'AUFNR' OR 'W_ORDEM'.
          SET PARAMETER ID 'ANR' FIELD LINHA_SELECIONADA-VALUE.
          CALL TRANSACTION 'IW32' AND SKIP FIRST SCREEN .
        WHEN 'WARPL'.
          SET PARAMETER ID 'MPL' FIELD LINHA_SELECIONADA-VALUE.
          CALL TRANSACTION 'IP10' AND SKIP FIRST SCREEN.
        WHEN 'QMNUM'.
          SET PARAMETER ID 'IQM' FIELD LINHA_SELECIONADA-VALUE.
          CALL TRANSACTION 'IW22' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.

    WHEN 'WARPL'.
      WA_PLANOS-WARPL = |{ WA_PLANOS-WARPL ALPHA = IN }|.
      SET PARAMETER ID 'MPL' FIELD WA_PLANOS-WARPL.
      CALL TRANSACTION 'IP10' AND SKIP FIRST SCREEN.

    WHEN 'EQUNR'.
      SET PARAMETER ID 'EQN' FIELD WA_PLANOS-EQUNR.
      CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.

    WHEN 'W_ORDEM'.
      SET PARAMETER ID 'ANR' FIELD WA_PLANOS-W_ORDEM.
      CALL TRANSACTION 'IW32' AND SKIP FIRST SCREEN .


  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IT_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1335   text
*      -->P_1336   text
*      -->P_1337   text
*      -->P_1338   text
*      -->P_1339   text
*      -->P_1340   text
*      -->P_1341   text
*      -->P_1342   text
*----------------------------------------------------------------------*
FORM IT_CATALOG  USING VALUE(P_COLNUM)
                  VALUE(P_FIELDNAME)
                  VALUE(P_TABNAME)
                  VALUE(P_LEN)
                  VALUE(P_EDIT)
                  VALUE(P_ICON)
                  VALUE(P_DO_SUM)
                  VALUE(P_HEADER)
                  VALUE(P_EMPHASIZE)
                  VALUE(P_HOTSPOT)
                  VALUE(P_REF_TABLE)
                  VALUE(P_REF_FIELD)
                  VALUE(P_NO_ZERO)
                  VALUE(P_OUTPUTLEN).



  DATA:  WA_FIELDCATALOG  TYPE LVC_S_FCAT.
  CLEAR: WA_FIELDCATALOG,
  GS_LAYOUT.

  WA_FIELDCATALOG-COL_POS     = P_COLNUM.
  WA_FIELDCATALOG-FIELDNAME   = P_FIELDNAME.
  WA_FIELDCATALOG-TABNAME     = P_TABNAME.
  WA_FIELDCATALOG-OUTPUTLEN   = P_LEN.
  WA_FIELDCATALOG-EDIT        = P_EDIT.
  WA_FIELDCATALOG-ICON        = P_ICON.
  WA_FIELDCATALOG-DO_SUM      = P_DO_SUM.
  WA_FIELDCATALOG-COLTEXT     = P_HEADER.
  WA_FIELDCATALOG-EMPHASIZE   = P_EMPHASIZE.
  WA_FIELDCATALOG-HOTSPOT     = P_HOTSPOT.
  WA_FIELDCATALOG-REF_TABLE   = P_REF_TABLE.
  WA_FIELDCATALOG-REF_TABLE   = P_REF_FIELD.
  WA_FIELDCATALOG-NO_ZERO     = P_NO_ZERO.
  WA_FIELDCATALOG-OUTPUTLEN   = P_OUTPUTLEN.

  GS_LAYOUT-INFO_FNAME     = 'ROWCOLOR'.  "Row color
  GS_LAYOUT-CTAB_FNAME     = 'CELL_COLOR'.
  GS_LAYOUT-EXCP_CONDS     = 'X'.
  GS_LAYOUT-ZEBRA          = 'X'.
  GS_LAYOUT-SEL_MODE       = 'A'.
  GS_LAYOUT-CWIDTH_OPT     = 'X'.     "  Otimizar colunas na tela
  GS_LAYOUT-TOTALS_BEF     = ' '.

  APPEND WA_FIELDCATALOG TO IT_S_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ENDMETHOD  text
*----------------------------------------------------------------------*
FORM F_BDC_DATA  USING    P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.

  APPEND VALUE #(
                PROGRAM   = P_PROGRAM
                DYNPRO    = P_DYNPRO
                DYNBEGIN  = P_START
                FNAM      = P_FNAM
                FVAL      = P_FVAL
  ) TO TI_BDCDATA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1709   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*


FORM ZF_CALL_TRANSACTION USING P_TRANS CHANGING P_ERRO.

  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'F5',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '312',
             C_MSGNE LIKE IT_MSG-MSGNR VALUE '539'.

  DATA: WL_CONT    TYPE SY-TABIX,
        WL_MODE(1).

  CLEAR WL_MODE.
  CLEAR WL_CONT.
  WL_MODE = 'E'.

  CALL TRANSACTION P_TRANS USING TI_BDCDATA
                           MODE WL_MODE
                           MESSAGES INTO IT_MSG.

  CLEAR: WL_CONT.

  IF LINE_EXISTS( IT_MSG[ MSGTYP = 'A' ] ).
    P_ERRO = ABAP_TRUE.
  ELSE.
    IF LINE_EXISTS( IT_MSG[ MSGTYP = 'E' ] ).
      P_ERRO = ABAP_TRUE.
    ENDIF.
  ENDIF.


ENDFORM.
