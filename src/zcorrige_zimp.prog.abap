*&---------------------------------------------------------------------*
*& Report  ZCORRIGE_ZIMP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCORRIGE_ZIMP.


*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
"tables: zimp_lanc_impost.

TYPES:

   " Lançamento de Impostos
    BEGIN OF TY_ZIMP_LANC_IMPOST,
        DOC_IMPOSTO   TYPE  ZIMP_LANC_IMPOST-DOC_IMPOSTO,
        BUKRS         TYPE  ZIMP_LANC_IMPOST-BUKRS,
        LOTE          TYPE  ZIMP_LANC_IMPOST-LOTE,
        DT_VENC       TYPE  ZIMP_LANC_IMPOST-DT_VENC,
        DT_APURACAO   TYPE  ZIMP_LANC_IMPOST-DT_APURACAO,
        MES_APURACAO  TYPE  ZIMP_LANC_IMPOST-MES_APURACAO,
        ANO_APURACAO  TYPE  ZIMP_LANC_IMPOST-ANO_APURACAO,
        OBSERVACAO    TYPE  ZIMP_LANC_IMPOST-OBSERVACAO,
        COD_IMPOSTO   TYPE  ZIMP_LANC_IMPOST-COD_IMPOSTO,
        REF_IMPOSTO   TYPE  ZIMP_LANC_IMPOST-REF_IMPOSTO,
        TP_IMPOSTO    TYPE  ZIMP_LANC_IMPOST-TP_IMPOSTO,
        COD_PGTO      TYPE  ZIMP_LANC_IMPOST-COD_PGTO,
        CONV_BANCO    TYPE  ZIMP_LANC_IMPOST-CONV_BANCO,
        HBKID         TYPE  ZIMP_LANC_IMPOST-HBKID,
        DATA_ATUAL    TYPE  ZIMP_LANC_IMPOST-DATA_ATUAL,
        HORA_ATUAL    TYPE  ZIMP_LANC_IMPOST-HORA_ATUAL,
        USUARIO       TYPE  ZIMP_LANC_IMPOST-USUARIO,
        LOEKZ         TYPE  ZIMP_LANC_IMPOST-LOEKZ,
    END OF TY_ZIMP_LANC_IMPOST,

    BEGIN OF TY_ZIMP_LANC_IMP_CT,
       BUKRS         TYPE ZIMP_LANC_IMP_CT-BUKRS   ,
       DOC_IMPOSTO   TYPE ZIMP_LANC_IMP_CT-DOC_IMPOSTO,
       COD_IMPOSTO   TYPE ZIMP_LANC_IMP_CT-COD_IMPOSTO,
       COD_ABERTURA  TYPE ZIMP_LANC_IMP_CT-COD_ABERTURA,
       BSCHL         TYPE ZIMP_LANC_IMP_CT-BSCHL,
       HKONT         TYPE ZIMP_LANC_IMP_CT-HKONT,
       LIFNR         TYPE ZIMP_LANC_IMP_CT-LIFNR,
       KOSTL         TYPE ZIMP_LANC_IMP_CT-KOSTL,
       GSBER         TYPE ZIMP_LANC_IMP_CT-GSBER,
       VALOR_IMP     TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
       DATA_ATUAL    TYPE ZIMP_LANC_IMP_CT-DATA_ATUAL,
       HORA_ATUAL    TYPE ZIMP_LANC_IMP_CT-HORA_ATUAL,
       USUARIO       TYPE ZIMP_LANC_IMP_CT-USUARIO,
    END OF TY_ZIMP_LANC_IMP_CT.



*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC     TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB TYPE TABLE OF BDCMSGCOLL,

      IT_ZIMP_LANC_IMPOST TYPE TABLE OF TY_ZIMP_LANC_IMPOST,
      IT_ZIMP_LANC_IMP_CT TYPE TABLE OF TY_ZIMP_LANC_IMP_CT,

      IT_COLOR            TYPE TABLE OF LVC_S_SCOL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT TYPE LVC_S_LAYO,

      WA_ZIMP_LANC_IMPOST TYPE TY_ZIMP_LANC_IMPOST,
      WA_ZIMP_LANC_IMP_CT TYPE TY_ZIMP_LANC_IMP_CT,

      WA_COLOR            TYPE LVC_S_SCOL.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
     S_VARIANT  TYPE DISVARIANT           , " Tabela Estrutura co
     T_TOP      TYPE SLIS_T_LISTHEADER    ,
     XS_EVENTS  TYPE SLIS_ALV_EVENT       ,
     EVENTS     TYPE SLIS_T_EVENT         ,
     GD_LAYOUT  TYPE SLIS_LAYOUT_ALV      ,
     T_PRINT    TYPE SLIS_PRINT_ALV       ,
     V_REPORT   LIKE SY-REPID             ,
     T_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
     IT_SETLEAF LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,

     VG_I       TYPE I,
     V_REPID            LIKE SY-REPID,
     V_CAMP(7),      " variável p/ montar campo dinâmico
     V_TEXT(100),    " variável p/ montar texto dinâmico
     V_CONTINUA(1),
     VTOTAL TYPE ZIMP_LANC_IMP_CT-VALOR_IMP VALUE 0.

DATA: REPID            LIKE SY-REPID.
DATA: S_FIELDCAT       TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
DATA: S_LAYOUT         TYPE SLIS_LAYOUT_ALV.
DATA: S_PRINT          TYPE SLIS_PRINT_ALV.
DATA: S_SORT           TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.
DATA: VARIANTE         LIKE DISVARIANT.
DATA: DEF_VARIANTE     LIKE DISVARIANT.
DATA: S_SELFIELD       TYPE SLIS_SELFIELD.
DATA: LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.



DEFINE MC_PREENCHE_CLASS.
  VG_I = VG_I + 1.
  CLEAR T_SORT.
  T_SORT-SPOS      = VG_I.
  T_SORT-FIELDNAME = &1.
  T_SORT-GROUP     = &2.
  T_SORT-UP        = &3.
  T_SORT-SUBTOT    = &4.
  APPEND T_SORT.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
"DATA   DYFIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_BUKRS   TYPE ZIMP_LANC_IMPOST-BUKRS .

SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:      F_SELECIONA_DADOS.



END-OF-SELECTION.



*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .
  DATA: WCONTA TYPE I,
        SCONTA(5),
        MSG(50).
  SELECT DOC_IMPOSTO BUKRS  LOTE DT_VENC DT_APURACAO MES_APURACAO ANO_APURACAO OBSERVACAO COD_IMPOSTO REF_IMPOSTO TP_IMPOSTO COD_PGTO CONV_BANCO
    HBKID  DATA_ATUAL HORA_ATUAL USUARIO LOEKZ
    FROM ZIMP_LANC_IMPOST
    INTO TABLE IT_ZIMP_LANC_IMPOST.


  CHECK IT_ZIMP_LANC_IMPOST[] IS NOT INITIAL.

  SELECT BUKRS DOC_IMPOSTO COD_IMPOSTO COD_ABERTURA BSCHL HKONT LIFNR KOSTL GSBER VALOR_IMP DATA_ATUAL HORA_ATUAL USUARIO
    FROM ZIMP_LANC_IMP_CT
    INTO TABLE IT_ZIMP_LANC_IMP_CT
    FOR ALL ENTRIES IN IT_ZIMP_LANC_IMPOST
    WHERE DOC_IMPOSTO = IT_ZIMP_LANC_IMPOST-DOC_IMPOSTO.


  SORT IT_ZIMP_LANC_IMP_CT BY DOC_IMPOSTO.
  WCONTA = 0.
  LOOP AT IT_ZIMP_LANC_IMPOST INTO WA_ZIMP_LANC_IMPOST.
    UPDATE ZIMP_LANC_IMP_CT SET BUKRS =  WA_ZIMP_LANC_IMPOST-BUKRS
          WHERE DOC_IMPOSTO = WA_ZIMP_LANC_IMPOST-DOC_IMPOSTO.
    LOOP AT IT_ZIMP_LANC_IMP_CT INTO WA_ZIMP_LANC_IMP_CT WHERE DOC_IMPOSTO = WA_ZIMP_LANC_IMPOST-DOC_IMPOSTO.
      ADD 1 TO WCONTA.
    ENDLOOP.
  ENDLOOP.
  MOVE WCONTA TO SCONTA.
  CONCATENATE 'alterados ' SCONTA INTO MSG SEPARATED BY SPACE.
  MESSAGE MSG TYPE 'I'.


ENDFORM.                    " F_SELECIONA_DADOS
*&-----------------------------------------------------------
