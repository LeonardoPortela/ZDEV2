*&---------------------------------------------------------------------*
*& Report  ZFIS22
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFIS22.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: FAGLFLEXT.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*


TYPES:


   BEGIN OF TY_SAIDA,
      CONTA      TYPE FAGLFLEXT-RACCT,
      DESC_CONTA TYPE SKAT-TXT50,
      SALDOR     TYPE FAGLFLEXT-HSLVT,
      SALDOU     TYPE FAGLFLEXT-KSLVT,
      SALDOA     TYPE FAGLFLEXT-KSLVT,
      MEDIA      TYPE P DECIMALS 4, "TCURR-UKURS,
   END OF TY_SAIDA,

   BEGIN OF TY_JOIN,
      CONTA      TYPE FAGLFLEXT-RACCT,
      SHKZG      TYPE BSAS-SHKZG,
      SALDOR     TYPE FAGLFLEXT-HSLVT,
      SALDOU     TYPE FAGLFLEXT-KSLVT,
   END OF TY_JOIN.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC     TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB TYPE TABLE OF BDCMSGCOLL,

      IT_FAGLFLEXT        TYPE TABLE OF FAGLFLEXT,
      IT_FAGLFLEXT2       TYPE TABLE OF FAGLFLEXT,
      IT_ZGL030_DRE_ACM   TYPE TABLE OF ZGL030_DRE_ACM ,
      IT_ZGL030_DRE_ACM2  TYPE TABLE OF ZGL030_DRE_ACM ,
      IT_SKAT             TYPE TABLE OF SKAT,
      IT_BSAS             TYPE TABLE OF BSAS,
      IT_BSIS             TYPE TABLE OF BSIS,
      IT_JOIN             TYPE TABLE OF TY_JOIN,
      IT_JOIN_AUX         TYPE TABLE OF TY_JOIN,
      IT_SAIDA            TYPE TABLE OF TY_SAIDA,
      IT_COLOR            TYPE TABLE OF LVC_S_SCOL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT TYPE LVC_S_LAYO,

      WA_FAGLFLEXT        TYPE FAGLFLEXT,
      WA_FAGLFLEXT2       TYPE FAGLFLEXT,
      WA_ZGL030_DRE_ACM   TYPE ZGL030_DRE_ACM ,
      WA_ZGL030_DRE_ACM2  TYPE ZGL030_DRE_ACM ,
      WA_SKAT             TYPE SKAT,
      WA_BSAS             TYPE BSAS,
      WA_BSIS             TYPE BSIS,
      WA_T001             TYPE T001,
      WA_JOIN             TYPE TY_JOIN,
      WA_JOIN_AUX         TYPE TY_JOIN,
      WA_SAIDA            TYPE TY_SAIDA,
      WA_COLOR            TYPE LVC_S_SCOL.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
     IT_FCAT    TYPE TABLE OF TY_ESTRUTURA,
     S_VARIANT  TYPE DISVARIANT           , " Tabela Estrutura co
     T_TOP      TYPE SLIS_T_LISTHEADER    ,
     XS_EVENTS  TYPE SLIS_ALV_EVENT       ,
     EVENTS     TYPE SLIS_T_EVENT         ,
     GD_LAYOUT  TYPE SLIS_LAYOUT_ALV      ,
     T_PRINT    TYPE SLIS_PRINT_ALV       ,
     V_REPORT   LIKE SY-REPID             ,
     T_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
     IT_SETLEAF LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
     ESTRUTURA  TYPE TABLE OF TY_ESTRUTURA,
     VG_I       TYPE I.

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

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-001.
PARAMETERS: R_DRE   LIKE BSID-UMSKZ AS CHECKBOX USER-COMMAND ACT DEFAULT ' ',
            R_SLD   LIKE BSID-UMSKZ AS CHECKBOX USER-COMMAND ACT DEFAULT ' '.
SELECTION-SCREEN: END OF BLOCK B3.


SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS: P_BUKRS TYPE FAGLFLEXT-RBUKRS OBLIGATORY,
            P_RPMAX TYPE FAGLFLEXT-RPMAX OBLIGATORY,
            P_RYEAR TYPE FAGLFLEXT-RYEAR OBLIGATORY.
SELECT-OPTIONS: P_RACCT       FOR  FAGLFLEXT-RACCT.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-008.
SELECT-OPTIONS P_RPMAX2   FOR FAGLFLEXT-RPMAX MODIF ID A.
SELECTION-SCREEN: END OF BLOCK B4.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF R_SLD = 'X'.
      IF SCREEN-GROUP1 = 'A'.
        "P_RPMAX2-LOW = P_RPMAX.
        SCREEN-ACTIVE = 1.
      ENDIF.
    ELSE.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            F_INICIAR_VARIAVES, " Cabeçalho
            F_SELECIONA_DADOS. " Form seleciona dados

  IF R_DRE = 'X'.
    PERFORM     F_SAIDA_DRE.
  ELSEIF R_SLD = 'X'.
    PERFORM     F_SAIDA_SLD.
  ELSE.
    PERFORM     F_SAIDA.
  ENDIF.
  PERFORM       F_IMPRIME_DADOS.

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
  IF R_DRE = 'X'.
    SELECT *
      FROM ZGL030_DRE_ACM
      INTO TABLE IT_ZGL030_DRE_ACM
      WHERE BUKRS	=	P_BUKRS "P_BUKRS2
      AND   MONAT	=	P_RPMAX "P_MONAT
      AND   GJAHR	=	P_RYEAR
      AND   SAKNR  IN  P_RACCT.

    CHECK IT_ZGL030_DRE_ACM[] IS NOT INITIAL.
    SELECT *
      FROM SKAT
      INTO TABLE IT_SKAT
      FOR ALL ENTRIES IN IT_ZGL030_DRE_ACM
      WHERE SPRAS = 'PT'
      AND KTOPL   = '0050'
      AND SAKNR   = IT_ZGL030_DRE_ACM-SAKNR.
  ELSEIF R_SLD = 'X'.
    SELECT *
      FROM BSAS
      INTO TABLE IT_BSAS
      WHERE BUKRS	= P_BUKRS
      AND   MONAT IN P_RPMAX2
      AND   GJAHR	= P_RYEAR
      AND   HKONT IN  P_RACCT
      AND   BSTAT NE 'S'.

    SELECT *
      FROM BSIS
      INTO TABLE IT_BSIS
      WHERE BUKRS	= P_BUKRS
      AND   MONAT IN  P_RPMAX2
      AND   GJAHR	= P_RYEAR
      AND   HKONT IN  P_RACCT
      AND   BSTAT NE 'S'.


  ELSE.

    SELECT *
      FROM FAGLFLEXT
      INTO TABLE IT_FAGLFLEXT
      WHERE RBUKRS  =  P_BUKRS
      AND RYEAR	=	P_RYEAR
      AND RACCT  IN  P_RACCT
      AND RLDNR = '0L'.

    CHECK IT_FAGLFLEXT[] IS NOT INITIAL.

    SELECT *
      FROM SKAT
      INTO TABLE IT_SKAT
      FOR ALL ENTRIES IN IT_FAGLFLEXT
      WHERE SPRAS = 'PT'
      AND KTOPL   = '0050'
      AND SAKNR   = IT_FAGLFLEXT-RACCT.
  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES .
  DATA:
              W_TEXTO1(10),
              W_TEXTO2(10),
              W_TEXTO3(40),

              W_EMPRESA_TEXTO(40),
              W_EXER_TEXTO(40),
              W_PER_TEXTO(40),

              EMPRESA      TYPE C LENGTH 99,
              EXERCICIO    TYPE C LENGTH 50,
              PERIODO      TYPE C LENGTH 50.

  SELECT SINGLE *
    FROM T001
    INTO WA_T001
    WHERE BUKRS = P_BUKRS.

  V_REPORT = SY-REPID.

  W_TEXTO3 = 'Saldo Contábil'.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO3.

  IF P_BUKRS IS NOT INITIAL.
    W_EMPRESA_TEXTO = 'Empresa    :'.
    CONCATENATE W_EMPRESA_TEXTO P_BUKRS '-' WA_T001-BUTXT INTO EMPRESA SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' EMPRESA.
  ENDIF.

  IF P_RYEAR IS NOT INITIAL.
    W_EXER_TEXTO = 'Exercício  :'.
    CONCATENATE W_EXER_TEXTO P_RYEAR  INTO EXERCICIO SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' EXERCICIO.
  ENDIF.

  IF P_RPMAX IS NOT INITIAL.
    W_EXER_TEXTO = 'Período:'.
    CONCATENATE W_EXER_TEXTO P_RPMAX  INTO PERIODO  SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' PERIODO.
  ENDIF.

ENDFORM.                    " F_INICIAR_VARIAVES


*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TYP        text
*      -->TEXT       text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO    USING TYP TEXT.


  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  IT_FAGLFLEXT2[] = IT_FAGLFLEXT[].
  SORT: IT_SKAT       BY SAKNR,
        IT_FAGLFLEXT  BY RBUKRS  RYEAR RPMAX RACCT ,
        IT_FAGLFLEXT2 BY RBUKRS  RYEAR RPMAX RACCT .

  DATA: TOTAL_REAL TYPE FAGLFLEXT-HSLVT VALUE 0,
        TOTAL_USD  TYPE FAGLFLEXT-KSLVT VALUE 0.

  LOOP AT IT_FAGLFLEXT INTO WA_FAGLFLEXT.
    TOTAL_REAL = 0.
    TOTAL_USD = 0.
    IF WA_FAGLFLEXT-RBUKRS = WA_FAGLFLEXT2-RBUKRS
       AND WA_FAGLFLEXT-RYEAR = WA_FAGLFLEXT2-RYEAR
       AND WA_FAGLFLEXT-RPMAX = WA_FAGLFLEXT2-RPMAX
       AND WA_FAGLFLEXT-RACCT = WA_FAGLFLEXT2-RACCT.
      CONTINUE.
    ENDIF.
    LOOP AT IT_FAGLFLEXT2 INTO WA_FAGLFLEXT2 WHERE RBUKRS = WA_FAGLFLEXT-RBUKRS
                                             AND   RYEAR  = WA_FAGLFLEXT-RYEAR
                                             AND   RPMAX  = WA_FAGLFLEXT-RPMAX
                                             AND   RACCT  = WA_FAGLFLEXT-RACCT.
      IF P_RPMAX = 1.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01.
      ELSEIF P_RPMAX = 2.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02.
      ELSEIF P_RPMAX = 3.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03.
      ELSEIF P_RPMAX = 4.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
      ELSEIF P_RPMAX = 5.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04 + WA_FAGLFLEXT2-HSL05.
      ELSEIF P_RPMAX = 6.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04 + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06.
      ELSEIF P_RPMAX = 7.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04 + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07.
      ELSEIF P_RPMAX = 8.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08.
      ELSEIF P_RPMAX = 9.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08  + WA_FAGLFLEXT2-HSL09.
      ELSEIF P_RPMAX = 10.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08  + WA_FAGLFLEXT2-HSL09 + WA_FAGLFLEXT2-HSL10.
      ELSEIF P_RPMAX = 11.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01  + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08  + WA_FAGLFLEXT2-HSL09.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL10 + WA_FAGLFLEXT2-HSL11 .
      ELSEIF P_RPMAX = 12.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01 + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08  + WA_FAGLFLEXT2-HSL09.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL10 + WA_FAGLFLEXT2-HSL11 + WA_FAGLFLEXT2-HSL12.
      ELSEIF P_RPMAX = 13.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01 + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08  + WA_FAGLFLEXT2-HSL09.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL10 + WA_FAGLFLEXT2-HSL11 + WA_FAGLFLEXT2-HSL12 + WA_FAGLFLEXT2-HSL13.
      ELSEIF P_RPMAX = 14.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01 + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08 + WA_FAGLFLEXT2-HSL09.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL10 + WA_FAGLFLEXT2-HSL11 + WA_FAGLFLEXT2-HSL12 + WA_FAGLFLEXT2-HSL13 + WA_FAGLFLEXT2-HSL14.
      ELSEIF P_RPMAX = 15.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01 + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08 + WA_FAGLFLEXT2-HSL09.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL10 + WA_FAGLFLEXT2-HSL11 + WA_FAGLFLEXT2-HSL12 + WA_FAGLFLEXT2-HSL13 + WA_FAGLFLEXT2-HSL14.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL15.
      ELSEIF P_RPMAX = 16.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSLVT + WA_FAGLFLEXT2-HSL01 + WA_FAGLFLEXT2-HSL02 + WA_FAGLFLEXT2-HSL03 + WA_FAGLFLEXT2-HSL04.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL05 + WA_FAGLFLEXT2-HSL06 + WA_FAGLFLEXT2-HSL07 + WA_FAGLFLEXT2-HSL08 + WA_FAGLFLEXT2-HSL09.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL10 + WA_FAGLFLEXT2-HSL11 + WA_FAGLFLEXT2-HSL12 + WA_FAGLFLEXT2-HSL13 + WA_FAGLFLEXT2-HSL14.
        TOTAL_REAL = TOTAL_REAL + WA_FAGLFLEXT2-HSL15 + WA_FAGLFLEXT2-HSL16.
      ENDIF.

      "USD
      IF P_RPMAX = 1.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01.
      ELSEIF P_RPMAX = 2.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02.
      ELSEIF P_RPMAX = 3.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03.
      ELSEIF P_RPMAX = 4.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
      ELSEIF P_RPMAX = 5.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04 + WA_FAGLFLEXT2-KSL05.
      ELSEIF P_RPMAX = 6.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04 + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06.
      ELSEIF P_RPMAX = 7.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04 + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07.
      ELSEIF P_RPMAX = 8.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08.
      ELSEIF P_RPMAX = 9.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08  + WA_FAGLFLEXT2-KSL09.
      ELSEIF P_RPMAX = 10.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08  + WA_FAGLFLEXT2-KSL09 + WA_FAGLFLEXT2-KSL10.
      ELSEIF P_RPMAX = 11.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01  + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08  + WA_FAGLFLEXT2-KSL09.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL10 + WA_FAGLFLEXT2-KSL11 .
      ELSEIF P_RPMAX = 12.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01 + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08  + WA_FAGLFLEXT2-KSL09.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL10 + WA_FAGLFLEXT2-KSL11 + WA_FAGLFLEXT2-KSL12.
      ELSEIF P_RPMAX = 13.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01 + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08  + WA_FAGLFLEXT2-KSL09.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL10 + WA_FAGLFLEXT2-KSL11 + WA_FAGLFLEXT2-KSL12 + WA_FAGLFLEXT2-KSL13.
      ELSEIF P_RPMAX = 14.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01 + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08 + WA_FAGLFLEXT2-KSL09.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL10 + WA_FAGLFLEXT2-KSL11 + WA_FAGLFLEXT2-KSL12 + WA_FAGLFLEXT2-KSL13 + WA_FAGLFLEXT2-KSL14.
      ELSEIF P_RPMAX = 15.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01 + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08 + WA_FAGLFLEXT2-KSL09.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL10 + WA_FAGLFLEXT2-KSL11 + WA_FAGLFLEXT2-KSL12 + WA_FAGLFLEXT2-KSL13 + WA_FAGLFLEXT2-KSL14.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL15.
      ELSEIF P_RPMAX = 16.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSLVT + WA_FAGLFLEXT2-KSL01 + WA_FAGLFLEXT2-KSL02 + WA_FAGLFLEXT2-KSL03 + WA_FAGLFLEXT2-KSL04.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL05 + WA_FAGLFLEXT2-KSL06 + WA_FAGLFLEXT2-KSL07 + WA_FAGLFLEXT2-KSL08 + WA_FAGLFLEXT2-KSL09.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL10 + WA_FAGLFLEXT2-KSL11 + WA_FAGLFLEXT2-KSL12 + WA_FAGLFLEXT2-KSL13 + WA_FAGLFLEXT2-KSL14.
        TOTAL_USD = TOTAL_USD + WA_FAGLFLEXT2-KSL15 + WA_FAGLFLEXT2-KSL16.
      ENDIF.

    ENDLOOP.
    READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_FAGLFLEXT-RACCT BINARY SEARCH.
    WA_SAIDA-CONTA      = WA_FAGLFLEXT-RACCT.
    WA_SAIDA-DESC_CONTA = WA_SKAT-TXT50.
    WA_SAIDA-SALDOR     = TOTAL_REAL.
    WA_SAIDA-SALDOU     = TOTAL_USD.
    IF TOTAL_USD NE 0.
      WA_SAIDA-MEDIA      = TOTAL_REAL / TOTAL_USD.
    ELSE.
      WA_SAIDA-MEDIA      = 0.
    ENDIF.
    APPEND WA_SAIDA TO IT_SAIDA.
  ENDLOOP.
ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM F_ALV.

  DATA V_NUM(10) TYPE C.
  LOOP AT IT_SAIDA INTO WA_SAIDA.
    V_NUM = WA_SAIDA-CONTA.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = V_NUM
      IMPORTING
        OUTPUT = V_NUM.
    WA_SAIDA-CONTA = V_NUM.
    MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX TRANSPORTING CONTA.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      IS_LAYOUT               = GD_LAYOUT
      "I_CALLBACK_PF_STATUS_SET  = 'SET_PF_STATUS'
      "I_CALLBACK_USER_COMMAND   = 'USER_COMMAND'
      IT_FIELDCAT             = IT_FCAT[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'X'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
*      IS_VARIANT              = VG_VARIANT
    TABLES
      T_OUTTAB                = IT_SAIDA.


ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_SORT .

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV .
  IF R_DRE = 'X' AND  P_BUKRS EQ '0100'.
    PERFORM ALV_PREENCHE_CAT USING:
            'CONTA'        TEXT-002        '15'       ' '     ' '    ' ' ,
            'DESC_CONTA'   TEXT-003        '30'       ' '     ' '    ' ' ,
            'SALDOR'       TEXT-004        '20'       ' '     ' '    ' ' ,
            'SALDOU'       TEXT-005        '20'       ' '     ' '    ' ' ,
            'SALDOA'       TEXT-007        '20'       ' '     ' '    ' ' ,
            'MEDIA'        TEXT-006        '20'       ' '     ' '    ' ' .
  ELSEIF R_SLD = 'X'.
    PERFORM ALV_PREENCHE_CAT USING:
           'CONTA'        TEXT-002        '15'       ' '     ' '    ' ' ,
           'SALDOR'       TEXT-004        '20'       ' '     ' '    ' ' ,
           'SALDOU'       TEXT-005        '20'       ' '     ' '    ' ' .

  ELSE.
    PERFORM ALV_PREENCHE_CAT USING:
               'CONTA'        TEXT-002        '15'       ' '     ' '    ' ' ,
               'DESC_CONTA'   TEXT-003        '30'       ' '     ' '    ' ' ,
               'SALDOR'       TEXT-004        '20'       ' '     ' '    ' ' ,
               'SALDOU'       TEXT-005        '20'       ' '     ' '    ' ' ,
               'MEDIA'        TEXT-006        '20'       ' '     ' '    ' ' .
  ENDIF.
ENDFORM.                    " F_AL
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO  TYPE C
                               P_DESC   TYPE C
                               P_TAM    TYPE C
                               P_HOT    TYPE C
                               P_ZERO   TYPE C
                               P_SOMA   TYPE C.


  DATA: WL_FCAT TYPE TY_ESTRUTURA.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SELTEXT_S = P_DESC.
  WL_FCAT-SELTEXT_M = P_DESC.
  WL_FCAT-SELTEXT_L = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-DO_SUM    = P_SOMA.

  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA_DRE .
  IT_ZGL030_DRE_ACM2[] = IT_ZGL030_DRE_ACM[].
  SORT: IT_SKAT       BY SAKNR,
        IT_ZGL030_DRE_ACM   BY BUKRS  GJAHR MONAT SAKNR ,
        IT_ZGL030_DRE_ACM2  BY BUKRS  GJAHR MONAT SAKNR .


  DATA: TOTAL_REAL   TYPE FAGLFLEXT-HSLVT VALUE 0,
        TOTAL_USD    TYPE FAGLFLEXT-KSLVT VALUE 0,
        TOTAL_GRUPO  TYPE FAGLFLEXT-KSLVT VALUE 0.

  LOOP AT IT_ZGL030_DRE_ACM INTO WA_ZGL030_DRE_ACM.
    TOTAL_REAL = 0.
    TOTAL_USD = 0.
    TOTAL_GRUPO = 0.
    IF WA_ZGL030_DRE_ACM-BUKRS     = WA_ZGL030_DRE_ACM2-BUKRS
       AND WA_ZGL030_DRE_ACM-GJAHR = WA_ZGL030_DRE_ACM2-GJAHR
       AND WA_ZGL030_DRE_ACM-MONAT = WA_ZGL030_DRE_ACM2-MONAT
       AND WA_ZGL030_DRE_ACM-SAKNR = WA_ZGL030_DRE_ACM2-SAKNR.
      CONTINUE.
    ENDIF.
    LOOP AT IT_ZGL030_DRE_ACM2 INTO WA_ZGL030_DRE_ACM2 WHERE BUKRS = WA_ZGL030_DRE_ACM-BUKRS
                                             AND   GJAHR  = WA_ZGL030_DRE_ACM-GJAHR
                                             AND   MONAT  = WA_ZGL030_DRE_ACM-MONAT
                                             AND   SAKNR  = WA_ZGL030_DRE_ACM-SAKNR.

      TOTAL_REAL = TOTAL_REAL + WA_ZGL030_DRE_ACM2-VLR_REA .
      TOTAL_USD = TOTAL_USD + WA_ZGL030_DRE_ACM2-VLR_DOLAR .
      TOTAL_GRUPO = TOTAL_GRUPO + WA_ZGL030_DRE_ACM2-VLR_GRUPO .

    ENDLOOP.
    READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_ZGL030_DRE_ACM-SAKNR BINARY SEARCH.
    WA_SAIDA-CONTA      = WA_ZGL030_DRE_ACM-SAKNR .
    WA_SAIDA-DESC_CONTA = WA_SKAT-TXT50.
    "Se for empresa 0100 variável XGRUPO senão variável  XREAL
    IF P_BUKRS = '0100'.
      WA_SAIDA-SALDOR     = TOTAL_GRUPO.
    ELSE.
      WA_SAIDA-SALDOR     = TOTAL_REAL.
    ENDIF.
    WA_SAIDA-SALDOU     = TOTAL_USD.
    WA_SAIDA-SALDOA     = TOTAL_REAL.
    IF TOTAL_USD NE 0.
      WA_SAIDA-MEDIA      = TOTAL_REAL / TOTAL_USD.
    ELSE.
      WA_SAIDA-MEDIA      = 0.
    ENDIF.
    APPEND WA_SAIDA TO IT_SAIDA.
  ENDLOOP.
ENDFORM.                    " F_SAIDA_DRE
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_SLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA_SLD .

  DATA: XREAL TYPE BSAS-DMBTR,
        XUSD  TYPE BSAS-DMBE2.

  SORT: IT_BSAS     BY HKONT,
        IT_BSIS     BY HKONT.

  LOOP AT IT_BSAS INTO WA_BSAS.
    WA_JOIN-CONTA   = WA_BSAS-HKONT.
    WA_JOIN-SHKZG   = WA_BSAS-SHKZG .
    WA_JOIN-SALDOR  = WA_BSAS-DMBTR.
    WA_JOIN-SALDOU  = WA_BSAS-DMBE2.
    APPEND WA_JOIN TO IT_JOIN.
  ENDLOOP.

  LOOP AT IT_BSIS INTO WA_BSIS.
    WA_JOIN-CONTA   = WA_BSIS-HKONT.
    WA_JOIN-SHKZG   = WA_BSIS-SHKZG .
    WA_JOIN-SALDOR  = WA_BSIS-DMBTR.
    WA_JOIN-SALDOU  = WA_BSIS-DMBE2.
    APPEND WA_JOIN TO IT_JOIN.
  ENDLOOP.

  IT_JOIN_AUX[] = IT_JOIN[].
  SORT: IT_JOIN     BY CONTA,
        IT_JOIN_AUX BY CONTA.


  DELETE ADJACENT DUPLICATES FROM IT_JOIN_AUX COMPARING CONTA.
  CLEAR WA_JOIN.
  LOOP AT IT_JOIN_AUX INTO WA_JOIN_AUX.
    XREAL = 0.
    XUSD = 0.
    IF WA_JOIN-CONTA = WA_JOIN_AUX-CONTA.
      CONTINUE.
    ENDIF.
    LOOP AT IT_JOIN INTO WA_JOIN WHERE CONTA = WA_JOIN_AUX-CONTA.
      IF WA_JOIN-SHKZG = 'H'.
        SUBTRACT WA_JOIN-SALDOR FROM XREAL.
        SUBTRACT WA_JOIN-SALDOU FROM XUSD.
      ELSE.
        ADD WA_JOIN-SALDOR TO XREAL.
        ADD WA_JOIN-SALDOU TO XUSD.
      ENDIF.
    ENDLOOP.
    WA_SAIDA-CONTA  = WA_JOIN-CONTA.
    WA_SAIDA-SALDOR = XREAL.
    WA_SAIDA-SALDOU = XUSD.
    APPEND WA_SAIDA TO IT_SAIDA.

  ENDLOOP.



ENDFORM.                    " F_SAIDA_SLD
