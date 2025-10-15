*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 03/05/2013                                              &*
*& Descrição: Agendamento Portos                                      &*
*& Transação: ZLES0072                                                &*
*& Criação..: DEVK929015                                              &*
*---------------------------------------------------------------------&*


REPORT  ZLESR0025.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: LIPS,VBPA.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*


TYPES:

   BEGIN OF TY_SAIDA,
      MARK(1),
      WERKS         TYPE LIPS-WERKS,
      BUKRS          TYPE TTDS-BUKRS  ,
      VGBEL         TYPE LIPS-VGBEL,
      KUNNR         TYPE VBPA-KUNNR,
      NAME1_K       TYPE KNA1-NAME1,
      MATNR         TYPE LIPS-MATNR,
      ERDAT         TYPE LIPS-ERDAT,
      NFENUM        TYPE J_1BNFDOC-NFENUM,
      SERIES        TYPE J_1BNFDOC-SERIES,
      BRGEW         TYPE J_1BNFDOC-BRGEW,
      NETWR         TYPE J_1BNFLIN-NETWR,
      VBELN         TYPE VBFA-VBELN,
      DACTE         TYPE J_1BNFDOC-NFENUM,
      TEXT1         TYPE VTTK-TEXT1,
      TEXT2         TYPE VTTK-TEXT2,
      TEXT3         TYPE VTTK-TEXT3,
      CD_RENAVAM1   TYPE ZLEST0002-CD_RENAVAM,
      CD_RENAVAM2   TYPE ZLEST0002-CD_RENAVAM,
      CD_RENAVAM3   TYPE ZLEST0002-CD_RENAVAM,
      NAME1_L       TYPE LFA1-NAME1,
      STCD4(35),
      TELF2         TYPE LFA1-TELF2,
      STCD2         TYPE LFA1-STCD2,
      TDLNR         TYPE VTTK-TDLNR,
      NAME1_A       TYPE LFA1-NAME1,
END OF TY_SAIDA,

BEGIN OF TY_LIPS,
   WERKS         TYPE LIPS-WERKS,
   VGBEL         TYPE LIPS-VGBEL,
   VBELN         TYPE LIPS-VBELN,
   MATNR         TYPE LIPS-MATNR,
   ERDAT         TYPE LIPS-ERDAT,
END OF TY_LIPS,

BEGIN OF TY_TTDS,
  TPLST          TYPE TTDS-TPLST,
  BUKRS          TYPE TTDS-BUKRS,
END OF TY_TTDS,

BEGIN OF TY_KNA1,
  KUNNR          TYPE KNA1-KUNNR,
  NAME1          TYPE KNA1-NAME1,
END OF TY_KNA1,

BEGIN OF TY_LFA1,
  LIFNR          TYPE LFA1-LIFNR,
  NAME1          TYPE LFA1-NAME1,
  STCD4          TYPE LFA1-STCD4,
  TELF2          TYPE LFA1-TELF2,
  STCD2          TYPE LFA1-STCD2,
END OF TY_LFA1,

BEGIN OF TY_VBFA,
   VBELV            TYPE VBFA-VBELV,
   VBTYP_N          TYPE VBFA-VBTYP_N,
   VBELN            TYPE J_1BNFLIN-REFKEY,
   VBELN_AUX        TYPE VBFA-VBELN,

END OF TY_VBFA,

BEGIN OF TY_J_1BNFLIN,
  REFKEY            TYPE J_1BNFLIN-REFKEY,
  DOCNUM            TYPE J_1BNFLIN-DOCNUM,
  NETWR             TYPE J_1BNFLIN-NETWR,
END OF TY_J_1BNFLIN,

BEGIN OF TY_J_1BNFDOC,
  DOCNUM            TYPE J_1BNFDOC-DOCNUM,
  NFENUM            TYPE J_1BNFDOC-NFENUM,
  SERIES            TYPE J_1BNFDOC-SERIES,
  BRGEW             TYPE J_1BNFDOC-BRGEW,
END OF TY_J_1BNFDOC,

BEGIN OF TY_VTTK,
 TKNUM              TYPE VTTK-TKNUM,
 TEXT1              TYPE VTTK-TEXT1,
 TEXT2              TYPE VTTK-TEXT2,
 TEXT3              TYPE VTTK-TEXT3,
 VSART              TYPE VTTK-VSART,
 TDLNR              TYPE VTTK-TDLNR,
 P1(7),
 P2(7),
 P3(7),
END OF TY_VTTK,

BEGIN OF TY_LFB1,
  LIFNR             TYPE LFB1-LIFNR,
  BUKRS             TYPE LFB1-BUKRS,
  ZSABE             TYPE LFB1-ZSABE,
END OF TY_LFB1,

BEGIN OF TY_VBAK,
  TKNUM             TYPE VBAK-TKNUM ,
  VBELN             TYPE VBAK-VBELN,
END OF TY_VBAK,

BEGIN OF TY_VBPA,
  VBELN   TYPE    VBPA-VBELN,
  KUNNR   TYPE    VBPA-KUNNR,
END OF TY_VBPA,

BEGIN OF TY_ZLEST0002,
  PC_VEICULO TYPE ZLEST0002-PC_VEICULO,
  CD_RENAVAM TYPE ZLEST0002-CD_RENAVAM,
END OF TY_ZLEST0002,


BEGIN OF TY_VTPA,
  VBELN   TYPE    VBPA-VBELN,
  LIFNR   TYPE    VTPA-LIFNR,
END OF TY_VTPA.


TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC     TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB TYPE TABLE OF BDCMSGCOLL,

      IT_LIPS             TYPE TABLE OF TY_LIPS,
      IT_TTDS             TYPE TABLE OF TY_TTDS,
      IT_KNA1             TYPE TABLE OF TY_KNA1,
      IT_LFA1             TYPE TABLE OF TY_LFA1,
      IT_LFA1_AUX         TYPE TABLE OF TY_LFA1,
      IT_VBFA_M           TYPE TABLE OF TY_VBFA,
      IT_VBFA_8           TYPE TABLE OF TY_VBFA,
      IT_VBFA_M8          TYPE TABLE OF TY_VBFA,
      IT_J_1BNFLIN        TYPE TABLE OF TY_J_1BNFLIN,
      IT_J_1BNFLIN_M8     TYPE TABLE OF TY_J_1BNFLIN,
      IT_J_1BNFDOC        TYPE TABLE OF TY_J_1BNFDOC,
      IT_J_1BNFDOC_M8     TYPE TABLE OF TY_J_1BNFDOC,
      IT_VTTK             TYPE TABLE OF TY_VTTK,
      IT_LFB1             TYPE TABLE OF TY_LFB1,
      IT_VBPA             TYPE TABLE OF TY_VBPA,
      IT_VTPA             TYPE TABLE OF TY_VTPA,
      IT_VBAK             TYPE TABLE OF TY_VBAK,
      IT_ZLEST0002        TYPE TABLE OF TY_ZLEST0002,
      IT_SAIDA            TYPE TABLE OF TY_SAIDA,

      IT_COLOR            TYPE TABLE OF LVC_S_SCOL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID,

      WA_LIPS             TYPE TY_LIPS,
      WA_TTDS             TYPE TY_TTDS,
      WA_KNA1             TYPE TY_KNA1,
      WA_LFA1             TYPE TY_LFA1,
      WA_VBFA             TYPE TY_VBFA,
      WA_J_1BNFLIN        TYPE TY_J_1BNFLIN,
      WA_J_1BNFDOC        TYPE TY_J_1BNFDOC,
      WA_VTTK             TYPE TY_VTTK,
      WA_LFB1             TYPE TY_LFB1,
      WA_VBPA             TYPE TY_VBPA,
      WA_VTPA             TYPE TY_VTPA,
      WA_VBAK             TYPE TY_VBAK,
      WA_ZLEST0002        TYPE TY_ZLEST0002,
      WA_SAIDA            TYPE TY_SAIDA,

      WA_COLOR            TYPE LVC_S_SCOL.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
     EDITCONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
     CL_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
     EDITOR           TYPE REF TO CL_GUI_TEXTEDIT,
     CL_CONTAINER_95  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
     CL_CONTAINER_05  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
     OBJ_DYNDOC_ID  TYPE REF TO CL_DD_DOCUMENT,
     CL_GRID          TYPE REF TO CL_GUI_ALV_GRID,
     WG_REPNAME LIKE SY-REPID,
     WG_SAVE(1) TYPE C,
     WA_AFIELD        TYPE LVC_S_FCAT,
     IT_FIELDCAT      TYPE LVC_T_FCAT,
     I_SORT           TYPE LVC_T_SORT,
     WA_LAYOUT        TYPE LVC_S_LAYO,
     IS_STABLE        TYPE LVC_S_STBL VALUE 'XX',
     WG_X_VARIANT LIKE DISVARIANT,
     WG_VARIANT LIKE DISVARIANT,
     VG_I       TYPE I,
     OK-CODE    TYPE   SY-UCOMM.

CONSTANTS:
         C_EXIT(4)         TYPE C VALUE 'EXIT',
         C_BACK(4)         TYPE C VALUE 'BACK',
         C_SAVE(4)         TYPE C VALUE 'SAVE'.


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

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_WERKS TYPE LIPS-WERKS OBLIGATORY.

SELECT-OPTIONS:  P_ERDAT FOR LIPS-ERDAT ,
                 P_MATNR FOR LIPS-MATNR ,
                 P_VGBEL FOR LIPS-VGBEL ,
                 P_KUNNR FOR VBPA-KUNNR .
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B4.


**************************************************************************
INITIALIZATION.
**************************************************************************

  WG_REPNAME = SY-REPID.
* Inicializa o layout do alv
  PERFORM ZF_INICIALIZA_VARIANTE.
*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
  DATA: VARIANTE        LIKE DISVARIANT,
        DEF_VARIANTE    LIKE DISVARIANT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.

  WG_SAVE = 'X'.
  WG_VARIANT-REPORT = WG_REPNAME.
  WG_X_VARIANT = WG_VARIANT.

  IF ( NOT P_VARI IS INITIAL ).
    WG_VARIANT-VARIANT = P_VARI.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = WG_VARIANT
      I_SAVE        = WG_SAVE
    IMPORTING
      ES_VARIANT    = WG_X_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( SY-SUBRC NE 0 ).
    MESSAGE S000(Z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE WG_X_VARIANT-VARIANT TO P_VARI.
  ENDIF.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

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
  SELECT  WERKS VGBEL VBELN MATNR  ERDAT
      FROM LIPS
      INTO TABLE IT_LIPS
      WHERE PSTYV	=	'ZRFL'
      AND WERKS	EQ P_WERKS
      AND ERDAT	IN P_ERDAT
      AND VGBEL IN P_VGBEL
      AND MATNR IN P_MATNR.

  CHECK IT_LIPS[] IS NOT INITIAL.

  SELECT TPLST BUKRS
    FROM  TTDS
    INTO TABLE IT_TTDS
    FOR ALL ENTRIES IN IT_LIPS
    WHERE TPLST EQ IT_LIPS-WERKS.

  SELECT VBELN KUNNR
   FROM VBPA
   INTO TABLE IT_VBPA
   FOR ALL ENTRIES IN IT_LIPS
   WHERE VBELN   EQ  IT_LIPS-VBELN
   AND PARVW  IN ('LR').


  IF IT_VBPA[] IS NOT  INITIAL.
    SELECT KUNNR NAME1
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_VBPA
      WHERE KUNNR EQ IT_VBPA-KUNNR.
  ENDIF.

  SELECT VBELV VBTYP_N VBELN VBELN
  FROM VBFA
  INTO TABLE IT_VBFA_M
  FOR ALL ENTRIES IN IT_LIPS
  WHERE VBELV   EQ IT_LIPS-VBELN
  AND   VBTYP_N	 IN ('M').


  IF IT_VBFA_M[] IS NOT INITIAL.

    SELECT REFKEY DOCNUM NETWR
      FROM J_1BNFLIN
      INTO TABLE IT_J_1BNFLIN
      FOR ALL ENTRIES IN IT_VBFA_M
      WHERE REFKEY 	EQ IT_VBFA_M-VBELN.

    SELECT DOCNUM NFENUM SERIES  BRGEW
      FROM J_1BNFDOC
      INTO TABLE IT_J_1BNFDOC
      FOR ALL ENTRIES IN IT_J_1BNFLIN
      WHERE DOCNUM 	EQ IT_J_1BNFLIN-DOCNUM .

  ENDIF.

  SELECT VBELV VBTYP_N VBELN VBELN
  FROM VBFA
  INTO TABLE IT_VBFA_8
  FOR ALL ENTRIES IN IT_LIPS
  WHERE VBELV   EQ IT_LIPS-VBELN
  AND   VBTYP_N	 IN ('8').

  IF IT_VBFA_8[] IS NOT INITIAL.
    SELECT  TKNUM TEXT1 TEXT2 TEXT3 VSART TDLNR
      FROM VTTK
      INTO TABLE IT_VTTK
      FOR ALL ENTRIES IN IT_VBFA_8
      WHERE TKNUM EQ IT_VBFA_8-VBELN_AUX.

    LOOP AT IT_VTTK INTO WA_VTTK    .
      WA_VTTK-P1 = WA_VTTK-TEXT1+0(7).
      WA_VTTK-P2 = WA_VTTK-TEXT2+0(7).
      WA_VTTK-P3 = WA_VTTK-TEXT3+0(7).
      MODIFY IT_VTTK FROM WA_VTTK INDEX SY-TABIX TRANSPORTING P1 P2 P3.
    ENDLOOP.

    IF IT_VTTK[] IS NOT INITIAL.
      SELECT PC_VEICULO CD_RENAVAM
        FROM ZLEST0002
        INTO TABLE IT_ZLEST0002
        FOR ALL ENTRIES IN IT_VTTK
        WHERE PC_VEICULO EQ IT_VTTK-P1
        OR    PC_VEICULO EQ IT_VTTK-P2
        OR    PC_VEICULO EQ IT_VTTK-P3.

      SELECT LIFNR NAME1 STCD4 TELF2 STCD2
       FROM LFA1
       INTO TABLE IT_LFA1_AUX
       FOR ALL ENTRIES IN IT_VTTK
       WHERE LIFNR EQ IT_VTTK-TDLNR.
    ENDIF.


    SELECT VBELN LIFNR
      FROM VTPA
      INTO TABLE IT_VTPA
      FOR ALL ENTRIES IN IT_VBFA_8
      WHERE VBELN   EQ  IT_VBFA_8-VBELN_AUX
      AND PARVW	EQ 'MT'.

* ---> S4 Migração - 19/06/2023 - FC - Inicio
      IF sy-subrc = 0.
        SORT IT_VTPA BY vbeln.
      ENDIF.
* <--- S4 Migração - 19/06/2023 - FC - Fim

    SELECT LIFNR NAME1 STCD4 TELF2 STCD2
      FROM LFA1
      INTO TABLE IT_LFA1
      FOR ALL ENTRIES IN IT_VTPA
      WHERE LIFNR EQ IT_VTPA-LIFNR.

    READ TABLE IT_TTDS INTO WA_TTDS INDEX 1.
    SELECT  LIFNR  BUKRS   ZSABE
     FROM LFB1
     INTO TABLE IT_LFB1
     FOR ALL ENTRIES IN IT_VTPA
     WHERE LIFNR EQ IT_VTPA-LIFNR
     AND   BUKRS EQ WA_TTDS-BUKRS.

    SELECT TKNUM VBELN
      FROM VBAK
      INTO TABLE IT_VBAK
      FOR ALL ENTRIES IN  IT_VBFA_8
      WHERE TKNUM = IT_VBFA_8-VBELN_AUX.

    IF IT_VBAK[] IS NOT INITIAL.
      SELECT VBELV VBTYP_N VBELN VBELN
       FROM VBFA
       INTO TABLE IT_VBFA_M8
       FOR ALL ENTRIES IN IT_VBAK
       WHERE VBELV   EQ IT_VBAK-VBELN
       AND   VBTYP_N   IN ('M').
      IF IT_VBFA_M8[] IS NOT INITIAL.
        SELECT REFKEY DOCNUM NETWR
          FROM J_1BNFLIN
          INTO TABLE IT_J_1BNFLIN_M8
          FOR ALL ENTRIES IN IT_VBFA_M8
          WHERE REFKEY 	EQ IT_VBFA_M8-VBELN.

        SELECT DOCNUM NFENUM SERIES  BRGEW
          FROM J_1BNFDOC
          INTO TABLE IT_J_1BNFDOC_M8
          FOR ALL ENTRIES IN IT_J_1BNFLIN_M8
          WHERE DOCNUM 	EQ IT_J_1BNFLIN_M8-DOCNUM .
      ENDIF.
    ENDIF.

  ENDIF.

  LOOP AT IT_LFA1_AUX INTO WA_LFA1.
    APPEND WA_LFA1 TO IT_LFA1.
  ENDLOOP.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  SORT: IT_VBPA       BY VBELN ,
        IT_KNA1       BY KUNNR,
        IT_LFA1       BY LIFNR,
        IT_LFB1       BY LIFNR,
        IT_VBFA_M     BY VBELV VBTYP_N,
        IT_VBFA_8     BY VBELV VBTYP_N,
        IT_VBFA_M8    BY VBELV VBTYP_N,
        IT_VTTK       BY TKNUM,
        IT_VBAK       BY TKNUM,
        IT_J_1BNFLIN  BY REFKEY,
        IT_J_1BNFLIN_M8  BY REFKEY,
        IT_J_1BNFDOC     BY DOCNUM,
        IT_J_1BNFDOC_M8  BY DOCNUM,
        IT_ZLEST0002     BY PC_VEICULO.

  LOOP AT IT_LIPS INTO WA_LIPS.
    WA_SAIDA-WERKS      = WA_LIPS-WERKS.
    WA_SAIDA-BUKRS      = WA_TTDS-BUKRS.
    WA_SAIDA-VGBEL      = WA_LIPS-VGBEL.

    READ TABLE IT_VBPA INTO WA_VBPA WITH KEY VBELN  =  WA_LIPS-VBELN      BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-KUNNR      = WA_VBPA-KUNNR.
      IF P_KUNNR IS NOT INITIAL AND  WA_VBPA-KUNNR NOT IN P_KUNNR.
        CONTINUE.
      ENDIF.
      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBPA-KUNNR BINARY SEARCH.
      WA_SAIDA-NAME1_K    = WA_KNA1-NAME1.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_LIPS-MATNR
      IMPORTING
        OUTPUT = WA_SAIDA-MATNR.

    WA_SAIDA-ERDAT = WA_LIPS-ERDAT.

    READ TABLE IT_VBFA_M INTO WA_VBFA WITH KEY VBELV    = WA_LIPS-VBELN
                                             VBTYP_N  = 'M' BINARY SEARCH.
    READ TABLE IT_J_1BNFLIN INTO WA_J_1BNFLIN WITH KEY REFKEY   = WA_VBFA-VBELN BINARY SEARCH.
    READ TABLE IT_J_1BNFDOC INTO WA_J_1BNFDOC WITH KEY DOCNUM   = WA_J_1BNFLIN-DOCNUM BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_SAIDA-NFENUM     = WA_J_1BNFDOC-NFENUM.
      WA_SAIDA-SERIES     = WA_J_1BNFDOC-SERIES.
      WA_SAIDA-BRGEW      = WA_J_1BNFDOC-BRGEW.
      WA_SAIDA-NETWR      = WA_J_1BNFLIN-NETWR.

      READ TABLE IT_VBFA_8 INTO WA_VBFA WITH KEY VBELV  = WA_LIPS-VBELN
                                             VBTYP_N  = '8' BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-VBELN      = WA_VBFA-VBELN.

        READ TABLE IT_VTTK INTO WA_VTTK WITH KEY TKNUM = 	WA_VBFA-VBELN BINARY SEARCH.
        IF SY-SUBRC = 0.
          IF WA_VTTK-VSART NE '01'.
            CONTINUE.
          ENDIF.
          READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_VTTK-TDLNR  BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-TDLNR = WA_LFA1-LIFNR.
            WA_SAIDA-NAME1_A = WA_LFA1-NAME1.
          ENDIF.

          WA_SAIDA-TEXT1      = WA_VTTK-TEXT1+0(7).
          WA_SAIDA-TEXT2      = WA_VTTK-TEXT2+0(7).
          WA_SAIDA-TEXT3      = WA_VTTK-TEXT3+0(7).
          READ TABLE IT_ZLEST0002 INTO WA_ZLEST0002 WITH KEY PC_VEICULO = WA_VTTK-P1 BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-CD_RENAVAM1 = WA_ZLEST0002-CD_RENAVAM.
          ELSE.
            CLEAR WA_SAIDA-CD_RENAVAM1.
          ENDIF.
          READ TABLE IT_ZLEST0002 INTO WA_ZLEST0002 WITH KEY PC_VEICULO = WA_VTTK-P2 BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-CD_RENAVAM2 = WA_ZLEST0002-CD_RENAVAM.
          ELSE.
            CLEAR WA_SAIDA-CD_RENAVAM2.
          ENDIF.
          READ TABLE IT_ZLEST0002 INTO WA_ZLEST0002 WITH KEY PC_VEICULO = WA_VTTK-P3 BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-CD_RENAVAM3 = WA_ZLEST0002-CD_RENAVAM.
          ELSE.
            CLEAR WA_SAIDA-CD_RENAVAM3.
          ENDIF.
        ELSE.
          CLEAR: WA_SAIDA-TEXT1,WA_SAIDA-TEXT2,WA_SAIDA-TEXT3,WA_SAIDA-CD_RENAVAM1,WA_SAIDA-CD_RENAVAM2,WA_SAIDA-CD_RENAVAM3.
        ENDIF.

        READ TABLE IT_VTPA INTO WA_VTPA WITH KEY VBELN  =  WA_VBFA-VBELN   BINARY SEARCH.
        READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_VTPA-LIFNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-NAME1_L    = WA_LFA1-NAME1.
          READ TABLE IT_LFB1 INTO WA_LFB1 WITH KEY LIFNR = WA_VTPA-LIFNR BINARY SEARCH..
          CONCATENATE WA_LFA1-STCD4 '-' WA_LFB1-ZSABE  INTO WA_SAIDA-STCD4 .
          WA_SAIDA-TELF2      = WA_LFA1-TELF2.
          WA_SAIDA-STCD2      = WA_LFA1-STCD2.
        ELSE.
          CLEAR: WA_SAIDA-NAME1_L,WA_SAIDA-STCD4 ,WA_SAIDA-TELF2, WA_SAIDA-STCD2 .
        ENDIF.
        READ TABLE IT_VBAK INTO WA_VBAK WITH KEY TKNUM = WA_VBFA-VBELN BINARY SEARCH.
        IF SY-SUBRC = 0.
          READ TABLE IT_VBFA_M8 INTO WA_VBFA WITH KEY VBELV    = WA_VBAK-VBELN
                                                   VBTYP_N  = 'M' BINARY SEARCH.
          IF SY-SUBRC = 0.
            READ TABLE IT_J_1BNFLIN_M8 INTO WA_J_1BNFLIN WITH KEY REFKEY   = WA_VBFA-VBELN BINARY SEARCH.
            IF SY-SUBRC = 0.
              READ TABLE IT_J_1BNFDOC_M8 INTO WA_J_1BNFDOC WITH KEY DOCNUM   = WA_J_1BNFLIN-DOCNUM BINARY SEARCH.
              IF SY-SUBRC = 0.
                WA_SAIDA-DACTE      = WA_J_1BNFDOC-NFENUM.
              ELSE.
                CLEAR WA_SAIDA-DACTE.
              ENDIF.
            ELSE.
              CLEAR WA_SAIDA-DACTE.
            ENDIF.
          ELSE.
            CLEAR WA_SAIDA-DACTE.
          ENDIF.
        ELSE.
          CLEAR WA_SAIDA-DACTE.
        ENDIF.
      ELSE.
        CLEAR: WA_SAIDA-VBELN    ,
               WA_SAIDA-TEXT1    ,
               WA_SAIDA-TEXT2    ,
               WA_SAIDA-TEXT3    ,
               WA_SAIDA-NAME1_L  ,
               WA_SAIDA-TELF2    ,
               WA_SAIDA-DACTE    .


      ENDIF.
    ELSE.
      CLEAR:  WA_SAIDA-NFENUM  ,
              WA_SAIDA-SERIES  ,
              WA_SAIDA-BRGEW   ,
              WA_SAIDA-NETWR   ,
              WA_SAIDA-VBELN   ,
              WA_SAIDA-TEXT1   ,
              WA_SAIDA-TEXT2   ,
              WA_SAIDA-TEXT3   ,
              WA_SAIDA-NAME1_L ,
              WA_SAIDA-TELF2   ,
              WA_SAIDA-DACTE   .
    ENDIF.
    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR WA_SAIDA.

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
  PERFORM:
           F_ALV_FIELDCAT,
           F_ALV_IMPRIME.
ENDFORM.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  ZF_INICIALIZA_VARIANTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_INICIALIZA_VARIANTE.
  CLEAR WG_VARIANT.
  WG_SAVE = 'X'.
  WG_VARIANT-REPORT = WG_REPNAME.
  WG_X_VARIANT = WG_VARIANT.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = WG_SAVE
    CHANGING
      CS_VARIANT = WG_X_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
    P_VARI = WG_X_VARIANT-VARIANT.
  ENDIF.
ENDFORM. " ZF_inicializa_variante
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_FIELDCAT .
  DATA I TYPE I.
  WA_AFIELD-TABNAME     = 'O_ALV'.
  WA_AFIELD-COLDDICTXT = 'M'.
  WA_AFIELD-SELDDICTXT = 'M'.
  WA_AFIELD-TIPDDICTXT = 'M'.
  WA_AFIELD-COL_OPT = 'X'.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'WERKS'.

  WA_AFIELD-SCRTEXT_S = 'Filial'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BUKRS'.
  WA_AFIELD-SCRTEXT_S = 'Empresa'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-EDIT          = ''.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.
  CLEAR WA_AFIELD-ICON.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VGBEL'.
  WA_AFIELD-SCRTEXT_S = 'Ordem de Venda'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'KUNNR'.
  WA_AFIELD-SCRTEXT_S = 'Cód Loc Entrega'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1_K'.
  WA_AFIELD-SCRTEXT_S = 'Descrição Local Entrega'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'MATNR'.
  WA_AFIELD-SCRTEXT_S = 'Material'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'ERDAT'.
  WA_AFIELD-SCRTEXT_S = 'Data Emissão'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NFENUM'.
  WA_AFIELD-SCRTEXT_S = 'NF'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'SERIES'.
  WA_AFIELD-SCRTEXT_S = 'Série'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'BRGEW'.
  WA_AFIELD-SCRTEXT_S = 'Qtde'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NETWR'.
  WA_AFIELD-SCRTEXT_S = 'Valor NF'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'VBELN'.
  WA_AFIELD-SCRTEXT_S = 'Doc Transp'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TDLNR'.
  WA_AFIELD-SCRTEXT_S = 'Cód. Agente Frete'.
  WA_AFIELD-SCRTEXT_L = 'Cód. Agente Frete'.
  WA_AFIELD-SCRTEXT_M = 'Cód. Agente Frete'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1_A'.
  WA_AFIELD-SCRTEXT_S = 'Nome Agente Frete'.
  WA_AFIELD-SCRTEXT_L = 'Nome Agente Frete'.
  WA_AFIELD-SCRTEXT_M = 'Nome Agente Frete'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'DACTE'.
  WA_AFIELD-SCRTEXT_S = 'Dacte'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TEXT1'.
  WA_AFIELD-SCRTEXT_S = 'Placa Cavalo'.
  WA_AFIELD-SCRTEXT_L = 'Placa Cavalo'.
  WA_AFIELD-SCRTEXT_M = 'Placa Cavalo'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CD_RENAVAM1'.
  WA_AFIELD-SCRTEXT_S = 'Renavan Cavalo'.
  WA_AFIELD-SCRTEXT_L = 'Renavan Cavalo'.
  WA_AFIELD-SCRTEXT_M = 'Renavan Cavalo'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.


  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TEXT2'.
  WA_AFIELD-SCRTEXT_S = 'Placa Carreta 1'.
  WA_AFIELD-SCRTEXT_L = 'Placa Carreta 1'.
  WA_AFIELD-SCRTEXT_M = 'Placa Carreta 1'.
  WA_AFIELD-OUTPUTLEN = 15.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CD_RENAVAM2'.
  WA_AFIELD-SCRTEXT_S = 'Renavan carreta 1'.
  WA_AFIELD-SCRTEXT_L = 'Renavan carreta 1'.
  WA_AFIELD-SCRTEXT_M = 'Renavan carreta 1'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TEXT3'.
  WA_AFIELD-SCRTEXT_S = 'Placa Carreta 2'.
  WA_AFIELD-SCRTEXT_L = 'Placa Carreta 2'.
  WA_AFIELD-SCRTEXT_M = 'Placa Carreta 2'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'CD_RENAVAM3'.
  WA_AFIELD-SCRTEXT_S = 'Renavan carreta 2'.
  WA_AFIELD-SCRTEXT_L = 'Renavan carreta 2'.
  WA_AFIELD-SCRTEXT_M = 'Renavan carreta 2'.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'NAME1_L'.
  WA_AFIELD-SCRTEXT_S = 'Motorista'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'STCD4'.
  WA_AFIELD-SCRTEXT_S = 'CNH'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'STCD2'.
  WA_AFIELD-SCRTEXT_S = 'CPF'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

  I = I + 1.
  CLEAR WA_AFIELD.
  WA_AFIELD-COL_POS       = I.
  WA_AFIELD-FIELDNAME     = 'TELF2'.
  WA_AFIELD-SCRTEXT_S = 'Nr. Celular'.
  WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
  WA_AFIELD-KEY           = ''.
  APPEND WA_AFIELD TO IT_FIELDCAT.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_IMPRIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV_IMPRIME .
  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  WA_LAYOUT-GRID_TITLE = 'Agendamento de Portos'.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT   = 'X'.
  WA_LAYOUT-BOX_FNAME       = 'MARK'.

  CALL SCREEN 9001.

ENDFORM.                    " F_ALV_IMPRIME
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9001 OUTPUT.
  DATA: W_DT TYPE SY-DATUM,
         TABIX TYPE SY-TABIX.

  PERFORM F_PF_STATUS_NOVO .

  IF CL_CONTAINER_95 IS INITIAL.

***    CREATE OBJECT CL_CONTAINER
***      EXPORTING
***        CONTAINER_NAME = 'CONTAINER'.
    CREATE OBJECT CL_CONTAINER_95
      EXPORTING
        SIDE  = '4'
        RATIO = '80'.

  ENDIF.

  IF NOT CL_GRID IS INITIAL.
    PERFORM ZF_ALV_HEADER USING '2'.
*
    CALL METHOD CL_GRID->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = IT_FIELDCAT[].
    LOOP AT IT_FIELDCAT INTO WA_AFIELD
      WHERE FIELDNAME EQ 'TEXT1'
         OR FIELDNAME EQ 'TEXT2'
         OR FIELDNAME EQ 'TEXT3'.

      WA_AFIELD-OUTPUTLEN = '15'.
      MODIFY IT_FIELDCAT FROM WA_AFIELD.
    ENDLOOP.
    CALL METHOD CL_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = IT_FIELDCAT[].

    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE      =
*        I_SOFT_REFRESH =
*      EXCEPTIONS
*        FINISHED       = 1
*        OTHERS         = 2
            .
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT OBJ_DYNDOC_ID
      EXPORTING
*    STYLE  =
*    BACKGROUND_COLOR =
*    BDS_STYLESHEET =
        NO_MARGINS = 'X'.

    PERFORM ZF_ALV_HEADER USING '1'.


    IF EDITCONTAINER IS INITIAL .
      CREATE OBJECT EDITCONTAINER
        EXPORTING
          CONTAINER_NAME = 'HEADER'.
    ENDIF .

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.



    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT      = CL_CONTAINER_95
        I_APPL_EVENTS = 'X'.


    CALL METHOD CL_GRID->REGISTER_EDIT_EVENT
      EXPORTING
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_VARIANT      = WG_X_VARIANT
        I_SAVE          = WG_SAVE
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCAT[]
        IT_SORT         = I_SORT[]
        IT_OUTTAB       = IT_SAIDA[].

*
*    CREATE OBJECT EVENT_RECEIVER.
*    SET HANDLER EVENT_RECEIVER->CATCH_HOTSPOT      FOR CL_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_PAGE FOR CL_GRID.



  ENDIF.


ENDMODULE.                 " STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_PF_STATUS_NOVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_PF_STATUS_NOVO ."USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'F_SET_PF'.
  SET TITLEBAR  'ZFTITLE'.
ENDFORM.                    " F_PF_STATUS_NOVO
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1950   text
*----------------------------------------------------------------------*
FORM ZF_ALV_HEADER  USING P_TIPO.
  DATA: WL_DATA(10),
        WL_HORA(8),
* ---> S4 Migração - 19/06/2023 - FC - Inicio
        "WL_LINHA(60),
        WL_LINHA(64),
* <--- S4 Migração - 19/06/2023 - FC - Fim
        WL_TEXT TYPE SDYDO_TEXT_ELEMENT,
        WL_DOLAR(15),
        WL_EURO(15),
        WL_BUTXT TYPE BUTXT.
  IF P_TIPO = '1'.
    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = 'Agendamento de Portos'
        SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

    SELECT SINGLE BUTXT FROM T001
      INTO WL_BUTXT
      WHERE BUKRS = WA_TTDS-BUKRS.
    IF SY-SUBRC = 0.
      MOVE WL_BUTXT TO WL_LINHA.
    ELSE.
      MOVE 'N/a' TO WL_LINHA.
    ENDIF.
    CONCATENATE  'Empresa.............:' WA_TTDS-BUKRS
                  '- ' WL_LINHA
            INTO WL_LINHA SEPARATED BY SPACE.
    WL_TEXT = WL_LINHA.
    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
      EXPORTING
        TEXT         = WL_TEXT "WL_LINHA
*      SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

    IF P_WERKS IS NOT INITIAL.
      CONCATENATE  'Filial...................:' P_WERKS
               INTO WL_LINHA SEPARATED BY SPACE.
      WL_TEXT = WL_LINHA.
      CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

      CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
        EXPORTING
          TEXT         = WL_TEXT "WL_LINHA
          SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
    ENDIF.


*    WRITE SY-UZEIT TO WL_HORA.
*    WRITE SY-DATUM TO WL_DATA.
*    CONCATENATE  'Data:' WL_DATA
*                  '- Horário:' WL_HORA
*
*            INTO WL_LINHA SEPARATED BY SPACE.
*    WL_TEXT = WL_LINHA.
*    CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.
*
*    CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
*      EXPORTING
*        TEXT         = WL_TEXT "WL_LINHA
*        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.

    IF P_ERDAT IS NOT INITIAL.
      WRITE P_ERDAT-LOW TO WL_DATA.
      CONCATENATE  'Data Emissão.......:' WL_DATA
               INTO WL_LINHA SEPARATED BY SPACE.
      IF P_ERDAT-HIGH IS NOT INITIAL.
        WRITE P_ERDAT-HIGH  TO WL_DATA.
        CONCATENATE  WL_LINHA 'a' WL_DATA
                 INTO WL_LINHA SEPARATED BY SPACE.
      ENDIF.

      WL_TEXT = WL_LINHA.
      CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

      CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
        EXPORTING
          TEXT         = WL_TEXT "WL_LINHA
          SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
    ENDIF.

    IF P_KUNNR IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = P_KUNNR-LOW
        IMPORTING
          OUTPUT = P_KUNNR-LOW.
      CONCATENATE  'Local de Entrega..:' P_KUNNR-LOW
               INTO WL_LINHA SEPARATED BY SPACE.
      IF P_KUNNR-HIGH IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = P_KUNNR-HIGH
          IMPORTING
            OUTPUT = P_KUNNR-HIGH.
        CONCATENATE  WL_LINHA 'a' P_KUNNR-HIGH
                 INTO WL_LINHA SEPARATED BY SPACE.
      ENDIF.

      WL_TEXT = WL_LINHA.
      CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

      CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
        EXPORTING
          TEXT         = WL_TEXT "WL_LINHA
          SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
    ENDIF.

    IF P_MATNR IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = P_MATNR-LOW
        IMPORTING
          OUTPUT = P_MATNR-LOW.
      CONCATENATE  'Material..............:' P_MATNR-LOW
               INTO WL_LINHA SEPARATED BY SPACE.
      IF P_MATNR-HIGH IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = P_MATNR-HIGH
          IMPORTING
            OUTPUT = P_MATNR-HIGH.
        CONCATENATE  WL_LINHA 'a' P_MATNR-HIGH
                 INTO WL_LINHA SEPARATED BY SPACE.
      ENDIF.

      WL_TEXT = WL_LINHA.
      CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

      CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
        EXPORTING
          TEXT         = WL_TEXT "WL_LINHA
          SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
    ENDIF.

    IF P_VGBEL IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = P_VGBEL-LOW
        IMPORTING
          OUTPUT = P_VGBEL-LOW.
      CONCATENATE  'Ordem de Venda..:' P_VGBEL-LOW
               INTO WL_LINHA SEPARATED BY SPACE.
      IF P_VGBEL-HIGH IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = P_VGBEL-HIGH
          IMPORTING
            OUTPUT = P_VGBEL-HIGH.
        CONCATENATE  WL_LINHA 'a' P_VGBEL-HIGH
                 INTO WL_LINHA SEPARATED BY SPACE.
      ENDIF.

      WL_TEXT = WL_LINHA.
      CALL METHOD OBJ_DYNDOC_ID->NEW_LINE.

      CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
        EXPORTING
          TEXT         = WL_TEXT "WL_LINHA
          SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
    ENDIF.

  ELSE.

    CALL METHOD OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    CALL METHOD OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      EXPORTING
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      EXCEPTIONS
        HTML_DISPLAY_ERROR = 1.


  ENDIF.

ENDFORM.                    " F_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9001 INPUT.
  DATA: V_VALID(1) TYPE C,
          INDROW TYPE LVC_T_ROW,
          W_IND TYPE LVC_T_ROW WITH HEADER LINE,
          V_MOD(1) TYPE C,
          WL_TABIX TYPE SY-TABIX,
          WL_ERRO(1).


  IF NOT CL_GRID IS INITIAL.
    CALL METHOD CL_GRID->DISPATCH
      EXPORTING
        CARGO         = SY-UCOMM
        EVENTID       = 19
        IS_SHELLEVENT = ' '.

    IF SY-UCOMM IS INITIAL.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = IS_STABLE.
    ENDIF.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'UP'.
      REFRESH IT_SAIDA.
      CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE OK-CODE.
    WHEN C_BACK.
      SET SCREEN 0.

    WHEN C_EXIT.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
