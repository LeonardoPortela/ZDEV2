*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDR0013                                                *
* Descrição  : Relatorio Consulta Saldo de Ordem de Venda              *
* Módulo     : FI                                Transação: ZSDT0039   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Camila Brand                         Data: 12/08/2011   *
* Observações: Desenvolvimento inicial do Programa (Copia ZFIS19)      *
*----------------------------------------------------------------------*

REPORT  ZSDR0013.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON, SLIS.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: VBAK ,
        VBAP ,
        TCURR,
        VBFA ,
        VBKD .

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  "     Dados Ordem de Venda (Cabeçalho)
  BEGIN OF TY_VBAK             ,
    VBELN TYPE VBAK-VBELN,
    ERDAT TYPE VBAK-ERDAT,
    AUART TYPE VBAK-AUART,
    AUDAT TYPE VBAK-AUDAT,
    VKORG TYPE VBAK-VKORG,
    VTWEG TYPE VBAK-VTWEG,
    SPART TYPE VBAK-SPART,
    KUNNR TYPE VBAK-KUNNR,
    WAERK TYPE VBAK-WAERK,
    KNUMV TYPE VBAK-KNUMV,
    VGBEL TYPE VBAK-VGBEL,
    KVGR2 TYPE VBAK-KVGR2,
  END   OF TY_VBAK             ,

  " Dados Contratos
  BEGIN OF TY_VBFA_CONT         ,
    VBELN TYPE VBFA-VBELN,
    VBELV TYPE VBFA-VBELN,
  END   OF TY_VBFA_CONT         ,

  "     Dados Ordem de Venda (Item)
  BEGIN OF TY_VBAP             ,
    VBELN TYPE VBAP-VBELN,
    POSNR TYPE VBAP-POSNR,
    MATNR TYPE VBAP-MATNR,
    WERKS TYPE VBAP-WERKS,
    CHARG TYPE VBAP-CHARG,
  END   OF TY_VBAP             ,

  BEGIN OF TY_KONV             ,
    KNUMV TYPE KONV-KNUMV,
    KSCHL TYPE KONV-KSCHL,
    KWERT TYPE KONV-KWERT,
    KBETR TYPE KONV-KBETR,
  END   OF TY_KONV             ,

  BEGIN OF TY_VBEP             ,
    VBELN TYPE VBEP-VBELN,
    POSNR TYPE VBEP-POSNR,
    ETENR TYPE VBEP-ETENR,
    WMENG TYPE VBEP-WMENG,
  END   OF TY_VBEP             ,

  BEGIN OF TY_KNA1             ,
    KUNNR TYPE KNA1-KUNNR,
    NAME1 TYPE KNA1-NAME1,
  END   OF TY_KNA1             ,

  BEGIN OF TY_MAKT             ,
    MATNR TYPE MAKT-MATNR,
    MAKTX TYPE MAKT-MAKTX,
  END   OF TY_MAKT             ,

  BEGIN OF TY_VBFA             ,
    VBELV TYPE VBFA-VBELV,
    RFMNG TYPE VBFA-RFMNG,
  END   OF TY_VBFA             ,

  BEGIN OF TY_TCURR            ,
    GDATU TYPE TCURR-GDATU,
    UKURS TYPE TCURR-UKURS,
  END   OF TY_TCURR            ,

  BEGIN OF TY_VBFA_INI         ,
    VBELV   TYPE VBFA-VBELV,
    VBTYP_N TYPE VBFA-VBTYP_N,
  END   OF TY_VBFA_INI         ,

  BEGIN OF TY_VBKD             ,
    VBELN TYPE VBKD-VBELN,
    POSNR TYPE VBKD-POSNR,
    INCO1 TYPE VBKD-INCO1,
    BSTKD TYPE VBKD-BSTKD,
  END OF TY_VBKD               ,

  BEGIN OF TY_VBFA_OV          ,
    VBELN TYPE VBFA-VBELN,
    VBELV TYPE VBFA-VBELV,
  END   OF TY_VBFA_OV          ,

  BEGIN OF TY_SAIDA            ,
    VBELN      TYPE VBAK-VBELN,
    VBELV      TYPE VBFA-VBELV,
    ERDAT      TYPE VBAK-ERDAT,
    AUART      TYPE VBAK-AUART,
    NAME1      TYPE KNA1-NAME1,
    MAKTX      TYPE MAKT-MAKTX,
    WERKS      TYPE VBAP-WERKS,
    WMENG      TYPE VBEP-WMENG,
    SD_ORDEM   TYPE VBFA-RFMNG,
    QT_DEV     TYPE VBFA-RFMNG,
    QT_COMPL   TYPE VBFA-RFMNG,
    XTRANT_ATU TYPE VBFA-RFMNG,
    XTRATU     TYPE VBFA-RFMNG,
    XTRANT_POS TYPE VBFA-RFMNG,
    XTRANT     TYPE VBFA-RFMNG,
    KUNNR      TYPE VBAK-KUNNR,
    MATNR      TYPE VBAP-MATNR,
    TOT_RS     TYPE KONV-KWERT,
    TOT_US     TYPE KONV-KWERT,
    SLD_OV     TYPE VBEP-WMENG,
    KBETR      TYPE KONV-KBETR,
    BSTKD      TYPE VBKD-BSTKD,
    VGBEL      TYPE VBAK-VGBEL,
  END OF TY_SAIDA                  .

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: IT_VBAK         TYPE TABLE OF TY_VBAK,
      IT_VBFA_CONT    TYPE TABLE OF TY_VBFA_CONT,
      IT_VBAP         TYPE TABLE OF TY_VBAP,
      IT_KONV         TYPE TABLE OF TY_KONV,
      IT_VBEP         TYPE TABLE OF TY_VBEP,
      IT_KNA1         TYPE TABLE OF TY_KNA1,
      IT_MAKT         TYPE TABLE OF TY_MAKT,
      IT_VBFA_OV      TYPE TABLE OF TY_VBFA_OV,
      IT_VBFA_DEV     TYPE TABLE OF TY_VBFA_OV,
      IT_VBFA_ANT     TYPE TABLE OF VBFA,
      IT_VBFA_ATU     TYPE TABLE OF VBFA,
      IT_VBFA_ANT_ATU TYPE TABLE OF VBFA,
* Wsb Complemento e Devolução INICIO
      IT_VBFA         TYPE TABLE OF VBFA,
      IT_VBFA_C       TYPE TABLE OF VBFA,
      IT_VBFA_C_E     TYPE TABLE OF VBFA,
      IT_VBFA_L       TYPE TABLE OF VBFA,
      IT_VBFA_L_E     TYPE TABLE OF VBFA,
      IT_VBFA_H       TYPE TABLE OF VBFA,
      IT_VBFA_H_E     TYPE TABLE OF VBFA,
* Wsb Complemento e Devolução FIM
      IT_TCURR        TYPE TABLE OF TY_TCURR,
      IT_VBFA_INI     TYPE TABLE OF TY_VBFA_INI,
      IT_VBKD         TYPE TABLE OF TY_VBKD,
      IT_SAIDA        TYPE TABLE OF TY_SAIDA,
      T_BDC           TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      IT_SETLEAF      LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
      T_MESSTAB       TYPE TABLE OF BDCMSGCOLL.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA:WA_VBAK         TYPE TY_VBAK,
     WA_VBFA_CONT    TYPE TY_VBFA_CONT,
     WA_VBAP         TYPE TY_VBAP,
     WA_KONV         TYPE TY_KONV,
     WA_VBEP         TYPE TY_VBEP,
     WA_KNA1         TYPE TY_KNA1,
     WA_MAKT         TYPE TY_MAKT,
     WA_VBFA_ANT     TYPE VBFA,
     WA_VBFA_ATU     TYPE VBFA,
     WA_VBFA_ANT_ATU TYPE VBFA,
* Wsb Complemento e Devolução INICIO
     WA_VBFA         TYPE VBFA,
     WA_VBFA_C       TYPE VBFA,
     WA_VBFA_C_E     TYPE VBFA,
     WA_VBFA_L       TYPE VBFA,
     WA_VBFA_L_E     TYPE VBFA,
     WA_VBFA_H       TYPE VBFA,
     WA_VBFA_H_E     TYPE VBFA,
* Wsb Complemento e Devolução FIM
     WA_VBFA_OV      TYPE TY_VBFA_OV,
     WA_TCURR        TYPE TY_TCURR,
     WA_VBFA_INI     TYPE TY_VBFA_INI,
     WA_VBKD         TYPE TY_VBKD,
     WA_SAIDA        TYPE TY_SAIDA,
     WA_SETLEAF      TYPE SETLEAF,
     WA_CONT         TYPE REF TO CL_GUI_CUSTOM_CONTAINER , " Objeto Container
     WA_ALV          TYPE REF TO CL_GUI_ALV_GRID         , " Objeto ALV
     WA_LAYOUT       TYPE LVC_S_LAYO                     . " Layout da Lista / Fim do DATA

*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA: IT_FCAT   TYPE TABLE OF TY_ESTRUTURA, " lvc_s_fcat,
      S_VARIANT TYPE DISVARIANT           , " Tabela Estrutura colunas relatorio
      T_TOP     TYPE SLIS_T_LISTHEADER,
      XS_EVENTS TYPE SLIS_ALV_EVENT,
      EVENTS    TYPE SLIS_T_EVENT,
      T_PRINT   TYPE SLIS_PRINT_ALV,
      V_REPORT  LIKE SY-REPID,
      T_SORT    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: P_VBELN  FOR VBAK-VBELN             ,
                P_VKORG  FOR VBAK-VKORG OBLIGATORY  ,           "Organização de Vendas
                P_VTWEG  FOR VBAK-VTWEG             ,           "Canal de distribuição
                P_SPART  FOR VBAK-SPART             ,           "Setor de Atividade
                P_CHARG  FOR VBAP-CHARG             ,           "Safra
                P_WERKS  FOR VBAP-WERKS             ,           "Centro
                P_KUNNR  FOR VBAK-KUNNR             ,           "Cliente
                P_MATNR  FOR VBAP-MATNR             ,           "Material
                P_WAERK  FOR VBAK-WAERK             ,           "Moeda
                P_AUART  FOR VBAK-AUART             ,           "Tipo de Ordem de Venda
                P_BSTKD  FOR VBKD-BSTKD             ,           "Numero do Pedido
                P_KURST  FOR TCURR-KURST DEFAULT 'B' OBLIGATORY,"Categoria Moeda
                P_ERDAT  FOR VBFA-ERDAT OBLIGATORY  ,           "Período
                P_PERIOD FOR VBAK-ERDAT OBLIGATORY  .
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_VARIA LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN: END OF BLOCK B2.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
DATA: VG_REPID   LIKE SY-REPID,
      VG_VARIANT TYPE DISVARIANT.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARIA.

  VG_REPID          = SY-REPID.
  S_VARIANT-REPORT = VG_REPID.

  IF ( NOT P_VARIA IS INITIAL ).
    VG_VARIANT-VARIANT = P_VARIA.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = S_VARIANT
      I_SAVE        = 'A'
    IMPORTING
      ES_VARIANT    = S_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( SY-SUBRC NE 0 ).
    MESSAGE S000(Z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE S_VARIANT-VARIANT TO P_VARIA.
  ENDIF.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA : MES_INICIAL TYPE C LENGTH 2,
         MES_FINAL   TYPE C LENGTH 2,
         ANO_INICIAL TYPE C LENGTH 4,
         ANO_FINAL   TYPE C LENGTH 4.

  IF P_ERDAT-LOW < P_PERIOD-LOW .
    MESSAGE I000(Z01) WITH 'Periodo Remessa: A Data inicial informada não pode '
                           'ser menor que a data inicial informada no Campo '
                           'Periodo da Ordem de Venda !'        .
    STOP.
  ENDIF.

  IF P_PERIOD-LOW > P_ERDAT-LOW .
    MESSAGE I000(Z01) WITH 'Periodo Ordem de Venda : A Data inicial informada não pode '
                           'ser maior que a data inicial informada no Campo '
                           'Periodo da Remessa !'        .
    STOP.
  ENDIF.

  PERFORM:  F_INICIAR_VARIAVES                      ,
            F_SELECIONA_DADOS                       , " Form seleciona dados
            F_SAIDA                                 , " Form de saida
            F_IMPRIME_DADOS                         . " Form ALV

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Form para selecionar os dados e relacionar as tabelas.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.


  DATA: IT_VBAK_AUX TYPE TABLE OF TY_VBAK,
        IT_VBAP_AUX TYPE TABLE OF TY_VBAP,
        IT_TVAK     TYPE TABLE OF TVAK WITH HEADER LINE,
        WA_TVAK     TYPE TVAK.



  REFRESH: IT_VBAK                    ,
           IT_VBAP                    .

  SELECT * INTO TABLE IT_TVAK FROM TVAK.

  IF P_AUART[] IS INITIAL.

    SELECT *
    FROM SETLEAF
    INTO TABLE IT_SETLEAF
     WHERE SETNAME EQ 'ZFIS19'.

    LOOP AT IT_SETLEAF INTO WA_SETLEAF.
      WA_TVAK-AUART = WA_SETLEAF-VALFROM(4).
      DELETE IT_TVAK WHERE AUART EQ WA_TVAK-AUART.
    ENDLOOP.


  ENDIF.


  SELECT VBELV
         VBTYP_N
    FROM VBFA
    INTO TABLE IT_VBFA_INI
    WHERE ERDAT IN P_PERIOD
      AND VBTYP_N IN ('J' , 'T').

  SELECT VBELN
         ERDAT
         AUART
         AUDAT
         VKORG
         VTWEG
         SPART
         KUNNR
         WAERK
         KNUMV
         VGBEL
         KVGR2
    FROM VBAK
    INTO TABLE IT_VBAK
    FOR ALL ENTRIES IN IT_TVAK
   WHERE VKORG IN P_VKORG
     AND VTWEG IN P_VTWEG
     AND SPART IN P_SPART
     AND KUNNR IN P_KUNNR
     AND WAERK IN P_WAERK
     AND ERDAT IN P_PERIOD
     AND AUART IN P_AUART
     AND VBELN IN P_VBELN
     AND AUART EQ IT_TVAK-AUART..

  CHECK SY-SUBRC IS INITIAL.

  SELECT VBELN
        VBELV
   FROM VBFA
   INTO TABLE IT_VBFA_OV
    FOR ALL ENTRIES IN IT_VBAK
  WHERE VBTYP_V EQ 'C'
    AND VBELN   EQ IT_VBAK-VGBEL.

  SELECT VBELN
         VBELV
    FROM VBFA
    INTO TABLE IT_VBFA_DEV
     FOR ALL ENTRIES IN IT_VBAK
   WHERE VBTYP_V EQ 'C'
     AND VBTYP_N EQ 'C'
     AND VBELV   EQ IT_VBAK-VGBEL.


  SELECT VBELN
         VBELV
    FROM VBFA
    INTO TABLE IT_VBFA_CONT
     FOR ALL ENTRIES IN IT_VBAK
   WHERE VBTYP_N EQ 'C'
     AND VBTYP_V EQ 'G'
     AND POSNV   NE ''
     AND VBELN   EQ IT_VBAK-VBELN.

  " Capturar o Tipo de Frete
  SELECT VK~VBELN
         VK~POSNR
         VK~INCO1
         VK~BSTKD
    FROM VBKD AS VK
   INNER JOIN VBAK AS VB ON VB~VBELN EQ VK~VBELN
    INTO TABLE IT_VBKD
     FOR ALL ENTRIES IN IT_VBAK
   WHERE VK~VBELN EQ IT_VBAK-VBELN.

  SELECT VBELN
         POSNR
         MATNR
         WERKS
         CHARG
    FROM VBAP
    INTO TABLE IT_VBAP
    FOR ALL ENTRIES IN IT_VBAK
    WHERE VBELN EQ IT_VBAK-VBELN
      AND CHARG IN P_CHARG
      AND WERKS IN P_WERKS
      AND MATNR IN P_MATNR.

  IT_VBAK_AUX[] = IT_VBAK[].
  SORT IT_VBAK_AUX BY KNUMV.
  DELETE ADJACENT DUPLICATES FROM IT_VBAK_AUX COMPARING KNUMV.

  IF IT_VBAK_AUX IS NOT INITIAL.

    SELECT FROM V_KONV FIELDS KNUMV , KSCHL , KWERT , KBETR FOR ALL ENTRIES IN @IT_VBAK_AUX WHERE KNUMV EQ @IT_VBAK_AUX-KNUMV AND KSCHL IN ( 'IBRX' , 'PR00' , 'ZCIF' ) INTO TABLE @IT_KONV .
  ENDIF.

  IF IT_VBAP IS NOT INITIAL.
    SELECT VBELN
           POSNR
           ETENR
           WMENG
      FROM VBEP
      INTO TABLE IT_VBEP
      FOR ALL ENTRIES IN IT_VBAP
      WHERE VBELN EQ IT_VBAP-VBELN
        AND POSNR EQ IT_VBAP-POSNR.
  ENDIF.

  IT_VBAK_AUX[] = IT_VBAK[].
  SORT IT_VBAK_AUX BY KUNNR.
  DELETE ADJACENT DUPLICATES FROM IT_VBAK_AUX COMPARING KUNNR.

  IF IT_VBAK_AUX IS NOT INITIAL.
    SELECT KUNNR
           NAME1
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_VBAK_AUX
      WHERE KUNNR EQ IT_VBAK_AUX-KUNNR.
  ENDIF.

  IT_VBAP_AUX[] = IT_VBAP[].

  SORT IT_VBAP_AUX BY MATNR.
  DELETE ADJACENT DUPLICATES FROM IT_VBAP_AUX COMPARING MATNR.

  IF IT_VBAP_AUX IS NOT INITIAL.
    SELECT MATNR
           MAKTX
      FROM MAKT
      INTO TABLE IT_MAKT
      FOR ALL ENTRIES IN IT_VBAP_AUX
      WHERE MATNR EQ IT_VBAP_AUX-MATNR
        AND SPRAS EQ SY-LANGU.
  ENDIF.

  IT_VBAP_AUX[] = IT_VBAP[].

  SORT IT_VBAP_AUX BY WERKS.
  DELETE ADJACENT DUPLICATES FROM IT_VBAP_AUX COMPARING WERKS.


*-----Dados Remessa-----------------
  SELECT *
    FROM VBFA
    INTO TABLE IT_VBFA_ANT
    FOR ALL ENTRIES IN IT_VBAK
    WHERE VBELV   = IT_VBAK-VBELN
      AND VBTYP_N IN ('J', 'h','T','O', 'R')
      AND ERDAT NOT IN P_ERDAT."< P_ERDAT-LOW.

  SELECT *
    FROM VBFA
    INTO TABLE IT_VBFA_ATU
    FOR ALL ENTRIES IN IT_VBAK
    WHERE VBELV   = IT_VBAK-VBELN
      AND VBTYP_N IN ('J','h','T','O', 'R')
      AND ERDAT   IN P_ERDAT.

  SELECT *
    FROM VBFA
    INTO TABLE IT_VBFA_ANT_ATU
    FOR ALL ENTRIES IN IT_VBAK
    WHERE VBELV   = IT_VBAK-VBELN
      AND VBTYP_N IN ('J', 'T' )
      AND ERDAT   LT P_ERDAT-LOW.

  DELETE IT_VBFA_ANT WHERE VBTYP_N EQ 'J' AND PLMIN NE '+'.
  DELETE IT_VBFA_ATU WHERE VBTYP_N EQ 'J' AND PLMIN NE '+'.
  DELETE IT_VBFA_ANT_ATU WHERE VBTYP_N EQ 'J' AND PLMIN NE '+'.

  DELETE IT_VBFA_ANT WHERE VBTYP_N EQ 'T' AND PLMIN NE '+'.
  DELETE IT_VBFA_ATU WHERE VBTYP_N EQ 'T' AND PLMIN NE '+'.
  DELETE IT_VBFA_ANT_ATU WHERE VBTYP_N EQ 'T' AND PLMIN NE '+'.

  SELECT GDATU
         UKURS
    FROM TCURR
    INTO TABLE IT_TCURR
    WHERE FCURR EQ 'USD'
      AND TCURR EQ 'BRL'
      AND KURST IN P_KURST.

*   Complemento e Devolução Inicio

  SELECT  *
     FROM VBFA
     INTO TABLE IT_VBFA
     FOR ALL ENTRIES IN IT_VBAK
     WHERE VBELV     EQ  IT_VBAK-VBELN
     AND   VBTYP_N   IN  ('J', 'H', 'C', 'L' )
     AND   VBTYP_V   EQ  'C'.

  IF NOT IT_VBFA[] IS INITIAL.

    SELECT  *
      FROM VBFA
      INTO TABLE IT_VBFA_C
      FOR ALL ENTRIES IN IT_VBFA
      WHERE VBELV     EQ    IT_VBFA-VBELN
      AND   VBTYP_N   EQ    'M'
      AND   VBTYP_V   EQ    'C'.

    IF IT_VBFA_C[] IS NOT INITIAL.
      SELECT  *
        FROM VBFA
        INTO TABLE IT_VBFA_C_E
        FOR ALL ENTRIES IN IT_VBFA_C
        WHERE VBELV     EQ    IT_VBFA_C-VBELN
        AND   VBTYP_N   EQ    'N'
        AND   VBTYP_V   EQ    'M'.
    ENDIF.

    SELECT  *
        FROM VBFA
        INTO TABLE IT_VBFA_L
        FOR ALL ENTRIES IN IT_VBFA
        WHERE VBELV     EQ    IT_VBFA-VBELN
        AND   VBTYP_N   EQ    'P'
        AND   VBTYP_V   EQ    'L'.

    IF IT_VBFA_L[] IS NOT INITIAL.
      SELECT  *
        FROM VBFA
        INTO TABLE IT_VBFA_L_E
        FOR ALL ENTRIES IN IT_VBFA_L
        WHERE VBELV     EQ    IT_VBFA_L-VBELN
        AND   VBTYP_N   EQ    'N'
        AND   VBTYP_V   EQ    'P'.
    ENDIF.

    SELECT  *
      FROM VBFA
      INTO TABLE IT_VBFA_H
      FOR ALL ENTRIES IN IT_VBFA
      WHERE VBELV     EQ    IT_VBFA-VBELN
      AND   VBTYP_N   EQ    'O'
      AND   VBTYP_V   EQ    'H'.

    IF IT_VBFA_H[] IS NOT INITIAL.
      SELECT  *
        FROM VBFA
        INTO TABLE IT_VBFA_H_E
        FOR ALL ENTRIES IN IT_VBFA_H
        WHERE VBELV     EQ    IT_VBFA_H-VBELN
        AND   VBTYP_N   EQ    'S'
        AND   VBTYP_V   EQ    'O'.
    ENDIF.

  ENDIF.

*   Complemento e Devolução Fim



ENDFORM.                    "f_seleciona_dados

*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .

  DATA: VL_INDEX       TYPE I,
        SLD_ANT        TYPE VBFA-RFMNG,
        SLD_POS        TYPE VBFA-RFMNG,
        SLD_ATU        TYPE VBFA-RFMNG,
        SLD_ANT_ATU    TYPE VBFA-RFMNG,
        SLD_ANT_AR     TYPE VBFA-RFMNG,
        SLD_ATU_AR     TYPE VBFA-RFMNG,
        SLD_RECR       TYPE VBFA-RFMNG,
        SLD_RECD       TYPE VBFA-RFMNG,
        SLD_ATU_AR_PG  TYPE VBFA-RFMNG,
        SLD_ATU_AR_PG2 TYPE VBFA-RFMNG,
        SLD_ANT_AR2    TYPE VBFA-RFMNG,
        SLD_ATU_AR2    TYPE VBFA-RFMNG,
        VSD_ORDEM      TYPE VBFA-RFMNG,
        QT_DEV         TYPE VBFA-RFMNG,
        QT_COM         TYPE VBFA-RFWRT,
        P_GDATU        TYPE TCURR-GDATU,
        SL_DATA        TYPE C LENGTH 10,
        VL_AUX         TYPE C LENGTH 1,
        XVLRREC        TYPE BSAD-DMBTR,
        XVLRRECD       TYPE BSAD-DMBE2.

  SORT: IT_VBAK        BY VBELN      ,
        IT_VBAP        BY VBELN      ,
        IT_KNA1        BY KUNNR      ,
        IT_MAKT        BY MATNR      ,
        IT_KONV        BY KNUMV KSCHL,
        IT_VBEP        BY VBELN POSNR,
        IT_VBFA_ANT    BY VBELV      ,
        IT_VBFA_ATU    BY VBELV      ,
        IT_TCURR       BY GDATU      ,
        IT_VBKD        BY VBELN      ,
        IT_VBFA_OV     BY VBELN      ,
        IT_VBFA_DEV    BY VBELN      ,
        IT_VBFA_INI    BY VBELV      ,
        IT_VBFA_CONT   BY VBELN      .


  LOOP AT IT_VBAK INTO WA_VBAK.

    WA_SAIDA-ERDAT = WA_VBAK-ERDAT.
    WA_SAIDA-AUART = WA_VBAK-AUART.
    WA_SAIDA-KUNNR = WA_VBAK-KUNNR.

    IF WA_VBAK-KVGR2 = 'OV'.
      READ TABLE IT_VBFA_OV INTO WA_VBFA_OV WITH KEY VBELN = WA_VBAK-VGBEL BINARY SEARCH.

      IF WA_VBFA_OV IS INITIAL.
        READ TABLE IT_VBFA_DEV INTO WA_VBFA_OV WITH KEY VBELV = WA_VBAK-VGBEL BINARY SEARCH.
      ENDIF.

      WA_SAIDA-VBELN = WA_VBFA_OV-VBELV.
      WA_SAIDA-VGBEL = WA_VBAK-VBELN.

    ELSE.
      WA_SAIDA-VBELN = WA_VBAK-VBELN.
    ENDIF.

    READ TABLE  IT_VBFA_CONT INTO WA_VBFA_CONT WITH KEY VBELN = WA_VBAK-VBELN BINARY SEARCH.
    WA_SAIDA-VBELV = WA_VBFA_CONT-VBELV.

    " Centro e Material
    READ TABLE IT_VBAP INTO WA_VBAP WITH KEY VBELN = WA_VBAK-VBELN BINARY SEARCH.

    IF ( ( NOT P_CHARG IS INITIAL ) AND ( WA_VBAP-CHARG NOT IN P_CHARG ) ) OR
       ( ( NOT P_WERKS IS INITIAL ) AND ( WA_VBAP-WERKS NOT IN P_WERKS ) ) OR
       ( ( NOT P_MATNR IS INITIAL ) AND ( WA_VBAP-MATNR NOT IN P_MATNR ) ).
      CONTINUE.
    ENDIF.

    WA_SAIDA-WERKS = WA_VBAP-WERKS.
    WA_SAIDA-MATNR = WA_VBAP-MATNR.


    CLEAR SL_DATA.

    CONCATENATE WA_VBAK-ERDAT+6(2) '.' WA_VBAK-ERDAT+4(2) '.' WA_VBAK-ERDAT(4) INTO SL_DATA.

    "Nome
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBAK-KUNNR BINARY SEARCH.
    WA_SAIDA-NAME1 = WA_KNA1-NAME1 .

    "Material
    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_VBAP-MATNR BINARY SEARCH.
    WA_SAIDA-MAKTX = WA_MAKT-MAKTX  .


    "6 - Taxa de Dolar
    IF NOT SL_DATA = '00.00.0000'.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          INPUT  = SL_DATA
        IMPORTING
          OUTPUT = P_GDATU.
    ENDIF.

    READ TABLE IT_TCURR INTO WA_TCURR  WITH KEY GDATU = P_GDATU BINARY SEARCH.

    IF NOT SY-SUBRC IS INITIAL.
      READ TABLE IT_TCURR INTO WA_TCURR INDEX 1.
    ENDIF.

    READ TABLE IT_KONV INTO WA_KONV WITH KEY KNUMV = WA_VBAK-KNUMV KSCHL = 'IBRX' BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      IF WA_VBAK-WAERK EQ 'BRL'.
        WA_SAIDA-TOT_RS = WA_KONV-KWERT.
        WA_SAIDA-TOT_US = WA_KONV-KWERT / WA_TCURR-UKURS.
      ELSE.
        WA_SAIDA-TOT_RS = WA_KONV-KWERT * WA_TCURR-UKURS.
        WA_SAIDA-TOT_US = WA_KONV-KWERT.
      ENDIF.
    ENDIF.


    "3 - Seleção Dados de Quantidade
*    READ TABLE IT_VBEP INTO WA_VBEP WITH KEY VBELN = WA_VBAP-VBELN  POSNR = WA_VBAP-POSNR BINARY SEARCH.
    LOOP AT IT_VBEP INTO WA_VBEP WHERE VBELN = WA_VBAP-VBELN  AND POSNR = WA_VBAP-POSNR.
      ADD WA_VBEP-WMENG TO WA_SAIDA-WMENG.
    ENDLOOP.


    "4.1 - Remessas entradas no período anterior e posterior
    SLD_ANT = 0.
    SLD_POS = 0.
    LOOP AT IT_VBFA_ANT INTO WA_VBFA_ANT WHERE VBELV EQ WA_VBAK-VBELN .

      IF WA_SAIDA-VGBEL IS INITIAL AND  WA_VBFA_ANT-VBTYP_N EQ 'T'.
        CONTINUE.
      ENDIF.

      IF WA_VBFA_ANT-ERDAT < P_ERDAT-LOW.
        IF ( ( WA_VBFA_ANT-VBTYP_N = 'h') ).
          SUBTRACT WA_VBFA_ANT-RFMNG FROM SLD_ANT.
        ELSE.
          IF WA_VBFA_ANT-STUFE EQ '01'.
            ADD WA_VBFA_ANT-RFMNG TO SLD_ANT.
          ENDIF.
        ENDIF.
      ELSE.
        IF P_ERDAT-HIGH IS NOT INITIAL.
          IF WA_VBFA_ANT-ERDAT > P_ERDAT-HIGH.
            IF ( ( WA_VBFA_ANT-VBTYP_N = 'h') ).
              SUBTRACT WA_VBFA_ANT-RFMNG FROM SLD_POS.
            ELSE.
              IF WA_VBFA_ANT-STUFE EQ '01'.
                ADD WA_VBFA_ANT-RFMNG TO SLD_POS.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          IF WA_VBFA_ANT-ERDAT > P_ERDAT-LOW.
            IF ( ( WA_VBFA_ANT-VBTYP_N = 'h') ).
              SUBTRACT WA_VBFA_ANT-RFMNG FROM SLD_POS.
            ELSE.
              IF WA_VBFA_ANT-STUFE EQ '01'.
                ADD WA_VBFA_ANT-RFMNG TO SLD_POS.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*      IF ( ( WA_VBFA_ANT-VBTYP_N = 'T') AND ( WA_VBFA_ANT-VBTYP_V = 'H' ) ).
*        SLD_ANT = SLD_ANT + ( WA_VBFA_ANT-RFMNG ).
*      ENDIF.

    ENDLOOP.

    WA_SAIDA-XTRANT     = SLD_ANT.
    WA_SAIDA-XTRANT_POS = SLD_POS.

    "4.2 - Remessas entradas no período selecionado
    SLD_ATU = 0.
    LOOP AT IT_VBFA_ATU INTO WA_VBFA_ATU WHERE VBELV EQ WA_VBAK-VBELN .

      IF WA_SAIDA-VGBEL IS INITIAL AND  WA_VBFA_ATU-VBTYP_N EQ 'T'.
        CONTINUE.
      ENDIF.

      IF ( ( WA_VBFA_ATU-VBTYP_N = 'h' ) ) .
        SUBTRACT WA_VBFA_ATU-RFMNG FROM SLD_ATU.
      ELSE.
        IF WA_VBFA_ATU-STUFE EQ '01'.
          ADD WA_VBFA_ATU-RFMNG TO SLD_ATU.
        ENDIF.
      ENDIF.

*      IF ( ( WA_VBFA_ATU-VBTYP_N = 'T' ) AND ( WA_VBFA_ATU-VBTYP_V = 'H' ) ).
*        SLD_ATU = SLD_ATU + ( WA_VBFA_ATU-RFMNG ).
*      ENDIF.

      CLEAR WA_VBFA_ATU.

    ENDLOOP.

*******************************************************************************************
    VSD_ORDEM = 0.
    QT_DEV    = 0.
    QT_COM    = 0.

    LOOP AT IT_VBFA INTO WA_VBFA WHERE VBELV = WA_VBAK-VBELN.

      IF WA_VBFA-VBTYP_N EQ 'J'.
        ADD WA_VBFA-RFMNG TO VSD_ORDEM.
      ENDIF.

      LOOP AT IT_VBFA_C INTO WA_VBFA_C WHERE VBELV = WA_VBFA-VBELN.
        READ TABLE IT_VBFA_C_E TRANSPORTING NO FIELDS WITH KEY VBELV = WA_VBFA_C-VBELN.
        IF SY-SUBRC IS NOT INITIAL.
          ADD WA_VBFA_C-RFMNG TO QT_COM.
        ENDIF.
      ENDLOOP.

      LOOP AT IT_VBFA_L INTO WA_VBFA_L WHERE VBELV = WA_VBFA-VBELN.
        READ TABLE IT_VBFA_L_E TRANSPORTING NO FIELDS WITH KEY VBELV = WA_VBFA_L-VBELN.
        IF SY-SUBRC IS NOT INITIAL.
          ADD WA_VBFA_L-RFMNG TO QT_COM.
        ENDIF.
      ENDLOOP.

      LOOP AT IT_VBFA_H INTO WA_VBFA_H WHERE VBELV = WA_VBFA-VBELN.
        READ TABLE IT_VBFA_H_E TRANSPORTING NO FIELDS WITH KEY VBELV = WA_VBFA_H-VBELN.
        IF SY-SUBRC IS NOT INITIAL.
          WA_VBFA_H-RFWRT = WA_VBFA_H-RFWRT * -1.
          ADD WA_VBFA_H-RFMNG TO QT_DEV.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

*    WA_SAIDA-SD_ORDEM =  WA_SAIDA-WMENG - VSD_ORDEM.    " Saldo da Ordem
    WA_SAIDA-QT_DEV   =  QT_DEV.                        " Quantidade de Devolução
    WA_SAIDA-QT_COMPL =  QT_COM.                        " Quantidade de Complemento

*******************************************************************************************

    " CSB
    "Remessas entradas no período menor que selecionado
    SLD_ANT_ATU = 0.
    LOOP AT IT_VBFA_ANT_ATU INTO WA_VBFA_ANT_ATU WHERE VBELV EQ WA_VBAK-VBELN .

      IF WA_SAIDA-VGBEL IS INITIAL AND  WA_VBFA_ATU-VBTYP_N EQ 'T'.
        CONTINUE.
      ENDIF.

      SLD_ANT_ATU = SLD_ANT_ATU + WA_VBFA_ANT_ATU-RFMNG.

      CLEAR WA_VBFA_ANT_ATU.
    ENDLOOP.

    WA_SAIDA-XTRATU = SLD_ATU.
    WA_SAIDA-XTRANT_ATU = SLD_ANT_ATU.


    IF WA_SAIDA-WMENG > 0 .
      WA_SAIDA-KBETR      = WA_SAIDA-TOT_RS / WA_SAIDA-WMENG.
    ELSE.
      WA_SAIDA-KBETR      = WA_SAIDA-TOT_RS.
    ENDIF.


    READ TABLE IT_VBKD INTO WA_VBKD WITH KEY VBELN = WA_VBAK-VBELN BINARY SEARCH.

    IF NOT P_BSTKD IS INITIAL AND WA_VBKD-BSTKD NOT IN P_BSTKD.
      CONTINUE.
    ENDIF.

    WA_SAIDA-BSTKD      =  WA_VBKD-BSTKD.
    WA_SAIDA-SLD_OV     = ( ( WA_SAIDA-WMENG   - WA_SAIDA-XTRATU ) - WA_SAIDA-XTRANT_ATU ) - WA_SAIDA-XTRANT_POS. "CSB

    VL_AUX = 'N'.

    IF WA_VBAK-KVGR2 = 'OV'.

      LOOP AT IT_VBFA_INI INTO WA_VBFA_INI WHERE VBELV = WA_VBAK-VBELN .
        IF WA_VBFA_INI-VBTYP_N EQ 'T'.
          VL_AUX = 'S'.
        ENDIF.
      ENDLOOP.

      IF VL_AUX EQ 'S'.
        WA_SAIDA-TOT_RS     = WA_SAIDA-TOT_RS     * -1.
        WA_SAIDA-TOT_US     = WA_SAIDA-TOT_US     * -1.
        WA_SAIDA-WMENG      = WA_SAIDA-WMENG      * -1.
        WA_SAIDA-XTRATU     = WA_SAIDA-XTRATU     * -1.
        WA_SAIDA-XTRANT     = WA_SAIDA-XTRANT     * -1.
        WA_SAIDA-SLD_OV     = WA_SAIDA-SLD_OV     * -1.
        WA_SAIDA-KBETR      = WA_SAIDA-KBETR      * -1.
      ENDIF  .
    ENDIF.

    APPEND WA_SAIDA TO IT_SAIDA.

    CLEAR: WA_VBAK       ,
           WA_VBAP       ,
           WA_KNA1       ,
           WA_MAKT       ,
           WA_KONV       ,
           WA_VBEP       ,
           WA_SAIDA      ,
           WA_VBFA_ATU   ,
           WA_VBFA_ANT   ,
           WA_TCURR      ,
           WA_VBKD       ,
           WA_VBFA_OV    ,
           WA_VBFA_CONT  .
  ENDLOOP.

ENDFORM.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_BDC_FIELD  USING    VALUE(P_FLAG)
                           VALUE(P_FNAM)
                           VALUE(P_FVAL).

  CLEAR T_BDC.
  IF NOT P_FLAG IS INITIAL.
    T_BDC-PROGRAM  = P_FNAM.
    T_BDC-DYNPRO   = P_FVAL.
    T_BDC-DYNBEGIN = 'X'.
  ELSE.
    T_BDC-FNAM = P_FNAM.
    T_BDC-FVAL = P_FVAL.
  ENDIF.
  APPEND T_BDC.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_STATUS OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_LAYOUT .
  WA_LAYOUT-ZEBRA = 'X'.
  WA_LAYOUT-NO_HEADERS = ' '.
ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO TYPE C
                               P_DESC  TYPE C
                               P_TAM   TYPE C
                               P_HOT   TYPE C
                               P_ZERO  TYPE C.
  DATA: WL_FCAT TYPE TY_ESTRUTURA.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO   .
  WL_FCAT-SELTEXT_S = P_DESC    .
  WL_FCAT-SELTEXT_M = P_DESC    .
  WL_FCAT-SELTEXT_L = P_DESC    .
  WL_FCAT-HOTSPOT   = P_HOT     .
  WL_FCAT-NO_ZERO   = P_ZERO    .
  WL_FCAT-OUTPUTLEN = P_TAM     .

  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.                    " ALV_PREENCHE_CAT

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA WA_EVENT TYPE REF TO LCL_EVENT_RECEIVER.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:ZM_HANDLE_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                  E_COLUMN_ID
                  ES_ROW_NO                      ,

      ZM_HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING
            E_OBJECT E_INTERACTIVE                   ,

      ZM_HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING
            E_UCOMM.


ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT.
    PERFORM Z_HANDLE_HOTSPOT USING    E_ROW_ID
                                      E_COLUMN_ID
                                      ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD ZM_HANDLE_TOOLBAR.
*   Incluindo Botão ALV
    PERFORM Z_HANDLE_TOOLBAR USING E_OBJECT
                                   E_INTERACTIVE.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD ZM_HANDLE_USER_COMMAND.
*   User Command Botões Incluidos
    PERFORM Z_HANDLE_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_USER_COMMAND INPUT.
  IF SY-DYNNR EQ '0100'.
    CASE SY-UCOMM.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM Z_HANDLE_HOTSPOT  USING    P_E_ROW_ID TYPE LVC_S_ROW
                                P_E_COLUMN_ID TYPE  LVC_S_COL
                                P_ES_ROW_NO TYPE  LVC_S_ROID.

  CASE P_E_COLUMN_ID.
    WHEN 'VBELN'.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_ES_ROW_NO-ROW_ID.
      IF NOT WA_SAIDA-VBELN IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD WA_SAIDA-VBELN.   "preenche o campo da tela de pesquisa
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado

      ENDIF.

  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM Z_HANDLE_TOOLBAR  USING    P_OBJECT  TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                                P_INTERACTIVE TYPE CHAR1 .

** Constants for button type
  CONSTANTS:
    C_BUTTON_NORMAL           TYPE I VALUE 0,
    C_MENU_AND_DEFAULT_BUTTON TYPE I VALUE 1,
    C_MENU                    TYPE I VALUE 2,
    C_SEPARATOR               TYPE I VALUE 3,
    C_RADIO_BUTTON            TYPE I VALUE 4,
    C_CHECKBOX                TYPE I VALUE 5,
    C_MENU_ENTRY              TYPE I VALUE 6.

  DATA SL_TOOLBAR TYPE STB_BUTTON.

* Append Seperator
  MOVE C_SEPARATOR  TO SL_TOOLBAR-BUTN_TYPE.
  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.



ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM Z_HANDLE_COMMAND  USING P_UCOMM TYPE SYUCOMM       .

  CASE P_UCOMM.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY .
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

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

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " f_carregar_eventos

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES.

  DATA: W_TEXTO1(10),
        W_TEXTO2(10),
        W_TEXTO3(50),
        PERIODO_R    TYPE C LENGTH 50,
        PERIODO_O    TYPE C LENGTH 50.


  V_REPORT = SY-REPID.

  IF P_VARIA IS NOT INITIAL.
    VG_VARIANT-VARIANT = P_VARIA.
  ENDIF.
  CLEAR: W_TEXTO1, W_TEXTO2, W_TEXTO3.

*** Nome do Report
  W_TEXTO3 = 'Relatório Consulta Saldo de Ordem de Venda'.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO3.

  CONCATENATE P_ERDAT-LOW+6(2)   '.' P_ERDAT-LOW+4(2)  '.' P_ERDAT-LOW(4)  INTO W_TEXTO1 .
  CONCATENATE P_ERDAT-HIGH+6(2)  '.' P_ERDAT-HIGH+4(2) '.' P_ERDAT-HIGH(4) INTO W_TEXTO2 .

  IF W_TEXTO2 IS INITIAL OR W_TEXTO2 EQ '00.00.0000'.
    CONCATENATE 'Periodo Remessa:' W_TEXTO1 INTO PERIODO_R SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Periodo Remessa:' W_TEXTO1 '-' W_TEXTO2 INTO PERIODO_R SEPARATED BY SPACE.
  ENDIF.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' PERIODO_R.

  CLEAR: W_TEXTO1, W_TEXTO2.
  CONCATENATE P_PERIOD-LOW+6(2)   '.' P_PERIOD-LOW+4(2)  '.' P_PERIOD-LOW(4)  INTO W_TEXTO1 .
  CONCATENATE P_PERIOD-HIGH+6(2)  '.' P_PERIOD-HIGH+4(2) '.' P_PERIOD-HIGH(4) INTO W_TEXTO2 .

  IF W_TEXTO2 IS INITIAL OR W_TEXTO2 EQ '00.00.0000'.
    CONCATENATE 'Periodo Ordem de Venda:' W_TEXTO1 INTO PERIODO_O SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Periodo Ordem de Venda:' W_TEXTO1 '-' W_TEXTO2 INTO PERIODO_O SEPARATED BY SPACE.
  ENDIF.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S' PERIODO_O.

ENDFORM.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV.
  PERFORM ALV_PREENCHE_CAT USING:
        'VBELN'         TEXT-004   '10'  'X'  'X', "Nrº O.V
        'VGBEL'         TEXT-005   '20'  'X'  'X', "Nrº O.V Comp/Dev
        'ERDAT'         TEXT-006   '12'  ' '  ' ', "Dt. Criação
        'WERKS'         TEXT-007   '8'   ' '  ' ', "Centro
        'MATNR'         TEXT-008   '15'  ' '  'X', "Cód. Material
        'MAKTX'         TEXT-009   '30'  ' '  ' ', "Material
        'BSTKD'         TEXT-010   '20'  ' '  ' ', "Numero do Pedido
        'VBELV'         TEXT-011   '15'  'X'  'X', "Nº Contrato
        'KUNNR'         TEXT-012   '15'  ' '  'X', "Cód. Cliente
        'NAME1'         TEXT-013   '25'  ' '  ' ', "Nome cliente
        'AUART'         TEXT-014   '10'  ' '  ' ', "Tipo OV
        'WMENG'         TEXT-015   '13'  ' '  ' ', "Quantidade
*        'SD_ORDEM'      TEXT-023   '13'  ' '  ' ', "Sdo Ordem
        'QT_DEV'        TEXT-024   '13'  ' '  ' ', "Qtd Devolução
        'QT_COMPL'      TEXT-025   '13'  ' '  ' ', "Qtd Complemento
        'KBETR'         TEXT-016   '13'  ' '  ' ', "VLR. Unit.
        'TOT_RS'        TEXT-017   '15'  ' '  ' ', "Valor Total R$
        'TOT_US'        TEXT-018   '15'  ' '  ' ', "Valor Total U$
        'XTRANT_ATU'    TEXT-019   '15'  ' '  ' ', "Qt.Rem.Per.Ant.
*        'XTRANT'        TEXT-020   '15'  ' '  ' ', "Qt.Rem.Per.Acu.
        'XTRATU'        TEXT-021   '15'  ' '  ' ', "Qt. Rem. Período
        'XTRANT_POS'    TEXT-026   '15'  ' '  ' ', "Qt.Rem.Per.POS.
        'SLD_OV'        TEXT-022   '15'  ' '  ' '. "Qtd. Saldo OV

ENDFORM.                    " F_ALV

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .

  IF IT_SAIDA[] IS INITIAL.
    MESSAGE I000(Z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.
  PERFORM F_DEFINIR_EVENTOS.
  PERFORM F_ALV_SORT.
  PERFORM F_ALV.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      I_CALLBACK_USER_COMMAND = 'F_USER_COMMAND'
      IT_FIELDCAT             = IT_FCAT[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'X'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
      IS_VARIANT              = VG_VARIANT
    TABLES
      T_OUTTAB                = IT_SAIDA.

ENDFORM.                    " F_IMPRIME_DADOS

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
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM F_USER_COMMAND USING L_UCOMM
                          L_SELFIELD TYPE SLIS_SELFIELD.

  CASE L_SELFIELD-FIELDNAME.
    WHEN 'VBELN'.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX L_SELFIELD-TABINDEX.
      IF NOT WA_SAIDA-VBELN IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD WA_SAIDA-VBELN.   "preenche o campo da tela de pesquisa
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado

      ENDIF.
    WHEN 'VGBEL'.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX L_SELFIELD-TABINDEX.
      IF NOT WA_SAIDA-VGBEL IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD WA_SAIDA-VGBEL.   "preenche o campo da tela de pesquisa
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado
      ENDIF.

    WHEN 'VBELV'.
      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX L_SELFIELD-TABINDEX.
      IF NOT WA_SAIDA-VBELV IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD WA_SAIDA-VBELV.   "preenche o campo da tela de pesquisa
        CALL TRANSACTION 'VA43' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado
      ENDIF.
  ENDCASE.

ENDFORM.                    "f_user_command
