*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Tavares                                         &*
*& Data.....: 25/11/2013                                              &*
*& Descrição: Relatório de Margem de Insumos                          &*
*& Transação: ZFI0038                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  ZFIR048.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.
*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_VBAK,
        VKORG TYPE VBAK-VKORG,
        SPART TYPE VBAK-SPART,
        VKBUR TYPE VBAK-VKBUR,
        VKGRP TYPE VBAK-VKGRP,
        AUART TYPE VBAK-AUART,
        KUNNR TYPE VBAK-KUNNR,
        VBELN TYPE VBAK-VBELN,
        KNUMV TYPE VBAK-KNUMV,
        ERDAT TYPE VBAK-ERDAT,
      END OF TY_VBAK,

      BEGIN OF TY_VBAP,
        VBELN  TYPE VBAP-VBELN,
        VBELV  TYPE VBAP-VBELV,
        MATNR  TYPE VBAP-MATNR,
        ARKTX  TYPE VBAP-ARKTX,
        MATKL  TYPE VBAP-MATKL,
        KWMENG TYPE VBAP-KWMENG,
        VRKME  TYPE VBAP-VRKME,
        WERKS  TYPE VBAP-WERKS,
        NETPR  TYPE VBAP-NETPR,
        WAERK  TYPE VBAP-WAERK,
        POSNR  TYPE VBAP-POSNR,
      END OF TY_VBAP,

      BEGIN OF TY_KONV,
        KNUMV TYPE KONV-KNUMV,
        KPOSN TYPE KONV-KPOSN,
        KSCHL TYPE KONV-KSCHL,
        KBETR TYPE KONV-KBETR,
        KWERT TYPE KONV-KWERT,
        KMEIN TYPE KONV-KMEIN,
      END OF TY_KONV,

      BEGIN OF TY_MARA,
        MATNR TYPE MARA-MATNR,
        WRKST TYPE MARA-WRKST,
      END OF TY_MARA,

      BEGIN OF TY_KNA1,
        KUNNR TYPE KNA1-KUNNR,
        NAME1 TYPE KNA1-NAME1,
      END OF TY_KNA1,

      BEGIN OF TY_EKPO,
        MATNR TYPE EKPO-MATNR,
        WERKS TYPE EKPO-WERKS,
        NETPR TYPE EKPO-NETPR,
        BPUMN TYPE EKPO-BPUMN,
      END OF TY_EKPO,

      BEGIN OF TY_ZSDT0037,
        FILIAL_ORIGEM  TYPE ZSDT0037-FILIAL_ORIGEM,
        FILIAL_DESTINO TYPE ZSDT0037-FILIAL_DESTINO,
        VAL_DE         TYPE ZSDT0037-VAL_DE,
        VAL_ATE        TYPE ZSDT0037-VAL_ATE,
        VLR_FRETE      TYPE ZSDT0037-VLR_FRETE,
      END OF TY_ZSDT0037,

      BEGIN OF TY_ZSDT0077,
        DT_COTACAO TYPE ZSDT0077-DT_COTACAO,
        UKURS      TYPE ZSDT0077-UKURS,
      END OF TY_ZSDT0077,

      BEGIN OF TY_T006,
        MSEHI TYPE T006-MSEHI,
        ZAEHL TYPE T006-ZAEHL,
      END OF TY_T006,

      BEGIN OF TY_ZSDT0078,
        MATNR TYPE ZSDT0078-MATNR,
        WERKS TYPE ZSDT0078-WERKS,
        MEINS TYPE ZSDT0078-MEINS,
        NETPR TYPE ZSDT0078-NETPR,
      END OF TY_ZSDT0078,

      BEGIN OF TY_VBFA,
        VBELV   TYPE VBFA-VBELV,
        VBELN   TYPE VBFA-VBELN,
        POSNV   TYPE VBFA-POSNV,
        VBTYP_N TYPE VBFA-VBTYP_N,
        VBTYP_V TYPE VBFA-VBTYP_V,
        RFMNG   TYPE VBFA-RFMNG,
      END OF TY_VBFA,

      BEGIN OF TY_VTFA,
        VBELV   TYPE VTFA-VBELV,
        VBTYP_N TYPE VTFA-VBTYP_N,
        VBTYP_V TYPE VTFA-VBTYP_V,
        VBELN   TYPE VTFA-VBELN,
      END OF TY_VTFA,

      BEGIN OF TY_VFKP,
        FKNUM TYPE VFKP-FKNUM,
        KNUMV TYPE VFKP-KNUMV,
      END OF TY_VFKP,

      BEGIN OF TY_SAIDA,
        KUNNR              TYPE VBAK-KUNNR ,
        NAME1              TYPE KNA1-NAME1 ,
        VKBUR              TYPE VBAK-VKBUR ,
        VBELN              TYPE VBAP-VBELN ,
        POSNR              TYPE VBAP-POSNR ,
        MATNR              TYPE VBAP-MATNR ,
        ARKTX              TYPE VBAP-ARKTX ,
        WRKST              TYPE MARA-WRKST ,
        KWMENG             TYPE VBAP-KWMENG,
        E_CUSTO_MAT        TYPE EKPO-NETPR,
        VLR_FRETE          TYPE ZSDT0037-VLR_FRETE,
        WAERK              TYPE VBAP-WAERK,
        E_VLR_UNIT_VDA     TYPE KONV-KBETR,
        E_VLR_TOTAL_COMPRA TYPE EKPO-BPUMN,
        E_VLR_TOTAL_VENDA  TYPE KONV-KWERT,
        E_TOTAL_FRETE      TYPE T006-ZAEHL,
        E_MARGEM_TO        TYPE T006-ZAEHL,
        E_MARGEM_TOTAL     TYPE T006-ZAEHL,
        R_CUSTO_MATERIAL   TYPE EKPO-NETPR,
        R_QTE_FATURADO     TYPE VBFA-RFMNG,
        R_FRETE_REAL       TYPE KONV-KWERT,
        R_VLR_UNIT_VDA     TYPE KONV-KBETR,
        R_VLR_TOTAL_COMPRA TYPE EKPO-BPUMN,
        R_VLR_TOTAL_VENDA  TYPE T006-ZAEHL,
        R_TOTAL_FRETE_REAL TYPE T006-ZAEHL,
        R_MARGEM_EFETIVA   TYPE EKPO-NETPR,
        R_MARGEM_TOTAL     TYPE T006-ZAEHL,
        MATERIAL(100),
        KWERT             TYPE KONV-KWERT,
      END OF TY_SAIDA,

      BEGIN OF TY_SMARTFORMS,
        MATERIAL(100),
        WRKST              TYPE MARA-WRKST,
        KWMENG             TYPE VBAP-KWMENG,
        E_VLR_TOTAL_COMPRA TYPE EKPO-BPUMN,
        KWERT              TYPE KONV-KWERT,
        E_TOTAL_FRETE      TYPE T006-ZAEHL,
        E_MARGEM_TO        TYPE T006-ZAEHL,
        E_MARGEM_TOTAL     TYPE T006-ZAEHL,
        R_TOTAL_FRETE_REAL TYPE T006-ZAEHL,
        R_MARGEM_EFETIVA   TYPE EKPO-NETPR,
        R_MARGEM_TOTAL     TYPE T006-ZAEHL,
      END OF TY_SMARTFORMS.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: T_VBAK       TYPE TABLE OF TY_VBAK,
      T_VBAP       TYPE TABLE OF TY_VBAP,
      T_KONV       TYPE TABLE OF TY_KONV,
      T_MARA       TYPE TABLE OF TY_MARA,
      T_KNA1       TYPE TABLE OF TY_KNA1,
      T_EKPO       TYPE TABLE OF TY_EKPO,
      T_0037       TYPE TABLE OF TY_ZSDT0037,
      T_0037_AUX   TYPE TABLE OF TY_ZSDT0037,
      T_0077       TYPE TABLE OF TY_ZSDT0077,
      T_T006       TYPE TABLE OF TY_T006,
      T_0078       TYPE TABLE OF TY_ZSDT0078,
      T_VBFA       TYPE TABLE OF TY_VBFA,
      T_VBFA_AUX   TYPE TABLE OF TY_VBFA,
      T_VTFA       TYPE TABLE OF TY_VTFA,
      T_VFKP       TYPE TABLE OF TY_VFKP,
      T_SMARTFORMS TYPE TABLE OF TY_SMARTFORMS,
      T_SAIDA      TYPE TABLE OF TY_SAIDA.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_VBAK        TYPE TY_VBAK,
      WA_VBAP        TYPE TY_VBAP,
      WA_KONV        TYPE TY_KONV,
      WA_MARA        TYPE TY_MARA,
      WA_KNA1        TYPE TY_KNA1,
      WA_EKPO        TYPE TY_EKPO,
      WA_0037        TYPE TY_ZSDT0037,
      WA_0077        TYPE TY_ZSDT0077,
      WA_T006        TYPE TY_T006,
      WA_0078        TYPE TY_ZSDT0078,
      WA_VBFA        TYPE TY_VBFA,
      WA_VTFA        TYPE TY_VTFA,
      WA_VFKP        TYPE TY_VFKP,
      WA_SMARTFORMS  TYPE TY_SMARTFORMS,
      WA_SAIDA       TYPE TY_SAIDA.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER.

**--------------------------------------------------------------------**
**                       Declaração - Variaveis globais
**--------------------------------------------------------------------**
DATA:  WG_FILE             TYPE RLGRAP-FILENAME.
DATA: X_FILENAME           TYPE STRING,
      WG_TABIX             TYPE SY-TABIX.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULÁRIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: S_VKORG  FOR WA_VBAK-VKORG," OBLIGATORY,
                S_SPART  FOR WA_VBAK-SPART,
                S_VKBUR  FOR WA_VBAK-VKBUR,
                S_VKGRP  FOR WA_VBAK-VKGRP,
                S_AUART  FOR WA_VBAK-AUART," OBLIGATORY,
                S_ERDAT  FOR WA_VBAK-ERDAT." OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.
SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME.
PARAMETERS: P_PDF RADIOBUTTON GROUP A1 USER-COMMAND MUDA_TELA,
            P_ALV RADIOBUTTON GROUP A1 DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK B2.


SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME.
PARAMETERS: P_FILE(250)  TYPE C OBLIGATORY DEFAULT 'C:\' MODIF ID C2.
SELECTION-SCREEN: END OF BLOCK B3.

AT SELECTION-SCREEN OUTPUT.
  IF P_PDF IS INITIAL.
  ENDIF.
  PERFORM MODIFICA_TELA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

PERFORM F_ABRE_ARQUIVO.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF P_PDF IS NOT  INITIAL.
    IF   S_VKORG-LOW IS INITIAL
      OR S_AUART-LOW IS INITIAL
      OR S_ERDAT-LOW IS INITIAL.

      MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'É obrigatório o preenchimento de todos os campos.'.
    ELSE.
      PERFORM SELECIONAR_DADOS.
      PERFORM ORGANIZAR_DADOS.
      PERFORM INICIAR_VARIAVEIS.
      PERFORM IMPRIMIR_DADOS.
    ENDIF.

  ELSEIF P_ALV IS NOT INITIAL.
    IF   S_VKORG-LOW IS INITIAL
      OR S_AUART-LOW IS INITIAL
      OR S_ERDAT-LOW IS INITIAL.

      MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH 'É obrigatório o preenchimento de todos os campos.'.
    ELSE.
      PERFORM SELECIONAR_DADOS.
      PERFORM ORGANIZAR_DADOS.
      PERFORM INICIAR_VARIAVEIS.
      PERFORM IMPRIMIR_DADOS.
    ENDIF.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  SELECIONAR_DADOS .
  SELECT VKORG SPART VKBUR VKGRP AUART KUNNR VBELN KNUMV ERDAT
    FROM VBAK
      INTO TABLE T_VBAK
        WHERE VKORG IN S_VKORG
          AND SPART IN S_SPART
          AND VKBUR IN S_VKBUR
          AND VKGRP IN S_VKGRP
          AND AUART IN S_AUART
          AND ERDAT IN S_ERDAT.

  IF SY-SUBRC IS INITIAL.
    SELECT VBELN VBELV MATNR ARKTX MATKL KWMENG VRKME WERKS NETPR WAERK POSNR
      FROM VBAP
        INTO TABLE T_VBAP
        FOR ALL ENTRIES IN T_VBAK
          WHERE VBELN EQ T_VBAK-VBELN.

*   Busca de Dados Materiais
    IF SY-SUBRC IS INITIAL.
      SELECT MATNR WRKST
        FROM MARA
          INTO TABLE T_MARA
          FOR ALL ENTRIES IN T_VBAP
            WHERE MATNR EQ T_VBAP-MATNR.

*     Busca de Dados Pedido de Compras
      SELECT MATNR WERKS NETPR BPUMN
        FROM EKPO
          INTO TABLE T_EKPO
          FOR ALL ENTRIES IN T_VBAP
             WHERE MATNR EQ T_VBAP-MATNR
               AND WERKS EQ T_VBAP-WERKS.

*     Busca de Dados Previsão de Preço de Compras
      SELECT MATNR WERKS MEINS NETPR
        FROM ZSDT0078
          INTO TABLE T_0078
          FOR ALL ENTRIES IN T_VBAP
            WHERE MATNR	EQ T_VBAP-MATNR
              AND WERKS	EQ T_VBAP-WERKS.

*     Busca de Dados Faturamento
      SELECT VBELV VBELN POSNV VBTYP_N VBTYP_V RFMNG
        FROM VBFA
          INTO TABLE T_VBFA
          FOR ALL ENTRIES IN T_VBAP
            WHERE VBELV	      EQ T_VBAP-VBELV
              AND POSNV	      EQ T_VBAP-POSNR
              AND (   VBTYP_N EQ 'M'
                   OR VBTYP_N EQ 'J' )
              AND VBTYP_V     EQ 'C'.

*     Busca de Dados Custo do Frete
      T_VBFA_AUX[] = T_VBFA.
      DELETE T_VBFA_AUX WHERE VBTYP_N NE 'J'.

      IF T_VBFA_AUX[] IS NOT INITIAL.
        SELECT VBELV VBELN POSNV VBTYP_N VBTYP_V RFMNG
          FROM VBFA
            APPENDING TABLE T_VBFA
            FOR ALL ENTRIES IN T_VBFA_AUX
              WHERE VBELV EQ T_VBFA_AUX-VBELN
                AND POSNV	EQ T_VBFA_AUX-POSNV
                AND VBTYP_N	EQ '8'
                AND VBTYP_V	EQ 'J'.

        IF SY-SUBRC IS INITIAL.
          SELECT VBELV VBTYP_N VBTYP_V VBELN
            FROM VTFA
              INTO TABLE T_VTFA
              FOR ALL ENTRIES IN T_VBFA
                WHERE VBELV EQ T_VBFA-VBELN
                  AND VBTYP_N EQ '8'
                  AND VBTYP_V EQ 'A'.

          IF SY-SUBRC IS INITIAL.
            SELECT FKNUM KNUMV
              FROM VFKP
                INTO TABLE T_VFKP
                FOR ALL ENTRIES IN T_VTFA
                  WHERE FKNUM EQ T_VTFA-VBELN.

            IF SY-SUBRC IS INITIAL.
              SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , KBETR , KWERT , KMEIN FOR ALL ENTRIES IN @T_VFKP WHERE KNUMV EQ @T_VFKP-KNUMV AND ( KSCHL EQ 'ZFRE' OR KSCHL EQ 'ZSEG' OR KSCHL EQ 'ZIOF' OR KSCHL EQ 'ZPED' ) INTO TABLE @T_KONV .
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.

*     Busca de Dados Previsão de Frete
      SELECT FILIAL_ORIGEM FILIAL_DESTINO VAL_DE VAL_ATE VLR_FRETE
        FROM ZSDT0037
          INTO TABLE T_0037
          FOR ALL ENTRIES IN T_VBAP
             WHERE FILIAL_ORIGEM EQ T_VBAP-WERKS
               AND VAL_DE  GE SY-DATUM
               AND VAL_ATE LE SY-DATUM.

    ENDIF.

    SELECT FROM V_KONV FIELDS KNUMV , KPOSN , KSCHL , KBETR , KWERT , KMEIN FOR ALL ENTRIES IN @T_VBAK WHERE KNUMV EQ @T_VBAK-KNUMV AND KSCHL EQ 'PR00' INTO TABLE @T_KONV .

*   Busca de Dados Conversão unidade de medida
    IF SY-SUBRC IS INITIAL.
      SELECT MSEHI ZAEHL
        FROM T006
          INTO TABLE T_T006
            FOR ALL ENTRIES IN T_KONV
              WHERE MSEHI	EQ T_KONV-KMEIN.
    ENDIF.

*   Busca de Dados Cliente
    SELECT KUNNR NAME1
      FROM KNA1
        INTO TABLE T_KNA1
        FOR ALL ENTRIES IN T_VBAK
          WHERE KUNNR EQ T_VBAK-KUNNR.

  ENDIF.

* Busca de Dados Previsão de Taxa de Cambio
  SELECT DT_COTACAO UKURS
    FROM ZSDT0077
      INTO TABLE T_0077.


ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZAR_DADOS .
  DATA: WL_TOTAL_FAT   TYPE VBFA-RFMNG,
        WL_TOTAL_FRETE TYPE KONV-KWERT.

  SORT: T_0077 BY DT_COTACAO DESCENDING.


  CLEAR:WL_TOTAL_FAT, WL_TOTAL_FRETE.

  LOOP AT T_VBAP INTO WA_VBAP.
    READ TABLE T_VBAK INTO WA_VBAK
      WITH KEY VBELN = WA_VBAP-VBELN.

    IF SY-SUBRC IS INITIAL.
      READ TABLE T_KONV INTO WA_KONV
        WITH KEY KNUMV = WA_VBAK-KNUMV
                 KPOSN = WA_VBAP-POSNR
                 KSCHL = 'PR00'.

      IF SY-SUBRC IS INITIAL.
        READ TABLE T_T006 INTO WA_T006
          WITH KEY MSEHI = WA_KONV-KMEIN.

        WA_SAIDA-E_TOTAL_FRETE = ( ( WA_VBAP-KWMENG * WA_0037-VLR_FRETE ) / WA_T006-ZAEHL ). "Smartforms
      ENDIF.

      READ TABLE T_MARA INTO WA_MARA
       WITH KEY MATNR = WA_VBAP-MATNR.

      READ TABLE T_KNA1 INTO WA_KNA1
        WITH KEY KUNNR = WA_VBAK-KUNNR.

      READ TABLE T_EKPO INTO WA_EKPO
        WITH KEY MATNR = WA_VBAP-MATNR
                 WERKS = WA_VBAP-WERKS.

      IF SY-SUBRC IS INITIAL.
        WA_SAIDA-E_CUSTO_MAT = WA_EKPO-NETPR.

      ELSE.
        READ TABLE T_0078 INTO WA_0078
          WITH KEY MATNR = WA_VBAP-MATNR
                   WERKS = WA_VBAP-WERKS.

        WA_SAIDA-E_CUSTO_MAT = WA_0078-NETPR.
      ENDIF.

      READ TABLE T_0037 INTO WA_0037
        WITH KEY FILIAL_ORIGEM  = WA_VBAP-WERKS
                 FILIAL_DESTINO = WA_VBAK-VKBUR.

      READ TABLE T_0078 INTO WA_0078
        WITH KEY MATNR = WA_VBAP-MATNR
                 WERKS = WA_VBAP-WERKS
                 MEINS = WA_KONV-KMEIN.

      LOOP AT T_VBFA INTO WA_VBFA
        WHERE VBELV EQ WA_VBAP-VBELV
          AND POSNV EQ WA_VBAP-POSNR
          AND VBTYP_N EQ 'M'
          AND VBTYP_V EQ 'C'.

        WL_TOTAL_FAT = WL_TOTAL_FAT + WA_VBFA-RFMNG.

      ENDLOOP.

      READ TABLE T_VBFA INTO WA_VBFA
        WITH KEY VBELV = WA_VBAP-VBELN
                 POSNV = WA_VBAP-POSNR
                 VBTYP_N = 'J'
                 VBTYP_V = 'C' .

      IF SY-SUBRC IS INITIAL.
        READ TABLE T_VBFA INTO WA_VBFA
          WITH KEY VBELV = WA_VBAP-VBELV
                   POSNV = WA_VBAP-POSNR
                   VBTYP_N = '8'
                   VBTYP_V = 'J'.

        IF SY-SUBRC IS INITIAL.
          READ TABLE T_VTFA INTO WA_VTFA
            WITH KEY VBELN = WA_VBFA-VBELN
                     VBTYP_N = '8'
                     VBTYP_V = 'A'.

          IF SY-SUBRC IS INITIAL.
            READ TABLE T_VFKP INTO WA_VFKP
              WITH KEY FKNUM = WA_VTFA-VBELN.

            IF SY-SUBRC IS INITIAL.
              READ TABLE T_KONV INTO WA_KONV
                WITH KEY KNUMV = WA_VFKP-KNUMV.

              READ TABLE T_KONV INTO WA_KONV
                 WITH KEY KNUMV  = WA_VFKP-KNUMV.

              IF  WA_KONV-KSCHL EQ 'ZFRE'
               OR WA_KONV-KSCHL EQ 'ZSEG'
               OR WA_KONV-KSCHL EQ 'ZIOF'
               OR WA_KONV-KSCHL EQ 'ZPED'.

                WL_TOTAL_FRETE = WL_TOTAL_FRETE + WA_KONV-KWERT.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF WA_VBAP-WAERK EQ 'BRL'.
        READ TABLE T_0077 INTO WA_0077 INDEX 1.
        TRY .
            WA_SAIDA-E_VLR_UNIT_VDA = WA_KONV-KBETR / WA_0077-UKURS.
            WA_SAIDA-R_VLR_UNIT_VDA = WA_KONV-KBETR / WA_0077-UKURS.
          CATCH CX_SY_ZERODIVIDE.

        ENDTRY.
      ELSE.
        WA_SAIDA-E_VLR_UNIT_VDA = WA_KONV-KBETR.
        WA_SAIDA-R_VLR_UNIT_VDA = WA_KONV-KBETR.
      ENDIF.


      WA_SAIDA-E_VLR_TOTAL_VENDA = WA_KONV-KWERT."Smartforms
      WA_SAIDA-E_MARGEM_TO = WA_SAIDA-E_VLR_UNIT_VDA - WA_SAIDA-E_CUSTO_MAT - WA_SAIDA-VLR_FRETE."Smartforms


      TRY .
          WA_SAIDA-E_VLR_TOTAL_COMPRA = ( ( WA_VBAP-KWMENG * WA_EKPO-NETPR ) / WA_EKPO-BPUMN ). "Smartforms
          WA_SAIDA-E_MARGEM_TOTAL     = ( ( WA_SAIDA-E_MARGEM_TO * WA_VBAP-KWMENG ) / WA_T006-ZAEHL ). "Smartforms
          WA_SAIDA-R_FRETE_REAL       = ( ( WL_TOTAL_FRETE / WL_TOTAL_FAT ) / WA_T006-ZAEHL ).
          WA_SAIDA-R_VLR_TOTAL_COMPRA = ( ( WA_VBAP-KWMENG * WA_EKPO-NETPR ) / WA_EKPO-BPUMN ).
          WA_SAIDA-R_VLR_TOTAL_VENDA  = ( ( WL_TOTAL_FAT * WA_SAIDA-E_VLR_UNIT_VDA ) / WA_T006-ZAEHL ).
          WA_SAIDA-R_TOTAL_FRETE_REAL = (   WL_TOTAL_FRETE / WA_T006-ZAEHL ). "Smartforms

        CATCH CX_SY_ZERODIVIDE.
        CATCH CX_SY_ARITHMETIC_OVERFLOW.
      ENDTRY.

      WA_SAIDA-R_QTE_FATURADO    = WL_TOTAL_FAT.
      WA_SAIDA-R_CUSTO_MATERIAL  = WA_EKPO-NETPR.
      WA_SAIDA-R_MARGEM_EFETIVA =  WA_SAIDA-R_VLR_UNIT_VDA - WA_SAIDA-R_CUSTO_MATERIAL - WA_SAIDA-R_FRETE_REAL . "Smartforms

      TRY .
          WA_SAIDA-R_MARGEM_TOTAL = ( ( WA_SAIDA-R_MARGEM_EFETIVA * WA_SAIDA-R_QTE_FATURADO ) / WA_T006-ZAEHL )."Smartforms
        CATCH CX_SY_ZERODIVIDE.
        CATCH CX_SY_ARITHMETIC_OVERFLOW.

      ENDTRY.

      WA_SAIDA-VLR_FRETE         = WA_0037-VLR_FRETE.
      WA_SAIDA-KUNNR             = WA_VBAK-KUNNR.
      WA_SAIDA-VKBUR             = WA_VBAK-VKBUR.
      WA_SAIDA-VBELN             = WA_VBAP-VBELN.
      WA_SAIDA-POSNR             = WA_VBAP-POSNR.
      WA_SAIDA-MATNR             = WA_VBAP-MATNR.
      WA_SAIDA-ARKTX             = WA_VBAP-ARKTX.
      WA_SAIDA-KWMENG            = WA_VBAP-KWMENG."Smartforms
      WA_SAIDA-WAERK             = WA_VBAP-WAERK.
      WA_SAIDA-WRKST             = WA_MARA-WRKST. "Smartforms
      WA_SAIDA-NAME1             = WA_KNA1-NAME1.

***** SMARTFORMS ZFIR0005 **********************************************************************"&
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'                                               "&
        EXPORTING                                                                                "&
          INPUT         = WA_VBAP-MATNR                                                          "&
       IMPORTING                                                                                 "&
         OUTPUT        = WA_VBAP-MATNR.                                                          "&
      "&
      CONCATENATE WA_VBAP-MATNR '-' WA_VBAP-ARKTX INTO WA_SAIDA-MATERIAL SEPARATED BY SPACE.    "&
      WA_SAIDA-KWERT = WA_KONV-KWERT.                                                           "&
************************************************************************************************"&
      MOVE-CORRESPONDING WA_SAIDA TO WA_SMARTFORMS.
      APPEND WA_SMARTFORMS TO T_SMARTFORMS.
      APPEND WA_SAIDA TO T_SAIDA.
    ENDIF.
    CLEAR: WL_TOTAL_FAT, WL_TOTAL_FRETE, WA_SAIDA, WA_VBAK, WA_KNA1, WA_KONV, WA_T006,
           WA_MARA, WA_EKPO, WA_0078, WA_0037, WA_VBFA, WA_VTFA, WA_VFKP.
  ENDLOOP.


ENDFORM.                    " ORGANIZAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT          TYPE SLIS_LAYOUT_ALV.
  DATA: VL_NAME            TYPE RS38L_FNAM,
        VL_FORMNAME        TYPE TDSFNAME,
        LS_CONTROL         TYPE SSFCTRLOP,
        LS_OPTIONS         TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO    TYPE SSFCRESCL,
        JOB_OUTPUT_OPTIONS TYPE SSFCRESOP,
        WL_DIALOG.
  DATA: "st_job_output_info       TYPE ssfcrescl,
        ST_DOCUMENT_OUTPUT_INFO  TYPE SSFCRESPD,
        "st_job_output_options    TYPE ssfcresop,
        V_BIN_FILESIZE           TYPE I,
        ST_JOB_OUTPUT_INFO       TYPE SSFCRESCL,
        V_GUIOBJ                 TYPE REF TO CL_GUI_FRONTEND_SERVICES,
        V_FILTER                 TYPE STRING,
        V_PATH                   TYPE STRING,
        V_FULLPATH               TYPE STRING,
        V_UACT                   TYPE I,
        V_LANGUAGE               TYPE SY-LANGU,
        V_E_DEVTYPE              TYPE RSPOPTYPE,
        V_FILENAME               TYPE STRING,
        V_NAME                   TYPE STRING.
  DATA: IT_OTF      TYPE STANDARD TABLE OF ITCOO,
        IT_DOCS     TYPE STANDARD TABLE OF DOCS,
        IT_LINES    TYPE STANDARD TABLE OF TLINE.

  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.

  IF P_ALV IS NOT INITIAL.
    WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
       I_CALLBACK_PROGRAM                = V_REPORT
*    I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
       IT_FIELDCAT                       = ESTRUTURA[]
       IS_LAYOUT                         = WL_LAYOUT
       I_SAVE                            = 'A'
       IT_EVENTS                         = EVENTS
       IS_PRINT                          = T_PRINT
      TABLES
        T_OUTTAB                          = T_SAIDA.

  ELSEIF P_PDF IS NOT  INITIAL.
    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
      EXPORTING
        I_LANGUAGE    = SY-LANGU
        I_APPLICATION = 'SAPDEFAULT'
      IMPORTING
        E_DEVTYPE     = V_E_DEVTYPE.

    JOB_OUTPUT_OPTIONS-TDPRINTER = V_E_DEVTYPE.
    LS_CONTROL-NO_DIALOG = 'X'.
    LS_CONTROL-GETOTF = 'X'.

    VL_FORMNAME = 'ZFIR0005'.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = VL_FORMNAME
      IMPORTING
        FM_NAME            = VL_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.


    LS_OPTIONS-TDDEST   = 'LOCL'.
*    LS_CONTROL-NO_OPEN = 'X'.
    CALL FUNCTION VL_NAME
      EXPORTING
        USER_SETTINGS      = ' '
        CONTROL_PARAMETERS = LS_CONTROL
        OUTPUT_OPTIONS     = LS_OPTIONS
        VKORG              = S_VKORG-LOW
        SPART              = S_SPART-LOW
        VKBUR              = S_VKBUR-LOW
        VKGRP              = S_VKGRP-LOW
        AUART              = S_AUART-LOW
        ERDAT              = S_ERDAT-LOW
      IMPORTING
        JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
        JOB_OUTPUT_OPTIONS = JOB_OUTPUT_OPTIONS
      TABLES
        T_SMARTFORMS       = T_SMARTFORMS
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

*.........................CONVERT TO OTF TO PDF.......................*
    CALL FUNCTION 'CONVERT_OTF_2_PDF'
      IMPORTING
        BIN_FILESIZE           = V_BIN_FILESIZE
      TABLES
        OTF                    = JOB_OUTPUT_INFO-OTFDATA
        DOCTAB_ARCHIVE         = IT_DOCS
        LINES                  = IT_LINES
      EXCEPTIONS
        ERR_CONV_NOT_POSSIBLE  = 1
        ERR_OTF_MC_NOENDMARKER = 2
        OTHERS                 = 3.

*........................GET THE FILE NAME TO STORE....................*
    CONCATENATE P_FILE '.pdf' INTO V_NAME.
*    CREATE OBJECT V_GUIOBJ.
*    CALL METHOD V_GUIOBJ->FILE_SAVE_DIALOG
*      EXPORTING
*        DEFAULT_EXTENSION = 'pdf'
*        DEFAULT_FILE_NAME = V_NAME
*        FILE_FILTER       = V_FILTER
*      CHANGING
*        FILENAME          = V_NAME
*        PATH              = V_PATH
*        FULLPATH          = V_FULLPATH
*        USER_ACTION       = V_UACT.
*    IF V_UACT = V_GUIOBJ->ACTION_CANCEL.
*      EXIT.
*    ENDIF.

*..................................DOWNLOAD AS FILE....................*

    MOVE P_FILE TO V_FILENAME.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        BIN_FILESIZE            = V_BIN_FILESIZE
        FILENAME                = V_FILENAME
        FILETYPE                = 'BIN'
      TABLES
        DATA_TAB                = IT_LINES
      EXCEPTIONS
        FILE_WRITE_ERROR        = 1
        NO_BATCH                = 2
        GUI_REFUSE_FILETRANSFER = 3
        INVALID_TYPE            = 4
        NO_AUTHORITY            = 5
        UNKNOWN_ERROR           = 6
        HEADER_NOT_ALLOWED      = 7
        SEPARATOR_NOT_ALLOWED   = 8
        FILESIZE_NOT_ALLOWED    = 9
        HEADER_TOO_LONG         = 10
        DP_ERROR_CREATE         = 11
        DP_ERROR_SEND           = 12
        DP_ERROR_WRITE          = 13
        UNKNOWN_DP_ERROR        = 14
        ACCESS_DENIED           = 15
        DP_OUT_OF_MEMORY        = 16
        DISK_FULL               = 17
        DP_TIMEOUT              = 18
        FILE_NOT_FOUND          = 19
        DATAPROVIDER_EXCEPTION  = 20
        CONTROL_FLUSH_ERROR     = 21
        OTHERS                  = 22.
  ENDIF.
ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.

  PERFORM F_CARREGAR_EVENTOS USING:
* para tira duplo click          SLIS_EV_USER_COMMAND 'XUSER_COMMAND',
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.
  PERFORM MONTAR_ESTRUTURA USING:
        1  'VBAK'             'KUNNR'         'T_SAIDA' 'KUNNR'                     'Cliente'                 ' ' ,
        2  'KNA1'             'NAME1'         'T_SAIDA' 'NAME1'                     'Nome do Cliente'         ' ' ,
        3  'VBAK'             'VKBUR'         'T_SAIDA' 'VKBUR'                     ' '                       ' ' ,
        4  'VBAP'             'VBELN'         'T_SAIDA' 'VBELN'                     'Contrato'                ' ' ,
        5  'VBAP'             'POSNR'         'T_SAIDA' 'POSNR'                     ' '                       ' ' ,
        6  'VBAP'             'MATNR'         'T_SAIDA' 'MATNR'                     ' '                       ' ' ,
        7  'VBAP'             'ARKTX'         'T_SAIDA' 'ARKTX'                     'Descrição Material'      ' ' ,
        8  'MARA'             'WRKST'         'T_SAIDA' 'WRKST'                     'Marca'                   ' ' ,
        9  'VBAP'             'KWMEN'         'T_SAIDA' 'KWMENG'                    'Qte.Contrato'            ' ' ,
       10  'EKPO'             'NETPR'         'T_SAIDA' 'E_CUSTO_MAT'               'ECusto Material'         ' ' ,
       11  'ZSDT0037'         'VLR_FRETE'     'T_SAIDA' 'VLR_FRETE'                 'EFrete Estimado'         ' ' ,
       12  'VBAP'             'WAERK'         'T_SAIDA' 'WAERK'                     'Moeda'                   ' ' ,
       13  'KONV'             'KBETR'         'T_SAIDA' 'E_VLR_UNIT_VDA'            'EVlr.Unit.Vda.'          ' ' ,
       14  'EKPO'             'BPUMN'         'T_SAIDA' 'E_VLR_TOTAL_COMPRA'        'EVlr.Total Compra'       ' ' ,
       15  'KONV'             'KWERT'         'T_SAIDA' 'E_VLR_TOTAL_VENDA'         'EVlr.Total Venda'        ' ' ,
       16  'T006'             'ZAEHL'         'T_SAIDA' 'E_TOTAL_FRETE'             'ETotal Frete	'           ' ' ,
       17  'T006'             'ZAEHL'         'T_SAIDA' 'E_MARGEM_TO'               'EMargem TO'              ' ' ,
       18  'T006'             'ZAEHL'         'T_SAIDA' 'E_MARGEM_TOTAL'            'EMargem Total'           ' ' ,
       19  'EKPO'             'NETPR'         'T_SAIDA' 'R_CUSTO_MATERIAL'          'RCusto Material'         ' ' ,
       20  'VBFA'             'RFMNG'         'T_SAIDA' 'R_QTE_FATURADO'            'RQte.Faturado'           ' ' ,
       21  'KONV'             'KWERT'         'T_SAIDA' 'R_FRETE_REAL'              'RFrete Real'             ' ' ,
       22  'KONV'             'KBETR'         'T_SAIDA' 'R_VLR_UNIT_VDA'            'RVlr.Unit.Vda.'          ' ' ,
       23  'EKPO'             'BPUMN'         'T_SAIDA' 'R_VLR_TOTAL_COMPRA'        'RVlr.Total Compra'       ' ' ,
       24  'T006'             'ZAEHL'         'T_SAIDA' 'R_VLR_TOTAL_VENDA'         'RVlr.Total Venda'        ' ' ,
       25  'T006'             'ZAEHL'         'T_SAIDA' 'R_TOTAL_FRETE_REAL'        'RTotal Frete Real'       ' ' ,
       26  'EKPO'             'NETPR'         'T_SAIDA' 'R_MARGEM_EFETIVA'          'RMargem Efetiva'         ' ' ,
       27  'T006'             'ZAEHL'         'T_SAIDA' 'R_MARGEM_TOTAL'            'RMargem Total'           ' ' .



ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR WA_ESTRUTURA.

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.

  V_REPORT = SY-REPID.

  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.

ENDFORM.                    " INICIAR_VARIAVES


*&---------------------------------------------------------------------*
*&      Form  F_ABRE_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ABRE_ARQUIVO.
  DATA: V_GUIOBJ                 TYPE REF TO CL_GUI_FRONTEND_SERVICES,
        V_FILTER                 TYPE STRING,
        V_PATH                   TYPE STRING,
        V_FULLPATH               TYPE STRING,
        V_UACT                   TYPE I,
        V_NAME                   TYPE STRING.

  CREATE OBJECT V_GUIOBJ.
  CALL METHOD V_GUIOBJ->FILE_SAVE_DIALOG
    EXPORTING
      DEFAULT_EXTENSION = 'pdf'
      DEFAULT_FILE_NAME = V_NAME
      FILE_FILTER       = V_FILTER
    CHANGING
      FILENAME          = V_NAME
      PATH              = V_PATH
      FULLPATH          = V_FULLPATH
      USER_ACTION       = V_UACT.
  IF V_UACT = V_GUIOBJ->ACTION_CANCEL.
    EXIT.
  ENDIF.
  CONCATENATE V_PATH V_NAME INTO P_FILE.


ENDFORM.                    " F_ABRE_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFICA_TELA .
  LOOP AT SCREEN.

    IF P_ALV = 'X'.

*SCREEN-GROUP1 = 'C2'.
      IF SCREEN-GROUP1 = 'C2'.
        SCREEN-INVISIBLE = 1.
        SCREEN-INPUT     = 0.
        SCREEN-ACTIVE    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF SCREEN-GROUP1 = 'C1'.

        SCREEN-INVISIBLE = 0.
        SCREEN-INPUT     = 1.
        SCREEN-ACTIVE    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ELSE.

      IF SCREEN-GROUP1 = 'C1'.
        SCREEN-INVISIBLE = 1.
        SCREEN-INPUT     = 0.
        SCREEN-ACTIVE    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF SCREEN-GROUP1 = 'C2'.
        SCREEN-INVISIBLE = 0.
        SCREEN-INPUT     = 1.
        SCREEN-ACTIVE    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFICA_TELA
