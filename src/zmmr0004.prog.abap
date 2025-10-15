*&--------------------------------------------------------------------&*
*& Report Name    : Consulta Geral de Saldos                          *&
*& Author         : Victor Hugo                                       *&
*& Date           : 08.05.2012                                        *&
*& Funcional Area : MM                                                *&
*&--------------------------------------------------------------------&*
*----------------------------------------------------------------------*
* Autor      : Igor Sobral                            Data: 21/06/2013 *
* Observações: Busca de Safra                                          *
*----------------------------------------------------------------------*
REPORT  ZMMR0004  MESSAGE-ID SABAPDOCU.

*&--------------------------------------------------------------------&*
*& TABLES
*&--------------------------------------------------------------------&*
TABLES: T001K, CHVW, MARA, VBAP, VBAK, AUSP.

TYPE-POOLS: SLIS.
INCLUDE <ICON>.
*&--------------------------------------------------------------------&*
*& Fields
*&--------------------------------------------------------------------&*
DATA: GT_FIELDCAT   TYPE LVC_T_FCAT,
      GT_SORT       TYPE LVC_T_SORT,
      GS_LAYOUT     TYPE LVC_S_LAYO,
      GS_TOOLBAR    TYPE STB_BUTTON,

      GT_HEADER     TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
      G_CONTAINER   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GRID          TYPE REF TO CL_GUI_ALV_GRID,

      WA_LAYOUT     TYPE LVC_S_LAYO,
      WA_VARIANT    TYPE DISVARIANT.

*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*
TYPES:  BEGIN OF TY_VBAK,
          VBELN         TYPE VBAK-VBELN,
          VKORG         TYPE VBAK-VKORG,
          VTWEG         TYPE VBAK-VTWEG,
          SPART         TYPE VBAK-SPART,
        END OF TY_VBAK,

        BEGIN OF TY_T001K,
          BUKRS         TYPE T001K-BUKRS,
          BWKEY         TYPE T001K-BWKEY,
        END OF TY_T001K,

        BEGIN OF TY_VBAP,
          VBELN         TYPE VBAP-VBELN,
          POSNR         TYPE VBAP-POSNR,
          WERKS         TYPE VBAP-WERKS,
          MATNR         TYPE VBAP-MATNR,
          MATKL         TYPE VBAP-MATKL,
          CHARG         TYPE VBAP-CHARG,
          BUKRS         TYPE T001K-BUKRS,
        END OF TY_VBAP,

        BEGIN OF TY_VBRP,
          VBELN         TYPE VBRP-VBELN,
          POSNR         TYPE VBRP-POSNR,
          MATNR         TYPE VBRP-MATNR,
          AUBEL         TYPE VBRP-AUBEL,
          SHKZG         TYPE VBRP-SHKZG,
          FKIMG         TYPE VBRP-FKIMG,
          VOLUM         TYPE VBRP-VOLUM,
        END OF TY_VBRP,

        BEGIN OF TY_VBRK,
          VBELN         TYPE VBRK-VBELN,
          FKSTO         TYPE VBRK-FKSTO,
          FKTYP         TYPE VBRK-FKTYP,
          VBTYP         TYPE VBRK-VBTYP,
          VKORG         TYPE VBRK-VKORG,
          VTWEG         TYPE VBRK-VTWEG,
        END OF TY_VBRK,

        BEGIN OF TY_MARA,
          MATNR         TYPE MARA-MATNR,
          MATKL         TYPE MARA-MATKL,
          NORMT         TYPE MARA-NORMT,
        END OF TY_MARA,

        BEGIN OF TY_VENDA,
          VBELN         TYPE VBAK-VBELN,
          VKORG         TYPE VBAK-VKORG,
          VTWEG         TYPE VBAK-VTWEG,
          SPART         TYPE VBAK-SPART,
          POSNR         TYPE VBAP-POSNR,
          MATNR         TYPE VBAP-MATNR,
          MATKL         TYPE VBAP-MATKL,
          CHARG         TYPE VBAP-CHARG,
          AUBEL         TYPE VBRP-AUBEL,
          SHKZG         TYPE VBRP-SHKZG,
          FKIMG         TYPE VBRP-FKIMG,
          VOLUM         TYPE VBRP-VOLUM,
          FKSTO         TYPE VBRK-FKSTO,
          VBELN_VBRK    TYPE VBRK-VBELN,
          BUKRS         TYPE T001K-BUKRS,
          WERKS         TYPE VBAP-WERKS,
          VBTYP         TYPE VBRK-VBTYP,
          FKIMG_SOMA    TYPE VBRP-FKIMG,
          VOLUM_SOMA    TYPE VBRP-VOLUM,
          NORMT         TYPE MARA-NORMT,
        END OF TY_VENDA,

        BEGIN OF TY_VENDAS,
          MATNR         TYPE VBAP-MATNR,
          WERKS         TYPE VBAP-WERKS,
          CHARG         TYPE VBAP-CHARG,
          FKIMG         TYPE VBRP-FKIMG,
          VOLUM         TYPE VBRP-VOLUM,
          NORMT         TYPE MARA-NORMT,
        END OF TY_VENDAS,

  " Produção
        BEGIN OF TY_CHVW,
          MATNR         TYPE CHVW-MATNR,
          WERKS         TYPE CHVW-WERKS,
          CHARG         TYPE CHVW-CHARG,
          MENGE         TYPE CHVW-MENGE,
          XZUGA         TYPE CHVW-XZUGA,
          BWART         TYPE CHVW-BWART,
          SHKZG         TYPE CHVW-SHKZG,
          MJAHR         TYPE CHVW-MJAHR,
          BUDAT         TYPE CHVW-BUDAT,
          MENGE_SOMA    TYPE CHVW-MENGE,
          DEL(1),
        END OF TY_CHVW,

        BEGIN OF TY_MCH1,
          MATNR         TYPE MCH1-MATNR,
          CUOBJ_BM      TYPE MCH1-CUOBJ_BM,
          CHARG         TYPE MCH1-CHARG,
          CUOBJ         TYPE AUSP-OBJEK,
        END OF TY_MCH1,

        BEGIN OF TY_AUSP,
          OBJEK         TYPE AUSP-OBJEK,
          KLART         TYPE AUSP-KLART,
          ATFLV         TYPE AUSP-ATFLV,
          ATINN         TYPE AUSP-ATINN,
          ATWRT         TYPE AUSP-ATWRT,
        END OF TY_AUSP,

        BEGIN OF TY_ZMMT0008,
         VBELN_VF       TYPE ZMMT0008-VBELN_VF,
         CHARG          TYPE ZMMT0008-CHARG,
        END OF TY_ZMMT0008,

        BEGIN OF TY_PRODUZIDO,
          MATNR         TYPE CHVW-MATNR,
          NORMT         TYPE MARA-NORMT,
          WERKS         TYPE CHVW-WERKS,
          MENGE         TYPE CHVW-MENGE,
          SHKZG         TYPE CHVW-SHKZG,
          CHARG         TYPE CHVW-CHARG,
          BWART         TYPE CHVW-BWART,
          SAFRA         TYPE NUMC4,
        END OF TY_PRODUZIDO,

      " MM - Compra - Reserva
         BEGIN OF TY_MCHB,
           MATNR        TYPE MCHB-MATNR,
           WERKS        TYPE MCHB-WERKS,
           CHARG        TYPE MCHB-CHARG,
           LGORT        TYPE MCHB-LGORT,
           CSPEM        TYPE MCHB-CSPEM,
           SAFRA        TYPE NUMC4,
           QTD          TYPE MCHB-CSPEM,
        END OF TY_MCHB,

        BEGIN OF TY_RESERVA,
          MATNR         TYPE MCHB-MATNR,
          WERKS         TYPE MCHB-WERKS,
          CSPEM         TYPE MCHB-CSPEM,
          SAFRA         TYPE NUMC4,
          QTD_FARDO     TYPE MCHB-CSPEM,
        END OF TY_RESERVA,

        BEGIN OF TY_PRINCIPAL,
          MATNR         TYPE MCHB-MATNR,
          WERKS         TYPE MCHB-WERKS,
          FKIMG         TYPE VBRP-FKIMG,
          VOLUM         TYPE VBRP-VOLUM,
          MENGE         TYPE CHVW-MENGE,
          CSPEM         TYPE MCHB-CSPEM,
          NORMT         TYPE MARA-NORMT,
          QTD_FARDO     TYPE MCHB-CSPEM,
          QTD_PRODUZIDO TYPE CHVW-MENGE,
        END OF TY_PRINCIPAL,

        BEGIN OF TY_DISPLAY,
          DESC          TYPE TEXT_35,
          DADOS         TYPE TEXT_35,
          MATNR         TYPE MCHB-MATNR,
          NORMT         TYPE MARA-NORMT,
          "Display em Fardos
          VOLUM_FA      TYPE VBRP-VOLUM,
          CSPEM_FA      TYPE MCHB-CSPEM,
          SALDO_FA      TYPE MCHB-CSPEM,
          MENGE_FA      TYPE CHVW-MENGE,
          " Display Produção KG
          MENGE_KG      TYPE CHVW-MENGE,
          FKIMG_KG      TYPE VBRP-FKIMG,
          CSPEM_KG      TYPE MCHB-CSPEM,
          SALDO_KG      TYPE MCHB-CSPEM,
          VOLUM         TYPE VBRP-VOLUM,
        END OF TY_DISPLAY,

        BEGIN OF TY_ZTSAFRAFARDOS,
          MANDT         TYPE ZTSAFRAFARDOS-MANDT,
          CHARG         TYPE ZTSAFRAFARDOS-CHARG,
          DATA_INICIO   TYPE ZTSAFRAFARDOS-DATA_INICIO,
          DATA_FIM      TYPE ZTSAFRAFARDOS-DATA_FIM,
          STATUS        TYPE ZTSAFRAFARDOS-STATUS,
        END OF TY_ZTSAFRAFARDOS,

        BEGIN OF TY_DESCRICAO,
          DATA          TYPE CHAR10,
          BUKRS         TYPE T001K-BUKRS,
          BUTXT         TYPE T001-BUTXT,
          WERKS         TYPE CHVW-WERKS,
          NAME1         TYPE T001W-NAME1,
          CHARG         TYPE VBAP-CHARG,
        END OF TY_DESCRICAO.

*&--------------------------------------------------------------------&*
*& Internal Table
*&--------------------------------------------------------------------&*
DATA: IT_VBAK           TYPE TABLE OF TY_VBAK,
      IT_T001K          TYPE TABLE OF TY_T001K,
      IT_VBAP           TYPE TABLE OF TY_VBAP,
      IT_VBAP_AUX       TYPE TABLE OF TY_VBAP,
      IT_VBRP           TYPE TABLE OF TY_VBRP,
      IT_VBRK           TYPE TABLE OF TY_VBRK,
      IT_MARA           TYPE TABLE OF TY_MARA,
      IT_MCHB           TYPE TABLE OF TY_MCHB,
      IT_ZMMT0008       TYPE TABLE OF TY_ZMMT0008,

      " Venda
      IT_VENDA          TYPE TABLE OF TY_VENDA,
      IT_VENDAS         TYPE TABLE OF TY_VENDAS,
      IT_VENDA_SOMA     TYPE TABLE OF TY_VENDA,

      " Produção
      IT_CHVW           TYPE TABLE OF TY_CHVW,
      IT_CHVW_AUX       TYPE TABLE OF TY_CHVW,
      IT_MCH1           TYPE TABLE OF TY_MCH1,
      IT_MCH1_AUX       TYPE TABLE OF TY_MCH1,
      IT_AUSP           TYPE TABLE OF TY_AUSP,
      IT_PRODUZIDO      TYPE TABLE OF TY_PRODUZIDO,
      IT_PRODUZIDO_AUX  TYPE TABLE OF TY_PRODUZIDO,

      " Reserva
       IT_RESERVA       TYPE TABLE OF TY_RESERVA,
       IT_RESERVA_AUX   TYPE TABLE OF TY_RESERVA,
       IT_RESERVA_CONT  TYPE TABLE OF TY_RESERVA,

       " Principal
       IT_PRINCIPAL     TYPE TABLE OF TY_PRINCIPAL,
       IT_PRINCIPAL_AUX TYPE TABLE OF TY_PRINCIPAL,
       IT_DISPLAY       TYPE TABLE OF TY_DISPLAY,
       IT_DISPLAY_TREE  TYPE TABLE OF TY_DISPLAY,
       IT_ZTSAFRAFARDOS TYPE TABLE OF TY_ZTSAFRAFARDOS.

*&--------------------------------------------------------------------&*
*& Work Area
*&--------------------------------------------------------------------&*
DATA: WA_VBAK           TYPE TY_VBAK,
      WA_T001K          TYPE TY_T001K,
      WA_VBAP           TYPE TY_VBAP,
      WA_VBAP_AUX       TYPE TY_VBAP,
      WA_VBRP           TYPE TY_VBRP,
      WA_VBRK           TYPE TY_VBRK,
      WA_MARA           TYPE TY_MARA,
      WA_MCHB           TYPE TY_MCHB,

      " Venda
      WA_VENDA          TYPE TY_VENDA,
      WA_VENDAS         TYPE TY_VENDAS,
      WA_VENDA_SOMA     TYPE TY_VENDA,
      WA_ZMMT0008       TYPE TY_ZMMT0008,

      " Produção
      WA_CHVW           TYPE TY_CHVW,
      WA_CHVW_AUX       TYPE TY_CHVW,
      WA_MCH1           TYPE TY_MCH1,
      WA_MCH1_AUX       TYPE TY_MCH1,
      WA_AUSP           TYPE TY_AUSP,
      WA_PRODUZIDO      TYPE TY_PRODUZIDO,
      WA_PRODUZIDO_AUX  TYPE TY_PRODUZIDO,

      "Reserva
      WA_RESERVA        TYPE TY_RESERVA,
      WA_RESERVA_AUX    TYPE TY_RESERVA,
      WA_RESERVA_CONT   TYPE TY_RESERVA,

      " Principal
      WA_PRINCIPAL      TYPE TY_PRINCIPAL,
      WA_PRINCIPAL_AUX  TYPE TY_PRINCIPAL,
      WA_DISPLAY        TYPE TY_DISPLAY,
      WA_DISPLAY_TREE   TYPE TY_DISPLAY,
      WA_MARA_TIPO      TYPE MARA,
      WA_DESCRICAO      TYPE TY_DESCRICAO,
      WA_ZTSAFRAFARDOS  TYPE TY_ZTSAFRAFARDOS.

*&--------------------------------------------------------------------&*
*& Tabelas de Caracteristicas
*&--------------------------------------------------------------------&*
DATA: IT_NUM   TYPE TABLE OF BAPI1003_ALLOC_VALUES_NUM,
      WA_NUM   TYPE          BAPI1003_ALLOC_VALUES_NUM,
      IT_CHAR  TYPE TABLE OF BAPI1003_ALLOC_VALUES_CHAR,
      IT_CURR  TYPE TABLE OF BAPI1003_ALLOC_VALUES_CURR,
      IT_RET   TYPE TABLE OF BAPIRET2.

*&--------------------------------------------------------------------&*
*& Variables
*&--------------------------------------------------------------------&*
DATA: ATINN TYPE CABN-ATINN,
*      safra TYPE ausp-atflv.
      SAFRA TYPE AUSP-ATWRT.

*&--------------------------------------------------------------------&*
*& Parameters
*&--------------------------------------------------------------------&*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS  FOR T001K-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION, " Empresa
                P_WERKS  FOR CHVW-WERKS OBLIGATORY NO INTERVALS NO-EXTENSION,  " Centro
                P_MATNR  FOR CHVW-MATNR,  " Nº do material
                P_MATKL  FOR MARA-MATKL NO INTERVALS NO-EXTENSION,  " Grupo de mercadorias
                P_CHARG  FOR VBAP-CHARG OBLIGATORY NO INTERVALS NO-EXTENSION.  " Safra
" p_mjahr  for chvw-mjahr obligatory no-extension,  " Ano Periodo
*                P_VKORG  FOR VBAK-VKORG OBLIGATORY NO INTERVALS NO-EXTENSION,  " Organização de vendas
*                P_VTWEG  FOR VBAK-VTWEG OBLIGATORY NO INTERVALS NO-EXTENSION,  " Canal de Distribuição
*                P_SPART  FOR VBAK-SPART OBLIGATORY NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK B1.

*&--------------------------------------------------------------------&*
*& Start of Selection
*&--------------------------------------------------------------------&*
START-OF-SELECTION.
  PERFORM: SELECIONA_DADOS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS.
  IF ( P_MATNR IS INITIAL ) AND ( P_MATKL IS INITIAL ).
    MESSAGE I000(Z01) WITH  'Obrigatorio Material ou Grupo de Mercadoria'.
    STOP.
  ELSE.
    PERFORM: VENDA,
             PRODUZIDO,
             RESERVA,
             AGRUPAMENTO,
             CALL_SCREEN.
  ENDIF.
ENDFORM.                    " SELECIONA_DADOS_VENDA

*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN .
  CONSTANTS:  TELA_01(4) TYPE C VALUE '0100'.
  CALL SCREEN TELA_01.
ENDFORM.                    " CALL_SCREEN

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.

  IF ( G_CONTAINER IS INITIAL ).
    PERFORM: CREATE_OBJECT.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0100.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  VENDAS
*&---------------------------------------------------------------------*
FORM VENDA.
  " Dados gerais de material
  SELECT MATNR MATKL NORMT
  FROM MARA
    INTO TABLE IT_MARA
  WHERE MATNR IN P_MATNR
    AND MATKL IN P_MATKL.

  " Documento de vendas: dados de cabeçalho
*  SELECT VBELN VKORG VTWEG SPART
*  FROM VBAK
*    INTO TABLE IT_VBAK
*  WHERE VKORG IN P_VKORG
*    AND VTWEG IN P_VTWEG
*    AND SPART IN P_SPART.

*  CHECK NOT IT_VBAK[] IS INITIAL.

  SELECT BUKRS BWKEY
  FROM T001K
    INTO TABLE IT_T001K
  WHERE BUKRS IN P_BUKRS
    AND BWKEY IN P_WERKS.

  CHECK NOT IT_T001K[] IS INITIAL.

  SELECT VBELN POSNR WERKS MATNR MATKL CHARG
  FROM VBAP
    INTO TABLE IT_VBAP_AUX
*    FOR ALL ENTRIES IN IT_VBAK
  WHERE WERKS IN P_WERKS
    AND MATNR IN P_MATNR
    AND MATKL IN P_MATKL
    AND CHARG IN P_CHARG.

  SORT: IT_T001K BY BWKEY.

  LOOP AT IT_VBAP_AUX INTO WA_VBAP_AUX.
    READ TABLE IT_T001K INTO WA_T001K WITH KEY BWKEY = WA_VBAP_AUX-WERKS BINARY SEARCH.
    IF ( SY-SUBRC EQ 0 ).
      WA_VBAP_AUX-BUKRS = WA_T001K-BUKRS.
      APPEND WA_VBAP_AUX TO IT_VBAP.
    ENDIF.

    CLEAR: WA_VBAP_AUX.
  ENDLOOP.

  CHECK NOT IT_VBAP[] IS INITIAL.

  SELECT VBELN POSNR MATNR AUBEL SHKZG FKIMG VOLUM
  FROM VBRP
    INTO TABLE IT_VBRP
    FOR ALL ENTRIES IN IT_VBAP
  WHERE AUBEL EQ IT_VBAP-VBELN
    AND WERKS EQ IT_VBAP-WERKS
    AND POSNR EQ IT_VBAP-POSNR
    AND MATNR EQ IT_VBAP-MATNR
    AND CHARG EQ IT_VBAP-CHARG AND DRAFT = SPACE .
*    AND SHKZG NE 'X'.

  CHECK NOT IT_VBRP[] IS INITIAL.

  SELECT VBELN_VF CHARG
  FROM ZMMT0008
    INTO TABLE IT_ZMMT0008
    FOR ALL ENTRIES IN IT_VBRP
  WHERE VBELN_VF = IT_VBRP-VBELN.

  CHECK NOT IT_ZMMT0008[] IS INITIAL.

  SELECT VBELN FKSTO FKTYP VBTYP VKORG VTWEG
  FROM VBRK
    INTO TABLE IT_VBRK
    FOR ALL ENTRIES IN IT_VBRP
  WHERE VBELN EQ IT_VBRP-VBELN
    AND FKSTO NE 'X'
    AND FKTYP EQ 'L'
    AND VBTYP IN ('M','O').

  SORT: IT_VBAK BY VBELN,
        IT_VBAP BY VBELN,
        IT_VBRP BY AUBEL,
        IT_VBRK BY VBELN.

*---> 05/07/2023 - Migração S4 - DL
    SORT IT_MARA BY MATNR.
*<--- 05/07/2023 - Migração S4 - DL

  LOOP AT IT_VBAP INTO WA_VBAP.
    READ TABLE IT_MARA INTO WA_MARA WITH KEY MATNR = WA_VBAP-MATNR BINARY SEARCH.
    IF ( SY-SUBRC EQ 0 ).
      LOOP AT IT_VBRP INTO WA_VBRP WHERE AUBEL = WA_VBAP-VBELN.
        READ TABLE IT_VBRK INTO WA_VBRK WITH KEY VBELN = WA_VBRP-VBELN BINARY SEARCH.
        IF ( SY-SUBRC EQ 0 ).
          WA_VENDA-VBELN      = WA_VBAP-VBELN.
          WA_VENDA-MATKL      = WA_VBAP-MATKL.
          WA_VENDA-CHARG      = WA_VBAP-CHARG.
          WA_VENDA-MATNR      = WA_VBRP-MATNR.
          WA_VENDA-POSNR      = WA_VBRP-POSNR.
          WA_VENDA-AUBEL      = WA_VBRP-AUBEL.
          WA_VENDA-SHKZG      = WA_VBRP-SHKZG.
          WA_VENDA-FKIMG      = WA_VBRP-FKIMG.
          WA_VENDA-FKSTO      = WA_VBRK-FKSTO.

          LOOP AT IT_ZMMT0008 INTO WA_ZMMT0008 WHERE VBELN_VF EQ WA_VBRP-VBELN.
            ADD 1 TO WA_VENDA-VOLUM.
          ENDLOOP.

          WA_VENDA-VBELN_VBRK = WA_VBRK-VBELN.
          WA_VENDA-VBTYP      = WA_VBRK-VBTYP.

          SELECT SINGLE * FROM MARA INTO WA_MARA_TIPO WHERE MATNR EQ WA_VBRP-MATNR.
          IF ( SY-SUBRC EQ  0 ).
            WA_VENDA-NORMT = WA_MARA_TIPO-NORMT.
          ENDIF.

          READ TABLE IT_VBAP INTO WA_VBAP WITH KEY VBELN = WA_VBRP-AUBEL BINARY SEARCH.

          WA_VENDA-BUKRS = WA_VBAP-BUKRS.
          WA_VENDA-WERKS = WA_VBAP-WERKS.

          APPEND WA_VENDA TO IT_VENDA.

          CLEAR: WA_VENDA.
        ENDIF.

        CLEAR: WA_VBRP, WA_VBRK.
      ENDLOOP.

      IF NOT ( IT_VENDA[] IS INITIAL ).
        DELETE IT_VBAP WHERE VBELN EQ WA_VBAP-VBELN.
      ENDIF.
    ENDIF.

    CLEAR: WA_VBAP, WA_VBAK, WA_VBRP, WA_VBRK.
  ENDLOOP.

  CHECK NOT IT_VENDA[] IS INITIAL.

  SORT: IT_VENDA BY MATNR.

  IT_VENDA_SOMA[] = IT_VENDA[].

  DATA: TOTAL_FKIMG TYPE VBRP-FKIMG,
        TOTAL_VOLUM TYPE VBRP-VOLUM.

  CLEAR: WA_VENDA, WA_VENDA_SOMA.

  LOOP AT IT_VENDA INTO WA_VENDA WHERE FKIMG_SOMA IS INITIAL
                                    OR VOLUM_SOMA IS INITIAL.

    LOOP AT IT_VENDA_SOMA INTO WA_VENDA_SOMA WHERE MATNR EQ WA_VENDA-MATNR
                                               AND WERKS EQ WA_VENDA-WERKS.
      "AND ( FKIMG_SOMA IS INITIAL OR VOLUM_SOMA IS INITIAL ).

      IF WA_VENDA_SOMA-VBTYP = 'M'.
        TOTAL_FKIMG = TOTAL_FKIMG + WA_VENDA_SOMA-FKIMG.
      ELSE.
        TOTAL_FKIMG = TOTAL_FKIMG - WA_VENDA_SOMA-FKIMG.
      ENDIF.

      TOTAL_VOLUM = TOTAL_VOLUM + WA_VENDA_SOMA-VOLUM.

      CLEAR: WA_VENDA_SOMA.
    ENDLOOP.

    IF NOT ( TOTAL_FKIMG IS INITIAL ) OR NOT ( TOTAL_VOLUM IS INITIAL ).
      WA_VENDA-FKIMG_SOMA = TOTAL_FKIMG.
      WA_VENDA-VOLUM_SOMA = TOTAL_VOLUM.

      APPEND WA_VENDA TO IT_VENDA.

      DELETE IT_VENDA_SOMA WHERE MATNR = WA_VENDA-MATNR
                             AND WERKS = WA_VENDA-WERKS.

      DELETE IT_VENDA WHERE MATNR = WA_VENDA-MATNR
                        AND WERKS = WA_VENDA-WERKS
                        AND ( FKIMG_SOMA IS INITIAL OR VOLUM_SOMA IS INITIAL ).

      CLEAR: TOTAL_FKIMG, TOTAL_VOLUM.
    ENDIF.

    CLEAR: TOTAL_FKIMG, TOTAL_VOLUM, WA_VENDA.
  ENDLOOP.

  CLEAR: WA_VENDA.

  LOOP AT IT_VENDA INTO WA_VENDA.
    WA_VENDAS-MATNR = WA_VENDA-MATNR.
    WA_VENDAS-NORMT = WA_VENDA-NORMT.
    WA_VENDAS-WERKS = WA_VENDA-WERKS.
    WA_VENDAS-CHARG = WA_VENDA-CHARG.
    WA_VENDAS-FKIMG = WA_VENDA-FKIMG_SOMA.
    WA_VENDAS-VOLUM = WA_VENDA-VOLUM_SOMA.

    APPEND WA_VENDAS    TO IT_VENDAS.

    WA_PRINCIPAL-MATNR = WA_VENDAS-MATNR.
    WA_PRINCIPAL-NORMT = WA_VENDAS-NORMT.
    WA_PRINCIPAL-WERKS = WA_VENDAS-WERKS.
    WA_PRINCIPAL-FKIMG = WA_VENDAS-FKIMG.
    WA_PRINCIPAL-VOLUM = WA_VENDAS-VOLUM.

    APPEND WA_PRINCIPAL TO IT_PRINCIPAL.

    CLEAR: WA_VENDA, WA_VENDAS, WA_PRINCIPAL.
  ENDLOOP.

ENDFORM.                    " VENDAS

*&---------------------------------------------------------------------*
*&      Form  PRODUZIDO
*&---------------------------------------------------------------------*
FORM PRODUZIDO.
  DATA: QTD           TYPE SY-TABIX,
        TOTAL         TYPE CHVW-MENGE,
        WA_PARAMETROS TYPE ZTSAFRAFARDOS.

  PERFORM: CARAC_ATINN USING 'SAFRA'.

  CLEAR: IT_MARA[].

  SELECT MATNR MATKL NORMT
  FROM MARA
    INTO TABLE IT_MARA
  WHERE MATNR IN P_MATNR
    AND MATKL IN P_MATKL.

  CHECK NOT IT_MARA[] IS INITIAL.

  SELECT MANDT CHARG DATA_INICIO DATA_FIM STATUS
  FROM ZTSAFRAFARDOS
    INTO TABLE IT_ZTSAFRAFARDOS
  WHERE CHARG IN P_CHARG
    AND STATUS EQ 'L'.

  CHECK NOT IT_ZTSAFRAFARDOS[] IS INITIAL.

  SELECT MATNR WERKS CHARG MENGE XZUGA BWART SHKZG MJAHR BUDAT
  FROM CHVW
    INTO TABLE IT_CHVW
    FOR ALL ENTRIES IN IT_ZTSAFRAFARDOS
  WHERE WERKS IN P_WERKS
    AND ( BUDAT >= IT_ZTSAFRAFARDOS-DATA_INICIO AND BUDAT <= IT_ZTSAFRAFARDOS-DATA_FIM )
    AND MENGE > 0.

  LOOP AT IT_CHVW INTO WA_CHVW.
    QTD = STRLEN( WA_CHVW-CHARG ).

    IF ( QTD <= 7 ).
      WA_CHVW-DEL = 'X'.
      MODIFY  IT_CHVW FROM WA_CHVW INDEX SY-TABIX TRANSPORTING DEL.
    ENDIF.
    CLEAR: WA_CHVW.
  ENDLOOP.
  DELETE IT_CHVW  WHERE DEL = 'X'.


  SELECT MATNR CUOBJ_BM CHARG
  FROM MCH1
    INTO TABLE IT_MCH1_AUX
    FOR ALL ENTRIES IN IT_CHVW
  WHERE MATNR EQ IT_CHVW-MATNR
    AND CHARG EQ IT_CHVW-CHARG.

  LOOP AT IT_MCH1_AUX INTO WA_MCH1_AUX.
    WA_MCH1_AUX-CUOBJ = WA_MCH1_AUX-CUOBJ_BM.

    APPEND WA_MCH1_AUX TO IT_MCH1.
    CLEAR: WA_MCH1_AUX.
  ENDLOOP.

  IF IT_MCH1_AUX[] IS NOT INITIAL.
    SELECT OBJEK KLART ATFLV ATINN
           ATWRT                    "ADD - 21.06.2013
    FROM AUSP
      INTO TABLE IT_AUSP
      FOR ALL ENTRIES IN IT_MCH1
    WHERE OBJEK EQ IT_MCH1-CUOBJ
      AND ATINN EQ ATINN
      AND ATWRT EQ SAFRA          "ADD - 21.06.2013
      AND KLART EQ '023'.
  ENDIF.

  SORT: IT_MARA BY MATNR,
        IT_MCH1 BY MATNR CHARG,
        IT_AUSP BY OBJEK.

  LOOP AT IT_CHVW INTO WA_CHVW.
    READ TABLE IT_MARA INTO WA_MARA WITH KEY MATNR = WA_CHVW-MATNR BINARY SEARCH.
    IF ( SY-SUBRC EQ 0 ).
      READ TABLE IT_MCH1 INTO WA_MCH1 WITH KEY MATNR = WA_CHVW-MATNR
                                               CHARG = WA_CHVW-CHARG BINARY SEARCH.
      IF ( SY-SUBRC EQ 0 ).
        READ TABLE IT_AUSP INTO WA_AUSP WITH KEY  OBJEK = WA_MCH1-CUOBJ BINARY SEARCH.
        IF ( SY-SUBRC EQ 0 ).
          WA_PRODUZIDO-MATNR  = WA_CHVW-MATNR.
          WA_PRODUZIDO-NORMT  = WA_MARA-NORMT.
          WA_PRODUZIDO-WERKS  = WA_CHVW-WERKS.
          WA_PRODUZIDO-MENGE  = WA_CHVW-MENGE.
          WA_PRODUZIDO-SHKZG  = WA_CHVW-SHKZG.
*          wa_produzido-safra  = wa_ausp-atflv.
          WA_PRODUZIDO-SAFRA  = WA_AUSP-ATWRT.
          WA_PRODUZIDO-CHARG  = WA_CHVW-CHARG.
          WA_PRODUZIDO-BWART  = WA_CHVW-BWART.

          APPEND WA_PRODUZIDO TO IT_PRODUZIDO_AUX.
        ENDIF.
      ENDIF.

      CLEAR: WA_AUSP, WA_MCH1, WA_CHVW, WA_PRODUZIDO.
    ENDIF.
  ENDLOOP.

  CLEAR: WA_PRODUZIDO.

  IT_PRODUZIDO[] = IT_PRODUZIDO_AUX[].

  LOOP AT IT_PRODUZIDO_AUX INTO WA_PRODUZIDO_AUX.
    CLEAR: TOTAL, QTD.

    LOOP AT IT_PRODUZIDO INTO WA_PRODUZIDO WHERE MATNR EQ WA_PRODUZIDO_AUX-MATNR
                                             AND WERKS EQ WA_PRODUZIDO_AUX-WERKS.

*      CASE WA_PRODUZIDO-SHKZG.
*        WHEN: 'S'.
*          TOTAL = TOTAL + WA_PRODUZIDO-MENGE.
*        WHEN: 'H'.
*          TOTAL = TOTAL - WA_PRODUZIDO-MENGE.
*      ENDCASE.

      CASE WA_PRODUZIDO-BWART.
        WHEN: '131'.
          QTD = QTD + 1.
          TOTAL = TOTAL + WA_PRODUZIDO-MENGE.
        WHEN: '132'.
          QTD = QTD - 1.
          TOTAL = TOTAL - WA_PRODUZIDO-MENGE.
        WHEN: '309'.
          IF ( WA_PRODUZIDO-SHKZG EQ 'S' ).
            QTD = QTD + 1.
            TOTAL = TOTAL + WA_PRODUZIDO-MENGE.
          ELSEIF (  WA_PRODUZIDO-SHKZG EQ 'H' ).
            QTD = QTD - 1.
            TOTAL = TOTAL - WA_PRODUZIDO-MENGE.
          ENDIF.
        WHEN: '261'.
          QTD = QTD - 1.
          TOTAL = TOTAL - WA_PRODUZIDO-MENGE.
        WHEN: '262'.
          QTD = QTD + 1.
          TOTAL = TOTAL + WA_PRODUZIDO-MENGE.
        WHEN: '701'.
          QTD = QTD + 1.
          TOTAL = TOTAL + WA_PRODUZIDO-MENGE.
        WHEN: '702'.
          QTD = QTD - 1.
          TOTAL = TOTAL - WA_PRODUZIDO-MENGE.
      ENDCASE.

      CLEAR: WA_PRODUZIDO.
    ENDLOOP.

    WA_PRINCIPAL-MATNR = WA_PRODUZIDO_AUX-MATNR.
    WA_PRINCIPAL-NORMT = WA_PRODUZIDO_AUX-NORMT.
    WA_PRINCIPAL-WERKS = WA_PRODUZIDO_AUX-WERKS.
    WA_PRINCIPAL-MENGE = TOTAL.
    WA_PRINCIPAL-QTD_PRODUZIDO = QTD.

    APPEND WA_PRINCIPAL TO IT_PRINCIPAL.

    DELETE IT_PRODUZIDO_AUX WHERE MATNR EQ WA_PRODUZIDO_AUX-MATNR
                              AND WERKS EQ WA_PRODUZIDO_AUX-WERKS.

    CLEAR: WA_PRODUZIDO_AUX, WA_PRINCIPAL, TOTAL, QTD.
  ENDLOOP.

ENDFORM.                    " PRODUZIDO

*&---------------------------------------------------------------------*
*&      Form  RESERVA
*&---------------------------------------------------------------------*
FORM RESERVA.

  DATA: QTD_FARDO TYPE MCHB-CSPEM.

  PERFORM: CARAC_ATINN USING 'SAFRA'.

  CLEAR: IT_MCH1_AUX[], IT_MCH1[], IT_AUSP[], WA_MCH1, WA_MCH1_AUX, WA_AUSP.

  SELECT MATNR WERKS CHARG LGORT CSPEM
  FROM MCHB
    INTO TABLE IT_MCHB
  WHERE MATNR IN P_MATNR
    AND WERKS IN P_WERKS
    AND CSPEM > 0.

  SELECT MATNR CUOBJ_BM CHARG
  FROM MCH1
    INTO TABLE IT_MCH1_AUX
    FOR ALL ENTRIES IN IT_MCHB
  WHERE MATNR EQ IT_MCHB-MATNR
    AND CHARG EQ IT_MCHB-CHARG.

  LOOP AT IT_MCH1_AUX INTO WA_MCH1_AUX.
    WA_MCH1_AUX-CUOBJ = WA_MCH1_AUX-CUOBJ_BM.

    APPEND WA_MCH1_AUX TO IT_MCH1.

    CLEAR: WA_MCH1_AUX.
  ENDLOOP.

  SELECT OBJEK KLART ATFLV ATINN
         ATWRT                    "ADD - 21.06.2013
  FROM AUSP
    INTO TABLE IT_AUSP
    FOR ALL ENTRIES IN IT_MCH1
  WHERE OBJEK EQ IT_MCH1-CUOBJ
    AND KLART EQ '023'
    AND ATINN EQ ATINN
    AND ATWRT EQ SAFRA.           "ADD - 21.06.2013
*    AND atflv EQ safra.

  LOOP AT IT_MCHB INTO WA_MCHB.
    READ TABLE IT_MCH1 INTO WA_MCH1 WITH KEY MATNR = WA_MCHB-MATNR
                                             CHARG = WA_MCHB-CHARG.
    IF ( SY-SUBRC EQ 0 ).
      READ TABLE IT_AUSP INTO WA_AUSP WITH KEY OBJEK = WA_MCH1-CUOBJ.
      IF ( SY-SUBRC EQ 0 ).
        WA_RESERVA_AUX-MATNR = WA_MCHB-MATNR.
        WA_RESERVA_AUX-WERKS = WA_MCHB-WERKS.
        WA_RESERVA_AUX-CSPEM = WA_MCHB-CSPEM.
*        wa_reserva_aux-safra = wa_ausp-atflv.
        WA_RESERVA_AUX-SAFRA = WA_AUSP-ATWRT.

        APPEND WA_RESERVA_AUX TO IT_RESERVA_AUX.
      ENDIF.
    ENDIF.

    CLEAR: WA_MCHB, WA_MCH1, WA_AUSP.
  ENDLOOP.

  IT_RESERVA_CONT[] = IT_RESERVA_AUX.

  DATA: SOMA TYPE MCHB-CSPEM.

  LOOP AT IT_RESERVA_AUX INTO WA_RESERVA_AUX.
    CLEAR: SOMA, QTD_FARDO.

    LOOP AT IT_RESERVA_CONT INTO WA_RESERVA_CONT WHERE MATNR EQ WA_RESERVA_AUX-MATNR.
      SOMA = SOMA + WA_RESERVA_CONT-CSPEM.
      QTD_FARDO = QTD_FARDO + 1.

      CLEAR: WA_RESERVA_CONT.
    ENDLOOP.

    WA_RESERVA-CSPEM      = SOMA.
    WA_RESERVA-MATNR      = WA_RESERVA_AUX-MATNR.
    WA_RESERVA-WERKS      = WA_RESERVA_AUX-WERKS.
    WA_RESERVA-SAFRA      = WA_RESERVA_AUX-SAFRA.
    WA_RESERVA-QTD_FARDO  = QTD_FARDO.

    APPEND WA_RESERVA TO IT_RESERVA.

    DELETE IT_RESERVA_AUX WHERE MATNR EQ WA_RESERVA_AUX-MATNR.

*    move: wa_reserva_aux-matnr to wa_reserva-matnr,
*          wa_reserva_aux-werks to wa_reserva-werks,
*          wa_reserva_aux-cspem to wa_reserva-cspem,
*          wa_reserva_aux-safra to wa_reserva-safra.
*
*    collect wa_reserva into it_reserva.

    CLEAR: WA_RESERVA, WA_RESERVA_AUX.
  ENDLOOP.

  LOOP AT IT_RESERVA INTO WA_RESERVA.
    SELECT SINGLE * FROM MARA INTO WA_MARA_TIPO WHERE MATNR EQ WA_RESERVA-MATNR.

    WA_PRINCIPAL-MATNR = WA_RESERVA-MATNR.
    WA_PRINCIPAL-NORMT = WA_MARA_TIPO-NORMT.
    WA_PRINCIPAL-WERKS = WA_RESERVA-WERKS.
    WA_PRINCIPAL-CSPEM = WA_RESERVA-CSPEM.
    WA_PRINCIPAL-QTD_FARDO = WA_RESERVA-QTD_FARDO.

    APPEND WA_PRINCIPAL TO IT_PRINCIPAL.

    CLEAR: WA_RESERVA.
  ENDLOOP.

  CLEAR: SAFRA.
ENDFORM.                    " RESERVA

*&---------------------------------------------------------------------*
*&      Form  AGRUPAMENTO
*&---------------------------------------------------------------------*
FORM AGRUPAMENTO.
  DATA: FKIMG TYPE VBRP-FKIMG,
        VOLUM TYPE VBRP-VOLUM,
        MENGE TYPE CHVW-MENGE,
        CSPEM TYPE MCHB-CSPEM.

  IT_PRINCIPAL_AUX[] = IT_PRINCIPAL[].

  PERFORM: CABECALHO.

  CLEAR: WA_PRINCIPAL.

  DATA: QTD_FARDO TYPE MCHB-CSPEM,
        QTD_PRODUZIDO TYPE CHVW-MENGE.

  LOOP AT IT_PRINCIPAL_AUX INTO WA_PRINCIPAL_AUX.
    LOOP AT IT_PRINCIPAL INTO WA_PRINCIPAL WHERE MATNR EQ WA_PRINCIPAL_AUX-MATNR.
      FKIMG = FKIMG + WA_PRINCIPAL-FKIMG.
      VOLUM = VOLUM + WA_PRINCIPAL-VOLUM.
      MENGE = MENGE + WA_PRINCIPAL-MENGE.
      CSPEM = CSPEM + WA_PRINCIPAL-CSPEM.
      " Display Fardos

      QTD_FARDO      = QTD_FARDO + WA_PRINCIPAL-QTD_FARDO.
      QTD_PRODUZIDO  = QTD_PRODUZIDO + WA_PRINCIPAL-QTD_PRODUZIDO.

      CLEAR: WA_PRINCIPAL.
    ENDLOOP.

    WA_DISPLAY-MATNR = WA_PRINCIPAL_AUX-MATNR.

    " Display em Fardos
    WA_DISPLAY-MENGE_FA = QTD_PRODUZIDO.
    WA_DISPLAY-VOLUM_FA = VOLUM.
    WA_DISPLAY-CSPEM_FA = QTD_FARDO.
    WA_DISPLAY-SALDO_FA = ( ( WA_DISPLAY-MENGE_FA - WA_DISPLAY-VOLUM_FA ) - WA_DISPLAY-CSPEM_FA ).

    " Produção KG
    WA_DISPLAY-MENGE_KG = MENGE.
    WA_DISPLAY-FKIMG_KG = FKIMG.
    WA_DISPLAY-CSPEM_KG = CSPEM.
    WA_DISPLAY-SALDO_KG = ( ( WA_DISPLAY-MENGE_KG  - WA_DISPLAY-FKIMG_KG ) - WA_DISPLAY-CSPEM_KG ) .
    WA_DISPLAY-NORMT    = WA_PRINCIPAL_AUX-NORMT.

    "    IF ( WA_DISPLAY-SALDO_FA > 0 ) OR (  WA_DISPLAY-SALDO_KG > 0 ).
    IF  WA_DISPLAY-MENGE_FA > 0 OR  WA_DISPLAY-VOLUM_FA > 0 OR  WA_DISPLAY-CSPEM_FA > 0 OR  WA_DISPLAY-MENGE_KG > 0 OR WA_DISPLAY-FKIMG_KG > 0 OR WA_DISPLAY-CSPEM_KG > 0.
      APPEND WA_DISPLAY TO IT_DISPLAY.
    ENDIF.

    DELETE IT_PRINCIPAL_AUX WHERE MATNR EQ WA_PRINCIPAL_AUX-MATNR.

    CLEAR: WA_PRINCIPAL, WA_PRINCIPAL_AUX, FKIMG, VOLUM, MENGE, CSPEM, QTD_FARDO, QTD_PRODUZIDO.
  ENDLOOP.

ENDFORM.                    " AGRUPAMENTO

*&---------------------------------------------------------------------*
*&      Form  CARAC_ATINN
*&---------------------------------------------------------------------*
FORM CARAC_ATINN USING P_CARAC.
  DATA: SAFRA_AUX TYPE NUMC4.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      INPUT  = P_CARAC
    IMPORTING
      OUTPUT = ATINN.

  SAFRA_AUX = P_CHARG.
  SAFRA     = SAFRA_AUX.

  CLEAR: SAFRA_AUX.
ENDFORM.                    " CARAC_ATINN

*&---------------------------------------------------------------------*
*&      RECEIVER
*&---------------------------------------------------------------------*
CLASS CL_EVENT_RECEIVER DEFINITION.

ENDCLASS.                    "CL_EVENT_RECEIVER DEFINITION

*---------------------------------------------------------------------*
*       CLASS CL_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS CL_EVENT_RECEIVER IMPLEMENTATION.

ENDCLASS.                    "CL_EVENT_RECEIVER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
FORM CREATE_OBJECT.

  CREATE OBJECT G_CONTAINER
    EXPORTING
      CONTAINER_NAME              = 'CONTAINER_PRINCIPAL'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  CREATE OBJECT GRID
    EXPORTING
      I_PARENT = G_CONTAINER.

  PERFORM: CREATE_FIELD.

  CALL METHOD GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IS_VARIANT                    = WA_VARIANT
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = IT_DISPLAY
      IT_FIELDCATALOG               = GT_FIELDCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

ENDFORM.                    " CREATE_OBJECT

*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD
*&---------------------------------------------------------------------*
FORM CREATE_FIELD.
  PERFORM CATALOG USING:
    'MATNR'     'Material'        ''    'X' '' '' '' '',
    'NORMT'     'Tipo'            ''    ''  '' '' '' '',

    " Estoque em Fardos
    'MENGE_FA'  'Produção Fardo'  '15'  ''  '' '' '' '',
    'VOLUM_FA'  'Saida Fardo'     '15'  ''  '' '' '' '',
    'CSPEM_FA'  'Reserva Fardo'   '15'  ''  '' '' '' '',
    'SALDO_FA'  'Saldo Fardo'     '15'  ''  '' '' '' '',

    " Estoque em Kg
    'MENGE_KG'  'Produção Kg.'    '15'  ''  '' '' '' '',
    'FKIMG_KG'  'Saida Kg.'       '15'  ''  '' '' '' '',
    'CSPEM_KG'  'Reserva Kg.'     '15'  ''  '' '' '' '',
    'SALDO_KG'  'Saldo Kg.'       '15'  ''  '' '' '' ''.

ENDFORM.                    " CREATE_FIELD

*&---------------------------------------------------------------------*
*&      Form  CATALOG
*&---------------------------------------------------------------------*
FORM CATALOG  USING   P_FIELDNAME P_DESC P_TAM P_NO_ZERO P_HOTSPOT
                      P_COR       P_JUST P_SUM.

  DATA: WA_FIELDCATALOG TYPE LVC_S_FCAT.

  WA_FIELDCATALOG-FIELDNAME = P_FIELDNAME.
  WA_FIELDCATALOG-SCRTEXT_L = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_M = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_S = P_DESC.
  WA_FIELDCATALOG-OUTPUTLEN = P_TAM.
  WA_FIELDCATALOG-NO_ZERO   = P_NO_ZERO.
  WA_FIELDCATALOG-HOTSPOT   = P_HOTSPOT.
  WA_FIELDCATALOG-EMPHASIZE = P_COR.
  WA_FIELDCATALOG-JUST      = P_JUST.
  WA_FIELDCATALOG-DO_SUM    = P_SUM.

  APPEND WA_FIELDCATALOG TO GT_FIELDCAT.

  CLEAR: WA_FIELDCATALOG.

ENDFORM.                    " CATALOG

*&---------------------------------------------------------------------*
*&      Form  CABECALHO
*&---------------------------------------------------------------------*
FORM CABECALHO.
  CLEAR: WA_DESCRICAO.

  DATA: DIA TYPE C LENGTH 2,
        MES TYPE C LENGTH 2,
        ANO TYPE C LENGTH 4.

  DATA: WA_T001  TYPE T001,
        WA_T001W TYPE T001W.

  MOVE: SY-DATUM+6(2) TO DIA,
        SY-DATUM+4(2) TO MES,
        SY-DATUM(4)   TO ANO.

  CONCATENATE DIA '/' MES '/' ANO INTO WA_DESCRICAO-DATA.
  WA_DESCRICAO-BUKRS = P_BUKRS-LOW.

  SELECT SINGLE * FROM T001 INTO WA_T001 WHERE BUKRS EQ P_BUKRS-LOW.

  WA_DESCRICAO-BUTXT = WA_T001-BUTXT.

  SELECT SINGLE * FROM T001W INTO WA_T001W WHERE WERKS EQ P_WERKS-LOW.
  WA_DESCRICAO-WERKS = P_WERKS-LOW.
  WA_DESCRICAO-NAME1 = WA_T001W-NAME1.
  WA_DESCRICAO-CHARG = P_CHARG-LOW.

ENDFORM.                    " CABECALHO
