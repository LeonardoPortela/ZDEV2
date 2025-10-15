REPORT  ZLESR0087.

TABLES: VBFA, LIPS, VBAK, ZLEST0097.

*-----------------------------
* Estrutura
*-----------------------------
TYPES: BEGIN OF TY_VBFA,
         VBELN     TYPE VBFA-VBELN,
         VBTYP_N   TYPE VBFA-VBTYP_N,
         VBTYP_V   TYPE VBFA-VBTYP_V,
         ERDAT     TYPE VBFA-ERDAT,
         VBELV     TYPE VBFA-VBELV,
         RFMNG     TYPE VBFA-RFMNG,
         ERDAT_LEN TYPE TCURR-GDATU,
         ERDAT_AUX TYPE TCURR-GDATU,
       END OF TY_VBFA,

       BEGIN OF TY_EKBE,
         EBELN     TYPE EKBE-EBELN,
         EBELP     TYPE EKBE-EBELP,
         BUDAT     TYPE EKBE-BUDAT,
         BELNR     TYPE EKBE-BELNR,
         ERDAT_LEN TYPE TCURR-GDATU,
         ERDAT_AUX TYPE TCURR-GDATU,
       END OF TY_EKBE,

       BEGIN OF TY_EKKO,
         EBELN TYPE EKKO-EBELN,
         RESWK TYPE EKKO-RESWK,
         BSART TYPE EKKO-BSART,
       END OF TY_EKKO,

       BEGIN OF TY_EKPO,
         EBELN TYPE EKPO-EBELN,
         EBELP TYPE EKPO-EBELP,
         WERKS TYPE EKPO-WERKS,
         INCO1 TYPE EKPO-INCO1,
       END OF TY_EKPO,

       BEGIN OF TY_EKPA,
         EBELN TYPE EKPA-EBELN,
         LIFN2 TYPE EKPA-LIFN2,
       END OF TY_EKPA,

       BEGIN OF TY_VBPA_AUX,
         VBELN TYPE VBPA-VBELN,
         LIFNR TYPE VBPA-LIFNR,
       END OF TY_VBPA_AUX,

       BEGIN OF TY_GT_ZCOT0007,
         VBELN TYPE ZCOT0007-VBELN,
         DMBTR TYPE ZCOT0007-DMBTR,
         DMBE2 TYPE ZCOT0007-DMBE2,
       END OF TY_GT_ZCOT0007,

       BEGIN OF TY_VBAK,
         VKORG TYPE VBAK-VKORG,
         VBELN TYPE VBAK-VBELN,
         AUART TYPE VBAK-AUART,
         KUNNR TYPE VBAK-KUNNR,
         KVGR3 TYPE VBAK-KVGR3,
         "FLAG(1)  TYPE C,
       END OF TY_VBAK.



*-----------------------------
* INTERNAL TABLE.
*-----------------------------
DATA: GT_VBFA         TYPE TABLE OF TY_VBFA,
      GT_ZCOT0007     TYPE TABLE OF TY_GT_ZCOT0007,
      GT_ZCOT0007_AUX TYPE TABLE OF TY_GT_ZCOT0007,
      GT_LIPS         TYPE TABLE OF LIPS,
      GT_VBAK         TYPE TABLE OF TY_VBAK,
      GT_VBAK_AUX     TYPE TABLE OF TY_VBAK,
      GT_VBPA         TYPE TABLE OF VBPA,
      GT_VBPA_AUX     TYPE TABLE OF TY_VBPA_AUX,
      "GT_MARA         TYPE TABLE OF MARA,
      GT_ZSDT0001     TYPE TABLE OF ZSDT0001,
      GT_VTTP         TYPE TABLE OF VTTP,
      GT_VTTK         TYPE TABLE OF VTTK,
      GT_VTFA         TYPE TABLE OF VTFA,
      GT_VFKP         TYPE TABLE OF VFKP,
      GT_KONV         TYPE TABLE OF KONV,
      "GT_TCURR        TYPE TABLE OF TCURR,
      GT_KNA1         TYPE TABLE OF KNA1,
      GT_LFA1         TYPE TABLE OF LFA1,
      GT_VBKD         TYPE TABLE OF VBKD,
      GT_ZLEST0060    TYPE TABLE OF ZLEST0060,
      GT_ZLEST0061    TYPE TABLE OF ZLEST0061,
      GT_ZLEST0070    TYPE TABLE OF ZLEST0070,
      GT_ZLEST0055    TYPE TABLE OF ZLEST0055,
      GT_ZLEST0098    TYPE TABLE OF ZLEST0098.

*-----------------------------
* INTERNAL TABLE MM
*-----------------------------
DATA: GT_EKKO        TYPE TABLE OF TY_EKKO,
      WA_EKKO        TYPE TY_EKKO,
      GT_EKPO        TYPE TABLE OF TY_EKPO,
      WA_EKPO        TYPE TY_EKPO,
      GT_EKBE        TYPE TABLE OF TY_EKBE,
      WA_EKBE        TYPE TY_EKBE,
      GT_EKPA        TYPE TABLE OF TY_EKPA,
      WA_EKPA        TYPE TY_EKPA,
      GT_LIPS_MM     TYPE TABLE OF LIPS,
      WA_LIPS_MM     TYPE LIPS,
      "GT_MARA_MM     TYPE TABLE OF MARA,
      GT_ZSDT0001_MM TYPE TABLE OF ZSDT0001,
      WA_ZSDT0001_MM TYPE ZSDT0001,
      GT_VTTP_MM     TYPE TABLE OF VTTP,
      WA_VTTP_MM     TYPE VTTP,
      GT_VTTK_MM     TYPE TABLE OF VTTK,
      GT_VFKP_MM     TYPE TABLE OF VFKP,
      WA_VFKP_MM     TYPE VFKP,
      GT_KONV_MM     TYPE TABLE OF KONV,
      WA_KONV_MM     TYPE KONV,
      GT_TCURR       TYPE TABLE OF TCURR,
      WA_TCURR       TYPE TCURR.
"GT_LFA1_MM     TYPE TABLE OF LFA1,
"GT_T001W_MM    TYPE TABLE OF T001W,
"WA_T001W_MM    TYPE T001W,
"WA_T001W_MM2   TYPE T001W.

*-----------------------------
* WORK AREA
*-----------------------------
DATA: GW_VBFA         TYPE TY_VBFA,
      GW_EKBE         TYPE TY_EKBE,
      GW_LIPS         TYPE LIPS,
      GW_VBAK         TYPE TY_VBAK,
      GW_VBAK_AUX     TYPE TY_VBAK,
      GW_VBPA         TYPE VBPA,
      GW_VBPA_AUX     TYPE TY_VBPA_AUX,
      GW_MARA         TYPE MARA,
      GW_ZSDT0001     TYPE ZSDT0001,
      GW_VTTP         TYPE VTTP,
      GW_VTTK         TYPE VTTK,
      GW_VTFA         TYPE VTFA,
      GW_VFKP         TYPE VFKP,
      GW_KONV         TYPE KONV,
      GW_TCURR        TYPE TCURR,
      GW_KNA1         TYPE KNA1,
      GW_LFA1         TYPE LFA1,
      GW_VBKD         TYPE VBKD,
      GW_ZLEST0060    TYPE ZLEST0060,
      GW_ZLEST0061    TYPE ZLEST0061,
      GW_ZLEST0070    TYPE ZLEST0070,
      GW_ZLEST0055    TYPE ZLEST0055,
      GW_ZLEST0098    TYPE ZLEST0098,
      GW_ZCOT0007     TYPE TY_GT_ZCOT0007,
      GW_ZCOT0007_AUX TYPE TY_GT_ZCOT0007.

DATA: GW_ZLEST0097 TYPE ZLEST0097.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:

                 P_VKORG FOR VBAK-VKORG OBLIGATORY , " Empresa
                 P_WERKS FOR LIPS-WERKS NO INTERVALS NO-EXTENSION,
                 P_ERDAT FOR VBFA-ERDAT NO-EXTENSION. " Data de Liquidação
SELECTION-SCREEN: END OF BLOCK B1.


START-OF-SELECTION.

  PERFORM:  SELECIONAR_DADOS,
            SELECIONAR_DADOS_MM,
            INSERIR.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  DATA: OBJ_ZCL_UTIL TYPE REF TO ZCL_UTIL.

  FIELD-SYMBOLS: <FS_VBFA> TYPE TY_VBFA,
                 <FS_EKBE> TYPE TY_EKBE.

  DATA: VAR_GDATU     TYPE SY-DATUM,
        VAR_DATA_CONV TYPE SY-DATUM,
        VAR_DATA      TYPE C LENGTH 10.

  "Movimento de Embarque Diario
  "Fluxo de documentos de vendas e distribuição
  " J  = ENTREGA - (Categoria de documento SD subseqüente)
  " C  = ORDEM   - (Ctg.documento de venda e distribuição (SD) precedente)

  IF SY-BATCH = 'X'.
    REFRESH: P_ERDAT, P_VKORG.

    P_ERDAT-LOW    = ( SY-DATUM ) - 30.
    P_ERDAT-HIGH   = SY-DATUM.
    P_ERDAT-SIGN   = 'I'.
    P_ERDAT-OPTION = 'BT'.
    APPEND P_ERDAT.

    P_VKORG-LOW    = '0001'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.

    P_VKORG-LOW    = '0015'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.

    P_VKORG-LOW    = '0018'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.
  ENDIF.

  SELECT VBELN VBTYP_N VBTYP_V ERDAT VBELV RFMNG
    FROM VBFA
    INTO TABLE GT_VBFA
  WHERE VBTYP_N EQ 'J'
    AND VBTYP_V EQ 'C'
    AND ERDAT   IN P_ERDAT.


  CHECK NOT GT_VBFA[] IS INITIAL.


  CREATE OBJECT OBJ_ZCL_UTIL.

  LOOP AT GT_VBFA ASSIGNING <FS_VBFA>.

    CLEAR: VAR_DATA, VAR_GDATU, VAR_DATA_CONV.

    OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = <FS_VBFA>-ERDAT
                                   RECEIVING E_DATA = VAR_DATA ).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = VAR_DATA
      IMPORTING
        OUTPUT = <FS_VBFA>-ERDAT_LEN.

    VAR_DATA_CONV = <FS_VBFA>-ERDAT - 1.

    OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = VAR_DATA_CONV
                                   RECEIVING E_DATA = VAR_DATA ).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = VAR_DATA
      IMPORTING
        OUTPUT = <FS_VBFA>-ERDAT_AUX.

  ENDLOOP.

  "Buscar dados da Remessa.
  "Documento SD: fornecimento: dados de item
  SELECT * FROM LIPS
    INTO TABLE GT_LIPS
    FOR ALL ENTRIES IN GT_VBFA
  WHERE VBELN EQ GT_VBFA-VBELN
    AND WERKS IN P_WERKS.

  "Frete Aquaviário - NF Vinculadas
  SELECT * FROM ZLEST0060
    INTO TABLE GT_ZLEST0060
    FOR ALL ENTRIES IN GT_LIPS
 WHERE DOC_REM EQ GT_LIPS-VBELN
   AND DOCNUM  NE SPACE.

  "Frete Aquaviário - Ordem de Venda
  SELECT * FROM ZLEST0061
    INTO TABLE GT_ZLEST0061
    FOR ALL ENTRIES IN GT_ZLEST0060
  WHERE DOCNUM EQ GT_ZLEST0060-DOCNUM.

  "Buscar dados da Ordem de Venda
  "Documento de vendas: dados de cabeçalho
  " ZRDC = Remessa Formação de Lote DCO
  " ZRFL = Remessa Formação de Lote
  SELECT VKORG
         VBELN
         AUART
         KUNNR
         KVGR3
     FROM VBAK
    INTO TABLE GT_VBAK
    FOR ALL ENTRIES IN GT_VBFA
  WHERE VBELN EQ GT_VBFA-VBELV
    AND VKORG IN P_VKORG
    AND AUART IN ('ZRDC','ZRFL').



  IF ( SY-SUBRC EQ 0 ).
    SELECT * FROM VBPA
      INTO TABLE GT_VBPA
      FOR ALL ENTRIES IN GT_VBFA
    WHERE VBELN EQ GT_VBFA-VBELV
      AND PARVW IN ('LR','Z1','AG').

    " Codigo da coleta
    SELECT VBELN LIFNR  FROM VBPA
      INTO TABLE GT_VBPA_AUX
      FOR ALL ENTRIES IN GT_VBFA
      WHERE VBELN EQ GT_VBFA-VBELV
      AND PARVW = 'PC'.

  ENDIF.

  SELECT VKORG
         VBELN
         AUART
         KUNNR
         KVGR3
     FROM VBAK
    APPENDING TABLE GT_VBAK
    FOR ALL ENTRIES IN GT_VBFA
  WHERE VBELN EQ GT_VBFA-VBELV
    AND VKORG IN P_VKORG.


  "Deletar todos os registros que são iguais a ZEXI, ZPER e ZEXP.
  " ZEXI = Venda Exportação Indireta
  " ZPER = Venda Performance
  " ZEXP = Venda Exportação Direta.

  IF ( SY-SUBRC EQ 0 ).
    DELETE GT_VBAK WHERE AUART EQ 'ZEXI'
                      OR AUART EQ 'ZPER'
                      OR AUART EQ 'ZEXP'.
  ENDIF.

  "Documento de vendas: dados comerciais
  SELECT * FROM VBKD
    INTO TABLE GT_VBKD
    FOR ALL ENTRIES IN GT_VBAK
  WHERE VBELN EQ GT_VBAK-VBELN.


  "Busca de dados Materiais e Grupo de Mercadoria
*  SELECT * FROM MARA
*    INTO TABLE GT_MARA
*    FOR ALL ENTRIES IN GT_LIPS
*  WHERE MATNR EQ GT_LIPS-MATNR.

  "Busca de Dados no Romaneio
  SELECT * FROM ZSDT0001
    INTO TABLE GT_ZSDT0001
    FOR ALL ENTRIES IN GT_LIPS
  WHERE TP_MOVIMENTO EQ 'S'
    AND DOC_REM      EQ GT_LIPS-VBELN.

  "Busca de Dados Custo de transporte
  SELECT * FROM VTTP
    INTO TABLE GT_VTTP
    FOR ALL ENTRIES IN GT_LIPS
  WHERE VBELN EQ GT_LIPS-VBELN.

  IF ( SY-SUBRC EQ 0 ).
    "Cabeçalho transporte
    SELECT * FROM VTTK
      INTO TABLE GT_VTTK
      FOR ALL ENTRIES IN GT_VTTP
    WHERE TKNUM EQ GT_VTTP-TKNUM
      AND VSART EQ '01'.
  ENDIF.

  "Fluxo docs.transporte
  SELECT * FROM VTFA
    INTO TABLE GT_VTFA
    FOR ALL ENTRIES IN GT_VTTK
  WHERE VBELV    EQ GT_VTTK-TKNUM
    AND VBTYP_V  EQ '8'
    AND VBTYP_N  EQ 'a'.

  "Custos de frete: dados do item
  SELECT * FROM VFKP
    INTO TABLE GT_VFKP
    FOR ALL ENTRIES IN GT_VTFA
  WHERE FKNUM EQ GT_VTFA-VBELN.

  "Condições (dados de operação)
  SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @GT_VFKP WHERE KNUMV EQ @GT_VFKP-KNUMV AND KSCHL IN ( 'ZFRE' , 'ZLOT' , 'ZPED' ) INTO CORRESPONDING FIELDS OF TABLE @GT_KONV .

  "Mestre de clientes (parte geral)
  SELECT * FROM KNA1
    INTO TABLE GT_KNA1
    FOR ALL ENTRIES IN GT_VBPA
  WHERE KUNNR EQ GT_VBPA-KUNNR.

  "Mestre de fornecedores (parte geral)
  SELECT * FROM LFA1
    INTO TABLE GT_LFA1
    FOR ALL ENTRIES IN GT_VBPA
  WHERE LIFNR EQ GT_VBPA-LIFNR.

*  SELECT VBELN VBTYP_N VBTYP_V ERDAT VBELV RFMNG
*    FROM VBFA
*    INTO TABLE GT_VBFA
*   WHERE VBTYP_N EQ 'J'
*     AND VBTYP_V EQ 'C'
*     AND ERDAT   IN P_ERDAT.
*
*  CHECK NOT GT_VBFA[] IS INITIAL AND GT_EKBE[] IS INITIAL.
*
*  CREATE OBJECT OBJ_ZCL_UTIL.
*
*  LOOP AT GT_VBFA ASSIGNING <FS_VBFA>.
*    CLEAR: VAR_DATA, VAR_GDATU, VAR_DATA_CONV.
*    OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = <FS_VBFA>-ERDAT
*                                   RECEIVING E_DATA = VAR_DATA ).
*    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*      EXPORTING
*        INPUT  = VAR_DATA
*      IMPORTING
*        OUTPUT = <FS_VBFA>-ERDAT_LEN.
*    VAR_DATA_CONV = <FS_VBFA>-ERDAT - 1.
*    OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = VAR_DATA_CONV
*                                   RECEIVING E_DATA = VAR_DATA ).
*    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*      EXPORTING
*        INPUT  = VAR_DATA
*      IMPORTING
*        OUTPUT = <FS_VBFA>-ERDAT_AUX.
*  ENDLOOP.
*
*  "Buscar dados da Remessa.
*  "Documento SD: fornecimento: dados de item
*  IF GT_VBFA[] IS NOT INITIAL.
*    SELECT * FROM LIPS
*      INTO TABLE GT_LIPS
*      FOR ALL ENTRIES IN GT_VBFA
*    WHERE VBELN EQ GT_VBFA-VBELN
*      AND WERKS IN P_WERKS.
*  ENDIF.
*
*  IF GT_EKBE[] IS NOT INITIAL.
*    SELECT * FROM LIPS
*      APPENDING TABLE GT_LIPS
*      FOR ALL ENTRIES IN GT_EKBE
*    WHERE VBELN EQ GT_EKBE-BELNR
*      AND WERKS IN P_WERKS.
*  ENDIF.
*
*  "Frete Aquaviário - NF Vinculadas
*  IF GT_LIPS[] IS NOT INITIAL.
*    SELECT * FROM ZLEST0060
*      INTO TABLE GT_ZLEST0060
*      FOR ALL ENTRIES IN GT_LIPS
*   WHERE DOC_REM EQ GT_LIPS-VBELN
*     AND DOCNUM  NE SPACE.
*  ENDIF.
*
*  "Frete Aquaviário - Ordem de Venda
*  IF GT_ZLEST0060[] IS NOT INITIAL.
*    SELECT * FROM ZLEST0061
*      INTO TABLE GT_ZLEST0061
*      FOR ALL ENTRIES IN GT_ZLEST0060
*    WHERE DOCNUM EQ GT_ZLEST0060-DOCNUM.
*  ENDIF.
*
*  "Buscar dados da Ordem de Venda
*  "Documento de vendas: dados de cabeçalho
*  " ZRDC = Remessa Formação de Lote DCO
*  " ZRFL = Remessa Formação de Lote
*  IF GT_VBFA[] IS NOT INITIAL.
*    SELECT VKORG
*           VBELN
*           AUART
*           KUNNR
*           KVGR3
*      FROM VBAK
*      INTO TABLE GT_VBAK
*       FOR ALL ENTRIES IN GT_VBFA
*     WHERE VBELN EQ GT_VBFA-VBELV
*       AND VKORG IN P_VKORG.
*    "AND AUART IN ('ZRDC','ZRFL').
*
*    SELECT * FROM VBPA
*      INTO TABLE GT_VBPA
*       FOR ALL ENTRIES IN GT_VBFA
*     WHERE VBELN EQ GT_VBFA-VBELV
*       AND PARVW IN ('LR','Z1','AG').
*
*    " Codigo da coleta
*    SELECT VBELN LIFNR  FROM VBPA
*      INTO TABLE GT_VBPA_AUX
*       FOR ALL ENTRIES IN GT_VBFA
*     WHERE VBELN EQ GT_VBFA-VBELV
*       AND PARVW EQ 'PC'.
*
**    SELECT VKORG VBELN AUART KUNNR KVGR3
**      FROM VBAK
**      APPENDING TABLE GT_VBAK
**       FOR ALL ENTRIES IN GT_VBFA
**     WHERE VBELN EQ GT_VBFA-VBELV
**       AND VKORG IN P_VKORG.
*
*    "Deletar todos os registros que são iguais a ZEXI, ZPER e ZEXP.
*    " ZEXI = Venda Exportação Indireta
*    " ZPER = Venda Performance
*    " ZEXP = Venda Exportação Direta.
*    IF ( SY-SUBRC EQ 0 ).
*      DELETE GT_VBAK WHERE AUART EQ 'ZEXI' OR AUART EQ 'ZPER' OR AUART EQ 'ZEXP'.
*    ENDIF.
*
*    "Documento de vendas: dados comerciais
*    IF GT_VBAK[] IS NOT INITIAL.
*      SELECT * FROM VBKD
*        INTO TABLE GT_VBKD
*         FOR ALL ENTRIES IN GT_VBAK
*       WHERE VBELN EQ GT_VBAK-VBELN.
*    ENDIF.
*
*  ENDIF.
*
*  "Busca de dados Materiais e Grupo de Mercadoria
*  IF GT_LIPS[] IS NOT INITIAL.
*
*    SELECT * FROM MARA
*      INTO TABLE GT_MARA
*       FOR ALL ENTRIES IN GT_LIPS
*     WHERE MATNR EQ GT_LIPS-MATNR.
*
*    "Busca de Dados no Romaneio
*    SELECT * FROM ZSDT0001
*      INTO TABLE GT_ZSDT0001
*       FOR ALL ENTRIES IN GT_LIPS
*     WHERE DOC_REM EQ GT_LIPS-VBELN.
*
*    "Busca de Dados Custo de transporte
*    SELECT * FROM VTTP
*      INTO TABLE GT_VTTP
*       FOR ALL ENTRIES IN GT_LIPS
*     WHERE VBELN EQ GT_LIPS-VBELN.
*
*    IF GT_VTTP[] IS NOT INITIAL.
*      "Cabeçalho transporte
*      SELECT * FROM VTTK
*        INTO TABLE GT_VTTK
*         FOR ALL ENTRIES IN GT_VTTP
*       WHERE TKNUM EQ GT_VTTP-TKNUM
*         AND VSART EQ '01'.
*
*      IF GT_VTTK[] IS NOT INITIAL.
*        "Fluxo docs.transporte
*        SELECT * FROM VTFA
*          INTO TABLE GT_VTFA
*           FOR ALL ENTRIES IN GT_VTTK
*         WHERE VBELV    EQ GT_VTTK-TKNUM
*           AND VBTYP_V  EQ '8'
*           AND VBTYP_N  EQ 'a'.
*
*        IF GT_VTFA[] IS NOT INITIAL.
*          "Custos de frete: dados do item
*          SELECT * FROM VFKP
*            INTO TABLE GT_VFKP
*             FOR ALL ENTRIES IN GT_VTFA
*           WHERE FKNUM EQ GT_VTFA-VBELN.
*
*          IF GT_VFKP[] IS NOT INITIAL.
*            "Condições (dados de operação)
*            SELECT * FROM KONV
*              INTO TABLE GT_KONV
*               FOR ALL ENTRIES IN GT_VFKP
*             WHERE KNUMV EQ GT_VFKP-KNUMV
*               AND KSCHL IN ('ZFRE','ZLOT','ZPED').
*
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  IF GT_VBPA[] IS NOT INITIAL.
*    "Mestre de clientes (parte geral)
*    SELECT * FROM KNA1
*      INTO TABLE GT_KNA1
*      FOR ALL ENTRIES IN GT_VBPA
*    WHERE KUNNR EQ GT_VBPA-KUNNR.
*
*    "Mestre de fornecedores (parte geral)
*    SELECT * FROM LFA1
*      INTO TABLE GT_LFA1
*      FOR ALL ENTRIES IN GT_VBPA
*    WHERE LIFNR EQ GT_VBPA-LIFNR.
*  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  INSERIR
*&---------------------------------------------------------------------*
FORM INSERIR .

  DATA: OBJ_ZCL_UTIL  TYPE REF TO ZCL_UTIL.
  DATA: OBJ_ZCL_UTIL2 TYPE REF TO ZCL_UTIL.

  DATA: VAR_DATA   TYPE C LENGTH 10,
        V_DATA_AUX TYPE TCURR-GDATU,
        V_UKURS    TYPE TCURR-UKURS,
        V_ERDAT    TYPE VBFA-ERDAT.


  DATA: GL_ZSDT_DEPARA_DEPO TYPE ZSDT_DEPARA_DEPO,
        GL_ZLEST0097        TYPE ZLEST0097,
        GL_ZLEST0098        TYPE ZLEST0098,
        IT_ZLEST0098        TYPE TABLE OF ZLEST0098,
        GL_ZLEST0055        TYPE ZLEST0055,
        GL_KNA1             TYPE KNA1.

  DATA: GL_VBPA_AQUA TYPE TABLE OF VBPA,
        WA_VBPA_AQUA TYPE VBPA.

  DATA: LT_KNA1 TYPE TABLE OF KNA1.



  DATA: VAR_X     TYPE C,
        STCD_STR  TYPE C LENGTH 9,
        STCD_CONC TYPE C LENGTH 4,
        VAR_TABIX TYPE SY-TABIX.

  CLEAR: IT_ZLEST0098[], IT_ZLEST0098.

  GL_VBPA_AQUA[] = GT_VBPA[].

  SORT GT_VBFA BY ERDAT.

  LOOP AT GT_VBFA INTO GW_VBFA.

    GL_ZLEST0098-ERDAT          = GW_VBFA-ERDAT.
    GL_ZLEST0098-VBELN          = GW_VBFA-VBELN.
    GL_ZLEST0098-LFIMG          = GW_VBFA-RFMNG.
    GL_ZLEST0098-VBELV          = GW_VBFA-VBELV.

    READ TABLE GT_LIPS INTO GW_LIPS WITH KEY VBELN = GW_VBFA-VBELN.
    IF ( SY-SUBRC NE 0 ).
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = GW_LIPS-MATKL
      IMPORTING
        OUTPUT = GW_LIPS-MATKL.

    GL_ZLEST0098-MATNR          = GW_LIPS-MATNR.
    GL_ZLEST0098-MATKL          = GW_LIPS-MATKL.
    GL_ZLEST0098-WERKS          = GW_LIPS-WERKS.
    GL_ZLEST0098-SAFRA          = GW_LIPS-CHARG.

    "Buscar ultima data com cambio cadastrado.
    CLEAR: V_UKURS, V_ERDAT.

    SELECT UKURS INTO V_UKURS
      FROM TCURR
        WHERE GDATU EQ GW_VBFA-ERDAT_LEN
        AND KURST EQ 'B'
        AND FCURR EQ 'USD'
        AND TCURR EQ 'BRL'.

    ENDSELECT.

    IF V_UKURS IS INITIAL.

      V_ERDAT = GW_VBFA-ERDAT.

      CREATE OBJECT OBJ_ZCL_UTIL.

      CLEAR: VAR_DATA, V_DATA_AUX, V_UKURS.

      WHILE V_UKURS IS INITIAL.

        V_ERDAT = V_ERDAT - 1.

        OBJ_ZCL_UTIL->CONV_DATA_US_BR( EXPORTING I_DATA = V_ERDAT "20150805
                                       RECEIVING E_DATA = VAR_DATA ). "05082015



        "Trabsforma para o formato tcurr
        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            INPUT  = VAR_DATA
          IMPORTING
            OUTPUT = V_DATA_AUX.


        SELECT UKURS INTO V_UKURS
        FROM TCURR
          CLIENT SPECIFIED
          WHERE GDATU = V_DATA_AUX
          AND KURST EQ 'B'
          AND FCURR EQ 'USD'
          AND TCURR EQ 'BRL'.
        ENDSELECT.
      ENDWHILE.
    ENDIF.


    READ TABLE GT_VBAK INTO GW_VBAK WITH KEY VBELN = GW_VBFA-VBELV.
    IF ( SY-SUBRC NE 0 ).
      CONTINUE.
    ENDIF.

    READ TABLE GT_ZSDT0001 INTO GW_ZSDT0001 WITH KEY DOC_REM = GW_LIPS-VBELN.
    GL_ZLEST0098-NR_ROMANEIO  = GW_ZSDT0001-NR_ROMANEIO.

    IF ( ( GW_VBAK-AUART <>  'ZRDC' ) AND  ( GW_VBAK-AUART <>  'ZRFL' ) ).
      CASE GW_ZSDT0001-TP_TRANSGENIA.
        WHEN: 'CO' OR 'C '.
          GL_ZLEST0098-TP_TRANSGENIA = 'NAO'.
        WHEN OTHERS.
          GL_ZLEST0098-TP_TRANSGENIA = 'SIM'.
      ENDCASE.
    ELSE.
      IF GW_VBAK-KVGR3 = 'C'.
        GL_ZLEST0098-TP_TRANSGENIA = 'NAO'.
      ELSE.
        IF GW_VBAK-KVGR3 = 'R'.
          GL_ZLEST0098-TP_TRANSGENIA = 'SIM'.
        ENDIF.
      ENDIF.
    ENDIF.

*    IF GW_VBAK-FLAG = 'X'.
*
*      READ TABLE GT_ZCOT0007 INTO GW_ZCOT0007 WITH  KEY VBELN_V = GW_VBAK-VBELN.
*
*      "Custo Estadia
*      GL_ZLEST0098-VLR_EST_R  =  GW_ZCOT0007-DMBTR.
*      GL_ZLEST0098-VLR_EST_US  = GW_ZCOT0007-DMBE2.
*
*    ENDIF.


    READ TABLE GT_VBPA_AUX INTO GW_VBPA_AUX WITH KEY VBELN = GW_VBFA-VBELV.
    GL_ZLEST0098-COD_COLETA	=	GW_VBPA_AUX-LIFNR.


    CASE GW_VBAK-AUART.
      WHEN: 'ZRDC' OR 'ZRFL' OR 'ZIND'.

        READ TABLE GT_VBPA INTO GW_VBPA WITH KEY VBELN = GW_VBFA-VBELV
                                                 PARVW = 'LR'.

        GL_ZLEST0098-COD_TRANSBORDO	=	GW_VBPA-KUNNR.

        CLEAR: GW_VBPA.
        READ TABLE GT_VBPA INTO GW_VBPA WITH KEY VBELN = GW_VBFA-VBELV
                                                PARVW = 'Z1'.

        GL_ZLEST0098-COD_PORTO      = GW_VBPA-LIFNR.


*        SELECT SINGLE * FROM ZSDT_DEPARA_DEPO INTO GL_ZSDT_DEPARA_DEPO WHERE WERKS EQ GW_LIPS-WERKS
*                                                                         AND LIFNR EQ GW_VBPA-LIFNR.

        CLEAR GL_ZSDT_DEPARA_DEPO.
        DATA(_OPERA) = 'RF'.
        IF GW_VBAK-AUART EQ 'ZIND'.
          _OPERA = 'RI'.
        ENDIF.

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  -   INICIO
*        CALL FUNCTION 'Z_BUSCA_DEPARA'
*          EXPORTING
*            I_WERKS          = GW_LIPS-WERKS
*            I_LIFNR          = GW_VBPA-LIFNR
*            I_OPERA          = _OPERA
*          IMPORTING
*            ZSDT_DEPARA_DEPO = GL_ZSDT_DEPARA_DEPO.

        ZCL_DEPARA_CENTRO_FIXO_VIRTUAL=>GET_DADOS_DEPARA(
          EXPORTING
            I_WERKS       = GW_LIPS-WERKS
            I_LIFNR       = GW_VBPA-LIFNR
            I_OPERACAO    = _OPERA
          IMPORTING
           E_SINGLE_DEPARA          = GL_ZSDT_DEPARA_DEPo  ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM



        GL_ZLEST0098-CENTRO_VIRTUAL	=	GL_ZSDT_DEPARA_DEPO-WERKS_V.
        SELECT SINGLE * FROM ZLEST0097 INTO GL_ZLEST0097 WHERE CENTRO_VIRTUAL EQ GL_ZSDT_DEPARA_DEPO-WERKS_V(2).
        GL_ZLEST0098-NOME_DESTINO	=	GL_ZLEST0097-DESCR_CENTRO.

        "Aquaviario - inicio
        READ TABLE GT_ZLEST0060 INTO GW_ZLEST0060 WITH KEY DOC_REM = GW_LIPS-VBELN.
        IF ( SY-SUBRC EQ 0 ).

          READ TABLE GT_ZLEST0061 INTO GW_ZLEST0061 WITH KEY DOCNUM = GW_ZLEST0060-DOCNUM.
          CASE GW_ZLEST0061-WAERK.

            WHEN: 'BRL'.
              GL_ZLEST0098-PRECO_AQUA_BRL = ( ( GW_LIPS-NTGEW * GW_ZLEST0061-NETPR ) / 1000 ).
              GL_ZLEST0098-PRECO_AQUA_US  = GL_ZLEST0098-PRECO_AQUA_BRL / GW_ZLEST0061-TAX_DOLAR.
            WHEN: 'USD'.
              GL_ZLEST0098-PRECO_AQUA_US   = ( ( GW_LIPS-NTGEW * GW_ZLEST0061-NETPR ) / 1000 ).
              GL_ZLEST0098-PRECO_AQUA_BRL  = GL_ZLEST0098-PRECO_AQUA_US * GW_ZLEST0061-TAX_DOLAR.

          ENDCASE.

        ELSE.

          READ TABLE GT_VBPA INTO WA_VBPA_AQUA WITH KEY VBELN = GW_VBFA-VBELV
                                                        PARVW = 'LR'.

          CASE WA_VBPA_AQUA-KUNNR.

            WHEN: '0000001003' OR '0000000161'.

              CLEAR: WA_VBPA_AQUA.
              READ TABLE GT_VBPA INTO WA_VBPA_AQUA WITH KEY VBELN = GW_VBFA-VBELV
                                                            PARVW = 'AG'.

              SELECT SINGLE * FROM ZLEST0055 INTO GL_ZLEST0055 WHERE MATKL  EQ GW_LIPS-MATKL
                                                                 AND VKORG  EQ '0010'
                                                                 AND VTWEG  EQ '10'
                                                                 AND SPART  EQ '08'
                                                                 AND STATUS EQ '1'
                                                                 AND AUART  IN ('ZTAB','ZTAM','ZTAG','ZTAF')
                                                                 AND KUNNR  EQ WA_VBPA_AQUA-KUNNR.

              IF  ( SY-SUBRC EQ 0 ).

                CASE GL_ZLEST0055-WAERK.
                  WHEN: 'BRL'.
                    GL_ZLEST0098-PRECO_AQUA_BRL = ( ( GW_LIPS-NTGEW * GL_ZLEST0055-NETPR ) / 1000 ).
                    GL_ZLEST0098-PRECO_AQUA_US  = GL_ZLEST0098-PRECO_AQUA_BRL / V_UKURS. "GW_TCURR-UKURS.
                  WHEN: 'USD'.
                    GL_ZLEST0098-PRECO_AQUA_US   = ( ( GW_LIPS-NTGEW * GL_ZLEST0055-NETPR ) / 1000 ).
                    GL_ZLEST0098-PRECO_AQUA_BRL  = GL_ZLEST0098-PRECO_AQUA_US * V_UKURS.  "GW_TCURR-UKURS.
                ENDCASE.

              ELSE.
                SELECT SINGLE * FROM KNA1 INTO GL_KNA1 WHERE KUNNR EQ WA_VBPA_AQUA-KUNNR.
                CONCATENATE GL_KNA1-STCD1(8) '%' INTO STCD_STR.

                SELECT * FROM KNA1
                  INTO TABLE LT_KNA1
                WHERE STCD1 LIKE STCD_STR.

                CHECK NOT LT_KNA1[] IS INITIAL.
                CLEAR: GL_KNA1.

                LOOP AT LT_KNA1 INTO GL_KNA1.

                  VAR_TABIX = SY-TABIX.

                  CLEAR: STCD_CONC.
                  CONCATENATE GL_KNA1-STCD1+8(1) GL_KNA1-STCD1+9(1) GL_KNA1-STCD1+10(1) GL_KNA1-STCD1+11(1) INTO STCD_CONC.

                  IF ( STCD_CONC NE '0001' ).
                    DELETE LT_KNA1 INDEX VAR_TABIX.
                  ELSE.

                    CLEAR: GL_ZLEST0055.
                    SELECT SINGLE * FROM ZLEST0055 INTO GL_ZLEST0055 WHERE MATKL  EQ GW_LIPS-MATKL
                                                                       AND VKORG  EQ '0010'
                                                                       AND VTWEG  EQ '10'
                                                                       AND SPART  EQ '08'
                                                                       AND STATUS EQ '1'
                                                                       AND AUART  IN ('ZTAB','ZTAM','ZTAG','ZTAF')
                                                                       AND KUNNR  EQ GL_KNA1-KUNNR.


                    CASE GL_ZLEST0055-WAERK.
                      WHEN: 'BRL'.
                        GL_ZLEST0098-PRECO_AQUA_BRL = ( ( GW_LIPS-NTGEW * GL_ZLEST0055-NETPR ) / 1000 ).
                        GL_ZLEST0098-PRECO_AQUA_US  = GL_ZLEST0098-PRECO_AQUA_BRL / V_UKURS. "GW_TCURR-UKURS.
                      WHEN: 'USD'.
                        GL_ZLEST0098-PRECO_AQUA_US   = ( ( GW_LIPS-NTGEW * GL_ZLEST0055-NETPR ) / 1000 ).
                        GL_ZLEST0098-PRECO_AQUA_BRL  = GL_ZLEST0098-PRECO_AQUA_US * V_UKURS. "GW_TCURR-UKURS.

                    ENDCASE.
                  ENDIF.
                ENDLOOP.
              ENDIF.

            WHEN: '0000111174' OR '0000112504'.

              CLEAR: WA_VBPA_AQUA.
              READ TABLE GT_VBPA INTO WA_VBPA_AQUA WITH KEY VBELN = GW_VBFA-VBELV
                                                            PARVW = 'AG'.

              SELECT SINGLE * FROM ZLEST0055 INTO GL_ZLEST0055 WHERE MATKL  EQ GW_LIPS-MATKL
                                                                 AND VKORG  EQ '0039'
                                                                 AND VTWEG  EQ '10'
                                                                 AND SPART  EQ '08'
                                                                 AND STATUS EQ '1'
                                                                 AND AUART  IN ('ZTAB','ZTAM','ZTAG','ZTAF')
                                                                 AND KUNNR  EQ WA_VBPA_AQUA-KUNNR.



              IF  ( SY-SUBRC EQ 0 ).

                CASE GL_ZLEST0055-WAERK.
                  WHEN: 'BRL'.
                    GL_ZLEST0098-PRECO_AQUA_BRL = ( ( GW_LIPS-NTGEW * GL_ZLEST0055-NETPR ) / 1000 ).
                    GL_ZLEST0098-PRECO_AQUA_US  = GL_ZLEST0098-PRECO_AQUA_BRL / V_UKURS."GW_TCURR-UKURS.
                  WHEN: 'USD'.
                    GL_ZLEST0098-PRECO_AQUA_US   = ( ( GW_LIPS-NTGEW * GL_ZLEST0055-NETPR ) / 1000 ).
                    GL_ZLEST0098-PRECO_AQUA_BRL  = GL_ZLEST0098-PRECO_AQUA_US * V_UKURS. "GW_TCURR-UKURS.
                ENDCASE.

              ELSE.
                SELECT SINGLE * FROM KNA1 INTO GL_KNA1 WHERE KUNNR EQ WA_VBPA_AQUA-KUNNR.
                CONCATENATE GL_KNA1-STCD1(8) '%' INTO STCD_STR.

                SELECT * FROM KNA1
                  INTO TABLE LT_KNA1
                WHERE STCD1 LIKE STCD_STR.

                CHECK NOT LT_KNA1[] IS INITIAL.
                CLEAR: GL_KNA1.

                LOOP AT LT_KNA1 INTO GL_KNA1.

                  VAR_TABIX = SY-TABIX.

                  CLEAR: STCD_CONC.
                  CONCATENATE GL_KNA1-STCD1+8(1) GL_KNA1-STCD1+9(1) GL_KNA1-STCD1+10(1) GL_KNA1-STCD1+11(1) INTO STCD_CONC.

                  IF ( STCD_CONC NE '0001' ).
                    DELETE LT_KNA1 INDEX VAR_TABIX.
                  ELSE.

                    CLEAR: GL_ZLEST0055.
                    SELECT SINGLE * FROM ZLEST0055 INTO GL_ZLEST0055 WHERE MATKL  EQ GW_LIPS-MATKL
                                                                       AND VKORG  EQ '0039'
                                                                       AND VTWEG  EQ '10'
                                                                       AND SPART  EQ '08'
                                                                       AND STATUS EQ '1'
                                                                       AND AUART  IN ('ZTAB','ZTAM','ZTAG','ZTAF')
                                                                       AND KUNNR  EQ GL_KNA1-KUNNR.
                    CASE GL_ZLEST0055-WAERK.
                      WHEN: 'BRL'.
                        GL_ZLEST0098-PRECO_AQUA_BRL = ( ( GW_LIPS-NTGEW * GL_ZLEST0055-NETPR ) / 1000 ).
                        GL_ZLEST0098-PRECO_AQUA_US  = GL_ZLEST0098-PRECO_AQUA_BRL / V_UKURS. "GW_TCURR-UKURS.
                      WHEN: 'USD'.
                        GL_ZLEST0098-PRECO_AQUA_US   = ( ( GW_LIPS-NTGEW * GL_ZLEST0055-NETPR ) / 1000 ).
                        GL_ZLEST0098-PRECO_AQUA_BRL  = GL_ZLEST0098-PRECO_AQUA_US * V_UKURS. "GW_TCURR-UKURS.

                    ENDCASE.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.

        ENDIF. "Aquaviario  - fim

    ENDCASE.


    GL_ZLEST0098-AUART          = GW_VBAK-AUART.
    GL_ZLEST0098-COD_CLIENTE    = GW_VBAK-KUNNR.

    READ TABLE GT_VBKD INTO GW_VBKD WITH KEY VBELN = GW_VBAK-VBELN.
    GL_ZLEST0098-INCO1 = GW_VBKD-INCO1.

    READ TABLE GT_VTTP INTO GW_VTTP WITH KEY VBELN = GW_LIPS-VBELN.

    READ TABLE GT_VTTK INTO GW_VTTK WITH KEY TKNUM = GW_VTTP-TKNUM.

    READ TABLE GT_VTFA INTO GW_VTFA WITH KEY VBELV = GW_VTTK-TKNUM.

    READ TABLE GT_VFKP INTO GW_VFKP WITH KEY FKNUM = GW_VTFA-VBELN.

    LOOP AT GT_KONV  INTO GW_KONV WHERE KNUMV = GW_VFKP-KNUMV  .

      IF  GW_KONV-KSCHL = 'ZPED'.
        GL_ZLEST0098-VLR_PED_R   = GW_KONV-KWERT.
        GL_ZLEST0098-VLR_PED_US  = GW_KONV-KWERT / V_UKURS. "GW_TCURR-UKURS.

      ELSE.
        GL_ZLEST0098-VLR_FRETE_BRL  = GW_KONV-KWERT.

        IF NOT ( GW_KONV-KWERT IS INITIAL ).
          GL_ZLEST0098-VLR_FRETE_US   = GW_KONV-KWERT  / V_UKURS. "GW_TCURR-UKURS.
        ENDIF.

      ENDIF.
    ENDLOOP.

    READ TABLE GT_KNA1 INTO GW_KNA1 WITH KEY KUNNR  = GL_ZLEST0098-COD_TRANSBORDO.
    READ TABLE GT_LFA1 INTO GW_LFA1 WITH KEY LIFNR  = GL_ZLEST0098-COD_PORTO.

    REFRESH: GT_ZLEST0070[].
    SELECT * FROM ZLEST0070
      INTO TABLE GT_ZLEST0070
    WHERE DOMICILIO_ORIGEM  EQ GW_KNA1-TXJCD
     AND DOMICILIO_DESTIN  EQ GW_LFA1-TXJCD
     AND TIPO              IN ('P','C').

    CLEAR: VAR_TABIX.
    LOOP AT GT_ZLEST0070 INTO GW_ZLEST0070.
      VAR_TABIX = SY-TABIX.
      IF ( GW_ZLEST0070-DT_INICIO+4(2) NE GW_VBFA-ERDAT+4(2) ).
        DELETE GT_ZLEST0070 INDEX VAR_TABIX.
      ELSE.

        CASE GW_ZLEST0070-TIPO.
          WHEN: 'P'.

            IF NOT ( GW_ZLEST0070-NETPR IS INITIAL ) AND NOT ( GW_ZLEST0070-PESO IS INITIAL ).
              GL_ZLEST0098-PRECO_FERRO_BRL = ( ( GW_LIPS-NTGEW * GW_ZLEST0070-NETPR ) / 1000 ).
              GL_ZLEST0098-PRECO_FERRO_US  = ( GL_ZLEST0098-PRECO_FERRO_BRL  / V_UKURS ). "GW_TCURR-UKURS ).
            ENDIF.


          WHEN: 'C'.
            IF NOT ( GW_ZLEST0070-NETPR IS INITIAL ) AND NOT ( GW_ZLEST0070-PESO IS INITIAL ).
              GL_ZLEST0098-PRECO_FERRO_BRL = ( ( GW_LIPS-NTGEW * GW_ZLEST0070-NETPR ) / 1000 ).
              GL_ZLEST0098-PRECO_FERRO_US  = ( GL_ZLEST0098-PRECO_FERRO_BRL  / V_UKURS ). "GW_TCURR-UKURS ).
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    GL_ZLEST0098-DT_ATUAL   = SY-DATUM.
    GL_ZLEST0098-HR_ATUAL   = SY-UZEIT.

    APPEND GL_ZLEST0098 TO IT_ZLEST0098.

    CLEAR:  GW_VBFA, GW_LIPS, GW_VBAK, GW_VBPA, GW_MARA,
            GW_ZSDT0001, GW_VTTP, GW_VTTK, GW_VTFA, GW_VFKP,
            GW_KONV, GW_TCURR, GW_KNA1, GW_LFA1, GW_ZLEST0070,
            GW_ZLEST0055, GL_ZLEST0097, GL_ZLEST0098, GL_ZLEST0055,
            GL_VBPA_AQUA, WA_VBPA_AQUA.

  ENDLOOP.

  "Pedido
  LOOP AT GT_EKKO INTO WA_EKKO.

    CLEAR: WA_EKPA.
    "Local de Coleta
    READ TABLE GT_EKPA INTO WA_EKPA WITH KEY EBELN = WA_EKKO-EBELN  BINARY SEARCH.

    "Itens do Pedido
    LOOP AT GT_EKPO INTO WA_EKPO WHERE EBELN EQ WA_EKKO-EBELN.

      "Remessas
      LOOP AT GT_EKBE INTO WA_EKBE WHERE EBELN EQ WA_EKPO-EBELN AND EBELP EQ WA_EKPO-EBELP.

        CLEAR: GL_ZLEST0098.
        GL_ZLEST0098-BSART       = WA_EKKO-BSART.
        GL_ZLEST0098-INCO1       = WA_EKPO-INCO1.
        GL_ZLEST0098-ERDAT       = WA_EKBE-BUDAT.
        GL_ZLEST0098-VBELV       = WA_EKBE-EBELN.
        GL_ZLEST0098-VBELN       = WA_EKBE-BELNR.

        IF WA_EKPA-LIFN2 IS NOT INITIAL.
          GL_ZLEST0098-COD_COLETA  = WA_EKPA-LIFN2.
        ELSE.
          GL_ZLEST0098-COD_COLETA  = WA_EKKO-RESWK.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = GL_ZLEST0098-COD_COLETA
            IMPORTING
              OUTPUT = GL_ZLEST0098-COD_COLETA.

        ENDIF.
        GL_ZLEST0098-COD_CLIENTE = WA_EKPO-WERKS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = GL_ZLEST0098-COD_CLIENTE
          IMPORTING
            OUTPUT = GL_ZLEST0098-COD_CLIENTE.

        "Romaneio de Saída
        READ TABLE GT_ZSDT0001_MM INTO WA_ZSDT0001_MM WITH KEY DOC_REM = WA_EKBE-BELNR BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          GL_ZLEST0098-NR_ROMANEIO  = WA_ZSDT0001_MM-NR_ROMANEIO.
          CASE WA_ZSDT0001_MM-TP_TRANSGENIA.
            WHEN: 'CO' OR 'C '.
              GL_ZLEST0098-TP_TRANSGENIA = 'NAO'.
            WHEN OTHERS.
              GL_ZLEST0098-TP_TRANSGENIA = 'SIM'.
          ENDCASE.
        ENDIF.

        "Informações de Remessa
        READ TABLE GT_LIPS_MM INTO WA_LIPS_MM WITH KEY VBELN = WA_EKBE-BELNR BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = GW_LIPS-MATKL
            IMPORTING
              OUTPUT = GW_LIPS-MATKL.

          GL_ZLEST0098-LFIMG  = WA_LIPS_MM-LFIMG.
          GL_ZLEST0098-MATNR  = WA_LIPS_MM-MATNR.
          GL_ZLEST0098-MATKL  = WA_LIPS_MM-MATKL.
          GL_ZLEST0098-WERKS  = WA_LIPS_MM-WERKS.
          GL_ZLEST0098-SAFRA  = WA_LIPS_MM-CHARG.
        ENDIF.

        CLEAR: V_UKURS, V_ERDAT.

        READ TABLE GT_TCURR INTO WA_TCURR WITH KEY GDATU = WA_EKBE-ERDAT_LEN BINARY SEARCH.
        IF SY-SUBRC IS NOT INITIAL.
          V_ERDAT = WA_EKBE-BUDAT.

          IF OBJ_ZCL_UTIL2 IS INITIAL.
            CREATE OBJECT OBJ_ZCL_UTIL2.
          ENDIF.

          WHILE WA_TCURR IS INITIAL.
            V_ERDAT = V_ERDAT - 1.
            OBJ_ZCL_UTIL2->CONV_DATA_US_BR( EXPORTING I_DATA = V_ERDAT
                                            RECEIVING E_DATA = VAR_DATA ).

            CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
              EXPORTING
                INPUT  = VAR_DATA
              IMPORTING
                OUTPUT = V_DATA_AUX.

            READ TABLE GT_TCURR INTO WA_TCURR WITH KEY GDATU = V_DATA_AUX BINARY SEARCH.
          ENDWHILE.
        ENDIF.

        GL_ZLEST0098-VLR_PED_R     = 0.
        GL_ZLEST0098-VLR_PED_US    = 0.
        GL_ZLEST0098-VLR_FRETE_BRL = 0.
        GL_ZLEST0098-VLR_FRETE_US  = 0.

        READ TABLE GT_VTTP_MM INTO WA_VTTP_MM WITH KEY VBELN = WA_EKBE-BELNR BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          LOOP AT GT_VFKP_MM INTO WA_VFKP_MM WHERE REBEL EQ WA_VTTP_MM-TKNUM.
            LOOP AT GT_KONV_MM INTO WA_KONV_MM WHERE KNUMV EQ WA_VFKP_MM-KNUMV.
              IF  WA_KONV_MM-KSCHL = 'ZPED'.
                ADD WA_KONV_MM-KWERT TO GL_ZLEST0098-VLR_PED_R.
              ELSE.
                ADD WA_KONV_MM-KWERT TO GL_ZLEST0098-VLR_FRETE_BRL.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

          IF V_UKURS IS NOT INITIAL.
            IF GL_ZLEST0098-VLR_PED_R NE 0.
              GL_ZLEST0098-VLR_PED_US = GL_ZLEST0098-VLR_PED_R / V_UKURS.
            ENDIF.
            IF GL_ZLEST0098-VLR_FRETE_BRL NE 0.
              GL_ZLEST0098-VLR_FRETE_US = GL_ZLEST0098-VLR_FRETE_BRL / V_UKURS.
            ENDIF.
          ENDIF.
        ENDIF.
        GL_ZLEST0098-DT_ATUAL = SY-DATUM.
        GL_ZLEST0098-HR_ATUAL = SY-UZEIT.
        APPEND GL_ZLEST0098 TO IT_ZLEST0098.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  MODIFY ZLEST0098 FROM TABLE IT_ZLEST0098.
  COMMIT WORK.

  " ESTADIAS.
  DATA: SVAR(8).

  SVAR = 'Estadia%'.
  SELECT ZCOT0007~VBELN  SUM( ZCOT0007~DMBTR ) SUM( ZCOT0007~DMBE2 )
    INTO TABLE GT_ZCOT0007
    FROM ZCOT0007
    INNER JOIN ZLEST0098
    ON  ZLEST0098~VBELN = ZCOT0007~VBELN
  WHERE ZCOT0007~BUKRS IN P_VKORG
    AND ZCOT0007~SGTXT LIKE SVAR
    GROUP BY ZCOT0007~VBELN.

  CLEAR GW_ZCOT0007.

  LOOP AT GT_ZCOT0007 INTO GW_ZCOT0007.
    UPDATE ZLEST0098
      SET   VLR_EST_R  =  GW_ZCOT0007-DMBTR
            VLR_EST_US =  GW_ZCOT0007-DMBE2
    WHERE VBELN = GW_ZCOT0007-VBELN.
  ENDLOOP.
  COMMIT WORK.

ENDFORM.                    " INSERIR

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_MM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS_MM .

  DATA: OBJ_ZCL_UTIL2 TYPE REF TO ZCL_UTIL.

  FIELD-SYMBOLS: <FS_EKBE> TYPE TY_EKBE.

  DATA: VAR_GDATU     TYPE SY-DATUM,
        VAR_DATA_CONV TYPE SY-DATUM,
        VAR_DATA      TYPE C LENGTH 10.

  IF SY-BATCH = 'X'.
    REFRESH: P_ERDAT, P_VKORG.

    P_ERDAT-LOW    = ( SY-DATUM ) - 30.
    P_ERDAT-HIGH   = SY-DATUM.
    P_ERDAT-SIGN   = 'I'.
    P_ERDAT-OPTION = 'BT'.
    APPEND P_ERDAT.

    P_VKORG-LOW    = '0001'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.

    P_VKORG-LOW    = '0015'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.

    P_VKORG-LOW    = '0018'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.
  ENDIF.

  "Busca Fluxo de Remessas
  SELECT EBELN EBELP BUDAT BELNR
    FROM EKBE
    INTO TABLE GT_EKBE
   WHERE VGABE EQ '8'
     AND BUDAT IN P_ERDAT.

  CHECK GT_EKBE[] IS NOT INITIAL.

  "Busca Pedidos de Transferência
  SELECT EBELN RESWK BSART
    INTO TABLE GT_EKKO
    FROM EKKO
     FOR ALL ENTRIES IN GT_EKBE
   WHERE EBELN EQ GT_EKBE-EBELN
     AND BSART EQ 'ZUB'
     AND BUKRS IN P_VKORG.

  CHECK GT_EKKO[] IS NOT INITIAL.

  "Busca Itens dos Pedidos
  SELECT EBELN EBELP WERKS INCO1
    INTO TABLE GT_EKPO
    FROM EKPO
     FOR ALL ENTRIES IN GT_EKKO
   WHERE EBELN EQ GT_EKKO-EBELN.

  "Busca Local de Coleta da Transferência
  SELECT EBELN LIFN2
    INTO TABLE GT_EKPA
    FROM EKPA
     FOR ALL ENTRIES IN GT_EKKO
   WHERE EBELN EQ GT_EKKO-EBELN.

  SORT GT_EKPA BY EBELN.


  CREATE OBJECT OBJ_ZCL_UTIL2.

  LOOP AT GT_EKBE ASSIGNING <FS_EKBE>.
    CLEAR: VAR_DATA, VAR_GDATU, VAR_DATA_CONV.

    OBJ_ZCL_UTIL2->CONV_DATA_US_BR( EXPORTING I_DATA = <FS_EKBE>-BUDAT
                                   RECEIVING E_DATA = VAR_DATA ).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = VAR_DATA
      IMPORTING
        OUTPUT = <FS_EKBE>-ERDAT_LEN.

    VAR_DATA_CONV = <FS_EKBE>-BUDAT - 1.

    OBJ_ZCL_UTIL2->CONV_DATA_US_BR( EXPORTING I_DATA = VAR_DATA_CONV
                                    RECEIVING E_DATA = VAR_DATA ).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        INPUT  = VAR_DATA
      IMPORTING
        OUTPUT = <FS_EKBE>-ERDAT_AUX.
  ENDLOOP.


  "Busca Remessas
  SELECT * FROM LIPS
    INTO TABLE GT_LIPS_MM
     FOR ALL ENTRIES IN GT_EKBE
   WHERE VBELN EQ GT_EKBE-BELNR
     AND WERKS IN P_WERKS.

  SORT GT_LIPS_MM BY VBELN.

  "Busca de Dados no Romaneio
  SELECT *
    FROM ZSDT0001
    INTO TABLE GT_ZSDT0001_MM
    FOR ALL ENTRIES IN GT_LIPS_MM
  WHERE TP_MOVIMENTO EQ 'E'
    AND DOC_REM      EQ GT_LIPS_MM-VBELN.

  SORT GT_ZSDT0001_MM BY DOC_REM.

  "Busca de Dados Custo de transporte
  SELECT *
    FROM VTTP
    INTO TABLE GT_VTTP_MM
    FOR ALL ENTRIES IN GT_LIPS_MM
  WHERE VBELN EQ GT_LIPS_MM-VBELN.

  SORT GT_VTTP_MM BY VBELN.

  IF SY-SUBRC IS INITIAL.

    "Cabeçalho transporte
    SELECT *
      FROM VTTK
      INTO TABLE GT_VTTK_MM
       FOR ALL ENTRIES IN GT_VTTP_MM
     WHERE TKNUM EQ GT_VTTP_MM-TKNUM
       AND VSART EQ '01'.

    SORT GT_VTTK_MM BY TKNUM.

    "Custos de frete: dados do item
    SELECT *
      FROM VFKP
      INTO TABLE GT_VFKP_MM
       FOR ALL ENTRIES IN GT_VTTK_MM
     WHERE REBEL EQ GT_VTTK_MM-TKNUM.

    "Condições (dados de operação)
    SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @GT_VFKP_MM WHERE KNUMV EQ @GT_VFKP_MM-KNUMV AND KSCHL IN ( 'ZFRE' , 'ZLOT' , 'ZPED' ) INTO CORRESPONDING FIELDS OF TABLE @GT_KONV_MM .
  ENDIF.

  SELECT *
    INTO TABLE GT_TCURR
    FROM TCURR
   WHERE KURST EQ 'B'
     AND FCURR EQ 'USD'
     AND TCURR EQ 'BRL'.

  SORT GT_TCURR BY GDATU.

ENDFORM.
