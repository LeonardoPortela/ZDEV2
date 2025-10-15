FUNCTION ZSDMF001_GERA_OV_COCKPIT_ZTRI.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_AUART) TYPE  VBAK-AUART
*"     REFERENCE(I_KUNNR) TYPE  ZSDT0089-KUNNR OPTIONAL
*"     REFERENCE(I_TXT_ORDEM) TYPE  ZSDT0089-TXT_ORDEM OPTIONAL
*"     REFERENCE(I_TXT_ITEM) TYPE  ZSDT0089-TXT_ITEM OPTIONAL
*"     REFERENCE(I_BILL_DOC) TYPE  BILL_DOC OPTIONAL
*"     REFERENCE(I_ACAO) TYPE  SY-UCOMM OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_VBELN) TYPE  ZSDT0090-VBELN
*"  TABLES
*"      IT_OV STRUCTURE  ZSDS015
*"      TE_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      IT_OBS STRUCTURE  TLINE OPTIONAL
*"----------------------------------------------------------------------
  TYPE-POOLS: RMDI.
  TYPES: BEGIN OF TY_HEADER,
           AUART TYPE VBAK-AUART,
           KNUMV TYPE VBAK-KNUMV,
           KUNNR TYPE VBAK-KUNNR,
           VKGRP TYPE VBAK-VKGRP,
           VKBUR TYPE VBAK-VKBUR,
           BSTNK TYPE VBAK-BSTNK,
           VKORG TYPE VBAK-VKORG,
           VTWEG TYPE VBAK-VTWEG,
           SPART TYPE VBAK-SPART,
           WAERK TYPE VBAK-WAERK,
           KURSK TYPE VBKD-KURSK,
           PRSDT TYPE VBKD-PRSDT,
           VSBED TYPE VBAK-VSBED,
           INCO1 TYPE VBKD-INCO1,
           ZTERM TYPE VBKD-ZTERM,
           ZLSCH TYPE VBKD-ZLSCH,
           VALDT TYPE VBKD-VALDT,
           VBELN TYPE VBAP-VBELN,
           POSNR TYPE VBAP-POSNR,
           WERKS TYPE VBAP-WERKS,
           MATNR TYPE VBAP-MATNR,
           GEWEI TYPE VBAP-GEWEI,
           CHARG TYPE VBAP-CHARG,
           LGORT TYPE VBAP-LGORT,
           VKAUS TYPE VBAP-VKAUS,
           VRKME TYPE VBAP-VRKME,
           KURRF TYPE VBKD-KURRF,
         END OF TY_HEADER,

         BEGIN OF TY_SUCESS,
           ETAPA TYPE SY-TABIX,
           DESC  TYPE CHAR30,
           ERRO  TYPE CHAR1,
           VBELN TYPE VBELN,
         END OF TY_SUCESS,

         BEGIN OF TY_KONV,
           KPOSN TYPE KONV-KPOSN,
           KBETR TYPE KONV-KBETR,
           KMEIN TYPE KONV-KMEIN,
         END OF TY_KONV,

         BEGIN OF TY_ITEMFAT,
           VBELN TYPE VBRP-VBELN,
           POSNR TYPE VBRP-POSNR,
           FKIMG TYPE VBRP-FKIMG,
           VRKME TYPE VBRP-VRKME,
           AUBEL TYPE VBRP-AUBEL,
           AUPOS TYPE VBRP-AUPOS,
           MATNR TYPE VBRP-MATNR,
           CHARG TYPE VBRP-CHARG,
         END OF TY_ITEMFAT.


  DATA: BEGIN OF IT_MSG OCCURS 0.
          INCLUDE STRUCTURE BDCMSGCOLL.
        DATA: END OF IT_MSG.

  TYPES BEGIN OF TY_0090.
          INCLUDE STRUCTURE ZSDT0090.
  TYPES POSNR TYPE VBAP-POSNR.
  TYPES END OF TY_0090.

  DATA: T_SUCCESS TYPE STANDARD TABLE OF BAPIVBRKSUCCESS WITH HEADER LINE.
  DATA: T_BILLING TYPE STANDARD TABLE OF BAPIVBRK WITH HEADER LINE.
  DATA: T_RETURN TYPE STANDARD TABLE OF BAPIRETURN1 WITH HEADER LINE.

  DATA: WL_HEADER         TYPE TY_HEADER,
        TL_ITENS          TYPE TABLE OF TY_HEADER WITH HEADER LINE,
        TL_KONV           TYPE TABLE OF TY_KONV  WITH HEADER LINE,
        WA_ZSDT0041       TYPE          ZSDT0041,
        WA_ZSDT0090       TYPE          ZSDT0090,
        IT_ZSDT0089       TYPE TABLE OF ZSDT0089 WITH HEADER LINE,
        IT_ZSDT0090       TYPE TABLE OF ZSDT0090 WITH HEADER LINE,
        WL_HEADER_IN      TYPE BAPISDHD1,
        WL_HEADER_INX     TYPE BAPISDHD1X,
        WL_HEADER_INX2    TYPE BAPISDH1X,
        WL_VBELN          TYPE VBAK-VBELN,
        RT_VBELN          TYPE VBAK-VBELN,
        WG_DOCUMENTO(10),
        W_SEQ             TYPE I,
        W_DOC_SIMULACAO   TYPE ZSDT0041-DOC_SIMULACAO,

        TL_ITEMFAT        TYPE TABLE OF TY_ITEMFAT,
        WL_ITEMFAT        TYPE TY_ITEMFAT,

        WL_FIELDNAME      TYPE RMDI_NAME,
        WL_TEXT           TYPE RMDI_DDTXT,
        TL_VBUV           TYPE  TABLE OF VBUV WITH HEADER LINE,
        TL_OVS            TYPE TABLE OF TY_OVS WITH HEADER LINE,

        TL_ITEMS_IN       TYPE TABLE OF BAPISDITM  WITH HEADER LINE,
        TL_HEADER         TYPE THEAD,
        TL_TEXT           TYPE STANDARD TABLE OF TLINE WITH HEADER LINE,
*        TL_ITEMS_IN_AUX   TYPE TABLE OF BAPISDITM  WITH HEADER LINE,
        TL_ITEMS_INX      TYPE TABLE OF BAPISDITMX WITH HEADER LINE,
        TL_PARTNERS       TYPE TABLE OF BAPIPARNR  WITH HEADER LINE,
*        TL_SCHEDULES      TYPE TABLE OF BAPISCHDL  WITH HEADER LINE,
        TL_SCHEDULES_IN   TYPE TABLE OF BAPISCHDL  WITH HEADER LINE,
*        TL_SCHEDULES_IN_AUX   TYPE TABLE OF BAPISCHDL  WITH HEADER LINE,
        TL_SCHEDULES_INX  TYPE TABLE OF BAPISCHDLX WITH HEADER LINE,
        TL_CONDITIONS_IN  TYPE TABLE OF BAPICOND   WITH HEADER LINE,
*        TL_CONDITIONS_IN_AUX  TYPE TABLE OF BAPICOND   WITH HEADER LINE,
        TL_CONDITIONS_INX TYPE TABLE OF BAPICONDX  WITH HEADER LINE,
        F_HEADINX         LIKE BAPISDH1X,
        TL_RETURN         TYPE TABLE OF BAPIRET2   WITH HEADER LINE,
        TL_RETURN_AUX     TYPE TABLE OF BAPIRET2   WITH HEADER LINE,
        TL_RETURN2        TYPE TABLE OF BAPIRET2   WITH HEADER LINE,
        TL_RETURN3        TYPE TABLE OF BAPIRET2   WITH HEADER LINE,
        TL_TEXT_IN        TYPE TABLE OF BAPISDTEXT WITH HEADER LINE,
        TL_SAIDA_EXEC     TYPE TABLE OF TY_SAIDA_EXEC WITH HEADER LINE,
        TL_MARA           TYPE TABLE OF MARA WITH HEADER LINE,
        WL_POSNR          TYPE SY-TABIX,
        WL_VLR_COVERT     TYPE DZMENG,
        TL_TEXTO          TYPE CATSXT_LONGTEXT_ITAB,
        WL_TEXTO          TYPE LINE OF CATSXT_LONGTEXT_ITAB,
*        wl_bapiparex  TYPE bapiparex         ,
        TL_BAPIPAREX      TYPE TABLE OF BAPIPAREX WITH HEADER LINE,
        WL_BAPE_VBAK      TYPE BAPE_VBAK,
        WL_BAPE_VBAKX     TYPE BAPE_VBAKX,
        WL_T100           TYPE T100,
        WL_ERRO(1),
        V_KUNNR           TYPE VBAK-KUNNR,
        P_0090            TYPE TY_0090,
        W_0090            TYPE ZSDT0090,
        IT_SUCESS         TYPE TABLE OF TY_SUCESS WITH HEADER LINE,
        SEQ               TYPE SY-TABIX.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  REFRESH: TL_ITEMS_IN, TL_ITEMS_INX,TL_PARTNERS, TL_SCHEDULES_IN, TL_SCHEDULES_INX,TL_OVS,TL_VBUV,
           TL_CONDITIONS_IN, TL_CONDITIONS_INX, TL_RETURN,TL_RETURN2,TL_RETURN3, TL_TEXTO,
           TL_TEXT_IN, TL_SAIDA_EXEC, ESTRUTURA, EVENTS, TL_MARA, TL_BAPIPAREX,IT_ZSDT0090, TL_ITEMFAT.
  CLEAR: WL_HEADER, WL_HEADER_IN, WL_HEADER_INX, TL_ITEMS_IN, TL_ITEMS_INX,TL_PARTNERS,TL_OVS,TL_VBUV,
          TL_SCHEDULES_IN, TL_SCHEDULES_INX, TL_CONDITIONS_IN, TL_CONDITIONS_INX, TL_RETURN,TL_RETURN2,TL_RETURN3,
         WL_TEXTO, TL_TEXT_IN, TL_SAIDA_EXEC, WA_ESTRUTURA, XS_EVENTS, TL_MARA,
         WL_BAPE_VBAK, WL_BAPE_VBAKX, TL_BAPIPAREX, WL_ITEMFAT.

  SELECT VBAK~AUART VBAK~KNUMV VBAK~KUNNR VBAK~VKGRP VBAK~VKBUR
         VBAK~BSTNK VBAK~VKORG VBAK~VTWEG VBAK~SPART VBAK~WAERK
         VBKD~KURSK VBKD~PRSDT VBAK~VSBED VBKD~INCO1 VBKD~ZTERM
         VBKD~ZLSCH VBKD~VALDT VBAP~VBELN VBAP~POSNR VBAP~WERKS
         VBAP~MATNR VBAP~GEWEI VBAP~CHARG VBAP~LGORT VBAP~VKAUS
         VBAP~VRKME VBKD~KURRF
    FROM VBAK
    INNER JOIN VBAP ON  VBAP~VBELN = VBAK~VBELN
    INNER JOIN VBKD ON  VBKD~VBELN = VBAP~VBELN
    "AND vbkd~posnr = vbap~posnr
    INTO TABLE TL_ITENS
    FOR ALL ENTRIES IN IT_OV
    WHERE VBAK~VBELN = IT_OV-VBELN.

  IF TL_ITENS[] IS INITIAL.
    EXIT.
  ENDIF.

  IF I_AUART EQ 'ZTRI'.

    LOOP AT TL_ITENS INTO WL_HEADER.
      IF WL_HEADER-AUART NE 'ZREM'.
        V_KUNNR = WL_HEADER-KUNNR.
      ENDIF.
    ENDLOOP.

    READ TABLE TL_ITENS INTO WL_HEADER WITH KEY AUART = 'ZREM'.
    WL_HEADER-KUNNR = V_KUNNR.

  ELSE.
    READ TABLE TL_ITENS INTO WL_HEADER INDEX 1.
  ENDIF.

  TRY.

CL_PRC_RESULT_FACTORY=>GET_INSTANCE( )->GET_PRC_RESULT( )->GET_PRICE_ELEMENT_DB(
  EXPORTING IT_SELECTION_ATTRIBUTE = VALUE #(
 ( fieldname = 'KNUMV' value = WL_HEADER-KNUMV )
 ( fieldname = 'KSCHL' value = 'PR00' )
 )
  IMPORTING ET_PRC_ELEMENT_CLASSIC_FORMAT = DATA(ETL192C2R3965) ).
  CLEAR TL_KONV.
  TYPES: BEGIN OF TYL192C2R7767,
    KPOSN TYPE KONV-KPOSN,
    KBETR TYPE KONV-KBETR,
    KMEIN TYPE KONV-KMEIN,
  END OF TYL192C2R7767.
  DATA: LML192C2R1052 TYPE TYL192C2R7767,
        LWL192C2R810 LIKE LINE OF TL_KONV.
  LOOP AT ETL192C2R3965 REFERENCE INTO DATA(LDRL192C2R2688).
    LML192C2R1052-KPOSN = LDRL192C2R2688->KPOSN.
    LML192C2R1052-KBETR = LDRL192C2R2688->KBETR.
    LML192C2R1052-KMEIN = LDRL192C2R2688->KMEIN.
    LWL192C2R810 = LML192C2R1052.
    APPEND LWL192C2R810 TO TL_KONV.
  ENDLOOP.
CATCH CX_PRC_RESULT .
  SY-SUBRC = 4.
ENDTRY.

  SELECT VBELN POSNR FKIMG VRKME
         AUBEL AUPOS MATNR CHARG
    FROM VBRP
    INTO TABLE TL_ITEMFAT
    WHERE VBELN EQ I_BILL_DOC.

  IF I_KUNNR IS NOT INITIAL.

    LOOP AT IT_OBS.
      TL_TEXT_IN-TEXT_LINE(72) = IT_OBS-TDLINE.
      TL_TEXT_IN-TEXT_ID    = '0001'.
      TL_TEXT_IN-LANGU      = SY-LANGU.
      TL_TEXT_IN-FORMAT_COL = '/'.
      APPEND TL_TEXT_IN.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        IM_TITLE = 'Texto para Ordem de Venda'
      CHANGING
        CH_TEXT  = TL_TEXTO.

    LOOP AT TL_TEXTO INTO WL_TEXTO.
      TL_TEXT_IN-TEXT_LINE(72) = WL_TEXTO.
      TL_TEXT_IN-TEXT_ID    = '0002'.
      TL_TEXT_IN-LANGU      = SY-LANGU.
      TL_TEXT_IN-FORMAT_COL = '/'.
      APPEND TL_TEXT_IN.
    ENDLOOP.
  ENDIF.

* Extension - Campo ZPESAGEM
  CLEAR TL_BAPIPAREX.
  TL_BAPIPAREX-STRUCTURE     = 'BAPE_VBAK'.


*SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980 - INICIO

"WL_BAPE_VBAK-ZPESAGEM = '02'.

* SELECT
*    'I' AS sign,
*    'EQ' AS option,
*    valfrom AS low,
*    valfrom AS high
* INTO TABLE @DATA(r_tpdoc)
* FROM setleaf
* WHERE setname = 'MV45AFZZ_WERKS'
* ORDER BY low ASCENDING.
*
*select single mtart from mara into @data(V_mtart) where matnr = @wl_header-matnr.
*
*IF wl_header-werks IN r_tpdoc AND V_mtart EQ 'ZFER'.
*  wl_bape_vbak-zpesagem = '01'.
*  ELSE.
*    wl_bape_vbak-zpesagem = '02'.
*    ENDIF.
wl_bape_vbak-zpesagem = obj_zcl_util_sd->set_tp_pesagem_ov_simulador(
                                                                      centro = wl_header-werks
                                                                      matnr = wl_header-matnr
                                                                     ).

*SD-ZSDT0087-AlteracaoProcessoGerarOVFertilizante - BG #83980 - fim
  TL_BAPIPAREX-VALUEPART1 = WL_BAPE_VBAK.
  APPEND TL_BAPIPAREX.
  CLEAR TL_BAPIPAREX.
  TL_BAPIPAREX-STRUCTURE     = 'BAPE_VBAKX'.
  WL_BAPE_VBAKX-ZPESAGEM = 'X'.
  TL_BAPIPAREX-VALUEPART1 = WL_BAPE_VBAKX.
  APPEND  TL_BAPIPAREX.

  READ TABLE IT_OV INDEX 1.

  WL_HEADER_IN-SALES_ORG  = WL_HEADER-VKORG.
  WL_HEADER_IN-DISTR_CHAN = WL_HEADER-VTWEG.
  WL_HEADER_IN-SALES_OFF  = WL_HEADER-VKBUR.
  WL_HEADER_IN-SALES_GRP  = WL_HEADER-VKGRP.
  WL_HEADER_IN-PURCH_DATE = SY-DATUM.
  WL_HEADER_IN-PURCH_NO_C = WL_HEADER-BSTNK.
  WL_HEADER_IN-CURRENCY   = WL_HEADER-WAERK.
  WL_HEADER_IN-PYMT_METH  = WL_HEADER-ZLSCH.
  WL_HEADER_IN-FIX_VAL_DY = WL_HEADER-VALDT.
  WL_HEADER_IN-EXRATE_FI  = WL_HEADER-KURRF.
  WL_HEADER_IN-PMNTTRMS   = WL_HEADER-ZTERM.
*  WL_HEADER_IN-INCOTERMS1 = SWITCH #( I_AUART WHEN 'ZFTE'
*                                                OR 'ZDEF'
*                                                OR 'ZSEM'
*                                                OR 'ZOFE'
*                                                OR 'ZODF'
*                                                OR 'ZOSM'
*                                              THEN IT_OV-INCO1
*                                              ELSE WL_HEADER-INCO1 ).

  WL_HEADER_IN-INCOTERMS1 = SWITCH #( I_ACAO WHEN 'TRANSF'
                                               THEN IT_OV-INCO1
                                             ELSE WL_HEADER-INCO1 ).

  CASE WL_HEADER-INCO1.
    WHEN 'CIF'.
      IF IT_OV-INCO1 EQ 'CPT'.
        DATA(V_DISPARO) = ABAP_TRUE.
      ENDIF.
    WHEN 'CPT'.
      IF IT_OV-INCO1 EQ 'CIF'.
        V_DISPARO = ABAP_TRUE.
      ENDIF.
  ENDCASE.

  IF WL_HEADER-INCO1 = IT_OV-INCO1.
    V_DISPARO = ABAP_FALSE.
  ENDIF.

  WL_HEADER_IN-INCOTERMS2 = WL_HEADER_IN-INCOTERMS1.
  WL_HEADER_IN-DIVISION   = WL_HEADER-SPART.
  WL_HEADER_IN-DOC_TYPE   = I_AUART.

  IF I_AUART EQ 'ZFUT'.
    WL_HEADER_IN-EXRATE_FI = IT_OV-KURSF.
    WL_HEADER_IN-EXCHG_RATE = IT_OV-KURSF.
  ENDIF.

  WL_HEADER_IN-REF_DOC_L  = WL_HEADER-VBELN.

  IF I_BILL_DOC IS NOT INITIAL.
    WL_HEADER_IN-REFOBJTYPE = 'VBRK'.
    WL_HEADER_IN-REFOBJKEY  = I_BILL_DOC.
    WL_HEADER_IN-REFDOCTYPE = 'M'.
    WL_HEADER_IN-REFDOC_CAT = 'M'.
    WL_HEADER_IN-REF_DOC    = I_BILL_DOC.
    WL_HEADER_IN-REF_DOC_L  = I_BILL_DOC.
  ENDIF.

  IF I_KUNNR IS NOT INITIAL.
    WL_HEADER-KUNNR = I_KUNNR.
  ENDIF.

*---Monta dados de Parceiro
  TL_PARTNERS-PARTN_ROLE = 'LR'.
  TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
  APPEND TL_PARTNERS.
  CLEAR TL_PARTNERS.

  TL_PARTNERS-PARTN_ROLE = 'PC'.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WL_HEADER-WERKS
    IMPORTING
      OUTPUT = TL_PARTNERS-PARTN_NUMB.
  APPEND TL_PARTNERS.
  CLEAR TL_PARTNERS.

  TL_PARTNERS-PARTN_ROLE = 'RE'.
  TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
  APPEND TL_PARTNERS.
  CLEAR TL_PARTNERS.

  TL_PARTNERS-PARTN_ROLE = 'RG'.
  TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
  APPEND TL_PARTNERS.
  CLEAR TL_PARTNERS.

  TL_PARTNERS-PARTN_ROLE = 'WE'.
  TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
  APPEND TL_PARTNERS.
  CLEAR TL_PARTNERS.

  CLEAR WL_POSNR.
  REFRESH: TL_ITEMS_IN, TL_CONDITIONS_IN, TL_SCHEDULES_IN.

  SORT: IT_OV    BY VBELN POSNR,
        TL_KONV  BY KPOSN.

  IF I_AUART EQ 'ZTRI'.
    DELETE TL_ITENS WHERE AUART NE 'ZREM'.
  ENDIF.

*  LOOP AT TL_ITENS.
*
*    IF TL_ITENS-AUART EQ 'ZREM'.
*      READ TABLE IT_OV WITH KEY VBELN = TL_ITENS-VBELN
*                                POSNR = TL_ITENS-POSNR
*                                AUART = TL_ITENS-AUART BINARY SEARCH.
*      IF SY-SUBRC NE 0.
*        CONTINUE.
*      ENDIF.
*    ELSE.
*      READ TABLE IT_OV WITH KEY VBELN = TL_ITENS-VBELN
*                          POSNR = TL_ITENS-POSNR BINARY SEARCH.
*      IF SY-SUBRC NE 0.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

  LOOP AT TL_ITEMFAT INTO WL_ITEMFAT.

    READ TABLE TL_ITENS WITH KEY VBELN = WL_ITEMFAT-AUBEL
                                 POSNR = WL_ITEMFAT-AUPOS.

    ADD 1 TO WL_POSNR.
    READ TABLE TL_KONV WITH KEY KPOSN = TL_ITENS-POSNR BINARY SEARCH.
    TL_ITEMS_IN-STORE_LOC    = TL_ITENS-LGORT.

*    IF I_AUART EQ 'ZTRI'.
*      TL_ITEMS_IN-ITM_NUMBER   = TL_ITENS-POSNR.
*    ELSE.
*      TL_ITEMS_IN-ITM_NUMBER   = WL_POSNR * 10.
*    ENDIF.

*    TL_ITEMS_IN-ITM_NUMBER   = WL_ITEMFAT-POSNR.
    TL_ITEMS_IN-ITM_NUMBER   = WL_POSNR * 10.
"*---> 12/07/2023 - Migração S4 - LO
*    TL_ITEMS_IN-MATERIAL     = WL_ITEMFAT-MATNR.

   DATA(v_len) = strlen( WL_ITEMFAT-MATNR ).

   IF v_len > 18.
    TL_ITEMS_IN-MATERIAL_LONG     = WL_ITEMFAT-MATNR.
   ELSE.
    TL_ITEMS_IN-MATERIAL          = WL_ITEMFAT-MATNR.
   ENDIF.
"*<--- 12/07/2023 - Migração S4 - LO
    TL_ITEMS_IN-TARGET_QTY   = WL_ITEMFAT-FKIMG.
    TL_ITEMS_IN-TARGET_QU    = WL_ITEMFAT-VRKME.
    TL_ITEMS_IN-SALES_UNIT   = WL_ITEMFAT-VRKME.

    IF I_AUART EQ 'ZFUT'.
      TL_ITEMS_IN-USAGE_IND    = 'S'.
    ELSE.
      TL_ITEMS_IN-USAGE_IND    = 'I'.
    ENDIF.

    TL_ITEMS_IN-PLANT        = TL_ITENS-WERKS.
    TL_ITEMS_IN-BATCH        = WL_ITEMFAT-CHARG.
    TL_ITEMS_IN-SHIP_POINT   = TL_ITENS-WERKS.
    TL_ITEMS_IN-MATFRGTGRP   = '00000001'.
    "
    IF I_BILL_DOC IS NOT INITIAL.
      TL_ITEMS_IN-REF_DOC_CA = 'M'.
      TL_ITEMS_IN-REFOBJTYPE = 'VBRK'.
      TL_ITEMS_IN-REFOBJKEY  = I_BILL_DOC.
      TL_ITEMS_IN-REF_DOC    = I_BILL_DOC.

      TL_ITEMS_IN-REF_DOC_IT = WL_ITEMFAT-POSNR.

*      IF I_AUART EQ 'ZTRI'.
*        TL_ITEMS_IN-REF_DOC_IT =  TL_ITENS-POSNR.
*      ELSE.
*        TL_ITEMS_IN-REF_DOC_IT =  WL_POSNR * 10.
*      ENDIF.

    ENDIF.
    APPEND TL_ITEMS_IN.

    CLEAR: TL_CONDITIONS_IN.
    TL_CONDITIONS_IN-ITM_NUMBER  = TL_ITEMS_IN-ITM_NUMBER .
    TL_CONDITIONS_IN-CURRENCY    = WL_HEADER-WAERK.

    DATA(COEFICIENTE_O) = ZCL_SOLICITACAO_OV=>GET_IMPOSTO(
                                                          _DIRECAO = 'O'
                                                          _VBELN   = TL_ITENS-VBELN
                                                          _POSNR   = TL_ITENS-POSNR
                                                       ).

    DATA(COEFICIENTE_D) = ZCL_SOLICITACAO_OV=>GET_IMPOSTO(
                                                          _CLIENTE    = WL_HEADER-KUNNR
                                                          _FORNECEDOR = CONV #( |{ TL_ITENS-WERKS ALPHA = IN }| )
                                                          _MATERIAL   = WL_ITEMFAT-MATNR
                                                          _TIPO_ORDEM = I_AUART
                                                          _DIRECAO    = 'D'
                                                          _WERKS      = TL_ITENS-WERKS    "<<RIM-SKM-IR120585-23.12.22
                                                       ).

    IF  COEFICIENTE_O NE COEFICIENTE_D.
*      ORIGEM COM IMPOSTO E DESTINO SEM IMPOSTO
      IF COEFICIENTE_O IS NOT INITIAL AND COEFICIENTE_D IS INITIAL.
        TL_CONDITIONS_IN-COND_VALUE = TL_KONV-KBETR / COEFICIENTE_O.
      ENDIF.
*      ORIGEM E DESTINO COM IMPOSTO
      IF COEFICIENTE_O IS NOT INITIAL AND COEFICIENTE_D IS NOT INITIAL.
        TL_CONDITIONS_IN-COND_VALUE = TL_KONV-KBETR / COEFICIENTE_O.
        TL_CONDITIONS_IN-COND_VALUE = TL_CONDITIONS_IN-COND_VALUE * COEFICIENTE_D.
      ENDIF.
*      ORIGEM SEM IMPOSTO E DESTINO COM IMPOSTO
      IF COEFICIENTE_O IS INITIAL AND COEFICIENTE_D IS NOT INITIAL.
        TL_CONDITIONS_IN-COND_VALUE = TL_KONV-KBETR * COEFICIENTE_D.
      ENDIF.
    ELSE.
      TL_CONDITIONS_IN-COND_VALUE  = TL_KONV-KBETR.
    ENDIF.

    TL_CONDITIONS_IN-COND_UNIT   = TL_KONV-KMEIN.
    TL_CONDITIONS_IN-CONEXCHRAT  = WL_HEADER-KURSK.
    TL_CONDITIONS_IN-COND_TYPE   = 'PR00'.
    APPEND TL_CONDITIONS_IN.

    CLEAR: TL_SCHEDULES_IN.
*    WL_VLR_COVERT              = IT_OV-ZMENG.
    WL_VLR_COVERT              = WL_ITEMFAT-FKIMG.
    TL_SCHEDULES_IN-ITM_NUMBER = TL_ITEMS_IN-ITM_NUMBER.
    TL_SCHEDULES_IN-REQ_QTY    = WL_VLR_COVERT.
    TL_SCHEDULES_IN-REQ_DLV_BL = '10'.
    APPEND TL_SCHEDULES_IN.

    CLEAR: TL_ITEMS_IN, TL_CONDITIONS_IN, TL_SCHEDULES_IN.
  ENDLOOP.

* Criar Ordem
******************SD_SALESDOCUMENT_CREATE*******************INICIO
  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      SALES_HEADER_IN      = WL_HEADER_IN
      SALES_HEADER_INX     = WL_HEADER_INX
    IMPORTING
      SALESDOCUMENT_EX     = WL_VBELN
    TABLES
      RETURN               = TL_RETURN
      SALES_ITEMS_IN       = TL_ITEMS_IN
      SALES_ITEMS_INX      = TL_ITEMS_INX
      SALES_PARTNERS       = TL_PARTNERS
      SALES_SCHEDULES_IN   = TL_SCHEDULES_IN
      SALES_SCHEDULES_INX  = TL_SCHEDULES_INX
      SALES_CONDITIONS_IN  = TL_CONDITIONS_IN
      SALES_CONDITIONS_INX = TL_CONDITIONS_INX
      SALES_TEXT           = TL_TEXT_IN
      EXTENSIONIN          = TL_BAPIPAREX.

* Verirfica se a ordem foi criada.
  CLEAR RT_VBELN.
  REFRESH: IT_MSG, TL_RETURN2,TL_RETURN3.

  IF NOT WL_VBELN IS INITIAL.
******************SD_SALESDOCUMENT_CREATE*******************INICIO SUCESSO

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.

    FREE: IT_SUCESS.

    ADD 1 TO SEQ.
    MOVE: SEQ            TO IT_SUCESS-ETAPA,
          'Criando ZFUT' TO IT_SUCESS-DESC,
          ''             TO IT_SUCESS-ERRO,
          WL_VBELN       TO IT_SUCESS-VBELN.

    APPEND IT_SUCESS.
    CLEAR  IT_SUCESS.

    MOVE WL_VBELN TO TL_OVS-VBELN.
    E_VBELN = WL_VBELN.

    APPEND TL_OVS.
    CLEAR: TL_OVS.
***
    REFRESH: TL_VBUV.
    SELECT *
      FROM VBUV
      INTO TABLE TL_VBUV
       WHERE VBELN EQ WL_VBELN.

    IF SY-SUBRC IS INITIAL.
******************WL_VBELN*******************EXISTE DOC IMCOMPLETOS

      LOOP AT TL_VBUV.
        CLEAR: TL_RETURN, WL_FIELDNAME, WL_TEXT.
        WL_FIELDNAME = TL_VBUV-FDNAM.

        CALL FUNCTION 'RM_DDIC_TEXTS_GET'
          EXPORTING
            I_NAME                = WL_FIELDNAME
            I_TYPE                = 'DTEL'
            I_LANGU               = SY-LANGU
          IMPORTING
            E_DDTXT               = WL_TEXT
          EXCEPTIONS
            OBJTYPE_NOT_SUPPORTED = 1
            ILLEGAL_INPUT         = 2
            OTHERS                = 3.

        IF SY-SUBRC <> 0.
          CONCATENATE 'Existem campos incompletos na OV:' TL_VBUV-FDNAM INTO TL_RETURN-MESSAGE SEPARATED BY SPACE.
        ELSE.
          CONCATENATE 'Existem campos incompletos na OV:' WL_TEXT INTO TL_RETURN-MESSAGE SEPARATED BY SPACE.
        ENDIF.

        TL_RETURN-TYPE = 'E'.
        APPEND TL_RETURN.

      ENDLOOP.

      REFRESH: TL_RETURN_AUX.
      WL_HEADER_INX2-UPDATEFLAG = 'D'.

"*---> 12/07/2023 - Migração S4 - LO --> Material não foi utilizado
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          SALESDOCUMENT    = WL_VBELN
*         ORDER_HEADER_IN  = WL_ORDERHEADERIN
          ORDER_HEADER_INX = WL_HEADER_INX2
        TABLES
          RETURN           = TL_RETURN_AUX.

      READ TABLE TL_RETURN_AUX  WITH KEY TYPE = 'E'.
      IF SY-SUBRC NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.
      ENDIF.

      CLEAR: WL_VBELN.
    ENDIF.

    IF WL_VBELN IS NOT INITIAL.
******************WL_VBELN*******************NÃO EXISTE CAMPOS IMCOMPLETOS

      IF I_AUART = 'ZFUT'.
        REFRESH   IT_MSG.
        REFRESH   T_BILLING.
        T_BILLING-REF_DOC       = WL_VBELN.
        T_BILLING-REF_DOC_CA    = 'C'.
        APPEND T_BILLING.

"*---> 12/07/2023 - Migração S4 - LO
        CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'"#EC CI_USAGE_OK[2438131]
          TABLES
            BILLINGDATAIN = T_BILLING
            RETURN        = T_RETURN
            SUCCESS       = T_SUCCESS.

        IF T_SUCCESS[] IS NOT INITIAL.
**********************************BAPI_BILLINGDOC_CREATEMULTIPLE****************** SUCCESS

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.

          WAIT UP TO 10 SECONDS.

          READ TABLE T_SUCCESS INDEX 1.
          WG_DOCUMENTO = T_SUCCESS-BILL_DOC.


          ADD 1 TO SEQ.
          MOVE: SEQ                 TO IT_SUCESS-ETAPA,
              'Criando Faturamento' TO IT_SUCESS-DESC,
              ABAP_FALSE            TO IT_SUCESS-ERRO,
              WG_DOCUMENTO          TO IT_SUCESS-VBELN.

          APPEND IT_SUCESS.
          CLEAR IT_SUCESS.


          CALL FUNCTION 'ZSDMF001_GERA_OV_SIMU_FUT'
            EXPORTING
              I_VBELN   = WG_DOCUMENTO " Documento de Faturamento
            IMPORTING
              E_VBELN   = RT_VBELN
            TABLES
              TE_RETURN = TL_RETURN2.

          IF RT_VBELN IS INITIAL.
**********************************ZSDMF001_GERA_OV_SIMU_FUT****************** ERRO

            ADD 1 TO SEQ.
            MOVE: SEQ            TO IT_SUCESS-ETAPA,
                  'Criando ZRFU' TO IT_SUCESS-DESC,
                  ABAP_TRUE      TO IT_SUCESS-ERRO,
                  RT_VBELN       TO IT_SUCESS-VBELN.

            APPEND IT_SUCESS.
            CLEAR IT_SUCESS.

            TL_RETURN2-TYPE = 'E'.
            TL_RETURN2-MESSAGE = |Erro na Criação da ZRFU!|.
            APPEND TL_RETURN2.

            APPEND LINES OF TL_RETURN2 TO TE_RETURN.
*            "
*            READ TABLE IT_OV INDEX 1.
*            SELECT SINGLE DOC_SIMULACAO
*               FROM ZSDT0041
*               INTO W_DOC_SIMULACAO
*              WHERE VBELN = IT_OV-VBELN.
*
*            IF SY-SUBRC IS NOT INITIAL.
*              SELECT SINGLE DOC_SIMULACAO
*                FROM ZSDT0090
*                INTO W_DOC_SIMULACAO
*                WHERE VBELN EQ IT_OV-VBELN.
*            ENDIF.

***            ESTORNO DO BILLING E A ORDEM ANTIGA


*            W_SEQ = 0.
*            SELECT COUNT(*)
*              FROM ZSDT0090
*              INTO W_SEQ
*              WHERE DOC_SIMULACAO  = W_DOC_SIMULACAO.
*
*            REFRESH IT_ZSDT0090.
*
*            IT_ZSDT0090-DOC_SIMULACAO  = W_DOC_SIMULACAO.
*            IT_ZSDT0090-SEQUENCIA      = W_SEQ.
*            IT_ZSDT0090-AUART          = 'ZFUT'.
*            IT_ZSDT0090-VBELN          = WL_VBELN.
*            IT_ZSDT0090-VBELV          = IT_OV-VBELN.
*            IT_ZSDT0090-ESTORNO        = ''.
*            IT_ZSDT0090-USNAM          = SY-UNAME.
*            IT_ZSDT0090-DATA_ATUAL     = SY-DATUM.
*            IT_ZSDT0090-HORA_ATUAL     = SY-UZEIT.
*
*            APPEND IT_ZSDT0090.
*            MODIFY ZSDT0090 FROM TABLE IT_ZSDT0090.

*            IF I_ACAO NE 'DESMEMBRAR'.
*              LOOP AT IT_OV.
*                PERFORM INSERT_ZSDT0090(ZSDR0042)
*                USING ''
*                      W_DOC_SIMULACAO
*                      WL_VBELN
*                      IT_OV-VBELN
*                      IT_OV-MATNR
*            CHANGING P_0090.
*
*              ENDLOOP.
*            ENDIF.

          ELSE.

**********************************ZSDMF001_GERA_OV_SIMU_FUT****************** SUCCESS

            ADD 1 TO SEQ.
            MOVE: SEQ            TO IT_SUCESS-ETAPA,
                  'Criando ZRFU' TO IT_SUCESS-DESC,
                  ABAP_FALSE     TO IT_SUCESS-ERRO,
                  RT_VBELN       TO IT_SUCESS-VBELN.

            APPEND IT_SUCESS.
            CLEAR IT_SUCESS.

            READ TABLE IT_OV INDEX 1.
            " gravar Controle de Transferencias de OV
            "Grava a OV
            SELECT SINGLE DOC_SIMULACAO
               FROM ZSDT0041
               INTO W_DOC_SIMULACAO
              WHERE VBELN = IT_OV-VBELN.

            IF SY-SUBRC IS NOT INITIAL.
              SELECT SINGLE DOC_SIMULACAO
                FROM ZSDT0090
                INTO W_DOC_SIMULACAO
                WHERE VBELN EQ IT_OV-VBELN.
            ENDIF.

*            W_SEQ = 0.
*            SELECT COUNT(*)
*              FROM ZSDT0090
*              INTO W_SEQ
*              WHERE DOC_SIMULACAO  = W_DOC_SIMULACAO.
*
*            REFRESH IT_ZSDT0090.
*            IT_ZSDT0090-DOC_SIMULACAO  = W_DOC_SIMULACAO.
*            IT_ZSDT0090-SEQUENCIA      = W_SEQ.
*            IT_ZSDT0090-AUART          = 'ZFUT'.
*            IT_ZSDT0090-VBELN          = WL_VBELN.
*            IT_ZSDT0090-VBELV          = IT_OV-VBELN.
*            IT_ZSDT0090-ESTORNO        = ''.
*            IT_ZSDT0090-USNAM          = SY-UNAME.
*            IT_ZSDT0090-DATA_ATUAL     = SY-DATUM.
*            IT_ZSDT0090-HORA_ATUAL     = SY-UZEIT.
*            APPEND IT_ZSDT0090.


*            IF I_ACAO NE 'DESMEMBRAR'.
*              LOOP AT IT_OV.
*
*                PERFORM INSERT_ZSDT0090(ZSDR0042)
*               USING ''
*                     W_DOC_SIMULACAO
*                     WL_VBELN       "NEW
*                     IT_OV-VBELN    "OLD
*                     IT_OV-MATNR
*                                 CHANGING P_0090.
*
*                PERFORM INSERT_ZSDT0090(ZSDR0042)
*                USING ''
*                      W_DOC_SIMULACAO
*                      RT_VBELN      "NEW
*                      WL_VBELN      "OLD
*                      IT_OV-MATNR
*                                  CHANGING P_0090.
*              ENDLOOP.
*            ENDIF.

*            ADD 1 TO W_SEQ.
*            IT_ZSDT0090-DOC_SIMULACAO  = W_DOC_SIMULACAO.
*            IT_ZSDT0090-SEQUENCIA      = W_SEQ.
*            IT_ZSDT0090-AUART          = 'ZRFU'.
*            IT_ZSDT0090-VBELN          = RT_VBELN.
*            IT_ZSDT0090-VBELV          = IT_OV-VBELN.
*            IT_ZSDT0090-ESTORNO        = ''.
*            IT_ZSDT0090-USNAM          = SY-UNAME.
*            IT_ZSDT0090-DATA_ATUAL     = SY-DATUM.
*            IT_ZSDT0090-HORA_ATUAL     = SY-UZEIT.
*            APPEND IT_ZSDT0090.

*            IF I_ACAO NE 'DESMEMBRAR'.
*              LOOP AT IT_OV.
*                PERFORM INSERT_ZSDT0090(ZSDR0042)
*                USING ''
*                      W_DOC_SIMULACAO
*                      WL_VBELN
*                      IT_OV-VBELN
*                      IT_OV-POSNR.
*              ENDLOOP.
*            ENDIF.

            "
*            MODIFY ZSDT0090 FROM TABLE IT_ZSDT0090.
            " atualiza saldo OV Origem
            CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIM'
              EXPORTING
                I_AUART   = WL_HEADER-AUART
                I_ACAO    = I_ACAO
              TABLES
                IT_OV     = IT_OV
                TE_RETURN = TL_RETURN3.

            READ TABLE TL_RETURN3 WITH KEY TYPE = 'E'.

            IT_SUCESS-VBELN = IT_OV[ 1 ]-VBELN.

            IF SY-SUBRC IS INITIAL.

              TL_RETURN3-TYPE = 'E'.
              TL_RETURN3-MESSAGE = |Erro na Atualização da Ordem { IT_SUCESS-VBELN }!|.

              APPEND TL_RETURN3.

              APPEND LINES OF TL_RETURN3 TO TE_RETURN.
              MOVE ABAP_TRUE   TO IT_SUCESS-ERRO.

            ELSE.
              MOVE ABAP_FALSE  TO IT_SUCESS-ERRO.
            ENDIF.

            ADD 1 TO SEQ.
            MOVE: SEQ            TO IT_SUCESS-ETAPA,
                  'Atualizando OV Antiga'  TO IT_SUCESS-DESC.

            IT_SUCESS-VBELN = IT_OV[ 1 ]-VBELN.

            APPEND IT_SUCESS.
            CLEAR IT_SUCESS.

          ENDIF.
        ELSE. " Elimina a ordem gerada
**********************************BAPI_BILLINGDOC_CREATEMULTIPLE****************** ERRO

          CLEAR F_HEADINX.
          F_HEADINX-UPDATEFLAG = 'D'.
"*---> 12/07/2023 - Migração S4 - LO --> Material não foi utilizado
          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'"#EC CI_USAGE_OK[2438131]
            EXPORTING
              SALESDOCUMENT    = WL_VBELN
              ORDER_HEADER_INX = F_HEADINX
            TABLES
              RETURN           = TL_RETURN2.

          READ TABLE TL_RETURN2  WITH KEY TYPE = 'E'.
          IF NOT SY-SUBRC IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = ABAP_TRUE.
          ENDIF.

          TL_RETURN2-TYPE = 'E'.
          TL_RETURN2-MESSAGE = |Erro na Criação da Fatura!|.
          APPEND TL_RETURN2.

          LOOP AT IT_MSG .
            SELECT SINGLE * FROM T100 INTO WL_T100 WHERE SPRSL = IT_MSG-MSGSPRA
                            AND   ARBGB = IT_MSG-MSGID
                            AND   MSGNR = IT_MSG-MSGNR.

            CLEAR TL_RETURN2.
            TL_RETURN2-TYPE = 'E'.
            CONCATENATE WL_T100-TEXT IT_MSG-MSGV1 IT_MSG-MSGV2 IT_MSG-MSGV3 IT_MSG-MSGV4 INTO TL_RETURN2-MESSAGE.
            APPEND TL_RETURN2.
          ENDLOOP.
        ENDIF.
      ELSE.
        "Grava a OV
        READ TABLE IT_OV INDEX 1.
        " Checar se OV é desmembrada.
        CLEAR W_DOC_SIMULACAO.
        SELECT SINGLE *
         FROM ZSDT0090
         INTO WA_ZSDT0090
        WHERE VBELN = IT_OV-VBELN.
        IF SY-SUBRC = 0.
          W_DOC_SIMULACAO = WA_ZSDT0090-DOC_SIMULACAO.
        ELSE.
          SELECT SINGLE *
            FROM ZSDT0041
            INTO WA_ZSDT0041
           WHERE VBELN = IT_OV-VBELN.
          IF SY-SUBRC = 0.
            W_DOC_SIMULACAO = WA_ZSDT0041-DOC_SIMULACAO.
          ENDIF.
        ENDIF.
        IF W_DOC_SIMULACAO IS NOT INITIAL.

          IF I_ACAO NE 'DESMEMBRAR'.
            IF I_ACAO NE 'ZTRI'.
              LOOP AT IT_OV.
                PERFORM INSERT_ZSDT0090(ZSDR0042)
                USING ''
                      W_DOC_SIMULACAO
                      WL_VBELN
                      IT_OV-VBELN
                      IT_OV-MATNR
                CHANGING P_0090.

                MOVE-CORRESPONDING P_0090 TO W_0090.

                CASE I_AUART.
                  WHEN 'ZFTE'
                    OR 'ZDEF'
                    OR 'ZSEM'
                    OR 'ZOFE'
                    OR 'ZODF'
                    OR 'ZOSM'.
                    IF V_DISPARO IS NOT INITIAL.
                      ZCL_WEBSERVICE_TX_CURVA=>HEDGE_INSUMOS( I_ACAO   = ''
                                                              I_0090   = W_0090
                                                              I_TIPO   = 'INV'
                                                              I_DIR    = ''
                                                             ).
                    ENDIF.
                ENDCASE.
              ENDLOOP.
            ELSE.
*              READ TABLE IT_OV INDEX 2.
*              PERFORM INSERT_ZSDT0090(ZSDR0042)
*              USING ''
*                    W_DOC_SIMULACAO
*                    WL_VBELN
*                    IT_OV-VBELN
*                    IT_OV-MATNR
*           CHANGING P_0090.

              CLEAR WL_POSNR.
              LOOP AT TL_ITEMFAT INTO WL_ITEMFAT.
                ADD 1 TO WL_POSNR.
                P_0090-POSNN = WL_POSNR * 10.     "Enviar Item OV ZTRI Criada c/ ref na Fatura
                PERFORM INSERT_ZSDT0090(ZSDR0042)
                USING ''
                  W_DOC_SIMULACAO
                  WL_VBELN            "NEW
                  WL_ITEMFAT-AUBEL    "OLD
                  WL_ITEMFAT-MATNR
                CHANGING P_0090.
              ENDLOOP.

            ENDIF.
          ENDIF.

          IF I_AUART = 'ZREM' OR I_KUNNR IS NOT INITIAL.
            W_SEQ = 0.
            SELECT COUNT(*)
              FROM ZSDT0089
              INTO W_SEQ
              WHERE DOC_SIMULACAO  = W_DOC_SIMULACAO.

            REFRESH IT_ZSDT0089.
            LOOP AT TL_ITENS.
              READ TABLE IT_OV WITH KEY VBELN = TL_ITENS-VBELN
                                        POSNR = TL_ITENS-POSNR BINARY SEARCH.
              IF SY-SUBRC NE 0.
                CONTINUE.
              ENDIF.

              ADD 1 TO W_SEQ.
              IT_ZSDT0089-DOC_SIMULACAO  = W_DOC_SIMULACAO.
              IT_ZSDT0089-SEQUENCIA      = W_SEQ.
              IT_ZSDT0089-VBELN          = WL_VBELN.
              IT_ZSDT0089-MATNR          = TL_ITENS-MATNR.
              IT_ZSDT0089-KUNNR          = I_KUNNR.
              IT_ZSDT0089-KWMENG         = IT_OV-ZMENG.
              IT_ZSDT0089-TXT_ORDEM      = I_TXT_ORDEM.
              IT_ZSDT0089-TXT_ITEM       = I_TXT_ITEM.
              IT_ZSDT0089-USNAM          = SY-UNAME.
              IT_ZSDT0089-DATA_ATUAL     = SY-DATUM.
              IT_ZSDT0089-HORA_ATUAL     = SY-UZEIT.
              APPEND IT_ZSDT0089.
            ENDLOOP.
            MODIFY ZSDT0089 FROM TABLE IT_ZSDT0089.
          ENDIF.
          IF I_AUART = 'ZTRI'.
            REFRESH   T_BILLING.
            T_BILLING-REF_DOC       = WL_VBELN.
            T_BILLING-REF_DOC_CA    = 'C'.
            APPEND T_BILLING.

"*---> 12/07/2023 - Migração S4 - LO
            CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'"#EC CI_USAGE_OK[2438131]
              TABLES
                BILLINGDATAIN = T_BILLING
                RETURN        = T_RETURN
                SUCCESS       = T_SUCCESS.

            WAIT UP TO 5 SECONDS.

            IF T_SUCCESS[] IS NOT INITIAL.
              LOOP AT T_RETURN.
                CLEAR TL_RETURN.
                MOVE-CORRESPONDING T_RETURN TO TE_RETURN.
                APPEND TL_RETURN.
              ENDLOOP.

              "Texto de cabeçalho
              IF TL_TEXT_IN[] IS NOT INITIAL.
                READ TABLE T_SUCCESS INDEX 1.

                LOOP AT TL_TEXTO INTO WL_TEXTO.
                  TL_TEXT-TDFORMAT = '*'.
                  TL_TEXT-TDLINE+0(72) = WL_TEXTO.
                  APPEND TL_TEXT.
                  CLEAR  TL_TEXT.
                ENDLOOP.

                TL_HEADER-TDOBJECT = 'VBBK'.
                TL_HEADER-TDNAME   = T_SUCCESS-BILL_DOC.
                TL_HEADER-TDID     = '0002'.
                TL_HEADER-TDSPRAS  = SY-LANGU.

                CALL FUNCTION 'SAVE_TEXT'
                  EXPORTING
                    CLIENT          = SY-MANDT
                    HEADER          = TL_HEADER
*                   INSERT          = ' '
                    SAVEMODE_DIRECT = 'X'
                  TABLES
                    LINES           = TL_TEXT
                  EXCEPTIONS
                    ID              = 1
                    LANGUAGE        = 2
                    NAME            = 3
                    OBJECT          = 4
                    OTHERS          = 5.

              ENDIF.
            ENDIF.
          ELSE.
            CALL FUNCTION 'ZSDMF001_ATUALI_OV_SIM'
              EXPORTING
                I_ACAO    = I_ACAO
              TABLES
                IT_OV     = IT_OV
                TE_RETURN = TL_RETURN3.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ELSE. " Não gerou OV
******************SD_SALESDOCUMENT_CREATE*******************INICIO ERRO



    IF I_AUART EQ 'ZFUT'.
      APPEND VALUE #(
                      TYPE = 'E'
                      MESSAGE  = |Erro na Criação da ZFUT!|
                    ) TO TL_RETURN.
    ENDIF.

    APPEND LINES OF TL_RETURN TO TE_RETURN.

  ENDIF.
******************SD_SALESDOCUMENT_CREATE*******************FIM

* PERFORM HEDGE USING 'F'.

  IF NOT IT_SUCESS[] IS INITIAL AND I_AUART = 'ZFUT'.

    TRY .
        DATA(ETAPA) = IT_SUCESS[ ERRO = ABAP_TRUE ]-ETAPA.
      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
        ETAPA = 0.
    ENDTRY.

    IF NOT ETAPA IS INITIAL.
      SORT IT_SUCESS BY ETAPA DESCENDING.

      LOOP AT IT_SUCESS.
        IF ETAPA NE IT_SUCESS-ETAPA.
          PERFORM ESTORNO_ERROS(ZSDR0042) USING IT_SUCESS-VBELN IT_SUCESS-ETAPA.
        ENDIF.
      ENDLOOP.
    ELSE.
      IF I_ACAO NE 'DESMEMBRAR'.
        LOOP AT IT_OV.

          PERFORM INSERT_ZSDT0090(ZSDR0042)
         USING ''
               W_DOC_SIMULACAO
               WL_VBELN       "NEW
               IT_OV-VBELN    "OLD
               IT_OV-MATNR
                           CHANGING P_0090.

          PERFORM INSERT_ZSDT0090(ZSDR0042)
          USING ''
                W_DOC_SIMULACAO
                RT_VBELN      "NEW
                WL_VBELN      "OLD
                IT_OV-MATNR
                            CHANGING P_0090.
        ENDLOOP.
      ENDIF.
    ENDIF.
    EXIT.
  ENDIF.

  CLEAR: TL_SAIDA_EXEC.
  MOVE: WL_HEADER-INCO1 TO TL_SAIDA_EXEC-INCO1,
        WL_HEADER-SPART TO TL_SAIDA_EXEC-SPART,
        I_AUART         TO TL_SAIDA_EXEC-AUART,
        WL_HEADER-WERKS TO TL_SAIDA_EXEC-WERKS,
        WL_VBELN        TO TL_SAIDA_EXEC-VBELN.

  IF WL_VBELN IS NOT INITIAL.
    APPEND TL_SAIDA_EXEC.
  ELSE.
    LOOP AT TL_RETURN WHERE TYPE EQ 'E'.
      MOVE: TL_RETURN-MESSAGE TO TL_SAIDA_EXEC-MSG.
      APPEND TL_SAIDA_EXEC.
    ENDLOOP.
    LOOP AT TL_RETURN3.
      MOVE: TL_RETURN3-MESSAGE TO TL_SAIDA_EXEC-MSG.
      APPEND TL_SAIDA_EXEC.
    ENDLOOP.
  ENDIF.

  IF I_AUART = 'ZFUT'.
    CLEAR: TL_SAIDA_EXEC.
    MOVE: WL_HEADER-INCO1 TO TL_SAIDA_EXEC-INCO1,
          WL_HEADER-SPART TO TL_SAIDA_EXEC-SPART,
          'ZRFU'          TO TL_SAIDA_EXEC-AUART,
          WL_HEADER-WERKS TO TL_SAIDA_EXEC-WERKS,
          RT_VBELN       TO TL_SAIDA_EXEC-VBELN.
    IF RT_VBELN IS NOT INITIAL.
      APPEND TL_SAIDA_EXEC.
    ELSE.
      LOOP AT TL_RETURN2.
        MOVE: TL_RETURN2-MESSAGE TO TL_SAIDA_EXEC-MSG.
        APPEND TL_SAIDA_EXEC.
      ENDLOOP.
      LOOP AT TL_RETURN3.
        MOVE: TL_RETURN3-MESSAGE TO TL_SAIDA_EXEC-MSG.
        APPEND TL_SAIDA_EXEC.
      ENDLOOP.
    ENDIF.

  ENDIF.

  IF TL_SAIDA_EXEC[] IS NOT INITIAL.
    PERFORM MONTAR_LAYOUT.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        IT_FIELDCAT           = ESTRUTURA[]
        I_SAVE                = 'A'
        I_SCREEN_START_COLUMN = 3
        I_SCREEN_START_LINE   = 3
        I_SCREEN_END_COLUMN   = 100
        I_SCREEN_END_LINE     = 13
      TABLES
        T_OUTTAB              = TL_SAIDA_EXEC.
  ENDIF.

ENDFUNCTION.
