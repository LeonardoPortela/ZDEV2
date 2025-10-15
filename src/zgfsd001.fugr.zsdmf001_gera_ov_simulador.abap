FUNCTION ZSDMF001_GERA_OV_SIMULADOR.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOC_SIMULACAO) TYPE  ZSDT0040-DOC_SIMULACAO
*"  EXPORTING
*"     VALUE(ERRO) TYPE  CHAR1
*"  TABLES
*"      TE_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      TE_VBAK STRUCTURE  VBAK OPTIONAL
*"      TE_VBAP STRUCTURE  VBAP OPTIONAL
*"  EXCEPTIONS
*"      OV_JA_CRIADA
*"      SIMULACAO_NAO_EXISTE
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOC_SIMULACAO) TYPE  ZSDT0040-DOC_SIMULACAO
*"  EXPORTING
*"     VALUE(ERRO) TYPE  CHAR1
*"  TABLES
*"      TE_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"      TE_VBAK STRUCTURE  VBAK OPTIONAL
*"      TE_VBAP STRUCTURE  VBAP OPTIONAL
*"  EXCEPTIONS
*"      OV_JA_CRIADA
*"      SIMULACAO_NAO_EXISTE

*&*---------------------------------------------------------------------------------&*
*&                    Histórico de Modificações                                     &*
*& Autor           Request      Data         Descrição                              &*
*& Sara Oikawa     DEVK9A0NSJ   14.10.2020   39781 -  Melhorias ZSDT0044 - Pacote 5 &*
*&----------------------------------------------------------------------------------&*

  TYPES: BEGIN OF TY_ZSDT0041.
           INCLUDE STRUCTURE ZSDT0041.
  TYPES:   MATKL    TYPE MARA-MATKL,
           MTART    TYPE MARA-MTART,
           ZPESAGEM TYPE ZPESAGEM,
           TAXA     TYPE KURSF,
           CH_AGRUP TYPE I,
         END OF TY_ZSDT0041.

  DATA: BEGIN OF IT_MSG OCCURS 0.
          INCLUDE STRUCTURE BDCMSGCOLL.
  DATA: END OF IT_MSG.

* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
  TYPES BEGIN OF TY_0090.
  INCLUDE STRUCTURE ZSDT0090.
  TYPES POSNR TYPE VBAP-POSNR.
  TYPES END OF TY_0090.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5

  DATA: WL_HEADER         TYPE ZSDT0040,
        WL_HEADER_IN      TYPE BAPISDHD1,
        WL_HEADER_INX     TYPE BAPISDHD1X,
        WL_VBELN          TYPE VBAK-VBELN,
        RT_VBELN          TYPE VBAK-VBELN,
        WG_DOCUMENTO(10),
        TL_OVS            TYPE TABLE OF TY_OVS        WITH HEADER LINE,
        WL_ITENS          TYPE ZSDT0041,
        WL_ITENS_AUX2     TYPE ZSDT0041,
        TL_ITENS          TYPE TABLE OF TY_ZSDT0041   WITH HEADER LINE,
        L_ITENS           TYPE TY_ZSDT0041,
        TL_ITENS_AUX      TYPE TABLE OF TY_ZSDT0041   WITH HEADER LINE,
        TL_ITENS_AUX2     TYPE TABLE OF TY_ZSDT0041   WITH HEADER LINE,
        TL_0175           TYPE TABLE OF TY_ZSDT0041   WITH HEADER LINE,
        TL_0048           TYPE TABLE OF ZSDT0048      WITH HEADER LINE,
        TL_ITEMS_IN       TYPE TABLE OF BAPISDITM     WITH HEADER LINE,
*        TL_ITEMS_IN_AUX       TYPE TABLE OF BAPISDITM  WITH HEADER LINE,
        TL_ITEMS_INX      TYPE TABLE OF BAPISDITMX    WITH HEADER LINE,
        TL_PARTNERS       TYPE TABLE OF BAPIPARNR     WITH HEADER LINE,
*        TL_SCHEDULES      TYPE TABLE OF BAPISCHDL  WITH HEADER LINE,
        TL_SCHEDULES_IN   TYPE TABLE OF BAPISCHDL     WITH HEADER LINE,
*        TL_SCHEDULES_IN_AUX   TYPE TABLE OF BAPISCHDL  WITH HEADER LINE,
        TL_SCHEDULES_INX  TYPE TABLE OF BAPISCHDLX    WITH HEADER LINE,
        TL_CONDITIONS_IN  TYPE TABLE OF BAPICOND      WITH HEADER LINE,
*        TL_CONDITIONS_IN_AUX  TYPE TABLE OF BAPICOND   WITH HEADER LINE,
        TL_CONDITIONS_INX TYPE TABLE OF BAPICONDX     WITH HEADER LINE,
        F_HEADINX         LIKE BAPISDH1X,
        WL_HEADER_INX2    LIKE BAPISDH1X,
        TL_RETURN         TYPE TABLE OF BAPIRET2      WITH HEADER LINE,
        TL_RETURN2        TYPE TABLE OF BAPIRET2      WITH HEADER LINE,
        TL_RETURN3        TYPE TABLE OF BAPIRET2      WITH HEADER LINE,
        TL_RETURN_AUX     TYPE TABLE OF BAPIRET2      WITH HEADER LINE,
        TL_TEXT_IN        TYPE TABLE OF BAPISDTEXT    WITH HEADER LINE,
        TL_SAIDA_EXEC     TYPE TABLE OF TY_SAIDA_EXEC WITH HEADER LINE,
        TL_MARA           TYPE TABLE OF MARA          WITH HEADER LINE,
        TL_VBUV           TYPE  TABLE OF VBUV         WITH HEADER LINE,
        WL_FIELDNAME      TYPE DCOBJDEF-NAME, "RMDI_NAME,
        WL_TEXT           TYPE DD04V-DDTEXT, "RMDI_DDTXT,
        WL_POSNR          TYPE SY-TABIX,
        WL_VLR_COVERT     TYPE DZMENG,
        TL_TEXTO          TYPE CATSXT_LONGTEXT_ITAB,
        WL_TEXTO          TYPE LINE OF CATSXT_LONGTEXT_ITAB,
        TL_BAPIPAREX      TYPE TABLE OF BAPIPAREX WITH HEADER LINE,
        WL_BAPE_VBAK      TYPE BAPE_VBAK,
        WL_BAPE_VBAKX     TYPE BAPE_VBAKX,
        WL_T100           TYPE T100,
        WL_ERRO(1),
        TABIX             TYPE SY-TABIX,
        WL_MATKL          TYPE MARA-MATKL,
        B_KNA1            TYPE KNA1,
        B_KNB1            TYPE KNB1,
        B_LFA1            TYPE LFA1.

  DATA: IT_LFA1  TYPE STANDARD TABLE OF LFA1,
        IT_LFB1  TYPE STANDARD TABLE OF LFB1,
        IT_LFM1  TYPE STANDARD TABLE OF LFM1,
        WA_LFA1  TYPE LFA1,
        VL_CHECK TYPE CHAR1,
        CH_AGRUP TYPE I.

* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
  DATA: TL_ITENS_OV  TYPE TABLE OF TY_ZSDT0041,
        WL_ITENS_OV  TYPE TY_ZSDT0041,
        P_0090       TYPE TY_0090,
        W_0090       TYPE ZSDT0090,
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
**** CS2022000324
        R_WERKS      TYPE RANGE OF WERKS_D,
        IT_VALUES    TYPE TABLE OF RGSB4,
        WA_VALUES    TYPE RGSB4,
        WA_WERKS     LIKE LINE OF R_WERKS,
        LV_MATNR_ANT TYPE MARA-MATNR.
**** CS2022000324

  REFRESH: TL_ITENS, TL_ITEMS_IN, TL_ITEMS_INX,TL_PARTNERS, TL_SCHEDULES_IN, TL_SCHEDULES_INX,
           TL_CONDITIONS_IN, TL_CONDITIONS_INX, TL_RETURN,TL_RETURN2,TL_RETURN3, TL_OVS, TL_ITENS_AUX, TL_TEXTO,
           TL_TEXT_IN, TL_0048, TL_SAIDA_EXEC, ESTRUTURA, EVENTS, TL_MARA, TL_BAPIPAREX, TL_ITENS_AUX2, TL_ITENS_OV, IT_VALUES.

  CLEAR: WL_HEADER, WL_HEADER_IN, WL_HEADER_INX, TL_ITENS, TL_ITEMS_IN, TL_ITEMS_INX,TL_PARTNERS,
          TL_SCHEDULES_IN, TL_SCHEDULES_INX, TL_CONDITIONS_IN, TL_CONDITIONS_INX, TL_RETURN,TL_RETURN2,TL_RETURN3,
         TL_OVS, TL_ITENS_AUX, WL_TEXTO, TL_TEXT_IN, TL_0048, TL_SAIDA_EXEC, WA_ESTRUTURA, XS_EVENTS, TL_MARA,
         WL_BAPE_VBAK, WL_BAPE_VBAKX, TL_BAPIPAREX, WL_ITENS_AUX2, WL_ITENS_OV.

* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
*  REFRESH tl_itens_ov.
*  CLEAR   wl_itens_ov.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5

**** CS2022000324
  REFRESH IT_VALUES.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      SETNR         = 'MV45AFZZ_WERKS'
      TABLE         = 'VBAP'
      CLASS         = '0000'
      FIELDNAME     = 'WERKS'
    TABLES
      SET_VALUES    = IT_VALUES
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.
  IF SY-SUBRC IS INITIAL.

    LOOP AT IT_VALUES INTO WA_VALUES.
      WA_WERKS = 'IEQ'.
      WA_WERKS-LOW    = WA_VALUES-FROM.
      IF WA_VALUES-TO IS NOT INITIAL.
        WA_WERKS = 'IBT'.
        WA_WERKS-HIGH = WA_VALUES-TO.
      ENDIF.

      APPEND WA_WERKS TO R_WERKS.
    ENDLOOP.
  ENDIF.
**** CS2022000324

  SELECT SINGLE *
    FROM ZSDT0040
    INTO WL_HEADER
     WHERE DOC_SIMULACAO EQ I_DOC_SIMULACAO
       AND STATUS        EQ 'A'.

  IF SY-SUBRC IS NOT INITIAL.
    RAISE SIMULACAO_NAO_EXISTE.
  ENDIF.

*  Inicio Bloqueio cliente com restrições CS2016000357 WB
  SELECT SINGLE *
    FROM KNA1
      INTO B_KNA1
        WHERE KUNNR EQ WL_HEADER-KUNNR.

  CASE ABAP_TRUE.
    WHEN B_KNA1-SPERR OR "  Bloqueio Geral p/ todas empresas
         B_KNA1-AUFSD OR "  Bloqueio de ordem centralizado para cliente
         B_KNA1-LIFSD OR "  Bloqueio de remessa centralizado para cliente
         B_KNA1-FAKSD OR "  Bloqueio centralizado de faturamento para cliente
         B_KNA1-CASSD.   "  Bloqueio de contatos central para cliente
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-001. ERRO = ABAP_TRUE. EXIT.
  ENDCASE.

**  Bloqueio Geral p/ todas empresas
*    IF     NOT B_KNA1-SPERR IS INITIAL.
*      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-001. ERRO = ABAP_TRUE. EXIT.
*
**  Bloqueio de ordem centralizado para cliente
*    ELSEIF NOT B_KNA1-AUFSD IS INITIAL.
*      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-001. ERRO = ABAP_TRUE. EXIT.
*
**  Bloqueio de remessa centralizado para cliente
*    ELSEIF NOT B_KNA1-LIFSD IS INITIAL.
*      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-001. ERRO = ABAP_TRUE. EXIT.
*
**  Bloqueio centralizado de faturamento para cliente
*    ELSEIF NOT B_KNA1-FAKSD IS INITIAL.
*      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-001. ERRO = ABAP_TRUE. EXIT.
*
**  Bloqueio de contatos central para cliente
*    ELSEIF NOT B_KNA1-CASSD IS INITIAL.
*      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-001. ERRO = ABAP_TRUE. EXIT.
*    ENDIF.

  SELECT SINGLE *
    FROM KNB1
      INTO B_KNB1
        WHERE KUNNR EQ WL_HEADER-KUNNR
          AND BUKRS EQ WL_HEADER-VKORG.

*  Bloqueio Especifico parea uma empresas
  IF     NOT B_KNB1-SPERR IS INITIAL.
    MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-001. ERRO = ABAP_TRUE. EXIT.
  ENDIF.

*-CS2019001753-12.04.2024-#65741-JT-inicio
*-validar se dados bancarios existem
*#165455 - 07.02.25 PQ - Desconsiderar calculo de imposto quando Empresa Argentina
  IF WL_HEADER-VKORG NE '0100'.
    SELECT SINGLE BANCO, AGENCIA, CONTA
      INTO @DATA(W_0290)
      FROM ZSDT0290
     WHERE AREA  = 'IN'
       AND VKORG = @WL_HEADER-VKORG.

    IF SY-SUBRC <> 0.
      MESSAGE S024(SD) WITH 'Dados Bancários nao existem para Org.Venda:' WL_HEADER-VKORG DISPLAY LIKE 'E'.
      ERRO = ABAP_TRUE.
      EXIT.
    ENDIF.
  ENDIF.
*#165455 - 07.02.25 - PQ
*-CS2019001753-12.04.2024-#65741-JT-fim

* Verifica se existe fornecedor bloqueado associado ao Cliente

*    IF NOT B_KNA1-STKZN IS INITIAL.
*      SELECT SINGLE *
*        FROM LFA1
*        INTO B_LFA1
*        WHERE STCD2 EQ B_KNA1-STCD2
*          AND STCD3 EQ B_KNA1-STCD3.
*    ELSE.
*      SELECT SINGLE *
*        FROM LFA1
*        INTO B_LFA1
*        WHERE STCD1 EQ B_KNA1-STCD1
*          AND STCD3 EQ B_KNA1-STCD3.
*    ENDIF.
*
*    CASE ABAP_TRUE.
*      WHEN B_LFA1-SPERR OR
*           B_LFA1-SPERM.
*        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-002. ERRO = ABAP_TRUE. EXIT.
*      WHEN OTHERS.
*        IF B_LFA1-SPERQ IS NOT INITIAL.
*          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-002. ERRO = ABAP_TRUE. EXIT.
*        ENDIF.
*    ENDCASE.

*****************************************************************************************
* Verifica se existe fornecedor bloqueado associado ao Cliente - LG CS2017001709 INÍCIO *
*****************************************************************************************

  CLEAR: IT_LFA1, VL_CHECK.

  IF NOT B_KNA1-STKZN IS INITIAL.

    SELECT *
      FROM LFA1
      INTO TABLE IT_LFA1
      WHERE STCD2 EQ B_KNA1-STCD2
        AND STCD3 EQ B_KNA1-STCD3.

  ELSE.

    SELECT *
      FROM LFA1 INTO TABLE IT_LFA1
      WHERE STCD1 EQ B_KNA1-STCD1
        AND STCD3 EQ B_KNA1-STCD3.

  ENDIF.

  IF IT_LFA1 IS NOT INITIAL.

    SELECT *
      FROM LFM1
      INTO TABLE IT_LFM1
      FOR ALL ENTRIES IN IT_LFA1
      WHERE LIFNR EQ IT_LFA1-LIFNR.

    SELECT *
      FROM LFB1
      INTO TABLE IT_LFB1
      FOR ALL ENTRIES IN IT_LFA1
      WHERE LIFNR EQ IT_LFA1-LIFNR.

    SORT IT_LFA1 BY SPERR SPERM SPERQ ASCENDING.
    DELETE IT_LFA1 WHERE SPERR EQ ABAP_TRUE
                      OR SPERM EQ ABAP_TRUE
                      OR SPERQ NE SPACE.

    IF IT_LFA1 IS INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-002.
      ERRO = ABAP_TRUE.
      EXIT.
    ELSE.

      LOOP AT IT_LFA1 INTO WA_LFA1.

        READ TABLE IT_LFM1 WITH KEY LIFNR = WA_LFA1-LIFNR
                                    SPERM = ABAP_TRUE TRANSPORTING NO FIELDS.
        IF SY-SUBRC IS NOT INITIAL.
          READ TABLE IT_LFB1 INTO DATA(W_LFB1) WITH KEY LIFNR = WA_LFA1-LIFNR
                                      SPERR = ABAP_TRUE TRANSPORTING NO FIELDS.
          IF SY-SUBRC IS NOT INITIAL.
            VL_CHECK = ABAP_TRUE.
            DATA(EMPRESA) = W_LFB1-BUKRS.
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF VL_CHECK IS INITIAL.
        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-002 && EMPRESA && '!'.
        ERRO = ABAP_TRUE.
        EXIT.
      ENDIF.

    ENDIF.

  ENDIF.

**************************************************************************************
* Verifica se existe fornecedor bloqueado associado ao Cliente - LG CS2017001709 FIM *
**************************************************************************************

*    Fim Bloqueio cliente com restrições CS2016000357 WB

  SELECT *
    FROM ZSDT0041
    INTO TABLE TL_ITENS
     WHERE DOC_SIMULACAO EQ WL_HEADER-DOC_SIMULACAO
       AND VBELN         EQ SPACE.

  IF SY-SUBRC IS INITIAL.

    SELECT *
        FROM ZSDT0094
          INTO TABLE @DATA(IT_0094)
        FOR ALL ENTRIES IN @TL_ITENS
          WHERE NRO_SOL_OV EQ @TL_ITENS-DOC_SIMULACAO
            AND PROGRAMA EQ 'ZSDR016'
            AND TIPO EQ 'VDI'
            AND ESTORNO EQ @ABAP_FALSE.

    SELECT *
      FROM ZSDT0048
      INTO TABLE TL_0048
       FOR ALL ENTRIES IN TL_ITENS
        WHERE BUKRS   EQ WL_HEADER-VKORG
          AND VTWEG   EQ WL_HEADER-VTWEG
          AND SPART   EQ TL_ITENS-SPART.
*            AND werks_d EQ tl_itens-werks
*            AND vkbur   EQ wl_header-vkbur.

    SELECT *
      FROM MARA
      INTO TABLE TL_MARA
       FOR ALL ENTRIES IN TL_ITENS
        WHERE MATNR EQ TL_ITENS-MATNR.

  ELSE.
    RAISE OV_JA_CRIADA.
  ENDIF.


  LOOP AT TL_ITENS.
    TABIX = SY-TABIX.
    READ TABLE TL_MARA
       WITH KEY MATNR = TL_ITENS-MATNR.
    MOVE TL_MARA-MATKL TO TL_ITENS-MATKL.
    MOVE TL_MARA-MTART TO TL_ITENS-MTART.

**** CS2022000324
*      IF tl_itens-werks EQ '0175' AND tl_itens-mtart EQ 'ZFER'.
    IF TL_ITENS-WERKS IN R_WERKS AND TL_ITENS-MTART EQ 'ZFER'.
      TL_ITENS-ZPESAGEM = '01'.
    ELSE.
      TL_ITENS-ZPESAGEM = '02'.
    ENDIF.

    IF WL_HEADER-WAERK NE 'USD'.
      CASE WL_HEADER-TPSIM.
        WHEN 'BN' OR 'PM'.
        WHEN OTHERS.
          TRY .
              TL_ITENS-TAXA = CONV #( IT_0094[ NRO_SOL_OV = TL_ITENS-DOC_SIMULACAO
                                       BEZEI = SWITCH #( TL_ITENS-MATKL
                                                        WHEN '700150' OR '658440' OR '700155' THEN 'FT' "165455 - PQ  21.02.25
                                                        WHEN '658445' OR '658447'             THEN 'DF' "165455 - PQ  21.02.25
                                                        WHEN '700230' OR '700130'             THEN 'SS'
                                                        WHEN '700240'                         THEN 'SM'
                                                        )
                                      ]-TAXA_CAMBIO ).
*              CATCH CX_SY_CONVERSION_NO_NUMBER.
            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
              TL_ITENS-TAXA = 0.
          ENDTRY.
      ENDCASE.
    ENDIF.

    MODIFY TL_ITENS INDEX TABIX TRANSPORTING MATKL MTART ZPESAGEM TAXA.

  ENDLOOP.


  TL_ITENS_AUX[] = TL_ITENS[].
  TL_ITENS_AUX2[] = TL_ITENS[].

  SORT: TL_ITENS BY AUART MATKL SPART INCO1,
        TL_ITENS_AUX BY DTVENC ASCENDING.

  CLEAR: TL_OVS.

  APPEND TL_TEXT_IN.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      IM_TITLE = 'Texto para Ordem de Venda'
    CHANGING
      CH_TEXT  = TL_TEXTO.

  " 03.01.2023 - RAMON - MELHORIAS HEDGE - PARTE 2 #97076 RBL -->
  IF SY-UCOMM = 'CX_CANC'.
    ERRO = ABAP_TRUE.
    EXIT.
  ENDIF.
  " 03.01.2023 - RAMON - MELHORIAS HEDGE - PARTE 2 #97076 RBL --<

  LOOP AT TL_TEXTO INTO WL_TEXTO.
    TL_TEXT_IN-TEXT_LINE(72) = WL_TEXTO.
    TL_TEXT_IN-TEXT_ID    = '0002'.
    TL_TEXT_IN-LANGU      = SY-LANGU.
    TL_TEXT_IN-FORMAT_COL = '/'.
    APPEND TL_TEXT_IN.
  ENDLOOP.


* Extension - Campo ZPESAGEM
  CLEAR TL_BAPIPAREX.
  TL_BAPIPAREX-STRUCTURE     = 'BAPE_VBAK'.

  WL_BAPE_VBAK-ZPESAGEM = '02'.

  TL_BAPIPAREX-VALUEPART1 = WL_BAPE_VBAK.
  APPEND TL_BAPIPAREX.
  CLEAR TL_BAPIPAREX.
  TL_BAPIPAREX-STRUCTURE     = 'BAPE_VBAKX'.
  WL_BAPE_VBAKX-ZPESAGEM = 'X'.
  TL_BAPIPAREX-VALUEPART1 = WL_BAPE_VBAKX.
  APPEND  TL_BAPIPAREX.


  WL_HEADER_IN-SALES_ORG  = WL_HEADER-VKORG.
  WL_HEADER_IN-DISTR_CHAN = WL_HEADER-VTWEG.
  WL_HEADER_IN-SALES_OFF  = WL_HEADER-VKBUR.
  WL_HEADER_IN-SALES_GRP  = WL_HEADER-VENDEDOR(3).
  WL_HEADER_IN-PURCH_DATE   = WL_HEADER-ERDAT.
  CONCATENATE WL_HEADER-CULTURA WL_HEADER-SAFRA I_DOC_SIMULACAO INTO WL_HEADER_IN-PURCH_NO_C SEPARATED BY '-'.
  WL_HEADER_IN-CURRENCY   = WL_HEADER-WAERK.
*#165455 - 24.04.25 PQ - Quando Empresa Argentina a forma de pagamento é diferente
  IF WL_HEADER-VKORG EQ '0100'.
    WL_HEADER_IN-PYMT_METH   = 'T'.
  ELSE.
    WL_HEADER_IN-PYMT_METH   = 'P'.
  ENDIF.
  CASE WL_HEADER-TPSIM.
    WHEN 'AD' OR 'VV'.
      WL_HEADER_IN-FIX_VAL_DY = WL_HEADER-DTVENCOV.
    WHEN OTHERS.
      WL_HEADER_IN-FIX_VAL_DY = WL_HEADER-DTPGTCULT.
  ENDCASE.

  "WBARBOSA 27.06.2025"
  WL_HEADER_IN-PMNTTRMS =
  SWITCH #( WL_HEADER-TPSIM WHEN 'TS' THEN 'I001'
                            WHEN 'TT' THEN 'I008'
                            WHEN 'AD' THEN 'I002'
                            WHEN 'VV' OR 'VF' OR 'BN' THEN 'I003'
                            WHEN 'TV' THEN 'I004'
                            WHEN 'VP' THEN 'I005'
                            WHEN 'PM' THEN 'I006' ).
  "WBARBOSA 27.06.2025"

*  IF WL_HEADER-TPSIM EQ 'TT'. "WBARBOSA 27.06.2025"
*    WL_HEADER_IN-PMNTTRMS = 'I008'. "WBARBOSA 27.06.2025"
*  ELSEIF WL_HEADER-TPSIM EQ 'TS'.
*    WL_HEADER_IN-PMNTTRMS = 'I001'.
*  ELSEIF WL_HEADER-TPSIM EQ 'TV'.
*    WL_HEADER_IN-PMNTTRMS = 'I004'.
*  ELSEIF WL_HEADER-TPSIM EQ 'AD'.
*    WL_HEADER_IN-PMNTTRMS = 'I002'.
*  ELSEIF WL_HEADER-TPSIM EQ 'VV' OR WL_HEADER-TPSIM EQ 'VF' OR WL_HEADER-TPSIM EQ 'BN'.
*    WL_HEADER_IN-PMNTTRMS = 'I003'.
*  ELSEIF WL_HEADER-TPSIM EQ 'TV'.
*    WL_HEADER_IN-PMNTTRMS = 'I004'.
*  ELSEIF WL_HEADER-TPSIM EQ 'VP'.
*    WL_HEADER_IN-PMNTTRMS = 'I005'.
*  ELSEIF WL_HEADER-TPSIM EQ 'PM'.
*    WL_HEADER_IN-PMNTTRMS = 'I006'.
*  ENDIF.

*    IF WL_HEADER-TPSIM EQ 'VF'.
*      WL_HEADER_IN-EXRATE_FI = WL_HEADER_IN-EXCHG_RATE =  WL_HEADER-KURSF.
*    ELSE.
*      WL_HEADER_IN-EXRATE_FI = WL_HEADER_IN-EXCHG_RATE =  TL_ITENS-TAXA.
*    ENDIF.
*---Monta dados de Parceiro
  TL_PARTNERS-PARTN_ROLE = 'AG'.
  TL_PARTNERS-PARTN_NUMB = WL_HEADER-KUNNR.
  APPEND TL_PARTNERS.
  CLEAR TL_PARTNERS.

*SD-ZSDT0044-Gera OV 1 Item Mat ZHAW- BG - #78071 - inicio PT01

* Inclusão - RIM-SKM-IR113740-12.05.23 - INICIO
  RANGES: RG_VKORG FOR T001-BUKRS.
  DATA: IT_SETLEAF LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
        WA_SETLEAF TYPE SETLEAF.

  SELECT     *
        FROM SETLEAF
        INTO TABLE IT_SETLEAF
         WHERE SETNAME EQ 'MAGGI_ZSDT0044_ORG_ZHAW'.

  LOOP AT IT_SETLEAF INTO WA_SETLEAF.
    RG_VKORG-SIGN   = 'I'.
    RG_VKORG-OPTION = 'EQ'.
    RG_VKORG-LOW    = WA_SETLEAF-VALFROM(4).
    APPEND RG_VKORG.
    CLEAR: RG_VKORG.
  ENDLOOP.
  IF WL_HEADER-VKORG IN RG_VKORG.
* Inclusão - RIM-SKM-IR113740-12.05.23 - FIM

    DATA: CH_REF TYPE I,
          LV_REF TYPE I.
    CH_REF = 1.

    LOOP AT TL_ITENS INTO L_ITENS
      WHERE SPART EQ '02'
        AND MTART EQ 'ZHAW'.
      TABIX = SY-TABIX.

      TL_ITENS-CH_AGRUP = CH_REF.
      MODIFY TL_ITENS INDEX TABIX TRANSPORTING CH_AGRUP.
      CH_REF = CH_REF + 1.
    ENDLOOP.

  ENDIF.    "<<RIM-SKM-IR113740-12.05.23

* Inicio - 08.11.2022 SD-ZSDT0044-Erro no momento da geração OV - FA - #93914
  SORT TL_ITENS BY INCO1 SPART AUART MATKL WERKS ZPESAGEM MATNR.

  TYPES:
    BEGIN OF TY_MARA,
      MARA TYPE MARA-MATNR,
    END OF TY_MARA.

  DATA: TL_ITENS_AUX3 TYPE TABLE OF TY_ZSDT0041,
        VL_INDEX_AUX  TYPE I,
        LT_MATNR      TYPE TABLE OF TY_MARA.

  FIELD-SYMBOLS: <FS_ITENS>  TYPE TY_ZSDT0041.

  TL_ITENS_AUX3 = TL_ITENS[].

  LOOP AT TL_ITENS_AUX3 INTO DATA(WL_ITENS_AUX3) WHERE CH_AGRUP = '0'.

    READ TABLE TL_ITENS TRANSPORTING NO FIELDS WITH KEY INCO1    = WL_ITENS_AUX3-INCO1
                                                        SPART    = WL_ITENS_AUX3-SPART
                                                        AUART    = WL_ITENS_AUX3-AUART
                                                        MATKL    = WL_ITENS_AUX3-MATKL
                                                        WERKS    = WL_ITENS_AUX3-WERKS
                                                        ZPESAGEM = WL_ITENS_AUX3-ZPESAGEM
                                                        BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      VL_INDEX_AUX = SY-TABIX.

      LOOP AT TL_ITENS ASSIGNING <FS_ITENS> FROM VL_INDEX_AUX.

        IF WL_ITENS_AUX3-INCO1    <> <FS_ITENS>-INCO1 OR
           WL_ITENS_AUX3-SPART    <> <FS_ITENS>-SPART OR
           WL_ITENS_AUX3-AUART    <> <FS_ITENS>-AUART OR
           WL_ITENS_AUX3-MATKL    <> <FS_ITENS>-MATKL OR
           WL_ITENS_AUX3-WERKS    <> <FS_ITENS>-WERKS OR
           WL_ITENS_AUX3-ZPESAGEM <> <FS_ITENS>-ZPESAGEM.

          CH_REF = CH_REF + 1.
          IF LV_REF > 0.
            CH_REF = LV_REF + 1.
          ENDIF.

          CLEAR: LV_MATNR_ANT,
                 LV_REF.

          EXIT.
        ENDIF.

        IF <FS_ITENS>-CH_AGRUP IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        IF LV_MATNR_ANT = <FS_ITENS>-MATNR AND LV_REF IS INITIAL.
          LV_REF = CH_REF + 1.
          <FS_ITENS>-CH_AGRUP = LV_REF.
        ELSEIF LV_MATNR_ANT = <FS_ITENS>-MATNR.

*          READ TABLE lt_matnr TRANSPORTING NO FIELDS
*          WITH KEY  = <fs_itens>-matnr
*          BINARY SEARCH.
*          IF sy-subrc IS INITIAL.

          CONTINUE.

*          ELSE.
*
*            APPEND INITIAL LINE TO lt_matnr ASSIGNING FIELD-SYMBOL(<fs_matnr>).
*            <fs_matnr> = <fs_itens>-matnr.
*
*            <fs_itens>-ch_agrup = lv_ref.
*          ENDIF.

        ELSEIF LV_MATNR_ANT <> <FS_ITENS>-MATNR.
          <FS_ITENS>-CH_AGRUP = CH_REF.
          CLEAR LV_REF.
        ENDIF.

        LV_MATNR_ANT = <FS_ITENS>-MATNR.

      ENDLOOP.

    ENDIF.

  ENDLOOP.


* Fim - 08.11.2022 SD-ZSDT0044-Erro no momento da geração OV - FA - #93914
*  LOOP AT tl_itens INTO l_itens
*       WHERE inco1    EQ tl_itens-inco1
*         AND spart    EQ tl_itens-spart
*         AND auart    EQ tl_itens-auart
*         AND matkl    EQ tl_itens-matkl
*         AND werks    EQ tl_itens-werks
*         AND zpesagem EQ tl_itens-zpesagem.
*    tabix = sy-tabix.
*    IF l_itens-ch_agrup EQ 0.
*
**   08.11.2022 SD-ZSDT0044-Erro no momento da geração OV - FA - #93914
*
*      READ TABLE tl_itens_ov INTO wl_itens_ov WITH KEY matnr = l_itens-matnr.
*      IF sy-subrc IS INITIAL.
*
*        tl_itens-ch_agrup = ch_ref.
*
*        ch_ref = ch_ref + 1.
*        CONTINUE.
*      ENDIF.
*      tl_itens-ch_agrup = ch_ref.
*
*      MODIFY tl_itens INDEX tabix TRANSPORTING ch_agrup.
*    ENDIF.
*  ENDLOOP.
*SD-ZSDT0044-Gera OV 1 Item Mat ZHAW- BG - #78071 - Fim PT01


  LOOP AT TL_ITENS.
    CLEAR: WL_ITENS_AUX2.

    REFRESH: TL_ITEMS_IN, TL_CONDITIONS_IN, TL_SCHEDULES_IN.
* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
    REFRESH TL_ITENS_OV.
    CLEAR   WL_ITENS_OV.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5

    WL_HEADER_IN-INCOTERMS1 = TL_ITENS-INCO1.
    WL_HEADER_IN-INCOTERMS2 = TL_ITENS-INCO1.
    WL_HEADER_IN-DIVISION   = TL_ITENS-SPART.
    WL_HEADER_IN-DOC_TYPE   = TL_ITENS-AUART.
    WL_MATKL                = TL_ITENS-MATKL.

    IF WL_HEADER-TPSIM EQ 'VF'.
      WL_HEADER_IN-EXRATE_FI = WL_HEADER_IN-EXCHG_RATE =  WL_HEADER-KURSF.
    ELSE.
      WL_HEADER_IN-EXRATE_FI = WL_HEADER_IN-EXCHG_RATE =  TL_ITENS-TAXA.
    ENDIF.

    READ TABLE TL_ITENS_AUX INTO WL_ITENS_AUX2
      WITH KEY INCO1 = WL_HEADER_IN-INCOTERMS1
               SPART = WL_HEADER_IN-DIVISION
               AUART = WL_HEADER_IN-DOC_TYPE
               MATKL = WL_MATKL.

* Solicitado pela Jaqueline Denardine a remoção da Regra Abaixo
****************************************************************
*      IF WL_HEADER-TPSIM EQ 'AD'.
*        WL_HEADER_IN-FIX_VAL_DY = WL_ITENS_AUX2-DTVENC.
*      ELSEIF WL_HEADER-TPSIM EQ 'VV'.
*        WL_HEADER_IN-FIX_VAL_DY = WL_ITENS_AUX2-DTVENC.
*      ENDIF.
***************************************************************



    CLEAR: WL_POSNR.
*SD-ZSDT0044-Gera OV 1 Item Mat ZHAW- BG - #78071 - Inicio PT 02
    LOOP AT TL_ITENS INTO WL_ITENS
      WHERE CH_AGRUP EQ TL_ITENS-CH_AGRUP.
*      WHERE  inco1 EQ wl_header_in-incoterms1
*        AND spart EQ wl_header_in-division
*        AND auart EQ wl_header_in-doc_type
*        AND matkl EQ wl_matkl
*        AND werks EQ tl_itens-werks
*        AND zpesagem EQ tl_itens-zpesagem.
*
** Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
*     READ TABLE tl_itens_ov INTO wl_itens_ov WITH KEY matnr = wl_itens-matnr.
*     IF sy-subrc IS INITIAL.
*       CONTINUE.
*      ENDIF.
** Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5

*SD-ZSDT0044-Gera OV 1 Item Mat ZHAW- BG - #78071 - FIM PT02

      CLEAR: TL_0048, TL_ITEMS_IN.

      IF WL_ITENS-SPART EQ '03'.
        READ TABLE TL_0048
          WITH KEY BUKRS = WL_HEADER-VKORG
                   VTWEG = WL_HEADER-VTWEG
                   SPART = WL_ITENS-SPART
                   VKBUR = WL_HEADER-VKBUR.

        MOVE: TL_0048-LGORT TO TL_ITEMS_IN-STORE_LOC.

      ELSEIF WL_ITENS-SPART EQ '04'.
        READ TABLE TL_0048
        WITH KEY BUKRS   = WL_HEADER-VKORG
                 VTWEG   = WL_HEADER-VTWEG
                 SPART   = WL_ITENS-SPART
                 WERKS_D = WL_ITENS-WERKS.
*                   vkbur   = space.
        MOVE: TL_0048-LGORT TO TL_ITEMS_IN-STORE_LOC.
***165455 - PQ  21.02.25
      ELSEIF WL_ITENS-SPART EQ '12'.  "fertilizantes argentina
        READ TABLE TL_0048
          WITH KEY BUKRS = WL_HEADER-VKORG
                   VTWEG = WL_HEADER-VTWEG
                   SPART = WL_ITENS-SPART
                   VKBUR = WL_HEADER-VKBUR.

        MOVE: TL_0048-LGORT TO TL_ITEMS_IN-STORE_LOC.

      ELSEIF WL_ITENS-SPART EQ '13'.  "defensivos argentina
        READ TABLE TL_0048
          WITH KEY BUKRS = WL_HEADER-VKORG
                   VTWEG = WL_HEADER-VTWEG
                   SPART = WL_ITENS-SPART
                   VKBUR = WL_HEADER-VKBUR.
***165455 - PQ  21.02.25

        MOVE: TL_0048-LGORT TO TL_ITEMS_IN-STORE_LOC.
      ENDIF.

      ADD 1 TO WL_POSNR.
      TL_ITEMS_IN-ITM_NUMBER   =  WL_POSNR * 10.
      TL_ITEMS_IN-MATERIAL     = WL_ITENS-MATNR.

      IF ( WL_HEADER_IN-DOC_TYPE  EQ 'ZFTE' OR
           WL_HEADER_IN-DOC_TYPE  EQ 'YFTE' OR "165455 - PQ  21.02.25
           WL_HEADER_IN-DOC_TYPE  EQ 'ZSEM' OR
           WL_HEADER_IN-DOC_TYPE  EQ 'ZOSM' OR
           WL_HEADER_IN-DOC_TYPE  EQ 'ZOFE' )
      AND WL_ITENS-ZIEME EQ 'TO'.
        TL_ITEMS_IN-TARGET_QTY   = WL_VLR_COVERT = ( WL_ITENS-ZMENG * 1000 ).
        TL_ITEMS_IN-TARGET_QU    = 'KG'.

      ELSE.
        TL_ITEMS_IN-TARGET_QTY   = WL_VLR_COVERT = WL_ITENS-ZMENG.
        TL_ITEMS_IN-TARGET_QU    = WL_ITENS-ZIEME.
      ENDIF.

      CLEAR: TL_MARA.
      READ TABLE TL_MARA
        WITH KEY MATNR = WL_ITENS-MATNR.
      IF ( WL_HEADER_IN-DOC_TYPE  EQ 'ZFTE' OR
           WL_HEADER_IN-DOC_TYPE  EQ 'YFTE' OR "165455 - PQ  21.02.25
           WL_HEADER_IN-DOC_TYPE  EQ 'ZSEM' OR
           WL_HEADER_IN-DOC_TYPE  EQ 'ZOSM' OR
           WL_HEADER_IN-DOC_TYPE  EQ 'ZOFE' )
      AND WL_ITENS-ZIEME EQ 'TO'.
        TL_ITEMS_IN-SALES_UNIT   = 'KG'.
      ELSE.
        TL_ITEMS_IN-SALES_UNIT   = WL_ITENS-ZIEME.
      ENDIF.

      IF WL_HEADER-TPSIM EQ 'VF'.
        TL_ITEMS_IN-USAGE_IND    = 'S'.
      ELSE.
        TL_ITEMS_IN-USAGE_IND    = 'I'.
      ENDIF.

      TL_ITEMS_IN-PLANT        = WL_ITENS-WERKS.
      TL_ITEMS_IN-BATCH        = WL_ITENS-CHARG.
      TL_ITEMS_IN-SHIP_POINT   = WL_ITENS-WERKS.

      IF TL_ITEMS_IN-STORE_LOC IS INITIAL.

**** CS2022000324
*          IF wl_itens-werks EQ '0175' AND
        IF WL_ITENS-WERKS IN R_WERKS AND
           TL_MARA[ MATNR = WL_ITENS-MATNR ]-MTART EQ 'ZFER'.

          TL_ITEMS_IN-STORE_LOC    = 'PR01'.

* Extension - Campo ZPESAGEM
          FREE TL_BAPIPAREX.
          CLEAR TL_BAPIPAREX.
          TL_BAPIPAREX-STRUCTURE     = 'BAPE_VBAK'.

          WL_BAPE_VBAK-ZPESAGEM = '01'.

          TL_BAPIPAREX-VALUEPART1 = WL_BAPE_VBAK.
          APPEND TL_BAPIPAREX.
          CLEAR TL_BAPIPAREX.
          TL_BAPIPAREX-STRUCTURE     = 'BAPE_VBAKX'.
          WL_BAPE_VBAKX-ZPESAGEM = 'X'.
          TL_BAPIPAREX-VALUEPART1 = WL_BAPE_VBAKX.
          APPEND  TL_BAPIPAREX.

        ELSE.
          TL_ITEMS_IN-STORE_LOC    = 'IN01'.
        ENDIF.

      ENDIF.

      TL_ITEMS_IN-MATFRGTGRP   = '00000001'.

      APPEND TL_ITEMS_IN.

      CLEAR: TL_CONDITIONS_IN.
      TL_CONDITIONS_IN-ITM_NUMBER  = TL_ITEMS_IN-ITM_NUMBER .
      TL_CONDITIONS_IN-CURRENCY    = WL_HEADER-WAERK.

      IF ( WL_HEADER_IN-DOC_TYPE EQ 'ZFTE' OR
           WL_HEADER_IN-DOC_TYPE EQ 'YFTE' OR "165455 - PQ  21.02.25
           WL_HEADER_IN-DOC_TYPE EQ 'ZSEM' OR
           WL_HEADER_IN-DOC_TYPE EQ 'ZOSM' OR
           WL_HEADER_IN-DOC_TYPE EQ 'ZOFE')
      AND WL_ITENS-ZIEME EQ 'TO'.
*          tl_conditions_in-cond_value  = ( wl_itens-zwert / 1000 ).
*          TL_CONDITIONS_IN-COND_VALUE  = WL_ITENS-ZWERT.
        TL_CONDITIONS_IN-COND_VALUE  = WL_ITENS-ZWERT_LIQDO.
        TL_CONDITIONS_IN-COND_UNIT  = WL_ITENS-ZIEME.

      ELSE.
*          TL_CONDITIONS_IN-COND_VALUE  = WL_ITENS-ZWERT.
        TL_CONDITIONS_IN-COND_VALUE  = WL_ITENS-ZWERT_LIQDO.

      ENDIF.


      IF WL_HEADER-TPSIM EQ 'VF'.
        TL_CONDITIONS_IN-CONEXCHRAT  = WL_HEADER-KURSF.
* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
        TL_CONDITIONS_IN-COND_VALUE  = WL_ITENS-ZWERT.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5

      ENDIF.
      TL_CONDITIONS_IN-COND_TYPE   = 'PR00'.

      APPEND TL_CONDITIONS_IN.

      IF WL_ITENS-VLR_AJUSTE IS NOT INITIAL.

        DATA(COEFICIENTE) =
          ZCL_SOLICITACAO_OV=>GET_IMPOSTO(
          _CLIENTE    = WL_HEADER-KUNNR
          _FORNECEDOR = CONV #( |{ WL_ITENS-WERKS ALPHA = IN }| )
          _MATERIAL   = WL_ITENS-MATNR
          _TIPO_ORDEM = WL_ITENS-AUART
          _DIRECAO    = 'D'
          _WERKS      = WL_ITENS-WERKS  "<<RIM-SKM-IR120585-23.12.22
        ).

        IF COEFICIENTE IS INITIAL.
          COEFICIENTE = 1.
        ELSE.
          DIVIDE COEFICIENTE BY 10.
        ENDIF.

        MULTIPLY WL_ITENS-VLR_AJUSTE BY COEFICIENTE.

        IF WL_ITENS-VLR_AJUSTE IS NOT INITIAL.

          APPEND VALUE #(
                          ITM_NUMBER  = TL_ITEMS_IN-ITM_NUMBER
                          COND_TYPE   = 'RB00'
                          COND_VALUE  = WL_ITENS-VLR_AJUSTE
                          COND_UNIT  = ABAP_FALSE
                        ) TO TL_CONDITIONS_IN[].

          APPEND VALUE #(
                          UPDATEFLAG  = 'U'
                          COND_COUNT  = '01'
                          ITM_NUMBER  = TL_CONDITIONS_IN-ITM_NUMBER
                          COND_TYPE   = 'RB00'
                          COND_VALUE = ABAP_TRUE
                          COND_UNIT  = ABAP_TRUE
                        ) TO TL_CONDITIONS_INX[].
        ENDIF.
      ENDIF.

*       ---Monta dados de Parceiro PONTO DE COLETA
      IF WL_ITENS-WERKS IS NOT INITIAL.

        READ TABLE TL_PARTNERS
          WITH KEY PARTN_ROLE = 'PC'.
        IF SY-SUBRC IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WL_ITENS-WERKS
            IMPORTING
              OUTPUT = TL_PARTNERS-PARTN_NUMB.

          MODIFY TL_PARTNERS INDEX SY-TABIX.
        ELSE.
          TL_PARTNERS-PARTN_ROLE = 'PC'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WL_ITENS-WERKS
            IMPORTING
              OUTPUT = TL_PARTNERS-PARTN_NUMB.

          APPEND TL_PARTNERS.
        ENDIF.
        CLEAR TL_PARTNERS.
      ENDIF.

* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
      "Nos casos onde se tem mais de um item com o mesmo código de material e com as demais caracteriais iguais,
      " o sistema deverá gerar ordens diferentes para esses itens
      WL_ITENS_OV = WL_ITENS.
      APPEND WL_ITENS_OV TO TL_ITENS_OV.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5

      CLEAR: TL_SCHEDULES_IN.
      TL_SCHEDULES_IN-ITM_NUMBER = TL_ITEMS_IN-ITM_NUMBER.
      TL_SCHEDULES_IN-REQ_QTY    = WL_VLR_COVERT.
      TL_SCHEDULES_IN-REQ_DLV_BL    = '10'.
*        tl_schedules_in-req_qty    = tl_itens-zmeng.
      APPEND TL_SCHEDULES_IN.
      CLEAR: TL_ITEMS_IN, TL_CONDITIONS_IN, TL_SCHEDULES_IN, WL_ITENS.


      DELETE TL_ITENS.
    ENDLOOP.

**** CS2022000324

    SELECT SINGLE *
      FROM ZSDT0293
      INTO @DATA(LWA_ZSDT0293)
      WHERE TIPO_REGISTRO = 'M'
        AND WERKS = @TL_ITENS-WERKS
        AND STATUS <> 'X'.

*      IF tl_itens-werks EQ '0175'
*      IF tl_itens-werks IN r_werks
    IF SY-SUBRC IS INITIAL
     AND TL_ITENS-SPART EQ '02'.
*        AND tl_itens-mtart EQ 'ZFER'.  Foi retirado esta verificação, pois o texto deve sair em todos os materias da fábrica
*
      CLEAR: TL_TEXT_IN.

      TL_TEXT_IN-TEXT_LINE(72) = LWA_ZSDT0293-TEXT_REGISTRO.

*        IF tl_itens-werks EQ '0175'.
*          tl_text_in-text_line(72) = '/REG. ESTABELECIMENTO MAPA: EP MT 000293-3 / IND. BRASILEIRA'.
*        ELSEIF tl_itens-werks EQ '0124'.
*          tl_text_in-text_line(72) = '/REG. ESTABELECIMENTO MAPA: EP MT XXXXXX-X / IND. BRASILEIRA'.
*        ENDIF.


      TL_TEXT_IN-TEXT_ID    = '0002'.
      TL_TEXT_IN-LANGU      = SY-LANGU.
      TL_TEXT_IN-FORMAT_COL = '/'.

      MODIFY TL_TEXT_IN INDEX 1.
    ENDIF.
**** CS2022000324

    DATA: LOGIC_SWITCH TYPE BAPISDLS.

    LOGIC_SWITCH-COND_HANDL   = ABAP_TRUE.

* Criar Ordem
    CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
      EXPORTING
        SALES_HEADER_IN      = WL_HEADER_IN
        SALES_HEADER_INX     = WL_HEADER_INX
        LOGIC_SWITCH         = LOGIC_SWITCH
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
    REFRESH:IT_MSG, TL_RETURN2,TL_RETURN3.
    IF NOT WL_VBELN IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CLEAR: TL_TEXT_IN.
      MODIFY TL_TEXT_IN INDEX 1.

** Verifica se existem ocorrências na criação da ordem
      REFRESH: TL_VBUV.
      SELECT *
        FROM VBUV
        INTO TABLE TL_VBUV
         WHERE VBELN EQ WL_VBELN.

      IF SY-SUBRC IS INITIAL.
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
        CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            SALESDOCUMENT    = WL_VBELN
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
*"// US-169490 WBARBOSA 12/08/2025 INICIO
        SELECT VBELN, POSNR
          FROM VBAP
          INTO TABLE @DATA(LT_VBAP_DESC)
          WHERE VBELN EQ @WL_VBELN.

        LOOP AT LT_VBAP_DESC INTO DATA(LS_VBAP_DESC).

          CALL METHOD ZCL_MANUTENCAO_INSUMOS=>SET_DESCONTO_ABS_OV
            EXPORTING
              I_VBELN  = LS_VBAP_DESC-VBELN
              I_POSNR  = LS_VBAP_DESC-POSNR
            IMPORTING
              R_RETURN = DATA(E_RETURN).

          APPEND LINES OF E_RETURN TO TE_RETURN.

        ENDLOOP.
*"// US-169490 WBARBOSA 12/08/2025 FIM

        MOVE WL_VBELN TO TL_OVS-VBELN.
        APPEND TL_OVS.
        CLEAR: TL_OVS.
* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
*          LOOP AT TL_ITENS_AUX
*          WHERE INCO1 EQ WL_HEADER_IN-INCOTERMS1
*            AND SPART EQ WL_HEADER_IN-DIVISION
*            AND AUART EQ WL_HEADER_IN-DOC_TYPE
*            AND MATKL EQ TL_ITENS-MATKL
*            AND WERKS EQ TL_ITENS-WERKS
*            AND ZPESAGEM EQ TL_ITENS-ZPESAGEM.
*
*            MOVE WL_VBELN TO TL_ITENS_AUX-VBELN.
*            MODIFY TL_ITENS_AUX.
*          ENDLOOP.
*          MODIFY ZSDT0041 FROM TABLE TL_ITENS_AUX.
        LOOP AT TL_ITENS_OV INTO WL_ITENS_OV.
          MOVE WL_VBELN TO WL_ITENS_OV-VBELN.
          MODIFY TL_ITENS_OV FROM WL_ITENS_OV.
        ENDLOOP.
        MODIFY ZSDT0041 FROM TABLE TL_ITENS_OV.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
* Inicio - RRIBEIRO - IR1233340 - 15/04/2025 - Stefanini
        IF SY-SUBRC IS INITIAL.
          COMMIT WORK AND WAIT.
        ENDIF.
* Fim - RRIBEIRO - IR1233340 - 15/04/2025 - Stefanini

        IF WL_HEADER-TPSIM EQ 'VF'. "Faturar ZFUT
          PERFORM F_SHDB_FAT(ZSDR016) TABLES IT_MSG CHANGING WL_VBELN WL_ERRO WG_DOCUMENTO.
          WAIT UP TO 10 SECONDS.
          IF WL_ERRO NE 'X'. " Criar remessa aqui
            CALL FUNCTION 'ZSDMF001_GERA_OV_SIMU_FUT'
              EXPORTING
                I_VBELN   = WG_DOCUMENTO
              IMPORTING
                E_VBELN   = RT_VBELN
              TABLES
                TE_RETURN = TL_RETURN2.
            IF RT_VBELN IS INITIAL.
              APPEND LINES OF TL_RETURN2 TO TE_RETURN.
* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
              "Caso Retorne erro na geração da ZRFU , O sistema deverá fazer:
              " - VF11 para estornar a fatura da ZFUT;
              " - Atualizar a quantidade da OV ZFUT para Zero;
              " - Atualizar a tabela ZSDT0041, para limpar o campo VBELN que foi  preenchido com o numero da ZFUT;
              PERFORM ZF_ESTORNO_ERRO_ZRFU USING WG_DOCUMENTO.
              PERFORM ZF_ZERA_QTDE_OV_ZFUT USING WL_VBELN.
              " Elimina
              LOOP AT TL_ITENS_OV INTO WL_ITENS_OV.
                CLEAR WL_ITENS_OV-VBELN.
                MODIFY TL_ITENS_OV FROM WL_ITENS_OV.
              ENDLOOP.
              MODIFY ZSDT0041 FROM TABLE TL_ITENS_OV.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
            ELSE.
              "Grava a OV gerada na 41

* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
              " Antes gravava o número gerado na tabela ZSDT0041-VBELN sobrescrevendo o
              " número da OV ZFUT que foi gravado no mesmo campo
              " Agora, o sistema deverá inserir um registro na tabela ZSDT0090,
              "levando em consideração que a OV velha é a ZFUT e a OV nova é a ZRFU

*                      LOOP AT TL_ITENS_AUX
*                         WHERE INCO1 EQ WL_HEADER_IN-INCOTERMS1
*                           AND SPART EQ WL_HEADER_IN-DIVISION
*                           AND AUART EQ WL_HEADER_IN-DOC_TYPE
*                           AND MATKL EQ TL_ITENS-MATKL
*                           AND WERKS EQ TL_ITENS-WERKS
*                           AND ZPESAGEM EQ TL_ITENS-ZPESAGEM.
*
*                  MOVE RT_VBELN TO TL_ITENS_AUX-VBELN.
*                  MODIFY TL_ITENS_AUX.
*                ENDLOOP.
*                MODIFY ZSDT0041 FROM TABLE TL_ITENS_AUX.

              LOOP AT TL_ITENS_OV INTO WL_ITENS_OV.
                PERFORM INSERT_ZSDT0090(ZSDR0042)
                 USING ''
                       WL_ITENS_OV-DOC_SIMULACAO
                       RT_VBELN                           " OV ZRFU
                       WL_ITENS_OV-VBELN                  " OV ZFUT
                       WL_ITENS_OV-MATNR
                 CHANGING P_0090.
              ENDLOOP.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
            ENDIF.
          ELSE. " Elimina a ordem gerada
            CLEAR F_HEADINX.
            F_HEADINX-UPDATEFLAG = 'D'.
            "*---> 12/07/2023 - Migração S4 - LO --> Material não foi utilizado
            CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                SALESDOCUMENT    = WL_VBELN
                ORDER_HEADER_INX = F_HEADINX
              TABLES
                RETURN           = TL_RETURN2.

            COMMIT WORK.
* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
*              LOOP AT TL_ITENS_AUX WHERE VBELN = WL_VBELN.
*                CLEAR TL_ITENS_AUX-VBELN.
*                MODIFY TL_ITENS_AUX INDEX SY-TABIX TRANSPORTING VBELN.
*              ENDLOOP.
* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5

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
        ENDIF.
        COMMIT WORK.
      ENDIF.
    ELSE.
      APPEND LINES OF TL_RETURN TO TE_RETURN.
    ENDIF.

    CLEAR: TL_SAIDA_EXEC.
    MOVE: TL_ITENS-INCO1 TO TL_SAIDA_EXEC-INCO1,
          TL_ITENS-SPART TO TL_SAIDA_EXEC-SPART,
          TL_ITENS-AUART TO TL_SAIDA_EXEC-AUART,
          TL_ITENS-WERKS TO TL_SAIDA_EXEC-WERKS,
          WL_VBELN       TO TL_SAIDA_EXEC-VBELN.

    IF WL_VBELN IS NOT INITIAL.
      APPEND TL_SAIDA_EXEC.
    ELSE.
      LOOP AT TL_RETURN WHERE TYPE EQ 'E'.
        MOVE: TL_RETURN-MESSAGE TO TL_SAIDA_EXEC-MSG.
        APPEND TL_SAIDA_EXEC.
      ENDLOOP.
    ENDIF.

    IF WL_HEADER-TPSIM EQ 'VF'.
      CLEAR: TL_SAIDA_EXEC.
      MOVE: TL_ITENS-INCO1 TO TL_SAIDA_EXEC-INCO1,
            TL_ITENS-SPART TO TL_SAIDA_EXEC-SPART,
            TL_ITENS-AUART TO TL_SAIDA_EXEC-AUART,
            TL_ITENS-WERKS TO TL_SAIDA_EXEC-WERKS,
            RT_VBELN       TO TL_SAIDA_EXEC-VBELN.
      IF RT_VBELN IS NOT INITIAL.
        APPEND TL_SAIDA_EXEC.
      ELSE.
        LOOP AT TL_RETURN2.
          MOVE: TL_RETURN2-MESSAGE TO TL_SAIDA_EXEC-MSG.
          APPEND TL_SAIDA_EXEC.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF TL_OVS[] IS NOT INITIAL.
*-CS2019001753-05.01.2023-#65741-JT-inicio
*   PERFORM f_cria_tabelas_ov USING i_doc_simulacao.
*-CS2019001753-05.01.2023-#65741-JT-fim

    SELECT *
      FROM VBAK
      INTO TABLE TE_VBAK
       FOR ALL ENTRIES IN TL_OVS
        WHERE VBELN EQ TL_OVS-VBELN.

    IF SY-SUBRC IS INITIAL.
      SELECT *
        FROM VBAP
        INTO TABLE TE_VBAP
         FOR ALL ENTRIES IN TL_OVS
          WHERE VBELN EQ TL_OVS-VBELN.
    ENDIF.
  ENDIF.

  IF TL_SAIDA_EXEC[] IS NOT INITIAL.
    PERFORM MONTAR_LAYOUT.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       i_callback_program    = v_report
*       I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
        IT_FIELDCAT           = ESTRUTURA[]
*       IT_SORT               = T_SORT[]
        I_SAVE                = 'A'
        I_SCREEN_START_COLUMN = 3
        I_SCREEN_START_LINE   = 3
        I_SCREEN_END_COLUMN   = 100
        I_SCREEN_END_LINE     = 13
      TABLES
        T_OUTTAB              = TL_SAIDA_EXEC.
  ENDIF.

ENDFUNCTION.
