*&---------------------------------------------------------------------*
*& Report  ZLESR0095
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0095.

TABLES: zlest0019, makt.

DATA: it_ferro      TYPE TABLE OF zde_ferroviario_alv WITH HEADER LINE,
      it_ferro_sel  TYPE TABLE OF zde_ferroviario_alv WITH HEADER LINE,
      it_ferro_nota TYPE TABLE OF zde_ferroviario_notas_alv WITH HEADER LINE,
      wa_ferro      TYPE zde_ferroviario_alv,
      ok_code       TYPE sy-ucomm.

DATA: ctl_con_0100       TYPE REF TO cl_gui_custom_container,
      gs_lay_0100        TYPE lvc_s_layo,
      gs_var_0100        TYPE disvariant,
      gs_scroll_col_0100 TYPE lvc_s_col,
      gs_scroll_row_0100 TYPE lvc_s_roid,
      it_catalog_0100    TYPE lvc_t_fcat,
      ctl_alv_0100       TYPE REF TO cl_gui_alv_grid.

DATA: it_exclude_0100 TYPE ui_functions,
      wa_exclude_0100 LIKE LINE OF it_exclude_0100.

DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.

INCLUDE zlesr0095_0102.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:     p_base   TYPE zlest0019-erdat DEFAULT sy-datum OBLIGATORY.
  SELECT-OPTIONS: p_erdats FOR zlest0019-erdat   NO-EXTENSION, " Período data Saída
                  p_erdatc FOR zlest0019-erdat   NO-EXTENSION. " Período data Chegada
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS: p_bukrs  FOR zlest0019-bukrs  NO-EXTENSION NO INTERVALS, " Empresa
                  p_werks  FOR zlest0019-branch NO-EXTENSION NO INTERVALS, " Filial de Carregamento
                  p_matnr  FOR makt-matnr       NO-EXTENSION NO INTERVALS,       " Material
                  p_nfenum FOR zlest0019-nfenum NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: p_zplaca FOR zlest0019-idvagao, "NO-EXTENSION NO INTERVALS, " Placa do Vagão
                  p_dcl    FOR zlest0019-dcl    . "NO-EXTENSION NO INTERVALS. " DCL
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_localc LIKE lfa1-lifnr,     " Local de Carregamento
              p_locald LIKE lfa1-lifnr, " Destinatário
              p_localt LIKE lfa1-lifnr. " Terminal de Descarga
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    s_trans TYPE char1 RADIOBUTTON GROUP rb01 , "USER-COMMAND ACT,
    s_trann TYPE char1 RADIOBUTTON GROUP rb01 , "USER-COMMAND ACT,
    s_todos TYPE char1 RADIOBUTTON GROUP rb01 DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b5.

*SELECTION-SCREEN: BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-006.
*PARAMETERS: CK_CARGA AS CHECKBOX.
*SELECTION-SCREEN: END OF BLOCK B6.

START-OF-SELECTION.

  DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string.

  DATA:
    qtd_registros       TYPE i,
    qtd_registros2      TYPE i,
    it_zlest0019_l2_20  TYPE TABLE OF zlest0019_l2_20 WITH HEADER LINE,
    it_zlest0019_l2_30  TYPE TABLE OF zlest0019_l2_30 WITH HEADER LINE,
    it_zlest0019_l3_20  TYPE TABLE OF zlest0019_l3_20 WITH HEADER LINE,
    it_zlest0019_l3_30  TYPE TABLE OF zlest0019_l3_30 WITH HEADER LINE,
    it_j_1bnfdoc        TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
    it_j_1bnfnad        TYPE TABLE OF j_1bnfnad WITH HEADER LINE,
    it_makt             TYPE TABLE OF makt WITH HEADER LINE,
    it_lfa1             TYPE TABLE OF lfa1 WITH HEADER LINE,
    ck_p_bukrs          TYPE char01,
    ck_p_werks          TYPE char01,
    ck_p_matnr          TYPE char01,
    ck_p_nfenum         TYPE char01,
    ck_p_localc         TYPE char01,
    ck_p_locald         TYPE char01,
    ck_p_localt         TYPE char01,
    it_zlest0019_l2_20b TYPE TABLE OF zlest0019_l2_20 WITH HEADER LINE,
    it_zlest0019_l3_20b TYPE TABLE OF zlest0019_l3_20 WITH HEADER LINE.

*   IF 1 = 2.
*      PERFORM mostrar_registro_notas.
*   ENDIF.

*  IF CK_CARGA EQ ABAP_TRUE.
*    PERFORM CARGA_DE_TABELAS.
*    RETURN.
*  ENDIF.

  CLEAR: it_ferro[], it_ferro.

  CLEAR: it_zlest0019_l2_20[],
         it_zlest0019_l2_30[],
         it_zlest0019_l3_20[],
         it_zlest0019_l3_30[],
         it_j_1bnfdoc[],
         it_j_1bnfnad[],
         it_makt[].

  PERFORM mostra_texto USING 'Pesquisa: L2/20'.

  SELECT * INTO TABLE it_zlest0019_l2_20
    FROM zlest0019_l2_20
   WHERE erdat      LE p_base
     AND idvagao    IN p_zplaca
     AND dcl        IN p_dcl
     AND dtadecarga IN p_erdats.

  IF it_zlest0019_l2_20[] IS NOT INITIAL.

    PERFORM mostra_texto USING 'Pesquisa: L2/30'.

    SELECT * INTO TABLE it_zlest0019_l2_30
      FROM zlest0019_l2_30
       FOR ALL ENTRIES IN it_zlest0019_l2_20
     WHERE erdat      LE p_base
       AND dcl        EQ it_zlest0019_l2_20-dcl
       AND id_refkey  EQ it_zlest0019_l2_20-id_refkey
       AND bukrs      IN p_bukrs
       AND branch     IN p_werks
       AND nfenum     IN p_nfenum
       AND matnr      IN p_matnr.

    PERFORM mostra_texto USING 'Pesquisa: L3/20'.

    SELECT * INTO TABLE it_zlest0019_l3_20
      FROM zlest0019_l3_20
       FOR ALL ENTRIES IN it_zlest0019_l2_20
     WHERE erdat      LE p_base
       AND idvagao    EQ it_zlest0019_l2_20-idvagao
       AND dcl        EQ it_zlest0019_l2_20-dcl.

    PERFORM mostra_texto USING 'Pesquisa: Local de Carregamento'.

    MOVE it_zlest0019_l2_20[] TO it_zlest0019_l2_20b[].
    SORT it_zlest0019_l2_20b BY cnpjferro.
    DELETE ADJACENT DUPLICATES FROM it_zlest0019_l2_20b COMPARING cnpjferro.
    DELETE it_zlest0019_l2_20b WHERE cnpjferro EQ space.

    IF it_zlest0019_l2_20b[] IS NOT INITIAL.
      SELECT * INTO TABLE it_lfa1
        FROM lfa1
         FOR ALL ENTRIES IN it_zlest0019_l2_20b
       WHERE stcd1 EQ it_zlest0019_l2_20b-cnpjferro.
    ENDIF.

    CLEAR: it_zlest0019_l2_20b[].

  ENDIF.

  IF it_zlest0019_l3_20[] IS NOT INITIAL.

    PERFORM mostra_texto USING 'Pesquisa: L3/30'.

    SELECT * INTO TABLE it_zlest0019_l3_30
      FROM zlest0019_l3_30
       FOR ALL ENTRIES IN it_zlest0019_l3_20
     WHERE erdat      LE p_base
       AND dcl        EQ it_zlest0019_l3_20-dcl
       AND id_refkey  EQ it_zlest0019_l3_20-id_refkey
       AND bukrs      IN p_bukrs
       AND branch     IN p_werks
       AND nfenum     IN p_nfenum
       AND matnr      IN p_matnr.

    PERFORM mostra_texto USING 'Pesquisa: Local de Entrega Efetivo'.

    MOVE it_zlest0019_l3_20[] TO it_zlest0019_l3_20b[].
    SORT it_zlest0019_l3_20b BY cnpjferro.
    DELETE ADJACENT DUPLICATES FROM it_zlest0019_l3_20b COMPARING cnpjferro.
    DELETE it_zlest0019_l3_20b WHERE cnpjferro EQ space.

    IF it_zlest0019_l3_20b[] IS NOT INITIAL.
      SELECT * APPENDING TABLE it_lfa1
        FROM lfa1
         FOR ALL ENTRIES IN it_zlest0019_l3_20b
       WHERE stcd1 EQ it_zlest0019_l3_20b-cnpjferro.
    ENDIF.

    CLEAR: it_zlest0019_l3_20b[].

  ENDIF.

  SORT it_lfa1 BY stcd1.

  IF it_zlest0019_l2_30[] IS NOT INITIAL.

    PERFORM mostra_texto USING 'Pesquisa: Notas Fiscais'.

    SELECT * INTO TABLE it_j_1bnfdoc
      FROM j_1bnfdoc
       FOR ALL ENTRIES IN it_zlest0019_l2_30
     WHERE docnum  EQ it_zlest0019_l2_30-docnum.

    SORT it_j_1bnfdoc BY docnum.
  ENDIF.

  IF it_j_1bnfdoc[] IS NOT INITIAL.
    PERFORM mostra_texto USING 'Pesquisa: Notas Fiscais - Transbordo'.

    SELECT * INTO TABLE it_j_1bnfnad
      FROM j_1bnfnad
       FOR ALL ENTRIES IN it_j_1bnfdoc
      WHERE docnum EQ it_j_1bnfdoc-docnum
        AND parvw  EQ 'Z1'.
  ENDIF.

  IF it_zlest0019_l2_30[] IS NOT INITIAL.

    PERFORM mostra_texto USING 'Pesquisa: Materiais'.

    SELECT * INTO TABLE it_makt
      FROM makt
       FOR ALL ENTRIES IN it_zlest0019_l2_30
     WHERE spras EQ sy-langu
       AND matnr EQ it_zlest0019_l2_30-matnr.

    SORT it_makt BY matnr.
  ENDIF.

  DESCRIBE TABLE it_zlest0019_l2_20 LINES qtd_registros.

  LOOP AT it_zlest0019_l2_20.

    PERFORM mostra_texto_p USING 'Gerando Relatório' qtd_registros sy-tabix.

    ck_p_bukrs  = abap_true.
    ck_p_werks  = abap_true.
    ck_p_matnr  = abap_true.
    ck_p_nfenum = abap_true.
    ck_p_localc = abap_true.
    ck_p_locald = abap_true.
    ck_p_localt = abap_true.

    IF p_bukrs-low IS NOT INITIAL.
      ck_p_bukrs = abap_false.
    ENDIF.

    IF p_werks-low IS NOT INITIAL.
      ck_p_werks = abap_false.
    ENDIF.

    IF p_matnr-low IS NOT INITIAL.
      ck_p_matnr = abap_false.
    ENDIF.

    IF p_nfenum-low IS NOT INITIAL.
      ck_p_nfenum = abap_false.
    ENDIF.

    IF p_localc IS NOT INITIAL.
      ck_p_localc = abap_false.
    ENDIF.

    IF p_locald IS NOT INITIAL.
      ck_p_locald = abap_false.
    ENDIF.

    IF p_localt IS NOT INITIAL.
      ck_p_localt = abap_false.
    ENDIF.

    CLEAR: wa_ferro.

    wa_ferro-mandt          = it_zlest0019_l2_20-mandt.
    wa_ferro-idinter        = it_zlest0019_l2_20-idinter.
    wa_ferro-tp_movi        = it_zlest0019_l2_20-tp_movi.
    wa_ferro-tp_reg         = it_zlest0019_l2_20-tp_reg.
    wa_ferro-chave          = it_zlest0019_l2_20-chave.
    wa_ferro-dcl            = it_zlest0019_l2_20-dcl.
    wa_ferro-seriedcl       = it_zlest0019_l2_20-seriedcl.
    wa_ferro-id_refkey      = it_zlest0019_l2_20-id_refkey.
    wa_ferro-idvagao        = it_zlest0019_l2_20-idvagao.
    wa_ferro-status_duplica = it_zlest0019_l2_20-status_duplica.
    wa_ferro-observacao     = it_zlest0019_l2_20-observacao.
    wa_ferro-loc_cnpj       = it_zlest0019_l2_20-cnpjferro.
    wa_ferro-sai_dt_saida   = it_zlest0019_l2_20-dtadecarga.
    wa_ferro-sai_hr_saida   = it_zlest0019_l2_20-horadescarga.
    wa_ferro-sai_ps_saida   = it_zlest0019_l2_20-pesovagao.

    wa_ferro-ds_transito          = 'Sim'.
    wa_ferro-chg_ps_chegada_notas = 0.

    LOOP AT it_zlest0019_l3_20 WHERE idvagao EQ it_zlest0019_l2_20-idvagao
                                 AND dcl     EQ it_zlest0019_l2_20-dcl.
      IF it_zlest0019_l3_20-dtadecarga IS NOT INITIAL.
        wa_ferro-ds_transito = 'Não'.
      ENDIF.
      wa_ferro-chg_cnpj       = it_zlest0019_l3_20-cnpjferro.
      wa_ferro-chg_dt_chegada = it_zlest0019_l3_20-dtadecarga.
      wa_ferro-chg_hr_chegada = it_zlest0019_l3_20-horadescarga.
      wa_ferro-chg_ps_chegada = it_zlest0019_l3_20-pesovagao.

      LOOP AT it_zlest0019_l3_30 WHERE dcl       EQ it_zlest0019_l3_20-dcl
                                   AND id_refkey EQ it_zlest0019_l3_20-id_refkey.
        ADD it_zlest0019_l3_30-pesodvagao TO wa_ferro-chg_ps_chegada_notas.
      ENDLOOP.

      READ TABLE it_lfa1 WITH KEY stcd1 = it_zlest0019_l3_20-cnpjferro BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_ferro-chg_lifnr     = it_lfa1-lifnr.
        wa_ferro-chg_nome      = it_lfa1-name1.
        wa_ferro-chg_municipio = it_lfa1-ort01.
        IF p_localt IS NOT INITIAL AND ck_p_localt = abap_false.
          IF wa_ferro-chg_lifnr EQ p_localt.
            ck_p_localt = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF p_erdatc-low IS NOT INITIAL AND ( wa_ferro-chg_dt_chegada LT p_erdatc-low OR wa_ferro-chg_dt_chegada IS INITIAL ).
      CONTINUE.
    ENDIF.

    IF p_erdatc-high IS NOT INITIAL AND ( wa_ferro-chg_dt_chegada GT p_erdatc-high OR wa_ferro-chg_dt_chegada IS INITIAL ).
      CONTINUE.
    ENDIF.

    IF it_zlest0019_l3_20-pesovagao IS INITIAL.
      wa_ferro-transito_peso  = it_zlest0019_l2_20-pesovagao.
      wa_ferro-diferenca_peso = 0.
    ELSE.
      wa_ferro-transito_peso  = 0.
      wa_ferro-diferenca_peso = it_zlest0019_l3_20-pesovagao - it_zlest0019_l2_20-pesovagao.
    ENDIF.

    READ TABLE it_lfa1 WITH KEY stcd1 = it_zlest0019_l2_20-cnpjferro BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_ferro-loc_lifnr = it_lfa1-lifnr.
      wa_ferro-loc_nome  = it_lfa1-name1.
      wa_ferro-loc_municipio = it_lfa1-ort01.
    ENDIF.

    IF p_localc IS NOT INITIAL AND ck_p_localc = abap_false.
      IF wa_ferro-loc_lifnr EQ p_localc.
        ck_p_localc = abap_true.
      ENDIF.
    ENDIF.

    LOOP AT it_zlest0019_l2_30 WHERE dcl       EQ it_zlest0019_l2_20-dcl
                                 AND id_refkey EQ it_zlest0019_l2_20-id_refkey.

      wa_ferro-bukrs  = it_zlest0019_l2_30-bukrs.
      wa_ferro-branch = it_zlest0019_l2_30-branch.

      IF p_matnr-low IS NOT INITIAL AND ck_p_matnr = abap_false.
        IF it_zlest0019_l2_30-matnr EQ p_matnr-low.
          ck_p_matnr = abap_true.
        ENDIF.
      ENDIF.

      wa_ferro-cd_material = it_zlest0019_l2_30-matnr.
      READ TABLE it_makt WITH KEY matnr = it_zlest0019_l2_30-matnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_ferro-ds_material = it_makt-maktx.
      ENDIF.

      IF p_bukrs IS NOT INITIAL AND ck_p_bukrs = abap_false.
        IF it_zlest0019_l2_30-bukrs EQ p_bukrs.
          ck_p_bukrs = abap_true.
        ENDIF.
      ENDIF.

      IF p_werks IS NOT INITIAL AND ck_p_werks = abap_false.
        IF it_zlest0019_l2_30-branch EQ p_werks.
          ck_p_werks = abap_true.
        ENDIF.
      ENDIF.

      IF p_nfenum IS NOT INITIAL AND ck_p_nfenum = abap_false.
        IF it_zlest0019_l2_30-nfenum EQ p_nfenum.
          ck_p_nfenum = abap_true.
        ENDIF.
      ENDIF.

      IF wa_ferro-cd_material IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE it_j_1bnfdoc WITH KEY docnum = it_zlest0019_l2_30-docnum BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE it_j_1bnfnad WITH KEY docnum = it_j_1bnfdoc-docnum.
        IF sy-subrc IS INITIAL.
          wa_ferro-dst_lifnr = it_j_1bnfnad-parid.
          wa_ferro-dst_cnpj  = it_j_1bnfnad-cgc.
          wa_ferro-dst_nome  = it_j_1bnfnad-name1.
          wa_ferro-dst_municipio = it_j_1bnfnad-ort01.

          IF p_locald IS NOT INITIAL AND ck_p_locald = abap_false.
            IF wa_ferro-dst_lifnr EQ p_locald.
              ck_p_locald = abap_true.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ck_p_bukrs  NE abap_true OR
       ck_p_werks  NE abap_true OR
       ck_p_matnr  NE abap_true OR
       ck_p_nfenum NE abap_true OR
       ck_p_localc NE abap_true OR
       ck_p_locald NE abap_true OR
       ck_p_localt NE abap_true.
      CONTINUE.
    ENDIF.

    CASE abap_true.
      WHEN s_trans.
        IF wa_ferro-ds_transito NE 'Sim'.
          CONTINUE.
        ENDIF.
      WHEN s_trann.
        IF wa_ferro-ds_transito NE 'Não'.
          CONTINUE.
        ENDIF.
    ENDCASE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_ferro-dcl
      IMPORTING
        output = wa_ferro-dcl.

    APPEND wa_ferro TO it_ferro.

  ENDLOOP.

*  TRY.
*      EXEC SQL.
*        OPEN DOCUMENTOS FOR
*          SELECT DCL.MANDT,
*                 DCL.IDINTER,
*                 DCL.TP_MOVI,
*                 DCL.TP_REG,
*                 DCL.CHAVE,
*                 DCL.DCL,
*                 DCL.SERIEDCL,
*                 DCL.ID_REFKEY,
*                 DCL.IDVAGAO,
*                 DCL.STATUS_DUPLICA,
*                 DCL.OBSERVACAO,
*                 CASE WHEN CHE_VAGAO.DTADECARGA IS NULL THEN 'Sim' ELSE 'Não' END AS DS_TRANSITO,
*
*                 LOC.LIFNR            AS LOC_LIFNR,
*                 DCL.CNPJFERRO        AS LOC_CNPJ,
*                 LOC.NAME1            AS LOC_NOME,
*
*                 DCL_MERCADORIA.PARID AS DST_LIFNR,
*                 DCL_MERCADORIA.CGC   AS DST_CNPJ,
*                 DCL_MERCADORIA.NAME1 AS DST_NOME,
*
*                 CHG.LIFNR            AS CHG_LIFNR,
*                 CHE_VAGAO.CNPJFERRO  AS CHG_CNPJ,
*                 CHG.NAME1            AS CHG_NOME,
*
*                 DCL.DTADECARGA       AS SAI_DT_SAIDA,
*                 DCL.PESOVAGAO        AS SAI_PS_SAIDA,
*
*                 CHE_VAGAO.DTADECARGA       AS CHG_DT_CHEGADA,
*                 COALESCE(CHE_VAGAO.PESOVAGAO,0) AS CHG_PS_CHEGADA,
*
*                 COALESCE(CASE WHEN CHE_VAGAO_NT.PESO_CHEGADA =  0 THEN COALESCE(CHE_VAGAO.PESOVAGAO,0) ELSE CHE_VAGAO_NT.PESO_CHEGADA END,0) AS CHG_PS_CHEGADA_NOTAS,
*                 CASE WHEN COALESCE(CHE_VAGAO.PESOVAGAO,0)    =  0 THEN DCL.PESOVAGAO ELSE 0 END                                           AS TRANSITO_PESO,
*                 CASE WHEN COALESCE(CHE_VAGAO.PESOVAGAO,0)    <> 0 THEN COALESCE(CHE_VAGAO.PESOVAGAO,0) - DCL.PESOVAGAO ELSE 0 END              AS DIFERENCA_PESO,
*
*                 DCL_MERCADORIA.MATNR AS CD_MATERIAL,
*                 MAT.MAKTX            AS DS_MATERIAL,
*                 COUNT(*) OVER (PARTITION BY DCL.IDVAGAO, DCL.DTADECARGA) AS QTD_CARGA_VAGAO_DIA
*
*            FROM (SELECT *
*                    FROM SAPHANADB.ZLEST0019_L2_20 DCL
*                   WHERE DCL.MANDT               = :SY-MANDT
*                     AND DCL.ERDAT              <= :P_BASE
*                     AND CASE WHEN :LC_IDVAGAO   = 'T' THEN 'X' WHEN DCL.IDVAGAO = :LC_IDVAGAO THEN 'X' END = 'X'
*                     AND CASE WHEN :LC_DCL       = 'T' THEN 'X' WHEN DCL.DCL     = :LC_DCL     THEN 'X' END = 'X'
*                     AND CASE WHEN :LC_SAIDA_INI = 'T' THEN 'X' WHEN DCL.DTADECARGA >= :LC_SAIDA_INI THEN 'X' END = 'X'
*                     AND CASE WHEN :LC_SAIDA_FIM = 'T' THEN 'X' WHEN DCL.DTADECARGA <= :LC_SAIDA_FIM THEN 'X' END = 'X' ) DCL,
*                 SAPHANADB.LFA1      LOC,
*
*                 (SELECT TR.MANDT,
*                         TR.IDVAGAO,
*                         TR.DCL,
*                         TR.DTADECARGA,
*                         TR.PESOVAGAO,
*                         TR.CNPJFERRO,
*                         TR.ID_REFKEY
*                    FROM SAPHANADB.ZLEST0019_L3_20 TR
*                   WHERE TR.MANDT       = :SY-MANDT
*                     AND TR.ERDAT      <= :P_BASE ) CHE_VAGAO,
*
*                 SAPHANADB.LFA1      CHG,
*
*                 (SELECT TR.MANDT,
*                         TR.ID_REFKEY,
*                         TR.DCL AS DCL,
*                         SUM(TR.PESODVAGAO) PESO_CHEGADA
*                    FROM SAPHANADB.ZLEST0019_L3_30 TR
*                   WHERE TR.MANDT       = :SY-MANDT
*                     AND TR.ERDAT      <= :P_BASE
*                     AND CASE WHEN :LC_BUKRS  = 'T' THEN 'X' WHEN TR.BUKRS  = :LC_BUKRS  THEN 'X' END = 'X'
*                     AND CASE WHEN :LC_BRANCH = 'T' THEN 'X' WHEN TR.BRANCH = :LC_BRANCH THEN 'X' END = 'X'
*                   GROUP BY TR.MANDT, TR.ID_REFKEY, TR.DCL ) CHE_VAGAO_NT ,
*
*                 (SELECT DISTINCT A.MANDT, A.ID_REFKEY, A.DCL, N.PARID, N.NAME1, N.CGC, L.MATNR
*                    FROM SAPHANADB.ZLEST0019_L2_30 A,
*                         SAPHANADB.J_1BNFDOC B,
*                         SAPHANADB.J_1BNFLIN L,
*                         SAPHANADB.J_1BNFNAD N
*                   WHERE A.MANDT       = :SY-MANDT
*                     AND A.ERDAT      <= :P_BASE
*                     AND A.MANDT       = B.MANDT
*                     AND A.BUKRS       = B.BUKRS
*                     AND A.BRANCH      = B.BRANCH
*                     AND A.NFENUM      = B.NFENUM
*                     AND B.MANDT       = L.MANDT
*                     AND B.DOCNUM      = L.DOCNUM
*                     AND L.ITMNUM      = '000010'
*                     AND B.MANDT       = N.MANDT
*                     AND B.DOCNUM      = N.DOCNUM
*                     AND N.PARVW       = 'Z1' ) DCL_MERCADORIA,
*                 (SELECT * FROM SAPHANADB.MAKT M WHERE M.SPRAS = :SY-LANGU ) MAT
*
*           WHERE DCL.MANDT            = LOC.MANDT               (+)
*             AND DCL.CNPJFERRO        = LOC.STCD1               (+)
*             AND CASE WHEN :LOCAL_CARREGAMENT = 'T' THEN 'X' WHEN LOC.LIFNR = :LOCAL_CARREGAMENT THEN 'X' END = 'X'
*             AND DCL.MANDT            = CHE_VAGAO.MANDT         (+)
*             AND DCL.IDVAGAO          = CHE_VAGAO.IDVAGAO       (+)
*             AND DCL.DCL              = CHE_VAGAO.DCL           (+)
*             AND CHE_VAGAO.MANDT      = CHG.MANDT               (+)
*             AND CHE_VAGAO.CNPJFERRO  = CHG.STCD1               (+)
*             AND CASE WHEN :LOCAL_CHEGADA_EFE = 'T' THEN 'X' WHEN CHG.LIFNR = :LOCAL_CHEGADA_EFE THEN 'X' END = 'X'
*             AND CHE_VAGAO.MANDT      = CHE_VAGAO_NT.MANDT      (+)
*             AND CHE_VAGAO.DCL        = CHE_VAGAO_NT.DCL        (+)
*             AND CHE_VAGAO.ID_REFKEY  = CHE_VAGAO_NT.ID_REFKEY  (+)
*             AND DCL.MANDT            = DCL_MERCADORIA.MANDT    (+)
*             AND DCL.DCL              = DCL_MERCADORIA.DCL      (+)
*             AND DCL.ID_REFKEY        = DCL_MERCADORIA.ID_REFKEY(+)
*             AND CASE WHEN :LOCAL_ENTREGA = 'T' THEN 'X' WHEN DCL_MERCADORIA.PARID = :LOCAL_ENTREGA THEN 'X' END = 'X'
*             AND CASE WHEN :LC_MATNR      = 'T' THEN 'X' WHEN DCL_MERCADORIA.MATNR = :LC_MATNR      THEN 'X' END = 'X'
*             AND DCL_MERCADORIA.MANDT = MAT.MANDT               (+)
*             AND DCL_MERCADORIA.MATNR = MAT.MATNR               (+)
*             AND CASE WHEN :LC_EM_TRANSITO = 'T' THEN 'X'
*                      WHEN CHE_VAGAO.DTADECARGA IS NULL THEN CASE WHEN :LC_EM_TRANSITO = 'A' THEN 'X' ELSE ' ' END
*                      ELSE CASE WHEN :LC_EM_TRANSITO = 'A' THEN ' ' ELSE 'X' END
*                 END = 'X'
*             AND CASE WHEN :LC_CHEGADA_INI = 'T' THEN 'X' WHEN EXISTS( SELECT * FROM SAPHANADB.ZLEST0019_L3_20 NDCL
*                                                                        WHERE NDCL.DCL         = DCL.DCL
*                                                                          AND NDCL.SERIEDCL    = DCL.SERIEDCL
*                                                                          AND NDCL.DTADECARGA >= :LC_CHEGADA_INI ) THEN 'X' END = 'X'
*             AND CASE WHEN :LC_CHEGADA_FIM = 'T' THEN 'X' WHEN EXISTS( SELECT * FROM SAPHANADB.ZLEST0019_L3_20 NDCL
*                                                                        WHERE NDCL.DCL         = DCL.DCL
*                                                                          AND NDCL.SERIEDCL    = DCL.SERIEDCL
*                                                                          AND NDCL.DTADECARGA <= :LC_CHEGADA_FIM ) THEN 'X' END = 'X'
*             AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019_L2_30 NDCL
*                           WHERE NDCL.DCL         = DCL.DCL
*                             AND NDCL.SERIEDCL    = DCL.SERIEDCL
*                             AND NDCL.ID_REFKEY   = DCL.ID_REFKEY
*                             AND CASE WHEN :LC_NFENUM    = 'T' THEN 'X' WHEN NDCL.NFENUM LIKE :LC_NFENUM  THEN 'X' END = 'X'
*                             AND CASE WHEN :LC_BUKRS     = 'T' THEN 'X' WHEN NDCL.BUKRS  = :LC_BUKRS      THEN 'X' END = 'X'
*                             AND CASE WHEN :LC_BRANCH    = 'T' THEN 'X' WHEN NDCL.BRANCH = :LC_BRANCH     THEN 'X' END = 'X' )
*      ENDEXEC.
*    CATCH cx_sy_native_sql_error INTO exc_ref.
*      error_text = exc_ref->get_text( ).
*      MESSAGE error_text TYPE 'E' RAISING erro_sql.
*  ENDTRY.
*
*  DO.
*    EXEC SQL.
*      FETCH NEXT DOCUMENTOS INTO
*        :WA_FERRO-MANDT,
*        :WA_FERRO-IDINTER,
*        :WA_FERRO-TP_MOVI,
*        :WA_FERRO-TP_REG,
*        :WA_FERRO-CHAVE,
*        :WA_FERRO-DCL,
*        :WA_FERRO-SERIEDCL,
*        :WA_FERRO-ID_REFKEY,
*        :WA_FERRO-IDVAGAO,
*        :WA_FERRO-STATUS_DUPLICA,
*        :WA_FERRO-OBSERVACAO,
*        :WA_FERRO-DS_TRANSITO,
*        :WA_FERRO-LOC_LIFNR,
*        :WA_FERRO-LOC_CNPJ,
*        :WA_FERRO-LOC_NOME,
*        :WA_FERRO-DST_LIFNR,
*        :WA_FERRO-DST_CNPJ,
*        :WA_FERRO-DST_NOME,
*        :WA_FERRO-CHG_LIFNR,
*        :WA_FERRO-CHG_CNPJ,
*        :WA_FERRO-CHG_NOME,
*        :WA_FERRO-SAI_DT_SAIDA,
*        :WA_FERRO-SAI_PS_SAIDA,
*        :WA_FERRO-CHG_DT_CHEGADA,
*        :WA_FERRO-CHG_PS_CHEGADA,
*        :WA_FERRO-CHG_PS_CHEGADA_NOTAS,
*        :WA_FERRO-TRANSITO_PESO,
*        :WA_FERRO-DIFERENCA_PESO,
*        :WA_FERRO-CD_MATERIAL,
*        :WA_FERRO-DS_MATERIAL,
*        :WA_FERRO-QTD_CARGA_VAGAO_DIA
*    ENDEXEC.
*    IF sy-subrc <> 0.
*      EXIT.
*    ELSE.
*      APPEND wa_ferro TO it_ferro.
*    ENDIF.
*  ENDDO.
*
*  EXEC SQL.
*    CLOSE DOCUMENTOS
*  ENDEXEC.

  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF ctl_con_0100 IS INITIAL.

    CREATE OBJECT ctl_con_0100
      EXPORTING
        container_name = 'ALV_FERRO'.

    CREATE OBJECT ctl_alv_0100
      EXPORTING
        i_parent = ctl_con_0100.

    PERFORM fill_it_fieldcatalog_0100.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0100.
*   Set layout parameters for ALV grid

    gs_lay_0100-sel_mode   = 'A'.
    gs_lay_0100-zebra      = abap_true.

    CALL METHOD ctl_alv_0100->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0100
        is_variant           = gs_var_0100
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_0100
      CHANGING
        it_fieldcatalog      = it_catalog_0100
        it_outtab            = it_ferro[].

    CALL METHOD ctl_alv_0100->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_0100->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0100->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0100
      es_row_no   = gs_scroll_row_0100.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0100 .

  DATA: lc_col_pos TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat_0100> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_FERROVIARIO_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_0100.

  lc_col_pos = 1.

  DELETE it_catalog_0100 WHERE fieldname EQ 'MANDT'.
  DELETE it_catalog_0100 WHERE fieldname EQ 'IDINTER'.
  DELETE it_catalog_0100 WHERE fieldname EQ 'TP_MOVI'.
  DELETE it_catalog_0100 WHERE fieldname EQ 'TP_REG'.
  DELETE it_catalog_0100 WHERE fieldname EQ 'CHAVE'.
  DELETE it_catalog_0100 WHERE fieldname EQ 'ID_REFKEY'.
  DELETE it_catalog_0100 WHERE fieldname EQ 'STATUS_DUPLICA'.

  LOOP AT it_catalog_0100 ASSIGNING <fs_cat_0100>.
    <fs_cat_0100>-col_pos = lc_col_pos.
    <fs_cat_0100>-tabname = 'IT_FERRO'.
    ADD 1 TO lc_col_pos.

    CASE <fs_cat_0100>-fieldname.
      WHEN 'OBSERVACAO'.
        <fs_cat_0100>-outputlen = 30.
    ENDCASE.

    CASE <fs_cat_0100>-datatype.
      WHEN 'QUAN'.
        <fs_cat_0100>-do_sum    = abap_true.
        <fs_cat_0100>-outputlen = 15.
      WHEN 'CURR'.
        <fs_cat_0100>-do_sum    = abap_true.
        <fs_cat_0100>-outputlen = 15.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0100

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0100 .

  gs_var_0100-report      = sy-repid.
  gs_var_0100-handle      = '0100'.
  gs_var_0100-log_group   = abap_false.
  gs_var_0100-username    = abap_false.
  gs_var_0100-variant     = abap_false.
  gs_var_0100-text        = abap_false.
  gs_var_0100-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0100

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'EDITAR'.
      PERFORM editar_registro.
    WHEN 'NOTAS'.
      PERFORM mostrar_registro_notas.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info INPUT.

  CALL METHOD ctl_alv_0100->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0100
      es_row_no   = gs_scroll_row_0100.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows INPUT.

  CLEAR it_selected_rows.

  CALL METHOD ctl_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_ferro_sel[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_ferro INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING it_ferro TO it_ferro_sel.
    APPEND it_ferro_sel.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM editar_registro .

  IF it_ferro_sel[] IS INITIAL.
    MESSAGE 'Selecione um registro para edição' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE it_ferro_sel INDEX 1.

  SELECT SINGLE *
    INTO zlest0019
    FROM zlest0019
   WHERE idinter  EQ it_ferro_sel-idinter
     AND tp_movi  EQ it_ferro_sel-tp_movi
     AND tp_reg   EQ it_ferro_sel-tp_reg
     AND chave    EQ it_ferro_sel-chave
     AND dcl      EQ it_ferro_sel-dcl
     AND seriedcl EQ it_ferro_sel-seriedcl.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Não foi encontrado o registro' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL SCREEN 0101 STARTING AT 05 05.

ENDFORM.

INCLUDE zlesr0095_0101.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_REGISTRO_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostrar_registro_notas .

  DATA: lc_exc_ref    TYPE REF TO cx_sy_native_sql_error,
        lc_error_text TYPE string,
        wa_notas      TYPE zde_ferroviario_notas_alv.

  IF it_ferro_sel[] IS INITIAL.
    MESSAGE 'Selecione um registro para edição' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: it_ferro_nota[], it_ferro_nota.

  READ TABLE it_ferro_sel INDEX 1.

*-- Conversao S4 Hana - Amaggi - WPP - Ini

  TRY.
      EXEC SQL.

        OPEN DOCUMENTOS FOR

            SELECT DCL2.BUKRS,
                 DCL2.BRANCH,
                 DCL2.NFENUM,
                 DCL2.PESONF     AS PESO_NOTA,
                 DCL1.DTADECARGA AS DT_SAIDA,
                 DCL2.PESODVAGAO AS PESO_SAIDA_NOTA,
                 CASE WHEN DCL3.DTACHEGADA = '' AND DCL4.DTADECARGA <> '' THEN DCL4.DTADECARGA ELSE DCL3.DTACHEGADA END AS DT_CHEGADA,
                 CASE WHEN DCL3.DTACHEGADA = '' AND DCL4.DTADECARGA <> '' THEN 0 ELSE DCL3.PESODVAGAO END AS PESO_ENTRADA_NOTA,
                 CASE WHEN DCL3.DTACHEGADA = '' AND DCL4.DTADECARGA <> '' THEN 0 ELSE DCL3.PESODVAGAO END - DCL2.PESODVAGAO AS DIF_PESO
            FROM SAPHANADB.ZLEST0019 DCL1 left join

           (SELECT DCL1.MANDT, LPAD(DCL1.DCL,10,'0') AS DCL,
                         DCL1.SERIEDCL,
                         DCL1.CNPJFERRO,
                         DCL1.IDVAGAO||'T' AS IDVAGAO,
                         DCL1.PESOVAGAO,
                         DCL1.DTADECARGA
                    FROM SAPHANADB.ZLEST0019 DCL1
                   WHERE DCL1.IDINTER          = 'L3'
                     AND DCL1.TP_REG           = '20' ) DCL4

           on  DCL1.MANDT            = DCL4.MANDT
           AND DCL1.DCL              = DCL4.DCL
           AND DCL1.IDVAGAO          = DCL4.IDVAGAO,


                 SAPHANADB.ZLEST0019 DCL2 left join

                 (SELECT DCL1.MANDT, LPAD(DCL1.DCL,10,'0') AS DCL,
                         DCL1.SERIEDCL,
                         DCL1.CNPJFERRO,
                         DCL1.IDVAGAO||'T' AS IDVAGAO,
                         DCL1.PESOVAGAO,
                         DCL2.BUKRS,
                         DCL2.BRANCH,
                         DCL2.NFENUM,
                         DCL2.PESONF,
                         DCL2.PESODVAGAO,
                         DCL2.DTACHEGADA
                    FROM SAPHANADB.ZLEST0019 DCL1,
                         SAPHANADB.ZLEST0019 DCL2
                   WHERE DCL1.IDINTER          = 'L3'
                     AND DCL1.TP_REG           = '20'
                     AND DCL1.MANDT            = DCL2.MANDT
                     AND DCL1.DCL              = DCL2.DCL
                     AND DCL1.ID_REFKEY        = DCL2.ID_REFKEY
                     AND DCL2.IDINTER          = 'L3'
                     AND DCL2.TP_REG           = '30' ) DCL3

                on     DCL2.MANDT            = DCL3.MANDT
               AND DCL2.DCL              = DCL3.DCL
               AND DCL2.BUKRS            = DCL3.BUKRS
               AND DCL2.BRANCH           = DCL3.BRANCH
               AND DCL2.NFENUM           = DCL3.NFENUM



           WHERE DCL1.MANDT            = '300'
             AND DCL1.IDINTER          = 'L2'
             AND DCL1.TP_REG           = '20'
             AND DCL1.IDVAGAO          = :IT_FERRO_SEL-IDVAGAO
             AND DCL1.DCL              = LPAD( :IT_FERRO_SEL-DCL,10,'0')
             AND DCL1.ID_REFKEY        = :IT_FERRO_SEL-ID_REFKEY
             AND DCL1.MANDT            = DCL2.MANDT
             AND DCL1.DCL              = DCL2.DCL
             AND DCL1.ID_REFKEY        = DCL2.ID_REFKEY
             AND DCL2.IDINTER          = 'L2'
             AND DCL2.TP_REG           = '30'


        UNION ALL

                  SELECT DCL2.BUKRS,
                         DCL2.BRANCH,
                         DCL2.NFENUM,
                         DCL2.PESONF     AS PESO_NOTA,
                         ''            AS DT_SAIDA,
                         0  AS PESO_SAIDA_NOTA,
                         DCL2.DTACHEGADA AS DT_CHEGADA,
                         CASE WHEN DCL2.DTACHEGADA = '' AND DCL1.DTADECARGA <> '' THEN 0 ELSE DCL1.PESODVAGAO END AS PESO_ENTRADA_NOTA,
                         CASE WHEN DCL2.DTACHEGADA = '' AND DCL1.DTADECARGA <> '' THEN 0 ELSE DCL1.PESODVAGAO END AS DIF_PESO
                    FROM SAPHANADB.ZLEST0019 DCL1,
                         SAPHANADB.ZLEST0019 DCL2
                   WHERE DCL1.IDINTER          = 'L3'
                     AND DCL1.TP_REG           = '20'
                     AND DCL1.IDVAGAO||'T'     = :IT_FERRO_SEL-IDVAGAO
                     AND LPAD(DCL1.DCL,10,'0') = LPAD( :IT_FERRO_SEL-DCL ,10,'0')
                     AND DCL1.MANDT            = DCL2.MANDT
                     AND DCL1.DCL              = DCL2.DCL
                     AND DCL1.ID_REFKEY        = DCL2.ID_REFKEY
                     AND DCL2.IDINTER          = 'L3'
                     AND DCL2.TP_REG           = '30'
                     AND NOT EXISTS ( SELECT *
                                        FROM SAPHANADB.ZLEST0019 DCL3,
                                             SAPHANADB.ZLEST0019 DCL4
                                       WHERE DCL3.MANDT   = DCL1.MANDT
                                         AND DCL3.IDINTER = 'L2'
                                         AND DCL3.TP_REG  = '20'
                                         AND DCL3.IDVAGAO = DCL1.IDVAGAO||'T'
                                         AND DCL3.DCL     = LPAD(DCL1.DCL,10,'0')
                                         AND DCL3.ID_REFKEY        = :IT_FERRO_SEL-ID_REFKEY
                                         AND DCL3.MANDT            = DCL4.MANDT
                                         AND DCL3.DCL              = DCL4.DCL
                                         AND DCL3.ID_REFKEY        = DCL4.ID_REFKEY
                                         AND DCL4.IDINTER          = 'L2'
                                         AND DCL4.TP_REG           = '30'
                                         AND DCL4.BUKRS            = DCL2.BUKRS
                                         AND DCL4.NFENUM           = DCL2.NFENUM
                                         AND DCL4.BRANCH           = DCL2.BRANCH )

      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO lc_exc_ref.
      lc_error_text = lc_exc_ref->get_text( ).
      MESSAGE lc_error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.



*  TRY.
*      EXEC SQL.
*        OPEN DOCUMENTOS FOR
*          SELECT DCL2.BUKRS,
*                 DCL2.BRANCH,
*                 DCL2.NFENUM,
*                 DCL2.PESONF     AS PESO_NOTA,
*                 DCL1.DTADECARGA AS DT_SAIDA,
*                 DCL2.PESODVAGAO AS PESO_SAIDA_NOTA,
*                 CASE WHEN DCL3.DTACHEGADA IS NULL AND DCL4.DTADECARGA IS NOT NULL THEN DCL4.DTADECARGA ELSE DCL3.DTACHEGADA END AS DT_CHEGADA,
*                 CASE WHEN DCL3.DTACHEGADA IS NULL AND DCL4.DTADECARGA IS NOT NULL THEN 0 ELSE DCL3.PESODVAGAO END AS PESO_ENTRADA_NOTA,
*                 CASE WHEN DCL3.DTACHEGADA IS NULL AND DCL4.DTADECARGA IS NOT NULL THEN 0 ELSE DCL3.PESODVAGAO END - DCL2.PESODVAGAO AS DIF_PESO
*            FROM SAPHANADB.ZLEST0019 DCL1,
*                 SAPHANADB.ZLEST0019 DCL2,
*
*                 (SELECT DCL1.MANDT, LPAD(DCL1.DCL,10,'0') AS DCL,
*                         DCL1.SERIEDCL,
*                         DCL1.CNPJFERRO,
*                         DCL1.IDVAGAO||'T' AS IDVAGAO,
*                         DCL1.PESOVAGAO,
*                         DCL2.BUKRS,
*                         DCL2.BRANCH,
*                         DCL2.NFENUM,
*                         DCL2.PESONF,
*                         DCL2.PESODVAGAO,
*                         DCL2.DTACHEGADA
*                    FROM SAPHANADB.ZLEST0019 DCL1,
*                         SAPHANADB.ZLEST0019 DCL2
*                   WHERE DCL1.IDINTER          = 'L3'
*                     AND DCL1.TP_REG           = '20'
*                     AND DCL1.MANDT            = DCL2.MANDT
*                     AND DCL1.DCL              = DCL2.DCL
*                     AND DCL1.ID_REFKEY        = DCL2.ID_REFKEY
*                     AND DCL2.IDINTER          = 'L3'
*                     AND DCL2.TP_REG           = '30' ) DCL3,
*
*                 (SELECT DCL1.MANDT, LPAD(DCL1.DCL,10,'0') AS DCL,
*                         DCL1.SERIEDCL,
*                         DCL1.CNPJFERRO,
*                         DCL1.IDVAGAO||'T' AS IDVAGAO,
*                         DCL1.PESOVAGAO,
*                         DCL1.DTADECARGA
*                    FROM SAPHANADB.ZLEST0019 DCL1
*                   WHERE DCL1.IDINTER          = 'L3'
*                     AND DCL1.TP_REG           = '20' ) DCL4
*
*           WHERE DCL1.MANDT            = '300'
*             AND DCL1.IDINTER          = 'L2'
*             AND DCL1.TP_REG           = '20'
*             AND DCL1.IDVAGAO          = :IT_FERRO_SEL-IDVAGAO
*             AND DCL1.DCL              = LPAD( :IT_FERRO_SEL-DCL,10,'0')
*             AND DCL1.ID_REFKEY        = :IT_FERRO_SEL-ID_REFKEY
*             AND DCL1.MANDT            = DCL2.MANDT
*             AND DCL1.DCL              = DCL2.DCL
*             AND DCL1.ID_REFKEY        = DCL2.ID_REFKEY
*             AND DCL2.IDINTER          = 'L2'
*             AND DCL2.TP_REG           = '30'
*             AND DCL2.MANDT            = DCL3.MANDT (+)
*             AND DCL2.DCL              = DCL3.DCL (+)
*             AND DCL2.BUKRS            = DCL3.BUKRS (+)
*             AND DCL2.BRANCH           = DCL3.BRANCH(+)
*             AND DCL2.NFENUM           = DCL3.NFENUM(+)
*             AND DCL1.MANDT            = DCL4.MANDT (+)
*             AND DCL1.DCL              = DCL4.DCL (+)
*             AND DCL1.IDVAGAO          = DCL4.IDVAGAO (+)
*
*        UNION ALL
*
*                  SELECT DCL2.BUKRS,
*                         DCL2.BRANCH,
*                         DCL2.NFENUM,
*                         DCL2.PESONF     AS PESO_NOTA,
*                         NULL            AS DT_SAIDA,
*                         NULL AS PESO_SAIDA_NOTA,
*                         DCL2.DTACHEGADA AS DT_CHEGADA,
*                         CASE WHEN DCL2.DTACHEGADA IS NULL AND DCL1.DTADECARGA IS NOT NULL THEN 0 ELSE DCL1.PESODVAGAO END AS PESO_ENTRADA_NOTA,
*                         CASE WHEN DCL2.DTACHEGADA IS NULL AND DCL1.DTADECARGA IS NOT NULL THEN 0 ELSE DCL1.PESODVAGAO END AS DIF_PESO
*                    FROM SAPHANADB.ZLEST0019 DCL1,
*                         SAPHANADB.ZLEST0019 DCL2
*                   WHERE DCL1.IDINTER          = 'L3'
*                     AND DCL1.TP_REG           = '20'
*                     AND DCL1.IDVAGAO||'T'     = :IT_FERRO_SEL-IDVAGAO
*                     AND LPAD(DCL1.DCL,10,'0') = LPAD( :IT_FERRO_SEL-DCL ,10,'0')
*                     AND DCL1.MANDT            = DCL2.MANDT
*                     AND DCL1.DCL              = DCL2.DCL
*                     AND DCL1.ID_REFKEY        = DCL2.ID_REFKEY
*                     AND DCL2.IDINTER          = 'L3'
*                     AND DCL2.TP_REG           = '30'
*                     AND NOT EXISTS ( SELECT *
*                                        FROM SAPHANADB.ZLEST0019 DCL3,
*                                             SAPHANADB.ZLEST0019 DCL4
*                                       WHERE DCL3.MANDT   = DCL1.MANDT
*                                         AND DCL3.IDINTER = 'L2'
*                                         AND DCL3.TP_REG  = '20'
*                                         AND DCL3.IDVAGAO = DCL1.IDVAGAO||'T'
*                                         AND DCL3.DCL     = LPAD(DCL1.DCL,10,'0')
*                                         AND DCL3.ID_REFKEY        = :IT_FERRO_SEL-ID_REFKEY
*                                         AND DCL3.MANDT            = DCL4.MANDT
*                                         AND DCL3.DCL              = DCL4.DCL
*                                         AND DCL3.ID_REFKEY        = DCL4.ID_REFKEY
*                                         AND DCL4.IDINTER          = 'L2'
*                                         AND DCL4.TP_REG           = '30'
*                                         AND DCL4.BUKRS            = DCL2.BUKRS
*                                         AND DCL4.NFENUM           = DCL2.NFENUM
*                                         AND DCL4.BRANCH           = DCL2.BRANCH )
*
*
*      ENDEXEC.
*    CATCH CX_SY_NATIVE_SQL_ERROR INTO LC_EXC_REF.
*      LC_ERROR_TEXT = LC_EXC_REF->GET_TEXT( ).
*      MESSAGE LC_ERROR_TEXT TYPE 'E' RAISING ERRO_SQL.
*  ENDTRY.

*-- Conversao S4 Hana - Amaggi - WPP - Fim

  DO.
    EXEC SQL.
      FETCH NEXT DOCUMENTOS INTO
        :WA_NOTAS-BUKRS,
        :WA_NOTAS-BRANCH,
        :WA_NOTAS-NFENUM,
        :WA_NOTAS-PESO_NOTA,
        :WA_NOTAS-SAI_DT_SAIDA,
        :WA_NOTAS-SAI_PS_SAIDA,
        :WA_NOTAS-CHG_DT_CHEGADA,
        :WA_NOTAS-CHG_PS_CHEGADA,
        :WA_NOTAS-DIFERENCA_PESO
    ENDEXEC.
    IF sy-subrc <> 0.
      EXIT.
    ELSE.
      APPEND wa_notas TO it_ferro_nota.
    ENDIF.
  ENDDO.

  EXEC SQL.
    CLOSE DOCUMENTOS
  ENDEXEC.

  IF it_ferro_nota[] IS INITIAL.
    MESSAGE 'Não foram encontrados registros' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL SCREEN 0102 STARTING AT 05 05.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CARGA_DE_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carga_de_tabelas .

  DATA: wa_zlest0131 TYPE zlest0131.

  DATA: it_zlest0019 TYPE TABLE OF zlest0019 WITH HEADER LINE,
        it_j_1bnfdoc TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
        wa_j_1bnflin TYPE j_1bnflin.

  DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string.

  FIELD-SYMBOLS: <fs_0019> TYPE zlest0019.

  SELECT SINGLE * INTO wa_zlest0131 FROM zlest0131.

  IF ( wa_zlest0131-fazer_carga EQ abap_true ) OR ( wa_zlest0131-executou EQ abap_false ).
    EXEC SQL.
      DELETE FROM SAPHANADB.ZLEST0019_L2_20
    ENDEXEC.
    EXEC SQL.
      DELETE FROM SAPHANADB.ZLEST0019_L2_30
    ENDEXEC.
    EXEC SQL.
      DELETE FROM SAPHANADB.ZLEST0019_L3_20
    ENDEXEC.
    EXEC SQL.
      DELETE FROM SAPHANADB.ZLEST0019_L3_30
    ENDEXEC.
    COMMIT WORK.

    wa_zlest0131-fazer_carga = abap_false.
    wa_zlest0131-executou    = abap_true.
    MODIFY zlest0131 FROM wa_zlest0131.
    COMMIT WORK.
  ENDIF.

  EXEC SQL.
    DELETE FROM SAPHANADB.ZLEST0019_L2_20 A
     WHERE NOT EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019 B
                         WHERE B.MANDT    = A.MANDT
                           AND B.IDINTER  = A.IDINTER
                           AND B.TP_MOVI  = A.TP_MOVI
                           AND B.TP_REG   = A.TP_REG
                           AND B.CHAVE    = A.CHAVE
                           AND B.DCL      = A.DCL
                           AND B.SERIEDCL = A.SERIEDCL )
  ENDEXEC.
  COMMIT WORK.


  EXEC SQL.
    DELETE FROM SAPHANADB.ZLEST0019_L2_30 A
     WHERE NOT EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019 B
                         WHERE B.MANDT    = A.MANDT
                           AND B.IDINTER  = A.IDINTER
                           AND B.TP_MOVI  = A.TP_MOVI
                           AND B.TP_REG   = A.TP_REG
                           AND B.CHAVE    = A.CHAVE
                           AND B.DCL      = A.DCL
                           AND B.SERIEDCL = A.SERIEDCL )
  ENDEXEC.
  COMMIT WORK.

  EXEC SQL.
    DELETE FROM SAPHANADB.ZLEST0019_L3_20 A
     WHERE NOT EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019 B
                         WHERE B.MANDT    = A.MANDT
                           AND B.IDINTER  = A.IDINTER
                           AND B.TP_MOVI  = A.TP_MOVI
                           AND B.TP_REG   = A.TP_REG
                           AND B.CHAVE    = A.CHAVE
                           AND COALESCE(LPAD(TRIM(B.DCL),10,'0'),' ') = A.DCL
                           AND B.SERIEDCL = A.SERIEDCL )
  ENDEXEC.
  COMMIT WORK.


  EXEC SQL.
    DELETE FROM SAPHANADB.ZLEST0019_L3_30 A
     WHERE NOT EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019 B
                         WHERE B.MANDT    = A.MANDT
                           AND B.IDINTER  = A.IDINTER
                           AND B.TP_MOVI  = A.TP_MOVI
                           AND B.TP_REG   = A.TP_REG
                           AND B.CHAVE    = A.CHAVE
                           AND COALESCE(LPAD(TRIM(B.DCL),10,'0'),' ') = A.DCL
                           AND B.SERIEDCL = A.SERIEDCL )
  ENDEXEC.
  COMMIT WORK.

  EXEC SQL.
    DELETE FROM SAPHANADB.ZLEST0019_L2_30 A
     WHERE A.DOCNUM = '0000000000'
       AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019 B
                     WHERE B.MANDT    = A.MANDT
                       AND B.IDINTER  = A.IDINTER
                       AND B.TP_MOVI  = A.TP_MOVI
                       AND B.TP_REG   = A.TP_REG
                       AND B.CHAVE    = A.CHAVE
                       AND B.DCL      = A.DCL
                       AND B.SERIEDCL = A.SERIEDCL
                       AND B.DOCNUM   <> '0000000000' )
  ENDEXEC.
  COMMIT WORK.

  EXEC SQL.
    DELETE FROM SAPHANADB.ZLEST0019_L3_30 A
     WHERE A.DOCNUM = '0000000000'
       AND EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019 B
                     WHERE B.MANDT    = A.MANDT
                       AND B.IDINTER  = A.IDINTER
                       AND B.TP_MOVI  = A.TP_MOVI
                       AND B.TP_REG   = A.TP_REG
                       AND B.CHAVE    = A.CHAVE
                       AND COALESCE(LPAD(TRIM(B.DCL),10,'0'),' ') = A.DCL
                       AND B.SERIEDCL = A.SERIEDCL
                       AND B.DOCNUM   <> '0000000000' )
  ENDEXEC.
  COMMIT WORK.

  SELECT * INTO TABLE it_zlest0019
    FROM zlest0019
   WHERE idinter  = 'L2'
     AND tp_reg   = '30'
     AND nfenum   <> space
     AND docnum   = space.

  SELECT * APPENDING TABLE it_zlest0019
    FROM zlest0019
   WHERE idinter  = 'L3'
     AND tp_reg   = '30'
     AND nfenum   <> space
     AND docnum   = space.

  DESCRIBE TABLE it_zlest0019 LINES qtd_registros.

  LOOP AT it_zlest0019 ASSIGNING <fs_0019>.
    PERFORM mostra_texto_p USING 'Atualizando L2_30/L3_30' qtd_registros sy-tabix.

    CLEAR: it_j_1bnfdoc[].
    SELECT * INTO TABLE it_j_1bnfdoc
      FROM j_1bnfdoc AS a
     WHERE bukrs  EQ <fs_0019>-bukrs
       AND branch EQ <fs_0019>-branch
       AND nfenum EQ <fs_0019>-nfenum
       AND form   EQ 'NF55'
       AND model  EQ '55'
       AND direct EQ '2'
       AND doctyp NE '5'
       AND doctyp NE '6'.

    IF sy-subrc IS INITIAL.
      DESCRIBE TABLE it_j_1bnfdoc LINES qtd_registros2.
      IF qtd_registros2 EQ 1.
        READ TABLE it_j_1bnfdoc INDEX 1.
        <fs_0019>-docnum = it_j_1bnfdoc-docnum.
        SELECT SINGLE matnr INTO <fs_0019>-matnr FROM j_1bnflin WHERE docnum EQ it_j_1bnfdoc-docnum.
      ELSEIF qtd_registros2 GT 1.
        SELECT * INTO wa_j_1bnflin
          FROM j_1bnflin
           FOR ALL ENTRIES IN it_j_1bnfdoc
         WHERE docnum EQ it_j_1bnfdoc-docnum.
          IF wa_j_1bnflin-matkl EQ '700190' OR wa_j_1bnflin-matkl EQ '700110' OR wa_j_1bnflin-matkl EQ '700170'.
            <fs_0019>-docnum = wa_j_1bnflin-docnum.
            <fs_0019>-matnr  = wa_j_1bnflin-matnr.
          ENDIF.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF it_zlest0019[] IS NOT INITIAL.
    MODIFY zlest0019 FROM TABLE it_zlest0019.
    COMMIT WORK.
  ENDIF.

  TRY.
      EXEC SQL.
        INSERT INTO SAPHANADB.ZLEST0019_L2_20 ( MANDT, IDINTER, TP_MOVI, TP_REG, CHAVE, DCL, SERIEDCL, CNPJFERRO, NOMEMPFERRO, DTAENVIO, HORAENVIO, OBS, IDVAGAO, PESOVAGAO,
                                      DTADECARGA, HORADESCARGA, CNPJCLIENTE, BUKRS, BRANCH, NFENUM, NFNUM, PESONF, PESODVAGAO, DTACHEGADA, PRODUTO, ERDAT, ERZET,
                                      UNAME, NR_NF_TERCEIRO, COD_FORNECEDOR, ID_ZLEST0019, ID_REFKEY, STATUS_DUPLICA, OBSERVACAO )
                               SELECT MANDT, IDINTER, TP_MOVI, TP_REG, CHAVE, DCL, SERIEDCL, CNPJFERRO, NOMEMPFERRO, DTAENVIO, HORAENVIO, OBS, IDVAGAO, PESOVAGAO,
                                      DTADECARGA, HORADESCARGA, CNPJCLIENTE, BUKRS, BRANCH, NFENUM, NFNUM, PESONF, PESODVAGAO, DTACHEGADA, PRODUTO, ERDAT, ERZET,
                                      UNAME, NR_NF_TERCEIRO, COD_FORNECEDOR, ID_ZLEST0019, ID_REFKEY, STATUS_DUPLICA, OBSERVACAO
                                 FROM SAPHANADB.ZLEST0019 DCL
                                WHERE DCL.MANDT               = '300'
                                  AND DCL.IDINTER             = 'L2'
                                  AND DCL.TP_REG              = '20'
                                  AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019_L2_20 B
                                                    WHERE B.MANDT    = DCL.MANDT
                                                      AND B.IDINTER  = DCL.IDINTER
                                                      AND B.TP_MOVI  = DCL.TP_MOVI
                                                      AND B.TP_REG   = DCL.TP_REG
                                                      AND B.CHAVE    = DCL.CHAVE
                                                      AND B.DCL      = DCL.DCL
                                                      AND B.SERIEDCL = DCL.SERIEDCL )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  COMMIT WORK.

  TRY.
      EXEC SQL.
        INSERT INTO SAPHANADB.ZLEST0019_L2_30 ( MANDT, IDINTER, TP_MOVI, TP_REG, CHAVE, DCL, SERIEDCL, CNPJFERRO, NOMEMPFERRO, DTAENVIO, HORAENVIO, OBS, IDVAGAO, PESOVAGAO,
                                      DTADECARGA, HORADESCARGA, CNPJCLIENTE, BUKRS, BRANCH, NFENUM, NFNUM, PESONF, PESODVAGAO, DTACHEGADA, PRODUTO, ERDAT, ERZET,
                                      UNAME, NR_NF_TERCEIRO, COD_FORNECEDOR, ID_ZLEST0019, ID_REFKEY, STATUS_DUPLICA, OBSERVACAO, DOCNUM, MATNR )
                               SELECT MANDT, IDINTER, TP_MOVI, TP_REG, CHAVE, DCL, SERIEDCL, CNPJFERRO, NOMEMPFERRO, DTAENVIO, HORAENVIO, OBS, IDVAGAO, PESOVAGAO,
                                      DTADECARGA, HORADESCARGA, CNPJCLIENTE, BUKRS, BRANCH, NFENUM, NFNUM, PESONF, PESODVAGAO, DTACHEGADA, PRODUTO, ERDAT, ERZET,
                                      UNAME, NR_NF_TERCEIRO, COD_FORNECEDOR, ID_ZLEST0019, ID_REFKEY, STATUS_DUPLICA, OBSERVACAO, DOCNUM, MATNR
                                 FROM SAPHANADB.ZLEST0019 DCL
                                WHERE DCL.MANDT               = '300'
                                  AND DCL.IDINTER             = 'L2'
                                  AND DCL.TP_REG              = '30'
                                  AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019_L2_30 B
                                                    WHERE B.MANDT    = DCL.MANDT
                                                      AND B.IDINTER  = DCL.IDINTER
                                                      AND B.TP_MOVI  = DCL.TP_MOVI
                                                      AND B.TP_REG   = DCL.TP_REG
                                                      AND B.CHAVE    = DCL.CHAVE
                                                      AND B.DCL      = DCL.DCL
                                                      AND B.SERIEDCL = DCL.SERIEDCL )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  COMMIT WORK.

  TRY.
      EXEC SQL.
        INSERT INTO SAPHANADB.ZLEST0019_L3_20 ( MANDT, IDINTER, TP_MOVI, TP_REG, CHAVE, DCL, SERIEDCL, CNPJFERRO, NOMEMPFERRO, DTAENVIO, HORAENVIO, OBS, IDVAGAO, PESOVAGAO,
                                      DTADECARGA, HORADESCARGA, CNPJCLIENTE, BUKRS, BRANCH, NFENUM, NFNUM, PESONF, PESODVAGAO, DTACHEGADA, PRODUTO, ERDAT, ERZET,
                                      UNAME, NR_NF_TERCEIRO, COD_FORNECEDOR, ID_ZLEST0019, ID_REFKEY, STATUS_DUPLICA, OBSERVACAO )
                               SELECT MANDT, IDINTER, TP_MOVI, TP_REG, CHAVE, COALESCE(LPAD(TRIM(DCL.DCL),10,'0'),' '), SERIEDCL, CNPJFERRO, NOMEMPFERRO, DTAENVIO, HORAENVIO, OBS, IDVAGAO||'T', PESOVAGAO,
                                      DTADECARGA, HORADESCARGA, CNPJCLIENTE, BUKRS, BRANCH, NFENUM, NFNUM, PESONF, PESODVAGAO, DTACHEGADA, PRODUTO, ERDAT, ERZET,
                                      UNAME, NR_NF_TERCEIRO, COD_FORNECEDOR, ID_ZLEST0019, ID_REFKEY, STATUS_DUPLICA, OBSERVACAO
                                 FROM SAPHANADB.ZLEST0019 DCL
                                WHERE DCL.MANDT               = '300'
                                  AND DCL.IDINTER             = 'L3'
                                  AND DCL.TP_REG              = '20'
                                  AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019_L3_20 B
                                                    WHERE B.MANDT    = DCL.MANDT
                                                      AND B.IDINTER  = DCL.IDINTER
                                                      AND B.TP_MOVI  = DCL.TP_MOVI
                                                      AND B.TP_REG   = DCL.TP_REG
                                                      AND B.CHAVE    = DCL.CHAVE
                                                      AND B.DCL      = COALESCE(LPAD(TRIM(DCL.DCL),10,'0'),' ')
                                                      AND B.SERIEDCL = DCL.SERIEDCL )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  COMMIT WORK.

  TRY.
      EXEC SQL.
        INSERT INTO SAPHANADB.ZLEST0019_L3_30 ( MANDT, IDINTER, TP_MOVI, TP_REG, CHAVE, DCL, SERIEDCL, CNPJFERRO, NOMEMPFERRO, DTAENVIO, HORAENVIO, OBS, IDVAGAO, PESOVAGAO,
                                      DTADECARGA, HORADESCARGA, CNPJCLIENTE, BUKRS, BRANCH, NFENUM, NFNUM, PESONF, PESODVAGAO, DTACHEGADA, PRODUTO, ERDAT, ERZET,
                                      UNAME, NR_NF_TERCEIRO, COD_FORNECEDOR, ID_ZLEST0019, ID_REFKEY, STATUS_DUPLICA, OBSERVACAO, DOCNUM, MATNR )
                               SELECT MANDT, IDINTER, TP_MOVI, TP_REG, CHAVE, COALESCE(LPAD(TRIM(DCL.DCL),10,'0'),' '), SERIEDCL, CNPJFERRO, NOMEMPFERRO, DTAENVIO, HORAENVIO, OBS, IDVAGAO, PESOVAGAO,
                                      DTADECARGA, HORADESCARGA, CNPJCLIENTE, BUKRS, BRANCH, NFENUM, NFNUM, PESONF, PESODVAGAO, DTACHEGADA, PRODUTO, ERDAT, ERZET,
                                      UNAME, NR_NF_TERCEIRO, COD_FORNECEDOR, ID_ZLEST0019, ID_REFKEY, STATUS_DUPLICA, OBSERVACAO, DOCNUM, MATNR
                                 FROM SAPHANADB.ZLEST0019 DCL
                                WHERE DCL.MANDT               = '300'
                                  AND DCL.IDINTER             = 'L3'
                                  AND DCL.TP_REG              = '30'
                                  AND NOT EXISTS ( SELECT * FROM SAPHANADB.ZLEST0019_L3_30 B
                                                    WHERE B.MANDT    = DCL.MANDT
                                                      AND B.IDINTER  = DCL.IDINTER
                                                      AND B.TP_MOVI  = DCL.TP_MOVI
                                                      AND B.TP_REG   = DCL.TP_REG
                                                      AND B.CHAVE    = DCL.CHAVE
                                                      AND B.DCL      = COALESCE(LPAD(TRIM(DCL.DCL),10,'0'),' ')
                                                      AND B.SERIEDCL = DCL.SERIEDCL )
      ENDEXEC.
    CATCH cx_sy_native_sql_error INTO exc_ref.
      error_text = exc_ref->get_text( ).
      MESSAGE error_text TYPE 'E' RAISING erro_sql.
  ENDTRY.

  COMMIT WORK.

*  TRY.
*      EXEC SQL.
*        UPDATE SAPHANADB.ZLEST0019_L3_20
*           SET DCL  = LPAD(TRIM(DCL),10,'0'),
*               IDVAGAO = IDVAGAO||'T'
*      ENDEXEC.
*    CATCH cx_sy_native_sql_error INTO exc_ref.
*      error_text = exc_ref->get_text( ).
*      MESSAGE error_text TYPE 'E' RAISING erro_sql.
*  ENDTRY.
*
*  TRY.
*      EXEC SQL.
*        UPDATE SAPHANADB.ZLEST0019_L3_30
*           SET DCL  = LPAD(TRIM(DCL),10,'0')
*      ENDEXEC.
*    CATCH cx_sy_native_sql_error INTO exc_ref.
*      error_text = exc_ref->get_text( ).
*      MESSAGE error_text TYPE 'E' RAISING erro_sql.
*  ENDTRY.
*
*  COMMIT WORK.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2463   text
*----------------------------------------------------------------------*
FORM mostra_texto_p  USING p_texto
                           p_total   TYPE i
                           p_posicao TYPE i.

  DATA: vmsg(100),
        p_percentage(20),
        p_perce TYPE i.

  MOVE p_texto TO vmsg.

  IF p_total NE 0.
    p_perce = ( p_posicao * 100 ) / p_total.
  ENDIF.

  WRITE p_perce TO p_percentage.

  CALL FUNCTION 'TH_REDISPATCH'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percentage
      text       = vmsg.

ENDFORM.                    " MOSTRA_TEXTO

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2463   text
*----------------------------------------------------------------------*
FORM mostra_texto  USING  p_texto.

  DATA: vmsg(50).

  MOVE p_texto TO vmsg.

  CALL FUNCTION 'TH_REDISPATCH'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = vmsg.

ENDFORM.                    " MOSTRA_TEXTO
