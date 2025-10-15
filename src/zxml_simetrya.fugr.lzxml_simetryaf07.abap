*----------------------------------------------------------------------*

DATA: cod_simulacao_0159 TYPE zsdt0159-doc_simulacao,
      wa_zsdt0090        TYPE zsdt0090.

DATA:  p_erro(1).
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados USING doc_numero tipo hbkid.
  TYPES: BEGIN OF ty_antec,
           mark(1),
           vbeln                   TYPE zsdt0159-vbeln,
           zid_lanc                TYPE zsdt0159-zid_lanc,
           adiant                  TYPE zsdt0159-adiant,
           gjahr                   TYPE zsdt0159-gjahr,
           mont_moeda              TYPE zsdt0159-mont_moeda,
           augbl                   TYPE bsad-augbl,
           augdt                   TYPE bsad-augdt,
           usnam                   TYPE zsdt0159-usnam,
           data_atual              TYPE zsdt0159-data_atual,
           usnam_e                 TYPE zsdt0159-usnam_e,
           data_atual_e            TYPE zsdt0159-data_atual_e,
           " 01.06.2023 - RAMON - 114397 -->
           id_transacao_financeira TYPE zsdt0159-id_transacao_financeira,
           " 01.06.2023 - RAMON - 114397 --<
           obj_key                 TYPE zsdt0159-obj_key,
           estorno                 TYPE zsdt0159-estorno,
           bukrs                   TYPE zib_contabil-bukrs,
           seq                     TYPE zsdt0159-seq,
           line_color(4)           TYPE c,
         END OF ty_antec.

  DATA: it_j_1bnflin_aux TYPE TABLE OF ty_j_1bnflin.

  DATA: vinstrucao TYPE zfit0048-instrucao,
        vseq_lcto  TYPE zfiwrt0008-seq_lcto,
        vobj_key   TYPE zfiwrt0008-obj_key.

  DATA: tg_antec      TYPE TABLE OF ty_antec WITH HEADER LINE,
        doc_simulacao TYPE zsdt0040-doc_simulacao.
  "Selecionar na tabela  ZFIWRT0008-SEQ_LCTO=J_1BNFLIN-REFKEY pegar o valor do campo ZFIWRT0008-OBJ_KEY (AWKEY da BKPF)

  IF tipo  = 'N'. " Boleto de Notas Fatura
    vinstrucao = '01'.
    SELECT  docnum refkey reftyp
      FROM j_1bnflin
      INTO TABLE it_j_1bnflin
      WHERE docnum = doc_numero.


    CHECK sy-subrc = 0.

    LOOP AT it_j_1bnflin INTO wa_j_1bnflin.
      IF wa_j_1bnflin-reftyp = 'ZW'.
        vseq_lcto = wa_j_1bnflin-refkey.
        SELECT SINGLE obj_key
          INTO vobj_key
          FROM zfiwrt0008
          WHERE seq_lcto = vseq_lcto.
        IF sy-subrc = 0.
          wa_j_1bnflin-awkey = vobj_key.
        ENDIF.
      ELSE.
        wa_j_1bnflin-awkey = wa_j_1bnflin-refkey.
      ENDIF.
      MODIFY it_j_1bnflin FROM wa_j_1bnflin INDEX sy-tabix TRANSPORTING awkey.
    ENDLOOP.

    SELECT SINGLE docnum bukrs pstdat nfenum
      FROM j_1bnfdoc
      INTO wa_j_1bnfdoc
      WHERE docnum = doc_numero.

    CHECK sy-subrc = 0.

    it_j_1bnflin_aux[] = it_j_1bnflin[].

    DELETE it_j_1bnflin_aux WHERE awkey IS INITIAL.

    CHECK it_j_1bnflin_aux[] IS NOT INITIAL.

    SELECT bukrs gjahr awkey belnr
      FROM bkpf
      INTO TABLE it_bkpf
      FOR ALL ENTRIES IN it_j_1bnflin_aux
      WHERE bukrs	=	wa_j_1bnfdoc-bukrs
      AND   gjahr	=	wa_j_1bnfdoc-pstdat+0(4)
      AND   awkey = it_j_1bnflin_aux-awkey.

    CHECK sy-subrc = 0.

    SELECT bukrs belnr gjahr dmbtr zbd1t zfbdt gsber hbkid kunnr
      FROM bsid
      INTO TABLE it_bsid
      FOR ALL ENTRIES IN it_bkpf
      WHERE bukrs =	it_bkpf-bukrs
      AND   belnr	=	it_bkpf-belnr
      AND   gjahr	=	it_bkpf-gjahr
      AND   zlsch	=	'D'.


    CHECK sy-subrc = 0.
  ELSEIF tipo  = 'A'. " Boleto antecipação
    vinstrucao = '03'.
    doc_simulacao =  doc_numero.
    cod_simulacao_0159 = doc_simulacao.
    PERFORM carrega_antec(zsdr016) USING tg_antec[] doc_simulacao.
    DELETE tg_antec WHERE estorno = 'X'.
    DELETE tg_antec WHERE augbl IS NOT INITIAL.

    CHECK tg_antec[] IS NOT INITIAL.
    REFRESH it_zsdt0054.
    LOOP AT tg_antec.
      wa_zsdt0051-nro_sol_ov = doc_numero.
      wa_zsdt0051-vkorg      = tg_antec-bukrs.
      "
      wa_zsdt0054-nro_sol_ov = tg_antec-vbeln.
      wa_zsdt0054-adiant = tg_antec-adiant.
      APPEND wa_zsdt0054 TO it_zsdt0054.
    ENDLOOP.

    READ TABLE it_zsdt0054 INTO wa_zsdt0054 INDEX 1.
    SELECT bsid~bukrs bsid~belnr bsid~gjahr bsid~dmbtr bsid~zbd1t bsid~zfbdt bsid~gsber bsid~hbkid bsid~kunnr
      FROM bsid
      INNER JOIN bkpf
      ON  bkpf~bukrs = bsid~bukrs
      AND bkpf~belnr = bsid~belnr
      AND bkpf~gjahr = bsid~gjahr
*      AND BKPF~XBLNR = P_XBLNR
      INTO TABLE it_bsid
      FOR ALL ENTRIES IN tg_antec
      WHERE bsid~bukrs =  tg_antec-bukrs
      AND   bsid~belnr  = tg_antec-adiant
      AND   bsid~gjahr  = tg_antec-gjahr
      AND   bsid~zlsch  = 'D'
      AND   bsid~hbkid  = hbkid. "'BBRA'. "US - 81799 - CBRAND

    CHECK sy-subrc = 0.
  ELSE.
    vinstrucao = '02'.
    SELECT  nro_sol_ov posnr valdt dmbtr adiant
      FROM zsdt0054
      INTO TABLE it_zsdt0054
      WHERE nro_sol_ov = doc_numero.

    CHECK sy-subrc = 0.

    SELECT SINGLE nro_sol_ov vkorg
      FROM zsdt0051
      INTO wa_zsdt0051
       WHERE nro_sol_ov = doc_numero.

    CHECK sy-subrc = 0.
    READ TABLE it_zsdt0054 INTO wa_zsdt0054 INDEX 1.
    SELECT bsid~bukrs bsid~belnr bsid~gjahr bsid~dmbtr bsid~zbd1t bsid~zfbdt bsid~gsber bsid~hbkid bsid~kunnr
      FROM bsid
      INNER JOIN bkpf
      ON  bkpf~bukrs = bsid~bukrs
      AND bkpf~belnr = bsid~belnr
      AND bkpf~gjahr = bsid~gjahr
      AND bkpf~xblnr = p_xblnr
      INTO TABLE it_bsid
      FOR ALL ENTRIES IN it_zsdt0054
      WHERE bsid~bukrs =  wa_zsdt0051-vkorg
      AND   bsid~belnr  = it_zsdt0054-adiant
      AND   bsid~zlsch  = 'D'
      AND   bsid~hbkid  = hbkid. "'BBRA'. "US - 81799 - CBRAND

    CHECK sy-subrc = 0.

  ENDIF.

  LOOP AT it_bsid INTO wa_bsid.
    CONCATENATE wa_bsid-bukrs+2(2) '01' INTO wa_bsid-lifnr_e.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_bsid-lifnr_e
      IMPORTING
        output = wa_bsid-lifnr_e.
    wa_bsid-lifnr_f = wa_bsid-gsber.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_bsid-lifnr_f
      IMPORTING
        output = wa_bsid-lifnr_f.
    MODIFY it_bsid FROM wa_bsid INDEX sy-tabix TRANSPORTING lifnr_e lifnr_f.
  ENDLOOP.


  SELECT  bukrs werks kunnr txt_instrucao
    FROM zfit0048
    INTO TABLE it_zfit0048
    FOR ALL ENTRIES IN it_bsid
    WHERE instrucao = vinstrucao
    AND   bukrs     = it_bsid-bukrs
    AND   werks     = it_bsid-gsber
    AND   kunnr     = it_bsid-kunnr.
  IF it_zfit0048[] IS INITIAL.
    SELECT  bukrs werks kunnr txt_instrucao
    FROM zfit0048
    INTO TABLE it_zfit0048
    FOR ALL ENTRIES IN it_bsid
    WHERE instrucao = vinstrucao
    AND   bukrs     = it_bsid-bukrs
    AND   werks     = it_bsid-gsber.
    IF it_zfit0048[] IS INITIAL.
      SELECT  bukrs werks kunnr txt_instrucao
      FROM zfit0048
      INTO TABLE it_zfit0048
      FOR ALL ENTRIES IN it_bsid
      WHERE instrucao = vinstrucao
      AND   bukrs     = it_bsid-bukrs.
      IF it_zfit0048[] IS INITIAL.
        SELECT  bukrs werks kunnr txt_instrucao
        FROM zfit0048
        INTO TABLE it_zfit0048
        WHERE instrucao = vinstrucao.
      ENDIF.
    ENDIF.
  ENDIF.


  SELECT  bukrs hbkid dtaid
    FROM t045t
    INTO TABLE it_t045t
    FOR ALL ENTRIES IN it_bsid
    WHERE bukrs	=	it_bsid-bukrs
    AND   zlsch	=	'D'
    AND   hbkid	=	hbkid. "'BBRA'. ( "US - 81799 - CBRAND.) - "IT_BSID-HBKID.

  SELECT  bukrs hbkid bankn bkont
    FROM t012k
    INTO TABLE it_t012k
    FOR ALL ENTRIES IN it_bsid
    WHERE bukrs	=	it_bsid-bukrs
    AND   hbkid	=	hbkid. "'BBRA'. ( "US - 81799 - CBRAND.)

  SELECT bukrs hbkid bankl
   FROM t012
   INTO TABLE it_t012
   FOR ALL ENTRIES IN it_bsid
   WHERE bukrs  = it_bsid-bukrs
   AND   hbkid  = hbkid. "'BBRA'. ( "US - 81799 - CBRAND.)  "IT_BSID-HBKID.

  "EMPRESA
  SELECT  lifnr name1 stras ort02 pstlz ort01 regio stcd1
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_bsid
    WHERE lifnr = it_bsid-lifnr_e.

  "FILIAL
  SELECT  lifnr name1 stras ort02 pstlz ort01 regio stcd1
    FROM lfa1
    APPENDING TABLE it_lfa1
    FOR ALL ENTRIES IN it_bsid
    WHERE lifnr = it_bsid-lifnr_f.



  "CLIENTE
  SELECT  kunnr name1 stras ort02 pstlz ort01 regio stcd1 stcd2 stkzn
  FROM kna1
  INTO TABLE it_kna1
  FOR ALL ENTRIES IN it_bsid
  WHERE kunnr = it_bsid-kunnr.

  SELECT  *
  FROM
    zsdt0090
  INTO wa_zsdt0090
  WHERE
    categoria = 'V'
    AND doc_simulacao = cod_simulacao_0159
                 ORDER BY sequencia ASCENDING.

  ENDSELECT.


ENDFORM.                    " F_SELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_dados USING p_tipo p_hbkid p_instrucoes STRUCTURE zfi_boleto.

  SORT:  it_bkpf     BY bukrs gjahr awkey,
         it_bsid     BY bukrs belnr gjahr,
         it_zfit0048 BY bukrs werks kunnr,
         it_t045t    BY bukrs	hbkid,
         it_t012k    BY bukrs	hbkid,
         it_t012     BY bukrs	hbkid,
         it_lfa1     BY lifnr,
         it_kna1     BY kunnr.

  DATA: wl_cont      TYPE sy-tabix,
        wl_cont_aux  TYPE sy-tabix,
        wl_cont_aux2 TYPE sy-tabix,
        var_bankl    TYPE t012-bankl,
        wl_bseg      TYPE bseg,
        v_xref2      TYPE bseg-xref2,
        v_xref3      TYPE bseg-xref3,
        v_api        TYPE c,
        v_tipo(1).

  DATA: lva_numero(9) TYPE c.

  REFRESH it_saida.
  " NOTAS FATURA tipo = 'N' Default
  LOOP AT it_j_1bnflin INTO wa_j_1bnflin.

    LOOP AT it_bkpf INTO wa_bkpf WHERE bukrs  = wa_j_1bnfdoc-bukrs
                                 AND   gjahr  = wa_j_1bnfdoc-pstdat+0(4)
                                 AND   awkey  = wa_j_1bnflin-awkey.

      LOOP AT it_bsid INTO wa_bsid WHERE bukrs = wa_bkpf-bukrs
                                   AND   belnr = wa_bkpf-belnr
                                   AND   gjahr = wa_bkpf-gjahr.
*** US - 81799 - Inicio - CBRAND.
        "WA_BSID-HBKID = 'BBRA'. " Temporario
        wa_bsid-hbkid = p_hbkid.  "US - 81799 - CBRAND.
*** US - 81799 - Fim - CBRAND.

*** BUG - 83643 - Inicio - CBRAND
        IF  p_hbkid = 'ITAU3' OR p_hbkid = 'ITAU4'.
          CLEAR: lva_numero.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_j_1bnfdoc-nfenum
            IMPORTING
              output = lva_numero.

          CONCATENATE '109' lva_numero INTO wa_saida-dtaid.
          CONDENSE wa_saida-dtaid NO-GAPS.
*** BUG - 83643 - Inicio - CBRAND
        ELSE.
          READ TABLE it_t045t INTO wa_t045t WITH KEY bukrs = wa_bsid-bukrs 	hbkid = wa_bsid-hbkid BINARY SEARCH.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_t045t-dtaid
            IMPORTING
              output = wa_t045t-dtaid.

          wa_saida-dtaid         = wa_t045t-dtaid+0(7).
          CONCATENATE wa_saida-dtaid wa_j_1bnflin-refkey+0(10)  INTO wa_saida-dtaid.
        ENDIF.

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsid-lifnr_e BINARY SEARCH.
        wa_saida-xename1       = wa_lfa1-name1.
        wa_saida-xestras       = wa_lfa1-stras.
        wa_saida-xeort02       = wa_lfa1-ort02.
        wa_saida-xeort01       = wa_lfa1-ort01.
        wa_saida-xeregio       = wa_lfa1-regio.
        wa_saida-xepstlz       = wa_lfa1-pstlz.
        wa_saida-xestcd1       = wa_lfa1-stcd1.

        wa_saida-nfenum        = wa_j_1bnfdoc-nfenum.
        wa_saida-refkey        = wa_j_1bnflin-refkey.

        IF wa_zsdt0090 IS INITIAL.
          wa_saida-zbd1t         = wa_bsid-zfbdt + wa_bsid-zbd1t.
        ELSE.
          wa_saida-zbd1t         = wa_zsdt0090-valdt.
        ENDIF.

        wa_saida-dmbtr         = wa_bsid-dmbtr.

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        wa_saida-txt_instrucao1 = p_instrucoes-txt_instrucao1.
        wa_saida-txt_instrucao2 = p_instrucoes-txt_instrucao2.
        wa_saida-txt_instrucao3 = p_instrucoes-txt_instrucao3.
        wa_saida-txt_instrucao4 = p_instrucoes-txt_instrucao4.
        wa_saida-txt_instrucao5 = p_instrucoes-txt_instrucao5.
        wa_saida-txt_instrucao6 = p_instrucoes-txt_instrucao6.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610


*** BUG - 83643 - CBRAND - Inicio
        IF wa_bsid-hbkid = 'BBRA'.
*** BUG - 83643 - CBRAND - Fim

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
***          IF it_zfit0048[] IS NOT INITIAL.
***            READ TABLE it_zfit0048 INTO wa_zfit0048 INDEX 1.
***            CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
***            wl_cont = strlen( wa_zfit0048-txt_instrucao ).
***            wl_cont_aux = wl_cont / 79.
***            DO.
***              IF sy-index = 1.
***                wa_saida-txt_instrucao1 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***              ELSEIF sy-index = 2.
***                wa_saida-txt_instrucao2 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***              ELSEIF sy-index = 3.
***                wa_saida-txt_instrucao3 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***              ELSEIF sy-index = 4.
***                wa_saida-txt_instrucao4 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***              ELSEIF sy-index = 5.
***                wa_saida-txt_instrucao5 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***              ELSEIF sy-index = 6.
***                wa_saida-txt_instrucao6 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***              ENDIF.
***              ADD 80 TO wl_cont_aux2.
***              IF wl_cont_aux2 GT wl_cont.
***                EXIT.
***              ENDIF.
***            ENDDO.
***          ELSE.
***            IF p_tipo = 'A'.
***              wa_saida-txt_instrucao5 = |- não receber após 3 dias úteis do vencimento;|.
***              wa_saida-txt_instrucao6 = |- sujeito a protesto;|.
***            ENDIF.
***          ENDIF.
        ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
        wa_saida-data_sist     = sy-datum.

        READ TABLE it_t012 INTO wa_t012 WITH KEY bukrs = wa_bsid-bukrs 	hbkid = wa_bsid-hbkid BINARY SEARCH.
        var_bankl              = wa_t012-bankl+4(10). "Agencia
        CONDENSE var_bankl NO-GAPS.
*** BUG - 83643 - Inicio - CBRAND
        IF p_hbkid = 'ITAU3' OR p_hbkid = 'ITAU4'.
          wa_saida-bankl+0(10)   = var_bankl.
        ELSE.
*** BUG - 83643 - Fim - CBRAND
          CONCATENATE var_bankl '-' '7' INTO var_bankl SEPARATED BY space.
          wa_saida-bankl+0(10)   = var_bankl.
        ENDIF.

        READ TABLE it_t012k INTO wa_t012k WITH KEY bukrs = wa_bsid-bukrs 	hbkid = wa_bsid-hbkid BINARY SEARCH.
        wa_saida-bankn         = wa_t012k-bankn.
        wa_saida-bkont         = wa_t012k-bkont.

        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsid-kunnr BINARY SEARCH.
        wa_saida-name1         = wa_kna1-name1.
        wa_saida-stcd1         = wa_kna1-stcd1.
*** BUG - 83643 - Inicio - CBRAND
        IF ( p_hbkid = 'ITAU3' OR p_hbkid = 'ITAU4' ) AND wa_kna1-stkzn = 'X'.
          CLEAR: wa_saida-stcd1.
          wa_saida-stcd1  = wa_kna1-stcd2.
        ENDIF.
*** BUG - 83643 - Fim - CBRAND

        wa_saida-stras         = wa_kna1-stras.
        wa_saida-ort01         = wa_kna1-ort01.
        wa_saida-regio         = wa_kna1-regio.
        wa_saida-pstlz         = wa_kna1-pstlz.

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsid-lifnr_f BINARY SEARCH.
        wa_saida-xfname1       = wa_lfa1-name1.
        wa_saida-xfstcd1       = wa_lfa1-stcd1.
*** BUG - 83643 - Inicio - CBRAND
        IF p_hbkid = 'ITAU3' OR p_hbkid = 'ITAU4'.
          PERFORM calcula_cod_barras_itau2 USING wa_saida-dmbtr wa_saida-zbd1t wa_saida-dtaid p_hbkid wa_saida-bankl
                wa_saida-bankn CHANGING vdv_nosso.
        ELSE.
*** BUG - 83643 - Fim - CBRAND
          PERFORM calcula_cod_barras USING wa_saida-dmbtr wa_saida-zbd1t wa_saida-dtaid p_hbkid CHANGING vdv_nosso.
        ENDIF.

* ---> S4 Migration - 07/07/2023 - JP
*        SELECT SINGLE *
*           FROM bseg
*           INTO wl_bseg
*           WHERE bukrs = wa_bsid-bukrs
*           AND   belnr = wa_bsid-belnr
*           AND   gjahr = wa_bsid-gjahr
*           AND   buzei = '001'.
*

        DATA: lv_rldnr TYPE  rldnr,
              lv_bukrs TYPE  bukrs,
              lv_belnr TYPE  belnr_d,
              lv_gjahr TYPE  gjahr,
              lv_buzei TYPE  buzei.

        lv_bukrs = wa_bsid-bukrs.
        lv_belnr = wa_bsid-belnr.
        lv_gjahr = wa_bsid-gjahr.
        lv_buzei = '001'.

        DATA: lt_bseg_aux TYPE fagl_t_bseg.

        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
          IMPORTING
            e_rldnr       = lv_rldnr
          EXCEPTIONS
            not_found     = 1
            more_than_one = 2.

        IF sy-subrc = 0.

          CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
            EXPORTING
              i_rldnr   = lv_rldnr
              i_bukrs   = lv_bukrs
              i_belnr   = lv_belnr
              i_gjahr   = lv_gjahr
              i_buzei   = lv_buzei
            IMPORTING
              et_bseg   = lt_bseg_aux
            EXCEPTIONS
              not_found = 1.
        ENDIF.


        IF sy-subrc <> 0 OR lines( lt_bseg_aux ) = 0.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ELSE.

          sy-dbcnt = lines( lt_bseg_aux ).
          READ TABLE lt_bseg_aux INTO wl_bseg INDEX 1.

        ENDIF.
* <--- S4 Migration - 07/07/2023 - JP

        v_xref2 = wa_j_1bnfdoc-nfenum.
        v_xref3 = wa_saida-dtaid.
        IF wl_bseg-xref3 NE v_xref3.
          v_tipo = 'N'.
          PERFORM executa_shdb_fb02 USING wa_bsid-bukrs wa_bsid-belnr wa_bsid-gjahr v_xref2 v_xref3 v_tipo p_hbkid.
        ENDIF.

        APPEND wa_saida TO it_saida.

      ENDLOOP.
    ENDLOOP.

  ENDLOOP.

  " Solicitação OV tipo <> 'N'
  LOOP AT it_zsdt0054 INTO wa_zsdt0054.

    LOOP AT it_bsid INTO wa_bsid  WHERE bukrs =	wa_zsdt0051-vkorg
                                  AND   belnr	=	wa_zsdt0054-adiant.


*** US - 81799 - Inicio - CBRAND.
      "WA_BSID-HBKID = 'BBRA'. " Temporario
      wa_bsid-hbkid = p_hbkid.
*** US - 81799 - Fim - CBRAND.


*** BUG - 83646 - Inicio - CBRAND
      IF p_hbkid = 'ITAU3' OR p_hbkid = 'ITAU4'.
        CLEAR: lva_numero.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_zsdt0054-nro_sol_ov
          IMPORTING
            output = lva_numero.

        CONCATENATE '109' lva_numero INTO wa_saida-dtaid.
        CONDENSE wa_saida-dtaid NO-GAPS.

      ELSE.
*** BUG - 83646 - Inicio - CBRAND
        READ TABLE it_t045t INTO wa_t045t WITH KEY bukrs = wa_bsid-bukrs 	hbkid = wa_bsid-hbkid BINARY SEARCH.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_t045t-dtaid
          IMPORTING
            output = wa_t045t-dtaid.

        wa_saida-dtaid         = wa_t045t-dtaid+0(7).
        CONCATENATE wa_saida-dtaid wa_bsid-belnr  INTO wa_saida-dtaid.

      ENDIF.

      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsid-lifnr_e BINARY SEARCH.
      wa_saida-xename1       = wa_lfa1-name1.
      wa_saida-xestras       = wa_lfa1-stras.
      wa_saida-xeort02       = wa_lfa1-ort02.
      wa_saida-xeort01       = wa_lfa1-ort01.
      wa_saida-xeregio       = wa_lfa1-regio.
      wa_saida-xepstlz       = wa_lfa1-pstlz.
      wa_saida-xestcd1       = wa_lfa1-stcd1.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zsdt0054-nro_sol_ov
        IMPORTING
          output = wa_saida-nfenum.

      wa_saida-refkey        = wa_bsid-belnr.

      IF wa_zsdt0090 IS INITIAL.
        wa_saida-zbd1t         = wa_bsid-zfbdt + wa_bsid-zbd1t.
      ELSE.
        wa_saida-zbd1t         = wa_zsdt0090-valdt.
      ENDIF.

      wa_saida-dmbtr         = wa_bsid-dmbtr.

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
      wa_saida-txt_instrucao1 = p_instrucoes-txt_instrucao1.
      wa_saida-txt_instrucao2 = p_instrucoes-txt_instrucao2.
      wa_saida-txt_instrucao3 = p_instrucoes-txt_instrucao3.
      wa_saida-txt_instrucao4 = p_instrucoes-txt_instrucao4.
      wa_saida-txt_instrucao5 = p_instrucoes-txt_instrucao5.
      wa_saida-txt_instrucao6 = p_instrucoes-txt_instrucao6.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610

*** BUG - 83643 - CBRAND - Inicio
      IF wa_bsid-hbkid = 'BBRA'.
*** BUG - 83643 - CBRAND - Fim

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
***        IF it_zfit0048[] IS NOT INITIAL.
***          READ TABLE it_zfit0048 INTO wa_zfit0048 INDEX 1.
***          CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
***          wl_cont = strlen( wa_zfit0048-txt_instrucao ).
***          wl_cont_aux = wl_cont / 79.
***          DO.
***            IF sy-index = 1.
***              wa_saida-txt_instrucao1 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***            ELSEIF sy-index = 2.
***              wa_saida-txt_instrucao2 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***            ELSEIF sy-index = 3.
***              wa_saida-txt_instrucao3 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***            ELSEIF sy-index = 4.
***              wa_saida-txt_instrucao4 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***            ELSEIF sy-index = 5.
***              wa_saida-txt_instrucao5 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***            ELSEIF sy-index = 6.
***              wa_saida-txt_instrucao6 = wa_zfit0048-txt_instrucao+wl_cont_aux2.
***            ENDIF.
***            ADD 80 TO wl_cont_aux2.
***            IF wl_cont_aux2 GT wl_cont.
***              EXIT.
***
***            ENDIF.
***          ENDDO.
***        ELSE.
***          IF p_tipo = 'A'.
***            wa_saida-txt_instrucao5 = |- não receber após 3 dias úteis do vencimento;|.
***            wa_saida-txt_instrucao6 = |- sujeito a protesto;|.
***          ENDIF.
***        ENDIF.
      ENDIF.

*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
      wa_saida-data_sist     = sy-datum.

      READ TABLE it_t012 INTO wa_t012 WITH KEY bukrs = wa_bsid-bukrs 	hbkid = wa_bsid-hbkid BINARY SEARCH.
      var_bankl              = wa_t012-bankl+4(10). "Agencia
      CONDENSE var_bankl NO-GAPS.
*** BUG - 83643 - Inicio - CBRAND
      IF p_hbkid = 'ITAU3' OR p_hbkid = 'ITAU4'.
        wa_saida-bankl+0(10)   = var_bankl.
      ELSE.
*** BUG - 83643 - Fim - CBRAND
        CONCATENATE var_bankl '-' '7' INTO var_bankl SEPARATED BY space.
        wa_saida-bankl+0(10)   = var_bankl.
      ENDIF.
      READ TABLE it_t012k INTO wa_t012k WITH KEY bukrs = wa_bsid-bukrs 	hbkid = wa_bsid-hbkid BINARY SEARCH.
      wa_saida-bankn         = wa_t012k-bankn.
      wa_saida-bkont         = wa_t012k-bkont.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsid-kunnr BINARY SEARCH.
      wa_saida-name1         = wa_kna1-name1.
      wa_saida-stcd1         = wa_kna1-stcd1.
*** BUG - 83643 - Inicio - CBRAND
      IF ( p_hbkid = 'ITAU3' OR  p_hbkid = 'ITAU4' ) AND wa_kna1-stkzn = 'X'.
        CLEAR: wa_saida-stcd1.
        wa_saida-stcd1  = wa_kna1-stcd2.
      ENDIF.
*** BUG - 83643 - Fim - CBRAND

      wa_saida-stras         = wa_kna1-stras.
      wa_saida-ort01         = wa_kna1-ort01.
      wa_saida-regio         = wa_kna1-regio.
      wa_saida-pstlz         = wa_kna1-pstlz.

      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsid-lifnr_f BINARY SEARCH.
      wa_saida-xfname1       = wa_lfa1-name1.
      wa_saida-xfstcd1       = wa_lfa1-stcd1.


*** BUG - 83643 - Inicio - CBRAND
      IF p_hbkid = 'ITAU3' OR p_hbkid = 'ITAU4'.
        PERFORM calcula_cod_barras_itau2 USING wa_saida-dmbtr wa_saida-zbd1t wa_saida-dtaid p_hbkid wa_saida-bankl
              wa_saida-bankn CHANGING vdv_nosso.
      ELSE.
*** BUG - 83643 - Fim - CBRAND
        PERFORM calcula_cod_barras USING wa_saida-dmbtr wa_saida-zbd1t wa_saida-dtaid p_hbkid CHANGING vdv_nosso.
      ENDIF.


* ---> S4 Migration - 07/07/2023 - JP
* US-162911 - WBARBOSA 25/02/2025
      SELECT SINGLE *
         FROM bseg
         INTO wl_bseg
         WHERE bukrs = wa_bsid-bukrs
         AND   belnr = wa_bsid-belnr
         AND   gjahr = wa_bsid-gjahr
         AND   buzei = '001'.
* US-162911 - WBARBOSA 25/02/2025

*      LV_BUKRS = WA_BSID-BUKRS.
*      LV_BELNR = WA_BSID-BELNR.
*      LV_GJAHR = WA_BSID-GJAHR.
*      LV_BUZEI = '001'.
*
*      CLEAR: LT_BSEG_AUX.
*
*      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
*        IMPORTING
*          E_RLDNR       = LV_RLDNR
*        EXCEPTIONS
*          NOT_FOUND     = 1
*          MORE_THAN_ONE = 2.
*
*      IF SY-SUBRC = 0.
*
*        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
*          EXPORTING
*            I_RLDNR   = LV_RLDNR
*            I_BUKRS   = LV_BUKRS
*            I_BELNR   = LV_BELNR
*            I_GJAHR   = LV_GJAHR
*            I_BUZEI   = LV_BUZEI
*          IMPORTING
*            ET_BSEG   = LT_BSEG_AUX
*          EXCEPTIONS
*            NOT_FOUND = 1.
*      ENDIF.
*
*
*      IF SY-SUBRC <> 0 OR LINES( LT_BSEG_AUX ) = 0.
*        SY-SUBRC = 4.
*        SY-DBCNT = 0.
*      ELSE.
*
*        SY-DBCNT = LINES( LT_BSEG_AUX ).
*        READ TABLE LT_BSEG_AUX INTO WL_BSEG INDEX 1.
*
*      ENDIF.
* <--- S4 Migration - 07/07/2023 - JP

* US-162911 - WBARBOSA 25/02/2025

      IF p_hbkid = 'ITAU3' OR p_hbkid = 'ITAU4'.

        v_api = abap_false.
**** BUG - 188695 - CBRAND - Inicio
*        CALL METHOD zcl_fi_utils=>get_stvarv
*          EXPORTING
*            i_name = CONV #( zcl_fi_utils=>lc_user-user_api )
*            i_low  = CONV #( sy-uname )
*          RECEIVING
*            is_ok  = DATA(is_valid).
*
*        IF is_valid IS NOT INITIAL.
**** BUG - 188695 - CBRAND - Fim
        zcl_fi_utils=>check_bolecode_itau(
          EXPORTING
            i_nosso_numero = CONV #( wl_bseg-xref3 )
            i_hbkid        = p_hbkid
            i_empresa      = wa_zsdt0051-vkorg
            i_solicitacao  = wa_zsdt0054-nro_sol_ov
            i_belnr        = wl_bseg-belnr "BUG - 189144 - CBRAND
            i_gjahr        = wl_bseg-gjahr "BUG - 189144 - CBRAND
          CHANGING
            c_dados_boleto = wa_saida
          RECEIVING
            e_retorno_itau = DATA(verificar)
        ).

        "ENDIF. BUG - 188695 - CBRAND

        v_api = abap_true.

      ENDIF.
* US-162911 - WBARBOSA 25/02/2025

* BUG - 189144 - Inicio - CBRAND
*      IF  wa_zsdt0051-nro_sol_ov IS NOT INITIAL.
*        v_xref2 = wa_zsdt0054-nro_sol_ov.
*        v_xref3 = wa_saida-dtaid.
*
*        IF v_api IS NOT INITIAL.
*          v_xref3 = wa_saida-dtaid+3.
*        ENDIF.
*
*        IF wl_bseg-xref3 NE v_xref3.
*          v_tipo = 'O'.
***** BUG - 188695 - CBRAND - Inicio
*          PERFORM fm_fi_document_change USING wa_bsid-bukrs wa_bsid-belnr wa_bsid-gjahr v_xref2 v_xref3 v_tipo p_hbkid.
*
**          PERFORM executa_shdb_fb02 USING wa_bsid-bukrs wa_bsid-belnr wa_bsid-gjahr v_xref2 v_xref3 v_tipo p_hbkid.
***** BUG - 188695 - CBRAND - Fim
*        ENDIF.
*      ENDIF.
* BUG - 189144 - Fim - CBRAND

      APPEND wa_saida TO it_saida.

    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " F_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  CALCULA_COD_BARRAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calcula_cod_barras USING p_valor p_vencto p_nosso p_hbkid CHANGING vdv_nosso.

  " Variáveis para o código de barras
  DATA: var_cod_barras(44),
        var_cod_barras_cop(44),
        var_cod_barras_inv(43),
        var_cod_barras_fim(44),
        var_banco(03)            VALUE '001',
        var_moeda(01)            VALUE '9',
        var_dig05(01)            VALUE ' ',
        var_fator(04),
        var_valor(10)            TYPE n,
        var_zeros(06)            VALUE '000000',                     "campo livre
        var_nosso_numero(17) ,   "campo livre
        var_nosso_numero_cop(17) ,                        "campo livre
        var_nosso_numero_inv(17) ,                        "campo livre
        "VDV_NOSSO(1),
        var_carteira(02)         VALUE '17'.                      "campo livre

  " Variáveis para a linha digitável
  DATA: var_linha_dig(54),
        vcampo1(9),
        vcampo1_fim(11),
        vcampo1_inv(9),
        vcampo1_cop(9),
        vdv1(1),
        vcampo2(10),
        vcampo2_fim(12),
        vcampo2_inv(10),
        vcampo2_cop(10),
        vdv2(1),
        vcampo3(10),
        vcampo3_fim(12),
        vcampo3_inv(10),
        vcampo3_cop(10),
        vdv3(1),
        vcampo4(1),
        vcampo5(14),
        xcalc             TYPE i,
        vcalc             TYPE i,
        ccalc(2)          TYPE n,
        vtot_campo_2(2)   TYPE n,
        vtot_campo_3(3)   TYPE n,
        vdezena           TYPE i.

  DATA : data_base TYPE sy-datum VALUE '19971007',
         xcontador TYPE i,
         xposicao  TYPE i,
         xmulti    TYPE i,
         xtotal    TYPE i,
         xresto    TYPE i,
         xdvcod    TYPE i,
         cdvcod(1).


  var_nosso_numero = p_nosso.

  " FGM 08/01/25 - Ajuste no fator de vencimento após estouro 9999
  "  var_fator = p_vencto - data_base.
  IF p_vencto <= '20250221'.
    var_fator = p_vencto - data_base.
  ELSE.
    data_base = '20250222'.
    var_fator = p_vencto - data_base.
    var_fator = var_fator + 1000.
  ENDIF.

  var_valor = p_valor * 100.

  CONCATENATE var_banco var_moeda var_dig05 var_fator var_valor var_zeros var_nosso_numero var_carteira INTO var_cod_barras.

  var_cod_barras_cop = var_cod_barras.

  " Inverte codigo de barras
  xcontador = 0.
  WHILE var_cod_barras_cop NE space.
    ADD 1 TO xcontador.
    xposicao = 43 - xcontador.
    var_cod_barras_inv+xposicao(1) = var_cod_barras_cop(1).
    SHIFT var_cod_barras_cop.
  ENDWHILE.

  " Calculo Digito DV
  xmulti = 2.
  xtotal = 0.
  WHILE var_cod_barras_inv NE space.
    xposicao = var_cod_barras_inv(1).
    IF xmulti = 10.
      xmulti = 2.
    ENDIF.
    xtotal = xtotal + ( xmulti * xposicao ).
    ADD 1 TO xmulti.
    SHIFT var_cod_barras_inv.
  ENDWHILE.

  xresto = xtotal MOD 11.

  xdvcod = 11 - xresto.

  IF xdvcod = 0 OR xdvcod = 10 OR xdvcod = 11.
    xdvcod = 1.
  ENDIF.

  cdvcod = xdvcod.
  CONCATENATE var_cod_barras+0(4) cdvcod var_cod_barras+4(39)  INTO var_cod_barras_fim.

  " LINHA DIGITAVEL.
  CONCATENATE var_banco var_moeda var_cod_barras_fim+19(5) INTO vcampo1.
  vcampo2 = var_cod_barras_fim+24(10).
  vcampo3 = var_cod_barras_fim+34(10).
  vcampo4 = cdvcod.
  CONCATENATE var_fator var_valor INTO vcampo5.


  " Inverte campo1
  vcampo1_cop = vcampo1.
  xcontador = 0.
  WHILE vcampo1_cop NE space.
    ADD 1 TO xcontador.
    xposicao = 9 - xcontador.
    vcampo1_inv+xposicao(1) = vcampo1_cop(1).
    SHIFT vcampo1_cop.
  ENDWHILE.

  " Calculo Digito DV Campo1
  xmulti = 2.
  xtotal = 0.
  WHILE vcampo1_inv NE space.
    xposicao = vcampo1_inv(1).
    IF xmulti = 0.
      xmulti = 2.
    ENDIF.
    xcalc = ( xmulti * xposicao ).
    ccalc = xcalc .
    xcalc = 0.
    WHILE ccalc NE space.
      vcalc = ccalc(1).
      xcalc = xcalc + vcalc.
      SHIFT ccalc.
    ENDWHILE.
    xtotal = xtotal + xcalc.
    SUBTRACT 1 FROM xmulti.
    SHIFT vcampo1_inv.
  ENDWHILE.

  IF xtotal LT 100.
    vtot_campo_2 = xtotal.
    IF  xtotal < 10.
      vdezena = 10.
    ELSE.
      vdezena = vtot_campo_2+0(1).
      vdezena = ( vdezena + 1 ) * 10.
    ENDIF.
  ELSE.
    vtot_campo_3 = xtotal.
    vdezena = vtot_campo_3+0(2).
    vdezena = ( vdezena + 1 ) * 10.
  ENDIF.

  xdvcod = vdezena - xtotal.
  IF xdvcod = 10.
    vdv1 = 0.
  ELSE.
    vdv1 = xdvcod.
  ENDIF.

  " Inverte campo2
  vcampo2_cop = vcampo2.
  xcontador = 0.
  WHILE vcampo2_cop NE space.
    ADD 1 TO xcontador.
    xposicao = 10 - xcontador.
    vcampo2_inv+xposicao(1) = vcampo2_cop(1).
    SHIFT vcampo2_cop.
  ENDWHILE.

  " Calculo Digito DV Campo2
  xmulti = 2.
  xtotal = 0.
  WHILE vcampo2_inv NE space.
    xposicao = vcampo2_inv(1).
    IF xmulti = 0.
      xmulti = 2.
    ENDIF.
    xcalc = ( xmulti * xposicao ).
    ccalc = xcalc .
    xcalc = 0.
    WHILE ccalc NE space.
      vcalc = ccalc(1).
      xcalc = xcalc + vcalc.
      SHIFT ccalc.
    ENDWHILE.
    xtotal = xtotal + xcalc.
    SUBTRACT 1 FROM xmulti.
    SHIFT vcampo2_inv.
  ENDWHILE.

  IF xtotal LT 100.
    vtot_campo_2 = xtotal.
    IF  xtotal < 10.
      vdezena = 10.
    ELSE.
      vdezena = vtot_campo_2+0(1).
      vdezena = ( vdezena + 1 ) * 10.
    ENDIF.
  ELSE.
    vtot_campo_3 = xtotal.
    vdezena = vtot_campo_3+0(2).
    vdezena = ( vdezena + 1 ) * 10.
  ENDIF.

  xdvcod = vdezena - xtotal.
  IF xdvcod = 10.
    vdv2 = 0.
  ELSE.
    vdv2 = xdvcod.
  ENDIF.

  " Inverte campo3
  vcampo3_cop = vcampo3.
  xcontador = 0.
  WHILE vcampo3_cop NE space.
    ADD 1 TO xcontador.
    xposicao = 10 - xcontador.
    vcampo3_inv+xposicao(1) = vcampo3_cop(1).
    SHIFT vcampo3_cop.
  ENDWHILE.

  " Calculo Digito DV Campo3
  xmulti = 2.
  xtotal = 0.
  WHILE vcampo3_inv NE space.
    xposicao = vcampo3_inv(1).
    IF xmulti = 0.
      xmulti = 2.
    ENDIF.
    xcalc = ( xmulti * xposicao ).
    ccalc = xcalc .
    xcalc = 0.
    WHILE ccalc NE space.
      vcalc = ccalc(1).
      xcalc = xcalc + vcalc.
      SHIFT ccalc.
    ENDWHILE.
    xtotal = xtotal + xcalc.
    SUBTRACT 1 FROM xmulti.
    SHIFT vcampo3_inv.
  ENDWHILE.

  IF xtotal LT 100.
    vtot_campo_2 = xtotal.
    IF  xtotal < 10.
      vdezena = 10.
    ELSE.
      vdezena = vtot_campo_2+0(1).
      vdezena = ( vdezena + 1 ) * 10.
    ENDIF.
  ELSE.
    vtot_campo_3 = xtotal.
    vdezena = vtot_campo_3+0(2).
    vdezena = ( vdezena + 1 ) * 10.
  ENDIF.

  xdvcod = vdezena - xtotal.
  IF xdvcod = 10.
    vdv3 = 0.
  ELSE.
    vdv3 = xdvcod.
  ENDIF.

  " monta linha digitável.
  CONCATENATE vcampo1+0(5) '.' vcampo1+5(4) vdv1 INTO  vcampo1_fim.
  CONCATENATE vcampo2+0(5) '.' vcampo2+5(5) vdv2 INTO  vcampo2_fim.
  CONCATENATE vcampo3+0(5) '.' vcampo3+5(5) vdv3 INTO  vcampo3_fim.
  CONCATENATE vcampo1_fim vcampo2_fim vcampo3_fim vcampo4 vcampo5 INTO var_linha_dig SEPARATED BY space.


  " Inverte NOSSO NUMERO
  var_nosso_numero_cop = var_nosso_numero.
  xcontador = 0.
  WHILE var_nosso_numero_cop NE space.
    ADD 1 TO xcontador.
    xposicao = 17 - xcontador.
    var_nosso_numero_inv+xposicao(1) = var_nosso_numero_cop(1).
    SHIFT var_nosso_numero_cop.
  ENDWHILE.

  " Calculo Digito NOSSO NUMERO
  xmulti = 9.
  xtotal = 0.
  WHILE var_nosso_numero_inv NE space.
    xposicao = var_nosso_numero_inv(1).
    IF xmulti = 1.
      xmulti = 9.
    ENDIF.
    xcalc = ( xmulti * xposicao ).
    xtotal = xtotal + xcalc.
    SUBTRACT 1 FROM xmulti.
    SHIFT var_nosso_numero_inv.
  ENDWHILE.

  xresto = xtotal MOD 11.

  CLEAR vdv_nosso.
  IF xresto LT 10.
    vdv_nosso = xresto.
  ELSEIF xresto EQ 10.
    vdv_nosso = 'X'.
  ENDIF.

  wa_saida-var_cod_barras_fim = var_cod_barras_fim.
  wa_saida-var_linha_dig = var_linha_dig.

ENDFORM.                    " CALCULA_COD_BARRAS

*&---------------------------------------------------------------------*
*&      Form  CALCULA_COD_BARRAS_ITAU2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calcula_cod_barras_itau2 USING p_valor p_vencto p_nosso p_hbkid p_var_bankl p_var_bankn CHANGING vdv_nosso.

  " Variáveis para o código de barras
  DATA: var_cod_barras(44),
        var_cod_barras_cop(44),
        var_cod_barras_inv(43),
        var_cod_barras_fim(44),
        var_banco(03)            VALUE '341',
        var_moeda(01)            VALUE '9',
        var_dig05(01)            VALUE ' ',
        var_fator(04),
        var_valor(10)            TYPE n,
        var_zeros(06)            VALUE '000000',                     "campo livre
        var_nosso_numero(17) ,   "campo livre
        var_nosso_numero_cop(17) ,                        "campo livre
        var_nosso_numero_inv(17) ,                        "campo livre
        var_carteira(03)         VALUE '109'.                      "campo livre

  " Variáveis para a linha digitável
  DATA: var_linha_dig(54),
        vcampo1(9),
        vcampo1_fim(11),
        vcampo1_inv(9),
        vcampo1_cop(9),
        vdv1(1),
        vcampo2(10),
        vcampo2_fim(12),
        vcampo2_inv(10),
        vcampo2_cop(10),
        vdv2(1),
        vcampo3(10),
        vcampo3_fim(13),
        vcampo3_inv(10),
        vcampo3_cop(10),
        vdv3(1),
        vcampo4(1)        TYPE n,
        vcampo5(14),
        xcalc             TYPE i,
        vcalc             TYPE i,
        ccalc(2)          TYPE n,
        vtot_campo_2(2)   TYPE n,
        vtot_campo_3(3)   TYPE n,
        vdezena           TYPE i.

  DATA : data_base TYPE sy-datum VALUE '19971007',
         xcontador TYPE i,
         xposicao  TYPE i,
         xmulti    TYPE i,
         xtotal    TYPE i,
         xresto    TYPE i,
         xdvcod    TYPE i,
         cdvcod(1).

  DATA: lva_dig_01     TYPE c,
        lva_dig_02     TYPE c,
        lva_dig_02_aux TYPE c,
        lva_dig_03     TYPE c,
        lva_dig_03_aux TYPE c,
        lva_dig_04     TYPE c,
        lva_dig_05     TYPE c,
        lva_bankl(15)  TYPE c.

  DATA: lva_dac_01(11)     TYPE c,
        lva_dac_02(20)     TYPE c,
        lva_dac_02_aux(11) TYPE c,
        lva_dac_03(12)     TYPE c,
        lva_dac_03_aux(12) TYPE c.




  REPLACE ALL OCCURRENCES OF '-' IN p_var_bankn WITH space.
  CONDENSE p_var_bankn NO-GAPS.

  " FGM 08/01/25 - Ajuste no fator de vencimento após estouro 9999
  "  var_fator = p_vencto - data_base.
  IF p_vencto <= '20250221'.
    var_fator = p_vencto - data_base.
  ELSE.
    data_base = '20250222'.
    var_fator = p_vencto - data_base.
    var_fator = var_fator + 1000.
  ENDIF.

  var_valor = p_valor * 100.

* Campo 1 (AAABC.CCDDX)
*  1 -  Banco - Moeda - Carteira - Nosso numero sem '109'
  CONCATENATE var_banco var_moeda var_carteira p_nosso+3(2)  INTO lva_dac_01.

  CALL FUNCTION 'CALCULATE_CHECK_DIGIT_MOD10'
    EXPORTING
      number_part = lva_dac_01
    IMPORTING
      check_digit = lva_dig_01.

  CONCATENATE lva_dac_01+0(5) '.' lva_dac_01+5(4) lva_dig_01 INTO  vcampo1_fim.


* 2 - DAC do campo [Agência/Conta/Carteira/ Nosso Número]

  CONCATENATE p_var_bankl+0(3) p_var_bankn var_carteira p_nosso+3(8) INTO lva_dac_02.

  CALL FUNCTION 'CALCULATE_CHECK_DIGIT_MOD10'
    EXPORTING
      number_part = lva_dac_02
    IMPORTING
      check_digit = lva_dig_02.

*Inicio Alteração - Leandro Valentim Ferreira - 31.05.23 - 112133
***  CONCATENATE p_nosso+5(6) lva_dig_02 p_var_bankl+0(3) INTO lva_dac_02_aux.
  lva_dig_02 = p_nosso+11(1).
  CONCATENATE p_nosso+5(6) lva_dig_02 p_var_bankl+0(3) INTO lva_dac_02_aux.
*Fim Alteração - Leandro Valentim Ferreira - 31.05.23 - 112133


  CALL FUNCTION 'CALCULATE_CHECK_DIGIT_MOD10'
    EXPORTING
      number_part = lva_dac_02_aux
    IMPORTING
      check_digit = lva_dig_02_aux.


  CONCATENATE p_nosso+5(5) '.' p_nosso+10(1) lva_dig_02 p_var_bankl+0(3) lva_dig_02_aux  INTO  vcampo2_fim.

* 3 DAC Agencia

  lva_bankl = p_var_bankl.
  lva_bankl = lva_bankl+3(12).

  CONDENSE lva_bankl NO-GAPS.

  CONCATENATE lva_bankl p_var_bankn  '000' INTO lva_dac_03.

  CALL FUNCTION 'CALCULATE_CHECK_DIGIT_MOD10'
    EXPORTING
      number_part = lva_dac_03
    IMPORTING
      check_digit = lva_dig_03.

  CONCATENATE lva_bankl p_var_bankn+0(4) '.'  p_var_bankn+4(2) '000' lva_dig_03  INTO  vcampo3_fim.


*  CALL FUNCTION 'CALCULATE_CHECK_DIGIT_MOD10'
*    EXPORTING
*      number_part = lva_dac_03_aux
*    IMPORTING
*      check_digit = lva_dig_03_aux.
*
*
*  CONCATENATE lva_bankl p_var_bankn+0(4) '.'  p_var_bankn+4(2) '000' lva_dig_03 lva_dig_03_aux INTO  vcampo3_fim.


  CONCATENATE var_fator var_valor INTO vcampo5.


* MODULO 11 PARA CALCULO DO DIGITO 04 - Inicio

  DATA: length  TYPE i, mult1 TYPE n, mult2 TYPE n VALUE 2,
        prod(2) TYPE n, addi TYPE p VALUE 0, modu(1) TYPE n, rest TYPE p.
  DATA: work_string(50) TYPE c.


  " A variavel lva_dig_02 é o codigo DAC para esse calculo

  CONCATENATE var_banco var_moeda var_fator var_valor var_carteira p_nosso+3(8) lva_dig_02  p_var_bankl p_var_bankn  '000' INTO work_string.

  CONDENSE work_string NO-GAPS.

  length = strlen( work_string ).

  SHIFT work_string RIGHT DELETING TRAILING space.

  DO length TIMES.
    SHIFT work_string RIGHT CIRCULAR.
    WRITE work_string(1) TO mult1.
    prod = mult1 * mult2.
    addi = addi  + prod.

*Inicio Alteração - Leandro Valentim Ferreira - 31.05.23 - 112133
***    IF mult2 < 9 .
***      mult2 =  mult2 + 1.
***    ELSE.
***      mult2 = 2.
***    ENDIF.
    IF mult2 = 9 .
      mult2 = 2.
    ELSE.
      mult2 =  mult2 + 1.
    ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 31.05.23 - 112133
  ENDDO.

  rest = addi MOD 11.

*Inicio Alteração - Leandro Valentim Ferreira - 31.05.23 - 112133
***  vcampo4 = ( 11 - rest ) MOD 11.
  vcampo4 = ( 11 - rest ).
*Fim Alteração - Leandro Valentim Ferreira - 31.05.23 - 112133
  CASE vcampo4 .
    WHEN  0.
      vcampo4 = 1.
    WHEN  10.
      vcampo4 = 1.
    WHEN 11.
      vcampo4 = 1.
  ENDCASE.
* MODULO 11 PARA CALCULO DO DIGITO 04 - Fim


  CONCATENATE vcampo1_fim vcampo2_fim vcampo3_fim vcampo4 vcampo5 INTO var_linha_dig SEPARATED BY space.


  CONCATENATE work_string+0(4) vcampo4 work_string+4(39)  INTO var_cod_barras_fim.

  wa_saida-var_cod_barras_fim = var_cod_barras_fim.
  wa_saida-var_linha_dig = var_linha_dig.

ENDFORM.                    " CALCULA_COD_BARRAS_ITAU2
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_SMART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM f_imprime_smart  USING    p_wa_saida wl_email doc_numero p_hbkid.
  "FORM
  DATA: ls_control       TYPE ssfctrlop,
        ls_options       TYPE ssfcompop,
        job_output_info  TYPE ssfcrescl,
        ls_xsfparam_line TYPE ssfxsfp.

  "PDF
  DATA: i_otf          TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline        TYPE TABLE OF tline WITH HEADER LINE,
        v_bin_filesize TYPE i,
        i_record       LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        wa_buffer      TYPE string. "To convert from 132 to 255
  "Mail
  DATA: i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
*        Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
*        Work Area declarations
        wa_objhead  TYPE soli_tab,
        wa_doc_chng TYPE sodocchgi1,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.


*** US - 81799 - Inicio - CBRAND
  CASE p_hbkid .
    WHEN 'BBRA'.
      vl_form = 'ZFI_BOLETO_BRA1'.
    WHEN 'ITAU3'.
*      VL_FORM = 'ZFI_BOLETO_ITAU'.
      vl_form = 'ZFI_BOLECODE'. "//  US-162911 - WBARBOSA 26/02/2025
    WHEN 'ITAU4'.
      vl_form = 'ZFI_BOLECODE'.
  ENDCASE.
*** US - 81799 - Fim - CBRAND

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  Impresora
  ls_control-no_dialog = 'X'. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.
  ls_options-tdnoprev = 'X'.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = 'X'.

  CLEAR:job_output_info.
  CALL FUNCTION vl_name
    EXPORTING
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
      wa_saida           = p_wa_saida
    IMPORTING
      job_output_info    = job_output_info
*    TABLES
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.

    i_otf[] = job_output_info-otfdata[].
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 132
      IMPORTING
        bin_filesize          = v_bin_filesize
      TABLES
        otf                   = i_otf
        lines                 = i_tline
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        OTHERS                = 4.

    IF sy-subrc EQ 0.
      IF wl_email IS INITIAL.
        CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
          EXPORTING
            i_otf                    = i_otf[]
          EXCEPTIONS
            convert_otf_to_pdf_error = 1
            cntl_error               = 2
            OTHERS                   = 3.
      ELSE.
        LOOP AT i_tline.
          TRANSLATE i_tline USING '~'.
          CONCATENATE wa_buffer i_tline INTO wa_buffer.
        ENDLOOP.
        TRANSLATE wa_buffer USING '~'.
        DO.
          i_record = wa_buffer.
          APPEND i_record.
          SHIFT wa_buffer LEFT BY 255 PLACES.
          IF wa_buffer IS INITIAL.
            EXIT.
          ENDIF.
        ENDDO.
* ATTACHMENT
        REFRESH: i_reclist,
        i_objtxt,
        i_objbin,
        i_objpack.
        CLEAR wa_objhead.
        i_objbin[] = i_record[].
* Create Message Body Title and Description
        i_objtxt = 'Conforme Combinado, segue em anexo Boleto de cobrança'.
        APPEND i_objtxt.

        DESCRIBE TABLE i_objtxt LINES v_lines_txt.
        READ TABLE i_objtxt INDEX v_lines_txt.
        wa_doc_chng-obj_name = 'smartform'.
        wa_doc_chng-expiry_dat = sy-datum + 10.
        CONCATENATE 'Confirmação de Venda  Nº' doc_numero INTO wa_doc_chng-obj_descr
          SEPARATED BY space.
*     = 'smartform'.
        wa_doc_chng-sensitivty = 'F'.
        wa_doc_chng-doc_size = v_lines_txt * 255.
* Main Text
        CLEAR i_objpack-transf_bin.
        i_objpack-head_start = 1.
        i_objpack-head_num = 0.
        i_objpack-body_start = 1.
        i_objpack-body_num = v_lines_txt.
        i_objpack-doc_type = 'RAW'.
        APPEND i_objpack.
* Attachment (pdf-Attachment)
        i_objpack-transf_bin = 'X'.
        i_objpack-head_start = 1.
        i_objpack-head_num = 0.
        i_objpack-body_start = 1.
        DESCRIBE TABLE i_objbin LINES v_lines_bin.
        READ TABLE i_objbin INDEX v_lines_bin.
        i_objpack-doc_size = v_lines_bin * 255 .
        i_objpack-body_num = v_lines_bin.
        i_objpack-doc_type = 'PDF'.
        i_objpack-obj_name = 'smart'.
        i_objpack-obj_descr = doc_numero.
        APPEND i_objpack.
        CLEAR i_reclist.
        i_reclist-receiver = wl_email.
        i_reclist-rec_type = 'U'.
        APPEND i_reclist.
        CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
          EXPORTING
            document_data              = wa_doc_chng
            put_in_outbox              = 'X'
            commit_work                = 'X'
          TABLES
            packing_list               = i_objpack
            object_header              = wa_objhead
            contents_bin               = i_objbin
            contents_txt               = i_objtxt
            receivers                  = i_reclist
          EXCEPTIONS
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            OTHERS                     = 8.
        IF sy-subrc NE 0.
*WRITE:/ ‘Error When Sending the File’, SY-SUBRC.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ocorreu um erro ao enviar o e-mail'.
        ELSE.
          MESSAGE s836(sd) WITH 'E-mail enviado com sucesso'.
*WRITE:/ ‘Mail sent’.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.                    " F_IMPRIME_SMAR

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA  text
*----------------------------------------------------------------------*
FORM f_imprime_declara  USING doc_numero TYPE j_1bnfdoc-docnum imprimir TYPE char01
                        CHANGING lc_declaracao TYPE xstring
                                 t_pdf         TYPE zlese0034.

  "FORM
  DATA: ls_control       TYPE ssfctrlop,
        ls_options       TYPE ssfcompop,
        job_output_info  TYPE ssfcrescl,
        ls_xsfparam_line TYPE ssfxsfp.

  "PDF
  DATA: i_otf          TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline        TYPE TABLE OF tline WITH HEADER LINE,
        v_bin_filesize TYPE i,
        i_record       LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        wa_buffer      TYPE string. "To convert from 132 to 255
  "Mail
  DATA: i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
*        Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
*        Work Area declarations
        wa_objhead  TYPE soli_tab,
        wa_doc_chng TYPE sodocchgi1,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.

  vl_form = 'ZLES0004'.

  BREAK peleite.
*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  Impresora
  ls_control-no_dialog = 'X'. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.
  ls_options-tdnoprev = 'X'.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = 'X'.

  CLEAR:job_output_info.
  CALL FUNCTION vl_name
    EXPORTING
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
      i_docnum           = doc_numero
    IMPORTING
      job_output_info    = job_output_info
*    TABLES
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.

    IF imprimir EQ abap_true.

      i_otf[] = job_output_info-otfdata[].

      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
          max_linewidth         = 132
        IMPORTING
          bin_filesize          = v_bin_filesize
        TABLES
          otf                   = i_otf
          lines                 = i_tline
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          OTHERS                = 4.

      IF sy-subrc EQ 0.
        CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
          EXPORTING
            i_otf                    = i_otf[]
          EXCEPTIONS
            convert_otf_to_pdf_error = 1
            cntl_error               = 2
            OTHERS                   = 3.
      ENDIF.
    ELSE.

      i_otf[] = job_output_info-otfdata[].

      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
          max_linewidth         = 132
        IMPORTING
          bin_filesize          = v_bin_filesize
          bin_file              = lc_declaracao
        TABLES
          otf                   = i_otf
          lines                 = i_tline
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          OTHERS                = 4.

    ENDIF.

  ENDIF.
ENDFORM.                    " F_IMPRIME_SMART

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'Z001'.

  LOOP AT SCREEN.
    IF rb_email IS NOT INITIAL.
      IF screen-name EQ 'P_EMAIL'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.

    ELSE.
      IF screen-name EQ 'P_EMAIL'.
        screen-input = 0.
        MODIFY SCREEN.
        CLEAR: p_email.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE ok-code.
    WHEN 'CANCEL'
      OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      IF rb_email IS  INITIAL.
        CLEAR p_email.
      ENDIF.
      PERFORM:
         f_seleciona_dados USING t_doc_numero t_tipo t_hbkid, " Form seleciona dados
*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
***         f_processa_dados USING t_tipo t_hbkid.
         f_processa_dados USING t_tipo t_hbkid wl_instrucoes.
*Fim Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
      LOOP AT it_saida INTO wa_saida.
        PERFORM f_imprime_smart USING wa_saida p_email t_doc_numero t_hbkid.
      ENDLOOP.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_SHDB_FB02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM executa_shdb_fb02 USING vbukrs vbelnr vgjahr v_xref2 vxref3 vtipo v_hbkid.
  DATA: wl_setleaf TYPE setleaf,
        i_head     TYPE tbtcjob.

  DATA:   wl_job_id   LIKE tbtcjob-jobcount.
  DATA:   wl_jobn(32).

  DATA: BEGIN OF i_steplist OCCURS 10.
          INCLUDE STRUCTURE tbtcstep.
  DATA: END OF i_steplist.
  DATA : c_no(1) TYPE c . "value 'N', " Criação do job

  DATA: wl_tbtcjob  TYPE  tbtcjob,
        wl_tbtcstrt TYPE  tbtcstrt.

  DATA: lv_repname LIKE  rsvar-report.           " for variant handling
  DATA: iv_varname LIKE  raldb-variant VALUE 'SAP_UPGRADE'.
  DATA: iv_varianttext  LIKE  varit-vtext VALUE 'Upgrade variant'.
  DATA: wl_subrc TYPE sy-subrc.
  DATA: tt_reportparam TYPE TABLE OF  rsparams WITH HEADER LINE.

  SELECT SINGLE *
   FROM setleaf
   INTO wl_setleaf
    WHERE setname EQ 'MAGGI_JOB_USER'.

  IF sy-subrc NE 0.
    MESSAGE 'ERRO AO EXECUTAR JOB, CONTACTE A T.I.' TYPE 'E'.
    EXIT.
  ENDIF.
  CONCATENATE 'Z_SHDB_FB02' sy-tcode  INTO wl_jobn SEPARATED BY '|'.

  i_head-jobname = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
  i_head-sdlstrttm = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
  i_head-stepcount = 1.

  tt_reportparam-selname = 'P_BUKRS'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = vbukrs.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  tt_reportparam-selname = 'P_BELNR'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = vbelnr.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  tt_reportparam-selname = 'P_GJAHR'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = vgjahr.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  tt_reportparam-selname = 'P_XREF2'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = v_xref2.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  tt_reportparam-selname = 'P_XREF3'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = vxref3.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  tt_reportparam-selname = 'P_TIPO'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = vtipo.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

*** BUG - 83955 - CBRAND - Inicio
  IF v_hbkid IS INITIAL.
    v_hbkid = 'BBRA'.
  ENDIF.

  tt_reportparam-selname = 'P_HBKID'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = v_hbkid.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.
*** BUG - 83955 - CBRAND - Fim


  lv_repname = 'Z_SHDB_FB02'.
*    Write the variant first (Insert or Update)
  CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
    EXPORTING
      iv_reportname         = lv_repname
      iv_variantname        = iv_varname
      iv_varianttext        = iv_varianttext
    IMPORTING
      ev_funcrc             = wl_subrc
    TABLES
      tt_reportparam        = tt_reportparam
    EXCEPTIONS
      exist_check_failed    = 1
      update_failed         = 2
      update_not_authorized = 3
      update_no_report      = 4
      update_no_variant     = 5
      update_variant_locked = 6
      insert_failed         = 7
      insert_not_authorized = 8
      insert_no_report      = 9
      insert_variant_exists = 10
      insert_variant_locked = 11
      OTHERS                = 12.

  i_steplist-parameter = iv_varname. " Nome da variante
  i_steplist-program = 'Z_SHDB_FB02'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
  i_steplist-typ = 'A'. " Tipo de Job
  i_steplist-authcknam = wl_setleaf-valfrom.
  i_steplist-language = sy-langu.
  i_steplist-arcuser = wl_setleaf-valfrom.

  APPEND i_steplist.


  c_no = 'N'.
  CALL FUNCTION 'BP_JOB_CREATE'
    EXPORTING
      job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
      job_cr_head_inp     = i_head " os valores atribuidos
    IMPORTING
      job_cr_head_out     = wl_tbtcjob
      job_cr_stdt_out     = wl_tbtcstrt
    TABLES
      job_cr_steplist     = i_steplist
    EXCEPTIONS
      cant_create_job     = 1
      invalid_dialog_type = 2
      invalid_job_data    = 3
      job_create_canceled = 4
      OTHERS              = 5.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobname   = wl_jobn
      jobcount  = wl_tbtcjob-jobcount
      strtimmed = 'X'.

ENDFORM.                    " EXECUTA_SHDB_FB02

*&---------------------------------------------------------------------*
*&      Form  FM_FI_DOCUMENT_CHANGE
*&---------------------------------------------------------------------*
*       Busca documento Contábil e salva XREF3
*----------------------------------------------------------------------*
FORM fm_fi_document_change USING p_bukrs p_belnr p_gjahr p_xref2 p_xref3 p_tipo p_hbkid. .

  DATA: t_bseg   TYPE TABLE OF bseg,
        t_accchg TYPE TABLE OF accchg,
        v_buzei	 TYPE	 bseg-buzei,
        v_bukrs  TYPE  accit-bukrs,
        v_belnr  TYPE  bseg-belnr,
        v_gjahr  TYPE  bseg-gjahr.

  v_bukrs = p_bukrs.
  v_belnr = p_belnr.
  v_gjahr = p_gjahr.

  CALL FUNCTION 'FI_DOCUMENT_READ'
    EXPORTING
      i_bukrs     = v_bukrs
      i_belnr     = v_belnr
      i_gjahr     = v_gjahr
    TABLES
      t_bseg      = t_bseg
    EXCEPTIONS
      wrong_input = 1
      not_found   = 2
      OTHERS      = 3.

  READ TABLE t_bseg INTO DATA(wl_bseg) WITH KEY bschl = '01'.
  IF sy-subrc NE 0.
    READ TABLE t_bseg INTO wl_bseg WITH KEY bschl = '09'.
  ENDIF.
  IF sy-subrc EQ 0.
    APPEND INITIAL LINE TO t_accchg ASSIGNING FIELD-SYMBOL(<fs_accchg>).
    <fs_accchg>-fdname = 'XREF2'.
    <fs_accchg>-newval = p_xref2.

    APPEND INITIAL LINE TO t_accchg ASSIGNING <fs_accchg>.
    <fs_accchg>-fdname = 'XREF3'.
    <fs_accchg>-newval = p_xref3.

*    APPEND INITIAL LINE TO t_accchg ASSIGNING <fs_accchg>.
*    <fs_accchg>-fdname = 'HBKID'.
*    <fs_accchg>-newval = p_hbkid.

    v_buzei = wl_bseg-buzei.

    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_buzei              = v_buzei
        i_bukrs              = v_bukrs
        i_belnr              = v_belnr
        i_gjahr              = v_gjahr
      TABLES
        t_accchg             = t_accchg
      EXCEPTIONS
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        OTHERS               = 6.

    IF sy-subrc NE 0.
      p_erro = 'X'.
    ENDIF.
    CLEAR: t_accchg[],t_bseg[],v_bukrs,v_belnr,v_gjahr,v_buzei.
  ENDIF.
ENDFORM.
