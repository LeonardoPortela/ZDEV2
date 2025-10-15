*&---------------------------------------------------------------------*
*& Report  ZFIR0077
*&---------------------------------------------------------------------*
*& Programa:    Informativo de pagamentos a fornecedores
*& Empresa:     ALZ                                                     "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
*& Autor:       Jean Antunes
*& Data:        21.05.2018
*&Solicitante:  Adriano Rossoni
*&---------------------------------------------------------------------*

REPORT zfir0077_alz.

TABLES: bsak, lfa1, t001, adr6.

*&---------------------------------------------------------------------*
*& TABELAS INTERNAS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
         empresa(50)        TYPE c,
         fornecedor(50)     TYPE c,
         lifnr              TYPE bsak-lifnr,
         divisao            TYPE t001w-werks,
         referencia         TYPE bsak-xblnr,
         doc_compensa       TYPE bsak-augbl,
         belnr              TYPE bsik-belnr,
         vencimento         TYPE bsak-zfbdt,
         moeda              TYPE bsak-waers,
         montante           TYPE bsak-dmbtr,
         receiver_email(50) TYPE c,
         uf_filial          TYPE lfa1-regio,
         blart              TYPE blart,
       END OF ty_saida.

DATA: tg_saida       TYPE TABLE OF ty_saida,
      tg_saida_aux   TYPE TABLE OF ty_saida,
      tg_envia       TYPE TABLE OF ty_saida,
      tg_envia_aux   TYPE TABLE OF ty_saida,
      tg_faturas     TYPE TABLE OF ty_saida,
      tg_faturas_aux TYPE TABLE OF ty_saida,
      wg_saida       LIKE LINE OF tg_saida,
      wg_saida_aux   LIKE LINE OF tg_saida,
      wg_envia       LIKE LINE OF tg_envia,
      v_jobnm        TYPE btcjob,
      v_stepc        TYPE btcstepcnt.

*--------Data for sending e-mail:
DATA: it_packing_list TYPE TABLE OF sopcklsti1,
      it_header       TYPE TABLE OF solisti1,
      it_contents_txt TYPE TABLE OF solisti1,
      it_contents_bin TYPE TABLE OF solisti1,
      it_receivers    TYPE TABLE OF somlreci1.

DATA: wa_packing_list TYPE sopcklsti1,
      wa_contents_txt TYPE solisti1,
      wa_receivers    TYPE somlreci1.

DEFINE new_line.
  CLEAR  wa_contents_txt.
         wa_contents_txt = &1.
  APPEND wa_contents_txt TO it_contents_txt.
END-OF-DEFINITION.

DEFINE add_receiver.
  CLEAR  wa_receivers.
         wa_receivers-receiver   = &1.
         wa_receivers-rec_type   = 'U'.
         wa_receivers-blind_copy = &2.
  APPEND wa_receivers TO it_receivers[].
END-OF-DEFINITION.

START-OF-SELECTION.
  PERFORM seleciona_dados.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados.

  DATA: r_forn  TYPE RANGE OF ty_saida-fornecedor WITH HEADER LINE,
        r_lifnr TYPE RANGE OF lfa1-lifnr WITH HEADER LINE,
        vl_trin TYPE sy-datum.



  DATA(vl_data)    = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum+0(4) }|.
  DATA(vl_ano)     = |{ sy-datum+0(4) }|.
  DATA(vl_assunto) = |Composição Pagamento -  { vl_data }|.
  DATA(vl_augdt)   = sy-datum.

  vl_trin    = sy-datum + 30.


  "#115227 SMC INICIO <<<
  IF sy-sysid = 'QAS'.
    CLEAR: vl_augdt, vl_data, vl_trin, vl_assunto, vl_ano.
    vl_augdt   = '20240401'."sy-datum.
    vl_data    = |{ vl_augdt+6(2) }.{ vl_augdt+4(2) }.{ vl_augdt+0(4) }|.
    vl_ano     = |{ vl_augdt+0(4) }|.
    vl_trin    = vl_augdt   + 30."sy-datum.
    vl_assunto = |TESTE QAS Composição Pagamento -  { vl_data }|.
  ENDIF.
  "#115227 SMC Fim <<<<

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      jobname         = v_jobnm
      stepcount       = v_stepc
    EXCEPTIONS
      no_runtime_info = 1
      OTHERS          = 2.

  IF ( v_jobnm = 'INF_PGTO_FORNEC_HR_ALZ' ).

    SELECT SINGLE jobname
        INTO @DATA(_count)
        FROM tbtco
        WHERE jobname = 'INF_PGTO_FORNEC_HR_ALZ'
          AND status in ('R', 'P'). "R ativo, P Escalonado

    CHECK ( sy-subrc = 0 ).

    SELECT lifnr, hora_atual
        FROM zfit0147
        INTO TABLE @DATA(t_zfit0147).

    DATA(_check_hora) = sy-uzeit+0(2).

    LOOP AT t_zfit0147[] INTO DATA(w_zfit0147).

      IF ( w_zfit0147-hora_atual+0(2) = _check_hora  ).

        r_lifnr-sign = 'I'.
        r_lifnr-option = 'EQ'.
        r_lifnr-low = w_zfit0147-lifnr.
        APPEND r_lifnr.

      ENDIF.

    ENDLOOP.

    DATA(v_zero) = |000000|.

    CHECK ( r_lifnr[] IS NOT INITIAL ).

    SELECT bs~bukrs, bs~lifnr, bs~gsber, bs~augdt, bs~dmbtr,
           bs~augbl, bs~gjahr, bs~belnr, bs~waers, bs~zfbdt,
           bs~xblnr, bs~blart, bk~belnr AS doc_bkpf,
           bk~stblg AS doc_estornado,
           t~regio

        FROM bsak AS bs INNER JOIN bkpf AS bk
        ON  bs~belnr = bk~belnr
        AND bs~bukrs = bk~bukrs
        INNER JOIN  t001w AS t  ON  t~werks = bs~gsber
    INTO TABLE @DATA(tg_bsak)
    WHERE bs~bukrs = '0035'
      "bs~bukrs BETWEEN '0001' AND '0050' 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
      AND bs~lifnr IN @r_lifnr
      AND bs~augdt EQ @vl_augdt
      AND bs~augbl EQ bs~belnr
      AND bs~gjahr EQ @vl_ano
      AND bs~blart IN ('ZP', 'KZ' ).

* Início - RJF
    SELECT bs~bukrs, bs~lifnr, bs~gsber, bs~augdt, bs~dmbtr,
           bs~augbl, bs~gjahr, bs~belnr, bs~waers, bs~zfbdt,
           bs~xblnr, bs~blart, bk~belnr AS doc_bkpf,
           bk~stblg AS doc_estornado,
           t~regio

        FROM bsik AS bs INNER JOIN bkpf AS bk
        ON  bs~belnr = bk~belnr
        AND bs~bukrs = bk~bukrs
        INNER JOIN  t001w AS t  ON  t~werks = bs~gsber
    INTO TABLE @DATA(tg_bsik)
    WHERE bs~bukrs = '0035'
      "bs~bukrs BETWEEN '0001' AND '0050' 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
      AND bs~lifnr IN @r_lifnr
     " AND bs~augdt EQ @vl_augdt "LP
     " AND bs~augbl EQ bs~belnr
      AND bs~gjahr EQ @vl_ano
      AND bs~zfbdt BETWEEN @vl_augdt AND @vl_trin
      AND bs~zlspr EQ ''
      AND bk~blart IN ('FT').
* Fim - RJF

  ELSEIF ( v_jobnm = 'INF_PGTO_FORNEC_DIA_ALZ' ).

    SELECT bs~bukrs bs~lifnr bs~gsber bs~augdt bs~dmbtr
           bs~augbl bs~gjahr bs~belnr bs~waers bs~zfbdt
           bs~xblnr bs~blart bk~belnr AS doc_bkpf
           bk~stblg AS doc_estornado
           t~regio

        FROM bsak AS bs INNER JOIN bkpf AS bk
        ON  bs~belnr = bk~belnr
        AND bs~bukrs = bk~bukrs
       INNER JOIN  t001w AS t  ON  t~werks = bs~gsber
    INTO TABLE tg_bsak
    WHERE bs~bukrs = '0035'
      "bs~bukrs BETWEEN '0001' AND '0050'
      AND bs~augdt EQ vl_augdt
      AND bs~augbl EQ bs~belnr
      AND bs~gjahr EQ vl_ano
      AND bs~blart IN ('ZP', 'KZ' ).

* Início - RJF
    SELECT bs~bukrs bs~lifnr bs~gsber bs~augdt bs~dmbtr
           bs~augbl bs~gjahr bs~belnr bs~waers bs~zfbdt
           bs~xblnr bs~blart bk~belnr AS doc_bkpf
           bk~stblg AS doc_estornado
           t~regio

        FROM bsik AS bs INNER JOIN bkpf AS bk
        ON  bs~belnr = bk~belnr
        AND bs~bukrs = bk~bukrs
       INNER JOIN  t001w AS t  ON  t~werks = bs~gsber
    INTO TABLE tg_bsik
    WHERE bs~bukrs = '0035'
     "bs~bukrs BETWEEN '0001' AND '0050'
     " AND bs~augdt EQ vl_augdt "LP
     " AND bs~augbl EQ bs~belnr
      AND bs~gjahr EQ vl_ano
      AND bs~zfbdt BETWEEN vl_augdt AND vl_trin
      AND bs~zlspr EQ ''
      AND bk~blart IN ('FT').
* Fim - RJF
  ENDIF.


  CHECK ( tg_bsak[] IS NOT INITIAL  OR tg_bsik IS NOT INITIAL ).

  SORT tg_bsak[] BY lifnr ASCENDING.
  SORT tg_bsik[] BY lifnr ASCENDING.

  DELETE tg_bsak[] WHERE doc_estornado IS NOT INITIAL.
  DELETE tg_bsik[] WHERE doc_estornado IS NOT INITIAL.
  "DELETE TG_BSAK[] WHERE BUKRS = '0035'.

  CHECK ( tg_bsak[] IS NOT INITIAL  OR tg_bsik IS NOT INITIAL ).

  SELECT
    'I' AS sign,
    'EQ' AS option,
    valfrom AS low,
    valfrom AS high
 INTO TABLE @DATA(r_tpdoc)
 FROM setleaf
 WHERE setname = 'ALZ_TPDOC_EMAIL' "'MAGGI_TPDOC_EMAIL' 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
 ORDER BY low ASCENDING.

  SELECT bukrs, lifnr, gsber, augdt, dmbtr,
         augbl, gjahr, belnr, waers, zfbdt, xblnr, blart
  FROM bsak
  INTO TABLE @DATA(tg_bsak_aux)
  FOR ALL ENTRIES IN @tg_bsak
  WHERE bukrs EQ @tg_bsak-bukrs
    AND augdt EQ @vl_augdt
    AND gjahr <= @vl_ano
    AND blart NOT IN ('ZP', 'KZ' )
    AND lifnr EQ @tg_bsak-lifnr
    AND augbl EQ @tg_bsak-augbl
    AND blart NOT IN @r_tpdoc.

* Início - RJF
  IF tg_bsik IS NOT INITIAL.
    SELECT bukrs, lifnr, gsber, augdt, dmbtr,
           augbl, gjahr, belnr, waers, zfbdt, xblnr, blart
    FROM bsik
    INTO TABLE @DATA(tg_bsik_aux)
    FOR ALL ENTRIES IN @tg_bsik
    WHERE bukrs EQ @tg_bsik-bukrs
     " AND augdt EQ @vl_augdt
      AND gjahr <= @vl_ano
      AND lifnr EQ @tg_bsik-lifnr
     " AND augbl EQ @tg_bsik-augbl.
      AND blart IN ('FT')          "LP
      AND zfbdt BETWEEN @vl_augdt AND @vl_trin" LP
      AND zlspr EQ ''.
* Fim - RJF

  ENDIF.
  CHECK ( tg_bsak_aux[] IS NOT INITIAL OR tg_bsik_aux IS NOT INITIAL   ).


  SELECT belnr,
         blart
  INTO TABLE
        @DATA(tg_bkpf)
  FROM
        bkpf
  FOR ALL ENTRIES IN
        @tg_bsak_aux
  WHERE
       bukrs EQ @tg_bsak_aux-bukrs
       AND  belnr EQ @tg_bsak_aux-augbl
       AND gjahr <= @tg_bsak_aux-gjahr
       AND blart NOT IN @r_tpdoc.

* Início - RJF
  IF tg_bsik_aux IS NOT INITIAL.
    SELECT belnr,
           blart
    INTO TABLE
          @DATA(tg_bkpf_aux)
    FROM
          bkpf
    FOR ALL ENTRIES IN
          @tg_bsik_aux
    WHERE
         bukrs EQ @tg_bsik_aux-bukrs
       "  AND  belnr EQ @tg_bsik_aux-augbl
      AND  belnr EQ @tg_bsik_aux-belnr
         AND gjahr <= @tg_bsik_aux-gjahr
         AND blart IN ('FT').
  ENDIF.

  IF tg_bsik IS NOT INITIAL.
    SELECT lf~lifnr,
           lf~name1,
           lf~adrnr,
           ad~smtp_addr
      FROM lfa1 AS lf
      INNER JOIN adr6 AS ad ON lf~adrnr = ad~addrnumber
      INTO TABLE @DATA(tg_lfa1_aux)
      FOR ALL ENTRIES IN @tg_bsik
      WHERE lifnr = @tg_bsik-lifnr.
    IF tg_lfa1_aux IS NOT INITIAL.
      SORT tg_lfa1_aux[] BY lifnr ASCENDING.
    ENDIF.

    SELECT bukrs, butxt
      FROM t001 INTO TABLE @DATA(tg_t001_aux)
      FOR ALL ENTRIES IN @tg_bsik
      WHERE bukrs EQ @tg_bsik-bukrs.

  ENDIF.
* Fim - RJF

  SELECT lf~lifnr,
         lf~name1,
         lf~adrnr,
         ad~smtp_addr
    FROM lfa1 AS lf
    INNER JOIN adr6 AS ad ON lf~adrnr = ad~addrnumber
    INTO TABLE @DATA(tg_lfa1)
    FOR ALL ENTRIES IN @tg_bsak
    WHERE lifnr = @tg_bsak-lifnr.

  CHECK ( tg_lfa1[] IS NOT INITIAL OR tg_t001_aux IS NOT INITIAL )  .

  SORT tg_lfa1[] BY lifnr ASCENDING.

  SELECT bukrs, butxt
    FROM t001 INTO TABLE @DATA(tg_t001)
    FOR ALL ENTRIES IN @tg_bsak
    WHERE bukrs EQ @tg_bsak-bukrs.

  LOOP AT tg_bsak INTO DATA(wl_bsak).

    LOOP AT tg_lfa1[] INTO DATA(wl_lfa1) WHERE lifnr = wl_bsak-lifnr.

      READ TABLE tg_bkpf INTO DATA(wl_bkpf) WITH KEY belnr = wl_bsak-belnr.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE tg_t001 INTO DATA(wl_t001) WITH KEY bukrs = wl_bsak-bukrs.

      wg_saida-empresa        = |{ wl_t001-bukrs } - { wl_t001-butxt }|.
      wg_saida-fornecedor     = |{ wl_lfa1-lifnr ALPHA = OUT } - { wl_lfa1-name1 }|.
      wg_saida-lifnr     = wl_bsak-lifnr.
      wg_saida-divisao        = wl_bsak-gsber.
      wg_saida-doc_compensa   = wl_bsak-augbl.
      wg_saida-vencimento     = wl_bsak-zfbdt.
      wg_saida-moeda          = wl_bsak-waers.
      wg_saida-montante       = wl_bsak-dmbtr.
      wg_saida-receiver_email = wl_lfa1-smtp_addr.
      wg_saida-uf_filial      = wl_bsak-regio.

      APPEND wg_saida TO tg_saida[].
      CLEAR: wl_lfa1, wl_t001, wg_saida.

    ENDLOOP.

  ENDLOOP.


* Início - RJF
  LOOP AT tg_bsik INTO DATA(wl_bsik).

    LOOP AT tg_lfa1_aux[] INTO DATA(wl_lfa1_aux) WHERE lifnr = wl_bsik-lifnr.

      READ TABLE tg_bkpf_aux INTO DATA(wl_bkpf_aux) WITH KEY belnr = wl_bsik-belnr.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      READ TABLE tg_t001_aux INTO DATA(wl_t001_aux) WITH KEY bukrs = wl_bsik-bukrs.

      wg_saida_aux-empresa        = |{ wl_t001_aux-bukrs } - { wl_t001_aux-butxt }|.
      wg_saida_aux-fornecedor     = |{ wl_lfa1_aux-lifnr ALPHA = OUT } - { wl_lfa1_aux-name1 }|.
      wg_saida_aux-lifnr     = wl_bsik-lifnr.
      wg_saida_aux-divisao        = wl_bsik-gsber.
      " wg_saida_aux-doc_compensa   = wl_bsik-augbl.
      wg_saida_aux-belnr   = wl_bsik-belnr. "LP
      wg_saida_aux-vencimento     = wl_bsik-zfbdt.
      wg_saida_aux-moeda          = wl_bsik-waers.
      wg_saida_aux-montante       = wl_bsik-dmbtr.
      wg_saida_aux-receiver_email = wl_lfa1_aux-smtp_addr.
      wg_saida_aux-uf_filial      = wl_bsik-regio.

      APPEND wg_saida_aux TO tg_saida_aux[].

      CLEAR: wl_lfa1_aux, wl_t001_aux, wg_saida_aux.

    ENDLOOP.

  ENDLOOP.

  SORT tg_saida_aux[] BY fornecedor doc_compensa.

* Fim - RJF


  SORT tg_saida[] BY fornecedor doc_compensa.

  "Criando lista de fornecedores para receber e-mail
  LOOP AT tg_saida[] INTO wg_saida.
    r_forn-sign = 'I'.
    r_forn-option = 'EQ'.
    r_forn-low = wg_saida-fornecedor.
    APPEND r_forn.
  ENDLOOP.

* Início - RJF
  "Criando lista de fornecedores para receber e-mail
  LOOP AT tg_saida_aux[] INTO wg_saida_aux.
    r_forn-sign = 'I'.
    r_forn-option = 'EQ'.
    r_forn-low = wg_saida_aux-fornecedor.
    APPEND r_forn.
  ENDLOOP.
* Fim - RJF

  DELETE ADJACENT DUPLICATES FROM r_forn[].

  LOOP AT r_forn[] INTO DATA(wl_forn).

    LOOP AT tg_saida[] INTO wg_saida WHERE ( fornecedor EQ wl_forn-low ) AND ( receiver_email IS NOT INITIAL ).
      APPEND wg_saida TO tg_envia[].
    ENDLOOP.

* Início - RJF
    LOOP AT tg_saida_aux[] INTO wg_saida_aux WHERE ( fornecedor EQ wl_forn-low ) AND ( receiver_email IS NOT INITIAL ).
      READ TABLE tg_envia INTO DATA(_wg_envia) WITH KEY lifnr = wg_saida_aux-lifnr receiver_email = wg_saida_aux-receiver_email . " divisao = wg_saida_aux-divisao  .

      IF sy-subrc NE 0.
        APPEND wg_saida_aux TO tg_envia[].
        CLEAR:_wg_envia.
      ENDIF.
    ENDLOOP.
    SORT tg_envia[] BY fornecedor lifnr divisao receiver_email .
    DELETE ADJACENT DUPLICATES FROM tg_envia[] COMPARING lifnr divisao doc_compensa receiver_email .

    "SORT tg_envia[] BY fornecedor.
    "fim lp
* Fim - RJF

    IF ( tg_envia[] IS NOT INITIAL ).

      LOOP AT tg_envia[] INTO wg_envia.

        LOOP AT tg_bsak_aux INTO DATA(wl_bsak_aux) WHERE augbl EQ wg_envia-doc_compensa.

          DATA(wl_faturas) = VALUE ty_saida( empresa        = wg_envia-empresa
                                             fornecedor     = wg_envia-fornecedor
                                             divisao        = wg_envia-divisao
                                             referencia     = wl_bsak_aux-xblnr
                                             doc_compensa   = wl_bsak_aux-belnr
                                             vencimento     = wl_bsak_aux-zfbdt
                                             moeda          = wl_bsak_aux-waers
                                             montante       = wl_bsak_aux-dmbtr ).

          APPEND wl_faturas TO tg_faturas[].

        ENDLOOP.

* Início - RJF

        "LOOP AT tg_bsik_aux INTO DATA(wl_bsik_aux) WHERE augbl EQ wg_envia-doc_compensa.
        LOOP AT tg_bsik_aux INTO DATA(wl_bsik_aux) WHERE lifnr EQ wg_envia-lifnr .

          DATA(wl_faturas_aux) = VALUE ty_saida( empresa        = wg_envia-empresa
                                             fornecedor     = wg_envia-fornecedor
                                            " divisao        = wg_envia-divisao
                                             divisao        = wl_bsik_aux-gsber
                                             referencia     = wl_bsik_aux-xblnr
                                             doc_compensa   = wl_bsik_aux-belnr
                                             vencimento     = wl_bsik_aux-zfbdt
                                             moeda          = wl_bsik_aux-waers
                                             montante       = wl_bsik_aux-dmbtr ).

          APPEND wl_faturas_aux TO tg_faturas_aux[].

        ENDLOOP.

        DELETE ADJACENT DUPLICATES FROM tg_faturas_aux[] COMPARING doc_compensa.

* Fim - RJF

        DELETE ADJACENT DUPLICATES FROM tg_faturas[] COMPARING doc_compensa.

        PERFORM: corpo_email USING wg_envia, send_email USING vl_assunto wg_envia.

        CLEAR: tg_faturas[].

* Início - RJF
        CLEAR: tg_faturas_aux[].
* Fim - RJF
      ENDLOOP.

    ENDIF.

    CLEAR: tg_envia[], tg_faturas[], wg_envia, wl_faturas, it_contents_txt[], it_receivers[].

* Início - RJF
    CLEAR: tg_faturas_aux[], wl_faturas_aux.
* Fim - RJF

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CORPO_EMAIL
*&---------------------------------------------------------------------*
FORM corpo_email USING wg_envia TYPE ty_saida.

  "CRIA TABELA EM HTML EM FORMATO DE E-MAIL.
  new_line'<!DOCTYPE HTML>'.
  new_line'<HTML>'.
  new_line'<BODY>'.
  new_line'<STYLE>'.
  new_line' TABLE, TH, TD { BORDER: 1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; }'.
  new_line' TH, TD { PADDING: 5PX; }'.
  new_line' TH { TEXT-ALIGN: LEFT; }'.
  new_line' TABLE#T01 TH { BACKGROUND-COLOR:#1c75b9; COLOR: WHITE; }'.
  new_line' TABLE#T02 TH { BACKGROUND-COLOR:#218bdb; COLOR: WHITE; }'.
  new_line'</STYLE>'.

  "new_line' <H4 ALIGN=LEFT>Prezado Fornecedor, Segue composição de pagamento.</H4>'.
  new_line' <H4 ALIGN=LEFT>Prezado Fornecedor, Segue composição de pagamento e programação de faturas futuras.</H4>'. "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA

  IF wg_envia-empresa(4) <> '0035' .
    new_line' <H4 ALIGN=LEFT>Qualquer dúvida, entrar em contato com contas.pagar@amaggi.com.br </H4>'.
  ELSE.
    new_line' <H4 ALIGN=LEFT>Qualquer dúvida, entrar em contato com fretes@alzgraos.com.br .</H4>'.

  ENDIF.


  new_line''.
  new_line' <TABLE BORDER="1" STYLE="WIDTH:100%" ID="T01">'.
  new_line'  <TR>'.
  new_line'   <TH><FONT SIZE="2">Empresa</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Fornecedor</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Filial</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">UF Filial</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">DocCompens</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">VencLíquid</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Moeda</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Valor</FONT></TH>'.
  new_line' </TR>'.

  DATA: vl_data_venc(15) TYPE c,
        vl_montante      TYPE c LENGTH 20.

  IF tg_faturas IS NOT INITIAL AND wg_envia-doc_compensa IS NOT INITIAL .
    "CONVERTE A DATA PARA FORMATO XX/XX/XXXX.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = wg_envia-vencimento
      IMPORTING
        output = vl_data_venc.
  ENDIF.
  "LP
  IF tg_faturas IS NOT INITIAL.
    WRITE wg_envia-montante TO vl_montante.
    CONDENSE vl_montante NO-GAPS.
  ENDIF.
  new_line' <TR>'.
  new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
  new_line      wg_envia-empresa.
  new_line'  </FONT></TD>'.

  new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
  new_line      wg_envia-fornecedor.
  new_line'  </FONT></TD>'.

  new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
  new_line      wg_envia-divisao.
  new_line'  </FONT></TD>'.

  new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
  new_line      wg_envia-uf_filial.
  new_line'  </FONT></TD>'.

  new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
  new_line      wg_envia-doc_compensa.
  new_line'   </FONT></TD>'.

  new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
  new_line      vl_data_venc.
  new_line'   </FONT></TD>'.

  new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
  new_line      wg_envia-moeda.
  new_line'   </FONT></TD>'.

  new_line'   <TD><FONT SIZE="2">'.
  new_line      vl_montante.
  new_line'   </FONT></TD>'.

  new_line ' </TR>'.

  CLEAR: vl_montante.

  new_line ' </TABLE>'.

  new_line' <H4 ALIGN=LEFT>Pagamento realizado referente à(s) fatura(s) abaixo:</H4>'.
  new_line''.
  new_line' <TABLE BORDER="1" STYLE="WIDTH:100%" ID="T02">'.
  new_line'  <TR>'.
  new_line'   <TH><FONT SIZE="2">Empresa</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Filial</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Fatura</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">DocContabil</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">VencLíquid</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Moeda</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Valor</FONT></TH>'.
  new_line' </TR>'.

  LOOP AT tg_faturas[] INTO DATA(wl_faturas).

    "CONVERTE A DATA PARA FORMATO XX/XX/XXXX.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = wl_faturas-vencimento IMPORTING output = vl_data_venc.

    WRITE wl_faturas-montante TO vl_montante.
    CONDENSE vl_montante NO-GAPS.

    new_line' <TR>'.
    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas-empresa.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas-divisao.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas-referencia.
    new_line'  </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas-doc_compensa.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      vl_data_venc.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas-moeda.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line      vl_montante.
    new_line'   </FONT></TD>'.

    new_line ' </TR>'.

    CLEAR: vl_montante.

  ENDLOOP.

  new_line ' </TABLE>'.

* Início - RJF
  new_line' <H4 ALIGN=LEFT>Previsão de Pagamentos Futuros:</H4>'.
  new_line''.
  new_line' <TABLE BORDER="1" STYLE="WIDTH:100%" ID="T02">'.
  new_line'  <TR>'.
  new_line'   <TH><FONT SIZE="2">Empresa</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Filial</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Fatura</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">DocContabil</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">VencLíquid</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Moeda</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Valor</FONT></TH>'.
  new_line' </TR>'.

  LOOP AT tg_faturas_aux[] INTO DATA(wl_faturas_aux).

    "CONVERTE A DATA PARA FORMATO XX/XX/XXXX.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = wl_faturas_aux-vencimento IMPORTING output = vl_data_venc.

    WRITE wl_faturas_aux-montante TO vl_montante.
    CONDENSE vl_montante NO-GAPS.

    new_line' <TR>'.
    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas_aux-empresa.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas_aux-divisao.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas_aux-referencia.
    new_line'  </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas_aux-doc_compensa.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      vl_data_venc.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_faturas_aux-moeda.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line      vl_montante.
    new_line'   </FONT></TD>'.

    new_line ' </TR>'.

    CLEAR: vl_montante.

  ENDLOOP.

  new_line ' </TABLE>'.
* Fim - RJF

  IF wg_envia-empresa(4) <> '0035' .
    new_line'  <H6>Atenciosamente,</H6><H6><b>AMAGGI - Corporativo</b></H6><H6>CSC - Financeiro</H6><H6>www.amaggi.com.br</H6> '.
  ELSE.
    new_line'  <H6>Atenciosamente,</H6><H6><b>ALZ Grãos</b></H6><H6>Palmas - Tocantins</H6>'.
  ENDIF.

  new_line '</BODY>'.
  new_line '</HTML>'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
FORM send_email USING p_vl_assunto wg_envia TYPE ty_saida.

  DATA: lv_lines    LIKE sy-tabix,
        wa_doc_data LIKE sodocchgi1.

  add_receiver wg_envia-receiver_email 'X'.
  lv_lines = lines( it_contents_txt ).

  wa_packing_list-transf_bin = space.
  wa_packing_list-head_start = 1.
  wa_packing_list-head_num   = 0.
  wa_packing_list-body_start = 1.
  wa_packing_list-body_num   = lv_lines.
  wa_packing_list-doc_type   = 'HTM'.
  APPEND wa_packing_list TO it_packing_list.

  READ TABLE it_contents_txt INTO wa_contents_txt INDEX lv_lines.

  wa_doc_data-obj_descr = p_vl_assunto.
  wa_doc_data-doc_size  = ( lv_lines - 1 ) * 255 + strlen( wa_contents_txt ).


  "#115227 SMC INICIO <<<
  IF sy-sysid = 'QAS'.
    FREE: it_receivers.
    APPEND VALUE #( receiver =   'samuel.cabana@amaggi.com.br' rec_type =   'U'  rec_date =   '00000000' ) TO it_receivers.
    APPEND VALUE #( receiver =   'pablo.alves@amaggi.com.br' rec_type =   'U'  rec_date =   '00000000' ) TO it_receivers.
*      APPEND VALUE #( receiver =   'larissa.pereira@amaggi.com.br' rec_type =   'U'  rec_date =   '00000000' ) TO it_receivers.
  ENDIF.
  "#115227 SMC FIM <<<

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = wa_doc_data
      put_in_outbox              = 'X'
      sender_address             = ''
      sender_address_type        = ''
      commit_work                = 'X'
    TABLES
      packing_list               = it_packing_list
      object_header              = it_header
      contents_bin               = it_contents_bin
      contents_txt               = it_contents_txt
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  CLEAR: wa_packing_list, wg_envia, wg_saida, it_contents_bin[], it_packing_list[], it_header[], it_contents_txt[], it_receivers[].

ENDFORM.
