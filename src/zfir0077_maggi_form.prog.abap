*&---------------------------------------------------------------------*
*& Include          ZFIR0077_MAGGI_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM seleciona_dados.
  FREE: tg_envia_salv.
  DATA r_forn   TYPE RANGE OF ty_saida-fornecedor WITH HEADER LINE.
  DATA r_lifnr  TYPE RANGE OF lfa1-lifnr WITH HEADER LINE.
  DATA vl_trin  TYPE sy-datum.
  DATA vl_data  TYPE c LENGTH 10.
  DATA vl_ano   TYPE gjahr.
  DATA vl_augdt TYPE zfit0091-dt_pgto.
  DATA cdfor    TYPE lifnr.
  DATA: cdcomp TYPE zfit0091-augbl.
  DATA vl_comp    TYPE sydatum.
  DATA vl_hoje    TYPE sydatum.

  CLEAR: vl_assunto,vl_comp,vl_horas,vl_hoje.

  vl_horas = sy-uzeit.
  vl_batch = sy-batch.
  vl_hoje = sy-datum.

  IF v_executa IS INITIAL AND vl_batch IS INITIAL.
    CLEAR: v_executa,vl_comp,lista,cdfor.
    IMPORT ex_executa = v_executa FROM MEMORY ID 'ZFIR0077_0'.
    FREE MEMORY ID 'ZFIR0077_0'.
    IMPORT ex_vl_augdt TO vl_comp FROM MEMORY ID 'ZFIR0077_2'.
    FREE MEMORY ID 'ZFIR0077_2'.
    IMPORT ex_lista = lista FROM MEMORY ID 'ZFIR0077_1'.
    FREE MEMORY ID 'ZFIR0077_1'.
    "IMPORT ex_vl_augdt TO vl_augdt FROM MEMORY ID 'ZFIR0077_2'.
    "FREE MEMORY ID 'ZFIR0077_2'.
    "IMPORT ex_vl_data TO vl_data FROM MEMORY ID 'ZFIR0077_3'.
    "FREE MEMORY ID 'ZFIR0077_3'.
    "IMPORT ex_vl_ano TO vl_ano FROM MEMORY ID 'ZFIR0077_4'.
    "FREE MEMORY ID 'ZFIR0077_4'.
    "IMPORT ex_vl_trin TO vl_trin FROM MEMORY ID 'ZFIR0077_5'.
    "FREE MEMORY ID 'ZFIR0077_5'.
    IMPORT ex_cdfor TO cdfor FROM MEMORY ID 'ZFIR0077_6'.
    FREE MEMORY ID 'ZFIR0077_6'.
    IMPORT ex_cdcomp TO cdcomp FROM MEMORY ID 'ZFIR0077_7'.
    FREE MEMORY ID 'ZFIR0077_7'.
    IF lista IS NOT INITIAL.
      SPLIT lista AT ';' INTO TABLE it_lista.
    ELSE.
      FREE:it_lista.
    ENDIF.
    p_manual = abap_true.
  ELSE.
    vl_comp = vl_hoje.
    CLEAR: p_manual.
    p_envia = abap_true.
  ENDIF.

  IF sy-sysid = 'QAS' AND vl_batch = abap_true.
    CLEAR: vl_comp,vl_horas.
    vl_comp = '20250404'.
    vl_horas = '080000'.
  ENDIF.


  vl_data  = |{ vl_comp+6(2) }.{ vl_comp+4(2) }.{ vl_comp+0(4) }|.
  vl_ano   = |{ vl_comp+0(4) }|.
  vl_augdt = vl_comp.

  vl_trin  = vl_comp + 30.

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      jobname         = v_executa
      stepcount       = v_stepc
    EXCEPTIONS
      no_runtime_info = 1
      OTHERS          = 2.


  IF v_executa = 'INF_PGTO_FORNEC_HR_MAGGI' AND vl_batch IS NOT INITIAL.

    SELECT SINGLE jobname
      INTO @DATA(_count)
      FROM tbtco
      WHERE jobname  = 'INF_PGTO_FORNEC_HR_MAGGI'
        AND status  IN ( 'R', 'P' ). " R ativo, P Escalonado

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT lifnr, hora_atual FROM zfit0147
      INTO TABLE @DATA(t_zfit0147).

    LOOP AT t_zfit0147[] INTO DATA(w_zfit0147).
      DATA(_check_hora) = vl_horas+0(2).
      IF w_zfit0147-hora_atual+0(2) = _check_hora.

        r_lifnr-sign   = 'I'.
        r_lifnr-option = 'EQ'.
        r_lifnr-low    = w_zfit0147-lifnr.
        APPEND r_lifnr.

      ENDIF.

    ENDLOOP.

    DATA(v_zero) = |000000|.

    IF r_lifnr[] IS INITIAL.
      RETURN.
    ENDIF.

    FREE: tg_bsik,tg_bsak.

    SELECT bs~bukrs
    ,bs~lifnr
    ,bs~gsber
    ,bs~augdt
    ,bs~dmbtr
    ,bs~wrbtr
    ,bs~augbl
    ,bs~gjahr
    ,bs~belnr
    ,bs~buzei
    ,bs~waers
    ,bs~zfbdt AS dtvenc
    ,bs~xblnr
    ,bs~blart
    ,bk~belnr AS doc_bkpf
    ,bk~stblg AS doc_estornado
    ,t~regio
    FROM bsak AS bs
    INNER JOIN
    bkpf AS bk ON  bs~belnr = bk~belnr
    AND bs~bukrs = bk~bukrs
    INNER JOIN
    t001w AS t ON t~werks = bs~gsber
          WHERE bs~bukrs BETWEEN '0001' AND '0050'
          AND bs~bukrs      <> '0035'
          AND bs~lifnr      IN @r_lifnr
          AND bs~augdt       = @vl_augdt
          AND bs~augbl       = bs~belnr
          AND bs~gjahr       = @vl_ano
          AND bs~blart      IN ( 'ZP', 'KZ' )
          AND bs~SHKZG       = 'S' "SMC 22-07-2025
*            INTO TABLE @tg_bsak. SMC - 22-07-2025

*SMC - 22-07-2025 >>>>>>>>
            INTO TABLE @DATA(prep_tg_bsak).
      " Ordena e remove duplicatas
      SORT prep_tg_bsak BY belnr ASCENDING augbl ASCENDING buzei ASCENDING.
      DELETE ADJACENT DUPLICATES FROM prep_tg_bsak.

      " Agrupa e soma os valores por documento
      DATA: lt_grouped LIKE prep_tg_bsak.
      DATA: wa_prep LIKE LINE OF prep_tg_bsak.
      DATA: wa_grouped LIKE LINE OF prep_tg_bsak.

      LOOP AT prep_tg_bsak INTO wa_prep.
        " Procura se já existe uma linha com mesma chave
        READ TABLE lt_grouped INTO wa_grouped
          WITH KEY belnr = wa_prep-belnr
                   augbl = wa_prep-augbl
                   gsber = wa_prep-gsber.
*                   buzei = wa_prep-buzei.

        IF sy-subrc <> 0.
          " Nova linha - adiciona à tabela
          APPEND wa_prep TO lt_grouped.
        ELSE.
          " Linha existente - soma os valores
          wa_grouped-dmbtr = wa_grouped-dmbtr + wa_prep-dmbtr.
          wa_grouped-wrbtr = wa_grouped-wrbtr + wa_prep-wrbtr.
          MODIFY lt_grouped FROM wa_grouped INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

     " Transfere dados agrupados para tg_bsak usando MOVE-CORRESPONDING
      DATA: wa_tg_bsak LIKE LINE OF tg_bsak.

      LOOP AT lt_grouped INTO wa_grouped.
        " Usa MOVE-CORRESPONDING para garantir que os campos sejam mapeados corretamente
        CLEAR wa_tg_bsak.
        MOVE-CORRESPONDING wa_grouped TO wa_tg_bsak.
        APPEND wa_tg_bsak TO tg_bsak.
      ENDLOOP.
*SMC - 22-07-2025 <<<<<<<<<<<<


    SELECT bs~bukrs
    ,bs~lifnr
    ,bs~gsber
    ,bs~augdt
    ,bs~dmbtr
    ,bs~wrbtr
    ,bs~augbl
    ,bs~gjahr
    ,bs~belnr
    ,bs~waers
    ,bseg~fdtag AS dtvenc
    ,bs~xblnr
    ,bs~blart
    ,bk~belnr AS doc_bkpf
    ,bk~stblg AS doc_estornado
    ,t~regio
    FROM bsik AS bs
    INNER JOIN
    bkpf AS bk ON  bs~belnr = bk~belnr
    AND bs~bukrs = bk~bukrs
      INNER JOIN bseg AS bseg ON bs~bukrs = bseg~bukrs AND bs~belnr = bseg~belnr
      "AND bs~gjahr = bseg~gjahr PSA IR216083
      AND bs~zfbdt = bseg~zfbdt "165663 - CS2023000355 - PSA
    INNER JOIN
    t001w AS t ON t~werks = bs~gsber
          WHERE bs~bukrs BETWEEN '0001' AND '0050'
          AND bs~bukrs      <> '0035'
          AND bs~lifnr      IN @r_lifnr
          "AND bs~gjahr       = @vl_ano PSA IR216083
          "AND bs~zfbdt BETWEEN @vl_augdt AND @vl_trin
      AND bseg~fdtag BETWEEN @vl_augdt AND @vl_trin "165663 - CS2023000355 - PSA
          AND bs~zlspr       = ''
          "AND bk~blart      IN ('FT')
            INTO TABLE @tg_bsik.

  ELSEIF v_executa = 'INF_PGTO_FORNEC_DIA_MAGGI' AND vl_batch IS NOT INITIAL.

    FREE: tg_bsik,tg_bsak.

    SELECT bs~bukrs
    ,bs~lifnr
    ,bs~gsber
    ,bs~augdt
    ,bs~dmbtr
    ,bs~wrbtr
    ,bs~augbl
    ,bs~gjahr
    ,bs~belnr
    ,bs~buzei
    ,bs~waers
    ,bs~zfbdt AS dtvenc
    ,bs~xblnr
    ,bs~blart
    ,bk~belnr AS doc_bkpf
    ,bk~stblg AS doc_estornado
    ,t~regio
    FROM bsak AS bs
    INNER JOIN
    bkpf AS bk ON  bs~belnr = bk~belnr
    AND bs~bukrs = bk~bukrs
    INNER JOIN
    t001w AS t ON t~werks = bs~gsber
    WHERE bs~bukrs BETWEEN '0001' AND '0050'
    AND bs~bukrs      <> '0035'
    AND bs~augdt       = @vl_augdt
    AND bs~augbl       = bs~belnr
    AND bs~gjahr       = @vl_ano
    AND bs~blart      IN ( 'ZP', 'KZ' )
    AND bs~SHKZG       = 'S' "SMC 22-07-2025
*        INTO TABLE @tg_bsak. SMC - 22-07-2025

*SMC - 22-07-2025 >>>>>>>>
            INTO TABLE @prep_tg_bsak.
      " Ordena e remove duplicatas
      SORT prep_tg_bsak BY belnr ASCENDING augbl ASCENDING buzei ASCENDING.
      DELETE ADJACENT DUPLICATES FROM prep_tg_bsak.

      " Agrupa e soma os valores por documento
*      DATA: lt_grouped LIKE prep_tg_bsak.
*      DATA: wa_prep LIKE LINE OF prep_tg_bsak.
*      DATA: wa_grouped LIKE LINE OF prep_tg_bsak.

      LOOP AT prep_tg_bsak INTO wa_prep.
        " Procura se já existe uma linha com mesma chave
        READ TABLE lt_grouped INTO wa_grouped
          WITH KEY belnr = wa_prep-belnr
                   augbl = wa_prep-augbl
                   gsber = wa_prep-gsber.
*                   buzei = wa_prep-buzei.

        IF sy-subrc <> 0.
          " Nova linha - adiciona à tabela
          APPEND wa_prep TO lt_grouped.
        ELSE.
          " Linha existente - soma os valores
          wa_grouped-dmbtr = wa_grouped-dmbtr + wa_prep-dmbtr.
          wa_grouped-wrbtr = wa_grouped-wrbtr + wa_prep-wrbtr.
          MODIFY lt_grouped FROM wa_grouped INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

     " Transfere dados agrupados para tg_bsak usando MOVE-CORRESPONDING
*      DATA: wa_tg_bsak LIKE LINE OF tg_bsak.

      LOOP AT lt_grouped INTO wa_grouped.
        " Usa MOVE-CORRESPONDING para garantir que os campos sejam mapeados corretamente
        CLEAR wa_tg_bsak.
        MOVE-CORRESPONDING wa_grouped TO wa_tg_bsak.
        APPEND wa_tg_bsak TO tg_bsak.
      ENDLOOP.
*SMC - 22-07-2025 <<<<<<<<<<<<


    SELECT bs~bukrs
    ,bs~lifnr
    ,bs~gsber
    ,bs~augdt
    ,bs~dmbtr
    ,bs~wrbtr
    ,bs~augbl
    ,bs~gjahr
    ,bs~belnr
    ,bs~waers
    ,bseg~fdtag AS dtvenc
    ,bs~xblnr
    ,bs~blart
    ,bk~belnr AS doc_bkpf
    ,bk~stblg AS doc_estornado
    ,t~regio
    FROM bsik AS bs
    INNER JOIN
    bkpf AS bk ON  bs~belnr = bk~belnr
    AND bs~bukrs = bk~bukrs
    INNER JOIN bseg AS bseg ON bs~bukrs = bseg~bukrs AND bs~belnr = bseg~belnr
      "AND bs~gjahr = bseg~gjahr PSA IR216083
      AND bs~zfbdt = bseg~zfbdt "165663 - CS2023000355 - PSA
    INNER JOIN
    t001w AS t ON t~werks = bs~gsber
    WHERE bs~bukrs BETWEEN '0001' AND '0050'
    AND bs~bukrs      <> '0035'
    "AND bs~gjahr       = @vl_ano PSA IR216083
    "AND bs~zfbdt BETWEEN @vl_augdt AND @vl_trin
      AND bseg~fdtag BETWEEN @vl_augdt AND @vl_trin "165663 - CS2023000355 - PSA
    AND bs~zlspr       = ''
    "AND bk~blart      IN ('FT')
      INTO TABLE @tg_bsik.

  ELSEIF v_executa = 'INF_PGTO_FORNEC_MANUAL_MAGGI' AND vl_batch IS INITIAL.

    FREE: tg_bsik,tg_bsak.


    IF cdfor IS NOT INITIAL.
      SELECT bs~bukrs
      ,bs~lifnr
      ,bs~gsber
      ,bs~augdt
      ,bs~dmbtr
      ,bs~wrbtr
      ,bs~augbl
      ,bs~gjahr
      ,bs~belnr
      ,bs~buzei
      ,bs~waers
      ,bs~zfbdt AS dtvenc
      ,bs~xblnr
      ,bs~blart
      ,bk~belnr AS doc_bkpf
      ,bk~stblg AS doc_estornado
      ,t~regio
      FROM bsak AS bs
      INNER JOIN
      bkpf AS bk ON  bs~belnr = bk~belnr
      AND bs~bukrs = bk~bukrs
      INNER JOIN
      t001w AS t ON t~werks = bs~gsber
      WHERE bs~bukrs BETWEEN '0001' AND '0050'
      AND bs~bukrs      <> '0035'
      AND bs~augdt       = @vl_augdt
      AND bs~augbl       = bs~belnr
      AND bs~gjahr       = @vl_ano
      AND bs~blart      IN ( 'ZP', 'KZ' )
        AND bs~SHKZG       = 'S' "SMC 22-07-2025
              AND bs~lifnr        = @cdfor
*        INTO TABLE @tg_bsak. " SMC - 22-07-2025

*SMC - 22-07-2025 >>>>>>>>
            INTO TABLE @prep_tg_bsak.
      " Ordena e remove duplicatas
      SORT prep_tg_bsak BY belnr ASCENDING augbl ASCENDING buzei ASCENDING.
      DELETE ADJACENT DUPLICATES FROM prep_tg_bsak.

      " Agrupa e soma os valores por documento
*      DATA: lt_grouped LIKE prep_tg_bsak.
*      DATA: wa_prep LIKE LINE OF prep_tg_bsak.
*      DATA: wa_grouped LIKE LINE OF prep_tg_bsak.

      LOOP AT prep_tg_bsak INTO wa_prep.
        " Procura se já existe uma linha com mesma chave
        READ TABLE lt_grouped INTO wa_grouped
          WITH KEY belnr = wa_prep-belnr
                   augbl = wa_prep-augbl
                   gsber = wa_prep-gsber.
*                   buzei = wa_prep-buzei.

        IF sy-subrc <> 0.
          " Nova linha - adiciona à tabela
          APPEND wa_prep TO lt_grouped.
        ELSE.
          " Linha existente - soma os valores
          wa_grouped-dmbtr = wa_grouped-dmbtr + wa_prep-dmbtr.
          wa_grouped-wrbtr = wa_grouped-wrbtr + wa_prep-wrbtr.
          MODIFY lt_grouped FROM wa_grouped INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

     " Transfere dados agrupados para tg_bsak usando MOVE-CORRESPONDING
*      DATA: wa_tg_bsak LIKE LINE OF tg_bsak.

      LOOP AT lt_grouped INTO wa_grouped.
        " Usa MOVE-CORRESPONDING para garantir que os campos sejam mapeados corretamente
        CLEAR wa_tg_bsak.
        MOVE-CORRESPONDING wa_grouped TO wa_tg_bsak.
        APPEND wa_tg_bsak TO tg_bsak.
      ENDLOOP.
*SMC - 22-07-2025 <<<<<<<<<<<<

      SELECT bs~bukrs
      ,bs~lifnr
      ,bs~gsber
      ,bs~augdt
      ,bs~dmbtr
      ,bs~wrbtr
      ,bs~augbl
      ,bs~gjahr
      ,bs~belnr
      ,bs~waers
      ,bseg~fdtag AS dtvenc
      ,bs~xblnr
      ,bs~blart
      ,bk~belnr AS doc_bkpf
      ,bk~stblg AS doc_estornado
      ,t~regio
      FROM bsik AS bs
      INNER JOIN
      bkpf AS bk ON  bs~belnr = bk~belnr
      AND bs~bukrs = bk~bukrs
      INNER JOIN bseg AS bseg ON bs~bukrs = bseg~bukrs AND bs~belnr = bseg~belnr
        "AND bs~gjahr = bseg~gjahr PSA IR216083
        AND bs~zfbdt = bseg~zfbdt "165663 - CS2023000355 - PSA
      INNER JOIN
      t001w AS t ON t~werks = bs~gsber
      WHERE bs~bukrs BETWEEN '0001' AND '0050'
      AND bs~bukrs        <> '0035'
      "AND bs~gjahr        = @vl_ano PSA IR216083
      "AND bs~zfbdt BETWEEN @vl_augdt AND @vl_trin
      AND bseg~fdtag BETWEEN @vl_augdt AND @vl_trin "165663 - CS2023000355 - PSA
      AND bs~zlspr        = ''
      "AND bk~blart      IN ('FT')
      AND bs~lifnr        = @cdfor
        INTO TABLE @tg_bsik.
    ELSE.
      SELECT bs~bukrs
      ,bs~lifnr
      ,bs~gsber
      ,bs~augdt
      ,bs~dmbtr
      ,bs~wrbtr
      ,bs~augbl
      ,bs~gjahr
      ,bs~belnr
      ,bs~buzei
      ,bs~waers
      ,bs~zfbdt AS dtvenc
      ,bs~xblnr
      ,bs~blart
      ,bk~belnr AS doc_bkpf
      ,bk~stblg AS doc_estornado
      ,t~regio
      FROM bsak AS bs
      INNER JOIN
      bkpf AS bk ON  bs~belnr = bk~belnr
      AND bs~bukrs = bk~bukrs
      INNER JOIN
      t001w AS t ON t~werks = bs~gsber
      WHERE bs~bukrs BETWEEN '0001' AND '0050'
      AND bs~bukrs      <> '0035'
      AND bs~augdt       = @vl_augdt
      AND bs~augbl       = bs~belnr
      AND bs~gjahr       = @vl_ano
      AND bs~blart      IN ( 'ZP', 'KZ' )
        AND bs~SHKZG       = 'S' "SMC 22-07-2025
*        INTO TABLE @tg_bsak. "SMC - 22-07-2025 >>>>>>>>

*SMC - 22-07-2025 >>>>>>>>
            INTO TABLE @prep_tg_bsak.
      " Ordena e remove duplicatas
      SORT prep_tg_bsak BY belnr ASCENDING augbl ASCENDING buzei ASCENDING.
      DELETE ADJACENT DUPLICATES FROM prep_tg_bsak.

      " Agrupa e soma os valores por documento
*      DATA: lt_grouped LIKE prep_tg_bsak.
*      DATA: wa_prep LIKE LINE OF prep_tg_bsak.
*      DATA: wa_grouped LIKE LINE OF prep_tg_bsak.

      LOOP AT prep_tg_bsak INTO wa_prep.
        " Procura se já existe uma linha com mesma chave
        READ TABLE lt_grouped INTO wa_grouped
          WITH KEY belnr = wa_prep-belnr
                   augbl = wa_prep-augbl
                   gsber = wa_prep-gsber.
*                   buzei = wa_prep-buzei.

        IF sy-subrc <> 0.
          " Nova linha - adiciona à tabela
          APPEND wa_prep TO lt_grouped.
        ELSE.
          " Linha existente - soma os valores
          wa_grouped-dmbtr = wa_grouped-dmbtr + wa_prep-dmbtr.
          wa_grouped-wrbtr = wa_grouped-wrbtr + wa_prep-wrbtr.
          MODIFY lt_grouped FROM wa_grouped INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

     " Transfere dados agrupados para tg_bsak usando MOVE-CORRESPONDING
*      DATA: wa_tg_bsak LIKE LINE OF tg_bsak.

      LOOP AT lt_grouped INTO wa_grouped.
        " Usa MOVE-CORRESPONDING para garantir que os campos sejam mapeados corretamente
        CLEAR wa_tg_bsak.
        MOVE-CORRESPONDING wa_grouped TO wa_tg_bsak.
        APPEND wa_tg_bsak TO tg_bsak.
      ENDLOOP.
*SMC - 22-07-2025 <<<<<<<<<<<<


      SELECT bs~bukrs
      ,bs~lifnr
      ,bs~gsber
      ,bs~augdt
      ,bs~dmbtr
      ,bs~wrbtr
      ,bs~augbl
      ,bs~gjahr
      ,bs~belnr
      ,bs~waers
      ,bseg~fdtag AS dtvenc
      ,bs~xblnr
      ,bs~blart
      ,bk~belnr AS doc_bkpf
      ,bk~stblg AS doc_estornado
      ,t~regio
      FROM bsik AS bs
      INNER JOIN
      bkpf AS bk ON  bs~belnr = bk~belnr
      AND bs~bukrs = bk~bukrs
      INNER JOIN bseg AS bseg ON bs~bukrs = bseg~bukrs AND bs~belnr = bseg~belnr
        "AND bs~gjahr = bseg~gjahr PSA IR216083
        AND bs~zfbdt = bseg~zfbdt "165663 - CS2023000355 - PSA
      INNER JOIN
      t001w AS t ON t~werks = bs~gsber
      WHERE bs~bukrs BETWEEN '0001' AND '0050'
      AND bs~bukrs      <> '0035'
      "AND bs~gjahr       = @vl_ano PSA IR216083
      "AND bs~zfbdt BETWEEN @vl_augdt AND @vl_trin
      AND bseg~fdtag BETWEEN @vl_augdt AND @vl_trin "165663 - CS2023000355 - PSA
      AND bs~zlspr       = ''
      "AND bk~blart      IN ('FT')
        INTO TABLE @tg_bsik.
    ENDIF.



    DATA(add_receivers_list) = abap_true.

  ELSE.
    MESSAGE 'Programa só pode ser executado via Job ou pela transação ZFI0153 !' TYPE 'I' DISPLAY LIKE 'I'.
    EXIT.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM tg_bsak[].

  DELETE tg_bsak[] WHERE doc_estornado IS NOT INITIAL.
  DELETE tg_bsik[] WHERE doc_estornado IS NOT INITIAL.

  DELETE ADJACENT DUPLICATES FROM tg_bsik[].

  IF cdcomp IS NOT INITIAL.
    DELETE tg_bsak[] WHERE augbl <> cdcomp.
    DELETE tg_bsik[] WHERE augbl <> cdcomp.
  ENDIF.

  IF tg_bsak[] IS INITIAL AND tg_bsik IS INITIAL.
    IF add_receivers_list = abap_true.
      MESSAGE 'Dados não foram encontrados!' TYPE 'I' DISPLAY LIKE 'I'.
      CLEAR add_receivers_list.
    ENDIF.
  ENDIF.

  IF tg_bsak[] IS INITIAL AND tg_bsik IS INITIAL.
    RETURN.
  ENDIF.

  SORT tg_bsak[] BY lifnr ASCENDING.
  SORT tg_bsik[] BY lifnr ASCENDING.

  IF tg_bsak[] IS INITIAL AND tg_bsik IS INITIAL.
    IF add_receivers_list = abap_true.
      MESSAGE 'Dados não foram encontrados!' TYPE 'I' DISPLAY LIKE 'I'.
      CLEAR add_receivers_list.
    ENDIF.
  ENDIF.

  IF tg_bsak[] IS INITIAL AND tg_bsik IS INITIAL.
    RETURN.
  ENDIF.



  SELECT 'I'     AS sign,
         'EQ'    AS option,
         valfrom AS low,
         valfrom AS high
    INTO TABLE @DATA(r_tpdoc)
    FROM setleaf
    WHERE setname = 'MAGGI_TPDOC_EMAIL'
    ORDER BY low ASCENDING.
  IF tg_bsak IS NOT INITIAL.
    CLEAR tg_bsak_aux.
    SELECT * FROM bsak
      INTO TABLE @tg_bsak_aux
      FOR ALL ENTRIES IN @tg_bsak[]
      WHERE bukrs      = @tg_bsak-bukrs
        AND gsber      = @tg_bsak-gsber
        AND augdt      = @tg_bsak-augdt
        "AND gjahr      = @tg_bsak-gjahr
        "AND blart NOT IN ( 'ZP', 'KZ' ) "SMC - 22-07-2025 >>>>>>>>>>>
        AND lifnr      = @tg_bsak-lifnr
        AND augbl      = @tg_bsak-augbl
        AND blart NOT IN @r_tpdoc.
    SORT tg_bsak_aux[].
    DELETE ADJACENT DUPLICATES FROM tg_bsak_aux[].

*SMC - 22-07-2025 >>>>>>>>>>>
 " Remove registros com BLART 'ZP' ou 'KZ' apenas quando AUGBL = BELNR
   LOOP AT tg_bsak_aux ASSIGNING FIELD-SYMBOL(<fs_bsak_aux>).
      IF ( <fs_bsak_aux>-blart = 'ZP' OR <fs_bsak_aux>-blart = 'KZ' )
         AND <fs_bsak_aux>-augbl = <fs_bsak_aux>-belnr.
        DELETE tg_bsak_aux INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
*SMC - 22-07-2025 <<<<<<<<<<<<

    TYPES: BEGIN OF ty_remove_bsak,
             bukrs TYPE bsak-bukrs,
             gsber TYPE bsak-gsber,
             augdt TYPE bsak-augdt,
             lifnr TYPE bsak-lifnr,
             augbl TYPE bsak-augbl,
           END OF ty_remove_bsak.

    DATA: it_remove_bsak TYPE STANDARD TABLE OF ty_remove_bsak,
          ls_remove_bsak TYPE ty_remove_bsak.

    LOOP AT tg_bsak ASSIGNING FIELD-SYMBOL(<fs_bsak>).
      DATA(_index) = sy-index + 1.
      READ TABLE tg_bsak_aux WITH KEY bukrs = <fs_bsak>-bukrs
                                      gsber = <fs_bsak>-gsber
                                      augdt = <fs_bsak>-augdt
                                      lifnr = <fs_bsak>-lifnr
                                      augbl = <fs_bsak>-augbl
                                      TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_remove_bsak-bukrs = <fs_bsak>-bukrs.
        ls_remove_bsak-gsber = <fs_bsak>-gsber.
        ls_remove_bsak-augdt = <fs_bsak>-augdt.
        ls_remove_bsak-lifnr = <fs_bsak>-lifnr.
        ls_remove_bsak-augbl = <fs_bsak>-augbl.
        APPEND ls_remove_bsak TO it_remove_bsak.
        CLEAR: ls_remove_bsak.
      ENDIF.
    ENDLOOP.

    IF it_remove_bsak IS NOT INITIAL.

      SORT it_remove_bsak.
      DELETE ADJACENT DUPLICATES FROM it_remove_bsak.

      LOOP AT it_remove_bsak ASSIGNING FIELD-SYMBOL(<_del_bsak>).
        DELETE tg_bsak WHERE bukrs = <_del_bsak>-bukrs AND
        gsber = <_del_bsak>-gsber AND
        augdt = <_del_bsak>-augdt AND
        lifnr = <_del_bsak>-lifnr AND
        augbl = <_del_bsak>-augbl.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF tg_bsik IS NOT INITIAL.
    "165663 - CS2023000355 - PSA Pegar a data base + das BSEG-ZBD1T
    FREE: tg_bsik_aux.
    CLEAR: tg_bsik_aux.

    SELECT a~* FROM bsik AS a
      INNER JOIN bseg AS b ON a~bukrs = b~bukrs AND a~belnr = b~belnr
      "AND a~gjahr = b~gjahr PSA IR216083
      AND a~zfbdt = b~zfbdt "165663 - CS2023000355 - PSA
      INTO TABLE @tg_bsik_aux
      FOR ALL ENTRIES IN @tg_bsik[]
      WHERE a~bukrs       = @tg_bsik-bukrs
        "AND a~gjahr      <= @vl_ano PSA IR216083
        AND a~lifnr       = @tg_bsik-lifnr
        "AND blart      IN ('FT')          " LP
        "AND a~zfbdt BETWEEN @vl_augdt AND @vl_trin " LP "165663 - CS2023000355 - PSA
      AND b~fdtag BETWEEN @vl_augdt AND @vl_trin " LP
        AND a~zlspr       = ''.

    SORT tg_bsik_aux[].
    DELETE ADJACENT DUPLICATES FROM tg_bsik_aux[].

  ENDIF.

  IF tg_bsak_aux[] IS INITIAL AND tg_bsik_aux[] IS INITIAL.
    RETURN.
  ENDIF.

  IF tg_bsak_aux IS NOT INITIAL.
    SELECT belnr,
       blart
  INTO TABLE
        @DATA(tg_bkpf)
  FROM bkpf
  FOR ALL ENTRIES IN
        @tg_bsak_aux
  WHERE bukrs      = @tg_bsak_aux-bukrs
    AND belnr      = @tg_bsak_aux-augbl
    AND gjahr     <= @tg_bsak_aux-gjahr
    AND blart NOT IN @r_tpdoc.
  ENDIF.

  IF tg_bsik_aux IS NOT INITIAL.
    SELECT belnr,
           blart
      INTO TABLE
            @DATA(tg_bkpf_aux)
      FROM bkpf
      FOR ALL ENTRIES IN
            @tg_bsik_aux
      WHERE bukrs  = @tg_bsik_aux-bukrs
        AND belnr  = @tg_bsik_aux-belnr
        AND gjahr <= @tg_bsik_aux-gjahr.
    "AND blart IN ('FT').
  ENDIF.

  IF tg_bsik IS NOT INITIAL.
    SELECT lf~lifnr,
           lf~name1,
           lf~adrnr,
           ad~smtp_addr
      FROM lfa1 AS lf
             INNER JOIN
               adr6 AS ad ON lf~adrnr = ad~addrnumber
      INTO TABLE @DATA(tg_lfa1_aux)
      FOR ALL ENTRIES IN @tg_bsik
      WHERE lifnr = @tg_bsik-lifnr.

    IF tg_lfa1_aux IS NOT INITIAL.
      SORT tg_lfa1_aux[] BY lifnr ASCENDING.
    ENDIF.

    SELECT bukrs, butxt FROM t001
      INTO TABLE @DATA(tg_t001_aux)
      FOR ALL ENTRIES IN @tg_bsik
      WHERE bukrs = @tg_bsik-bukrs.

  ENDIF.

  CLEAR tg_lfa1.

  "Se não preencher a tg_lfa1 é porque esta com erro no cadastro da adr6 com lfa1
  SELECT lf~lifnr,
         lf~name1,
         lf~adrnr,
         ad~smtp_addr
    FROM lfa1 AS lf
           INNER JOIN
             adr6 AS ad ON lf~adrnr = ad~addrnumber
    INTO TABLE @tg_lfa1
    FOR ALL ENTRIES IN @tg_bsak
    WHERE lifnr = @tg_bsak-lifnr.

  IF tg_lfa1[] IS INITIAL AND tg_t001_aux IS INITIAL.
    RETURN.
  ENDIF.

  SORT tg_lfa1[] BY lifnr ASCENDING.

  SELECT bukrs, butxt FROM t001
    INTO TABLE @DATA(tg_t001)
    FOR ALL ENTRIES IN @tg_bsak
    WHERE bukrs = @tg_bsak-bukrs.

  LOOP AT tg_bsak INTO DATA(wl_bsak).

    LOOP AT tg_lfa1[] INTO DATA(wl_lfa1_bsak) WHERE lifnr = wl_bsak-lifnr.

      READ TABLE tg_t001 INTO DATA(wl_t001_bsak) WITH KEY bukrs = wl_bsak-bukrs.
      DATA: _new_lifnr_bsak TYPE lifnr.
      CLEAR: _new_lifnr_bsak.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wl_lfa1_bsak-lifnr
        IMPORTING
          output = _new_lifnr_bsak.
      wg_saida_bsak-compensacao   = 'Fechado'.
      wg_saida_bsak-bukrs         = wl_bsak-bukrs.
      wg_saida_bsak-empresa       = |{ wl_t001_bsak-bukrs } - { wl_t001_bsak-butxt }|.
      wg_saida_bsak-fornecedor    = |{ _new_lifnr_bsak } - { wl_lfa1_bsak-name1 }|.
      wg_saida_bsak-lifnr         = wl_bsak-lifnr.
      wg_saida_bsak-nmfor         = wl_lfa1_bsak-name1.
      wg_saida_bsak-divisao       = wl_bsak-gsber.
      wg_saida_bsak-doc_compensa  = wl_bsak-augbl.
      wg_saida_bsak-vencimento    = wl_bsak-dtvenc."zfbdt. 165663 - CS2023000355 - PSA
      wg_saida_bsak-moeda         = wl_bsak-waers.
      wg_saida_bsak-montante      = wl_bsak-dmbtr.

      IF wl_bsak-wrbtr IS NOT INITIAL AND wl_bsak-wrbtr <> 0.
        wg_saida_bsak-vlrdoc = wl_bsak-wrbtr.
        wg_saida_bsak-ptax   = wl_bsak-dmbtr / wl_bsak-wrbtr.
      ELSE.
        wg_saida_bsak-vlrdoc = 0.
        wg_saida_bsak-ptax   = 0.
      ENDIF.

      wg_saida_bsak-receiver_email = wl_lfa1_bsak-smtp_addr.
      wg_saida_bsak-uf_filial      = wl_bsak-regio.

      SELECT SINGLE blart FROM bsak AS bs
        WHERE bs~augdt      = @wg_saida_bsak-vencimento
          AND bs~augbl      = @wg_saida_bsak-doc_compensa
          AND bs~gjahr      = @wg_saida_bsak-vencimento+0(4)
          AND bs~lifnr      = @wl_lfa1_bsak-lifnr
          AND bs~blart NOT IN ( 'ZP', 'KZ' )
        INTO @wg_saida_bsak-blart.
      IF wg_saida_bsak-blart IN lr_frete.
        wg_saida_bsak-tipo = |Frete|.
      ENDIF.

      IF wg_saida_bsak-blart NOT IN lr_frete.
        wg_saida_bsak-tipo = |Normal|.
      ENDIF.
      APPEND wg_saida_bsak TO tg_saida_bsak[].
      CLEAR: wl_lfa1_bsak,
             wl_t001_bsak,
             wl_bsak.

    ENDLOOP.

  ENDLOOP.

  LOOP AT tg_bsik INTO DATA(wl_bsik).

    LOOP AT tg_lfa1_aux[] INTO DATA(wl_lfa1_bsik) WHERE lifnr = wl_bsik-lifnr.

      READ TABLE tg_t001_aux INTO DATA(wl_t001_bsik) WITH KEY bukrs = wl_bsik-bukrs.
      DATA: _new_lifnr_bsik TYPE lifnr.
      CLEAR: _new_lifnr_bsik.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wl_lfa1_bsik-lifnr
        IMPORTING
          output = _new_lifnr_bsik.

      wg_saida_bsik-compensacao   = 'Aberto'.
      wg_saida_bsik-bukrs         = wl_bsik-bukrs.
      wg_saida_bsik-empresa       = |{ wl_t001_bsik-bukrs } - { wl_t001_bsik-butxt }|.
      wg_saida_bsik-fornecedor    = |{ _new_lifnr_bsik } - { wl_lfa1_bsik-name1 }|.
      wg_saida_bsik-lifnr         = wl_bsik-lifnr.
      wg_saida_bsik-nmfor         = wl_lfa1_bsik-name1.
      wg_saida_bsik-divisao       = wl_bsik-gsber.
      wg_saida_bsik-doc_compensa  = wl_bsik-augbl.
      wg_saida_bsik-belnr         = wl_bsik-belnr.
      wg_saida_bsik-vencimento    = wl_bsik-dtvenc."zfbdt. "165663 - CS2023000355 - PSA
      wg_saida_bsik-moeda         = wl_bsik-waers.
      wg_saida_bsik-montante      = wl_bsik-dmbtr.

      IF wl_bsik-wrbtr IS NOT INITIAL AND wl_bsik-wrbtr <> 0.
        wg_saida_bsik-vlrdoc = wl_bsik-wrbtr.
        wg_saida_bsik-ptax   = wl_bsik-dmbtr / wl_bsik-wrbtr.
      ELSE.
        wg_saida_bsik-vlrdoc = 0.
        wg_saida_bsik-ptax   = 0.
      ENDIF.

      wg_saida_bsik-receiver_email = wl_lfa1_bsik-smtp_addr.
      wg_saida_bsik-uf_filial      = wl_bsik-regio.

      SELECT SINGLE blart FROM bsik AS bs
        WHERE bs~augdt      = @wg_saida_bsik-vencimento
          AND bs~augbl      = @wg_saida_bsik-doc_compensa
          "AND bs~gjahr      = @wg_saida_bsik-vencimento+0(4) PSA IR216083
          AND bs~lifnr      = @wl_lfa1_bsik-lifnr
          AND bs~blart NOT IN ( 'ZP', 'KZ' )
        INTO @wg_saida_bsik-blart.
      IF wg_saida_bsik-blart IN lr_frete.
        wg_saida_bsik-tipo = |Frete|.
      ENDIF.

      IF wg_saida_bsik-blart NOT IN lr_frete.
        wg_saida_bsik-tipo = |Normal|.
      ENDIF.
      APPEND wg_saida_bsik TO tg_saida_bsik[].

      CLEAR: wl_lfa1_bsik,
             wl_t001_bsik,
             wg_saida_bsik,
             wl_bsik.

    ENDLOOP.

  ENDLOOP.

  SORT tg_saida_bsik[] BY bukrs lifnr doc_compensa compensacao.
  SORT tg_saida_bsak[] BY bukrs lifnr doc_compensa compensacao.


**********************************************************************
***Move os dados da BASK E BSIK PARA ENVIO E DELETA OS DUPLICADO - INICIO
  FREE: tg_envia.

  APPEND LINES OF tg_saida_bsak[] TO tg_envia[].
  APPEND LINES OF tg_saida_bsik[] TO tg_envia[].

  DELETE tg_envia[] WHERE doc_compensa = abap_false.

  "DELETE ADJACENT DUPLICATES FROM tg_envia[] COMPARING bukrs lifnr doc_compensa vencimento moeda.
***Move os dados da BASK E BSIK PARA ENVIO E DELETA OS DUPLICADO - FIM
**********************************************************************
***COLETA OS DESTINATARIOS - INICIO
  TYPES: BEGIN OF ty_for,
           lifnr          TYPE lifnr,
           receiver_email TYPE somlreci1-receiver,
         END OF ty_for.

  DATA: it_fornecedor TYPE STANDARD TABLE OF ty_for INITIAL SIZE 0,
        wa_fornecedor TYPE ty_for.
  FREE: it_fornecedor.

*  LOOP AT tg_envia[] ASSIGNING FIELD-SYMBOL(<_move_for>) GROUP BY ( lifnr = <_move_for>-lifnr receiver_email = <_move_for>-receiver_email ).
*    wa_fornecedor-lifnr   = <_move_for>-lifnr.
*    wa_fornecedor-receiver_email = <_move_for>-receiver_email.
*    APPEND wa_fornecedor TO it_fornecedor.
*    CLEAR: wa_fornecedor.
*  ENDLOOP.

  LOOP AT tg_envia[] ASSIGNING FIELD-SYMBOL(<_move_for>) GROUP BY ( lifnr = <_move_for>-lifnr ) ."receiver_email = <_move_for>-receiver_email ). psa 04052025 1622
    LOOP AT tg_lfa1 ASSIGNING FIELD-SYMBOL(<fs_for_email>) WHERE lifnr = <_move_for>-lifnr.
      wa_fornecedor-lifnr   = <fs_for_email>-lifnr.
      wa_fornecedor-receiver_email = <fs_for_email>-smtp_addr.
      APPEND wa_fornecedor TO it_fornecedor.
      CLEAR: wa_fornecedor.
    ENDLOOP.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_fornecedor.

  IF it_lista IS NOT INITIAL.
**********************************************************************Adiciona lista manual ao Fornecedor
    TYPES: BEGIN OF ty_for2,
             lifnr TYPE lifnr,
           END OF ty_for2.

    DATA: lr_fornecedor TYPE STANDARD TABLE OF ty_for2 INITIAL SIZE 0.
    CLEAR: lr_fornecedor,lr_fornecedor[].

    MOVE-CORRESPONDING it_fornecedor TO lr_fornecedor.
    DELETE ADJACENT DUPLICATES FROM lr_fornecedor.
    IF lr_fornecedor IS NOT INITIAL.
      LOOP AT lr_fornecedor INTO DATA(wa_formail).
        LOOP AT it_lista INTO DATA(wa_list).
          APPEND VALUE #( receiver_email = wa_list  lifnr = wa_formail-lifnr ) TO it_fornecedor.
          CLEAR:wa_list.
        ENDLOOP.
        CLEAR:wa_formail.
      ENDLOOP.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM it_fornecedor.
  ENDIF.

**********************************************************************

  DATA(pvirg) = ';'.
  DATA _receiver_email TYPE somlreci1-receiver.
  DATA lv_len_receiver TYPE i.
  CLEAR: _receiver_email,lv_len_receiver.
  LOOP AT tg_envia[] ASSIGNING FIELD-SYMBOL(<_back_receivers>).
    CLEAR: <_back_receivers>-receiver_email,lv_len_receiver,_receiver_email.
    LOOP AT it_fornecedor ASSIGNING FIELD-SYMBOL(<_rolback>) WHERE lifnr = <_back_receivers>-lifnr GROUP BY ( lifnr = <_rolback>-lifnr receiver_email = <_rolback>-receiver_email ).

      CONCATENATE _receiver_email <_rolback>-receiver_email INTO _receiver_email SEPARATED BY pvirg.
      <_back_receivers>-receiver_qtd = <_back_receivers>-receiver_qtd + 1.
    ENDLOOP.

    lv_len_receiver = strlen( _receiver_email ) - 1.
    <_back_receivers>-receiver_email = _receiver_email+1(lv_len_receiver).


  ENDLOOP.
***COLETA OS DESTINATARIOS - FIM
**********************************************************************html
  CLEAR:tg_envia_filtro[].
  MOVE-CORRESPONDING tg_envia[] TO tg_envia_filtro[].
  DELETE ADJACENT DUPLICATES FROM tg_envia_filtro[].

  DELETE ADJACENT DUPLICATES FROM tg_envia[] COMPARING empresa fornecedor lifnr doc_compensa moeda receiver_email
  receiver_qtd tipo bukrs nmfor compensacao.

  SORT tg_saida_bsik[] BY empresa fornecedor lifnr doc_compensa tipo bukrs nmfor compensacao divisao vlrdoc.
  SORT tg_saida_bsak[] BY empresa fornecedor lifnr doc_compensa tipo bukrs nmfor compensacao divisao vlrdoc.
  DELETE ADJACENT DUPLICATES FROM tg_saida_bsik[] COMPARING empresa fornecedor lifnr doc_compensa tipo bukrs nmfor compensacao divisao vlrdoc.
  DELETE ADJACENT DUPLICATES FROM tg_saida_bsak[] COMPARING empresa fornecedor lifnr doc_compensa tipo bukrs nmfor compensacao divisao vlrdoc.
***MONTA O HTML DE ENVIO - INICIO
  LOOP AT tg_envia_filtro[] ASSIGNING FIELD-SYMBOL(<_html>). "GROUP BY ( lifnr = <_move_for>-lifnr doc_compensa = <_move_for>-doc_compensa ).

    IF <_html>-tipo = 'Normal'.
      CLEAR: vl_assunto,
             menssagem_email1,
             menssagem_email2,
             menssagem_email3.
      vl_assunto = |Composição Pagamento -  { vl_data }|.
      menssagem_email1 = ' <H4 ALIGN=LEFT>Prezado Fornecedor,</H4>'.
      menssagem_email2 = ' <H4 ALIGN=LEFT>Segue abaixo a composição de pagamento. Informamos que a programação de pagamentos futuros poderá sofrer alterações conforme necessidade da empresa.</H4>'.
      menssagem_email3 = ' <H4 ALIGN=LEFT>Em caso de dúvidas, favor entrar em contato com o contratante (comprador). </H4>'.
    ENDIF.

    IF <_html>-tipo = 'Frete'.
      CLEAR: vl_assunto,
             menssagem_email1,
             menssagem_email2,
             menssagem_email3.
      vl_assunto = |Composição Pagamento - Fretes -  { vl_data }|.
      menssagem_email1 = ' <H4 ALIGN=LEFT>Prezado Fornecedor,</H4>'.
      menssagem_email2 = ' <H4 ALIGN=LEFT>Segue abaixo a composição de pagamento. Informamos que a programação de pagamentos futuros poderá sofrer alterações conforme necessidade da empresa.</H4>'.
      menssagem_email3 = ' <H4 ALIGN=LEFT>Em caso de dúvidas, favor entrar em contato pelo e-mail cscfinanceiro.fretes@amaggi.com.br </H4>'.
    ENDIF.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR aux_doc_compensa.
    aux_doc_compensa = <_html>-doc_compensa.
    aux_doc_empresa = <_html>-bukrs.
    " CRIA TABELA EM HTML EM FORMATO DE E-MAIL.
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
    new_line menssagem_email1.
    IF <_html>-moeda <> 'BRL'.
      menssagem_email11 = ' <H4 ALIGN=LEFT>Para pagamento em moeda estrangeira, solicitamos que considere o valor indicado no coluna moeda docto. Em anexo, enviamos o comprovante de pagamento do valor em reais.</H4>'.
      new_line menssagem_email11.
    ENDIF.
    new_line menssagem_email2.
    new_line menssagem_email3.

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
    new_line'   <TH><FONT SIZE="2">Valor BRL</FONT></TH>'.         "Valor - 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
    new_line'   <TH><FONT SIZE="2">PTAX</FONT></TH>'.              "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
    new_line'   <TH><FONT SIZE="2">Valor Moeda Docto</FONT></TH>'. "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
    new_line' </TR>'.

    CLEAR:aux_vlrTot, vl_montante_final,aux_vlrdocTot.
    LOOP AT tg_saida_bsak[] ASSIGNING FIELD-SYMBOL(<wg_envia>) WHERE doc_compensa = <_html>-doc_compensa AND lifnr = <_html>-lifnr.

      CLEAR: vl_montante,vlr_ptax,vl_vlrdoc,vl_data_venc,vl_vlrdoc.

      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          input  = <wg_envia>-vencimento
        IMPORTING
          output = vl_data_venc.


      IF <wg_envia>-moeda <> 'BRL'.

        vlr_ptax = <wg_envia>-ptax .

        WRITE vlr_ptax TO vl_ptax. "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
        CONDENSE vl_ptax NO-GAPS.

        WRITE <wg_envia>-vlrdoc TO vl_vlrdoc. "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA

        CLEAR: <wg_envia>-montante.
        <wg_envia>-montante = <wg_envia>-vlrdoc * vlr_ptax.

        WRITE <wg_envia>-montante TO vl_montante.
        CONDENSE vl_montante NO-GAPS.

        aux_vlrdocTot = aux_vlrdocTot + <wg_envia>-vlrdoc. "165663 - CS2023000355 - PSA

      ELSE.

        WRITE <wg_envia>-montante TO vl_montante.
        CONDENSE vl_montante NO-GAPS.

        vl_ptax = abap_false.
        vl_vlrdoc = abap_false.
        aux_vlrdocTot = abap_false.

      ENDIF.


      aux_vlrTot = aux_vlrTot + <wg_envia>-montante.


      new_line' <TR>'.
      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wg_envia>-empresa.
      new_line'  </FONT></TD>'.

      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wg_envia>-fornecedor.
      new_line'  </FONT></TD>'.

      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wg_envia>-divisao.
      new_line'  </FONT></TD>'.

      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wg_envia>-uf_filial.
      new_line'  </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wg_envia>-doc_compensa.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      vl_data_venc.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wg_envia>-moeda.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line      vl_montante.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line      vl_ptax.        "PTAX 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line      vl_vlrdoc.        "Valor Moeda Docto115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
      new_line'   </FONT></TD>'.

      new_line ' </TR>'.


    ENDLOOP.

    WRITE aux_vlrTot TO vl_montante_final.
    CONDENSE vl_montante_final NO-GAPS.

    IF aux_vlrdocTot = 0.
      vl_montante_final_doc = abap_false.
    ELSE.
      WRITE aux_vlrdocTot TO vl_montante_final_doc. "165663 - CS2023000355 - PSA
      CONDENSE vl_montante_final_doc NO-GAPS.
    ENDIF.

    new_line' <TR>'.
    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line   ''.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line    ''.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line    ''.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line    ''.
    new_line'  </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line    ''.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line    ''.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line     ''.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line      vl_montante_final.
    CLEAR:aux_vlrTot, vl_montante_final.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line     ''.        "PTAX 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line       vl_montante_final_doc. "165663 - CS2023000355 - PSA
    CLEAR:aux_vlrdoctot, vl_montante_final_doc.
    new_line'   </FONT></TD>'.

    new_line ' </TR>'.

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
    new_line'   <TH><FONT SIZE="2">Valor BRL</FONT></TH>'.         "Valor - 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
    new_line'   <TH><FONT SIZE="2">PTAX</FONT></TH>'.              "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
    new_line'   <TH><FONT SIZE="2">Valor Moeda Docto</FONT></TH>'. "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
    new_line' </TR>'.

    CLEAR:aux_vlrTot, vl_montante_final.
    FREE: tg_faturas.
    LOOP AT tg_bsak_aux ASSIGNING FIELD-SYMBOL(<wl_bsak_aux>) WHERE augbl EQ  <_html>-doc_compensa AND lifnr = <_html>-lifnr."<grupo_compensacao>-doc_compensa.

      CLEAR: aux_ptax. "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA

      aux_vlrTot = aux_vlrTot + <wg_envia>-montante.

      IF <wl_bsak_aux>-waers <> 'BRL'. "<wl_bsak_aux>-wrbtr IS NOT INITIAL AND <wl_bsak_aux>-wrbtr <> 0.
        aux_ptax           = <wl_bsak_aux>-dmbtr / <wl_bsak_aux>-wrbtr.
      ELSE.
        <wl_bsak_aux>-wrbtr  = 0.
        aux_ptax           = 0.
      ENDIF.


      DATA(wl_faturas) = VALUE ty_saida( empresa        = <wl_bsak_aux>-bukrs
                                         fornecedor     = <wl_bsak_aux>-lifnr
                                         divisao        = <wl_bsak_aux>-gsber
                                         referencia     = <wl_bsak_aux>-xblnr
                                         doc_compensa   = <wl_bsak_aux>-belnr
                                         vencimento     = <wl_bsak_aux>-zfbdt
                                         moeda          = <wl_bsak_aux>-waers
                                         montante       = <wl_bsak_aux>-dmbtr
                                         ptax           = aux_ptax          "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
                                         vlrdoc         = <wl_bsak_aux>-wrbtr "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
                                         ).

      APPEND wl_faturas TO tg_faturas[].
      CLEAR: wl_faturas.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM tg_faturas[] COMPARING doc_compensa.
    CLEAR:aux_vlrTot, vl_montante_final.

    SORT tg_faturas[] BY empresa divisao.

    LOOP AT tg_faturas[] ASSIGNING FIELD-SYMBOL(<wl_faturas>).
      CLEAR: vl_montante,vlr_ptax,vl_vlrdoc,vl_data_venc,vl_vlrdoc.
      "CONVERTE A DATA PARA FORMATO XX/XX/XXXX.
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = <wl_faturas>-vencimento IMPORTING output = vl_data_venc.
      aux_vlrTot = aux_vlrTot + <wl_faturas>-montante.

      IF <wl_faturas>-moeda <> 'BRL'.

        aux_vlrdocTot = aux_vlrdocTot + <wl_faturas>-vlrdoc. "165663 - CS2023000355 - PSA

        WRITE <wl_faturas>-montante TO vl_montante.
        CONDENSE vl_montante NO-GAPS.

        WRITE <wl_faturas>-vlrdoc TO vl_vlrdoc.
        CONDENSE vl_vlrdoc NO-GAPS.

        WRITE <wl_faturas>-ptax TO vl_ptax.
        CONDENSE vl_ptax NO-GAPS.
      ELSE.

        WRITE <wl_faturas>-montante TO vl_montante.
        CONDENSE vl_montante NO-GAPS.

        vl_ptax = abap_false.
        vl_vlrdoc = abap_false.
        aux_vlrdocTot = abap_false.

      ENDIF.


      new_line' <TR>'.
      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wl_faturas>-empresa.
      new_line'  </FONT></TD>'.

      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wl_faturas>-divisao.
      new_line'  </FONT></TD>'.

      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wl_faturas>-referencia.
      new_line'  </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wl_faturas>-doc_compensa.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      vl_data_venc.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      <wl_faturas>-moeda.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line      vl_montante.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line      vl_ptax.        "PTAX 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line      vl_vlrdoc.        "Valor Moeda Docto115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
      new_line'   </FONT></TD>'.

      new_line ' </TR>'.
    ENDLOOP.

    CLEAR:tg_faturas[].
    FREE: tg_faturas[].

    WRITE aux_vlrTot TO vl_montante_final.
    CONDENSE vl_montante_final NO-GAPS.

    IF aux_vlrdocTot = 0.
      vl_montante_final_doc = abap_false.
    ELSE.
      WRITE aux_vlrdocTot TO vl_montante_final_doc. "165663 - CS2023000355 - PSA
      CONDENSE vl_montante_final_doc NO-GAPS.
    ENDIF.

    new_line' <TR>'.
    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line     ''.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line    ''.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      ''.
    new_line'  </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line     ''.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line     ''.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line     ''.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line      vl_montante_final.
    CLEAR:aux_vlrTot, vl_montante_final.
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line     ''.        "PTAX 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
    new_line'   </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line       vl_montante_final_doc. "165663 - CS2023000355 - PSA
    CLEAR:aux_vlrdoctot, vl_montante_final_doc.
    new_line'   </FONT></TD>'.

    new_line ' </TR>'.

    new_line ' </TABLE>'.

    READ TABLE tg_bsik_aux INTO DATA(wa_progfutura) WITH KEY lifnr = <_html>-lifnr. "165663 - CS2023000355 - PSA

    IF wa_progfutura-lifnr = <_html>-lifnr.

      DATA: qtd_dias             TYPE i,
            v_mostra_prog_futura TYPE boolean.
      CLEAR: v_mostra_prog_futura,qtd_dias.

      IF v_executa = 'INF_PGTO_FORNEC_MANUAL_MAGGI'. "165663 - CS2023000355 - PSA
        "aqui se verifica no enviuo manual se a data informada é menor ou igual a 7
        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            begda = vl_augdt
            endda = vl_hoje
          IMPORTING
            days  = qtd_dias.
        IF qtd_dias <= 7.
          v_mostra_prog_futura = abap_true.
        ELSE.
          v_mostra_prog_futura = abap_false.
        ENDIF.
      ELSE.
        v_mostra_prog_futura = abap_true.
      ENDIF.

      IF v_mostra_prog_futura = abap_true."165663 - CS2023000355 - PSA

* Início - RJF
        new_line' <H4 ALIGN=LEFT>Previsão de pagamentos futuros:</H4>'.
        new_line''.
        new_line' <TABLE BORDER="1" STYLE="WIDTH:100%" ID="T02">'.
        new_line'  <TR>'.
        new_line'   <TH><FONT SIZE="2">Empresa</FONT></TH>'.
        new_line'   <TH><FONT SIZE="2">Filial</FONT></TH>'.
        new_line'   <TH><FONT SIZE="2">Fatura</FONT></TH>'.
        new_line'   <TH><FONT SIZE="2">DocContabil</FONT></TH>'.
        new_line'   <TH><FONT SIZE="2">VencLíquid</FONT></TH>'.
        new_line'   <TH><FONT SIZE="2">Moeda</FONT></TH>'.
        new_line'   <TH><FONT SIZE="2">Valor BRL</FONT></TH>'.         "Valor - 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
        new_line'   <TH><FONT SIZE="2">PTAX</FONT></TH>'.              "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
        new_line'   <TH><FONT SIZE="2">Valor Moeda Docto</FONT></TH>'. "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
        new_line' </TR>'.
        CLEAR:aux_vlrTot, vl_montante_final.

        FREE:tg_faturas_aux.
        LOOP AT tg_bsik_aux ASSIGNING FIELD-SYMBOL(<wl_bsik_aux>) WHERE lifnr EQ wa_progfutura-lifnr."<_html>-lifnr ."165663 - CS2023000355 - PSA

          CLEAR: aux_ptax. "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA

          IF <wl_bsik_aux>-waers <> 'BRL'.
            aux_ptax           = <wl_bsik_aux>-dmbtr / <wl_bsik_aux>-wrbtr.
          ELSE.
            <wl_bsik_aux>-wrbtr  = 0.
            aux_ptax           = 0.
          ENDIF.

          "SELECT SINGLE * FROM bseg WHERE bukrs = @<wl_bsik_aux>-bukrs AND belnr = @<wl_bsik_aux>-belnr AND gjahr = @<wl_bsik_aux>-gjahr AND zfbdt = @<wl_bsik_aux>-zfbdt INTO @DATA(get_bseg_futuro). "165663 - CS2023000355 - PSA

          READ TABLE tg_bsik INTO DATA(get_bsik) WITH KEY bukrs = <wl_bsik_aux>-bukrs belnr = <wl_bsik_aux>-belnr gjahr = <wl_bsik_aux>-gjahr lifnr = wa_progfutura-lifnr.  "165663 - CS2023000355 - PSA

          DATA(wl_faturas_aux) = VALUE ty_saida( empresa        = <wl_bsik_aux>-bukrs
                                             fornecedor     = <wl_bsik_aux>-lifnr
                                             "divisao        = <wl_bsik_aux>-divisao
                                             divisao        = <wl_bsik_aux>-gsber
                                             referencia     = <wl_bsik_aux>-xblnr
                                             doc_compensa   = <wl_bsik_aux>-belnr
                                             "vencimento     = <wl_bsik_aux>-zfbdt
                                             "vencimento     = get_bseg_futuro-fdtag "165663 - CS2023000355 - PSA
                                             vencimento      = get_bsik-dtvenc
                                             moeda          = <wl_bsik_aux>-waers
                                             montante       = <wl_bsik_aux>-dmbtr
                                             ptax           = aux_ptax          "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
                                             vlrdoc         = <wl_bsik_aux>-wrbtr "115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
                                             ).

          APPEND wl_faturas_aux TO tg_faturas_aux[].
          CLEAR: wl_faturas_aux,get_bsik.
        ENDLOOP.
        DELETE ADJACENT DUPLICATES FROM tg_faturas_aux[] COMPARING doc_compensa.

        CLEAR:aux_vlrTot, vl_montante_final.

        SORT tg_faturas_aux[] BY empresa divisao.


        LOOP AT tg_faturas_aux[] ASSIGNING FIELD-SYMBOL(<wl_faturas_aux>). "Futuras

          CLEAR: vl_montante,vlr_ptax,vl_vlrdoc,vl_data_venc,vl_vlrdoc.
          "CONVERTE A DATA PARA FORMATO XX/XX/XXXX.
          CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT' EXPORTING input = <wl_faturas_aux>-vencimento IMPORTING output = vl_data_venc.

          aux_vlrTot = aux_vlrTot + <wl_faturas_aux>-montante.

          IF <wl_faturas_aux>-moeda <> 'BRL'.
            aux_vlrdocTot = aux_vlrdocTot + <wl_faturas_aux>-vlrdoc. "165663 - CS2023000355 - PSA

            WRITE <wl_faturas_aux>-montante TO vl_montante.
            CONDENSE vl_montante NO-GAPS.

            WRITE <wl_faturas_aux>-vlrdoc TO vl_vlrdoc.
            CONDENSE vl_vlrdoc NO-GAPS.

            WRITE <wl_faturas_aux>-ptax TO vl_ptax.
            CONDENSE vl_ptax NO-GAPS.

          ELSE.

            WRITE <wl_faturas_aux>-montante TO vl_montante.
            CONDENSE vl_montante NO-GAPS.

            vl_ptax = abap_false.
            vl_vlrdoc = abap_false.
            aux_vlrdocTot = abap_false.

          ENDIF.



          new_line' <TR>'.
          new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
          new_line      <wl_faturas_aux>-empresa.
          new_line'  </FONT></TD>'.

          new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
          new_line      <wl_faturas_aux>-divisao.
          new_line'  </FONT></TD>'.

          new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
          new_line      <wl_faturas_aux>-referencia.
          new_line'  </FONT></TD>'.

          new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
          new_line      <wl_faturas_aux>-doc_compensa.
          new_line'   </FONT></TD>'.

          new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
          new_line      vl_data_venc.
          new_line'   </FONT></TD>'.

          new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
          new_line      <wl_faturas_aux>-moeda.
          new_line'   </FONT></TD>'.

          new_line'   <TD><FONT SIZE="2">'.
          new_line      vl_montante.
          new_line'   </FONT></TD>'.

          new_line'   <TD><FONT SIZE="2">'.
          new_line      vl_ptax.        "PTAX 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
          new_line'   </FONT></TD>'.

          new_line'   <TD><FONT SIZE="2">'.
          new_line      vl_vlrdoc.        "Valor Moeda Docto115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
          new_line'   </FONT></TD>'.

          new_line ' </TR>'.

        ENDLOOP.
        CLEAR: tg_faturas_aux[].
        FREE: tg_faturas_aux[].

        WRITE aux_vlrTot TO vl_montante_final.
        CONDENSE vl_montante_final NO-GAPS.

        IF aux_vlrdocTot = 0.
          vl_montante_final_doc = abap_false.
        ELSE.
          WRITE aux_vlrdocTot TO vl_montante_final_doc. "165663 - CS2023000355 - PSA
          CONDENSE vl_montante_final_doc NO-GAPS.
        ENDIF.
      ENDIF.

      new_line' <TR>'.
      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      ''.
      new_line'  </FONT></TD>'.

      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line     ''.
      new_line'  </FONT></TD>'.

      new_line'  <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line     ''.
      new_line'  </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      ''.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      ''.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      ''.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line       vl_montante_final.
      CLEAR:aux_vlrTot, vl_montante_final.
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line      ''.        "PTAX 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
      new_line'   </FONT></TD>'.

      new_line'   <TD><FONT SIZE="2">'.
      new_line       vl_montante_final_doc. "165663 - CS2023000355 - PSA
      CLEAR:aux_vlrdoctot, vl_montante_final_doc.
      new_line'   </FONT></TD>'.

      new_line ' </TABLE>'.

    ENDIF.

    IF wg_envia-empresa(4) <> '0035' .
      new_line'  <H6>Atenciosamente,</H6><H6><b>AMAGGI - Corporativo</b></H6><H6>CSC - Financeiro</H6><H6>www.amaggi.com.br</H6> '.
    ELSE.
      new_line'  <H6>Atenciosamente,</H6><H6><b>ALZ Grãos</b></H6><H6>Palmas - Tocantins</H6>'.
    ENDIF.

    new_line '</BODY>'.
    new_line '</HTML>'.
***MONTA O HTML DE ENVIO - FIM
**********************************************************************
***COLETA O HTML DE ENVIO - INICIO

    " open html
    DATA l_HTML TYPE string.
    DATA lv_len_html TYPE i.
    CLEAR: l_HTML,lv_len_html.
    LOOP AT it_html INTO DATA(lv_line).
      CONCATENATE l_HTML lv_line INTO l_HTML SEPARATED BY '<!--#-->'."cl_abap_char_utilities=>cr_lf.
    ENDLOOP.
    lv_len_html = strlen( l_HTML ) - 8.

    FREE: it_html.
    APPEND VALUE #(
    dt_COMP = vl_comp
    dt_send = sy-datum
    hr_send = sy-uzeit
    doc_compensa = <_html>-doc_compensa
    bukrs = <_html>-bukrs
    lifnr = <_html>-lifnr
    nmfor = <_html>-nmfor
    tipo = <_html>-tipo
    receiver_email = <_html>-receiver_email
    receiver_qtd = <_html>-receiver_qtd
    empresa = <_html>-empresa
    fornecedor = <_html>-fornecedor
    assunto = vl_assunto
    pdf = ''
    html = l_HTML+8(lv_len_html)
    usuario = sy-uname
    execucao = v_executa
    compensacao = <_html>-compensacao
    moeda = <_html>-moeda
    ) TO tg_envia_salv[].

    CLEAR:aux_vlrTot,
           vl_montante_final.
  ENDLOOP.

  FREE: tg_envia[],
  tg_envia_filtro[],
      tg_faturas_aux[],
        tg_faturas[],
        tg_saida_bsak[],
        tg_saida_bsik[].

  "MOVE-CORRESPONDING <_html> to wa_envia_salv.
  "tg_envia_salv[] = <_html>.
  "MOVE-CORRESPONDING tg_envia[] TO  tg_envia_salv[].


***COLETA O HTML DE ENVIO - FIM

**********************************************************************
***COLETA O COMPROVANTE DE PAGAMENTO - INICIO
  DATA gw_objbin     LIKE solisti1 OCCURS 0 WITH HEADER LINE.
  DATA gw_objpack    LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE.
  DATA gw_objtxt     LIKE solisti1 OCCURS 0 WITH HEADER LINE.
  DATA gt_zfit0091 TYPE STANDARD TABLE OF zfit0091 INITIAL SIZE 0.
  DATA it_job_output  TYPE ssfcrescl.
  DATA xstring_pdf TYPE xstring.
  DATA v_bin_filesize TYPE i.
  DATA wl_buffer      TYPE string.

  DATA: wa_zfit0091 TYPE zfit0091.

  LOOP AT tg_envia_salv[] ASSIGNING FIELD-SYMBOL(<get_pdf>).
    FREE: gw_objbin[],gw_objpack[],gw_objtxt[],it_job_output.
    CLEAR: gw_objtxt,gw_objpack,gw_objbin,gw_tline,xstring_pdf, v_bin_filesize.
    SELECT SINGLE * FROM zfit0091 WHERE augbl = @<get_pdf>-doc_compensa AND bukrs = @<get_pdf>-bukrs INTO @wa_zfit0091.

    IF sy-subrc = 0.
      FREE:gt_zfit0091.
      IF wa_zfit0091-bukrs IS NOT INITIAL AND wa_zfit0091-augbl IS NOT INITIAL.
        APPEND wa_zfit0091 TO gt_zfit0091.
        CLEAR: wa_zfit0091.
        TRY.
            CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
              EXPORTING
                formname = c_smart_forms
              IMPORTING
                fm_name  = v_func_name.

            CALL FUNCTION v_func_name
              EXPORTING
                control_parameters = wl_control
                output_options     = wl_output_opt
                user_settings      = ' '
              IMPORTING
                job_output_info    = it_job_output
              TABLES
                it_zfit0091        = gt_zfit0091
              EXCEPTIONS
                formatting_error   = 1
                internal_error     = 2
                send_error         = 3
                user_canceled      = 4
                OTHERS             = 5.

            CALL FUNCTION 'CONVERT_OTF'
              EXPORTING
                format                = 'PDF'
                max_linewidth         = 132
              IMPORTING
                bin_filesize          = v_bin_filesize
                bin_file              = xstring_pdf
              TABLES
                otf                   = it_job_output-otfdata[]
                lines                 = gw_tline
              EXCEPTIONS
                err_max_linewidth     = 1
                err_format            = 2
                err_conv_not_possible = 3
                OTHERS                = 4.
          CATCH cx_root.

        ENDTRY.

        IF sy-subrc = 0.
          <get_pdf>-pdf = xstring_pdf.
        ENDIF.
      ENDIF.
    ENDIF.
    FREE: gw_objbin[],gw_objpack[],gw_objtxt[],gt_zfit0091,it_job_output.
    CLEAR: gw_objtxt,gw_objpack,gw_objbin,gw_tline,xstring_pdf, v_bin_filesize,wa_zfit0091.
  ENDLOOP.
***COLETA O COMPROVANTE DE PAGAMENTO - FIM
ENDFORM.


FORM f_call_smart_forms
  USING    i_output_options TYPE ssfcompop
           i_COMPROVANTE
           i_control_params TYPE ssfctrlop
  CHANGING c_job_output     TYPE ssfcrescl.

  DATA gt_zfit0091 TYPE TABLE OF zfit0091.
  DATA i_job_info TYPE ssfcrescl.
  FREE: gt_zfit0091.
  APPEND i_comprovante TO gt_zfit0091.
  READ TABLE gt_zfit0091 INTO DATA(lr_zfit0091) INDEX 1.
  CLEAR p_not_file.

  IF lr_zfit0091-archive IS NOT INITIAL.
    APPEND i_comprovante TO gt_zfit0091.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = c_smart_forms
      IMPORTING
        fm_name  = v_func_name.

    CALL FUNCTION v_func_name
      EXPORTING
        control_parameters = i_control_params
        output_options     = i_output_options
        user_settings      = ' '
      IMPORTING
        job_output_info    = c_job_output
      TABLES
        it_zfit0091        = gt_zfit0091
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

  ELSE.
    p_not_file = abap_true.
  ENDIF.
ENDFORM.
FORM f_generate_pdf USING i_job_info TYPE ssfcrescl.
  DATA gw_otf         TYPE itcoo OCCURS 0 WITH HEADER LINE.
  DATA v_bin_filesize TYPE i.
  DATA wl_buffer      TYPE string.

  CLEAR: gw_record,
         gw_tline.

  gw_otf[] = i_job_info-otfdata[].

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
      max_linewidth         = 132
    IMPORTING
      bin_filesize          = v_bin_filesize
    TABLES
      otf                   = gw_otf
      lines                 = gw_tline
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      OTHERS                = 4.

*  LOOP AT gw_tline.
*    TRANSLATE gw_tline USING '~'.
*    CONCATENATE wl_buffer gw_tline INTO wl_buffer.
*  ENDLOOP.
*  TRANSLATE wl_buffer USING '~'.
*  DO.
*    gw_record = wl_buffer.
*    APPEND gw_record.
*    SHIFT wl_buffer LEFT BY 255 PLACES.
*    IF wl_buffer IS INITIAL.
*      EXIT.
*    ENDIF.
*  ENDDO.

ENDFORM.

FORM show_function_info USING i_function TYPE sy-ucomm  i_text TYPE string.

  IF i_function IS INITIAL.
    RETURN.
  ENDIF.
  " Botões da Grid
  CASE i_function.
    WHEN 'ATUALIZAR_ALV'.
      gr_table->refresh( ).
    WHEN 'ENVIAR'.
      p_envia = abap_true.
      PERFORM valida_envio.
      MESSAGE 'E-mails Enviados com Sucesso!' TYPE 'I' DISPLAY LIKE 'I'.
      FREE: tg_envia_salv[].
      SET SCREEN 0.
      LEAVE SCREEN.

  ENDCASE.

ENDFORM.

FORM hotspot USING p_row p_column.

  READ TABLE tg_envia_salv ASSIGNING FIELD-SYMBOL(<_get>) INDEX p_row.

  CHECK <_get> IS NOT INITIAL.

  CASE p_column.
    WHEN 'ASSUNTO'.
      cl_abap_browser=>show_html( html_string = <_get>-html title = 'Html' ).
    WHEN 'DOC_COMPENSA'.

      IF <_get>-pdf IS NOT INITIAL.

        DATA: lo_html_viewer TYPE REF TO cl_gui_html_viewer,
              lt_pdf_content TYPE TABLE OF char255,
              lv_html        TYPE string,
              lv_base64_pdf  TYPE string.


        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = <_get>-pdf
          IMPORTING
            output = lv_base64_pdf.

        lv_html = '<html><body><embed src="data:application/pdf;base64,' && lv_base64_pdf && '" width="100%" height="100%"></embed></body></html>'.

        cl_abap_browser=>show_html( html_string = lv_html title = 'PDF' ).
      ELSE.
        MESSAGE 'Comprovante não Encontrado!' TYPE 'I' DISPLAY LIKE 'I'.
        EXIT.
      ENDIF.

    WHEN 'RECEIVER_QTD'.

      TYPES: BEGIN OF ty_email,
               email TYPE adr6-smtp_addr,
             END OF ty_email.
      DATA: lt_destinatarios  TYPE TABLE OF ty_email INITIAL SIZE 0.
      FREE: lt_destinatarios.
      SPLIT <_get>-receiver_email AT ';' INTO TABLE lt_destinatarios.


      DATA go_alv TYPE REF TO cl_salv_table.

      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = go_alv
            CHANGING
              t_table      = lt_destinatarios[] ).
        CATCH cx_salv_msg.

      ENDTRY.


      TRY.
          "***MOSTRA
          DATA(go_column) = go_alv->get_columns( )->get_column( 'EMAIL' ).
          go_column->set_short_text( 'Email' ).
          go_column->set_medium_text( 'Email' ).
          go_column->set_long_text( 'Email' ).
          go_column->set_optimized( abap_false ).
        CATCH cx_salv_not_found.

      ENDTRY.



      DATA go_title              TYPE lvc_title.
      go_title = |Destinatários|.
      DATA(go_display_settings) = go_alv->get_display_settings( ).
      go_display_settings->set_list_header_size( '10' ). " 0=l, 1=s, 2=m
      go_display_settings->set_list_header( go_title ).

      go_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
      go_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
      go_display_settings->set_list_header( go_title ).

      DATA(ltot) = lines( lt_destinatarios[] ).
      DATA total TYPE i.

      total = ltot + 10.

      go_alv->set_screen_popup(
        start_column = 1
        end_column   = 100
        start_line   = 1
        end_line     = total ).

      go_alv->display( ).
      "WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM send .
  DATA lv_lines    LIKE sy-tabix.
  DATA wa_doc_data LIKE sodocchgi1.
  LOOP AT tg_envia_salv[] ASSIGNING FIELD-SYMBOL(<_send>).
**********************************************************************DESTINATARIOS
    FREE: it_receivers[].
    CLEAR: it_receivers, wa_receivers.
    SPLIT <_send>-receiver_email AT ';' INTO TABLE DATA(it_destinatarios).

    LOOP AT it_destinatarios ASSIGNING FIELD-SYMBOL(<_destinatarios>).
      APPEND VALUE #( receiver =   <_destinatarios>   rec_type =   'U'  rec_date =   vl_hoje ) TO it_receivers.
    ENDLOOP.
**********************************************************************HTML_BODY
    FREE:it_contents_txt[],it_packing_list[].
    CLEAR: WA_contents_txt,it_packing_list,lv_lines.
    SPLIT <_send>-html AT '<!--#-->' INTO TABLE it_contents_txt.
    lv_lines = lines( it_contents_txt[] ).
    wa_packing_list-transf_bin = space.
    wa_packing_list-head_start = 1.
    wa_packing_list-head_num   = 1.
    wa_packing_list-body_start = 1.
    wa_packing_list-body_num   = lv_lines.
    wa_packing_list-doc_type   = 'HTM'.
    APPEND wa_packing_list TO it_packing_list[].

**********************************************************************PDF
    IF <_send>-pdf IS NOT INITIAL.
      DATA : _size_bin         TYPE i.

      DATA : BEGIN OF itab_bin OCCURS 0,
               line TYPE x LENGTH 255,
             END OF itab_bin.
      FREE: itab_bin,it_contents_bin.
      CLEAR: _size_bin.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = <_send>-pdf
        IMPORTING
          output_length = _size_bin
        TABLES
          binary_tab    = it_contents_bin.

      IF sy-subrc = 0.
        wa_anexo-transf_bin = abap_true.
        wa_anexo-head_start = 1.
        wa_anexo-head_num   = 1.
        wa_anexo-body_start = 1.
        wa_anexo-body_num =  lines( it_contents_bin ).
        wa_anexo-doc_type   = 'PDF'.
        wa_anexo-obj_descr  = |{ <_send>-doc_compensa }|.
        wa_anexo-obj_name   = |{ <_send>-doc_compensa }|.
        wa_anexo-doc_size   = _size_bin.

        APPEND wa_anexo TO it_packing_list.

      ENDIF.

    ELSE.
      FREE: it_contents_bin.
    ENDIF.


*     IF <_send>-pdf IS NOT INITIAL.
*
*        DATA: lo_html_viewer TYPE REF TO cl_gui_html_viewer,
*              lt_pdf_content TYPE TABLE OF char255,
*              lv_html        TYPE string,
*              lv_base64_pdf  TYPE string.
*
*
*        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
*          EXPORTING
*            input  = <_send>-pdf
*          IMPORTING
*            output = lv_base64_pdf.
*
*        lv_html = '<html><body><embed src="data:application/pdf;base64,' && lv_base64_pdf && '" width="100%" height="100%"></embed></body></html>'.
*
*
*      ELSE.
*        MESSAGE 'Comprovante não Encontrado!' TYPE 'I' DISPLAY LIKE 'I'.
*        EXIT.
*      ENDIF.

    wa_doc_data-obj_descr = <_send>-assunto.
    "wa_doc_data-doc_size  = _size_anexo + _size_body.
    wa_doc_data-obj_langu  = sy-langu.
    CLEAR: wa_anexo,wa_packing_list.
**********************************************************************
    TRY.

        CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
          EXPORTING
            document_data              = wa_doc_data
            put_in_outbox              = 'X'
            commit_work                = 'X'
          TABLES
            packing_list               = it_packing_list
            contents_txt               = it_contents_txt
            contents_bin               = it_contents_bin
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

        IF sy-subrc <> 0.
          CLEAR: enviado_sucesso.
        ELSE.
          enviado_sucesso = abap_true.
        ENDIF.
      CATCH cx_root.
    ENDTRY.
  ENDLOOP.

  FREE: it_packing_list,
         it_header,
         it_contents_bin,
         it_contents_txt,
         it_contents_hex,
         it_receivers.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM table .
  LOOP AT tg_envia_salv ASSIGNING FIELD-SYMBOL(<_table>).
    MODIFY zfit0077_monitor FROM <_table>.
    COMMIT WORK.
  ENDLOOP.
ENDFORM.


FORM action_process.
  " Botões do Programa Cabeçalho
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDFORM.

FORM valida_envio .
  IF p_envia = abap_true.
    CASE vl_acao.
      WHEN 'T'.
        PERFORM table.
      WHEN 'S'.
        PERFORM send.
      WHEN 'A'.
        PERFORM send.
        PERFORM table.
      WHEN OTHERS.
    ENDCASE.

    FREE: tg_envia_salv.

  ENDIF.

ENDFORM.

FORM make_alv.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = painel_1
          container_name = 'CONTAINER'
        IMPORTING
          r_salv_table   = gr_table
        CHANGING
          t_table        = tg_envia_salv[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( abap_true ).

  lr_columns = gr_table->get_columns( ).

  lr_columns_tb = gr_table->get_columns( ).


  "... §3.2 include own functions
  DATA l_text_send TYPE string.
  DATA l_icon_send TYPE string.
  l_text_send = 'Enviar'.
  l_icon_send =  icon_envelope_closed.
  TRY.
      lr_functions->add_function( name     = 'ENVIAR'
                                  icon     = l_icon_send
                                  text     = l_text_send
                                  tooltip  = 'Enviar'
                                  position = if_salv_c_function_position=>right_of_salv_functions ).
    CATCH cx_root.
  ENDTRY.

  TRY.
      "***MOSTRA
      lr_column = lr_columns->get_column( 'EMPRESA' ).
      lr_column->set_short_text( 'Empresa' ).
      lr_column->set_medium_text( 'Empresa' ).
      lr_column->set_long_text( 'Empresa' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'LIFNR' ).
      lr_column->set_short_text( 'Fornecedor' ).
      lr_column->set_medium_text( 'Fornecedor' ).
      lr_column->set_long_text( 'Fornecedor' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'DOC_COMPENSA' ).
      lr_column->set_short_text( 'Doc.' ).
      lr_column->set_medium_text( 'Doc.Comp.' ).
      lr_column->set_long_text( 'Doc.Comp.' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'TIPO' ).
      lr_column->set_short_text( 'Tipo' ).
      lr_column->set_medium_text( 'Tipo' ).
      lr_column->set_long_text( 'Tipo' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '7' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'RECEIVER_QTD' ).
      lr_column->set_short_text( 'QtdDest.' ).
      lr_column->set_medium_text( 'QtdDest.' ).
      lr_column->set_long_text( 'QtdDest.' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'ASSUNTO' ).
      lr_column->set_short_text( 'Assunto' ).
      lr_column->set_medium_text( 'Assunto' ).
      lr_column->set_long_text( 'Assunto' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '51' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'USUARIO' ).
      lr_column->set_short_text( 'Usuario' ).
      lr_column->set_medium_text( 'Usuario' ).
      lr_column->set_long_text( 'Usuario' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'DT_SEND' ).
      lr_column->set_short_text( 'Dt Envio' ).
      lr_column->set_medium_text( 'Dt Envio' ).
      lr_column->set_long_text( 'Dt Envio' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '10' ).
      lr_column->set_edit_mask( value = '__/__/____' ).
      lr_column = lr_columns->get_column( 'HR_SEND' ).
      lr_column->set_short_text( 'Hr Envio' ).
      lr_column->set_medium_text( 'Hr Envio' ).
      lr_column->set_long_text( 'Hr Envio' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '10' ).
      lr_column->set_edit_mask( value = '__:__:__' ).
      lr_column = lr_columns->get_column( 'MOEDA' ).
      lr_column->set_short_text( 'Moeda' ).
      lr_column->set_medium_text( 'Moeda' ).
      lr_column->set_long_text( 'Moeda' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'EXECUCAO' ).
      lr_column->set_short_text( 'Execucao' ).
      lr_column->set_medium_text( 'Execucao' ).
      lr_column->set_long_text( 'Execucao' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '26' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'DT_COMP' ).
      lr_column->set_short_text( 'Dt Comp.' ).
      lr_column->set_medium_text( 'Dt Compensação' ).
      lr_column->set_long_text( 'Dt Compensação' ).
      lr_column->set_optimized( abap_true ).

      "***ESCONDE
      lr_column = lr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'BUKRS' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'FORNECEDOR' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'PDF' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'HTML' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'RECEIVER_EMAIL' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'COMPENSACAO' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found.
  ENDTRY.

  lr_columns->set_column_position( columnname = 'EMPRESA'         position   = 01 ).
  lr_columns->set_column_position( columnname = 'LIFNR'           position   = 02 ).
  lr_columns->set_column_position( columnname = 'NMFOR'           position   = 03 ).
  lr_columns->set_column_position( columnname = 'DOC_COMPENSA'    position   = 04 ).
  lr_columns->set_column_position( columnname = 'DT_COMP'         position   = 05 ).
  lr_columns->set_column_position( columnname = 'ASSUNTO'         position   = 06 ).
  lr_columns->set_column_position( columnname = 'TIPO'            position   = 07 ).
  lr_columns->set_column_position( columnname = 'RECEIVER_QTD'    position   = 08 ).
  lr_columns->set_column_position( columnname = 'EXECUCAO'        position   = 09 ).
  lr_columns->set_column_position( columnname = 'COMPENSACAO'     position   = 10 ).
  lr_columns->set_column_position( columnname = 'MOEDA'           position   = 11 ).
  lr_columns->set_column_position( columnname = 'USUARIO'         position   = 12 ).
  lr_columns->set_column_position( columnname = 'DT_SEND'         position   = 13 ).
  lr_columns->set_column_position( columnname = 'HR_SEND'         position   = 14 ).

  TRY.
      lr_column_tb ?= lr_columns_tb->get_column( 'RECEIVER_QTD' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).

      lr_column_tb ?= lr_columns_tb->get_column( 'ASSUNTO' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).

      lr_column_tb ?= lr_columns_tb->get_column( 'DOC_COMPENSA' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found  .                          "#EC NO_HANDLER
  ENDTRY.

  lr_events = gr_table->get_event( ).

  CREATE OBJECT gr_events_r.

  SET HANDLER gr_events_r->on_user_command_r  FOR lr_events.
  SET HANDLER gr_events_r->on_before_user_command_r FOR lr_events.
  SET HANDLER gr_events_r->on_after_user_command_r FOR lr_events.
  SET HANDLER gr_events_r->on_hotspot_click_r FOR lr_events.
  SET HANDLER gr_events_r->make_toolbar_r FOR ALL INSTANCES.
  SET HANDLER gr_events_r->on_data_changed_finished_r FOR ALL INSTANCES ACTIVATION 'X'.

  gr_table->display( ).

ENDFORM.

FORM make_docker .
  CREATE OBJECT container_main
    EXPORTING
      container_name = 'CONTAINER'
      lifetime       = container_main->lifetime_dynpro.

* Cria Splitter Container
  CREATE OBJECT painel_control
    EXPORTING
      parent  = container_main
      rows    = 1
      columns = 1
      align   = 70.

* Exibe Painel 1
  CALL METHOD painel_control->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = painel_1.
ENDFORM.

MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  "PERFORM make_docker.
  PERFORM make_alv.
ENDMODULE.
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
MODULE user_command_0200 INPUT.
  PERFORM action_process.
ENDMODULE.
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.
