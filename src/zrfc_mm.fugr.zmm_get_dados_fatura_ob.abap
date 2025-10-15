FUNCTION zmm_get_dados_fatura_ob.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CPUDT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_CPUDT_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZMME_DADOS_FATURA_OUT
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_BKPF_REF,
            AWKEY TYPE BKPF-AWKEY,
         END OF TY_BKPF_REF.

  DATA: lra_cpudt TYPE RANGE OF cpudt,
        lra_awkey TYPE RANGE OF awkey,
        lv_awkey  TYPE bkpf-awkey,
        LIT_AWKEY TYPE TABLE OF TY_BKPF_REF.

  IF i_cpudt_ini IS NOT INITIAL OR i_cpudt_fim IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'BT' low = i_cpudt_ini high = i_cpudt_fim ) TO lra_cpudt.
  ENDIF.

  SELECT DISTINCT
        forn~name1 AS nome_fornecedor,
        forn~stkzn,
        forn~stcd1,
        forn~stcd2,
        miro~belnr AS nro_doc_sap,
        miro~gjahr AS ano_doc_sap,
        miro~budat AS dt_doc_miro,
        miro~bldat AS dt_lac_miro,
        miro~xblnr AS nro_referencia,
        miro~bukrs AS bukrs_miro,
        miro~gsber,
        miro~waers AS moeda,
        miro~rmwwr AS valor,
        miro~lifnr,
        miro~bukrs,
        miro~stblg,
        pedi~ebeln AS nro_pedido,
        pedi~ebelp as item_pedido,
        pedi~banfn as nro_requisicao,
        mirs~buzei AS item_doc_sap
        FROM rbkp AS miro INNER JOIN lfa1 AS forn ON miro~lifnr = forn~lifnr
                          left outer JOIN rseg AS mirs ON miro~belnr = mirs~belnr
                                                 and miro~gjahr = mirs~gjahr
                          LEFT outer join ekpo as PEDI on mirs~ebeln = pedi~ebeln
                                                      and mirs~ebelp = pedi~ebelp
    INTO TABLE @DATA(lt_dados)
       WHERE miro~cpudt IN @lra_cpudt.

  DELETE lt_dados WHERE STBLG is NOT INITIAL.

  IF lt_dados[] IS not INITIAL.

    DATA(lt_dados_aux) = lt_dados.
    SORT lt_dados_aux BY nro_doc_sap ano_doc_sap.
    DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING nro_doc_sap ano_doc_sap.

    SELECT belnr, gjahr , docnum, model, series, nfnum, nfenum, nfe, branch , bukrs
      FROM j_1bnfdoc
      INTO TABLE @DATA(lt_j1bnfdoc)
      FOR ALL ENTRIES IN @lt_dados_aux
      WHERE bukrs = @lt_dados_aux-bukrs
        and belnr = @lt_dados_aux-nro_doc_sap
        AND gjahr = @lt_dados_aux-ano_doc_sap.
    IF sy-subrc IS INITIAL.
      SORT lt_j1bnfdoc BY belnr gjahr.

      data(lt_j1bnfdoc_aux) = lt_j1bnfdoc.
      SORT lt_j1bnfdoc_aux BY docnum.
      DELETE ADJACENT DUPLICATES FROM lt_j1bnfdoc_aux COMPARING docnum.

      SELECT regio,
             nfyear,
             nfmonth,
             stcd1,
             model,
             serie,
             nfnum9,
             docnum9,
             docnum,
             cdv
      FROM j_1bnfe_active
        INTO TABLE @DATA(lt_j1bnfe)
      FOR ALL ENTRIES IN @lt_j1bnfdoc_aux
      WHERE docnum = @lt_j1bnfdoc_aux-docnum.
      IF sy-subrc IS INITIAL.
        SORT lt_j1bnfe BY docnum.
      ENDIF.

    ENDIF.

    lt_dados_aux = lt_dados.
    SORT lt_dados_aux BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING lifnr.

    SELECT lifnr,name1,stkzn,stcd1,stcd2
      FROM lfa1
      INTO TABLE @DATA(lt_lfa1)
      FOR ALL ENTRIES IN @lt_dados_aux
      WHERE lifnr = @lt_dados_aux-lifnr.
    IF sy-subrc IS INITIAL.
      SORT lt_lfa1 BY lifnr.
    ENDIF.

*    lt_dados_aux = lt_dados.
*    SORT lt_dados_aux BY nro_doc_sap ano_doc_sap.
*    DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING nro_doc_sap ano_doc_sap.
*
*    SELECT belnr, gjahr, ebeln, ebelp
*      FROM rseg
*      INTO TABLE @DATA(lt_rseg)
*      FOR ALL ENTRIES IN @lt_dados_aux
*      WHERE belnr = @lt_dados_aux-nro_doc_sap
*        AND gjahr = @lt_dados_aux-ano_doc_sap.
*    IF sy-subrc IS INITIAL.
*      SORT lt_rseg BY belnr gjahr.
*
*      DATA(lt_rseg_aux) = lt_rseg.
*      SORT lt_rseg_aux BY ebeln ebelp.
*      DELETE ADJACENT DUPLICATES FROM lt_rseg_aux COMPARING ebeln ebelp.
*
*      SELECT ebeln, ebelp, banfn
*        FROM ekpo
*        INTO TABLE @DATA(lt_ekpo)
*        FOR ALL ENTRIES IN @lt_rseg_aux
*        WHERE ebeln = @lt_rseg_aux-ebeln
*          AND ebelp = @lt_rseg_aux-ebelp.
*      IF sy-subrc IS INITIAL.
*        SORT lt_ekpo BY ebeln ebelp.
*      ENDIF.
*
*    ENDIF.

    lt_dados_aux = lt_dados.
    SORT lt_dados_aux BY nro_doc_sap ano_doc_sap.
    DELETE ADJACENT DUPLICATES FROM lt_dados_aux COMPARING nro_doc_sap ano_doc_sap.

    LOOP AT lt_dados_aux ASSIGNING FIELD-SYMBOL(<fs_dados_aux>).

      APPEND INITIAL LINE TO LIT_AWKEY ASSIGNING FIELD-SYMBOL(<FS_REF_AWKEY>).

      <FS_REF_AWKEY>-awkey = <fs_dados_aux>-nro_doc_sap && <fs_dados_aux>-ano_doc_sap.

    ENDLOOP.

    IF LIT_AWKEY[] IS  NOT INITIAL.
      SELECT DISTINCT pf~awkey, si~zlspr
        FROM bkpf AS pf INNER JOIN bsik AS si
          ON pf~bukrs = si~bukrs
         AND pf~belnr = si~belnr
         AND pf~gjahr = si~gjahr
         INTO TABLE @DATA(lt_bkpf)
        FOR ALL ENTRIES IN @LIT_AWKEY
        WHERE pf~awtyp = 'RMRP'
          AND pf~awkey EQ @LIT_AWKEY-awkey
          AND si~zlspr <> ''.
      IF sy-subrc IS INITIAL.
        SORT lt_bkpf BY awkey.
      ENDIF.
    ENDIF.


    LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).
      APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

      MOVE-CORRESPONDING <fs_dados> TO <fs_saida>.

      READ TABLE lt_j1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j1bnfdoc>)
      WITH KEY belnr = <fs_dados>-nro_doc_sap
               gjahr = <fs_dados>-ano_doc_sap
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_saida>-docnum = <fs_j1bnfdoc>-docnum.
        <fs_saida>-model  = <fs_j1bnfdoc>-model .
        <fs_saida>-series = <fs_j1bnfdoc>-series.

        IF <fs_j1bnfdoc>-bukrs IS INITIAL.
          <fs_saida>-empresa = <fs_dados>-bukrs_miro.
        ELSE.
          <fs_saida>-empresa = <fs_j1bnfdoc>-bukrs.
        ENDIF.

        IF <fs_j1bnfdoc>-branch IS INITIAL.
          <fs_saida>-filial = <fs_dados>-gsber.
        ELSE.
          <fs_saida>-filial = <fs_j1bnfdoc>-branch.
        ENDIF.

        IF <fs_j1bnfdoc>-nfe EQ space.
          <fs_saida>-nfnum = <fs_j1bnfdoc>-nfnum.
        ELSEIF <fs_j1bnfdoc>-nfe EQ 'X'.
          <fs_saida>-nfnum = <fs_j1bnfdoc>-nfenum.

          READ TABLE lt_j1bnfe ASSIGNING FIELD-SYMBOL(<fs_j1bnfe>)
          WITH KEY docnum = <fs_j1bnfdoc>-docnum
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            CONCATENATE <fs_j1bnfe>-regio <fs_j1bnfe>-nfyear <fs_j1bnfe>-nfmonth <fs_j1bnfe>-stcd1 <fs_j1bnfe>-model <fs_j1bnfe>-serie <fs_j1bnfe>-nfnum9 <fs_j1bnfe>-docnum9
                        <fs_j1bnfe>-cdv INTO <fs_saida>-chavenfe.
          ENDIF.

        ENDIF.

      ENDIF.

      READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
      WITH KEY lifnr = <fs_dados>-lifnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF <fs_lfa1>-stkzn EQ space.
          <fs_saida>-cnpj_cpf = <fs_lfa1>-stcd1.
        ELSEIF <fs_lfa1>-stkzn EQ 'X'.
          <fs_saida>-cnpj_cpf = <fs_lfa1>-stcd2.
        ENDIF.
      ENDIF.

*      READ TABLE lt_rseg ASSIGNING FIELD-SYMBOL(<fs_rseg>)
*      WITH KEY belnr = <fs_dados>-nro_doc_sap
*               gjahr = <fs_dados>-ano_doc_sap
*      BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        <fs_saida>-nro_pedido = <fs_rseg>-ebeln.
*
*        READ TABLE lt_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>)
*        WITH KEY ebeln = <fs_rseg>-ebeln
*                 ebelp = <fs_rseg>-ebelp
*        BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          <fs_saida>-nro_requisicao = <fs_ekpo>-banfn.
*        ENDIF.
*
*      ENDIF.

      lv_awkey = <fs_dados>-nro_doc_sap && <fs_dados>-ano_doc_sap.

      READ TABLE lt_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf>)
      WITH KEY awkey = lv_awkey
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_saida>-chave_bloqueio = <fs_bkpf>-zlspr.
      ENDIF.

    ENDLOOP.

  ENDIF.

  sort t_saida.
  delete ADJACENT DUPLICATES FROM t_saida COMPARING ALL FIELDS.

ENDFUNCTION.
