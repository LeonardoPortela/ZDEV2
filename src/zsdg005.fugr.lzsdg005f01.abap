*----------------------------------------------------------------------*
***INCLUDE LZSDG005F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saida USING p_waerks TYPE zrsdsselopts.

  REFRESH it_saida_sd.

  DATA: x_saldo       TYPE vbap-zmeng,
        ntgewaux      TYPE bstmg,
        sac           TYPE vbap-vrkme,
        wl_xconversor TYPE konv-kwert,
        aux(100)      TYPE c.

  DATA: lc_safra          TYPE ajahr,
        lc_cultura        TYPE acc_txtlg,
        lc_doc_simulacao  TYPE zsded003,
        lc_safra_apl      TYPE zsdt0041-safra_apl,
        lc_cultura_apl    TYPE zsdt0041-cultura_apl,
        vl_vbeln_exec     TYPE vbap-vbeln,
        vl_cont_exec      TYPE i,
        vl_achou_frete    TYPE char1,
        vl_vbeln_frete    TYPE vbap-vbeln,
        vl_posnr_frete    TYPE vbap-posnr,
        vl_matnr_frete    TYPE vbap-matnr,
        vl_achou_safra    TYPE char1,
        vl_cpf            TYPE kna1-stcd1,
        vl_vbeln_safra    TYPE vbap-vbeln,
        vl_posnr_safra    TYPE vbap-posnr,
        wa_zsdt0041_safra TYPE zsdt0041,
        wa_zsdt0090_safra TYPE zsdt0090,
        wa_zsdt0041_frete TYPE zsdt0041,
        wa_zsdt0090_frete TYPE zsdt0090,
        wa_konv_imp       TYPE ty_konv.

  SORT: it_vbap      BY vbeln,
        it_vbfa_tot  BY vbelnrfmg posnn,
        it_vbfa_tot2 BY vbelnrfmg posnn,
        it_kna1      BY kunnr,
        it_konv      BY knumv kposn,
        it_konv_imp  BY knumv kposn,
        it_vbkd      BY vbeln,
        it_mara      BY matnr,
        it_tvzbt     BY zterm,
        it_vbrk      BY vbeln.

  SORT it_bsad BY bukrs belnr gjahr augdt ASCENDING.

  LOOP AT it_vbak INTO wa_vbak.

    CLEAR : wa_vbfa,wa_vbap,wa_kna1, wa_saida_sd, wa_vbfa_tot, wl_xconversor, lc_safra, lc_cultura.

    READ TABLE it_zsdt0041 WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      lc_doc_simulacao = it_zsdt0041-doc_simulacao.
      READ TABLE it_zsdt0040 WITH KEY doc_simulacao = it_zsdt0041-doc_simulacao BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lc_safra       = it_zsdt0040-safra.
        READ TABLE it_zsdt0038 WITH KEY cultura = it_zsdt0040-cultura BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lc_cultura   = it_zsdt0038-descricao.
        ENDIF.
      ENDIF.
    ENDIF.


    LOOP AT it_zsdt0274.
      TRANSLATE it_zsdt0274-cultura TO UPPER CASE.
      MODIFY it_zsdt0274.
      CLEAR  it_zsdt0274.
    ENDLOOP.

    TRANSLATE lc_cultura TO UPPER CASE.

    READ TABLE it_zsdt0275 WITH KEY doc_simulacao = lc_doc_simulacao.
    IF sy-subrc = 0.
      READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0275-nr_proposta
                                       safra       = lc_safra
                                       cultura     = lc_cultura.
      IF sy-subrc = 0.
        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
        CLEAR: vl_cpf.

        IF wa_kna1-stcd1 IS NOT INITIAL.
          vl_cpf = wa_kna1-stcd1.
        ELSE.
          vl_cpf = wa_kna1-stcd2.
        ENDIF.
        CLEAR: wa_kna1.

        READ TABLE it_zsdt0273 WITH KEY nr_proposta = it_zsdt0274-nr_proposta
                                        cpf_cnpj    = vl_cpf .

        IF sy-subrc = 0.
          LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0273-nr_proposta.
            IF wa_saida_sd-nrpropopr IS INITIAL.
              wa_saida_sd-nrpropopr = it_zsdt0272-nr_proposta.
            ELSE.
              CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrpropopr
                INTO wa_saida_sd-nrpropopr SEPARATED BY ','.
            ENDIF.

            IF wa_saida_sd-alcpropopr IS INITIAL.
              wa_saida_sd-alcpropopr = it_zsdt0272-estagio.
            ELSE.
              CONCATENATE it_zsdt0272-estagio wa_saida_sd-alcpropopr
              INTO wa_saida_sd-alcpropopr SEPARATED BY ','.
            ENDIF.

            IF wa_saida_sd-efetivada IS INITIAL.
              wa_saida_sd-efetivada = it_zsdt0272-efetivada.
            ELSE.
              CONCATENATE it_zsdt0272-efetivada wa_saida_sd-efetivada
              INTO wa_saida_sd-efetivada SEPARATED BY ','.
            ENDIF.

            IF wa_saida_sd-nrpropopr IS NOT INITIAL.
              IF wa_saida_sd-nrproplmt  IS INITIAL.
                wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta_ref.
              ELSE.
                CONCATENATE it_zsdt0272-nr_proposta_ref wa_saida_sd-nrproplmt
                INTO wa_saida_sd-nrproplmt SEPARATED BY ','.
              ENDIF.
              IF wa_saida_sd-alcproplmt IS INITIAL.
                wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
              ELSE.
                CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
                INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
              ENDIF.
            ELSE.

              READ TABLE it_zsdt0273 WITH KEY cpf_cnpj    = vl_cpf .
              IF sy-subrc IS INITIAL.
                READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0273-nr_proposta
                                 safra       = lc_safra
                                 cultura     = lc_cultura .
                IF sy-subrc = 0.
                  LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0274-nr_proposta
                    AND tp_proposta = '1'.
                    IF wa_saida_sd-nrproplmt IS INITIAL.
                      wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta.
                    ELSE.
                      CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrproplmt INTO
                      wa_saida_sd-nrproplmt SEPARATED BY ','.
                    ENDIF.

                    IF wa_saida_sd-alcproplmt IS INITIAL.
                      wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
                    ELSE.
                      CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
                      INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_saida_sd-nrpropopr IS INITIAL.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
      CLEAR: vl_cpf.

      IF wa_kna1-stcd1 IS NOT INITIAL.
        vl_cpf = wa_kna1-stcd1.
      ELSE.
        vl_cpf = wa_kna1-stcd2.
      ENDIF.
      CLEAR: wa_kna1.

      READ TABLE it_zsdt0273 WITH KEY cpf_cnpj    = vl_cpf .
      IF sy-subrc IS INITIAL.

        READ TABLE it_zsdt0274 WITH KEY nr_proposta = it_zsdt0273-nr_proposta
                 safra       = lc_safra
                 cultura     = lc_cultura .
        IF sy-subrc = 0.
          LOOP AT it_zsdt0272 WHERE  nr_proposta = it_zsdt0274-nr_proposta
           AND tp_proposta = '1'.
            IF wa_saida_sd-nrproplmt IS INITIAL.
              wa_saida_sd-nrproplmt = it_zsdt0272-nr_proposta.
            ELSE.
              CONCATENATE it_zsdt0272-nr_proposta wa_saida_sd-nrproplmt INTO
              wa_saida_sd-nrproplmt SEPARATED BY ','.
            ENDIF.

            IF wa_saida_sd-alcproplmt IS INITIAL.
              wa_saida_sd-alcproplmt = it_zsdt0272-estagio.
            ELSE.
              CONCATENATE it_zsdt0272-estagio  wa_saida_sd-alcproplmt
              INTO wa_saida_sd-alcproplmt SEPARATED BY ','.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT it_vbap INTO wa_vbap WHERE vbeln EQ wa_vbak-vbeln.

      wa_saida_sd-j_1bcfop  = wa_vbap-j_1bcfop.
      wa_saida_sd-auart     = wa_vbak-auart.
      wa_saida_sd-j_1btxsdc = wa_vbap-j_1btxsdc.

      READ TABLE it_konv_imp INTO wa_konv_imp WITH KEY knumv = wa_vbak-knumv
                                                       kposn = wa_vbap-posnr BINARY SEARCH.

      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv
                                               kposn = wa_vbap-posnr BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        IF wa_konv-kmein NE wa_konv_imp-kmein.
          IF wa_konv_imp-kmein EQ 'TO' AND wa_konv-kmein EQ 'KG'.
            IF wa_vbap-kwmeng IS INITIAL.
              wa_saida_sd-kbetr2 = 0.
            ELSE.
              wa_saida_sd-kbetr2 = wa_konv_imp-kwert / ( wa_vbap-kwmeng * 1000 ).
            ENDIF.
          ELSEIF wa_konv_imp-kmein EQ 'KG' AND wa_konv-kmein EQ 'TO'.
            IF wa_vbap-kwmeng IS INITIAL.
              wa_saida_sd-kbetr2 = 0.
            ELSE.
              wa_saida_sd-kbetr2 = wa_konv_imp-kwert / ( wa_vbap-kwmeng / 1000 ).
            ENDIF.
          ENDIF.
        ELSE.
          wa_saida_sd-kbetr2 = wa_konv_imp-kbetr.
        ENDIF.
      ENDIF.

      CLEAR: wa_konv.

      CLEAR: vl_vbeln_frete, vl_posnr_frete, vl_matnr_frete, vl_achou_frete.
      vl_vbeln_exec = wa_vbap-vbeln.
      vl_cont_exec = 0.

      vl_vbeln_frete = wa_vbap-vbeln.
      vl_posnr_frete = wa_vbap-posnr.
      vl_matnr_frete = wa_vbap-matnr.

      WHILE vl_achou_frete IS INITIAL.

        READ TABLE it_zsdt0041_frete INTO wa_zsdt0041_frete WITH KEY vbeln = vl_vbeln_frete
                                                                     matnr = vl_matnr_frete.
        IF sy-subrc IS INITIAL.
          wa_saida_sd-vlr_frete = wa_zsdt0041_frete-vlr_frete.
          vl_achou_frete = abap_true.
        ELSE.
          READ TABLE it_zsdt0090_frete INTO wa_zsdt0090_frete WITH KEY vbeln = vl_vbeln_frete
                                                                       posnn = vl_posnr_frete.
          IF sy-subrc IS INITIAL.
            vl_vbeln_frete = wa_zsdt0090_frete-vbelv.
            vl_posnr_frete = wa_zsdt0090_frete-posnv.
            vl_matnr_frete = wa_zsdt0090_frete-matnrv.
          ELSE.
            "Não tem ordem nem na 0090 nem na 0041  --> Mostra frete em Branco
            wa_saida_sd-vlr_frete = space.
            vl_achou_frete = abap_true.
          ENDIF.
        ENDIF.

      ENDWHILE.

      CLEAR: vl_vbeln_safra,
             vl_posnr_safra,
             lc_safra_apl,
             lc_cultura_apl,
             vl_achou_safra.

      vl_vbeln_safra = wa_vbap-vbeln.
      vl_posnr_safra = wa_vbap-posnr.

      WHILE vl_achou_safra IS INITIAL.

        READ TABLE it_zsdt0041_safra INTO wa_zsdt0041_safra WITH KEY vbeln = vl_vbeln_safra.
        IF sy-subrc IS INITIAL.
          lc_safra_apl     = wa_zsdt0041_safra-safra_apl.
          lc_cultura_apl   = wa_zsdt0041_safra-cultura_apl.
          vl_achou_safra = abap_true.
        ELSE.

          READ TABLE it_zsdt0090_safra INTO wa_zsdt0090_safra WITH KEY vbeln = vl_vbeln_safra
                                                                       posnn = vl_posnr_safra.
          IF sy-subrc IS INITIAL.
            vl_vbeln_safra = wa_zsdt0090_safra-vbelv.
            vl_posnr_safra = wa_zsdt0090_safra-posnv.
          ELSE.
            vl_achou_safra = abap_true.
          ENDIF.
        ENDIF.
      ENDWHILE.

      wa_saida_sd-safra_apl   =  lc_safra_apl.
      wa_saida_sd-cultura_apl =  lc_cultura_apl.


      LOOP AT it_zsdt0082 WHERE vbeln EQ wa_vbap-vbeln
                           AND  posnr = wa_vbap-posnr.
        wa_saida_sd-qte_sol = wa_saida_sd-qte_sol + it_zsdt0082-qte_sol.

      ENDLOOP.

      wa_saida_sd-erdat = wa_vbak-erdat.

      CLEAR: wa_saida_sd-status.

      wa_saida_sd-safra         = lc_safra.
      wa_saida_sd-cultura       = lc_cultura.
      wa_saida_sd-doc_simulacao = lc_doc_simulacao.

      LOOP AT it_0026 WHERE vbeln EQ wa_vbap-vbeln.

        IF it_0026-docnum = '0000000000'.
          READ TABLE it_zib WITH  KEY obj_key = it_0026-obj_key.
          READ TABLE it_zib_bsid WITH KEY belnr = it_zib-belnr.
        ELSE.
          READ TABLE it_zib_bsid WITH KEY belnr = it_0026-docnum.
        ENDIF.

        READ TABLE it_bsid WITH KEY bukrs = it_zib_bsid-bukrs
                            belnr = it_zib_bsid-belnr
                            gjahr = it_zib_bsid-gjahr.
        IF sy-subrc IS INITIAL.
          IF wa_saida_sd-status EQ 'Q'.
            wa_saida_sd-status = 'P'.
            sy-subrc = 0.
            CONTINUE.
          ELSE.
            sy-subrc = 8.
          ENDIF.

        ELSE.
          wa_saida_sd-status = 'Q'.
          sy-subrc = 0.
*       continue.
        ENDIF .

        READ TABLE it_bsad WITH KEY bukrs = it_zib_bsid-bukrs
                                    belnr = it_zib_bsid-belnr
                                    gjahr = it_zib_bsid-gjahr.
        IF sy-subrc IS INITIAL.
          wa_saida_sd-augdt = it_bsad-augdt .
        ENDIF.

      ENDLOOP.

      IF sy-subrc IS NOT INITIAL.
        wa_saida_sd-status = 'A'.
      ENDIF.

      READ TABLE it_vbkd_aux WITH KEY vbeln = wa_vbap-vbeln.

      IF sy-subrc IS INITIAL .
        wa_saida_sd-inco1 = it_vbkd_aux-inco1.
        wa_saida_sd-inco2 = it_vbkd_aux-inco2.
        wa_saida_sd-kurrf = it_vbkd_aux-kurrf.

        READ TABLE it_t052u WITH KEY zterm = it_vbkd_aux-zterm.
        IF sy-subrc IS INITIAL.
          CONCATENATE it_vbkd_aux-zterm '-' it_t052u-text1 INTO wa_saida_sd-zterm SEPARATED BY space.
        ENDIF.

      ENDIF.

      READ TABLE it_tvzbt INTO wa_tvzbt WITH KEY zterm = it_vbkd_aux-zterm.

      wa_saida_sd-vtext = wa_tvzbt-vtext.

      READ TABLE it_vbfa_tot INTO wa_vbfa_tot WITH KEY vbelnrfmg = wa_vbap-vbeln
                                                       posnn = wa_vbap-posnr   BINARY SEARCH.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.

      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv
                                               kposn = wa_vbap-posnr BINARY SEARCH.

      IF p_waerks IS NOT INITIAL.
        DATA(vl_waerks) = p_waerks[ 1 ]-low.
        IF wa_konv-waers <> vl_waerks.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE it_vbep INTO wa_vbep WITH KEY vbeln = wa_vbap-vbeln
                                               posnr = wa_vbap-posnr
                                               etenr = 1.

      READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      aux = ''.

      CLEAR: x_saldo.

      wa_saida_sd-lifsp = wa_vbep-lifsp.

      wa_saida_sd-name1 = wa_kna1-name1. " Cliente
      wa_saida_sd-vkbur = wa_vbak-vkbur. " Escr.Vendas

      READ TABLE it_tvbur WITH KEY vkbur =  wa_vbak-vkbur.
      IF sy-subrc = 0.
        READ TABLE it_tvkbt WITH KEY vkbur = it_tvbur-vkbur.
        wa_saida_sd-bezei = it_tvkbt-bezei.
      ENDIF.
      READ TABLE it_zsdt0271 WITH KEY filial =  wa_vbak-vkbur.
      IF sy-subrc = 0.
        READ TABLE it_zsdt0270 WITH KEY cod_regional = it_zsdt0271-cod_regional.
        wa_saida_sd-regional = it_zsdt0270-regional.
      ENDIF.

      IF wa_vbak-lifsk IS NOT INITIAL.
        READ TABLE it_tvls WITH KEY lifsp = wa_vbak-lifsk.
        READ TABLE it_tvlst WITH KEY lifsp = it_tvls-lifsp.
        wa_saida_sd-lifsp_t = it_tvlst-vtext.
      ENDIF.

      IF wa_vbak-faksk IS NOT INITIAL.
        READ TABLE it_tvfs  WITH  KEY faksp = wa_vbak-lifsk.
        READ TABLE it_tvfst WITH  KEY faksp = it_tvfs-faksp.
        wa_saida_sd-lifsp_t = it_tvfst-vtext.
      ENDIF.

      wa_saida_sd-vbeln = wa_vbak-vbeln. " Contrato
      wa_saida_sd-posnr = wa_vbap-posnr. " Item
      wa_saida_sd-erdat = wa_vbak-erdat. "Data criaçâo
      wa_saida_sd-waers = wa_konv-waers.
      wa_saida_sd-kbetr = wa_konv-kbetr.
      wa_saida_sd-valdt = wa_vbkd-valdt.
      wa_saida_sd-matnr = wa_vbap-matnr.
      wa_saida_sd-arktx = wa_vbap-arktx.

      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_vbap-matnr BINARY SEARCH.
      wa_saida_sd-wrkst = wa_mara-wrkst.

      IF ( wa_vbap-vrkme EQ 'BAG' ).
        wa_saida_sd-qtd  = wa_vbap-kwmeng.
        wa_saida_sd-unid = 'SAC'.
      ELSE.
        wa_saida_sd-qtd  = wa_vbap-kwmeng.
        wa_saida_sd-unid = wa_vbap-vrkme.
      ENDIF.

      wa_saida_sd-qtd = wa_saida_sd-qtd - wa_vbfa_tot-totaldim.
      READ TABLE it_vbfa_tot2 WITH KEY vbelnrfmg = wa_vbap-vbeln
                                        posnn     = wa_vbap-posnr   BINARY SEARCH.

      SUBTRACT it_vbfa_tot2-totalmenge FROM wa_saida_sd-qtd.

      wa_saida_sd-qtefaturado =  wa_vbfa_tot-totalmenge.
      x_saldo =  wa_saida_sd-qtd - wa_saida_sd-qtefaturado.
      wa_saida_sd-saldo =  x_saldo.
      READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv
                                               kposn = wa_vbap-posnr.
      TRY.
          IF wa_konv-kmein NE wa_vbap-vrkme.
            IF  wa_konv-kmein EQ 'TO'
            AND wa_vbap-vrkme EQ 'KG'.
              wa_saida_sd-vlr_qtecont = ( wa_saida_sd-qtd / 1000 ) * wa_saida_sd-kbetr .
              wa_saida_sd-vlr_saldo   = ( wa_saida_sd-saldo / 1000 ) * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_qtefat  = ( wa_saida_sd-qtefaturado / 1000 ) * wa_saida_sd-kbetr.

            ELSEIF wa_konv-kmein EQ 'KG'
            AND    wa_vbap-vrkme EQ 'TO'.
              wa_saida_sd-vlr_qtecont = ( wa_saida_sd-qtd * 1000 ) * wa_saida_sd-kbetr .
              wa_saida_sd-vlr_saldo   = ( wa_saida_sd-saldo * 1000 ) * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_qtefat  = ( wa_saida_sd-qtefaturado * 1000 ) * wa_saida_sd-kbetr.

            ELSE.
              wa_saida_sd-vlr_qtecont = wa_saida_sd-qtd * wa_saida_sd-kbetr .
              wa_saida_sd-vlr_saldo   = wa_saida_sd-saldo * wa_saida_sd-kbetr.
              wa_saida_sd-vlr_qtefat  = wa_saida_sd-qtefaturado * wa_saida_sd-kbetr.

            ENDIF.

          ELSE.
            wa_saida_sd-vlr_qtecont = wa_saida_sd-qtd * wa_saida_sd-kbetr .
            wa_saida_sd-vlr_saldo   = wa_saida_sd-saldo * wa_saida_sd-kbetr.
            wa_saida_sd-vlr_qtefat  = wa_saida_sd-qtefaturado * wa_saida_sd-kbetr.

          ENDIF.

        CATCH cx_root.
      ENDTRY.

      READ TABLE it_vbep2 WITH KEY vbeln = wa_vbak-vbeln
                                   posnr = wa_vbap-posnr
                                   lifsp = '12'.
      IF sy-subrc IS INITIAL.
        wa_saida_sd-qtd = 0.
      ENDIF.
      IF wa_saida_sd-qtd GT 0.
        APPEND wa_saida_sd TO it_saida_sd.
      ENDIF.

      CLEAR: wa_vbfa_tot,
             wa_vbfa_tot-totalmenge,
             it_vbfa_tot2-totalmenge,
             wa_vbap,
             wa_vbfa,
             wa_kna1,
             wa_mara,
             wa_tvzbt,
             wa_saida_sd.

    ENDLOOP.

    CLEAR: wa_vbap,
           wa_vbfa,
           wa_kna1,
           wa_konv,
           wa_vbkd,
           wa_saida_sd.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM remessa .

  DATA: bel_refkey TYPE j_1bnflin-refkey,
        vrefkey    TYPE j_1bnflin-refkey.

  DATA: wa_lin TYPE j_1bnflin,
        it_lin TYPE TABLE OF j_1bnflin,
        wa_doc TYPE j_1bnfdoc.



  SORT: it_rbkp      BY belnr,
        it_j_1bnflin BY refkey,
        it_j_1bnfdoc BY docnum.

  CLEAR : wa_saida_sd2,
                  it_saida_sd2,
                  it_fcat2.

  DATA: day(2)    TYPE c,
        month(2)  TYPE c,
        year(4)   TYPE c,
        dta(8)    TYPE c,
        vg_refkey TYPE j_1bnflin-refkey,
        it_vbfa_2 TYPE STANDARD TABLE OF vbfa,
        wa_vbfa_2 TYPE vbfa.

  SORT:
        it_j_1bnflin BY refkey itmnum,
        it_j_1bnfdoc BY docnum,
        it_vbfa BY vbelv posnn,
        it_vbrk      BY vbeln.

  CHECK it_vbfa IS NOT INITIAL.

  SELECT *
    FROM vbfa
    INTO TABLE it_vbfa_2
    FOR ALL ENTRIES IN it_vbfa
    WHERE vbeln EQ it_vbfa-vbeln.

  LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv EQ vl_vbeln AND posnv EQ vl_posnr. " Dados da Fatura

    READ TABLE it_vbrk INTO wa_vbrk WITH KEY vbeln = wa_vbfa-vbeln.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF wa_vbrk-fkart = 'ZTRI'.
      CONTINUE.
    ENDIF.

    wa_vbfa-vbeln = |{ wa_vbfa-vbeln ALPHA = IN }|.
    vg_refkey = wa_vbfa-vbeln.

    CLEAR: wa_lin, wa_doc.
    FREE it_lin.

    SELECT * FROM j_1bnflin INTO TABLE it_lin WHERE refkey EQ vg_refkey AND refitm EQ wa_vbfa-posnn.

    LOOP AT it_lin INTO wa_lin.

      SELECT SINGLE *
        FROM j_1bnfdoc
        INTO wa_doc
        WHERE docnum EQ wa_lin-docnum.

      IF NOT wa_doc-nfe = 'X'.
        MOVE wa_doc-nfnum TO wa_doc-nfenum.
        wa_doc-nfenum = |{ wa_doc-nfenum ALPHA = IN }|.
      ENDIF.

      CLEAR: wa_saida_sd2-vbeln3.

      IF wa_vbfa-vbtyp_n EQ 'O'.
        READ TABLE it_vbfa_2 INTO wa_vbfa_2 WITH KEY vbeln   = wa_lin-refkey
                                                     posnn   = wa_lin-refitm
                                                     vbtyp_n = wa_vbfa-vbtyp_n
                                                     vbtyp_v = 'H'.

        IF sy-subrc IS INITIAL.
          wa_saida_sd2-vbeln3  = wa_vbfa_2-vbelv.
        ENDIF.

      ELSEIF wa_vbfa-vbtyp_n EQ 'N'.

        READ TABLE it_vbfa_2 INTO wa_vbfa_2 WITH KEY vbeln   = wa_lin-refkey
                                                     posnn   = wa_lin-refitm
                                                     vbtyp_n = wa_vbfa-vbtyp_n
                                                     vbtyp_v = 'M'.

        IF sy-subrc IS INITIAL.
          wa_saida_sd2-vbeln3  = wa_vbfa_2-vbelv.
        ENDIF.

      ENDIF.

      wa_saida_sd2-name12  = wa_doc-name1.
      wa_saida_sd2-vbeln2  = wa_vbfa-vbelv.
      wa_saida_sd2-vbelnk  = wa_vbfa-vbeln.

      wa_saida_sd2-docnum2 = wa_lin-docnum.
      wa_saida_sd2-nfenum  = wa_doc-nfenum.

      wa_saida_sd2-vrkme   =  wa_doc-shpunt.

      day   = ''.
      month = ''.
      year  = ''.
      dta   = ''.

      day   =  wa_vbfa-erdat+6(2) .
      month =  wa_vbfa-erdat+4(2) .
      year  =  wa_vbfa-erdat(4) .

      dta = |{ year }{ month }{ day  }|.

      wa_saida_sd2-erdat2 =    dta.       "Dt.Emissão


      IF wa_vbfa-vbtyp_n = 'M'.

*        ADD wa_vbfa-rfwrt TO wa_saida_sd2-rfwrt2.       " Valor Nota
        ADD wa_lin-netwr TO wa_saida_sd2-rfwrt2.       " Valor Nota

        ADD wa_lin-menge  TO wa_saida_sd2-qtdfatur.     " Qte. Faturado

      ELSEIF ( wa_vbfa-vbtyp_n = 'N' ) OR ( wa_vbfa-vbtyp_n = 'O' ).

        MULTIPLY wa_lin-netwr BY -1.
*        ADD wa_vbfa_2-rfwrt TO wa_saida_sd2-rfwrt2.   " Valor Nota
        ADD wa_lin-netwr TO wa_saida_sd2-rfwrt2.   " Valor Nota

        MULTIPLY wa_lin-menge BY -1.
        ADD wa_lin-menge TO wa_saida_sd2-qtdfatur.  " Qte. Faturado

      ENDIF.

    ENDLOOP.

    APPEND wa_saida_sd2 TO it_saida_sd2.
    CLEAR: wa_saida_sd2.

  ENDLOOP.


ENDFORM.
