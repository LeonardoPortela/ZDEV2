FUNCTION z_sd_info_cte_avulso_zw.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     REFERENCE(P_IDENTIFICA) TYPE  ZCTE_IDENTIFICA
*"     REFERENCE(P_PARCEIROS) TYPE  ZCTE_PARCEIROS
*"  TABLES
*"      IT_NOTAS_INFO STRUCTURE  ZCTE_INFO_NOTA OPTIONAL
*"      IT_TRANS STRUCTURE  ZCTE_TRANS OPTIONAL
*"      IT_CIOT STRUCTURE  ZCTE_CIOT OPTIONAL
*"      IT_MOTORISTA STRUCTURE  ZCTE_MOTORISTA OPTIONAL
*"      IT_OBS_GERAL STRUCTURE  ZCTE_OBS_GERAIS OPTIONAL
*"      IT_SEGURO STRUCTURE  ZCTE_SEGURO OPTIONAL
*"  EXCEPTIONS
*"      DOC_NAO_ZW
*"----------------------------------------------------------------------

  DATA: it_j_1bnfnad TYPE TABLE OF j_1bnfnad,
        it_j_1bnfstx TYPE TABLE OF j_1bnfstx,
        wa_j_1bnfdoc TYPE j_1bnfdoc,
        wa_j_1bnflin TYPE j_1bnflin,
        vg_langu     TYPE sy-langu,
        wa_j_1bnfnad TYPE j_1bnfnad,
        wa_j_1bnfstx TYPE j_1bnfstx,
        wa_j_1bnfftx TYPE j_1bnfftx,
        wa_obs_geral TYPE zcte_obs_gerais,
        wa_kna1      TYPE kna1,
        wa_j1baj     TYPE j_1baj,
        lv_taxsit    TYPE char2,
        cidade       TYPE j_1btxjurt,
        emitente     TYPE zctet_endereco,
        p_iniciada   TYPE char01,
        taxjurcode   TYPE j_1btxjcd.

  SELECT SINGLE * INTO wa_j_1bnfdoc
    FROM j_1bnfdoc
   WHERE docnum EQ p_cte_avulso.

  SELECT SINGLE * INTO wa_j_1bnflin
    FROM j_1bnflin
   WHERE docnum EQ p_cte_avulso.

  IF wa_j_1bnflin-reftyp NE 'ZW'.
    MESSAGE e033 WITH p_cte_avulso RAISING doc_nao_zw.
  ENDIF.

  p_identifica-docnum  = p_cte_avulso.
  p_identifica-dhemi   = wa_j_1bnfdoc-credat.
  p_identifica-hremi   = wa_j_1bnfdoc-cretim.

  "Ini. CS2017002043 04.10.2017
  IF ( wa_j_1bnfdoc-docnum IS NOT INITIAL ).
    DATA: v_time_br TYPE erzet.
    CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
      EXPORTING
        i_bukrs  = wa_j_1bnfdoc-bukrs
        i_branch = wa_j_1bnfdoc-branch
      IMPORTING
        e_time   = v_time_br.
    IF v_time_br IS NOT INITIAL.
      p_identifica-hremi = v_time_br.
    ENDIF.
  ENDIF.
  "Fim. CS2017002043 04.10.2017

  p_identifica-serie   = wa_j_1bnfdoc-series.
  p_identifica-nct     = wa_j_1bnfdoc-nfenum.
  p_identifica-cfop    = wa_j_1bnflin-cfop.
  "0 - Pago; 1 - A pagar; 2 - Outros
  p_identifica-forpag  = c_0.
  "0 - CT-e Normal; 1 - CT-e de Complemento de Valores; 2 - CT-e de Anulação de Valores; 3 - CT-e Substituto
  p_identifica-tpcte   = c_0.
  "0 - Normal; 1 - Subcontratação; 2 - Redespacho; 3 - Redespacho Intermediário
  p_identifica-tpserv	 = c_0.
  "Indicador se o Recebedor retira no Aeroporto, Filial, Porto ou Estação de Destino? 0 - sim; 1 - não
  p_identifica-retira	 = c_1.
  "01-Rodoviário; 02-A éreo; 03-Aquaviário; 04-Ferroviário; 05-Dutoviário
  p_identifica-modal   = c_01.
  "0-Remetente; 1-Expedidor; 2-Recebedor; 3-Destinatário; 4-Outros;
  p_identifica-toma    = c_0.

  p_identifica-spras   = 'P'.
  p_identifica-country = 'BR'.
  p_identifica-waers   = wa_j_1bnfdoc-waerk.

  p_identifica-rodo_frete_lota = c_0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_j_1bnfdoc-branch
    IMPORTING
      output = p_identifica-emissor.

  p_identifica-rodo_dt_inicio = wa_j_1bnfdoc-docdat.
  p_identifica-rodo_dt_prev   = wa_j_1bnfdoc-docdat.

  SELECT SINGLE bahns INTO p_identifica-rodo_rntrc FROM lfa1 WHERE lifnr = p_identifica-emissor.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input            = 'PT'
    IMPORTING
      output           = vg_langu
    EXCEPTIONS
      unknown_language = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SELECT SINGLE cfotxt INTO p_identifica-cfotxt
    FROM j_1bagt
   WHERE spras EQ vg_langu
     AND cfop  EQ wa_j_1bnflin-cfop.

  SELECT * INTO TABLE it_j_1bnfnad
    FROM j_1bnfnad
   WHERE docnum EQ p_cte_avulso.

  CALL FUNCTION 'Z_XML_CTE_1_04'
    IMPORTING
      p_iniciada = p_iniciada.

  IF NOT p_iniciada IS INITIAL.

    CALL FUNCTION 'Z_SD_INFO_CIDADE_EMITE'
      EXPORTING
        wa_j_1bnfdoc = wa_j_1bnfdoc
      IMPORTING
        taxjurcode   = taxjurcode.

    IF NOT taxjurcode IS INITIAL.
      p_identifica-cmunenv = taxjurcode+3(7).
      p_identifica-ufenv   = taxjurcode(3).

      SELECT SINGLE * INTO cidade
        FROM j_1btxjurt
       WHERE spras      = p_identifica-spras
         AND country    = p_identifica-country
         AND taxjurcode = taxjurcode.

      IF sy-subrc IS INITIAL.
        p_identifica-nmunenv = cidade-text.
      ENDIF.
    ENDIF.
  ENDIF.

  "SP Origem
  READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'SP'.
  IF sy-subrc IS INITIAL.

    SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr = wa_j_1bnfnad-parid.

    p_identifica-cmunini = wa_j_1bnfnad-txjcd+3(7).
    p_identifica-ufini   = wa_j_1bnfnad-txjcd(3).

    SELECT SINGLE * INTO cidade
      FROM j_1btxjurt
     WHERE spras      = p_identifica-spras
       AND country    = p_identifica-country
       AND taxjurcode = wa_j_1bnfnad-txjcd.

    IF sy-subrc IS INITIAL.
      p_identifica-nmunini = cidade-text.
    ENDIF.

  ENDIF.

  "SH Destino
  READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'SH'.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE * INTO wa_kna1 FROM kna1 WHERE kunnr = wa_j_1bnfnad-parid.
    p_identifica-cmunfim = wa_j_1bnfnad-txjcd+3(7).
    p_identifica-uffim   = wa_j_1bnfnad-txjcd(3).
    SELECT SINGLE * INTO cidade
      FROM j_1btxjurt
     WHERE spras      = p_identifica-spras
       AND country    = p_identifica-country
       AND taxjurcode = wa_j_1bnfnad-txjcd.
    IF sy-subrc IS INITIAL.
      p_identifica-nmunfim = cidade-text.
    ENDIF.
  ENDIF.

  "Impostos ICMS Frete
  SELECT * INTO TABLE it_j_1bnfstx
    FROM j_1bnfstx
   WHERE docnum EQ wa_j_1bnflin-docnum
     AND itmnum EQ wa_j_1bnflin-itmnum.

  LOOP AT it_j_1bnfstx INTO wa_j_1bnfstx.

    CALL FUNCTION 'J_1BAJ_READ'
      EXPORTING
        taxtype              = wa_j_1bnfstx-taxtyp
      IMPORTING
        e_j_1baj             = wa_j1baj
      EXCEPTIONS
        not_found            = 1
        parameters_incorrect = 2
        OTHERS               = 3.

    IF wa_j1baj-taxgrp = 'ICMS'.
      CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
        EXPORTING
          input  = wa_j_1bnflin-taxsit
        IMPORTING
          output = lv_taxsit.

      p_identifica-cst        = lv_taxsit.
      p_identifica-vbc        = wa_j_1bnfstx-base.
      p_identifica-picms      = wa_j_1bnfstx-rate.
      p_identifica-vicms      = wa_j_1bnfstx-taxval.
      p_identifica-predbc     = wa_j_1bnfstx-basered1.
      p_identifica-vbcstret   = 0.
      p_identifica-vicmsstret = 0.
      p_identifica-picmsstret = 0.
      p_identifica-vcred      = 0.
    ELSEIF wa_j1baj-taxgrp = 'ICST'.
      CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
        EXPORTING
          input  = wa_j_1bnflin-taxsit
        IMPORTING
          output = lv_taxsit.

      p_identifica-cst        = lv_taxsit.
      p_identifica-vbc        = 0.
      p_identifica-picms      = 0.
      p_identifica-vicms      = 0.
      p_identifica-predbc     = 100 - wa_j_1bnfstx-basered1.
      p_identifica-vbcstret   = wa_j_1bnfstx-base.
      p_identifica-vicmsstret = wa_j_1bnfstx-taxval.
      p_identifica-picmsstret = wa_j_1bnfstx-rate.
      p_identifica-vcred      = 0.
    ENDIF.

  ENDLOOP.

  p_identifica-vtprest          = wa_j_1bnfdoc-nftot.
  p_identifica-vrec             = wa_j_1bnfdoc-nftot.
  p_identifica-vlr_seguro       = 0.
  p_identifica-vlr_combustivel  = 0.
  p_identifica-vlr_pedagio      = 0.
  p_identifica-vlr_impostos     = 0.
  p_identifica-vlr_inss         = 0.
  p_identifica-vlr_iss          = 0.
  p_identifica-vlr_sest         = 0.
  p_identifica-vlr_irpf         = 0.
  p_identifica-vlr_iof          = 0.
  p_identifica-vlr_adiantamento = 0.
  p_identifica-frete_lotacao    = space.

*  p_identifica-vlr_unit_frete   = wa_komv-kbetr.
*  p_identifica-unid_vlr_frete   = wa_komv-kmein.
  p_identifica-tp_fornecimento  = space.
  p_identifica-dc_fornecimento  = space.

**********************************************************************************************
**********************************************************************************************
** Parceiros

  p_parceiros-docnum = p_cte_avulso.

  "Busca Emissor do Conhecimento
  SELECT SINGLE a~name1   j~state_insc j~stcd1  j~stcd2
                a~city2   a~post_code1 a~street a~house_num1
                a~country a~langu      a~region a~taxjurcode
                a~tel_number
           INTO emitente
    FROM j_1bbranch AS j
   INNER JOIN adrc AS a ON a~addrnumber = j~adrnr
   WHERE bukrs  = wa_j_1bnfdoc-bukrs
     AND branch = wa_j_1bnfdoc-branch.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_j_1bnfdoc-branch
      IMPORTING
        output = p_parceiros-emit_codigo.

    p_parceiros-emit_cnpj    = emitente-stcd1.
    p_parceiros-emit_ie      = emitente-state_insc.
    p_parceiros-emit_xnome   = emitente-name1.
    p_parceiros-emit_xfant   = emitente-name1.
    p_parceiros-emit_xlgr    = emitente-street(25).
    IF emitente-house_num1 IS INITIAL.
      p_parceiros-emit_nro     = c_sn.
    ELSE.
      p_parceiros-emit_nro     = emitente-house_num1.
    ENDIF.
    "p_parceiros-emit_xcpl    = emitente-.
    p_parceiros-emit_xbairro = emitente-city2.
    p_parceiros-emit_uf      = emitente-taxjurcode(3).
    p_parceiros-emit_cmun    = emitente-taxjurcode+3(7).

    SELECT SINGLE * INTO cidade
      FROM j_1btxjurt
     WHERE spras      = emitente-langu
       AND country    = emitente-country
       AND taxjurcode = emitente-taxjurcode.

    IF sy-subrc IS INITIAL.
      p_parceiros-emit_xmun    = cidade-text.
    ENDIF.
    p_parceiros-emit_cep     = emitente-post_code1.
    p_parceiros-emit_uf      = emitente-region.
    p_parceiros-emit_fone    = emitente-tel_number.

  ENDIF.

  "Remetente da Mercadoria
  READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'SP'.
  IF sy-subrc IS INITIAL.

    CLEAR: emitente.

    "Busca Emissor do Conhecimento
    SELECT SINGLE a~name1   j~stcd3      j~stcd1  j~stcd2
                  a~city2   a~post_code1 a~street a~house_num1
                  a~country a~langu      a~region a~taxjurcode
                  a~tel_number
             INTO emitente
      FROM kna1 AS j
     INNER JOIN adrc AS a ON a~addrnumber = j~adrnr
     WHERE j~kunnr EQ wa_j_1bnfnad-parid.

    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_j_1bnfdoc-parid
        IMPORTING
          output = p_parceiros-reme_codigo.

      p_parceiros-reme_cnpj    = emitente-stcd1.
      p_parceiros-reme_ie      = emitente-state_insc.
      p_parceiros-reme_xnome   = emitente-name1.
      p_parceiros-reme_xfant   = emitente-name1.
      p_parceiros-reme_xlgr    = emitente-street(25).
      IF emitente-house_num1 IS INITIAL.
        p_parceiros-reme_nro     = c_sn.
      ELSE.
        p_parceiros-reme_nro     = emitente-house_num1.
      ENDIF.
      "p_parceiros-emit_xcpl    = emitente-.
      p_parceiros-reme_xbairro = emitente-city2.
      p_parceiros-reme_cmun    = emitente-taxjurcode+3(7).

      SELECT SINGLE * INTO cidade
        FROM j_1btxjurt
       WHERE spras      = emitente-langu
         AND country    = emitente-country
         AND taxjurcode = emitente-taxjurcode.

      IF sy-subrc IS INITIAL.
        p_parceiros-reme_xmun    = cidade-text.
      ENDIF.
      p_parceiros-reme_cep     = emitente-post_code1.
      p_parceiros-reme_uf      = emitente-region.
      p_parceiros-reme_fone    = emitente-tel_number.
    ENDIF.

  ENDIF.

  "Destino da Mercadoria
  READ TABLE it_j_1bnfnad INTO wa_j_1bnfnad WITH KEY parvw = 'SH'.
  IF sy-subrc IS INITIAL.

    CLEAR: emitente.

    "Busca Emissor do Conhecimento
    SELECT SINGLE a~name1   j~stcd3      j~stcd1  j~stcd2
                  a~city2   a~post_code1 a~street a~house_num1
                  a~country a~langu      a~region a~taxjurcode
                  a~tel_number
             INTO emitente
      FROM kna1 AS j
     INNER JOIN adrc AS a ON a~addrnumber = j~adrnr
     WHERE j~kunnr EQ wa_j_1bnfnad-parid.

    IF sy-subrc IS INITIAL.

      p_parceiros-dest_codigo  = wa_j_1bnfnad-parid.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_parceiros-dest_codigo
        IMPORTING
          output = p_parceiros-dest_codigo.

      p_parceiros-dest_cnpj    = emitente-stcd1.
      p_parceiros-dest_ie      = emitente-state_insc.
      p_parceiros-dest_xnome   = emitente-name1.
      p_parceiros-dest_xfant   = emitente-name1.
      p_parceiros-dest_xlgr    = emitente-street(25).

      IF emitente-house_num1 IS INITIAL.
        p_parceiros-dest_nro     = c_sn.
      ELSE.
        p_parceiros-dest_nro     = emitente-house_num1.
      ENDIF.

      "p_parceiros-emit_xcpl    = emitente-.
      p_parceiros-dest_xbairro = emitente-city2.
      p_parceiros-dest_cmun    = emitente-taxjurcode+3(7).

      SELECT SINGLE * INTO cidade
        FROM j_1btxjurt
       WHERE spras      = emitente-langu
         AND country    = emitente-country
         AND taxjurcode = emitente-taxjurcode.

      IF sy-subrc IS INITIAL.
        p_parceiros-dest_xmun    = cidade-text.
      ENDIF.

      p_parceiros-dest_cep     = emitente-post_code1.
      p_parceiros-dest_uf      = emitente-region.
      p_parceiros-dest_fone    = emitente-tel_number.
    ENDIF.

  ENDIF.

  wa_obs_geral-docnum = p_cte_avulso.
  wa_obs_geral-linha  = 0.

  SELECT *
    FROM j_1bnfftx INTO wa_j_1bnfftx
   WHERE docnum EQ p_cte_avulso
   ORDER BY PRIMARY KEY.
    ADD 1 TO wa_obs_geral-linha.
    wa_obs_geral-texto = wa_j_1bnfftx-message.
    APPEND wa_obs_geral TO it_obs_geral.
  ENDSELECT.

  CONCATENATE 'Numero do Documento:' p_cte_avulso INTO wa_obs_geral-texto SEPARATED BY space.
  ADD 1 TO wa_obs_geral-linha.
  APPEND wa_obs_geral TO it_obs_geral.

ENDFUNCTION.
