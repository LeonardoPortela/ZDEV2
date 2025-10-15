FUNCTION z_les_info_aquav.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"     REFERENCE(P_COMPLEMENTO) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     VALUE(P_ZLEST0066) TYPE  ZLEST0066
*"  TABLES
*"      IT_CTE_OBSG STRUCTURE  ZCTE_OBS_GERAIS OPTIONAL
*"      IT_ZLEST0067 STRUCTURE  ZLEST0067 OPTIONAL
*"      IT_OBS_GERAIS STRUCTURE  ZCTE_OBS_GERAIS OPTIONAL
*"----------------------------------------------------------------------


  "tYPES
  TYPES: BEGIN OF ty_vbfa,
           vbeln     TYPE vbfa-vbeln,
           vbeln_ref TYPE j_1bnflin-refkey,
         END OF ty_vbfa,

         BEGIN OF ty_j_1bnfe_active,
           docnum  TYPE j_1bnfe_active-docnum,
           regio   TYPE j_1bnfe_active-regio,
           nfyear  TYPE j_1bnfe_active-nfyear,
           nfmonth TYPE j_1bnfe_active-nfmonth,
           stcd1   TYPE j_1bnfe_active-stcd1,
           model   TYPE j_1bnfe_active-model,
           serie   TYPE j_1bnfe_active-serie,
           nfnum9  TYPE j_1bnfe_active-nfnum9,
           tpemis  TYPE j_1bnfe_active-tpemis,
           docnum9 TYPE j_1bnfe_active-docnum9,
           cdv     TYPE j_1bnfe_active-cdv,
           chave   TYPE c LENGTH 44,
         END OF ty_j_1bnfe_active,

         BEGIN OF ty_zlest0056,
           bukrs       TYPE zlest0056-bukrs,
           werks       TYPE zlest0056-werks,
           ano_viagem  TYPE zlest0056-ano_viagem,
           nr_viagem   TYPE zlest0056-nr_viagem,
           po_embarque TYPE zlest0056-po_embarque,
           po_destino  TYPE zlest0056-po_destino,
           direcao     TYPE zlest0056-direcao,
           werks_aux   TYPE lfa1-lifnr,

         END OF ty_zlest0056.

  "Fiscal
  DATA:
    it_j_1bnfdoc TYPE TABLE OF j_1bnfdoc, "Cabeçalho da nota fiscal
    wa_j_1bnfdoc TYPE j_1bnfdoc, "Cabeçalho da nota fiscal
    it_j_1bnflin TYPE TABLE OF j_1bnflin, "Partidas individuais da nota fiscal
    wa_j_1bnflin TYPE j_1bnflin, "Partidas individuais da nota fiscal
    it_j_1bnfftx TYPE TABLE OF j_1bnfftx,
    wa_j_1bnfftx TYPE j_1bnfftx,
    it_j_1bnfstx TYPE TABLE OF j_1bnfstx,
    wa_j_1bnfstx TYPE j_1bnfstx,
    it_j_1baj    TYPE TABLE OF j_1baj,
    wa_j_1baj    TYPE j_1baj,
    it_j_1bagnt  TYPE TABLE OF j_1bagnt,
    wa_j_1bagnt  TYPE j_1bagnt.

  "Aquaviário
  DATA: it_zlest0061 TYPE TABLE OF zlest0061, "Frete Aquaviário - Ordem de Venda
        wa_zlest0061 TYPE zlest0061, "Frete Aquaviário - Ordem de Venda
        it_zlest0063 TYPE TABLE OF zlest0063, "Frete Aquáviário - Comboio
        wa_zlest0063 TYPE zlest0063, "Frete Aquáviário - Comboio
        it_zlest0053 TYPE TABLE OF zlest0053, "Cadastro rebocadores/empurradores e barcaças
        wa_zlest0053 TYPE zlest0053, "Cadastro rebocadores/empurradores e barcaças
        it_zlest0056 TYPE TABLE OF ty_zlest0056, "Frete Aquaviário - Viagem
        wa_zlest0056 TYPE ty_zlest0056, "Frete Aquaviário - Viagem
        it_zlest0057 TYPE TABLE OF zlest0057, "Frete Aquaviário - Vinculação
        wa_zlest0057 TYPE zlest0057, "Frete Aquaviário - Vinculação
        wa_zlest0067 TYPE zlest0067. "Nota Fiscal de Transportada no Aquaviário - XML
  "IT_ZLEST0065 TYPE TABLE OF ZLEST0065,
  "WA_ZLEST0065 TYPE ZLEST0065.

  "Informações Gerais
  DATA:
    it_lfa1         TYPE TABLE OF lfa1, "Mestre de fornecedores (parte geral)
    wa_lfa1         TYPE lfa1, "Mestre de fornecedores (parte geral)

    it_lfa1_emissor TYPE TABLE OF lfa1, "Mestre de fornecedores (parte geral)
    wa_lfa1_emissor TYPE lfa1, "Mestre de fornecedores (parte geral)



    it_kna1         TYPE TABLE OF kna1, "Mestre de clientes (parte geral)
    wa_kna1         TYPE kna1, "Mestre de clientes (parte geral)
    wa_obs          TYPE zcte_obs_gerais,
    it_makt         TYPE TABLE OF makt,
    wa_makt         TYPE makt.



  "Observações Fiscais  + Impostos.
  DATA: it_j_1batl1  TYPE TABLE OF j_1batl1, "ICMS
        wa_j_1batl1  TYPE j_1batl1, "ICMS
        it_j_1batl1t TYPE TABLE OF j_1batl1t, "ICMS
        wa_j_1batl1t TYPE j_1batl1t,


        it_j_1batl4a TYPE TABLE OF j_1batl4a, "COFINS
        wa_j_1batl4a TYPE j_1batl4a, "COFINS
        it_j_1batl4t TYPE TABLE OF j_1batl4t, "COFINS
        wa_j_1batl4t TYPE j_1batl4t, "COFINS

        it_j_1batl5  TYPE TABLE OF j_1batl5, "PIS
        wa_j_1batl5  TYPE j_1batl5,
        it_j_1batl5t TYPE TABLE OF j_1batl5t,
        wa_j_1batl5t TYPE j_1batl5t,

        wa_zcte_obs  TYPE zcte_obs_gerais.



*  "Informações do Remetente e Destinatario.
  DATA:
    it_lfa1_rem    TYPE TABLE OF lfa1, "Mestre de fornecedores (parte geral)
    wa_lfa1_rem    TYPE lfa1, "Mestre de fornecedores (parte geral)


    it_adrc_rem    TYPE TABLE OF adrc, "Endereços (administração de endereços central)
    wa_adrc_rem    TYPE adrc, "Endereços (administração de endereços central).


    it_kna1_dest   TYPE TABLE OF kna1, "Mestre de fornecedores (parte geral)
    wa_kna1_dest   TYPE kna1, "Mestre de fornecedores (parte geral)

    it_adrc_dest   TYPE TABLE OF adrc, "Endereços (administração de endereços central)
    wa_adrc_dest   TYPE adrc, "Endereços (administração de endereços central),

    it_adrc_p_emb  TYPE TABLE OF adrc, "Endereços (administração de endereços central)
    wa_adrc_p_emb  TYPE adrc, "Endereços (administração de endereços central)

    it_adrc_p_dest TYPE TABLE OF adrc, "Endereços (administração de endereços central)
    wa_adrc_p_dest TYPE adrc, "Endereços (administração de endereços central)

    it_lfa1_pemb   TYPE TABLE OF lfa1, "Mestre de fornecedores (parte geral)
    wa_lfa1_pemb   TYPE lfa1. "Mestre de fornecedores (parte geral)



  "Notas Fiscais
  DATA: it_zsdt0001       TYPE TABLE OF zsdt0001,
        wa_zsdt0001       TYPE zsdt0001,
        it_zlest0060_nf   TYPE TABLE OF zlest0060,
        wa_zlest0060_nf   TYPE zlest0060,
        it_vbfa           TYPE TABLE OF ty_vbfa,
        wa_vbfa           TYPE ty_vbfa,
        it_j_1bnflin_nf   TYPE TABLE OF j_1bnflin,
        wa_j_1bnflin_nf   TYPE j_1bnflin,
        it_j_1bnfe_active TYPE TABLE OF ty_j_1bnfe_active,
        wa_j_1bnfe_active TYPE ty_j_1bnfe_active.

  "Variaveis
  DATA: data_emi  TYPE c LENGTH 10,
        hora_emi  TYPE c LENGTH 8,
        vl_lifnr  TYPE lfa1-lifnr,
        tabix     TYPE sy-tabix,
        vl_taxsit TYPE c LENGTH 2.

  DATA: vlr_dolar TYPE c LENGTH 20,
        vlr_taxa  TYPE c LENGTH 20.

  DATA: taxjurcode TYPE  j_1btxjcd.

  CONSTANTS: a_0  TYPE c LENGTH 1 VALUE '0',
             a_01 TYPE c LENGTH 2 VALUE '01',
             a_1  TYPE c LENGTH 1 VALUE '1',
             a_2  TYPE c LENGTH 1 VALUE '2',
             a_3  TYPE c LENGTH 1 VALUE '3'.

  "Cabeçalho da nota fiscal
  SELECT * FROM j_1bnfdoc
    INTO TABLE it_j_1bnfdoc
  WHERE docnum EQ p_cte_avulso
    AND cancel NE 'X'.

  CHECK NOT it_j_1bnfdoc[] IS INITIAL.

  "Mensagem do cabeçalho da nota fiscal
  SELECT * FROM j_1bnfftx
    INTO TABLE it_j_1bnfftx
    FOR ALL ENTRIES IN it_j_1bnfdoc
  WHERE docnum EQ it_j_1bnfdoc-docnum.

  "Observações Gerais
  LOOP AT it_j_1bnfftx INTO wa_j_1bnfftx.

    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_j_1bnfftx-docnum.

    IF ( sy-subrc EQ 0 ).

      wa_obs-docnum = wa_j_1bnfftx-docnum.
      wa_obs-linha  = wa_j_1bnfftx-linnum.
      wa_obs-texto  = wa_j_1bnfftx-message.

      APPEND wa_obs TO it_cte_obsg.
    ENDIF.
  ENDLOOP.

  "Partidas individuais da nota fiscal
  SELECT * FROM j_1bnflin
    INTO TABLE it_j_1bnflin
    FOR ALL ENTRIES IN it_j_1bnfdoc
  WHERE docnum EQ it_j_1bnfdoc-docnum.

  CHECK NOT it_j_1bnflin[] IS INITIAL.

  "CFOP
  SELECT * FROM j_1bagnt
    INTO TABLE it_j_1bagnt
    FOR ALL ENTRIES IN it_j_1bnflin
  WHERE cfop    EQ it_j_1bnflin-cfop
    AND spras   EQ 'PT'
    AND version EQ '2'.



  IF p_complemento IS NOT INITIAL.
    "Frete Aquaviário - Ordem de Venda
    SELECT * FROM zlest0061
      INTO TABLE it_zlest0061
      FOR ALL ENTRIES IN it_j_1bnfdoc
    WHERE docnum EQ it_j_1bnfdoc-docref.

  ELSE.

    "Frete Aquaviário - Ordem de Venda
    SELECT * FROM zlest0061
      INTO TABLE it_zlest0061
      FOR ALL ENTRIES IN it_j_1bnfdoc
    WHERE docnum EQ it_j_1bnfdoc-docnum.

  ENDIF.

  "Material
  SELECT * FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_zlest0061
  WHERE matnr EQ it_zlest0061-cod_material
    AND spras EQ 'P'.

  "Frete Aquáviário - Comboio
  SELECT * FROM zlest0063
    INTO TABLE it_zlest0063
    FOR ALL ENTRIES IN it_zlest0061
  WHERE bukrs       EQ it_zlest0061-bukrs
    AND werks       EQ it_zlest0061-werks
    AND ano_viagem  EQ it_zlest0061-ano_viagem
    AND nr_viagem   EQ it_zlest0061-nr_viagem.

  "Cadastro rebocadores/empurradores e barcaças
  SELECT * FROM zlest0053
    INTO TABLE it_zlest0053
    FOR ALL ENTRIES IN it_zlest0063
  WHERE embarcacao  EQ it_zlest0063-embarcacao.

  "Frete Aquaviário - Viagem
  SELECT bukrs
         werks
         ano_viagem
         nr_viagem
         po_embarque
         po_destino
         direcao
    FROM zlest0056
    INTO TABLE it_zlest0056
    FOR ALL ENTRIES IN it_zlest0061
  WHERE bukrs       EQ it_zlest0061-bukrs
    AND werks       EQ it_zlest0061-werks
    AND ano_viagem  EQ it_zlest0061-ano_viagem
    AND nr_viagem   EQ it_zlest0061-nr_viagem.


  LOOP AT it_zlest0056 INTO wa_zlest0056.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zlest0056-werks
      IMPORTING
        output = wa_zlest0056-werks_aux.
    MODIFY it_zlest0056 FROM wa_zlest0056 INDEX sy-tabix TRANSPORTING werks_aux.
    CLEAR: wa_zlest0056.
  ENDLOOP.


*  "Frete Aquaviário - Vinculação
*  SELECT * FROM ZLEST0057
*    INTO TABLE IT_ZLEST0057
*    FOR ALL ENTRIES IN IT_ZLEST0061
*  WHERE BUKRS      EQ IT_ZLEST0061-BUKRS
*    AND WERKS      EQ IT_ZLEST0061-WERKS
*    AND ANO_VIAGEM EQ IT_ZLEST0061-ANO_VIAGEM
*    AND NR_VIAGEM  EQ IT_ZLEST0061-NR_VIAGEM
*    AND NOME_EMB   EQ IT_ZLEST0061-NOME_EMB
*    AND CL_CODIGO  EQ IT_ZLEST0061-CL_CODIGO
*    AND SAFRA      EQ IT_ZLEST0061-SAFRA
*    AND NR_DCO     EQ IT_ZLEST0061-NR_DCO.
*
*  LOOP AT IT_ZLEST0057 INTO WA_ZLEST0057.
*    CLEAR: TABIX.
*    TABIX = SY-TABIX.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WA_ZLEST0057-DT_CODIGO
*      IMPORTING
*        OUTPUT = WA_ZLEST0057-DT_CODIGO.
*
*    MODIFY IT_ZLEST0057 FROM WA_ZLEST0057 INDEX TABIX.
*  ENDLOOP.

*  "Mestre de fornecedores (parte geral)
*  SELECT * FROM LFA1
*    INTO TABLE IT_LFA1
*    FOR ALL ENTRIES IN IT_ZLEST0056
*  WHERE LIFNR EQ IT_ZLEST0056-WERKS_AUX.

  "Mestre de fornecedores (parte geral)
  SELECT * FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_zlest0056
  WHERE lifnr EQ it_zlest0056-po_embarque.

  SELECT * FROM lfa1
    INTO TABLE it_lfa1_emissor
    FOR ALL ENTRIES IN it_zlest0056
  WHERE lifnr EQ it_zlest0056-werks_aux.


  SELECT * FROM lfa1
    INTO TABLE it_lfa1_pemb
    FOR ALL ENTRIES IN it_zlest0056
  WHERE lifnr EQ it_zlest0056-po_embarque.

  SELECT * FROM adrc
    INTO TABLE it_adrc_p_emb
    FOR ALL ENTRIES IN it_lfa1_pemb
  WHERE addrnumber  EQ it_lfa1_pemb-adrnr.


  "Mestre de clientes (parte geral)
  SELECT * FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_zlest0056
  WHERE kunnr EQ it_zlest0056-po_destino.

  SELECT * FROM adrc
    INTO TABLE it_adrc_p_dest
    FOR ALL ENTRIES IN it_kna1
  WHERE addrnumber  EQ it_kna1-adrnr.

**  "Recuperar Notas Fiscais
  SELECT * FROM zlest0060
    INTO TABLE it_zlest0060_nf
    FOR ALL ENTRIES IN it_zlest0061
  WHERE bukrs      EQ it_zlest0061-bukrs
    AND werks      EQ it_zlest0061-werks
    AND nr_viagem  EQ it_zlest0061-nr_viagem
    AND ano_viagem EQ it_zlest0061-ano_viagem
    AND nr_dco     EQ it_zlest0061-nr_dco
    AND safra      EQ it_zlest0061-safra
    AND cl_codigo  EQ it_zlest0061-cl_codigo
    AND nome_emb   EQ it_zlest0061-nome_emb
    AND docnum     EQ it_zlest0061-docnum.

  SELECT * FROM zsdt0001
    INTO TABLE it_zsdt0001
    FOR ALL ENTRIES IN it_zlest0060_nf
  WHERE tp_movimento EQ 'E'
    AND bukrs  EQ it_zlest0060_nf-bukrs
    AND parid  EQ it_zlest0060_nf-rm_codigo
    AND nfnum  EQ it_zlest0060_nf-nfnum
    AND series EQ it_zlest0060_nf-series
    AND docdat EQ it_zlest0060_nf-docdat.

  CLEAR: p_zlest0066-vcarga,
         p_zlest0066-qcarga.

  LOOP AT it_zlest0060_nf INTO wa_zlest0060_nf.
    IF NOT ( wa_zlest0060_nf-chave_nfe IS INITIAL ).

      IF wa_zlest0060_nf-peso_liq_ret IS NOT INITIAL.
        p_zlest0066-vcarga  = p_zlest0066-vcarga + wa_zlest0060_nf-vlr_liq_ret.
        p_zlest0066-qcarga  = p_zlest0066-qcarga + wa_zlest0060_nf-peso_liq_ret.
        wa_zlest0067-qcarga = wa_zlest0060_nf-peso_liq_ret.
      ELSE.
        p_zlest0066-vcarga  = p_zlest0066-vcarga + wa_zlest0060_nf-netwr.
        p_zlest0066-qcarga  = p_zlest0066-qcarga + wa_zlest0060_nf-peso_fiscal .
        wa_zlest0067-qcarga = wa_zlest0060_nf-peso_fiscal .
      ENDIF.

      wa_zlest0067-chave  = wa_zlest0060_nf-chave_nfe.
      wa_zlest0067-docdat = wa_zlest0060_nf-docdat.
      wa_zlest0067-nome_emb = wa_zlest0060_nf-nome_emb.

      APPEND wa_zlest0067 TO it_zlest0067.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zlest0060_nf-dt_codigo
      IMPORTING
        output = wa_zlest0060_nf-dt_codigo.

    MODIFY it_zlest0060_nf FROM wa_zlest0060_nf INDEX sy-tabix TRANSPORTING dt_codigo.

    CLEAR: wa_zlest0060_nf, wa_zlest0067.
  ENDLOOP.

  IF p_complemento IS NOT INITIAL.
    p_zlest0066-vcarga = 1.
    p_zlest0066-qcarga = 1.
  ENDIF.

*  "Informações do Remetente.
  SELECT * FROM lfa1
    INTO TABLE it_lfa1_rem
    FOR ALL ENTRIES IN it_zlest0060_nf
  WHERE lifnr EQ it_zlest0060_nf-rm_codigo.

  SELECT * FROM adrc
    INTO TABLE it_adrc_rem
    FOR ALL ENTRIES IN it_lfa1_rem
  WHERE addrnumber  EQ it_lfa1_rem-adrnr.

  "Informações Destintarios
  SELECT * FROM kna1
    INTO TABLE it_kna1_dest
    FOR ALL ENTRIES IN it_zlest0060_nf
  WHERE kunnr EQ it_zlest0060_nf-dt_codigo.

  SELECT * FROM adrc
    INTO TABLE it_adrc_dest
    FOR ALL ENTRIES IN it_kna1_dest
  WHERE addrnumber  EQ it_kna1_dest-adrnr.


  "Impostos

  "ICMS
  SELECT * FROM j_1batl1
    INTO TABLE it_j_1batl1
    FOR ALL ENTRIES IN it_j_1bnflin
  WHERE taxlaw EQ it_j_1bnflin-taxlw1.

  "IF VL_COMPLEMENTO IS NOT INITIAL.

  SELECT * FROM j_1bnfstx
    INTO TABLE it_j_1bnfstx
  WHERE docnum EQ p_cte_avulso.

*  ELSE.
*    SELECT * FROM J_1BNFSTX
*      INTO TABLE IT_J_1BNFSTX
*      FOR ALL ENTRIES IN IT_ZLEST0061
*    WHERE DOCNUM EQ IT_ZLEST0061-DOCNUM.
*  ENDIF.

  SELECT * FROM j_1baj
    INTO TABLE it_j_1baj
    FOR ALL ENTRIES IN it_j_1bnfstx
  WHERE  taxtyp EQ it_j_1bnfstx-taxtyp
    AND  taxgrp EQ 'ICMS'.

  "COFINS
  SELECT * FROM j_1batl4a
    INTO TABLE it_j_1batl4a
    FOR ALL ENTRIES IN it_j_1bnflin
  WHERE taxlaw EQ it_j_1bnflin-taxlw4.

  "PIS
  SELECT * FROM j_1batl5
    INTO TABLE it_j_1batl5
    FOR ALL ENTRIES IN it_j_1bnflin
  WHERE taxlaw EQ it_j_1bnflin-taxlw5.

  "Observações dos Impostos

  "ICMS
  SELECT * FROM j_1batl1t
    INTO TABLE it_j_1batl1t
    FOR ALL ENTRIES IN it_j_1batl1
  WHERE taxlaw EQ it_j_1batl1-taxlaw.


  IF NOT ( it_j_1batl1t[] IS INITIAL ).
    LOOP AT it_j_1batl1t INTO wa_j_1batl1t.
      IF NOT ( wa_j_1batl1t-line1 IS INITIAL ).
        wa_zcte_obs-linha = wa_j_1batl1t-taxlaw.
        wa_zcte_obs-texto = wa_j_1batl1t-line1.
        APPEND wa_zcte_obs TO it_obs_gerais.
      ENDIF.
      IF NOT ( wa_j_1batl1t-line2 IS INITIAL ).
        wa_zcte_obs-linha = wa_j_1batl1t-taxlaw.
        wa_zcte_obs-texto = wa_j_1batl1t-line2.
        APPEND wa_zcte_obs TO it_obs_gerais.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "COFINS
  SELECT * FROM j_1batl4t
    INTO TABLE it_j_1batl4t
    FOR ALL ENTRIES IN it_j_1batl4a
  WHERE taxlaw EQ it_j_1batl4a-taxlaw.

  IF NOT ( it_j_1batl4t[] IS INITIAL ).
    LOOP AT it_j_1batl4t INTO wa_j_1batl4t.
      IF NOT ( wa_j_1batl4t-line1 IS INITIAL ).
        wa_zcte_obs-linha = wa_j_1batl4t-taxlaw.
        wa_zcte_obs-texto = wa_j_1batl4t-line1.
        APPEND wa_zcte_obs TO it_obs_gerais.
      ENDIF.
      IF NOT ( wa_j_1batl4t-line2 IS INITIAL ).
        wa_zcte_obs-linha = wa_j_1batl4t-taxlaw.
        wa_zcte_obs-texto = wa_j_1batl4t-line2.
        APPEND wa_zcte_obs TO it_obs_gerais.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "PIS
  SELECT * FROM j_1batl5t
    INTO TABLE it_j_1batl5t
    FOR ALL ENTRIES IN it_j_1batl5
  WHERE taxlaw EQ it_j_1batl5-taxlaw.

  IF NOT ( it_j_1batl5t[] IS INITIAL ).

    LOOP AT it_j_1batl5t INTO wa_j_1batl5t.
      IF NOT ( wa_j_1batl5t-line1 IS INITIAL ).
        wa_zcte_obs-linha = wa_j_1batl5t-taxlaw.
        wa_zcte_obs-texto = wa_j_1batl5t-line1.
        APPEND wa_zcte_obs TO it_obs_gerais.
      ENDIF.
      IF NOT ( wa_j_1batl5t-line2 IS INITIAL ).
        wa_zcte_obs-linha = wa_j_1batl5t-taxlaw.
        wa_zcte_obs-texto = wa_j_1batl5t-line2.
        APPEND wa_zcte_obs TO it_obs_gerais.
      ENDIF.
    ENDLOOP.

  ENDIF.


  LOOP AT it_j_1bnfdoc INTO wa_j_1bnfdoc.


    "Identificação do CT-e (TAG-IDE).
    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY docnum = wa_j_1bnfdoc-docnum.
    p_zlest0066-cfop    = wa_j_1bnflin-cfop.

    READ TABLE it_j_1bagnt INTO wa_j_1bagnt WITH KEY cfop    = wa_j_1bnflin-cfop
                                                     spras   = 'PT'
                                                     version = '2'.
    p_zlest0066-natop   = wa_j_1bagnt-cfotxt.

    IF p_complemento IS NOT INITIAL.
      READ TABLE it_zlest0061 INTO wa_zlest0061 WITH KEY docnum      = wa_j_1bnfdoc-docref.
    ELSE.
      READ TABLE it_zlest0061 INTO wa_zlest0061 WITH KEY docnum      = wa_j_1bnfdoc-docnum.
    ENDIF.

    READ TABLE it_zlest0060_nf INTO wa_zlest0060_nf    WITH KEY bukrs      = wa_zlest0061-bukrs
                                                                werks      = wa_zlest0061-werks
                                                                nr_viagem  = wa_zlest0061-nr_viagem
                                                                ano_viagem = wa_zlest0061-ano_viagem
                                                                nr_dco     = wa_zlest0061-nr_dco
                                                                safra      = wa_zlest0061-safra
                                                                cl_codigo  = wa_zlest0061-cl_codigo
                                                                nome_emb   = wa_zlest0061-nome_emb
                                                                docnum     = wa_zlest0061-docnum.

    CASE  wa_zlest0060_nf-tomador_serv.
      WHEN: 'R'.
        p_zlest0066-forpag = a_0.
        p_zlest0066-toma   = a_0.
      WHEN: 'D'.
        p_zlest0066-forpag = a_1.
        p_zlest0066-toma   = a_3.
    ENDCASE.

    p_zlest0066-serie = wa_j_1bnfdoc-series.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_j_1bnfdoc-nfenum
      IMPORTING
        output = p_zlest0066-nct.

    p_zlest0066-safra = wa_zlest0061-safra.

    CONCATENATE wa_j_1bnfdoc-docdat(4) '-' wa_j_1bnfdoc-docdat+4(2) '-' wa_j_1bnfdoc-docdat+6(2) INTO data_emi.
    CONCATENATE wa_j_1bnfdoc-cretim(2) ':' wa_j_1bnfdoc-cretim+2(2) ':' wa_j_1bnfdoc-cretim+4(2) INTO hora_emi.

    "Ini. CS2017002043 04.10.2017
    IF wa_j_1bnfdoc-docnum IS NOT INITIAL.
      DATA: v_time_br TYPE erzet.
      CALL FUNCTION 'Z_FUSO_HORARIO_FILIAL'
        EXPORTING
          i_bukrs  = wa_j_1bnfdoc-bukrs
          i_branch = wa_j_1bnfdoc-branch
        IMPORTING
          e_time   = v_time_br.
      IF v_time_br IS NOT INITIAL.
        CONCATENATE v_time_br(2) ':' v_time_br+2(2) ':' v_time_br+4(2)  INTO hora_emi.
      ENDIF.
    ENDIF.
    "Fim. CS2017002043 04.10.2017

    CONCATENATE data_emi hora_emi INTO p_zlest0066-dhemi SEPARATED BY space.

    p_zlest0066-tpcte   = a_0.

    IF p_complemento IS NOT INITIAL.
      p_zlest0066-tpcte   = a_1.
    ENDIF.

    p_zlest0066-modal   = a_3.
    p_zlest0066-tpserv  = a_0.

    CALL FUNCTION 'Z_SD_INFO_CIDADE_EMITE'
      EXPORTING
        wa_j_1bnfdoc = wa_j_1bnfdoc
      IMPORTING
        taxjurcode   = taxjurcode.

    IF NOT ( taxjurcode IS INITIAL ).
      p_zlest0066-cmunenv  = taxjurcode+3(12).
    ENDIF.

    READ TABLE it_zlest0056 INTO wa_zlest0056 WITH KEY bukrs       = wa_zlest0061-bukrs
                                                       werks       = wa_zlest0061-werks
                                                       ano_viagem  = wa_zlest0061-ano_viagem
                                                       nr_viagem   = wa_zlest0061-nr_viagem.


    "READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR  = WA_ZLEST0056-WERKS_AUX.
    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr  = wa_zlest0056-po_embarque.

    p_zlest0066-cmunini = wa_lfa1-txjcd+3(12).
    p_zlest0066-ufini   = wa_lfa1-txjcd(2).
    p_zlest0066-xmunini = wa_lfa1-ort01.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zlest0056-po_destino.
    p_zlest0066-cmunfim = wa_kna1-txjcd+3(12).
    p_zlest0066-uffim   = wa_kna1-txjcd(2).
    p_zlest0066-xmunfim = wa_kna1-ort01.

    p_zlest0066-retira  = a_1.

    "Identificação do Emitente do CT-e.
*    CLEAR: WA_LFA1, VL_LIFNR.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WA_ZLEST0061-WERKS
*      IMPORTING
*        OUTPUT = VL_LIFNR.
*
*    READ TABLE IT_LFA1_EMISSOR INTO WA_LFA1_EMISSOR WITH KEY LIFNR = VL_LIFNR.
*    P_ZLEST0066-CNPJ_EMITE = WA_LFA1_EMISSOR-STCD1.
*    P_ZLEST0066-IE_EMITE   = WA_LFA1_EMISSOR-STCD3.

    "17.10.2018
    IF wa_zlest0061-docnum IS NOT INITIAL.
      SELECT SINGLE *
        FROM j_1bnfdoc INTO @DATA(_wl_doc)
       WHERE docnum EQ @wa_zlest0061-docnum.

      IF sy-subrc EQ 0.
        CLEAR: vl_lifnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = _wl_doc-branch
          IMPORTING
            output = vl_lifnr.

        SELECT SINGLE *
          FROM lfa1 INTO @DATA(_wl_lfa1)
         WHERE lifnr = @vl_lifnr.

        IF sy-subrc EQ 0  .
          p_zlest0066-cnpj_emite = _wl_lfa1-stcd1.
          p_zlest0066-ie_emite   = _wl_lfa1-stcd3.
        ENDIF.
      ENDIF.
    ENDIF.

    "Informações do Remetente das mercadorias transportadas pelo CT-e
    READ TABLE it_lfa1_rem INTO wa_lfa1_rem WITH KEY lifnr =  wa_zlest0060_nf-rm_codigo.
    IF  wa_zlest0060_nf-chave_nfe+0(1) = 'F'.
      p_zlest0066-cnpj_reme  = wa_lfa1_rem-stcd2.
    ELSE.
      p_zlest0066-cnpj_reme  = wa_lfa1_rem-stcd1.
    ENDIF.
    p_zlest0066-ie_reme    = wa_lfa1_rem-stcd3.
    p_zlest0066-xnome_reme = wa_lfa1_rem-name1.
    p_zlest0066-xfant_reme = wa_lfa1_rem-sortl.

    READ TABLE it_adrc_rem INTO wa_adrc_rem WITH KEY addrnumber = wa_lfa1_rem-adrnr.
    p_zlest0066-xlgr_reme     = wa_adrc_rem-street.
    p_zlest0066-nro_reme      = wa_adrc_rem-house_num1.
    p_zlest0066-xbairro_reme  = wa_adrc_rem-city2.
    p_zlest0066-cmun_reme     = wa_adrc_rem-taxjurcode+3(12).
    p_zlest0066-cep_reme      = wa_adrc_rem-post_code1.

    READ TABLE it_kna1_dest INTO wa_kna1_dest WITH KEY kunnr =  wa_zlest0060_nf-dt_codigo.
    p_zlest0066-cnpj_dest  = wa_kna1_dest-stcd1.
    p_zlest0066-ie_dest    = wa_kna1_dest-stcd3.
    p_zlest0066-xnome_dest = wa_kna1_dest-name1.

    READ TABLE it_adrc_dest INTO wa_adrc_dest WITH KEY addrnumber = wa_kna1_dest-adrnr.
    p_zlest0066-xlgr_dest     = wa_adrc_dest-street.
    p_zlest0066-nro_dest      = wa_adrc_dest-house_num1.
    p_zlest0066-xbairro_dest  = wa_adrc_dest-city2.
    p_zlest0066-cmun_dest     = wa_adrc_dest-taxjurcode+3(12).
    p_zlest0066-cep_dest      = wa_adrc_dest-post_code1.

    "Valores da Prestação de Serviço
    p_zlest0066-vtprest = wa_j_1bnflin-netwr.
    p_zlest0066-vrec    = wa_j_1bnflin-netwr.
    p_zlest0066-vcomp   = wa_j_1bnflin-netwr.

    READ TABLE it_j_1batl1 INTO wa_j_1batl1 WITH KEY taxlaw = wa_j_1bnflin-taxlw1.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = wa_j_1batl1-taxsit
      IMPORTING
        output = vl_taxsit.

    CASE vl_taxsit.

      WHEN: '00'.
        READ TABLE it_j_1bnfstx INTO wa_j_1bnfstx WITH KEY docnum = p_cte_avulso."WA_ZLEST0061-DOCNUM.

        READ TABLE it_j_1baj INTO wa_j_1baj WITH KEY taxtyp = wa_j_1bnfstx-taxtyp
                                                     taxgrp = 'ICMS'.

        IF ( sy-subrc EQ 0 ).
          p_zlest0066-cst    = vl_taxsit.
          p_zlest0066-vbc    = wa_j_1bnfstx-base.
          p_zlest0066-picms  = wa_j_1bnfstx-rate.
          p_zlest0066-vicms  = wa_j_1bnfstx-taxval.
        ENDIF.

      WHEN: '40' OR '41' OR '51'.
        p_zlest0066-cst    = vl_taxsit.
    ENDCASE.

    READ TABLE it_zlest0063 INTO wa_zlest0063 WITH KEY bukrs      = wa_zlest0061-bukrs
                                                       werks      = wa_zlest0061-werks
                                                       ano_viagem = wa_zlest0061-ano_viagem
                                                       nr_viagem  = wa_zlest0061-nr_viagem
                                                       nome_emb   = wa_zlest0061-nome_emb.

    CASE wa_zlest0063-tp_class.
      WHEN: 'CO'.
        p_zlest0066-tp_class = 'Convencional'.
      WHEN OTHERS.
        p_zlest0066-tp_class = 'Transgênico'.
    ENDCASE.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zlest0061-cod_material.

    "Grupo de informações do CT-e Normal e Substituto
    "   P_ZLEST0066-VCARGA  = WA_ZLEST0057-VALOR_VINCULADO.
    p_zlest0066-propred = wa_makt-maktx.
    p_zlest0066-cunid     = a_01.
    p_zlest0066-cunid_sap = 'KG'.
    p_zlest0066-tpmed     = 'PESO BRUTO'.
    "    P_ZLEST0066-QCARGA  = WA_ZLEST0057-PESO_VINCULADO.

    "Informações do Aquaviário
    p_zlest0066-vprest = wa_j_1bnflin-netwr.

    "80265
    SELECT SINGLE aliq
      FROM zfit0176
      INTO @DATA(lv_aliq_afrmm).
    IF sy-subrc EQ 0.
      p_zlest0066-vafrmm = ( p_zlest0066-vprest * lv_aliq_afrmm ) / 100.
    ELSE.
      p_zlest0066-vafrmm = a_0.
    ENDIF.


    " P_ZLEST0066-VAFRMM = A_0.

*   << "80265
    READ TABLE it_zlest0053 INTO wa_zlest0053 WITH KEY  embarcacao = wa_zlest0063-embarcacao
                                                        nome       = wa_zlest0063-nome_emb.

    CONCATENATE wa_zlest0063-nome_emb '-' wa_zlest0053-insc_marinha INTO p_zlest0066-xbalsa.

    p_zlest0066-nviag   = wa_zlest0061-nr_viagem.
    p_zlest0066-direc   = wa_zlest0056-direcao.

    READ TABLE it_lfa1_pemb  INTO wa_lfa1_pemb WITH KEY lifnr  = wa_zlest0056-po_embarque.
    READ TABLE it_adrc_p_emb INTO wa_adrc_p_emb WITH KEY addrnumber = wa_lfa1_pemb-adrnr.

    CONCATENATE wa_lfa1_pemb-name1 '-' wa_adrc_p_emb-city1 INTO p_zlest0066-prtemb.


    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zlest0056-po_destino.
    READ TABLE it_adrc_p_dest INTO wa_adrc_p_dest WITH KEY addrnumber = wa_kna1-adrnr.

    CONCATENATE wa_kna1-name1 '-' wa_adrc_p_dest-city1 INTO p_zlest0066-prtdest.

    READ TABLE it_zlest0053 INTO wa_zlest0053 WITH KEY  embarcacao = wa_zlest0063-nome_emb.
    p_zlest0066-tpnav   = a_0.

    READ TABLE it_zlest0063 INTO wa_zlest0063 WITH KEY  embarcacao = 'E'.
    READ TABLE it_zlest0053 INTO wa_zlest0053 WITH KEY  nome = wa_zlest0063-nome_emb.
    CONCATENATE wa_zlest0053-nome '-' wa_zlest0053-insc_marinha INTO p_zlest0066-xnavio.
    p_zlest0066-irin    = wa_zlest0053-irin.

    "Dados para Observações

    p_zlest0066-nr_ov      = wa_zlest0061-nr_ov.
    p_zlest0066-nr_fatura  = wa_zlest0061-fatura.

    p_zlest0066-nr_viagem  = wa_zlest0061-nr_viagem.

    p_zlest0066-vlr_usd    = wa_zlest0061-vlr_usd.
    p_zlest0066-tax_dolar  = wa_zlest0061-tax_dolar.
    p_zlest0066-nr_dco     = wa_zlest0061-nr_dco.


  ENDLOOP.

  CLEAR: wa_obs.
  CONCATENATE 'Nr. O.V:' p_zlest0066-nr_ov INTO wa_obs-texto SEPARATED BY space.
  APPEND wa_obs TO it_cte_obsg.

  CLEAR: wa_obs.
  CONCATENATE 'Nr. Fatura' p_zlest0066-nr_fatura INTO wa_obs-texto SEPARATED BY space.
  APPEND wa_obs TO it_cte_obsg.

  CLEAR: wa_obs.
  CONCATENATE 'Nr. Viagem'    p_zlest0066-nr_viagem INTO wa_obs-texto SEPARATED BY space.
  APPEND wa_obs TO it_cte_obsg.

  CLEAR: wa_obs.
  CONCATENATE 'Classificação' p_zlest0066-tp_class INTO wa_obs-texto SEPARATED BY space.
  APPEND wa_obs TO it_cte_obsg.

  IF ( p_zlest0066-nr_dco IS NOT INITIAL ).
    CLEAR: wa_obs.
    CONCATENATE 'NR. DCO'       p_zlest0066-nr_dco INTO wa_obs-texto SEPARATED BY space.
    APPEND wa_obs TO it_cte_obsg.
  ENDIF.


  vlr_dolar = p_zlest0066-vlr_usd.
  CONDENSE vlr_dolar NO-GAPS.

  CLEAR: wa_obs.
  CONCATENATE 'Vlr. Dolar'  vlr_dolar INTO wa_obs-texto SEPARATED BY space.
  APPEND wa_obs TO it_cte_obsg.

  vlr_taxa = p_zlest0066-tax_dolar.
  CONDENSE vlr_taxa NO-GAPS.

  CLEAR: wa_obs.
  CONCATENATE 'Tx. Dolar'  vlr_taxa INTO wa_obs-texto SEPARATED BY space.
  APPEND wa_obs TO it_cte_obsg.

  IF p_complemento IS NOT INITIAL.
    CLEAR: it_cte_obsg[].
  ENDIF.


ENDFUNCTION.
