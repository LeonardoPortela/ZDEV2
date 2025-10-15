CLASS zcl_int_ib_cad_parc_linkana DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_dados_obrigatorios,
        grupo               TYPE ktokk,
        regiao              TYPE regio,
        ie                  TYPE stcd3,
        cnpj                TYPE stcd1,
        cpf                 TYPE stcd2,
        data_nasc           TYPE char10,
        nis_pis             TYPE kraus_cm,
        nome                TYPE name1_gp,
        rntrc_antt          TYPE bahns,
        emissor_nota_fiscal TYPE char1,
      END OF ty_dados_obrigatorios .
    TYPES:
      BEGIN OF ty_dados_banco,
        id          TYPE char4,
        regiao      TYPE char2,
        chave_banco TYPE bu_bankk,
        conta       TYPE bu_bankn,
      END OF ty_dados_banco .
    TYPES:
      tb_empresas TYPE TABLE OF bukrs .

    DATA:
    tb_banco1 TYPE TABLE OF ty_dados_banco .

    TYPES:
      BEGIN OF ty_dados_gerais,
        rua             TYPE ad_street,
        numero          TYPE ad_hsnm1,
        cep             TYPE ad_pstcd1,
        pais            TYPE land1,
        cidade          TYPE ad_city1,
        bairro          TYPE ad_city2,
        estado          TYPE regio,
        fax             TYPE ad_fxnmbr1,
        email           TYPE ad_smtpadr,
        estado_civil    TYPE char1,
        regime_bens     TYPE char2,
        rg              TYPE char7,
        orgao_exp       TYPE char50,
        dados_bancarios LIKE tb_banco1,
      END OF ty_dados_gerais .


    DATA:
      tb_erros  TYPE TABLE OF bapiret2-message .
    DATA:
      BEGIN OF zde_data_request,
        parceiro           TYPE lfa1-lifnr,
        grupo              TYPE lfa1-ktokk,
        cpf                TYPE lfa1-stcd2,
        cnpj               TYPE lfa1-stcd1,
        Nome               TYPE lfa1-name1,
        Rua                TYPE adrc-street,
        numero             TYPE adrc-house_num1,
        bairro             TYPE adrc-city2,
        cep                TYPE adrc-post_code1,
        cidade             TYPE adrc-city1,
        regiao             TYPE adrc-region,
        pais               TYPE adrc-country,
        telefone           TYPE adrc-tel_number,
        email              TYPE adr6-smtp_addr,
        ie                 TYPE lfa1-stcd3,
        pis                TYPE lfa1-kraus, "US #172720 - MMSILVA - 01.04.2025
        data_nasc          TYPE lfa1-gbdat, "US #172720 - MMSILVA - 01.04.2025
*** Inicio - Rubenilson - 06.12.24 - US159528
        marcacao_arq       TYPE but000-xdele,
        bloqueio_central   TYPE but000-xblck,
        bloq_centr_elimin  TYPE lfa1-nodel,
        marc_elimin_centr  TYPE lfa1-loevm,
        bloq_pagamento     TYPE lfa1-sperz,
        todas_empresas     TYPE lfa1-sperr,
        empresa_selec      TYPE lfb1-sperr,
        bloq_elim_empresa  TYPE lfb1-sperr,
        marc_elim_empresa  TYPE lfb1-loevm,
        bloq_compras       TYPE lfa1-sperm,
        bloq_comp_org_comp TYPE lfm1-sperm,
*** Fim - Rubenilson - 06.12.24 - US159528
        tipo_industria     TYPE char3," Rubenilson - 14.10.24 - #155127
        dados_bancarios    LIKE tb_banco1,
        empresa            TYPE tb_empresas,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        parceiro    TYPE but000-partner,
        msg_sucesso TYPE string,
        erros       LIKE tb_erros,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '221' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
    METHODS cria_fornecedor
      IMPORTING
        !account           TYPE rf02k-ktokk
        !lwa_data_request  LIKE zde_data_request
      EXPORTING
        !lwa_data_response LIKE zde_data_response
      CHANGING
        !addr1             TYPE addr1_data
        !lfa1              TYPE lfa1 .
    METHODS cria_cliente
      IMPORTING
        !account           TYPE rf02k-ktokk
        !lwa_data_request  LIKE zde_data_request
      EXPORTING
        !lwa_data_response LIKE zde_data_response
      CHANGING
        !cs_addr1          TYPE addr1_data
        !cs_kna1           TYPE kna1 .
    METHODS valida_dados_banco
      IMPORTING
        !lwa_data_request  LIKE zde_data_request
        !iv_partner        TYPE bu_partner
      EXPORTING
        !lwa_data_response LIKE zde_data_response .
    METHODS atualiza_fornecedor
      IMPORTING
        !account           TYPE rf02k-ktokk
        !lwa_data_request  LIKE zde_data_request
      EXPORTING
        !lwa_data_response LIKE zde_data_response
      CHANGING
        !addr1             TYPE addr1_data
        !lfa1              TYPE lfa1 .
    METHODS atualiza_cliente
      IMPORTING
        !account           TYPE rf02k-ktokk
        !lwa_data_request  LIKE zde_data_request
      EXPORTING
        !lwa_data_response LIKE zde_data_response
      CHANGING
        !cs_addr1          TYPE addr1_data
        !cs_kna1           TYPE kna1 .
    METHODS preenche_lfbw
      IMPORTING
        !bukrs            TYPE bukrs
        !lwa_data_request LIKE zde_data_request
      EXPORTING
        !lfbw             TYPE lfbw_tab .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CAD_PARC_LINKANA IMPLEMENTATION.


  METHOD atualiza_cliente.

    CONSTANTS: lc_person TYPE /netrin/compde_person_type VALUE 'A',
               lc_r      TYPE char7               VALUE 'REGULAR'.

    CONSTANTS: lc_j TYPE c VALUE 'J',
               lc_f TYPE c VALUE 'F'.

    DATA: lo_adcn      TYPE REF TO /netrin/compcl_business,
          lo_utilities TYPE REF TO /netrin/compcl_utilities,
          lo_bp        TYPE REF TO zcl_bp_utils_2.

    DATA: lr_bukrs TYPE RANGE OF bukrs.

    DATA: lt_group    TYPE TABLE OF /netrin/group,
          lt_log      TYPE /netrin/comptt_log,
          lt_log_sint TYPE /netrin/comptt_log,
          lt_log_sufr TYPE /netrin/comptt_log.

    DATA: ls_group          LIKE LINE OF lt_group,
          ls_log            TYPE /netrin/compst_log,
          ls_tcode          TYPE /netrin/tcodes,
          ls_self_fill      TYPE /netrin/compst_self_fill,
          ls_search         TYPE /netrin/compst_search,
          ls_personal_data  TYPE bus_ei_struc_central_person,
          ls_personal_data2 TYPE fsbp_ei_struc_person.

    DATA: lv_status      TYPE /netrin/compde_status,
          lv_person_type TYPE c,
          lv_account     TYPE kna1-ktokd,
          lv_lin         TYPE i,
          lv_j           TYPE c,
          lv_f           TYPE c,
          lv_popup       TYPE c,
          lv_teste       TYPE c.

    DATA: gv_test          TYPE  abap_bool,
          gv_tcode         TYPE  tcode,
          gt_adsmtp        TYPE  bbpt_er_adsmtp,
          gt_bdc           TYPE  bdcdata_tab,
          gt_knb1          TYPE  cvis_knb1_t,
          gt_knbw          TYPE  cvis_knbw_t,
          gt_knvi          TYPE  cvis_knvi_t,
          gt_knbk          TYPE  knbk_t,
          gt_knvv          TYPE  cvis_knvv_t,
          gt_lfbk          TYPE  lfbk_t,
          gt_return        TYPE  bapiret2_t,
          gt_bdcdata       TYPE  bdcdata_tab,
          gt_bdcmsg        TYPE  tab_bdcmsgcoll,
          gs_addr1_data    TYPE  addr1_data,
          gs_adsmtp        TYPE  adsmtp,
          gs_but000        TYPE  but000,
          gs_kna1          TYPE  kna1,
          gs_knb1          TYPE  knb1,
          gs_knbw          TYPE  knbw,
          gs_knvi          TYPE  knvi,
          gs_knvk          TYPE  knvk,
          gs_knvv          TYPE  knvv,
          gs_lfa1          TYPE  lfa1,
          gt_lfb1          TYPE  cvis_lfb1_t,
          gt_lfbw          TYPE  cvis_lfbw_t,
          gs_rf02d         TYPE	rf02d,
          gs_rf02k         TYPE	rf02k,
          gs_sza1_d0100    TYPE  sza1_d0100,
          gs_addr2_data    TYPE  addr2_data,
          gt_lfm1          TYPE  cvis_lfm1_t,
          gs_bus_joel_main TYPE	bus_joel_main,
          gv_bu_group      TYPE	bu_group,
          lv_dia           TYPE char2,
          lv_mes           TYPE char2,
          lv_ano           TYPE char4,
          lwa_knvv         TYPE knvv,
          lwa_knvi         TYPE knvi,
          lwa_knb1         TYPE knb1,
          gv_bp_type       TYPE bu_type.


    DATA: lr_message_return TYPE RANGE OF /netrin/log-status.

    cs_kna1-ktokd = lwa_data_request-grupo.

**\ Get parâmetros da transação
    CALL METHOD /netrin/compcl_utilities=>get_tcode
      EXPORTING
        i_tcode = 'BP'
      RECEIVING
        r_tcode = ls_tcode.

**\ Se estiver marcado para enriquecimento de dados
    CHECK ls_tcode-self_fill EQ abap_true.

**\ Mensagens de Cadastro Irregular
    /netrin/compcl_utilities=>get_multiple_value_parameter(
      EXPORTING
        i_parameter = 'MESSAGE_RETURN'
      CHANGING
        c_range     = lr_message_return ).

    CHECK ls_tcode-active EQ abap_true.

    CREATE OBJECT lo_adcn.
    CREATE OBJECT lo_utilities.

**\ Get Account Group
    lv_account = lwa_data_request-grupo.

**\ Get Consultas a realizar
    SELECT *
    FROM /netrin/group
    INTO TABLE lt_group
    WHERE account_group EQ lv_account.

    READ TABLE lt_group INTO ls_group INDEX 1.

***\ Exibe POPUP para preencher Dados para Consultas
*    CALL METHOD /netrin/compcl_utilities=>self_fill_popup
*      EXPORTING
*        i_parameter = lv_account
*      IMPORTING
*        e_result    = ls_self_fill.


*    SPLIT lwa_data_request-data_nasc AT '/' INTO lv_dia lv_mes lv_ano.

    ls_self_fill-gbdat = lv_ano && lv_mes && lv_dia.
    ls_self_fill-stcd2 = lwa_data_request-cpf.
    ls_self_fill-stcd3 = lwa_data_request-ie.
    ls_self_fill-regio = lwa_data_request-regiao.
    ls_self_fill-stcd1 = lwa_data_request-cnpj.


    cs_addr1-city1      = lwa_data_request-cidade.
    cs_addr1-city2      = lwa_data_request-regiao.
    cs_addr1-region     = lwa_data_request-pais.
    cs_addr1-post_code1 = lwa_data_request-cep.
    cs_addr1-street     = lwa_data_request-rua.
    cs_addr1-house_num1 = lwa_data_request-numero.
    cs_addr1-country    = lwa_data_request-pais.

*** Dados pessoais
    ls_personal_data-birthdate     = ls_self_fill-gbdat.
*    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp into ls_personal_data-birthplace SEPARATED BY space.
*    ls_personal_data-maritalstatus = lwa_data_request-dados_gerais-estado_civil.
*    ls_personal_data2-proprty_st   = lwa_data_request-dados_gerais-regime_bens.

*    gs_sza1_d0100-fax_number = lwa_data_request-dados_gerais-fax.
*    gs_sza1_d0100-smtp_addr = lwa_data_request-dados_gerais-email.

*** Local de nascimento
*    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO cs_kna1-stcd4 SEPARATED BY space.

    MOVE-CORRESPONDING cs_addr1 TO gs_addr1_Data.
    MOVE-CORRESPONDING cs_kna1 TO gs_kna1.

    CREATE OBJECT lo_bp.

    SELECT *
      FROM tbc001
      INTO @DATA(ls_tbc001)
      UP TO 1 ROWS
      WHERE ktokk = @lwa_data_request-grupo.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      gs_bus_joel_main-creation_group = ls_tbc001-bu_group.
      gs_bus_joel_main-partner_role = 'FLVN00'.
    ELSE.

      SELECT *
        FROM tbd001
        INTO @DATA(ls_tbd001)
        UP TO 1 ROWS
        WHERE ktokd = @lwa_data_request-grupo.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        gs_bus_joel_main-creation_group = ls_tbd001-bu_group.
        gs_bus_joel_main-partner_role = 'FLCU00'.
      ENDIF.

    ENDIF.

    IF lwa_data_request-empresa IS NOT INITIAL.

      lr_bukrs = VALUE #( FOR ls_bukrs IN lwa_data_request-empresa
                          LET s = 'I'
                              o = 'EQ'
                           IN sign   = s
                              option = o
                           (  low = ls_bukrs ) ).


      SELECT * FROM zsdt0317
                  INTO TABLE @DATA(it_zsdt0317)
                  WHERE cancelado <> 'X'
                   AND ktokd = @lwa_data_request-grupo
                   AND bukrs IN @lr_bukrs.

      SELECT * FROM zsdt0319
        INTO TABLE @DATA(it_zsdt0319)
        FOR ALL ENTRIES IN @it_zsdt0317
      WHERE cancelado <> 'X'
        AND id = @it_zsdt0317-id .

      SELECT * FROM zsdt0320
        INTO TABLE @DATA(it_zsdt0320)
        FOR ALL ENTRIES IN @it_zsdt0319
        WHERE id =  @it_zsdt0319-id
         AND seq_canal = @it_zsdt0319-seq_canal
         AND cancelado <> 'X'.

      SELECT * FROM zsdt0322
        INTO TABLE @DATA(it_zsdt0322)
        FOR ALL ENTRIES IN @it_zsdt0319
        WHERE id =  @it_zsdt0319-id
         AND  seq_canal = @it_zsdt0319-seq_canal
         AND  cancelado <> 'X'.

      SORT it_zsdt0317 BY bukrs.
      LOOP AT it_zsdt0317 INTO DATA(lwa_zsdt0317).

        CLEAR: lwa_knb1.
        lwa_knb1-bukrs = lwa_zsdt0317-bukrs.
        lwa_knb1-akont = lwa_zsdt0317-akont.
        lwa_knb1-zuawa = lwa_zsdt0317-zuawa.
        lwa_knb1-fdgrv = lwa_zsdt0317-fdgrv.
        lwa_knb1-zterm = lwa_zsdt0317-zterm.
        lwa_knb1-zwels = lwa_zsdt0317-zwels.

        APPEND lwa_knb1 TO gt_knb1.

        LOOP AT it_zsdt0319 INTO DATA(lwa_zsdt0319) WHERE id = lwa_zsdt0317-id.

* MESTRE DE CLIENTES: DADOS DE VENDAS E DISTRIBUIÇÃO
          CLEAR: lwa_knvv.
          lwa_knvv-vkorg    = lwa_zsdt0317-bukrs.
          lwa_knvv-vtweg    = lwa_zsdt0319-vtweg.

          LOOP AT it_zsdt0320 INTO DATA(lwa_zsdt0320) WHERE id = lwa_zsdt0319-id
                                                       AND  seq_canal = lwa_zsdt0319-seq_canal .

            READ TABLE it_zsdt0322 INTO DATA(lwa_zsdt0322) WITH KEY id = lwa_zsdt0319-id
                                                             seq_canal = lwa_zsdt0319-seq_canal.
            lwa_knvv-spart    = lwa_zsdt0320-spart.
            lwa_knvv-kalks    = lwa_zsdt0322-kalks.
            IF lwa_data_request-grupo EQ 'ZCPF' OR
               lwa_data_request-grupo EQ 'ZCPJ'.
              lwa_knvv-kdgrp    = '6'.
            ELSE.
              lwa_knvv-kdgrp    = '7'.
            ENDIF.
            lwa_knvv-waers    = lwa_zsdt0322-waers.
            lwa_knvv-ktgrd    = lwa_zsdt0322-ktgrd.
            lwa_knvv-versg    = lwa_zsdt0322-versg.
            lwa_knvv-lprio    = lwa_zsdt0322-lprio.
            lwa_knvv-vsbed    = lwa_zsdt0322-vsbed.
            lwa_knvv-kzazu    = lwa_zsdt0322-kzazu.
            lwa_knvv-kztlf    = lwa_zsdt0322-kztlf.
            lwa_knvv-perfk    = lwa_zsdt0322-perfk.

            APPEND lwa_knvv TO gt_knvv.

* MESTRE DE CLIENTES - INDICADORES DE IMPOSTOS
            CLEAR: gt_knvi[], lwa_knvi.

            lwa_knvi-aland = 'BR'.               "tax country
            lwa_knvi-tatyp = lwa_zsdt0322-tatyp. "tax category
            lwa_knvi-taxkd = lwa_zsdt0322-taxkd. "tax classification
            APPEND lwa_knvi TO gt_knvi.
            CLEAR:  lwa_knvi.

          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

    ENDIF.

    CALL METHOD lo_bp->mt_bp_update_customer(
      EXPORTING
        im_test           = gv_test
        im_bp_type        = gv_bp_type
        is_kna1           = gs_kna1         " Mestre de fornecedores (parte geral)
        is_knvk           = gs_knvk         " Interface externa: dados detalhes bancários
        it_knb1           = gt_knb1         " Mestre de fornecedores (empresa)
        it_knbw           = gt_knbw         " Atual.dados mestre fornecedor: campos de tela e operativos
        it_knvi           = gt_knvi         " Mestre de clientes - indicadores de impostos
        it_knvv           = gt_knvv         " Mestre de clientes (área de vendas)
        it_adsmtp         = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        is_addr1_data     = gs_addr1_data   " Estrutura de transferência para endereços
        is_addr2_data     = gs_addr2_data   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        is_sza1_d0100     = gs_sza1_d0100   " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        is_personal_Data  = ls_personal_data
        is_personal_Data2 = ls_personal_data2
      CHANGING
        et_return         = gt_return       " Tabela de retorno
        em_partner        = lwa_data_response-parceiro ).    " Nº parceiro de negócios

    SORT gt_return BY type.
    READ TABLE gt_return ASSIGNING FIELD-SYMBOL(<fs_return>)
    WITH KEY type = 'E'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT gt_return ASSIGNING <fs_return>.

        APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
        <fs_erros> = <fs_return>-message.

      ENDLOOP.

    ELSE.

      lwa_data_response-msg_sucesso = 'Parceiro atualizado com sucesso!'.

    ENDIF.

  ENDMETHOD.


  METHOD atualiza_fornecedor.


**/ Constants
    CONSTANTS: lc_j    TYPE char1             VALUE 'J',
               lc_f    TYPE char1             VALUE 'F',
               lc_r    TYPE char7             VALUE 'REGULAR',
               lc_antt TYPE /netrin/compde_parameter VALUE 'ANTT_ERROR'.

**/ Internal tables
    DATA: lt_group    TYPE TABLE OF /netrin/group,
          lt_log_sint TYPE /netrin/comptt_log,
          lt_log      TYPE TABLE OF /netrin/log,
          lt_log_sufr TYPE /netrin/comptt_log,
          lt_log_ibam TYPE /netrin/comptt_log,
          lt_log_antt TYPE /netrin/comptt_log.

    DATA: lr_bukrs  TYPE RANGE OF bukrs,
          lr_emp_id TYPE RANGE OF zmmt0187-emp_id. " Rubenilson - 10.10.24 - 154893

**/ Work areas
    DATA: ls_tcode          TYPE /netrin/tcodes,
          ls_self_fill      TYPE /netrin/compst_self_fill,
          ls_search         TYPE /netrin/compst_search,
          ls_group          LIKE LINE OF lt_group,
          ls_log            TYPE /netrin/compst_log,
          ls_addr_in        TYPE com_jur,
          ls_addr_out       TYPE com_jur,
          ls_personal_Data  TYPE bus_ei_struc_central_person,
          ls_personal_data2 TYPE fsbp_ei_struc_person.

**/Ranges
    DATA: lr_message_return TYPE RANGE OF /netrin/log-status,
          lr_cnfprgo        TYPE RANGE OF /netrin/compde_parameter_value.

**/ Objects
    DATA: lo_adcn      TYPE REF TO /netrin/compcl_business,
          lo_utilities TYPE REF TO /netrin/compcl_utilities.

**/ Variables
    DATA: lv_account     TYPE ktokk,
          lv_person_type TYPE /netrin/compde_person_type,
          lv_j           TYPE char1,
          lv_f           TYPE char1,
          lv_lin         TYPE i,
          lv_status      TYPE /netrin/compde_status,
          lv_popup       TYPE char1,
          lv_antt_error  TYPE /netrin/compde_parameter_value.

    DATA: gv_test            TYPE abap_bool,
          gv_tcode           TYPE tcode,
          gv_bp_type         TYPE bu_type,
          gv_bu_group        TYPE bu_group,
          gt_adsmtp          TYPE bbpt_er_adsmtp,
          gt_bdc             TYPE bdcdata_tab,
          gt_knb1            TYPE cvis_knb1_t,
          gt_knbw            TYPE cvis_knbw_t,
          gt_knvi            TYPE cvis_knvi_t,
          gt_knbk            TYPE knbk_t,
          gt_knvv            TYPE cvis_knvv_t,
          gs_lfa1            TYPE lfa1,
          gt_lfm1            TYPE TABLE OF lfm1,
          gt_lfbk            TYPE lfbk_t,
          gt_lfb1            TYPE TABLE OF lfb1,
          gt_lfbw            TYPE TABLE OF lfbw,
          gt_return          TYPE bapiret2_t,
          gt_bdcdata         TYPE bdcdata_tab,
          gt_bdcmsg          TYPE tab_bdcmsgcoll,
          gs_addr1_data      TYPE addr1_data,
          gs_adsmtp          TYPE adsmtp,
          gs_but000          TYPE but000,
          gs_kna1            TYPE kna1,
          gs_knb1            TYPE knb1,
          knbw               TYPE knbw,
          knvi               TYPE knvi,
          knvk               TYPE knvk,
          knvv               TYPE knvv,
          cvis_lfb1_t        TYPE cvis_lfb1_t,
          cvis_lfbw_t        TYPE cvis_lfbw_t,
          rf02d              TYPE rf02d,
          gs_rf02k           TYPE rf02k,
          gs_sza1_d0100      TYPE sza1_d0100,
          gs_addr2_data      TYPE addr2_data,
          gt_cvis_lfm1_t     TYPE cvis_lfm1_t,
          gs_bus_joel_main   TYPE bus_joel_main,
          bu_group           TYPE bu_group,
          gv_partner         TYPE bu_partner,
          lv_dia             TYPE char2,
          lv_mes             TYPE char2,
          lv_ano             TYPE char4,
          ls_addr            TYPE bapibus1006_address,
          ls_bp_central_data TYPE bus_ei_struc_central, "Rubenilson - 01.10.24 #153421
          lt_bukrs           TYPE TABLE OF string. " Rubenilson - 10.10.24 - 154893

    DATA: lr_chave_banco  TYPE RANGE OF bnka-bankl,
          lr_chave_banco2 TYPE RANGE OF bnka-bankl.

    DATA: lo_bp TYPE REF TO zcl_bp_utils_2.

    gs_lfa1-lifnr = lwa_data_request-parceiro.

    SELECT SINGLE  *
      FROM ibpsupplier INTO @DATA(lwa_ibpsupplier)
     WHERE supplier = @gs_lfa1-lifnr.

    IF sy-subrc NE 0  .
      APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
      <fs_erros> = | BP para fornecedor { gs_lfa1-lifnr } não encontrada !|.
      EXIT.
    ENDIF.

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner = lwa_ibpsupplier-businesspartner  " Business Partner Number
      IMPORTING
        es_but000  = gs_but000.     " Business Partner Data

    IF gs_but000 IS NOT INITIAL.
      gv_bp_type = gs_but000-type.
      gv_bu_group = gs_but000-bu_group.
    ENDIF.

    CREATE OBJECT lo_bp.

    CALL FUNCTION 'BAPI_BUPA_FS_ADDRESS_GET'
      EXPORTING
        businesspartner = lwa_data_request-parceiro
      IMPORTING
        addressdata     = ls_addr.

    MOVE-CORRESPONDING ls_addr TO gs_addr1_data.
    gs_addr1_data-city1      = lwa_data_request-cidade.
    gs_addr1_data-city2      = lwa_data_request-bairro.
    gs_addr1_data-region     = lwa_data_request-regiao.
    gs_addr1_data-post_code1 = lwa_data_request-cep.
    gs_addr1_data-street     = lwa_data_request-rua.
    gs_addr1_data-house_num1 = lwa_data_request-numero.
    gs_addr1_data-country    = lwa_data_request-pais.

    gs_sza1_d0100-fax_number = lwa_data_request-telefone.
    gs_sza1_d0100-smtp_addr = lwa_data_request-email.

*** Inicio - Rubenilson - 01.10.24 #153421
*** Flags
    ls_bp_central_data-centralarchivingflag = lwa_data_request-marcacao_arq.
    ls_bp_central_data-centralblock         = lwa_data_request-bloqueio_central.
    gs_lfa1-nodel                              = lwa_data_request-bloq_centr_elimin.
    gs_lfa1-loevm                              = lwa_data_request-marc_elimin_centr.
    gs_lfa1-sperz                              = lwa_data_request-bloq_pagamento.
    gs_lfa1-sperr                              = lwa_data_request-todas_empresas.
    gs_lfa1-sperm                              = lwa_data_request-bloq_compras.
*** Fim - Rubenilson - 01.10.24 #153421

*** Inicio -  Rubenilson - 14.10.24 - #155127
    IF lwa_data_request-tipo_industria EQ 'SIM'.
      gs_lfa1-j_1kftind = 'AL5BAN-A'.
    ENDIF.
    gs_lfa1-j_1kfrepre = 'LINKANA'.
*** Fim -  Rubenilson - 14.10.24 - #155127

*** Dados bancários
*** Inicio - Rubenilson  - 14.10.24 #155127
    IF lwa_data_request-dados_bancarios IS NOT INITIAL.

      DATA(lt_dados_banco) = lwa_data_request-dados_bancarios.
      DELETE lt_dados_banco WHERE chave_banco IS INITIAL.
      IF lt_dados_banco IS NOT INITIAL.

        lr_chave_banco = VALUE #( FOR ls_dados_banco IN lt_dados_banco
                                 ( sign   = 'I'
                                   option = 'EQ'
                                   low    = ls_dados_banco-chave_banco ) ).

        lr_chave_banco2 = VALUE #( FOR ls_dados_banco IN lt_dados_banco
                                   ( sign   = 'I'
                                     option = 'CP'
                                     low    = ls_dados_banco-chave_banco(3) && '*' && ls_dados_banco-chave_banco+4(4) ) ).

        SELECT bankl
          FROM bnka
          INTO TABLE @DATA(lt_bnka)
          WHERE bankl IN @lr_chave_banco
             OR bankl IN @lr_chave_banco2.
        IF sy-subrc IS INITIAL.
          SORT lt_bnka BY bankl.
        ENDIF.

      ENDIF.

      LOOP AT lwa_data_request-dados_bancarios ASSIGNING FIELD-SYMBOL(<fs_dados_bancarios>).

        IF <fs_dados_bancarios>-chave_banco IS NOT INITIAL."Rubenilson - 10.10.24 - 154893

          APPEND INITIAL LINE TO gt_lfbk ASSIGNING FIELD-SYMBOL(<fs_lfbk>).

          READ TABLE lt_bnka ASSIGNING FIELD-SYMBOL(<fs_chave_banco>)
          WITH KEY bankl = <fs_dados_bancarios>-chave_banco
          BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.

            READ TABLE lt_bnka ASSIGNING <fs_chave_banco>
            WITH KEY bankl(3) = <fs_dados_bancarios>-chave_banco(3)
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_lfbk>-bankl = <fs_chave_banco>-bankl.
            ENDIF.
          ELSE.
            <fs_lfbk>-bankl = <fs_dados_bancarios>-chave_banco.
          ENDIF.

          <fs_lfbk>-banks = <fs_dados_bancarios>-regiao.
          <fs_lfbk>-bankn = <fs_dados_bancarios>-conta.
          <fs_lfbk>-bvtyp = <fs_dados_bancarios>-id.

        ENDIF.

      ENDLOOP.

    ENDIF.
*** Fim - Rubenilson  - 14.10.24 #155127

    ls_addr_in-country = gs_addr1_data-country.
    ls_addr_in-county  = gs_addr1_data-city2.
    ls_addr_in-state   = gs_addr1_data-region.
    ls_addr_in-city    = gs_addr1_data-city1.
    ls_addr_in-zipcode = gs_addr1_data-post_code1.

    CALL FUNCTION 'JURISDICTION_DETERMINE'
      EXPORTING
        address_in                     = ls_addr_in
        no_dialog_flag                 = abap_true
      IMPORTING
        address_out                    = ls_addr_out
      EXCEPTIONS
        input_incomplete               = 1
        no_tax_procedure               = 2
        no_taxjurcode_required         = 3
        taxjurcode_not_found           = 4
        rfcdest_not_found              = 5
        taxjur_not_unique_in_no_dialog = 6
        OTHERS                         = 7.
    IF sy-subrc = 0.
      gs_addr1_Data-taxjurcode = ls_addr_out-txjcd.
    ENDIF.

    gs_lfa1-name1 = lwa_data_request-nome(35).
    gs_lfa1-sortl = lwa_data_request-nome(10).
    gs_lfa1-stcd1 = lwa_data_request-cnpj.
    gs_lfa1-stcd2 = lwa_data_request-cpf.
    gs_lfa1-stcd3 = lwa_data_request-ie.

    IF lwa_data_request-empresa IS NOT INITIAL.

*** Inicio - Rubenilson - 10.10.24 - 154893
      LOOP AT lwa_data_request-empresa ASSIGNING FIELD-SYMBOL(<fs_empresa>).
        APPEND INITIAL LINE TO lr_emp_id ASSIGNING FIELD-SYMBOL(<fs_emp_id>).

        <fs_emp_id>-sign = 'I'.
        <fs_emp_id>-option = 'CP'.
        <fs_emp_id>-low    = '*' && <fs_empresa> && '*'.

      ENDLOOP.

      SELECT *
        FROM zmmt0187
        INTO TABLE @DATA(lt_ZMMT0187)
        WHERE emp_id IN @lr_emp_id.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_ZMMT0187 ASSIGNING FIELD-SYMBOL(<fs_zmmt0187>).
          SPLIT <fs_zmmt0187>-emp_id AT '/' INTO TABLE lt_bukrs.
        ENDLOOP.

        lr_bukrs = VALUE #( FOR ls_bukrs IN lt_BUKRS
                            LET s = 'I'
                                o = 'EQ'
                             IN sign   = s
                                option = o
                             (  low = ls_bukrs ) ).

        DELETE ADJACENT DUPLICATES FROM lr_bukrs COMPARING ALL FIELDS.

      ENDIF.
*** Fim - Rubenilson - 10.10.24 - 154893

      SELECT *
        FROM zsdt0317
        INTO TABLE @DATA(lt_0317)
        WHERE bukrs IN @lr_bukrs
          AND ktokd EQ @lwa_data_request-grupo
          AND cancelado EQ @space. "Rubenilson - 01.10.24 #153421
      IF sy-subrc IS INITIAL.

        SELECT *
          FROM zsdt0341
          INTO TABLE @DATA(lt_zsdt0341)
          FOR ALL ENTRIES IN @lt_0317
          WHERE id        = @lt_0317-id
            AND cancelado EQ @space. "Rubenilson - 01.10.24 #153421
        IF sy-subrc IS INITIAL.
          SORT lt_zsdt0341 BY id.

          LOOP AT lt_0317 ASSIGNING FIELD-SYMBOL(<fs_0317>).

            READ TABLE lt_zsdt0341 ASSIGNING FIELD-SYMBOL(<fs_zsdt0341>)
            WITH KEY id =  <fs_0317>-id
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              APPEND INITIAL LINE TO gt_lfb1 ASSIGNING FIELD-SYMBOL(<lf_lfb1>).

              <lf_lfb1>-bukrs = <fs_zsdt0341>-bukrs.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs_zsdt0341>-akont
                IMPORTING
                  output = <lf_lfb1>-akont.

              <lf_lfb1>-lifnr = lwa_data_request-parceiro. "Rubenilson - 01.10.24 #153421
              <lf_lfb1>-fdgrv = <fs_zsdt0341>-fdgrv.
              <lf_lfb1>-zterm = <fs_zsdt0341>-zterm.
              <lf_lfb1>-zwels = <fs_zsdt0341>-zwels.
              <lf_lfb1>-qland = <fs_zsdt0341>-qland.

*** Inicio - Rubenilson - 01.10.24 #153421
              <lf_lfb1>-nodel = lwa_data_request-bloq_elim_empresa.
              <lf_lfb1>-sperr = lwa_data_request-empresa_selec.
              <lf_lfb1>-loevm = lwa_data_request-marc_elim_empresa.
*** Fim - Rubenilson - 01.10.24 #153421

              IF lwa_data_request-grupo EQ 'ZPRF' OR
                 lwa_data_request-grupo EQ 'ZPRJ' .

                CALL METHOD me->preenche_lfbw
                  EXPORTING
                    bukrs            = <fs_0317>-bukrs
                    lwa_data_request = lwa_data_request
                  IMPORTING
                    lfbw             = gt_lfbw.

              ENDIF.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

** Inicio - Rubenilson - 01.10.24 #153421
*** Mapeamento aba compras - Inicio
    SELECT *
      FROM zsdt0342
      INTO TABLE @DATA(lt_zsdt0342).
    IF sy-subrc IS INITIAL.
      LOOP AT lt_zsdt0342 ASSIGNING FIELD-SYMBOL(<fs_zsdt0342>).
        APPEND INITIAL LINE TO gt_lfm1 ASSIGNING FIELD-SYMBOL(<fs_lfm1>).

        <fs_lfm1>-ekorg   = <fs_zsdt0342>-ekorg.
        <fs_lfm1>-waers   = <fs_zsdt0342>-waers.
        <fs_lfm1>-zterm   = <fs_zsdt0342>-zterm.
        <fs_lfm1>-inco1   = <fs_zsdt0342>-inco1.
        <fs_lfm1>-inco2_l = <fs_zsdt0342>-inco2_l.
        <fs_lfm1>-vsbed   = <fs_zsdt0342>-vsbed.
        <fs_lfm1>-webre   = <fs_zsdt0342>-webre.
        <fs_lfm1>-nrgew   = <fs_zsdt0342>-nrgew.
        <fs_lfm1>-kzaut   = <fs_zsdt0342>-kzaut.
        <fs_lfm1>-sperm   = lwa_data_request-bloq_comp_org_comp.

      ENDLOOP.
    ENDIF.
*** Mapeamento aba compras - Fim
*** Fim - Rubenilson - 01.10.24 #153421

    CALL METHOD lo_bp->mt_bp_update_supplier(
      EXPORTING
        im_test            = gv_test
        im_bp_type         = gv_bp_type      " Categoria do parceiro de negócios
        im_bu_group        = gv_bu_group     " ALRS
        is_lfa1            = gs_lfa1         " Mestre de fornecedores (parte geral)
        it_lfbk            = gt_lfbk         " Mestre de fornecedores (coordenadas do banco)
        it_lfb1            = gt_lfb1         " Mestre de fornecedores (empresa)
        it_lfbw            = gt_lfbw         " Mestre de fornecedores (empresa)
        is_rf02k           = gs_rf02k        " Atual.dados mestre fornecedor: campos de tela e operativos
        is_addr1_data      = gs_addr1_data   " Estrutura de transferência para endereços
        is_sza1_d0100      = gs_sza1_d0100   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        it_lfm1            = gt_lfm1
        is_personal_data   = ls_personal_data
        is_personal_data2  = ls_personal_data2
        is_bp_central_data = ls_bp_central_data " Rubenilson - 01.10.24 #153421
      IMPORTING
        et_return          = gt_return       " Tabela de retorno
        em_partner         = gv_partner      " Nº parceiro de negócios
    ).

    SORT gt_return BY type.
    READ TABLE gt_return ASSIGNING FIELD-SYMBOL(<fs_return>)
    WITH KEY type = 'E'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT gt_return ASSIGNING <fs_return>.
        APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
        <fs_erros> = <fs_return>-message.
      ENDLOOP.

    ELSE.

      lwa_data_response-msg_sucesso = 'Parceiro atualizado com sucesso!'.
      lwa_data_response-parceiro    = lwa_Data_request-parceiro.

    ENDIF.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD CRIA_CLIENTE.

    CONSTANTS: lc_person TYPE /netrin/compde_person_type VALUE 'A',
               lc_r      TYPE char7               VALUE 'REGULAR'.

    CONSTANTS: lc_j TYPE c VALUE 'J',
               lc_f TYPE c VALUE 'F'.

    DATA: lo_adcn      TYPE REF TO /netrin/compcl_business,
          lo_utilities TYPE REF TO /netrin/compcl_utilities,
          lo_bp        TYPE REF TO zcl_bp_utils_2.

    DATA: lr_bukrs TYPE RANGE OF bukrs.

    DATA: lt_group    TYPE TABLE OF /netrin/group,
          lt_log      TYPE /netrin/comptt_log,
          lt_log_sint TYPE /netrin/comptt_log,
          lt_log_sufr TYPE /netrin/comptt_log.

    DATA: ls_group          LIKE LINE OF lt_group,
          ls_log            TYPE /netrin/compst_log,
          ls_tcode          TYPE /netrin/tcodes,
          ls_self_fill      TYPE /netrin/compst_self_fill,
          ls_search         TYPE /netrin/compst_search,
          ls_personal_data  TYPE bus_ei_struc_central_person,
          ls_personal_data2 TYPE fsbp_ei_struc_person.

    DATA: lv_status      TYPE /netrin/compde_status,
          lv_person_type TYPE c,
          lv_account     TYPE kna1-ktokd,
          lv_lin         TYPE i,
          lv_j           TYPE c,
          lv_f           TYPE c,
          lv_popup       TYPE c,
          lv_teste       TYPE c.

    DATA: gv_test          TYPE  abap_bool,
          gv_tcode         TYPE  tcode,
          gt_adsmtp        TYPE  bbpt_er_adsmtp,
          gt_bdc           TYPE  bdcdata_tab,
          gt_knb1          TYPE  cvis_knb1_t,
          gt_knbw          TYPE  cvis_knbw_t,
          gt_knvi          TYPE  cvis_knvi_t,
          gt_knbk          TYPE  knbk_t,
          gt_knvv          TYPE  cvis_knvv_t,
          gt_lfbk          TYPE  lfbk_t,
          gt_return        TYPE  bapiret2_t,
          gt_bdcdata       TYPE  bdcdata_tab,
          gt_bdcmsg        TYPE  tab_bdcmsgcoll,
          gs_addr1_data    TYPE  addr1_data,
          gs_adsmtp        TYPE  adsmtp,
          gs_but000        TYPE  but000,
          gs_kna1          TYPE  kna1,
          gs_knb1          TYPE  knb1,
          gs_knbw          TYPE  knbw,
          gs_knvi          TYPE  knvi,
          gs_knvk          TYPE  knvk,
          gs_knvv          TYPE  knvv,
          gs_lfa1          TYPE  lfa1,
          gt_lfb1          TYPE  cvis_lfb1_t,
          gt_lfbw          TYPE  cvis_lfbw_t,
          gs_rf02d         TYPE	rf02d,
          gs_rf02k         TYPE	rf02k,
          gs_sza1_d0100    TYPE  sza1_d0100,
          gs_addr2_data    TYPE  addr2_data,
          gt_lfm1          TYPE  cvis_lfm1_t,
          gs_bus_joel_main TYPE	bus_joel_main,
          gv_bu_group      TYPE	bu_group,
          lv_dia           TYPE char2,
          lv_mes           TYPE char2,
          lv_ano           TYPE char4,
          lwa_knvv         TYPE knvv,
          lwa_knvi         TYPE knvi,
          lwa_knb1         TYPE knb1.


    DATA: lr_message_return TYPE RANGE OF /netrin/log-status.

    cs_kna1-ktokd = lwa_data_request-grupo.


*      SPLIT lwa_data_request-consulta_netrin-data_nasc AT '/' INTO lv_dia lv_mes lv_ano.

      ls_self_fill-gbdat = lv_ano && lv_mes && lv_dia.
      ls_self_fill-stcd2 = lwa_data_request-cpf.
      ls_self_fill-stcd3 = lwa_data_request-ie.
      ls_self_fill-regio = lwa_data_request-regiao.
      ls_self_fill-stcd1 = lwa_data_request-cnpj.

**\ Apenas Grpos de Contas mapeados
      CHECK ls_self_fill IS NOT INITIAL.

      IF ls_self_fill-stcd1 IS NOT INITIAL.
        cs_kna1-stcd1 = ls_self_fill-stcd1.
      ENDIF.

      IF ls_self_fill-stcd2 IS NOT INITIAL.
        cs_kna1-stcd2 = ls_self_fill-stcd2.
      ENDIF.

      IF ls_self_fill-stcd3 IS NOT INITIAL.
        cs_kna1-stcd3 = ls_self_fill-stcd3.
      ENDIF.

      IF ls_self_fill-regio IS NOT INITIAL.
        cs_addr1-region = ls_self_fill-regio.
      ENDIF.

*Se tiver CNPJ é PJ, se não PF TEM QUE AJUSTAR ESSA REGRA NOT FOUND
      IF ls_self_fill-stcd1 IS NOT INITIAL.
        lv_person_type = lc_j.
        lv_j = abap_true.
      ELSE.
        lv_person_type = lc_f.
        lv_f = abap_true.
      ENDIF.

      cs_kna1-name1 = lwa_data_request-nome(35).
      cs_kna1-sortl = lwa_data_request-nome(10).
      cs_kna1-stcd1 = lwa_data_request-cnpj.
      cs_kna1-stcd2 = lwa_data_request-cpf.

      cs_addr1-city1      = lwa_data_request-cidade.
      cs_addr1-city2      = lwa_data_request-bairro.
      cs_addr1-region     = lwa_data_request-regiao.
      cs_addr1-post_code1 = lwa_data_request-cep.
      cs_addr1-street     = lwa_data_request-rua.
      cs_addr1-house_num1 = lwa_data_request-numero.
      cs_addr1-country    = lwa_data_request-pais.

**** Local de nascimento
*    gs_sza1_d0100-fax_number = lwa_data_request-dados_gerais-fax.
*    gs_sza1_d0100-smtp_addr = lwa_data_request-dados_gerais-email.

*** Dados pessoais
    ls_personal_data-birthdate     = ls_self_fill-gbdat.

*    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO ls_personal_data-birthplace SEPARATED BY space.

*    ls_personal_data-maritalstatus = lwa_data_request-dados_gerais-estado_civil.
*    ls_personal_data2-proprty_st   = lwa_data_request-dados_gerais-regime_bens.

*** Local de nascimento
*    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO cs_kna1-stcd4 SEPARATED BY space.

    MOVE-CORRESPONDING cs_addr1 TO gs_addr1_Data.
    MOVE-CORRESPONDING cs_kna1 TO gs_kna1.

    CREATE OBJECT lo_bp.

    SELECT *
      FROM tbc001
      INTO @DATA(ls_tbc001)
      UP TO 1 ROWS
      WHERE ktokk = @lwa_data_request-grupo.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      gs_bus_joel_main-creation_group = ls_tbc001-bu_group.
      gs_bus_joel_main-partner_role = 'FLVN00'.
    ELSE.

      SELECT *
        FROM tbd001
        INTO @DATA(ls_tbd001)
        UP TO 1 ROWS
        WHERE ktokd = @lwa_data_request-grupo.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        gs_bus_joel_main-creation_group = ls_tbd001-bu_group.
        gs_bus_joel_main-partner_role = 'FLCU00'.
      ENDIF.

    ENDIF.

    IF lwa_data_request-empresa IS NOT INITIAL.

      lr_bukrs = VALUE #( FOR ls_bukrs IN lwa_data_request-empresa
                          LET s = 'I'
                              o = 'EQ'
                           IN sign   = s
                              option = o
                           (  low = ls_bukrs ) ).


      SELECT * FROM zsdt0317
                  INTO TABLE @DATA(it_zsdt0317)
                  WHERE cancelado <> 'X'
                   AND ktokd = @lwa_data_request-grupo
                   AND bukrs IN @lr_bukrs.

      SELECT * FROM zsdt0319
        INTO TABLE @DATA(it_zsdt0319)
        FOR ALL ENTRIES IN @it_zsdt0317
      WHERE cancelado <> 'X'
        AND id = @it_zsdt0317-id .

      SELECT * FROM zsdt0320
        INTO TABLE @DATA(it_zsdt0320)
        FOR ALL ENTRIES IN @it_zsdt0319
        WHERE id =  @it_zsdt0319-id
         AND seq_canal = @it_zsdt0319-seq_canal
         AND cancelado <> 'X'.

      SELECT * FROM zsdt0322
        INTO TABLE @DATA(it_zsdt0322)
        FOR ALL ENTRIES IN @it_zsdt0319
        WHERE id =  @it_zsdt0319-id
         AND  seq_canal = @it_zsdt0319-seq_canal
         AND  cancelado <> 'X'.

      SORT it_zsdt0317 BY bukrs.
      LOOP AT it_zsdt0317 INTO DATA(lwa_zsdt0317).

        CLEAR: lwa_knb1.
        lwa_knb1-bukrs = lwa_zsdt0317-bukrs.
        lwa_knb1-akont = lwa_zsdt0317-akont.
        lwa_knb1-zuawa = lwa_zsdt0317-zuawa.
        lwa_knb1-fdgrv = lwa_zsdt0317-fdgrv.
        lwa_knb1-zterm = lwa_zsdt0317-zterm.
        lwa_knb1-zwels = lwa_zsdt0317-zwels.

        APPEND lwa_knb1 TO gt_knb1.

        LOOP AT it_zsdt0319 INTO DATA(lwa_zsdt0319) WHERE id = lwa_zsdt0317-id.

* MESTRE DE CLIENTES: DADOS DE VENDAS E DISTRIBUIÇÃO
          CLEAR: lwa_knvv.
          lwa_knvv-vkorg    = lwa_zsdt0317-bukrs.
          lwa_knvv-vtweg    = lwa_zsdt0319-vtweg.

          LOOP AT it_zsdt0320 INTO DATA(lwa_zsdt0320) WHERE id = lwa_zsdt0319-id
                                                       AND  seq_canal = lwa_zsdt0319-seq_canal .

            READ TABLE it_zsdt0322 INTO DATA(lwa_zsdt0322) WITH KEY id = lwa_zsdt0319-id
                                                             seq_canal = lwa_zsdt0319-seq_canal.
            lwa_knvv-spart    = lwa_zsdt0320-spart.
            lwa_knvv-kalks    = lwa_zsdt0322-kalks.
            IF lwa_data_request-grupo EQ 'ZCPF' OR
               lwa_data_request-grupo EQ 'ZCPJ'.
              lwa_knvv-kdgrp    = '6'.
            ELSE.
              lwa_knvv-kdgrp    = '7'.
            ENDIF.
            lwa_knvv-waers    = lwa_zsdt0322-waers.
            lwa_knvv-ktgrd    = lwa_zsdt0322-ktgrd.
            lwa_knvv-versg    = lwa_zsdt0322-versg.
            lwa_knvv-lprio    = lwa_zsdt0322-lprio.
            lwa_knvv-vsbed    = lwa_zsdt0322-vsbed.
            lwa_knvv-kzazu    = lwa_zsdt0322-kzazu.
            lwa_knvv-kztlf    = lwa_zsdt0322-kztlf.
            lwa_knvv-perfk    = lwa_zsdt0322-perfk.

            APPEND lwa_knvv TO gt_knvv.

* MESTRE DE CLIENTES - INDICADORES DE IMPOSTOS
            CLEAR: gt_knvi[], lwa_knvi.

            lwa_knvi-aland = 'BR'.               "tax country
            lwa_knvi-tatyp = lwa_zsdt0322-tatyp. "tax category
            lwa_knvi-taxkd = lwa_zsdt0322-taxkd. "tax classification
            APPEND lwa_knvi TO gt_knvi.
            CLEAR:  lwa_knvi.

          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

    ENDIF.

    CALL METHOD lo_bp->mt_bp_create_customer(
      EXPORTING
        im_test           = lv_teste
        is_kna1           = gs_kna1         " Mestre de fornecedores (parte geral)
        is_knvk           = gs_knvk         " Interface externa: dados detalhes bancários
        it_knb1           = gt_knb1         " Mestre de fornecedores (empresa)
        it_knbw           = gt_knbw         " Atual.dados mestre fornecedor: campos de tela e operativos
        it_knvi           = gt_knvi         " Mestre de clientes - indicadores de impostos
        it_knvv           = gt_knvv         " Mestre de clientes (área de vendas)
        it_adsmtp         = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        is_addr1_data     = gs_addr1_data   " Estrutura de transferência para endereços
        is_addr2_data     = gs_addr2_data   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        is_sza1_d0100     = gs_sza1_d0100   " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        is_BUS_JOEL_MAIN  = gs_bus_joel_main
        is_personal_data  = ls_personal_data
        is_personal_data2 = ls_personal_data2
      CHANGING
        et_return         = gt_return       " Tabela de retorno
        em_partner        = lwa_data_response-parceiro ).      " Nº parceiro de negócios

    SORT gt_return BY type.
    READ TABLE gt_return ASSIGNING FIELD-SYMBOL(<fs_return>)
    WITH KEY type = 'E'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT gt_return ASSIGNING <fs_return>.

        APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
        <fs_erros> = <fs_return>-message.

      ENDLOOP.

    ELSE.

      lwa_data_response-msg_sucesso = 'Parceiro criado com sucesso!'.

    ENDIF.

  ENDMETHOD.


  METHOD cria_fornecedor.

**/ Constants
    CONSTANTS: lc_j    TYPE char1             VALUE 'J',
               lc_f    TYPE char1             VALUE 'F',
               lc_r    TYPE char7             VALUE 'REGULAR',
               lc_antt TYPE /netrin/compde_parameter VALUE 'ANTT_ERROR'.

**/ Internal tables
    DATA: lt_group           TYPE TABLE OF /netrin/group,
          lt_log_sint        TYPE /netrin/comptt_log,
          lt_log             TYPE TABLE OF /netrin/log,
          lt_log_sufr        TYPE /netrin/comptt_log,
          lt_log_ibam        TYPE /netrin/comptt_log,
          lt_log_antt        TYPE /netrin/comptt_log,
          ls_personal_data2  TYPE fsbp_ei_struc_person,
          ls_bp_central_data TYPE bus_ei_struc_central.

    DATA: lr_bukrs  TYPE RANGE OF bukrs,
          lr_emp_id TYPE RANGE OF zmmt0187-emp_id. " Rubenilson - 10.10.24 - 154893

**/ Work areas
    DATA: ls_tcode         TYPE /netrin/tcodes,
          ls_self_fill     TYPE /netrin/compst_self_fill,
          ls_search        TYPE /netrin/compst_search,
          ls_group         LIKE LINE OF lt_group,
          ls_log           TYPE /netrin/compst_log,
          ls_personal_data TYPE bus_ei_struc_central_person.

**/Ranges
    DATA: lr_message_return TYPE RANGE OF /netrin/log-status,
          lr_cnfprgo        TYPE RANGE OF /netrin/compde_parameter_value.

**/ Objects
    DATA: lo_adcn      TYPE REF TO /netrin/compcl_business,
          lo_utilities TYPE REF TO /netrin/compcl_utilities,
          lo_bp        TYPE REF TO zcl_bp_utils_2.

**/ Variables
    DATA: lv_account     TYPE ktokk,
          lv_person_type TYPE /netrin/compde_person_type,
          lv_j           TYPE char1,
          lv_f           TYPE char1,
          lv_lin         TYPE i,
          lv_status      TYPE /netrin/compde_status,
          lv_popup       TYPE char1,
          lv_antt_error  TYPE /netrin/compde_parameter_value.

    DATA: gv_test          TYPE abap_bool,
          gv_tcode         TYPE tcode,
          gt_adsmtp        TYPE bbpt_er_adsmtp,
          gt_bdc           TYPE bdcdata_tab,
          gt_knb1          TYPE cvis_knb1_t,
          gt_knbw          TYPE cvis_knbw_t,
          gt_knvi          TYPE cvis_knvi_t,
          gt_knbk          TYPE knbk_t,
          gt_knvv          TYPE cvis_knvv_t,
          gt_lfbk          TYPE lfbk_t,
          gt_lfb1          TYPE TABLE OF lfb1,
          gt_lfm1          TYPE TABLE OF lfm1,
          gt_lfbw          TYPE TABLE OF lfbw,
          gt_return        TYPE bapiret2_t,
          gt_bdcdata       TYPE bdcdata_tab,
          gt_bdcmsg        TYPE tab_bdcmsgcoll,
          gs_addr1_data    TYPE addr1_data,
          gs_adsmtp        TYPE adsmtp,
          gs_but000        TYPE but000,
          gs_kna1          TYPE kna1,
          gs_knb1          TYPE knb1,
          knbw             TYPE knbw,
          knvi             TYPE knvi,
          knvk             TYPE knvk,
          knvv             TYPE knvv,
          cvis_lfb1_t      TYPE cvis_lfb1_t,
          cvis_lfbw_t      TYPE cvis_lfbw_t,
          rf02d            TYPE rf02d,
          gs_rf02k         TYPE rf02k,
          gs_sza1_d0100    TYPE sza1_d0100,
          gs_addr2_data    TYPE addr2_data,
          gt_cvis_lfm1_t   TYPE cvis_lfm1_t,
          gs_bus_joel_main TYPE bus_joel_main,
          bu_group         TYPE bu_group,
          gv_partner       TYPE bu_partner,
          lv_dia           TYPE char2,
          lv_mes           TYPE char2,
          lv_ano           TYPE char4,
          lt_bukrs         TYPE TABLE OF string. " Rubenilson - 10.10.24 - 154893

    DATA: ld_process TYPE REF TO data.

    DATA: lr_chave_banco  TYPE RANGE OF bnka-bankl,
          lr_chave_banco2 TYPE RANGE OF bnka-bankl.

    FIELD-SYMBOLS: <fs_process> TYPE any.

    CREATE DATA ld_process TYPE /netrin/compst_search.

    lfa1-ktokk       = lwa_data_request-grupo.
    lfa1-name1       = lwa_data_request-nome(35).
    lfa1-sortl       = lwa_data_request-nome(10).
    lfa1-stcd1       = lwa_data_request-cnpj.
    lfa1-stcd2       = lwa_data_request-cpf.
    lfa1-stcd3       = lwa_data_request-ie.
    lfa1-scacd       = '9999'.
    addr1-city1      = lwa_data_request-cidade.
    addr1-city2      = lwa_data_request-bairro.
    addr1-region     = lwa_data_request-regiao.
    addr1-post_code1 = lwa_data_request-cep.
    addr1-street     = lwa_data_request-rua.
    addr1-house_num1 = lwa_data_request-numero.
    addr1-country    = lwa_data_request-pais.

*** Inicio - Rubenilson - 01.10.24 #153421
*** Flags
    ls_bp_central_data-centralarchivingflag = lwa_data_request-marcacao_arq.
    ls_bp_central_data-centralblock         = lwa_data_request-bloqueio_central.
    lfa1-nodel                              = lwa_data_request-bloq_centr_elimin.
    lfa1-loevm                              = lwa_data_request-marc_elimin_centr.
    lfa1-sperz                              = lwa_data_request-bloq_pagamento.
    lfa1-sperr                              = lwa_data_request-todas_empresas.
    lfa1-sperm                              = lwa_data_request-bloq_compras.
*** Fim - Rubenilson - 01.10.24 #153421

*** Inicio -  Rubenilson - 14.10.24 - #155127
    IF lwa_data_request-tipo_industria EQ 'SIM'.
      lfa1-j_1kftind = 'AL5BAN-A'.
    ENDIF.
    lfa1-j_1kfrepre = 'LINKANA'.
*** Fim -  Rubenilson - 14.10.24 - #155127

*** Inicio - MMSILVA - 01.04.25 - #172720
    IF lwa_data_request-grupo EQ 'ZFNF'.
      IF lwa_data_request-pis IS NOT INITIAL.
        lfa1-kraus = lwa_data_request-pis.
        lfa1-stenr = lwa_data_request-pis.
      ENDIF.

      IF lwa_data_request-data_nasc IS NOT INITIAL.
        lfa1-gbdat = lwa_data_request-data_nasc.
      ENDIF.

    ENDIF.
*** Fim - MMSILVA - 01.04.25 - #172720

*** Dados bancários
*** Inicio - Rubenilson  - 14.10.24 #155127
    IF lwa_data_request-dados_bancarios IS NOT INITIAL.

      DATA(lt_dados_banco) = lwa_data_request-dados_bancarios.
      DELETE lt_dados_banco WHERE chave_banco IS INITIAL.
      IF lt_dados_banco IS NOT INITIAL.

        lr_chave_banco = VALUE #( FOR ls_dados_banco IN lt_dados_banco
                                 ( sign   = 'I'
                                   option = 'EQ'
                                   low    = ls_dados_banco-chave_banco ) ).

        lr_chave_banco2 = VALUE #( FOR ls_dados_banco IN lt_dados_banco
                                   ( sign   = 'I'
                                     option = 'CP'
                                     low    = ls_dados_banco-chave_banco(3) && '*' && ls_dados_banco-chave_banco+4(4) ) ).

        SELECT bankl
          FROM bnka
          INTO TABLE @DATA(lt_bnka)
          WHERE bankl IN @lr_chave_banco
             OR bankl IN @lr_chave_banco2.
        IF sy-subrc IS INITIAL.
          SORT lt_bnka BY bankl.
        ENDIF.

      ENDIF.

      LOOP AT lwa_data_request-dados_bancarios ASSIGNING FIELD-SYMBOL(<fs_dados_bancarios>).

        IF <fs_dados_bancarios>-chave_banco IS NOT INITIAL."Rubenilson - 10.10.24 - 154893

          APPEND INITIAL LINE TO gt_lfbk ASSIGNING FIELD-SYMBOL(<fs_lfbk>).

          READ TABLE lt_bnka ASSIGNING FIELD-SYMBOL(<fs_chave_banco>)
          WITH KEY bankl = <fs_dados_bancarios>-chave_banco
          BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE lt_bnka ASSIGNING <fs_chave_banco>
            WITH KEY bankl(3) = <fs_dados_bancarios>-chave_banco(3)
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_lfbk>-bankl = <fs_chave_banco>-bankl.
            ENDIF.
          ELSE.
            <fs_lfbk>-bankl = <fs_dados_bancarios>-chave_banco.
          ENDIF.

          <fs_lfbk>-banks = <fs_dados_bancarios>-regiao.
          <fs_lfbk>-bankn = <fs_dados_bancarios>-conta.
          <fs_lfbk>-bvtyp = <fs_dados_bancarios>-id.

        ENDIF.

      ENDLOOP.

    ENDIF.
*** Fim - Rubenilson  - 14.10.24 #155127

*** Fax e Email
    gs_sza1_d0100-tel_number = lwa_data_request-telefone.
    gs_sza1_d0100-smtp_addr  = lwa_data_request-email.

    DATA(gs_lfa1)       = lfa1.

    gs_addr1_data = addr1.

*** Função parceiro e grupo
    SELECT *
      FROM tbc001
      INTO @DATA(ls_tbc001)
      UP TO 1 ROWS
      WHERE ktokk = @lwa_data_request-grupo.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      gs_bus_joel_main-creation_group = ls_tbc001-bu_group.
      gs_bus_joel_main-partner_role = 'FLVN00'.
    ELSE.

      SELECT *
        FROM tbd001
        INTO @DATA(ls_tbd001)
        UP TO 1 ROWS
        WHERE ktokd = @lwa_data_request-grupo.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        gs_bus_joel_main-creation_group = ls_tbd001-bu_group.
        gs_bus_joel_main-partner_role = 'FLCU00'.
      ENDIF.

    ENDIF.

    CREATE OBJECT lo_bp.

*** Mapeamento expansão de empresas - Inicio
    IF lwa_data_request-empresa IS NOT INITIAL.

*** Inicio - Rubenilson - 10.10.24 - 154893
      LOOP AT lwa_data_request-empresa ASSIGNING FIELD-SYMBOL(<fs_empresa>).
        APPEND INITIAL LINE TO lr_emp_id ASSIGNING FIELD-SYMBOL(<fs_emp_id>).

        <fs_emp_id>-sign = 'I'.
        <fs_emp_id>-option = 'CP'.
        <fs_emp_id>-low    = '*' && <fs_empresa> && '*'.

      ENDLOOP.

      SELECT *
        FROM zmmt0187
        INTO TABLE @DATA(lt_ZMMT0187)
        WHERE emp_id IN @lr_emp_id.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_ZMMT0187 ASSIGNING FIELD-SYMBOL(<fs_zmmt0187>).
          SPLIT <fs_zmmt0187>-emp_id AT '/' INTO TABLE lt_bukrs.
        ENDLOOP.

        lr_bukrs = VALUE #( FOR ls_bukrs IN lt_BUKRS
                            LET s = 'I'
                                o = 'EQ'
                             IN sign   = s
                                option = o
                             (  low = ls_bukrs ) ).

        DELETE ADJACENT DUPLICATES FROM lr_bukrs COMPARING ALL FIELDS.

      ENDIF.
*** Fim - Rubenilson - 10.10.24 - 154893

      SELECT *
        FROM zsdt0317
        INTO TABLE @DATA(lt_0317)
        WHERE bukrs IN @lr_bukrs
          AND ktokd EQ @lwa_data_request-grupo
          AND cancelado EQ @space. "Rubenilson - 01.10.24 #153421
      IF sy-subrc IS INITIAL.

        SELECT *
          FROM zsdt0341
          INTO TABLE @DATA(lt_zsdt0341)
          FOR ALL ENTRIES IN @lt_0317
          WHERE id = @lt_0317-id
            AND cancelado = @space. "Rubenilson - 01.10.24 #153421
        IF sy-subrc IS INITIAL.
          SORT lt_zsdt0341 BY id.

          LOOP AT lt_0317 ASSIGNING FIELD-SYMBOL(<fs_0317>).

            READ TABLE lt_zsdt0341 ASSIGNING FIELD-SYMBOL(<fs_zsdt0341>)
            WITH KEY id =  <fs_0317>-id
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              APPEND INITIAL LINE TO gt_lfb1 ASSIGNING FIELD-SYMBOL(<lf_lfb1>).

              <lf_lfb1>-bukrs = <fs_zsdt0341>-bukrs.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs_zsdt0341>-akont
                IMPORTING
                  output = <lf_lfb1>-akont.

              <lf_lfb1>-fdgrv = <fs_zsdt0341>-fdgrv.
              <lf_lfb1>-zterm = <fs_zsdt0341>-zterm.
              <lf_lfb1>-zwels = <fs_zsdt0341>-zwels.
              <lf_lfb1>-qland = <fs_zsdt0341>-qland.
*** Inicio - Rubenilson - 01.10.24 #153421
              <lf_lfb1>-nodel = lwa_data_request-bloq_elim_empresa.
              <lf_lfb1>-loevm = lwa_data_request-marc_elim_empresa.
*** Fim - Rubenilson - 01.10.24 #153421

              IF lwa_data_request-grupo EQ 'ZPRF' OR
                 lwa_data_request-grupo EQ 'ZPRJ' .

                CALL METHOD me->preenche_lfbw
                  EXPORTING
                    bukrs            = <fs_0317>-bukrs
                    lwa_data_request = lwa_data_request
                  IMPORTING
                    lfbw             = gt_lfbw.

              ENDIF.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.
*** Mapeamento expansão de empresas - Fim

*** Inicio - Rubenilson - 01.10.24 #153421
*** Mapeamento aba compras - Inicio
    SELECT *
      FROM zsdt0342
      INTO TABLE @DATA(lt_zsdt0342).
    IF sy-subrc IS INITIAL.
      LOOP AT lt_zsdt0342 ASSIGNING FIELD-SYMBOL(<fs_zsdt0342>).
        APPEND INITIAL LINE TO gt_lfm1 ASSIGNING FIELD-SYMBOL(<fs_lfm1>).

        <fs_lfm1>-ekorg   = <fs_zsdt0342>-ekorg.
        <fs_lfm1>-waers   = <fs_zsdt0342>-waers.
        <fs_lfm1>-zterm   = <fs_zsdt0342>-zterm.
        <fs_lfm1>-inco1   = <fs_zsdt0342>-inco1.
        <fs_lfm1>-inco2_l = <fs_zsdt0342>-inco2_l.
        <fs_lfm1>-vsbed   = <fs_zsdt0342>-vsbed.
        <fs_lfm1>-webre   = <fs_zsdt0342>-webre.
        <fs_lfm1>-nrgew   = <fs_zsdt0342>-nrgew.
        <fs_lfm1>-kzaut   = <fs_zsdt0342>-kzaut.
        <fs_lfm1>-sperm   = lwa_data_request-bloq_comp_org_comp.

      ENDLOOP.
    ENDIF.
*** Mapeamento aba compras - Fim
*** Fim - Rubenilson - 01.10.24 #153421

    CALL METHOD lo_bp->mt_bp_create_supplier(
      EXPORTING
        im_test            = gv_test
        is_lfa1            = gs_lfa1         " Mestre de fornecedores (parte geral)
        it_lfbk            = gt_lfbk         " Interface externa: dados detalhes bancários
        it_lfb1            = gt_lfb1         " Mestre de fornecedores (empresa)
        it_lfbw            = gt_lfbw         " Mestre de fornecedores (empresa)
        is_rf02k           = gs_rf02k        " Atual.dados mestre fornecedor: campos de tela e operativos
        is_addr1_data      = gs_addr1_data   " Estrutura de transferência para endereços
        is_sza1_d0100      = gs_sza1_d0100   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        is_bus_joel_main   = gs_bus_joel_main "ALRS
        it_adsmtp          = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        it_lfm1            = gt_lfm1
        is_personal_data   = ls_personal_data
        is_personal_data2  = ls_personal_data2
        is_bp_central_data = ls_bp_central_data "Rubenilson - 01.10.24 #153421
      CHANGING
        et_return          = gt_return       " Tabela de retorno
        em_partner         = lwa_data_response-parceiro      " Nº parceiro de negócios
    ).

    SORT gt_return BY type.
    READ TABLE gt_return ASSIGNING FIELD-SYMBOL(<fs_return>)
    WITH KEY type = 'E'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT gt_return ASSIGNING <fs_return>.

        APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
        <fs_erros> = <fs_return>-message.

      ENDLOOP.

    ELSE.

      lwa_data_response-msg_sucesso = 'Parceiro criado com sucesso!'.

    ENDIF.

  ENDMETHOD.


  METHOD PREENCHE_LFBW.

    IF lwa_Data_request-grupo EQ 'ZPRF'.

      IF lwa_data_request-regiao EQ 'MT'.
        APPEND INITIAL LINE TO lfbw ASSIGNING FIELD-SYMBOL(<fs_lfbw>).
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'FC'.
        <fs_lfbw>-wt_withcd = 'F0'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'FR'.
        <fs_lfbw>-wt_withcd = 'R0'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'FT'.
        <fs_lfbw>-wt_withcd = 'F1'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'IF'.
        <fs_lfbw>-wt_withcd = 'R3'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'SE'.
        <fs_lfbw>-wt_withcd = 'S0'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'SF'.
        <fs_lfbw>-wt_withcd = 'S0'.
        <fs_lfbw>-wt_subjct = abap_true.

      ELSEIF lwa_data_request-regiao EQ 'GO'.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'FG'.
        <fs_lfbw>-wt_withcd = 'F1'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'FI'.
        <fs_lfbw>-wt_withcd = 'R1'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'FR'.
        <fs_lfbw>-wt_withcd = 'R0'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'IF'.
        <fs_lfbw>-wt_withcd = 'R3'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'SE'.
        <fs_lfbw>-wt_withcd = 'S0'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'SF'.
        <fs_lfbw>-wt_withcd = 'S0'.
        <fs_lfbw>-wt_subjct = abap_true.

      ELSE.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'FR'.
        <fs_lfbw>-wt_withcd = 'R0'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'IF'.
        <fs_lfbw>-wt_withcd = 'R3'.
        <fs_lfbw>-wt_subjct = abap_true.


        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'SE'.
        <fs_lfbw>-wt_withcd = 'S0'.
        <fs_lfbw>-wt_subjct = abap_true.

        APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
        <fs_lfbw>-bukrs     = bukrs.
        <fs_lfbw>-witht     = 'SF'.
        <fs_lfbw>-wt_withcd = 'S0'.
        <fs_lfbw>-wt_subjct = abap_true.


      ENDIF.

    ELSEIF lwa_Data_request-grupo EQ 'ZPRJ'.

      APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
      <fs_lfbw>-bukrs     = bukrs.
      <fs_lfbw>-witht     = 'FC'.
      <fs_lfbw>-wt_withcd = 'F0'.
      <fs_lfbw>-wt_subjct = abap_true.

      APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
      <fs_lfbw>-bukrs     = bukrs.
      <fs_lfbw>-witht     = 'IM'.
      <fs_lfbw>-wt_withcd = 'IM'.
      <fs_lfbw>-wt_subjct = abap_true.

      APPEND INITIAL LINE TO lfbw ASSIGNING <fs_lfbw>.
      <fs_lfbw>-bukrs     = bukrs.
      <fs_lfbw>-witht     = 'FT'.
      <fs_lfbw>-wt_withcd = 'F1'.
      <fs_lfbw>-wt_subjct = abap_true.

    ENDIF.
  ENDMETHOD.


  METHOD VALIDA_DADOS_BANCO.
*
*    TYPES: BEGIN OF ty_data,
*             key(2)    TYPE c,
*             value(20) TYPE c,
*           END OF ty_data.
*
*    DATA: lo_webservice   TYPE REF TO /netrin/compif_webservice,
*          lt_but0bk(21)   TYPE c VALUE '(SAPLBUD0)GT_BUT0BK[]',
*          lt_bptax(32)    TYPE c VALUE '(SAPLBUPA_BUTX_DIALOG)GT_BPTAX[]',
*          lv_pgto_alt(35) TYPE c VALUE '(SAPLCVI_FS_UI_VENDOR)GS_LFA1-LNRZA',
*          ld_data         TYPE REF TO data,
*          lt_data_table   TYPE TABLE OF ty_data,
*          ls_data_table   TYPE ty_data,
*          lv_key_name     TYPE char10,
*          lv_key_value    TYPE char15,
*          lv_conta        TYPE string,
*          lv_digito_conta TYPE string,
*          ls_log          TYPE /netrin/log,
*          lt_log          TYPE TABLE OF /netrin/log,
*          lv_guid         TYPE guid_16,
*          lt_return       TYPE /netrin/comptt_return,
*          lv_search_date  TYPE dats,
*          et_return       TYPE TABLE OF bapiret2,
*          lt_ident        TYPE TABLE OF dfkkbptaxnum.
*
*    FIELD-SYMBOLS:
*      <fs_return_bp> TYPE bapiret2,
*      <fs_pgto_alt>  TYPE any.
*
*    DATA: lt_dados_banco TYPE TABLE OF but0bk.
*
*    IF lwa_data_request-consulta_netrin-cnpj IS NOT INITIAL.
*      APPEND INITIAL LINE TO lt_ident ASSIGNING FIELD-SYMBOL(<fs_ident>).
*      <fs_ident>-taxnum = lwa_data_request-consulta_netrin-cnpj.
*      <fs_ident>-taxtype = 'BR1'.
*    ELSEIF  lwa_data_request-consulta_netrin-cpf IS NOT INITIAL.
*      APPEND INITIAL LINE TO lt_ident ASSIGNING <fs_ident>.
*      <fs_ident>-taxnum = lwa_data_request-consulta_netrin-cpf.
*      <fs_ident>-taxtype = 'BR1'.
*    ELSE.
*      EXIT.
*    ENDIF.
*
*    LOOP AT lwa_data_request-dados_gerais-dados_bancarios ASSIGNING FIELD-SYMBOL(<fs_dados_bancarios>).
*
*      APPEND INITIAL LINE TO lt_dados_banco ASSIGNING FIELD-SYMBOL(<fs_dados_banco>).
*      <fs_dados_banco>-bkvid = <fs_dados_bancarios>-id.
*      <fs_dados_banco>-banks = <fs_dados_bancarios>-regiao.
*      <fs_dados_banco>-bankl = <fs_dados_bancarios>-chave_banco.
*      <fs_dados_banco>-bankn = <fs_dados_bancarios>-conta.
*
*    ENDLOOP.
*
*
*    IF lt_dados_banco IS NOT INITIAL.
*
*      REFRESH lt_return.
*
*      SELECT FROM but0bk
*        FIELDS *
*        WHERE partner EQ @iv_partner
*        INTO TABLE @DATA(lt_bankdata).
*
*      SELECT SINGLE FROM lfa1
*        FIELDS *
*        WHERE lifnr EQ @iv_partner
*        INTO @DATA(ls_lfa1).
*
*      SELECT SINGLE FROM kna1
*        FIELDS *
*        WHERE kunnr EQ @iv_partner
*        INTO @DATA(ls_kna1).
*
*      IF lt_ident IS INITIAL.
*
*        SELECT SINGLE FROM dfkkbptaxnum
*          FIELDS *
*          WHERE partner EQ @iv_partner
*          AND   taxtype IN ('BR1','BR2')
*          INTO @DATA(ls_dfkkbptaxnum).
*        IF sy-subrc = 0.
*
*          APPEND INITIAL LINE TO lt_ident ASSIGNING FIELD-SYMBOL(<fs_ident_ap>).
*          ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <fs_ident_ap> TO FIELD-SYMBOL(<partnr>).
*          ASSIGN COMPONENT 'TAXNUM' OF STRUCTURE <fs_ident_ap> TO FIELD-SYMBOL(<taxnum>).
*          ASSIGN COMPONENT 'TAXTYPE' OF STRUCTURE <fs_ident_ap> TO FIELD-SYMBOL(<taxtype>).
*
*          <partnr>  = iv_partner.
*          <taxtype> = ls_dfkkbptaxnum-taxtype.
*          <taxnum>  = ls_dfkkbptaxnum-taxnum.
*
*        ENDIF.
*
*      ENDIF.
*
*      lo_webservice = NEW /netrin/compcl_ws_vcb( ).
*      CREATE DATA ld_data TYPE TABLE OF ty_data.
*      FIELD-SYMBOLS: <data_table> TYPE STANDARD TABLE.
*
*      "Parâmetro para saber se faz a busca na API ou no log atravez da qtde de dias informado.
*      CALL METHOD /netrin/compcl_utilities=>get_single_value_parameter
*        EXPORTING
*          i_parameter = 'APINETRIN_SEARCH_DATE'
*        RECEIVING
*          r_value     = DATA(lv_qtde_days).
*
*      IF lv_qtde_days IS NOT INITIAL.
*
*        lv_search_date = ( sy-datum - lv_qtde_days ).
*
*      ELSE.
*
*        lv_search_date = sy-datum.
*
*      ENDIF.
*
*      "Parâmetro para saber se pesquisa todos os registros - Dados Bancários
*      CALL METHOD /netrin/compcl_utilities=>get_single_value_parameter
*        EXPORTING
*          i_parameter = 'APINETRIN_ALL_RECORDS'
*        RECEIVING
*          r_value     = DATA(lv_all_records).
*
*      READ TABLE lt_ident ASSIGNING FIELD-SYMBOL(<fs_ident_cnpj>) WITH KEY ('TAXTYPE') = 'BR1'.
*      IF sy-subrc = 0.
*
*        ASSIGN COMPONENT 'TAXNUM' OF STRUCTURE <fs_ident_cnpj> TO FIELD-SYMBOL(<taxnum_cnpj>).
*
*        SELECT FROM /netrin/log
*          FIELDS *
*          WHERE service_header EQ 'VDB'
*          AND stcd1            EQ @<taxnum_cnpj>
*          AND partner          EQ @iv_partner
*          AND search_date      GE @lv_search_date
*          INTO TABLE @lt_log.
*
*        APPEND VALUE #( key = 'CC' value = <taxnum_cnpj> ) TO lt_data_table.
*        ls_data_table-key   = 'CC'.
*        ls_data_table-value = <taxnum_cnpj>.
*
*      ELSE.
*
*        READ TABLE lt_ident ASSIGNING FIELD-SYMBOL(<fs_ident_cpf>) WITH KEY ('TAXTYPE') = 'BR2'.
*        IF sy-subrc = 0.
*
*          ASSIGN COMPONENT 'TAXNUM' OF STRUCTURE <fs_ident_cpf> TO FIELD-SYMBOL(<taxnum_cpf>).
*
*          SELECT FROM /netrin/log
*            FIELDS *
*            WHERE service_header EQ 'VDB'
*            AND stcd2            EQ @<taxnum_cpf>
*            AND partner          EQ @iv_partner
*            AND search_date      GE @lv_search_date
*            INTO TABLE @lt_log.
*
*          APPEND VALUE #( key = 'CC' value = <taxnum_cpf> ) TO lt_data_table.
*          ls_data_table-key   = 'CC'.
*          ls_data_table-value = <taxnum_cpf>.
*
*        ELSE.
*
*          APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
*          <fs_erros> = 'CNPJ/CPF Obrigatório para validação de dados bancários'.
*
*        ENDIF.
*
*      ENDIF.
*
*      LOOP AT lt_dados_banco ASSIGNING FIELD-SYMBOL(<fs_bankdata_tela>).
*
*        ASSIGN COMPONENT 'BKVID' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bkvid>).
*        ASSIGN COMPONENT 'BANKL' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bankl>).
*        ASSIGN COMPONENT 'BANKN' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bankn>).
*        ASSIGN COMPONENT 'BKONT' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bkont>).
*        ASSIGN COMPONENT 'BKREF' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bkref>).
*
*        TRANSLATE <bkref> TO UPPER CASE.
*
*        READ TABLE lt_bankdata ASSIGNING FIELD-SYMBOL(<fs_bankdata>) WITH KEY partner = iv_partner
*                                                                              bkvid   = <bkvid>.
*        IF sy-subrc = 0.
*
*          IF <fs_bankdata>-bankl EQ <bankl> AND
*             <fs_bankdata>-bankn EQ <bankn> AND
*             <fs_bankdata>-bkont EQ <bkont> AND
*             <fs_bankdata>-bkref EQ <bkref>.
*
*            IF lv_all_records EQ abap_true.
*
*              READ TABLE lt_log ASSIGNING FIELD-SYMBOL(<fs_log1>) WITH KEY bkvid = <fs_bankdata>-bkvid.
*              IF sy-subrc NE 0.
*
*                APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
*                APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.
*
*                SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
*                IF lv_digito_conta IS INITIAL.
*                  REFRESH: lt_data_table.
*                  APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.
*
*                  CALL FUNCTION 'GUID_CREATE'
*                    IMPORTING
*                      ev_guid_16 = lv_guid.
*
*                  ls_log-guid           = lv_guid.
*                  ls_log-service_header = 'VDB'.
*                  ls_log-service        = 'VDB'.
*                  ls_log-search_date    = sy-datum.
*                  ls_log-search_time    = sy-uzeit.
*
*                  IF <taxnum_cnpj> IS ASSIGNED.
*                    ls_log-stcd1 = <taxnum_cnpj>.
*                  ELSEIF <taxnum_cpf> IS ASSIGNED.
*                    ls_log-stcd2 = <taxnum_cpf>.
*                  ENDIF.
*
*                  ls_log-lifnr          = ls_lfa1-lifnr.
*                  ls_log-kunnr          = ls_kna1-kunnr.
*                  ls_log-tcode          = sy-tcode.
*                  ls_log-username       = sy-uname.
*                  ls_log-partner        = iv_partner.
*                  ls_log-bkvid          = <bkvid>.
*                  ls_log-banks          = 'BR'.
*                  ls_log-bankl          = <bankl>.
*                  ls_log-bankn          = <bankn>.
*                  ls_log-bkont          = <bkont>.
*                  ls_log-bkref          = <bkref>.
*                  ls_log-koinh          = ''.
*                  ls_log-validacaoconta = 'Não'.
*                  ls_log-status         = 'Consulta não realizada'.
*                  ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.
*
*                  APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*                  <fs_erros> = ls_log-mensagem.
*                  MODIFY /netrin/log FROM ls_log.
*                  RETURN.
*
*                ENDIF.
*
*                APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
*                APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.
*
*                IF <bkref> IS NOT INITIAL.
*
*                  IF <bkref> CS 'POUPAN'.
*
*                    APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.
*
*                  ENDIF.
*
*                  IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.
*
*                    APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.
*
*                  ENDIF.
*
*                ELSE.
*
*                  APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.
*
*                ENDIF.
*
*              ELSE.
*
*                IF <fs_log1>-search_date LT lv_search_date.
*
*                  APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
*                  APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.
*
*                  SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
*                  IF lv_digito_conta IS INITIAL.
*                    REFRESH: lt_data_table.
*                    APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.
*
*                    CALL FUNCTION 'GUID_CREATE'
*                      IMPORTING
*                        ev_guid_16 = lv_guid.
*
*                    ls_log-guid           = lv_guid.
*                    ls_log-service_header = 'VDB'.
*                    ls_log-service        = 'VDB'.
*                    ls_log-search_date    = sy-datum.
*                    ls_log-search_time    = sy-uzeit.
*
*                    IF <taxnum_cnpj> IS ASSIGNED.
*                      ls_log-stcd1 = <taxnum_cnpj>.
*                    ELSEIF <taxnum_cpf> IS ASSIGNED.
*                      ls_log-stcd2 = <taxnum_cpf>.
*                    ENDIF.
*
*                    ls_log-lifnr          = ls_lfa1-lifnr.
*                    ls_log-kunnr          = ls_kna1-kunnr.
*                    ls_log-tcode          = sy-tcode.
*                    ls_log-username       = sy-uname.
*                    ls_log-partner        = iv_partner.
*                    ls_log-bkvid          = <bkvid>.
*                    ls_log-banks          = 'BR'.
*                    ls_log-bankl          = <bankl>.
*                    ls_log-bankn          = <bankn>.
*                    ls_log-bkont          = <bkont>.
*                    ls_log-bkref          = <bkref>.
*                    ls_log-koinh          = ''.
*                    ls_log-validacaoconta = 'Não'.
*                    ls_log-status         = 'Consulta não realizada'.
*                    ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.
*
*                    APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*                    <fs_erros> = ls_log-mensagem.
*                    MODIFY /netrin/log FROM ls_log.
*                    RETURN.
*
*                  ENDIF.
*
*                  APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
*                  APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.
*
*                  IF <bkref> IS NOT INITIAL.
*
*                    IF <bkref> CS 'POUPAN'.
*
*                      APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.
*
*                    ENDIF.
*
*                    IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.
*
*                      APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.
*
*                    ENDIF.
*
*                  ELSE.
*
*                    APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.
*
*                  ENDIF.
*
*                ELSEIF <fs_log1>-validacaoconta NE 'SIM'.
*
*                  APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
*                  APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.
*
*                  SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
*                  IF lv_digito_conta IS INITIAL.
*                    REFRESH: lt_data_table.
*                    APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.
*
*                    CALL FUNCTION 'GUID_CREATE'
*                      IMPORTING
*                        ev_guid_16 = lv_guid.
*
*                    ls_log-guid           = lv_guid.
*                    ls_log-service_header = 'VDB'.
*                    ls_log-service        = 'VDB'.
*                    ls_log-search_date    = sy-datum.
*                    ls_log-search_time    = sy-uzeit.
*
*                    IF <taxnum_cnpj> IS ASSIGNED.
*                      ls_log-stcd1 = <taxnum_cnpj>.
*                    ELSEIF <taxnum_cpf> IS ASSIGNED.
*                      ls_log-stcd2 = <taxnum_cpf>.
*                    ENDIF.
*
*                    ls_log-lifnr          = ls_lfa1-lifnr.
*                    ls_log-kunnr          = ls_kna1-kunnr.
*                    ls_log-partner        = iv_partner.
*                    ls_log-tcode          = sy-tcode.
*                    ls_log-username       = sy-uname.
*                    ls_log-bkvid          = <bkvid>.
*                    ls_log-banks          = 'BR'.
*                    ls_log-bankl          = <bankl>.
*                    ls_log-bankn          = <bankn>.
*                    ls_log-bkont          = <bkont>.
*                    ls_log-bkref          = <bkref>.
*                    ls_log-koinh          = ''.
*                    ls_log-validacaoconta = 'Não'.
*                    ls_log-status         = 'Consulta não realizada'.
*                    ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.
*
*                    APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*                    <fs_erros> = ls_log-mensagem.
*                    MODIFY /netrin/log FROM ls_log.
*                    RETURN.
*
*                  ENDIF.
*
*                  APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
*                  APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.
*
*                  IF <bkref> IS NOT INITIAL.
*
*                    IF <bkref> CS 'POUPAN'.
*
*                      APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.
*
*                    ENDIF.
*
*                    IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.
*
*                      APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.
*
*                    ENDIF.
*
*                  ELSE.
*
*                    APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.
*
*                  ENDIF.
*
*                ELSE.
*
*                  CALL FUNCTION 'GUID_CREATE'
*                    IMPORTING
*                      ev_guid_16 = lv_guid.
*
*                  ls_log-guid           = lv_guid.
*                  ls_log-service_header = 'VDB'.
*                  ls_log-service        = 'VDB'.
*                  ls_log-search_date    = sy-datum.
*                  ls_log-search_time    = sy-uzeit.
*
*                  IF <taxnum_cnpj> IS ASSIGNED.
*                    ls_log-stcd1 = <taxnum_cnpj>.
*                  ELSEIF <taxnum_cpf> IS ASSIGNED.
*                    ls_log-stcd2 = <taxnum_cpf>.
*                  ENDIF.
*
*                  ls_log-lifnr          = ls_lfa1-lifnr.
*                  ls_log-kunnr          = ls_kna1-kunnr.
*                  ls_log-partner        = iv_partner.
*                  ls_log-tcode          = sy-tcode.
*                  ls_log-username       = sy-uname.
*                  ls_log-bkvid          = <bkvid>.
*                  ls_log-banks          = 'BR'.
*                  ls_log-bankl          = <bankl>.
*                  ls_log-bankn          = <bankn>.
*                  ls_log-bkont          = <bkont>.
*                  ls_log-bkref          = <bkref>.
*                  ls_log-koinh          = ''.
*                  ls_log-validacaoconta = <fs_log1>-validacaoconta.
*                  ls_log-status         = <fs_log1>-status.
*                  ls_log-mensagem       = <fs_log1>-mensagem.
*
*                  APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*                  <fs_erros> = ls_log-mensagem.
*
*                  MODIFY /netrin/log FROM ls_log.
*                  RETURN.
*
*                ENDIF.
*
*              ENDIF.
*
*            ELSE.
*
*              CONTINUE.
*
*            ENDIF.
*
*          ELSE.
*
**              IF lv_all_records EQ abap_true.
*
*            READ TABLE lt_log ASSIGNING FIELD-SYMBOL(<fs_log2>) WITH KEY bkvid = <fs_bankdata>-bkvid bankl = <bankl>.
*            IF sy-subrc NE 0.
*
*              APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
*              APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.
*
*              SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
*              IF lv_digito_conta IS INITIAL.
*                REFRESH: lt_data_table.
*                APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.
*
*                CALL FUNCTION 'GUID_CREATE'
*                  IMPORTING
*                    ev_guid_16 = lv_guid.
*
*                ls_log-guid           = lv_guid.
*                ls_log-service_header = 'VDB'.
*                ls_log-service        = 'VDB'.
*                ls_log-search_date    = sy-datum.
*                ls_log-search_time    = sy-uzeit.
*
*                IF <taxnum_cnpj> IS ASSIGNED.
*                  ls_log-stcd1 = <taxnum_cnpj>.
*                ELSEIF <taxnum_cpf> IS ASSIGNED.
*                  ls_log-stcd2 = <taxnum_cpf>.
*                ENDIF.
*
*                ls_log-lifnr          = ls_lfa1-lifnr.
*                ls_log-kunnr          = ls_kna1-kunnr.
*                ls_log-partner        = iv_partner.
*                ls_log-tcode          = sy-tcode.
*                ls_log-username       = sy-uname.
*                ls_log-bkvid          = <bkvid>.
*                ls_log-banks          = 'BR'.
*                ls_log-bankl          = <bankl>.
*                ls_log-bankn          = <bankn>.
*                ls_log-bkont          = <bkont>.
*                ls_log-bkref          = <bkref>.
*                ls_log-koinh          = ''.
*                ls_log-validacaoconta = 'Não'.
*                ls_log-status         = 'Consulta não realizada'.
*                ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.
*
*                APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*                <fs_erros> = ls_log-mensagem.
*
*                MODIFY /netrin/log FROM ls_log.
*                RETURN.
*
*              ENDIF.
*
*              APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
*              APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.
*
*              IF <bkref> IS NOT INITIAL.
*
*                IF <bkref> CS 'POUPAN'.
*
*                  APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.
*
*                ENDIF.
*
*                IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.
*
*                  APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.
*
*                ENDIF.
*
*              ELSE.
*
*                APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.
*
*              ENDIF.
*
*            ELSE.
*
*              IF <fs_log2>-search_date LT lv_search_date.
*
*                APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
*                APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.
*
*                SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
*                IF lv_digito_conta IS INITIAL.
*                  REFRESH: lt_data_table.
*                  APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.
*
*                  CALL FUNCTION 'GUID_CREATE'
*                    IMPORTING
*                      ev_guid_16 = lv_guid.
*
*                  ls_log-guid           = lv_guid.
*                  ls_log-service_header = 'VDB'.
*                  ls_log-service        = 'VDB'.
*                  ls_log-search_date    = sy-datum.
*                  ls_log-search_time    = sy-uzeit.
*
*                  IF <taxnum_cnpj> IS ASSIGNED.
*                    ls_log-stcd1 = <taxnum_cnpj>.
*                  ELSEIF <taxnum_cpf> IS ASSIGNED.
*                    ls_log-stcd2 = <taxnum_cpf>.
*                  ENDIF.
*
*                  ls_log-lifnr          = ls_lfa1-lifnr.
*                  ls_log-kunnr          = ls_kna1-kunnr.
*                  ls_log-partner        = iv_partner.
*                  ls_log-tcode          = sy-tcode.
*                  ls_log-username       = sy-uname.
*                  ls_log-bkvid          = <bkvid>.
*                  ls_log-banks          = 'BR'.
*                  ls_log-bankl          = <bankl>.
*                  ls_log-bankn          = <bankn>.
*                  ls_log-bkont          = <bkont>.
*                  ls_log-bkref          = <bkref>.
*                  ls_log-koinh          = ''.
*                  ls_log-validacaoconta = 'Não'.
*                  ls_log-status         = 'Consulta não realizada'.
*                  ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.
*
*                  APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*                  <fs_erros> = ls_log-mensagem.
*
*                  MODIFY /netrin/log FROM ls_log.
*                  RETURN.
*
*                ENDIF.
*
*                APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
*                APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.
*
*                IF <bkref> IS NOT INITIAL.
*
*                  IF <bkref> CS 'POUPAN'.
*
*                    APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.
*
*                  ENDIF.
*
*                  IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.
*
*                    APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.
*
*                  ENDIF.
*
*                ELSE.
*
*                  APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.
*
*                ENDIF.
*
*              ELSEIF <fs_log2>-validacaoconta NE 'SIM'.
*
*                APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
*                APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.
*
*                SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
*                IF lv_digito_conta IS INITIAL.
*                  REFRESH: lt_data_table.
*                  APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.
*
*                  CALL FUNCTION 'GUID_CREATE'
*                    IMPORTING
*                      ev_guid_16 = lv_guid.
*
*                  ls_log-guid           = lv_guid.
*                  ls_log-service_header = 'VDB'.
*                  ls_log-service        = 'VDB'.
*                  ls_log-search_date    = sy-datum.
*                  ls_log-search_time    = sy-uzeit.
*
*                  IF <taxnum_cnpj> IS ASSIGNED.
*                    ls_log-stcd1 = <taxnum_cnpj>.
*                  ELSEIF <taxnum_cpf> IS ASSIGNED.
*                    ls_log-stcd2 = <taxnum_cpf>.
*                  ENDIF.
*
*                  ls_log-lifnr          = ls_lfa1-lifnr.
*                  ls_log-kunnr          = ls_kna1-kunnr.
*                  ls_log-partner        = iv_partner.
*                  ls_log-tcode          = sy-tcode.
*                  ls_log-username       = sy-uname.
*                  ls_log-bkvid          = <bkvid>.
*                  ls_log-banks          = 'BR'.
*                  ls_log-bankl          = <bankl>.
*                  ls_log-bankn          = <bankn>.
*                  ls_log-bkont          = <bkont>.
*                  ls_log-bkref          = <bkref>.
*                  ls_log-koinh          = ''.
*                  ls_log-validacaoconta = 'Não'.
*                  ls_log-status         = 'Consulta não realizada'.
*                  ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.
*
*                  APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*                  <fs_erros> = ls_log-mensagem.
*
*                  MODIFY /netrin/log FROM ls_log.
*                  RETURN.
*
*                ENDIF.
*
*                APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
*                APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.
*
*                IF <bkref> IS NOT INITIAL.
*
*                  IF <bkref> CS 'POUPAN'.
*
*                    APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.
*
*                  ENDIF.
*
*                  IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.
*
*                    APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.
*
*                  ENDIF.
*
*                ELSE.
*
*                  APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.
*
*                ENDIF.
*
*              ELSE.
*
*                CONTINUE.
*
*              ENDIF.
*
*            ENDIF.
*
**              ELSE.
**
**                CONTINUE.
**
**              ENDIF.
*
*          ENDIF.
*
*        ELSE.
*
*          APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
*          APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.
*
*          SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
*          IF lv_digito_conta IS INITIAL.
*            REFRESH: lt_data_table.
*            APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.
*
*            CALL FUNCTION 'GUID_CREATE'
*              IMPORTING
*                ev_guid_16 = lv_guid.
*
*            ls_log-guid           = lv_guid.
*            ls_log-service_header = 'VDB'.
*            ls_log-service        = 'VDB'.
*            ls_log-search_date    = sy-datum.
*            ls_log-search_time    = sy-uzeit.
*
*            IF <taxnum_cnpj> IS ASSIGNED.
*              ls_log-stcd1 = <taxnum_cnpj>.
*            ELSEIF <taxnum_cpf> IS ASSIGNED.
*              ls_log-stcd2 = <taxnum_cpf>.
*            ENDIF.
*
*            ls_log-lifnr          = ls_lfa1-lifnr.
*            ls_log-kunnr          = ls_kna1-kunnr.
*            ls_log-partner        = iv_partner.
*            ls_log-tcode          = sy-tcode.
*            ls_log-username       = sy-uname.
*            ls_log-bkvid          = <bkvid>.
*            ls_log-banks          = 'BR'.
*            ls_log-bankl          = <bankl>.
*            ls_log-bankn          = <bankn>.
*            ls_log-bkont          = <bkont>.
*            ls_log-bkref          = <bkref>.
*            ls_log-koinh          = ''.
*            ls_log-validacaoconta = 'Não'.
*            ls_log-status         = 'Consulta não realizada'.
*            ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.
*
*            APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*            <fs_erros> = ls_log-mensagem.
*
*            MODIFY /netrin/log FROM ls_log.
*            RETURN.
*
*          ENDIF.
*
*          APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
*          APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.
*
*          IF <bkref> IS NOT INITIAL.
*
*            IF <bkref> CS 'POUPAN'.
*
*              APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.
*
*            ENDIF.
*
*            IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.
*
*              APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.
*
*            ENDIF.
*
*          ELSE.
*
*            APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.
*
*          ENDIF.
*
*        ENDIF.
*
*        ASSIGN ld_data->* TO <data_table>.
*
*        <data_table>[] = lt_data_table[].
*
***/ Send JSON request do WebService
*        lo_webservice->consume(
*          EXPORTING
*            i_data_table  = ld_data
*            io_webservice = lo_webservice
*          IMPORTING
*            e_table       = lt_return
*            e_json        = DATA(lv_json)
*        ).
*        IF lt_return IS NOT INITIAL.
*
*          READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_value>) WITH KEY ('FIELD') = 'MESSAGE'.
*          IF sy-subrc = 0.
*            ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_value> TO FIELD-SYMBOL(<m_value>).
*            IF <m_value> IS ASSIGNED.
*
*              IF <m_value> IS NOT INITIAL.
*                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return_bp>.
*                <fs_return_bp>-type = 'E'.
*                <fs_return_bp>-message = |{ 'ID:' }{ <bkvid> }{ <m_value> }|.
*
*              ELSE.
*
*                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return_bp>.
*                <fs_return_bp>-type = 'E'.
*                <fs_return_bp>-message = |{ 'ID:' }{ <bkvid> }{ 'Erro de comunicação ao executar a consulta!' }|.
*              ENDIF.
*
*              CALL FUNCTION 'GUID_CREATE'
*                IMPORTING
*                  ev_guid_16 = lv_guid.
*
*              ls_log-guid           = lv_guid.
*              ls_log-service_header = 'VDB'.
*              ls_log-service        = 'VDB'.
*              ls_log-search_date    = sy-datum.
*              ls_log-search_time    = sy-uzeit.
*
*              IF <taxnum_cnpj> IS ASSIGNED.
*                ls_log-stcd1 = <taxnum_cnpj>.
*              ELSEIF <taxnum_cpf> IS ASSIGNED.
*                ls_log-stcd2 = <taxnum_cpf>.
*              ENDIF.
*
*              ls_log-lifnr          = ls_lfa1-lifnr.
*              ls_log-kunnr          = ls_kna1-kunnr.
*              ls_log-partner        = iv_partner.
*              ls_log-tcode          = sy-tcode.
*              ls_log-username       = sy-uname.
*              ls_log-bkvid          = <bkvid>.
*              ls_log-banks          = 'BR'.
*              ls_log-bankl          = <bankl>.
*              ls_log-bankn          = <bankn>.
*              ls_log-bkont          = <bkont>.
*              ls_log-bkref          = <bkref>.
*              ls_log-koinh          = ''.
*              ls_log-validacaoconta = 'Não'.
*              ls_log-status         = 'FALHA NA CONSULTA'.
*              ls_log-mensagem       = <fs_return_bp>-message.
*
*              APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*              <fs_erros> = ls_log-mensagem.
*
*              MODIFY /netrin/log FROM ls_log.
*              RETURN.
*
*            ENDIF.
*          ENDIF.
*
*          READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_validacaoconta>) WITH KEY ('FIELD') = 'VALIDACAOCONTA'.
*          IF sy-subrc = 0.
*            ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_validacaoconta> TO FIELD-SYMBOL(<vc_value>).
*
*            READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_mensagem>) WITH KEY ('FIELD') = 'MENSAGEM'.
*            IF sy-subrc = 0.
*              ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_mensagem> TO FIELD-SYMBOL(<me_value>).
*              IF <me_value> IS NOT INITIAL.
*
*                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return_bp>.
*                <fs_return_bp>-type       = 'E'.
*                <fs_return_bp>-id         = 'Erro'.
*                <fs_return_bp>-number     = '999'.
*                <fs_return_bp>-message_v1 = |{ 'ID:' }{ <bkvid> }{ <me_value> }|.
*
*                CALL FUNCTION 'GUID_CREATE'
*                  IMPORTING
*                    ev_guid_16 = lv_guid.
*
*                ls_log-guid           = lv_guid.
*                ls_log-service_header = 'VDB'.
*                ls_log-service        = 'VDB'.
*                ls_log-search_date    = sy-datum.
*                ls_log-search_time    = sy-uzeit.
*
*                IF <taxnum_cnpj> IS ASSIGNED.
*                  ls_log-stcd1 = <taxnum_cnpj>.
*                ELSEIF <taxnum_cpf> IS ASSIGNED.
*                  ls_log-stcd2 = <taxnum_cpf>.
*                ENDIF.
*
*                ls_log-lifnr          = ls_lfa1-lifnr.
*                ls_log-kunnr          = ls_kna1-kunnr.
*                ls_log-partner        = iv_partner.
*                ls_log-tcode          = sy-tcode.
*                ls_log-username       = sy-uname.
*                ls_log-bkvid          = <bkvid>.
*                ls_log-banks          = 'BR'.
*                ls_log-bankl          = <bankl>.
*                ls_log-bankn          = <bankn>.
*                ls_log-bkont          = <bkont>.
*                ls_log-bkref          = <bkref>.
*                ls_log-koinh          = ''.
*                ls_log-validacaoconta = <vc_value>.
*                ls_log-status         = 'INATIVO'.
*                ls_log-mensagem       = <fs_return_bp>-message_v1.
*
*                "**/ Get Receipt HTML
*                READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_urlcomprovante1>) WITH KEY ('FIELD') = 'URLCOMPROVANTE'.
*                IF sy-subrc = 0.
*                  ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_urlcomprovante1> TO FIELD-SYMBOL(<urlcomprovante_value1>).
*
*                  IF /netrin/compcl_utilities=>get_single_value_parameter( 'COMPROVANTE_URL' ).
*                    ls_log-receipt = <urlcomprovante_value1>.
*                  ELSE.
*
*                    ls_log-receipt = NEW /netrin/compcl_utilities( )->convert_comprovante_from_url(
*                        io_webservice = NEW /netrin/compcl_ws_vcb( )
*                        i_url         = <urlcomprovante_value1>
*                        i_service     = 'VDB'
*                    ).
*
*                  ENDIF.
*
*                ENDIF.
*
*                APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
*                <fs_erros> = <me_value>.
*
*                MODIFY /netrin/log FROM ls_log.
*
*
*                EXIT.
*
*              ENDIF.
*
*            ELSEIF <vc_value> EQ 'SIM'.
*
*              CALL FUNCTION 'GUID_CREATE'
*                IMPORTING
*                  ev_guid_16 = lv_guid.
*
*              ls_log-guid           = lv_guid.
*              ls_log-service_header = 'VDB'.
*              ls_log-service        = 'VDB'.
*              ls_log-search_date    = sy-datum.
*              ls_log-search_time    = sy-uzeit.
*
*              IF <taxnum_cnpj> IS ASSIGNED.
*                ls_log-stcd1 = <taxnum_cnpj>.
*              ELSEIF <taxnum_cpf> IS ASSIGNED.
*                ls_log-stcd2 = <taxnum_cpf>.
*              ENDIF.
*
*              ls_log-lifnr          = ls_lfa1-lifnr.
*              ls_log-kunnr          = ls_kna1-kunnr.
*              ls_log-partner        = iv_partner.
*              ls_log-tcode          = sy-tcode.
*              ls_log-username       = sy-uname.
*              ls_log-bkvid          = <bkvid>.
*              ls_log-banks          = 'BR'.
*              ls_log-bankl          = <bankl>.
*              ls_log-bankn          = <bankn>.
*              ls_log-bkont          = <bkont>.
*              ls_log-bkref          = <bkref>.
*              ls_log-koinh          = ''.
*              ls_log-validacaoconta = <vc_value>.
*              ls_log-status         = 'ATIVO'.
*              ls_log-mensagem       = 'Conta Validado'.
*
*              "**/ Get Receipt HTML
*              READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_urlcomprovante2>) WITH KEY ('FIELD') = 'URLCOMPROVANTE'.
*              IF sy-subrc = 0.
*                ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_urlcomprovante2> TO FIELD-SYMBOL(<urlcomprovante_value2>).
*
*                IF /netrin/compcl_utilities=>get_single_value_parameter( 'COMPROVANTE_URL' ).
*                  ls_log-receipt = <urlcomprovante_value2>.
*                ELSE.
*
*                  ls_log-receipt = NEW /netrin/compcl_utilities( )->convert_comprovante_from_url(
*                      io_webservice = NEW /netrin/compcl_ws_vcb( )
*                      i_url         = <urlcomprovante_value2>
*                      i_service     = 'VDB'
*                  ).
*
*                ENDIF.
*
*              ENDIF.
*
*              MODIFY /netrin/log FROM ls_log.
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*        REFRESH: lt_data_table, lt_return.
*        APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.
*
*      ENDLOOP.
*
*    ENDIF.
*
  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  method zif_integracao_inbound~validar_dados_inbound.
*
    data: lwa_data_request like zde_data_request,
          wa_tx            type zsdt0327tx,
          lv_message       type string,
          lva_type         type dd01v-datatype,
*** US #183582 - MMSILVA - 26.06.2025 - Ini ***
          ls_return        type bapiret2,
          ls_bankaddress   type bapi1011_address,
          ls_bnka          type bnka,
          ls_pais_banco    type bapi1011_key-bank_ctry,
          ls_chave_banco   type bapi1011_key-bank_key.
*** US #183582 - MMSILVA - 26.06.2025 - Fim ***

    data: lr_chave_banco  type range of bnka-bankl,
          lr_chave_banco2 type range of bnka-bankl.

    clear: r_msg_erro.

    if me->zif_integracao_inject~at_info_request_http-ds_metodo ne zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'LK'.
      wa_tx-msg_processamento = 'Metodo informado não previsto!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      modify zsdt0327tx from wa_tx.
      clear: wa_tx.
      commit work.

      return.
    endif.

    if i_data_inbound is initial.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'LK'.
      wa_tx-msg_processamento = 'Payload Requisição não pode ser vazio!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      modify zsdt0327tx from wa_tx.
      clear: wa_tx.
      commit work.

      return.
    endif.

    /ui2/cl_json=>deserialize( exporting json = i_data_inbound changing data = lwa_data_request ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    if lwa_data_request is initial.
      r_msg_erro = 'Nenhum filtro foi informado!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'LK'.
      wa_tx-msg_processamento = 'Nenhum filtro foi informado!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      modify zsdt0327tx from wa_tx.
      clear: wa_tx.
      commit work.

      return.
    endif.

    if lwa_data_request-grupo is initial.
      r_msg_erro = 'Grupo de contas não informado!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'LK'.
      wa_tx-msg_processamento = 'Grupo de contas não informado!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      modify zsdt0327tx from wa_tx.
      clear: wa_tx.
      commit work.

      return.
    endif.

*** Dados bancários
*** Inicio - Rubenilson  - 14.10.24 #155127
    if lwa_data_request-dados_bancarios is not initial.

      data(lt_dados_banco) = lwa_data_request-dados_bancarios.
      delete lt_dados_banco where chave_banco is initial.
      if lt_dados_banco is not initial.

        lr_chave_banco = value #( for ls_dados_banco in lt_dados_banco
                                 ( sign   = 'I'
                                   option = 'EQ'
                                   low    = ls_dados_banco-chave_banco ) ).

        lr_chave_banco2 = value #( for ls_dados_banco in lt_dados_banco
                                   ( sign   = 'I'
                                     option = 'CP'
                                     low    = ls_dados_banco-chave_banco(3) && '*' && ls_dados_banco-chave_banco+4(4) ) ).

        select bankl
          from bnka
          into table @data(lt_bnka)
          where bankl in @lr_chave_banco.
*             or bankl in @lr_chave_banco2.
        if sy-subrc is initial.
          sort lt_bnka by bankl.
*** US #183582 - MMSILVA - 26.06.2025 - Ini ***
        else.
          loop at lwa_data_request-dados_bancarios assigning field-symbol(<fs_create_dados_bancarios>).

            data(ls_prefix_banco) = |{ <fs_create_dados_bancarios>-chave_banco(3) }%|.
            select single * from bnka into @ls_bnka where bankl like @ls_prefix_banco and erdat between '20250101' and @sy-datum.
            if sy-subrc is initial.
              ls_pais_banco             = <fs_create_dados_bancarios>-regiao.
*              ls_chave_banco            = |{ ls_bnka-bankl+0(4) }{ <fs_create_dados_bancarios>-chave_banco+4(4) }|.
              ls_chave_banco            = <fs_create_dados_bancarios>-chave_banco.
              ls_bankaddress-bank_name  = ls_bnka-banka.

              call function 'BAPI_BANK_CREATE'
                exporting
                  bank_ctry    = ls_pais_banco         " BANKS - País do banco
                  bank_key     = ls_chave_banco        " BANKL - Código do banco
                  bank_address = ls_bankaddress        " Nome do banco
                  i_xupdate    = 'X'                   " Atualização em update task
                importing
                  return       = ls_return.

              data(lv_error) = abap_false.
              if ls_return-type = 'E' or ls_return-type = 'A'.
                lv_error = abap_true.
              endif.

              if lv_error = abap_false.
                call function 'BAPI_TRANSACTION_COMMIT'
                  exporting
                    wait = 'X'.

                append ls_chave_banco to lt_bnka.
              endif.

            endif.

          endloop.
*** US #183582 - MMSILVA - 26.06.2025 - Fim ***
        endif.

      endif.

      loop at lwa_data_request-dados_bancarios assigning field-symbol(<fs_dados_bancarios>).

        if <fs_dados_bancarios>-chave_banco is not initial."Rubenilson - 10.10.24 - 154893

          read table lt_bnka assigning field-symbol(<fs_chave_banco>)
          with key bankl = <fs_dados_bancarios>-chave_banco
          binary search.
          if sy-subrc is not initial.

*            read table lt_bnka assigning <fs_chave_banco>
*            with key bankl(3) = <fs_dados_bancarios>-chave_banco(3)
*            binary search.
*            if sy-subrc is initial.
*              <fs_dados_bancarios>-chave_banco = <fs_chave_banco>-bankl.
*
*            else.

            concatenate 'Banco' <fs_dados_bancarios>-chave_banco 'para país/região BR não existe' into lv_message.

            r_msg_erro = lv_message.

            wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
            wa_tx-id_origem         = me->at_id_interface.
            wa_tx-origem_cadastro   = 'LK'.
            wa_tx-msg_processamento = lv_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro = sy-datum.
            wa_tx-hr_registro = sy-uzeit.

            modify zsdt0327tx from wa_tx.
            clear: wa_tx.
            commit work.

            return.


*            endif.

          endif.

        endif.

      endloop.

    endif.
*** Fim - Rubenilson  - 14.10.24 #155127

    if  lwa_data_request-cnpj is not initial.
      if lwa_data_request-cnpj ca '/.,:;"[]\{}|<>?/'.

        concatenate 'Caractere Especial não é permitido para o CNPJ:'  lwa_data_request-cnpj
        into lv_message  separated by space.

        r_msg_erro = lv_message.

        wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
        wa_tx-id_origem         = me->at_id_interface.
        wa_tx-origem_cadastro   = 'LK'.
        wa_tx-msg_processamento = lv_message.
        wa_tx-status_proc       = 'E'.
        wa_tx-id_cli_processado = ''.
        wa_tx-dt_registro = sy-datum.
        wa_tx-hr_registro = sy-uzeit.

        modify zsdt0327tx from wa_tx.
        clear: wa_tx.
        commit work.

        return.

      else.

        clear: lva_type.
        call function 'NUMERIC_CHECK'
          exporting
            string_in = lwa_data_request-cnpj
          importing
            htype     = lva_type.

        if lva_type ne 'NUMC'.

          concatenate 'String não permitido no campo CNPJ:'  lwa_data_request-cnpj
             into lv_message  separated by space.

          r_msg_erro = lv_message.

          wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
          wa_tx-id_origem         = me->at_id_interface.
          wa_tx-origem_cadastro   = 'LK'.
          wa_tx-msg_processamento = lv_message.
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.

          modify zsdt0327tx from wa_tx.
          clear: wa_tx.
          commit work.

          return.

        endif.

      endif.

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response,
          ls_addr1          TYPE addr1_data,
          ls_lfa1           TYPE lfa1,
          ls_kna1           TYPE kna1,
          wa_tx             TYPE zsdt0327tx,
          lt_tx             TYPE TABLE OF zsdt0327tx.

    DATA:
      lra_mblnr TYPE RANGE OF mblnr,
      lra_mjahr TYPE RANGE OF mjahr,
      lra_budat TYPE RANGE OF budat,
      lra_bldat TYPE RANGE OF bldat,
      lra_xblnr TYPE RANGE OF xblnr,
      lra_smbln TYPE RANGE OF mblnr,
      lra_smblp TYPE RANGE OF mblpo,
      lra_mat   TYPE RANGE OF mblnr.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
    ENDIF.

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound =  i_msg_inbound IMPORTING e_status_code  =  DATA(_status_code)  RECEIVING r_msg_erro = e_msg_erro ).

    IF e_msg_erro IS NOT INITIAL.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request
      ENDIF.

      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

    IF lwa_data_request-parceiro IS INITIAL.

      CALL METHOD me->cria_fornecedor
        EXPORTING
          account           = lwa_data_request-grupo
          lwa_data_request  = lwa_data_request
        IMPORTING
          lwa_data_response = lwa_data_response
        CHANGING
          addr1             = ls_addr1
          lfa1              = ls_lfa1.

      IF lwa_data_response-erros IS NOT INITIAL.

        LOOP AT lwa_data_response-erros  ASSIGNING FIELD-SYMBOL(<fs_erros>).

          wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
          wa_tx-id_origem         = me->at_id_interface.
          wa_tx-origem_cadastro   = 'LK'.
          wa_tx-msg_processamento = <fs_erros>.
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.

          APPEND wa_tx TO  lt_tx.
          CLEAR wa_tx.

        ENDLOOP.


        IF lt_tx IS NOT INITIAL.
          MODIFY zsdt0327tx FROM TABLE lt_tx.
          COMMIT WORK.
        ENDIF.

      ELSE.

        wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
        wa_tx-id_origem         = me->at_id_interface.
        wa_tx-origem_cadastro   = 'LK'.
        CONCATENATE 'Parceiro' lwa_data_response-parceiro 'criado com sucesso!' INTO wa_tx-msg_processamento SEPARATED BY space .
        wa_tx-status_proc       = 'E'.
        wa_tx-id_cli_processado = ''.
        wa_tx-dt_registro = sy-datum.
        wa_tx-hr_registro = sy-uzeit.
        MODIFY zsdt0327tx FROM wa_tx.
        COMMIT WORK.

      ENDIF.

    ELSE.

      CALL METHOD me->atualiza_fornecedor
        EXPORTING
          account           = lwa_data_request-grupo
          lwa_data_request  = lwa_data_request
        IMPORTING
          lwa_data_response = lwa_data_response
        CHANGING
          addr1             = ls_addr1
          lfa1              = ls_lfa1.

      IF lwa_data_response-erros IS NOT INITIAL.

        LOOP AT lwa_data_response-erros  ASSIGNING <fs_erros>.

          wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
          wa_tx-id_origem         = me->at_id_interface.
          wa_tx-origem_cadastro   = 'LK'.
          wa_tx-msg_processamento = <fs_erros>.
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.

          APPEND wa_tx TO  lt_tx.
          CLEAR wa_tx.

        ENDLOOP.


        IF lt_tx IS NOT INITIAL.
          MODIFY zsdt0327tx FROM TABLE lt_tx.
          COMMIT WORK.
        ENDIF.

      ELSE.

        wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
        wa_tx-id_origem         = me->at_id_interface.
        wa_tx-origem_cadastro   = 'LK'.
        CONCATENATE 'Parceiro' lwa_data_response-parceiro 'atualizado com sucesso!' INTO wa_tx-msg_processamento SEPARATED BY space .
        wa_tx-status_proc       = 'E'.
        wa_tx-id_cli_processado = ''.
        wa_tx-dt_registro = sy-datum.
        wa_tx-hr_registro = sy-uzeit.
        MODIFY zsdt0327tx FROM wa_tx.
        COMMIT WORK.

      ENDIF.

    ENDIF.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.
