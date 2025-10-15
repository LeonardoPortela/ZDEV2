class ZCL_INT_IB_CADAST_PARCEIRO_SE definition
  public
  final
  create public .

  public section.

    interfaces ZIF_INTEGRACAO_INJECT .
    interfaces ZIF_INTEGRACAO_INBOUND .

    types:
      BEGIN OF ty_dados_obrigatorios,
        grupo            TYPE ktokk,
        regiao           TYPE regio,
        ie               TYPE stcd3,
        cnpj             TYPE stcd1,
        cpf              TYPE stcd2,
        data_nasc        TYPE char10,
        nis_pis          TYPE kraus_cm,
        nome             TYPE name1_gp,
        rntrc_antt       TYPE bahns,
        ignorar_sintegra TYPE char1,
        ie_isento        TYPE char1,
        nis_pis_isento   TYPE char1,
      END OF ty_dados_obrigatorios .
    types:
      BEGIN OF ty_dados_banco,
        id                   TYPE char4,
        regiao               TYPE char2,
        chave_banco          TYPE bu_bankk,
        conta                TYPE bu_bankn,
        digito_verif_agencia TYPE bkont,
        indicacao_referencia TYPE bu_bkref,
      END OF ty_dados_banco ,

      BEGIN OF ty_impostos,
        categ_irf TYPE cvis_lfbw-witht,
        cod_irf   TYPE cvis_lfbw-wt_withcd,
      END OF ty_impostos .

    data:
    tb_impostos TYPE TABLE OF ty_impostos .

    data:
    tb_banco1 TYPE TABLE OF ty_dados_banco .

    TYPES: BEGIN OF ty_empresas,
             empresa  TYPE bukrs,
             impostos like tb_impostos,
           END OF ty_empresas .
    types:
      tb_empresas TYPE TABLE OF ty_empresas .

    types:
      BEGIN OF ty_dados_gerais,
        rua                   TYPE ad_street,
        numero                TYPE ad_hsnm1,
        cep                   TYPE ad_pstcd1,
        pais                  TYPE land1,
        cidade                TYPE ad_city1,
        bairro                TYPE ad_city2,
        estado                TYPE regio,
        fax                   TYPE ad_fxnmbr1,
        email                 TYPE ad_smtpadr,
        estado_civil          TYPE char1,
        regime_bens           TYPE char2,
        rg                    TYPE char20, "Rubenilson - 06.12.24 - US159528
        orgao_exp             TYPE char40, "Rubenilson - 06.12.24 - US159528
        emissor_nota_fiscal   TYPE char1,
        dados_bancarios       LIKE tb_banco1,
* ----> US #184108 - MMSILVA - 29.05.2025 - Inicio <----
        dados_bancarios_subst LIKE tb_banco1,
        telefone              TYPE lfa1-telf1,
        telefone_subst        TYPE lfa1-telf1,
        email_subst           TYPE ad_smtpadr,
* ----> US #184108 - MMSILVA - 29.05.2025 - Fim <----
      END OF ty_dados_gerais .

    data:
      tb_erros  TYPE TABLE OF bapiret2-message .

    data:
      BEGIN OF zde_data_request,
        parceiro        TYPE lifnr,
        consulta_netrin TYPE ty_dados_obrigatorios,
        dados_gerais    TYPE ty_dados_gerais,
        empresas        TYPE tb_empresas,
      END OF zde_data_request .
    data:
      BEGIN OF zde_data_response,
        parceiro    TYPE string,
        msg_sucesso TYPE string,
        erros       LIKE tb_erros,
      END OF zde_data_response .
    constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '219' ##NO_TEXT.

    methods CONSTRUCTOR
      raising
        ZCX_INTEGRACAO .
    methods CRIA_FORNECEDOR
      importing
        !ACCOUNT           type RF02K-KTOKK
        !LWA_DATA_REQUEST  like ZDE_DATA_REQUEST
      exporting
        !LWA_DATA_RESPONSE like ZDE_DATA_RESPONSE
      changing
        !ADDR1             type ADDR1_DATA
        !LFA1              type LFA1 .
    methods CRIA_CLIENTE
      importing
        !ACCOUNT           type RF02K-KTOKK
        !LWA_DATA_REQUEST  like ZDE_DATA_REQUEST
      exporting
        !LWA_DATA_RESPONSE like ZDE_DATA_RESPONSE
      changing
        !CS_ADDR1          type ADDR1_DATA
        !CS_KNA1           type KNA1 .
    methods VALIDA_DADOS_BANCO
      importing
        !LWA_DATA_REQUEST  like ZDE_DATA_REQUEST
        !IV_PARTNER        type BU_PARTNER
      exporting
        !LWA_DATA_RESPONSE like ZDE_DATA_RESPONSE .
    methods ATUALIZA_FORNECEDOR
      importing
        !ACCOUNT           type RF02K-KTOKK
        !LWA_DATA_REQUEST  like ZDE_DATA_REQUEST
      exporting
        !LWA_DATA_RESPONSE like ZDE_DATA_RESPONSE
      changing
        !ADDR1             type ADDR1_DATA
        !LFA1              type LFA1 .
    methods ATUALIZA_CLIENTE
      importing
        !ACCOUNT           type RF02K-KTOKK
        !LWA_DATA_REQUEST  like ZDE_DATA_REQUEST
      exporting
        !LWA_DATA_RESPONSE like ZDE_DATA_RESPONSE
      changing
        !CS_ADDR1          type ADDR1_DATA
        !CS_KNA1           type KNA1 .
    methods PREENCHE_LFBW
      importing
        !BUKRS            type BUKRS
        !LWA_DATA_REQUEST like ZDE_DATA_REQUEST
      exporting
        !LFBW             type LFBW_TAB .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CADAST_PARCEIRO_SE IMPLEMENTATION.


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

    DATA: ls_group           LIKE LINE OF lt_group,
          ls_log             TYPE /netrin/compst_log,
          ls_tcode           TYPE /netrin/tcodes,
          ls_self_fill       TYPE /netrin/compst_self_fill,
          ls_search          TYPE /netrin/compst_search,
          ls_personal_data   TYPE bus_ei_struc_central_person,
          ls_personal_data2  TYPE fsbp_ei_struc_person,
          ls_bp_central_data TYPE bus_ei_struc_central. "US #184108 - MMSILVA - 29.05.2025

    DATA: lv_status      TYPE /netrin/compde_status,
          lv_person_type TYPE c,
          lv_account     TYPE kna1-ktokd,
          lv_lin         TYPE i,
          lv_j           TYPE c,
          lv_f           TYPE c,
          lv_popup       TYPE c,
          lv_teste       TYPE c,
          lv_parceiro    TYPE bu_partner.

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
          gv_bp_type       TYPE bu_type,
          gs_alteracao     TYPE zde_bp_alteracao. "US #184108 - MMSILVA - 03.06.2025


    DATA: lr_message_return TYPE RANGE OF /netrin/log-status.

    cs_kna1-ktokd = lwa_data_request-consulta_netrin-grupo.
    cs_kna1-kunnr = lwa_data_request-parceiro. " Rubenilson - 01.10.24 #153421

*   US #184108 - MMSILVA - 29.05.2025 - Inicio
    MOVE-CORRESPONDING lwa_data_request-dados_gerais TO gs_alteracao.

    SELECT SINGLE  *
      FROM ibpsupplier INTO @DATA(lwa_ibpsupplier)
     WHERE supplier = @cs_kna1-kunnr.

    IF sy-subrc NE 0  .
      APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erro_bp>).
      <fs_erro_bp> = | BP para cliente { cs_kna1-kunnr } não encontrada !|.
      EXIT.
    ENDIF.

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner = lwa_ibpsupplier-businesspartner  " Business Partner Number
      IMPORTING
        es_but000  = gs_but000.     " Business Partner Data
*   US #184108 - MMSILVA - 29.05.2025 - Fim

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
    lv_account = lwa_data_request-consulta_netrin-grupo.

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


    SPLIT lwa_data_request-consulta_netrin-data_nasc AT '/' INTO lv_dia lv_mes lv_ano.

    ls_self_fill-gbdat = lv_ano && lv_mes && lv_dia.
    ls_self_fill-stcd2 = lwa_data_request-consulta_netrin-cpf.
    ls_self_fill-stcd3 = lwa_data_request-consulta_netrin-ie.
    ls_self_fill-regio = lwa_data_request-consulta_netrin-regiao.
    ls_self_fill-stcd1 = lwa_data_request-consulta_netrin-cnpj.

**\ Apenas Grpos de Contas mapeados
    CHECK ls_self_fill IS NOT INITIAL.

    IF ls_self_fill-stcd1 IS NOT INITIAL.
      cs_kna1-stcd1 = ls_self_fill-stcd1.
    ENDIF.

    IF ls_self_fill-stcd2 IS NOT INITIAL.
      cs_kna1-stcd2 = ls_self_fill-stcd2.
    ENDIF.

    IF ls_self_fill-stcd3 IS NOT INITIAL.
*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
      SELECT SINGLE stcd3 FROM kna1 INTO @DATA(ls_stcd3) WHERE kunnr = @cs_kna1-kunnr.
      IF ( lwa_data_request-consulta_netrin-grupo EQ 'ZCPF' OR lwa_data_request-consulta_netrin-grupo EQ 'ZCPJ' ) AND cs_kna1-kunnr IS NOT INITIAL AND ls_stcd3 <> ls_self_fill-stcd3.
        APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erro_stcd3>).
        CONCATENATE 'Grupo de Contas' lwa_data_request-consulta_netrin-grupo 'não permitido alterar IE.' INTO <fs_erro_stcd3> SEPARATED BY space.
        RETURN.
      ELSE.
        cs_kna1-stcd3 = ls_self_fill-stcd3.
      ENDIF.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***
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

**\ Preenhce work area para consultas conforme dados informados no POPUP
    MOVE-CORRESPONDING ls_self_fill TO ls_search.
*    ls_search-kunnr = iv_cliente.
    ls_search-ktokd = lv_account.

    IF lv_j EQ abap_true.
      READ TABLE lt_group INTO ls_group WITH KEY check_pj = lv_j.
    ELSE.
      READ TABLE lt_group INTO ls_group WITH KEY check_pf = lv_f.
    ENDIF.

    CHECK sy-subrc IS INITIAL.

**\ Sintegra
    IF ls_group-check_sintegra EQ abap_true AND
       ls_tcode-sint           EQ abap_true AND
       ls_search-regio         IS NOT INITIAL.

**/ Execute Call of Sintegra
      CALL METHOD /netrin/compcl_utilities=>execute_call_sintegra
        EXPORTING
          is_search       = ls_search
          iv_tcode        = sy-tcode
          iv_return_table = abap_true
        IMPORTING
          et_log          = lt_log_sint.

      DESCRIBE TABLE lt_log_sint  LINES lv_lin.

      IF lv_lin > 1.

*        /netrin/compcl_utilities=>popup_choice_ie( CHANGING ch_log = lt_log_sint ).
*
*        READ TABLE lt_log_sint INTO ls_log INDEX 1.

      ELSE.

        READ TABLE lt_log_sint INTO ls_log INDEX 1.

      ENDIF.

      IF ls_search-stcd3 IS NOT INITIAL AND
         ( ls_search-stcd3 <> ls_log-stcd3 ).

*        /netrin/compcl_utilities=>popup_choice_ie( EXPORTING iv_dif = abap_true
*                                                   CHANGING ch_log = lt_log_sint ).
*
*        READ TABLE lt_log_sint INTO ls_log INDEX 1.

      ENDIF.

      APPEND ls_log TO lt_log.

      lv_status = ls_log-status.

      IF ls_log-stcd1 IS NOT INITIAL.

        ls_search-stcd1 = ls_log-stcd1.

      ENDIF.

      IF ls_log-stcd2 IS NOT INITIAL.

        ls_search-stcd2 = ls_log-stcd2.

      ENDIF.

      IF ls_log-stcd3 IS NOT INITIAL.

        ls_search-stcd3 = ls_log-stcd3.

      ENDIF.

      IF ls_log-status IN lr_message_return.

        IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPF'.
          APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
          CONCATENATE 'Sintegra com Status' ls_log-status INTO <fs_erros> SEPARATED BY space.
          RETURN.
        ENDIF.
***\ Executa POPUP
*        CALL METHOD lo_utilities->execute_popup
*          EXPORTING
*            it_log = lt_log.
*
***\ Executa as ações cadastradas na tabela de parâmetro conforme o LOG
*        CALL METHOD /netrin/compcl_utilities=>execute_parameters_defined
*          EXPORTING
*            is_search = ls_search
*            is_log    = ls_log
*            iv_tcode  = sy-tcode.
*
***\ Se mensagem de Warning o fluxo continua
*        CLEAR: lv_status.

      ENDIF.

      CLEAR: ls_log,
             lt_log_sint.

    ENDIF.

**\ RFB
    IF ls_group-check_rfb EQ abap_true AND
       ls_tcode-rfb       EQ abap_true AND
       lv_status          NOT IN lr_message_return.

      CALL METHOD lo_adcn->search_rfb
        EXPORTING
          i_stcd1  = ls_search-stcd1
          i_stcd2  = ls_search-stcd2
          i_gbdat  = ls_search-gbdat
          i_regio  = ls_search-regio
        IMPORTING
          e_log    = ls_log
          e_status = lv_status.

      IF ls_log-status NE 'ATIVA' AND
         ls_log-status NE lc_r.

        IF ls_search-stcd1 IS NOT INITIAL.
          APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
          CONCATENATE 'CNPJ com status:' ls_log-status INTO <fs_erros> SEPARATED BY space.
          RETURN.
        ENDIF.

*        ls_log-status = lv_status.
*        APPEND ls_log TO lt_log.
*
*        lv_popup = abap_true.
*
****\ Executa POPUP
**        CALL METHOD lo_utilities->execute_popup
**          EXPORTING
**            it_log = lt_log.
*
***\ Executa as ações cadastradas na tabela de parâmetro conforme o LOG
*        CALL METHOD /netrin/compcl_utilities=>execute_parameters_defined
*          EXPORTING
*            is_search = ls_search
*            is_log    = ls_log
*            iv_tcode  = sy-tcode.

      ELSE.

        ls_log-status = lv_status.
        APPEND ls_log TO lt_log.

      ENDIF.

      CLEAR: ls_log,
             lv_status.

    ENDIF.

**\ Simples
    IF ls_group-check_simples EQ abap_true AND
       ls_tcode-simp          EQ abap_true.

      CALL METHOD lo_adcn->search_simples
        EXPORTING
          i_stcd1  = ls_search-stcd1
          i_regio  = ls_search-regio
          i_kunnr  = ls_search-kunnr
          i_lifnr  = ls_search-lifnr
        IMPORTING
          e_log    = ls_log
          e_status = lv_status.

      ls_log-status = lv_status.
      APPEND ls_log TO lt_log.

      IF NOT ls_log-status CS 'NAO'.
        cs_kna1-indtyp = 'Z2'.
      ENDIF.

      CLEAR: ls_log,
             lv_status,
             lv_popup.

    ENDIF.

**\ Suframa
    IF ls_group-check_suframa EQ abap_true AND
       ls_tcode-suframa       EQ abap_true.

      /netrin/compcl_utilities=>execute_call_suframa(
        EXPORTING
          is_search       = ls_search
          iv_tcode        = sy-tcode
          iv_return_table = abap_true
        IMPORTING
          et_log          = lt_log_sufr ).

      DESCRIBE TABLE lt_log_sufr LINES lv_lin.

      IF lv_lin > 1.

*        /netrin/compcl_utilities=>popup_choice_ie( CHANGING ch_log = lt_log_sufr ).
*
*        READ TABLE lt_log_sufr INTO ls_log INDEX 1.

      ELSE.

        READ TABLE lt_log_sufr INTO ls_log INDEX 1.

      ENDIF.

      cs_kna1-suframa = ls_log-suframa.

      APPEND ls_log TO lt_log.

      CLEAR: lt_log_sufr,
             ls_log,
             lv_popup.

    ENDIF.

**\ Preenche os campos com os valores da consulta
    /netrin/compcl_utilities=>fill_fields_register(
      EXPORTING
        it_log = lt_log
      IMPORTING
        es_addr1 = cs_addr1
        es_kna1  = cs_kna1 ).

*** US #184108 - MMSILVA - 29.05.2025 - Inicio ***
    "Limpa campo CNAE (não há valores nesse campo)
    CLEAR cs_kna1-cnae.

    "Preencher apenas no campo sobrenome em caso de Pessoa Física
    IF cs_kna1-stkzn IS NOT INITIAL.
      cs_addr1-name2 = cs_addr1-name1.
      CLEAR cs_addr1-name1.
    ENDIF.
*** US #184108 - MMSILVA - 29.05.2025 - Fim ***

    IF cs_addr1-city1 IS INITIAL.

      cs_addr1-city1      = lwa_data_request-dados_gerais-cidade.
      cs_addr1-city2      = lwa_data_request-dados_gerais-bairro.
      cs_addr1-region     = lwa_data_request-dados_gerais-estado.
      cs_addr1-post_code1 = lwa_data_request-dados_gerais-cep.
      cs_addr1-street     = lwa_data_request-dados_gerais-rua.
      cs_addr1-house_num1 = lwa_data_request-dados_gerais-numero.
      cs_addr1-country    = lwa_data_request-dados_gerais-pais.

    ENDIF.

*** Dados pessoais
    ls_personal_data-birthdate     = ls_self_fill-gbdat.
    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO ls_personal_data-birthplace SEPARATED BY space.
    ls_personal_data-maritalstatus = lwa_data_request-dados_gerais-estado_civil.
    ls_personal_data2-proprty_st   = lwa_data_request-dados_gerais-regime_bens.

    gs_sza1_d0100-fax_number = lwa_data_request-dados_gerais-fax.
    gs_sza1_d0100-smtp_addr = lwa_data_request-dados_gerais-email.
*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
    gs_sza1_d0100-tel_number = lwa_data_request-dados_gerais-telefone.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***

*** Local de nascimento
    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO cs_kna1-stcd4 SEPARATED BY space.

    MOVE-CORRESPONDING cs_addr1 TO gs_addr1_Data.
    MOVE-CORRESPONDING cs_kna1 TO gs_kna1.

    CREATE OBJECT lo_bp.

    SELECT *
      FROM tbc001
      INTO @DATA(ls_tbc001)
      UP TO 1 ROWS
      WHERE ktokk = @lwa_data_request-consulta_netrin-grupo.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      gs_bus_joel_main-creation_group = ls_tbc001-bu_group.
      gs_bus_joel_main-partner_role = 'FLVN00'.
    ELSE.

      SELECT *
        FROM tbd001
        INTO @DATA(ls_tbd001)
        UP TO 1 ROWS
        WHERE ktokd = @lwa_data_request-consulta_netrin-grupo.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        gs_bus_joel_main-creation_group = ls_tbd001-bu_group.
        gs_bus_joel_main-partner_role = 'FLCU00'.
      ENDIF.

    ENDIF.

    IF lwa_data_request-empresas IS NOT INITIAL.

      lr_bukrs = VALUE #( FOR ls_bukrs IN lwa_data_request-empresas
                          LET s = 'I'
                              o = 'EQ'
                           IN sign   = s
                              option = o
                           (  low = ls_bukrs-empresa ) ).


      SELECT * FROM zsdt0317
                  INTO TABLE @DATA(it_zsdt0317)
                  WHERE cancelado <> 'X'
                   AND ktokd = @lwa_data_request-consulta_netrin-grupo
                   AND bukrs IN @lr_bukrs
                   AND cancelado EQ @abap_false.
      IF sy-subrc IS INITIAL." Rubenilson - 01.10.24 #153421
        SELECT * FROM zsdt0319
          INTO TABLE @DATA(it_zsdt0319)
          FOR ALL ENTRIES IN @it_zsdt0317
        WHERE cancelado <> 'X'
          AND id = @it_zsdt0317-id .
        IF sy-subrc IS INITIAL." Rubenilson - 01.10.24 #153421
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
        ENDIF.

      ENDIF.

      SORT it_zsdt0317 BY bukrs.
      LOOP AT it_zsdt0317 INTO DATA(lwa_zsdt0317).

        CLEAR: lwa_knb1.
        lwa_knb1-bukrs = lwa_zsdt0317-bukrs.
        lwa_knb1-akont = lwa_zsdt0317-akont.
        lwa_knb1-zuawa = lwa_zsdt0317-zuawa.
        lwa_knb1-fdgrv = lwa_zsdt0317-fdgrv.
        lwa_knb1-zterm = lwa_zsdt0317-zterm.
        lwa_knb1-zwels = lwa_zsdt0317-zwels.
***     US #184108 - MMSILVA - 29.05.2025 - Inicio ***
        lwa_knb1-kunnr = lwa_data_request-parceiro.
***     US #184108 - MMSILVA - 29.05.2025 - Fim ***

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
            IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPF' OR
               lwa_data_request-consulta_netrin-grupo EQ 'ZCPJ'.
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
        is_alteracao      = gs_alteracao "US #184108 - MMSILVA - 03.06.2025
      CHANGING
        et_return         = gt_return       " Tabela de retorno
        em_partner        = lv_parceiro ).    " Nº parceiro de negócios

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
      lwa_data_response-parceiro = lv_parceiro.
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

    DATA: lr_bukrs TYPE RANGE OF bukrs.

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
          ls_bp_central_data TYPE bus_ei_struc_central, "US #184108 - MMSILVA - 29.05.2025
          gs_alteracao       TYPE zde_bp_alteracao. "US #184108 - MMSILVA - 29.05.2025

    DATA: lo_bp TYPE REF TO zcl_bp_utils_2.

*   US #184108 - MMSILVA - 29.05.2025 - Inicio
    MOVE-CORRESPONDING lwa_data_request-dados_gerais TO gs_alteracao.
*   US #184108 - MMSILVA - 29.05.2025 - Fim

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
    gs_addr1_data-city1      = lwa_data_request-dados_gerais-cidade.
    gs_addr1_data-city2      = lwa_data_request-dados_gerais-bairro.
    gs_addr1_data-region     = lwa_data_request-dados_gerais-estado.
    gs_addr1_data-post_code1 = lwa_data_request-dados_gerais-cep.
    gs_addr1_data-street     = lwa_data_request-dados_gerais-rua.
    gs_addr1_data-house_num1 = lwa_data_request-dados_gerais-numero.
    gs_addr1_data-country    = lwa_data_request-dados_gerais-pais.

    gs_sza1_d0100-fax_number = lwa_data_request-dados_gerais-fax.
    gs_sza1_d0100-smtp_addr = lwa_data_request-dados_gerais-email.

*   US #184108 - MMSILVA - 29.05.2025 - Inicio
    gs_sza1_d0100-tel_number = lwa_data_request-dados_gerais-telefone.

    gs_lfa1-j_1kfrepre = 'SOFTEXPERT'.

    SPLIT lwa_data_request-consulta_netrin-data_nasc AT '/' INTO lv_dia lv_mes lv_ano.
    gs_lfa1-gbdat = lv_ano && lv_mes && lv_dia.
*   US #184108 - MMSILVA - 29.05.2025 - Fim

    ls_addr_in-country = gs_addr1_data-country.
    ls_addr_in-county  = gs_addr1_data-city2.
    ls_addr_in-state   = gs_addr1_data-region.
    ls_addr_in-city    = gs_addr1_data-city1.
    ls_addr_in-zipcode = gs_addr1_data-post_code1.

*** Local de nascimento
    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO gs_lfa1-stcd4 SEPARATED BY space.

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

    IF lwa_data_request-dados_gerais-emissor_nota_fiscal EQ abap_true.
      gs_lfa1-scacd = '9999'.
    ELSE.
      gs_lfa1-scacd = '8888'.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZFFJ' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZFFF'.

      gs_lfa1-dlgrp = '0001'.

    ENDIF.

*** Inicio - Rubenilson - 06.12.24 #159528
    IF lwa_data_request-consulta_netrin-grupo NE 'ZCPF' AND
       lwa_data_request-consulta_netrin-grupo NE 'ZCPJ' AND
      lwa_data_request-consulta_netrin-grupo NE 'ZPRF' AND
      lwa_data_request-consulta_netrin-grupo NE 'ZPRJ'.
*** Fim - Rubenilson - 06.12.24 #159528

      SELECT SINGLE * "US #184108 - MMSILVA - 15.06.2025
        FROM lfa1
        INTO @DATA(ls_name1)
        WHERE lifnr = @gs_lfa1-lifnr.

      IF lwa_data_request-consulta_netrin-nome IS INITIAL.

*** US #184108 - MMSILVA - 15.06.2025 - Ini ***
        IF ls_name1-stkzn IS NOT INITIAL.
          gs_lfa1-name2 = ls_name1-name1.
        ELSE.
          gs_lfa1-name1 = ls_name1-name1.
        ENDIF.
*** US #184108 - MMSILVA - 15.06.2025 - Fim ***

      ELSE.

*** US #184108 - MMSILVA - 15.06.2025 - Fim ***
        IF ls_name1-stkzn IS INITIAL.
          gs_lfa1-name1 = lwa_data_request-consulta_netrin-nome(35).
        ELSE.
          gs_lfa1-name2 = lwa_data_request-consulta_netrin-nome(35).
        ENDIF.
*** US #184108 - MMSILVA - 15.06.2025 - Fim ***
        gs_lfa1-sortl = lwa_data_request-consulta_netrin-nome(10).

      ENDIF.

    ENDIF." Rubenilson - 06.12.24 #159528

*** Local de nascimento
    IF gs_lfa1-gbort IS INITIAL.
      CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO gs_lfa1-gbort SEPARATED BY space.
    ENDIF.
    ls_personal_data-maritalstatus = lwa_data_request-dados_gerais-estado_civil.

    ls_personal_data2-proprty_st = lwa_data_request-dados_gerais-regime_bens.

    DATA(lt_Dados_banco) = lwa_data_request-dados_gerais-dados_bancarios.
    DELETE lt_Dados_banco WHERE chave_banco IS INITIAL.

    IF lt_Dados_banco IS NOT INITIAL.

      CALL METHOD me->valida_dados_banco(
        EXPORTING
          lwa_data_request  = lwa_data_request
          iv_partner        = gs_lfa1-lifnr
        IMPORTING
          lwa_data_response = lwa_data_response ).

      IF lwa_data_response-erros IS NOT INITIAL.
*        RETURN. "US #174393 - MMSILVA - 17.04.2025 - Comentado devido gerar cadastro incompleto ZPRF/ZCPF e ZPRJ/ZCPJ.

      ELSE.
        LOOP AT lwa_data_request-dados_gerais-dados_bancarios ASSIGNING FIELD-SYMBOL(<fs_banco>).
          APPEND INITIAL LINE TO gt_lfbk ASSIGNING FIELD-SYMBOL(<fs_lfbk>).
          <fs_lfbk>-bankl = <fs_banco>-chave_banco.
          <fs_lfbk>-bankn = <fs_banco>-conta.
          <fs_lfbk>-banks = <fs_banco>-regiao.
          <fs_lfbk>-bkont = <fs_banco>-digito_verif_agencia.
          <fs_lfbk>-bkref = <fs_banco>-indicacao_referencia.
*** US #184108 - MMSILVA - 13.06.2025 - Ini ***
          <fs_lfbk>-koinh = <fs_banco>-id.
*** US #184108 - MMSILVA - 13.06.2025 - Fim ***
        ENDLOOP.
      ENDIF.

    ENDIF.

    IF lwa_data_request-empresas IS NOT INITIAL.

      lr_bukrs = VALUE #( FOR ls_bukrs IN lwa_data_request-empresas
                          LET s = 'I'
                              o = 'EQ'
                           IN sign   = s
                              option = o
                           (  low = ls_bukrs-empresa ) ).

      SELECT *
        FROM zsdt0317
        INTO TABLE @DATA(lt_0317)
        WHERE bukrs IN @lr_bukrs
          AND ktokd EQ @lwa_data_request-consulta_netrin-grupo
          AND cancelado EQ @abap_false.
      IF sy-subrc IS INITIAL.

        SELECT *
          FROM zsdt0341
          INTO TABLE @DATA(lt_zsdt0341)
          FOR ALL ENTRIES IN @lt_0317
          WHERE id = @lt_0317-id.
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
*             US #184108 - MMSILVA - 29.05.2025 - Inicio
              <lf_lfb1>-lifnr = lwa_data_request-parceiro.
*             US #184108 - MMSILVA - 29.05.2025 - Fim

              IF lwa_data_request-consulta_netrin-grupo EQ 'ZPRF' OR
                 lwa_data_request-consulta_netrin-grupo EQ 'ZPRJ' .

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

      ENDLOOP.
    ENDIF.

    CLEAR gs_lfa1-cnae.

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
        is_bp_central_data = ls_bp_central_data " US #184108 - MMSILVA - 29.05.2025
        is_alteracao       = gs_alteracao " US #184108 - MMSILVA - 29.05.2025
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


  METHOD cria_cliente.

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
          lv_teste       TYPE c,
          lv_parceiro    TYPE bu_partner.

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

    cs_kna1-ktokd = lwa_data_request-consulta_netrin-grupo.

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
    lv_account = lwa_data_request-consulta_netrin-grupo.

**\ Get Consultas a realizar
    SELECT *
    FROM /netrin/group
    INTO TABLE lt_group
    WHERE account_group EQ lv_account.

    READ TABLE lt_group INTO ls_group INDEX 1.
    IF sy-subrc IS INITIAL.

***\ Exibe POPUP para preencher Dados para Consultas
*    CALL METHOD /netrin/compcl_utilities=>self_fill_popup
*      EXPORTING
*        i_parameter = lv_account
*      IMPORTING
*        e_result    = ls_self_fill.


      SPLIT lwa_data_request-consulta_netrin-data_nasc AT '/' INTO lv_dia lv_mes lv_ano.

      ls_self_fill-gbdat = lv_ano && lv_mes && lv_dia.
      ls_self_fill-stcd2 = lwa_data_request-consulta_netrin-cpf.
      ls_self_fill-stcd3 = lwa_data_request-consulta_netrin-ie.
      ls_self_fill-regio = lwa_data_request-consulta_netrin-regiao.
      ls_self_fill-stcd1 = lwa_data_request-consulta_netrin-cnpj.

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

**\ Preenhce work area para consultas conforme dados informados no POPUP
      MOVE-CORRESPONDING ls_self_fill TO ls_search.
*    ls_search-kunnr = iv_cliente.
      ls_search-ktokd = lv_account.

      IF lv_j EQ abap_true.
        READ TABLE lt_group INTO ls_group WITH KEY check_pj = lv_j.
      ELSE.
        READ TABLE lt_group INTO ls_group WITH KEY check_pf = lv_f.
      ENDIF.

      CHECK sy-subrc IS INITIAL.

**\ Sintegra
      IF ls_group-check_sintegra EQ abap_true AND
         ls_tcode-sint           EQ abap_true AND
         ls_search-regio         IS NOT INITIAL.

**/ Execute Call of Sintegra
        CALL METHOD /netrin/compcl_utilities=>execute_call_sintegra
          EXPORTING
            is_search       = ls_search
            iv_tcode        = sy-tcode
            iv_return_table = abap_true
          IMPORTING
            et_log          = lt_log_sint.

        DESCRIBE TABLE lt_log_sint  LINES lv_lin.

        IF lv_lin > 1.

*        /netrin/compcl_utilities=>popup_choice_ie( CHANGING ch_log = lt_log_sint ).
*
*        READ TABLE lt_log_sint INTO ls_log INDEX 1.

        ELSE.

          READ TABLE lt_log_sint INTO ls_log INDEX 1.

        ENDIF.

        IF ls_search-stcd3 IS NOT INITIAL AND
           ( ls_search-stcd3 <> ls_log-stcd3 ).

*        /netrin/compcl_utilities=>popup_choice_ie( EXPORTING iv_dif = abap_true
*                                                   CHANGING ch_log = lt_log_sint ).
*
*        READ TABLE lt_log_sint INTO ls_log INDEX 1.

        ENDIF.

        APPEND ls_log TO lt_log.

        lv_status = ls_log-status.

        IF ls_log-stcd1 IS NOT INITIAL.

          ls_search-stcd1 = ls_log-stcd1.

        ENDIF.

        IF ls_log-stcd2 IS NOT INITIAL.

          ls_search-stcd2 = ls_log-stcd2.

        ENDIF.

        IF ls_log-stcd3 IS NOT INITIAL.

          ls_search-stcd3 = ls_log-stcd3.

        ENDIF.

        IF ls_log-status IN lr_message_return.

          IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPF'.
            APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
            CONCATENATE 'Sintegra com Status' ls_log-status INTO <fs_erros> SEPARATED BY space.
            RETURN.
          ENDIF.
***\ Executa POPUP
*        CALL METHOD lo_utilities->execute_popup
*          EXPORTING
*            it_log = lt_log.
*
***\ Executa as ações cadastradas na tabela de parâmetro conforme o LOG
*        CALL METHOD /netrin/compcl_utilities=>execute_parameters_defined
*          EXPORTING
*            is_search = ls_search
*            is_log    = ls_log
*            iv_tcode  = sy-tcode.
*
***\ Se mensagem de Warning o fluxo continua
*        CLEAR: lv_status.

        ENDIF.

        CLEAR: ls_log,
               lt_log_sint.

      ENDIF.

**\ RFB
      IF ls_group-check_rfb EQ abap_true AND
         ls_tcode-rfb       EQ abap_true AND
         lv_status          NOT IN lr_message_return.

        CALL METHOD lo_adcn->search_rfb
          EXPORTING
            i_stcd1  = ls_search-stcd1
            i_stcd2  = ls_search-stcd2
            i_gbdat  = ls_search-gbdat
            i_regio  = ls_search-regio
          IMPORTING
            e_log    = ls_log
            e_status = lv_status.

        IF ls_log-status NE 'ATIVA' AND
           ls_log-status NE lc_r.

          IF ls_search-stcd1 IS NOT INITIAL.
            APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
            CONCATENATE 'CNPJ com status:' ls_log-status INTO <fs_erros> SEPARATED BY space.
            RETURN.
          ENDIF.

*        ls_log-status = lv_status.
*        APPEND ls_log TO lt_log.
*
*        lv_popup = abap_true.
*
****\ Executa POPUP
**        CALL METHOD lo_utilities->execute_popup
**          EXPORTING
**            it_log = lt_log.
*
***\ Executa as ações cadastradas na tabela de parâmetro conforme o LOG
*        CALL METHOD /netrin/compcl_utilities=>execute_parameters_defined
*          EXPORTING
*            is_search = ls_search
*            is_log    = ls_log
*            iv_tcode  = sy-tcode.

        ELSE.

          ls_log-status = lv_status.
          APPEND ls_log TO lt_log.

        ENDIF.

        CLEAR: ls_log,
               lv_status.

      ENDIF.

**\ Simples
      IF ls_group-check_simples EQ abap_true AND
         ls_tcode-simp          EQ abap_true.

        CALL METHOD lo_adcn->search_simples
          EXPORTING
            i_stcd1  = ls_search-stcd1
            i_regio  = ls_search-regio
            i_kunnr  = ls_search-kunnr
            i_lifnr  = ls_search-lifnr
          IMPORTING
            e_log    = ls_log
            e_status = lv_status.

        ls_log-status = lv_status.
        APPEND ls_log TO lt_log.

        IF NOT ls_log-status CS 'NAO'.
          cs_kna1-indtyp = 'Z2'.
        ENDIF.

        CLEAR: ls_log,
               lv_status,
               lv_popup.

      ENDIF.

**\ Suframa
      IF ls_group-check_suframa EQ abap_true AND
         ls_tcode-suframa       EQ abap_true.

        /netrin/compcl_utilities=>execute_call_suframa(
          EXPORTING
            is_search       = ls_search
            iv_tcode        = sy-tcode
            iv_return_table = abap_true
          IMPORTING
            et_log          = lt_log_sufr ).

        DESCRIBE TABLE lt_log_sufr LINES lv_lin.

        IF lv_lin > 1.

*        /netrin/compcl_utilities=>popup_choice_ie( CHANGING ch_log = lt_log_sufr ).
*
*        READ TABLE lt_log_sufr INTO ls_log INDEX 1.

        ELSE.

          READ TABLE lt_log_sufr INTO ls_log INDEX 1.

        ENDIF.

        cs_kna1-suframa = ls_log-suframa.

        APPEND ls_log TO lt_log.

        CLEAR: lt_log_sufr,
               ls_log,
               lv_popup.

      ENDIF.

**\ Preenche os campos com os valores da consulta
      /netrin/compcl_utilities=>fill_fields_register(
        EXPORTING
          it_log = lt_log
        IMPORTING
          es_addr1 = cs_addr1
          es_kna1  = cs_kna1 ).

    ENDIF.

    IF lwa_Data_request-consulta_netrin-grupo EQ 'ZCEX'.
      cs_kna1-name1 = lwa_data_request-consulta_netrin-nome(35).
      cs_kna1-sortl = lwa_data_request-consulta_netrin-nome(10).
      cs_kna1-stcd1 = lwa_data_request-consulta_netrin-cnpj.
      cs_kna1-stcd2 = lwa_data_request-consulta_netrin-cpf.
    ENDIF.

*** Inicio - Rubenilson - 06.12.24 #159528
    IF lwa_data_request-consulta_netrin-grupo EQ 'ZFEX' OR
           lwa_data_request-consulta_netrin-grupo EQ 'ZPRF' OR
           lwa_data_request-consulta_netrin-grupo EQ 'ZCPF'.

      IF cs_addr1-name1 IS NOT INITIAL.
        cs_addr1-name2 = cs_addr1-name1.
        clear cs_addr1-name1.
      ELSE.
        cs_addr1-name2 = lwa_data_request-consulta_netrin-nome(35).
      ENDIF.

    ENDIF.
*** Fim - Rubenilson - 06.12.24 #159528

    IF cs_addr1-city1 IS INITIAL.

      cs_addr1-city1      = lwa_data_request-dados_gerais-cidade.
      cs_addr1-city2      = lwa_data_request-dados_gerais-bairro.
      cs_addr1-region     = lwa_data_request-dados_gerais-estado.
      cs_addr1-post_code1 = lwa_data_request-dados_gerais-cep.
      cs_addr1-street     = lwa_data_request-dados_gerais-rua.
      cs_addr1-house_num1 = lwa_data_request-dados_gerais-numero.
      cs_addr1-country    = lwa_data_request-dados_gerais-pais.

    ENDIF.

*** Local de nascimento
    gs_sza1_d0100-fax_number = lwa_data_request-dados_gerais-fax.
    gs_sza1_d0100-smtp_addr = lwa_data_request-dados_gerais-email.

*** Dados pessoais
    ls_personal_data-birthdate     = ls_self_fill-gbdat.

    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO ls_personal_data-birthplace SEPARATED BY space.

    ls_personal_data-maritalstatus = lwa_data_request-dados_gerais-estado_civil.
    ls_personal_data2-proprty_st   = lwa_data_request-dados_gerais-regime_bens.

*** Local de nascimento
    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO cs_kna1-stcd4 SEPARATED BY space.

    MOVE-CORRESPONDING cs_addr1 TO gs_addr1_Data.
    MOVE-CORRESPONDING cs_kna1 TO gs_kna1.

    CREATE OBJECT lo_bp.

    SELECT *
      FROM tbc001
      INTO @DATA(ls_tbc001)
      UP TO 1 ROWS
      WHERE ktokk = @lwa_data_request-consulta_netrin-grupo.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      gs_bus_joel_main-creation_group = ls_tbc001-bu_group.
      gs_bus_joel_main-partner_role = 'FLVN00'.
    ELSE.

      SELECT *
        FROM tbd001
        INTO @DATA(ls_tbd001)
        UP TO 1 ROWS
        WHERE ktokd = @lwa_data_request-consulta_netrin-grupo.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        gs_bus_joel_main-creation_group = ls_tbd001-bu_group.
        gs_bus_joel_main-partner_role = 'FLCU00'.
      ENDIF.

    ENDIF.

    IF lwa_data_request-empresas IS NOT INITIAL.

      lr_bukrs = VALUE #( FOR ls_bukrs IN lwa_data_request-empresas
                          LET s = 'I'
                              o = 'EQ'
                           IN sign   = s
                              option = o
                           (  low = ls_bukrs-empresa ) ).


      SELECT * FROM zsdt0317
                  INTO TABLE @DATA(it_zsdt0317)
                  WHERE cancelado <> 'X'
                   AND ktokd = @lwa_data_request-consulta_netrin-grupo
                   AND bukrs IN @lr_bukrs
                   AND cancelado EQ @abap_false.
      IF sy-subrc IS INITIAL." Rubenilson - 06.12.24 #159528
        SELECT * FROM zsdt0319
          INTO TABLE @DATA(it_zsdt0319)
          FOR ALL ENTRIES IN @it_zsdt0317
        WHERE cancelado <> 'X'
          AND id = @it_zsdt0317-id .
        IF sy-subrc IS INITIAL." Rubenilson - 06.12.24 #159528
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
        ENDIF.

      ENDIF.

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
            IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPF' OR
               lwa_data_request-consulta_netrin-grupo EQ 'ZCPJ'.
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

    CLEAR gs_kna1-cnae.

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
        em_partner        = lv_parceiro ).      " Nº parceiro de negócios

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

      lwa_data_response-msg_sucesso = 'Parceiro criado com sucesso!'.
      lwa_data_response-parceiro = lv_parceiro.
    ENDIF.

  ENDMETHOD.


  METHOD cria_fornecedor.

**/ Constants
    CONSTANTS: lc_j    TYPE char1             VALUE 'J',
               lc_f    TYPE char1             VALUE 'F',
               lc_r    TYPE char7             VALUE 'REGULAR',
               lc_antt TYPE /netrin/compde_parameter VALUE 'ANTT_ERROR'.

**/ Internal tables
    DATA: lt_group          TYPE TABLE OF /netrin/group,
          lt_log_sint       TYPE /netrin/comptt_log,
          lt_log            TYPE TABLE OF /netrin/log,
          lt_log_sufr       TYPE /netrin/comptt_log,
          lt_log_ibam       TYPE /netrin/comptt_log,
          lt_log_antt       TYPE /netrin/comptt_log,
          ls_personal_data2 TYPE fsbp_ei_struc_person,
          lv_parceiro       TYPE bu_partner.

    DATA lr_bukrs TYPE RANGE OF bukrs.

**/ Work areas
    DATA: ls_tcode           TYPE /netrin/tcodes,
          ls_self_fill       TYPE /netrin/compst_self_fill,
          ls_search          TYPE /netrin/compst_search,
          ls_group           LIKE LINE OF lt_group,
          ls_log             TYPE /netrin/compst_log,
          ls_personal_data   TYPE bus_ei_struc_central_person,
          ls_bp_central_data TYPE bus_ei_struc_central. "US #184108 - MMSILVA - 29.05.2025

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
          lv_ano           TYPE char4.

    DATA: ld_process TYPE REF TO data.
    FIELD-SYMBOLS: <fs_process> TYPE any.

    CREATE DATA ld_process TYPE /netrin/compst_search.

    lfa1-ktokk = lwa_data_request-consulta_netrin-grupo.

    CALL METHOD /netrin/compcl_utilities=>get_tcode
      EXPORTING
        i_tcode = 'BP'
      RECEIVING
        r_tcode = ls_tcode.

**/ Seleciona consultas de acordo com seu Grupo de Contas
    SELECT *
      FROM /netrin/group
      INTO TABLE lt_group
     WHERE account_group EQ account.

    READ TABLE lt_group INTO ls_group INDEX 1.

**/ Verificar se encontrou dados para efetuar as consultas
    IF sy-subrc IS INITIAL.

**\ Mensagens de Cadastro Irregular
      /netrin/compcl_utilities=>get_multiple_value_parameter(
        EXPORTING
          i_parameter = 'MESSAGE_RETURN'
        CHANGING
          c_range     = lr_message_return ).

**\ Mensagens de Cadastro Irregular
      /netrin/compcl_utilities=>get_multiple_value_parameter(
        EXPORTING
          i_parameter = 'CNFPRGO_ESTADO'
        CHANGING
          c_range     = lr_cnfprgo ).


**/ Create Objects
      CREATE OBJECT lo_adcn.
      CREATE OBJECT lo_utilities.

**\ Get Account Group
      lv_account = account.

***\ Exibe POPUP para preencher Dados para Consultas
*  CALL METHOD /netrin/compcl_utilities=>self_fill_popup
*    EXPORTING
*      i_parameter = lv_account
*    IMPORTING
*      e_result    = ls_self_fill.

      ls_self_fill-stcd1 = lwa_data_request-consulta_netrin-cnpj.
      ls_self_fill-stcd2 = lwa_data_request-consulta_netrin-cPF.
      ls_self_fill-stcd3 = lwa_data_request-consulta_netrin-ie.
      ls_self_fill-regio = lwa_data_request-consulta_netrin-regiao.

*     US #184108 - MMSILVA - 29.05.2025 - Inicio
      lfa1-j_1kfrepre = 'SOFTEXPERT'.

      IF lwa_data_request-consulta_netrin-grupo EQ 'ZFNF'.
        IF lwa_data_request-consulta_netrin-nis_pis IS NOT INITIAL.
          lfa1-kraus = lwa_data_request-consulta_netrin-nis_pis.
          lfa1-stenr = lwa_data_request-consulta_netrin-nis_pis.
        ENDIF.

        IF lwa_data_request-consulta_netrin-data_nasc IS NOT INITIAL.
          lfa1-gbdat = lwa_data_request-consulta_netrin-data_nasc.
        ENDIF.

      ENDIF.
*     US #184108 - MMSILVA - 29.05.2025 - Fim

      SPLIT lwa_data_request-consulta_netrin-data_nasc AT '/' INTO lv_dia lv_mes lv_ano.
      ls_self_fill-gbdat = lv_ano && lv_mes && lv_dia.

      ls_self_fill-name = lwa_data_request-consulta_netrin-nome.

**\ Apenas Grpos de Contas mapeados
      CHECK ls_self_fill IS NOT INITIAL.

**/ Preenche os campos Standard que o usuário preencheu no pop-up
      IF ls_self_fill-stcd1 IS NOT INITIAL.
        lfa1-stcd1 = ls_self_fill-stcd1.
      ENDIF.

      IF ls_self_fill-stcd2 IS NOT INITIAL.
        lfa1-stcd2 = ls_self_fill-stcd2.
      ENDIF.

      IF ls_self_fill-stcd3 IS NOT INITIAL.
        lfa1-stcd3 = ls_self_fill-stcd3.
      ENDIF.

      IF ls_self_fill-nis IS NOT INITIAL.
        lfa1-kraus = ls_self_fill-nis.
*==> Begin of TSR - Meta - 19.11.2019 - DEVK9A0E40
        lfa1-stenr = ls_self_fill-nis.
*==> End of TSR - Meta - 19.11.2019 - DEVK9A0E40
      ENDIF.

      IF ls_self_fill-rntrc IS NOT INITIAL.
        lfa1-bahns = ls_self_fill-rntrc.
      ENDIF.

      IF ls_self_fill-regio IS NOT INITIAL.
        addr1-region = ls_self_fill-regio.
      ENDIF.

      IF ls_self_fill-gbdat IS NOT INITIAL.
        lfa1-gbdat = ls_self_fill-gbdat.
      ENDIF.

      IF ls_self_fill-name IS NOT INITIAL.

        IF lwa_data_request-consulta_netrin-grupo EQ 'ZFEX' OR
*** Inicio - Rubenilson - 06.12.24 #159528
           lwa_data_request-consulta_netrin-grupo EQ 'ZPRF' OR
           lwa_data_request-consulta_netrin-grupo EQ 'ZCPF' OR
           lwa_data_request-consulta_netrin-grupo EQ 'ZPRJ' OR
           lwa_data_request-consulta_netrin-grupo EQ 'ZCPJ'.
*** Fim - Rubenilson - 06.12.24 #159528

          addr1-name2 = ls_self_fill-name(40).
          addr1-sort1 = ls_self_fill-name(40).

        ELSE.

          IF strlen( ls_self_fill-name ) GT 35.

            addr1-name1 = ls_self_fill-name(35).
            addr1-name2 = ls_self_fill-name+35.

          ELSE.

            addr1-name1 = ls_self_fill-name.
            addr1-sort1 = ls_self_fill-name.

          ENDIF.

        ENDIF.

      ENDIF.

**/ Se tiver CNPJ é PJ, se não PF TEM QUE AJUSTAR ESSA REGRA NOT FOUND
      IF ls_self_fill-stcd1 IS NOT INITIAL OR
         ls_group-check_pj EQ abap_true.

        lv_person_type = lc_j.
        lv_j = abap_true.
      ELSE.

        lv_person_type = lc_f.
        lv_f = abap_true.
      ENDIF.

**\ Mensagens de erro ANTT
      /netrin/compcl_utilities=>get_single_value_parameter(
        EXPORTING
          i_parameter = lc_antt
        RECEIVING
          r_value     = lv_antt_error ).

**\ Preenhce work area para consultas conforme dados informados no POPUP
      MOVE-CORRESPONDING ls_self_fill TO ls_search.
*    ls_search-lifnr = iv_fornecedor.
      ls_search-ktokk = account.
      ls_search-placa = lfa1-lifnr.

      IF lv_j EQ abap_true.
        READ TABLE lt_group INTO ls_group WITH KEY check_pj = lv_j.
      ELSE.
        READ TABLE lt_group INTO ls_group WITH KEY check_pf = lv_f.
      ENDIF.

      CHECK sy-subrc IS INITIAL.

      IF ls_group-check_sintegra EQ abap_true AND
         ls_tcode-sint           EQ abap_true AND
         ls_search-regio         IS NOT INITIAL.

**/ Execute Call of Sintegra
        CALL METHOD /netrin/compcl_utilities=>execute_call_sintegra
          EXPORTING
            is_search       = ls_search
            iv_tcode        = sy-tcode
            iv_return_table = abap_true
          IMPORTING
            et_log          = lt_log_sint.

        DESCRIBE TABLE lt_log_sint  LINES lv_lin.

        IF lv_lin > 1.

          /netrin/compcl_utilities=>popup_choice_ie( CHANGING ch_log = lt_log_sint ).

          READ TABLE lt_log_sint INTO ls_log INDEX 1.

        ELSE.

          READ TABLE lt_log_sint INTO ls_log INDEX 1.

        ENDIF.

        IF ls_search-stcd3 IS NOT INITIAL AND
           ( ls_search-stcd3 <> ls_log-stcd3 ).

          /netrin/compcl_utilities=>popup_choice_ie( EXPORTING iv_dif = abap_true
                                                     CHANGING ch_log = lt_log_sint ).

          READ TABLE lt_log_sint INTO ls_log INDEX 1.

        ENDIF.

        IF ls_log IS NOT INITIAL.
          APPEND ls_log TO lt_log.
        ENDIF.

*        lv_status = ls_log-status.

        IF ls_log-stcd1 IS NOT INITIAL.

          ls_search-stcd1 = ls_log-stcd1.

        ENDIF.

        IF ls_log-stcd2 IS NOT INITIAL.

          ls_search-stcd2 = ls_log-stcd2.

        ENDIF.

        IF ls_log-stcd3 IS NOT INITIAL.

          ls_search-stcd3 = ls_log-stcd3.

        ENDIF.


        IF ls_log-status IN lr_message_return AND lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.

          APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
          CONCATENATE 'Sintegra com Status' ls_log-status INTO <fs_erros> SEPARATED BY space.

          RETURN.

***\ Executa POPUP
*        CALL METHOD lo_utilities->execute_popup
*          EXPORTING
*            it_log = lt_log.
*
***\ Executa as ações cadastradas na tabela de parâmetro conforme o LOG
*        CALL METHOD /netrin/compcl_utilities=>execute_parameters_defined
*          EXPORTING
*            is_search = ls_search
*            is_log    = ls_log
*            iv_tcode  = sy-tcode.
*
***\ Se mensagem de Warning o fluxo continua
*        CLEAR: lv_status.

        ENDIF.

        CLEAR: ls_log,
               lt_log_sint.

      ENDIF.

**\ RFB
      IF ls_group-check_rfb EQ abap_true AND
         ls_tcode-rfb       EQ abap_true AND
         lv_status          NOT IN lr_message_return.

        CALL METHOD lo_adcn->search_rfb
          EXPORTING
            i_stcd1  = ls_search-stcd1
            i_stcd2  = ls_search-stcd2
            i_gbdat  = ls_search-gbdat
            i_regio  = ls_search-regio
          IMPORTING
            e_log    = ls_log
            e_status = lv_status.

        IF ls_log-status NE 'ATIVA' AND
           ls_log-status NE lc_r.

          IF ls_search-stcd1 IS NOT INITIAL.
            APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
            CONCATENATE 'CNPJ com status:' ls_log-status INTO <fs_erros> SEPARATED BY space.
            RETURN.
          ENDIF.

*        ls_log-stcd3  = ls_search-stcd3.
*
*        IF ls_log IS NOT INITIAL.
*          APPEND ls_log TO lt_log.
*        ENDIF.
*
*        lv_popup = abap_true.
*
***\ Executa POPUP
*        CALL METHOD lo_utilities->execute_popup
*          EXPORTING
*            it_log = lt_log.
*
***\ Executa as ações cadastradas na tabela de parâmetro conforme o LOG
*        CALL METHOD /netrin/compcl_utilities=>execute_parameters_defined
*          EXPORTING
*            is_search = ls_search
*            is_log    = ls_log
*            iv_tcode  = sy-tcode.

        ELSE.

          ls_log-stcd3  = ls_search-stcd3.

          IF ls_log IS NOT INITIAL.
            APPEND ls_log TO lt_log.
          ENDIF.

        ENDIF.

        CLEAR: ls_log,
               lv_status.

      ENDIF.

**/ ANTT - Veículo
      IF ls_group-check_antt_v EQ abap_true AND
         ls_tcode-antt   EQ abap_true.

        ASSIGN ld_process->* TO <fs_process>.
        <fs_process> = ls_search.

        CALL METHOD lo_adcn->search_antt_v
          EXPORTING
            i_process      = ld_process
            i_process_serv = ld_process
          IMPORTING
            e_log          = lt_log_antt
            e_status       = lv_status.

        READ TABLE lt_log_antt INTO ls_log INDEX 1.

*      IF ls_log-status CS 'O VEÍCULO NÃO ESTÁ'.
*
*        MESSAGE 'O VEÍCULO NÃO ESTÁ CADASTRADO NA FROTA' TYPE lv_antt_error.
*
*      ENDIF.


        IF ls_log IS NOT INITIAL.
          APPEND ls_log TO lt_log.
        ENDIF.

        lv_status = ls_log-status.

        CLEAR: ls_log,
               lv_status,
               lv_popup.

        REFRESH: lt_log_antt.

      ENDIF.

**/ ANTT - Transportador
      IF ls_group-check_antt_t EQ abap_true AND
         ls_tcode-antt         EQ abap_true.

        ASSIGN ld_process->* TO <fs_process>.
        <fs_process> = ls_search.

        CALL METHOD lo_adcn->search_antt_t
          EXPORTING
            i_process      = ld_process
            i_process_serv = ld_process
          IMPORTING
            e_log          = lt_log_antt
            e_status       = lv_status.

        READ TABLE lt_log_antt INTO ls_log INDEX 1.


        IF ls_log IS NOT INITIAL.

          IF lwa_data_request-consulta_netrin-grupo = 'ZFFJ' OR
             lwa_data_request-consulta_netrin-grupo = 'ZFFF'.

            IF ls_log-status NS 'ESSE TRANSPORTADOR ESTÁ APTO'.
              APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
              <fs_erros> = ls_log-status.
              RETURN.
            ENDIF.

          ENDIF.

          APPEND ls_log TO lt_log.
        ENDIF.

        lv_status = ls_log-status.

        CLEAR: ls_log,
               lv_status,
               lv_popup.

        REFRESH: lt_log_antt.

      ENDIF.

**\ Simples
      IF ls_group-check_simples EQ abap_true AND
         ls_tcode-simp EQ abap_true.

        CALL METHOD lo_adcn->search_simples
          EXPORTING
            i_stcd1 = ls_search-stcd1
            i_regio = ls_search-regio
            i_kunnr = ls_search-kunnr
            i_lifnr = ls_search-lifnr
          IMPORTING
            e_log   = ls_log.

        IF ls_log IS NOT INITIAL.
          APPEND ls_log TO lt_log.
        ENDIF.

        "Simples = OPTANTE
        IF NOT ls_log-status  CS 'NAO' AND
           NOT ls_log-status  CS 'NÃO' AND
               lfa1-indtyp IS INITIAL.

          lfa1-indtyp = 'Z2'.
          lfa1-crtn   = 1.

          "Simples = NAO OPTANTE
        ELSEIF ( ls_log-status  CS 'NAO' OR
                 ls_log-status  CS 'NÃO' ) AND
               lfa1-indtyp IS INITIAL.

          lfa1-indtyp = 'Z1'.
          lfa1-crtn   = 3.

        ENDIF.

        CLEAR: ls_log,
               lv_status,
               lv_popup.

      ENDIF.

**\ Suframa
      IF ls_group-check_suframa EQ abap_true AND
         ls_tcode-suframa       EQ abap_true.

        /netrin/compcl_utilities=>execute_call_suframa(
          EXPORTING
            is_search       = ls_search
            iv_tcode        = sy-tcode
            iv_return_table = abap_true
          IMPORTING
            et_log          = lt_log_sufr ).

        READ TABLE lt_log_sufr INTO ls_log INDEX 1.

        IF ls_log IS NOT INITIAL.
          APPEND ls_log TO lt_log.
        ENDIF.

        CLEAR: lt_log_sufr,
               ls_log,
               lv_popup.

      ENDIF.

**/ E-Social
      IF ls_group-check_esocial EQ abap_true AND
         ls_tcode-esocial       EQ abap_true.

        ASSIGN ld_process->* TO <fs_process>.
        <fs_process> = ls_search.

        CALL METHOD lo_adcn->search_dataprev
          EXPORTING
            i_process      = ld_process
            i_process_serv = ld_process
          IMPORTING
            e_log          = DATA(lt_log_esocial).

        READ TABLE lt_log_esocial INTO ls_log INDEX 1.

        IF ls_log IS NOT INITIAL.
          APPEND ls_log TO lt_log.
        ENDIF.

        lv_status = ls_log-status.

        CLEAR: ls_log,
               lv_status,
               lv_popup.

      ENDIF.

      IF ls_group-check_ibama EQ abap_true AND
         ls_tcode-ibama       EQ abap_true.

        CALL METHOD lo_adcn->search_ibama
          EXPORTING
            i_stcd1  = ls_search-stcd1
            i_stcd2  = ls_search-stcd2
            i_lifnr  = ls_search-lifnr
          IMPORTING
            e_log    = lt_log_ibam
            e_status = lv_status.

        READ TABLE lt_log_ibam INTO ls_log INDEX 1.

        IF ls_log IS NOT INITIAL.
          APPEND ls_log TO lt_log.
        ENDIF.

        lv_status = ls_log-status.

        CLEAR: ls_log,
               lv_status,
               lv_popup.

      ENDIF.

**/ CNDE
      IF ls_group-check_cnde EQ abap_true AND
         ls_tcode-cnde       EQ abap_true.

        SELECT SINGLE *
          FROM /netrin/cnde
          INTO @DATA(ls_cnde)
          WHERE estado EQ @ls_search-regio.

        IF sy-subrc IS INITIAL AND
           ls_cnde-ativo = abap_true.

          ASSIGN ld_process->* TO <fs_process>.
          <fs_process> = ls_search.

          CALL METHOD lo_adcn->search_cnd_e
            EXPORTING
              i_process      = ld_process
              i_process_serv = ld_process
            IMPORTING
              e_log          = DATA(lt_log_cnde).

          READ TABLE lt_log_cnde INTO ls_log INDEX 1.

          IF ls_log IS NOT INITIAL.
            APPEND ls_log TO lt_log.
          ENDIF.

          lv_status = ls_log-status.

          CLEAR: ls_log,
                 lv_status,
                 lv_popup.
        ENDIF.
      ENDIF.

**/ CNDE
      IF ls_group-check_cnfprgo EQ abap_true AND
         ls_tcode-cnfprgo       EQ abap_true AND
         lr_cnfprgo             IS NOT INITIAL AND
         ls_search-regio        IN lr_cnfprgo.

        ASSIGN ld_process->* TO <fs_process>.
        <fs_process> = ls_search.

        CALL METHOD lo_adcn->search_cnfprgo
          EXPORTING
            i_process      = ld_process
            i_process_serv = ld_process
          IMPORTING
            e_log          = DATA(lt_log_cnfprgo).

        READ TABLE lt_log_cnfprgo INTO ls_log INDEX 1.

        IF ls_log IS NOT INITIAL.
          APPEND ls_log TO lt_log.
        ENDIF.

        lv_status = ls_log-status.

        CLEAR: ls_log,
               lv_status,
               lv_popup.

      ENDIF.


**\ Preenche os campos com os valores da consulta
      /netrin/compcl_utilities=>fill_fields_register(
        EXPORTING
          it_log = lt_log
        IMPORTING
          es_addr1 = addr1
          es_lfa1  = lfa1 ).

    ENDIF.

*** Inicio - Rubenilson - 06.12.24 #159528
    IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPF' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZPRF' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZCPJ' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZPFJ'.

      addr1-name2 = addr1-name1.
      CLEAR addr1-name1.

    ENDIF.
*** Fim - Rubenilson - 06.12.24 #159528

    IF lwa_data_request-dados_gerais-emissor_nota_fiscal EQ abap_true.
      lfa1-scacd = '9999'.
    ELSE.
      lfa1-scacd = '8888'.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZFFJ' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZFFF'.

      lfa1-dlgrp = '0001'.

    ENDIF.

    IF lwa_Data_request-consulta_netrin-grupo EQ 'ZFEX'.
      lfa1-name1 = lwa_data_request-consulta_netrin-nome(35).
      lfa1-sortl = lwa_data_request-consulta_netrin-nome(10).
      lfa1-stcd1 = lwa_data_request-consulta_netrin-cnpj.
      lfa1-stcd2 = lwa_data_request-consulta_netrin-cpf.
    ENDIF.

*** Endereço
    IF addr1-city1 IS INITIAL.

      addr1-city1      = lwa_data_request-dados_gerais-cidade.
      addr1-city2      = lwa_data_request-dados_gerais-bairro.
      addr1-region     = lwa_data_request-dados_gerais-estado.
      addr1-post_code1 = lwa_data_request-dados_gerais-cep.
      addr1-street     = lwa_data_request-dados_gerais-rua.
      addr1-house_num1 = lwa_data_request-dados_gerais-numero.
      addr1-country    = lwa_data_request-dados_gerais-pais.

    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZIMP' AND
       addr1-region IS INITIAL.
      addr1-region = lwa_data_request-consulta_netrin-regiao.
    ENDIF.

*** Dados pessoais
    IF lfa1-gbort IS INITIAL.
      CONCATENATE lwa_data_request-dados_gerais-rg  lwa_data_request-dados_gerais-orgao_exp INTO lfa1-gbort SEPARATED BY space.
    ENDIF.

    ls_personal_data-maritalstatus = lwa_data_request-dados_gerais-estado_civil.
    ls_personal_data2-proprty_st = lwa_Data_request-dados_gerais-regime_bens.

*** Fax e Email
    gs_sza1_d0100-fax_number = lwa_data_request-dados_gerais-fax.
    gs_sza1_d0100-smtp_addr = lwa_data_request-dados_gerais-email.

*** Local de nascimento
    CONCATENATE lwa_data_request-dados_gerais-rg lwa_data_request-dados_gerais-orgao_exp INTO lfa1-stcd4 SEPARATED BY space.

    DATA(gs_lfa1)       = lfa1.

    gs_addr1_data = addr1.

*** Função parceiro e grupo
    SELECT *
      FROM tbc001
      INTO @DATA(ls_tbc001)
      UP TO 1 ROWS
      WHERE ktokk = @lwa_data_request-consulta_netrin-grupo.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      gs_bus_joel_main-creation_group = ls_tbc001-bu_group.
      gs_bus_joel_main-partner_role = 'FLVN00'.
    ELSE.

      SELECT *
        FROM tbd001
        INTO @DATA(ls_tbd001)
        UP TO 1 ROWS
        WHERE ktokd = @lwa_data_request-consulta_netrin-grupo.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        gs_bus_joel_main-creation_group = ls_tbd001-bu_group.
        gs_bus_joel_main-partner_role = 'FLCU00'.
      ENDIF.

    ENDIF.

    CREATE OBJECT lo_bp.

*** Validação dados bancários
    DATA(lt_Dados_banco) = lwa_data_request-dados_gerais-dados_bancarios.
    DELETE lt_Dados_banco WHERE chave_banco IS INITIAL.

    IF lt_Dados_banco IS NOT INITIAL.

      CALL METHOD me->valida_dados_banco(
        EXPORTING
          lwa_data_request  = lwa_data_request
          iv_partner        = gs_lfa1-lifnr
        IMPORTING
          lwa_data_response = lwa_data_response ).

      IF lwa_data_response-erros IS NOT INITIAL.
        RETURN.
      ELSE.
        LOOP AT lwa_data_request-dados_gerais-dados_bancarios ASSIGNING FIELD-SYMBOL(<fs_banco>).
          APPEND INITIAL LINE TO gt_lfbk ASSIGNING FIELD-SYMBOL(<fs_lfbk>).
          <fs_lfbk>-bankl = <fs_banco>-chave_banco.
          <fs_lfbk>-bankn = <fs_banco>-conta.
          <fs_lfbk>-banks = <fs_banco>-regiao.
          <fs_lfbk>-bkont = <fs_banco>-digito_verif_agencia.
          <fs_lfbk>-bkref = <fs_banco>-indicacao_referencia.
        ENDLOOP.
      ENDIF.

    ENDIF.

*** Mapeamento expansão de empresas - Inicio
    IF lwa_data_request-empresas IS NOT INITIAL.

      lr_bukrs = VALUE #( FOR ls_bukrs IN lwa_data_request-empresas
                          LET s = 'I'
                              o = 'EQ'
                           IN sign   = s
                              option = o
                           (  low = ls_bukrs-empresa ) ).

      SELECT *
        FROM zsdt0317
        INTO TABLE @DATA(lt_0317)
        WHERE bukrs IN @lr_bukrs
          AND ktokd EQ @lwa_data_request-consulta_netrin-grupo
          AND cancelado EQ @abap_false.
      IF sy-subrc IS INITIAL.

        SELECT *
          FROM zsdt0341
          INTO TABLE @DATA(lt_zsdt0341)
          FOR ALL ENTRIES IN @lt_0317
          WHERE id = @lt_0317-id.
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

              IF lwa_data_request-consulta_netrin-grupo EQ 'ZPRF' OR
                 lwa_data_request-consulta_netrin-grupo EQ 'ZPRJ' .

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

      ENDLOOP.
    ENDIF.
*** Mapeamento aba compras - Fim

    CLEAR gs_lfa1-cnae.

    CALL METHOD lo_bp->mt_bp_create_supplier(
      EXPORTING
        im_test           = gv_test
        is_lfa1           = gs_lfa1         " Mestre de fornecedores (parte geral)
        it_lfbk           = gt_lfbk         " Interface externa: dados detalhes bancários
        it_lfb1           = gt_lfb1         " Mestre de fornecedores (empresa)
        it_lfbw           = gt_lfbw         " Mestre de fornecedores (empresa)
        is_rf02k          = gs_rf02k        " Atual.dados mestre fornecedor: campos de tela e operativos
        is_addr1_data     = gs_addr1_data   " Estrutura de transferência para endereços
        is_sza1_d0100     = gs_sza1_d0100   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        is_bus_joel_main  = gs_bus_joel_main "ALRS
        it_adsmtp         = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        it_lfm1           = gt_lfm1
        is_personal_data  = ls_personal_data
        is_personal_data2 = ls_personal_data2
        is_bp_central_data = ls_bp_central_data "US #184108 - MMSILVA - 29.05.2025
      CHANGING
        et_return         = gt_return       " Tabela de retorno
        em_partner        = lv_parceiro      " Nº parceiro de negócios
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

      lwa_data_response-msg_sucesso = 'Parceiro criado com sucesso!'.
      lwa_data_response-parceiro = lv_parceiro.
    ENDIF.

  ENDMETHOD.


  METHOD preenche_lfbw.

    LOOP AT lwa_data_request-empresas ASSIGNING FIELD-SYMBOL(<fs_empresas>).

      LOOP AT <fs_empresas>-impostos ASSIGNING FIELD-SYMBOL(<fs_impostos>).
        APPEND INITIAL LINE TO lfbw ASSIGNING FIELD-SYMBOL(<fs_lfbw>).
        <fs_lfbw>-bukrs     = <fs_empresas>-empresa.
        <fs_lfbw>-witht     = <fs_impostos>-categ_irf.
        <fs_lfbw>-wt_withcd = <fs_impostos>-cod_irf.
        <fs_lfbw>-wt_subjct = abap_true.
      ENDLOOP.

    ENDLOOP.

ENDMETHOD.


  METHOD valida_dados_banco.

    TYPES: BEGIN OF ty_data,
             key(2)    TYPE c,
             value(20) TYPE c,
           END OF ty_data.

    DATA: lo_webservice   TYPE REF TO /netrin/compif_webservice,
          lt_but0bk(21)   TYPE c VALUE '(SAPLBUD0)GT_BUT0BK[]',
          lt_bptax(32)    TYPE c VALUE '(SAPLBUPA_BUTX_DIALOG)GT_BPTAX[]',
          lv_pgto_alt(35) TYPE c VALUE '(SAPLCVI_FS_UI_VENDOR)GS_LFA1-LNRZA',
          ld_data         TYPE REF TO data,
          lt_data_table   TYPE TABLE OF ty_data,
          ls_data_table   TYPE ty_data,
          lv_key_name     TYPE char10,
          lv_key_value    TYPE char15,
          lv_conta        TYPE string,
          lv_digito_conta TYPE string,
          ls_log          TYPE /netrin/log,
          lt_log          TYPE TABLE OF /netrin/log,
          lv_guid         TYPE guid_16,
          lt_return       TYPE /netrin/comptt_return,
          lv_search_date  TYPE dats,
          et_return       TYPE TABLE OF bapiret2,
          lt_ident        TYPE TABLE OF dfkkbptaxnum.

    FIELD-SYMBOLS:
      <fs_return_bp> TYPE bapiret2,
      <fs_pgto_alt>  TYPE any.

    DATA: lt_dados_banco TYPE TABLE OF but0bk.

    IF lwa_data_request-consulta_netrin-cnpj IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_ident ASSIGNING FIELD-SYMBOL(<fs_ident>).
      <fs_ident>-taxnum = lwa_data_request-consulta_netrin-cnpj.
      <fs_ident>-taxtype = 'BR1'.
    ELSEIF  lwa_data_request-consulta_netrin-cpf IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_ident ASSIGNING <fs_ident>.
      <fs_ident>-taxnum = lwa_data_request-consulta_netrin-cpf.
      <fs_ident>-taxtype = 'BR1'.
    ELSE.
      EXIT.
    ENDIF.

    LOOP AT lwa_data_request-dados_gerais-dados_bancarios ASSIGNING FIELD-SYMBOL(<fs_dados_bancarios>).

      APPEND INITIAL LINE TO lt_dados_banco ASSIGNING FIELD-SYMBOL(<fs_dados_banco>).
      <fs_dados_banco>-bkvid = <fs_dados_bancarios>-id.
      <fs_dados_banco>-banks = <fs_dados_bancarios>-regiao.
      <fs_dados_banco>-bankl = <fs_dados_bancarios>-chave_banco.
      <fs_dados_banco>-bankn = <fs_dados_bancarios>-conta.
      <fs_dados_banco>-bkont = <fs_dados_bancarios>-digito_verif_agencia.
    ENDLOOP.


    IF lt_dados_banco IS NOT INITIAL.

      REFRESH lt_return.

      SELECT FROM but0bk
        FIELDS *
        WHERE partner EQ @iv_partner
        INTO TABLE @DATA(lt_bankdata).

      SELECT SINGLE FROM lfa1
        FIELDS *
        WHERE lifnr EQ @iv_partner
        INTO @DATA(ls_lfa1).

      SELECT SINGLE FROM kna1
        FIELDS *
        WHERE kunnr EQ @iv_partner
        INTO @DATA(ls_kna1).

      IF lt_ident IS INITIAL.

        SELECT SINGLE FROM dfkkbptaxnum
          FIELDS *
          WHERE partner EQ @iv_partner
          AND   taxtype IN ('BR1','BR2')
          INTO @DATA(ls_dfkkbptaxnum).
        IF sy-subrc = 0.

          APPEND INITIAL LINE TO lt_ident ASSIGNING FIELD-SYMBOL(<fs_ident_ap>).
          ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <fs_ident_ap> TO FIELD-SYMBOL(<partnr>).
          ASSIGN COMPONENT 'TAXNUM' OF STRUCTURE <fs_ident_ap> TO FIELD-SYMBOL(<taxnum>).
          ASSIGN COMPONENT 'TAXTYPE' OF STRUCTURE <fs_ident_ap> TO FIELD-SYMBOL(<taxtype>).

          <partnr>  = iv_partner.
          <taxtype> = ls_dfkkbptaxnum-taxtype.
          <taxnum>  = ls_dfkkbptaxnum-taxnum.

        ENDIF.

      ENDIF.

      lo_webservice = NEW /netrin/compcl_ws_vcb( ).
      CREATE DATA ld_data TYPE TABLE OF ty_data.
      FIELD-SYMBOLS: <data_table> TYPE STANDARD TABLE.

      "Parâmetro para saber se faz a busca na API ou no log atravez da qtde de dias informado.
      CALL METHOD /netrin/compcl_utilities=>get_single_value_parameter
        EXPORTING
          i_parameter = 'APINETRIN_SEARCH_DATE'
        RECEIVING
          r_value     = DATA(lv_qtde_days).

      IF lv_qtde_days IS NOT INITIAL.

        lv_search_date = ( sy-datum - lv_qtde_days ).

      ELSE.

        lv_search_date = sy-datum.

      ENDIF.

      "Parâmetro para saber se pesquisa todos os registros - Dados Bancários
      CALL METHOD /netrin/compcl_utilities=>get_single_value_parameter
        EXPORTING
          i_parameter = 'APINETRIN_ALL_RECORDS'
        RECEIVING
          r_value     = DATA(lv_all_records).

      READ TABLE lt_ident ASSIGNING FIELD-SYMBOL(<fs_ident_cnpj>) WITH KEY ('TAXTYPE') = 'BR1'.
      IF sy-subrc = 0.

        ASSIGN COMPONENT 'TAXNUM' OF STRUCTURE <fs_ident_cnpj> TO FIELD-SYMBOL(<taxnum_cnpj>).

        SELECT FROM /netrin/log
          FIELDS *
          WHERE service_header EQ 'VDB'
          AND stcd1            EQ @<taxnum_cnpj>
          AND partner          EQ @iv_partner
          AND search_date      GE @lv_search_date
          INTO TABLE @lt_log.

        APPEND VALUE #( key = 'CC' value = <taxnum_cnpj> ) TO lt_data_table.
        ls_data_table-key   = 'CC'.
        ls_data_table-value = <taxnum_cnpj>.

      ELSE.

        READ TABLE lt_ident ASSIGNING FIELD-SYMBOL(<fs_ident_cpf>) WITH KEY ('TAXTYPE') = 'BR2'.
        IF sy-subrc = 0.

          ASSIGN COMPONENT 'TAXNUM' OF STRUCTURE <fs_ident_cpf> TO FIELD-SYMBOL(<taxnum_cpf>).

          SELECT FROM /netrin/log
            FIELDS *
            WHERE service_header EQ 'VDB'
            AND stcd2            EQ @<taxnum_cpf>
            AND partner          EQ @iv_partner
            AND search_date      GE @lv_search_date
            INTO TABLE @lt_log.

          APPEND VALUE #( key = 'CC' value = <taxnum_cpf> ) TO lt_data_table.
          ls_data_table-key   = 'CC'.
          ls_data_table-value = <taxnum_cpf>.

        ELSE.

          APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING FIELD-SYMBOL(<fs_erros>).
          <fs_erros> = 'CNPJ/CPF Obrigatório para validação de dados bancários'.

        ENDIF.

      ENDIF.

      LOOP AT lt_dados_banco ASSIGNING FIELD-SYMBOL(<fs_bankdata_tela>).

        ASSIGN COMPONENT 'BKVID' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bkvid>).
        ASSIGN COMPONENT 'BANKL' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bankl>).
        ASSIGN COMPONENT 'BANKN' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bankn>).
        ASSIGN COMPONENT 'BKONT' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bkont>).
        ASSIGN COMPONENT 'BKREF' OF STRUCTURE <fs_bankdata_tela> TO FIELD-SYMBOL(<bkref>).

        TRANSLATE <bkref> TO UPPER CASE.

        READ TABLE lt_bankdata ASSIGNING FIELD-SYMBOL(<fs_bankdata>) WITH KEY partner = iv_partner
                                                                              bkvid   = <bkvid>.
        IF sy-subrc = 0.

          IF <fs_bankdata>-bankl EQ <bankl> AND
             <fs_bankdata>-bankn EQ <bankn> AND
             <fs_bankdata>-bkont EQ <bkont> AND
             <fs_bankdata>-bkref EQ <bkref>.

            IF lv_all_records EQ abap_true.

              READ TABLE lt_log ASSIGNING FIELD-SYMBOL(<fs_log1>) WITH KEY bkvid = <fs_bankdata>-bkvid.
              IF sy-subrc NE 0.

                APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
                APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.

                SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
                IF lv_digito_conta IS INITIAL.
                  REFRESH: lt_data_table.
                  APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.

                  CALL FUNCTION 'GUID_CREATE'
                    IMPORTING
                      ev_guid_16 = lv_guid.

                  ls_log-guid           = lv_guid.
                  ls_log-service_header = 'VDB'.
                  ls_log-service        = 'VDB'.
                  ls_log-search_date    = sy-datum.
                  ls_log-search_time    = sy-uzeit.

                  IF <taxnum_cnpj> IS ASSIGNED.
                    ls_log-stcd1 = <taxnum_cnpj>.
                  ELSEIF <taxnum_cpf> IS ASSIGNED.
                    ls_log-stcd2 = <taxnum_cpf>.
                  ENDIF.

                  ls_log-lifnr          = ls_lfa1-lifnr.
                  ls_log-kunnr          = ls_kna1-kunnr.
                  ls_log-tcode          = sy-tcode.
                  ls_log-username       = sy-uname.
                  ls_log-partner        = iv_partner.
                  ls_log-bkvid          = <bkvid>.
                  ls_log-banks          = 'BR'.
                  ls_log-bankl          = <bankl>.
                  ls_log-bankn          = <bankn>.
                  ls_log-bkont          = <bkont>.
                  ls_log-bkref          = <bkref>.
                  ls_log-koinh          = ''.
                  ls_log-validacaoconta = 'Não'.
                  ls_log-status         = 'Consulta não realizada'.
                  ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.

                  APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
                  <fs_erros> = ls_log-mensagem.
                  MODIFY /netrin/log FROM ls_log.
                  RETURN.

                ENDIF.

                APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
                APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.

                IF <bkref> IS NOT INITIAL.

                  IF <bkref> CS 'POUPAN'.

                    APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.

                  ENDIF.

                  IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.

                    APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.

                  ENDIF.

                ELSE.

                  APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.

                ENDIF.

              ELSE.

                IF <fs_log1>-search_date LT lv_search_date.

                  APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
                  APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.

                  SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
                  IF lv_digito_conta IS INITIAL.
                    REFRESH: lt_data_table.
                    APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.

                    CALL FUNCTION 'GUID_CREATE'
                      IMPORTING
                        ev_guid_16 = lv_guid.

                    ls_log-guid           = lv_guid.
                    ls_log-service_header = 'VDB'.
                    ls_log-service        = 'VDB'.
                    ls_log-search_date    = sy-datum.
                    ls_log-search_time    = sy-uzeit.

                    IF <taxnum_cnpj> IS ASSIGNED.
                      ls_log-stcd1 = <taxnum_cnpj>.
                    ELSEIF <taxnum_cpf> IS ASSIGNED.
                      ls_log-stcd2 = <taxnum_cpf>.
                    ENDIF.

                    ls_log-lifnr          = ls_lfa1-lifnr.
                    ls_log-kunnr          = ls_kna1-kunnr.
                    ls_log-tcode          = sy-tcode.
                    ls_log-username       = sy-uname.
                    ls_log-partner        = iv_partner.
                    ls_log-bkvid          = <bkvid>.
                    ls_log-banks          = 'BR'.
                    ls_log-bankl          = <bankl>.
                    ls_log-bankn          = <bankn>.
                    ls_log-bkont          = <bkont>.
                    ls_log-bkref          = <bkref>.
                    ls_log-koinh          = ''.
                    ls_log-validacaoconta = 'Não'.
                    ls_log-status         = 'Consulta não realizada'.
                    ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.

                    APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
                    <fs_erros> = ls_log-mensagem.
                    MODIFY /netrin/log FROM ls_log.
                    RETURN.

                  ENDIF.

                  APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
                  APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.

                  IF <bkref> IS NOT INITIAL.

                    IF <bkref> CS 'POUPAN'.

                      APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.

                    ENDIF.

                    IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.

                      APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.

                    ENDIF.

                  ELSE.

                    APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.

                  ENDIF.

                ELSEIF <fs_log1>-validacaoconta NE 'SIM'.

                  APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
                  APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.

                  SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
                  IF lv_digito_conta IS INITIAL.
                    REFRESH: lt_data_table.
                    APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.

                    CALL FUNCTION 'GUID_CREATE'
                      IMPORTING
                        ev_guid_16 = lv_guid.

                    ls_log-guid           = lv_guid.
                    ls_log-service_header = 'VDB'.
                    ls_log-service        = 'VDB'.
                    ls_log-search_date    = sy-datum.
                    ls_log-search_time    = sy-uzeit.

                    IF <taxnum_cnpj> IS ASSIGNED.
                      ls_log-stcd1 = <taxnum_cnpj>.
                    ELSEIF <taxnum_cpf> IS ASSIGNED.
                      ls_log-stcd2 = <taxnum_cpf>.
                    ENDIF.

                    ls_log-lifnr          = ls_lfa1-lifnr.
                    ls_log-kunnr          = ls_kna1-kunnr.
                    ls_log-partner        = iv_partner.
                    ls_log-tcode          = sy-tcode.
                    ls_log-username       = sy-uname.
                    ls_log-bkvid          = <bkvid>.
                    ls_log-banks          = 'BR'.
                    ls_log-bankl          = <bankl>.
                    ls_log-bankn          = <bankn>.
                    ls_log-bkont          = <bkont>.
                    ls_log-bkref          = <bkref>.
                    ls_log-koinh          = ''.
                    ls_log-validacaoconta = 'Não'.
                    ls_log-status         = 'Consulta não realizada'.
                    ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.

                    APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
                    <fs_erros> = ls_log-mensagem.
                    MODIFY /netrin/log FROM ls_log.
                    RETURN.

                  ENDIF.

                  APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
                  APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.

                  IF <bkref> IS NOT INITIAL.

                    IF <bkref> CS 'POUPAN'.

                      APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.

                    ENDIF.

                    IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.

                      APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.

                    ENDIF.

                  ELSE.

                    APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.

                  ENDIF.

                ELSE.

                  CALL FUNCTION 'GUID_CREATE'
                    IMPORTING
                      ev_guid_16 = lv_guid.

                  ls_log-guid           = lv_guid.
                  ls_log-service_header = 'VDB'.
                  ls_log-service        = 'VDB'.
                  ls_log-search_date    = sy-datum.
                  ls_log-search_time    = sy-uzeit.

                  IF <taxnum_cnpj> IS ASSIGNED.
                    ls_log-stcd1 = <taxnum_cnpj>.
                  ELSEIF <taxnum_cpf> IS ASSIGNED.
                    ls_log-stcd2 = <taxnum_cpf>.
                  ENDIF.

                  ls_log-lifnr          = ls_lfa1-lifnr.
                  ls_log-kunnr          = ls_kna1-kunnr.
                  ls_log-partner        = iv_partner.
                  ls_log-tcode          = sy-tcode.
                  ls_log-username       = sy-uname.
                  ls_log-bkvid          = <bkvid>.
                  ls_log-banks          = 'BR'.
                  ls_log-bankl          = <bankl>.
                  ls_log-bankn          = <bankn>.
                  ls_log-bkont          = <bkont>.
                  ls_log-bkref          = <bkref>.
                  ls_log-koinh          = ''.
                  ls_log-validacaoconta = <fs_log1>-validacaoconta.
                  ls_log-status         = <fs_log1>-status.
                  ls_log-mensagem       = <fs_log1>-mensagem.

                  APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
                  <fs_erros> = ls_log-mensagem.

                  MODIFY /netrin/log FROM ls_log.
                  RETURN.

                ENDIF.

              ENDIF.

            ELSE.

              CONTINUE.

            ENDIF.

          ELSE.

*              IF lv_all_records EQ abap_true.

            READ TABLE lt_log ASSIGNING FIELD-SYMBOL(<fs_log2>) WITH KEY bkvid = <fs_bankdata>-bkvid bankl = <bankl>.
            IF sy-subrc NE 0.

              APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
              APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.

              SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
              IF lv_digito_conta IS INITIAL.
                REFRESH: lt_data_table.
                APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.

                CALL FUNCTION 'GUID_CREATE'
                  IMPORTING
                    ev_guid_16 = lv_guid.

                ls_log-guid           = lv_guid.
                ls_log-service_header = 'VDB'.
                ls_log-service        = 'VDB'.
                ls_log-search_date    = sy-datum.
                ls_log-search_time    = sy-uzeit.

                IF <taxnum_cnpj> IS ASSIGNED.
                  ls_log-stcd1 = <taxnum_cnpj>.
                ELSEIF <taxnum_cpf> IS ASSIGNED.
                  ls_log-stcd2 = <taxnum_cpf>.
                ENDIF.

                ls_log-lifnr          = ls_lfa1-lifnr.
                ls_log-kunnr          = ls_kna1-kunnr.
                ls_log-partner        = iv_partner.
                ls_log-tcode          = sy-tcode.
                ls_log-username       = sy-uname.
                ls_log-bkvid          = <bkvid>.
                ls_log-banks          = 'BR'.
                ls_log-bankl          = <bankl>.
                ls_log-bankn          = <bankn>.
                ls_log-bkont          = <bkont>.
                ls_log-bkref          = <bkref>.
                ls_log-koinh          = ''.
                ls_log-validacaoconta = 'Não'.
                ls_log-status         = 'Consulta não realizada'.
                ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.

                APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
                <fs_erros> = ls_log-mensagem.

                MODIFY /netrin/log FROM ls_log.
                RETURN.

              ENDIF.

              APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
              APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.

              IF <bkref> IS NOT INITIAL.

                IF <bkref> CS 'POUPAN'.

                  APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.

                ENDIF.

                IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.

                  APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.

                ENDIF.

              ELSE.

                APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.

              ENDIF.

            ELSE.

              IF <fs_log2>-search_date LT lv_search_date.

                APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
                APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.

                SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
                IF lv_digito_conta IS INITIAL.
                  REFRESH: lt_data_table.
                  APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.

                  CALL FUNCTION 'GUID_CREATE'
                    IMPORTING
                      ev_guid_16 = lv_guid.

                  ls_log-guid           = lv_guid.
                  ls_log-service_header = 'VDB'.
                  ls_log-service        = 'VDB'.
                  ls_log-search_date    = sy-datum.
                  ls_log-search_time    = sy-uzeit.

                  IF <taxnum_cnpj> IS ASSIGNED.
                    ls_log-stcd1 = <taxnum_cnpj>.
                  ELSEIF <taxnum_cpf> IS ASSIGNED.
                    ls_log-stcd2 = <taxnum_cpf>.
                  ENDIF.

                  ls_log-lifnr          = ls_lfa1-lifnr.
                  ls_log-kunnr          = ls_kna1-kunnr.
                  ls_log-partner        = iv_partner.
                  ls_log-tcode          = sy-tcode.
                  ls_log-username       = sy-uname.
                  ls_log-bkvid          = <bkvid>.
                  ls_log-banks          = 'BR'.
                  ls_log-bankl          = <bankl>.
                  ls_log-bankn          = <bankn>.
                  ls_log-bkont          = <bkont>.
                  ls_log-bkref          = <bkref>.
                  ls_log-koinh          = ''.
                  ls_log-validacaoconta = 'Não'.
                  ls_log-status         = 'Consulta não realizada'.
                  ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.

                  APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
                  <fs_erros> = ls_log-mensagem.

                  MODIFY /netrin/log FROM ls_log.
                  RETURN.

                ENDIF.

                APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
                APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.

                IF <bkref> IS NOT INITIAL.

                  IF <bkref> CS 'POUPAN'.

                    APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.

                  ENDIF.

                  IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.

                    APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.

                  ENDIF.

                ELSE.

                  APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.

                ENDIF.

              ELSEIF <fs_log2>-validacaoconta NE 'SIM'.

                APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
                APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.

                SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
                IF lv_digito_conta IS INITIAL.
                  REFRESH: lt_data_table.
                  APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.

                  CALL FUNCTION 'GUID_CREATE'
                    IMPORTING
                      ev_guid_16 = lv_guid.

                  ls_log-guid           = lv_guid.
                  ls_log-service_header = 'VDB'.
                  ls_log-service        = 'VDB'.
                  ls_log-search_date    = sy-datum.
                  ls_log-search_time    = sy-uzeit.

                  IF <taxnum_cnpj> IS ASSIGNED.
                    ls_log-stcd1 = <taxnum_cnpj>.
                  ELSEIF <taxnum_cpf> IS ASSIGNED.
                    ls_log-stcd2 = <taxnum_cpf>.
                  ENDIF.

                  ls_log-lifnr          = ls_lfa1-lifnr.
                  ls_log-kunnr          = ls_kna1-kunnr.
                  ls_log-partner        = iv_partner.
                  ls_log-tcode          = sy-tcode.
                  ls_log-username       = sy-uname.
                  ls_log-bkvid          = <bkvid>.
                  ls_log-banks          = 'BR'.
                  ls_log-bankl          = <bankl>.
                  ls_log-bankn          = <bankn>.
                  ls_log-bkont          = <bkont>.
                  ls_log-bkref          = <bkref>.
                  ls_log-koinh          = ''.
                  ls_log-validacaoconta = 'Não'.
                  ls_log-status         = 'Consulta não realizada'.
                  ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.

                  APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
                  <fs_erros> = ls_log-mensagem.

                  MODIFY /netrin/log FROM ls_log.
                  RETURN.

                ENDIF.

                APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
                APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.

                IF <bkref> IS NOT INITIAL.

                  IF <bkref> CS 'POUPAN'.

                    APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.

                  ENDIF.

                  IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.

                    APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.

                  ENDIF.

                ELSE.

                  APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.

                ENDIF.

              ELSE.

                CONTINUE.

              ENDIF.

            ENDIF.

*              ELSE.
*
*                CONTINUE.
*
*              ENDIF.

          ENDIF.

        ELSE.

          APPEND VALUE #( key = 'CB' value = <bankl>(3) )         TO lt_data_table.
          APPEND VALUE #( key = 'AG' value = <bankl>+4(4) )       TO lt_data_table.

          SPLIT <bankn> AT '-' INTO lv_conta lv_digito_conta.
          IF lv_digito_conta IS INITIAL.
            REFRESH: lt_data_table.
            APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.

            CALL FUNCTION 'GUID_CREATE'
              IMPORTING
                ev_guid_16 = lv_guid.

            ls_log-guid           = lv_guid.
            ls_log-service_header = 'VDB'.
            ls_log-service        = 'VDB'.
            ls_log-search_date    = sy-datum.
            ls_log-search_time    = sy-uzeit.

            IF <taxnum_cnpj> IS ASSIGNED.
              ls_log-stcd1 = <taxnum_cnpj>.
            ELSEIF <taxnum_cpf> IS ASSIGNED.
              ls_log-stcd2 = <taxnum_cpf>.
            ENDIF.

            ls_log-lifnr          = ls_lfa1-lifnr.
            ls_log-kunnr          = ls_kna1-kunnr.
            ls_log-partner        = iv_partner.
            ls_log-tcode          = sy-tcode.
            ls_log-username       = sy-uname.
            ls_log-bkvid          = <bkvid>.
            ls_log-banks          = 'BR'.
            ls_log-bankl          = <bankl>.
            ls_log-bankn          = <bankn>.
            ls_log-bkont          = <bkont>.
            ls_log-bkref          = <bkref>.
            ls_log-koinh          = ''.
            ls_log-validacaoconta = 'Não'.
            ls_log-status         = 'Consulta não realizada'.
            ls_log-mensagem       = |{ 'ID:' }{ <bkvid> }{ ' - Sem dígito da conta bancária' }|.

            APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
            <fs_erros> = ls_log-mensagem.

            MODIFY /netrin/log FROM ls_log.
            RETURN.

          ENDIF.

          APPEND VALUE #( key = 'CO' value = lv_conta )           TO lt_data_table.
          APPEND VALUE #( key = 'DC' value = lv_digito_conta )    TO lt_data_table.

          IF <bkref> IS NOT INITIAL.

            IF <bkref> CS 'POUPAN'.

              APPEND VALUE #( key = 'TC' value = 'CONTA_POUPANCA' ) TO lt_data_table.

            ENDIF.

            IF <bkref> EQ 'SALÁRIO' OR <bkref> EQ 'SALARIO'.

              APPEND VALUE #( key = 'TC' value = 'CONTA_PAGAMENTO' ) TO lt_data_table.

            ENDIF.

          ELSE.

            APPEND VALUE #( key = 'TC' value = 'CONTA_CORRENTE' ) TO lt_data_table.

          ENDIF.

        ENDIF.

        ASSIGN ld_data->* TO <data_table>.

        <data_table>[] = lt_data_table[].

**/ Send JSON request do WebService
        lo_webservice->consume(
          EXPORTING
            i_data_table  = ld_data
            io_webservice = lo_webservice
          IMPORTING
            e_table       = lt_return
            e_json        = DATA(lv_json)
        ).
        IF lt_return IS NOT INITIAL.

          READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_value>) WITH KEY ('FIELD') = 'MESSAGE'.
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_value> TO FIELD-SYMBOL(<m_value>).
            IF <m_value> IS ASSIGNED.

              IF <m_value> IS NOT INITIAL.
                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return_bp>.
                <fs_return_bp>-type = 'E'.
                <fs_return_bp>-message = |{ 'ID:' }{ <bkvid> }{ <m_value> }|.

              ELSE.

                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return_bp>.
                <fs_return_bp>-type = 'E'.
                <fs_return_bp>-message = |{ 'ID:' }{ <bkvid> }{ 'Erro de comunicação ao executar a consulta!' }|.
              ENDIF.

              CALL FUNCTION 'GUID_CREATE'
                IMPORTING
                  ev_guid_16 = lv_guid.

              ls_log-guid           = lv_guid.
              ls_log-service_header = 'VDB'.
              ls_log-service        = 'VDB'.
              ls_log-search_date    = sy-datum.
              ls_log-search_time    = sy-uzeit.

              IF <taxnum_cnpj> IS ASSIGNED.
                ls_log-stcd1 = <taxnum_cnpj>.
              ELSEIF <taxnum_cpf> IS ASSIGNED.
                ls_log-stcd2 = <taxnum_cpf>.
              ENDIF.

              ls_log-lifnr          = ls_lfa1-lifnr.
              ls_log-kunnr          = ls_kna1-kunnr.
              ls_log-partner        = iv_partner.
              ls_log-tcode          = sy-tcode.
              ls_log-username       = sy-uname.
              ls_log-bkvid          = <bkvid>.
              ls_log-banks          = 'BR'.
              ls_log-bankl          = <bankl>.
              ls_log-bankn          = <bankn>.
              ls_log-bkont          = <bkont>.
              ls_log-bkref          = <bkref>.
              ls_log-koinh          = ''.
              ls_log-validacaoconta = 'Não'.
              ls_log-status         = 'FALHA NA CONSULTA'.
              ls_log-mensagem       = <fs_return_bp>-message.

              APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
              <fs_erros> = ls_log-mensagem.

              MODIFY /netrin/log FROM ls_log.
              RETURN.

            ENDIF.
          ENDIF.

          READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_validacaoconta>) WITH KEY ('FIELD') = 'VALIDACAOCONTA'.
          IF sy-subrc = 0.
            ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_validacaoconta> TO FIELD-SYMBOL(<vc_value>).

            READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_mensagem>) WITH KEY ('FIELD') = 'MENSAGEM'.
            IF sy-subrc = 0.
              ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_mensagem> TO FIELD-SYMBOL(<me_value>).
              IF <me_value> IS NOT INITIAL.

                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return_bp>.
                <fs_return_bp>-type       = 'E'.
                <fs_return_bp>-id         = 'Erro'.
                <fs_return_bp>-number     = '999'.
                <fs_return_bp>-message_v1 = |{ 'ID:' }{ <bkvid> }{ <me_value> }|.

                CALL FUNCTION 'GUID_CREATE'
                  IMPORTING
                    ev_guid_16 = lv_guid.

                ls_log-guid           = lv_guid.
                ls_log-service_header = 'VDB'.
                ls_log-service        = 'VDB'.
                ls_log-search_date    = sy-datum.
                ls_log-search_time    = sy-uzeit.

                IF <taxnum_cnpj> IS ASSIGNED.
                  ls_log-stcd1 = <taxnum_cnpj>.
                ELSEIF <taxnum_cpf> IS ASSIGNED.
                  ls_log-stcd2 = <taxnum_cpf>.
                ENDIF.

                ls_log-lifnr          = ls_lfa1-lifnr.
                ls_log-kunnr          = ls_kna1-kunnr.
                ls_log-partner        = iv_partner.
                ls_log-tcode          = sy-tcode.
                ls_log-username       = sy-uname.
                ls_log-bkvid          = <bkvid>.
                ls_log-banks          = 'BR'.
                ls_log-bankl          = <bankl>.
                ls_log-bankn          = <bankn>.
                ls_log-bkont          = <bkont>.
                ls_log-bkref          = <bkref>.
                ls_log-koinh          = ''.
                ls_log-validacaoconta = <vc_value>.
                ls_log-status         = 'INATIVO'.
                ls_log-mensagem       = <fs_return_bp>-message_v1.

                "**/ Get Receipt HTML
                READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_urlcomprovante1>) WITH KEY ('FIELD') = 'URLCOMPROVANTE'.
                IF sy-subrc = 0.
                  ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_urlcomprovante1> TO FIELD-SYMBOL(<urlcomprovante_value1>).

                  IF /netrin/compcl_utilities=>get_single_value_parameter( 'COMPROVANTE_URL' ).
                    ls_log-receipt = <urlcomprovante_value1>.
                  ELSE.

                    ls_log-receipt = NEW /netrin/compcl_utilities( )->convert_comprovante_from_url(
                        io_webservice = NEW /netrin/compcl_ws_vcb( )
                        i_url         = <urlcomprovante_value1>
                        i_service     = 'VDB'
                    ).

                  ENDIF.

                ENDIF.

                APPEND INITIAL LINE TO lwa_data_response-erros ASSIGNING <fs_erros>.
                <fs_erros> = <me_value>.

                MODIFY /netrin/log FROM ls_log.


                EXIT.

              ENDIF.

            ELSEIF <vc_value> EQ 'SIM'.

              CALL FUNCTION 'GUID_CREATE'
                IMPORTING
                  ev_guid_16 = lv_guid.

              ls_log-guid           = lv_guid.
              ls_log-service_header = 'VDB'.
              ls_log-service        = 'VDB'.
              ls_log-search_date    = sy-datum.
              ls_log-search_time    = sy-uzeit.

              IF <taxnum_cnpj> IS ASSIGNED.
                ls_log-stcd1 = <taxnum_cnpj>.
              ELSEIF <taxnum_cpf> IS ASSIGNED.
                ls_log-stcd2 = <taxnum_cpf>.
              ENDIF.

              ls_log-lifnr          = ls_lfa1-lifnr.
              ls_log-kunnr          = ls_kna1-kunnr.
              ls_log-partner        = iv_partner.
              ls_log-tcode          = sy-tcode.
              ls_log-username       = sy-uname.
              ls_log-bkvid          = <bkvid>.
              ls_log-banks          = 'BR'.
              ls_log-bankl          = <bankl>.
              ls_log-bankn          = <bankn>.
              ls_log-bkont          = <bkont>.
              ls_log-bkref          = <bkref>.
              ls_log-koinh          = ''.
              ls_log-validacaoconta = <vc_value>.
              ls_log-status         = 'ATIVO'.
              ls_log-mensagem       = 'Conta Validado'.

              "**/ Get Receipt HTML
              READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_return_urlcomprovante2>) WITH KEY ('FIELD') = 'URLCOMPROVANTE'.
              IF sy-subrc = 0.
                ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_return_urlcomprovante2> TO FIELD-SYMBOL(<urlcomprovante_value2>).

                IF /netrin/compcl_utilities=>get_single_value_parameter( 'COMPROVANTE_URL' ).
                  ls_log-receipt = <urlcomprovante_value2>.
                ELSE.

                  ls_log-receipt = NEW /netrin/compcl_utilities( )->convert_comprovante_from_url(
                      io_webservice = NEW /netrin/compcl_ws_vcb( )
                      i_url         = <urlcomprovante_value2>
                      i_service     = 'VDB'
                  ).

                ENDIF.

              ENDIF.

              MODIFY /netrin/log FROM ls_log.

            ENDIF.

          ENDIF.

        ENDIF.

        REFRESH: lt_data_table, lt_return.
        APPEND CORRESPONDING #( ls_data_table ) TO lt_data_table.

      ENDLOOP.

    ENDIF.

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


  METHOD zif_integracao_inbound~validar_dados_inbound.
*
    DATA: lwa_data_request LIKE zde_data_request,
          wa_tx            TYPE zsdt0327tx,
          lv_message       TYPE string,
          lva_type         TYPE dd01v-datatype.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Metodo informado não previsto!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Payload Requisição não pode ser vazio!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    IF lwa_data_request IS INITIAL.
      r_msg_erro = 'Nenhum filtro foi informado!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Nenhum filtro foi informado!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo IS INITIAL.
      r_msg_erro = 'Grupo de contas não informado!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Grupo de contas não informado!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZIMP' AND
       lwa_data_request-consulta_netrin-cNPJ IS INITIAL.
      r_msg_erro = 'Para o grupo ZIMP necessário preencher os campos CNPJ'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZIMP necessário preencher os campos CNPJ'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZCNF' AND
       ( lwa_data_request-consulta_netrin-cpf IS INITIAL OR
         lwa_data_request-consulta_netrin-data_nasc IS INITIAL ).
      r_msg_erro = 'Para o grupo ZCNF necessário preencher os campos CPF e DATA NASCIMENTO!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZCNF necessário preencher os campos CPF e DATA NASCIMENTO!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZFUN' AND
   ( lwa_data_request-consulta_netrin-cpf IS INITIAL OR
     lwa_data_request-consulta_netrin-data_nasc IS INITIAL ).
      r_msg_erro = 'Para o grupo ZFUN necessário preencher os campos CPF e DATA NASCIMENTO!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZFUN necessário preencher os campos CPF e DATA NASCIMENTO!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZCFU' AND
   ( lwa_data_request-consulta_netrin-cpf IS INITIAL OR
     lwa_data_request-consulta_netrin-data_nasc IS INITIAL ).
      r_msg_erro = 'Para o grupo ZCFU necessário preencher os campos CPF e DATA NASCIMENTO!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZCFU necessário preencher os campos CPF e DATA NASCIMENTO!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZPEN' AND
   ( lwa_data_request-consulta_netrin-cpf IS INITIAL OR
     lwa_data_request-consulta_netrin-data_nasc IS INITIAL ).
      r_msg_erro = 'Para o grupo ZPEN necessário preencher os campos CPF e DATA NASCIMENTO!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZPEN necessário preencher os campos CPF e DATA NASCIMENTO!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZCFF' AND
   ( lwa_data_request-consulta_netrin-cpf IS INITIAL OR
     lwa_data_request-consulta_netrin-data_nasc IS INITIAL ).
      r_msg_erro = 'Para o grupo ZCFF necessário preencher os campos CPF e DATA NASCIMENTO!'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZCFF necessário preencher os campos CPF e DATA NASCIMENTO!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-nis_pis_isento = abap_true.

      IF lwa_data_request-consulta_netrin-grupo EQ 'ZFNF' AND
         ( lwa_data_request-consulta_netrin-cpf IS INITIAL OR
           lwa_data_request-consulta_netrin-data_nasc IS INITIAL OR
           lwa_data_request-consulta_netrin-nome IS INITIAL ).
        r_msg_erro = 'Para o grupo ZFNF necessário preencher os campos CPF, DATA NASCIMENTO e NOME!'.

        wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
        wa_tx-id_origem         = me->at_id_interface.
        wa_tx-origem_cadastro   = 'SE'.
        wa_tx-msg_processamento = 'Para o grupo ZFNF necessário preencher os campos CPF, DATA NASCIMENTO e NOME!'.
        wa_tx-status_proc       = 'E'.
        wa_tx-id_cli_processado = ''.
        wa_tx-dt_registro = sy-datum.
        wa_tx-hr_registro = sy-uzeit.

        MODIFY zsdt0327tx FROM wa_tx.
        CLEAR: wa_tx.
        COMMIT WORK.

        RETURN.
      ENDIF.

    ELSE.

      IF lwa_data_request-consulta_netrin-grupo EQ 'ZFNF' AND
         ( lwa_data_request-consulta_netrin-cpf IS INITIAL OR
           lwa_data_request-consulta_netrin-nis_pis IS INITIAL OR
           lwa_data_request-consulta_netrin-data_nasc IS INITIAL OR
           lwa_data_request-consulta_netrin-nome IS INITIAL ).
        r_msg_erro = 'Para o grupo ZFNF necessário preencher os campos CPF, DATA NASCIMENTO, NIS_PIS e NOME!'.

        wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
        wa_tx-id_origem         = me->at_id_interface.
        wa_tx-origem_cadastro   = 'SE'.
        wa_tx-msg_processamento = 'Para o grupo ZFNF necessário preencher os campos CPF, DATA NASCIMENTO, NIS_PIS e NOME!'.
        wa_tx-status_proc       = 'E'.
        wa_tx-id_cli_processado = ''.
        wa_tx-dt_registro = sy-datum.
        wa_tx-hr_registro = sy-uzeit.

        MODIFY zsdt0327tx FROM wa_tx.
        CLEAR: wa_tx.
        COMMIT WORK.

        RETURN.
      ENDIF.

    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZFNJ' AND
       ( lwa_data_request-consulta_netrin-regiao IS INITIAL OR
         lwa_data_request-consulta_netrin-ie IS INITIAL OR
         lwa_data_request-consulta_netrin-cnpj IS INITIAL ) AND
         lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.
      r_msg_erro = 'Para o grupo ZFNJ necessário preencher os campos UF, IE e CNPJ'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZFNJ necessário preencher os campos UF, IE e CNPJ'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZCNJ' AND
       ( lwa_data_request-consulta_netrin-regiao IS INITIAL OR
         lwa_data_request-consulta_netrin-ie IS INITIAL OR
         lwa_data_request-consulta_netrin-cnpj IS INITIAL ) AND
         lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.

      r_msg_erro = 'Para o grupo ZCNJ necessário preencher os campos UF, IE e CNPJ'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZCNJ necessário preencher os campos UF, IE e CNPJ'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZPRJ' AND
   ( lwa_data_request-consulta_netrin-regiao IS INITIAL OR
     lwa_data_request-consulta_netrin-ie IS INITIAL OR
     lwa_data_request-consulta_netrin-cnpj IS INITIAL ) AND
         lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.

      r_msg_erro = 'Para o grupo ZPRJ necessário preencher os campos UF, IE e CNPJ'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZPRJ necessário preencher os campos UF, IE e CNPJ'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPJ' AND
      ( lwa_data_request-consulta_netrin-regiao IS INITIAL OR
       lwa_data_request-consulta_netrin-ie IS INITIAL OR
       lwa_data_request-consulta_netrin-cnpj IS INITIAL ) AND
         lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.

      r_msg_erro = 'Para o grupo ZCPJ necessário preencher os campos UF, IE e CNPJ'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZCPJ necessário preencher os campos UF, IE e CNPJ'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPF' AND
     ( lwa_data_request-consulta_netrin-regiao IS INITIAL OR
       lwa_data_request-consulta_netrin-ie IS INITIAL OR
       lwa_data_request-consulta_netrin-cpf IS INITIAL ) AND
         lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.

      r_msg_erro = 'Para o grupo ZCPF necessário preencher os campos UF, IE e CPF'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZCPF necessário preencher os campos UF, IE e CPF'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZPRF' AND
     ( lwa_data_request-consulta_netrin-regiao IS INITIAL OR
       lwa_data_request-consulta_netrin-ie IS INITIAL OR
       lwa_data_request-consulta_netrin-cpf IS INITIAL ) AND
         lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.

      r_msg_erro = 'Para o grupo ZPRF necessário preencher os campos UF, IE e CPF'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZPRF necessário preencher os campos UF, IE e CPF'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZFFJ' AND
      ( lwa_data_request-consulta_netrin-regiao IS INITIAL OR
       lwa_data_request-consulta_netrin-ie IS INITIAL OR
       lwa_data_request-consulta_netrin-cnpj IS INITIAL  ) AND
         lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.

      r_msg_erro = 'Para o grupo ZFFJ necessário preencher os campos UF, IE, CNPJ e RNTRC_ANTT'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZFFJ necessário preencher os campos UF, IE, CNPJ e RNTRC_ANTT'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZCFJ' AND
  ( lwa_data_request-consulta_netrin-regiao IS INITIAL OR
   lwa_data_request-consulta_netrin-ie IS INITIAL OR
   lwa_data_request-consulta_netrin-cnpj IS INITIAL ) AND
         lwa_data_request-consulta_netrin-ignorar_sintegra IS INITIAL.

      r_msg_erro = 'Para o grupo ZCFJ necessário preencher os campos UF, IE e CNPJ'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZCFJ necessário preencher os campos UF, IE e CNPJ'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZFFF' AND
  ( lwa_data_request-consulta_netrin-data_nasc IS INITIAL OR
   lwa_data_request-consulta_netrin-nis_pis IS INITIAL OR
   lwa_data_request-consulta_netrin-cpf IS INITIAL OR
   lwa_data_request-consulta_netrin-nome  IS INITIAL ).
      r_msg_erro = 'Para o grupo ZFFF necessário preencher os campos CPF, RNTRC/ANTT, NIS/PIS, DATA DE NASCIMENTO e NOME'.

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'SE'.
      wa_tx-msg_processamento = 'Para o grupo ZFFF necessário preencher os campos CPF, RNTRC/ANTT, NIS/PIS, DATA DE NASCIMENTO e NOME'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

      RETURN.
    ENDIF.

* VALIDAR CPF / RG / INSCRICAO COM CARACTERES ESPECIAIS
    IF lwa_data_request-consulta_netrin-cpf IS NOT  INITIAL.
      IF lwa_data_request-consulta_netrin-cpf CA '/.,:;"[]\{}|<>?/'.

        CONCATENATE 'Caractere Especial não é permitido para o CPF:'  lwa_data_request-consulta_netrin-cpf
        INTO lv_message  SEPARATED BY space.

        r_msg_erro = lv_message.

        wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
        wa_tx-id_origem         = me->at_id_interface.
        wa_tx-origem_cadastro   = 'SE'.
        wa_tx-msg_processamento = lv_message.
        wa_tx-status_proc       = 'E'.
        wa_tx-id_cli_processado = ''.
        wa_tx-dt_registro = sy-datum.
        wa_tx-hr_registro = sy-uzeit.

        MODIFY zsdt0327tx FROM wa_tx.
        CLEAR: wa_tx.
        COMMIT WORK.

        RETURN.

      ELSE.

        CLEAR: lva_type.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = lwa_data_request-consulta_netrin-cpf
          IMPORTING
            htype     = lva_type.

        IF lva_type NE 'NUMC'.
          CONCATENATE 'String não permitido no campo CPF:'  lwa_data_request-consulta_netrin-cpf
             INTO lv_message  SEPARATED BY space.

          r_msg_erro = lv_message.

          wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
          wa_tx-id_origem         = me->at_id_interface.
          wa_tx-origem_cadastro   = 'SE'.
          wa_tx-msg_processamento = lv_message.
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.

          MODIFY zsdt0327tx FROM wa_tx.
          CLEAR: wa_tx.
          COMMIT WORK.

          RETURN.

        ENDIF.

      ENDIF.

    ENDIF.

    IF  lwa_data_request-consulta_netrin-cnpj IS NOT INITIAL.
      IF lwa_data_request-consulta_netrin-cnpj  CA '/.,:;"[]\{}|<>?/'.

        CONCATENATE 'Caractere Especial não é permitido para o CNPJ:'  lwa_data_request-consulta_netrin-cnpj
        INTO lv_message  SEPARATED BY space.

        r_msg_erro = lv_message.

        wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
        wa_tx-id_origem         = me->at_id_interface.
        wa_tx-origem_cadastro   = 'SE'.
        wa_tx-msg_processamento = lv_message.
        wa_tx-status_proc       = 'E'.
        wa_tx-id_cli_processado = ''.
        wa_tx-dt_registro = sy-datum.
        wa_tx-hr_registro = sy-uzeit.

        MODIFY zsdt0327tx FROM wa_tx.
        CLEAR: wa_tx.
        COMMIT WORK.

        RETURN.

      ELSE.

        CLEAR: lva_type.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = lwa_data_request-consulta_netrin-cnpj
          IMPORTING
            htype     = lva_type.

        IF lva_type NE 'NUMC'.

          CONCATENATE 'String não permitido no campo CNPJ:'  lwa_data_request-consulta_netrin-cnpj
             INTO lv_message  SEPARATED BY space.

          r_msg_erro = lv_message.

          wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
          wa_tx-id_origem         = me->at_id_interface.
          wa_tx-origem_cadastro   = 'SE'.
          wa_tx-msg_processamento = lv_message.
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.

          MODIFY zsdt0327tx FROM wa_tx.
          CLEAR: wa_tx.
          COMMIT WORK.

          RETURN.

        ENDIF.

      ENDIF.

    ENDIF.

    IF lwa_data_request-dados_gerais-rg IS NOT INITIAL.
      IF lwa_data_request-dados_gerais-rg  CA '/.,:;"[]\{}|<>?/'.
        CONCATENATE 'Caractere Especial não é permitido para o RG:'  lwa_data_request-dados_gerais-rg
        INTO lv_message  SEPARATED BY space.

        r_msg_erro = lv_message.

        wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
        wa_tx-id_origem         = me->at_id_interface.
        wa_tx-origem_cadastro   = 'SE'.
        wa_tx-msg_processamento = lv_message.
        wa_tx-status_proc       = 'E'.
        wa_tx-id_cli_processado = ''.
        wa_tx-dt_registro = sy-datum.
        wa_tx-hr_registro = sy-uzeit.

        MODIFY zsdt0327tx FROM wa_tx.
        CLEAR: wa_tx.
        COMMIT WORK.

        RETURN.

      ENDIF.

    ENDIF.

  ENDMETHOD.


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

    DATA: lwa_data_request   LIKE zde_data_request,
          lwa_data_response  LIKE zde_data_response,
          lwa_data_response2 LIKE zde_data_response,
          ls_addr1           TYPE addr1_data,
          ls_lfa1            TYPE lfa1,
          ls_kna1            TYPE kna1,
          wa_tx              TYPE zsdt0327tx,
          lt_tx              TYPE TABLE OF zsdt0327tx.

    DATA:
      lra_mblnr      TYPE RANGE OF mblnr,
      lra_mjahr      TYPE RANGE OF mjahr,
      lra_budat      TYPE RANGE OF budat,
      lra_bldat      TYPE RANGE OF bldat,
      lra_xblnr      TYPE RANGE OF xblnr,
      lra_smbln      TYPE RANGE OF mblnr,
      lra_smblp      TYPE RANGE OF mblpo,
      lra_mat        TYPE RANGE OF mblnr,
      lv_cnpj_matriz TYPE lfa1-stcd1.

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

*** Inicio - Rubenilson - 06.12.24 - US159528
    IF  lwa_data_request-consulta_netrin-grupo EQ 'ZPRF' AND lwa_data_request-parceiro IS INITIAL. "US #184108 - MMSILVA - 03.06.2025 - Incluído "lwa_data_request-parceiro"
      lwa_data_request-consulta_netrin-grupo = 'ZCPF'.
    ELSEIF lwa_data_request-consulta_netrin-grupo EQ 'ZPRJ' AND lwa_data_request-parceiro IS INITIAL. "US #184108 - MMSILVA - 03.06.2025 - Incluído "lwa_data_request-parceiro"
      lwa_data_request-consulta_netrin-grupo = 'ZCPJ'.
    ENDIF.
*** Fim - Rubenilson - 06.12.24 - US159528

    IF lwa_data_request-consulta_netrin-grupo EQ 'ZFNF' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZFUN' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZIMP' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZPEN' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZFNJ' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZPRF' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZPRJ' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZFFJ' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZFFF' OR
       lwa_data_request-consulta_netrin-grupo EQ 'ZFEX'.

*    IF lwa_data_request-consulta_netrin-cnpj IS NOT INITIAL.

      IF lwa_data_request-parceiro  IS INITIAL.

        CALL METHOD me->cria_fornecedor
          EXPORTING
            account           = lwa_data_request-consulta_netrin-grupo
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
            wa_tx-origem_cadastro   = 'SE'.
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
          wa_tx-origem_cadastro   = 'SE'.
          CONCATENATE 'Parceiro' lwa_data_response-parceiro 'criado com sucesso!' INTO wa_tx-msg_processamento SEPARATED BY space .
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.
          MODIFY zsdt0327tx FROM wa_tx.
          COMMIT WORK.
*** Inicio - Rubenilson - 06.12.24 - US159528
          IF lwa_data_request-consulta_netrin-grupo EQ 'ZPRJ' OR
             lwa_data_request-consulta_netrin-grupo EQ 'ZPRF'.

            lwa_data_request-parceiro  = lwa_data_response-parceiro.

            IF lwa_data_request-consulta_netrin-grupo EQ 'ZPRF'.
              lwa_data_request-consulta_netrin-grupo = 'ZCPF'.
            ELSE.
              lwa_data_request-consulta_netrin-grupo = 'ZCPJ'.
            ENDIF.

            CALL METHOD me->atualiza_cliente
              EXPORTING
                account           = lwa_data_request-consulta_netrin-grupo
                lwa_data_request  = lwa_data_request
              IMPORTING
                lwa_data_response = lwa_data_response
              CHANGING
                cs_addr1          = ls_addr1
                cs_kna1           = ls_kna1.

          ENDIF.
*** Fim - Rubenilson - 06.12.24 - US159528

        ENDIF.

      ELSE.

        CALL METHOD me->atualiza_fornecedor
          EXPORTING
            account           = lwa_data_request-consulta_netrin-grupo
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
            wa_tx-origem_cadastro   = 'SE'.
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
          wa_tx-origem_cadastro   = 'SE'.
          CONCATENATE 'Parceiro' lwa_data_response-parceiro 'atualizado com sucesso!' INTO wa_tx-msg_processamento SEPARATED BY space .
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.
          MODIFY zsdt0327tx FROM wa_tx.
          COMMIT WORK.

        ENDIF.

      ENDIF.

    ELSE.

      IF lwa_data_request-parceiro IS INITIAL.

        CALL METHOD me->cria_cliente
          EXPORTING
            account           = lwa_data_request-consulta_netrin-grupo
            lwa_data_request  = lwa_data_request
          IMPORTING
            lwa_data_response = lwa_data_response
          CHANGING
            cs_addr1          = ls_addr1
            cs_kna1           = ls_kna1.

        IF lwa_data_response-erros IS NOT INITIAL.

          LOOP AT lwa_data_response-erros  ASSIGNING <fs_erros>.

            wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
            wa_tx-id_origem         = me->at_id_interface.
            wa_tx-origem_cadastro   = 'SE'.
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
          wa_tx-origem_cadastro   = 'SE'.
          CONCATENATE 'Parceiro' lwa_data_response-parceiro 'criado com sucesso!' INTO wa_tx-msg_processamento SEPARATED BY space .
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.
          MODIFY zsdt0327tx FROM wa_tx.
          COMMIT WORK.
*** Inicio - Rubenilson - 06.12.24 - US159528
          IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPF' OR
             lwa_data_request-consulta_netrin-grupo EQ 'ZCPJ'.

            lwa_data_request-parceiro = lwa_data_response-parceiro .

            IF lwa_data_request-consulta_netrin-grupo EQ 'ZCPF'.
              lwa_data_request-consulta_netrin-grupo = 'ZPRF'.
            ELSE.
              lwa_data_request-consulta_netrin-grupo = 'ZPRJ'.
            ENDIF.

            CALL METHOD me->atualiza_fornecedor
              EXPORTING
                account           = lwa_data_request-consulta_netrin-grupo
                lwa_data_request  = lwa_data_request
              IMPORTING
                lwa_data_response = lwa_data_response
              CHANGING
                addr1             = ls_addr1
                lfa1              = ls_lfa1.

          ENDIF.

        ENDIF.
*** Fim - Rubenilson - 06.12.24 - US159528
      ELSE.

        CALL METHOD me->atualiza_cliente
          EXPORTING
            account           = lwa_data_request-consulta_netrin-grupo
            lwa_data_request  = lwa_data_request
          IMPORTING
            lwa_data_response = lwa_data_response
          CHANGING
            cs_addr1          = ls_addr1
            cs_kna1           = ls_kna1.

        IF lwa_data_response-erros IS NOT INITIAL.

          LOOP AT lwa_data_response-erros  ASSIGNING <fs_erros>.

            wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
            wa_tx-id_origem         = me->at_id_interface.
            wa_tx-origem_cadastro   = 'SE'.
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

***       US #184108 - MMSILVA - 29.05.2025 - Inicio ***

            wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
            wa_tx-id_origem         = me->at_id_interface.
            wa_tx-origem_cadastro   = 'SE'.
            CONCATENATE 'Parceiro' lwa_data_response-parceiro 'atualizado com sucesso!' INTO wa_tx-msg_processamento SEPARATED BY space .
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro = sy-datum.
            wa_tx-hr_registro = sy-uzeit.
            MODIFY zsdt0327tx FROM wa_tx.
            COMMIT WORK.
***       US #184108 - MMSILVA - 29.05.2025 - Fim ***

        ENDIF.

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
