class ZCL_INT_IB_SD_DUE_ANTECIPADA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  types:
    BEGIN OF ty_branch,
        cnpj    TYPE bapibranch-cgc_number,
        name    TYPE bapibranch-name,
        country TYPE adrc-country,
        region  TYPE adrc-region,
        street  TYPE adrc-street,
        city2   TYPE adrc-city2,
        city1   TYPE adrc-city1.
    TYPES  END OF ty_branch .
  types:
    BEGIN OF ty_zsdt0170,
        include TYPE zsdt0170.
    TYPES  END OF ty_zsdt0170 .
  types:
    tyt_zsdt0170 TYPE TABLE OF ty_zsdt0170 .

  data AT_ID_INTERFACE type ZDE_ID_INTERFACE .
  data AT_ID_DUE type NUM10 .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  methods GET_FILIAL_MATRIZ
    importing
      !IV_BUKRS type BUKRS
    exporting
      !ES_BRANCH type TY_BRANCH .
  methods GET_IMPORTADOR
    changing
      !IS_IT_DUE type ZSDT0172 .
  methods GET_DESPACHO
    changing
      !IS_CAB_DUE type ZSDT0170 .
  methods GET_QTD_UN_MEDIDA
    changing
      !IS_IT_DUE type ZSDT0172 .
  methods GET_NEW_DUE
    changing
      !CS_CAB_DUE type TYT_ZSDT0170 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_SD_DUE_ANTECIPADA IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD get_despacho.

    IF is_cab_due-codigo_ra_despacho IS NOT INITIAL.
      "Dados Mestre Recinto Alfandegado
      SELECT SINGLE *
        FROM zsdt0168 INTO @DATA(_wl_0168)
       WHERE codigo_ra      EQ @is_cab_due-codigo_ra_despacho
         AND local_despacho EQ @abap_true.

      IF sy-subrc = 0.
        "Depara Recinto Alfandegado x Unidade Receita Federal
        SELECT SINGLE *
          FROM zsdt0169 INTO @DATA(_wl_0169)
         WHERE codigo_ra EQ @_wl_0168-codigo_ra.

        IF ( sy-subrc = 0 ) AND ( _wl_0169-codigo_urf IS NOT INITIAL ).
          "Dados Mestre Unidade Receita Federal
          SELECT SINGLE *
            FROM zsdt0167 INTO @DATA(_wl_0167)
           WHERE codigo_urf EQ @_wl_0169-codigo_urf.

          IF sy-subrc NE 0.
*            MESSAGE | Cadastro da URF { _wl_0169-codigo_urf } não encontrada! | TYPE 'W'.
*            CLEAR: is_cab_due-codigo_ra_despacho.
          ELSE.
            "CAB_DUE-DS_RA_DESPACHO      = _WL_0168-DS_RA.
            is_cab_due-codigo_urf_despacho = _wl_0167-codigo_urf.
            "CAB_DUE-DS_URF_DESPACHO     = _WL_0167-DS_URF.
          ENDIF.
        ELSE.
*          MESSAGE | Depara para o Recinto { is_cab_due-codigo_ra_despacho } não encontrado( Transação ZSDT0137)! | TYPE 'W'.
*          CLEAR: is_cab_due-codigo_ra_despacho.
        ENDIF.
      ELSE.
*        MESSAGE | O Código { is_cab_due-codigo_ra_despacho } não se refere à um Recinto Alfandegado de Despacho! | TYPE 'W'.
*        CLEAR: is_cab_due-codigo_ra_despacho.
      ENDIF.
    ENDIF.

    IF is_cab_due-codigo_ra_embarque IS NOT INITIAL.
      "Dados Mestre Recinto Alfandegado
      SELECT SINGLE *
        FROM zsdt0168 INTO _wl_0168
       WHERE codigo_ra      EQ is_cab_due-codigo_ra_embarque
         AND local_embarque EQ abap_true.

      IF sy-subrc = 0.
        "Depara Recinto Alfandegado x Unidade Receita Federal
        SELECT SINGLE *
          FROM zsdt0169 INTO _wl_0169
         WHERE codigo_ra EQ _wl_0168-codigo_ra.

        IF ( sy-subrc = 0 ) AND ( _wl_0169-codigo_urf IS NOT INITIAL ).
          "Dados Mestre Unidade Receita Federal
          SELECT SINGLE *
            FROM zsdt0167 INTO _wl_0167
           WHERE codigo_urf EQ _wl_0169-codigo_urf.

          IF sy-subrc NE 0.
*          MESSAGE | Cadastro da URF { _wl_0169-codigo_urf } não encontrada! | TYPE 'W'.
*          CLEAR: cab_due-codigo_ra_embarque.
          ELSE.
            "CAB_DUE-DS_RA_EMBARQUE      = _WL_0168-DS_RA.
            is_cab_due-codigo_urf_embarque = _wl_0167-codigo_urf.
            "CAB_DUE-DS_URF_EMBARQUE     = _WL_0167-DS_URF.
          ENDIF.
        ELSE.
*        MESSAGE | Depara para o Recinto { is_cab_due-codigo_ra_embarque } não encontrado( Transação ZSDT0137)! | TYPE 'W'.
*        CLEAR: is_cab_due-codigo_ra_embarque.
        ENDIF.
      ELSE.
*      MESSAGE | O Código { is_cab_due-codigo_ra_embarque } não se refere à um Recinto Alfandegado de Embarque! | TYPE 'W'.
*      CLEAR: is_cab_due-codigo_ra_embarque.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD get_filial_matriz.

    DATA: wl_branch_detail TYPE bapibranch.

    CLEAR: wl_branch_detail, es_branch.

    CHECK iv_bukrs IS NOT INITIAL.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(_wl_branch)
     WHERE bukrs  = @iv_bukrs
       AND branch = '0001'. "Matriz

    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
        EXPORTING
          company       = _wl_branch-bukrs
          branch        = _wl_branch-branch
        IMPORTING
          branch_detail = wl_branch_detail.

      es_branch-cnpj = wl_branch_detail-cgc_number.
      es_branch-name = wl_branch_detail-name.

      IF wl_branch_detail-adrnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc INTO @DATA(_wl_adrc)
         WHERE addrnumber = @wl_branch_detail-adrnr.

        IF ( sy-subrc = 0 ).
          es_branch-country =  _wl_adrc-country.
          es_branch-region  =  _wl_adrc-region.
          es_branch-street  =  _wl_adrc-street.
          es_branch-city2   =  _wl_adrc-city2.
          es_branch-city1   =  _wl_adrc-city1.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_importador.

    IF is_it_due-importador_codigo IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = is_it_due-importador_codigo
        IMPORTING
          output = is_it_due-importador_codigo.

      SELECT SINGLE *
        FROM kna1 INTO @DATA(_wl_kna1)
       WHERE kunnr EQ @is_it_due-importador_codigo.

      IF sy-subrc NE 0.
*      MESSAGE |Cliente Importador { IS_IT_DUE-importador_codigo } não encontrado! | TYPE 'S'.
*      CLEAR: IS_IT_DUE-importador_codigo.
      ELSE.
        is_it_due-importador_nome = _wl_kna1-name1.

        IF _wl_kna1-adrnr IS NOT INITIAL.
          SELECT SINGLE *
            FROM adrc INTO @DATA(_wl_adrc)
           WHERE addrnumber EQ @_wl_kna1-adrnr.

          IF sy-subrc = 0.
            is_it_due-importador_country  = _wl_adrc-country.
            CONCATENATE _wl_adrc-street _wl_adrc-city2 '/' _wl_adrc-city1
                   INTO is_it_due-importador_cpl_end SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_new_due.

*----------------------------------------------------------------
*    LOOP AT cs_cab_due ASSIGNING FIELD-SYMBOL(<fs_cab_due>).
** RANGE
*    ENDLOOP.

    DATA: it_zsdt0170 TYPE TABLE OF zsdt0170,
          ic_zsdt0170 TYPE TABLE OF zsdt0170.

    MOVE cs_cab_due[] TO ic_zsdt0170[].
    DATA(lv_datant) = sy-datum - 1.
    SELECT *
      FROM zsdt0170
      INTO TABLE it_zsdt0170
     FOR ALL ENTRIES IN ic_zsdt0170
     WHERE  bukrs            EQ ic_zsdt0170-bukrs
        AND id_nomeacao_tran EQ ic_zsdt0170-id_nomeacao_tran
        AND cnpj_declarante  EQ ic_zsdt0170-cnpj_declarante
        AND land1            EQ ic_zsdt0170-land1
        AND ( dt_registro    EQ sy-datum
         OR dt_registro      EQ lv_datant ).

    IF sy-subrc IS INITIAL AND it_zsdt0170[] IS NOT INITIAL.
      MOVE it_zsdt0170[] TO cs_cab_due[].
    ENDIF.

  ENDMETHOD.


  METHOD get_qtd_un_medida.

    DATA: v_matnr     TYPE matnr18,"TYPE mara-matnr,
          wl_mara     TYPE mara,
          v_meins_in  TYPE mara-meins,
          v_meins_out TYPE mara-meins,
          v_qtde      TYPE j1b_nf_xml_item-qtrib,
          var_answer  TYPE c,
          v_vlr       TYPE zsdt0172-vlr_local_embarque,
          v_ncm       TYPE j1b_nf_xml_item-ncm.

    IF is_it_due-matnr IS NOT INITIAL.

**********************************************************************
* PSA CONVERT MATNR 18
*** Formata o código do material
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = is_it_due-matnr
    IMPORTING
      output = v_matnr.

* END CONVERT
**********************************************************************

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = is_it_due-matnr
*        IMPORTING
*          output = v_matnr.


      is_it_due-matnr = v_matnr.

      CLEAR: wl_mara.
      SELECT SINGLE *
        FROM mara INTO wl_mara
       WHERE matnr EQ v_matnr.

      SELECT SINGLE *
        FROM makt INTO @DATA(_wl_makt)
       WHERE matnr EQ @v_matnr
         AND spras EQ @sy-langu.

      IF ( sy-subrc NE 0 ) OR ( wl_mara IS INITIAL ).
*        MESSAGE |Material { is_it_due-matnr   } não encontrado! | TYPE 'S'.
*        CLEAR: is_it_due-matnr.
      ELSE.

        IF wl_mara-meins IS NOT INITIAL.
*          MESSAGE |Material { is_it_due-matnr   } sem unidade de Medida!! | TYPE 'S'.
*          CLEAR: is_it_due-matnr.
*        ELSE.
          is_it_due-uc_exportada    = wl_mara-meins.
          is_it_due-desc_mercadoria = _wl_makt-maktx.

          SELECT *
            FROM marc AS a INTO TABLE @DATA(_tg_marc)
           WHERE a~matnr EQ @_wl_makt-matnr
             AND a~steuc NE @space
             AND EXISTS ( SELECT b~centrov_1
                            FROM zsdt_depara_cen AS b
                           WHERE b~centrov_1 EQ a~werks ).

          IF _tg_marc[] IS INITIAL.
*            MESSAGE |NCM do Material { is_it_due-matnr } não encontrado! | TYPE 'S'.
          ELSE.
            READ TABLE _tg_marc INTO DATA(_wl_marc) INDEX 1.
            IF is_it_due-codigo_ncm IS INITIAL.
              is_it_due-codigo_ncm = _wl_marc-steuc.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ( is_it_due-matnr      IS NOT INITIAL ) AND
       ( is_it_due-codigo_ncm IS NOT INITIAL ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = is_it_due-matnr
        IMPORTING
          output = v_matnr.

      CLEAR: _tg_marc[].
      SELECT *
        FROM marc AS a INTO TABLE _tg_marc
       WHERE a~matnr EQ v_matnr
         AND a~steuc EQ is_it_due-codigo_ncm
         AND EXISTS ( SELECT b~centrov_1
                        FROM zsdt_depara_cen AS b
                       WHERE b~centrov_1 EQ a~werks ).

*      IF _tg_marc[] IS INITIAL.
      "MESSAGE |NCM do Material { IS_IT_DUE-MATNR } é inválido! | TYPE 'S'.
*        CLEAR: is_it_due-codigo_ncm.
*      ENDIF.

*    ELSE.
*      CLEAR: is_it_due-codigo_ncm.
    ENDIF.

*    PERFORM f_atrib_ue_exp USING is_it_due-codigo_ncm
*                        CHANGING is_it_due-ue_exportada.

*FORM f_atrib_ue_exp  USING p_ncm  TYPE zsdt0172-codigo_ncm
*                  CHANGING ue_exp TYPE zsdt0172-ue_exportada.

*  CLEAR: ue_exp.

    v_ncm = is_it_due-codigo_ncm.

    IF v_ncm IS NOT INITIAL.

*  v_ncm = p_ncm.

      REPLACE ALL OCCURRENCES OF '.' IN v_ncm WITH ''.

      SELECT SINGLE *
        FROM setleaf INTO @DATA(_wl_set_ncm_utrib)
       WHERE setname = 'MAGGI_NCM_UTRIB_EXP'
         AND valfrom = @v_ncm.

      IF sy-subrc EQ 0.
        is_it_due-ue_exportada = 'TON'.
      ELSE.
        is_it_due-ue_exportada = 'KG'.
      ENDIF.
    ENDIF.

*ENDFORM.


    IF ( is_it_due-matnr             IS NOT INITIAL ) AND
       ( is_it_due-peso_liq_total    IS NOT INITIAL ) AND
       ( is_it_due-ue_exportada      IS NOT INITIAL ) AND
       ( is_it_due-uc_exportada      IS NOT INITIAL ).

      v_meins_in  = 'KG'.

*------------------------------------------------------*
*   Quantidade Estatistica
*------------------------------------------------------*
      v_qtde      = is_it_due-peso_liq_total.

      v_meins_out = is_it_due-ue_exportada.

      IF v_meins_out = 'TON'.
        v_meins_out = 'TO'.
      ENDIF.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = is_it_due-matnr
          i_in_me              = v_meins_in
          i_out_me             = v_meins_out
          i_menge              = v_qtde
        IMPORTING
          e_menge              = v_qtde
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

      IF sy-subrc = 0.
        is_it_due-qtde_ue_exportada = v_qtde.
      ENDIF.

*------------------------------------------------------*
*   Quantidade Comercializada
*------------------------------------------------------*
      v_qtde      = is_it_due-peso_liq_total.

      v_meins_out = is_it_due-uc_exportada.

      IF v_meins_out = 'TON'.
        v_meins_out = 'TO'.
      ENDIF.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = is_it_due-matnr
          i_in_me              = v_meins_in
          i_out_me             = v_meins_out
          i_menge              = v_qtde
        IMPORTING
          e_menge              = v_qtde
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

      IF sy-subrc = 0.
        is_it_due-qtde_uc_exportada = v_qtde.
      ENDIF.

      IF is_it_due-preco_ton IS NOT INITIAL.
        v_qtde = is_it_due-peso_liq_total / 1000. "Converter de Kg para Toneladas
        v_vlr  = v_qtde * is_it_due-preco_ton.

        IF is_it_due-vlr_local_embarque IS INITIAL.
          is_it_due-vlr_local_embarque = v_vlr.
        ELSEIF is_it_due-vlr_local_embarque NE v_vlr.

*          CALL FUNCTION 'POPUP_TO_CONFIRM'
*            EXPORTING
*              titlebar              = 'Confirmação'
*              text_question         = |Valor Local Embarque atual: { is_it_due-vlr_local_embarque } é diferente do valor Calculado: { v_vlr }! Deseja corrigir?|
*              text_button_1         = 'Sim'
*              text_button_2         = 'Não'
*              default_button        = '1'
*              display_cancel_button = ''
*            IMPORTING
*              answer                = var_answer
*            EXCEPTIONS
*              text_not_found        = 1
*              OTHERS                = 2.

*          IF var_answer EQ '1'.
          is_it_due-vlr_local_embarque = v_vlr.
*          ENDIF.

        ENDIF.

        IF is_it_due-vlr_cond_venda IS INITIAL.
          is_it_due-vlr_cond_venda = v_vlr.
        ELSEIF is_it_due-vlr_cond_venda NE v_vlr.

*          CALL FUNCTION 'POPUP_TO_CONFIRM'
*            EXPORTING
*              titlebar              = 'Confirmação'
*              text_question         = |Valor Condição Venda atual: { is_it_due-vlr_cond_venda } é diferente do valor Calculado: { v_vlr }! Deseja corrigir?|
*              text_button_1         = 'Sim'
*              text_button_2         = 'Não'
*              default_button        = '1'
*              display_cancel_button = ''
*            IMPORTING
*              answer                = var_answer
*            EXCEPTIONS
*              text_not_found        = 1
*              OTHERS                = 2.
*
*          IF var_answer EQ '1'.
          is_it_due-vlr_cond_venda = v_vlr.
*          ENDIF.
        ENDIF.
      ENDIF.
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


  method zif_integracao_inbound~validar_dados_inbound.
*
    data: lit_data_inbound type zsde015,
*          lit_data_inbound TYPE zsdt0313_t,
          les_cab          type zsdt0170,
          ln_id(4)         type n,
          ox_err           type ref to cx_root,
          lit_cab          type table of zsdt0170,
          lic_cab          type tyt_zsdt0170,
          lit_itens        type table of zsdt0172,
          wa_itens         type zsdt0172,
          wa_branch        type ty_branch,
          lit_paises       type table of zsdt0174,
          wa_paises        type zsdt0174,
*** US #182074 - MMSILVA - 25.06.2025 - Ini ***
          lit_lpco         type table of zsdt0190,
          wa_lpco          type zsdt0190.
*** US #182074 - MMSILVA - 25.06.2025 - Fim ***

    clear: r_msg_erro.

    try.

        data(lo_due) = new zcl_due( ).

        if me->zif_integracao_inject~at_info_request_http-ds_metodo ne me->zif_integracao_inject~co_request_method_post   and
           me->zif_integracao_inject~at_info_request_http-ds_metodo ne me->zif_integracao_inject~co_request_method_delete.
          r_msg_erro = 'Metodo informado não reconhecido!'(e01).
          return.

        endif.

        /ui2/cl_json=>deserialize( exporting json = i_data_inbound changing data = lit_data_inbound ).
*        /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = les_cab ).
*        /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_cab ).
*        /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_itens ).
*        /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_paises ).

        if lit_data_inbound is not initial.

* CAB
          les_cab-id_nomeacao_tran      = lit_data_inbound-cab_id_nomeacao.
*          les_cab-ds_empresa            = lit_data_inbound-cab_empresa. "CAB_DUE-BUKRS
          les_cab-bukrs                 = lit_data_inbound-cab_empresa. "CAB_DUE-BUKRS
          les_cab-tp_due                = lit_data_inbound-cab_tp_due.
          les_cab-cnpj_declarante       = lit_data_inbound-cab_cnpj_declarante.
          les_cab-land1                 = lit_data_inbound-cab_pais.
          les_cab-tp_cod_local_despacho = lit_data_inbound-cab_tplocal.  " Esse faz parte? Analisar?


          les_cab-codigo_ra_despacho    = lit_data_inbound-cab_d_recinto_alfandegado.
*          les_cab-codigo_urf_despacho   = lit_data_inbound-cab_d_rfb.
*          "( Verificar tratativa igual a existente hj  : Include:  LZDUEF01 Linha 88 a 114 utilizando CAB_DUE-CODIGO_RA_DESPACHO )


          les_cab-forma_exportacao      = lit_data_inbound-cab_forma_exportacao.
          les_cab-situacao_especial     = lit_data_inbound-cab_sitespecial.
          les_cab-moeda_cambio          = lit_data_inbound-cab_moeda."
          les_cab-observacoes_gerais    = lit_data_inbound-cab_e_erfb. "Observações gerais
          les_cab-codigo_ra_embarque    = lit_data_inbound-cab_e_recinto_alfandegado.
*          les_cab-codigo_urf_embarque   =
          "mesma tratativa atual: include: LZDUEF01 linhas 149 - 184  (CAB_DUE-CODIGO_URF_DESPACHO ) " Analisar faz parte lógica despacho acima em conjunto?

          me->get_despacho( changing is_cab_due = les_cab ).

* Itens
          loop at lit_data_inbound-itens[] assigning field-symbol(<fs_itens>).

            ln_id = ln_id + 1.
            wa_itens-id_due_item = ln_id.
            wa_itens-exportador_cnpj = les_cab-cnpj_declarante.

            me->get_filial_matriz( exporting iv_bukrs = les_cab-bukrs
                                   importing es_branch = wa_branch ).

            "Dados Exportador
            if wa_branch is not initial.
              wa_itens-exportador_nome       = wa_branch-name.
              wa_itens-exportador_country    = wa_branch-country.
              wa_itens-exportador_region     = wa_branch-region.
              concatenate wa_branch-street wa_branch-city2 '/' wa_branch-city1
                     into wa_itens-exportador_cpl_end separated by space.
            endif.

            wa_itens-importador_codigo = lit_data_inbound-cab_importador.

            me->get_importador( changing is_it_due = wa_itens ).
*            wa_itens-importador_nome =
*            wa_itens-importador_country =
*            wa_itens-importador_cpl_end=
**campos acima preencher baseado na tratativa atual: LZDUEF01 linhas 1210 - 1240 utilizando : ZSDT0172-IMPORTADOR_CODIGO

            wa_itens-fatura_tp_codigo          = <fs_itens>-tpcodigo.
            wa_itens-fatura_motivo_dispensa_nf = <fs_itens>-dispensa_nf.
            wa_itens-matnr                     = <fs_itens>-cod_produto.

*            wa_itens-CODIGO_NCM                =
*  Conforme tratativa atual: verificar include LZDUEF01 linhas:1242 - 1291

            wa_itens-peso_liq_total            = <fs_itens>-peso_total_kg.
            wa_itens-preco_ton                 = <fs_itens>-preco_tonelada.
            wa_itens-vlr_local_embarque        = <fs_itens>-vlr_local_embarque.
            wa_itens-vlr_cond_venda            = <fs_itens>-vlr_cond_venda.

            me->get_qtd_un_medida( changing is_it_due = wa_itens ).

*            wa_itens                           = <fs_itens>-qtd_estatistica.    " Analisar fazem parte log abaixo ou vem
*            wa_itens                           = <fs_itens>-qtd_comercializada. " Analisar fazem parte log abaixo ou vem

*            wa_itens-vlr_local_embarque =
*            wa_itens-vlr_cond_venda =
*            wa_itens-qtde_ue_exportada =
*            wa_itens-ue_exportada =
*            wa_itens-qtde_uc_exportada =
*            wa_itens-uc_exportada=
**Verificar regras de tratativa de quantidade e unidade de medida no include: LZDUEF01 linhas 1293 -- 1466 Exemplo:

            wa_itens-codigo_cond_venda = <fs_itens>-item_cond_venda.
            wa_itens-codigo_enquadramento = <fs_itens>-cod_enquadramento.

* Países
            loop at <fs_itens>-paises[] assigning field-symbol(<fs_paises>).

              wa_paises-id_due_item                = wa_itens-id_due_item.
              wa_paises-destino_country            = <fs_paises>-pais.
              wa_paises-peso_liq_total             = <fs_paises>-peso_liq_total.
              wa_paises-destino_qtde_ue_exportada  = <fs_paises>-qtde_exportada.
              wa_paises-ue_exportada               = <fs_paises>-ue_exp.
              append wa_paises to lit_paises.
              clear wa_paises.

            endloop.

* Precisa info paises para itens?

*** US #182074 - MMSILVA - 25.06.2025 - Ini ***
            loop at <fs_itens>-lpco[] assigning field-symbol(<fs_lpco>).

              wa_lpco-id_due        = wa_itens-id_due.
              wa_lpco-id_due_item   = wa_itens-id_due_item.
              wa_lpco-nr_lpco       = <fs_lpco>-nr_lpco.

              append wa_lpco to lit_lpco.
              clear wa_lpco.

            endloop.
*** US #182074 - MMSILVA - 25.06.2025 - Fim ***

            append wa_itens to lit_itens.
            clear wa_itens.

          endloop.

        endif.

        if lit_cab[] is initial.
          append les_cab to lit_cab.
          data(lv_single) = abap_true.
        endif.

        if lit_cab[] is not initial and lit_itens is not initial. " Prevendo possibilidade em carga em massa
          sort lit_cab   by id_due.
          sort lit_itens by id_due.
          sort lit_paises by id_due id_due_item.
          sort lit_lpco   by id_due id_due_item. "US #182074 - MMSILVA - 25.06.2025

          loop at lit_cab[] assigning field-symbol(<lwa_data_due_ant_cab>).

            lo_due->set_cabecalho( i_zsdt0170 = <lwa_data_due_ant_cab> ).

            if lv_single is not initial.

              loop at lit_itens[] assigning field-symbol(<lwa_data_due_ant_itens>).

                lo_due->add_item( i_zsdt0172 = <lwa_data_due_ant_itens> ).

                if lit_paises is not initial.
                  loop at lit_paises[] assigning field-symbol(<lwa_data_due_ant_paises>).
                    lo_due->add_item_pais_destino( i_zsdt0174 = <lwa_data_due_ant_paises> ).
                  endloop.
                endif.

*** US #182074 - MMSILVA - 25.06.2025 - Ini ***
                if lit_lpco is not initial.
                  loop at lit_lpco[] assigning field-symbol(<lwa_data_due_ant_lpco>).
                    lo_due->add_item_lpco( i_zsdt0190 = <lwa_data_due_ant_lpco> ).
                  endloop.
                endif.
*** US #182074 - MMSILVA - 25.06.2025 - Fim ***
              endloop.

            else. "Estr. Complex

              read table lit_itens[] assigning <lwa_data_due_ant_itens>
                                     with key id_due = <lwa_data_due_ant_cab>-id_due
                                     binary search.
              if sy-subrc is initial.
                loop at lit_itens[] assigning <lwa_data_due_ant_itens> from sy-tabix.
                  if <lwa_data_due_ant_itens>-id_due eq <lwa_data_due_ant_cab>-id_due.
                    lo_due->add_item( i_zsdt0172 = <lwa_data_due_ant_itens> ).

                    read table lit_paises[] assigning <lwa_data_due_ant_paises>
                                           with key id_due = <lwa_data_due_ant_cab>-id_due
                                                    id_due_item = <lwa_data_due_ant_itens>-id_due_item
                                           binary search.
                    if sy-subrc is initial.
                      loop at lit_paises[] assigning <lwa_data_due_ant_paises> from sy-tabix.
                        if <lwa_data_due_ant_itens>-id_due eq <lwa_data_due_ant_cab>-id_due
                        and <lwa_data_due_ant_itens>-id_due_item eq <lwa_data_due_ant_paises>-id_due_item.
                          lo_due->add_item_pais_destino( i_zsdt0174 = <lwa_data_due_ant_paises> ).
                        else.
                          exit.
                        endif.
                      endloop.
                    endif.

*** US #182074 - MMSILVA - 25.06.2025 - Ini ***
                    read table lit_lpco[] assigning <lwa_data_due_ant_lpco>
                                           with key id_due = <lwa_data_due_ant_cab>-id_due
                                                    id_due_item = <lwa_data_due_ant_itens>-id_due_item
                                           binary search.
                    if sy-subrc is initial.
                      loop at lit_lpco[] assigning <lwa_data_due_ant_lpco> from sy-tabix.
                        if <lwa_data_due_ant_itens>-id_due eq <lwa_data_due_ant_cab>-id_due
                        and <lwa_data_due_ant_itens>-id_due_item eq <lwa_data_due_ant_lpco>-id_due_item.
                          lo_due->add_item_lpco( i_zsdt0190 = <lwa_data_due_ant_lpco> ).
                        else.
                          exit.
                        endif.
                      endloop.
                    endif.
*** US #182074 - MMSILVA - 25.06.2025 - Fim ***

                  else.
                    exit.
                  endif.
                endloop.
              endif.
            endif.
          endloop.

          try .
              data: lv_id_due(10) type n.
*              DATA(lo_due_ob) = NEW zcl_int_ob_sd_due_antecipada( ).
              data(r_gravou) = lo_due->gravar_registro( importing e_id_due = lv_id_due
                                                                  e_zsdt0170 = les_cab ).

              if r_gravou is not initial.
                me->at_id_due = lv_id_due.

* Retorno das due's criadas...?
* ZIF_INTEGRACAO_OUTBOUND " Analisar..
*                lo_due_ob->zif_integracao_outbound~get_instance( )->execute_request( i_info_request = les_cab ).

*                DATA(lv_e_sucesso)   = abap_true.
*                DATA(lv_e_nm_code)      = '200'.
*                DATA(lv_e_msg_erro)     = 'Ok'.
*                DATA(lv_e_msg_outbound) = ' { "id_due" : "'         && lv_id_due     &&  '" ,' && cl_abap_char_utilities=>newline &&
*                                          '   "status_code" : "'    && lv_e_nm_code  &&  '" '  && cl_abap_char_utilities=>newline &&
*                                          ' }'.
*
*                me->zif_integracao_inject~set_integrar_inbound( EXPORTING  e_sucesso      = lv_e_sucesso
*                                                                           e_nm_code      = lv_e_nm_code
*                                                                           e_msg_erro     = lv_e_msg_erro
*                                                                           e_msg_outbound = lv_e_msg_outbound ).
*
*              ELSE.


              endif.

            catch zcx_due into data(ex_due). " Classe de Erro DU-e
              ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
*              r_msg_erro = |{ 'Erro na gravação e publicação da Due:'(e03) }| & |{ 'Erro ZCL_*DUE_ANTECIPADA!'(e02) }|.
              if ex_due->get_longtext( ) is not initial.
                r_msg_erro = ex_due->get_longtext( ).
              else.
                r_msg_erro = ex_due->get_text( ).
              endif.
          endtry.

        endif.
      catch cx_root into ox_err.
        r_msg_erro = |{ ox_err->get_text( ) }| & |{ 'Erro ZCL_*DUE_ANTECIPADA!'(e02) }|.
*        lo_due_ob->zif_integracao_outbound~get_instance( )->execute_request( i_info_request = r_msg_erro ).
    endtry.

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

    DATA: lit_data_inbound  TYPE zsde015,
          lit_data_inbounds TYPE string,
          lva_operacao      TYPE string,
          lva_ds_erro       TYPE string.


    r_if_integracao_inject = me.
    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lit_data_inbound ).
    ENDIF.

    e_msg_erro = me->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound  ).
    IF e_msg_erro IS NOT INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

    DATA(_change_bd) = abap_false.

*    LOOP AT lit_data_inbound INTO DATA(lwa_data_inbound).

    CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
      WHEN 'POST'. "Inclusão/Modificação

        SELECT SINGLE *
          FROM zsdt0170 INTO @DATA(lwa_zsdt0170)
         WHERE id_due EQ @me->at_id_due.

*        IF sy-subrc EQ 0.
*          lit_data_inbound-dt_registro = lwa_zsdt0170-dt_registro.
*          lit_data_inbound-hr_registro = lwa_zsdt0170-hr_registro.
*          lit_data_inbound-dt_update   = sy-datum.
*          lit_data_inbound-hr_update   = sy-uzeit.
*        ELSE.
*          lit_data_inbound-dt_registro = sy-datum.
*          lit_data_inbound-hr_registro = sy-uzeit.
*        ENDIF.
*
*        lwa_data_inbound-id_integracao = me->zif_int_ib_sd_due_antecipada~at_id_integracao.  "Set Protocolo Integração
*        MODIFY zsdt0001od FROM lwa_data_inbound.

        IF sy-subrc EQ 0.
          _change_bd = abap_true.
        ELSE.
          e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
          EXIT.
        ENDIF.

      WHEN 'DELETE'.

        DELETE FROM zsdt0170 WHERE id_due EQ me->at_id_due.

        SELECT SINGLE *
          FROM zsdt0170 INTO lwa_zsdt0170
         WHERE id_due EQ me->at_id_due.

        IF sy-subrc NE 0.
          _change_bd = abap_true.
        ELSE.
          e_msg_erro = 'Houve um erro ao excluir o registro no banco de dados!'.
          EXIT.
        ENDIF.

      WHEN OTHERS.
        e_msg_erro = 'Operação não prevista!'.
        EXIT.
    ENDCASE.

    IF e_msg_erro IS NOT INITIAL.
      EXIT.
    ENDIF.

*    ENDLOOP.

    IF ( _change_bd = abap_true ) AND ( e_msg_erro IS INITIAL ).

      COMMIT WORK.

      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.
      e_msg_outbound = ' { "Id_Due" : "'   && me->at_id_due &&  '" ,'   && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code            &&  '" '  && cl_abap_char_utilities=>newline &&
                       ' }'.
    ELSE.

      ROLLBACK WORK.

      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
    ENDIF.

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
