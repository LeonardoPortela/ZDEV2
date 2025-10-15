class ZCL_HCM_RSDATA_TGG_CPI definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_insert_empresa .
        INCLUDE TYPE zhcms_rsdata_empresa_s.
    TYPES: END OF ty_insert_empresa .
  types:
    BEGIN OF ty_get_func.
        INCLUDE TYPE zhcms_rsdata_func_emp_s.
    TYPES: END OF ty_get_func .
  types:
    BEGIN OF ty_saida_func.
        INCLUDE TYPE   zhcms_rsdata_func_emp_get_s.
    TYPES: END OF ty_saida_func .
  types:
    ty_rg_pernr TYPE RANGE OF pa0001-pernr .
  types:
    ty_rg_bukrs TYPE RANGE OF pa0001-bukrs .
  types:
    ty_rg_werks TYPE RANGE OF pa0001-werks .
  types:
    ty_rg_orgeh TYPE RANGE OF pa0001-orgeh .
  types:
    ty_rg_stell TYPE RANGE OF pa0001-stell .
  types:
    ty_rg_plans TYPE RANGE OF pa0001-plans .
  types:
    BEGIN OF ty_dados_funcionario,
        pernr TYPE pa0001-pernr,
        endda TYPE pa0001-endda,
        begda TYPE pa0001-begda,
        bukrs TYPE pa0001-bukrs,
        werks TYPE pa0001-werks,
        stell TYPE pa0001-stell,
        persk TYPE pa0001-persk,
        abkrs TYPE pa0001-abkrs,
        orgeh TYPE pa0001-orgeh,
        plans TYPE pa0001-plans,
        stat2 TYPE pa0000-stat2,
        cname TYPE pa0002-cname,
        nachn TYPE pa0002-nachn,
        vorna TYPE pa0002-vorna,
        gesch TYPE pa0002-gesch,
        famst TYPE pa0002-famst,
        gbdat TYPE pa0002-gbdat,
      END OF ty_dados_funcionario .
  types:
    BEGIN OF ty_dados_empresa,
        bukrs      TYPE j_1bbranch-bukrs,
        werks      TYPE j_1bbranch-branch,
        name       TYPE j_1bbranch-name,
        cgc_branch TYPE j_1bbranch-cgc_branch,
        state_insc TYPE j_1bbranch-state_insc,
        adrnr      TYPE j_1bbranch-adrnr,
        stcd1      TYPE j_1bbranch-stcd1,
        name_adr   TYPE adrc-name1,
        city1      TYPE adrc-city1,
        bairro     TYPE adrc-city2,
        post_code1 TYPE adrc-post_code1,
        street     TYPE adrc-street,
        house_num1 TYPE adrc-house_num1,
        uf         TYPE adrc-region,
        tel_number TYPE adrc-tel_number,
        cnae       TYPE t7brae-econi,
      END OF ty_dados_empresa .
  types:
    ty_t_dados_empresa     TYPE TABLE OF ty_dados_empresa .
  types:
    ty_t_dados_funcionario TYPE TABLE OF ty_dados_funcionario .

  class-data AT_IT_DADOS_EMPRESA type TY_T_DADOS_EMPRESA .
  class-data AT_IT_DADOS_EMPREGADO type TY_T_DADOS_FUNCIONARIO .
  class-data AT_RG_PERNR type TY_RG_PERNR .
  class-data AT_DATA_BASE type BEGDA .
  class-data AT_BUKRS type ZRSDSSELOPTS .

  methods GET_LISTA_PERNR
    importing
      value(R_BUKRS) type TY_RG_BUKRS optional
      value(R_WERKS) type TY_RG_WERKS optional
      value(R_PERNR) type TY_RG_PERNR optional
    returning
      value(IT_FUNCIONARIOS) type ZHCM_T_FUNCIONARIOS_LIST .
  methods SET_HEADER_SECURITY
    importing
      value(I_SERVICE_NAME) type CHAR50
    changing
      value(CL_WEB_EMPREGADO) type ref to ZRSDATACO_EMPREGADO_PORT optional
      value(CL_WEB_EMPRESA) type ref to ZRSEMPRESACO_EMPRESA_PORT optional
      value(CL_WEB_MOVIMENTO) type ref to ZRSDATAMOVCO_MOVIMENTO_EMPREGA optional .
  methods INICIA_CLASSE_WEBSERVICE
    importing
      value(I_PORT_NAME) type PRX_LOGICAL_PORT_NAME default 'ZRSHR_TGG'
      value(I_SERVICE_NAME) type CHAR50
    changing
      value(CL_WEB_EMPREGADO) type ref to ZRSDATACO_EMPREGADO_PORT optional
      value(CL_WEB_EMPRESA) type ref to ZRSEMPRESACO_EMPRESA_PORT optional
      value(CL_WEB_MOVIMENTO) type ref to ZRSDATAMOVCO_MOVIMENTO_EMPREGA optional .
  methods GET_DADOS_EMPREGADOS
    importing
      value(IT_FUNCIONARIOS) type ZHCM_T_FUNCIONARIOS_LIST
      value(I_INSERT_EMPREGADOS) type CHAR1 optional
    returning
      value(I_REQUEST) type ZRSDATAINSERT_EMPREGADOS_REQUE .
  methods INSERT_EMPRESA .
  methods INSERT_FERIAS .
  methods INSERT_EMPREGADOS_TURNOS .
  methods INSERT_ABSENTEISMO .
  methods TRANSFER_EMPREGADOS
    importing
      value(R_PERNR) type TY_RG_PERNR optional
    exporting
      value(E_PERNR) type TY_RG_PERNR .
  methods INSERT_EMPREGADOS
    importing
      value(I_REQUEST) type ZRSDATAINSERT_EMPREGADOS_REQUE
      value(I_REQUEST_CPF) type ZRSDATAINSERT_EMPREGADOS_REQUE
      value(I_REQUEST_INS) type ZRSDATAINSERT_EMPREGADOS_REQUE .
  methods INSERT_FERIAS_RETROATIVO .
  methods INSERT_ABSENTEISMO_RETROATIVO .
  methods GET_LISTA_PERNR_RETROATIVO
    importing
      value(R_BUKRS) type TY_RG_BUKRS optional
      value(R_WERKS) type TY_RG_WERKS optional
      value(R_PERNR) type TY_RG_PERNR optional
    returning
      value(IT_FUNCIONARIOS) type ZHCM_T_FUNCIONARIOS_LIST .
  methods GET_CNPJ_WERKS
    importing
      !I_BUKRS type BUKRS
      !I_WERKS type J_1BBRANC_
    returning
      value(E_CNPJ) type J_1BCGC .
  methods GET_OUT_EMPREGADOS
    importing
      value(I_REQUEST) type ZRSDATAINSERT_EMPREGADOS_REQUE
    exporting
      value(I_REQUEST_INS) type ZRSDATAINSERT_EMPREGADOS_REQUE
      value(I_REQUEST_CPF) type ZRSDATAINSERT_EMPREGADOS_REQUE .
  methods DELETE_EMPREGADOS
    importing
      !I_DELETE type ZHCMS_RSDATA_EMPREG_RES_GET
    exporting
      !E_PERNR type TY_RG_PERNR .
  methods DELETE_EMPREGADOS_GET
    importing
      value(I_REQUEST_DEL) type ZHCMS_RSDATA_EMPREG_RES_GET
    exporting
      value(E_PERNR) type TY_RG_PERNR .
  methods DELETE_ABSENTEISMO .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_HCM_RSDATA_TGG_CPI IMPLEMENTATION.


  METHOD delete_absenteismo.
** US - 156381 - CBRAND - Inicio
** Necessário comentar devido alterações feitas na tabela de LOG
*
*    DATA:
*      s_insert_absenteismo_req TYPE zrsdatamovinsert_absenteismo_1,
*      zcl_proxy                TYPE REF TO zrsdatamovco_movimento_emprega,
*      it_log                   TYPE TABLE OF zhcmt_pa_0023,
*      v_data_retorno           TYPE endda,
*      v_cnpj                   TYPE char20,
*      v_ini_abs                TYPE char20,
*      v_fim_abs                TYPE char20,
*      v_prev_retorno           TYPE char20,
*      v_dias_afast             TYPE i.
*
*    DATA: s_data_json TYPE zhcms_rsdata_empabsent_service,
*          e_json      TYPE string.
*
*    DATA: lit_ret_ferias TYPE zhcms_rsdata_emp_ferias_resp.
*    DATA: lwa_service TYPE zde_rs_data_cpi.
*
*    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
*    CREATE OBJECT obj_rsdata_cpi.
*
*    Absenteísmos/Ausências 2001
*    SELECT
*            p2001~pernr,
*            p2001~subty,
*            p2001~awart,
*            p2001~begda,
*            p2001~endda,
*            p2001~aedtm,
*            p2001~docnr,
*            p1~bukrs,
*            p1~werks,
*            t54t~atext
*      FROM zpa2001_log_del AS p2001
*      LEFT JOIN t554t AS t54t
*      ON t54t~awart = p2001~awart
*      LEFT JOIN pa0001 AS p1
*      ON p1~pernr = p2001~pernr
*      INTO TABLE @DATA(it_afastamentos)
*      WHERE p2001~pernr IN @me->at_rg_pernr[] AND
*            p2001~data_del >= @me->at_data_base AND
*            p2001~awart NOT IN ( '0100', '0110', '0120', '0901', '0902', '0907', '0908', '0910', '0911', '0954', '0955', '0956', '0965' ) AND
*            p1~begda <= @me->at_data_base AND
*            p1~endda >= @me->at_data_base AND
*            t54t~sprsl  = @sy-langu   AND
*            t54t~moabw  = '37'.
*
*    SORT it_afastamentos[] BY pernr ASCENDING.
*
*    LOOP AT it_afastamentos[] INTO DATA(w_afast)
*        GROUP BY ( werks = w_afast-werks
*                               size       = GROUP SIZE
*                               index      = GROUP INDEX ) ASCENDING
*                REFERENCE INTO DATA(group_werks).
*
*      ->Criando estrutura para envio de ausências
*      APPEND INITIAL LINE TO s_data_json-movimentoservice-dados[] ASSIGNING FIELD-SYMBOL(<w_empresa>).
*
*      TRY.
*          DATA(w_detalhe_emp) = me->at_it_dados_empresa[ werks = group_werks->werks ].
*        CATCH cx_sy_itab_line_not_found.
*      ENDTRY.
*
*      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
*        EXPORTING
*          input  = w_detalhe_emp-stcd1
*        IMPORTING
*          output = v_cnpj.
*
*      <w_empresa>-nrcnpjempresa = v_cnpj+0(18).
*
*      LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.
*
*        -> Acrescenta dados das férias encontradas para o funcionário
*        APPEND INITIAL LINE TO <w_empresa>-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).
*
*        <w_empregado>-matricula = <w_func>-pernr.
*
*        APPEND INITIAL LINE TO <w_empregado>-absenteismos ASSIGNING FIELD-SYMBOL(<w_afast>).
*
*        IF ( <w_func>-endda <> '99991231' ).
*          v_data_retorno = <w_func>-endda + 1.
*          WRITE v_data_retorno TO  v_prev_retorno DD/MM/YYYY.
*          v_dias_afast = v_data_retorno - <w_func>-begda.
*        ELSE.
*          v_dias_afast = 0.
*        ENDIF.
*
*        WRITE <w_func>-begda TO  v_ini_abs DD/MM/YYYY.
*        WRITE <w_func>-endda TO  v_fim_abs DD/MM/YYYY.
*
*        REPLACE ALL OCCURRENCES OF '.' IN v_ini_abs WITH '/'.
*        REPLACE ALL OCCURRENCES OF '.' IN v_fim_abs WITH '/'.
*        REPLACE ALL OCCURRENCES OF '.' IN v_prev_retorno WITH '/'.
*
*
*        <w_afast>-data = v_ini_abs.
*        <w_afast>-hora = '00:00'.
*        <w_afast>-dataprevretorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ).
*        <w_afast>-prevafastamento = v_dias_afast.
*        <w_afast>-tpprevafastamento = '2'.
*        <w_afast>-dataretorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ). "V_FIM_ABS.
*        <w_afast>-tempoafastamento = v_dias_afast.
*        <w_afast>-tpafastamento = '2'.
*        <w_afast>-motivoafastamento = <w_func>-atext.
*        <w_afast>-observacoes = <w_func>-atext.
*        <w_afast>-idabsenteismo = <w_func>-docnr.
*        <w_afast>-excluir = '1'.
*
*        CLEAR: v_dias_afast, v_cnpj, v_data_retorno.
*
*      ENDLOOP.
*    ENDLOOP.
*
*    DELETE s_data_json-movimentoservice-dados[] WHERE empregado IS INITIAL.
*
*    IF s_data_json-movimentoservice-dados[] IS NOT INITIAL.
*
*      s_data_json-movimentoservice-usernametoken-username = 'integracao.db0801amaggiexp@rsdata.com.br'.
*      s_data_json-movimentoservice-usernametoken-password = '43a19edaac7b63358c252c544be06c49'.
*      s_data_json-movimentoservice-config-metodo          = 'insertAbsenteismo'.
*
*      e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
*                                                  pretty_name = /ui2/cl_json=>pretty_mode-low_case
*                                                 ).
*
*      lwa_service-service = 'MovimentoService'.
*      lwa_service-json    = e_json.
*      lwa_service-tipo    = 'DeleteAbsenteismoTGG'.
*
*
*      -> Envio das informações
*** Chamar classe RSDATA que envia os dados.
*      TRY .
*          obj_rsdata_cpi->zif_integracao_outbound~get_instance(
*          )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
*          IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
*            DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.
*
*            /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_ferias ).
*
*            LOOP AT lit_ret_ferias-insertferiasresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
*              READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.
*
*            try.
*                data(w_absenteismo) = w_empregados-absenteismos-absenteismo[ 1 ].
*              catch cx_sy_itab_line_not_found.
*            endtry.
*
*              DATA(w_log) = VALUE zhcmt_pa_0023(
*                  servico         = 'Delete_Absenteismo_TGG'
*                  data_envio      = sy-datum
*                  hora_envio      = sy-uzeit
*                  pernr           = conv #( w_empregados-matricula )
*                  bukrs           = ''
*                  werks           = '' ).
*              dados_enviados  = |{ w_absenteismo-motivo_afastamento } Retorno:{ w_absenteismo-data_prev_retorno }| ).
*
*              LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
*                w_log-retorno_servico = w_mensagem-txdescricao.
*                w_log-id_log = sy-tabix.
*                APPEND w_log TO it_log[].
*              ENDLOOP.
*
*            ENDLOOP.
*          ENDIF.
*
*        CATCH zcx_integracao.
*          CLEAR: w_log.
*          w_log = VALUE zhcmt_pa_0023(
*          servico         = 'Delete_Absenteismo_TGG'
*          data_envio      = sy-datum
*          hora_envio      = sy-uzeit
*          pernr           = <lfs_trans_emp>-nr_matricula
*          retorno_servico = 'Erro Delete Absenteismo TGG'  ).
*
*          APPEND w_log TO it_log[].
*        CATCH zcx_error.
*          CLEAR: w_log.
*          w_log = VALUE zhcmt_pa_0023(
*          servico         = 'Delete_Absenteismo_TGG'
*          data_envio      = sy-datum
*          hora_envio      = sy-uzeit
*          pernr           = <lfs_trans_emp>-nr_matricula
*          retorno_servico = 'Erro Delete Absenteismo TGG'  ).
*
*          APPEND w_log TO it_log[].
*      ENDTRY.
*
*      IF ( it_log[] IS NOT INITIAL ).
*        MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
*      ENDIF.
*
*    ENDIF.
** US - 156381 - CBRAND - Inicio
** Necessário comentar devido alterações feitas na tabela de LOG
ENDMETHOD.


  METHOD delete_empregados.

    TYPES: ty_r_bukrs TYPE RANGE OF pa0001-bukrs,
           ty_r_werks TYPE RANGE OF pa0001-werks.

    DATA: it_log          TYPE TABLE OF zhcmt_pa_0023,
          s_pernr         LIKE LINE OF e_pernr,
          "s_data_json     type zhcms_rsdata_empreg_insert,
          lit_ret_emp_del TYPE 	zhcms_rsdata_empregados_resp,
          e_json          TYPE string.

    DATA: s_data_json     TYPE zhcms_rsdata_empreg_service,
          lwa_delete_json TYPE zhcms_rsdata_func_emp_s,
          lit_ret_emp_ins TYPE 	zhcms_rsdata_empregados_resp.

    DATA: lwa_service TYPE zde_rs_data_cpi.
    DATA: lit_empregados_del TYPE  TABLE OF zhcms_rsdata_empreg_service.
    DATA: lit_del_dados TYPE zhcm_t_funcionarios_list."type zhcms_rsdata_empreg_res_get. ".TYPE ZHCM_T_FUNCIONARIOS_LIST


    DATA: v_cnpj           TYPE char20,
          v_paval          TYPE t001z-paval,
          v_cgc_branch     TYPE j_1bbranch-cgc_branch,
          v_cgc_number     TYPE j_1bwfield-cgc_number,
          v_cgc_company    TYPE j_1bwfield-cgc_compan,
          v_cgc_branch_aux TYPE j_1bwfield-cgc_branch,
          v_data_aux       TYPE char20.

    DATA(v_data_base) = sy-datum.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    " me->get_lista_pernr_excl( ) ." zcl_rsdata->get_lista_pernr_excl( r_pernr = lrg_pernr[] ) .

    SELECT *
      FROM zhcmt0010
    INTO TABLE @DATA(lit_func)
      WHERE data_exclusao = @sy-datum.


    IF lit_func IS NOT INITIAL.

      "-> Cria range de empresas encontradas
      DATA(rg_bukrs) = VALUE ty_r_bukrs( FOR _line IN lit_func[] (
                                          sign   = 'I'
                                          option = 'EQ'
                                          low    = _line-bukrs
                                          high    = _line-bukrs
                                      ) ).

      SORT rg_bukrs[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM rg_bukrs COMPARING low.

      DATA(rg_werks) = VALUE ty_r_werks( FOR _line IN lit_func[] (
                                          sign   = 'I'
                                          option = 'EQ'
                                          low    = _line-werks
                                          high    = _line-werks
                                      ) ).

      SELECT  j1~bukrs,
              j1~branch AS werks,
              j1~name,
              j1~cgc_branch,
              j1~state_insc,
              j1~adrnr,
              j1~stcd1,
              ad~name1 AS name_adr,
              ad~city1,
              ad~city2 AS bairro,
              ad~post_code1,
              ad~street,
              ad~house_num1,
              ad~region AS uf,
              ad~tel_number,
              tae~econi AS cnae
        FROM j_1bbranch AS j1
        LEFT JOIN adrc AS ad
        ON ad~addrnumber = j1~adrnr
        LEFT JOIN t7brb1 AS tb1
        ON tb1~bukrs = j1~bukrs
        AND tb1~filia = j1~branch
        LEFT JOIN t7brae AS tae
        ON tae~econa = tb1~econa
        INTO TABLE @DATA(it_dados_empresa)
        WHERE j1~bukrs IN @rg_bukrs[]
          AND j1~branch IN @rg_werks[]
          AND ad~date_to >= @v_data_base
          AND ad~langu = @sy-langu
          AND tb1~endda >= @v_data_base
          AND tae~endda >= @v_data_base.
      SORT it_dados_empresa[] BY bukrs werks ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_dados_empresa[] COMPARING bukrs werks.

      s_data_json-empregadoservice-usernametoken-username = ''."'integracao.db0801amaggiexp@rsdata.com.br'.
      s_data_json-empregadoservice-usernametoken-password = ''."'43a19edaac7b63358c252c544be06c49'.

      s_data_json-empregadoservice-config-tpverempregado  = 'MATRICULA'.
      s_data_json-empregadoservice-config-tpversetorcargo = 'NOME'.
      s_data_json-empregadoservice-config-metodo          = 'excluirEmpregados'.


      LOOP AT lit_func[] ASSIGNING FIELD-SYMBOL(<lfs_emp_delete>) .

        CLEAR: v_paval, v_cgc_branch, v_cgc_number, v_cgc_company, v_cgc_branch_aux, v_data_aux.
        SELECT SINGLE paval FROM t001z
           INTO @v_paval
           WHERE bukrs = @<lfs_emp_delete>-bukrs
           AND   party = 'J_1BCG'.

        SELECT SINGLE cgc_branch FROM j_1bbranch
           INTO @v_cgc_branch
           WHERE bukrs = @<lfs_emp_delete>-bukrs
           AND   branch = @<lfs_emp_delete>-werks.


        MOVE   v_paval       TO  v_cgc_company.
        MOVE   v_cgc_branch  TO  v_cgc_branch_aux.

        CALL FUNCTION 'J_1BBUILD_CGC'
          EXPORTING
            cgc_company = v_cgc_company
            cgc_branch  = v_cgc_branch_aux
          IMPORTING
            cgc_number  = v_cgc_number.


        lwa_delete_json-nrcnpjempresa = v_cgc_number.

        READ TABLE it_dados_empresa[] INTO DATA(w_matriz) WITH KEY werks = <lfs_emp_delete>-werks
                                                                   bukrs = <lfs_emp_delete>-bukrs.

        lwa_delete_json-razaosocialempresa                = w_matriz-name_adr.
        "lwa_delete_json-nrnit                             =  <lfs_emp_delete>-pis_nr.
        lwa_delete_json-nrcpf                             =  <lfs_emp_delete>-cpf_nr .
        lwa_delete_json-nrmatricula                       =  <lfs_emp_delete>-pernr. .
        lwa_delete_json-nomeempregado                     =  <lfs_emp_delete>-cname.

        WRITE <lfs_emp_delete>-hiredate TO v_data_aux DD/MM/YYYY.
        REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
        lwa_delete_json-dtadmissao = v_data_aux.

        APPEND lwa_delete_json TO s_data_json-empregadoservice-empregados[].
        CLEAR: w_matriz.


        CLEAR: e_json.
        e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                    pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                   ).

        lwa_service-service = 'EmpregadoService'.
        lwa_service-json    = e_json.
        lwa_service-tipo    = 'excluirEmpregados_TGG'.

*** Busca os dados que estão no RS data
        TRY .
            obj_rsdata_cpi->zif_integracao_outbound~get_instance(
            )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
            IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
              DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.

              /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_emp_del ).


              "-> Gera Log
              LOOP AT lit_ret_emp_del-empregadosresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
                "
                DATA(lva_index) = sy-tabix.
                READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX lva_index.

                LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(lwa_mensagem).

                  DATA(w_log) = VALUE zhcmt_pa_0023(
                         servico          = 'insertEmpregadosDelete'
                          data_envio      = sy-datum
                          hora_envio      = sy-uzeit
                          pernr           = <lfs_emp_delete>-pernr
                          bukrs           = <lfs_emp_delete>-bukrs
                          werks           = <lfs_emp_delete>-werks
                         dados_enviados   = |{ <lfs_emp_delete>-pernr }{ <lfs_emp_delete>-cname }/{ <lfs_emp_delete>-bukrs }| ).


                  w_log-retorno_servico = lwa_mensagem-txdescricao.

                  APPEND w_log TO it_log[].
                  CLEAR: w_log.

                ENDLOOP.
              ENDLOOP.
            ELSE.
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'DeleteEmpregados'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_emp_delete>-pernr
              retorno_servico = 'Erro Delete Empregados - Classe'  ).

              APPEND w_log TO it_log[].

            ENDIF.
          CATCH zcx_integracao INTO DATA(r_msg).

            CLEAR: w_log.
            w_log = VALUE zhcmt_pa_0023(
            servico         = 'DeleteEmpregados'
            data_envio      = sy-datum
            hora_envio      = sy-uzeit
            pernr           = <lfs_emp_delete>-pernr
            retorno_servico = 'Erro Delete Empregados - Classe'  ).

            APPEND w_log TO it_log[].

**** ERRO remove a matricula para não enviar o INSERT
            s_pernr-option = 'EQ'.
            s_pernr-sign = 'I'.
            s_pernr-low = <lfs_emp_delete>-pernr.
            APPEND s_pernr TO e_pernr.
            CLEAR: s_pernr.

          CATCH zcx_error INTO DATA(r_msg_error).
            CLEAR: w_log.
            w_log = VALUE zhcmt_pa_0023(
            servico         = 'DeleteEmpregados'
            data_envio      = sy-datum
            hora_envio      = sy-uzeit
            pernr           = <lfs_emp_delete>-pernr
            retorno_servico = 'Erro Delete Empregados - Classe'  ).

            APPEND w_log TO it_log[].

**** ERRO remove a matricula para não enviar o INSERT
            s_pernr-option = 'EQ'.
            s_pernr-sign = 'I'.
            s_pernr-low = <lfs_emp_delete>-pernr.
            APPEND s_pernr TO e_pernr.
            CLEAR: s_pernr.
        ENDTRY.
        CLEAR: s_data_json-empregadoservice-empregados[], _lwa_ret_call, lva_json_retorno, lit_ret_emp_del.", w_setor_cargo.

        IF <lfs_emp_delete> IS ASSIGNED.
          CLEAR <lfs_emp_delete>.
        ENDIF.

        IF <lfs_emp_response> IS ASSIGNED.
          CLEAR <lfs_emp_response>.
        ENDIF.

        IF <lfs_w_mensagem_tab> IS ASSIGNED.
          CLEAR <lfs_w_mensagem_tab>.
        ENDIF.

      ENDLOOP.

      "-> Grava log do envio do serviço 'insertEmpregados':
      IF ( it_log[] IS NOT INITIAL ).
        MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD delete_empregados_get.
    DATA: zcl_proxy TYPE REF TO zrsdataco_empregado_port,
          it_log    TYPE TABLE OF zhcmt_pa_0023.

    DATA: s_data_json     TYPE zhcms_rsdata_empreg_service,
          lwa_insert_json TYPE zhcms_rsdata_func_emp_s,
          lit_ret_emp_ins TYPE 	zhcms_rsdata_empregados_resp,
          e_json          TYPE string.

    DATA: s_pernr LIKE LINE OF e_pernr.

    DATA: lwa_service TYPE zde_rs_data_cpi.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    IF i_request_del-empregadoservice-empregados[] IS NOT INITIAL.

      s_data_json-empregadoservice-usernametoken-username = ''."'integracao.db0801amaggiexp@rsdata.com.br'.
      s_data_json-empregadoservice-usernametoken-password = ''."'43a19edaac7b63358c252c544be06c49'.

      s_data_json-empregadoservice-config-tpverempregado  = 'MATRICULA'.
      s_data_json-empregadoservice-config-tpversetorcargo = 'NOME'.
      s_data_json-empregadoservice-config-metodo          = 'insertEmpregadosRequest'.

      LOOP AT i_request_del-empregadoservice-empregados  ASSIGNING FIELD-SYMBOL(<lfs_insert_emp>).

        "DATA(lva_index) = sy-tabix.

        lwa_insert_json-novocontrato                      =  <lfs_insert_emp>-novocontrato  .
        lwa_insert_json-dttransferencia                   =  <lfs_insert_emp>-dttransferencia .
        lwa_insert_json-idempresadestino                  =  <lfs_insert_emp>-idempresadestino .
        lwa_insert_json-statusempresadestino              =  <lfs_insert_emp>-statusempresadestino .
        lwa_insert_json-codintegracaoempresadestino       =  <lfs_insert_emp>-codintegracaoempresadestino .
        lwa_insert_json-nrcnpjempresadestino              =  <lfs_insert_emp>-nrcnpjempresadestino .
        lwa_insert_json-razaosocialempresadestino         =  <lfs_insert_emp>-razaosocialempresadestino .
        lwa_insert_json-denominacaoempresadestino         =  <lfs_insert_emp>-denominacaoempresadestino  .
        "lwa_insert_json-NRMATRICULADESTINO                =  <lfs_insert_emp>- .
        "lwa_insert_json-MATRICULARHDESTINO                =  <lfs_insert_emp>- .
        lwa_insert_json-forcartransferencia               =  <lfs_insert_emp>-forcartransferencia .
        lwa_insert_json-statusempresa                     =  <lfs_insert_emp>-statusempresa .
        lwa_insert_json-idempresa                         =  <lfs_insert_emp>-idempresa .
        lwa_insert_json-codintegracaoempresa              =  <lfs_insert_emp>-codintegracaoempresa .
        lwa_insert_json-nrcnpjempresa                     =  <lfs_insert_emp>-nrcnpjempresa .
        lwa_insert_json-razaosocialempresa                =  <lfs_insert_emp>-razaosocialempresa .
        lwa_insert_json-denominacaoempresa                =  <lfs_insert_emp>-denominacaoempresa  .
        lwa_insert_json-idempregado                       =  <lfs_insert_emp>-idempregado .
        lwa_insert_json-codintegracaoempregado            =  <lfs_insert_emp>-codintegracaoempregado .
        lwa_insert_json-nomeempregado                     =  <lfs_insert_emp>-nomeempregado .
        lwa_insert_json-carteiratrabalhodigital           =  <lfs_insert_emp>-carteiratrabalhodigital  .
        lwa_insert_json-dtnascimento                      =  <lfs_insert_emp>-dtnascimento  .
        lwa_insert_json-tpsexo                            =  <lfs_insert_emp>-tpsexo  .
        "lwa_insert_json-raca                              =  <lfs_insert_emp>- .
        lwa_insert_json-nrctps                            =  <lfs_insert_emp>-nrctps  .
        lwa_insert_json-nrseriectps                       =  <lfs_insert_emp>-nrseriectps  .
        lwa_insert_json-dtemictps                         =  <lfs_insert_emp>-dtemictps  .
        lwa_insert_json-ufemictps                         =  <lfs_insert_emp>-ufemictps  .
        lwa_insert_json-nridentidade                      =  <lfs_insert_emp>-nridentidade  .
        lwa_insert_json-orgaoexpedidorrg                  =  <lfs_insert_emp>-orgaoexpedidorrg .
        lwa_insert_json-dtemirg                           =  <lfs_insert_emp>-dtemirg  .
        lwa_insert_json-ufemirg                           =  <lfs_insert_emp>-ufemirg  .
        lwa_insert_json-nrnit                             =  <lfs_insert_emp>-nrnit .
        lwa_insert_json-nrcpf                             =  <lfs_insert_emp>-nrcpf .
        lwa_insert_json-nrmatricula                       =  <lfs_insert_emp>-nrmatricula .
        lwa_insert_json-categoriatrabalhador              =  <lfs_insert_emp>-categoriatrabalhador  .
        lwa_insert_json-tpvinculo                         =  <lfs_insert_emp>-tpvinculo .
        lwa_insert_json-br_pdh                            =  <lfs_insert_emp>-br_pdh .
        lwa_insert_json-regimerevezamento                 =  <lfs_insert_emp>-regimerevezamento .
        lwa_insert_json-dtadmissao                        =  <lfs_insert_emp>-dtadmissao  .
        lwa_insert_json-dtdemissao                        =  <lfs_insert_emp>-dtdemissao  .
        lwa_insert_json-txobs                             =  <lfs_insert_emp>-txobs .
        lwa_insert_json-enderecoempregado                 =  <lfs_insert_emp>-enderecoempregado .
        lwa_insert_json-cidadeempregado                   =  <lfs_insert_emp>-cidadeempregado .
        lwa_insert_json-cidadecodibge                     =  <lfs_insert_emp>-cidadecodibge  .
        lwa_insert_json-bairroempregado                   =  <lfs_insert_emp>-bairroempregado .
        lwa_insert_json-estadoempregado                   =  <lfs_insert_emp>-estadoempregado .
        lwa_insert_json-nrcep                             =  <lfs_insert_emp>-nrcep .
        lwa_insert_json-dddcelular                        =  <lfs_insert_emp>-dddcelular  .
        lwa_insert_json-nrcelular                         =  <lfs_insert_emp>-nrcelular .
        lwa_insert_json-dddtelefone                       =  <lfs_insert_emp>-dddtelefone .
        lwa_insert_json-nrtelefone                        =  <lfs_insert_emp>-nrtelefone  .
        lwa_insert_json-remuneracaomensal                 =  <lfs_insert_emp>-remuneracaomensal .
        lwa_insert_json-nomemae                           =  <lfs_insert_emp>-nomemae .
        lwa_insert_json-tpfilprevidencia                  =  <lfs_insert_emp>-tpfilprevidencia .
        lwa_insert_json-tpestadocivil                     =  <lfs_insert_emp>-tpestadocivil  .
        lwa_insert_json-tpaposentado                      =  <lfs_insert_emp>-tpaposentado  .
        lwa_insert_json-nreleitor                         =  <lfs_insert_emp>-nreleitor .
        lwa_insert_json-nrcnh                             =  <lfs_insert_emp>-nrcnh .
        lwa_insert_json-dtvalcnh                          =  <lfs_insert_emp>-dtvalcnh .
        lwa_insert_json-cdrfid                            =  <lfs_insert_emp>-cdrfid  .
        lwa_insert_json-cdbarras                          =  <lfs_insert_emp>-cdbarras  .
        lwa_insert_json-gruposanguineo                    =  <lfs_insert_emp>-gruposanguineo  .
        lwa_insert_json-deficiencia                       =  <lfs_insert_emp>-deficiencia  .
        lwa_insert_json-tpdeficiencia                     =  <lfs_insert_emp>-tpdeficiencia .
        lwa_insert_json-email                             =  <lfs_insert_emp>-email  .
        lwa_insert_json-anulado                           =  <lfs_insert_emp>-anulado  .
        lwa_insert_json-motivoexcluido                    =  <lfs_insert_emp>-motivoexcluido  .
        lwa_insert_json-rne                               =  <lfs_insert_emp>-rne  .
        lwa_insert_json-nacionalidade                     =  <lfs_insert_emp>-nacionalidade  .
        lwa_insert_json-pais                              =  <lfs_insert_emp>-pais .


        READ TABLE <lfs_insert_emp>-setorcargo ASSIGNING FIELD-SYMBOL(<lfs_setor_cargo>) INDEX 1.

        IF <lfs_setor_cargo> IS ASSIGNED.

          lwa_insert_json-setorcargo-tpmovimentacao               =    <lfs_setor_cargo>-tpmovimentacao.
          lwa_insert_json-setorcargo-dtinicio                     =    <lfs_setor_cargo>-dtinicio.
          lwa_insert_json-setorcargo-dtsaida                      =    <lfs_setor_cargo>-dtsaida.
          lwa_insert_json-setorcargo-idsetor                      =    <lfs_setor_cargo>-idsetor.
          lwa_insert_json-setorcargo-cdsetor                      =    <lfs_setor_cargo>-cdsetor.
          lwa_insert_json-setorcargo-nomesetor                    =    <lfs_setor_cargo>-nomesetor.
          lwa_insert_json-setorcargo-cdsetordesenvolvido          =    <lfs_setor_cargo>-cdsetordesenvolvido.
          lwa_insert_json-setorcargo-nomesetordesenvolvido        =    <lfs_setor_cargo>-nomesetordesenvolvido.
          "lwa_insert_json-setorcargo-idcargo                      =    <lfs_setor_cargo>-idcargo.
          lwa_insert_json-setorcargo-cdcargo                      =    <lfs_setor_cargo>-cdcargo.
          lwa_insert_json-setorcargo-nomecargo                    =    <lfs_setor_cargo>-nomecargo.
          lwa_insert_json-setorcargo-cdcargodesenvolvido          =    <lfs_setor_cargo>-cdcargodesenvolvido.
          lwa_insert_json-setorcargo-cargodesenvolvido            =    <lfs_setor_cargo>-cargodesenvolvido.
          lwa_insert_json-setorcargo-cargocbo                     =    <lfs_setor_cargo>-cargocbo.
          lwa_insert_json-setorcargo-descsumariacargo             =    <lfs_setor_cargo>-descsumariacargo.
          lwa_insert_json-setorcargo-descdetalhadacargo           =    <lfs_setor_cargo>-descdetalhadacargo.
          lwa_insert_json-setorcargo-cdposicaotrabalho            =    <lfs_setor_cargo>-cdposicaotrabalho.
          lwa_insert_json-setorcargo-nomeposicaotrabalho          =    <lfs_setor_cargo>-nomeposicaotrabalho.
          lwa_insert_json-setorcargo-descsumariaposicaotrabalho   =    <lfs_setor_cargo>-descsumariaposicaotrabalho.
          lwa_insert_json-setorcargo-descdetalhadaposicaotrabalho =    <lfs_setor_cargo>-descdetalhadaposicaotrabalho.

          APPEND lwa_insert_json TO s_data_json-empregadoservice-empregados[].
          CLEAR: lwa_insert_json.


          e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                      pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                     ).

          lwa_service-service = 'EmpregadoService'.
          lwa_service-json    = e_json.
          lwa_service-tipo    = 'DELETE_EMP_GET_TGG'.

          TRY .
              obj_rsdata_cpi->zif_integracao_outbound~get_instance(
              )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
              IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.

                DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.
                /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_emp_ins ).

                LOOP AT lit_ret_emp_ins-empregadosresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
                  "data(lva_index)  = sy-tabix.

                  READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.

                  TRY.
                      DATA(w_setor_cargo) = <lfs_insert_emp>-setorcargo[ 1 ].
                    CATCH cx_sy_itab_line_not_found.
                  ENDTRY.

                  DATA(w_log) = VALUE zhcmt_pa_0023(
                    servico         = 'InsertEmpregadosDelete'
                    data_envio      = sy-datum
                    hora_envio      = sy-uzeit
                    pernr           = <lfs_insert_emp>-nrmatricula
                    bukrs           = <lfs_insert_emp>-idempresa
                    werks           = <lfs_insert_emp>-idempresa
                    dados_enviados  = |{ w_setor_cargo-cdcargo }{ w_setor_cargo-cdposicaotrabalho }/{ w_setor_cargo-cdsetor }| ).

                  LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
                    w_log-retorno_servico = w_mensagem-txdescricao.
                    w_log-id_log = sy-tabix.
                    APPEND w_log TO it_log[].
                  ENDLOOP.

                  IF  lit_ret_emp_ins-empregadosresponse-cdmsg = '500'.
**** ERRO remove a matricula para não enviar o INSERT - Codigo de erro 500.
                    s_pernr-option = 'EQ'.
                    s_pernr-sign = 'I'.
                    s_pernr-low = <lfs_insert_emp>-nrmatricula.
                    APPEND s_pernr TO e_pernr.
                    CLEAR: s_pernr.
                  ENDIF.
                ENDLOOP.
              ELSE.
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                     servico         = 'InsertEmpregadosDelete'
                     data_envio      = sy-datum
                     hora_envio      = sy-uzeit
                     pernr           = <lfs_insert_emp>-nrmatricula
                     bukrs           = <lfs_insert_emp>-idempresa
                     werks           = <lfs_insert_emp>-idempresa
                     dados_enviados  = |{ w_setor_cargo-cdcargo }{ w_setor_cargo-cdposicaotrabalho }/{ w_setor_cargo-cdsetor }| ).


                w_log-retorno_servico = 'Erro Insert Empregados Delete - Classe'.
                w_log-id_log = sy-tabix.
                APPEND w_log TO it_log[].

**** ERRO remove a matricula para não enviar o INSERT
                s_pernr-option = 'EQ'.
                s_pernr-sign = 'I'.
                s_pernr-low = <lfs_insert_emp>-nrmatricula.
                APPEND s_pernr TO e_pernr.
                CLEAR: s_pernr.

              ENDIF.
            CATCH zcx_integracao INTO DATA(r_msg).

              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'InsertEmpregadosDelete'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_insert_emp>-nrmatricula
              retorno_servico = 'Erro Insert Delete Empregados'  ).

              APPEND w_log TO it_log[].

**** ERRO remove a matricula para não enviar o INSERT
              s_pernr-option = 'EQ'.
              s_pernr-sign = 'I'.
              s_pernr-low = <lfs_insert_emp>-nrmatricula.
              APPEND s_pernr TO e_pernr.
              CLEAR: s_pernr.


            CATCH zcx_error INTO DATA(r_msg_error).
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'InsertEmpregadosDelete'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_insert_emp>-nrmatricula
              retorno_servico = 'Erro Insert Delete Empregados'  ).

              APPEND w_log TO it_log[].

**** ERRO remove a matricula para não enviar o INSERT
              s_pernr-option = 'EQ'.
              s_pernr-sign = 'I'.
              s_pernr-low = <lfs_insert_emp>-nrmatricula.
              APPEND s_pernr TO e_pernr.
              CLEAR: s_pernr.

          ENDTRY.
          CLEAR: s_data_json-empregadoservice-empregados, w_setor_cargo, lit_ret_emp_ins, lva_json_retorno, _lwa_ret_call,e_json.

          IF <lfs_setor_cargo> IS ASSIGNED.
            CLEAR <lfs_setor_cargo>.
          ENDIF.

          IF <lfs_insert_emp> IS ASSIGNED.
            CLEAR <lfs_insert_emp>.
          ENDIF.

          IF <lfs_emp_response> IS ASSIGNED.
            CLEAR <lfs_emp_response>.
          ENDIF.

          IF <lfs_w_mensagem_tab> IS ASSIGNED.
            CLEAR <lfs_w_mensagem_tab>.
          ENDIF.

        ELSE.
          "BREAK-POINT.
        ENDIF.


      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CNPJ_WERKS.

    DATA: v_cnpj           TYPE char20,
          v_paval          TYPE t001z-paval,
          v_cgc_branch     TYPE j_1bbranch-cgc_branch,
          v_cgc_number     TYPE j_1bwfield-cgc_number,
          v_cgc_company    TYPE j_1bwfield-cgc_compan,
          v_cgc_branch_aux TYPE j_1bwfield-cgc_branch.

    CLEAR: v_paval, v_cgc_branch, v_cgc_number, v_cgc_company, v_cgc_branch_aux.
    SELECT SINGLE paval FROM t001z
       INTO @v_paval
       WHERE bukrs = @i_bukrs
       AND   party = 'J_1BCG'.

    SELECT SINGLE cgc_branch FROM j_1bbranch
       INTO @v_cgc_branch
       WHERE bukrs = @i_bukrs
       AND   branch = @i_werks.

    MOVE   v_paval       TO  v_cgc_company.
    MOVE   v_cgc_branch  TO  v_cgc_branch_aux.

    CALL FUNCTION 'J_1BBUILD_CGC'
      EXPORTING
        cgc_company = v_cgc_company
        cgc_branch  = v_cgc_branch_aux
      IMPORTING
        cgc_number  = e_cnpj.

  ENDMETHOD.


  METHOD get_dados_empregados.

    DATA: v_message_handler TYPE REF TO if_hrpa_message_handler,
          v_hire_date       TYPE sy-datum,
          v_fire_date       TYPE sy-datum,
          v_data_aux        TYPE char20,
          v_cnpj            TYPE char20,
          it_excl_pernr     TYPE ty_rg_pernr.

    CHECK ( me->at_it_dados_empregado[] IS NOT INITIAL ).

    DATA(lit_dados_empregado) = me->at_it_dados_empregado[].

**********************************************************************
***  Setor / Substituído por Área de Risco HRP9665
**********************************************************************
    DATA(r_plans) = VALUE ty_rg_plans( FOR w_ativos IN me->at_it_dados_empregado[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_ativos-plans )  ).
    SORT r_plans BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM r_plans COMPARING low.

    SELECT
      h65~objid,
      h65~begda,
      h65~endda,
      h65~setrisc,
      h65~stext
    FROM hrp9665 AS h65
    INTO TABLE @DATA(it_orgeh)
    WHERE
        h65~objid IN @r_plans[] AND
        h65~plvar = '01' AND
        h65~otype = 'S'  AND
        h65~endda >= @me->at_data_base AND
        h65~begda <= @me->at_data_base AND
        h65~setrisc IS NOT NULL.

    SORT it_orgeh[] BY objid ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_orgeh[] COMPARING objid.

    "-> Criar range com posições que possuem setor de risco:
    DATA(r_plans_risco) = VALUE ty_rg_plans( FOR _plans IN it_orgeh[] (
        sign   = 'I'
        option = 'EQ'
        low    = _plans-objid
        high   = _plans-objid  ) ).
    DELETE ADJACENT DUPLICATES FROM r_plans_risco[] COMPARING low.
    SORT r_plans_risco[] BY low ASCENDING.


***** Caso não encotre o setor de risco. ( Admissão futura )
    DELETE lit_dados_empregado[]       WHERE werks IS INITIAL OR
                                             stell IS INITIAL OR
                                             plans IS INITIAL OR
                                             plans IN r_plans_risco[].

    DATA(r_plans_aux) = VALUE ty_rg_plans( FOR w_ativos IN lit_dados_empregado[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_ativos-plans )  ).
    SORT r_plans_aux BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM r_plans_aux COMPARING low.

    SELECT
      h65~objid,
      h65~begda,
      h65~endda,
      h65~setrisc,
      h65~stext
    FROM hrp9665 AS h65
    INTO TABLE @DATA(it_orgeh_aux)
    WHERE
        h65~objid IN @r_plans_aux[] AND
        h65~plvar = '01' AND
        h65~otype = 'S'  AND
        h65~endda >= @me->at_data_base AND
        h65~setrisc IS NOT NULL.

    SORT it_orgeh_aux[] BY objid ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_orgeh_aux[] COMPARING objid.

    DATA lwa_plans_risco LIKE LINE OF r_plans_risco.

    LOOP AT it_orgeh_aux INTO DATA(lwa_orgeh_aux).
      lwa_plans_risco-sign   = 'I'.
      lwa_plans_risco-option = 'EQ'.
      lwa_plans_risco-low    = lwa_orgeh_aux-objid.
      lwa_plans_risco-high   = lwa_orgeh_aux-objid.
      APPEND  lwa_plans_risco TO r_plans_risco.
      CLEAR: lwa_plans_risco.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM r_plans_risco COMPARING low.
    SORT r_plans_risco BY low ASCENDING.

    "-> Validando informações básicas para cadastro de risco
    "-> Somente enviar funcionários que tenham: Unidade, Cargo e Setor de Risco
    DELETE me->at_it_dados_empregado[] WHERE werks IS INITIAL OR
                                             stell IS INITIAL OR
                                             plans IS INITIAL OR
                                             plans NOT IN r_plans_risco[].

**********************************************************************
*** Fim da validação de setor de risco
**********************************************************************

    DATA(r_pernr) = VALUE ty_rg_pernr( FOR w_ativos IN me->at_it_dados_empregado[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_ativos-pernr )  ).
    DATA(r_bukrs) = VALUE ty_rg_bukrs( FOR w_ativos IN me->at_it_dados_empregado[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_ativos-bukrs )  ).
    DATA(r_werks) = VALUE ty_rg_werks( FOR w_ativos IN me->at_it_dados_empregado[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_ativos-werks )  ).
    DATA(r_stell) = VALUE ty_rg_stell( FOR w_ativos IN me->at_it_dados_empregado[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_ativos-stell )  ).
    DATA(r_orgeh) = VALUE ty_rg_orgeh( FOR w_ativos IN me->at_it_dados_empregado[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_ativos-orgeh )  ).
    SORT: r_pernr, r_bukrs, r_werks, r_stell, r_orgeh BY low ASCENDING.

    DELETE ADJACENT DUPLICATES FROM r_pernr COMPARING low.
    DELETE ADJACENT DUPLICATES FROM r_bukrs COMPARING low.
    DELETE ADJACENT DUPLICATES FROM r_werks COMPARING low.
    DELETE ADJACENT DUPLICATES FROM r_stell COMPARING low.
    DELETE ADJACENT DUPLICATES FROM r_orgeh COMPARING low.

    MOVE-CORRESPONDING r_pernr[] TO me->at_rg_pernr[].

    " -> Chama método para atualizar empresas no RSDATA
    me->insert_empresa( ). "PBI 6326 --  Retirar metodo de dentro de busca empregados

    IF ( i_insert_empregados = abap_false ).
      EXIT.
    ENDIF.


    "-> Documentos
    SELECT  p465~pernr,
            p465~subty,
            p465~endda,
            p465~tpdoc,
            p465~dt_emis,
            p465~es_emis,
            p465~cpf_nr,
            p465~ident_nr,
            p465~ident_org,
            p465~ctps_nr,
            p465~ctps_serie,
            p465~pis_nr,
            p465~doc_issuer,
            p465~doc_nr,
            p465~elec_nr,
            p465~drive_nr
      FROM pa0465 AS p465
      INTO TABLE @DATA(it_doc)
      WHERE p465~pernr IN @r_pernr[]
        AND p465~subty = p465~tpdoc
        AND p465~objps = ' '
        AND p465~sprps = ' '
        AND p465~endda >= @me->at_data_base
        AND p465~begda <= @me->at_data_base.
    SORT it_doc[] BY pernr subty ASCENDING.

    "-> Vínculos empregatícios
    SELECT pv~pernr,
           pv~empid,
           pv~cattr,
           t7~emtxt
        FROM pa0398 AS pv
        LEFT JOIN t7brmt AS t7
        ON t7~empid = pv~empid
        INTO TABLE @DATA(it_vinculo)
        WHERE pv~pernr IN @r_pernr[]
        AND pv~endda >= @me->at_data_base
        AND pv~begda <= @me->at_data_base
        AND t7~spras = @sy-langu
     ORDER BY pernr ASCENDING.

    "-> Endereço Pessoal
    SELECT  pernr,
            stras AS rua,
            ort01 AS cidade,
            ort02 AS bairro,
            pstlz AS cep,
            land1 AS cod_pais,
            state AS uf,
            num01 AS telefone
      FROM pa0006
      INTO TABLE @DATA(it_endereco)
      WHERE pernr IN @r_pernr[]
        AND endda >= @me->at_data_base
      ORDER BY pernr ASCENDING.

    "-> Deficiência
    SELECT  p4~pernr,
            p4~sbgru,
            p4~sbpro,
            p4~sbart,
            t54~sbtxt
      FROM pa0004 AS p4
      LEFT JOIN t543s AS t54
      ON t54~sbgru = p4~sbgru
      "LEFT JOIN T523T AS T23
      "ON P4~SBART = T23~SBART
      INTO TABLE @DATA(it_deficiencia)
      WHERE p4~pernr IN @r_pernr[]
        AND p4~endda >= @me->at_data_base
        AND p4~begda <= @me->at_data_base
        AND t54~sprsl = @sy-langu
     ORDER BY pernr ASCENDING.
***    SORT it_deficiencia[] BY pernr ASCENDING.


    "-> Filial
    SELECT persa, name1
      FROM t500p
      INTO TABLE @DATA(it_filial)
      WHERE persa IN @r_werks.
    SORT it_filial[] BY persa ASCENDING.

    "-> Cargo
    SELECT
      h0~objid,
      h0~stext
    FROM hrp1000 AS h0
    INTO TABLE @DATA(it_cargos)
    WHERE
      h0~plvar = '01'          AND
      h0~otype = 'C'           AND
      h0~objid IN @r_stell     AND
      h0~begda <= @me->at_data_base AND
      h0~endda >= @me->at_data_base AND
      h0~langu = @sy-langu.
    SORT it_cargos[] BY objid ASCENDING.

    "-> Posição
    SELECT
      h0~objid,
      h0~stext
    FROM hrp1000 AS h0
    INTO TABLE @DATA(it_posicoes)
    WHERE
      h0~plvar = '01'          AND
      h0~otype = 'S'           AND
      h0~objid IN @r_plans     AND
      h0~begda <= @me->at_data_base AND
      h0~endda >= @me->at_data_base AND
      h0~langu = @sy-langu.
    SORT it_posicoes[] BY objid ASCENDING.

    "-> Família
    SELECT  pernr,
            subty,
            endda,
            famsa,
            fasex,
            fcnam
      FROM pa0021
      INTO TABLE @DATA(it_familia)
      WHERE pernr IN @r_pernr[]
        AND endda >= @me->at_data_base
        AND famsa = '12'
     ORDER BY pernr ASCENDING.


    "-> Comunicação / e-mail
    SELECT  pernr,
            subty,
            usrid_long
      FROM pa0105
      INTO TABLE @DATA(it_email)
      WHERE pernr IN @r_pernr[]
        and subty = 'MAIL'
        AND endda >= @me->at_data_base
      ORDER BY pernr ASCENDING.

*    CBO
    SELECT * FROM t7brcb
        INTO TABLE @DATA(it_cbo)
        WHERE endda >= @me->at_data_base
        ORDER BY plans ASCENDING.


    "-> Validando informações básicas para cadastro de risco
    "-> Somente enviar funcionários que tenham: Unidade, Cargo e Setor de Risco
    DELETE me->at_it_dados_empregado[] WHERE werks IS INITIAL OR
                                             stell IS INITIAL OR
                                             plans IS INITIAL OR
                                             plans NOT IN r_plans_risco[].

    CLEAR: me->at_rg_pernr[].

    me->at_rg_pernr[] = VALUE ty_rg_pernr( FOR _pernr IN me->at_it_dados_empregado[] (
        sign   = 'I'
        option = 'EQ'
        low    = _pernr-pernr
        high   = _pernr-pernr ) ).


    IF ( i_insert_empregados = abap_true ).

      LOOP AT me->at_it_dados_empregado[] INTO DATA(w_func).

        i_request-rsdata-config-tp_ver_empregado = 'MATRICULA'.
        i_request-rsdata-config-tp_ver_setor_cargo = 'CODIGO'..

        APPEND INITIAL LINE TO i_request-rsdata-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).
        DATA(l_tabix) = sy-tabix.

        IF ( <w_empregado> IS ASSIGNED ).
          CLEAR: <w_empregado>.
        ENDIF.

        SORT me->at_it_dados_empresa[] BY werks ASCENDING.

        READ TABLE me->at_it_dados_empresa[] ASSIGNING FIELD-SYMBOL(<lfs_empresa>)
            WITH KEY werks = w_func-werks BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-nr_cnpjempresa = <lfs_empresa>-stcd1.
          <w_empregado>-razao_social_empresa = <lfs_empresa>-name.
          <w_empregado>-denominacao_empresa = <lfs_empresa>-name_adr.
        ENDIF.

        <w_empregado>-nr_matricula = w_func-pernr.
        <w_empregado>-nome_empregado = w_func-cname.
        WRITE w_func-gbdat TO v_data_aux DD/MM/YYYY.
        REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
        <w_empregado>-dt_nascimento = v_data_aux.
        <w_empregado>-tp_sexo = COND #( WHEN w_func-gesch = '1' THEN 'MASCULINO' ELSE 'FEMININO' ).

        READ TABLE it_doc[] ASSIGNING FIELD-SYMBOL(<lfs_doc>)
            WITH KEY pernr = w_func-pernr subty = '0003' BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-nr_ctps = <lfs_doc>-ctps_nr.
          <w_empregado>-nr_serie_ctps = <lfs_doc>-ctps_serie.
          DATA(v_dt_emis) = <lfs_doc>-dt_emis.
          <w_empregado>-uf_emi_ctps = <lfs_doc>-es_emis.

          CLEAR v_data_aux.
          WRITE v_dt_emis TO v_data_aux DD/MM/YYYY.
          REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
          <w_empregado>-dt_emi_ctps = v_data_aux.
        ENDIF.


        READ TABLE it_doc[] ASSIGNING <lfs_doc>
            WITH KEY pernr = w_func-pernr subty = '0002' BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-nr_identidade = <lfs_doc>-ident_nr.
          <w_empregado>-orgao_expedidor_rg = <lfs_doc>-doc_issuer.
          DATA(v_emissao_rg) = <lfs_doc>-dt_emis.
          <w_empregado>-uf_emi_rg = <lfs_doc>-es_emis.

          WRITE v_emissao_rg TO v_data_aux DD/MM/YYYY.
          REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
          <w_empregado>-dt_emi_rg = v_data_aux.
        ENDIF.

        READ TABLE it_doc[] ASSIGNING <lfs_doc>
            WITH KEY pernr = w_func-pernr subty = '0006' BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-nr_nit = <lfs_doc>-pis_nr.
        ENDIF.

        READ TABLE it_doc[] ASSIGNING <lfs_doc>
            WITH KEY pernr = w_func-pernr subty = '0001' BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-nr_cpf = <lfs_doc>-cpf_nr.
        ENDIF.

        READ TABLE it_doc[] ASSIGNING <lfs_doc>
            WITH KEY pernr = w_func-pernr subty = '0010' BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-nr_cnh = <lfs_doc>-drive_nr.
        ENDIF.

        READ TABLE it_doc[] ASSIGNING <lfs_doc>
            WITH KEY pernr = w_func-pernr subty = '0016' BINARY SEARCH.
* Inicio - Ajuste  - RMNI - CS1030728 - 11.10.2022
        IF ( sy-subrc = 0 ).
          IF NOT at_data_base IS INITIAL.
            IF <lfs_doc>-endda >= at_data_base.
              <w_empregado>-matricula_rh = <lfs_doc>-doc_nr.
            ENDIF.
          ELSE.
            IF <lfs_doc>-endda >= sy-datum.
              <w_empregado>-matricula_rh = <lfs_doc>-doc_nr.
            ENDIF.
          ENDIF.
* Final  - Ajuste  - RMNI - CS1030728 - 11.10.2022
        ENDIF.

        READ TABLE it_vinculo[] ASSIGNING FIELD-SYMBOL(<lfs_vinculo>)
            WITH KEY pernr = w_func-pernr BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-tp_vinculo = <lfs_vinculo>-emtxt.
          <w_empregado>-categoria_trabalhador = COND #(
           WHEN <lfs_vinculo>-empid = '01' THEN '101'
           WHEN <lfs_vinculo>-empid = '05' THEN '101'
           WHEN <lfs_vinculo>-empid = '09' THEN '101'
           WHEN <lfs_vinculo>-empid = '02' THEN '102'
           WHEN <lfs_vinculo>-empid = '04' THEN '901'
           WHEN <lfs_vinculo>-empid = '05' THEN '901'
           WHEN <lfs_vinculo>-empid = '06' THEN '723'
           WHEN <lfs_vinculo>-empid = '07' THEN '106'
           WHEN <lfs_vinculo>-empid = '12' THEN '103' ).
        ENDIF.

        "Data de admissão
        CALL FUNCTION 'HR_ECM_GET_HIRE_DATE'
          EXPORTING
            pernr           = w_func-pernr
            message_handler = v_message_handler
          IMPORTING
            hire_date       = v_hire_date.
        IF ( v_hire_date IS NOT INITIAL ).
          CLEAR v_data_aux.
          WRITE v_hire_date TO v_data_aux DD/MM/YYYY.
          REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
          <w_empregado>-dt_admissao = v_data_aux.
        ENDIF.

        "Data de demissão
        CLEAR v_fire_date.

        CALL FUNCTION 'RP_GET_FIRE_DATE'
          EXPORTING
            persnr   = w_func-pernr
          IMPORTING
            firedate = v_fire_date.
        IF ( v_fire_date IS NOT INITIAL ).
          "Quando for insertEmpregados, enviar demitidos somente dos últimos 3 dias:
          DATA(v_dt_demissao_limite) = me->at_data_base.
          "v_dt_demissao_limite = ( me->at_data_base - 3 ).
          v_dt_demissao_limite = |{ me->at_data_base(4) }{ me->at_data_base+4(2) }01|.
*-PBI 69293 - 02.12.2021 - JT - inicio
          v_dt_demissao_limite = v_dt_demissao_limite - 1.
*-PBI 69293 - 02.12.2021 - JT - fim

          SELECT SINGLE stat2 FROM pa0000
            INTO @DATA(lva_stat2)
            WHERE pernr = @w_func-pernr
              AND endda >= @me->at_data_base
              AND begda <= @me->at_data_base.

          IF ( i_insert_empregados = abap_true ) AND ( v_fire_date >= v_dt_demissao_limite
                                                   AND v_fire_date <= me->at_data_base ).
            "Envia demissão
            CLEAR v_data_aux.
            WRITE v_fire_date TO v_data_aux DD/MM/YYYY.
            REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
            <w_empregado>-dt_demissao = v_data_aux.

          ELSEIF ( i_insert_empregados = abap_true ) AND ( v_fire_date > me->at_data_base )
                                                     AND ( lva_stat2 = '3' ).
            "Não preenche o campo dt_demissao
          ELSE.
*-PBI 69293 - 02.12.2021 - JT - inicio
            CLEAR <w_empregado>.
            DELETE i_request-rsdata-empregados-empregado INDEX l_tabix.
*-PBI 69293 - 02.12.2021 - JT - fim
            CONTINUE.
            "Não envia matrícula para o RSdata
          ENDIF.
        ENDIF.

        READ TABLE it_endereco[] ASSIGNING FIELD-SYMBOL(<lfs_endereco>)
            WITH KEY pernr = w_func-pernr BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-endereco_empregado = <lfs_endereco>-rua.
          <w_empregado>-cidade_empregado = <lfs_endereco>-cidade.
          <w_empregado>-cidade_cod_ibge = ''.
          <w_empregado>-bairro_empregado = <lfs_endereco>-bairro.
          <w_empregado>-estado_empregado = <lfs_endereco>-uf.
          <w_empregado>-nr_cep = <lfs_endereco>-cep.
          <w_empregado>-nr_telefone = <lfs_endereco>-telefone.
        ENDIF.


        READ TABLE it_familia[] ASSIGNING FIELD-SYMBOL(<lfs_familia>)
            WITH KEY pernr = w_func-pernr BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-nome_mae = <lfs_familia>-fcnam.
        ENDIF.

        <w_empregado>-tp_fil_previdencia = 'EMPREGADO'.

        <w_empregado>-tp_estado_civil = COND #(
          WHEN w_func-famst = '0' THEN 'SOLTEIRO'
          WHEN w_func-famst = '1' THEN 'CASADO'
          WHEN w_func-famst = '2' THEN 'VIUVO'
          WHEN w_func-famst = '3' THEN 'SEPARADO'
          ELSE 'OUTRO' ).

        READ TABLE it_doc[] ASSIGNING <lfs_doc>
            WITH KEY pernr = w_func-pernr subty = '0005' BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-nr_eleitor = <lfs_doc>-elec_nr.
        ENDIF.

        READ TABLE it_deficiencia[] ASSIGNING FIELD-SYMBOL(<lfs_deficiencia>)
            WITH KEY pernr = w_func-pernr BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-deficiencia = <lfs_deficiencia>-sbtxt.
          <w_empregado>-tp_deficiencia = COND #(
            WHEN <w_empregado>-deficiencia IS INITIAL THEN 'NAO'
            ELSE 'SIM' ).
        ENDIF.


        READ TABLE it_email[] ASSIGNING FIELD-SYMBOL(<lfs_email>)
            WITH KEY pernr = w_func-pernr BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_empregado>-email = <lfs_email>-usrid_long.
        ENDIF.

        APPEND INITIAL LINE TO <w_empregado>-setores_cargos-setor_cargo[] ASSIGNING FIELD-SYMBOL(<w_setor>).

        TRY.
            <w_setor>-cd_setor = it_orgeh[ objid = w_func-plans ]-setrisc. "W_FUNC-ORGEH.
            <w_setor>-nome_setor = it_orgeh[ objid = w_func-plans ]-stext.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

*** Admissão futura.
        IF <w_setor>-cd_setor IS INITIAL.
          TRY.
              <w_setor>-cd_setor = it_orgeh_aux[ objid = w_func-plans ]-setrisc.
              <w_setor>-nome_setor = it_orgeh_aux[ objid = w_func-plans ]-stext.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDIF.

        IF <w_empregado>-dt_demissao IS NOT INITIAL.
          <w_setor>-dt_saida = <w_empregado>-dt_demissao.
        ENDIF.


        TRY.
            DATA(lwa_orgeh) = it_orgeh[ objid = w_func-plans ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        CLEAR v_data_aux.

        "Utilizar como data do setor sempre a mais recente entre Cargo(PA0001) x Setor(HRP9665):
        IF ( lwa_orgeh-begda >= w_func-begda ) AND ( lwa_orgeh-endda <= w_func-endda ).
          WRITE lwa_orgeh-begda TO v_data_aux DD/MM/YYYY.
          REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
          <w_setor>-dt_inicio = v_data_aux.

*-CS2020000297 - BUG 69088 - 18.11.2021 - JT - inicio
          IF lwa_orgeh-endda <> '99991231'.
            CLEAR v_data_aux.
            WRITE lwa_orgeh-endda TO v_data_aux DD/MM/YYYY.
            REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
            <w_setor>-dt_saida = v_data_aux.
          ENDIF.
*-CS2020000297 - BUG 69088 - 18.11.2021 - JT - fim

        ELSE.

          WRITE w_func-begda TO v_data_aux DD/MM/YYYY.
          REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
          <w_setor>-dt_inicio = v_data_aux.

*-CS2020000297 - BUG 69088 - 18.11.2021 - JT - inicio
          IF w_func-endda <> '99991231'.
            CLEAR v_data_aux.
            WRITE w_func-endda TO v_data_aux DD/MM/YYYY.
            REPLACE ALL OCCURRENCES OF '.' IN v_data_aux WITH '/'.
            <w_setor>-dt_saida = v_data_aux.
          ENDIF.
*-CS2020000297 - BUG 69088 - 18.11.2021 - JT - fim

        ENDIF.


        IF w_func-stell IS NOT INITIAL.
          <w_setor>-cd_cargo = w_func-stell.
          READ TABLE it_cargos[] ASSIGNING FIELD-SYMBOL(<lfs_cargo>)
            WITH KEY objid = w_func-stell BINARY SEARCH.
          IF ( sy-subrc = 0 ).
            <w_setor>-nome_cargo = <lfs_cargo>-stext.
          ENDIF.

        ELSE.
          DATA(w_pernr) = VALUE ty_rg_pernr( (
              sign   = 'I'
              option = 'EQ'
              low    = <w_empregado>-nr_matricula ) ).
          APPEND w_pernr[ 1 ] TO it_excl_pernr[].
        ENDIF.

        READ TABLE it_cbo[] ASSIGNING FIELD-SYMBOL(<lfs_cbo>) WITH KEY plans = w_func-plans BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          <w_setor>-cargo_cbo = CONV #( <lfs_cbo>-cbo ).
        ENDIF.

        CLEAR: v_data_aux, v_cnpj, v_emissao_rg, v_hire_date, v_fire_date.

      ENDLOOP.

      IF ( it_excl_pernr[] IS NOT INITIAL ).
        DELETE i_request-rsdata-empregados-empregado[] WHERE nr_matricula IN it_excl_pernr[].
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD GET_LISTA_PERNR.

    DATA: v_endda TYPE endda.

    SELECT
      p1~pernr,
      p1~endda,
      p1~begda,
      p1~bukrs,
      p1~werks,
      p1~stell,
      p1~persk,
      p1~abkrs,
      p1~orgeh,
      p1~plans,
      p0~begda AS demissao_date,
      p0~stat2,
      p2~cname,
      p2~nachn,
      p2~vorna,
      p2~gesch,
      p2~famst,
      p2~gbdat
    FROM pa0001 AS p1
    LEFT JOIN pa0000 AS p0
    ON p1~pernr = p0~pernr
    LEFT JOIN pa0002 AS p2
    ON p1~pernr = p2~pernr
    INTO TABLE @DATA(it_func)
    WHERE
      p1~pernr IN @r_pernr[]        AND
      p1~endda >= @me->at_data_base AND
      p1~bukrs IN @r_bukrs[]        AND
      p1~bukrs IN @me->at_bukrs[]   AND
      p1~werks IN @r_werks[]        AND
      p1~abkrs <> 'BA'              AND
      p0~begda <= @me->at_data_base AND
      p0~endda >= @me->at_data_base AND
      p0~stat2 IN ( '3', '0' )      AND
      p2~endda > @me->at_data_base.
    SORT it_func[] BY pernr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_func[] COMPARING pernr.

    IF ( it_func[] IS NOT INITIAL ).
***      DELETE it_func[] WHERE stat2 = '0' AND demissao_date  <= me->at_data_base.
***      it_funcionarios[] = CORRESPONDING #( it_func[] ).
***      MOVE-CORRESPONDING it_funcionarios[] TO me->at_it_dados_empregado[].
      me->at_it_dados_empregado[] = CORRESPONDING #( it_func[] ).
      CLEAR: it_func[]. FREE: it_func[].
    ENDIF.

    "-> Se existir demitidos à serem enviados, buscar posição anterior.
    IF ( line_exists( me->at_it_dados_empregado[ plans = '99999999' ] ) ).

      LOOP AT me->at_it_dados_empregado[]
        ASSIGNING FIELD-SYMBOL(<w_empregado>)  WHERE plans = '99999999'.

        v_endda = <w_empregado>-begda - 1.

        SELECT SINGLE pernr, begda, endda, stell, plans, orgeh
            FROM pa0001
            WHERE pernr = @<w_empregado>-pernr
                AND endda = @v_endda
                AND plans <> '99999999'
             INTO @DATA(w_posicao_demitido) .

        IF ( sy-subrc = 0 ).
          <w_empregado>-begda = w_posicao_demitido-begda.
          <w_empregado>-stell = w_posicao_demitido-stell.
          <w_empregado>-plans = w_posicao_demitido-plans.
          <w_empregado>-orgeh = w_posicao_demitido-orgeh.
        ENDIF.

        CLEAR: v_endda, w_posicao_demitido.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD GET_LISTA_PERNR_RETROATIVO.

    DATA: v_endda TYPE endda.

    SELECT
      p1~pernr,
      p1~endda,
      p1~begda,
      p1~bukrs,
      p1~werks,
      p1~stell,
      p1~persk,
      p1~abkrs,
      p1~orgeh,
      p1~plans,
      p0~begda AS demissao_date,
      p0~stat2,
      p2~cname,
      p2~nachn,
      p2~vorna,
      p2~gesch,
      p2~famst,
      p2~gbdat
    FROM pa0001 AS p1
    LEFT JOIN pa0000 AS p0
    ON p1~pernr = p0~pernr
    LEFT JOIN pa0002 AS p2
    ON p1~pernr = p2~pernr
    INTO TABLE @DATA(it_func)
    WHERE
      p1~pernr IN @me->at_rg_pernr[] AND
      p1~endda >= @sy-datum AND
      p1~bukrs IN @me->at_bukrs[] AND
      p1~werks IN @r_werks[]        AND
      p1~abkrs <> 'BA'              AND
      p0~endda >= @sy-datum AND
      p0~stat2 IN ( '3', '0' )      AND
      p2~endda >= @sy-datum.

    SORT it_func[] BY pernr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_func[] COMPARING pernr.

    IF ( it_func[] IS NOT INITIAL ).
      me->at_it_dados_empregado[] = CORRESPONDING #( it_func[] ).
      CLEAR: it_func[]. FREE: it_func[].
    ENDIF.

    "-> Se existir demitidos à serem enviados, buscar posição anterior.
    IF ( line_exists( me->at_it_dados_empregado[ plans = '99999999' ] ) ).

      LOOP AT me->at_it_dados_empregado[]
        ASSIGNING FIELD-SYMBOL(<w_empregado>)  WHERE plans = '99999999'.

        v_endda = <w_empregado>-begda - 1.

        SELECT SINGLE pernr, begda, endda, stell, plans, orgeh
            FROM pa0001
            WHERE pernr = @<w_empregado>-pernr
                AND endda = @v_endda
                AND plans <> '99999999'
             INTO @DATA(w_posicao_demitido) .

        IF ( sy-subrc = 0 ).
          <w_empregado>-begda = w_posicao_demitido-begda.
          <w_empregado>-stell = w_posicao_demitido-stell.
          <w_empregado>-plans = w_posicao_demitido-plans.
          <w_empregado>-orgeh = w_posicao_demitido-orgeh.
        ENDIF.

        CLEAR: v_endda, w_posicao_demitido.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD get_out_empregados.

    DATA: it_log    TYPE TABLE OF zhcmt_pa_0023.

    DATA: s_data_json      TYPE zhcms_rsdata_empreg_service, "zhcms_rsdata_func, "
          lit_empregados   TYPE STANDARD TABLE OF ty_get_func,
          lit_get_dados    TYPE zhcms_rsdata_empreg_res_get,
          lit_del_dados    TYPE zhcms_rsdata_empreg_res_get,
          lit_cpf_dados    TYPE zrsdatainsert_empregados_reque,
          lit_insert_dados TYPE zrsdatainsert_empregados_reque,
          lit_saida_func   TYPE STANDARD TABLE OF ty_saida_func,
          lwa_empregados   TYPE ty_get_func,
          e_json           TYPE string.

    DATA: lva_text TYPE string VALUE 'Empregado NAO encontrado!'.
    DATA: lwa_service TYPE zde_rs_data_cpi.

    DATA: v_hire_date    TYPE sy-datum,
          v_data_adm     TYPE char20,
          v_data_demi    TYPE char20,
          v_data_crg_sap TYPE char20,
          v_cpf          TYPE char20.

    TYPES lr_pernr_type TYPE RANGE OF pa0001-pernr.

    DATA: s_get_empreg_reques TYPE zrsdataget_empregados_request.
    DATA: s_ins_empreg_reques TYPE zrsdatainsert_empregados_reque.
    DATA: lt_func           TYPE TABLE OF zhcm_funcionarios_list.

    DATA: zcl_rsdata TYPE REF TO zcl_hcm_rsdata_tgg_cpi.
    CREATE OBJECT zcl_rsdata.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    IF ( i_request-rsdata-empregados-empregado[] IS NOT INITIAL ).

      s_data_json-empregadoservice-usernametoken-username = ''."'integracao.db0801amaggiexp@rsdata.com.br'.
      s_data_json-empregadoservice-usernametoken-password = ''."'43a19edaac7b63358c252c544be06c49'.

      s_data_json-empregadoservice-config-tpverempregado  = 'MATRICULA'.
      s_data_json-empregadoservice-config-tpversetorcargo = 'NOME'.
      s_data_json-empregadoservice-config-metodo          = 'getEmpregadosRequest'.

      LOOP AT i_request-rsdata-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<lfs_emp_resp_cand>) .

        lwa_empregados-idempresa                = <lfs_emp_resp_cand>-id_empresa .
        lwa_empregados-codintegracaoempresa     = <lfs_emp_resp_cand>-cod_integracao_empresa.
        lwa_empregados-idempregado              = <lfs_emp_resp_cand>-id_empregado    .
        lwa_empregados-nrcnpjempresa            = <lfs_emp_resp_cand>-nr_cnpjempresa .
        lwa_empregados-razaosocialempresa       = <lfs_emp_resp_cand>-razao_social_empresa  .
        lwa_empregados-denominacaoempresa       = <lfs_emp_resp_cand>-denominacao_empresa.
        lwa_empregados-nomeempregado            = <lfs_emp_resp_cand>-nome_empregado.
        lwa_empregados-nrnit                    = <lfs_emp_resp_cand>-nr_nit .
        lwa_empregados-nrcpf                    = <lfs_emp_resp_cand>-nr_cpf .
        lwa_empregados-nrmatricula              = <lfs_emp_resp_cand>-nr_matricula   .


        APPEND lwa_empregados TO lit_empregados.

        CLEAR: s_data_json-empregadoservice-empregados[], e_json, lwa_service.
        s_data_json-empregadoservice-empregados[] = lit_empregados[].

        e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                    pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                   ).

        lwa_service-service = 'EmpregadoService'.
        lwa_service-json    = e_json.
        lwa_service-tipo    = 'getEmpregadosRequest_TGG'.
*** Chamar classe RSDATA que envia os dados.
        TRY .
            obj_rsdata_cpi->zif_integracao_outbound~get_instance(
            )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
            IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.

              DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.

              /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_get_dados ).

              IF lit_get_dados-empregadoservice-empregados[] IS NOT INITIAL.


                v_cpf = <lfs_emp_resp_cand>-nr_cpf.

                REPLACE ALL OCCURRENCES OF '.' IN v_cpf WITH ''.
                REPLACE ALL OCCURRENCES OF '-' IN v_cpf WITH ''.

                CONDENSE v_cpf NO-GAPS.

                READ TABLE lit_get_dados-empregadoservice-empregados ASSIGNING FIELD-SYMBOL(<lfs_emp_resp>) WITH KEY nrcpf = v_cpf.

                IF sy-subrc = 0.

                  IF <lfs_emp_resp>-tpvinculo = 'Candidato'.
*** Nova inserção . Cadastro no RSdata como Candidato.
                    IF <lfs_emp_resp_cand>-nr_matricula IS INITIAL.
                      APPEND <lfs_emp_resp_cand> TO lit_cpf_dados-rsdata-empregados-empregado.
                    ENDIF.

                  ELSE.

                    APPEND <lfs_emp_resp_cand> TO lit_insert_dados-rsdata-empregados-empregado.

                    v_data_adm  = <lfs_emp_resp_cand>-dt_admissao.
                    v_data_demi = <lfs_emp_resp_cand>-dt_demissao.

                    READ TABLE <lfs_emp_resp_cand>-setores_cargos-setor_cargo INTO DATA(w_setor_cargo) INDEX 1.

                    IF sy-subrc = 0.

                      DATA(lva_setor) =  w_setor_cargo-cd_setor.
                      v_data_crg_sap = w_setor_cargo-dt_inicio.

                      IF  <lfs_emp_resp>-setorcargo[] IS NOT INITIAL.

                        READ TABLE <lfs_emp_resp>-setorcargo[] INTO DATA(lwa_crg_set) WITH KEY dtsaida = ''.

                        IF sy-subrc = 0.

                          IF ( ( <lfs_emp_resp>-dtadmissao <> v_data_adm ) ) OR
                             ( ( <lfs_emp_resp>-dtadmissao = v_data_crg_sap ) AND (  ( lwa_crg_set-cdsetor <> lva_setor ) OR  ( lwa_crg_set-cdcargo <> w_setor_cargo-cd_cargo ) ) ).

** excluir e enviar novamente os dados de cargos ou setor
                            READ TABLE <lfs_emp_resp>-setorcargo ASSIGNING FIELD-SYMBOL(<lfs_crg_set_del>) INDEX 1.
                            IF <lfs_crg_set_del> IS ASSIGNED.
                              <lfs_crg_set_del>-tpmovimentacao = 'E'.
                            ENDIF.

                            APPEND <lfs_emp_resp> TO lit_del_dados-empregadoservice-empregados.

                          ELSE.
                            IF   ( ( lwa_crg_set-dtinicio <> v_data_crg_sap ) AND (  ( lwa_crg_set-cdsetor = lva_setor ) AND  ( lwa_crg_set-cdcargo = w_setor_cargo-cd_cargo ) )
                              AND v_data_demi  IS INITIAL ).
** Removo essa informação para envio.
                              DELETE lit_insert_dados-rsdata-empregados-empregado[]  WHERE nr_matricula = <lfs_emp_resp>-nrmatricula.

                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ELSE.
*** Se não voltou no GET é um novo insert.
                  APPEND <lfs_emp_resp_cand> TO lit_insert_dados-rsdata-empregados-empregado.

                ENDIF.
                CLEAR: v_cpf, v_data_adm, v_data_demi, lva_setor, v_data_crg_sap.

              ELSE.
***** Se o empregado não foi encontrado no RSDATA deve enviar novamente.
                "org.apache.cxf.binding.soap.SoapFault: Empregado NAO encontrado!

                FIND FIRST OCCURRENCE OF lva_text IN lva_json_retorno.
                IF sy-subrc = 0.
**** Verificar se é candidato.
                  s_data_json-empregadoservice-config-tpverempregado  = 'NOME'.
                  s_data_json-empregadoservice-config-tpversetorcargo = ''.

                  CLEAR: _lwa_ret_call, lva_json_retorno, lit_get_dados, e_json, lwa_service-json .

                  e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                              pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                             ).

                  lwa_service-json    = e_json.
                  TRY .
                      obj_rsdata_cpi->zif_integracao_outbound~get_instance(
                      )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = _lwa_ret_call ).
                      IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
                        lva_json_retorno = _lwa_ret_call-ds_data_retorno.

                        FIND FIRST OCCURRENCE OF lva_text IN lva_json_retorno.
                        IF sy-subrc = 0.
***** Se o empregado não foi encontrado no RSDATA deve enviar novamente.
                          APPEND <lfs_emp_resp_cand> TO lit_insert_dados-rsdata-empregados-empregado.
                        ELSE.
                          /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_get_dados ).

                          IF lit_get_dados-empregadoservice-empregados[] IS NOT INITIAL.
                            CLEAR:   v_cpf.
                            v_cpf = <lfs_emp_resp_cand>-nr_cpf.

                            REPLACE ALL OCCURRENCES OF '.' IN v_cpf WITH ''.
                            REPLACE ALL OCCURRENCES OF '-' IN v_cpf WITH ''.

                            CONDENSE v_cpf NO-GAPS.

                            IF <lfs_emp_resp> IS ASSIGNED.
                              CLEAR <lfs_emp_resp>.
                            ENDIF.

                            READ TABLE lit_get_dados-empregadoservice-empregados ASSIGNING <lfs_emp_resp> WITH KEY nrcpf = v_cpf.
                            IF sy-subrc = 0.
                              IF <lfs_emp_resp>-tpvinculo = 'Candidato'.
*** Nova inserção . Cadastro no RSdata como Candidato.
                                APPEND <lfs_emp_resp_cand> TO lit_cpf_dados-rsdata-empregados-empregado.
                              ELSE.
** Nova Contratação. Nova matricula ( Porém é uma recontratação ) Então ele acha com o nome do funcionário.
                                APPEND <lfs_emp_resp_cand> TO lit_insert_dados-rsdata-empregados-empregado.

                              ENDIF.
                            ENDIF.
                          ELSE.
***** Salva Log * GET Deu Erro não continua com o processo.
                            DATA(w_log) = VALUE zhcmt_pa_0023(
                                        servico         = 'GetOutEmpregados'
                                        data_envio      = sy-datum
                                        hora_envio      = sy-uzeit
                                        pernr           = <lfs_emp_resp_cand>-nr_matricula
                                        retorno_servico = 'Erro recebimento de dados'  ).

                            APPEND w_log TO it_log[].

                          ENDIF.
                        ENDIF.
                      ENDIF.
                  ENDTRY.
                ELSE.

***** Salva Log * GET Deu Erro não continua com o processo.
                  CLEAR: w_log.
                  w_log = VALUE zhcmt_pa_0023(
                              servico         = 'GetOutEmpregados'
                              data_envio      = sy-datum
                              hora_envio      = sy-uzeit
                              pernr           = <lfs_emp_resp_cand>-nr_matricula
                              retorno_servico = 'Erro recebimento de dados'  ).

                  APPEND w_log TO it_log[].
                ENDIF.
              ENDIF.
            ELSE.
              " ERRO AO BUSCAR O GET FUNCIONARIOS
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
                servico         = 'GetOutEmpregados'
                data_envio      = sy-datum
                hora_envio      = sy-uzeit
                pernr           = <lfs_emp_resp_cand>-nr_matricula
                retorno_servico = 'Erro recebimento de dados "GET" '  ).

              APPEND w_log TO it_log[].

            ENDIF.
          CATCH zcx_integracao.
            CLEAR: w_log.
            w_log = VALUE zhcmt_pa_0023(
              servico         = 'GetOutEmpregados'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_emp_resp_cand>-nr_matricula
              retorno_servico = 'Erro integração de dados - GET '  ).

            APPEND w_log TO it_log[].
          CATCH zcx_error.
            CLEAR: w_log.
            w_log = VALUE zhcmt_pa_0023(
              servico         = 'GetOutEmpregados'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_emp_resp_cand>-nr_matricula
              retorno_servico = 'Erro integração de dados - GET '  ).

            APPEND w_log TO it_log[].
        ENDTRY.
        CLEAR: lit_empregados, lwa_empregados, lit_get_dados, _lwa_ret_call, lva_json_retorno, w_setor_cargo, lwa_crg_set.

        IF <lfs_emp_resp_cand> IS ASSIGNED.
          CLEAR: <lfs_emp_resp_cand>.
        ENDIF.

        IF <lfs_emp_resp> IS ASSIGNED.
          CLEAR <lfs_emp_resp>.
        ENDIF.

        IF <lfs_crg_set_del> IS ASSIGNED.
          CLEAR <lfs_crg_set_del>.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF lit_del_dados-empregadoservice-empregados[] IS NOT INITIAL.
      "-> Chama método para enviar os empregados: DELETE
      zcl_rsdata->delete_empregados_get( EXPORTING i_request_del = lit_del_dados IMPORTING  e_pernr = DATA(rg_pernr) ).
    ENDIF.

    IF lit_cpf_dados-rsdata-empregados-empregado[] IS NOT INITIAL.
      " MOVE-CORRESPONDING lit_cpf_dados-rsdata-empregados-empregado[] TO i_request_cpf-empregadoservice-empregados[].
      MOVE-CORRESPONDING lit_cpf_dados-rsdata-empregados-empregado[] TO i_request_cpf-rsdata-empregados-empregado[].
    ENDIF.

    IF  lit_insert_dados-rsdata-empregados-empregado[] IS NOT INITIAL.
      "MOVE-CORRESPONDING  lit_insert_dados-rsdata-empregados-empregado[] TO i_request_ins-empregadoservice-empregados[].

      MOVE-CORRESPONDING  lit_insert_dados-rsdata-empregados-empregado[] TO i_request_ins-rsdata-empregados-empregado[].

      IF rg_pernr[] IS NOT INITIAL.
        DELETE i_request_ins-rsdata-empregados-empregado WHERE nr_matricula IN rg_pernr[].
      ENDIF.

    ENDIF.

    "-> Grava log do envio do serviço 'GetOutEmpregados':
    IF ( it_log[] IS NOT INITIAL ).
      MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
    ENDIF.
  ENDMETHOD.


  METHOD INICIA_CLASSE_WEBSERVICE.

    CASE i_service_name.

      WHEN 'ZRSDATACO_EMPREGADO_PORT'.

        TRY.
            CREATE OBJECT cl_web_empregado
              EXPORTING
                logical_port_name = i_port_name.
          CATCH cx_ai_system_fault INTO DATA(v_msg_erro).
        ENDTRY.

      WHEN 'ZRSEMPRESACO_EMPRESA_PORT'.

        TRY.
            CREATE OBJECT cl_web_empresa
              EXPORTING
                logical_port_name = i_port_name.
          CATCH cx_ai_system_fault INTO v_msg_erro.
        ENDTRY.

      WHEN 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'.

        TRY.
            CREATE OBJECT cl_web_movimento
              EXPORTING
                logical_port_name = i_port_name.
          CATCH cx_ai_system_fault INTO v_msg_erro.
        ENDTRY.

    ENDCASE.


  ENDMETHOD.


  METHOD insert_absenteismo.

    DATA:
      s_insert_absenteismo_req TYPE zrsdatamovinsert_absenteismo_1,
      zcl_proxy                TYPE REF TO zrsdatamovco_movimento_emprega,
      it_log                   TYPE TABLE OF zhcmt_pa_0023,
      v_data_retorno           TYPE endda,
      v_cnpj                   TYPE char20,
      v_ini_abs                TYPE char20,
      v_fim_abs                TYPE char20,
      v_prev_retorno           TYPE char20,
      v_dias_afast             TYPE i.

    DATA: s_data_json TYPE zhcms_rsdata_empabsent_service,
          e_json      TYPE string.

    DATA: lit_ret_ferias TYPE zhcms_rsdata_emp_ferias_resp.
    DATA: lwa_service TYPE zde_rs_data_cpi.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    "Absenteísmos/Ausências 2001
    SELECT
            p2001~pernr,
            p2001~subty,
            p2001~awart,
            p2001~begda,
            p2001~endda,
            p2001~aedtm,
            p1~bukrs,
            p1~werks,
            t54t~atext
      FROM pa2001 AS p2001
      LEFT JOIN t554t AS t54t
      ON t54t~awart = p2001~awart
      LEFT JOIN pa0001 AS p1
      ON p1~pernr = p2001~pernr
      INTO TABLE @DATA(it_afastamentos)
      WHERE p2001~pernr IN @me->at_rg_pernr[] AND
            p2001~aedtm = @me->at_data_base AND
            p2001~awart NOT IN ( '0100', '0110', '0120', '0901', '0902', '0907', '0908', '0910', '0911', '0954', '0955', '0956', '0965' ) AND
            p1~begda <= @me->at_data_base AND
            p1~endda >= @me->at_data_base AND
            t54t~sprsl  = @sy-langu   AND
            t54t~moabw  = '37'.

    SORT it_afastamentos[] BY pernr ASCENDING.

    LOOP AT it_afastamentos[] INTO DATA(w_afast)
        GROUP BY ( werks = w_afast-werks
                               size       = GROUP SIZE
                               index      = GROUP INDEX ) ASCENDING
                REFERENCE INTO DATA(group_werks).

      "->Criando estrutura para envio de ausências
      APPEND INITIAL LINE TO s_data_json-movimentoservice-dados[] ASSIGNING FIELD-SYMBOL(<w_empresa>).

      TRY.
          DATA(w_detalhe_emp) = me->at_it_dados_empresa[ werks = group_werks->werks ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = w_detalhe_emp-stcd1
        IMPORTING
          output = v_cnpj.

      <w_empresa>-nrcnpjempresa = v_cnpj+0(18).

      LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.

        "-> Acrescenta dados das férias encontradas para o funcionário
        APPEND INITIAL LINE TO <w_empresa>-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).

        <w_empregado>-matricula = <w_func>-pernr.

        APPEND INITIAL LINE TO <w_empregado>-absenteismos ASSIGNING FIELD-SYMBOL(<w_afast>).

        IF ( <w_func>-endda <> '99991231' ).
          v_data_retorno = <w_func>-endda + 1.
          WRITE v_data_retorno TO  v_prev_retorno DD/MM/YYYY.
          v_dias_afast = v_data_retorno - <w_func>-begda.
        ELSE.
          v_dias_afast = 0.
        ENDIF.

        WRITE <w_func>-begda TO  v_ini_abs DD/MM/YYYY.
        WRITE <w_func>-endda TO  v_fim_abs DD/MM/YYYY.

        REPLACE ALL OCCURRENCES OF '.' IN v_ini_abs WITH '/'.
        REPLACE ALL OCCURRENCES OF '.' IN v_fim_abs WITH '/'.
        REPLACE ALL OCCURRENCES OF '.' IN v_prev_retorno WITH '/'.


        <w_afast>-data = v_ini_abs.
        <w_afast>-hora = '00:00'.
        <w_afast>-dataprevretorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ).
        <w_afast>-prevafastamento = v_dias_afast.
        <w_afast>-tpprevafastamento = '2'.
        <w_afast>-dataretorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ). "V_FIM_ABS.
        <w_afast>-tempoafastamento = v_dias_afast.
        <w_afast>-tpafastamento = '2'.
        <w_afast>-motivoafastamento = <w_func>-atext.
        <w_afast>-observacoes = <w_func>-atext.

        CLEAR: v_dias_afast, v_cnpj, v_data_retorno.
      ENDLOOP.
    ENDLOOP.

    DELETE s_data_json-movimentoservice-dados[] WHERE empregado IS INITIAL.

    IF s_data_json-movimentoservice-dados[] IS NOT INITIAL.

      s_data_json-movimentoservice-usernametoken-username = 'integracao.db0801amaggiexp@rsdata.com.br'.
      s_data_json-movimentoservice-usernametoken-password = '43a19edaac7b63358c252c544be06c49'.
      s_data_json-movimentoservice-config-metodo          = 'insertAbsenteismo'.

      e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                  pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                 ).

      lwa_service-service = 'MovimentoService'.
      lwa_service-json    = e_json.
      lwa_service-tipo    = 'insertAbsenteismo_TGG'.

      "-> Envio das informações
*** Chamar classe RSDATA que envia os dados.
      TRY .
          obj_rsdata_cpi->zif_integracao_outbound~get_instance(
          )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
          IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
            DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.

            /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_ferias ).



            LOOP AT lit_ret_ferias-insertferiasresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
              READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.

*            try.
*                data(w_absenteismo) = w_empregados-absenteismos-absenteismo[ 1 ].
*              catch cx_sy_itab_line_not_found.
*            endtry.
*
              DATA(w_log) = VALUE zhcmt_pa_0023(
                  servico         = 'insertAbsenteismo'
                  data_envio      = sy-datum
                  hora_envio      = sy-uzeit
                  "pernr           = conv #( w_empregados-matricula )
                  bukrs           = ''
                  werks           = '' ).
              "dados_enviados  = |{ w_absenteismo-motivo_afastamento } Retorno:{ w_absenteismo-data_prev_retorno }| ).

              LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
                w_log-retorno_servico = w_mensagem-txdescricao.
                w_log-id_log = sy-tabix.
                APPEND w_log TO it_log[].
              ENDLOOP.

            ENDLOOP.
          ENDIF.

        CATCH zcx_integracao.
          CLEAR: w_log.
          w_log = VALUE zhcmt_pa_0023(
          servico         = 'Insert_Absenteismo'
          data_envio      = sy-datum
          hora_envio      = sy-uzeit
         " pernr           = <lfs_trans_emp>-nr_matricula
          retorno_servico = 'Erro Insert Absenteismo'  ).

          APPEND w_log TO it_log[].
        CATCH zcx_error.
          CLEAR: w_log.
          w_log = VALUE zhcmt_pa_0023(
          servico         = 'Insert_Absenteismo'
          data_envio      = sy-datum
          hora_envio      = sy-uzeit
         " pernr           = <lfs_trans_emp>-nr_matricula
          retorno_servico = 'Erro Insert Absenteismo'  ).

          APPEND w_log TO it_log[].
      ENDTRY.

      IF ( it_log[] IS NOT INITIAL ).
        MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD insert_absenteismo_retroativo.

    DATA:
      s_insert_absenteismo_req TYPE zrsdatamovinsert_absenteismo_1,
      zcl_proxy                TYPE REF TO zrsdatamovco_movimento_emprega,
      it_log                   TYPE TABLE OF zhcmt_pa_0023,
      v_data_retorno           TYPE endda,
      v_cnpj                   TYPE char20,
      v_ini_abs                TYPE char20,
      v_fim_abs                TYPE char20,
      v_prev_retorno           TYPE char20,
      v_dias_afast             TYPE i.

    DATA: s_data_json TYPE zhcms_rsdata_empabsent_service,
          e_json      TYPE string.

    DATA: lit_ret_ferias TYPE zhcms_rsdata_emp_ferias_resp.
    DATA: lwa_service TYPE zde_rs_data_cpi.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    "Absenteísmos/Ausências 2001
    SELECT
            p2001~pernr,
            p2001~subty,
            p2001~awart,
            p2001~begda,
            p2001~endda,
            p2001~aedtm,
            p1~bukrs,
            p1~werks,
            t54t~atext
      FROM pa2001 AS p2001
      LEFT JOIN t554t AS t54t
      ON t54t~awart = p2001~awart
      LEFT JOIN pa0001 AS p1
      ON p1~pernr = p2001~pernr
      INTO TABLE @DATA(it_afastamentos)
      WHERE p2001~pernr IN @me->at_rg_pernr[] AND
            p2001~endda >= @me->at_data_base AND
            p2001~awart NOT IN ( '0100', '0110', '0120', '0901', '0902', '0907', '0908', '0910', '0911', '0954', '0955', '0956', '0965' ) AND
            p1~endda >= @sy-datum AND
            t54t~sprsl  = @sy-langu   AND
            t54t~moabw  = '37'.
    SORT it_afastamentos[] BY pernr ASCENDING.

    LOOP AT it_afastamentos[] INTO DATA(w_afast)
        GROUP BY ( werks = w_afast-werks
                               size       = GROUP SIZE
                               index      = GROUP INDEX ) ASCENDING
                REFERENCE INTO DATA(group_werks).

      "->Criando estrutura para envio de ausências
      " APPEND INITIAL LINE TO s_insert_absenteismo_req-dados-empresa[] ASSIGNING FIELD-SYMBOL(<w_empresa>).
      APPEND INITIAL LINE TO s_data_json-movimentoservice-dados[] ASSIGNING FIELD-SYMBOL(<w_empresa>).

      TRY.
          DATA(w_detalhe_emp) = me->at_it_dados_empresa[ werks = group_werks->werks ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = w_detalhe_emp-stcd1
        IMPORTING
          output = v_cnpj.

      <w_empresa>-nrcnpjempresa =  v_cnpj+0(18).

      LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.

        "-> Acrescenta dados das férias encontradas para o funcionário
        "APPEND INITIAL LINE TO <w_empresa>-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).
        APPEND INITIAL LINE TO <w_empresa>-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).

        <w_empregado>-matricula = <w_func>-pernr.

        APPEND INITIAL LINE TO <w_empregado>-absenteismos ASSIGNING FIELD-SYMBOL(<w_afast>).

        IF ( <w_func>-endda <> '99991231' ).
          v_data_retorno = <w_func>-endda + 1.
          WRITE v_data_retorno TO  v_prev_retorno DD/MM/YYYY.
          v_dias_afast = v_data_retorno - <w_func>-begda.
        ELSE.
          v_dias_afast = 0.
        ENDIF.

        WRITE <w_func>-begda TO  v_ini_abs DD/MM/YYYY.
        WRITE <w_func>-endda TO  v_fim_abs DD/MM/YYYY.

        REPLACE ALL OCCURRENCES OF '.' IN v_ini_abs WITH '/'.
        REPLACE ALL OCCURRENCES OF '.' IN v_fim_abs WITH '/'.
        REPLACE ALL OCCURRENCES OF '.' IN v_prev_retorno WITH '/'.


        <w_afast>-data = v_ini_abs.
        <w_afast>-hora = '00:00'.
        <w_afast>-dataprevretorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ).
        <w_afast>-prevafastamento = v_dias_afast.
        <w_afast>-tpprevafastamento = '2'.
        <w_afast>-dataretorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ). "V_FIM_ABS.
        <w_afast>-tempoafastamento = v_dias_afast.
        <w_afast>-tpafastamento = '2'.
        <w_afast>-motivoafastamento = <w_func>-atext.
        <w_afast>-observacoes = <w_func>-atext.

        CLEAR: v_dias_afast, v_cnpj, v_data_retorno.


      ENDLOOP.

    ENDLOOP.

    DELETE s_data_json-movimentoservice-dados[] WHERE empregado IS INITIAL.

    IF s_data_json-movimentoservice-dados[] IS NOT INITIAL.

      s_data_json-movimentoservice-usernametoken-username = 'integracao.db0801amaggiexp@rsdata.com.br'.
      s_data_json-movimentoservice-usernametoken-password = '43a19edaac7b63358c252c544be06c49'.
      s_data_json-movimentoservice-config-metodo          = 'insertAbsenteismo'.

      e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                  pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                 ).

      lwa_service-service = 'MovimentoService'.
      lwa_service-json    = e_json.
      lwa_service-tipo    = 'insertAbsenteismo_retro_TGG'.

      "-> Envio das informações
*** Chamar classe RSDATA que envia os dados.
      TRY .
          obj_rsdata_cpi->zif_integracao_outbound~get_instance(
          )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
          IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
            DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.

            /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_ferias ).



            LOOP AT lit_ret_ferias-insertferiasresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
              READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.

*            try.
*                data(w_absenteismo) = w_empregados-absenteismos-absenteismo[ 1 ].
*              catch cx_sy_itab_line_not_found.
*            endtry.
*
              DATA(w_log) = VALUE zhcmt_pa_0023(
                  servico         = 'insertAbsenteismoTggRetro'
                  data_envio      = sy-datum
                  hora_envio      = sy-uzeit
                  "pernr           = conv #( w_empregados-matricula )
                  bukrs           = ''
                  werks           = '' ).
              "dados_enviados  = |{ w_absenteismo-motivo_afastamento } Retorno:{ w_absenteismo-data_prev_retorno }| ).

              LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
                w_log-retorno_servico = w_mensagem-txdescricao.
                w_log-id_log = sy-tabix.
                APPEND w_log TO it_log[].
              ENDLOOP.

            ENDLOOP.
          ENDIF.

        CATCH zcx_integracao.
          CLEAR: w_log.
          w_log = VALUE zhcmt_pa_0023(
          servico         = 'Insert_Absenteismo'
          data_envio      = sy-datum
          hora_envio      = sy-uzeit
         " pernr           = <lfs_trans_emp>-nr_matricula
          retorno_servico = 'Erro Insert Absenteismo Tgg Retro'  ).

          APPEND w_log TO it_log[].
        CATCH zcx_error.
          CLEAR: w_log.
          w_log = VALUE zhcmt_pa_0023(
          servico         = 'Insert_Absenteismo'
          data_envio      = sy-datum
          hora_envio      = sy-uzeit
         " pernr           = <lfs_trans_emp>-nr_matricula
          retorno_servico = 'Erro Insert Absenteismo Tgg Retro'  ).

          APPEND w_log TO it_log[].
      ENDTRY.

      IF ( it_log[] IS NOT INITIAL ).
        MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD insert_empregados.

    DATA: zcl_proxy TYPE REF TO zrsdataco_empregado_port,
          it_log    TYPE TABLE OF zhcmt_pa_0023.

    DATA: s_data_json     TYPE zhcms_rsdata_empreg_service,
          lwa_insert_json TYPE zhcms_rsdata_func_emp_s,
          lit_ret_emp_ins TYPE 	zhcms_rsdata_empregados_resp,
          e_json          TYPE string.

    DATA: lwa_service TYPE zde_rs_data_cpi.


    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

*** Primeiro faz o insert dos empregados ja com matricula.
    IF i_request_ins-rsdata-empregados-empregado[] IS NOT INITIAL.

      s_data_json-empregadoservice-usernametoken-username = ''."'integracao.db0801amaggiexp@rsdata.com.br'.
      s_data_json-empregadoservice-usernametoken-password = ''."'43a19edaac7b63358c252c544be06c49'.

      s_data_json-empregadoservice-config-tpverempregado  = 'MATRICULA'.
      s_data_json-empregadoservice-config-tpversetorcargo = 'NOME'.
      s_data_json-empregadoservice-config-metodo          = 'insertEmpregadosRequest'.

      LOOP AT i_request_ins-rsdata-empregados-empregado[]  ASSIGNING FIELD-SYMBOL(<lfs_insert_emp>).

        "DATA(lva_index) = sy-tabix.

        lwa_insert_json-novocontrato                      =  <lfs_insert_emp>-novo_contrato  .
        lwa_insert_json-dttransferencia                   =  <lfs_insert_emp>-dt_transferencia .
        lwa_insert_json-idempresadestino                  =  <lfs_insert_emp>-id_empresa_destino .
        lwa_insert_json-statusempresadestino              =  <lfs_insert_emp>-status_empresa_destino .
        lwa_insert_json-codintegracaoempresadestino       =  <lfs_insert_emp>-cod_integracao_empresa_destino .
        lwa_insert_json-nrcnpjempresadestino              =  <lfs_insert_emp>-nr_cnpjempresa_destino .
        lwa_insert_json-razaosocialempresadestino         =  <lfs_insert_emp>-razao_social_empresa_destino .
        lwa_insert_json-denominacaoempresadestino         =  <lfs_insert_emp>-denominacao_empresa_destino  .
        "lwa_insert_json-NRMATRICULADESTINO                =  <lfs_insert_emp>- .
        "lwa_insert_json-MATRICULARHDESTINO                =  <lfs_insert_emp>- .
        lwa_insert_json-forcartransferencia               =  <lfs_insert_emp>-forcar_transferencia .
        lwa_insert_json-statusempresa                     =  <lfs_insert_emp>-status_empresa .
        lwa_insert_json-idempresa                         =  <lfs_insert_emp>-id_empresa .
        lwa_insert_json-codintegracaoempresa              =  <lfs_insert_emp>-cod_integracao_empresa .
        lwa_insert_json-nrcnpjempresa                     =  <lfs_insert_emp>-nr_cnpjempresa .
        lwa_insert_json-razaosocialempresa                =  <lfs_insert_emp>-razao_social_empresa .
        lwa_insert_json-denominacaoempresa                =  <lfs_insert_emp>-denominacao_empresa  .
        lwa_insert_json-idempregado                       =  <lfs_insert_emp>-id_empregado .
        lwa_insert_json-codintegracaoempregado            =  <lfs_insert_emp>-cod_integracao_empregado .
        lwa_insert_json-nomeempregado                     =  <lfs_insert_emp>-nome_empregado .
        lwa_insert_json-carteiratrabalhodigital           =  <lfs_insert_emp>-carteira_trabalho_digital  .
        lwa_insert_json-dtnascimento                      =  <lfs_insert_emp>-dt_nascimento  .
        lwa_insert_json-tpsexo                            =  <lfs_insert_emp>-tp_sexo  .
        "lwa_insert_json-raca                              =  <lfs_insert_emp>- .
        lwa_insert_json-nrctps                            =  <lfs_insert_emp>-nr_ctps  .
        lwa_insert_json-nrseriectps                       =  <lfs_insert_emp>-nr_serie_ctps  .
        lwa_insert_json-dtemictps                         =  <lfs_insert_emp>-dt_emi_ctps  .
        lwa_insert_json-ufemictps                         =  <lfs_insert_emp>-uf_emi_ctps  .
        lwa_insert_json-nridentidade                      =  <lfs_insert_emp>-nr_identidade  .
        lwa_insert_json-orgaoexpedidorrg                  =  <lfs_insert_emp>-orgao_expedidor_rg .
        lwa_insert_json-dtemirg                           =  <lfs_insert_emp>-dt_emi_rg  .
        lwa_insert_json-ufemirg                           =  <lfs_insert_emp>-uf_emi_rg  .
        lwa_insert_json-nrnit                             =  <lfs_insert_emp>-nr_nit .
        lwa_insert_json-nrcpf                             =  <lfs_insert_emp>-nr_cpf .
        lwa_insert_json-nrmatricula                       =  <lfs_insert_emp>-nr_matricula .
        lwa_insert_json-categoriatrabalhador              =  <lfs_insert_emp>-categoria_trabalhador  .
        lwa_insert_json-tpvinculo                         =  <lfs_insert_emp>-tp_vinculo .
        lwa_insert_json-br_pdh                            =  <lfs_insert_emp>-br_pdh .
        lwa_insert_json-regimerevezamento                 =  <lfs_insert_emp>-regime_revezamento .
        lwa_insert_json-dtadmissao                        =  <lfs_insert_emp>-dt_admissao  .
        lwa_insert_json-dtdemissao                        =  <lfs_insert_emp>-dt_demissao  .
        lwa_insert_json-txobs                             =  <lfs_insert_emp>-tx_obs .
        lwa_insert_json-enderecoempregado                 =  <lfs_insert_emp>-endereco_empregado .
        lwa_insert_json-cidadeempregado                   =  <lfs_insert_emp>-cidade_empregado .
        lwa_insert_json-cidadecodibge                     =  <lfs_insert_emp>-cidade_cod_ibge  .
        lwa_insert_json-bairroempregado                   =  <lfs_insert_emp>-bairro_empregado .
        lwa_insert_json-estadoempregado                   =  <lfs_insert_emp>-estado_empregado .
        lwa_insert_json-nrcep                             =  <lfs_insert_emp>-nr_cep .
        lwa_insert_json-dddcelular                        =  <lfs_insert_emp>-ddd_celular  .
        lwa_insert_json-nrcelular                         =  <lfs_insert_emp>-nr_celular .
        lwa_insert_json-dddtelefone                       =  <lfs_insert_emp>-ddd_telefone .
        lwa_insert_json-nrtelefone                        =  <lfs_insert_emp>-nr_telefone  .
        lwa_insert_json-remuneracaomensal                 =  <lfs_insert_emp>-remuneracao_mensal .
        lwa_insert_json-nomemae                           =  <lfs_insert_emp>-nome_mae .
        lwa_insert_json-tpfilprevidencia                  =  <lfs_insert_emp>-tp_fil_previdencia .
        lwa_insert_json-tpestadocivil                     =  <lfs_insert_emp>-tp_estado_civil  .
        lwa_insert_json-tpaposentado                      =  <lfs_insert_emp>-tp_aposentado  .
        lwa_insert_json-nreleitor                         =  <lfs_insert_emp>-nr_eleitor .
        lwa_insert_json-nrcnh                             =  <lfs_insert_emp>-nr_cnh .
        lwa_insert_json-dtvalcnh                          =  <lfs_insert_emp>-dt_val_cnh .
        lwa_insert_json-cdrfid                            =  <lfs_insert_emp>-cd_rfid  .
        lwa_insert_json-cdbarras                          =  <lfs_insert_emp>-cd_barras  .
        lwa_insert_json-gruposanguineo                    =  <lfs_insert_emp>-grupo_sanguineo  .
        lwa_insert_json-deficiencia                       =  <lfs_insert_emp>-deficiencia  .
        lwa_insert_json-tpdeficiencia                     =  <lfs_insert_emp>-tp_deficiencia .
        lwa_insert_json-email                             =  <lfs_insert_emp>-email  .
        lwa_insert_json-anulado                           =  <lfs_insert_emp>-anulado  .
        lwa_insert_json-motivoexcluido                    =  <lfs_insert_emp>-motivo_excluido  .
        lwa_insert_json-rne                               =  <lfs_insert_emp>-rne  .
        lwa_insert_json-nacionalidade                     =  <lfs_insert_emp>-nacionalidade  .
        lwa_insert_json-pais                              =  <lfs_insert_emp>-pais .


        READ TABLE <lfs_insert_emp>-setores_cargos-setor_cargo  ASSIGNING FIELD-SYMBOL(<lfs_setor_cargo>) INDEX 1.

        IF <lfs_setor_cargo> IS ASSIGNED.

          lwa_insert_json-setorcargo-tpmovimentacao               =    <lfs_setor_cargo>-tp_movimentacao.
          lwa_insert_json-setorcargo-dtinicio                     =    <lfs_setor_cargo>-dt_inicio.
          lwa_insert_json-setorcargo-dtsaida                      =    <lfs_setor_cargo>-dt_saida.
          lwa_insert_json-setorcargo-idsetor                      =    <lfs_setor_cargo>-id_setor.
          lwa_insert_json-setorcargo-cdsetor                      =    <lfs_setor_cargo>-cd_setor.
          lwa_insert_json-setorcargo-nomesetor                    =    <lfs_setor_cargo>-nome_setor.
          lwa_insert_json-setorcargo-cdsetordesenvolvido          =    <lfs_setor_cargo>-cd_setor_desenvolvido.
          lwa_insert_json-setorcargo-nomesetordesenvolvido        =    <lfs_setor_cargo>-nome_setor_desenvolvido.
          "lwa_insert_json-setorcargo-idcargo                      =    <lfs_setor_cargo>-id_cargo.
          lwa_insert_json-setorcargo-cdcargo                      =    <lfs_setor_cargo>-cd_cargo.
          lwa_insert_json-setorcargo-nomecargo                    =    <lfs_setor_cargo>-nome_cargo.
          lwa_insert_json-setorcargo-cdcargodesenvolvido          =    <lfs_setor_cargo>-cd_cargo_desenvolvido.
          lwa_insert_json-setorcargo-cargodesenvolvido            =    <lfs_setor_cargo>-cargo_desenvolvido.
          lwa_insert_json-setorcargo-cargocbo                     =    <lfs_setor_cargo>-cargo_cbo.
          lwa_insert_json-setorcargo-descsumariacargo             =    <lfs_setor_cargo>-desc_sumaria_cargo.
          lwa_insert_json-setorcargo-descdetalhadacargo           =    <lfs_setor_cargo>-desc_detalhada_cargo.
          lwa_insert_json-setorcargo-cdposicaotrabalho            =    <lfs_setor_cargo>-cd_posicao_trabalho.
          lwa_insert_json-setorcargo-nomeposicaotrabalho          =    <lfs_setor_cargo>-nome_posicao_trabalho.
          lwa_insert_json-setorcargo-descsumariaposicaotrabalho   =    <lfs_setor_cargo>-desc_sumaria_posicao_trabalho.
          lwa_insert_json-setorcargo-descdetalhadaposicaotrabalho =    <lfs_setor_cargo>-desc_detalhada_posicao_trabalh.

          APPEND lwa_insert_json TO s_data_json-empregadoservice-empregados[].
          CLEAR: lwa_insert_json.


          e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                      pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                     ).

          lwa_service-service = 'EmpregadoService'.
          lwa_service-json    = e_json.
          lwa_service-tipo    = 'insertEmpregadosRequest_TGG'.

          TRY .
              obj_rsdata_cpi->zif_integracao_outbound~get_instance(
              )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
              IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.

                DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.
                /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_emp_ins ).

                LOOP AT lit_ret_emp_ins-empregadosresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
                  "data(lva_index)  = sy-tabix.

                  READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.


                  TRY.
                      DATA(w_setor_cargo) = <lfs_insert_emp>-setores_cargos-setor_cargo[ 1 ].
                    CATCH cx_sy_itab_line_not_found.
                  ENDTRY.


                  DATA(w_log) = VALUE zhcmt_pa_0023(
                    servico         = 'InsertEmpregados - TGG'
                    data_envio      = sy-datum
                    hora_envio      = sy-uzeit
                    pernr           = <lfs_insert_emp>-nr_matricula
                    bukrs           = <lfs_insert_emp>-id_empresa
                    werks           = <lfs_insert_emp>-id_empresa
                    dados_enviados  = |{ w_setor_cargo-cd_cargo }{ w_setor_cargo-cd_posicao_trabalho }/{ w_setor_cargo-cd_setor }| ).

                  LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
                    w_log-retorno_servico = w_mensagem-txdescricao.
                    w_log-id_log = sy-tabix.
                    APPEND w_log TO it_log[].
                  ENDLOOP.

                ENDLOOP.
              ELSE.
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                     servico         = 'InsertEmpregados - TGG'
                     data_envio      = sy-datum
                     hora_envio      = sy-uzeit
                     pernr           = <lfs_insert_emp>-nr_matricula
                     bukrs           = <lfs_insert_emp>-id_empresa
                     werks           = <lfs_insert_emp>-id_empresa
                     dados_enviados  = |{ w_setor_cargo-cd_cargo }{ w_setor_cargo-cd_posicao_trabalho }/{ w_setor_cargo-cd_setor }| ).


                w_log-retorno_servico = 'Erro Insert Empregados - TGG - Classe'.
                w_log-id_log = sy-tabix.
                APPEND w_log TO it_log[].

              ENDIF.
            CATCH zcx_integracao INTO DATA(r_msg).

              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'InsertEmpregados - TGG'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_insert_emp>-nr_matricula
              retorno_servico = 'Erro Insert Empregados - TGG'  ).

              APPEND w_log TO it_log[].


            CATCH zcx_error INTO DATA(r_msg_error).
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'InsertEmpregados - TGG'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_insert_emp>-nr_matricula
              retorno_servico = 'Erro Insert Empregados TGG'  ).

              APPEND w_log TO it_log[].
          ENDTRY.
          CLEAR: s_data_json-empregadoservice-empregados, w_setor_cargo, lit_ret_emp_ins, lva_json_retorno, _lwa_ret_call,e_json.

          IF <lfs_setor_cargo> IS ASSIGNED.
            CLEAR <lfs_setor_cargo>.
          ENDIF.

          IF <lfs_insert_emp> IS ASSIGNED.
            CLEAR <lfs_insert_emp>.
          ENDIF.

          IF <lfs_emp_response> IS ASSIGNED.
            CLEAR <lfs_emp_response>.
          ENDIF.

          IF <lfs_w_mensagem_tab> IS ASSIGNED.
            CLEAR <lfs_w_mensagem_tab>.
          ENDIF.

        ELSE.
          "BREAK-POINT.
        ENDIF.


      ENDLOOP.

*--------------------------------------------------------------------------------------------------------------
*********************************************
**** Depois faz o insert dos cadidatos.******
*********************************************
      IF i_request_cpf-rsdata-empregados-empregado[] IS NOT INITIAL.

        CLEAR:  s_data_json.

        s_data_json-empregadoservice-usernametoken-username = ''."'integracao.db0801amaggiexp@rsdata.com.br'.
        s_data_json-empregadoservice-usernametoken-password = ''."'43a19edaac7b63358c252c544be06c49'.

        s_data_json-empregadoservice-config-tpverempregado  = 'CPF'.
        s_data_json-empregadoservice-config-tpversetorcargo = 'NOME'.
        s_data_json-empregadoservice-config-metodo          = 'insertEmpregadosRequest'.


        LOOP AT i_request_cpf-rsdata-empregados-empregado[]  ASSIGNING FIELD-SYMBOL(<lfs_insert_emp_cpf>).

          "lva_index = sy-tabix.

          lwa_insert_json-novocontrato                      =  <lfs_insert_emp_cpf>-novo_contrato  .
          lwa_insert_json-dttransferencia                   =  <lfs_insert_emp_cpf>-dt_transferencia .
          lwa_insert_json-idempresadestino                  =  <lfs_insert_emp_cpf>-id_empresa_destino .
          lwa_insert_json-statusempresadestino              =  <lfs_insert_emp_cpf>-status_empresa_destino .
          lwa_insert_json-codintegracaoempresadestino       =  <lfs_insert_emp_cpf>-cod_integracao_empresa_destino .
          lwa_insert_json-nrcnpjempresadestino              =  <lfs_insert_emp_cpf>-nr_cnpjempresa_destino .
          lwa_insert_json-razaosocialempresadestino         =  <lfs_insert_emp_cpf>-razao_social_empresa_destino .
          lwa_insert_json-denominacaoempresadestino         =  <lfs_insert_emp_cpf>-denominacao_empresa_destino  .
          "lwa_insert_json-NRMATRICULADESTINO                =  <lfs_insert_emp_cpf>- .
          "lwa_insert_json-MATRICULARHDESTINO                =  <lfs_insert_emp_cpf>- .
          lwa_insert_json-forcartransferencia               =  <lfs_insert_emp_cpf>-forcar_transferencia .
          lwa_insert_json-statusempresa                     =  <lfs_insert_emp_cpf>-status_empresa .
          lwa_insert_json-idempresa                         =  <lfs_insert_emp_cpf>-id_empresa .
          lwa_insert_json-codintegracaoempresa              =  <lfs_insert_emp_cpf>-cod_integracao_empresa .
          lwa_insert_json-nrcnpjempresa                     =  <lfs_insert_emp_cpf>-nr_cnpjempresa .
          lwa_insert_json-razaosocialempresa                =  <lfs_insert_emp_cpf>-razao_social_empresa .
          lwa_insert_json-denominacaoempresa                =  <lfs_insert_emp_cpf>-denominacao_empresa  .
          lwa_insert_json-idempregado                       =  <lfs_insert_emp_cpf>-id_empregado .
          lwa_insert_json-codintegracaoempregado            =  <lfs_insert_emp_cpf>-cod_integracao_empregado .
          lwa_insert_json-nomeempregado                     =  <lfs_insert_emp_cpf>-nome_empregado .
          lwa_insert_json-carteiratrabalhodigital           =  <lfs_insert_emp_cpf>-carteira_trabalho_digital  .
          lwa_insert_json-dtnascimento                      =  <lfs_insert_emp_cpf>-dt_nascimento  .
          lwa_insert_json-tpsexo                            =  <lfs_insert_emp_cpf>-tp_sexo  .
          "lwa_insert_json-raca                              =  <lfs_insert_emp_cpf>- .
          lwa_insert_json-nrctps                            =  <lfs_insert_emp_cpf>-nr_ctps  .
          lwa_insert_json-nrseriectps                       =  <lfs_insert_emp_cpf>-nr_serie_ctps  .
          lwa_insert_json-dtemictps                         =  <lfs_insert_emp_cpf>-dt_emi_ctps  .
          lwa_insert_json-ufemictps                         =  <lfs_insert_emp_cpf>-uf_emi_ctps  .
          lwa_insert_json-nridentidade                      =  <lfs_insert_emp_cpf>-nr_identidade  .
          lwa_insert_json-orgaoexpedidorrg                  =  <lfs_insert_emp_cpf>-orgao_expedidor_rg .
          lwa_insert_json-dtemirg                           =  <lfs_insert_emp_cpf>-dt_emi_rg  .
          lwa_insert_json-ufemirg                           =  <lfs_insert_emp_cpf>-uf_emi_rg  .
          lwa_insert_json-nrnit                             =  <lfs_insert_emp_cpf>-nr_nit .
          lwa_insert_json-nrcpf                             =  <lfs_insert_emp_cpf>-nr_cpf .
          lwa_insert_json-nrmatricula                       =  <lfs_insert_emp_cpf>-nr_matricula .
          lwa_insert_json-categoriatrabalhador              =  <lfs_insert_emp_cpf>-categoria_trabalhador  .
          lwa_insert_json-tpvinculo                         =  <lfs_insert_emp_cpf>-tp_vinculo .
          lwa_insert_json-br_pdh                            =  <lfs_insert_emp_cpf>-br_pdh .
          lwa_insert_json-regimerevezamento                 =  <lfs_insert_emp_cpf>-regime_revezamento .
          lwa_insert_json-dtadmissao                        =  <lfs_insert_emp_cpf>-dt_admissao  .
          lwa_insert_json-dtdemissao                        =  <lfs_insert_emp_cpf>-dt_demissao  .
          lwa_insert_json-txobs                             =  <lfs_insert_emp_cpf>-tx_obs .
          lwa_insert_json-enderecoempregado                 =  <lfs_insert_emp_cpf>-endereco_empregado .
          lwa_insert_json-cidadeempregado                   =  <lfs_insert_emp_cpf>-cidade_empregado .
          lwa_insert_json-cidadecodibge                     =  <lfs_insert_emp_cpf>-cidade_cod_ibge  .
          lwa_insert_json-bairroempregado                   =  <lfs_insert_emp_cpf>-bairro_empregado .
          lwa_insert_json-estadoempregado                   =  <lfs_insert_emp_cpf>-estado_empregado .
          lwa_insert_json-nrcep                             =  <lfs_insert_emp_cpf>-nr_cep .
          lwa_insert_json-dddcelular                        =  <lfs_insert_emp_cpf>-ddd_celular  .
          lwa_insert_json-nrcelular                         =  <lfs_insert_emp_cpf>-nr_celular .
          lwa_insert_json-dddtelefone                       =  <lfs_insert_emp_cpf>-ddd_telefone .
          lwa_insert_json-nrtelefone                        =  <lfs_insert_emp_cpf>-nr_telefone  .
          lwa_insert_json-remuneracaomensal                 =  <lfs_insert_emp_cpf>-remuneracao_mensal .
          lwa_insert_json-nomemae                           =  <lfs_insert_emp_cpf>-nome_mae .
          lwa_insert_json-tpfilprevidencia                  =  <lfs_insert_emp_cpf>-tp_fil_previdencia .
          lwa_insert_json-tpestadocivil                     =  <lfs_insert_emp_cpf>-tp_estado_civil  .
          lwa_insert_json-tpaposentado                      =  <lfs_insert_emp_cpf>-tp_aposentado  .
          lwa_insert_json-nreleitor                         =  <lfs_insert_emp_cpf>-nr_eleitor .
          lwa_insert_json-nrcnh                             =  <lfs_insert_emp_cpf>-nr_cnh .
          lwa_insert_json-dtvalcnh                          =  <lfs_insert_emp_cpf>-dt_val_cnh .
          lwa_insert_json-cdrfid                            =  <lfs_insert_emp_cpf>-cd_rfid  .
          lwa_insert_json-cdbarras                          =  <lfs_insert_emp_cpf>-cd_barras  .
          lwa_insert_json-gruposanguineo                    =  <lfs_insert_emp_cpf>-grupo_sanguineo  .
          lwa_insert_json-deficiencia                       =  <lfs_insert_emp_cpf>-deficiencia  .
          lwa_insert_json-tpdeficiencia                     =  <lfs_insert_emp_cpf>-tp_deficiencia .
          lwa_insert_json-email                             =  <lfs_insert_emp_cpf>-email  .
          lwa_insert_json-anulado                           =  <lfs_insert_emp_cpf>-anulado  .
          lwa_insert_json-motivoexcluido                    =  <lfs_insert_emp_cpf>-motivo_excluido  .
          lwa_insert_json-rne                               =  <lfs_insert_emp_cpf>-rne  .
          lwa_insert_json-nacionalidade                     =  <lfs_insert_emp_cpf>-nacionalidade  .
          lwa_insert_json-pais                              =  <lfs_insert_emp_cpf>-pais .


          READ TABLE <lfs_insert_emp_cpf>-setores_cargos-setor_cargo  ASSIGNING FIELD-SYMBOL(<lfs_setor_cargo_cpf>) INDEX 1.

          IF <lfs_setor_cargo_cpf> IS ASSIGNED.

            lwa_insert_json-setorcargo-tpmovimentacao               =    <lfs_setor_cargo_cpf>-tp_movimentacao.
            lwa_insert_json-setorcargo-dtinicio                     =    <lfs_setor_cargo_cpf>-dt_inicio.
            lwa_insert_json-setorcargo-dtsaida                      =    <lfs_setor_cargo_cpf>-dt_saida.
            lwa_insert_json-setorcargo-idsetor                      =    <lfs_setor_cargo_cpf>-id_setor.
            lwa_insert_json-setorcargo-cdsetor                      =    <lfs_setor_cargo_cpf>-cd_setor.
            lwa_insert_json-setorcargo-nomesetor                    =    <lfs_setor_cargo_cpf>-nome_setor.
            lwa_insert_json-setorcargo-cdsetordesenvolvido          =    <lfs_setor_cargo_cpf>-cd_setor_desenvolvido.
            lwa_insert_json-setorcargo-nomesetordesenvolvido        =    <lfs_setor_cargo_cpf>-nome_setor_desenvolvido.
            "lwa_insert_json-setorcargo-idcargo                      =    <lfs_setor_cargo_cpf>-id_cargo.
            lwa_insert_json-setorcargo-cdcargo                      =    <lfs_setor_cargo_cpf>-cd_cargo.
            lwa_insert_json-setorcargo-nomecargo                    =    <lfs_setor_cargo_cpf>-nome_cargo.
            lwa_insert_json-setorcargo-cdcargodesenvolvido          =    <lfs_setor_cargo_cpf>-cd_cargo_desenvolvido.
            lwa_insert_json-setorcargo-cargodesenvolvido            =    <lfs_setor_cargo_cpf>-cargo_desenvolvido.
            lwa_insert_json-setorcargo-cargocbo                     =    <lfs_setor_cargo_cpf>-cargo_cbo.
            lwa_insert_json-setorcargo-descsumariacargo             =    <lfs_setor_cargo_cpf>-desc_sumaria_cargo.
            lwa_insert_json-setorcargo-descdetalhadacargo           =    <lfs_setor_cargo_cpf>-desc_detalhada_cargo.
            lwa_insert_json-setorcargo-cdposicaotrabalho            =    <lfs_setor_cargo_cpf>-cd_posicao_trabalho.
            lwa_insert_json-setorcargo-nomeposicaotrabalho          =    <lfs_setor_cargo_cpf>-nome_posicao_trabalho.
            lwa_insert_json-setorcargo-descsumariaposicaotrabalho   =    <lfs_setor_cargo_cpf>-desc_sumaria_posicao_trabalho.
            lwa_insert_json-setorcargo-descdetalhadaposicaotrabalho =    <lfs_setor_cargo_cpf>-desc_detalhada_posicao_trabalh.

            APPEND lwa_insert_json TO s_data_json-empregadoservice-empregados[].
            CLEAR: lwa_insert_json.

            CLEAR: e_json.
            e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                       ).

            lwa_service-service = 'EmpregadoService'.
            lwa_service-json    = e_json.
            lwa_service-tipo    = 'insertEmpregadosRequestCPF_TGG'.

            CLEAR: lva_json_retorno,lit_ret_emp_ins.

*** Envia dados para o RSDATA
            TRY .
                obj_rsdata_cpi->zif_integracao_outbound~get_instance(
                )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call_cand)  ).
                IF _lwa_ret_call_cand IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
                  lva_json_retorno = _lwa_ret_call-ds_data_retorno.
                  /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_emp_ins ).

                  LOOP AT lit_ret_emp_ins-empregadosresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response_cand>).
                    "
                    DATA(lva_index_cand) = sy-tabix.

                    READ TABLE <lfs_emp_response_cand>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab_cand>) INDEX lva_index_cand.

                    TRY.
                        DATA(w_setor_cargo_cpf) = <lfs_insert_emp_cpf>-setores_cargos-setor_cargo[ 1 ].
                      CATCH cx_sy_itab_line_not_found.
                    ENDTRY.

                    CLEAR: w_log.
                    w_log = VALUE zhcmt_pa_0023(
                       servico         = 'InsertEmpregados TGG'
                       data_envio      = sy-datum
                       hora_envio      = sy-uzeit
                       pernr           = <lfs_insert_emp_cpf>-nr_matricula
                       bukrs           = <lfs_insert_emp_cpf>-id_empresa
                       werks           = <lfs_insert_emp_cpf>-id_empresa
                       dados_enviados  = |{ w_setor_cargo_cpf-cd_cargo }{ w_setor_cargo_cpf-cd_posicao_trabalho }/{ w_setor_cargo_cpf-cd_setor }| ).

                    LOOP AT <lfs_w_mensagem_tab_cand>-mensagemdet INTO DATA(w_mensagem_cand).
                      w_log-retorno_servico = w_mensagem_cand-txdescricao.
                      w_log-id_log = sy-tabix.
                      APPEND w_log TO it_log[].
                    ENDLOOP.
                  ENDLOOP.
                ELSE.
                  CLEAR: w_log.
                  w_log = VALUE zhcmt_pa_0023(
                  servico         = 'InsertEmpregados TGG'
                  data_envio      = sy-datum
                  hora_envio      = sy-uzeit
                  pernr           = <lfs_insert_emp_cpf>-nr_matricula
                  retorno_servico = 'Erro Insert Empregados Candidato TGG'  ).
                ENDIF.
              CATCH zcx_integracao INTO DATA(r_msg_cand).
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                servico         = 'InsertEmpregados TGG'
                data_envio      = sy-datum
                hora_envio      = sy-uzeit
                pernr           = <lfs_insert_emp_cpf>-nr_matricula
                retorno_servico = 'Erro Insert Empregados Candidato TGG'  ).

                APPEND w_log TO it_log[].
              CATCH zcx_error INTO DATA(r_msg_error_cand).
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                servico         = 'InsertEmpregados TGG'
                data_envio      = sy-datum
                hora_envio      = sy-uzeit
                pernr           = <lfs_insert_emp_cpf>-nr_matricula
                retorno_servico = 'Erro Insert Empregados Candidato TGG'  ).

                APPEND w_log TO it_log[].
            ENDTRY.

            CLEAR: w_setor_cargo_cpf.

            IF <lfs_setor_cargo_cpf> IS ASSIGNED.
              CLEAR <lfs_setor_cargo_cpf>.
            ENDIF.

            IF <lfs_insert_emp_cpf> IS ASSIGNED.
              CLEAR <lfs_insert_emp_cpf>.
            ENDIF.

            IF <lfs_emp_response_cand> IS ASSIGNED.
              CLEAR <lfs_emp_response_cand>.
            ENDIF.

            IF <lfs_w_mensagem_tab_cand> IS ASSIGNED.
              CLEAR: <lfs_w_mensagem_tab_cand>.
            ENDIF.
          ELSE.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "-> Grava log do envio do serviço 'insertEmpregados':
    IF ( it_log[] IS NOT INITIAL ).
      MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
    ENDIF.

  ENDMETHOD.


  METHOD insert_empregados_turnos.

    TYPES: ty_rg_schkz TYPE RANGE OF pa0007-schkz.

    DATA: s_insert_turnos_reques TYPE zrsdatamovinsert_empregados_t1,
          zcl_proxy              TYPE REF TO zrsdatamovco_movimento_emprega,
          it_log                 TYPE TABLE OF zhcmt_pa_0023,
          v_cnpj                 TYPE char20,
          v_data_ini             TYPE char20,
          v_data_fim             TYPE char20,
          v_hora_ini             TYPE char20,
          v_hora_fim             TYPE char20.

    DATA: s_data_json TYPE zhcms_rsdata_empturnos_service,
          e_json      TYPE string.

    DATA: lit_ret_ferias TYPE zhcms_rsdata_emp_ferias_resp.
    DATA: lwa_service TYPE zde_rs_data_cpi.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    IF ( me->at_it_dados_empregado IS NOT INITIAL ).

      "-> Buscar horários para serem enviados no serviço insertEmpregadosTurnos ()
      SELECT p7~pernr,
             p7~begda,
             p7~endda,
             p7~schkz
        FROM pa0007 AS p7
        INTO TABLE @DATA(it_p0007)
        WHERE p7~pernr IN @me->at_rg_pernr[]
          AND p7~endda >= @me->at_data_base
          AND p7~aedtm = @me->at_data_base
          ORDER BY pernr ASCENDING.

      CHECK it_p0007[] IS NOT INITIAL.

      DATA(r_schkz) = VALUE ty_rg_schkz( FOR w_schkz IN it_p0007[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_schkz-schkz+0(4) )  ).
      SORT r_schkz[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM r_schkz[] COMPARING low.

      "-> Criar range com matrículas que tiveram alteração de horário
      DATA(r_pernr_0007) = VALUE ty_rg_pernr( FOR w_0007 IN it_p0007[] (
                                        sign = 'I'
                                        option = 'EQ'
                                        low = w_0007-pernr )  ).
      SORT r_pernr_0007[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM r_pernr_0007[] COMPARING low.


      SELECT t51~zmodn,
             t51~tprg1,
             t51~tprg2,
             t51~tprg3,
             t50~sobeg,
             t50~soend,
             t51s~ztext
        FROM t551a AS t51
        LEFT JOIN t550a AS t50
        ON t50~tprog = t51~tprg1 OR
           t50~tprog = t51~tprg2
        LEFT JOIN t551s AS t51s
        ON t51s~zmodn = t51~zmodn
        INTO TABLE @DATA(it_horarios)
        WHERE t51~motpr = '37'
          AND t51~zmodn IN @r_schkz[]
          AND ( t51~tprg1 <> 'COMP' OR
                t51~tprg1 <> 'DESC' OR
                t51~tprg2 <> 'COMP' OR
                t51~tprg2 <> 'DESC' )
          AND t50~motpr = '37'
          AND t50~endda >= @me->at_data_base
          AND t50~begda <= @me->at_data_base
        ORDER BY t51~zmodn ASCENDING.

      "-> Envio de Turnos - insertEmpregadosTurnos ()
      SORT me->at_it_dados_empregado[] BY bukrs werks pernr ASCENDING.
      DATA(it_empregados_alterados) = me->at_it_dados_empregado[].
      IF ( it_empregados_alterados[] IS NOT INITIAL ).
        DELETE it_empregados_alterados[] WHERE pernr NOT IN r_pernr_0007[].
      ENDIF.

      LOOP AT me->at_it_dados_empresa[] INTO DATA(w_empresa).

        IF NOT ( line_exists( it_empregados_alterados[ bukrs = w_empresa-bukrs
                                                       werks = w_empresa-werks ] ) ).
          CONTINUE.
        ENDIF.

        "->Criando estrutura para envio de Turnos
        APPEND INITIAL LINE TO s_data_json-movimentoservice-dados[] ASSIGNING FIELD-SYMBOL(<w_turno_emp>).

        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = w_empresa-stcd1
          IMPORTING
            output = v_cnpj.

        <w_turno_emp>-nrcnpjempresa = v_cnpj.

        LOOP AT it_empregados_alterados[] INTO DATA(w_func) WHERE bukrs = w_empresa-bukrs
                                                              AND werks = w_empresa-werks.


          READ TABLE it_p0007[] INTO DATA(w_p0007) WITH KEY pernr = w_func-pernr BINARY SEARCH.
          IF ( sy-subrc = 0 ).

            READ TABLE it_horarios[] INTO DATA(w_horario) WITH KEY zmodn = w_p0007-schkz BINARY SEARCH.
            IF ( sy-subrc <> 0 ).
              CONTINUE.
            ENDIF.

            APPEND INITIAL LINE TO <w_turno_emp>-empregado[] ASSIGNING FIELD-SYMBOL(<w_turno_func>).
            APPEND INITIAL LINE TO <w_turno_func>-turnos[] ASSIGNING FIELD-SYMBOL(<w_turno>).

            <w_turno_func>-matricula = w_func-pernr.

            IF ( w_horario IS NOT INITIAL ).

              <w_turno>-nome = w_horario-ztext.

              WRITE w_p0007-begda TO v_data_ini DD/MM/YYYY.
              WRITE w_p0007-endda TO v_data_fim DD/MM/YYYY.

              REPLACE ALL OCCURRENCES OF '.' IN v_data_ini WITH '/'.
              REPLACE ALL OCCURRENCES OF '.' IN v_data_fim WITH '/'.

              <w_turno>-datainicio = v_data_ini.
              <w_turno>-datafim = COND #( WHEN w_p0007-endda <> '99991231' THEN v_data_fim ELSE '' ).

              v_hora_ini = |{ w_horario-sobeg+0(2) }:{ w_horario-sobeg+2(2) }|.
              v_hora_fim = |{ w_horario-soend+0(2) }:{ w_horario-soend+2(2) }|.

              <w_turno>-horainicio = COND #(
                  WHEN v_hora_ini <> 'COMP' AND v_hora_ini <> 'DESC' THEN
                       v_hora_ini ).

              <w_turno>-horafim = COND #(
                  WHEN v_hora_fim <> 'COMP' AND v_hora_fim <> 'DESC' THEN
                       v_hora_fim ).

            ENDIF.

            CLEAR: v_data_ini, v_data_fim, w_p0007, w_horario.

          ENDIF.


          DELETE s_data_json-movimentoservice-dados[] WHERE empregado IS INITIAL.

          IF s_data_json-movimentoservice-dados[] IS NOT INITIAL.


            s_data_json-movimentoservice-usernametoken-username = 'integracao.db0801amaggiexp@rsdata.com.br'.
            s_data_json-movimentoservice-usernametoken-password = '43a19edaac7b63358c252c544be06c49'.
            s_data_json-movimentoservice-config-metodo          = 'insertEmpregadosTurnos'.

            e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                        pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                       ).

            lwa_service-service = 'MovimentoService'.
            lwa_service-json    = e_json.
            lwa_service-tipo    = 'insertEmpregadosTurnos_TGG'.
            "-> Envio das informações
*** Chamar classe RSDATA que envia os dados.
            TRY .
                obj_rsdata_cpi->zif_integracao_outbound~get_instance(
                )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
                IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
                  DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.

                  /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_ferias ).

                  IF lit_ret_ferias-insertferiasresponse-mensagem[] IS NOT INITIAL.
                    LOOP AT lit_ret_ferias-insertferiasresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
                      READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.
                      DATA(w_log) = VALUE zhcmt_pa_0023(
                          servico         = 'Insert_Turnos'
                          data_envio      = sy-datum
                          hora_envio      = sy-uzeit
                          pernr           = CONV #( <w_turno_func>-matricula )
                          bukrs           = ''
                          werks           = ''
                          dados_enviados  = |{ <w_turno>-datainicio }{ <w_turno>-datafim }/{ <w_turno>-horainicio }/{ <w_turno>-horafim }/{ <w_turno>-jornadatrabalho }| ).

                      LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
                        w_log-retorno_servico = w_mensagem-txdescricao.
                        w_log-id_log = sy-tabix.
                        APPEND w_log TO it_log[].
                      ENDLOOP.

                    ENDLOOP.
                  ELSE.
                    IF _lwa_ret_call-ds_data_retorno IS NOT INITIAL.
                      CLEAR: w_log.
                      w_log = VALUE zhcmt_pa_0023(
                          servico         = 'Insert_Turnos'
                          data_envio      = sy-datum
                          hora_envio      = sy-uzeit
                          pernr           = CONV #( <w_turno_func>-matricula )
                          bukrs           = ''
                          werks           = ''
                          dados_enviados = _lwa_ret_call-ds_data_retorno ).
                    ENDIF.
                  ENDIF.

                ENDIF.

              CATCH zcx_integracao.
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                servico         = 'Insert_Turnos'
                data_envio      = sy-datum
                hora_envio      = sy-uzeit
                pernr           = <w_turno_func>-matricula
                retorno_servico = 'Erro Insert Turnos'  ).

                APPEND w_log TO it_log[].
              CATCH zcx_error.
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                servico         = 'IInsert_Turnos'
                data_envio      = sy-datum
                hora_envio      = sy-uzeit
                pernr           = <w_turno_func>-matricula
                retorno_servico = 'Erro Insert Turnos'  ).

                APPEND w_log TO it_log[].
            ENDTRY.
          ENDIF.

          CLEAR: s_data_json-movimentoservice-dados[], lva_json_retorno, lit_ret_ferias, e_json, lwa_service.

        ENDLOOP.

        CLEAR: v_cnpj, w_func.

      ENDLOOP.

      IF ( it_log[] IS NOT INITIAL ).
        MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD insert_empresa.

    TYPES: ty_r_bukrs TYPE RANGE OF pa0001-bukrs,
           ty_r_werks TYPE RANGE OF pa0001-werks.


    DATA: s_data_json     TYPE zhcms_rsdata_empresa_insert,
          lit_empresa     TYPE STANDARD TABLE OF ty_insert_empresa,
          lwa_empresa     TYPE ty_insert_empresa,
          lit_ret_empresa TYPE zhcms_rsdata_empresa_resp,
          e_json          TYPE string.

    DATA: lwa_service TYPE zde_rs_data_cpi.

    "-> Estrutura da requisição para o método InsertEmpresa
    DATA: s_req_insert_empresa TYPE zrsempresainsert_empresas_requ,
          it_log               TYPE TABLE OF zhcmt_pa_0023,
          v_cnpj               TYPE char20,
          v_paval              TYPE t001z-paval,
          v_cgc_branch         TYPE j_1bbranch-cgc_branch,
          v_cgc_number         TYPE j_1bwfield-cgc_number,
          v_cgc_company        TYPE j_1bwfield-cgc_compan,
          v_cgc_branch_aux     TYPE j_1bwfield-cgc_branch.

    "DATA: zcl_proxy  TYPE REF TO zrsempresaco_empresa_port.

    DATA: zcl_rsdata TYPE REF TO zcl_hcm_rsdata_tgg_cpi.
    CREATE OBJECT zcl_rsdata.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.


    DATA(v_data_base) = sy-datum.

    IF ( me->at_it_dados_empregado[] IS NOT INITIAL ). " IT_MATRICULAS_ATIVAS[]

      SORT me->at_it_dados_empregado[] BY bukrs werks ASCENDING.
      "-> Cria range de empresas encontradas
      DATA(rg_bukrs) = VALUE ty_r_bukrs( FOR _line IN me->at_it_dados_empregado[] (
          sign   = 'I'
          option = 'EQ'
          low    = _line-bukrs
          high    = _line-bukrs
      ) ).
      SORT rg_bukrs[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM rg_bukrs COMPARING low.

      DATA(rg_werks) = VALUE ty_r_werks( FOR _line IN me->at_it_dados_empregado[] (
          sign   = 'I'
          option = 'EQ'
          low    = _line-werks
          high    = _line-werks
      ) ).

      "-> Buscar matriz de cada empresa:
      IF ( rg_bukrs[] IS NOT INITIAL ).

        SELECT tb1~bukrs,
               tb1~filia
            FROM t7brb1 AS tb1
            INTO TABLE @DATA(it_matriz)
            WHERE tb1~bukrs IN @rg_bukrs[] AND
                  tb1~endda >= @sy-datum AND
                  tb1~begda <= @sy-datum AND
                  tb1~mtriz = @abap_true.
        SORT it_matriz[] BY bukrs ASCENDING.

        LOOP AT it_matriz[] ASSIGNING FIELD-SYMBOL(<lfs_matriz>).
          APPEND INITIAL LINE TO rg_werks[] ASSIGNING FIELD-SYMBOL(<lfs_werks>).
          <lfs_werks>-sign = 'I'.
          <lfs_werks>-option = 'EQ'.
          <lfs_werks>-low = <lfs_matriz>-filia.
          <lfs_werks>-high = <lfs_matriz>-filia.
        ENDLOOP.

        SORT rg_werks[] BY low ASCENDING.
        DELETE ADJACENT DUPLICATES FROM rg_werks COMPARING low.

      ENDIF.

      SELECT  j1~bukrs,
              j1~branch AS werks,
              j1~name,
              j1~cgc_branch,
              j1~state_insc,
              j1~adrnr,
              j1~stcd1,
              ad~name1 AS name_adr,
              ad~city1,
              ad~city2 AS bairro,
              ad~post_code1,
              ad~street,
              ad~house_num1,
              ad~region AS uf,
              ad~tel_number,
              tae~econi AS cnae
        FROM j_1bbranch AS j1
        LEFT JOIN adrc AS ad
        ON ad~addrnumber = j1~adrnr
        LEFT JOIN t7brb1 AS tb1
        ON tb1~bukrs = j1~bukrs
        AND tb1~filia = j1~branch
        LEFT JOIN t7brae AS tae
        ON tae~econa = tb1~econa
        INTO TABLE @DATA(it_dados_empresa)
        WHERE j1~bukrs IN @rg_bukrs[]
          AND j1~branch IN @rg_werks[]
          AND ad~date_to >= @v_data_base
          AND ad~langu = @sy-langu
          AND tb1~endda >= @v_data_base
          AND tae~endda >= @v_data_base.
      SORT it_dados_empresa[] BY bukrs werks ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_dados_empresa[] COMPARING bukrs werks.

      IF ( it_dados_empresa[] IS NOT INITIAL ).

        LOOP AT it_dados_empresa[] ASSIGNING FIELD-SYMBOL(<lfs_dados_empresa>).

*          APPEND INITIAL LINE TO s_req_insert_empresa-detalhes_empresas-detalhe_empresa[]
*            ASSIGNING FIELD-SYMBOL(<w_empresa>).

          APPEND INITIAL LINE TO s_data_json-empresaservice-detalheempresa[]
             ASSIGNING FIELD-SYMBOL(<w_empresa>).

          "-> número de empregados na filial:
          DATA(v_count_empregados) = REDUCE i( INIT x = 0 FOR wa IN me->at_it_dados_empregado[]
            WHERE ( werks = <lfs_dados_empresa>-werks ) NEXT x = x + 1 ).

          "-> buscando matriz da empresa:
          TRY.
              DATA(w_emp_matriz) = it_matriz[ bukrs = <lfs_dados_empresa>-bukrs ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          <w_empresa>-tipovalidacao = '1'.
          <w_empresa>-tipoempresa = COND #( WHEN <lfs_dados_empresa>-werks = w_emp_matriz-filia THEN '0' ELSE '1' ).

          IF ( <w_empresa>-tipoempresa = '1' ).

            LOOP AT it_dados_empresa[] INTO DATA(w_matriz) WHERE werks = w_emp_matriz-filia.

              CLEAR: v_paval, v_cgc_branch, v_cgc_number, v_cgc_company, v_cgc_branch_aux.
              SELECT SINGLE paval FROM t001z
                 INTO @v_paval
                 WHERE bukrs = @w_matriz-bukrs
                 AND   party = 'J_1BCG'.

              SELECT SINGLE cgc_branch FROM j_1bbranch
                 INTO @v_cgc_branch
                 WHERE bukrs = @w_matriz-bukrs
                 AND   branch = @w_matriz-werks.


              MOVE   v_paval       TO  v_cgc_company.
              MOVE   v_cgc_branch  TO  v_cgc_branch_aux.

              CALL FUNCTION 'J_1BBUILD_CGC'
                EXPORTING
                  cgc_company = v_cgc_company
                  cgc_branch  = v_cgc_branch_aux
                IMPORTING
                  cgc_number  = v_cgc_number.

              <w_empresa>-cnpjmatriz = v_cgc_number.
              <w_empresa>-razaosocialmatriz = w_matriz-name_adr.
              <w_empresa>-fantasiamatriz = w_matriz-name.

              EXIT.

            ENDLOOP.

            CLEAR: v_cnpj.

          ENDIF.

*** BUG - 48856 - Inicio - Camila Brand
*<w_empresa>-nr_cnpj = w_empresa-stcd1. "v_cnpj.
          CLEAR: v_paval, v_cgc_branch, v_cgc_number, v_cgc_company, v_cgc_branch_aux.
          SELECT SINGLE paval FROM t001z
             INTO @v_paval
             WHERE bukrs = @<lfs_dados_empresa>-bukrs
             AND   party = 'J_1BCG'.

          SELECT SINGLE cgc_branch FROM j_1bbranch
             INTO @v_cgc_branch
             WHERE bukrs = @<lfs_dados_empresa>-bukrs
             AND   branch = @<lfs_dados_empresa>-werks.

          MOVE   v_paval       TO  v_cgc_company.
          MOVE   v_cgc_branch  TO  v_cgc_branch_aux.

          CALL FUNCTION 'J_1BBUILD_CGC'
            EXPORTING
              cgc_company = v_cgc_company
              cgc_branch  = v_cgc_branch_aux
            IMPORTING
              cgc_number  = v_cgc_number.

          <w_empresa>-nrcnpj = v_cgc_number.
          <lfs_dados_empresa>-stcd1 = v_cgc_number.

*** BUG - 48856 - Fim - Camila Brand

          <w_empresa>-tipomatricula = 'J'.
          <w_empresa>-razaosocial = <lfs_dados_empresa>-name_adr.
          <w_empresa>-fantasia = <lfs_dados_empresa>-name.
          <w_empresa>-endereco = <lfs_dados_empresa>-street.
          <w_empresa>-uf = <lfs_dados_empresa>-uf.
          REPLACE ALL OCCURRENCES OF '-' IN <lfs_dados_empresa>-post_code1 WITH ''.
          <w_empresa>-cep = <lfs_dados_empresa>-post_code1.
          <w_empresa>-cidade = <lfs_dados_empresa>-city1.
          <w_empresa>-bairro = <lfs_dados_empresa>-bairro.
          <w_empresa>-fone = <lfs_dados_empresa>-tel_number.
          <w_empresa>-ie = <lfs_dados_empresa>-state_insc.
          "<w_empresa>-homepage = 'https://www.amaggi.com.br/'.
          <w_empresa>-empregados = v_count_empregados.
          <w_empresa>-cnae = <lfs_dados_empresa>-cnae.
          "<W_EMPRESA>-MATRICULA_RH = '1'.

          CLEAR: v_cnpj, v_count_empregados.

        ENDLOOP.

        me->at_it_dados_empresa[] = CORRESPONDING #( it_dados_empresa[] ).

        " IF ( s_req_insert_empresa IS NOT INITIAL ).
        IF s_data_json-empresaservice-detalheempresa[] IS NOT INITIAL.

          DELETE s_data_json-empresaservice-detalheempresa[] WHERE nrcnpj IS INITIAL.


          s_data_json-empresaservice-usernametoken-username = 'integracao.db0801amaggiexp@rsdata.com.br'.
          s_data_json-empresaservice-usernametoken-password = '43a19edaac7b63358c252c544be06c49'.
          s_data_json-empresaservice-config-metodo          = 'insertempresas'.

          e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                      pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                     ).

          lwa_service-service = 'EmpresaService'.
          lwa_service-json    = e_json.
          lwa_service-tipo    = 'insertempresas_TGG'.

*** Chamar classe RSDATA que envia os dados.
          TRY .
              obj_rsdata_cpi->zif_integracao_outbound~get_instance(
              )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
              IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
                DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.

                /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_empresa ).

                LOOP AT lit_ret_empresa-insertempresasresponse-rsdatareturn[] ASSIGNING FIELD-SYMBOL(<lfs_empresa>).

                  DATA(lva_index) = sy-tabix.
                  READ TABLE me->at_it_dados_empresa[] INTO DATA(w_dados_empresa) WITH KEY stcd1 = <lfs_empresa>-nrcnpjempresa.

                  DATA(w_log) = VALUE zhcmt_pa_0023(
                      servico         = 'insertEmpresa'
                      data_envio      = sy-datum
                      hora_envio      = sy-uzeit
                      pernr           = '00000000'
                      id_log          = sy-tabix
                      bukrs           = w_dados_empresa-bukrs
                      werks           = w_dados_empresa-werks
                      dados_enviados  = |{ lwa_empresa-razaosocial }-{ lwa_empresa-fantasia }-{ lwa_empresa-empregados }| ).

                  LOOP AT <lfs_empresa>-mensagemdet INTO DATA(w_mensagem_tab).

                    w_log-retorno_servico = w_mensagem_tab-txdescricao.
                    APPEND w_log TO it_log[].
                    CLEAR: w_log.

                  ENDLOOP.
                ENDLOOP.
              ELSE.
                w_log = VALUE zhcmt_pa_0023(
                 servico         = 'insertEmpresa'
                 data_envio      = sy-datum
                 hora_envio      = sy-uzeit
                 pernr           = '00000000'
                 id_log          = sy-tabix
                 bukrs           = ''
                 werks           = ''
                 dados_enviados  = |{ lwa_empresa-razaosocial }-{ lwa_empresa-fantasia }-{ lwa_empresa-empregados }| ).

                w_log-retorno_servico = 'Erro Insert Empresa TGG'.

                APPEND w_log TO it_log[].
                CLEAR: w_log.
              ENDIF.
            CATCH zcx_integracao.
              w_log = VALUE zhcmt_pa_0023(
               servico         = 'insertEmpresa'
               data_envio      = sy-datum
               hora_envio      = sy-uzeit
               pernr           = '00000000'
               id_log          = sy-tabix
               bukrs           = ''
               werks           = ''
               dados_enviados  = |{ lwa_empresa-razaosocial }-{ lwa_empresa-fantasia }-{ lwa_empresa-empregados }| ).

              w_log-retorno_servico = 'Erro Insert Empresa TGG'.

              APPEND w_log TO it_log[].
              CLEAR: w_log.

            CATCH zcx_error.
              w_log = VALUE zhcmt_pa_0023(
               servico         = 'insertEmpresa'
               data_envio      = sy-datum
               hora_envio      = sy-uzeit
               pernr           = '00000000'
               id_log          = sy-tabix
               bukrs           = ''
               werks           = ''
               dados_enviados  = |{ lwa_empresa-razaosocial }-{ lwa_empresa-fantasia }-{ lwa_empresa-empregados }| ).

              w_log-retorno_servico = 'Erro Insert Empresa - TGG'.

              APPEND w_log TO it_log[].
              CLEAR: w_log.
          ENDTRY.

          IF ( it_log[] IS NOT INITIAL ).
            MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD insert_ferias.

    DATA: s_insert_ferias_reques TYPE zrsdatamovinsert_ferias_reques,
          zcl_proxy              TYPE REF TO zrsdatamovco_movimento_emprega,
          it_log                 TYPE TABLE OF zhcmt_pa_0023,
          v_begda                TYPE begda,
          v_endda                TYPE endda,
          v_cnpj                 TYPE char20,
          v_ini_ferias           TYPE char20,
          v_fim_ferias           TYPE char20.

    DATA: s_data_json         TYPE zhcms_rsdata_empferias_service,
          lwa_ferias_json     TYPE zhcms_rsdata_func_ferias_s, "zhcms_rsdata_func_ferias_emp_s,
          lwa_ferias_emp_json TYPE zhcms_rsdata_func_ferias_emp_s,
          e_json              TYPE string.

    DATA: lit_ret_ferias TYPE zhcms_rsdata_emp_ferias_resp.

    DATA: lwa_service TYPE zde_rs_data_cpi.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    IF ( me->at_rg_pernr[] IS NOT INITIAL ).

      v_begda = sy-datum - 3.
      v_endda = sy-datum + 3.

      "-> Buscar Férias cadastradas
      SELECT p2001~pernr,
             p2001~subty,
             p2001~endda,
             p2001~begda,
             p2001~aedtm,
             p2001~awart,
             p2001~ocrsn , "Ajuste durante comparação ZHCMr_0119
             p1~bukrs,
             p1~werks,
             j1~stcd1
        FROM pa2001 AS p2001
        LEFT JOIN pa0001 AS p1
        ON p1~pernr = p2001~pernr
        LEFT JOIN j_1bbranch AS j1
        ON j1~bukrs = p1~bukrs AND
           j1~branch = p1~werks
        INTO TABLE @DATA(it_ferias)
        WHERE p2001~pernr IN @me->at_rg_pernr[]
          AND p2001~subty = '0100'
          AND p2001~aedtm >= @v_begda
          AND p2001~aedtm <= @v_endda
         AND p2001~ocrsn = 'FERI' ""Ajuste durante comparação ZHCMr_0119
          AND p1~begda <= @v_begda
          AND p1~endda >= @v_begda.
      SORT it_ferias[] BY bukrs ASCENDING.

      IF ( it_ferias IS NOT INITIAL ).

        LOOP AT it_ferias[] INTO DATA(w_ferias)
          GROUP BY ( werks = w_ferias-werks
                     stcd1 = w_ferias-stcd1
                     size       = GROUP SIZE
                     index      = GROUP INDEX ) ASCENDING
                REFERENCE INTO DATA(group_werks).

          READ TABLE me->at_it_dados_empresa[] INTO DATA(w_dados_empresa) WITH KEY werks = group_werks->werks.

          IF ( sy-subrc = 0 ).

            "->Criando estrutura para envio de férias
            APPEND INITIAL LINE TO s_data_json-movimentoservice-dados[]
             ASSIGNING FIELD-SYMBOL(<w_empresa>).

            "-> Acrescenta dados da empresa
            <w_empresa>-nrcnpjempresa = w_dados_empresa-stcd1.

            LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.

              "-> Acrescenta dados do empregado
              APPEND INITIAL LINE TO <w_empresa>-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).

              <w_empregado>-matricula = <w_func>-pernr.

              "-> Acrescenta dados das férias encontradas para o funcionário
              "APPEND INITIAL LINE TO <w_empregado>-ferias-feria[] ASSIGNING FIELD-SYMBOL(<w_ferias>).

              WRITE <w_func>-begda TO v_ini_ferias DD/MM/YYYY.
              WRITE <w_func>-endda TO v_fim_ferias DD/MM/YYYY.

              REPLACE ALL OCCURRENCES OF '.' IN v_ini_ferias WITH '/'.
              REPLACE ALL OCCURRENCES OF '.' IN v_fim_ferias WITH '/'.

* Ajuste ZHCMR_PA0119
*              <w_ferias>-data_inicio = v_ini_ferias.
*              <w_ferias>-data_fim    = v_fim_ferias.
*              <w_ferias>-excluir = '0'.
              <w_empregado>-ferias-datainicio = v_ini_ferias.
              <w_empregado>-ferias-datafim    = v_fim_ferias.
              <w_empregado>-ferias-excluir = '0'.
* Ajuste ZHCMR_PA0119
            ENDLOOP.
            CLEAR: v_cnpj.
          ENDIF.
        ENDLOOP.

        IF s_data_json-movimentoservice-dados[] IS NOT INITIAL.

          s_data_json-movimentoservice-usernametoken-username = 'integracao.db0801amaggiexp@rsdata.com.br'.
          s_data_json-movimentoservice-usernametoken-password = '43a19edaac7b63358c252c544be06c49'.
          s_data_json-movimentoservice-config-metodo          = 'insertferias'.

          e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                      pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                     ).

          lwa_service-service = 'MovimentoService'.
          lwa_service-json    = e_json.
          lwa_service-tipo    = 'insertferias_TGG'.

          "-> Envio das informações
*** Chamar classe RSDATA que envia os dados.
          TRY .
              obj_rsdata_cpi->zif_integracao_outbound~get_instance(
              )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
              IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
                DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.

                /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_ferias ).

                LOOP AT lit_ret_ferias-insertferiasresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
                  READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.

*                  try.
*                      data(w_emp_ferias) = w_empregados-ferias-feria[ 1 ].
*                    catch cx_sy_itab_line_not_found.
*                  endtry.

                  DATA(w_log) = VALUE zhcmt_pa_0023(
                      servico         = 'insertFerias'
                      data_envio      = sy-datum
                      hora_envio      = sy-uzeit
                     " pernr           = w_empregados-matricula
                      bukrs           = ''
                      werks           = '' ).
                  " dados_enviados  = |{ w_emp_ferias-data_inicio }/{ w_emp_ferias-data_fim }| ).

                  LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
                    w_log-retorno_servico = w_mensagem-txdescricao.
                    w_log-id_log = sy-tabix.
                    APPEND w_log TO it_log[].
                  ENDLOOP.

                ENDLOOP.
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                     servico         = 'insertFerias'
                     data_envio      = sy-datum
                     hora_envio      = sy-uzeit ).
                "pernr           = <lfs_trans_emp>-nr_matricula
                "bukrs           = <lfs_trans_emp>-id_empresa
                "werks           = <lfs_trans_emp>-id_empresa
                "dados_enviados  = |{ w_setor_cargo-cd_cargo }{ w_setor_cargo-cd_posicao_trabalho }/{ w_setor_cargo-cd_setor }| ).

                w_log-retorno_servico = 'Erro Insert Férias - Classe'.
                w_log-id_log = sy-tabix.
                APPEND w_log TO it_log[].
              ENDIF.
            CATCH zcx_integracao.
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'insertFerias'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
             " pernr           = <lfs_trans_emp>-nr_matricula
              retorno_servico = 'Erro Insert Férias'  ).

              APPEND w_log TO it_log[].
            CATCH zcx_error.
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'insertFerias'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
             " pernr           = <lfs_trans_emp>-nr_matricula
              retorno_servico = 'Erro Insert Férias'  ).

              APPEND w_log TO it_log[].
          ENDTRY.

          IF ( it_log[] IS NOT INITIAL ).
            MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD insert_ferias_retroativo.

    DATA: s_insert_ferias_reques TYPE zrsdatamovinsert_ferias_reques,
          zcl_proxy              TYPE REF TO zrsdatamovco_movimento_emprega,
          it_log                 TYPE TABLE OF zhcmt_pa_0023,
          v_begda                TYPE begda,
          v_endda                TYPE endda,
          v_cnpj                 TYPE char20,
          v_ini_ferias           TYPE char20,
          v_fim_ferias           TYPE char20.

    DATA: s_data_json         TYPE zhcms_rsdata_empferias_service,
          lwa_ferias_json     TYPE zhcms_rsdata_func_ferias_s, "zhcms_rsdata_func_ferias_emp_s,
          lwa_ferias_emp_json TYPE zhcms_rsdata_func_ferias_emp_s,
          e_json              TYPE string.

    DATA: lit_ret_ferias TYPE zhcms_rsdata_emp_ferias_resp.
    DATA: lwa_service TYPE zde_rs_data_cpi.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    IF ( me->at_rg_pernr[] IS NOT INITIAL ).

      v_begda = sy-datum - 3.
      v_endda = sy-datum + 3.

      "-> Buscar Férias cadastradas
      SELECT p2001~pernr,
             p2001~subty,
             p2001~endda,
             p2001~begda,
             p2001~aedtm,
             p2001~awart,
             p2001~ocrsn,
             p1~bukrs,
             p1~werks,
             j1~stcd1
        FROM pa2001 AS p2001
        LEFT JOIN pa0001 AS p1
        ON p1~pernr = p2001~pernr
        LEFT JOIN j_1bbranch AS j1
        ON j1~bukrs = p1~bukrs AND
           j1~branch = p1~werks
        INTO TABLE @DATA(it_ferias)
      WHERE p2001~pernr IN @me->at_rg_pernr[]
        AND p2001~subty = '0100'
        AND p2001~aedtm >= @at_data_base
        AND p2001~ocrsn = 'FERI'
        AND p1~endda >= @sy-datum.
      SORT it_ferias[] BY bukrs ASCENDING.

      IF ( it_ferias IS NOT INITIAL ).

        LOOP AT it_ferias[] INTO DATA(w_ferias)
          GROUP BY ( werks = w_ferias-werks
                     stcd1 = w_ferias-stcd1
                     size       = GROUP SIZE
                     index      = GROUP INDEX ) ASCENDING
                REFERENCE INTO DATA(group_werks).

          READ TABLE me->at_it_dados_empresa[] INTO DATA(w_dados_empresa) WITH KEY werks = group_werks->werks.

          IF ( sy-subrc = 0 ).

            "->Criando estrutura para envio de férias
            APPEND INITIAL LINE TO s_data_json-movimentoservice-dados[]
              ASSIGNING FIELD-SYMBOL(<w_empresa>).

            "-> Acrescenta dados da empresa
            <w_empresa>-nrcnpjempresa = w_dados_empresa-stcd1. "V_CNPJ.

            LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.

              "-> Acrescenta dados do empregado
              APPEND INITIAL LINE TO <w_empresa>-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).
              <w_empregado>-matricula = <w_func>-pernr.

              "-> Acrescenta dados das férias encontradas para o funcionário
              " APPEND INITIAL LINE TO <w_empregado>-ferias-feria[] ASSIGNING FIELD-SYMBOL(<w_ferias>).

              WRITE <w_func>-begda TO v_ini_ferias DD/MM/YYYY.
              WRITE <w_func>-endda TO v_fim_ferias DD/MM/YYYY.

              REPLACE ALL OCCURRENCES OF '.' IN v_ini_ferias WITH '/'.
              REPLACE ALL OCCURRENCES OF '.' IN v_fim_ferias WITH '/'.

              <w_empregado>-ferias-datainicio = v_ini_ferias.
              <w_empregado>-ferias-datafim    = v_fim_ferias.
              <w_empregado>-ferias-excluir = '0'.

            ENDLOOP.

            CLEAR: v_cnpj.

          ENDIF.

        ENDLOOP.


        IF s_data_json-movimentoservice-dados[] IS NOT INITIAL.

          s_data_json-movimentoservice-usernametoken-username = 'integracao.db0801amaggiexp@rsdata.com.br'.
          s_data_json-movimentoservice-usernametoken-password = '43a19edaac7b63358c252c544be06c49'.
          s_data_json-movimentoservice-config-metodo          = 'insertferias'.

          e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                      pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                     ).

          lwa_service-service = 'MovimentoService'.
          lwa_service-json    = e_json.
          lwa_service-tipo    = 'insertferias_retro_TGG'.

*** Chamar classe RSDATA que envia os dados.
          TRY .
              obj_rsdata_cpi->zif_integracao_outbound~get_instance(
              )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
              IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
                DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.

                /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_ferias ).

                LOOP AT lit_ret_ferias-insertferiasresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
                  READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.

*                  try.
*                      data(w_emp_ferias) = w_empregados-ferias-feria[ 1 ].
*                    catch cx_sy_itab_line_not_found.
*                  endtry.

                  DATA(w_log) = VALUE zhcmt_pa_0023(
                      servico         = 'insertFerias'
                      data_envio      = sy-datum
                      hora_envio      = sy-uzeit
                     " pernr           = w_empregados-matricula
                      bukrs           = ''
                      werks           = '' ).
                  " dados_enviados  = |{ w_emp_ferias-data_inicio }/{ w_emp_ferias-data_fim }| ).

                  LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
                    w_log-retorno_servico = w_mensagem-txdescricao.
                    w_log-id_log = sy-tabix.
                    APPEND w_log TO it_log[].
                  ENDLOOP.

                ENDLOOP.
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                     servico         = 'insertFeriasTggRetro'
                     data_envio      = sy-datum
                     hora_envio      = sy-uzeit ).
                "pernr           = <lfs_trans_emp>-nr_matricula
                "bukrs           = <lfs_trans_emp>-id_empresa
                "werks           = <lfs_trans_emp>-id_empresa
                "dados_enviados  = |{ w_setor_cargo-cd_cargo }{ w_setor_cargo-cd_posicao_trabalho }/{ w_setor_cargo-cd_setor }| ).

                w_log-retorno_servico = 'Erro Insert Férias TGG - Retro - Classe'.
                w_log-id_log = sy-tabix.
                APPEND w_log TO it_log[].
              ENDIF.
            CATCH zcx_integracao.
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'insertFerias'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
             " pernr           = <lfs_trans_emp>-nr_matricula
              retorno_servico = 'Erro Insert Férias TGG - Retro'  ).

              APPEND w_log TO it_log[].
            CATCH zcx_error.
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'insertFerias'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
             " pernr           = <lfs_trans_emp>-nr_matricula
              retorno_servico = 'Erro Insert Férias TGG - Retro'  ).

              APPEND w_log TO it_log[].
          ENDTRY.

          IF ( it_log[] IS NOT INITIAL ).
            MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD SET_HEADER_SECURITY.

    DATA: l_string         TYPE string,
          ws_header        TYPE REF TO if_wsprotocol_ws_header,
          name             TYPE string,
          namespace        TYPE string,
          obj_ixml         TYPE REF TO if_ixml,
          obj_ixml_factory TYPE REF TO if_ixml_stream_factory,
          obj_ixml_istream TYPE REF TO if_ixml_istream,
          obj_parser       TYPE REF TO if_ixml_parser,
          w_error          TYPE i,
          xml_document     TYPE REF TO if_ixml_document,
          xml_root         TYPE REF TO if_ixml_element,
          xml_element      TYPE REF TO if_ixml_element.

    IF ( i_service_name IS NOT INITIAL ).

      SELECT SINGLE username, password
        FROM zauth_webservice
        INTO @DATA(w_webservice)
        WHERE service = 'RSDATA_EMPREGADO_TGG'.
      CHECK ( sy-subrc = 0 ).


      CONCATENATE
        '<soapenv:Header>'
        '<wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">'
        '<wsse:UsernameToken>'
        '<wsse:Username>'
        w_webservice-username
        '</wsse:Username>'
        '<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">'
        w_webservice-password
        '</wsse:Password>'
        '</wsse:UsernameToken>'
        '</wsse:Security>'
        '</soapenv:Header>'
      INTO l_string.

      CASE i_service_name.

        WHEN 'ZRSDATACO_EMPREGADO_PORT'.

          TRY.
              ws_header ?= cl_web_empregado->get_protocol( if_wsprotocol=>ws_header ).
            CATCH cx_ai_system_fault INTO DATA(v_error_cl_empreg).
          ENDTRY.

        WHEN 'ZRSEMPRESACO_EMPRESA_PORT'.

          TRY.
              ws_header ?= cl_web_empresa->get_protocol( if_wsprotocol=>ws_header ).
            CATCH cx_ai_system_fault INTO DATA(v_error_cl_empresa).
          ENDTRY.

        WHEN 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'.

          TRY.
              ws_header ?= cl_web_movimento->get_protocol( if_wsprotocol=>ws_header ).
            CATCH cx_ai_system_fault INTO DATA(v_error_cl_mov).
          ENDTRY.

      ENDCASE.


      obj_ixml = cl_ixml=>create( ).

      obj_ixml_factory = obj_ixml->create_stream_factory( ).

      obj_ixml_istream = obj_ixml_factory->create_istream_string( l_string ).

      xml_document = obj_ixml->create_document( ).

      obj_parser = obj_ixml->create_parser(
             document       = xml_document
             istream        = obj_ixml_istream
             stream_factory = obj_ixml_factory ).

      w_error = obj_parser->parse( ).

      IF ( sy-subrc = 0 AND xml_document IS NOT INITIAL ).

        xml_root = xml_document->get_root_element( ).

        xml_element ?= xml_root->get_first_child( ).

        WHILE NOT xml_element IS INITIAL.

          name = xml_element->get_name( ).
          namespace = xml_element->get_namespace_uri( ).

          ws_header->set_request_header(
            name = name
            namespace = namespace
            dom = xml_element ).

          xml_element ?= xml_element->get_next( ).

        ENDWHILE.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD transfer_empregados.

    TYPES: BEGIN OF ty_plans_hist,
             objid TYPE hrp9665-objid,
             pernr TYPE pa0001-pernr,
             endda TYPE pa0001-endda,
             begda TYPE pa0001-begda,
             bukrs TYPE pa0001-bukrs,
             werks TYPE pa0001-werks.
    TYPES: END OF ty_plans_hist.

    TYPES: ty_rg_plans TYPE RANGE OF pa0001-plans.

    DATA:
      s_transfer_empreg_req       TYPE zrsdatatransferir_empregados_1,
      it_plans_hist               TYPE TABLE OF ty_plans_hist,
      zcl_proxy                   TYPE REF TO zrsdataco_empregado_port,
      it_funcionarios_encontrados TYPE ty_t_dados_funcionario,
      it_dados_empregado_atual    TYPE ty_t_dados_funcionario,
      it_log                      TYPE TABLE OF zhcmt_pa_0023,
      v_data_ini                  TYPE begda,
      v_cnpj                      TYPE char20,
      v_dt_ini                    TYPE char20,
      v_dt_fim                    TYPE char20,
      v_doc_nr                    TYPE p0465-doc_nr,
      v_stcd1                     TYPE j_1bbranch-stcd1.

    DATA: s_pernr LIKE LINE OF e_pernr.

    DATA: s_data_json   TYPE zhcms_rsdata_empreg_service,
          lwa_temp_json TYPE zhcms_rsdata_func_emp_s,
          e_json        TYPE string.

    DATA: lit_ret_temp TYPE   zhcms_rsdata_empregados_resp.

    DATA: lwa_service TYPE zde_rs_data_cpi.

    DATA: obj_rsdata_cpi  TYPE REF TO zcl_int_ob_rsdata.
    CREATE OBJECT obj_rsdata_cpi.

    DATA: v_opt(2)   TYPE c.                                "pbi 63026
    IMPORT v_opt TO v_opt FROM MEMORY ID 'v_opt'.

    CLEAR me->at_it_dados_empregado[].

    IF v_opt NE 'TR'. "PBI 63026 Verificar se é uma transferencia.
*      Buscar dados válidos no período informado:
      SELECT
      p1~pernr,
      p1~endda,
      p1~begda,
      p1~bukrs,
      p1~werks,
      p1~stell,
      p1~persk,
      p1~abkrs,
      p1~orgeh,
      p1~plans,
      p0~begda AS demissao_date,
      p0~stat2,
      p2~cname,
      p2~nachn,
      p2~vorna,
      p2~gesch,
      p2~famst,
      p2~gbdat
    FROM pa0001 AS p1
    LEFT JOIN pa0000 AS p0
    ON p1~pernr = p0~pernr
    LEFT JOIN pa0002 AS p2
    ON p1~pernr = p2~pernr
    INTO TABLE @DATA(it_func)
    WHERE ( p1~begda = @me->at_data_base OR
            p1~endda = @me->at_data_base OR
            p1~aedtm = @me->at_data_base ) AND
      p1~abkrs <> 'BA'              AND
      p1~bukrs IN @me->at_bukrs     AND
      p0~begda <= @me->at_data_base AND
      p0~endda >= @me->at_data_base AND
      p0~stat2 = '3' AND "IN ( '3',  '0' )      AND
      p2~endda > @me->at_data_base.

    ELSE.

*      Buscar dados válidos no período informado:
      SELECT
      p1~pernr,
      p1~endda,
      p1~begda,
      p1~bukrs,
      p1~werks,
      p1~stell,
      p1~persk,
      p1~abkrs,
      p1~orgeh,
      p1~plans,
      p0~begda AS demissao_date,
      p0~stat2,
      p2~cname,
      p2~nachn,
      p2~vorna,
      p2~gesch,
      p2~famst,
      p2~gbdat
    FROM pa0001 AS p1
    LEFT JOIN pa0000 AS p0
    ON p1~pernr = p0~pernr
    LEFT JOIN pa0002 AS p2
    ON p1~pernr = p2~pernr
    INTO TABLE @it_func
    WHERE ( p1~begda = @me->at_data_base OR
            p1~endda = @me->at_data_base OR
            p1~aedtm = @me->at_data_base ) AND
      p1~bukrs IN @me->at_bukrs      AND
      p1~pernr IN @r_pernr[]        AND
      p1~abkrs <> 'BA'              AND
      p0~begda <= @me->at_data_base AND
      p0~endda >= @me->at_data_base AND
      p0~stat2 = '3' AND "IN ( '3',  '0' )      AND
      p2~endda > @me->at_data_base.

      me->at_it_dados_empregado[] = CORRESPONDING #( it_func[] ).
      me->insert_empresa( ). "PBI 6326 --  Retirar metodo de dentro de busca empregados
    ENDIF.

    CHECK it_func[] IS NOT INITIAL.

    SORT it_func[] BY pernr ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_func[] COMPARING pernr.

    IF ( it_func[] IS NOT INITIAL ).
      me->at_it_dados_empregado[] = CORRESPONDING #( it_func[] ).
      it_dados_empregado_atual[] = me->at_it_dados_empregado[].
    ENDIF.

    "-> cria cópia da tabela de empregados que serão enviados alterando a data para buscar histórico
    it_funcionarios_encontrados = VALUE #( FOR _func IN it_dados_empregado_atual[] (
        pernr = _func-pernr
        begda = ( _func-begda - 1 )
        bukrs = _func-bukrs
        werks = _func-werks
        stell = _func-stell
        persk = _func-persk
        abkrs = _func-abkrs
        orgeh = _func-orgeh
        plans = _func-plans
        stat2 = _func-stat2
        cname = _func-cname
        nachn = _func-nachn
        vorna = _func-vorna
        gesch = _func-gesch
        famst = _func-famst
        gbdat = _func-gbdat
    ) ).


    SELECT p1~pernr, p1~endda, p1~begda, p1~bukrs, p1~werks, p1~plans,
       p1~stell, p1~orgeh, p1~aedtm
      FROM pa0001 AS p1
      INTO TABLE @DATA(it_hist_func)
      FOR ALL ENTRIES IN @it_funcionarios_encontrados[]
      WHERE
        p1~pernr = @it_funcionarios_encontrados-pernr AND
        p1~endda = @it_funcionarios_encontrados-begda.

    SORT it_hist_func[] BY pernr begda ASCENDING.

    it_plans_hist = VALUE #( FOR w_pa01 IN it_hist_func[] (
             objid  = w_pa01-plans
             pernr  = w_pa01-pernr
             endda  = w_pa01-endda
             begda  = w_pa01-begda
             bukrs  = w_pa01-bukrs
             werks  = w_pa01-werks ) ).

    SORT it_plans_hist[] BY objid ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_plans_hist[] COMPARING objid.

    SELECT h65~objid,
       h65~begda AS begda_risco,
       h65~endda AS endda_risco,
       h65~setrisc,
       h65~stext
           FROM hrp9665 AS h65
      INTO TABLE @DATA(it_9665_hist)
        FOR ALL ENTRIES IN @it_plans_hist
      WHERE h65~objid EQ @it_plans_hist-objid
        AND h65~endda >= @v_data_ini
        AND h65~plvar = '01'
        AND h65~otype = 'S'
        AND h65~endda >= @me->at_data_base
        AND h65~begda <= @me->at_data_base
        AND h65~setrisc IS NOT NULL.

    DATA(r_plans) = VALUE ty_rg_plans(
      FOR w_ativos IN it_dados_empregado_atual[] (
          sign = 'I'
          option = 'EQ'
          low = w_ativos-plans )  ).
    SORT r_plans BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM r_plans COMPARING low.

    IF ( r_plans[] IS NOT INITIAL ).
      "-> Setor / Substituído por Área de Risco HRP9665
      SELECT
        h65~objid,
        h65~setrisc,
        h65~stext
      FROM hrp9665 AS h65
      INTO TABLE @DATA(it_orgeh)
      WHERE
          h65~objid IN @r_plans[] AND
          h65~plvar = '01' AND
          h65~otype = 'S'  AND
          h65~endda >= @me->at_data_base AND
          h65~begda <= @me->at_data_base AND
          h65~setrisc IS NOT NULL.
      SORT it_orgeh[] BY objid ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_orgeh[] COMPARING objid.
    ENDIF.


    IF ( it_hist_func[] IS NOT INITIAL ).

      DATA(rg_stell) = VALUE ty_rg_stell( FOR _func_hist IN it_dados_empregado_atual[] (
          sign   = 'I'
          option = 'EQ'
          low    = _func_hist-stell
          high   = _func_hist-stell )  ).
      SORT rg_stell[] BY low ASCENDING.
      DELETE ADJACENT DUPLICATES FROM rg_stell[] COMPARING low.

      IF ( rg_stell[] IS NOT INITIAL ).

        "-> Cargo
        SELECT
          h0~objid,
          h0~stext
        FROM hrp1000 AS h0
        INTO TABLE @DATA(it_cargos)
        WHERE
          h0~plvar = '01'          AND
          h0~otype = 'C'           AND
          h0~objid IN @rg_stell     AND
          h0~endda >= @me->at_data_base AND
          h0~begda <= @me->at_data_base AND
          h0~langu = @sy-langu.
        SORT it_cargos[] BY objid ASCENDING.

      ENDIF.

      LOOP AT it_dados_empregado_atual[] INTO DATA(w_dados_emp).

        s_transfer_empreg_req-rsdata-config-tp_ver_empregado = 'MATRICULA'.
        s_transfer_empreg_req-rsdata-config-tp_ver_setor_cargo = 'CODIGO'.
        s_transfer_empreg_req-rsdata-config-forcar_inclusao = 'N'.

        LOOP AT it_hist_func[] ASSIGNING FIELD-SYMBOL(<w_hist_func>)
          WHERE pernr = w_dados_emp-pernr AND endda <= me->at_data_base. " v_data_ini. "OR endda__risco <= v_data_ini ).

          TRY.
              DATA(v_nova_area_risco) = it_orgeh[ objid = w_dados_emp-plans ]-setrisc.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          TRY.
              DATA(w_hist_9665) = it_9665_hist[ objid = <w_hist_func>-plans ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

*         Comparar filial para incluir transferência
          IF ( <w_hist_func>-werks <> w_dados_emp-werks ).

            APPEND INITIAL LINE TO s_transfer_empreg_req-rsdata-empregados-empregado[]
              ASSIGNING FIELD-SYMBOL(<w_empregado>).

            <w_empregado>-nr_matricula = w_dados_emp-pernr.

* Alteracao - Inicio  - RMNI - CS1030728 - 16.11.2022 - Veio do ZHCMR_PA0119
            SELECT SINGLE doc_nr
              FROM pa0465
              INTO v_doc_nr
             WHERE pernr = w_dados_emp-pernr
               AND subty = `0016`
               AND ( endda >= me->at_data_base
                OR   endda >= sy-datum ).
            IF sy-subrc IS INITIAL.
              <w_empregado>-matricula_rh = v_doc_nr.
            ENDIF.
* Alteracao - Final   - RMNI - CS1030728 - 16.11.2022

            "-> Buscando dados da empresa anterior e de destino:
            SORT me->at_it_dados_empresa[] BY werks ASCENDING.
            READ TABLE me->at_it_dados_empresa[] INTO DATA(w_empresa_dest) WITH KEY werks = w_dados_emp-werks BINARY SEARCH.
            IF ( sy-subrc = 0 ).
              CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
                EXPORTING
                  input  = w_empresa_dest-stcd1
                IMPORTING
                  output = v_cnpj.

              <w_empregado>-nr_cnpjempresa_destino = v_cnpj.
              <w_empregado>-forcar_transferencia = 'S'.
            ENDIF.
            CLEAR: v_cnpj, v_stcd1.


            v_stcd1 = me->get_cnpj_werks(
              i_bukrs = <w_hist_func>-bukrs
              i_werks = <w_hist_func>-werks
            ).

            CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
              EXPORTING
                input  = v_stcd1
              IMPORTING
                output = v_cnpj.

            <w_empregado>-nr_cnpjempresa = v_cnpj.

            CLEAR: v_cnpj.

            "-> Inclui informações de cargo e setor
            APPEND INITIAL LINE TO <w_empregado>-setores_cargos-setor_cargo[]
              ASSIGNING FIELD-SYMBOL(<w_setor>).

            WRITE w_dados_emp-begda TO v_dt_ini DD/MM/YYYY.
            WRITE w_dados_emp-endda TO v_dt_fim DD/MM/YYYY.
            REPLACE ALL OCCURRENCES OF '.' IN v_dt_ini WITH '/'.
            REPLACE ALL OCCURRENCES OF '.' IN v_dt_fim WITH '/'.

            <w_empregado>-dt_transferencia = v_dt_ini.
            <w_setor>-dt_inicio = v_dt_ini.

            IF <w_empregado>-dt_demissao IS NOT INITIAL.
              <w_setor>-dt_saida = <w_empregado>-dt_demissao.
            ENDIF.

*-CS2020000297 - BUG 69088 - 18.11.2021 - JT - inicio
            IF w_dados_emp-endda <> '99991231'.
              <w_setor>-dt_saida = v_dt_fim.
            ENDIF.
*-CS2020000297 - BUG 69088 - 18.11.2021 - JT - fim

            "-> Se mudou o setor, enviar novo código e descrição
            IF ( w_hist_9665-setrisc <>  v_nova_area_risco ).
              TRY.
                  <w_setor>-cd_setor = v_nova_area_risco.
                  <w_setor>-nome_setor =  it_orgeh[ objid = w_dados_emp-plans ]-stext.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.
            ENDIF.

            "-> Se mudou o cargo, enviar novo código e descrição
            IF ( <w_hist_func>-stell <> w_dados_emp-stell ).
              <w_setor>-cd_cargo = w_dados_emp-stell.
              TRY.
                  <w_setor>-nome_cargo = it_cargos[ objid = w_dados_emp-stell ]-stext.
                CATCH  cx_sy_itab_line_not_found..
              ENDTRY.
            ENDIF.

            DATA(w_log) = VALUE zhcmt_pa_0023(
               servico         = 'transferEmpregados'
               data_envio      = sy-datum
               hora_envio      = sy-uzeit
               pernr           = w_dados_emp-pernr
               bukrs           = w_dados_emp-bukrs
               werks           = w_dados_emp-werks
               dados_enviados  = |{ w_dados_emp-stell }/{ w_dados_emp-plans }/{ v_nova_area_risco }| ).

            APPEND w_log TO it_log[].
            CLEAR: w_log, v_nova_area_risco, w_hist_9665.

          ENDIF.

        ENDLOOP.

      ENDLOOP.

      IF ( lines( s_transfer_empreg_req-rsdata-empregados-empregado[] ) > 0 ).

        s_data_json-empregadoservice-usernametoken-username = ''."'integracao.db0801amaggiexp@rsdata.com.br'.
        s_data_json-empregadoservice-usernametoken-password = ''."'43a19edaac7b63358c252c544be06c49'.
        s_data_json-empregadoservice-config-metodo          = 'transferirEmpregadosRequest'.

        s_data_json-empregadoservice-config-tpverempregado  = 'MATRICULA'.
        s_data_json-empregadoservice-config-tpversetorcargo = 'CODIGO'.
        s_data_json-empregadoservice-config-forcarinclusao  = 'N'.

        LOOP AT s_transfer_empreg_req-rsdata-empregados-empregado[]  ASSIGNING FIELD-SYMBOL(<lfs_trans_emp>).

          lwa_temp_json-nrmatricula               =  <lfs_trans_emp>-nr_matricula.
          lwa_temp_json-matricularh               =  <lfs_trans_emp>-matricula_rh.
          lwa_temp_json-nrcnpjempresadestino      =  <lfs_trans_emp>-nr_cnpjempresa_destino.
          lwa_temp_json-forcartransferencia       =  <lfs_trans_emp>-forcar_transferencia.
          lwa_temp_json-nrcnpjempresa             =  <lfs_trans_emp>-nr_cnpjempresa.
          lwa_temp_json-dttransferencia           =  <lfs_trans_emp>-dt_transferencia.

          LOOP AT <lfs_trans_emp>-setores_cargos-setor_cargo  ASSIGNING FIELD-SYMBOL(<lfs_setor_cargo>).
            IF <lfs_setor_cargo> IS ASSIGNED.
              lwa_temp_json-setorcargo-dtinicio   = <lfs_setor_cargo>-dt_inicio.
              lwa_temp_json-setorcargo-dtsaida    = <lfs_trans_emp>-dt_demissao.
              lwa_temp_json-setorcargo-cdsetor    = <lfs_setor_cargo>-cd_setor.
              lwa_temp_json-setorcargo-nomesetor  = <lfs_setor_cargo>-nome_setor.
              lwa_temp_json-setorcargo-cdcargo    = <lfs_setor_cargo>-cd_cargo.
              lwa_temp_json-setorcargo-nomecargo  = <lfs_setor_cargo>-nome_cargo.
            ENDIF.
          ENDLOOP.


          APPEND lwa_temp_json TO s_data_json-empregadoservice-empregados[].
          CLEAR: lwa_temp_json.

          e_json = /ui2/cl_json=>serialize( EXPORTING data        = s_data_json
                                                      pretty_name = /ui2/cl_json=>pretty_mode-low_case
                                                     ).

          lwa_service-service = 'EmpregadoService'.
          lwa_service-json    = e_json.
          lwa_service-tipo    = 'transferirEmpregadosRequest_TGG'.

*** Chamar classe RSDATA que envia os dados.
          TRY .
              obj_rsdata_cpi->zif_integracao_outbound~get_instance(
              )->execute_request( EXPORTING i_info_request = lwa_service IMPORTING e_integracao = DATA(_lwa_ret_call)  ).
              IF _lwa_ret_call IS NOT INITIAL AND _lwa_ret_call-nm_code = '0200'.
                DATA(lva_json_retorno) = _lwa_ret_call-ds_data_retorno.
                /ui2/cl_json=>deserialize( EXPORTING json = lva_json_retorno CHANGING data = lit_ret_temp ).

                IF lit_ret_temp-empregadosresponse-cdmsg = '500'.
**** ERRO remove a matricula para não enviar o INSERT - Codigo de erro 500.
                  s_pernr-option = 'EQ'.
                  s_pernr-sign = 'I'.
                  s_pernr-low = <lfs_trans_emp>-nr_matricula.
                  APPEND s_pernr TO e_pernr.
                  CLEAR: s_pernr.
                ENDIF.

                LOOP AT lit_ret_temp-empregadosresponse-mensagem[]  ASSIGNING FIELD-SYMBOL(<lfs_emp_response>).
                  "data(lva_index)  = sy-tabix.

                  READ TABLE <lfs_emp_response>-detalhes  ASSIGNING FIELD-SYMBOL(<lfs_w_mensagem_tab>) INDEX 1.


                  TRY.
                      DATA(w_setor_cargo) = <lfs_trans_emp>-setores_cargos-setor_cargo[ 1 ].
                    CATCH cx_sy_itab_line_not_found.
                  ENDTRY.

                  CLEAR: w_log.
                  w_log = VALUE zhcmt_pa_0023(
                      servico         = 'TransfereEmpregados'
                      data_envio      = sy-datum
                      hora_envio      = sy-uzeit
                      pernr           = <lfs_trans_emp>-nr_matricula
                      bukrs           = <lfs_trans_emp>-id_empresa
                      werks           = <lfs_trans_emp>-id_empresa
                      dados_enviados  = |{ w_setor_cargo-cd_cargo }{ w_setor_cargo-cd_posicao_trabalho }/{ w_setor_cargo-cd_setor }| ).

                  LOOP AT <lfs_w_mensagem_tab>-mensagemdet INTO DATA(w_mensagem).
                    w_log-retorno_servico = w_mensagem-txdescricao.
                    w_log-id_log = sy-tabix.
                    APPEND w_log TO it_log[].
                  ENDLOOP.

                ENDLOOP.
              ELSE.
                CLEAR: w_log.
                w_log = VALUE zhcmt_pa_0023(
                     servico         = 'TransfereEmpregados'
                     data_envio      = sy-datum
                     hora_envio      = sy-uzeit
                     pernr           = <lfs_trans_emp>-nr_matricula
                     bukrs           = <lfs_trans_emp>-id_empresa
                     werks           = <lfs_trans_emp>-id_empresa
                     dados_enviados  = |{ w_setor_cargo-cd_cargo }{ w_setor_cargo-cd_posicao_trabalho }/{ w_setor_cargo-cd_setor }| ).


                w_log-retorno_servico = 'Erro Transfere Empregados - Classe'.
                w_log-id_log = sy-tabix.
                APPEND w_log TO it_log[].

                s_pernr-option = 'EQ'.
                s_pernr-sign = 'I'.
                s_pernr-low = <lfs_trans_emp>-nr_matricula.
                APPEND s_pernr TO e_pernr.
                CLEAR: s_pernr.

              ENDIF.
            CATCH zcx_integracao INTO DATA(r_msg).
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'TransfereEmpregados'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_trans_emp>-nr_matricula
              retorno_servico = 'Erro Insert Empregados'  ).

              APPEND w_log TO it_log[].

              s_pernr-option = 'EQ'.
              s_pernr-sign = 'I'.
              s_pernr-low = <lfs_trans_emp>-nr_matricula.
              APPEND s_pernr TO e_pernr.
              CLEAR: s_pernr.


            CATCH zcx_error INTO DATA(r_msg_error).
              CLEAR: w_log.
              w_log = VALUE zhcmt_pa_0023(
              servico         = 'TransfereEmpregados'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = <lfs_trans_emp>-nr_matricula
              retorno_servico = 'Erro Insert Empregados'  ).


              s_pernr-option = 'EQ'.
              s_pernr-sign = 'I'.
              s_pernr-low = <lfs_trans_emp>-nr_matricula.
              APPEND s_pernr TO e_pernr.
              CLEAR: s_pernr.

          ENDTRY.
          CLEAR: s_data_json-empregadoservice-empregados[], e_json, lwa_service.
          IF <lfs_setor_cargo> IS ASSIGNED .
            UNASSIGN <lfs_setor_cargo> .
          ENDIF.
        ENDLOOP.
        IF ( it_log[] IS NOT INITIAL ).
          MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
