class ZCL_HCM_RSDATA_TGG definition
  public
  final
  create public .

public section.

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
      value(R_PERNR) type TY_RG_PERNR optional .
  methods INSERT_EMPREGADOS
    importing
      value(I_REQUEST) type ZRSDATAINSERT_EMPREGADOS_REQUE .
  methods INSERT_FERIAS_RETROATIVO .
  methods INSERT_ABSENTEISMO_RETROATIVO .
  methods GET_LISTA_PERNR_RETROATIVO
    importing
      value(R_BUKRS) type TY_RG_BUKRS optional
      value(R_WERKS) type TY_RG_WERKS optional
      value(R_PERNR) type TY_RG_PERNR optional
    returning
      value(IT_FUNCIONARIOS) type ZHCM_T_FUNCIONARIOS_LIST .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_HCM_RSDATA_TGG IMPLEMENTATION.


  METHOD GET_DADOS_EMPREGADOS.

    DATA: v_message_handler TYPE REF TO if_hrpa_message_handler,
          v_hire_date       TYPE sy-datum,
          v_fire_date       TYPE sy-datum,
          v_data_aux        TYPE char20,
          v_cnpj            TYPE char20,
          it_excl_pernr     TYPE ty_rg_pernr.

    CHECK ( me->at_it_dados_empregado[] IS NOT INITIAL ).

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
      WHERE subty = 'MAIL'
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
          if not at_data_base is initial.
            if <lfs_doc>-endda >= at_data_base.
              <w_empregado>-matricula_rh = <lfs_doc>-doc_nr.
            endif.
          else.
            if <lfs_doc>-endda >= sy-datum.
              <w_empregado>-matricula_rh = <lfs_doc>-doc_nr.
            endif.
          endif.
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


  METHOD get_lista_pernr_retroativo.

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


  METHOD INSERT_ABSENTEISMO.

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
* Inicio  - Alteracao - RMNI - 02.10.2022 - CS1028487
*            p2001~endda >= @me->at_data_base  AND
*            p2001~begda <= @me->at_data_base  AND
            p2001~aedtm = @me->at_data_base AND
* Inicio  - Alteracao - RMNI - 02.10.2022 - CS1028487
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
      APPEND INITIAL LINE TO s_insert_absenteismo_req-dados-empresa[] ASSIGNING FIELD-SYMBOL(<w_empresa>).

      TRY.
          DATA(w_detalhe_emp) = me->at_it_dados_empresa[ werks = group_werks->werks ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = w_detalhe_emp-stcd1
        IMPORTING
          output = v_cnpj.

      <w_empresa>-nr_cnpjempresa = v_cnpj.

      LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.

        "-> Acrescenta dados das férias encontradas para o funcionário
        APPEND INITIAL LINE TO <w_empresa>-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).

        <w_empregado>-matricula = <w_func>-pernr.

        APPEND INITIAL LINE TO <w_empregado>-absenteismos-absenteismo[] ASSIGNING FIELD-SYMBOL(<w_afast>).

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
        <w_afast>-data_prev_retorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ).
        <w_afast>-prev_afastamento = v_dias_afast.
        <w_afast>-tp_prev_afastamento = '2'.
        <w_afast>-data_retorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ). "V_FIM_ABS.
        <w_afast>-tempo_afastamento = v_dias_afast.
        <w_afast>-tp_afastamento = '2'.
        <w_afast>-motivo_afastamento = <w_func>-atext.
        <w_afast>-observacoes = <w_func>-atext.

        CLEAR: v_dias_afast, v_cnpj, v_data_retorno.


*        ME->INICIA_CLASSE_WEBSERVICE(
*         EXPORTING
*           I_PORT_NAME      = 'ZH01'    " Nome da porta lógica
*           I_SERVICE_NAME   = 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
*         CHANGING
*            CL_WEB_MOVIMENTO =  ZCL_PROXY  ).
*
*        ME->SET_HEADER_SECURITY(
*          EXPORTING
*            I_SERVICE_NAME   =  'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
*          CHANGING
*            CL_WEB_MOVIMENTO = ZCL_PROXY
*        ).

*        "-> Envio das informações
*        TRY.
*            ZCL_PROXY->INSERT_ABSENTEISMO(
*              EXPORTING
*                INSERT_ABSENTEISMO_REQUEST  = S_INSERT_ABSENTEISMO_REQ
*              IMPORTING
*                INSERT_ABSENTEISMO_RESPONSE = DATA(RESPONSE)  ).
*          CATCH   CX_AI_SYSTEM_FAULT INTO DATA(R_MSG).
*        ENDTRY.

***        DATA(w_log) = VALUE zhcmt_pa_0023(
***                 servico         = 'insertAbsenteismo'
***                 data_envio      = sy-datum
***                 hora_envio      = sy-uzeit
***                 pernr           =  <w_func>-pernr
***                 bukrs           =  <w_func>-bukrs
***                 werks           =  <w_func>-werks
***                 dados_enviados  = |{ <w_func>-begda }/{ <w_func>-endda }/{ <w_func>-atext  }| ).

*        IF ( RESPONSE IS NOT INITIAL ).
*          LOOP AT RESPONSE-RSDATA_RETURN-MENSAGENS-MENSAGEM[] INTO DATA(W_MENSAGEM_TAB).
*            LOOP AT W_MENSAGEM_TAB-DETALHES-MENSAGEM_DET[] INTO DATA(W_MENSAGEM).
*              W_LOG-RETORNO_SERVICO = W_MENSAGEM-TX_DESCRICAO.
*              APPEND W_LOG TO IT_LOG[].
*            ENDLOOP.
*          ENDLOOP.
*        ELSE.
*          W_LOG-RETORNO_SERVICO = R_MSG->ERRORTEXT.
*          APPEND W_LOG TO IT_LOG[].
*        ENDIF.

***        APPEND w_log TO it_log[].
***        CLEAR: w_log. ", <W_EMPRESA>-EMPREGADOS-EMPREGADO[].

      ENDLOOP.

*      CLEAR: S_INSERT_ABSENTEISMO_REQ-DADOS-EMPRESA[].
*
*      WAIT UP TO 1 SECONDS.

    ENDLOOP.

*    IF ( IT_LOG[] IS NOT INITIAL ).
*      MODIFY ZHCMT_PA_0023 FROM TABLE IT_LOG[].
*    ENDIF.

    DELETE s_insert_absenteismo_req-dados-empresa[] WHERE empregados-empregado IS INITIAL.

    IF ( s_insert_absenteismo_req-dados-empresa IS NOT INITIAL ).

      me->inicia_classe_webservice(
         EXPORTING
           i_port_name      = 'ZRSHR_TGG'    " Nome da porta lógica
           i_service_name   = 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
         CHANGING
            cl_web_movimento =  zcl_proxy  ).

      me->set_header_security(
        EXPORTING
          i_service_name   =  'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
        CHANGING
          cl_web_movimento = zcl_proxy
      ).

      "-> Envio das informações
      TRY.
          zcl_proxy->insert_absenteismo(
            EXPORTING
              insert_absenteismo_request  = s_insert_absenteismo_req
            IMPORTING
              insert_absenteismo_response = DATA(s_return)  ).
        CATCH   cx_ai_system_fault INTO DATA(r_msg).
      ENDTRY.

      IF ( s_return IS NOT INITIAL ) OR ( r_msg IS NOT INITIAL ).
        LOOP AT s_insert_absenteismo_req-dados-empresa[] INTO DATA(w_abs_empresa).

          LOOP AT w_abs_empresa-empregados-empregado[] INTO DATA(w_empregados).

            DATA(lva_index) = sy-tabix.

            TRY.
                DATA(w_absenteismo) = w_empregados-absenteismos-absenteismo[ 1 ].
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            DATA(w_log) = VALUE zhcmt_pa_0023(
                servico         = 'insertAbsenteismo'
                data_envio      = sy-datum
                hora_envio      = sy-uzeit
                pernr           = CONV #( w_empregados-matricula )
                bukrs           = ''
                werks           = ''
                dados_enviados  = |{ w_absenteismo-motivo_afastamento } Retorno:{ w_absenteismo-data_prev_retorno }| ).

            IF ( s_return IS NOT INITIAL ).
              LOOP AT s_return-rsdata_return-mensagens-mensagem[] INTO DATA(w_mensagem_tab) FROM lva_index.
                LOOP AT w_mensagem_tab-detalhes-mensagem_det[] INTO DATA(w_mensagem).
                  w_log-retorno_servico = w_mensagem-tx_descricao.
                  w_log-id_log          = CONV #( sy-tabix ).
                  APPEND w_log TO it_log[].
                ENDLOOP.
                EXIT.
              ENDLOOP.
            ELSE.
              IF r_msg IS NOT INITIAL.
                w_log-retorno_servico = r_msg->errortext.
                w_log-id_log          = CONV #( sy-tabix ).
                APPEND w_log TO it_log[].
              ENDIF.
            ENDIF.

            CLEAR: w_absenteismo, w_log.

          ENDLOOP.

        ENDLOOP.

      ENDIF.

      IF ( it_log[] IS NOT INITIAL ).
        MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
      ENDIF.

    ENDIF.

***      IF ( s_return IS NOT INITIAL ) OR ( r_msg IS NOT INITIAL ).
***
***        DATA(lva_tabix) = sy-tabix.
***        DATA(lva_tabix_soma) = sy-tabix.
***        CLEAR: lva_tabix, lva_tabix_soma.
***
***        IF ( s_return IS NOT INITIAL ).
***          LOOP AT s_insert_absenteismo_req-dados-empresa[] ASSIGNING FIELD-SYMBOL(<lfs_empresa>).
***            LOOP AT <lfs_empresa>-empregados-empregado ASSIGNING FIELD-SYMBOL(<lfs_emp>).
***              IF lva_tabix_soma IS INITIAL.
***                lva_tabix = sy-tabix.
***                lva_tabix_soma = sy-tabix.
***              ELSE.
***                lva_tabix_soma = lva_tabix_soma + lva_tabix.
***              ENDIF.
***
***              LOOP AT it_log[] ASSIGNING FIELD-SYMBOL(<w_log>) WHERE pernr = <lfs_emp>-matricula.
***                LOOP AT s_return-rsdata_return-mensagens-mensagem[] ASSIGNING FIELD-SYMBOL(<lfs_mensagens>) FROM lva_tabix_soma.
***                  LOOP AT <lfs_mensagens>-detalhes-mensagem_det[] ASSIGNING FIELD-SYMBOL(<lfs_mensagem>).
***                    <w_log>-retorno_servico = <lfs_mensagem>-tx_descricao.
***                    <w_log>-id_log          = CONV #( sy-tabix ).
***                  ENDLOOP.
***                  EXIT.
***                ENDLOOP.
***                IF ( s_return-rsdata_return-mensagens-mensagem[] IS INITIAL ).
***                  <w_log>-retorno_servico = r_msg->errortext.
***                  <w_log>-id_log          = CONV #( sy-tabix ).
***                ENDIF.
***              ENDLOOP.
***            ENDLOOP.
***          ENDLOOP.
***        ENDIF.
***
***        IF ( it_log[] IS NOT INITIAL ).
***          MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
***        ENDIF.
***
***      ENDIF.

***  ENDIF.

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
      APPEND INITIAL LINE TO s_insert_absenteismo_req-dados-empresa[] ASSIGNING FIELD-SYMBOL(<w_empresa>).

      TRY.
          DATA(w_detalhe_emp) = me->at_it_dados_empresa[ werks = group_werks->werks ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = w_detalhe_emp-stcd1
        IMPORTING
          output = v_cnpj.

      <w_empresa>-nr_cnpjempresa = v_cnpj.

      LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.

        "-> Acrescenta dados das férias encontradas para o funcionário
        APPEND INITIAL LINE TO <w_empresa>-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).

        <w_empregado>-matricula = <w_func>-pernr.

        APPEND INITIAL LINE TO <w_empregado>-absenteismos-absenteismo[] ASSIGNING FIELD-SYMBOL(<w_afast>).

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
        <w_afast>-data_prev_retorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ).
        <w_afast>-prev_afastamento = v_dias_afast.
        <w_afast>-tp_prev_afastamento = '2'.
        <w_afast>-data_retorno = COND #( WHEN <w_func>-endda = '99991231' THEN '' ELSE v_prev_retorno ). "V_FIM_ABS.
        <w_afast>-tempo_afastamento = v_dias_afast.
        <w_afast>-tp_afastamento = '2'.
        <w_afast>-motivo_afastamento = <w_func>-atext.
        <w_afast>-observacoes = <w_func>-atext.

        CLEAR: v_dias_afast, v_cnpj, v_data_retorno.


      ENDLOOP.

    ENDLOOP.

    DELETE s_insert_absenteismo_req-dados-empresa[] WHERE empregados-empregado IS INITIAL.

    IF ( s_insert_absenteismo_req-dados-empresa IS NOT INITIAL ).

      me->inicia_classe_webservice(
         EXPORTING
           i_port_name      = 'ZRSHR_TGG'    " Nome da porta lógica
           i_service_name   = 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
         CHANGING
            cl_web_movimento =  zcl_proxy  ).

      me->set_header_security(
        EXPORTING
          i_service_name   =  'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
        CHANGING
          cl_web_movimento = zcl_proxy
      ).

      "-> Envio das informações
      TRY.
          zcl_proxy->insert_absenteismo(
            EXPORTING
              insert_absenteismo_request  = s_insert_absenteismo_req
            IMPORTING
              insert_absenteismo_response = DATA(s_return)  ).
        CATCH   cx_ai_system_fault INTO DATA(r_msg).
      ENDTRY.

      IF ( s_return IS NOT INITIAL ) OR ( r_msg IS NOT INITIAL ).
        LOOP AT s_insert_absenteismo_req-dados-empresa[] INTO DATA(w_abs_empresa).

          LOOP AT w_abs_empresa-empregados-empregado[] INTO DATA(w_empregados).

            DATA(lva_index) = sy-tabix.

            TRY.
                DATA(w_absenteismo) = w_empregados-absenteismos-absenteismo[ 1 ].
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            DATA(w_log) = VALUE zhcmt_pa_0023(
                servico         = 'insertAbsenteismo'
                data_envio      = sy-datum
                hora_envio      = sy-uzeit
                pernr           = CONV #( w_empregados-matricula )
                bukrs           = ''
                werks           = ''
                dados_enviados  = |{ w_absenteismo-motivo_afastamento } Retorno:{ w_absenteismo-data_prev_retorno }| ).

            IF ( s_return IS NOT INITIAL ).
              LOOP AT s_return-rsdata_return-mensagens-mensagem[] INTO DATA(w_mensagem_tab) FROM lva_index.
                LOOP AT w_mensagem_tab-detalhes-mensagem_det[] INTO DATA(w_mensagem).
                  w_log-retorno_servico = w_mensagem-tx_descricao.
                  w_log-id_log          = CONV #( sy-tabix ).
                  APPEND w_log TO it_log[].
                ENDLOOP.
                EXIT.
              ENDLOOP.
            ELSE.
              IF r_msg IS NOT INITIAL.
                w_log-retorno_servico = r_msg->errortext.
                w_log-id_log          = CONV #( sy-tabix ).
                APPEND w_log TO it_log[].
              ENDIF.
            ENDIF.

            CLEAR: w_absenteismo, w_log.

          ENDLOOP.

        ENDLOOP.

      ENDIF.

      IF ( it_log[] IS NOT INITIAL ).
        MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD INSERT_EMPREGADOS.

    DATA: zcl_proxy TYPE REF TO zrsdataco_empregado_port,
          it_log    TYPE TABLE OF zhcmt_pa_0023.

    me->inicia_classe_webservice(
    EXPORTING
      i_port_name    = 'ZRSHR_TGG'    " Nome da porta lógica
      i_service_name = 'ZRSDATACO_EMPREGADO_PORT'
    CHANGING
      cl_web_empregado = zcl_proxy ).

    me->set_header_security(
      EXPORTING
        i_service_name   = 'ZRSDATACO_EMPREGADO_PORT'
      CHANGING
        cl_web_empregado = zcl_proxy ).

    TRY .
        zcl_proxy->insert_empregados(
        EXPORTING
          insert_empregados_request  = i_request " V_REQUEST_INDIVIDUAL
        IMPORTING
          insert_empregados_response = DATA(response)
      ).
      CATCH cx_ai_system_fault INTO DATA(r_msg).
    ENDTRY.

    IF ( response IS NOT INITIAL ) OR ( r_msg IS NOT INITIAL ).

      "-> Gera Log
      LOOP AT i_request-rsdata-empregados-empregado[] INTO DATA(w_dados_func).

        DATA(lva_index) = sy-tabix.

        TRY.
            DATA(w_setor_cargo) = w_dados_func-setores_cargos-setor_cargo[ 1 ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        DATA(w_log) = VALUE zhcmt_pa_0023(
              servico         = 'insertEmpregados'
              data_envio      = sy-datum
              hora_envio      = sy-uzeit
              pernr           = w_dados_func-nr_matricula
              bukrs           = w_dados_func-id_empresa
              werks           = w_dados_func-id_empresa
              dados_enviados  = |{ w_setor_cargo-cd_cargo }{ w_setor_cargo-cd_posicao_trabalho }/{ w_setor_cargo-cd_setor }| ).

        IF ( response IS NOT INITIAL ).
          LOOP AT response-rsdata_return-mensagens-mensagem[] INTO DATA(w_mensagem_tab) FROM lva_index.
            LOOP AT w_mensagem_tab-detalhes-mensagem_det[] INTO DATA(w_mensagem).
              w_log-retorno_servico = w_mensagem-tx_descricao.
              w_log-id_log = sy-tabix.
              APPEND w_log TO it_log[].
            ENDLOOP.
            EXIT.
          ENDLOOP.
        ELSE.
          IF r_msg IS NOT INITIAL.
            w_log-retorno_servico = r_msg->errortext.
            w_log-id_log = sy-tabix.
            APPEND w_log TO it_log[].
          ENDIF.
        ENDIF.

        CLEAR: w_setor_cargo, w_log. ", RESPONSE.

      ENDLOOP.

    ENDIF.

    "CLEAR: W_EMPREGADO. "V_REQUEST_INDIVIDUAL-RSDATA-EMPREGADOS-EMPREGADO[].

*    WAIT UP TO 1 SECONDS.

*  ENDLOOP.

    "-> Grava log do envio do serviço 'insertEmpregados':
    IF ( it_log[] IS NOT INITIAL ).
      MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
    ENDIF.

  ENDMETHOD.


  METHOD INSERT_EMPREGADOS_TURNOS.

    TYPES: ty_rg_schkz TYPE RANGE OF pa0007-schkz.

    DATA: s_insert_turnos_reques TYPE zrsdatamovinsert_empregados_t1,
          zcl_proxy              TYPE REF TO zrsdatamovco_movimento_emprega,
          it_log                 TYPE TABLE OF zhcmt_pa_0023,
          v_cnpj                 TYPE char20,
          v_data_ini             TYPE char20,
          v_data_fim             TYPE char20,
          v_hora_ini             TYPE char20,
          v_hora_fim             TYPE char20.

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
        APPEND INITIAL LINE TO s_insert_turnos_reques-dados-empresa[]
          ASSIGNING FIELD-SYMBOL(<w_turno_emp>).

        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = w_empresa-stcd1
          IMPORTING
            output = v_cnpj.

        <w_turno_emp>-nr_cnpjempresa = v_cnpj.

        LOOP AT it_empregados_alterados[] INTO DATA(w_func) WHERE bukrs = w_empresa-bukrs
                                                              AND werks = w_empresa-werks.


          READ TABLE it_p0007[] INTO DATA(w_p0007) WITH KEY pernr = w_func-pernr BINARY SEARCH.
          IF ( sy-subrc = 0 ).

            READ TABLE it_horarios[] INTO DATA(w_horario) WITH KEY zmodn = w_p0007-schkz BINARY SEARCH.
            IF ( sy-subrc <> 0 ).
              CONTINUE.
            ENDIF.

            APPEND INITIAL LINE TO <w_turno_emp>-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<w_turno_func>).
            APPEND INITIAL LINE TO <w_turno_func>-turnos-turno[] ASSIGNING FIELD-SYMBOL(<w_turno>).

            <w_turno_func>-matricula = w_func-pernr.

            IF ( w_horario IS NOT INITIAL ).

              <w_turno>-nome = w_horario-ztext.

              WRITE w_p0007-begda TO v_data_ini DD/MM/YYYY.
              WRITE w_p0007-endda TO v_data_fim DD/MM/YYYY.

              REPLACE ALL OCCURRENCES OF '.' IN v_data_ini WITH '/'.
              REPLACE ALL OCCURRENCES OF '.' IN v_data_fim WITH '/'.

              <w_turno>-data_inicio = v_data_ini.
              <w_turno>-data_fim = COND #( WHEN w_p0007-endda <> '99991231' THEN v_data_fim ELSE '' ).

              v_hora_ini = |{ w_horario-sobeg+0(2) }:{ w_horario-sobeg+2(2) }|.
              v_hora_fim = |{ w_horario-soend+0(2) }:{ w_horario-soend+2(2) }|.

              <w_turno>-hora_inicio = COND #(
                  WHEN v_hora_ini <> 'COMP' AND v_hora_ini <> 'DESC' THEN
                       v_hora_ini ).

              <w_turno>-hora_fim = COND #(
                  WHEN v_hora_fim <> 'COMP' AND v_hora_fim <> 'DESC' THEN
                       v_hora_fim ).

            ENDIF.

            CLEAR: v_data_ini, v_data_fim, w_p0007, w_horario.

          ENDIF.

        ENDLOOP.

        CLEAR: v_cnpj, w_func.

      ENDLOOP.

      DELETE s_insert_turnos_reques-dados-empresa[] WHERE empregados-empregado IS INITIAL.

      IF ( s_insert_turnos_reques-dados-empresa[] IS NOT INITIAL ).


        me->inicia_classe_webservice(
          EXPORTING
            i_port_name      = 'ZRSHR_TGG'    " Nome da porta lógica
            i_service_name   = 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
          CHANGING
            cl_web_movimento   = zcl_proxy ).

        me->set_header_security(
          EXPORTING
            i_service_name   = 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
          CHANGING
            cl_web_movimento = zcl_proxy ).

        TRY .
            zcl_proxy->insert_empregados_turnos(
              EXPORTING
                insert_empregados_turnos_reque = s_insert_turnos_reques
              IMPORTING
                insert_empregados_turnos_respo = DATA(s_insert_turnos_retorno)
            ).
          CATCH cx_ai_system_fault INTO DATA(r_msg).
        ENDTRY.

        IF ( s_insert_turnos_retorno IS NOT INITIAL ) OR ( r_msg IS NOT INITIAL ).
          LOOP AT s_insert_turnos_reques-dados-empresa[] INTO DATA(w_turnos_empresa).

            LOOP AT w_turnos_empresa-empregados-empregado[] INTO DATA(w_empregados).

              DATA(lva_index) = sy-tabix.

              TRY.
                  DATA(w_turno) = w_empregados-turnos-turno[ 1 ].
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              DATA(w_log) = VALUE zhcmt_pa_0023(
                  servico         = 'insertEmpregadosTurnos'
                  data_envio      = sy-datum
                  hora_envio      = sy-uzeit
                  pernr           = w_empregados-matricula
                  bukrs           = ''
                  werks           = ''
                  dados_enviados  = |{ w_turno-data_inicio }{ w_turno-data_fim }/{ w_turno-hora_inicio }/{ w_turno-hora_fim }/{ w_turno-jornada_trabalho }| ).
***                  RETORNO_SERVICO = COND #(
***                      WHEN R_MSG IS NOT INITIAL THEN R_MSG->ERRORTEXT
***                      ELSE S_INSERT_TURNOS_RETORNO-RETORNO_MSG ) ).

              IF ( s_insert_turnos_retorno IS NOT INITIAL ).
                LOOP AT s_insert_turnos_retorno-rsdata_return-mensagens-mensagem[] INTO DATA(w_mensagem_tab) FROM lva_index.
                  LOOP AT w_mensagem_tab-detalhes-mensagem_det[] INTO DATA(w_mensagem).
                    w_log-retorno_servico = w_mensagem-tx_descricao.
                    w_log-id_log          = CONV #( sy-tabix ).
                    APPEND w_log TO it_log[].
                  ENDLOOP.
                  EXIT.
                ENDLOOP.
              ELSE.
                IF r_msg IS NOT INITIAL.
                  w_log-retorno_servico = r_msg->errortext.
                  w_log-id_log          = CONV #( sy-tabix ).
                  APPEND w_log TO it_log[].
                ENDIF.
              ENDIF.

              CLEAR: w_turno, w_log.

            ENDLOOP.

          ENDLOOP.

        ENDIF.

        IF ( it_log[] IS NOT INITIAL ).
          MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
        ENDIF.

      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD INSERT_EMPRESA.

    TYPES: ty_r_bukrs TYPE RANGE OF pa0001-bukrs,
           ty_r_werks TYPE RANGE OF pa0001-werks.

    "-> Estrutura da requisição para o método InsertEmpresa
    DATA: s_req_insert_empresa TYPE zrsempresainsert_empresas_requ,
          it_log               TYPE TABLE OF zhcmt_pa_0023,
          v_cnpj               TYPE char20,
          v_paval              TYPE t001z-paval,
          v_cgc_branch         TYPE j_1bbranch-cgc_branch,
          v_cgc_number         TYPE j_1bwfield-cgc_number,
          v_cgc_company        TYPE j_1bwfield-cgc_compan,
          v_cgc_branch_aux     TYPE j_1bwfield-cgc_branch.

    DATA: zcl_proxy  TYPE REF TO zrsempresaco_empresa_port.

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

          APPEND INITIAL LINE TO s_req_insert_empresa-detalhes_empresas-detalhe_empresa[]
            ASSIGNING FIELD-SYMBOL(<w_empresa>).

          "-> número de empregados na filial:
          DATA(v_count_empregados) = REDUCE i( INIT x = 0 FOR wa IN me->at_it_dados_empregado[]
            WHERE ( werks = <lfs_dados_empresa>-werks ) NEXT x = x + 1 ).

          "-> buscando matriz da empresa:
          TRY.
              DATA(w_emp_matriz) = it_matriz[ bukrs = <lfs_dados_empresa>-bukrs ].
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          <w_empresa>-tipo_validacao = '1'.
          <w_empresa>-tipo_empresa = COND #( WHEN <lfs_dados_empresa>-werks = w_emp_matriz-filia THEN '0' ELSE '1' ).

          IF ( <w_empresa>-tipo_empresa = '1' ).

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

              <w_empresa>-cnpj_matriz = v_cgc_number.
              <w_empresa>-razao_social_matriz = w_matriz-name_adr.
              <w_empresa>-fantasia_matriz = w_matriz-name.

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

          <w_empresa>-nr_cnpj = v_cgc_number.
          <lfs_dados_empresa>-stcd1 = v_cgc_number.

*** BUG - 48856 - Fim - Camila Brand

          <w_empresa>-tipo_matricula = 'J'.
          <w_empresa>-razao_social = <lfs_dados_empresa>-name_adr.
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

        IF ( s_req_insert_empresa IS NOT INITIAL ).

          DELETE s_req_insert_empresa-detalhes_empresas-detalhe_empresa[] WHERE nr_cnpj IS INITIAL.

          me->inicia_classe_webservice(
            EXPORTING
              i_port_name      = 'ZRSHR_TGG'    " Nome da porta lógica
              i_service_name   = 'ZRSEMPRESACO_EMPRESA_PORT'
            CHANGING
              cl_web_empresa   = zcl_proxy ).

          me->set_header_security(
            EXPORTING
              i_service_name   = 'ZRSEMPRESACO_EMPRESA_PORT'
            CHANGING
              cl_web_empresa = zcl_proxy ).

          TRY .
              zcl_proxy->insert_empresas(
                EXPORTING
                  insert_empresas_request  = s_req_insert_empresa
                IMPORTING
                  insert_empresas_response = DATA(s_resp_insert_empresa)
              ).
            CATCH cx_ai_system_fault INTO DATA(r_msg).
          ENDTRY.

          LOOP AT s_req_insert_empresa-detalhes_empresas-detalhe_empresa[] ASSIGNING FIELD-SYMBOL(<lfs_empresa>).

            DATA(lva_index) = sy-tabix.

            DATA(w_log) = VALUE zhcmt_pa_0023(
                servico         = 'insertEmpresa'
                data_envio      = sy-datum
                hora_envio      = sy-uzeit
                pernr           = '00000000'
                id_log          = sy-tabix
                bukrs           = ''
                werks           = ''
                dados_enviados  = |{ <lfs_empresa>-razao_social }-{ <lfs_empresa>-fantasia }-{ <lfs_empresa>-empregados }| ).

            IF ( s_resp_insert_empresa IS NOT INITIAL ).
              LOOP AT s_resp_insert_empresa-rsdata_return-mensagens-mensagem[] INTO DATA(w_mensagem_tab) FROM lva_index.
                LOOP AT w_mensagem_tab-detalhes-mensagem_det[] INTO DATA(w_mensagem).
                  w_log-retorno_servico = w_mensagem-tx_descricao.
                  APPEND w_log TO it_log[].
                ENDLOOP.
                EXIT.
              ENDLOOP.
            ELSE.
              IF r_msg IS NOT INITIAL.
                w_log-retorno_servico = r_msg->errortext.
                APPEND w_log TO it_log[].
              ENDIF.
            ENDIF.

            CLEAR: w_log.

          ENDLOOP.

          IF ( it_log[] IS NOT INITIAL ).
            MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD INSERT_FERIAS.

    DATA: s_insert_ferias_reques TYPE zrsdatamovinsert_ferias_reques,
          zcl_proxy              TYPE REF TO zrsdatamovco_movimento_emprega,
          it_log                 TYPE TABLE OF zhcmt_pa_0023,
          v_begda                TYPE begda,
          v_endda                TYPE endda,
          v_cnpj                 TYPE char20,
          v_ini_ferias           TYPE char20,
          v_fim_ferias           TYPE char20.


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
            APPEND INITIAL LINE TO s_insert_ferias_reques-dados-empresa[]
              ASSIGNING FIELD-SYMBOL(<w_empresa>).


*            CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
*              EXPORTING
*                INPUT  = GROUP_WERKS->STCD1
*              IMPORTING
*                OUTPUT = V_CNPJ.

            "-> Acrescenta dados da empresa
            <w_empresa>-nr_cnpjempresa = w_dados_empresa-stcd1. "V_CNPJ.

            LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.

              "-> Acrescenta dados do empregado
              APPEND INITIAL LINE TO <w_empresa>-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).

              <w_empregado>-matricula = <w_func>-pernr.

              "-> Acrescenta dados das férias encontradas para o funcionário
              APPEND INITIAL LINE TO <w_empregado>-ferias-feria[] ASSIGNING FIELD-SYMBOL(<w_ferias>).

              WRITE <w_func>-begda TO v_ini_ferias DD/MM/YYYY.
              WRITE <w_func>-endda TO v_fim_ferias DD/MM/YYYY.

              REPLACE ALL OCCURRENCES OF '.' IN v_ini_ferias WITH '/'.
              REPLACE ALL OCCURRENCES OF '.' IN v_fim_ferias WITH '/'.

              <w_ferias>-data_inicio = v_ini_ferias.
              <w_ferias>-data_fim    = v_fim_ferias.
              <w_ferias>-excluir = '0'.

            ENDLOOP.

            CLEAR: v_cnpj.

          ENDIF.

        ENDLOOP.

        IF ( s_insert_ferias_reques IS NOT INITIAL ).

          me->inicia_classe_webservice(
            EXPORTING
              i_port_name      = 'ZRSHR_TGG'    " Nome da porta lógica
              i_service_name   = 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
            CHANGING
               cl_web_movimento =  zcl_proxy  ).

          me->set_header_security(
            EXPORTING
              i_service_name   =  'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
            CHANGING
              cl_web_movimento = zcl_proxy
          ).


          "-> Envio das informações
          TRY.
              zcl_proxy->insert_ferias(
                EXPORTING
                    insert_ferias_request  = s_insert_ferias_reques
                IMPORTING
                    insert_ferias_response = DATA(s_insert_ferias_resposta) ).
            CATCH cx_ai_system_fault INTO DATA(r_msg).
          ENDTRY.

          IF ( s_insert_ferias_resposta IS NOT INITIAL ) OR ( r_msg IS NOT INITIAL ).

            "-> Armazenar log do envio
            LOOP AT s_insert_ferias_reques-dados-empresa[] INTO DATA(w_ferias_empresa).

              LOOP AT w_ferias_empresa-empregados-empregado[] INTO DATA(w_empregados).

                DATA(lva_index) = sy-tabix.

                TRY.
                    DATA(w_emp_ferias) = w_empregados-ferias-feria[ 1 ].
                  CATCH cx_sy_itab_line_not_found.
                ENDTRY.

                DATA(w_log) = VALUE zhcmt_pa_0023(
                    servico         = 'insertFerias'
                    data_envio      = sy-datum
                    hora_envio      = sy-uzeit
                    pernr           = w_empregados-matricula
                    bukrs           = ''
                    werks           = ''
                    dados_enviados  = |{ w_emp_ferias-data_inicio }/{ w_emp_ferias-data_fim }| ).
***                    RETORNO_SERVICO = COND #(
***                  WHEN R_MSG IS NOT INITIAL THEN R_MSG->ERRORTEXT
***                  ELSE S_INSERT_FERIAS_RESPOSTA-RETORNO_MSG ) ).

                IF ( s_insert_ferias_resposta IS NOT INITIAL ).
                  LOOP AT s_insert_ferias_resposta-rsdata_return-mensagens-mensagem[] INTO DATA(w_mensagem_tab) FROM lva_index.
                    LOOP AT w_mensagem_tab-detalhes-mensagem_det[] INTO DATA(w_mensagem).
                      w_log-retorno_servico = w_mensagem-tx_descricao.
                      w_log-id_log          = CONV #( sy-tabix ).
                      APPEND w_log TO it_log[].
                    ENDLOOP.
                    EXIT.
                  ENDLOOP.
                ELSE.
                  IF r_msg IS NOT INITIAL.
                    w_log-retorno_servico = r_msg->errortext.
                    w_log-id_log          = CONV #( sy-tabix ).
                    APPEND w_log TO it_log[].
                  ENDIF.
                ENDIF.

                CLEAR: w_emp_ferias, w_log.

              ENDLOOP.

            ENDLOOP.

          ENDIF.

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
            APPEND INITIAL LINE TO s_insert_ferias_reques-dados-empresa[]
              ASSIGNING FIELD-SYMBOL(<w_empresa>).


*            CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
*              EXPORTING
*                INPUT  = GROUP_WERKS->STCD1
*              IMPORTING
*                OUTPUT = V_CNPJ.

            "-> Acrescenta dados da empresa
            <w_empresa>-nr_cnpjempresa = w_dados_empresa-stcd1. "V_CNPJ.

            LOOP AT GROUP group_werks ASSIGNING FIELD-SYMBOL(<w_func>) WHERE werks = group_werks->werks.

              "-> Acrescenta dados do empregado
              APPEND INITIAL LINE TO <w_empresa>-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<w_empregado>).

              <w_empregado>-matricula = <w_func>-pernr.

              "-> Acrescenta dados das férias encontradas para o funcionário
              APPEND INITIAL LINE TO <w_empregado>-ferias-feria[] ASSIGNING FIELD-SYMBOL(<w_ferias>).

              WRITE <w_func>-begda TO v_ini_ferias DD/MM/YYYY.
              WRITE <w_func>-endda TO v_fim_ferias DD/MM/YYYY.

              REPLACE ALL OCCURRENCES OF '.' IN v_ini_ferias WITH '/'.
              REPLACE ALL OCCURRENCES OF '.' IN v_fim_ferias WITH '/'.

              <w_ferias>-data_inicio = v_ini_ferias.
              <w_ferias>-data_fim    = v_fim_ferias.
              <w_ferias>-excluir = '0'.

            ENDLOOP.

            CLEAR: v_cnpj.

          ENDIF.

        ENDLOOP.

        IF ( s_insert_ferias_reques IS NOT INITIAL ).

          me->inicia_classe_webservice(
            EXPORTING
              i_port_name      = 'ZRSHR_TGG'    " Nome da porta lógica
              i_service_name   = 'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
            CHANGING
               cl_web_movimento =  zcl_proxy  ).

          me->set_header_security(
            EXPORTING
              i_service_name   =  'ZRSDATAMOVCO_MOVIMENTO_EMPREGA'
            CHANGING
              cl_web_movimento = zcl_proxy
          ).


          "-> Envio das informações
          TRY.
              zcl_proxy->insert_ferias(
                EXPORTING
                    insert_ferias_request  = s_insert_ferias_reques
                IMPORTING
                    insert_ferias_response = DATA(s_insert_ferias_resposta) ).
            CATCH cx_ai_system_fault INTO DATA(r_msg).
          ENDTRY.

          IF ( s_insert_ferias_resposta IS NOT INITIAL ) OR ( r_msg IS NOT INITIAL ).

            "-> Armazenar log do envio
            LOOP AT s_insert_ferias_reques-dados-empresa[] INTO DATA(w_ferias_empresa).

              LOOP AT w_ferias_empresa-empregados-empregado[] INTO DATA(w_empregados).

                DATA(lva_index) = sy-tabix.

                TRY.
                    DATA(w_emp_ferias) = w_empregados-ferias-feria[ 1 ].
                  CATCH cx_sy_itab_line_not_found.
                ENDTRY.

                DATA(w_log) = VALUE zhcmt_pa_0023(
                    servico         = 'insertFerias'
                    data_envio      = sy-datum
                    hora_envio      = sy-uzeit
                    pernr           = w_empregados-matricula
                    bukrs           = ''
                    werks           = ''
                    dados_enviados  = |{ w_emp_ferias-data_inicio }/{ w_emp_ferias-data_fim }| ).
***                    RETORNO_SERVICO = COND #(
***                  WHEN R_MSG IS NOT INITIAL THEN R_MSG->ERRORTEXT
***                  ELSE S_INSERT_FERIAS_RESPOSTA-RETORNO_MSG ) ).

                IF ( s_insert_ferias_resposta IS NOT INITIAL ).
                  LOOP AT s_insert_ferias_resposta-rsdata_return-mensagens-mensagem[] INTO DATA(w_mensagem_tab) FROM lva_index.
                    LOOP AT w_mensagem_tab-detalhes-mensagem_det[] INTO DATA(w_mensagem).
                      w_log-retorno_servico = w_mensagem-tx_descricao.
                      w_log-id_log          = CONV #( sy-tabix ).
                      APPEND w_log TO it_log[].
                    ENDLOOP.
                    EXIT.
                  ENDLOOP.
                ELSE.
                  IF r_msg IS NOT INITIAL.
                    w_log-retorno_servico = r_msg->errortext.
                    w_log-id_log          = CONV #( sy-tabix ).
                    APPEND w_log TO it_log[].
                  ENDIF.
                ENDIF.

                CLEAR: w_emp_ferias, w_log.

              ENDLOOP.

            ENDLOOP.

          ENDIF.

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
      v_dt_fim                    TYPE char20.

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

*    SORT it_func[] BY bukrs ASCENDING.
*    DELETE it_func[] WHERE bukrs NOT IN  me->at_bukrs.

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

*    DATA(rg_plans_hist) = VALUE ty_rg_plans( FOR w_pa01 IN it_hist_func[] (
*        sign   = 'I'
*        option = 'EQ'
*        low    = w_pa01-plans
*        high   = w_pa01-plans    )  ).
*    SORT rg_plans_hist[] BY low ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM rg_plans_hist[] COMPARING low.

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
            CLEAR: v_cnpj.

            READ TABLE me->at_it_dados_empresa[] INTO DATA(w_empresa_ant) WITH KEY werks = <w_hist_func>-werks BINARY SEARCH.
            IF ( sy-subrc = 0 ).
              CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
                EXPORTING
                  input  = w_empresa_ant-stcd1
                IMPORTING
                  output = v_cnpj.

              <w_empregado>-nr_cnpjempresa = v_cnpj.
            ENDIF.
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

*        ENDIF.

      ENDLOOP.

      IF ( lines( s_transfer_empreg_req-rsdata-empregados-empregado[] ) > 0 ).

        me->inicia_classe_webservice(
            EXPORTING
              i_port_name      = 'ZRSHR_TGG'    " Nome da porta lógica
              i_service_name   = 'ZRSDATACO_EMPREGADO_PORT'
            CHANGING
               cl_web_empregado =  zcl_proxy  ).

        me->set_header_security(
          EXPORTING
            i_service_name   =  'ZRSDATACO_EMPREGADO_PORT'
          CHANGING
            cl_web_empregado = zcl_proxy
        ).

        "-> Envio das informações
        TRY.
            zcl_proxy->transferir_empregados(
              EXPORTING
                transferir_empregados_request  = s_transfer_empreg_req
              IMPORTING
                transferir_empregados_response = DATA(s_transfer_return)  ).
          CATCH cx_ai_system_fault INTO DATA(r_msg).
        ENDTRY.

        IF ( s_transfer_return IS NOT INITIAL ).
          LOOP AT s_transfer_empreg_req-rsdata-empregados-empregado[] ASSIGNING FIELD-SYMBOL(<lfs_emp>).
            DATA(lva_tabix) = sy-tabix.
            LOOP AT it_log[] ASSIGNING FIELD-SYMBOL(<w_log>) WHERE pernr = <lfs_emp>-nr_matricula.
              LOOP AT s_transfer_return-rsdata_return-mensagens-mensagem[] ASSIGNING FIELD-SYMBOL(<lfs_mensagens>) FROM lva_tabix.
                LOOP AT <lfs_mensagens>-detalhes-mensagem_det[] ASSIGNING FIELD-SYMBOL(<lfs_mensagem>).
                  <w_log>-retorno_servico = <lfs_mensagem>-tx_descricao.
                  <w_log>-id_log = CONV #( lva_tabix ).
                ENDLOOP.
                EXIT.
              ENDLOOP.
              IF ( s_transfer_return-rsdata_return-mensagens-mensagem[] IS INITIAL ).
                <w_log>-retorno_servico = r_msg->errortext.
                <w_log>-id_log = CONV #( lva_tabix ).
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDIF.


        IF ( it_log[] IS NOT INITIAL ).
          MODIFY zhcmt_pa_0023 FROM TABLE it_log[].
        ENDIF.

      ENDIF.

    ENDIF.

***    ENDIF.


  ENDMETHOD.
ENDCLASS.
