class ZCL_PERFIL_ACESSO definition
  public
  final
  create public .

public section.

  data GV_MESSAGE type CHAR255 .
  data GV_LOG_HANDLE type BALLOGHNDL .

  methods GET_PERFIS_ACESSO
    importing
      value(I_PARAM) type RS_T_SELECT optional
    returning
      value(E_RESULT) type ZPERE0001_T .
  methods CREATE_RAS
    importing
      value(I_PARAM) type ZPERE0001_T optional
      !I_TIPO_REMOV type CHAR10 .
  methods GERAR_ARQ_EXCEL
    importing
      !I_PARAM type ZPERE0001_T
    exporting
      !LO_TABLE type ref to CL_SALV_TABLE .
  methods CONVERT_ARQUIVO_TO_BIN
    importing
      !I_ARQUIVO type ref to CL_SALV_TABLE
    exporting
      !E_STREAM type XML_RAWDATA .
  methods CONVERT_ARQUIVO_TO_BASE64
    importing
      !I_ARQUIVO type ZPERE0001_T
      !I_TIPO_REMOV type CHAR10
    exporting
      !E_BASE64 type STRING .
  methods GET_LOGS_PROC
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(E_RESULT) type ZPERE0002_T .
  methods REMOVER_PERFIL
    importing
      !I_RAS type ZZID_SE
    returning
      value(E_RETORNO) type CHAR01 .
  methods ADD_MSG_LOG .
  methods SAVE_LOG .
  methods ADD_PERFIL
    importing
      value(I_PARAM) type ZPERE0002_T
    exporting
      value(E_PARAM) type ZPERE0002_T .
  methods GET_CCUSTO_ATIVO
    returning
      value(E_RESULT) type ZPERE0003_T .
  methods GET_PERFIS_ACESSO_TRANS
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(E_RESULT) type ZPERE0001_T .
  methods GET_LOGS_ACESSO
    importing
      !I_PARAM type RS_T_SELECT
    returning
      value(E_RESULT) type ZPERE0004_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PERFIL_ACESSO IMPLEMENTATION.


  method ADD_MSG_LOG.

    DATA: ls_msg TYPE bal_s_msg.

      MOVE-CORRESPONDING syst TO ls_msg.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = me->gv_log_handle
          i_s_msg          = ls_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.
      CLEAR me->gv_message.

  endmethod.


  method add_perfil.
    data: it_zpere0002 type zpere0002_t.
    data: lt_activity_groups type table of agr_txt.
    data: ls_activity_groups type agr_txt,
          it_zpert0001       type table of zpert0001.


    check i_param is not initial.
    free: it_zpert0001.
    select * from zpert0001
    into table it_zpert0001
      for all entries in i_param
      where usuario eq i_param-usuario
        and perfil_composto eq i_param-perfil_composto
        and perfil_simples eq i_param-perfil_simples.

*   add roles simples a uma role composta
    it_zpere0002 = i_param.
    e_param      = i_param.

    sort i_param by usuario perfil_composto.
    delete adjacent duplicates from i_param  comparing usuario perfil_composto.

    loop at i_param assigning field-symbol(<ws_param>).
      loop at it_zpere0002 into data(ws_zpere0002) where usuario = <ws_param>-usuario and perfil_composto = <ws_param>-perfil_composto.
        move ws_zpere0002-perfil_simples to ls_activity_groups-agr_name.
        append ls_activity_groups to lt_activity_groups.
      endloop.

      call function 'PRGN_RFC_ADD_AGRS_TO_COLL_AGR'
        exporting
          activity_group  = <ws_param>-perfil_composto
        tables
          activity_groups = lt_activity_groups.

      if sy-subrc = 0.
*   Rôles ont été ajoutées au rôle &
        message s022(zmpo) with <ws_param>-perfil_composto into me->gv_message.
        me->add_msg_log( ).

        loop at it_zpert0001 assigning field-symbol(<ws_param_2>) where usuario eq <ws_param>-usuario and perfil_composto eq <ws_param>-perfil_composto.
          <ws_param_2>-status = 'R'.
          <ws_param_2>-desc_status = 'Perfil retornado ao usuario'.
          <ws_param_2>-user_modif  = sy-uname.
          <ws_param_2>-date_modif  = sy-datum.
          <ws_param_2>-hr_modif    = sy-uname.
        endloop.
      endif.
      free: lt_activity_groups.
      clear: ws_zpere0002.
    endloop.

    if it_zpert0001 is not initial.
      modify zpert0001 from table it_zpert0001.
      commit work.
    endif.
  endmethod.


  method convert_arquivo_to_base64.
    data: lt_lines   type table of string,
          lv_string  type string,
          lv_lines   type string,
          lv_xstring type xstring,
          lv_base64  type string.        "Base64

    check i_arquivo is not initial.

    clear: lt_lines, lv_xstring, e_base64.
    " Conteúdo do CSV como uma tabela interna de strings (cada linha representa uma linha no arquivo)

    if i_tipo_remov eq 'TRANS'.
       append 'Perfil simples;Transação' to lt_lines.       " Cabeçalho
      loop at i_arquivo assigning field-symbol(<ws_arquivo>).
        lv_lines = |{ <ws_arquivo>-perfil_simples };{ <ws_arquivo>-transacao }|.
        append lv_lines to lt_lines.
      endloop.
    else.
      append 'Nome do usuário;Perfil composto;Perfil simples;Transação' to lt_lines.       " Cabeçalho
      loop at i_arquivo assigning <ws_arquivo>.
        lv_lines = |{ <ws_arquivo>-usuario };{ <ws_arquivo>-perfil_composto };{ <ws_arquivo>-perfil_simples }|.
        append lv_lines to lt_lines.
      endloop.
    endif.

    " Concatenar as linhas com CR_LF para criar uma única string
    loop at lt_lines into data(lv_line).
      concatenate lv_string lv_line cl_abap_char_utilities=>newline into lv_string.
    endloop.

*    *Convert string to xstring
    call function 'SCMS_STRING_TO_XSTRING'
      exporting
        text   = lv_string
      importing
        buffer = lv_xstring
      exceptions
        failed = 1
        others = 2.


    " Passo 4: Codificar o xstring para Base64
    call function 'SCMS_BASE64_ENCODE_STR'
      exporting
        input  = lv_xstring
      importing
        output = e_base64.
  endmethod.


  method convert_arquivo_to_bin.
    data: lo_table      type ref to cl_salv_table,
          lx_xml        type xstring,
          ls_length     type i,
          ls_xml_stream type xml_rawdata.

    check i_arquivo is not initial.

    lx_xml = i_arquivo->to_xml( xml_type = '10' ). "XLSX

    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer        = lx_xml
      importing
        output_length = ls_length
      tables
        binary_tab    = e_stream.

  endmethod.


  method create_ras.
    types: begin of ty_body_retorn,
             message     type string,
             status_code type string,
             detail      type string,
             response    type string,
           end of ty_body_retorn.


    data: usuario      type zde_usuario,
          zvar_texto   type string,
          lo_table     type ref to cl_salv_table,
          zvar_estream type xml_rawdata.

    data: lc_retorno           type ty_body_retorn,
          zdata                type string,
          l_data               type table of w3mime,
          zva_modulo           type char05,
          lva_dt_start         type erdat,
          lva_duration_integer type i,
          rg_user              type range of xubname,
          base64               type string.

    data: zvar_dias       type p value '90',
          zvar_date_base  type sy-datum,
          zvar_date_atual type char10,
          zvar_date_ini   type char10,
          zv_periodo      type char30.

    data: it_zpert0001 type table of zpert0001.

    free: it_zpert0001.

    check i_param is not initial.

    "Converter dados em arquivo CSV and BASE64.
    clear: base64.
    me->convert_arquivo_to_base64(
      exporting
        i_arquivo    = i_param                " Estrutura de dados perfil de acesso
        i_tipo_remov = i_tipo_remov
      importing
        e_base64     = base64
    ).

    "Seleciona quantidade de dias / acesso transação.
    select single * from tvarvc into
    @data(ws_perf)
    where name eq 'ZPERFIL_EXPIRA_ACESSO_DIAS'.
    if sy-subrc eq 0.
      zvar_dias = ws_perf-low.
    endif.
    zvar_date_base = sy-datum - zvar_dias.

    zvar_date_ini   = |{ zvar_date_base+6(2) }/{ zvar_date_base+4(2) }/{ zvar_date_base+0(4) }|.
    zvar_date_atual = |{ sy-datum+6(2) }/{ sy-datum+4(2) }/{ sy-datum+0(4) }|.

    "Seleciona usuario.
    select single samaccountname from zhcmt0007
    into usuario
    where bname eq sy-uname.
    if usuario is initial.
      usuario = 'anderson.oenning'.
    endif.

    "Monta JSON / BODY.
    zvar_texto = |Revisão de Perfil SAP período de { zvar_date_ini } a { zvar_date_atual }, conforme listagem anexo abaixo.|.
    if base64 is not initial.
      data(zbody) = '{"iniciador": " ' && usuario && '",'
               && '"sistema": "SAP",'
               && '"descricao":"' && zvar_texto && '",'
               && '"anexos": [{"nome_arquivo": "perfil_acesso.csv","conteudo":"' && base64 && '"}]}'.
    else.
      zbody   = '{"iniciador": " ' && usuario && '",'
               && '"sistema": "SAP",'
               && '"descricao":"' && zvar_texto && '"'
               && '"anexos": []}'.

    endif.

    try .
        "Chama API para criar incidente.
        zcl_int_ob_create_ras=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = zbody importing e_integracao = data(r_response) ).
        if r_response is not initial.
          /ui2/cl_json=>deserialize( exporting json = r_response-ds_data_retorno changing data = lc_retorno ).
          if lc_retorno is not initial.
            it_zpert0001 = value #( for l in i_param (
                                                      usuario         = l-usuario
                                                      perfil_composto = l-perfil_composto
                                                      Perfil_simples  = l-perfil_simples
                                                      kostl           = l-kostl
                                                      transacao       = l-transacao
                                                      numero_ras      = lc_retorno-response
                                                      date_criacao    = sy-datum
                                                      hr_criacao      = sy-uzeit
                                                      user_criacao    = sy-uname
                                                      tipo_proc       = 'A' "Abertura chamado no SE
                                                      id_integracao   = ''
                                                      status          = 'P' "Enviado solicitação para o SE abertura chamado RAS.
                                                      tipo_remov      = i_tipo_remov
                                                      desc_status     = |Aguardando aprovação { lc_retorno-response }|
            ) ).

            if it_zpert0001 is not initial.
              modify zpert0001 from table it_zpert0001.
              commit work.

              message s024(sd) with 'Chamado aberto com sucesso nº' lc_retorno-response.
            endif.
          endif.
        endif.
      catch zcx_integracao into data(ws_integracao).
        message s024(sd) with 'Erro ao abrir chamado RAS'.
      catch zcx_error into data(ws_erro).
        message s024(sd) with 'Erro ao abrir chamado RAS'.
    endtry.
  endmethod.


  method gerar_arq_excel.

    types: begin of ty_data,
             fieldname type c length 1024,
           end of ty_data.

    data: lt_fieldname type standard table of ty_data,
          ls_fieldname type ty_data.

    data: it_dados type standard table of zpere0001.

    check i_param is not initial.

    it_dados = i_param.

    "Cabecalho.
    ls_fieldname-fieldname = 'Descrição do status'.
    append ls_fieldname to lt_fieldname. clear: ls_fieldname.
    ls_fieldname-fieldname = 'Usuario'.
    append ls_fieldname to lt_fieldname. clear: ls_fieldname.
    ls_fieldname-fieldname = 'Perfil'.
    append ls_fieldname to lt_fieldname. clear: ls_fieldname.
    ls_fieldname-fieldname = 'Tranção'.
    append ls_fieldname to lt_fieldname. clear: ls_fieldname.

    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = lo_table
          changing
            t_table      = it_dados ).
      catch cx_salv_msg.
    endtry.

  endmethod.


  method get_ccusto_ativo.
    data: lv_date           type sy-datum,
          linha_selecionada type slis_selfield,
          _exit             type c.


    "Seleciona usuario ativos.
    select a~kostl, a~ccusto, a~bukrs, a~butxt, a~werks, a~werksn, b~gltgb, c~numero_ras
    from zhcmt0007 as a
    inner join usr02 as b on b~bname eq a~bname
    left join zpert0001 as c on c~kostl eq a~kostl
    into table @data(it_dados_ccusto)
      where b~gltgb eq @lv_date or
            b~gltgb >= @sy-datum.
    if sy-subrc eq 0.
      sort it_dados_ccusto by kostl bukrs werks.
      delete adjacent duplicates from it_dados_ccusto comparing kostl bukrs werks.

      "Buscar ras aberta para centro de custo.
      select * from zpert0001 into table @data(it_dados_log)
        for all entries in @it_dados_ccusto
        where kostl eq @it_dados_ccusto-kostl.
*      if sy-subrc eq 0.
        move-corresponding it_dados_ccusto to e_result.

        sort it_dados_log descending by numero_ras date_criacao hr_criacao.

        loop at e_result assigning field-symbol(<ws_resul>) where numero_ras ne space.
          read table it_dados_log into data(ws_log) with key kostl = <ws_resul>-kostl.
          if sy-subrc eq 0.
            <ws_resul>-numero_ras   = ws_log-numero_ras.
            <ws_resul>-date_criacao = ws_log-date_criacao.
            <ws_resul>-hr_criacao   = ws_log-hr_criacao.
          endif.
        endloop.
*      endif.
    endif.

  endmethod.


  method get_logs_acesso.

    data: rg_date  type range of sy-datum,
          rg_uname type range of rslguser,
          RG_trans type range of char255.

    if i_param is not initial.
      loop at i_param into data(wa_param).
        case wa_param-fieldnm.
          when 'DATE'.
            rg_date = value #( ( sign = 'I' option = 'BT' low = wa_param-low high = wa_param-high ) ).
          when 'UNAME'.
            rg_uname = value #( ( sign = 'I' option = 'EQ' low = wa_param-low ) ).
          when 'TCODE'.
            rg_trans = value #( ( sign = 'I' option = 'EQ' low = wa_param-low ) ).
          when others.
        endcase.
      endloop.
    endif.

    select a~data a~hora a~slguser b~cname b~funcao b~bukrs b~butxt b~werks b~werksn b~kostl b~ccusto a~sal_data c~ttext
      from ztd_opns_018 as a
      left join zhcmt0007 as b on b~bname eq a~slguser
      left join tstct as c on c~tcode eq a~sal_data and c~sprsl eq sy-langu
      into corresponding fields of table e_result
      where data in rg_date
        and slguser in rg_uname
        and sal_data in RG_trans.

  endmethod.


  method get_logs_proc.

    data: rg_date     type range of sy-datum,
          rg_uname    type range of xubname,
          rg_perfil_c type range of agr_name,
          rg_perfil_s type range of agr_name,
          rg_ras      type range of zpert0001-numero_ras,
          rg_ccusto   type range of kostl.

    if i_param is not initial.
      loop at i_param into data(wa_param).
        case wa_param-fieldnm.
          when 'DATE'.
            rg_date = value #( ( sign = 'I' option = 'BT' low = wa_param-low high = wa_param-high ) ).
          when others.
        endcase.
      endloop.
    endif.

    if i_param is not initial.
      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'UNAME'.
            rg_uname = value #( ( sign = 'I' option = 'EQ' low = wa_param-low ) ).
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'PERFIL_COMPOSTO'.
            rg_perfil_c = value #( ( sign = 'I' option = 'EQ' low = wa_param-low ) ).
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'PERFIL_SIMPLES'.
            rg_perfil_s = value #( ( sign = 'I' option = 'EQ' low = wa_param-low ) ).
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'CENTRO_CUSTO'.
            rg_ccusto = value #( ( sign = 'I' option = 'EQ' low = wa_param-low ) ).
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'RAS'.
            rg_ras = value #( ( sign = 'I' option = 'EQ' low = wa_param-low ) ).
          when others.
        endcase.
      endloop.
    endif.

    "Seleciona dados logs processamento.
    select distinct a~*, b~cname, b~funcao, b~werks, b~werksn, b~kostl, b~ccusto, d~stext
    from zpert0001 as a
    inner join zhcmt0007 as b on b~bname eq a~usuario
    inner join pa0001 as c on c~pernr eq b~pernr and c~endda >= @sy-datum
    inner join hrp1000 as d on d~objid eq c~orgeh
    into corresponding fields of table @e_result
    where a~date_criacao in @rg_date
      and a~usuario in @rg_uname
      and a~perfil_composto in @rg_perfil_c
      and a~perfil_simples in @rg_perfil_s
      and a~numero_ras in @rg_ras
      and b~kostl in @rg_ccusto
      and a~tipo_proc ne 'M'.


    if sy-subrc eq 0.
      loop at e_result assigning field-symbol(<ws_result>).
        case <ws_result>-status.
          when 'P'. "Pendente aprovação RAS para remover o perfil do usuario.
            <ws_result>-icon_name = icon_yellow_light.
          when 'A'. "RAS aprovada e removido perfil do usuario.
            <ws_result>-icon_name = icon_release.
          when 'R'. "Retornado perfil para o usuario.
            <ws_result>-icon_name = icon_system_undo. "Perfil retornado.
          when others.
        endcase.
      endloop.
    endif.



  endmethod.


  method get_perfis_acesso.

    " Definição da estrutura para a tabela de dados
    types: begin of ty_dados,
             uname           type rslguser,
             perfil_composto type agr_name,
             perfil_simples  type agr_name,
             tcode           type char255, "agxreport,
           end of ty_dados.

    data: it_agr_users type table of ty_dados,
          tg_agr_users type table of ty_dados,
          it_ZPERE0001 type zpere0001_t.

    data: zvar_dias      type p value '90',
          zvar_date_base type sy-datum,
          zquant_acesso  type p.

    data: rg_uname    type range of xubname,
          rg_perfil_c type range of agr_name,
          rg_perfil_s type range of agr_name,
          rg_ccusto   type range of kostl.
*          tg_agr_users type table of agr_users.

    if i_param is not initial.
      loop at i_param into data(wa_param).
        case wa_param-fieldnm.
          when 'UNAME'.
            append value #( sign = 'I' option = 'EQ' low = wa_param-low  ) to rg_uname.
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'PERFIL_COMPOSTO'.
            append value #( sign = 'I' option = 'EQ' low = wa_param-low  ) to rg_perfil_c.
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'PERFIL_SIMPLES'.
            append value #( sign = 'I' option = 'EQ' low = wa_param-low ) to rg_perfil_s.
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'CENTRO_CUSTO'.
            append  value #( sign = 'I' option = 'EQ' low = wa_param-low ) to rg_ccusto.
          when others.
        endcase.
      endloop.
    endif.

    "Seleciona quantidade de dias / acesso transação.
    select single * from tvarvc into
    @data(ws_perf)
    where name eq 'ZPERFIL_EXPIRA_ACESSO_DIAS'.
    if sy-subrc eq 0.
      zvar_dias = ws_perf-low.
    endif.
    zvar_date_base = sy-datum - zvar_dias.

    select a~uname, a~agr_name as perfil_composto, b~child_agr as perfil_simples, c~tcode
    from agr_users as a
    left join agr_agrs as b on b~agr_name eq a~agr_name and b~child_agr like 'YS:%'
    left join agr_tcodes as c on c~agr_name eq b~child_agr
    left join zhcmt0007 as d on d~bname eq a~uname
    into table @it_agr_users
    where a~uname  in @rg_uname"@it_user-bname
    and a~agr_name in @rg_perfil_c
    and b~child_agr in @rg_perfil_s
    and d~kostl     in @rg_ccusto
    and a~agr_name like 'YC:%'.

    if it_agr_users is not initial.

      "Dados do usuario.
      select distinct a~bname, a~cname, a~bukrs, a~butxt, a~werks, a~werksn, a~funcao, a~kostl, a~ccusto, c~stext
      from zhcmt0007 as a
      left join pa0001 as b on b~pernr eq a~pernr and b~endda >= @sy-datum
      left join hrp1000 as c on c~objid eq b~orgeh
      into table @data(it_dados_user)
      for all entries in @it_agr_users
      where a~bname eq @it_agr_users-uname.


      select * from zpert0001 into table @data(it_zpert0001)
        for all entries in @it_agr_users
        where usuario eq @it_agr_users-uname
          and perfil_composto eq @it_agr_users-perfil_composto
          and perfil_simples eq @it_agr_users-perfil_simples
          and status ne 'A'.

      "Dados log de acesso.
      select a~slguser, a~slgrepna, a~sal_data
**          --a~data, a~hora,
      from ztd_opns_018 as a
      into table @data(it_log_Acesso)
        for all entries in @it_agr_users
        where  a~sal_data eq @it_agr_users-tcode
           and a~data >= @zvar_date_base.


      "Seleciona transação exceção.
      select * from zpft0001 into table @data(it_tcode_exc).
      sort it_tcode_exc by tcode.
    endif.


    if it_agr_users is not initial.
      tg_agr_users = it_agr_users.

      sort: it_agr_users  by uname perfil_composto perfil_simples.
      sort: tg_agr_users  by uname perfil_composto perfil_simples tcode.
      sort: it_zpert0001  by usuario perfil_composto perfil_simples.
      delete adjacent duplicates from it_agr_users comparing uname perfil_composto perfil_simples.
    endif.

    sort: it_log_Acesso by slguser sal_data.
    delete adjacent duplicates from it_log_Acesso comparing slguser sal_data.

    loop at it_agr_users assigning field-symbol(<ws_agr_users>).
      clear: zquant_acesso.

      append initial line to e_result assigning field-symbol(<fs_RESULT>).
      <fs_RESULT>-usuario          = <ws_agr_users>-uname.
      <fs_RESULT>-perfil_composto  = <ws_agr_users>-perfil_composto.
      <fs_RESULT>-perfil_simples   = <ws_agr_users>-perfil_simples.
      <fs_RESULT>-transacao        = <ws_agr_users>-tcode.

      read table it_dados_user into data(wa_dados_user) with key bname = <ws_agr_users>-uname.
      if sy-subrc eq 0.
        <fs_RESULT>-cname  =  wa_dados_user-cname .
        <fs_RESULT>-werks  =  wa_dados_user-werks .
        <fs_RESULT>-werksn =  wa_dados_user-werksn.
        <fs_RESULT>-stext  =  wa_dados_user-stext.
        <fs_RESULT>-funcao =  wa_dados_user-funcao.
        <fs_RESULT>-kostl  =  wa_dados_user-kostl.
        <fs_RESULT>-ccusto =  wa_dados_user-ccusto.
      endif.

      loop at tg_agr_users assigning field-symbol(<ws_users>) where uname            = <ws_agr_users>-uname
                                                                and perfil_composto  = <ws_agr_users>-perfil_composto
                                                                and perfil_simples   = <ws_agr_users>-perfil_simples.

        read table it_log_Acesso  assigning field-symbol(<wa_log>) with key sal_data = <ws_users>-tcode. "slguser  = <ws_users>-uname
        if sy-subrc eq 0.
          add 1 to zquant_acesso.
        else.
          "Verifica se a transação esta na tabela de exceção.
          read table it_tcode_exc into data(wa_tcode_exc) with key tcode = <ws_users>-tcode.
          if sy-subrc eq 0.
            add 1 to zquant_acesso.
          endif.
        endif.
      endloop.

      if zquant_acesso > 0.
        <fs_RESULT>-zquant_acesso = zquant_acesso.
        condense <fs_RESULT>-zquant_acesso no-gaps.
        <fs_RESULT>-status      = abap_false.
        <fs_RESULT>-desc_status = 'Acesso liberado'.
        <fs_RESULT>-icon_name   = icon_green_light.
      else.
        <fs_RESULT>-icon_name   = icon_red_light.
        <fs_RESULT>-zquant_acesso = 0.
        condense <fs_RESULT>-zquant_acesso no-gaps.
        <fs_RESULT>-status      = abap_true.
        <fs_RESULT>-desc_status = 'Bloquear acesso'.
      endif.

      read table it_zpert0001 into data(ws_zpert0001) with key usuario = <ws_agr_users>-uname
                                                            perfil_composto = <ws_agr_users>-perfil_composto
                                                            perfil_simples = <ws_agr_users>-perfil_simples binary search.
      if sy-subrc eq 0 and ws_zpert0001-status eq 'P'.
        <fs_RESULT>-numero_ras = ws_zpert0001-numero_ras.
        <fs_RESULT>-icon_name   = icon_yellow_light.
        <fs_RESULT>-desc_status = 'Aguardando aprovação'.
      endif.

*      if <fs_RESULT>-status eq 'P'.
*        <fs_RESULT>-desc_status = 'Aguardando aprovação'.
*        <fs_RESULT>-icon_name   = icon_yellow_light.
*      endif.
    endloop.
    clear: wa_dados_user.

    sort: e_result  by usuario perfil_composto perfil_simples transacao.
  endmethod.


  method get_perfis_acesso_trans.

    " Definição da estrutura para a tabela de dados
    types: begin of ty_dados,
             uname           type rslguser,
             perfil_composto type agr_name,
             perfil_simples  type agr_name,
             tcode           type char255, "agxreport,
           end of ty_dados.

    data: it_agr_users type table of ty_dados,
          tg_agr_users type table of ty_dados,
          it_ZPERE0001 type zpere0001_t.

    data: zvar_dias      type p value '90',
          zvar_date_base type sy-datum,
          zquant_acesso  type p.

    data: rg_uname    type range of xubname,
          rg_perfil_c type range of agr_name,
          rg_perfil_s type range of agr_name,
          rg_ras      type range of zpert0001-numero_ras,
          rg_ccusto   type range of kostl.

*          tg_agr_users type table of agr_users.

    if i_param is not initial.
      loop at i_param into data(wa_param).
        case wa_param-fieldnm.
          when 'UNAME'.
            append value #( sign = 'I' option = 'EQ' low = wa_param-low  ) to rg_uname.
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'PERFIL_COMPOSTO'.
            append value #( sign = 'I' option = 'EQ' low = wa_param-low  ) to rg_perfil_c.
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'PERFIL_SIMPLES'.
            append value #( sign = 'I' option = 'EQ' low = wa_param-low ) to rg_perfil_s.
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'CENTRO_CUSTO'.
            append  value #( sign = 'I' option = 'EQ' low = wa_param-low ) to rg_ccusto.
          when others.
        endcase.
      endloop.

      loop at i_param into wa_param.
        case wa_param-fieldnm.
          when 'RAS'.
            rg_ras = value #( ( sign = 'I' option = 'EQ' low = wa_param-low ) ).
          when others.
        endcase.
      endloop.
    endif.

    "Seleciona quantidade de dias / acesso transação.
    select single * from tvarvc into
    @data(ws_perf)
    where name eq 'ZPERFIL_EXPIRA_ACESSO_DIAS'.
    if sy-subrc eq 0.
      zvar_dias = ws_perf-low.
    endif.
    zvar_date_base = sy-datum - zvar_dias.

    if rg_ras is initial.
      select a~uname, b~agr_name as perfil_composto, b~child_agr as perfil_simples, c~tcode
      from agr_users as a
      left join agr_agrs as b on b~agr_name eq a~agr_name and b~child_agr like 'YS:%'
      left join agr_tcodes as c on c~agr_name eq b~child_agr
      left join zhcmt0007 as d on d~bname eq a~uname
      into table @it_agr_users
      where a~uname  in @rg_uname"@it_user-bname
      and a~agr_name in @rg_perfil_c
      and b~child_agr in @rg_perfil_s
      and d~kostl     in @rg_ccusto
      and a~agr_name like 'YC:%'.

      if sy-subrc eq 0.
        select * from zpert0001 into table @data(it_zpert0001)
       for all entries in @it_agr_users
      where perfil_simples eq @it_agr_users-perfil_simples
      and status ne 'A'.
      endif.

    else.

      select * from zpert0001 into table it_zpert0001
         where numero_ras in rg_ras.

      if sy-subrc eq 0.
        select a~uname, b~agr_name as perfil_composto, b~child_agr as perfil_simples, c~tcode
        from agr_users as a
        inner join agr_agrs as b on b~agr_name eq a~agr_name and b~child_agr like 'YS:%'
        inner join agr_tcodes as c on c~agr_name eq b~child_agr
        left join zhcmt0007 as d on d~bname eq a~uname
        into table @it_agr_users
         for all entries in @it_zpert0001
        where b~child_agr eq @it_zpert0001-perfil_simples
        and c~tcode eq  @it_zpert0001-transacao
        and a~agr_name like 'YC:%'.
      endif.

    endif.

    if it_agr_users is not initial.

      "Dados do usuario.
      select distinct a~bname, a~cname, a~bukrs, a~butxt, a~werks, a~werksn, a~funcao, a~kostl, a~ccusto, c~stext
      from zhcmt0007 as a
      left join pa0001 as b on b~pernr eq a~pernr and b~endda >= @sy-datum
      left join hrp1000 as c on c~objid eq b~orgeh
      into table @data(it_dados_user)
      for all entries in @it_agr_users
      where a~bname eq @it_agr_users-uname.


      "Dados log de acesso.
      select a~slguser, a~slgrepna, a~sal_data
**          --a~data, a~hora,
      from ztd_opns_018 as a
      into table @data(it_log_Acesso)
        for all entries in @it_agr_users
        where  a~sal_data eq @it_agr_users-tcode
           and a~data >= @zvar_date_base.


      "Seleciona transação exceção.
      select * from zpft0001 into table @data(it_tcode_exc).
      sort it_tcode_exc by tcode.
    endif.


    if it_agr_users is not initial.
      tg_agr_users = it_agr_users.

      sort: it_agr_users by uname perfil_composto perfil_simples tcode.
      sort: tg_agr_users by uname perfil_composto perfil_simples tcode.
*      sort: it_zpert0001  by usuario perfil_composto perfil_simples.
*      delete adjacent duplicates from it_agr_users comparing uname perfil_composto perfil_simples tcode.
    endif.

    sort: it_log_Acesso by slguser sal_data.
    delete adjacent duplicates from it_log_Acesso comparing slguser sal_data.



    loop at it_agr_users assigning field-symbol(<ws_agr_users>).
      clear: zquant_acesso.

      append initial line to e_result assigning field-symbol(<fs_RESULT>).
      <fs_RESULT>-usuario          = <ws_agr_users>-uname.
      <fs_RESULT>-perfil_composto  = <ws_agr_users>-perfil_composto.
      <fs_RESULT>-perfil_simples   = <ws_agr_users>-perfil_simples.
      <fs_RESULT>-transacao        = <ws_agr_users>-tcode.

      read table it_dados_user into data(wa_dados_user) with key bname = <ws_agr_users>-uname.
      if sy-subrc eq 0.
        <fs_RESULT>-cname  =  wa_dados_user-cname .
        <fs_RESULT>-werks  =  wa_dados_user-werks .
        <fs_RESULT>-werksn =  wa_dados_user-werksn.
        <fs_RESULT>-stext  =  wa_dados_user-stext.
        <fs_RESULT>-funcao =  wa_dados_user-funcao.
        <fs_RESULT>-kostl  =  wa_dados_user-kostl.
        <fs_RESULT>-ccusto =  wa_dados_user-ccusto.
      endif.

*      loop at tg_agr_users assigning field-symbol(<ws_users>) where uname            = <ws_agr_users>-uname
*                                                                and tcode            = <ws_agr_users>-tcode
*                                                                and perfil_composto  = <ws_agr_users>-perfil_composto
*                                                                and perfil_simples   = <ws_agr_users>-perfil_simples.
      read table it_tcode_exc into data(wa_tcode_exc) with key tcode = <ws_agr_users>-tcode.
      if sy-subrc eq 0.
        add 1 to zquant_acesso.
      else.
        loop at it_log_Acesso  assigning field-symbol(<wa_log>) where sal_data = <ws_agr_users>-tcode. "slguser  = <ws_users>-uname
          add 1 to zquant_acesso.
        endloop.
      endif.

      if zquant_acesso > 0.
        <fs_RESULT>-zquant_acesso = zquant_acesso.
        condense <fs_RESULT>-zquant_acesso no-gaps.
        <fs_RESULT>-status      = abap_false.
        <fs_RESULT>-desc_status = 'Acesso liberado'.
        <fs_RESULT>-icon_name   = icon_green_light.
      else.
        <fs_RESULT>-icon_name   = icon_red_light.
        <fs_RESULT>-zquant_acesso = 0.
        condense <fs_RESULT>-zquant_acesso no-gaps.
        <fs_RESULT>-status      = abap_true.
        <fs_RESULT>-desc_status = 'Bloquear acesso'.
      endif.

      read table it_zpert0001 into data(ws_zpert0001) with key  perfil_simples = <ws_agr_users>-perfil_simples
                                                                transacao      = <ws_agr_users>-tcode.

*                                                                 binary search.
      if sy-subrc eq 0 and ws_zpert0001-status eq 'P'.
        <fs_RESULT>-numero_ras = ws_zpert0001-numero_ras.
        <fs_RESULT>-icon_name   = icon_yellow_light.
        <fs_RESULT>-desc_status = 'Aguardando aprovação/remover manualmente'.
      elseif sy-subrc eq 0 and ws_zpert0001-status eq 'M'.
        <fs_RESULT>-numero_ras = ws_zpert0001-numero_ras.
        <fs_RESULT>-icon_name   = icon_release.
        <fs_RESULT>-desc_status = ws_zpert0001-desc_status.
      endif.
    endloop.
    clear: wa_dados_user.

  endmethod.


  method remover_perfil.

    data: it_zpert0001       type table of zpert0001,
          tg_zpert0001       type table of zpert0001,
          it_zpert0001_aux   type table of zpert0001,
          lt_activity_groups type table of agr_txt,
          lt_return          type          bapirettab,
          ls_activity_groups type agr_txt.


    check i_ras is not initial.
    free: it_zpert0001_aux, lt_activity_groups, tg_zpert0001.
    select * from zpert0001
    into table tg_zpert0001
      where numero_ras eq i_ras.
    if sy-subrc ne 0.
*      *  Rôle & a été supprimé
      me->gv_message = |Ras { i_ras } nao encontrada!|.
      e_retorno = 'E'.
      exit.
    endif.


    it_zpert0001_aux = tg_zpert0001.
    it_zpert0001     = tg_zpert0001.
    sort tg_zpert0001 by perfil_composto.
    sort it_zpert0001_aux by perfil_composto perfil_simples.
    delete adjacent duplicates from tg_zpert0001 comparing perfil_composto.
    delete adjacent duplicates from it_zpert0001_aux comparing perfil_composto perfil_simples.

    loop at tg_zpert0001 assigning field-symbol(<ws_para>).
      if <ws_para>-tipo_remov = 'TRANS'.
        loop at it_zpert0001 assigning field-symbol(<ws_param>) where perfil_simples eq <ws_para>-perfil_simples and transacao eq <ws_para>-transacao.
          <ws_param>-status = 'M'. "Aprovado para remover manualemnte.
          <ws_param>-tipo_proc = 'M'. "Remover manualmente.
          <ws_param>-desc_status = 'Aprovado/Aguardando remover manualmente'.
          <ws_param>-user_modif  = sy-uname.
          <ws_param>-date_modif  = sy-datum.
          <ws_param>-hr_modif    = sy-uzeit.
          e_retorno = 'S'.
          me->gv_message = |Funções foram removidas da role composta|.
        endloop.
        continue.
      endif.

      loop at it_zpert0001_aux assigning field-symbol(<wa_para>) where perfil_composto eq <ws_para>-perfil_composto.
        move <wa_para>-perfil_simples to ls_activity_groups-agr_name.
        append ls_activity_groups to lt_activity_groups.
        clear: ls_activity_groups.
      endloop.

      call function 'PRGN_RFC_DEL_AGRS_IN_COLL_AGR'
        exporting
          activity_group                = <ws_para>-perfil_composto
        tables
          activity_groups               = lt_activity_groups
          return                        = lt_return
        exceptions
          activity_group_does_not_exist = 1
          no_collective_activity_group  = 2
          activity_group_enqueued       = 3
          namespace_problem             = 4
          not_authorized                = 5
          authority_incomplete          = 6
          others                        = 7.

      if sy-subrc = 0.
*        *   Rôles ont été ajoutées au rôle &
        message s024(zmpo) with <ws_para>-perfil_composto into me->gv_message.
*        me->add_msg_log( ).

        loop at it_zpert0001 assigning <ws_param> where perfil_composto eq <ws_para>-perfil_composto.
          <ws_param>-status = 'A'.
          <ws_param>-tipo_proc = 'R'.
          <ws_param>-desc_status = 'Perfil removido'.
          <ws_param>-user_modif  = sy-uname.
          <ws_param>-date_modif  = sy-datum.
          <ws_param>-hr_modif    = sy-uzeit.
          e_retorno = 'S'.
          me->gv_message = |Funções foram removidas da role composta|.
        endloop.
      else.

        read table lt_return into data(ws_retun) with key type = 'E'.
        if sy-subrc eq 0.

        endif.

        loop at it_zpert0001 assigning <ws_param> where perfil_composto eq <ws_para>-perfil_composto.
          <ws_param>-status = 'A'.
          <ws_param>-tipo_proc = 'R'.
          <ws_param>-desc_status = ws_retun-message.
          <ws_param>-user_modif  = sy-uname.
          <ws_param>-date_modif  = sy-datum.
          <ws_param>-hr_modif    = sy-uzeit.
          e_retorno = 'E'.
          me->gv_message = ws_retun-message.
        endloop.

      endif.
      free: lt_activity_groups.
    endloop.

    if it_zpert0001 is not initial.
      modify zpert0001 from table it_zpert0001.
      commit work.
    endif.

  endmethod.


  method save_log.

    call function 'BAL_DB_SAVE'
      exporting
        i_save_all       = abap_true
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.

  endmethod.
ENDCLASS.
