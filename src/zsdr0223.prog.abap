*&---------------------------------------------------------------------*
*& Report  ZSDR0223                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
* Autor      : Sara Oikawa                                             *
* Data       : 10.07.2020                                              *
* Descrição  : Executar ZSDT0051 (Relatorio Consulta de Entregas -     *
*              INSUMOS ) através de JOB armazenando retorno em tabela. *
* Módulo     : SD                                                      *
* Request    : DEVK9A0LOE (SD - 38883_Adequação na ZSDT0051)           *
*----------------------------------------------------------------------*
* Data Modif  Autor           Descriçao    Hora          Request       *
*----------------------------------------------------------------------*
* 19.09.2024  Nilton Segantin Melhoria     09:00:00      DEVK9A26WH    *
*----------------------------------------------------------------------*
**<<<------"152703 - NMS - INI------>>>
*REPORT zsdr0223.
report zsdr0223 line-size 80.
**<<<------"152703 - NMS - FIM------>>>


*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

tables: vbak,
        vbap,
        vbfa,
        mara,
        ekko,
        ekpo,
        ekbe,
        zfit0045,
**<<<------"152703 - NMS - INI------>>>
        tvarvc,
**<<<------"152703 - NMS - FIM------>>>
        indx.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*

types: begin of ty_mara,
         matkl type mara-matkl,
         matnr type mara-matnr,
       end of ty_mara.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
data: it_zsdt0257 type table of zsdt0257,
      it_zmmt0189 type table of zmmt0189, "-US 156984-07-11-2024-#156984-RJF
      wa_zsdt0257 type zsdt0257.

data: vg_job      type i.
data: xv_jobnm type btcjob.
data: xv_stepc type btcstepcnt.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

selection-screen:begin of block b1 with frame title text-s01.

  select-options:  p_tpcont    for vbak-auart no intervals,    " Tipo de Contrato
                   p_cont      for vbak-vbeln  ,               " Contrato
                   p_orgven    for vbak-vkorg no intervals no-extension  ,  " Organização de vendas
                   p_cdist     for vbak-vtweg  , " Canal distribuição
                   p_sativ     for vbak-spart  , " Setor de atividade
                   p_escven    for vbak-vkbur  , " Escritório de vendas
                   p_clien     for vbak-kunnr  , " Cliente
                   p_datent    for vbak-erdat  , " Data de Entrada
                   p_fatuv     for vbfa-erdat no-extension , " Data de criação do registro
                   p_cent      for vbap-werks , " Centro
                   p_mater     for vbap-matnr , " Material
                   p_grupo     for mara-matkl , " Grp Material
                   p_waerks    for vbak-waerk no intervals no-extension . " Moeda

  select-options:   p_pedido   for ekko-bsart no intervals modif id mm,                  " Tipo Pedido
*                    p_numped   for ekko-ebeln no intervals modif id mm,                  " Numero do pedido
*                    p_sol      for zfit0045-nro_sol no intervals modif id mm,            " Numero da solicitação
                    p_emp      for ekko-bukrs no intervals modif id mm,                  " Nome Empresa
*                    p_forn     for ekko-lifnr modif id mm,                               " Fornecedor
                    p_centro   for ekpo-werks modif id mm,                               " Campo Centro
*                    p_mat      for ekpo-matnr modif id mm,                               " Material
                    p_matkl    for ekpo-matkl modif id mm,                               " Grupo de Mercadoria
                    p_data     for ekko-bedat modif id mm,                               " Data de Entrega
                    p_fatu     for ekbe-budat modif id mm.                               " Data de criação do registro
*                    p_waerkm   for vbak-waerk no intervals no-extension modif id mm.     " Moeda
selection-screen: end  of block  b1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*

initialization.

at selection-screen output.
**<<<------"152703 - NMS - INI------>>>
*  DATA: lv_ano_ant TYPE gjahr.
*
*  " Tratar datas
*
*  lv_ano_ant = sy-datum(4) - 1.
*
*  IF p_datent[] IS INITIAL.
*
*    " Preenche Data Entrada
*    p_datent-sign    = 'I'.
*    p_datent-option  = 'BT'.
*    CONCATENATE lv_ano_ant  '01' '01' INTO p_datent-low.
*    CONCATENATE sy-datum(4) '12' '31' INTO p_datent-high.
*    APPEND p_datent.
*
*  ENDIF.
*
*  IF p_fatuv[] IS INITIAL.
*
*    " Preenche Data Faturamento
*    p_fatuv-sign    = 'I'.
*    p_fatuv-option  = 'BT'.
*    CONCATENATE sy-datum(4) '01' '01' INTO p_fatuv-low.
*    CONCATENATE sy-datum(4) '12' '31' INTO p_fatuv-high.
*    APPEND p_fatuv.
*
*  ENDIF.
  clear: p_datent, p_datent[].
  select single * from tvarvc where name eq 'ZSD_ZSDR0223_DT_ENTR'.
  if sy-subrc is initial.
    move-corresponding tvarvc to p_datent.
    move tvarvc-opti to p_datent-option.
    append p_datent.

  endif.

  clear: p_fatuv, p_fatuv[], tvarvc.
  select single * from tvarvc where name eq 'ZSD_ZSDR0223_DT_FATUR'.
  if sy-subrc is initial.
    move-corresponding tvarvc to p_fatuv.
    move tvarvc-opti to p_fatuv-option.
    append p_fatuv.

  endif.

  clear tvarvc.
**<<<------"152703 - NMS - FIM------>>>
*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
start-of-selection.

  call function 'GET_JOB_RUNTIME_INFO'
    importing
*     EVENTID         =
*     EVENTPARM       =
*     EXTERNAL_PROGRAM_ACTIVE       =
*     JOBCOUNT        = XV_JOBCN
      jobname         = xv_jobnm
      stepcount       = xv_stepc
    exceptions
      no_runtime_info = 1
      others          = 2.

  if xv_jobnm = 'ZSDR0223'. "SD

    " Submit ZSDT0051
    perform f_busca_dados_zsdt0051_sd.

    " Grava dados retornados em tabela
    perform f_grava_zsdt0257.
  else.
*-US 156984-07-11-2024-#156984-RJF-Inicio
    perform f_busca_dados_zsdt0051_mm.
*-US 156984-07-11-2024-#156984-RJF-Fim
  endif.

end-of-selection.

*----------------------------------------------------------------------*
* FORMS                                                                *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS_ZSDT0051_SD
*&---------------------------------------------------------------------*

form f_busca_dados_zsdt0051_sd.


  data: vl_dt_ini(8)   type c,
        vl_dt_fim(8)   type c,
        vl_dt_high_in  type sy-datum,
        vl_dt_ini_fat  type sy-datum,
        vl_dt_high_out type sy-datum.

  data: it_rsparams  type table of rsparams,
        wa_rsparams  type rsparams,

        lv_call_prog type sy-repid.


  clear: it_rsparams[].

  " Preenche parâmetros de seleção para ZSDT0051

  wa_rsparams-selname = 'SD'.
  wa_rsparams-kind    = 'P'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
  wa_rsparams-low     = 'X'.
  append wa_rsparams to it_rsparams.

  wa_rsparams-selname = 'MM'.
  wa_rsparams-kind    = 'P'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
  wa_rsparams-low     = ''.
  append wa_rsparams to it_rsparams.

  "TIPO DE CONTRATO
  if p_tpcont[] is not initial.
    loop at p_tpcont.
      wa_rsparams-selname = 'P_TPCONT'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = p_tpcont-low.
      wa_rsparams-high    = p_tpcont-high.
      append wa_rsparams to it_rsparams.
    endloop.
  endif.

  "Organização de Vendas
  if p_orgven[] is not initial.
    loop at p_orgven.
      wa_rsparams-selname = 'P_ORGVEN'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = p_orgven-low.
      wa_rsparams-high    = p_orgven-high.
      append wa_rsparams to it_rsparams.
    endloop.
  endif.

  "Canal Distribuição
  wa_rsparams-selname = 'P_CDIST'.
  wa_rsparams-kind    = 'S'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
*  WA_RSPARAMS-LOW     = '10'. "Mercado Interno
  wa_rsparams-low     = p_cdist-low.
  append wa_rsparams to it_rsparams.

  "Setor Atividade
  if p_sativ[] is not initial.
    loop at p_sativ.
      wa_rsparams-selname = 'P_SATIV'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = p_sativ-low.
      wa_rsparams-high    = p_sativ-high.
      append wa_rsparams to it_rsparams.
    endloop.
  endif.

  "Data de Entrada
                                                            "US156984
  vl_dt_ini_fat = sy-datum - 1825. "5 anos
  concatenate vl_dt_ini_fat+0(4) '0101' into vl_dt_ini_fat.
  wa_rsparams-selname = 'P_DATENT'.
  wa_rsparams-kind    = 'S'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'BT'.            "p_datent-option.
  wa_rsparams-low     = vl_dt_ini_fat.   "p_datent-low.
  wa_rsparams-high    = sy-datum.        "p_datent-high.
  append wa_rsparams to it_rsparams.

  "Data de Faturamento
  wa_rsparams-selname = 'P_FATUV'.
  wa_rsparams-kind    = 'S'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'BT'.             "p_fatuv-option.
  wa_rsparams-low     = vl_dt_ini_fat.    "p_fatuv-low.
  wa_rsparams-high    = sy-datum.         "p_fatuv-high.
  append wa_rsparams to it_rsparams.
                                                            "US156984

  "Centro
  "FILIAL
  loop at p_cent.
    wa_rsparams-selname = 'P_CENT'.
    wa_rsparams-kind    = 'S'.
    wa_rsparams-sign    = 'I'.
    wa_rsparams-option  = p_cent-option.
    wa_rsparams-low     = p_cent-low.
    wa_rsparams-high    = p_cent-high.
    append wa_rsparams to it_rsparams.
  endloop.

  "Material
  if p_mater[] is not initial.
    loop at p_mater.
      wa_rsparams-selname = 'P_MATER'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = p_mater-option.
      wa_rsparams-low     = p_mater-low.
      wa_rsparams-high    = p_mater-high.
      append wa_rsparams to it_rsparams.
    endloop.

  elseif ( p_mater[] is not initial ) and ( p_mater-low ne '*' ).
    select *
      from mara into table @data(it_mara_aux)
     where matkl in @p_grupo.

    loop at it_mara_aux into data(_wl_mara).
      wa_rsparams-selname = 'P_MATER'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = _wl_mara-matnr.
      append wa_rsparams to it_rsparams.
    endloop.
  endif.

  append
  value #(
           selname = 'P_T257'
           kind    = 'P'
           sign    = 'I'
           option  = 'EQ'
           low     = abap_true
         ) to it_rsparams.

*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      PERCENTAGE = SY-TABIX
*      TEXT       = 'Extraindo dados em ZSDT0051(SD)...'.

  " Chama ZSDT0051
  lv_call_prog = sy-repid.
  export lv_call_prog to database indx(zp) id 'ZSDT0257'.
  submit zsdr0018  with selection-table it_rsparams
                   and return.

  " Obtém dados retornados
  import it_zsdt0257 from database indx(zk) id 'ZSDT0257'.
  delete from database indx(zk) id 'ZSDT0257'.


endform.

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_ZSDT0257
*&---------------------------------------------------------------------*
form f_grava_zsdt0257 .


  data: lv_lines type sy-tabix,
        lv_qtd   type sy-tabix.


  data: lv_cont         type sy-tfill,
**<<<------"152703 - NMS - INI------>>>
        lv_cont_ex      type sy-tfill,
**<<<------"152703 - NMS - FIM------>>>
        lt_zsdt0257_api type zsdt_zsdt0257,
        lt_zsdt0257_aux type zsdt_zsdt0257.

  describe table it_zsdt0257 lines lv_cont.

  data(lit_zsdt0257_novo) = it_zsdt0257[].

  if lv_cont is initial.

    "Não foram encontrados registros para a seleção
    write: / text-m01.

  else.

*** Inicio - Rubenilson Pereira - 20.02.2023 - PROJHANA #104481
    select *
      from zsdt0257
      into table @data(lit_zsdt0257_current).
    if sy-subrc is initial.
      sort lit_zsdt0257_novo by doc_simulacao
                                vbeln
                                posnr.

      sort lit_zsdt0257_current by doc_simulacao
                                   vbeln
                                   posnr.


      "Verificar Registros Novos/Atualizados
      loop at lit_zsdt0257_novo assigning field-symbol(<fs_zsdt0257_novo>).

        read table lit_zsdt0257_current assigning field-symbol(<fs_zsdt0257_current>)
        with key doc_simulacao = <fs_zsdt0257_novo>-doc_simulacao
                 vbeln         = <fs_zsdt0257_novo>-vbeln
                 posnr         = <fs_zsdt0257_novo>-posnr
        binary search.
        if sy-subrc is initial.

          <fs_zsdt0257_novo>-mandt               = <fs_zsdt0257_current>-mandt.
**<<<------"152703 - NMS - INI------>>>
*          <fs_zsdt0257_novo>-usnam               = <fs_zsdt0257_current>-usnam.
*          <fs_zsdt0257_novo>-data_atual          = <fs_zsdt0257_current>-data_atual.
*          <fs_zsdt0257_novo>-hora_atual          = <fs_zsdt0257_current>-hora_atual.
          <fs_zsdt0257_current>-usnam            = <fs_zsdt0257_novo>-usnam.
          <fs_zsdt0257_current>-data_atual       = <fs_zsdt0257_novo>-data_atual.
          <fs_zsdt0257_current>-hora_atual       = <fs_zsdt0257_novo>-hora_atual.
**<<<------"152703 - NMS - FIM------>>>
          <fs_zsdt0257_novo>-int_sistemas_legado = <fs_zsdt0257_current>-int_sistemas_legado.
          <fs_zsdt0257_novo>-dt_int_legado       = <fs_zsdt0257_current>-dt_int_legado.
          <fs_zsdt0257_novo>-hr_int_legado       = <fs_zsdt0257_current>-hr_int_legado.

          if ( <fs_zsdt0257_novo> ne <fs_zsdt0257_current> ) or <fs_zsdt0257_current>-int_sistemas_legado eq abap_false.

            append initial line to lt_zsdt0257_api assigning field-symbol(<fs_zsdt0257_api>).

            <fs_zsdt0257_novo>-int_sistemas_legado = abap_true.
            <fs_zsdt0257_novo>-dt_int_legado       = sy-datum.
            <fs_zsdt0257_novo>-hr_int_legado       = sy-uzeit.

            move-corresponding <fs_zsdt0257_novo> to <fs_zsdt0257_api>.

          endif.

        else.

          append initial line to lt_zsdt0257_api assigning <fs_zsdt0257_api>.

          <fs_zsdt0257_novo>-int_sistemas_legado = abap_true.
          <fs_zsdt0257_novo>-dt_int_legado       = sy-datum.
          <fs_zsdt0257_novo>-hr_int_legado       = sy-uzeit.

          move-corresponding <fs_zsdt0257_novo> to <fs_zsdt0257_api>.

        endif.


      endloop.

      "Verificar Registros Deletados
      loop at lit_zsdt0257_current assigning <fs_zsdt0257_current>.

        read table lit_zsdt0257_novo assigning <fs_zsdt0257_novo>
        with key doc_simulacao = <fs_zsdt0257_current>-doc_simulacao
                 vbeln         = <fs_zsdt0257_current>-vbeln
                 posnr         = <fs_zsdt0257_current>-posnr
        binary search.
        if sy-subrc is not initial.
          append initial line to lt_zsdt0257_api assigning <fs_zsdt0257_api>.
          move-corresponding <fs_zsdt0257_current> to <fs_zsdt0257_api>.
          <fs_zsdt0257_api>-excluido = 'X'.
        endif.


      endloop.


    endif.

*** Fim - Rubenilson Pereira - 20.02.2023 - PROJHANA #104481
**<<<------"152703 - NMS - INI------>>>
*    DELETE FROM zsdt0257.
*
*    IF lit_zsdt0257_novo[] IS NOT INITIAL.
*      MODIFY zsdt0257 FROM TABLE lit_zsdt0257_novo.
*    ENDIF.
*
*    "Processamento OK. Registros gravados na tabela ZSDT0257 ='
*    WRITE: / TEXT-m02 , lv_cont.
    lt_zsdt0257_aux = lt_zsdt0257_api.

    delete lt_zsdt0257_aux where excluido is initial.

    if not lt_zsdt0257_aux[] is initial.
      move-corresponding lt_zsdt0257_aux[] to lit_zsdt0257_novo[].
*** Excluí dados dados que não existem mais.
      delete zsdt0257 from table lit_zsdt0257_novo.

      if sy-subrc is initial.
        lv_cont_ex      = lines( lit_zsdt0257_novo ).
        lt_zsdt0257_aux = lt_zsdt0257_api.

        delete lt_zsdt0257_aux where excluido is not initial.

        if not lt_zsdt0257_aux[] is initial.
* Grava dados novos e que sofreram modificações.
          perform zf_modify_new_update_registers tables lt_zsdt0257_aux
                                                  using lv_cont_ex.

        else.
          commit work.
* Dados gravados com sucesso.
          message s001(zhcm_ehs).
* Processamento OK. Registros excluídos da tabela ZSDT0257 =
          data(lv_mesg) = text-m04 && '!' && lv_cont_ex.
          translate lv_mesg using '! '.
          write: / lv_mesg.

        endif.

      else.
        rollback work.
* Ocorreu erro ao gravar dados
        message i073(gd) display like 'E'.
        message i073(gd) into lv_mesg.
        write: / lv_mesg.

      endif.

    else.
      lt_zsdt0257_aux = lt_zsdt0257_api.

      delete lt_zsdt0257_aux where excluido is not initial.

      if not lt_zsdt0257_aux[] is initial.
* Grava dados novos e que sofreram modificações.
        perform zf_modify_new_update_registers tables lt_zsdt0257_aux
                                                using 0.

      else.
        rollback work.
* Dados não foram gravados& &
        message i499(5a) display like 'E'.
        message i499(5a) into lv_mesg.
        write: / lv_mesg.

      endif.

    endif.
**<<<------"152703 - NMS - FIM------>>>

* "// INICIO - 23/04/25 WBARBOSA - Mudança de posição da Chamada do API BUG-156984
        clear: lv_qtd.

    lv_lines = lines( lt_zsdt0257_api[] ).

    loop at lt_zsdt0257_api assigning <fs_zsdt0257_api>.

      add 1 to lv_qtd.

      append <fs_zsdt0257_api> to lt_zsdt0257_aux.

      if lv_qtd = 400 or lv_qtd = lv_lines.

        try.
            zcl_int_ob_zsdt0051_legados=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = lt_zsdt0257_aux ).

          catch zcx_integracao into data(ex_integra).    "

            write: / text-m03.
            return.

          catch zcx_error into data(ex_error).    "  "

            write: / text-m03.
            return.

        endtry.


        lv_lines = lv_lines - lv_qtd.
        clear lv_qtd.
        refresh: lt_zsdt0257_aux.

      endif.


    endloop.
* "// FIM - 23/04/25 WBARBOSA - Mudança de posição da Chamada do API BUG-156984

  endif.

endform.
**<<<------"152703 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_modify_new_update_registers
*&---------------------------------------------------------------------*
*& Grava dados novos e que sofreram modificações
*&---------------------------------------------------------------------*
*&      --> PT_ZSDT0257_AUX TI de apoio de dados
*&      --> UV_CONT_EX      Quantidade de registros excluídos
*&---------------------------------------------------------------------*
form zf_modify_new_update_registers tables pt_zsdt0257_aux structure zsde_zsdt0257
                                     using uv_cont_ex      type      syst_tfill.

  data: lt_zsdt0257 type table of zsdt0257.

  data: lv_cont    type sy-tfill.

  move-corresponding pt_zsdt0257_aux[] to lt_zsdt0257[].
*** Gravar os dados Novos e só atualizar os dados que sofreram modificações.
  modify zsdt0257 from table lt_zsdt0257.

  if sy-subrc is initial.
    lv_cont = lines( lt_zsdt0257 ).
    commit work.
* Dados gravados com sucesso.
    message s001(zhcm_ehs).
    data(lv_mesg) = text-m02 && '!' && lv_cont.
    translate lv_mesg using '! '.
* Processamento OK. Registros gravados na tabela ZSDT0257 ='
    write: / lv_mesg.

    lv_mesg = text-m04 && '!' && uv_cont_ex.
    translate lv_mesg using '! '.
* Processamento OK. Registros excluídos da tabela ZSDT0257 =
    write: / lv_mesg.

  else.
    rollback work.
* Ocorreu erro ao gravar dados
    message i073(gd) display like 'E'.
    message i073(gd) into lv_mesg.
* Processamento OK. Registros excluídos da tabela ZSDT0257 =
    write: / lv_mesg.

  endif.

endform.
**<<<------"152703 - NMS - FIM------>>>
*-US 156984-07-11-2024-#156984-RJF-Inicio
*&---------------------------------------------------------------------*
*& Form f_busca_dados_zsdt0051_mm
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_busca_dados_zsdt0051_mm .

  data: vl_dt_ini(8)  type c,
        vl_dt_fim(8)  type c,
        vl_dt_high_in type sy-datum,
        vl_dt_ini_mm  type sy-datum,
        vl_seq        type zmmt0189-sequencia.

  data: it_rsparams  type table of rsparams,
        wa_rsparams  type rsparams,

        lv_call_prog type sy-repid.

  clear: it_rsparams[].

  " Preenche parâmetros de seleção para ZSDT0051
  wa_rsparams-selname = 'SD'.
  wa_rsparams-kind    = 'P'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
  wa_rsparams-low     = ''.
  append wa_rsparams to it_rsparams.

  wa_rsparams-selname = 'MM'.
  wa_rsparams-kind    = 'P'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
  wa_rsparams-low     = 'X'.
  append wa_rsparams to it_rsparams.

*p_pedido
  loop at p_pedido.
    wa_rsparams-selname = 'P_PEDIDO'.
    wa_rsparams-kind    = 'S'.
    wa_rsparams-sign    = 'I'.
    wa_rsparams-option  = p_pedido-option.
    wa_rsparams-low     = p_pedido-low.
    wa_rsparams-high    = p_pedido-high.
    append wa_rsparams to it_rsparams.
  endloop.


*p_emp
  loop at p_emp.
    wa_rsparams-selname = 'P_EMP'.
    wa_rsparams-kind    = 'S'.
    wa_rsparams-sign    = 'I'.
    wa_rsparams-option  = p_emp-option.
    wa_rsparams-low     = p_emp-low.
    wa_rsparams-high    = p_emp-high.
    append wa_rsparams to it_rsparams.
  endloop.

*p_matkl
  loop at p_matkl.
    wa_rsparams-selname = 'P_MATKL'.
    wa_rsparams-kind    = 'S'.
    wa_rsparams-sign    = 'I'.
    wa_rsparams-option  = p_matkl-option.
    wa_rsparams-low     = p_matkl-low.
    wa_rsparams-high    = p_matkl-high.
    append wa_rsparams to it_rsparams.
  endloop.

  "Data de Entrada
                                                            "US156984
  vl_dt_ini_mm = sy-datum - 1825. "5 anos
  concatenate vl_dt_ini_mm+0(4) '0101' into vl_dt_ini_mm.
*  loop at p_data.
  wa_rsparams-selname = 'P_DATA'.
  wa_rsparams-kind    = 'S'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'BT'.          "p_data-option.
  wa_rsparams-low     = vl_dt_ini_mm.  "p_data-low.
  wa_rsparams-high    = sy-datum.      "p_data-high.
  append wa_rsparams to it_rsparams.
*  endloop.

  "Data de Faturamento
  "loop at p_fatu.
  wa_rsparams-selname = 'P_FATU'.
  wa_rsparams-kind    = 'S'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'BT'.            "p_fatu-option.
  wa_rsparams-low     = vl_dt_ini_mm.    "p_fatu-low.
  wa_rsparams-high    = sy-datum.        "p_fatu-high.
  append wa_rsparams to it_rsparams.
  " endloop.
                                                            "US156984

  "Centro
  "FILIAL
  loop at p_centro.
    wa_rsparams-selname = 'P_CENTRO'.
    wa_rsparams-kind    = 'S'.
    wa_rsparams-sign    = 'I'.
    wa_rsparams-option  = p_centro-option.
    wa_rsparams-low     = p_centro-low.
    wa_rsparams-high    = p_centro-high.
    append wa_rsparams to it_rsparams.
  endloop.

  "Material
  if p_mater[] is not initial.
    loop at p_mater.
      wa_rsparams-selname = 'P_MAT'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = p_mater-option.
      wa_rsparams-low     = p_mater-low.
      wa_rsparams-high    = p_mater-high.
      append wa_rsparams to it_rsparams.
    endloop.

  elseif ( p_mater[] is not initial ) and ( p_mater-low ne '*' ).
    select *
      from mara into table @data(it_mara_aux)
     where matkl in @p_grupo.

    loop at it_mara_aux into data(_wl_mara).
      wa_rsparams-selname = 'P_MAT'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = _wl_mara-matnr.
      append wa_rsparams to it_rsparams.
    endloop.
  endif.

  append
  value #(
           selname = 'P_T189'
           kind    = 'P'
           sign    = 'I'
           option  = 'EQ'
           low     = abap_true
         ) to it_rsparams.

  " Chama ZSDT0051
*  lv_call_prog = sy-repid.
*  EXPORT lv_call_prog TO DATABASE indx(zp) ID 'ZMMT0189'.
  submit zsdr0018  with selection-table it_rsparams
                   and return.

  " Obtém dados retornados
  import it_zmmt0189 from database indx(zk) id 'ZMMT0189'.
  delete from database indx(zk) id 'ZMMT0189'.
  delete from zmmt0189
   where job = xv_jobnm.
  commit work.

  clear vl_seq.
  loop at it_zmmt0189 assigning field-symbol(<wa_zmmt0189>).
    add 1 to vl_seq.
    <wa_zmmt0189>-job = xv_jobnm.
    <wa_zmmt0189>-sequencia = vl_seq.
  endloop.
  modify zmmt0189 from table it_zmmt0189.
  if sy-subrc is initial.
    commit work and wait.
* Processamento OK. Registros gravados na tabela ZMMT0189.
    data: lv_mesg type char60,
          lv_lin  type i.
    lv_lin = lines( it_zmmt0189 ).
    lv_mesg = text-m05 && ' COMPRAS = ' && lv_lin.
    write: / lv_mesg.
  else.
    rollback work.
    message i073(gd) into lv_mesg.
* Ocorreu erro ao gravar dados
    write: / lv_mesg.
  endif.

endform.
*-US 156984-07-11-2024-#156984-RJF-Fim
