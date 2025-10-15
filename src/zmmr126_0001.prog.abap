*&---------------------------------------------------------------------*
*& Report  ZMMR126
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zmmr126_0001 message-id zcarga.

tables: zde_zsdt0001cg_alv,
        zde_zsdt0001od_alv,
        zde_zsdt0001nt_alv,
        zde_zsdt0001ov_alv,
        zde_zsdt0001acb_alv.

class lcl_event_receiver definition deferred.
class lcl_event_receiver_est definition deferred.

*****************************************************************************
*****************************************************************************

data: it_retorno              type zde_zsdt0001cg_alv_t,
      ck_selecionou           type char01,
      ck_confer_carga         type char01,
      ck_dado_frete_9011      type char01,
      ck_erro_frete_9011      type char01,
      ck_erro_pedagio_9011    type char01,
      gb_id_carga             type zde_id_carga,
      gb_st_carga             type char01,
      gb_id_carga_est         type zde_id_carga,
      gb_st_carga_est         type char01,
      it_mensagens            type zde_ob_mensagem_t,
      it_notas                type table of zde_zsdt0001nt_alv with header line,
      wa_nota_selecionada     type zde_zsdt0001nt_alv,
      it_takes_saldo          type table of zde_zsdt0001tk_alv_vinc with header line,
      it_takes_vincu          type table of zde_zsdt0001tk_alv with header line,
      it_ordens_venda         type table of zde_zsdt0001ov_alv with header line,
      it_pedido_compra        type table of zde_zsdt0001ek_alv with header line,
      ex_parceiros            type ref to zcx_parceiros,
      ex_carga                type ref to zcx_carga,
      ex_cadastro             type ref to zcx_cadastro,
      ex_romaneio             type ref to zcx_romaneio,
      ex_ordem                type ref to zcx_ordem_carregamento,
      ex_ordem_venda          type ref to zcx_ordem_venda,
      ex_soft_expert_workflow type ref to zcx_soft_expert_workflow,
      ex_job                  type ref to zcx_job,
      ex_pedido               type ref to zcx_pedido_compra_exception,
      ex_miro                 type ref to zcx_miro_exception,
      go_alarm                type ref to lcl_event_receiver,
      go_clock                type ref to cl_gui_timer,
      go_alarm_est            type ref to lcl_event_receiver_est,
      go_clock_est            type ref to cl_gui_timer,
      lc_zcl_job              type ref to zcl_job,
      lc_zcl_job_est          type ref to zcl_job,
      vg_text(16)             type c,
      vg_ord_ext,
      screen_tela             type sy-dynnr,
      "
      "US143677
      zde_zsdt0001cg_aux      type zde_zsdt0001cg_alv.



types: begin of ty_itens_alv.
         include structure zde_zsdt0001cg_alv.
types:   line_color(4) type c, "Used to store row color attributes
         color_cell    type lvc_t_scol,  " Cell color
         style         type lvc_t_styl,
         ico_carga     type char04,
       end of ty_itens_alv.


*****************************************************************************
*****************************************************************************

*---------------------------------------------------------------------*
*       CLASS c_service DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
class c_service definition.
  public section.
    class-methods get_pic_tab             importing mime_url type csequence exporting completo type xstring pic_tab type standard table .
    class-methods verifica_filtro_initial returning value(r_vazio) type char01.
    class-methods verifica_filtro_initial_e returning value(r_vazio) type char01.
endclass.                    "c_service DEFINITION

class lcl_event_receiver definition.
  public section.
    methods:
      on_finished for event finished of cl_gui_timer.
  private section.
endclass.

class lcl_event_receiver_est definition.
  public section.
    methods:
      on_finished for event finished of cl_gui_timer.
  private section.
endclass.

class cl_myevent_handler definition.

  public section.
    constants st_action_grao type c length 4 value 'GRAO'.
    constants st_action_algo type c length 4 value 'ALGO'.
    methods: on_sapevent for event sapevent of cl_gui_html_viewer importing action frame getdata postdata query_table.

endclass.


constants: cs_line_color_finalizado  type c length 4 value 'C500',
           cs_line_color_alterado    type c length 4 value 'C300',
           cs_line_color_selecionada type c length 4 value 'C601'.

data: lc_filtro type zde_filtro_zsdt0001cg.

data: ctl_cccontainer_html1 type ref to cl_gui_container,
      ctl_cccontainer_html2 type ref to cl_gui_container,
      splitter              type ref to cl_gui_splitter_container,
      picture               type ref to cl_gui_picture,
      objeto                type ref to zif_carga,
      html_control_produto  type ref to cl_gui_html_viewer,
      html_control_grafico  type ref to cl_gui_html_viewer,
      evt_receiver          type ref to cl_myevent_handler,
      myevent               type cntl_simple_event,
      myevent_tab           type cntl_simple_events.

types: begin of ty_add_nfe.
types: n55_chave_acesso	type zde_chave_doc_e,
       docnum_nfe       type j_1bdocnum,
       n55_stat_sefaz	  type j_1bstatuscode,
       dt_emissao       type zde_zsdt0001nt_alv-dt_emissao,
       numero           type zde_zsdt0001nt_alv-nr_nota,
       serie            type zde_zsdt0001nt_alv-nm_serie,
       bukrs            type bukrs,
       branch	          type j_1bbranc_,
       parid            type j_1bparid, "Ajuda de Pesquisa DEBI_KRED
       parid_ie         type zde_ie,
       butxt            type butxt,
       name	            type name1,
       name1            type name1_gp,
       nftot            type j_1bnftot,
       ntgew            type ntgew_15,
       ck_incluir       type char01,
       cfop             type zde_zsdt0001nt_alv-cfop.
types: end of ty_add_nfe.

data: wa_add_nfe_9002 type ty_add_nfe.

data: ok_code   type sy-ucomm,
      nm_report type zbit0003-ds_nome_tecnico,
      nm_atual  type char01.

selection-screen begin of screen 0101 as subscreen.
  selection-screen begin of block cte with frame title text-108.
    select-options: eidcarga for zde_zsdt0001cg_alv-id_carga,
                    erroment for zde_zsdt0001nt_alv-nr_romaneio_ent,
                    erromsai for zde_zsdt0001ov_alv-nr_romaneio_sai,
                    eidordem for zde_zsdt0001cg_alv-id_ordem,
                    eidentra for zde_zsdt0001nt_alv-id_entrada,
                    eidlocal for zde_zsdt0001cg_alv-id_local_entrega,
                    edtmovim for zde_zsdt0001cg_alv-dt_movimento,
                    enrsafra for zde_zsdt0001cg_alv-nr_safra  no-display,
                    eidbukrs for zde_zsdt0001cg_alv-id_bukrs  no-display,
                    eidbranc for zde_zsdt0001cg_alv-id_branch no-display,
                    eidagent for zde_zsdt0001cg_alv-id_agent_frete,
                    eidcolet for zde_zsdt0001cg_alv-id_local_coleta,
                    eiddesti for zde_zsdt0001cg_alv-id_local_destino,
                    eiddesca for zde_zsdt0001cg_alv-id_local_descarga,
                    eidprodu for zde_zsdt0001cg_alv-id_produto,
                    edstrato for zde_zsdt0001cg_alv-ds_placa_trator,
                    edsrebo1 for zde_zsdt0001cg_alv-ds_placa_reboq_1,
                    edsrebo2 for zde_zsdt0001cg_alv-ds_placa_reboq_2,
                    edsrebo3 for zde_zsdt0001cg_alv-ds_placa_reboq_3,
                    eidmotor for zde_zsdt0001cg_alv-id_motorista,
                    enrticke for zde_zsdt0001cg_alv-nr_ticket,
                    etpstatu for zde_zsdt0001cg_alv-tp_status,
                    edtabert for zde_zsdt0001cg_alv-dt_abertura,
                    ehrabert for zde_zsdt0001cg_alv-hr_abertura,
                    edtfecha for zde_zsdt0001cg_alv-dt_fechamento,
                    ehrfecha for zde_zsdt0001cg_alv-hr_fechamento.
  selection-screen end of block cte.
selection-screen end of screen 0101.

select-options: iidcarga for zde_zsdt0001cg_alv-id_carga no-display,
                iidordem for zde_zsdt0001cg_alv-id_ordem no-display,
                iidentra for zde_zsdt0001nt_alv-id_entrada no-display,
                iidlocal for zde_zsdt0001cg_alv-id_local_entrega no-display,
                idtmovim for zde_zsdt0001cg_alv-dt_movimento no-display,
                inrsafra for zde_zsdt0001cg_alv-nr_safra no-display,
                iidbukrs for zde_zsdt0001cg_alv-id_bukrs no-display,
                iidbranc for zde_zsdt0001cg_alv-id_branch no-display,
                iidagent for zde_zsdt0001cg_alv-id_agent_frete no-display,
                iidcolet for zde_zsdt0001cg_alv-id_local_coleta no-display,
                iiddesti for zde_zsdt0001cg_alv-id_local_destino no-display,
                iiddesca for zde_zsdt0001cg_alv-id_local_descarga no-display,
                iidprodu for zde_zsdt0001cg_alv-id_produto no-display,
                idstrato for zde_zsdt0001cg_alv-ds_placa_trator no-display,
                idsrebo1 for zde_zsdt0001cg_alv-ds_placa_reboq_1 no-display,
                idsrebo2 for zde_zsdt0001cg_alv-ds_placa_reboq_2 no-display,
                idsrebo3 for zde_zsdt0001cg_alv-ds_placa_reboq_3 no-display,
                iidmotor for zde_zsdt0001cg_alv-id_motorista no-display,
                itpstatu for zde_zsdt0001cg_alv-tp_status no-display,
                inrticke for zde_zsdt0001cg_alv-nr_ticket no-display,
                idtabert for zde_zsdt0001cg_alv-dt_abertura no-display,
                ihrabert for zde_zsdt0001cg_alv-hr_abertura no-display,
                idtfecha for zde_zsdt0001cg_alv-dt_fechamento no-display,
                ihrfecha for zde_zsdt0001cg_alv-hr_fechamento no-display,
                irroment for zde_zsdt0001nt_alv-nr_romaneio_ent no-display,
                irromsai for zde_zsdt0001ov_alv-nr_romaneio_sai no-display.

"Seleção Padrão
parameters: ptipca   type zde_tp_carga      no-display,
            psafra   type zde_nr_safra      no-display,
            pempre   type zde_bukrs_receb   no-display,
            pfilia   type zde_branch_receb  no-display,
            pmanut   type char01            no-display,
            pck_cad  type char01            no-display,
            pidcarga type zde_id_carga      no-display,
            pidsolic type zde_id_sol_ajuste no-display.

include zmmr126_tree_view if found.

include zmmr126_status_0200_0001.
*INCLUDE ZMMR126_STATUS_0200.
include zmmr126_user_command_0001_0001.
*INCLUDE ZMMR126_USER_COMMAND_0001.
include zmmr126_status_9007_0001.
*INCLUDE ZMMR126_STATUS_9007.
include zmmr126_status_9004_0001.
*INCLUDE ZMMR126_STATUS_9004.
include zmmr126_status_0312_0001.
*INCLUDE ZMMR126_STATUS_0312.
include zmmr126_status_0313_0001.
*INCLUDE ZMMR126_STATUS_0313.
include zmmr126_status_0300_0001.
*INCLUDE ZMMR126_STATUS_0300.
include zmmr126_status_9001_0001.
*INCLUDE ZMMR126_STATUS_9001.
include zmmr126_status_9002_0001.
*INCLUDE ZMMR126_STATUS_9002.
include zmmr126_status_9003_0001.
*INCLUDE ZMMR126_STATUS_9003.
include zmmr126_atribui_0308_0001.
*INCLUDE ZMMR126_ATRIBUI_0308.

initialization.

  "EXPORT '' TO MEMORY ID 'IDCARGA'.
  set parameter id 'ZIDCARGA' field ''.
*  SELECT SINGLE * INTO @DATA(WA_CENTRO_A_FIXAR)
*    FROM ZSDT_DEPARA_CEN
*   WHERE TP_CENTRO_VIRTUAL EQ @ZCL_PEDIDO_COMPRA=>ST_TP_CENTRO_A_FIXAR.
*
*  IF SY-SUBRC IS NOT INITIAL.
*    SELECT * INTO TABLE @DATA(IT_ZMMT0017)
*      FROM ZMMT0017.
*
*    SORT IT_ZMMT0017 BY CENTRO_FIXO CENTRO_A_FIXAR.
*    DELETE ADJACENT DUPLICATES FROM IT_ZMMT0017 COMPARING CENTRO_FIXO CENTRO_A_FIXAR.
*
*    LOOP AT IT_ZMMT0017 INTO DATA(WA_ZMMT0017).
*      UPDATE ZSDT_DEPARA_CEN
*         SET TP_CENTRO_VIRTUAL = ZCL_PEDIDO_COMPRA=>ST_TP_CENTRO_A_FIXAR
*       WHERE CENTRO_REAL EQ WA_ZMMT0017-CENTRO_FIXO
*         AND CENTROV_1   EQ WA_ZMMT0017-CENTRO_A_FIXAR.
*    ENDLOOP.
*    COMMIT WORK.
*  ENDIF.

start-of-selection.

  data: wa_carga type zsdt0001cg,
        lc_ok    type char01.

  if ptipca  is initial or
     psafra  is initial or
     pempre  is initial or
     pfilia  is initial.
    perform busca_ultimo_acesso using lc_ok.
    if lc_ok eq abap_false.
      perform selecao_padrao.
    endif.
  endif.

  if pck_cad eq abap_true.
    clear: zde_zsdt0001cg_alv,
           zde_zsdt0001od_alv,
           zde_zsdt0001nt_alv.


    if pidcarga is not initial.

      data: e_name_class type string.

      objeto = zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto_id( i_id_carga = pidcarga
        )->get_factory_objeto_name( importing e_name = e_name_class
        )->get_factory_objeto(
        ).

      case e_name_class.
        when zif_factory_carga=>at_recebe_algodao.

          submit zmmr153 with ptipca   eq ptipca
                         with pck_cad  eq abap_true
                         with pmanut   eq pmanut
                         with psafra   eq psafra
                         with pempre   eq pempre
                         with pfilia   eq pfilia
                         with pidcarga eq pidcarga
                         with pidsolic eq pidsolic and return.

          leave program.

        when others.

          try .
              if pmanut eq abap_false.
                objeto->set_registro( i_id_carga = pidcarga ).
              else.
                if pidsolic is initial.
                  objeto->set_cria_manutencao( i_id_carga = pidcarga ).
                else.
                  objeto->set_registro_manutencao( i_id_solicitacao = pidsolic ).
                endif.
              endif.
            catch zcx_carga into ex_carga.
              ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
              perform limpar_tela.
              leave program.
            catch zcx_ordem_carregamento into ex_ordem_carregamento.
              ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
              perform limpar_tela.
              leave program.
            catch zcx_parceiros into ex_parceiros.
              ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
              perform limpar_tela.
              leave program.
            catch zcx_ordem_venda into ex_ordem_venda.
              ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
              perform limpar_tela.
              leave program.
            catch zcx_soft_expert_workflow into ex_soft_expert_workflow.
              ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
              perform limpar_tela.
              leave program.
          endtry.

          objeto->get_info_alv_apresentacao( importing e_apresentacao = e_apresentacao ).
          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].
      endcase.

    else.

      try .
          objeto = zcl_factory_carga=>zif_factory_carga~get_instance(
            )->set_factory_objeto( i_tp_carga = ptipca i_tp_produto = zif_carga=>st_tp_produto_carga_granel
            )->get_factory_objeto(
            )->novo_registro( exporting i_id_branch = pfilia  importing e_carga_recebimento = zde_zsdt0001cg_alv
            )->set_abrir( exporting i_nr_safra          = psafra
                                    i_id_bukrs          = pempre
                                    i_id_branch         = pfilia
                                    i_tipo_produto      = zif_carga=>st_tp_produto_carga_granel "*-CS2022000332-#78064-07.06.2022-JT
                          importing e_carga_recebimento = zde_zsdt0001cg_alv
            ).
        catch zcx_carga into ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          perform limpar_tela.
          leave program.
        catch zcx_ordem_carregamento into ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
          perform limpar_tela.
          leave program.
      endtry.

*-CS2021000183-#71105-29.03.2022-JT-inicio
      if zde_zsdt0001cg_alv-nr_perc_ava_arq is not initial.
        zde_zsdt0001cg_alv-nr_perc_ava_arq_old = zde_zsdt0001cg_alv-nr_perc_ava_arq.
        objeto->ck_digitado_ava_arq            = abap_true.
      endif.
      if zde_zsdt0001cg_alv-nr_perc_ava_que is not initial.
        zde_zsdt0001cg_alv-nr_perc_ava_que_old = zde_zsdt0001cg_alv-nr_perc_ava_que.
        objeto->ck_digitado_ava_que            = abap_true.
      endif.
      if zde_zsdt0001cg_alv-nr_perc_ava_mof is not initial.
        zde_zsdt0001cg_alv-nr_perc_ava_mof_old = zde_zsdt0001cg_alv-nr_perc_ava_mof.
        objeto->ck_digitado_ava_mof            = abap_true.
      endif.
      if zde_zsdt0001cg_alv-nr_perc_ava_pic is not initial.
        zde_zsdt0001cg_alv-nr_perc_ava_pic_old = zde_zsdt0001cg_alv-nr_perc_ava_pic.
        objeto->ck_digitado_ava_pic            = abap_true.
      endif.
      if zde_zsdt0001cg_alv-nr_perc_ava_fer is not initial.
        zde_zsdt0001cg_alv-nr_perc_ava_fer_old = zde_zsdt0001cg_alv-nr_perc_ava_fer.
        objeto->ck_digitado_ava_fer            = abap_true.
      endif.
      if zde_zsdt0001cg_alv-nr_perc_ava_ger is not initial.
        zde_zsdt0001cg_alv-nr_perc_ava_ger_old = zde_zsdt0001cg_alv-nr_perc_ava_ger.
        objeto->ck_digitado_ava_ger            = abap_true.
      endif.
      if zde_zsdt0001cg_alv-nr_perc_ava_ard is not initial.
        zde_zsdt0001cg_alv-nr_perc_ava_ard_old = zde_zsdt0001cg_alv-nr_perc_ava_ard.
        objeto->ck_digitado_ava_ard            = abap_true.
      endif.
      if zde_zsdt0001cg_alv-nr_perc_ava_ges is not initial.
        zde_zsdt0001cg_alv-nr_perc_ava_ges_old = zde_zsdt0001cg_alv-nr_perc_ava_ges.
        objeto->ck_digitado_ava_ges            = abap_true.
      endif.
*-CS2021000183-#71105-29.03.2022-JT-inicio

    endif.
    clear: ck_confer_carga.
    call screen 0300.

    if pidcarga is initial and ck_confer_carga eq abap_true and pmanut eq abap_false.
      try.
          clear: lc_filtro.
          objeto->get_registro( importing e_registro = wa_carga )->free( ).
          lc_filtro-iidcarga = value #( option = 'EQ' sign = 'I' ( low = wa_carga-id_carga  high = wa_carga-id_carga  ) ).
          lc_filtro-inrsafra = value #( option = 'EQ' sign = 'I' ( low = wa_carga-nr_safra  high = wa_carga-nr_safra  ) ).
          lc_filtro-iidbukrs = value #( option = 'EQ' sign = 'I' ( low = wa_carga-id_bukrs  high = wa_carga-id_bukrs  ) ).
          lc_filtro-iidbranc = value #( option = 'EQ' sign = 'I' ( low = wa_carga-id_branch high = wa_carga-id_branch ) ).
          objeto->pesquisar( exporting i_filtros = lc_filtro importing e_registros = it_retorno e_pesquisou = data(e_pesquisou) ).
          if e_pesquisou eq abap_true.
            call screen 0200.
          endif.
        catch zcx_carga into ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      endtry.
    endif.

    perform limpar_tela.

    leave program.
  endif.

  if c_service=>verifica_filtro_initial( ) eq abap_true.
    perform ajusta_tabela_tipo_entrada.
    call screen 0001.
  else.

    "Pesquisar
    try.
        clear: lc_filtro.
        lc_filtro-iidcarga[] = iidcarga[].
        lc_filtro-iidordem[] = iidordem[].
        lc_filtro-iidentra[] = iidentra[].
        lc_filtro-iidlocal[] = iidlocal[].
        lc_filtro-idtmovim[] = idtmovim[].
        lc_filtro-inrsafra[] = inrsafra[].
        lc_filtro-iidbukrs[] = iidbukrs[].
        lc_filtro-iidbranc[] = iidbranc[].
        lc_filtro-iidagent[] = iidagent[].
        lc_filtro-iidcolet[] = iidcolet[].
        lc_filtro-iiddesti[] = iiddesti[].
        lc_filtro-iiddesca[] = iiddesca[].
        lc_filtro-iidprodu[] = iidprodu[].
        lc_filtro-idstrato[] = idstrato[].
        lc_filtro-idsrebo1[] = idsrebo1[].
        lc_filtro-idsrebo2[] = idsrebo2[].
        lc_filtro-idsrebo3[] = idsrebo3[].
        lc_filtro-iidmotor[] = iidmotor[].
        lc_filtro-inrticke[] = inrticke[].
        lc_filtro-itpstatu[] = itpstatu[].
        lc_filtro-idtabert[] = idtabert[].
        lc_filtro-ihrabert[] = ihrabert[].
        lc_filtro-idtfecha[] = idtfecha[].
        lc_filtro-ihrfecha[] = ihrfecha[].
        lc_filtro-irroment[] = irroment[].
        lc_filtro-irromsai[] = irromsai[].

        zcl_factory_carga=>zif_factory_carga~get_instance(
          )->set_factory_objeto( exporting i_tp_carga = ptipca i_tp_produto = zif_carga=>st_tp_produto_carga_granel
          )->get_factory_objeto(
          )->pesquisar( exporting i_filtros = lc_filtro importing e_registros = it_retorno e_pesquisou = e_pesquisou
          ).

        if e_pesquisou eq abap_true.
          call screen 0200.
        endif.

      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    endtry.

    perform limpar_tela.
    leave program.
  endif.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  if c_service=>verifica_filtro_initial_e( ) eq abap_false.

    enrsafra[] = value #( option = 'EQ' sign = 'I' ( low = psafra high = psafra ) ).
    eidbukrs[] = value #( option = 'EQ' sign = 'I' ( low = pempre high = pempre ) ).
    eidbranc[] = value #( option = 'EQ' sign = 'I' ( low = pfilia high = pfilia ) ).

    submit zmmr126_0001
                   with iidcarga in eidcarga
                   with iidordem in eidordem
                   with iidentra in eidentra
                   with iidlocal in eidlocal
                   with idtmovim in edtmovim
                   with inrsafra in enrsafra
                   with iidbukrs in eidbukrs
                   with iidbranc in eidbranc
                   with iidagent in eidagent
                   with iidcolet in eidcolet
                   with iiddesti in eiddesti
                   with iiddesca in eiddesca
                   with iidprodu in eidprodu
                   with idstrato in edstrato
                   with idsrebo1 in edsrebo1
                   with idsrebo2 in edsrebo2
                   with idsrebo3 in edsrebo3
                   with iidmotor in eidmotor
                   with inrticke in enrticke
                   with itpstatu in etpstatu
                   with idtabert in edtabert
                   with ihrabert in ehrabert
                   with idtfecha in edtfecha
                   with ihrfecha in ehrfecha
                   with irroment in erroment
                   with irromsai in erromsai
                   with psafra   eq psafra
                   with pempre   eq pempre
                   with pfilia   eq pfilia
                    and return.
  endif.
  "  CLEAR: zib_nfe_dist_ter-chave_nfe.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100_exit input.
  leave to screen 0.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status 'PF0100'.
  set titlebar 'TL0100'.

  if edtabert[] is initial.
    append value #( sign = 'I' option = 'EQ' low = sy-datlo high = sy-datlo ) to edtabert.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0001 output.

  data: e_uri    type string,
        c_uri    type c length 400,
        it_ucomm type table of sy-ucomm.

  clear: it_ucomm[], it_ucomm.

  case ptipca.
    when zif_carga=>st_tp_carga_entrada_fob.
      append 'AUTOMATICO' to it_ucomm.
    when zif_carga=>st_tp_carga_saida_opus.
      append 'NOVA_CARGA' to it_ucomm.
      append 'NOVA_SOLIC' to it_ucomm.
      append 'AUTOMATICO' to it_ucomm.
    when zif_carga=>st_tp_carga_saida_ent_fob.
      append 'NOVA_CARGA' to it_ucomm.
      append 'NOVA_SOLIC' to it_ucomm.
  endcase.

  set pf-status 'PF0001' excluding it_ucomm.

  select single * into @data(wa_local)
    from j_1bbranch
   where bukrs  eq @pempre
     and branch eq @pfilia.

  concatenate wa_local-branch '-' wa_local-name into data(tx_local).

  case ptipca.
    when zif_carga=>st_tp_carga_entrada_fob.
      set titlebar 'TL0001' with psafra tx_local.
    when zif_carga=>st_tp_carga_saida_opus.
      set titlebar 'TL0002' with psafra tx_local.
  endcase.

  perform get_html_grafico changing html_pagina2.

  if splitter is initial.

    perform get_html_tela_inicial changing html_pagina .

    create object splitter
      exporting
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = 1
        columns = ( cond string( when html_pagina2 is initial then 1 else 2 ) ).

    call method splitter->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = ctl_cccontainer_html1.

    create object html_control_produto
      exporting
        parent = ctl_cccontainer_html1.

    data: data_table type standard table of text255,
          i_url2     type c length 200,
          i_url3     type c length 200.

    call function 'CONVERT_STRING_TO_TABLE'
      exporting
        i_string         = html_pagina
        i_tabline_length = 255
      tables
        et_table         = data_table.

    html_control_produto->load_data(
      importing
        assigned_url           = i_url2
      changing
        data_table             = data_table
      exceptions
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        others                 = 5
    ).

    if html_pagina2 is not initial.
      call method splitter->get_container
        exporting
          row       = 1
          column    = 2
        receiving
          container = ctl_cccontainer_html2.

      create object html_control_grafico
        exporting
          parent = ctl_cccontainer_html2.

    endif.

    myevent-eventid = html_control_produto->m_id_sapevent.
    myevent-appl_event = abap_true.
    append myevent to myevent_tab.

    call method html_control_produto->set_registered_events
      exporting
        events = myevent_tab.

    create object evt_receiver.

    set handler evt_receiver->on_sapevent for html_control_produto.

    html_control_produto->show_url(
      exporting
        url                    = i_url2
      exceptions
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        others                 = 5
    ).

  endif.

  if html_control_grafico is not initial.

    clear: data_table, data_table[].

    call function 'CONVERT_STRING_TO_TABLE'
      exporting
        i_string         = html_pagina2
        i_tabline_length = 255
      tables
        et_table         = data_table.

    html_control_grafico->load_data(
      "EXPORTING
      "  URL                    = I_URL
      importing
        assigned_url           = i_url3
      changing
        data_table             = data_table
      exceptions
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        others                 = 5
    ).

    html_control_grafico->show_url(
      exporting
        url                    = i_url3
      exceptions
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        others                 = 5
    ).

  endif.

*  IF PICTURE IS INITIAL.
*
*    CREATE OBJECT PICTURE
*      EXPORTING
*        PARENT = CTL_CCCONTAINER_PICTURE
*      EXCEPTIONS
*        ERROR  = 1.
*
*    CALL METHOD PICTURE->SET_DISPLAY_MODE
*      EXPORTING
*        DISPLAY_MODE = PICTURE->DISPLAY_MODE_STRETCH
*      EXCEPTIONS
*        ERROR        = 1.
*
*  PERFORM LOAD_PIC_FROM_DB USING PICTURE.
*
*  ENDIF.

endmodule.

form load_url_from_db changing p_url.

  data url(255).
  types pic_line(1022) type x.
  data  pic_tab type table of pic_line.

  clear url.
  url = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'.

  c_service=>get_pic_tab(
        exporting mime_url = '/SAP/PUBLIC/AMAGGI/Logo Principal.jpg'
        importing pic_tab  = pic_tab ).

  call function 'DP_CREATE_URL'
    exporting
      type    = 'image'
      subtype = 'GIF'
    tables
      data    = pic_tab
    changing
      url     = url
    exceptions
      others  = 1.

  p_url = url.

endform.

##PERF_NO_TYPE
form load_pic_from_db  using  gui_picture type ref to cl_gui_picture.

  data url(255).

  perform load_url_from_db changing url.

  call method gui_picture->load_picture_from_url
    exporting
      url = url.

endform.                               " LOAD_PIC_FROM_DB

class c_service implementation.
  method verifica_filtro_initial_e.
    if not ( eidcarga[] is initial and
             eidordem[] is initial and
             eidentra[] is initial and
             eidlocal[] is initial and
             edtmovim[] is initial and
             enrsafra[] is initial and
             eidbukrs[] is initial and
             eidbranc[] is initial and
             eidagent[] is initial and
             eidcolet[] is initial and
             eiddesti[] is initial and
             eiddesca[] is initial and
             eidprodu[] is initial and
             edstrato[] is initial and
             edsrebo1[] is initial and
             edsrebo2[] is initial and
             edsrebo3[] is initial and
             eidmotor[] is initial and
             enrticke[] is initial and
             etpstatu[] is initial and
             edtabert[] is initial and
             ehrabert[] is initial and
             edtfecha[] is initial and
             ehrfecha[] is initial and
             erroment[] is initial and
             erromsai[] is initial ).
      r_vazio = abap_false.
    else.
      r_vazio = abap_true.
    endif.
  endmethod.
  method verifica_filtro_initial.
    if not ( iidcarga[] is initial and
             iidordem[] is initial and
             iidentra[] is initial and
             iidlocal[] is initial and
             idtmovim[] is initial and
             inrsafra[] is initial and
             iidbukrs[] is initial and
             iidbranc[] is initial and
             iidagent[] is initial and
             iidcolet[] is initial and
             iiddesti[] is initial and
             iiddesca[] is initial and
             iidprodu[] is initial and
             idstrato[] is initial and
             idsrebo1[] is initial and
             idsrebo2[] is initial and
             idsrebo3[] is initial and
             iidmotor[] is initial and
             inrticke[] is initial and
             itpstatu[] is initial and
             idtabert[] is initial and
             ihrabert[] is initial and
             idtfecha[] is initial and
             ihrfecha[] is initial and
             irroment[] is initial and
             irromsai[] is initial ).
      r_vazio = abap_false.
    else.
      r_vazio = abap_true.
    endif.
  endmethod.

  method get_pic_tab.
    data pic_wa type xstring.
    data length type i.
    data mime_api type ref to if_mr_api.
    mime_api = cl_mime_repository_api=>get_api( ).
    mime_api->get( exporting
                     i_url             = mime_url
                     i_check_authority = abap_false
                   importing
                     e_content = pic_wa
                   exceptions others = 4 ).
    if sy-subrc = 4.
      return.
    endif.
    completo = pic_wa.
    clear pic_tab.
    length = xstrlen( pic_wa ).
    while length >= 1022.
      append pic_wa(1022) to pic_tab.
      shift pic_wa by 1022 places left in byte mode.
      length = xstrlen( pic_wa ).
    endwhile.
    if length > 0.
      append pic_wa to pic_tab.
    endif.
  endmethod.                    "get_pic_tab

endclass.                    "c_service IMPLEMENTATION


class lcl_event_receiver implementation.
  method: on_finished.

    data: lc_tempo type c length 11.

    if gb_st_carga eq 'W'.
      try .
          if lc_zcl_job is initial.
            zcl_job=>get_time_next_job(
              exporting
                i_job_name = zcl_job=>st_name_job_entrada_estoque
              importing
                e_jobname  = data(e_jobname)
                e_jobcount = data(e_jobcount) ).

            create object lc_zcl_job.
            lc_zcl_job->set_key_job( exporting i_jobname = e_jobname i_jobcount = e_jobcount ).

          endif.

          lc_zcl_job->get_job_registro_info( importing e_texto_info = data(e_texto_info) e_atrasado = data(e_atrasado) ).

          if e_atrasado ne abap_true.
            message e_texto_info type 'S'.
          else.
            message e_texto_info type 'S' display like 'E'.
          endif.

        catch zcx_job.    "
      endtry.
    endif.

    if go_clock is not initial.
      go_clock->interval = 5.
      go_clock->run(
        exceptions
          error  = 1
          others = 2
      ).
      if sy-subrc <> 0.
      endif.
    endif.

  endmethod.                           " ON_FINISHED
endclass.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


class lcl_event_receiver_est implementation.
  method: on_finished.

    data: lc_tempo type c length 11.

    if gb_st_carga_est eq 'W'.
      try .

          if lc_zcl_job_est is initial.

            zcl_job=>get_time_next_job(
                exporting
                   i_job_name = zcl_job=>st_name_job_estorno_estoque
                importing
                   e_jobname = data(e_jobname)
                   e_jobcount = data(e_jobcount) ).

            create object lc_zcl_job_est.
            lc_zcl_job_est->set_key_job( exporting i_jobname = e_jobname i_jobcount = e_jobcount ).
          endif.

          lc_zcl_job_est->get_job_registro_info( importing e_texto_info = data(e_texto_info) e_atrasado = data(e_atrasado) ).

          if e_atrasado ne abap_true.
            message e_texto_info type 'S'.
          else.
            message e_texto_info type 'S' display like 'E'.
          endif.

        catch zcx_job.    "
      endtry.
    endif.

    if go_clock_est is not initial.
      go_clock_est->interval = 5.
      go_clock_est->run(
        exceptions
          error  = 1
          others = 2
      ).
      if sy-subrc <> 0.
      endif.
    endif.

  endmethod.                           " ON_FINISHED
endclass.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

form limpar_tela.

  clear: objeto.

endform.

*&---------------------------------------------------------------------*
*&      Form  SELECAO_PADRAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selecao_padrao .
  ck_selecionou = abap_false.

  screen_tela = 9001.

  call screen 9010.

  if ck_selecionou eq abap_false.
    leave program.
  else.
    clear: enrsafra, eidbukrs, eidbranc.
    enrsafra-sign   = 'I'.
    enrsafra-option = 'EQ'.
    enrsafra-high   = psafra.
    enrsafra-low    = psafra.
    append enrsafra.

    eidbukrs-sign   = 'I'.
    eidbukrs-option = 'EQ'.
    eidbukrs-high   = pempre.
    eidbukrs-low    = pempre.
    append eidbukrs.

    eidbranc-sign   = 'I'.
    eidbranc-option = 'EQ'.
    eidbranc-high   = pfilia.
    eidbranc-low    = pfilia.
    append eidbranc.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_TABELA_TIPO_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ajusta_tabela_tipo_entrada .

*  DATA: WA_ZSDT0001TETX TYPE ZSDT0001TETX.
*
*  SELECT CE~ID_ENTRADA,
*         CE~DS_ENTRADA
*    FROM ZSDT0001TE AS CE
*    INTO TABLE @DATA(IT_ZSDT0001TE).
*
*  SELECT *
*    FROM ZSDT0001TETX
*    INTO TABLE @DATA(IT_ZSDT0001TETX).
*
*  SORT IT_ZSDT0001TE BY ID_ENTRADA.
*  DELETE ADJACENT DUPLICATES FROM IT_ZSDT0001TE COMPARING ID_ENTRADA.
*
*  DESCRIBE TABLE IT_ZSDT0001TE   LINES DATA(LC_QTD_LINHAS1).
*  DESCRIBE TABLE IT_ZSDT0001TETX LINES DATA(LC_QTD_LINHAS2).
*
*  IF LC_QTD_LINHAS1 NE LC_QTD_LINHAS2.
*
*    DELETE FROM ZSDT0001TETX.
*
*    CLEAR: IT_ZSDT0001TETX[], IT_ZSDT0001TETX.
*
*    LOOP AT IT_ZSDT0001TE INTO DATA(WA_ZSDT0001TE).
*      WA_ZSDT0001TETX-ID_ENTRADA = WA_ZSDT0001TE-ID_ENTRADA.
*      WA_ZSDT0001TETX-DS_ENTRADA = WA_ZSDT0001TE-DS_ENTRADA.
*      APPEND WA_ZSDT0001TETX TO IT_ZSDT0001TETX.
*    ENDLOOP.
*
*    IF IT_ZSDT0001TETX[] IS NOT INITIAL.
*      MODIFY ZSDT0001TETX FROM TABLE IT_ZSDT0001TETX[].
*      COMMIT WORK.
*    ENDIF.
*
*  ENDIF.

endform.

*&---------------------------------------------------------------------*
*&      Form  NOVA_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form nova_carga .

*-CS2022000332-#78064-07.06.2022-JT-inicio
  try.
      zcl_carga_recebimento_v0001=>zif_carga~get_instance(
        )->set_validar_safra( exporting i_safra = psafra
                                        i_acao  = cl_myevent_handler=>st_action_grao
        ).
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      exit.
  endtry.
*-CS2022000332-#78064-07.06.2022-JT-fim

  submit zmmr126_0001 with ptipca   eq ptipca
                      with pck_cad  eq abap_true
                      with psafra   eq psafra
                      with pempre   eq pempre
                      with pfilia   eq pfilia and return.
endform.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETORNO_ALV_ID_CARGA  text
*----------------------------------------------------------------------*
form mostrar_carga  using p_id_carga type zde_zsdt0001cg_alv-id_carga changing p_ret_alv type ty_itens_alv.

  data: e_apresentacao type zde_carga_apresentacao.

  submit zmmr126_0001 with pck_cad  eq abap_true
                      with psafra   eq psafra
                      with pempre   eq pempre
                      with pfilia   eq pfilia
                      with pidcarga eq p_id_carga and return.

  try .

      zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto_id( i_id_carga = p_id_carga
        )->get_factory_objeto(
        )->set_registro( i_id_carga = p_id_carga
        )->get_info_alv_apresentacao( importing e_apresentacao = e_apresentacao
        )->free(
        ).

      move-corresponding e_apresentacao-carga to p_ret_alv.
      perform seta_icone_status using p_ret_alv-tp_status changing p_ret_alv-ico_carga.
    catch zcx_carga.
    catch zcx_ordem_carregamento.
  endtry.

endform.

*&---------------------------------------------------------------------*
*&      Form  GERAR_ENTRADA_ESTOQUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_CARGA>  text
*----------------------------------------------------------------------*
form gerar_entrada_estoque using p_id_carga type zde_zsdt0001cg_alv-id_carga changing p_ret_alv type ty_itens_alv.

  try .

      zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto_id( i_id_carga =  p_id_carga
        )->get_factory_objeto(
        )->set_registro( i_id_carga = p_id_carga
        )->set_gerar_romaneio_saida(
        )->set_gerar_entrada_estoque(
        )->set_processar_entrada(
        )->get_info_alv_apresentacao( importing e_apresentacao = data(e_apresentacao)
        )->free(
        ).

      move-corresponding e_apresentacao-carga to p_ret_alv.

    catch zcx_parceiros into ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_cadastro into ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_ordem_carregamento into ex_ordem_carregamento.
      ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_pedido_compra_exception into ex_pedido.
      ex_pedido->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_miro_exception into ex_miro.
      ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_ordem_venda into ex_ordem_venda.
      ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_job into ex_job.
      ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  endtry.

  perform chamar_atualizador using e_apresentacao.

endform.

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_ENTRADA_ESTOQUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_CARGA>  text
*----------------------------------------------------------------------*
form estornar_entrada_estoque using p_id_carga type zde_zsdt0001cg_alv-id_carga changing p_ret_alv type ty_itens_alv.

  try .

      zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto_id( i_id_carga = p_id_carga
        )->get_factory_objeto(
        )->set_registro( i_id_carga = p_id_carga
        )->set_gerar_estorno_estoque(
        )->set_processar_estorno(
        )->get_info_alv_apresentacao( importing e_apresentacao = data(e_apresentacao)
        )->free(
        ).

      move-corresponding e_apresentacao-carga to p_ret_alv.

    catch zcx_parceiros into ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_cadastro into ex_cadastro.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_ordem_carregamento into ex_ordem_carregamento.
      ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_pedido_compra_exception into data(ex_pedido).
      ex_pedido->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    catch zcx_job into data(ex_job).
      ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  endtry.

  perform chamar_atualizador_estorno using e_apresentacao.

endform.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_MSG_INTERFACE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CARGA_ROMANEIO_ID_CARGA  text
*----------------------------------------------------------------------*
form mostra_msg_interface  using  p_id_carga type zde_id_carga.

  try .

      zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto_id( i_id_carga = p_id_carga
        )->get_factory_objeto(
        )->get_mens_interface_entrada( exporting i_id_carga  = p_id_carga importing e_mensagens = it_mensagens
        )->free(
        ).

      call screen 9003 starting at 40 05.

    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
  endtry.

endform.

*&---------------------------------------------------------------------*
*&      Form  SETA_ICONE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RETORNO_ALV_TP_STATUS  text
*      <--P_IT_RETORNO_ALV_ICO_CARGA  text
*----------------------------------------------------------------------*
form seta_icone_status  using    p_tp_status type zde_status_carga
                        changing p_ico_carga type char04.

  case p_tp_status.
    when zif_carga=>st_status_aberto.
      p_ico_carga = icon_import_transport_request.
    when zif_carga=>st_status_fechado.
      p_ico_carga = icon_import_all_requests.
    when zif_carga=>st_status_conferido.
      p_ico_carga = icon_transport_proposal.
    when zif_carga=>st_status_cancelada.
      p_ico_carga = icon_terminated_position.
  endcase.

endform.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_ATUALIZADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RET_ALV  text
*----------------------------------------------------------------------*
form chamar_atualizador  using p_carga type zde_carga_apresentacao.

  data: ck_chamar type c length 1.

  check gb_id_carga is initial.

  ck_chamar = abap_false.

  loop at p_carga-notas into data(wa_notas).
    if wa_notas-obj_key_entrada is not initial and wa_notas-docnum is initial.
      ck_chamar = abap_true.
    endif.
  endloop.

  check ck_chamar eq abap_true.

  gb_id_carga = p_carga-carga-id_carga.
  try .
      call function 'Z_VERIFICA_ENTRADA_STATUS'
        starting new task 'VERIFICA_STATUS'
        destination 'NONE'
        performing mostra_status on end of task
        exporting
          id_carga = gb_id_carga.
    catch cx_root.
      clear: gb_id_carga.
  endtry.

  gb_st_carga = 'W'.

  perform time using abap_true.

endform.

form mostra_status using taskname.

  data: r_status type char01.

  receive results from function 'Z_VERIFICA_ENTRADA_STATUS'
   importing r_status = r_status.

  perform time using abap_false.

  if gb_id_carga eq wa_carga_romaneio-id_carga.
    clear: gb_id_carga.
    if r_status eq 'E' or r_status eq 'S'.
      gb_st_carga = r_status.
      if obg_toolbar_2 is not initial.
        set user-command 'REFRESH_EN'.
      endif.
    else.
      clear: gb_id_carga, gb_st_carga.
    endif.
  else.
    clear: gb_id_carga, gb_st_carga.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_ATUALIZADOR_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RET_ALV  text
*----------------------------------------------------------------------*
form chamar_atualizador_estorno using p_carga type zde_carga_apresentacao.

  data: ck_chamar type c length 1.

  check gb_id_carga_est is initial.

  ck_chamar = abap_false.

  loop at p_carga-notas into data(wa_notas).
    if wa_notas-obj_key_entrada is not initial and wa_notas-docnum is not initial.
      ck_chamar = abap_true.
    endif.
  endloop.

  check ck_chamar eq abap_true.

  gb_id_carga_est = p_carga-carga-id_carga.
  try .
      call function 'Z_VERIFICA_ESTORNO_STATUS'
        starting new task 'VERIFICA_STATUS_ESTORNO'
        destination 'NONE'
        performing mostra_status_estorno on end of task
        exporting
          id_carga = gb_id_carga_est.
    catch cx_root.
      clear: gb_id_carga_est.
  endtry.

  gb_st_carga_est = 'W'.

  perform time_estorno using abap_true.

endform.

form mostra_status_estorno using taskname.

  data: r_status type char01.

  receive results from function 'Z_VERIFICA_ESTORNO_STATUS'
   importing r_status = r_status.

  perform time_estorno using abap_false.

  if gb_id_carga_est eq wa_carga_romaneio-id_carga.
    clear: gb_id_carga_est.
    if r_status eq 'E' or r_status eq 'S'.
      gb_st_carga_est = r_status.
      if obg_toolbar_2 is not initial.
        set user-command 'REFRESH_ES'.
      endif.
    else.
      clear: gb_id_carga_est, gb_st_carga_est.
    endif.
  else.
    clear: gb_id_carga_est, gb_st_carga_est.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ATIVAR text
*----------------------------------------------------------------------*
form time  using p_ativar type char01.

  case p_ativar.
    when abap_true.

      if go_clock is initial.
        create object: go_clock.
      else.
        go_clock->cancel( ).
      endif.

      if go_alarm is initial.
        create object: go_alarm.
        set handler go_alarm->on_finished for go_clock.
      endif.

      go_clock->interval = 1.
      call method go_clock->run.

    when abap_false.

      clear:
         go_alarm,
         go_clock,
         lc_zcl_job.

  endcase.

endform.

*&---------------------------------------------------------------------*
*&      Form  TIME_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ATIVAR text
*----------------------------------------------------------------------*
form time_estorno  using p_ativar type char01.

  case p_ativar.
    when abap_true.

      if go_clock_est is initial.
        create object: go_clock_est.
      else.
        go_clock_est->cancel( ).
      endif.

      if go_alarm_est is initial.
        create object: go_alarm_est.
        set handler go_alarm_est->on_finished for go_clock_est.
      endif.

      go_clock_est->interval = 1.
      call method go_clock_est->run.

    when abap_false.

      clear:
         go_alarm_est,
         go_clock_est,
         lc_zcl_job_est.

  endcase.

endform.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_ULTIMO_ACESSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_ultimo_acesso using p_ok type char01.

  p_ok = abap_false.

  select single * into @data(wa_zsdt0001us)
    from zsdt0001us
   where us_name eq @sy-uname.

  check sy-subrc is initial.
  check wa_zsdt0001us-id_bukrs is not initial.
  check wa_zsdt0001us-id_branch is not initial.
  check wa_zsdt0001us-nr_safra is not initial.
  check wa_zsdt0001us-tp_carga is not initial.

  authority-check object 'M_MATE_WRK'
      id 'WERKS' field  wa_zsdt0001us-id_branch
      id 'ACTVT' field '03'.    "Alteração

  psafra  = wa_zsdt0001us-nr_safra.
  pempre  = wa_zsdt0001us-id_bukrs.
  pfilia  = wa_zsdt0001us-id_branch.
  ptipca  = wa_zsdt0001us-tp_carga.

  p_ok = abap_true.

endform.

include zmmr126_status_9005_0001.
*INCLUDE ZMMR126_STATUS_9005.

include zmmr126_status_9006_0001.
*INCLUDE ZMMR126_STATUS_9006.

include zmmr126_status_9011_0001.
*INCLUDE ZMMR126_STATUS_9011.

class cl_myevent_handler implementation.

  method on_sapevent.

    data: lc_id_carga type zde_id_carga.

    case action.
      when cl_myevent_handler=>st_action_grao.
*-CS2022000332-#78064-07.06.2022-JT-inicio
        try.
            zcl_carga_recebimento_v0001=>zif_carga~get_instance(
              )->set_validar_safra( exporting i_safra = psafra
                                              i_acao  = cl_myevent_handler=>st_action_grao
              ).
          catch zcx_carga into ex_carga.
            ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
        endtry.
*-CS2022000332-#78064-07.06.2022-JT-fim

        submit zmmr126_0001 with pck_cad eq abap_true
                            with psafra   eq psafra
                            with pempre   eq pempre
                            with pfilia   eq pfilia and return.

        perform import_memory changing lc_id_carga.

        check lc_id_carga is not initial.

        objeto = zcl_factory_carga=>zif_factory_carga~get_instance(
          )->set_factory_objeto_id( i_id_carga = lc_id_carga
          )->get_factory_objeto(
          ).

        try.
            clear: lc_filtro.
            lc_filtro-iidcarga = value #( option = 'EQ' sign = 'I' ( low = lc_id_carga  high = lc_id_carga  ) ).
            lc_filtro-inrsafra = value #( option = 'EQ' sign = 'I' ( low = psafra high = psafra ) ).
            lc_filtro-iidbukrs = value #( option = 'EQ' sign = 'I' ( low = pempre high = pempre ) ).
            lc_filtro-iidbranc = value #( option = 'EQ' sign = 'I' ( low = pfilia high = pfilia ) ).
            objeto->pesquisar( exporting i_filtros = lc_filtro importing e_registros = it_retorno e_pesquisou = data(e_pesquisou) ).
            if e_pesquisou eq abap_true.
              call screen 0200.
            endif.
          catch zcx_carga into ex_carga.
            ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        endtry.

      when cl_myevent_handler=>st_action_algo.
*-CS2022000332-#78064-07.06.2022-JT-inicio
        try.
            zcl_carga_recebimento_v0001=>zif_carga~get_instance(
              )->set_validar_safra( exporting i_safra = psafra
                                              i_acao  = cl_myevent_handler=>st_action_algo
              ).
          catch zcx_carga into ex_carga.
            ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
        endtry.
*-CS2022000332-#78064-07.06.2022-JT-fim

        clear: lc_id_carga.

        submit zmmr153 with pck_cad eq abap_true
                       with psafra   eq psafra
                       with pempre   eq pempre
                       with pfilia   eq pfilia
                       and return.

        perform import_memory changing lc_id_carga.

        check lc_id_carga is not initial.

        objeto = zcl_factory_carga=>zif_factory_carga~get_instance(
          )->set_factory_objeto_id( i_id_carga = lc_id_carga
          )->get_factory_objeto(
          ).

        try.
            clear: lc_filtro.
            lc_filtro-iidcarga = value #( option = 'EQ' sign = 'I' ( low = lc_id_carga  high = lc_id_carga  ) ).
            lc_filtro-inrsafra = value #( option = 'EQ' sign = 'I' ( low = psafra high = psafra ) ).
            lc_filtro-iidbukrs = value #( option = 'EQ' sign = 'I' ( low = pempre high = pempre ) ).
            lc_filtro-iidbranc = value #( option = 'EQ' sign = 'I' ( low = pfilia high = pfilia ) ).
            objeto->pesquisar( exporting i_filtros = lc_filtro importing e_registros = it_retorno e_pesquisou = e_pesquisou ).
            if e_pesquisou eq abap_true.
              call screen 0200.
            endif.
          catch zcx_carga into ex_carga.
            ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        endtry.

      when others.
        exit.
    endcase.

  endmethod.

endclass.

*&---------------------------------------------------------------------*
*&      Form  GET_HTML_TELA_INICIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_HTML_PAGINA  text
*----------------------------------------------------------------------*
form get_html_tela_inicial
   changing
     p_html_pagina type string.

  data: lc_url type c length 255.

  perform load_url_from_db changing lc_url.

  data: image_granelx type xstring.
  c_service=>get_pic_tab( exporting mime_url = '/SAP/PUBLIC/AMAGGI/Soja.jpg' importing completo = image_granelx ).
  data(imagem_granel) = 'data:image/jpeg;base64,' && zcl_string=>xstring_to_base64( i_xstring = image_granelx ).

  data: image_algodaox type xstring.
  c_service=>get_pic_tab( exporting mime_url = '/SAP/PUBLIC/AMAGGI/Algodao.jpg' importing completo = image_algodaox ).
  data(image_algodao) = 'data:image/jpeg;base64,' && zcl_string=>xstring_to_base64( i_xstring = image_algodaox ).

  data(imagem_amaggi) =
  'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOAAAAAiCAYAAABGDdVeAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsQAAA7EAZUrDhsAACYxSURBVHhe7VwJfFXVmf/yspKVJATCIigoCEpVlCquaKeMiFupI67oiExB0CqCdBhaRBlaxCKtIq' &&
  'jFWtx1BKFFGGfEBSUqiyiCsgjImhAe2ffkZf7/75zz3n0vLyGBznTmN/z18N6799xzvvPtZ7mJaQTkBE7gBP4m8NnPEziBE/gboI0RsNZ+OruNQeF3frYO9Shxwi7xTMDTtc/+5qc0mGsSaz+PEYGA+Wz0+JmYRgn4zPUA2o/Tb21DwNLnC9JJkNa20OvGHskH0hbpF+04IhGw9cAz0m' &&
  'ToSQhvz/Xji2yTcPUCYT2Ydny4xnZDY4oLG69BfRgPKV3TUj3psDDy5nW0FUabQT3oj/M2DRkpVBeODvZKBPWKUF0yX7XrRk9bsaF6TZ4Nk4WnLUJ/e+5rG+Z3Pb4fiy61yQApZK9AiLZ2Gs0AnUFEwtcmhY4GGJtyP4SQGrpvlsltQqMyPBLHIoAgLC+ojNFwtLa9ihQ5ZiI6L9mnqc' &&
  't/nUOhjCPlTIQU3H1aqLKb665vn/I3ko7oBliDZ9lbnG2nBt/1N2+2Ahw7KXcUOzojjcv9bg6mnjGmSPCeu+7qEbzmfEei/WwL2mSAxngsHNODzG8dTBuuS/eci6wUWUj4x6XQgFcwVK5gxArEQ1fQNz0t9IRKYxSm9eNw46ayOCRaQR8rSG+QRiD03YygCQJVuEwu4X6A/aKoMzPPOT' &&
  '7yMzovjcGYlskI0M9swTwGsB2OyRkS+BYJ5QPr8Dvq++zDAXBGaSNMNG3eCdTpNyd7/mJPxyx/0kR4ZExDYbscqzEU1sF9HbOr19RoWS/M8MgKF011OKxH2GttRNtSUJfSBcm08DHtwb2oaU4kTHdeL2MG5hlI2ACPA05/rRCMsqBtx0R3XUXDzlrbIRqmwkNZ6/FIHB1IADzxUbRtJd' &&
  'oImKTG419fUHGbpoCRIN9CEcdCDZEgfe47ELUZ9mp6DvKEiKEsnTwcYiOlrgg5U0OHGiFhCfLSH93J4fmg7Al7vw1OPfx5Cz5vSVE5Eyp7fA/K3vObAKlBA4y4HozKEddV790UJzTUVqONBmgiVQBerga9OU/VTo2PRFnCWoTpzikdxWb8n2lLPZ8b5DEMKAxOAEAN2iJzWZJQ2C/Zpk' &&
  'qvFdlZ6zpkG6SZbfBJw4NKNMh5Txv8NscJImrAC7bFQrD9/eUVUlleK9v3+6W0xC/lleXmpkVqcqokJCbIyZ1zJS05SZJTE6RdSoqkgm3JuG/GBmocD6IODTdhMJRnna3g6MBopBA0FBwqld0H86W2pjYqDekZ2ZKbmaI0dM5Ol4T4WO3fgXQkWh7TGENRMFwPCHcnzjn6ozl0W68K9a' &&
  'iZoTwqBCcRukbVK/Rbgfocn4ObrXqfT0dJRH1OCWh83vqR4HhTzNc2o3kDVOUINygqBolZv2OPLHzjL1JYWiXXDblY7rzifGWey+FZr3k1NN3Rc9OIj9Q1yNNvLJX1X38jfbp1lp/dMUJ6pKZIOxVaVK1pBm4YXpopdJFytLMtv0iefP5VKSgrklsuHShDhw5Rxmk/GnWaMx5vCkJV8U' &&
  'kpfj/5b+/KJ+s2SZfcLjLmpivljNxMtBUN0egCrDKQXzTAavz6EnxdvfFbWf7ZFsnbdQQMT9WqRoucmlpQ+ZhK1zMNxb3GWhnUM1fap8fLmb1Okr49TpL+vU6WvqArRVWL5hhBA8eDsdf7ktWhUBbrv/pGlq3eJB9/vUO2FlvXGAf1jWYMjgaOJYARxFQpDf26dpJTumVL/1O6yLk/6C' &&
  'tZMMqQgzI0cPGGI1J92nlQ/jMvT8qLi+WRcXdJGmWitY4ifzXAGHn367Xy7Orleikhne41HBf1/IFcecGlkhubpr9nvTxfthTnR61L1JZWy6wxEyUH9fP9B+WRRU9LoIPXrYTjzKxuMnrY7ZITxWaOhugGyIaCYZoX8F0bb5B1+WUycOwjIhkn4zqYX1Qgk0acK9NuuVZSlCH18DAJxu' &&
  'Pw0TB45xwNUgLNunXaQlm+NR9hCcpWWSN90mvkpRnjZUBuOmoaAbAdGjUFpoaOEg4OgcqCu56VQYHQA0gLP9x5SK54aD5cdjaug0Z/sUy4+iyZMXq4VQy26PxgJGqlAvdYIxF1S6CsD859VRZ+vEekfa4IBCmNRbJ23mQ5D8pOPrmFFHp9H2kjX3zxasjOjBItr6rAq0+hgL/90zuyfP' &&
  'NhQ3cyuJcIc2aVoiLQDUUpLdbnwpAAqmgYCVQOGhh5wE+gvExkz3bZ8NZsOaerkUaNGoDx7Aof6iN1LsSYFq/6TJ545V3ZegSdZlG2QCzNAyhC3zR0ooFEAbGWz3GgE5GwCRgtYfeDTkuQj2bfD+eMHzo/NBzYXxcrLyx9T15c+RkMHXRh3IN6JMhK1E1v1mFEAcZSiGd//uKvZZn/K0' &&
  'nKSoTaxklCg3m2NrZRqooq5JykLvLC6IelC4zqy7Ld8ncL/zmsbk2ccTb8zfrXZv9A5t/+zyr3OR+8LrM2vi3tEOm97RL8XVJaLr8fNkZGnnaZSoAg9cZht+xEWjbAWDbgGomVXbh8w70Py4ZyKFo8mE7PW1chcrhAVs39uVzSszM6BfPoFQmriCG49ng/AGMulYF3PSrSsQcuUYkg1L' &&
  'py6RNXqkZ4NhQ6lI4wTWt+scQsw1tgHlRv0wx61ysm/U4krbPtA4OogzKV7JZ9bzwuXZUUtBfNwytq0TZTJ+O1X/zkC7nrsbdBc3dzmzw4vFOGnpYjb/7mQTvB50g4Tn7iSXVeZLO5RrDNMpSnEEmnPr/S8sDGUNJX7kf1Mplw1SXSu1dX6X1yd4kN1EkDDDkhI0lqS6rlyKEy2V1wSA' &&
  '4cOiR52/ZI3sEDzMcgqiwYLQZWUqCOwfCxUVcbzTyT/DPGz6g3+pdzZcWWQpEOnUCDTaasXPt17yK3DD5dep7SU1NNwtFBMD09cLBQdu3zS96OQtlyAA7DB4cRB5cJpzqsR6q8Pn2UKrLLjCjRn/7it7JiG4w0PQe/iDoZlNOoBpiqsmxZcb2gazgAXv1o9jip6GycDA3DayTxh2qkb1' &&
  'onefqe2ZKF4b/0yXKZvPYlqeoUJ5lwBgSNj3D1J10yQm6/aBjkVCqTXpwnH9bt1DYj2yZY/53x86Ubsrf0oP2Y7Mm0Gh0tzwF1zlevKQpV54GXl8j8xZ+JZPbV2xJAmkSBNUDtqjfL5gXT5HR4a18DBuQmuWFgKywmGmxEWjjwZ3OhfF1xDc9YBkh9hfRLrZc/Tb3dpHY2WhgjIbMi23' &&
  'XCxb+oWw/Fou/e6C+Si/9pljG+uPZaT2LYDmg49I0aYCekR2ZuADShl6BD4TOJxmGMfwKRr5e5xQhBRSXK6+TxWy+UMdddbNNnM+dhMaMibeSnQRV4OvW5xTJnxSaR7G7mYp2NOOX58vwDP5GrLzoHysg0GY8q38JBJeW4+RSdTTmGsXvXQdm2e48s/WSdrPjwQ1n70jNyXkeMXR2YoY' &&
  'sWwHnft+XVcsYdcICJGZjEdGCTqIdG4FCYSv7r2BFyIZwqQQlwLhcJ55jo+TlDdPPGNRs2aSpdU1UsHz31sNYj2EYV6o9g5nMArTbCYGLIR6SvMMAVMEBQ0zZAfhUY3+vbP5T7li/QSBUJGgoj1T+eNVR+c/lIdZSjX50pf67bZioAzgAJZ4SvjXtELkjrJp+W7dOoGa1twkXN3yFq5l' &&
  'Bf7QId0ZIBOr6EgUI1Aue3OKRgIovX75D5r8P4MuAlSwtwC1dpfG7VLZApc/6wTL26Gl+bYCklYqikdbLlSIXcNvUp2Qwj5SSbdHBV0M3HooORL0GVUY3vbqTKVC69hbGwENQmoDbO9svmoxofq6IOUtRSfN732z+ZNJYKU4+0rAa+lzxgyciWiQv/Ip8j4pbb8Tjmaq/BSA5+wvheQy' &&
  'Sd8+fPYXxQ8KDg69TxbH5hstwI48vBk5oiwwkwlfUWg1pwpVbneDkw7lPgSy8/uZOMwpz8tV+Ok81v/1HngGw3CCUjXuU0ccYCsB7RMoE8ArU0Pv8+mXDlD2UpDIEZDVPlRLQdzfgIzuN5h/dzQEe/5BjQkCETbhiixrTsiV9ialKp42jHcWDOSeUP1IB/dRibk4kXdIjOKbYSdHrXIA' &&
  'X8h0791RgiwYiVkZ4qf/xyhazauVGvTb3xXkmFE0istxmbRSNoYkpa1zFRJr38pHzXUCX90k7WNDNa2wQNkynwAqSrXOTRleRWwOlIs6BCc0XulhnzMDdA2lVZKWOvOw3pzXZwH8yjB2MkTOkoCz/YK69hLsFUR+daLSDoFRrpv61wqdgN5XLT4P5ou0S21qerETJNYkqpxtAMTMpn/u' &&
  'WCixoftwWSkkx7DaWmMPoRuJdQbyMCFDRahCG4OliDOSAXXfK2mwg2qH2lplYKOiAWpM7kz/g5b2pEIi0OwbHC+KrQVjHGc9cTS1Afxsdq5B+fxzyPUZ+LUCmMljRa0GlOlbCVUNEUTVN93sOndkdDq9e0nWlQv+RkXZ1jtAuqOb9DPovhAFZs3Wfmm/FsB32V+WXoqRnyCObGTAzj1P' &&
  'ir8UwCxGn4H1mY0rIY4Bp5DtDguMTRARkG6eV18pGfrO1LRFRmVuKJOscKd5CDiyCP3TxF53sOLkUkaIQ0lHvfmCPfI2XtgfngkzdO0MgYiaL4BjXCL6oPyNyPlqhM7zztYjVwGigL4b6zsO2Fny6XTZhjMmgwswpmV83AZ5uxBbAPkLlcsKcIJj7xAoQEZcHEftQFPWX6rT+RKTdcAo' &&
  'XBvMESoukT5hBjnlopm2AATEtUqdXzMxemahihqbCDQiPcNaC4VK7t310+nHUP+vPrStyQB+Zpukpj0cm8hWnTFKY1VMAte5HWjn8WGtBRBnROko9n3yl9OyNqVUORNGKjmgcumhiFNnwwxmiuU7nWIKpNfSUPEkY7h76XP0wbLQ+PHQ6FPWjrQfs5f4tL0TnQb5BaVtFQoLSh9JZKzv' &&
  'GLvPv5V6gLRXVOKpaL3iIThvaX/ohYNBpdsNBFC5fCGtpChR80fn6nksWjOxoAnwM9vMdi6zJS0plQBlSmN1etwVQCDpVzeQXqFRfKpH+6xfSvxo+eYXzKYxgt9/wii4nIqIb/nHw5ZaHjIPXGHOPVgPnbzD8tmEUpQtfMNzu+KKBsjHyoB/jE/9o/9x8xXsb72bfeq5HKO1fzGgrniT' &&
  'PeeFJ1+7KeZ8uU066S6iPGbRJMRbNqDeVcqGHU/BRRk2NzUZN12BbhTV1d1OScNPJUk9NVb3RXv2nSOhR7w0QFfvHJq4hoKzbu1ZWuPu1r5V9+frPOS+658ToZ1AupCz13LEVm0xxMqn8193nBVFwFbro0kca2Gg6uoqlaElQeszR86andZdVjY8G5WtlyuEwj4UF0wYjsBkHlZrtqmL' &&
  'i0Lr9CBtyPOV9CQPok18uLv5kkZ2ZnSgaitibkKnzS5AWvKRvw4RZ6SCk6g7Gzz7Ez/2BSxRK/LJg4QnojQtFQnp8wAga5H3VZH+1SIKg3Z8nHurLJeYlRJhbyl79FPln7rfJWEcBzFGDREblsQG+rsKxPelGHMlG58FlvATiUoJDNSRM1AP2JZ+xUQOWAeSyrE4zAKzbswPMwfFUeQ0' &&
  'u/nidL75M64hueU/pM5GUNo2KRNLDwDls2iw2uMDUNIkwRDR2BOEZWp/SWF0FgBBHK2xRwaOQPUz3wlXrBhrlb0xfp4uSzr9fI5o2ADjScNws2yYcwKnJoxJXXa9R0dfnpfY5G6KImV1EZNV0q6jVEggbPqPnK6nd06qZDo3qg2A8SwH8VOnJ6MoUdNPN0KuE6RJ0xc5Eqcd5XVCDzp9' &&
  'wtHVGF6QVTi98/eIfIEUYBjJwRgIQgnVmxu1qeRcrGvS1lDLwp2wulKs2BXtcR1yA/xByEq3h9UgOyFQP+0diZsg3pME3dpB1VltZGjboDx8H4QOuArBp5b/4UzKeQymlbR4eyUA0avICyGk+eINPmvSFby8GmCr8M65stN2F+xWjCuREXSUZdfjoiITIBprYuve3YQ1deqegmxyCtvF' &&
  'evXperhTrvYkR2iG+ULl25GEWEBNRWOANQgJduzqxRCeC9776H0+DCGaHKA9rq6uWU7BRprynjsffvwK0O9sXWaWaMBsodjeoWakDK+VaDzoSyUZaqo/Br0XSZJaZSMvB51+ChcmGHU/QZB2+k4nyQRnUYRsX9QUZNLrq4qMnirX84qU6jJpPVSxE1aeAuarp6zhDZ9u/ff0s+goFXkJ' &&
  'fWEVqOG39l4TMpkmW4ph38Eid+1NJFB85TkBY+fteP5QIYhNnUJYNrdYXy8VFXwzgPqwDNKh7aS8uVqS+vkA2IAkqi9fYmsjQHQ6SLwlSKFCnURYSFs8ZLH+TXTEdv/8Vs2QUjZMrJ+QQXNLg3OeiBh6DEaZj3xOqqW1fk8ExVgizknhf78DC1CdT4EQPRbjnaf2f917JwDaI/9yirDs' &&
  'ljk27STWJdFW30aboz4e5r1UHoaqiuiKINzm0SM2TK7Gd0D9GkZmyf/4guMEk8xMHoR1BwiPQ8zdI2dYwG0kcZ1SnvyXE6Qjouw/1GKS7BXFgzDwtLV25msvt6nGBPVcp/fuOYyJnDnMtb7fPVR98EPxroxHVrCvIMVO2Q0oOfin/bEvHveElKC97DNcxrgVRJl8nDxwSNyhmJ+6SBMV' &&
  '2kURVLqS6ycNuB9QlvVCM4v2PU/PP2D9VabrnkKrkkvltYJPS2ndAjQw08HwZe5YIKdNuM3um4Y71TelRkCsb9IUYwPY0BMNUcqcvrIIqaBAWlkfLh0bg+7IwOZuOVk3nuYbES5hdM3RgF6tUMmBoxpYmGaGLHtYA5ETMAaeTiuRNkQK5PNuwvUSM8BJLpJLjaOXDir5FhZsqwPrmS99' &&
  'R9kmI0TaMjUa/RiX1YRjQL1KMtod19UNjrZiw0ixSI8kunjtLFEZ1rcHkBnOR8lOkoMwPuuYUhpYMs+nSPvPHJFxqxXRqoqCWPPKDgWCmIkICOCZhvFkKGnLuOm/uqnH/3o3LGmOnGGYIHFaQ/aICWWSAgv6jSutfj7B+yY8Rl+v4CpjD3zHxa2v/kAT3xFB3qnloF5T8iXrX/Mzm8+X' &&
  'WRPR9IZtEXWur3rZTqL/8g/s1/kQw47/OSu8uvfjoqaIQONBQaGBdZaFTPf7CC0pEbLxomF3btG6zrjNDVpxFym2NL2W7pgKg5beQ46VAdH9a2g9fA88nOZnYGwjSfxsfw/u3eIpn64idwiTCsQ99rqpmtgiKjzGICVyXpjdIhMkYGRR04ztSwwcwLt1bGyZwXluqTQc+n/0aC7UYOwk' &&
  'zmOXnnPmAvKDqXtAed2kM2HKzWAwGzX1kmoyY/hRSwnYw850J5efooSUf6UcKuGoyxG6UDkuhxj6ZYuI8UiWx/aPZrupAj1eUy4ZofyuBzz9TldI6/BmO3pq2pzwBkBly6l1KqL9pQwWGkWd11tZNbKU7FlKoEKL9mC1b5WT8hQffP6Kpa3mppGZx7LVu/Wa4c/7Bccf/vZOHGItkq3a' &&
  'Vf+2529JYHNfDcLmUmILeD/r1y5GgsagV4tG7ay4ul240TZczTH8iinUi1GzvDl3GLPWTyBuG/jo56GN+3Ughjy/R9L+1iS/RqQ2ydpMQVSXzCEWlXtk6jIlNRbk1c3uP04EonjcUbEd3K5X7/QT2aeN/1N4etirKeq08jZHr5+KI/qqPqj6g56oJhYW07OAN///tv5Z01y00UtJGQVu' &&
  'YALTEnJJguMGIUIGINmLxAJBtp1P5duuhwXk4mKpJR0GzbCMk361oJGgWWTr5K54niy9L70ojIkZItc95ZLa8gCvBpTpBpiK4oYrzRgAbgBhEw6aMS7tP8uSvmJ2/OvFdGntsd6W2pzH5tjWw9UKTHyp5+8HpVbqaPuvNHj4MoYHsxq6AaBKHspC3GGKhhhVlXYwpOuhb85T1Z/g3mFj' &&
  'Bappfj7x6ubsesCgbU8SSqCPC7MVmX238xerj0y6VhYTxUbAqYaWZ6pqbyKiIfE1KRfll20YppO+txewQp6IZN3yklpIUmHOQFP5mlRBTWccWB433r3z+SDYegTp16gga0jzE3VJcoPyn807pyLxM0MAUmT5g6x6bLhr2NUlVRoXVAUKivFhDs21OXqebMJRsxGToZmRCcuDoYvaX0BW' &&
  'VCrkb3yM0A7ccUSOl3H0hunck4YhtUMvrpa0jWz6TYMml3eIdUF62XHBjhjBsmBxdZ3PyO393+H1dFR735uKaLF8Convnxg8H5nTMkgvX5bF67A7rfR528b/AI3Zpge6znjZpEeRefnrjh8TfeiZyG+ahQZAhvcD7FRQddMYTCDh1wqoy84nyt6GpFgo1yHvljRAhdkOAmfSzmTNzTIy' &&
  'CEu+a8rkIJxHJOxs1jY8AKawitRWfwbMGUe2Tt87+UpdNuk81vPKr7VqSMxUzQW1YaE6EMzIho4GAMUjdud0x89j9M6pm/V9NLHl3ybkQbXoTAXxQG9/CYMUQi77sSTelrJEf7u/Ise/Ilzi7CcBU5o5M89+f3Na2m+ut2i87lmgdP/rhiIokZV3wyZqek30VSd8IGoJz51oIkc7x2TL' &&
  'qKDWRmyZKVn2gUVQfBeRb0g0+HWgiHkaPjNz9tm3Ao/x2oPmLmeM2BkZCFRlhbW4wxVEgaHPdDQ24Pi2zOEB3cymUhvl99xnlqVF7QsFjfffJs6IqdG9WvcGuCaa4zaGeEDoya0xfN01VUwkxjDMBeLh/XQnQJOl9Z+NE2PAEPCe8/b+o4Pb3AlSWjdFagEeAKF+tNH3ejHqZGs+AEJ6' &&
  'eozwWJtO563lBPyRCqWJbIWn43ZtAaMEpzFZaHtf8eRs/oy01rDp2tB1d0j4aggqBvXe73gfkxut2hC09FR+Txnw3Xhac0KJWbT4aBe3s2t2f/3JpgxiD53Jqw4EpndjeZumiVzsk46puvuVzbDwGUox4XmXgo2vhb1uQxQK5kApoJeAuucTFCzZWFQrWRPIgQX2OTMoK/qJATLu+jR8' &&
  '70NA/7tx576qurdJGLm+ZslsrCaN/8CjaoC5BiQzUpiQp1CH8dJDXCsCxobNHA63W6T4uuUa6xK5feNNGB12gkNCousmRAh2bcPL7JKRlnWPzk1sTNy2bJESmVXnZrgkboIh/r8DsLDXZ13T7d0Fcb4OEVC5UJmU3Pz0gl6TAY/z55Zdw10oW02oWDYMSKABXehdWu8Y1mQeLQHv2Nns' &&
  'wHPP2KHSV6SoZvQOjWRPOiahk+pHlIM0m4i0psj/Tpim6rmvVWojL7dM+OG+j6NgAwtG+6XXiyTNKFJxuddFxWMHiOffMX35a4DRnDsLO7gyh6X9Cnp4VQA/NBtyjFlV3NFsrhzZkp8D7TQaSrY+YtkeXrd0gpshGm003Vi32zUDh8zvROB8kSchMhxXHQcUCeHIk6gXYYTS3ntQCVC+' &&
  '6Y52bvnv6cWbFUOZk+zCH4ZsAFLjgjrguE7f8F4eV3BFq41RRNjceloC1B3+RC4cplz9r0qEbI9JHzwUfeWijfNZTD6abrOVBvZPOCJ2VohH9avhQmKHDUZ+t8MPKomuvLbei/BQOv8rBIE1AK7eH5izVS8ZWgKT/tL9efe6okgmgqAQ8Oq7eNmtrVQTQN1mPW6uHdGbdfpK/86HyIB5' &&
  'ZZUnN1T3E7DJ0kRqZxQWM9CjjBN0ImcWa/UhGVtuaka+ryLChpIQPfW/+1BM9mwnM+9+j95v0ubZ79sVAN3VvoZGxIkPrqCeaIfCXrmal36tsWOrdimkkjw+fW0kRN8dkkswWJK0H7qMMTIS5NRCp6y68XyXNLPw6mowbuhI4DaNM3sc1eH1dZoyt/U3ALSQ8SjL0ZTsAsYtiBwm7bSV' &&
  '5hjO65rtbDBD49VlXlxqpRNwRzPpdJsPmTD7xLvnLbSA1a0YLxthFJWd2kOgZBogXQKKsaMiSeh/AD5D/mtmCN2++L3KB3qSijFvf7xr76a/Hj0nmeRRYXzQh+ujcoZm5/R/4NRsXDKbddNiLsGJwXNFga+B/fW6anZJwOKmco7OWbTG7Nd7Luv5XvyZkUjYailXhQl6lPE/CuZTA8Jk' &&
  '8ujr9hiIkC9K7B/TGIJzVDFyS40hY0wAQXDfkbpb5KUnTnKjpYy/ztFQAK746SNQFpRbOGZZEwffMsKO9zy+Gh+fZspv+gvIIozrckVIFMVSZaqmRU+KbqRBpioaQcS70++/zEkbDsIlTnk7aR9E6y8N21GuG44c23RwYkwwDKbMbABSyeToGzmvjyGrnk7kdlAWTD7GR/XUDTF+OATK' &&
  'lAs/sbArrHx/R2ETKMCXxX8at8GBK9NuhXYk0cJVfpUAkeJOCB7wX3/0SkYLtxBEzFuUAVnyxbA6lyxYOPyT88Ok8PjjMt3Y+oSIOkNOkumLzSSXDhjjS8jXHNxVz31ofm6vOafus80+s4POAilW5bkUgTZZRT3s36SMSkSmz7TlJGOqOAxleOpJNGmt75AlxJomWphvGJc+wpGbc1ET' &&
  'lfo5GsObxLVy4Jt98XGTWdMbqoyUWWbFShgXuPtbl5Iw2Wn5xrcmuCL3UTMau+O9B4xfjZ5n20cr+sfeoB8/6Y3j5WNMq6/GLz4q6eN4QgCHKhqEgmDe+rL/B+A8UayNeFOvfEPRIEQZTly9JJ18lVmN8dHw0AIhjnUL+DEk98fTUiPOa27ugX0r99L/5C39YeM/NpWfS1iQQjz8yQp6' &&
  'fco8JidDUrva0D2c65EueLTCGn83Wj97fqQXUDMIBbNEV79FUoGiH3M+e/ukxmL14H24XStqfi2pFTOZGR6MuwSQkyqGs7aa/vaIaw83ClnhKSRiTLqGNO2FCZ0TCfpxPkviO8LvvkCSY6SToSGhxXwHmKiI4xjy9G88A9ozajsjsqVswcQaRPlk+yMtPCaChuiJW8nXiONp6chsJsCX' &&
  'LkyShdEcaYuQJdUa2r1bNGD1dz/ClfR/qe7eNBnkdFBsA3QT5/6j7lPRFV/gwMMfvFv/0/JKbkS0lDdkGj884FiwI9JL3XYEnKvhC/rPw0csMQQQ+dhvcFXi9oWDQ2GihTUG418HD1TfN+paulbm7nwLqMkDx188ytM/Wk2Av2XUPOK50Bep9hmjpr4G36rmFs9pkXPpy3AwysrpPH7x' &&
  'wi15/XR/e2JObouXXzwKQ2tZ2celInWbrqI7QNRteAAVUQCPznmg9WyPln9Ze09HR57rmlEADuVyFaVEM5kRINufBMOat7Z+O8jwcxjBQiq7/+VlZt+BJ9IPSzj0ooVEWRjL99qKxY+a5Mf/ZtGAkVt1yWzJkiGWAiYxnNry00ULHg58BszJ/AvzMG9JWVKz8QfxH5i36rDqOYxZfNmz' &&
  'bIDX93qWSir8H9+8j1l54tPdoHZP/O7ah/CPUZZ4AkKHJqJsaSIPsqY2UHmtlRnogSr8UfgMLzlatEUMw0lgci+KoP+uqT0ih3XNxX7vj7c2XaPw6XkzKSodwNEkNlbIzVORtjdyfIaviQC2To+WcgKy6QL3bvBX9smsR5YCIMKzFF+zI0mL5Jy75K3kf/7VDioGRVoJtnb2vg0FAGdU' &&
  '2TMT8+S+6+frAMHXy2pCPjoat97d/flx17wYsq0MqFINCdlVgnP7v2Mr1PvkflvTrQTEnO6S51viTxVxfDtmp0bZ2lJuNcye51NdJPzLFjPMYFXWCJgaNvFxOQbj27yzt5H0lpXS38G+TlCuygpqZWkutjJf+rnXLVoCskJ7G95HTKkrzP15n6qOMK63Izfk/BIUmtrJaBvc+S07v3lg' &&
  'O7tsmGI/tMPbTbHsNMKG/QwrY//+JLGf6jIRKz9fCBxtI67mSJ9EbkS1dPwQk5VfDYwflKOdSRrwZ5UWPtOgcJ0UmYb3GTmnDXIQPzEq7R5uOESaO3+f1SqLt1BuyD4Hj5Nz84fvafCSU+JTUF6TcVz6w+RvXCzYD1TdQ0L+NyTsZXqQ76TQTxorK8TM49tXvw1A7fnuY8Czot34Mn/G' &&
  'NMO3ft1DfNt+wvkCNFZSbSedCvcxc9v8kjZFkdU6RXbid9c55vzHfJytSUiCro2Gi2KyhfIkFlZFZ3OUouohiHxai897s9svbb7VJQWiFbthbKjv0HwvvHc32yszQiZiHy9emWie+p0qVzjv6hqE4d0yUHvOR7HuzfzdW1P0wd+CcpyBcev+Mfn3I4LxfRlwt/XGxTU4wE6gb/rCQDRY' &&
  'EEKkPRz5cCZ4V5Xw0ykPDYFoLTzUJkBYWVfns1hIpYH+QSkMLCYhlwUn/pit/kzU7oypEEcig6+AZFb64hoP39CDR7y7wr3U3B+nDWFXSCyhSd7zX7NnvbQFE7G6JvMuArK3Garej2hoKWZ5SC/xqvhyf1x/E5AUNBUMPtJ8FNbM4frQJC2JxTGVoBPqI84I+28sGwM/w5XuPv0D3yhz' &&
  'BncdkXB4yiysdxh3b3DHecOzEtkWv8TXpZjLLR8C3Z2pdtj4B87RcUtMCU3/tnMvRBG3Ps/J8rv+7ABGUWUnNnVOYxp+imB9OvocO17UZLoJb24yg1fGI/upClDgGIaoBsG1kEj1rYk05hCOpty38KgtQEt6vc3I4Dsgt5NDaCzzNn02BAeOoEEc1OWB+0sB2lwfuMrU9XxolZTJ39kx' &&
  'TOc5vk6/jhVuz0DCTAf90f9uKKFFmvf5iIiqcnMsxpHMIt0DTHwNbCKbD5I0QoqnAomKMw/Qo6Al3KB2NIlBMIX3PR+oaWVoGKq0fZwumnSvGKU0iOz/2Zd/Meo+maiux44KBJMPfZuMxvFVJNU40kFNVDCCm7vh+I9tmidxSGLly1r0fxaB3NgUatMvIqj+fhkOKyNvnITwc7Wq7M4n' &&
  'nOtZR28hO/3V+/ZnNsx/tnO7jXyD8YxfscW41tK2oEC8rSjlNlZxft1MlgFEqe6bfZQOIxNkdTCq/hueCfkuANC+osOU7rUD7ZusE+LF36NjwQzGwsLyPB29xlaNvfBf2/CsvsZoXxVwP7+e/uw6E1fR0jPeSXU6oWecb2if+pMXvh+nag1VH5j40W41wioAHCGBQRtc5x4v+HAZ7ACf' &&
  'wvRci8T+AETuB/HCcM8ARO4G8Gkf8CO1XW6Qr6nvUAAAAASUVORK5CYII='.

  p_html_pagina =
'<!DOCTYPE HTML>' && cl_abap_char_utilities=>newline &&
'<HTML>' && cl_abap_char_utilities=>newline &&
'<HEAD>' && cl_abap_char_utilities=>newline &&
'<META NAME="escolha_processo" content="width=device-width, initial-scale=1">' && cl_abap_char_utilities=>newline &&

'<script type="text/javascript" src="https://platform.linkedin.com/badges/js/profile.js" async defer></script>' &&

'<style>' && cl_abap_char_utilities=>newline &&

'  body {' && cl_abap_char_utilities=>newline &&
'    font-family: Verdana;' && cl_abap_char_utilities=>newline &&
'    color: black;' && cl_abap_char_utilities=>newline &&
'    background-color: MintCream;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  .card {' && cl_abap_char_utilities=>newline &&
'    transition: transform 0.5s;' && cl_abap_char_utilities=>newline &&
'    width: 200px;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  .card:hover {' && cl_abap_char_utilities=>newline &&
'    transform: scale(1.5); ' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  a {' && cl_abap_char_utilities=>newline &&
'     text-decoration:none;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&

'  .container {' && cl_abap_char_utilities=>newline &&
'    color: black;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  .split {' && cl_abap_char_utilities=>newline &&
'    height: 80%;' && cl_abap_char_utilities=>newline &&
'    width: 50%;' && cl_abap_char_utilities=>newline &&
'    position: fixed;' && cl_abap_char_utilities=>newline &&
'    z-index: 4;' && cl_abap_char_utilities=>newline &&
'    top: 0;' && cl_abap_char_utilities=>newline &&
'    overflow-x: hidden;' && cl_abap_char_utilities=>newline &&
'    overflow-y: hidden;' && cl_abap_char_utilities=>newline &&
'    padding-top: 20px;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  .centered-bottom {' && cl_abap_char_utilities=>newline &&
'    position: absolute;' && cl_abap_char_utilities=>newline &&
'    bottom: 80px;' && cl_abap_char_utilities=>newline &&
'    left: 50%;' && cl_abap_char_utilities=>newline &&
'    transform: translate(-50%, -50%);' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  .left {' && cl_abap_char_utilities=>newline &&
'    left: 0;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  .right {' && cl_abap_char_utilities=>newline &&
'    right: 0;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  .centered {' && cl_abap_char_utilities=>newline &&
'    position: absolute;' && cl_abap_char_utilities=>newline &&
'    top: 50%;' && cl_abap_char_utilities=>newline &&
'    left: 50%;' && cl_abap_char_utilities=>newline &&
'    transform: translate(-50%, -50%);' && cl_abap_char_utilities=>newline &&
'    text-align: center;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  .centered img {' && cl_abap_char_utilities=>newline &&
'    width: 150px;' && cl_abap_char_utilities=>newline &&
'    border-radius: 45%;' && cl_abap_char_utilities=>newline &&
'  }' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'</style>' && cl_abap_char_utilities=>newline &&
'</head>' && cl_abap_char_utilities=>newline &&
'<body>' && cl_abap_char_utilities=>newline &&
' ' && cl_abap_char_utilities=>newline &&

'  <div class="split left">' && cl_abap_char_utilities=>newline &&
'    <div class="centered">' && cl_abap_char_utilities=>newline &&
'      <a href="grao"><A HREF=SAPEVENT:' && cl_myevent_handler=>st_action_grao && '>' && cl_abap_char_utilities=>newline &&
'      <div class="card">' && cl_abap_char_utilities=>newline &&
'        <img src="' && imagem_granel && '" style="width:100%">' && cl_abap_char_utilities=>newline &&
'        <div class="container">' && cl_abap_char_utilities=>newline &&
'          <h4>(GRANEL) Nova Carga</h4> ' && cl_abap_char_utilities=>newline &&
'        </div>' && cl_abap_char_utilities=>newline &&
'      </div>' && cl_abap_char_utilities=>newline &&
'      </A></a>' && cl_abap_char_utilities=>newline &&
'    </div>' && cl_abap_char_utilities=>newline &&
'  </div>' && cl_abap_char_utilities=>newline &&
'' && cl_abap_char_utilities=>newline &&

'  <div class="split right">' && cl_abap_char_utilities=>newline &&
'    <div class="centered">' && cl_abap_char_utilities=>newline &&
'      <a href="algodao"><A HREF=SAPEVENT:' && cl_myevent_handler=>st_action_algo && '>' && cl_abap_char_utilities=>newline &&
'      <div class="card">' && cl_abap_char_utilities=>newline &&
'        <img src="' && image_algodao && '" style="width:100%">' && cl_abap_char_utilities=>newline &&
'        <div class="container">' && cl_abap_char_utilities=>newline &&
'          <h4>(ALGODÃO) Nova Carga</h4> ' && cl_abap_char_utilities=>newline &&
'        </div>' && cl_abap_char_utilities=>newline &&
'      </div>' && cl_abap_char_utilities=>newline &&
'      </A></a>' && cl_abap_char_utilities=>newline &&
'    </div>' && cl_abap_char_utilities=>newline &&
'  </div>' && cl_abap_char_utilities=>newline &&
'     ' && cl_abap_char_utilities=>newline &&

'  <div class="centered-bottom container">' && cl_abap_char_utilities=>newline &&
'     <img src="' && imagem_amaggi && '" style="width:100%">' && cl_abap_char_utilities=>newline &&
"'      <div style="width:100%" class="LI-profile-badge"  data-version="v1" data-size="medium" data-locale="pt_BR" data-type="horizontal" ' &&
"'      data-theme="light" data-vanity="marcus-bárbara-8379ab31"><a class="LI-simple-link"  ' &&
"' href=https://br.linkedin.com/in/marcus-b%C3%A1rbara-8379ab31?trk=profile-badge>Marcus Bárbara</a></div> ' &&
'  </div>     ' && cl_abap_char_utilities=>newline &&
'    ' && cl_abap_char_utilities=>newline &&
'</body>' && cl_abap_char_utilities=>newline &&
'</html>'.

endform.

*&---------------------------------------------------------------------*
*&      Form  GET_HTML_GRAFICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_HTML_PAGINA2  text
*----------------------------------------------------------------------*
form get_html_grafico
  changing p_html_pagina2 type string.

  data(ini_mes) = sy-datum.
  "INI_MES = INI_MES(06) && '01'.

  select lf~lifnr as codigo,
         lf~name1 as nome,
         cg~id_produto,
         sum( nt~nm_peso_liquido ) as peso_liquido
    into table @data(it_data)
    from zsdt0001cg as cg
   inner join zsdt0001nt as nt on nt~id_carga eq cg~id_carga
   inner join lfa1 as lf on lf~lifnr eq nt~id_fornecedor
   where cg~id_bukrs      eq @pempre
     and cg~id_branch     eq @pfilia
     and cg~nr_safra      eq @psafra
     and cg~dt_abertura   eq @ini_mes
     and cg~dt_fechamento eq @ini_mes
     and cg~tp_status   eq @zif_carga=>st_status_conferido
   group by lf~lifnr, lf~name1, cg~id_produto.

  data: lc_dados type string.

*  APPEND VALUE #( CODIGO = '1' NOME = 'MARCUS' ID_PRODUTO = 1 PESO_LIQUIDO = 500 ) TO IT_DATA.
*  APPEND VALUE #( CODIGO = '2' NOME = 'HELLEN' ID_PRODUTO = 2 PESO_LIQUIDO = 200 ) TO IT_DATA.
*  APPEND VALUE #( CODIGO = '2' NOME = 'HELLEN' ID_PRODUTO = 1 PESO_LIQUIDO = 300 ) TO IT_DATA.
*  APPEND VALUE #( CODIGO = '1' NOME = 'MARCUS' ID_PRODUTO = 2 PESO_LIQUIDO = 800 ) TO IT_DATA.
*  APPEND VALUE #( CODIGO = '3' NOME = 'MELISSA' ID_PRODUTO = 2 PESO_LIQUIDO = 800 ) TO IT_DATA.

  if it_data[] is not initial.
    select matnr, maktx into table @data(it_makt)
      from makt
       for all entries in @it_data
     where matnr eq @it_data-id_produto
       and spras eq @sy-langu.

*    APPEND VALUE #( MATNR = 1 MAKTX = 'SOJA' ) TO IT_MAKT.
*    APPEND VALUE #( MATNR = 2 MAKTX = 'MILHO' ) TO IT_MAKT.

    sort it_makt by maktx ascending.
  endif.

  data(it_data_produtor) = it_data[].
  sort it_data_produtor by codigo.
  delete adjacent duplicates from it_data_produtor comparing codigo.
  sort it_data_produtor by nome ascending.

  data: lc_aux  type string,
        lc_soma type p length 15,
        lc_qtd  type p length 15.

  loop at it_data_produtor into data(wa_data_produtor).
    if lc_dados is not initial.
      lc_dados = lc_dados && ', '.
    endif.
    lc_dados = lc_dados && '[''' && zcl_string=>initialcap( conv #( wa_data_produtor-nome ) ) && ''','.

    clear: lc_aux.
    loop at it_makt into data(wa_makt).
      if lc_aux is not initial.
        lc_aux = lc_aux && ','.
      endif.
      lc_soma = 0.
      loop at it_data into data(wa_data) where id_produto eq wa_makt-matnr and codigo eq wa_data_produtor-codigo.
        move wa_data-peso_liquido to lc_qtd.
        add lc_qtd to lc_soma.
      endloop.
      lc_aux = lc_aux && lc_soma.
    endloop.
    lc_dados = lc_dados && lc_aux && ']'.
  endloop.

*  LOOP AT IT_DATA INTO DATA(WA_DATA).
*    IF LC_DADOS IS NOT INITIAL.
*      LC_DADOS = ',' && LC_DADOS.
*    ENDIF.
*    LC_DADOS = LC_DADOS && '[''' && WA_DATA-NOME && ''', ''' && WA_DATA-PESO_LIQUIDO && ''']'.
*  ENDLOOP.

  if lc_dados is not initial.

    clear: lc_aux.
    loop at it_makt into wa_makt.
      if lc_aux is not initial.
        lc_aux = lc_aux && ', '.
      endif.
      lc_aux = lc_aux && '''' && zcl_string=>initialcap( conv #( wa_makt-maktx ) ) && ''''.
    endloop.

    lc_dados = '[ ''Produtor'', ' && lc_aux && ' ]' && ',' && lc_dados.

    p_html_pagina2 =
    '<!DOCTYPE html>' && cl_abap_char_utilities=>newline &&
    '<html>' && cl_abap_char_utilities=>newline &&
    '<head>' && cl_abap_char_utilities=>newline &&
    '<style>' && cl_abap_char_utilities=>newline &&

    '  body {' && cl_abap_char_utilities=>newline &&
    '    font-family: Verdana;' && cl_abap_char_utilities=>newline &&
    '    color: black;' && cl_abap_char_utilities=>newline &&
    '    background-color: white;' && cl_abap_char_utilities=>newline &&
    '    overflow: hidden; ' && cl_abap_char_utilities=>newline &&
    '  }' && cl_abap_char_utilities=>newline &&
    '' && cl_abap_char_utilities=>newline &&

    '</style>' && cl_abap_char_utilities=>newline &&
    '<script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>' && cl_abap_char_utilities=>newline &&
    '<script type="text/javascript">' && cl_abap_char_utilities=>newline &&
    'google.charts.load(''current'', {''packages'':[''corechart'']});' && cl_abap_char_utilities=>newline &&
    'google.charts.setOnLoadCallback(drawChart);' && cl_abap_char_utilities=>newline &&
    'function drawChart() {' && cl_abap_char_utilities=>newline &&
    '  var data = google.visualization.arrayToDataTable([' && lc_dados && ']);' && cl_abap_char_utilities=>newline &&
    '  var options = {''title'':''Total Peso Líquido por Produtor e Produto'', width: "100%", height: 600, colors: [''#66CDAA'', ''#1E90FF'', ''#FFDAB9'', ''#8FBC8F'', ''#008B8B''], is3D: true };' && cl_abap_char_utilities=>newline &&
    '  var chart = new google.visualization.ColumnChart(document.getElementById(''piechart''));' && cl_abap_char_utilities=>newline &&
    '  chart.draw(data, options);' && cl_abap_char_utilities=>newline &&
    '}' && cl_abap_char_utilities=>newline &&
    '</script>' && cl_abap_char_utilities=>newline &&
    '</head>' && cl_abap_char_utilities=>newline &&
    '<body>' && cl_abap_char_utilities=>newline &&
    '<div id="piechart"></div>' && cl_abap_char_utilities=>newline &&
    '</body>' && cl_abap_char_utilities=>newline &&
    '</html>'.

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  IMPORT_MEMORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LC_ID_CARGA  text
*----------------------------------------------------------------------*
form import_memory changing p_lc_id_carga type zde_id_carga.

*  DATA: IT_DADOS TYPE TABLE OF ABAPLIST.
*
*  CALL FUNCTION 'LIST_FROM_MEMORY'
*    TABLES
*      LISTOBJECT = IT_DADOS
*    EXCEPTIONS
*      NOT_FOUND  = 1
*      OTHERS     = 2.
*
*  CALL FUNCTION 'LIST_FREE_MEMORY'.

  get parameter id 'ZIDCARGA' field p_lc_id_carga.

  if p_lc_id_carga is initial.
    import p_lc_id_carga from memory id 'IDCARGA' .

    if p_lc_id_carga is initial.
      p_lc_id_carga = zcl_util=>at_param.
    endif.
  endif.

endform.
