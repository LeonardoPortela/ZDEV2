*======================================================================*
* PROJETO            : SAP Ninjas                                      *
* PROGRAMA           : ZFIS0002                                        *
* TRANSACAO          : ZFIS0001                                        *
* DESCRICAO          : Relatório Crédito Presumido Sobre Vendas Fabr.  *
*======================================================================*
* AUTOR              : Ronaldo Freitas                                 *
* Solicitante        : Adelson Silva                                   *
* DATA               : 15.08.2024                                      *
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
*&---------------------------------------------------------------------*
*&  Include           ZFIS0002_TOP
*&---------------------------------------------------------------------*
report zfis0002.
*======================================================================*
* Declarações
*======================================================================*

include <icon>.

tables: zsdt0245, vbrk.

types: begin of ty_vbrkp,
         fkart  type vbrk-fkart,
         vbeln  type vbrk-vbeln,
         kunag  type vbrk-kunag,
         namek  type kna1-kunnr,
         vkorg  type vbrk-vkorg,
         fkdat  type vbrk-fkdat,
         matnr  type vbrp-matnr,
         werks  type vbrp-werks,
         name1  type t001w-name1,
         arktx  type vbrp-arktx,
         netwr  type vbrp-netwr,
         objk   type awkey,
         docnum type j_1bnflin-docnum,
       end of ty_vbrkp.

types:
  begin of ty_saida,
    status            type char4,
    vkorg             type vbrk-vkorg,
    kunag             type vbrk-kunag,
    name1             type kna1-name1,
    werks             type vbrp-werks,
    fname1            type t001w-name1,
    matnr             type vbrp-matnr,
    arktx             type vbrp-arktx, " Desc. Material
    vbeln             type vbrk-vbeln, " Doc.Faturamento    =  VBRK-VBELN
    fkart             type vbrk-fkart,
    fkdat             type char10, " Data Faturamento   =  VBRK-FKDAT
    netwr             type zde_vlr15_02, " Valor Venda R$     =  VBRP-NETWR
    apis              type zperc_sd, " Aliq.PIS           =  variável XPERC_PIS
    apisv             type j_1btxrate4dec,
    vpis              type zde_vlr15_02, " Valor PIS          =  Variável XPIS
    acofins           type vbrp-netwr, " Aliq.Cofins        =  variável XPERC_COFINS
    vcofins           type zde_vlr15_02, " Valor COFINS       =  Variável XCOFINS
    belnr             type zib_contabil_chv-belnr, " Doc. Contabil      =  ZIB_CONTABIL_CHV-BELNR
    obj_key           type zib_contabil_chv-obj_key,
    act               type char1,
    docnum            type j_1bnflin-docnum,
    gjahr             type gjahr,
    stblg             type stblg,
    status_doc_fiscal type c length 20,      "Status documento fiscal
    check_canc        type char01,
  end of ty_saida.

data: begin of it_tab,
        mes(2) type n,
        ano(4) type n,
      end of it_tab.

types: begin of ty_material,
         matnr type mara-matnr,
         matkl type mara-matkl,
       end of ty_material.

data:
  it_saida    type table of ty_saida,
  it_material type table of ty_material,
  c_alv_tm    type ref to cl_alv_grid_toolbar_manager,
  wa_saida    type ty_saida.

data: gs_variant       type disvariant,
      wa_layout        type lvc_s_layo,
      lv_erro,
      lva_data(22)     type          c,
      gob_gui_alv_grid type ref to   cl_gui_alv_grid,
      wa_stable        type lvc_s_stbl value 'XX',
      _rows            type lvc_t_row,
      t_rows           type lvc_t_row,
      w_rows           type lvc_s_row,
      git_fcat_pend    type          lvc_t_fcat,
      message          type itex132.

data:
  lt_vbrkp type table of ty_vbrkp,
  wa_vbrkp type ty_vbrkp.

*======================================================================*
*& TELA DE SELEÇÃO
*======================================================================*
selection-screen: begin of block b1 with frame title text-001.
  select-options:
                  s_werks  for  zsdt0245-werks, " OBLIGATORY NO INTERVALS NO-EXTENSION,
                  s_vbeln  for  vbrk-vbeln,
                  s_matnr  for  zsdt0245-matnr,
                  s_fkart  for  vbrk-fkart,
                  s_fkdat  for  vbrk-fkdat obligatory.
selection-screen: end of block b1.

*======================================================================*
*** Lógica Principal
**======================================================================*
try.
*======================================================================*
*** Busca informações relevantes para o processo
*======================================================================*
    perform fm_selecao.

    if it_saida[] is not initial.
      perform fm_exibirdados.
    endif.

  catch cx_root.
    message s836(sd) with 'Erro no processamento!' display like 'E'.
    exit.
endtry.

*======================================================================*
*** Fim Lógica Principal
*======================================================================*

*======================================================================*
* classes / implementacoes
*======================================================================*

class lcl_event_receiver definition.

  public section.
    methods:
      constructor importing io_alv_grid  type ref to cl_gui_alv_grid,
      on_toolbar  for event toolbar               of cl_gui_alv_grid importing e_object e_interactive sender,
      on_handle   for event user_command          of cl_gui_alv_grid importing e_ucomm,
      zm_handle_hotspot_report
        for event hotspot_click of cl_gui_alv_grid
        importing e_row_id e_column_id es_row_no.

endclass.

data: o_event  type ref to lcl_event_receiver.

class lcl_event_receiver implementation.

  method constructor.
    create object c_alv_tm
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.


  method: zm_handle_hotspot_report.
    perform user_command_0100 using e_row_id e_column_id es_row_no.
  endmethod.

  method on_toolbar.
    append value #( icon = '@39@' function  = 'GERAR'   quickinfo = 'Gerar Contabilização' text = 'Gerar Contabilização' butn_type = 0 ) to e_object->mt_toolbar.
    append value #( icon = '@2W@' function  = 'ESTORNAR'   quickinfo = 'Estornar Contabilização' text = 'Estornar Contabilização' butn_type = 0 ) to e_object->mt_toolbar.
    append value #( icon = '@42@' function  = 'ATUAL'   quickinfo = 'Atualizar' text = 'Atualizar' butn_type = 0 ) to e_object->mt_toolbar.
  endmethod.

  method on_handle.

    types: begin of gs_ascii,
             line type char140,
           end of gs_ascii.

    data:
      lt_auart   type table of zcl_cont_credito=>ty_auart,
      lt_werks   type table of zcl_cont_credito=>ty_werks,
      lt_matnr   type table of zcl_cont_credito=>ty_matnr,
      lt_vbeln   type table of zcl_cont_credito=>ty_vbeln,
      lt_matkl   type table of zcl_cont_credito=>ty_matkl,
      lt_vkorg   type table of zcl_cont_credito=>ty_vkorg,
      ls_auart   type zcl_cont_credito=>ty_auart,
      ls_werks   type zcl_cont_credito=>ty_werks,
      ls_matnr   type zcl_cont_credito=>ty_matnr,
      ls_vbeln   type zcl_cont_credito=>ty_vbeln,
      ls_vkorg   type zcl_cont_credito=>ty_vkorg,
      ls_matkl   type zcl_cont_credito=>ty_matkl,
      lv_message type string.

    data: t_listasci type standard table of gs_ascii.

    types begin of ty_normt.
    types tipo type normt.
    types end of ty_normt.

    types: begin of ty_erro,
             obj_key type awkey,
             message type bapi_msg,
           end of ty_erro.

    data: lit_zib_err type table of ty_erro,
          wa_zib_err  type ty_erro.

    data: _tipo  type table of ty_normt,
          status type c.

    data list_tab type table of abaplist.

    call method gob_gui_alv_grid->get_selected_rows
      importing
        et_index_rows = t_rows.

    _rows[] = t_rows[].

    case e_ucomm.
      when 'ESTORNAR'.
        data: vg_stblg type stblg.

        loop at _rows[] assigning field-symbol(<fs_rows>).
          loop at it_saida[] assigning field-symbol(<fs_saida>) from <fs_rows>-index.



            if <fs_saida>-belnr is not initial.
              select single vkorg from t001w into @data(vg_bukrs)
                where werks eq @<fs_saida>-werks.
              clear: vg_stblg.
              call function 'Z_ESTORNO_DOCNUM_FB08'
                exporting
                  i_BELNR = <fs_saida>-belnr
                  i_bukrs = vg_bukrs
                  i_gjahr = <fs_saida>-gjahr
                  i_stgrd = '01'
                importing
                  e_stblg = <fs_saida>-stblg.
              clear: vg_bukrs.
            endif.

            if <fs_saida>-stblg is not initial.
              select single * from zfit0229 into @data(wa_zfit0229)
              where vbeln eq @<fs_saida>-vbeln
              and gjahr eq @<fs_saida>-gjahr
              and bukrs eq @<fs_saida>-vkorg.
              if sy-subrc eq 0.
                wa_zfit0229-obj_key = <fs_saida>-obj_key.
                modify zfit0229 from wa_zfit0229.
                commit work.
              else.
                wa_zfit0229-vbeln = <fs_saida>-vbeln.
                wa_zfit0229-bukrs = <fs_saida>-vkorg.
                wa_zfit0229-gjahr = <fs_saida>-gjahr.
                wa_zfit0229-obj_key = <fs_saida>-obj_key.
                wa_zfit0229-obj_key_ref = <fs_saida>-obj_key.
                wa_zfit0229-us_criacao = sy-uname.
                wa_zfit0229-dt_criacao = sy-datum.
                wa_zfit0229-hr_criacao = sy-uzeit.
                modify zfit0229 from wa_zfit0229.
                commit work.
              endif.
            endif.
          endloop.
        endloop.
      when 'GERAR'.

        if _rows[] is not initial.

          if it_saida[] is not initial.
            select * from zib_contabil into table @data(it_contabil_f)
              for all entries in @it_saida where obj_key eq @it_saida-obj_key.
            if sy-subrc is initial.
              sort it_contabil_f by obj_key.
            endif.
          endif.

          data(go) = new zcl_cont_credito( ).

          loop at _rows[] assigning <fs_rows>.
            loop at it_saida[] assigning <fs_saida> from <fs_rows>-index.

              if <fs_saida>-status eq '@5D@' or <fs_saida>-status eq '@F1@'.

                ls_auart = value #( sign = zcl_cont_credito=>lc_i option = zcl_cont_credito=>lc_eq low = space high = space ).
                ls_matnr = ls_auart.
                ls_werks = ls_auart.
                ls_vbeln = ls_auart.


                ls_auart-low = <fs_saida>-fkart.
                append ls_auart to lt_auart.
                ls_matnr-low = <fs_saida>-matnr.
                append ls_matnr to lt_matnr.
                ls_werks-low = <fs_saida>-werks.
                append ls_werks to lt_werks.
                ls_vbeln-low = <fs_saida>-vbeln.
                append ls_vbeln to lt_vbeln.

                if <fs_saida>-matnr is not initial.
                  select single matkl from mara into @data(va_matkl) where matnr eq @<fs_saida>-matnr.
                  if sy-subrc eq 0.
                    lt_matkl = value #( ( sign = 'I' option = 'EQ' low = va_matkl ) ).
                  endif.
                endif.


                go->set( lt_werks = lt_werks[]
                         lt_matnr = lt_matnr[]
                         lt_auart = lt_auart[]
                         lt_vbeln = lt_vbeln[]
                         lt_matkl = lt_matkl[] ).

*                go->at_obj_key = <fs_saida>-obj_key.

                go->process( importing lt_message = lv_message ).

*                <fs_saida>-obj_key = go->at_obj_key.
*                wa_zib_err-obj_key = go->at_obj_key.
                wa_zib_err-message = lv_message.
                append wa_zib_err to lit_zib_err.
                clear wa_zib_err.
              endif.
              exit.
            endloop.
          endloop.

          " Erro...
          if lit_zib_err[] is not initial.
            cl_demo_output=>new(
              )->begin_section( `ZIB_CONTABIL:`
              )->write_text( |Erro(s) encontrado(s) para: \n|
              )->write_data( lit_zib_err[]
              )->end_section(
              )->display( ).
          endif.

        else.
          message |Selecionar ao menos uma linha!| type 'S' display like 'E'.
          exit.
        endif.

        perform fm_selecao.

        if it_saida[] is not initial.
          perform fm_exibirdadosv2.
        endif.

      when 'ATUAL'.

        perform fm_selecao.

        if it_saida[] is not initial.
          perform fm_exibirdadosv2.
        endif.

    endcase.

  endmethod.

endclass.
