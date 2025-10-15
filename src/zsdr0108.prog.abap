*&---------------------------------------------------------------------*
*& Report  ZSDR0108
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zsdr0108.

tables: zlest0061, zsdt0225.

types: begin of ty_saida_01,
         id_frete_aqua  type zlest0061-id_frete_aqua,
         emite_nfs      type zlest0061-emite_nfs,
         dt_fatura      type zlest0061-dt_fatura,
         nr_viagem      type zlest0061-nr_viagem,
         nome_emb       type zlest0061-nome_emb,
         cod_material   type zlest0061-cod_material,
*         matkl          TYPE zsdt0226-matkl,
         tp_class       type zlest0061-tp_class,
         maktx          type makt-maktx,
         nr_dco         type zlest0061-nr_dco,
         safra          type zlest0061-safra,
         peso_vinculado type zlest0061-peso_vinculado,
         cl_codigo      type zlest0061-cl_codigo,
         bukrs          type zlest0061-bukrs,
         werks          type zlest0061-werks,
         ano_viagem     type zlest0061-ano_viagem,
         embarcacao     type zlest0061-embarcacao,
         dt_movimento   type zlest0061-dt_movimento,
         rm_codigo      type zlest0061-rm_codigo,
         auart          type zlest0061-auart,
         id_seq         type zlest0061-id_seq,
         vkorg          type zlest0061-vkorg,
         vtweg          type zlest0061-vtweg,
         spart          type zlest0061-spart,
         netpr          type zlest0061-netpr,
         zterm          type zlest0061-zterm,
         matnr_ov       type zlest0061-matnr_ov,
         po_embarque    type zlest0056-po_embarque,
         po_destino     type zlest0056-po_destino,
         centro_fat     type zlest0061-centro_fat,
         operacao       type zlest0061-operacao,
         color(4)       type c,
       end of ty_saida_01.


types: begin of ty_saida_02,
         cl_codigo      type zlest0061-cl_codigo,
         safra          type zlest0061-safra,
         cod_material   type zlest0061-cod_material,
         tp_class       type zlest0061-tp_class,
*         matkl          TYPE zsdt0226-matkl,
         nr_dco         type zlest0061-nr_dco,
         operacao       type zlest0061-operacao,
         peso_vinculado type zlest0061-peso_vinculado,
         vkorg          type zlest0061-vkorg,
         vtweg          type zlest0061-vtweg,
         spart          type zlest0061-spart,
         netpr          type zlest0061-netpr,
         zterm          type zlest0061-zterm,
         matnr_ov       type zlest0061-matnr_ov,
         po_embarque    type zlest0056-po_embarque,
         po_destino     type zlest0056-po_destino,
         del            type c,
       end of   ty_saida_02.

types: begin of ty_saida_03,
         id_seq         type zsdt0225-id_seq,
         cl_codigo      type zsdt0225-cl_codigo,
         safra          type zsdt0225-safra,
         cod_material   type zsdt0225-cod_material,
         tp_class       type zsdt0225-tp_class,
*         matkl          TYPE zsdt0226-matkl,
         nr_dco         type zsdt0225-nr_dco,
         peso_vinculado type zsdt0225-peso_vinculado,
         dt_fatura      type zsdt0225-dt_fatura,
         auart          type zsdt0225-auart,
         waerk          type zsdt0225-waerk,
         tax_dolar      type zsdt0225-tax_dolar,
         vlr_usd        type zsdt0225-vlr_usd,
         vlr_brl        type zsdt0225-vlr_brl,
         nr_ov          type zsdt0225-nr_ov,
         waerk_fatura   type zsdt0225-waerk_fatura,
         fatura         type zsdt0225-fatura,
         docnum         type zsdt0225-docnum,
         bukrs          type zsdt0225-bukrs,
         werks          type zsdt0225-werks,
         ano_viagem     type zsdt0225-ano_viagem,
         vkorg          type zlest0061-vkorg,
         vtweg          type zlest0061-vtweg,
         spart          type zlest0061-spart,
         netpr          type zlest0061-netpr,
         zterm          type zlest0061-zterm,
         matnr_ov       type zlest0061-matnr_ov,
         po_embarque    type zlest0056-po_embarque,
         po_destino     type zlest0056-po_destino,
         kunnr          type zlest0055-kunnr,
         operacao       type zlest0061-operacao,
         celltab        type lvc_t_styl,
       end of    ty_saida_03.

types: begin of ty_head,
         bukrs type t001-bukrs,
         butxt type t001-butxt,
         werks type t001w-werks,
         name1 type t001w-name1,
       end of ty_head.


data: begin of it_partner occurs 0.
        include structure bapiparnr.
data: end of it_partner.

data: begin of it_return occurs 0.
        include structure bapiret2.
data: end of it_return.

data: begin of it_itemdata occurs 0.
        include structure bapisditm.
data: end of it_itemdata.

data: begin of it_condition occurs 0.
        include structure bapicond .
data: end of it_condition.

data: it_items_inx    type table of bapisditmx with header line,
      wl_items_inx    type bapisditmx,
      tl_schedules_in type table of bapischdl  with header line,
      wl_schedules_in type bapischdl,
      tl_vbuv         type table of vbuv with header line,
      wl_vbuv         type vbuv,
      wl_fieldname    type rmdi_name,
      wl_text         type rmdi_ddtxt,
      tl_bapiparex    type table of bapiparex with header line,
      wl_bape_vbak    type bape_vbak,
      wl_bape_vbakx   type bape_vbakx.

data: tg_msg_ret type table of zfiwrs0002 with header line,
      wl_msg_ret type zfiwrs0002.

data: wl_header_in   type bapisdhd1,
      wl_header_inx2 type bapisdh1x,
      wl_header_inx  type bapisdhd1x,
      wl_return      type bapiret2.

data: it_saida_01  type table of ty_saida_01,
      wa_saida_01  type ty_saida_01,
      it_saida_02  type table of ty_saida_02,
      wa_saida_02  type  ty_saida_02,
      it_saida_03  type table of ty_saida_03,
      wa_saida_03  type ty_saida_03,
      it_zlest0061 type table of zlest0061,
      wa_zlest0061 type zlest0061,
      it_zlest0056 type table of zlest0056,
      wa_zlest0056 type  zlest0056,
      it_zsdt0225  type table of zsdt0225,
      wa_zsdt0225  type  zsdt0225,
      wa_zsdt0226  type  zsdt0226,
      it_t0225     type table of zsdt0225,
      wa_t0225     type  zsdt0225,
      it_makt      type table of makt,
      it_mara      type table of mara,
      wa_makt      type makt,
      it_kna1      type table of kna1,
      wa_kna1      type kna1,
      it_jbbranch  type table of j_1bbranch,
      wa_jbbranch  type j_1bbranch,
      it_zsdt0226  type table of zsdt0226,
      wa_zlest0055 type zlest0055,
      wa_mara      type mara,
      wa_head      type ty_head,
      wa_head_03   type ty_head.


data: g_grid_01               type ref to cl_gui_alv_grid,
      g_custom_container_01   type ref to cl_gui_custom_container,
      c_alv_toolbarmanager_01 type ref to cl_alv_grid_toolbar_manager,
      tl_function_01          type ui_functions,
      wl_function_01          like tl_function_01 with header line,
      it_fcat_01              type table of lvc_s_fcat,
      wl_fcat_01              type lvc_s_fcat,
      wa_layout_01            type lvc_s_layo.

data: g_grid_02               type ref to cl_gui_alv_grid,
      g_custom_container_02   type ref to cl_gui_custom_container,
      c_alv_toolbarmanager_02 type ref to cl_alv_grid_toolbar_manager,
      tl_function_02          type ui_functions,
      wl_function_02          like tl_function_02 with header line,
      it_fcat_02              type table of lvc_s_fcat,
      wl_fcat_02              type lvc_s_fcat,
      wa_layout_02            type lvc_s_layo,
      ty_toolbar_02           type stb_button.

data: g_grid_03               type ref to cl_gui_alv_grid,
      g_custom_container_03   type ref to cl_gui_custom_container,
      c_alv_toolbarmanager_03 type ref to cl_alv_grid_toolbar_manager,
      tl_function_03          type ui_functions,
      wl_function_03          like tl_function_03 with header line,
      it_fcat_03              type table of lvc_s_fcat,
      wl_fcat_03              type lvc_s_fcat,
      gt_layout_03            type lvc_s_layo,
      gt_estilo_03            type lvc_t_styl,
      ty_toolbar_03           type stb_button,
      wa_variant              type disvariant.

data: wa_stable    type lvc_s_stbl value 'XX'.

data: obj_zcl_util_sd type ref to zcl_util_sd, "Classe de Utilitário SD
      obj_zcl_util    type ref to zcl_util. "Classe para Utilidade Geral.

data: tg_selectedrow type lvc_t_row,
      wg_selectedrow type lvc_s_row.

data: tl_bdc type table of bdcdata,
      wl_bdc type bdcdata.

data: tela(4)   type c value '0101'.
data: tela02(4) type c value '0103'.

data: vbukrs      type t001-bukrs,
      vwerks      type t001w-werks,
      vbukrs3     type t001-bukrs,
      vwerks3     type t001w-werks,
      vmatnr      type matnr,
      vmatnr18    type matnr18,
      v_a_faturar type c,
      v_faturado  type c.

data: p_bukrs       type range of j_1bbranch-bukrs,
      w_bukrs       like line of p_bukrs,
      v_vlr_brl(25) type c.

data: it_value    like rgsb4 occurs 0 with header line,
      r_dt_fatura type range of zlest0061-dt_fatura with header line.

data: it_value_01 like rgsb4 occurs 0 with header line,
      r_vlr_brl   type range of zsdt0225-vlr_brl with header line.


selection-screen begin of screen 0101 as subscreen.
  selection-screen begin of block b1.
    select-options: p_ano     for zlest0061-ano_viagem no intervals no-extension,
                    p_viagem  for zlest0061-nr_viagem.

  selection-screen end of block b1.
selection-screen end of screen 0101.


selection-screen begin of screen 0103 as subscreen.
  selection-screen begin of block b2.
    select-options: s_ano     for zlest0061-ano_viagem no intervals no-extension,
                    s_dt_fat  for zsdt0225-dt_fatura.
  selection-screen end of block b2.
selection-screen end of screen 0103 .

start-of-selection.

  perform z_get_set.

  call screen 0100.

class lcl_event_receiver definition.
  public section.
    methods: zm_handle_click for event double_click of cl_gui_alv_grid
      importing e_column e_row es_row_no.
endclass.

class lcl_event_receiver implementation.
  method zm_handle_click.

    check e_row-rowtype is initial.
    perform z_handler_double_click using e_row-index e_column es_row_no.

  endmethod.
endclass.

class lcl_alv_toolbar definition.
  public section.

    methods: constructor
      importing io_alv_grid type ref to cl_gui_alv_grid,

      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.

class lcl_alv_toolbar implementation.
  method constructor.
    create object c_alv_toolbarmanager_02
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.

  method on_toolbar.
    clear ty_toolbar_02.

    ty_toolbar_02-icon        =  icon_delete_row.
    ty_toolbar_02-function    =  'EXCLUIR'.
    ty_toolbar_02-butn_type   =  0.
    append ty_toolbar_02 to e_object->mt_toolbar.

    ty_toolbar_02-icon        =  icon_system_save.
    ty_toolbar_02-function    =  'SALVAR'.
    ty_toolbar_02-butn_type   =  0.
    append ty_toolbar_02 to e_object->mt_toolbar.

    call method c_alv_toolbarmanager_02->reorganize
      exporting
        io_alv_toolbar = e_object.

  endmethod.

  method handle_user_command.

    case e_ucomm.
      when 'EXCLUIR'.
        call method g_grid_02->get_selected_rows
          importing
            et_index_rows = tg_selectedrow.

        if tg_selectedrow is initial.
          message 'Favor selecione uma linha!' type 'I'.
          exit.
        else.
          loop at tg_selectedrow into wg_selectedrow.
            read table it_saida_02 assigning field-symbol(<fs_saida_02>) index wg_selectedrow-index.

            "Limpar variável de seleção do ALV principal
            read table it_saida_01 assigning field-symbol(<fs_saida_01>)
                                                      with key  cl_codigo    = <fs_saida_02>-cl_codigo
                                                                cod_material = <fs_saida_02>-cod_material
                                                                safra        = <fs_saida_02>-safra
                                                                tp_class     = <fs_saida_02>-tp_class
                                                                po_destino   = <fs_saida_02>-po_destino
                                                                po_embarque  = <fs_saida_02>-po_embarque
                                                                operacao     = <fs_saida_02>-operacao
                                                                emite_nfs    = abap_true.


            if <fs_saida_01> is assigned.
              <fs_saida_01>-emite_nfs = abap_false.
              <fs_saida_01>-color = ''.
              <fs_saida_02>-del = abap_true.
            endif.

          endloop.
          delete it_saida_02 where del eq abap_true.
        endif.

        call method g_grid_02->refresh_table_display
          exporting
            is_stable = wa_stable.

      when 'SALVAR'.
        loop at it_saida_02 into wa_saida_02.
          wa_t0225-bukrs           =  vbukrs.
          wa_t0225-werks           =  vwerks.
          wa_t0225-ano_viagem      =  p_ano-low.
          wa_t0225-cl_codigo       =  |{ wa_saida_02-cl_codigo alpha = in }|.
          wa_t0225-safra           =  wa_saida_02-safra.
          vmatnr18                 =  |{ wa_saida_02-cod_material alpha = in }|.
          wa_t0225-cod_material    =  vmatnr18.
          wa_t0225-tp_class        =  wa_saida_02-tp_class.
          wa_t0225-nr_dco          =  wa_saida_02-nr_dco.
          wa_t0225-po_embarque     =  |{ wa_saida_02-po_embarque  alpha = in }|.
          wa_t0225-po_destino      =  |{ wa_saida_02-po_destino  alpha = in }|.
          wa_t0225-peso_vinculado  =  wa_t0225-peso_vinculado + wa_saida_02-peso_vinculado.
          wa_t0225-operacao        = wa_saida_02-operacao.

          clear wa_saida_02.
        endloop.
        perform  get_next_number in program zsdr0108 using  'ZSEQ_SD108'  '01'  changing  wa_t0225-id_seq.

        modify zsdt0225 from wa_t0225.
        commit work.

        loop at it_saida_01 into wa_saida_01
          where emite_nfs = 'X'.

*          DATA(cod_material) = |{ wa_saida_01-cod_material ALPHA = IN }|.
          vmatnr18 = |{ wa_saida_01-cod_material alpha = in }|.
          vmatnr   = vmatnr18.
          data(cl_codigo)    = |{ wa_saida_01-cl_codigo alpha = in }|.


          update zlest0061 set emite_nfs = 'X'
                               id_seq    = wa_t0225-id_seq
          where bukrs           eq  wa_saida_01-bukrs        and
                werks           eq  wa_saida_01-werks        and
                ano_viagem      eq  wa_saida_01-ano_viagem   and
                nr_viagem       eq  wa_saida_01-nr_viagem    and
                embarcacao      eq  wa_saida_01-embarcacao   and
                nome_emb        eq  wa_saida_01-nome_emb     and
                cod_material    eq  vmatnr                   and
                tp_class        eq  wa_saida_01-tp_class     and
                dt_movimento    eq  wa_saida_01-dt_movimento and
                cl_codigo       eq  cl_codigo  and
                rm_codigo       eq  wa_saida_01-rm_codigo    and
                auart           eq  wa_saida_01-auart        and
                nr_dco          eq  wa_saida_01-nr_dco       and
                safra           eq  wa_saida_01-safra        and
                operacao        eq  wa_saida_01-operacao     and
                id_frete_aqua   eq  wa_saida_01-id_frete_aqua.

          clear wa_saida_01.
        endloop.
        commit work.

        clear wa_t0225.
        refresh: it_saida_01, it_saida_02.
        perform z_buscar_dados.
    endcase.

    call method g_grid_01->refresh_table_display
      exporting
        is_stable = wa_stable.

    call method g_grid_02->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.
endclass.

class lcl_event_handler definition.
  public section.

    class-methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,


      on_hotspot_click for event hotspot_click of cl_gui_alv_grid
        importing e_column_id e_row_id es_row_no sender.

endclass.

class lcl_event_handler implementation.
  method on_data_changed.

    data: vl_ukurs    type ukurs_curr,
          vl_gdatu    type gdatu_inv,
          v_vlr_usd   type zlest0061-vlr_usd,
          v_vlr_brl   type zlest0061-vlr_brl,
          v_tax_dolar type zlest0061-tax_dolar,
          stcd_str    type c length 9,
          stcd_conc   type c length 4,
          vl_tabix    type sy-tabix.


    loop at er_data_changed->mt_good_cells into data(wa_good_cells)
      where  fieldname eq 'WAERK'.

      loop at it_saida_03 into wa_saida_03.

        check wa_good_cells-row_id eq sy-tabix.

        case wa_good_cells-fieldname.
          when 'WAERK'.

            if wa_saida_03-dt_fatura is initial.
              message 'Favor informar a Data da Fatura!' type 'I'.
              exit.
            elseif wa_saida_03-auart is initial.
              message 'Favor informar Tp OV!' type 'I'.
              exit.
            else.
              select single *
                from mara into wa_mara
               where matnr eq wa_saida_03-cod_material.

*              wa_mara-matkl = |{ wa_mara-matkl ALPHA = IN }|.

              data(cl_codigo) = |{ wa_saida_03-cl_codigo alpha = in }|.

*              SELECT SINGLE * FROM lfa1 INTO @DATA(wl_lfa1) WHERE lifnr EQ @cl_codigo.     " Comentado porque esta fazendo a busca errado do cliente/fornecedor ( Leila Mara )
*              SELECT SINGLE * FROM kna1 INTO @DATA(wl_kna1) WHERE kunnr EQ @wl_lfa1-kunnr. " Comentado porque esta fazendo a busca errado do cliente/fornecedor ( Leila Mara )
*              IF sy-subrc NE 0.
              select single * from kna1 into @data(wl_kna1) where kunnr eq @cl_codigo.
*              ENDIF.

              if sy-subrc eq 0.

                concatenate wl_kna1-stcd1(8) '%' into stcd_str.

                select * from kna1 into table @data(tl_kna1)
                  where stcd1 like @stcd_str.

                check not tl_kna1[] is initial.

                loop at tl_kna1 into data(wkna1).

                  vl_tabix = sy-tabix.

                  clear: stcd_conc.
                  concatenate wkna1-stcd1+8(1) wkna1-stcd1+9(1) wkna1-stcd1+10(1)  wkna1-stcd1+11(1) into stcd_conc.

                  if ( stcd_conc ne '0001' ).
                    delete tl_kna1 index vl_tabix.
                  else.

                    select single *
                      from zsdt0226 into @data(wa_0226)
                     where  emp_viagem     eq @wa_saida_03-bukrs
                      and   centro_viagem  eq @wa_saida_03-werks
                      and   po_destino     eq @wa_saida_03-po_destino
                      and   po_embarque    eq @wa_saida_03-po_embarque.

                    if sy-subrc = 4.
                      message 'Empresa Fatura Serviço não encontrado. Favor realize o parametro na transação ZSDT0159 !' type 'S'.
                      exit.
                    endif.

                    select single *
                        from zlest0055 into wa_zlest0055
                        where kunnr     eq wkna1-kunnr
                         and  auart     eq wa_saida_03-auart
*                         AND  matkl     EQ wa_mara-matkl
                         and  dt_fim    ge wa_saida_03-dt_fatura
                         and  waerk     eq wa_good_cells-value
                         and  status    eq '1'
                         and  vkorg     eq wa_0226-emp_fat_serv.

                    case  wa_zlest0055-waerk.
                      when 'BRL'.
                        v_vlr_brl = ( ( wa_saida_03-peso_vinculado / 1000 ) *  wa_zlest0055-netpr  ).

                        clear: obj_zcl_util_sd.
                        create object obj_zcl_util_sd.

                        vl_gdatu =  wa_saida_03-dt_fatura.
                        obj_zcl_util_sd->set_data( exporting i_data = vl_gdatu ).
                        obj_zcl_util_sd->set_kurst( exporting i_kurst = wa_zlest0055-kurst ).
                        obj_zcl_util_sd->set_waerk( exporting i_waerk = wa_zlest0055-waerk ).
                        obj_zcl_util_sd->set_tcurr( exporting i_tcurr = 'USD' ).
                        obj_zcl_util_sd->taxa_cambio( receiving e_ukurs = vl_ukurs ).

                        if vl_ukurs is not initial.

                          wa_saida_03-tax_dolar =  ( vl_ukurs * -1 ).
                          v_vlr_usd = ( v_vlr_brl /  ( vl_ukurs * -1 ) ).
                          wa_saida_03-vlr_brl      = v_vlr_brl.
                          wa_saida_03-vlr_usd      = v_vlr_usd.
                          wa_saida_03-waerk_fatura = vl_ukurs.

                        else.
                          message s000(zwrm001) display like 'E' with 'Taxa do câmbio não cadastrada.'.
                        endif.

                      when 'USD'.
                        v_vlr_usd = ( ( wa_saida_03-peso_vinculado / 1000 ) *  wa_zlest0055-netpr  ).

                        clear: vl_ukurs, vl_gdatu .
                        clear: obj_zcl_util_sd.
                        create object obj_zcl_util_sd.

                        vl_gdatu =  wa_saida_03-dt_fatura.
                        condense wa_zlest0055-kurst no-gaps.
                        obj_zcl_util_sd->set_data( exporting i_data = vl_gdatu ).
                        obj_zcl_util_sd->set_kurst( exporting i_kurst = wa_zlest0055-kurst ).
                        obj_zcl_util_sd->set_waerk( exporting i_waerk = wa_zlest0055-waerk ).
                        obj_zcl_util_sd->set_tcurr( exporting i_tcurr = 'BRL' ).
                        obj_zcl_util_sd->taxa_cambio( receiving e_ukurs = vl_ukurs ).

                        if vl_ukurs is not initial.

                          v_vlr_brl = v_vlr_usd * vl_ukurs.
                          wa_saida_03-tax_dolar    = vl_ukurs.
                          wa_saida_03-vlr_brl      = v_vlr_brl.
                          wa_saida_03-vlr_usd      = v_vlr_usd.
                          wa_saida_03-waerk_fatura = vl_ukurs.

                        else.
                          message s000(zwrm001) display like 'E' with 'Taxa do câmbio não cadastrada.'.
                        endif.
                    endcase.
                    wa_saida_03-netpr        =  wa_zlest0055-netpr.
                    wa_saida_03-kunnr        =  wa_zlest0055-kunnr.
                    wa_saida_03-waerk_fatura =  wa_zlest0055-waerk_fatura.
                    modify it_saida_03 from wa_saida_03 index wa_good_cells-row_id.
                  endif.
                endloop.
              endif.
            endif.
        endcase.
        clear: wa_saida_03, wa_mara, wa_zlest0055.
      endloop.
    endloop.

    call method g_grid_03->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.

  method on_hotspot_click.

    read table it_saida_03 into wa_saida_03 index e_row_id-index.

    case e_column_id.
      when 'NR_OV'.
        if wa_saida_03-nr_ov is not initial.
          set parameter id 'AUN' field wa_saida_03-nr_ov.
          call transaction 'VA03' and skip first screen.
        endif.
      when 'FATURA'.
        if wa_saida_03-fatura is not initial.
          set parameter id 'VF' field wa_saida_03-fatura.
          call transaction 'VF03' and skip first screen.
        endif.
      when 'DOCNUM'.
        if wa_saida_03-docnum is not initial.
          set parameter id 'JEF' field wa_saida_03-docnum.
          call transaction 'J1B3N' and skip first screen.
        endif.
    endcase.
  endmethod.

endclass.

class lcl_event_f403 definition.
  public section.
    class-methods:
      on_f4_03 for event onf4 of cl_gui_alv_grid
        importing e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.
endclass.

class lcl_event_f403 implementation.
  method on_f4_03.

    types: begin of ty_field,
             tabname   type dd03l-tabname,
             fieldname type dd03l-fieldname,
             s(1)      type c,
           end of ty_field.

    types: begin of ty_value,
             tabname     type dd03l-tabname,
             fieldname   type  dd03l-fieldname,
             char79(100) type c,
           end of  ty_value.

    data: begin of wl_auart,
            field(50),
          end of wl_auart.

    data: begin of wl_waerk,
            field(50),
          end of wl_waerk.

    data: tl_auart    like table of wl_auart,
          tl_waerk    like table of wl_waerk,
          tl_field    type table of ty_field,
          wl_field    type ty_field,
          tl_value    type table of ty_value,
          wl_value    type ty_value,
          wl_char(20),
          wl_index    type sy-tabix.

    if e_fieldname = 'AUART'.

      select *
        from tvakt into table @data(it_tvakt)
        where spras eq @sy-langu.

      loop at it_tvakt into data(wa_tvakt) .
        move: wa_tvakt-auart to wl_auart-field.
        append wl_auart to tl_auart.

        move: wa_tvakt-bezei to wl_auart-field.
        append wl_auart to tl_auart.
      endloop.

      wl_field-tabname   = 'TVAKT'.
      wl_field-fieldname = 'AUART'.
      wl_field-s         = 'X'.
      append wl_field to tl_field.

      wl_field-tabname   = 'TVAKT'.
      wl_field-fieldname = 'BEZEI'.
      wl_field-s         = ' '.
      append wl_field to tl_field.


      call function 'HELP_VALUES_GET_WITH_TABLE_EXT'
        exporting
          fieldname                 = 'TVAKT'
          tabname                   = 'AUART'
        importing
          index                     = wl_index
          select_value              = wl_char
        tables
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_auart
        exceptions
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      if sy-subrc is initial.
        read table it_tvakt into wa_tvakt index wl_index.
        if es_row_no-row_id gt 0.
          read table it_saida_03 into wa_saida_03  index es_row_no-row_id.
          if sy-subrc is initial.
            move: wa_tvakt-auart   to wa_saida_03-auart.
            modify it_saida_03 from wa_saida_03 index es_row_no-row_id.
          endif.
        endif.
      endif.

    elseif  e_fieldname = 'WAERK'.

      select *
        from tcurt into table @data(it_tcurt)
        where spras eq @sy-langu.

      loop at it_tcurt into data(wa_tcurt) .
        move: wa_tcurt-waers to wl_waerk-field.
        append wl_waerk to tl_waerk.

        move: wa_tcurt-ltext to wl_waerk-field.
        append wl_waerk to tl_waerk.
      endloop.

      wl_field-tabname   = 'TCURT'.
      wl_field-fieldname = 'WAERS'.
      wl_field-s         = 'X'.
      append wl_field to tl_field.

      wl_field-tabname   = 'TCURT'.
      wl_field-fieldname = 'LTEXT'.
      wl_field-s         = ' '.
      append wl_field to tl_field.


      call function 'HELP_VALUES_GET_WITH_TABLE_EXT'
        exporting
          fieldname                 = 'TCURT'
          tabname                   = 'WAERS'
        importing
          index                     = wl_index
          select_value              = wl_char
        tables
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_waerk
        exceptions
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      if sy-subrc is initial.
        read table it_tcurt into wa_tcurt index wl_index.
        if es_row_no-row_id gt 0.
          read table it_saida_03 into wa_saida_03  index es_row_no-row_id.
          if sy-subrc is initial.
            move: wa_tcurt-waers   to wa_saida_03-waerk.
            modify it_saida_03 from wa_saida_03 index es_row_no-row_id.
          endif.
        endif.
      endif.
    endif.

    call method g_grid_03->refresh_table_display
      exporting
        is_stable = wa_stable.

  endmethod.
endclass.


form z_buscar_dados.

  refresh: it_zlest0061, it_saida_01.

  select *
    from zlest0061  into table it_zlest0061
   where bukrs       eq vbukrs
    and  werks       eq vwerks
    and  ano_viagem  in p_ano
    and  nr_viagem   in p_viagem
    and  dt_fatura   in r_dt_fatura
    and  ck_anulado  eq ' '
    and  emite_nfs   eq ' '.

  check it_zlest0061 is not initial.


  loop at it_zlest0061 into wa_zlest0061.

    select single *
     from kna1 into wa_kna1
    where kunnr eq wa_zlest0061-cl_codigo
     and  ktokd eq 'ZCIC'.

    if sy-subrc = 0.

      select single *
        from j_1bbranch into wa_jbbranch
       where branch eq wa_zlest0061-cl_codigo+6(4).

      select single *
        from zsdt0226 into wa_zsdt0226
        where centro_viagem eq wa_zlest0061-werks.

      if sy-subrc = 0.

        if  wa_zsdt0226-emp_fat_serv = wa_jbbranch-bukrs.

          update zlest0061 set emite_nfs = 'X'
           where bukrs         eq wa_zlest0061-bukrs
             and werks         eq wa_zlest0061-werks
             and ano_viagem    eq wa_zlest0061-ano_viagem
             and nr_viagem     eq wa_zlest0061-nr_viagem
             and embarcacao    eq wa_zlest0061-embarcacao
             and nome_emb      eq wa_zlest0061-nome_emb
             and cod_material  eq wa_zlest0061-cod_material
             and tp_class      eq wa_zlest0061-tp_class
             and dt_movimento  eq wa_zlest0061-dt_movimento
             and cl_codigo     eq wa_zlest0061-cl_codigo
             and rm_codigo     eq wa_zlest0061-rm_codigo
             and auart         eq wa_zlest0061-auart
             and nr_dco        eq wa_zlest0061-nr_dco
             and safra         eq wa_zlest0061-safra
             and id_frete_aqua eq wa_zlest0061-id_frete_aqua.

          commit work.
        endif.
      endif.
    endif.

    clear: wa_zlest0061, wa_zsdt0226, wa_kna1, wa_jbbranch.
  endloop.

  refresh it_zlest0061.

  select *
     from zlest0061  into table it_zlest0061
    where bukrs       eq vbukrs
     and  werks       eq vwerks
     and  ano_viagem  in p_ano
     and  nr_viagem   in p_viagem
     and  dt_fatura   in r_dt_fatura
     and  ck_anulado  eq ' '
     and  emite_nfs   eq ' '.

  check it_zlest0061 is not initial.


  select * from zlest0056
      into table it_zlest0056
    for all entries in it_zlest0061
     where bukrs      eq it_zlest0061-bukrs
       and werks      eq it_zlest0061-werks
       and ano_viagem eq it_zlest0061-ano_viagem
       and nr_viagem  eq it_zlest0061-nr_viagem.

  select *
    from makt into table it_makt
    for all entries in it_zlest0061
  where matnr eq it_zlest0061-cod_material
    and spras eq sy-langu.

  "Check grupo material.
*  IF it_zlest0061 IS NOT INITIAL.
*    SELECT *
*       FROM mara INTO TABLE it_mara
*       FOR ALL ENTRIES IN it_zlest0061
*     WHERE matnr EQ it_zlest0061-cod_material.
*  ENDIF.



  perform z_trata_dados.
endform.

form z_trata_dados.

  loop at it_zlest0061 into wa_zlest0061.

    read table it_makt into wa_makt with key matnr = wa_zlest0061-cod_material.

    read table it_zlest0056 into wa_zlest0056 with key bukrs       =  wa_zlest0061-bukrs
                                                       werks       =  wa_zlest0061-werks
                                                       ano_viagem  =  wa_zlest0061-ano_viagem
                                                       nr_viagem   =  wa_zlest0061-nr_viagem.

    wa_saida_01-id_frete_aqua    = wa_zlest0061-id_frete_aqua.
    wa_saida_01-dt_fatura        = wa_zlest0061-dt_fatura.
    wa_saida_01-nr_viagem        = wa_zlest0061-nr_viagem.
    wa_saida_01-nome_emb         = wa_zlest0061-nome_emb.
    wa_saida_01-cod_material     = |{ wa_zlest0061-cod_material alpha = out }|.
    wa_saida_01-maktx            = wa_makt-maktx.
    wa_saida_01-tp_class         = wa_zlest0061-tp_class.
    wa_saida_01-nr_dco           = wa_zlest0061-nr_dco.
    wa_saida_01-safra            = wa_zlest0061-safra.
    wa_saida_01-peso_vinculado   = wa_zlest0061-peso_vinculado.
    wa_saida_01-cl_codigo        = |{ wa_zlest0061-cl_codigo alpha = out }|.
    wa_saida_01-bukrs            = wa_zlest0061-bukrs.
    wa_saida_01-werks            = wa_zlest0061-werks.
    wa_saida_01-ano_viagem       = wa_zlest0061-ano_viagem.
    wa_saida_01-embarcacao       = wa_zlest0061-embarcacao.
    wa_saida_01-dt_movimento     = wa_zlest0061-dt_movimento.
    wa_saida_01-rm_codigo        = wa_zlest0061-rm_codigo.
    wa_saida_01-auart            = wa_zlest0061-auart.
    wa_saida_01-vkorg            = wa_zlest0061-vkorg.
    wa_saida_01-vtweg            = wa_zlest0061-vtweg.
    wa_saida_01-spart            = wa_zlest0061-spart.
    wa_saida_01-netpr            = wa_zlest0061-netpr.
    wa_saida_01-zterm            = wa_zlest0061-zterm.
    wa_saida_01-matnr_ov         = wa_zlest0061-matnr_ov.
    wa_saida_01-po_destino       = |{ wa_zlest0056-po_destino alpha = out }|.
    wa_saida_01-po_embarque      = |{ wa_zlest0056-po_embarque alpha = out }|.
    wa_saida_01-centro_fat       = wa_zlest0061-centro_fat.
    wa_saida_01-operacao         = wa_zlest0061-operacao.

    read table it_mara into data(ws_mara) with key matnr = wa_zlest0061-cod_material.
    if sy-subrc eq 0.
*      wa_saida_01-matkl = ws_mara-matkl.
    endif.

    append wa_saida_01 to it_saida_01.
    clear: wa_saida_01, wa_zlest0061.
  endloop.

endform.

form z_busca_nf.

  refresh: it_saida_03, it_zsdt0225.

  if vbukrs3 is initial.
    message 'Favor informar Empresa !' type 'S'.
    exit.
  elseif vwerks3 is initial.
    message 'Favor informar Filial !' type 'S'.
    exit.
  elseif s_ano is initial.
    message 'Favor informar Ano !' type 'S'.
    exit.
  else.
    if v_a_faturar is not initial.

      select *
        from zsdt0225 into table it_zsdt0225
      where bukrs      eq vbukrs3
        and werks      eq vwerks3
        and ano_viagem in s_ano
       and dt_fatura   eq '00000000'.

    elseif v_faturado is not initial.

      if s_dt_fat is initial.
        message 'Favor informar a Data Fatura!' type 'S'.
        exit.
      else.

        select *
          from zsdt0225 into table it_zsdt0225
        where bukrs      eq vbukrs3
          and werks      eq vwerks3
          and ano_viagem in s_ano
          and dt_fatura  in s_dt_fat.
      endif.
    endif.

    perform z_trata_dados_nf.
  endif.
endform.

form z_trata_dados_nf.

  loop at it_zsdt0225 into wa_zsdt0225.

    wa_saida_03-id_seq           =   wa_zsdt0225-id_seq.
    wa_saida_03-cl_codigo        =   wa_zsdt0225-cl_codigo.
    wa_saida_03-safra            =   wa_zsdt0225-safra.
    wa_saida_03-cod_material     =   wa_zsdt0225-cod_material.
    wa_saida_03-tp_class         =   wa_zsdt0225-tp_class.
    wa_saida_03-nr_dco           =   wa_zsdt0225-nr_dco.
    wa_saida_03-peso_vinculado   =   wa_zsdt0225-peso_vinculado.
    wa_saida_03-dt_fatura        =   wa_zsdt0225-dt_fatura.
    wa_saida_03-auart            =   wa_zsdt0225-auart.
    wa_saida_03-waerk            =   wa_zsdt0225-waerk.
    wa_saida_03-waerk_fatura     =   wa_zsdt0225-waerk_fatura.
    wa_saida_03-tax_dolar        =   wa_zsdt0225-tax_dolar.
    wa_saida_03-vlr_usd          =   wa_zsdt0225-vlr_usd.
    wa_saida_03-vlr_brl          =   wa_zsdt0225-vlr_brl.
    wa_saida_03-nr_ov            =   wa_zsdt0225-nr_ov.
    wa_saida_03-fatura           =   wa_zsdt0225-fatura.
    wa_saida_03-docnum           =   wa_zsdt0225-docnum.
    wa_saida_03-bukrs            =   wa_zsdt0225-bukrs.
    wa_saida_03-werks            =   wa_zsdt0225-werks.
    wa_saida_03-ano_viagem       =   wa_zsdt0225-ano_viagem.
    wa_saida_03-vkorg            =   wa_zsdt0225-vkorg.
    wa_saida_03-vtweg            =   wa_zsdt0225-vtweg.
    wa_saida_03-spart            =   wa_zsdt0225-spart.
    wa_saida_03-netpr            =   wa_zsdt0225-netpr.
    wa_saida_03-zterm            =   wa_zsdt0225-zterm.
    wa_saida_03-matnr_ov         =   wa_zsdt0225-matnr_ov.
    wa_saida_03-po_destino       =   wa_zsdt0225-po_destino.
    wa_saida_03-po_embarque      =   wa_zsdt0225-po_embarque.
    wa_saida_03-operacao         =   wa_zsdt0225-operacao.

    if wa_saida_03-fatura is initial and wa_saida_03-docnum is initial and
      wa_saida_03-nr_ov is initial.

      free wa_saida_03-celltab.
      gt_estilo_03 =  value #( ( fieldname = 'DT_FATURA'  style = cl_gui_alv_grid=>mc_style_enabled  )
                               ( fieldname = 'AUART'      style = cl_gui_alv_grid=>mc_style_enabled  )
                               ( fieldname = 'WAERK'      style = cl_gui_alv_grid=>mc_style_enabled  ) ).
    else.
      free wa_saida_03-celltab.
      gt_estilo_03 =  value #( ( fieldname = 'DT_FATURA'  style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'AUART'      style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'WAERK'      style = cl_gui_alv_grid=>mc_style_disabled  ) ).
    endif.

    if wa_zsdt0225-cod_material is not initial.
      select single * from mara into @data(ws_mara) where matnr eq @wa_zsdt0225-cod_material.
      if sy-subrc eq 0.
*        wa_saida_03-matkl = ws_mara-matkl.
      endif.
    endif.

    insert lines of gt_estilo_03 into table wa_saida_03-celltab.
    append wa_saida_03 to it_saida_03.

    clear: wa_saida_03, wa_zsdt0225.
  endloop.
endform.


form z_handler_double_click using p_row
                                  p_column
                                  p_row_no.
  data verro type c.

  read table it_saida_01 into wa_saida_01 index p_row.
  if sy-subrc = 0.
    if wa_saida_01-emite_nfs is initial.

      read table it_saida_02 into data(wsaida2) index 1.
      if sy-subrc = 0.
        verro = abap_false.

        loop at it_saida_02 into wsaida2.
          if  wsaida2-cl_codigo   eq wa_saida_01-cl_codigo  and  wsaida2-cod_material  eq wa_saida_01-cod_material and
              wsaida2-safra       eq wa_saida_01-safra      and  wsaida2-tp_class      eq wa_saida_01-tp_class     and
              wsaida2-po_destino  eq wa_saida_01-po_destino and  wsaida2-po_embarque   eq wa_saida_01-po_embarque  and
              wsaida2-operacao    eq wa_saida_01-operacao.
            verro = abap_false.
          else.
            verro = abap_true.
          endif.
          clear: wsaida2.

        endloop.

        if verro = abap_true.
          message 'Os lançamentos devem ser do mesmo cliente, safra, material e operação!' type 'I'.
          exit.
        else.
          wa_saida_02-cl_codigo          = wa_saida_01-cl_codigo.
          wa_saida_02-safra              = wa_saida_01-safra.
          wa_saida_02-cod_material       = wa_saida_01-cod_material.
          wa_saida_02-tp_class           = wa_saida_01-tp_class.
*          wa_saida_02-matkl              = wa_saida_01-matkl.
          wa_saida_02-nr_dco             = wa_saida_01-nr_dco.
          wa_saida_02-peso_vinculado     = wa_saida_01-peso_vinculado.
          wa_saida_02-vkorg              = wa_saida_01-vkorg.
          wa_saida_02-vtweg              = wa_saida_01-vtweg.
          wa_saida_02-spart              = wa_saida_01-spart.
          wa_saida_02-netpr              = wa_saida_01-netpr.
          wa_saida_02-zterm              = wa_saida_01-zterm.
          wa_saida_02-matnr_ov           = wa_saida_01-matnr_ov.
          wa_saida_02-po_destino         = wa_saida_01-po_destino.
          wa_saida_02-po_embarque        = wa_saida_01-po_embarque.
          wa_saida_02-operacao           = wa_saida_01-operacao.
          append wa_saida_02 to it_saida_02.
        endif.
      else.
        wa_saida_02-cl_codigo          = wa_saida_01-cl_codigo.
        wa_saida_02-safra              = wa_saida_01-safra.
        wa_saida_02-cod_material       = wa_saida_01-cod_material.
        wa_saida_02-tp_class           = wa_saida_01-tp_class.
*        wa_saida_02-matkl              = wa_saida_01-matkl.
        wa_saida_02-nr_dco             = wa_saida_01-nr_dco.
        wa_saida_02-peso_vinculado     = wa_saida_01-peso_vinculado.
        wa_saida_02-vkorg              = wa_saida_01-vkorg.
        wa_saida_02-vtweg              = wa_saida_01-vtweg.
        wa_saida_02-spart              = wa_saida_01-spart.
        wa_saida_02-netpr              = wa_saida_01-netpr.
        wa_saida_02-zterm              = wa_saida_01-zterm.
        wa_saida_02-matnr_ov           = wa_saida_01-matnr_ov.
        wa_saida_02-po_destino         = wa_saida_01-po_destino.
        wa_saida_02-po_embarque        = wa_saida_01-po_embarque.
        wa_saida_02-operacao           = wa_saida_01-operacao.
        append wa_saida_02 to it_saida_02.
      endif.

      wa_saida_01-emite_nfs = 'X'.
      modify it_saida_01 from wa_saida_01 index p_row.
      clear: wa_saida_01, wa_saida_02, wsaida2.
    endif.
  endif.

  call method g_grid_02->refresh_table_display
    exporting
      is_stable = wa_stable.

endform.

form get_next_number using p_object
                           p_nr_range  changing p_number.
  clear p_number.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = p_nr_range
      object                  = p_object
    importing
      number                  = p_number
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.
  if sy-subrc <> 0.
    clear p_number.
    message e836(sd) with 'O intervalo de numeração não foi encontrado!'.
  endif.
endform.

form alv_01.
  clear wl_fcat_01.
  refresh it_fcat_01[].

  perform preenche_cat_01 using :
        'DT_FATURA'            'Data Fatura'         '10'    ''    ''     ''     ''    '',
        'NR_VIAGEM'            'Nr. Viagem'          '10'    ''    ''     ''     ''    '',
        'NOME_EMB'             'Nome Emb.'           '20'    ''    ''     ''     ''    '',
        'COD_MATERIAL'         'Material'            '18'    ''    ''     ''     ''    '',
        'MAKTX'                'Descrição'           '25'    ''    ''     ''     ''    '',
        'TP_CLASS'             'Class.Mat.'          '08'    ''    ''     ''     ''    '',
*        'MATKL   '             'Gp Material'         '10'    ''    ''     ''     ''    '',
        'NR_DCO'               'Nr.Dco.'             '06'    ''    ''     ''     ''    '',
        'OPERACAO'             'Operação'            '10'    ''    ''     ''     ''    '',
        'SAFRA'                'Safra'               '05'    ''    ''     ''     ''    '',
        'PESO_VINCULADO'       'Peso Vinc.'          '15'    ''    ''     'X'    ''    '',
        'CL_CODIGO'            'Cliente'             '10'    'X'   ''     ''     ''    '',
        'PO_EMBARQUE'          'Porto Embarque'      '14'    ''    ''     ''     ''    '',
        'PO_DESTINO'           'Porto Destino'       '13'    ''    ''     ''     ''    '',
        'CENTRO_FAT'           'Centro Fat.'         '11'    ''    ''     ''     ''    ''.

endform.


form preenche_cat_01 using  value(p_campo)
                            value(p_desc)
                            value(p_tam)
                            value(p_zero)
                            value(p_hot)
                            value(p_sum)
                            value(p_just)
                            value(p_cor).

  wl_fcat_01-fieldname = p_campo.
  wl_fcat_01-scrtext_l = p_desc.
  wl_fcat_01-scrtext_m = p_desc.
  wl_fcat_01-scrtext_s = p_desc.
  wl_fcat_01-outputlen = p_tam.
  wl_fcat_01-hotspot   = p_hot.
  wl_fcat_01-no_zero   = p_zero.
  wl_fcat_01-do_sum    = p_sum.
  wl_fcat_01-just      = p_just.
  wl_fcat_01-emphasize = p_cor.


  append wl_fcat_01 to  it_fcat_01.

endform.

form alv_02.
  clear wl_fcat_02.
  refresh it_fcat_02[].

  perform preenche_cat_02 using :
        'CL_CODIGO'            'Cliente'          '10'    'X'    ''     ''     ''    '',
        'SAFRA'                'Safra'            '05'    ''     ''     ''     ''    '',
        'COD_MATERIAL'         'Material'         '10'    'X'    ''     ''     ''    '',
        'TP_CLASS'             'Class.Mat.'       '10'    ''     ''     ''     ''    '',
*        'MATKL   '             'Gp Material'      '10'    ''     ''     ''     ''    '',
        'NR_DCO'               'Nr.Dco'           '06'    ''     ''     ''     ''    '',
        'OPERACAO'             'Operação'         '10'    ''     ''     ''     ''    '',
        'PESO_VINCULADO'       'Peso Vinc.'       '15'    ''     ''     'X'    ''    ''.
endform.

form preenche_cat_02 using  value(p_campo)
                            value(p_desc)
                            value(p_tam)
                            value(p_zero)
                            value(p_hot)
                            value(p_sum)
                            value(p_just)
                            value(p_cor).

  wl_fcat_02-fieldname = p_campo.
  wl_fcat_02-scrtext_l = p_desc.
  wl_fcat_02-scrtext_m = p_desc.
  wl_fcat_02-scrtext_s = p_desc.
  wl_fcat_02-outputlen = p_tam.
  wl_fcat_02-hotspot   = p_hot.
  wl_fcat_02-no_zero   = p_zero.
  wl_fcat_02-do_sum    = p_sum.
  wl_fcat_02-just      = p_just.
  wl_fcat_02-emphasize = p_cor.

  append wl_fcat_02 to  it_fcat_02.
endform.


form alv_03.

  clear wl_fcat_03.
  refresh it_fcat_03[].

  perform preenche_cat_03 using :
        'CL_CODIGO'            'Cliente'          '07'    'X'     ''     ''     ''   ''    ''   ''           ''             '',
        'SAFRA'                'Safra'            '05'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'COD_MATERIAL'         'Material'         '10'    'X'     ''     ''     ''   ''    ''   ''           ''             '',
        'TP_CLASS'             'Class.Mat.'       '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
*        'MATKL   '             'Gp Material'      '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'NR_DCO'               'Nro.Dco'          '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'OPERACAO'             'OP'               '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'PO_EMBARQUE'          'Porto Embarque'   '14'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'PO_DESTINO'           'Porto Destino'    '12'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'PESO_VINCULADO'       'Peso Vinc.'       '15'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'DT_FATURA'            'Dt Fatura'        '12'    ''      ''     ''     ''   ''    'X'  'ZSDT0225'   'DT_FATURA'    '',
        'AUART'                'Tp OV'            '10'    ''      ''     ''     ''   ''    'X'  ''           ''             'X',
        'WAERK'                'Moeda Ctr'        '10'    ''      ''     ''     ''   ''    'X'  ''           ''             'X',
        'NETPR'                'Preço Unit'       '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'TAX_DOLAR'            'Tx OV'            '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'VLR_USD'              'Vlr USD'          '15'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'VLR_BRL'              'Vlr BRL'          '15'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'WAERK_FATURA'         'Moeda Fatura'     '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'NR_OV'                'Nr.OV'            '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'FATURA'               'Fatura'           '10'    ''      ''     ''     ''   ''    ''   ''           ''             '',
        'DOCNUM'               'Docnum'           '10'    ''      ''     ''     ''   ''    ''   ''           ''             ''.

endform.

form preenche_cat_03 using  value(p_campo)
                            value(p_desc)
                            value(p_tam)
                            value(p_zero)
                            value(p_hot)
                            value(p_sum)
                            value(p_just)
                            value(p_cor)
                            value(p_edit)
                            value(p_table) like dd02d-tabname
                            value(p_field) like dd03d-fieldname
                            value(p_f4).

  wl_fcat_03-fieldname      = p_campo.
  wl_fcat_03-scrtext_l      = p_desc.
  wl_fcat_03-scrtext_m      = p_desc.
  wl_fcat_03-scrtext_s      = p_desc.
  wl_fcat_03-outputlen      = p_tam.
  wl_fcat_03-hotspot        = p_hot.
  wl_fcat_03-no_zero        = p_zero.
  wl_fcat_03-do_sum         = p_sum.
  wl_fcat_03-just           = p_just.
  wl_fcat_03-emphasize      = p_cor.
  wl_fcat_03-edit           = p_edit.
  wl_fcat_03-ref_table      = p_table.
  wl_fcat_03-ref_field      = p_field.
  wl_fcat_03-f4availabl     = p_f4.
  append wl_fcat_03 to  it_fcat_03.
endform.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  data: wa_event    type ref to lcl_event_receiver.
  data: obg_toolbar type ref to lcl_alv_toolbar.

  clear   wa_variant.

  set pf-status 'ST_0100'.
  set titlebar 'TL_0100'.

  perform alv_01.
  perform alv_02.

  if g_custom_container_01 is initial.

    create object g_custom_container_01
      exporting
        container_name              = 'CONTAINER01'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    if g_grid_01 is initial and  g_custom_container_01 is not initial.

      create object g_grid_01
        exporting
          i_parent          = g_custom_container_01
        exceptions
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          others            = 5.
    endif.

    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_check.
    append wl_function_01 to tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function_01 to tl_function_01.

    if wa_event is initial.
      create object wa_event.
      set handler: wa_event->zm_handle_click for g_grid_01.
    endif.

    wa_variant = value #( report = sy-repid ).

    wa_layout_01 = value #(
                            info_fname = 'COLOR'
                          ).

    call method g_grid_01->set_table_for_first_display
      exporting
        is_layout                     = wa_layout_01
        is_variant                    = wa_variant
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function_01
      changing
        it_outtab                     = it_saida_01[]
        it_fieldcatalog               = it_fcat_01
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

    call method g_grid_01->set_ready_for_input
      exporting
        i_ready_for_input = 1.

  else.
    call method g_grid_01->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.


  if g_custom_container_02 is initial.

    create object g_custom_container_02
      exporting
        container_name              = 'CONTAINER02'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    if g_grid_02 is initial and  g_custom_container_02 is not initial.

      create object g_grid_02
        exporting
          i_parent          = g_custom_container_02
        exceptions
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          others            = 5.
    endif.

    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_check.
    append wl_function_02 to tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function_02 to tl_function_02.

    if obg_toolbar is initial.

      create object obg_toolbar
        exporting
          io_alv_grid = g_grid_02.

      wa_variant = value #( report = sy-repid ).

      set handler: obg_toolbar->on_toolbar          for g_grid_02,
                   obg_toolbar->handle_user_command for g_grid_02.
    endif.

    call method g_grid_02->set_table_for_first_display
      exporting
        is_layout                     = wa_layout_02
        is_variant                    = wa_variant
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function_02
      changing
        it_outtab                     = it_saida_02[]
        it_fieldcatalog               = it_fcat_02
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

    call method g_grid_02->set_ready_for_input
      exporting
        i_ready_for_input = 1.
  else.
    call method g_grid_02->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.
    when 'BUSCAR'.
      if vbukrs is initial.
        message 'Favor informar campo Empresa !' type 'S'.
        exit.
      elseif vwerks is initial.
        message 'Favor informar campo Filial !' type 'S'.
        exit.
      elseif p_ano   is initial.
        message 'Favor informar Ano !' type 'S'.
        exit.
      elseif p_viagem is initial.
        message 'Favor informar Nr.Viagem !' type 'S'.
        exit.
      else.
        perform z_buscar_dados.
      endif.
    when 'REFRESH'.
      refresh it_saida_01.
      perform z_buscar_dados.
    when 'FATURAM'.
      clear: vbukrs3, vwerks3, s_ano[], s_dt_fat[].

      call screen 0102.
    when 'BTN_ADD'.
      perform f_add_selecao.

  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0102 output.

  data: lt_f401 type lvc_t_f4,
        wl_f401 type lvc_s_f4.

  clear   wa_variant.

  set pf-status 'ST_0102'.
  set titlebar 'TL_0102'.

  perform alv_03.

  if g_custom_container_03 is initial.

    create object g_custom_container_03
      exporting
        container_name              = 'CONTAINER03'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    if g_grid_03 is initial and  g_custom_container_03 is not initial.

      create object g_grid_03
        exporting
          i_parent          = g_custom_container_03
        exceptions
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          others            = 5.
    endif.

    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_check.
    append wl_function_03 to tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function_03 to tl_function_03.

    gt_layout_03-stylefname = 'CELLTAB'.

    wl_f401-fieldname  = 'AUART'.
    wl_f401-register   = 'X'.
    wl_f401-getbefore  = 'X'.
    append wl_f401 to  lt_f401.
    clear wl_f401.

    wl_f401-fieldname  = 'WAERK'.
    wl_f401-register   = 'X'.
    wl_f401-getbefore  = 'X'.
    append wl_f401 to  lt_f401.
    clear wl_f401.

    wa_variant = value #( report = sy-repid ).

    set handler: lcl_event_handler=>on_data_changed for g_grid_03,
                 lcl_event_handler=>on_hotspot_click for g_grid_03.

    call method g_grid_03->set_table_for_first_display
      exporting
        is_layout                     = gt_layout_03
        is_variant                    = wa_variant
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function_03
      changing
        it_outtab                     = it_saida_03[]
        it_fieldcatalog               = it_fcat_03
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.

    set handler: lcl_event_f403=>on_f4_03 for g_grid_03.

    call method g_grid_03->set_ready_for_input
      exporting
        i_ready_for_input = 1.

    call method g_grid_03->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid_03->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method g_grid_03->register_f4_for_fields
      exporting
        it_f4 = lt_f401[].

  else.

    call method g_grid_03->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0102 input.
  case sy-ucomm.
    when 'BACK'.
      refresh: it_saida_03, it_zsdt0225.
      clear: wa_head_03,  vbukrs3, vwerks3, s_ano, s_dt_fat.
      leave to screen 0.
    when 'BUSCAR'.
      perform z_busca_nf.
    when 'GERA_OV'.
      perform z_gera_ov.
    when 'ESTORNAR'.
      perform z_estorna_doc.
    when 'EXCLUIR'.
      call method g_grid_03->get_selected_rows
        importing
          et_index_rows = tg_selectedrow.

      if tg_selectedrow[] is initial.
        message 'Favor selecione uma linha! '  type 'I'.
        exit.
      else.
        loop at tg_selectedrow into wg_selectedrow.
          read table it_saida_03 into wa_saida_03 index wg_selectedrow-index.

          delete from zsdt0225 where id_seq = wa_saida_03-id_seq.
          commit work.

          update zlest0061 set emite_nfs = ' '
                               id_seq    = ' '  where id_seq  eq  wa_saida_03-id_seq.
          commit work.
        endloop.

        refresh it_saida_03.
        perform z_busca_nf.

        call method g_grid_03->refresh_table_display
          exporting
            is_stable = wa_stable.
      endif.
  endcase.
endmodule.

form z_gera_ov.

  data: wa_headerdata     type bapisdhd1,
        wa_itemdata       type bapisditm,
        wa_condition      type bapicond,
        wa_partner        type bapiparnr,
        wa_return         type bapiret2,
        vbeln_ov          type bapivbeln-vbeln,
        vbeln_fatura      type c length 10,
        wa_j_1bnflin      type j_1bnflin,
        tl_input_zsdt0225 type table of zsdt0225,
        wl_input_zsdt0225 type zsdt0225,
        wl_lifnr          type lfa1-lifnr,
        t_success         type table of bapivbrksuccess,
        t_billing         type table of bapivbrk,
        t_return          type table of bapireturn1,
        vl_lifnr          type lfa1-lifnr,
        vlr_icms          type zlest0061-vlr_usd,
        vlr_pis           type zlest0061-vlr_usd,
        vlr_cofins        type zlest0061-vlr_usd,
        vlr_iss           type zlest0061-vlr_usd,
        vlr_liquido       type zlest0061-vlr_usd,
        vl_validto        type j_1btxiss-validto,
        vl_validfrom      type j_1btxiss-validfrom,
        vl_data           type c length 10,
        qtd_rows          type sy-tabix,
        vpurch_no_s       type char35,
        wg_mensagem(30),
        wl_erro(1),
        msg(150),
        _dia              type i.

  data: r_auart_ztrg type range of auart.

  call method g_grid_03->get_selected_rows
    importing
      et_index_rows = tg_selectedrow.

  describe table tg_selectedrow lines qtd_rows.

  case qtd_rows.
    when 1.

      read table tg_selectedrow into wg_selectedrow index 1.
      read table it_saida_03 into wa_saida_03 index  wg_selectedrow-index.

      if wa_saida_03-waerk_fatura is initial.
        message s000(zwrm001) display like 'E' with 'Moeda Fatura não encontrada!'.
        return.
      endif.

      if wa_saida_03-vlr_brl > r_vlr_brl-low.
        msg = 'Valor BRL não pode ser maior que R$'.
        concatenate msg v_vlr_brl into  msg separated by space.
        message s000(zwrm001) display like 'E' with msg.
        return.
      endif.

      select single *
        from mara into @data(wa_mara)
       where matnr eq  @wa_saida_03-cod_material.

*      wa_mara-matkl = |{ wa_mara-matkl ALPHA = IN  }|.


      select single *
        from zsdt0225 into @data(wa_0225)
       where id_seq      eq @wa_saida_03-id_seq
         and bukrs       eq @wa_saida_03-bukrs
         and werks       eq @wa_saida_03-werks
         and ano_viagem  eq @wa_saida_03-ano_viagem
         and cl_codigo   eq @wa_saida_03-cl_codigo
         and nr_dco      eq @wa_saida_03-nr_dco
         and safra       eq @wa_saida_03-safra
         and po_embarque eq @wa_saida_03-po_embarque
         and po_destino  eq @wa_saida_03-po_destino
         and nr_ov       ne ' '.

      if sy-subrc = 0.
        message s000(zwrm001) display like 'E' with 'O.V de faturamento já gerada' 'Atualizar dados da tela!' .
        return.
      endif.

      select single * from zlest0059
        into @data(wl_zlest0059)
        where bukrs       eq @wa_saida_03-bukrs
          and auart       eq @wa_saida_03-auart
          and po_embarque eq @wa_saida_03-po_embarque
          and po_destino  eq @wa_saida_03-po_destino.

      if sy-subrc ne 0.
        message s000(zwrm001) display like 'E' with 'Material não encontrado para Tipo Venda'.
        exit.
      else.
        wa_saida_03-matnr_ov  =  wl_zlest0059-matnr.
      endif.

      select single *
        from zsdt0226 into wa_zsdt0226
       where emp_viagem     eq wa_saida_03-bukrs
        and  centro_viagem  eq wa_saida_03-werks
        and  po_embarque    eq wa_saida_03-po_embarque
        and  po_destino     eq wa_saida_03-po_destino.
*        AND  matkl          EQ wa_saida_03-matkl.

      if sy-subrc eq 0.
        wl_lifnr = |{ wa_zsdt0226-centro_fat_serv alpha = in }|.
      endif.


      select * from zlest0055
        into table @data(it_zlest0055)
       where kunnr       eq @wa_saida_03-kunnr
        and  auart       eq @wa_saida_03-auart
*        AND  matkl       EQ @wa_mara-matkl
        and  dt_fim      >= @wa_saida_03-dt_fatura
        and  status      eq '1'
        and  po_embarque eq @wa_saida_03-po_embarque
        and  po_destino  eq @wa_saida_03-po_destino
        and  vkorg       eq @wa_zsdt0226-emp_fat_serv.


      read table it_zlest0055 into data(wa_zlest0055) index 1.

      wa_saida_03-vkorg    = wa_zlest0055-vkorg.
      wa_saida_03-vtweg    = wa_zlest0055-vtweg.
      wa_saida_03-spart    = wa_zlest0055-spart.
      wa_saida_03-netpr    = wa_zlest0055-netpr.

      clear _dia.

      _dia = wa_saida_03-dt_fatura+6(2).
      if ( _dia >= 01 ) and  (  _dia <= 15 ).
        wa_saida_03-zterm    = wa_zlest0055-zterm.
      elseif ( _dia >= 16 ) and  (  _dia <= 31 ).
        wa_saida_03-zterm    = wa_zlest0055-zterm2.
      endif.


      select single * from lfa1 into @data(wa_lfa1) where lifnr eq @wl_lifnr.

      select single * from kna1 into @data(wa_kna1) where kunnr eq @wa_saida_03-cl_codigo.


      select * from zsdt0008
        into table @data(it_zsdt0008)
      where auart      eq @wa_saida_03-auart
        and vkaus      eq @wa_zlest0055-vkaus
        and uf_centro  eq @wa_lfa1-regio
        and uf_cliente eq @wa_kna1-regio
        and mwsk1      eq 'SD'
        and ownpr      ne 'X'.

      if sy-subrc eq 0.

        select * from j_1btxsdc
          into table @data(it_j_1btxsdc)
          for all entries in @it_zsdt0008
        where taxcode   eq @it_zsdt0008-j_1btxsdc
          and custusage eq 1.

        if sy-subrc eq 0.

          loop at it_j_1btxsdc into data(wa_j_1btxsdc).
            clear:vlr_icms, vlr_pis, vlr_cofins.

            if wa_j_1btxsdc-icms eq 'X'.

              select single * from j_1btxic1
                into @data(wa_j_1btxic1)
               where land1    eq 'BR'
                and  shipfrom eq @wa_lfa1-regio
                and  shipto   eq @wa_kna1-regio.

              case wa_saida_03-waerk_fatura.
                when 'BRL'.
                  vlr_icms = ( wa_saida_03-vlr_brl * wa_j_1btxic1-rate  ) / 100.
                when 'USD'.
                  vlr_icms = ( wa_saida_03-vlr_usd * wa_j_1btxic1-rate  ) / 100.
              endcase.
            endif.

            if wa_j_1btxsdc-pis eq 'X'.

              select single * from j_1btxpis
                into @data(wa_j_1btxpis)
               where country    eq 'BR'
                and  gruop      eq '72'
                and  value      eq @wa_saida_03-werks
                and  validto    <= @wa_saida_03-dt_fatura
                and  validfrom  >= @wa_saida_03-dt_fatura.

              case wa_saida_03-waerk_fatura.
                when 'BRL'.
                  vlr_pis = ( wa_saida_03-vlr_brl *  wa_j_1btxpis-rate  ) / 100.
                when 'USD'.
                  vlr_pis = ( wa_saida_03-vlr_usd *  wa_j_1btxpis-rate  ) / 100.
              endcase.
            endif.

            if ( wa_j_1btxsdc-cofins eq 'X' ).
              select single * from j_1btxcof
                into @data(wa_j_1btxcof)
                where country   eq 'BR'
                  and gruop     eq '71'
                  and value     eq @wa_saida_03-werks
                  and validto   <= @wa_saida_03-dt_fatura
                  and validfrom >= @wa_saida_03-dt_fatura.

              case wa_saida_03-waerk_fatura.
                when: 'BRL'.
                  vlr_cofins  = ( wa_saida_03-vlr_brl * wa_j_1btxcof-rate ) / 100.
                when: 'USD'.
                  vlr_cofins  = ( wa_saida_03-vlr_usd * wa_j_1btxcof-rate ) / 100.
              endcase.
            endif.

            case wa_saida_03-waerk_fatura.
              when: 'BRL'.
                vlr_liquido = wa_saida_03-vlr_brl - vlr_pis - vlr_cofins - vlr_icms.
              when: 'USD'.
                vlr_liquido = wa_saida_03-vlr_usd - vlr_pis - vlr_cofins - vlr_icms.
            endcase.
          endloop.
        endif.

      else.

        refresh: it_j_1btxsdc.

        select * from j_1bsdica
          into table @data(it_j_1bsdica)
        where auart eq @wa_saida_03-auart.

        select * from j_1btxsdc
          into table  it_j_1btxsdc
          for all entries in it_j_1bsdica
         where taxcode  eq it_j_1bsdica-txsdc
          and custusage eq 1.

        if sy-subrc eq 0.
          clear: wa_j_1btxpis, wa_j_1btxcof, vl_data, vl_validto, vl_validfrom.

          concatenate wa_saida_03-dt_fatura+6(2) '.'  wa_saida_03-dt_fatura+4(2) '.' wa_saida_03-dt_fatura(4) into vl_data.

          call function 'CONVERSION_EXIT_INVDT_INPUT'
            exporting
              input  = vl_data
            importing
              output = vl_validto.

          vl_validfrom  = vl_validto.

          loop at it_j_1btxsdc into wa_j_1btxsdc.

            if wa_j_1btxsdc-iss eq 'X'.

              select single * from j_1btxiss
                      into @data(wa_j_1btxiss)
                     where country    eq 'BR'
                       and gruop      eq '73'
                       and taxjurcode eq @wa_kna1-txjcd
                       and value      eq @wa_lfa1-txjcd
                       and validto   <=  @vl_validto
                       and validfrom >=  @vl_validfrom.

              case wa_saida_03-waerk_fatura.
                when: 'BRL'.
                  vlr_iss   = (  wa_saida_03-vlr_brl * wa_j_1btxiss-rate ) / 100.
                when: 'USD'.
                  vlr_iss    = ( wa_saida_03-vlr_usd * wa_j_1btxiss-rate ) / 100.
              endcase.
            endif.

            if wa_j_1btxsdc-pis eq 'X'.
              select  single * from j_1btxpis
                into wa_j_1btxpis
              where country   eq 'BR'
                and gruop     eq '72'
                and value     eq wa_saida_03-werks
                and validto   <= vl_validto
                and validfrom >= vl_validfrom.

              case wa_saida_03-waerk_fatura.
                when: 'BRL'.
                  vlr_pis     = ( wa_saida_03-vlr_brl * wa_j_1btxpis-rate ) / 100.
                when: 'USD'.
                  vlr_pis     = ( wa_saida_03-vlr_usd * wa_j_1btxpis-rate ) / 100.
              endcase.
            endif.

            if wa_j_1btxsdc-cofins eq 'X'.

              select single * from j_1btxcof
                into wa_j_1btxcof
             where country    eq 'BR'
               and gruop      eq '71'
               and value      eq wa_saida_03-werks
               and validto    <= vl_validto
               and validfrom  >= vl_validfrom.

              case wa_saida_03-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                when: 'BRL'.
                  vlr_cofins  = ( wa_saida_03-vlr_brl * wa_j_1btxcof-rate ) / 100.
                when: 'USD'.
                  vlr_cofins  = ( wa_saida_03-vlr_usd * wa_j_1btxcof-rate ) / 100.
              endcase.
            endif.
          endloop.

          case wa_saida_03-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
            when: 'BRL'.
              vlr_liquido = wa_saida_03-vlr_brl - vlr_iss - vlr_pis - vlr_cofins.
            when: 'USD'.
              vlr_liquido = wa_saida_03-vlr_usd - vlr_iss - vlr_pis - vlr_cofins.
          endcase.
        endif.
      endif.

      select single *  from mara into @data(wmara) where matnr eq  @wa_saida_03-matnr_ov.

      vpurch_no_s = wa_saida_03-id_seq.

      wl_header_in-sales_org  = wa_saida_03-vkorg. "(org. de venda)
      wl_header_in-distr_chan = wa_saida_03-vtweg. "(canal distr.)
      wl_header_in-currency   = wa_saida_03-waerk_fatura. "(moeda.) " gw_saida_ger_ov-waerk 12.12.16
      wl_header_in-pymt_meth  = 'P'.
      wl_header_in-division   = wa_saida_03-spart. "(setor atividade)
      wl_header_in-doc_type   = wa_saida_03-auart. "(tipo de ordem)
      wl_header_in-pmnttrms   = wa_saida_03-zterm. "ZTERM.
      wl_header_in-exrate_fi  = wa_saida_03-tax_dolar."(taxa dolar)
      wl_header_in-bill_date  = wa_saida_03-dt_fatura.
      wl_header_in-purch_no_c = '.'.
      wl_header_in-purch_no_s = vpurch_no_s.
      wl_header_in-fix_val_dy = wa_saida_03-dt_fatura. "VALDT Data efetiva fixa
      wl_header_in-pymt_meth  =  ''. "ZLSCH. Forma de pagamento
      wl_header_in-dlvschduse =  wa_zlest0055-vkaus. "VKAUS. Código de utilização
      wl_header_in-incoterms1  = 'SRV'.
      wl_header_in-incoterms2  = 'Serviço'.

      "Set utilizado por odens ZTRG criadas pela transação ZSDT0158
      select *
            from setleaf
            into table @data(it_set_ztrg)
           where setname = 'MAGGI_VF01_TP_OV'.

      loop at it_set_ztrg into data(wa_set_ztrg).
        append value rsdsselopt( option = wa_set_ztrg-valoption
                                 sign   = wa_set_ztrg-valsign
                                 low    = wa_set_ztrg-valfrom )
                            to r_auart_ztrg.
      endloop.

      if wa_saida_03-auart in r_auart_ztrg.
        wl_header_in-req_date_h = wa_saida_03-dt_fatura.
        wl_header_in-price_date  =  wa_saida_03-dt_fatura.
      endif.

      wa_itemdata-itm_number  = 10.
      wa_itemdata-material    = wa_saida_03-matnr_ov.
      wa_itemdata-plant       = wa_zsdt0226-centro_fat_serv.
      wa_itemdata-target_qty  = 1.
      wa_itemdata-target_qu   = wmara-meins.
      wa_itemdata-sales_unit  = wmara-meins.
      wa_itemdata-gross_wght  = wa_saida_03-peso_vinculado.
      wa_itemdata-net_weight  = wa_saida_03-peso_vinculado.
      wa_itemdata-untof_wght  = wmara-gewei.
      wa_itemdata-fix_val_dy  = wa_saida_03-dt_fatura.
      wa_itemdata-price_date  = wa_saida_03-dt_fatura.
      wa_itemdata-ex_rate_fi  = wa_saida_03-tax_dolar.
      wa_itemdata-dlvschduse  = wa_zlest0055-vkaus.
      wa_itemdata-incoterms1  = 'SRV'.
      wa_itemdata-incoterms2  = 'Serviço'.


      select single * from knvv into @data(wa_knvv)
        where kunnr  eq @wa_saida_03-cl_codigo
        and   vkorg  eq @wa_saida_03-vkorg
        and   vtweg  eq @wa_saida_03-vtweg
        and   spart  eq @wa_saida_03-spart.

      wa_knvv-kdgrp = |{ wa_knvv-kdgrp alpha = in }|.


      select single *
        from marc into @data(wa_marc) where matnr eq @wa_saida_03-matnr_ov
                                      and   werks eq @wa_zsdt0226-centro_fat_serv.

      select single *
        from mbew into @data(wa_mbew)   where matnr eq @wa_saida_03-matnr_ov
                                          and bwkey eq @wa_zsdt0226-centro_fat_serv.


      if wa_lfa1-regio eq wa_kna1-regio.

        select single *
           from j_1bapn
                  into @data(wa_1bapn)
                   where direct eq '2'
                     and dstcat eq '0'
                     and indus3 eq @wa_marc-indus
                     and itmtyp eq 'ZH'
                     and ownpro eq ' '
                     and matuse eq @wa_mbew-mtuse
                     and indus1 eq ' '.
      else.

        select single *
           from j_1bapn
                  into wa_1bapn
                   where direct eq '2'
                     and dstcat eq '1'
                     and indus3 eq wa_marc-indus
                     and itmtyp eq 'ZH'
                     and ownpro eq ' '
                     and matuse eq wa_mbew-mtuse
                     and indus1 eq ' '.
      endif.

      wa_itemdata-cfop_long = wa_1bapn-cfop.
      append wa_itemdata to it_itemdata.

      wl_items_inx-itm_number = 10.
      wl_items_inx-target_qty = 'X'.
      append wl_items_inx to it_items_inx.

      wl_schedules_in-itm_number = 10.
      wl_schedules_in-req_qty    = 1.
      append wl_schedules_in to  tl_schedules_in.

      wa_condition-itm_number  = '10'.
      wa_condition-cond_type  = 'PR00'.

      if vlr_liquido is not initial.
        wa_condition-cond_value =  vlr_liquido.
      else.
        if wa_saida_03-waerk_fatura = 'BRL'.
          wa_condition-cond_value =  wa_saida_03-vlr_brl.
        else.
          wa_condition-cond_value =  wa_saida_03-vlr_usd.
        endif.
      endif.

      wa_condition-currency  = wa_saida_03-waerk_fatura.
      wa_condition-cond_unit = wmara-meins.
      append  wa_condition to it_condition.

      wa_partner-partn_role = 'AG'.
      wa_partner-partn_numb = wa_saida_03-cl_codigo.
      append wa_partner to it_partner.

      wa_partner-partn_role = 'RE'.
      wa_partner-partn_numb = wa_saida_03-cl_codigo.
      append wa_partner to it_partner.

      wa_partner-partn_role = 'RG'.
      wa_partner-partn_numb = wa_saida_03-cl_codigo.
      append wa_partner to it_partner.

      wa_partner-partn_role = 'WE'.
      wa_partner-partn_numb = wa_saida_03-cl_codigo.
      append wa_partner to it_partner.


      call function 'SD_SALESDOCUMENT_CREATE'
        exporting
          sales_header_in     = wl_header_in
          sales_header_inx    = wl_header_inx
        importing
          salesdocument_ex    = vbeln_ov
        tables
          return              = it_return
          sales_items_in      = it_itemdata
          sales_items_inx     = it_items_inx
          sales_partners      = it_partner
          sales_schedules_in  = tl_schedules_in
          sales_conditions_in = it_condition.

      read table it_return into wa_return with key type = 'E'.
      if sy-subrc is initial.
        call function 'BAPI_TRANSACTION_ROLLBACK'.

        clear : wa_return, wl_msg_ret.

        loop at it_return into wa_return.
          wl_msg_ret-msg = wa_return-message.
          append wl_msg_ret to tg_msg_ret.
        endloop.

        check tg_msg_ret[] is not initial.

        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen      = '100'
            i_show        = 'X'
            i_repid       = sy-repid
            i_pressed_tab = 'TABSTRIP-ACTIVETAB'
            i_set_field   = 'X_FIELD'
          importing
            e_messagem    = wg_mensagem
          tables
            it_msgs       = tg_msg_ret.
      else.

        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = 'X'.

        select * from vbuv into table tl_vbuv
          where vbeln eq vbeln_ov.

        if sy-subrc is initial.

          loop at  tl_vbuv into wl_vbuv.
            clear: wl_fieldname, wl_text.

            wl_fieldname = wl_vbuv-fdnam.

            call function 'RM_DDIC_TEXTS_GET'
              exporting
                i_name                = wl_fieldname
                i_type                = 'DTEL'
                i_langu               = sy-langu
              importing
                e_ddtxt               = wl_text
              exceptions
                objtype_not_supported = 1
                illegal_input         = 2
                others                = 3.

            if sy-subrc <> 0.
              concatenate 'Dados incompletos na O.V: ' wl_vbuv-fdnam into wl_msg_ret-msg separated by space.
            else.
              concatenate 'Dados incompletos na O.V: ' wl_text into wl_msg_ret-msg separated by space.
            endif.

            append wl_msg_ret to tg_msg_ret.
          endloop.

          check tg_msg_ret[] is not initial.

          call function 'Z_DOC_CHECK_NEW'
            exporting
              i_screen      = '100'
              i_show        = 'X'
              i_repid       = sy-repid
              i_pressed_tab = 'TABSTRIP-ACTIVETAB'
              i_set_field   = 'X_FIELD'
            importing
              e_messagem    = wg_mensagem
            tables
              it_msgs       = tg_msg_ret.

          wl_erro = 'X'.
        endif.

        case wl_erro.
          when 'X'.

            refresh it_return.
            clear: wl_header_in.
            wl_header_inx2-updateflag = 'D'.
            call function 'BAPI_SALESORDER_CHANGE'  "#EC CI_USAGE_OK[2438131]
              exporting
                salesdocument    = vbeln_ov
                order_header_inx = wl_header_inx2
              tables
                return           = it_return.

            read table it_return into wa_return with key type = 'E'.

            if sy-subrc = 0.

              refresh tg_selectedrow.

              call method g_grid_03->get_selected_rows
                importing
                  et_index_rows = tg_selectedrow.

              clear: wa_saida_03, wg_selectedrow.
              read table tg_selectedrow into wg_selectedrow index 1.
              read table it_saida_03 into wa_saida_03 index  wg_selectedrow-index.

              clear: wa_saida_03-auart,
                     wa_saida_03-tax_dolar,
                     wa_saida_03-vlr_usd,
                     wa_saida_03-vlr_brl,
                     wa_saida_03-waerk,
                     wa_saida_03-waerk_fatura.

              modify it_saida_03 from wa_saida_03 index wg_selectedrow-index transporting auart
                                                                                          tax_dolar
                                                                                          vlr_usd
                                                                                          vlr_brl
                                                                                          waerk
                                                                                          waerk_fatura.
              call method g_grid_03->refresh_table_display
                exporting
                  is_stable = wa_stable.

              call function 'BAPI_TRANSACTION_COMMIT'
                exporting
                  wait = 'X'.
            endif.

          when others.

            vbeln_fatura = vbeln_ov.

            "Set utilizado por odens ZTRG criadas pela transação ZSDT0158
            if wa_saida_03-auart  in r_auart_ztrg.

              t_billing = value #( (
                                  ref_doc     = vbeln_fatura
                                  ref_doc_ca  = 'C'
                                  bill_date   = wa_saida_03-dt_fatura  )  ). "sy-datum  )  ).

            else.
              t_billing = value #( (
                                  ref_doc     = vbeln_fatura
                                  ref_doc_ca  = 'C'
                                  bill_date   = sy-datum  )  ).

            endif.

            call function 'BAPI_BILLINGDOC_CREATEMULTIPLE'  "#EC CI_USAGE_OK[2438131]
              tables
                billingdatain = t_billing
                return        = t_return
                success       = t_success.

            if t_success is not initial.

              call function 'BAPI_TRANSACTION_COMMIT'
                exporting
                  wait = 'X'.

              try.
                  vbeln_fatura = t_success[ 1 ]-bill_doc.
                catch cx_sy_itab_line_not_found.
              endtry.
            else.
              call function 'BAPI_TRANSACTION_ROLLBACK'.
              wl_erro =  abap_true.
            endif.

            wait up to 12 seconds.
            clear: wa_j_1bnflin, wl_input_zsdt0225.
            select single * from j_1bnflin into wa_j_1bnflin where refkey eq vbeln_fatura.

            if sy-subrc eq 0.

              wl_input_zsdt0225-mandt                 = sy-mandt.
              wl_input_zsdt0225-id_seq                = wa_saida_03-id_seq.
              wl_input_zsdt0225-bukrs                 = wa_saida_03-bukrs.
              wl_input_zsdt0225-werks                 = wa_saida_03-werks.
              wl_input_zsdt0225-ano_viagem            = wa_saida_03-ano_viagem.
              wl_input_zsdt0225-cl_codigo             = wa_saida_03-cl_codigo.
              wl_input_zsdt0225-safra                 = wa_saida_03-safra.
              wl_input_zsdt0225-cod_material          = wa_saida_03-cod_material.
              wl_input_zsdt0225-tp_class              = wa_saida_03-tp_class.
              wl_input_zsdt0225-nr_dco                = wa_saida_03-nr_dco.
              wl_input_zsdt0225-po_embarque           = wa_saida_03-po_embarque.
              wl_input_zsdt0225-po_destino            = wa_saida_03-po_destino.
              wl_input_zsdt0225-peso_vinculado        = wa_saida_03-peso_vinculado.
              wl_input_zsdt0225-dt_fatura             = wa_saida_03-dt_fatura.
              wl_input_zsdt0225-auart                 = wa_saida_03-auart.
              wl_input_zsdt0225-bukrs_serv            = wa_zsdt0226-emp_fat_serv.
              wl_input_zsdt0225-werks_serv            = wa_zsdt0226-centro_fat_serv.
              wl_input_zsdt0225-waerk                 = wa_saida_03-waerk.
              wl_input_zsdt0225-netpr                 = wa_saida_03-netpr.
              wl_input_zsdt0225-tax_dolar             = wa_saida_03-tax_dolar.
              wl_input_zsdt0225-vlr_usd               = wa_saida_03-vlr_usd.
              wl_input_zsdt0225-vlr_brl               = wa_saida_03-vlr_brl.
              wl_input_zsdt0225-vkorg                 = wa_saida_03-vkorg.
              wl_input_zsdt0225-vtweg                 = wa_saida_03-vtweg.
              wl_input_zsdt0225-spart                 = wa_saida_03-spart.
              wl_input_zsdt0225-matnr_ov              = wa_saida_03-matnr_ov.
              wl_input_zsdt0225-zterm                 = wa_saida_03-zterm.
              wl_input_zsdt0225-nr_ov                 = vbeln_ov.
              wl_input_zsdt0225-fatura                = vbeln_fatura.
              wl_input_zsdt0225-docnum                = wa_j_1bnflin-docnum.
              wl_input_zsdt0225-waerk_fatura          = wa_saida_03-waerk_fatura.
              wl_input_zsdt0225-operacao              = wa_saida_03-operacao.
              wl_input_zsdt0225-usuario               = sy-uname.
              wl_input_zsdt0225-data_registro         = sy-datum.
              wl_input_zsdt0225-hora_registro         = sy-uzeit.

              append  wl_input_zsdt0225 to tl_input_zsdt0225.
              modify zsdt0225 from table tl_input_zsdt0225.

              commit work.

              message 'O.V Gerada com Sucesso!' type 'S'.

              perform z_valida_ov_gerada.

              refresh it_saida_03.
              perform z_busca_nf.

              call method g_grid_03->refresh_table_display
                exporting
                  is_stable = wa_stable.

            endif.
        endcase.
      endif.

    when 2.
      message s000(zwrm001) display like 'W' with 'Selecionar somente uma ' 'linha para Geração da O.V.'.
    when others.
      if ( tg_selectedrow is initial ).
        message s000(zwrm001) display like 'W' with 'Linha não selecionada.'.
      endif.
  endcase.

  refresh:  it_return,  it_itemdata,  it_items_inx,   it_partner,   tl_schedules_in,   it_condition,  tg_msg_ret,
            t_billing, t_return, t_success, tl_input_zsdt0225.

  clear:  wl_header_in, wl_header_inx, vbeln_ov, wg_mensagem, wl_text, wl_msg_ret,
          vl_lifnr, vlr_icms, vlr_pis, vlr_cofins, vlr_iss, vlr_liquido, vl_validto,
          vl_validfrom, vl_data, wa_headerdata, wa_itemdata, wa_condition, wa_partner,
          wa_return, vbeln_ov, vbeln_fatura, wa_j_1bnflin,wl_input_zsdt0225, wl_lifnr,
          wg_mensagem, wl_erro, qtd_rows.

endform.


form z_estorna_doc.
  data: tg_msg_ret      type table of zfiwrs0002 with header line,
        wl_msg_ret      type zfiwrs0002,
        tg_estorno      type table of zsdt0225,
        wg_estorno      type  zsdt0225,
        t_success       type table of bapivbrksuccess,
        t_return        type table of bapireturn1,
        vdocnum_est     type j_1bdocnum,
*        is_cancelled    TYPE bapivbrkout-cancelled,
        is_cancelled    type bapivbrkout,
        estornado       type c,
        txt_bstkd       type vbkd-bstkd,
        data_ov         type c length 10,
        wg_mensagem(30),
        tl_bdc          type table of bdcdata,
        wl_bdc          type bdcdata,
        tl_msg          type table of bdcmsgcoll with header line,
        wl_msg          type bdcmsgcoll,
        w_bukrs         type t001-bukrs,  "Empresa
        w_desc_bukrs    type t001-butxt,  "Descrição da Empresa
        w_werks         type t001w-werks, "Centro Emissor
        w_desc_werks    type t001w-name1, "Descrição do Centro
        w_ano           type zlest0056-ano_viagem, "Ano da Viagem
        w_viagem        type zlest0058-nr_viagem.



  call method g_grid_03->get_selected_rows
    importing
      et_index_rows = tg_selectedrow.


  read table tg_selectedrow into wg_selectedrow index 1.
  read table it_saida_03 into wa_saida_03 index  wg_selectedrow-index.

  if sy-subrc eq 0.

    "FF #156930 - inicio
*Se ZSDT0225-DOC_ZNFW estiver preenchido, não devemos permitir o estorno.

    select single from zsdt0225
    fields doc_znfw
    where nr_ov  = @wa_saida_03-nr_ov
      and fatura = @wa_saida_03-fatura
      and docnum = @wa_saida_03-docnum
      and doc_znfw is not initial
      into @data(lv_dummy).

    if sy-subrc = 0.
      message 'Nota Fiscal registrada no tomador do serviço. Entre em contato com o CSC Financeiro' type 'E'.
    endif.

    "FF #156930 - fim

*---> S4 MIGRATION 07/07/2023 - MA
*    CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED'
*      EXPORTING
*        billingdoc_number       = wa_saida_03-fatura
*      IMPORTING
*        billingdoc_is_cancelled = is_cancelled.

    data: vl_bill_doc type bapivbrksuccess-bill_doc.

    vl_bill_doc = conv #( wa_saida_03-fatura ).
    call function 'BAPI_BILLINGDOC_GETDETAIL'
      exporting
        billingdocument       = vl_bill_doc
      importing
        billingdocumentdetail = is_cancelled
*       RETURN                =
      .
*<--- S4 MIGRATION 07/07/2023 - MA
    if is_cancelled-cancelled is initial.

      call function 'ZBAPI_BILLINGDOC_CANCEL1'
        exporting
          billingdocument = wa_saida_03-fatura
        tables
          return          = t_return
          success         = t_success.

    endif.

    if t_success[] is not initial or  is_cancelled-cancelled is not initial.

      select single docnum
        from j_1bnflin
        into @data(_docnum)
       where docnum eq @wa_saida_03-docnum.

      select single candat
        from j_1bnfdoc
        into @data(_vcandat)
       where docnum eq @_docnum.

      if _vcandat is initial.

        select single docnum
            from j_1bnfe_active  into @data(v_docnum)
         where docnum eq @_docnum
          and  docsta eq '1'
          and  cancel eq ''.

        if sy-subrc ne 0.

          call function 'J_1B_NF_DOCUMENT_CANCEL'
            exporting
              doc_number               = _docnum
              ref_type                 = space
              ref_key                  = space
              can_dat                  = sy-datum
            importing
              doc_number               = vdocnum_est
            exceptions
              document_not_found       = 1
              cancel_not_possible      = 2
              nf_cancel_type_not_found = 3
              database_problem         = 4
              docum_lock               = 5
              nfe_cancel_simulation    = 6
              others                   = 7.

          if sy-subrc eq 0.
            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = abap_true.
          else.
            data(w_erro) =  abap_true.
          endif.
        else.
          w_erro =  abap_true.
        endif.

        if w_erro is not initial.
          append value #( msg = |Impossível estorno de fatura { wa_saida_03-fatura }. Danfe, não estornada| ) to tg_msg_ret.
        endif.
      endif.
    endif.


    wait up to 2 seconds.

    delete t_return where type ne 'E' .

    if t_return[] is initial and w_erro is not initial.

      loop at t_return into data(wa).

        select *
          from t100 into table @data(it_t100)
         where sprsl eq 'PT'
          and  arbgb eq @wa-id
          and  msgnr eq @wa-number.

        check it_t100 is not  initial.

        loop at it_t100 into data(wa_t100).
          concatenate wa_saida_03-fatura '-' wa_t100-text into wl_msg_ret-msg separated by space.
          append wl_msg_ret to tg_msg_ret.
        endloop.

        if not ( tg_msg_ret[] is initial ).

          call function 'Z_DOC_CHECK_NEW'
            exporting
              i_screen      = '100'
              i_show        = 'X'
              i_repid       = sy-repid
              i_pressed_tab = 'TABSTRIP-ACTIVETAB'
              i_set_field   = 'X_FIELD'
            importing
              e_messagem    = wg_mensagem
            tables
              it_msgs       = tg_msg_ret.
        else.
          estornado = 'X'.
        endif.
      endloop.
    else.
      estornado = 'X'.
    endif.

    case estornado.
      when 'X'.
        refresh: tl_msg, tl_bdc.
        clear:  wl_msg, data_ov, estornado.

        concatenate sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) into data_ov.
        txt_bstkd  = '.'.

        perform f_preencher_dynpro using:
                'X' 'SAPMV45A'    '0102',
                ''  'BDC_CURSOR'  'VBAK-VBELN',
                ''  'BDC_OKCODE'  '/00',
                ''  'VBAK-VBELN'  wa_saida_03-nr_ov,

                'X' 'SAPMV45A'  '4001',
                '' 'BDC_OKCODE'  '/00',
                '' 'BDC_SUBSCR'  'SAPMV45A',
                '' 'VBKD-BSTKD'	txt_bstkd,
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'KUWEV-KUNNR'  wa_saida_03-cl_codigo,
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_CURSOR'	'VBAK-FAKSK',
                '' 'RV45A-KETDAT'	data_ov,
                '' 'RV45A-KPRGBZ'	'D',
                '' 'VBAK-FAKSK'	'03',
                '' 'VBKD-PRSDT'	data_ov,
                '' 'VBKD-ZTERM'	'Z150',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPLV45W',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPMV45A',

                'X' 'SAPMV45A'  '4001',
                '' 'BDC_OKCODE'	'=SICH',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'VBKD-BSTKD'	txt_bstkd,
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'KUWEV-KUNNR'  wa_saida_03-cl_codigo,
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'RV45A-KETDAT'	data_ov,
                '' 'RV45A-KPRGBZ' 'D',
                '' 'VBAK-FAKSK'	'03',
                '' 'VBKD-PRSDT'	data_ov,
                '' 'VBKD-ZTERM'	'Z150',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPLV45W',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_CURSOR'	'RV45A-MABNR(02)',
                '' 'BDC_SUBSCR' 'SAPMV45A'.

        call transaction 'VA02' using tl_bdc mode 'S' messages into tl_msg.
        wait up to 2 seconds.

        clear wg_estorno.
        clear:  wa_saida_03-dt_fatura,
                wa_saida_03-auart,
                wa_saida_03-waerk,
                wa_saida_03-nr_ov,
                wa_saida_03-docnum,
                wa_saida_03-fatura,
                wa_saida_03-vkorg,
                wa_saida_03-vtweg,
                wa_saida_03-spart,
                wa_saida_03-netpr,
                wa_saida_03-zterm,
                wa_saida_03-matnr_ov,
                wa_saida_03-waerk_fatura,
                wa_saida_03-tax_dolar,
                wa_saida_03-vlr_usd,
                wa_saida_03-vlr_brl.

        move-corresponding wa_saida_03 to wg_estorno.
        modify zsdt0225 from wg_estorno.

        refresh it_saida_03.

        perform z_busca_nf.

        call method g_grid_03->refresh_table_display
          exporting
            is_stable = wa_stable.
    endcase.
  endif.
endform.

form z_valida_ov_gerada.

  data: it_zsdt0225_ov type table of zsdt0225,
        wa_zsdt0225_ov type zsdt0225.



  read table tg_selectedrow into wg_selectedrow index 1.

  read table it_saida_03 into wa_saida_03 index   wg_selectedrow-index.

  select single *
    from zsdt0225 into wa_zsdt0225
   where id_seq eq wa_saida_03-id_seq.

  if wa_zsdt0225-fatura is initial and wa_zsdt0225-docnum  is initial and wa_zsdt0225-nr_ov is initial.

    wa_zsdt0225_ov-id_seq          = wa_zsdt0225-id_seq.
    wa_zsdt0225_ov-bukrs           = wa_zsdt0225-bukrs.
    wa_zsdt0225_ov-werks           = wa_zsdt0225-werks.
    wa_zsdt0225_ov-ano_viagem	     = wa_zsdt0225-ano_viagem.
    wa_zsdt0225_ov-safra           = wa_zsdt0225-safra.
    wa_zsdt0225_ov-cl_codigo       = wa_zsdt0225-cl_codigo.
    wa_zsdt0225_ov-cod_material	   = wa_zsdt0225-cod_material.
    wa_zsdt0225_ov-tp_class	       = wa_zsdt0225-tp_class.
    wa_zsdt0225_ov-nr_dco	         = wa_zsdt0225-nr_dco.
    wa_zsdt0225_ov-po_embarque     = wa_zsdt0225-po_embarque.
    wa_zsdt0225_ov-po_destino      = wa_zsdt0225-po_destino.
    wa_zsdt0225_ov-peso_vinculado  = wa_zsdt0225-peso_vinculado.
    wa_zsdt0225_ov-dt_fatura       = wa_saida_03-dt_fatura.
    wa_zsdt0225_ov-waerk           = wa_saida_03-waerk.
    wa_zsdt0225_ov-waerk_fatura    = wa_saida_03-waerk_fatura.
    wa_zsdt0225_ov-netpr           = wa_saida_03-netpr.
    wa_zsdt0225_ov-vlr_usd         = wa_saida_03-vlr_usd.
    wa_zsdt0225_ov-vlr_brl         = wa_saida_03-vlr_brl.
    wa_zsdt0225_ov-usuario         = sy-uname.
    wa_zsdt0225_ov-data_registro   = sy-datum.
    wa_zsdt0225_ov-hora_registro   = sy-uzeit.

    select single *  from zsdt0226 into @data(wa_t0226)  where emp_viagem     eq @wa_saida_03-bukrs
                                                          and  centro_viagem  eq @wa_saida_03-werks
                                                          and  po_embarque    eq @wa_saida_03-po_embarque
                                                          and  po_destino     eq @wa_saida_03-po_destino.

    wa_zsdt0225_ov-bukrs_serv   = wa_t0226-emp_fat_serv.
    wa_zsdt0225_ov-werks_serv   = wa_t0226-centro_fat_serv.

    select single  * from vbkd into @data(wa_vbkd)  where bstkd_e eq @wa_zsdt0225-id_seq
                                                    and   posnr   eq '000010'.
    if sy-subrc = 0.
      wa_zsdt0225_ov-nr_ov = wa_vbkd-vbeln.
      wa_zsdt0225_ov-zterm = wa_vbkd-zterm.

      select single * from vbfa into @data(wa_vbfa) where vbelv eq @wa_vbkd-vbeln
                                                     and  posnv eq @wa_vbkd-posnr.
      if sy-subrc = 0.
        wa_zsdt0225_ov-fatura = wa_vbfa-vbeln.
      endif.

      select single * from j_1bnflin into @data(wa_j_1bnflin)  where refkey eq @wa_vbfa-vbeln.
      if sy-subrc = 0.
        wa_zsdt0225_ov-docnum = wa_j_1bnflin-docnum.
      endif.


      select single * from vbrk into @data(wa_vbrk)  where vbeln eq @wa_vbfa-vbeln.
      if sy-subrc = 0.
        wa_zsdt0225_ov-tax_dolar = wa_vbrk-kurrf.
      endif.


      select single * from vbak into @data(wa_vbak) where vbeln eq @wa_vbkd-vbeln.
      if sy-subrc = 0.
        wa_zsdt0225_ov-auart = wa_vbak-auart.
        wa_zsdt0225_ov-vkorg = wa_vbak-vkorg.
        wa_zsdt0225_ov-vtweg = wa_vbak-vtweg.
        wa_zsdt0225_ov-spart = wa_vbak-spart.
      endif.

      select single * from vbap into @data(wa_vbap) where vbeln eq @wa_vbkd-vbeln
                                                     and  posnr eq @wa_vbkd-posnr.
      if sy-subrc = 0.
        wa_zsdt0225_ov-matnr_ov =  wa_vbap-matnr.
      endif.

      append wa_zsdt0225_ov to it_zsdt0225_ov.
      modify zsdt0225 from table it_zsdt0225_ov.
      commit work.
    endif.
  endif.
endform.


form f_preencher_dynpro using l_start type c l_name type c l_value.

  move l_start to wl_bdc-dynbegin.
  if l_start = 'X'.
    move:
  l_name  to wl_bdc-program,
  l_value to wl_bdc-dynpro.
  else.
    move:
      l_name  to wl_bdc-fnam,
      l_value to wl_bdc-fval.
  endif.
  append wl_bdc to tl_bdc.
  clear: wl_bdc.

endform.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_FORMA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_forma_bukrs  input.

  clear: wa_head-bukrs, wa_head-butxt.

  wa_head-bukrs = vbukrs.

  if wa_head-bukrs is not initial.

    select single butxt from t001 into wa_head-butxt
      where bukrs eq wa_head-bukrs.
  else.
    clear wa_head-butxt.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_FORMA_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_forma_werks input.

  clear: wa_head-werks, wa_head-name1.

  wa_head-werks = vwerks.

  if wa_head-werks is not initial.

    select single name1 from t001w into wa_head-name1
      where werks eq wa_head-werks.
  else.
    clear wa_head-name1.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_FORMA_BUKRS3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_forma_bukrs3 input.

  clear: wa_head_03-bukrs, wa_head_03-butxt.

  wa_head_03-bukrs = vbukrs3.

  if wa_head_03-bukrs is not initial.

    select single butxt from t001 into wa_head_03-butxt
      where bukrs eq wa_head_03-bukrs.
  else.
    clear wa_head_03-butxt.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_FORMA_WERKS3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_forma_werks3 input.

  clear: wa_head_03-werks, wa_head_03-name1.

  wa_head_03-werks = vwerks3.

  if wa_head_03-werks is not initial.

    select single name1 from t001w into wa_head_03-name1
      where werks eq wa_head_03-werks.
  else.
    clear wa_head_03-name1.
  endif.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  Z_GET_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form z_get_set .

  data: valor         type p decimals 2,
        text_out(255) type c.


  call function 'G_SET_GET_ALL_VALUES'
    exporting
      setnr         = 'ZSDR0108_DT_FATURA'
      class         = '0000'
    tables
      set_values    = it_value
    exceptions
      set_not_found = 1
      others        = 2.

  check it_value is not initial.

  clear r_dt_fatura.
  loop at it_value.
    r_dt_fatura-sign   = 'I'.
    r_dt_fatura-option = 'GE'.
    r_dt_fatura-low    = it_value-from.
    append r_dt_fatura.
  endloop.


  call function 'G_SET_GET_ALL_VALUES'
    exporting
      setnr         = 'MAGGI_ZSDT0158_VLR'
      class         = '0000'
    tables
      set_values    = it_value_01
    exceptions
      set_not_found = 1
      others        = 2.

  check it_value_01 is not initial.

  clear: r_vlr_brl, v_vlr_brl.
  loop at it_value_01.
    r_vlr_brl-sign    = 'I'.
    r_vlr_brl-option  = 'EQ'.

    text_out = it_value_01-from.
    do 5  times.
      replace  ',00' with ' ' into text_out.
    enddo.

    condense text_out no-gaps.
    move text_out to valor.
    r_vlr_brl-low = valor.
    v_vlr_brl = r_vlr_brl-low.
    condense v_vlr_brl no-gaps.
    append r_vlr_brl.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  F_ADD_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_add_selecao .

  data: it_saida_aux  type table of ty_saida_02.
  data: it_saida_del  type table of ty_saida_02.
  data: wl_saida_02  type ty_saida_02.

  free: tg_selectedrow, it_saida_aux, it_saida_del.

  call method g_grid_01->get_selected_rows
    importing
      et_index_rows = tg_selectedrow.

  if tg_selectedrow is initial.
    message 'Favor selecione uma linha!' type 'I'.
    exit.
  endif.

  delete tg_selectedrow  where rowtype is not initial .

  loop at tg_selectedrow into wg_selectedrow.

    read table it_saida_01 into wa_saida_01 index wg_selectedrow-index.
    if sy-subrc is initial.
      check wa_saida_01-emite_nfs is initial.
      move-corresponding wa_saida_01 to wl_saida_02.
      append wl_saida_02 to it_saida_aux.
      append wl_saida_02 to it_saida_del.
    endif.
  endloop.

  append lines of it_saida_02 to it_saida_del.
  sort it_saida_del by cl_codigo safra po_destino operacao cod_material tp_class po_embarque.
  delete adjacent duplicates from it_saida_del comparing cl_codigo safra po_destino operacao cod_material tp_class po_embarque.

  if lines( it_saida_del[] ) <= 1.
    append lines of it_saida_aux to it_saida_02.

    loop at tg_selectedrow into wg_selectedrow.
      read table it_saida_01 into wa_saida_01 index wg_selectedrow-index.
      if sy-subrc is initial.
        wa_saida_01-emite_nfs = abap_true.
        wa_saida_01-color = 'C510'.
        modify it_saida_01 from wa_saida_01 index wg_selectedrow-index.
      endif.
    endloop.

  else.
    message 'Os lançamentos devem ser do mesmo cliente, safra, material e operação!' type 'I'.
    exit.
  endif.

endform.
