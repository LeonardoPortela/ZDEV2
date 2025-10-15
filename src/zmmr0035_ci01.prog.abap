class lcl_utilities implementation. "Utilidades para todos as clases.

  method calculate_days_by_datum.
    rv_date = sy-datum - iv_number_of_days.
  endmethod.

  method remover_zeros_esquerda.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = iv_input
      importing
        output = ev_ouput.

  endmethod.

  method adicionar_zeros_esquerda.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = iv_input
      importing
        output = ev_ouput.

  endmethod.

endclass.

class lcl_coupa_integration_log implementation. "Classe responsável por administrar log de integração - Tabela ZINTEGRCOUPA01.

  method insert_new_log_key.

    data: ls_zintegrcoupa01 type zintegrcoupa01.

    free: ls_zintegrcoupa01.

    ls_zintegrcoupa01-id_integr  = iv_zcoupa_integration_key-id_integr.
    ls_zintegrcoupa01-ident_proc = iv_zcoupa_integration_key-ident_proc.
    ls_zintegrcoupa01-dt_atual   = sy-datum.
    ls_zintegrcoupa01-hr_atual   = sy-uzeit.
    ls_zintegrcoupa01-status     = space.
    append ls_zintegrcoupa01 to me->gt_zintegrcoupa01.

  endmethod.

  method check_existence_of_key.
    "ALRS
    data vforce(1).
    if s_chave-low is not initial.
      if s_lookup-low = 'OC'.
        if 'IM_OM_OI_CC' cs p_op_obj.
          vforce = 'X'.
        endif.
      endif.
    endif.
    "ALRS

    select count(*)
      from zintegrcoupa01
      where id_integr  = iv_zcoupa_integration_key-id_integr
      and   ident_proc = iv_zcoupa_integration_key-ident_proc
      and   status     = 'S'.
    if sy-subrc is initial and  vforce ne 'X'.
      rv_existence = 'X'.
    else.
      me->insert_new_log_key( iv_zcoupa_integration_key
                                   = iv_zcoupa_integration_key ).
    endif.

  endmethod.

  method set_executed_status.

    read table me->gt_zintegrcoupa01 assigning field-symbol(<zintegrcoupa0>) with key id_integr  = iv_zcoupa_integration_key-id_integr
                                                                                      ident_proc = iv_zcoupa_integration_key-ident_proc.
    if sy-subrc is initial.
      <zintegrcoupa0>-status = 'S'. "Status - Integração processa com sucesso.
    endif.

  endmethod.

  method save_log.

    check me->gt_zintegrcoupa01 is not initial.

    modify zintegrcoupa01 from table me->gt_zintegrcoupa01.

    commit work.

  endmethod.

endclass.

class lcl_process_lookup_values_e implementation. "Importar dados Empresa

  method constructor.

    if iv_days is not initial.
      me->gv_amount_days_ago = iv_days.
    else.
      me->gv_amount_days_ago = 1.
    endif.

    me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                 = me->gv_amount_days_ago ).

    create object me->if_process_lookup_values~go_coupa_integration_log.

  endmethod.

  method if_process_lookup_values~select_data.
    "Por enquanto esse processo será realizado manualmente, sem necessidade de implementação no momento,
    "porém a classe está disponível para implementação.
  endmethod.

  method if_process_lookup_values~build_out_data.
    "Por enquanto esse processo será realizado manualmente, sem necessidade de implementação no momento,
    "porém a classe está disponível para implementação.
  endmethod.

  method if_process_lookup_values~execute_process.

    me->if_process_lookup_values~select_data( ).
    me->if_process_lookup_values~build_out_data( ).

    et_import_data[] = me->if_process_lookup_values~gt_import_data[].

  endmethod.

endclass.

class lcl_process_lookup_values_cl implementation. "Importar dados Centro Logístico

  method constructor.

    if iv_days is not initial.
      me->gv_amount_days_ago = iv_days.
    else.
      me->gv_amount_days_ago = 1.
    endif.

    me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                 = me->gv_amount_days_ago ).

    create object me->if_process_lookup_values~go_coupa_integration_log.

  endmethod.

  method if_process_lookup_values~select_data.

    data: ls_integration_key type zcoupa_integration_key.
    data: rg_centro type range of t001w-werks,
          vg_centro type t001w-werks.


    loop at s_chave into data(w_chave).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = w_chave-low
        importing
          output = vg_centro.
      append value #( sign = 'I' option = 'EQ' low = vg_centro ) to rg_centro.
    endloop.

    "O Centro de Custo será criado no Coupa de forma manual, sendo necessário ser integrado os aprovadores dos campos customizados via tabela Z.
    if p_op_obj = 'CL' and
       s_chave[] is not initial.
      "O Centro Logístico será criado no Coupa de forma manual, sendo necessário ser integrado os aprovadores dos campos customizados via tabela Z.
      select tp_estrat, kostl, werks,
             aprovador, name1, tp_oper, nivel
        from zprovcoupa01
        where werks     in @rg_centro
        and   tp_estrat eq 'E'
        into table @data(gt_zprovcoupa01).
    endif.
    "
    if rg_centro[] is initial.
      select tp_estrat, kostl, werks,
              aprovador, name1, tp_oper, nivel
         from zprovcoupa01
         where status    eq 'S'
         and   tp_estrat eq 'E'
         into table @gt_zprovcoupa01.
    endif.

    if gt_zprovcoupa01[] is not initial.
      sort: gt_zprovcoupa01 by tp_estrat kostl werks nivel.

      loop at gt_zprovcoupa01 into data(gs_zprovcoupa01).
        clear: gs_coupa_cl, ls_integration_key.

        "<active>
        gs_coupa_cl-active = 'true'.
        "<name>
        concatenate gs_zprovcoupa01-werks '-' gs_zprovcoupa01-name1 into gs_coupa_cl-name separated by space.

        "Verificar se o registro já está processado anteriormente
        read table gt_coupa_cl with key name = gs_coupa_cl-name transporting no fields.
        if sy-subrc is initial.
          continue.
        endif.

        "<external-ref-num>
        gs_coupa_cl-external_ref_num  = gs_zprovcoupa01-werks.
***==========================
        "<parent> - > </external-ref-code>
*        gs_coupa_cl-parent-external_ref_code = gs_zprovcoupa01-werks.
        "<lookup> -> <name>.
***==========================
        gs_coupa_cl-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.
        "<custom-fields> -> <processo-suprimentos>
        gs_coupa_cl-custom_fields-processo_suprimentos = gs_zprovcoupa01-tp_oper.

        read table gt_zprovcoupa01 into data(ls_zprovcoupa01_nivel) with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                             kostl     = gs_zprovcoupa01-kostl
                                                                             werks     = gs_zprovcoupa01-werks
                                                                             nivel     = '1' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n1>
          gs_coupa_cl-custom_fields-n1-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '2' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n2>
          gs_coupa_cl-custom_fields-n2-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '3' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n3>
          gs_coupa_cl-custom_fields-n3-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '4' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n4>
          gs_coupa_cl-custom_fields-n4-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '5' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n5>
          gs_coupa_cl-custom_fields-n5-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '6' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n6>
          gs_coupa_cl-custom_fields-n6-login = ls_zprovcoupa01_nivel-aprovador.
        endif.


        append gs_coupa_cl to gt_coupa_cl.
      endloop.

      free: gt_zprovcoupa01, gs_zprovcoupa01, ls_integration_key, gs_coupa_cl, ls_zprovcoupa01_nivel.
    endif.

  endmethod.

  method if_process_lookup_values~build_out_data.

    data: ls_import_data type zcoupa_import_data.

    loop at me->gt_coupa_cl into gs_coupa_cl.
      clear: ls_import_data.

      call transformation zt_coupa_cl
        source lookup_value = gs_coupa_cl
        result xml ls_import_data-xml
        options xml_header = 'no'.

      check sy-subrc is initial.

      ls_import_data-tp_referencia = 'AE_COUPA_CL'.
      ls_import_data-id_referencia = gs_coupa_cl-name.
      append ls_import_data to me->if_process_lookup_values~gt_import_data.
    endloop.

    free: ls_import_data.

  endmethod.

  method if_process_lookup_values~execute_process.

    me->if_process_lookup_values~select_data( ).
    me->if_process_lookup_values~build_out_data( ).

    et_import_data[] = me->if_process_lookup_values~gt_import_data[].

  endmethod.

endclass.

class lcl_process_lookup_values_ccc implementation. "Importar dados Categoria de Classificação Contábil

  method constructor.

    if iv_days is not initial.
      me->gv_amount_days_ago = iv_days.
    else.
      me->gv_amount_days_ago = 1.
    endif.

    me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                 = me->gv_amount_days_ago ).

    create object me->if_process_lookup_values~go_coupa_integration_log.

  endmethod.

  method if_process_lookup_values~select_data.
    "Por enquanto esse processo será realizado manualmente, sem necessidade de implementação no momento,
    "porém a classe está disponível para implementação.
  endmethod.

  method if_process_lookup_values~build_out_data.
    "Por enquanto esse processo será realizado manualmente, sem necessidade de implementação no momento,
    "porém a classe está disponível para implementação.
  endmethod.

  method if_process_lookup_values~execute_process.

    me->if_process_lookup_values~select_data( ).
    me->if_process_lookup_values~build_out_data( ).

    et_import_data[] = me->if_process_lookup_values~gt_import_data[].

  endmethod.

endclass.

class lcl_process_lookup_values_cc implementation. "Importar dados Centro de Custo

  method constructor.

    if iv_days is not initial.
      me->gv_amount_days_ago = iv_days.
    else.
      me->gv_amount_days_ago = 1.
    endif.

    me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                 = me->gv_amount_days_ago ).

    create object me->if_process_lookup_values~go_coupa_integration_log.

  endmethod.

  method if_process_lookup_values~select_data.

    data: ls_integration_key type zcoupa_integration_key.
    data: rg_centro_custo type range of csks-kostl,
          vg_centro_custo type csks-kostl.


    loop at s_chave into data(w_chave).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = w_chave-low
        importing
          output = vg_centro_custo.
      append value #( sign = 'I' option = 'EQ' low = vg_centro_custo ) to rg_centro_custo.
    endloop.

    "O Centro de Custo será criado no Coupa de forma manual, sendo necessário ser integrado os aprovadores dos campos customizados via tabela Z.
    if p_op_obj = 'CC' and
       s_chave[] is not initial.
      select tp_estrat, kostl, werks,
             aprovador, name1, tp_oper, nivel, ltext
        from zprovcoupa01
        where kostl     in @rg_centro_custo
        and   tp_estrat = 'K'
        into table @data(gt_zprovcoupa01).
    endif.
    if rg_centro_custo[] is initial.
      select tp_estrat, kostl, werks,
             aprovador, name1, tp_oper, nivel, ltext
        from zprovcoupa01
        where status    eq 'S'
        and   tp_estrat = 'K'
        into table @gt_zprovcoupa01.
    endif.

    if gt_zprovcoupa01[] is not initial.
      sort: gt_zprovcoupa01 by tp_estrat kostl werks nivel.

      loop at gt_zprovcoupa01 into data(gs_zprovcoupa01).

        lcl_utilities=>remover_zeros_esquerda( exporting
                                                iv_input = gs_zprovcoupa01-kostl
                                               importing
                                                ev_ouput = gs_zprovcoupa01-kostl ).
        "<active>
        gs_coupa_cc-active = 'true'.
        "<name>
        concatenate gs_zprovcoupa01-kostl '-' gs_zprovcoupa01-ltext into gs_coupa_cc-name separated by space.

        "Verificar se o registro já está processado anteriormente
        read table gt_coupa_cc with key name = gs_coupa_cc-name transporting no fields.
        if sy-subrc is initial.
          continue.
        endif.

        "<external-ref-num>
        gs_coupa_cc-external_ref_num  = gs_zprovcoupa01-kostl.
        "<lookup> -> <name>.
        gs_coupa_cc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.
        "<custom-fields> -> <processo-suprimentos>
        gs_coupa_cc-custom_fields-processo_suprimentos = gs_zprovcoupa01-tp_oper.

        lcl_utilities=>adicionar_zeros_esquerda( exporting
                                                  iv_input = gs_zprovcoupa01-kostl
                                                 importing
                                                  ev_ouput = gs_zprovcoupa01-kostl ).

        read table gt_zprovcoupa01 into data(ls_zprovcoupa01_nivel) with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                             kostl     = gs_zprovcoupa01-kostl
                                                                             werks     = gs_zprovcoupa01-werks
                                                                             nivel     = '1' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n1>
          gs_coupa_cc-custom_fields-n1-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '2' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n2>
          gs_coupa_cc-custom_fields-n2-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '3' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n3>
          gs_coupa_cc-custom_fields-n3-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '4' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n4>
          gs_coupa_cc-custom_fields-n4-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '5' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n5>
          gs_coupa_cc-custom_fields-n5-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        free: ls_zprovcoupa01_nivel.
        read table gt_zprovcoupa01 into ls_zprovcoupa01_nivel with key tp_estrat = gs_zprovcoupa01-tp_estrat
                                                                       kostl     = gs_zprovcoupa01-kostl
                                                                       werks     = gs_zprovcoupa01-werks
                                                                       nivel     = '6' binary search transporting aprovador.
        if sy-subrc is initial.
          "<custom-fields> -> <n6>
          gs_coupa_cc-custom_fields-n6-login = ls_zprovcoupa01_nivel-aprovador.
        endif.

        split 'K/A/H/FI/FM' at '/' into table data(lt_classifications).
        "É necessário enviar o mesmo centro de custo e nível para cada classificação acima.
        loop at lt_classifications into data(ls_classifications).

          "<parent> -> <external-ref-code>
          concatenate gs_zprovcoupa01-werks '|' ls_classifications into gs_coupa_cc-parent-external_ref_code.

          append gs_coupa_cc to gt_coupa_cc.
        endloop.

        free: gs_coupa_cc, ls_classifications, lt_classifications, ls_integration_key, ls_zprovcoupa01_nivel.

      endloop.

      free: gt_zprovcoupa01, gs_zprovcoupa01.
    endif.

  endmethod.

  method if_process_lookup_values~build_out_data.

    data: ls_import_data type zcoupa_import_data.

    loop at me->gt_coupa_cc into gs_coupa_cc.
      clear: ls_import_data.

      call transformation zt_coupa_cc
        source lookup_value = gs_coupa_cc
        result xml ls_import_data-xml
        options xml_header = 'no'.

      check sy-subrc is initial.

      ls_import_data-tp_referencia = 'AK_COUPA_CC'.
      ls_import_data-id_referencia = gs_coupa_cc-external_ref_num. "Código Centro de Custo
      append ls_import_data to me->if_process_lookup_values~gt_import_data.
    endloop.

    free: ls_import_data.

  endmethod.

  method if_process_lookup_values~execute_process.

    me->if_process_lookup_values~select_data( ).
    me->if_process_lookup_values~build_out_data( ).

    et_import_data[] = me->if_process_lookup_values~gt_import_data[].

  endmethod.

endclass.

class lcl_process_lookup_values_oc implementation. "Importar dados Objeto Contábil

  method constructor.

    if iv_days is not initial.
      me->gv_amount_days_ago = iv_days.
    else.
      me->gv_amount_days_ago = 1.
    endif.

    me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                 = me->gv_amount_days_ago ).

    create object me->if_process_lookup_values~go_coupa_integration_log.

    clear: rg_imobilizado, rg_ordem_manu, rg_ordem_inve, rg_centro_custo.

    loop at s_chave into data(l_chave).
      condense l_chave-low no-gaps.
      case p_op_obj.
        when 'IM'.
          data: l_anln1 type anla-anln1.
          split l_chave-low at '-' into l_anln1 data(l_anln2).
          l_anln1 = |{ l_anln1 alpha = in }|.
          select count(*)
            from anla
            where anln1 = l_anln1.
          if sy-subrc is initial.
            append value #( sign = 'I' option = 'EQ' low = l_anln1 ) to rg_imobilizado.
          else.
            message text-m01 type 'I'.
            call transaction 'ZMM0199'.
          endif.
        when 'OM'.
          data: l_aufnr type coas-aufnr.

          l_aufnr     = l_chave-low.
          l_aufnr     = |{ l_aufnr alpha = in }|.
*          SELECT COUNT(*)
*            FROM coas
*            WHERE aufnr = l_aufnr.
          select single *
           into @data(w_coas)
           from coas
           where aufnr = @l_aufnr.
          if sy-subrc is initial.
            if w_coas-phas1 is initial.
              if sy-batch ne 'X'.
                message text-m05 type 'I'.
                call transaction 'ZMM0199'.
              else.
                append value #( sign = 'I' option = 'EQ' low = 'ERRO' ) to rg_ordem_manu.
              endif.
            else.
              append value #( sign = 'I' option = 'EQ' low = l_aufnr ) to rg_ordem_manu.
            endif.
          else.
            if sy-batch ne 'X'.
              message text-m02 type 'I'.
              call transaction 'ZMM0199'.
            else.
              append value #( sign = 'I' option = 'EQ' low = 'ERRO' ) to rg_ordem_manu.
            endif.
          endif.
        when 'OI'.

          l_aufnr     = l_chave-low.
          l_aufnr     = |{ l_aufnr alpha = in }|.
*          SELECT COUNT(*)
*            FROM coas
*            WHERE aufnr = l_aufnr.
          select single *
            into @data(w_coas2)
            from coas
            where aufnr = @l_aufnr.
          if sy-subrc is initial.
            if w_coas2-phas1 is initial.
              if sy-batch ne 'X'.
                message text-m05 type 'I'.
                call transaction 'ZMM0199'.
              else.
                append value #( sign = 'I' option = 'EQ' low = 'ERRO' ) to rg_ordem_inve.
              endif.
            else.
              append value #( sign = 'I' option = 'EQ' low = l_aufnr ) to rg_ordem_inve.
            endif.
          else.
            if sy-batch ne 'X'.
              message text-m03 type 'I'.
              call transaction 'ZMM0199'.
             ELSE.
              append value #( sign = 'I' option = 'EQ' low = 'ERRO' ) to rg_ordem_inve.
            endif.
          endif.
        when 'CC'.
          data: l_kostl type csks-kostl.

          l_kostl     = l_chave-low.
          l_kostl     = |{ l_kostl alpha = in }|.
          select count(*)
            from csks
            where kostl = l_kostl.
          if sy-subrc is initial.
            append value #( sign = 'I' option = 'EQ' low = l_kostl ) to rg_centro_custo.
          else.
            message text-m04 type 'I'.
            call transaction 'ZMM0199'.
          endif.
        when others.
      endcase.
    endloop.

  endmethod.

  method if_process_lookup_values~select_data.

    data: ls_integration_key type zcoupa_integration_key.

    data: l_cc_create type xflag.
*-CS1099462-#RIMINI-06.01.2023-BEGIN
    data: l_imob_filt  type anla-anln1,
          rl_imob_filt type range of anla-anln1.

*-  Buscar na tabela de parâmetros filtro dos imobilizados
    select * from tvarvc
       into table @data(tl_imob_filt)
            where name eq 'ZMMR0035_COUPA_FILTRO_IMOB'.

*-  Montar range
    loop at tl_imob_filt assigning field-symbol(<fs_imob_filt>).
      concatenate <fs_imob_filt>-low '*'
             into l_imob_filt.

      append value #( sign = 'I' option = 'CP' low = l_imob_filt )
                to rl_imob_filt.
    endloop.
*-CS1099462-#RIMINI-06.01.2023-END
    if p_op_obj is initial or p_op_obj eq 'IM'.
      if rg_imobilizado is initial.
        "Imobilizados criados.
        select anla~bukrs,anla~anln1, anla~anln2, anla~txt50,
               anlz~kostl, anlz~gsber
          into table @data(gt_anla_created)
          from anla
          inner join anlz
          on    anla~bukrs = anlz~bukrs
          and   anla~anln1 = anlz~anln1
          and   anla~anln2 = anlz~anln2
          where erdat      ge @me->gv_base_date
          and exists ( select *
                        from  setleaf
                        where setname = 'MAGGI_EMPRESAS_COUPA'
                        and   valfrom = anla~bukrs ).
      else.
        "Imobilizados criados.
        select anla~bukrs anla~anln1 anla~anln2 anla~txt50
               anlz~kostl anlz~gsber
          into table gt_anla_created
          from anla
          inner join anlz
          on    anla~bukrs = anlz~bukrs
          and   anla~anln1 = anlz~anln1
          and   anla~anln2 = anlz~anln2
          where anla~anln1 in rg_imobilizado
          and exists ( select *
                        from  setleaf
                        where setname = 'MAGGI_EMPRESAS_COUPA'
                        and   valfrom = anla~bukrs ).
      endif.
    endif.
*-CS1099462-#RIMINI-06.01.2023-BEGIN
*-  Elimina os imobilizados que não devem subir para o COUPA
    if not rl_imob_filt[] is initial.
      delete gt_anla_created where anln1 in rl_imob_filt.

    endif.
*-CS1099462-#RIMINI-06.01.2023-END
    loop at gt_anla_created into data(ls_anla_created).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_created-anln1
                                             importing
                                              ev_ouput = ls_anla_created-anln1 ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_created-anln2
                                             importing
                                              ev_ouput = ls_anla_created-anln2 ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_created-kostl
                                             importing
                                              ev_ouput = ls_anla_created-kostl ).
      "<active>
      gs_coupa_oc-active = 'true'.
      "<name>
      concatenate ls_anla_created-anln1 '-' ls_anla_created-anln2 '-' ls_anla_created-txt50 into gs_coupa_oc-name separated by space.

      "<external-ref-num>
*      CONCATENATE ls_anla_created-anln1 '-' ls_anla_created-anln2 INTO gs_coupa_oc-external_ref_num SEPARATED BY space. ALRS
      concatenate ls_anla_created-bukrs ls_anla_created-anln1 '-' ls_anla_created-anln2 into gs_coupa_oc-external_ref_num separated by space.

      "<parent> -> <external-ref-code>
      concatenate ls_anla_created-gsber '|' 'A' '|' ls_anla_created-kostl into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'IM'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num .

      check me->if_process_lookup_values~go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
                                                                                              = ls_integration_key ) eq abap_false.

      concatenate 'IM' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_anla_created, ls_anla_created, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial.
      "Imobilizado alterados.
      select anla~anln1, anla~anln2, anla~txt50,
             anla~bukrs, anlz~kostl, anlz~gsber
        into table @data(gt_anla_modified)
        from anla
        inner join anlz
        on    anla~bukrs = anlz~bukrs
        and   anla~anln1 = anlz~anln1
        and   anla~anln2 = anlz~anln2
        where aedat      ge @me->gv_base_date
        and exists ( select *
                      from  setleaf
                      where setname = 'MAGGI_EMPRESAS_COUPA'
                      and   valfrom = anla~bukrs ).
    endif.
*-CS1099462-#RIMINI-06.01.2023-BEGIN
*-  Elimina os imobilizados que não devem subir para o COUPA
    if not rl_imob_filt[] is initial.
      delete gt_anla_modified where anln1 in rl_imob_filt.

    endif.
*-CS1099462-#RIMINI-06.01.2023-END
    loop at gt_anla_modified into data(ls_anla_modified).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_modified-anln1
                                             importing
                                              ev_ouput = ls_anla_modified-anln1 ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_modified-anln2
                                             importing
                                              ev_ouput = ls_anla_modified-anln2 ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_modified-kostl
                                             importing
                                              ev_ouput = ls_anla_modified-kostl ).
      "<active>
      gs_coupa_oc-active = 'true'.
      "<name>
      concatenate ls_anla_modified-anln1 '-' ls_anla_modified-anln2 '-' ls_anla_modified-txt50 into gs_coupa_oc-name separated by space.

      "<external-ref-num>
*      CONCATENATE ls_anla_modified-anln1 '-' ls_anla_modified-anln2 INTO gs_coupa_oc-external_ref_num SEPARATED BY space. ALRS
      concatenate ls_anla_modified-bukrs ls_anla_modified-anln1 '-' ls_anla_modified-anln2 into gs_coupa_oc-external_ref_num separated by space.

      "<parent> -> <external-ref-code>
      concatenate ls_anla_modified-gsber '|' 'A' '|' ls_anla_modified-kostl into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'IM'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num.
      concatenate 'IM' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_anla_modified, ls_anla_modified, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial.
      "Imobilizados eliminados.
      select anla~anln1, anla~anln2, anla~txt50,
             anla~bukrs, anlz~kostl, anlz~gsber, anlz~werks
        into table @data(gt_anla_deactivated)
        from anla
        inner join anlz
        on    anla~bukrs = anlz~bukrs
        and   anla~anln1 = anlz~anln1
        and   anla~anln2 = anlz~anln2
        where deakt      ge @me->gv_base_date
        and exists ( select *
                      from  setleaf
                      where setname = 'MAGGI_EMPRESAS_COUPA'
                      and   valfrom = anla~bukrs ).
    endif.
*-CS1099462-#RIMINI-06.01.2023-BEGIN
*-  Elimina os imobilizados que não devem subir para o COUPA
    if not rl_imob_filt[] is initial.
      delete gt_anla_deactivated where anln1 in rl_imob_filt.

    endif.
*-CS1099462-#RIMINI-06.01.2023-END
    loop at gt_anla_deactivated into data(ls_anla_deactivated).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_deactivated-anln1
                                             importing
                                              ev_ouput = ls_anla_deactivated-anln1 ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_deactivated-anln2
                                             importing
                                              ev_ouput = ls_anla_deactivated-anln2 ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_anla_deactivated-kostl
                                             importing
                                              ev_ouput = ls_anla_deactivated-kostl ).
      "<active>
      gs_coupa_oc-active = 'false'.
      "<name>
      concatenate ls_anla_deactivated-anln1 '-' ls_anla_deactivated-anln2 '-' ls_anla_deactivated-txt50 into gs_coupa_oc-name separated by space.

      "<external-ref-num>
*      CONCATENATE ls_anla_deactivated-anln1 '-' ls_anla_deactivated-anln2 INTO gs_coupa_oc-external_ref_num SEPARATED BY space. ALRS
      concatenate ls_anla_deactivated-bukrs ls_anla_deactivated-anln1 '-' ls_anla_deactivated-anln2 into gs_coupa_oc-external_ref_num separated by space.

      "<parent> -> <external-ref-code>
      concatenate ls_anla_deactivated-gsber '|' 'A' '|' ls_anla_deactivated-kostl into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'IM'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num.
      concatenate 'IM' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_anla_deactivated, ls_anla_deactivated, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial or p_op_obj eq 'OM'.
*    IF  p_op_obj EQ 'OM'.
      if rg_ordem_manu is initial.
        "Ordem de manutenção criadas.
        select  aufnr, werks, kostl, ktext, bukrs
          into table @data(gt_coas_created)
          from coas
          where autyp = '30'
          and   phas1 = 'X'
          and   erdat ge @me->gv_base_date
          and exists ( select *
                        from  setleaf
                        where setname = 'MAGGI_EMPRESAS_COUPA'
                        and   valfrom = coas~bukrs ).
      else.
        "Ordem de manutenção criadas.
        select  aufnr werks kostl ktext bukrs
          into table gt_coas_created
          from coas
          where autyp = '30'
          and   phas1 = 'X'
          and   aufnr in rg_ordem_manu
          and exists ( select *
                        from  setleaf
                        where setname = 'MAGGI_EMPRESAS_COUPA'
                        and   valfrom = coas~bukrs ).
      endif.
    endif.

    loop at gt_coas_created into data(ls_coas_created).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_created-aufnr
                                             importing
                                              ev_ouput = ls_coas_created-aufnr ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_created-kostl
                                             importing
                                              ev_ouput = ls_coas_created-kostl ).
      "<active>
      gs_coupa_oc-active = 'true'.
      "<name>
      concatenate ls_coas_created-aufnr '-' ls_coas_created-ktext into gs_coupa_oc-name separated by space.
      "<external-ref-num>
      gs_coupa_oc-external_ref_num  = ls_coas_created-aufnr.
      "<parent> -> <external-ref-code>
      concatenate ls_coas_created-werks '|' 'FM' '|' ls_coas_created-kostl into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'OM'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num .

      check me->if_process_lookup_values~go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
                                                                                              = ls_integration_key ) eq abap_false.

      concatenate 'OM' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_coas_created, ls_coas_created, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial.
*    IF  p_op_obj EQ 'OM'.
      "Ordem de manutenção modificadas
      select  aufnr, werks, kostl, ktext, bukrs
        into table @data(gt_coas_modified)
        from coas
        where autyp = '30'
        and   phas1 = 'X'
        and   aedat ge @me->gv_base_date
        and exists ( select *
                      from  setleaf
                      where setname = 'MAGGI_EMPRESAS_COUPA'
                      and   valfrom = coas~bukrs ).
    endif.

    loop at gt_coas_modified into data(ls_coas_modified).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_modified-aufnr
                                             importing
                                              ev_ouput = ls_coas_modified-aufnr ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_modified-kostl
                                             importing
                                              ev_ouput = ls_coas_modified-kostl ).
      "<active>
      gs_coupa_oc-active = 'true'.
      "<name>
      concatenate ls_coas_modified-aufnr '-' ls_coas_modified-ktext into gs_coupa_oc-name separated by space.
      "<external-ref-num>
      gs_coupa_oc-external_ref_num  = ls_coas_modified-aufnr.
      "<parent> -> <external-ref-code>
      concatenate ls_coas_modified-werks '|' 'FM' '|' ls_coas_modified-kostl into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'OM'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num.

      me->if_process_lookup_values~go_coupa_integration_log->insert_new_log_key( iv_zcoupa_integration_key
                                                                                              = ls_integration_key ).

      concatenate 'OM' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_coas_modified, ls_coas_modified, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial.
*    IF  p_op_obj EQ 'OM'.
      "Ordem de manutenção desativadas
      select  aufnr, werks, kostl, ktext, bukrs
        into table @data(gt_coas_deactivated)
        from coas
        where autyp = '30'
        and   phas1 = @space
        and   aedat ge @me->gv_base_date
        and exists ( select *
                      from  setleaf
                      where setname = 'MAGGI_EMPRESAS_COUPA'
                      and   valfrom = coas~bukrs ).
    endif.

    loop at gt_coas_deactivated into data(ls_coas_deactivated).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_deactivated-aufnr
                                             importing
                                              ev_ouput = ls_coas_deactivated-aufnr ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_deactivated-kostl
                                             importing
                                              ev_ouput = ls_coas_deactivated-kostl ).
      "<active>
      gs_coupa_oc-active = 'false'.
      "<name>
      concatenate ls_coas_deactivated-aufnr '-' ls_coas_deactivated-ktext into gs_coupa_oc-name separated by space.
      "<external-ref-num>
      gs_coupa_oc-external_ref_num  = ls_coas_deactivated-aufnr.
      "<parent> -> <external-ref-code>
      concatenate ls_coas_deactivated-werks '|' 'FM' '|' ls_coas_deactivated-kostl into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'OM'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num.
      concatenate 'OM' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_coas_deactivated, ls_coas_deactivated, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial or p_op_obj eq 'OI'.
      if rg_ordem_inve is initial.
        "Ordem de Investimentos criadas.
        select  aufnr, werks, kostv, ktext, bukrs
          into table @data(gt_coas_invest_created)
          from coas
          where autyp = '01'
          and   phas1 = 'X'
          and   erdat ge @me->gv_base_date
          and exists ( select *
                        from  setleaf
                        where setname = 'MAGGI_EMPRESAS_COUPA'
                        and   valfrom = coas~bukrs ).
      else.
        "Ordem de Investimentos criadas.
        select  aufnr werks kostv ktext bukrs
          into table gt_coas_invest_created
          from coas
          where autyp = '01'
          and   phas1 = 'X'
          and   aufnr in rg_ordem_inve
          and exists ( select *
                        from  setleaf
                        where setname = 'MAGGI_EMPRESAS_COUPA'
                        and   valfrom = coas~bukrs ).
      endif.
    endif.

    loop at gt_coas_invest_created into data(ls_coas_invest_created).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_invest_created-aufnr
                                             importing
                                              ev_ouput = ls_coas_invest_created-aufnr ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_invest_created-kostv
                                             importing
                                              ev_ouput = ls_coas_invest_created-kostv ).
      "<active>
      gs_coupa_oc-active = 'true'.
      "<name>
      concatenate ls_coas_invest_created-aufnr '-' ls_coas_invest_created-ktext into gs_coupa_oc-name separated by space.
      "<external-ref-num>
      gs_coupa_oc-external_ref_num  = ls_coas_invest_created-aufnr.
      "<parent> -> <external-ref-code>
      concatenate ls_coas_invest_created-werks '|' 'FI' '|' ls_coas_invest_created-kostv into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'OI'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num .

      check me->if_process_lookup_values~go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
                                                                                              = ls_integration_key ) eq abap_false.
      concatenate 'OI' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_coas_invest_created, ls_coas_invest_created, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial.
      "Ordem de Investimentos modificadas.
      select  aufnr, werks, kostv, ktext, bukrs
        into table @data(gt_coas_invest_modified)
        from coas
        where autyp = '01'
        and   phas1 = 'X'
        and   aedat ge @me->gv_base_date
        and exists ( select *
                      from  setleaf
                      where setname = 'MAGGI_EMPRESAS_COUPA'
                      and   valfrom = coas~bukrs ).
    endif.

    loop at gt_coas_invest_modified into data(ls_coas_invest_modified).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_invest_modified-aufnr
                                             importing
                                              ev_ouput = ls_coas_invest_modified-aufnr ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_invest_modified-kostv
                                             importing
                                              ev_ouput = ls_coas_invest_modified-kostv ).
      "<active>
      gs_coupa_oc-active = 'true'.
      "<name>
      concatenate ls_coas_invest_modified-aufnr '-' ls_coas_invest_modified-ktext into gs_coupa_oc-name separated by space.
      "<external-ref-num>
      gs_coupa_oc-external_ref_num  = ls_coas_invest_modified-aufnr.
      "<parent> -> <external-ref-code>
      concatenate ls_coas_invest_modified-werks '|' 'FI' '|' ls_coas_invest_modified-kostv into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'OI'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num.
      concatenate 'OI' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_coas_invest_modified, ls_coas_invest_modified, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial.
      "Ordem de Investimentos desativadas
      select  aufnr, werks, kostv, ktext, bukrs
        into table @data(gt_coas_invest_deactivated)
        from coas
        where autyp = '01'
        and   phas1 = @space
        and   aedat ge @me->gv_base_date
        and exists ( select *
                      from  setleaf
                      where setname = 'MAGGI_EMPRESAS_COUPA'
                      and   valfrom = coas~bukrs ).
    endif.

    loop at gt_coas_invest_deactivated into data(ls_coas_invest_deactivated).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_invest_deactivated-aufnr
                                             importing
                                              ev_ouput = ls_coas_invest_deactivated-aufnr ).

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_coas_invest_deactivated-kostv
                                             importing
                                              ev_ouput = ls_coas_invest_deactivated-kostv ).
      "<active>
      gs_coupa_oc-active = 'false'.
      "<name>
      concatenate ls_coas_invest_deactivated-aufnr '-' ls_coas_invest_deactivated-ktext into gs_coupa_oc-name separated by space.
      "<external-ref-num>
      gs_coupa_oc-external_ref_num  = ls_coas_invest_deactivated-aufnr.
      "<parent> -> <external-ref-code>
      concatenate ls_coas_invest_deactivated-werks '|' 'FI' '|' ls_coas_invest_deactivated-kostv into gs_coupa_oc-parent-external_ref_code.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      ls_integration_key-ident_proc = 'OI'.
      ls_integration_key-id_integr  = gs_coupa_oc-external_ref_num.
      concatenate 'OI' gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

      append gs_coupa_oc to gt_coupa_oc.
    endloop.

    free: gt_coas_invest_deactivated, ls_coas_invest_deactivated, ls_integration_key, gs_coupa_oc.

    if p_op_obj is initial or p_op_obj eq 'CC'.
      if rg_centro_custo is initial.
        "Centros de Custo criados.
        select csks~kostl, csks~kokrs, csks~gsber,
               cskt~ltext, csks~bukrs
          into table @data(gt_csks_created)
          from  csks inner join cskt
          on    csks~kokrs = cskt~kokrs
          and   csks~kostl = cskt~kostl
          where csks~ersda ge @me->gv_base_date
          and   cskt~spras = @sy-langu
          and exists ( select *
                        from  setleaf
                        where setname = 'MAGGI_EMPRESAS_COUPA'
                        and   valfrom = csks~bukrs ).
      else.
        "Centros de Custo criados.
        select csks~kostl csks~kokrs csks~gsber
               cskt~ltext csks~bukrs
          into table gt_csks_created
          from  csks inner join cskt
          on    csks~kokrs = cskt~kokrs
          and   csks~kostl = cskt~kostl
          where csks~kostl in rg_centro_custo
          and   cskt~spras = sy-langu
          and exists ( select *
                        from  setleaf
                        where setname = 'MAGGI_EMPRESAS_COUPA'
                        and   valfrom = csks~bukrs ).
      endif.
    endif.

    loop at gt_csks_created into data(ls_csks_created).
      clear: gs_coupa_oc, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_csks_created-kostl
                                             importing
                                              ev_ouput = ls_csks_created-kostl ).
      "<active>
      gs_coupa_oc-active = 'true'.
      "<name>
      concatenate ls_csks_created-kostl '-' ls_csks_created-ltext into gs_coupa_oc-name separated by space.
      "<external-ref-num>
      gs_coupa_oc-external_ref_num  = ls_csks_created-kostl.
      "<lookup> -> <name>.
      gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

      split 'K/A/H/FI/FM' at '/' into table data(lt_classifications).
      "É necessário enviar o mesmo centro de custo e nível para cada classificação acima.
      loop at lt_classifications into data(ls_classifications).
        clear: gs_coupa_oc-external_ref_num.
        gs_coupa_oc-external_ref_num = ls_csks_created-kostl.

        "<parent> -> <external-ref-code>
        concatenate ls_csks_created-gsber '|' ls_classifications into gs_coupa_oc-parent-external_ref_code.

        ls_integration_key-id_integr  = ls_classifications && gs_coupa_oc-external_ref_num. "Código Centro de Custo
        ls_integration_key-ident_proc = 'CC'.

        concatenate 'CC' ls_classifications gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.

        check me->if_process_lookup_values~go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
                                                                                            = ls_integration_key ) eq abap_false.

        append gs_coupa_oc to gt_coupa_oc.
      endloop.

    endloop.

    free: gt_csks_created, ls_csks_created, ls_integration_key.

    if p_op_obj is initial.

      "Centros de Custo modificados.
      select objectid
        into table @data(gt_cdhdr_cc)
        from cdhdr
        where objectclas = 'KOSTL'
        and   tcode      = 'KS02'
        and   udate      ge @me->gv_base_date.
      if sy-subrc is initial.
        loop at gt_cdhdr_cc into data(ls_cdhdr_cc).
          append value #( kokrs = ls_cdhdr_cc-objectid(04) kostl = ls_cdhdr_cc-objectid+05 ) to gt_csks.
        endloop.

        if gt_csks is not initial.

          loop at gt_csks assigning field-symbol(<cks>).
            data(l_kostl) = <cks>-kostl.

            lcl_utilities=>adicionar_zeros_esquerda( exporting
                                                    iv_input = l_kostl
                                                   importing
                                                    ev_ouput = l_kostl ).

            <cks>-kostl = l_kostl.

          endloop.


          select csks~kostl, csks~kokrs, csks~gsber, csks~bkzkp, csks~bkzks,
                 cskt~ltext, csks~bukrs
            into table @data(gt_csks_modified)
            from  csks inner join cskt
            on    csks~kokrs = cskt~kokrs
            and   csks~kostl = cskt~kostl
            for all entries in @gt_csks
            where csks~kokrs = @gt_csks-kokrs
            and   csks~kostl = @gt_csks-kostl
            and   cskt~spras = @sy-langu
            and exists ( select *
                          from  setleaf
                          where setname = 'MAGGI_EMPRESAS_COUPA'
                          and   valfrom = csks~bukrs ).

          loop at gt_csks_modified into data(ls_csks_modified).
            clear: gs_coupa_oc, ls_integration_key.

            lcl_utilities=>remover_zeros_esquerda( exporting
                                                    iv_input = ls_csks_modified-kostl
                                                   importing
                                                    ev_ouput = ls_csks_modified-kostl ).
            "<active>
            gs_coupa_oc-active = 'true'.
            if ls_csks_modified-bkzkp eq 'X' and ls_csks_modified-bkzks eq 'X'.
              "<active>
              gs_coupa_oc-active = 'false'.
            endif.
            "<name>
            concatenate ls_csks_modified-kostl '-' ls_csks_modified-ltext into gs_coupa_oc-name separated by space.
            "<external-ref-num>
            gs_coupa_oc-external_ref_num  = ls_csks_modified-kostl.
            "<lookup> -> <name>.
            gs_coupa_oc-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

            split 'K/A/H/FI/FM' at '/' into table lt_classifications.
            "É necessário enviar o mesmo centro de custo e nível para cada classificação acima.
            loop at lt_classifications into ls_classifications.
              clear: gs_coupa_oc-external_ref_num.
              gs_coupa_oc-external_ref_num = ls_csks_modified-kostl.

              "<parent> -> <external-ref-code>
              concatenate ls_csks_modified-gsber '|' ls_classifications into gs_coupa_oc-parent-external_ref_code.

              concatenate 'CC' ls_classifications gs_coupa_oc-external_ref_num into gs_coupa_oc-external_ref_num.
              append gs_coupa_oc to gt_coupa_oc.
            endloop.

          endloop.

          free: gt_csks_modified, gt_cdhdr_cc, ls_cdhdr_cc, gt_csks, ls_csks_modified, ls_integration_key.

        endif.
      endif.

    endif.

  endmethod.

  method if_process_lookup_values~build_out_data.

    data: ls_import_data type zcoupa_import_data.

    loop at gt_coupa_oc into gs_coupa_oc.
      clear: ls_import_data.

      ls_import_data-tp_referencia = gs_coupa_oc-external_ref_num(2).
      ls_import_data-id_referencia = gs_coupa_oc-external_ref_num+2.
      if ls_import_data-tp_referencia eq 'CC'.
        if gs_coupa_oc-external_ref_num+2(1) = 'K' or gs_coupa_oc-external_ref_num+2(1) = 'A' or gs_coupa_oc-external_ref_num+2(1) = 'H'.
          gs_coupa_oc-external_ref_num = gs_coupa_oc-external_ref_num+3.
        else.
          gs_coupa_oc-external_ref_num = gs_coupa_oc-external_ref_num+4.
        endif.
      elseif ls_import_data-tp_referencia eq 'IM'.   "ALRS
        gs_coupa_oc-external_ref_num = gs_coupa_oc-external_ref_num+7.
      else.
        gs_coupa_oc-external_ref_num = gs_coupa_oc-external_ref_num+2.
      endif.

      call transformation zt_coupa_oc
        source lookup_value = gs_coupa_oc
        result xml ls_import_data-xml
        options xml_header = 'no'.

      check sy-subrc is initial.

      append ls_import_data to me->if_process_lookup_values~gt_import_data.
    endloop.

    free: ls_import_data.

  endmethod.

  method if_process_lookup_values~execute_process.

    me->if_process_lookup_values~select_data( ).
    me->if_process_lookup_values~build_out_data( ).

    et_import_data[] = me->if_process_lookup_values~gt_import_data[].

  endmethod.

endclass.

class lcl_process_lookup_values_to implementation. "Importar dados Tipo de Operação

  method constructor.

    if iv_days is not initial.
      me->gv_amount_days_ago = iv_days.
    else.
      me->gv_amount_days_ago = 1.
    endif.

    me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                 = me->gv_amount_days_ago ).

    create object me->if_process_lookup_values~go_coupa_integration_log.

  endmethod.

  method if_process_lookup_values~select_data.

    data: ls_integration_key type zcoupa_integration_key,
          ls_aufnr           type coas-aufnr.

    data: rg_aufnr type range of coas-aufnr.

    select *
      from zintegrcoupa01
      into table @data(lt_ordens_importadas)
      where ident_proc = 'OM'
      and   status     = 'S'
      and   dt_atual   ge @me->gv_base_date.

    loop at lt_ordens_importadas assigning field-symbol(<ordens_importadas>).
      ls_aufnr = <ordens_importadas>-id_integr.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = ls_aufnr
        importing
          output = ls_aufnr.

      append value #( sign = 'I' option = 'EQ' low = ls_aufnr ) to rg_aufnr.
    endloop.

    if rg_aufnr is not initial.
      select  aufnr, werks, kostl, ktext
        into table @data(gt_coas_full)
        from coas
        where aufnr in @rg_aufnr.
    endif.

    if gt_coas_full is not initial.

      sort gt_coas_full by aufnr.
      delete adjacent duplicates from gt_coas_full comparing aufnr.

      select aufnr, vornr, ltxa1
        into table @data(gt_npact)
        from v_npact
        for all entries in @gt_coas_full
        where aufnr = @gt_coas_full-aufnr.

      loop at gt_npact into data(ls_npact).
        clear: gs_coupa_to, ls_integration_key.

        "<active>
        gs_coupa_to-active = 'true'.
        "<name>
        concatenate ls_npact-vornr '-' ls_npact-ltxa1 into gs_coupa_to-name separated by space.
        "<external-ref-num>
        gs_coupa_to-external_ref_num = ls_npact-vornr.
        "<parent> -> <external-ref-code>
        read table gt_coas_full into data(ls_coas_full) with key aufnr = ls_npact-aufnr binary search.
        if sy-subrc is initial.

          lcl_utilities=>remover_zeros_esquerda( exporting
                                        iv_input = ls_coas_full-aufnr
                                       importing
                                        ev_ouput = ls_coas_full-aufnr ).

          lcl_utilities=>remover_zeros_esquerda( exporting
                                        iv_input = ls_coas_full-kostl
                                       importing
                                        ev_ouput = ls_coas_full-kostl ).

          concatenate ls_coas_full-werks '|' 'FM' '|' ls_coas_full-kostl '|' ls_coas_full-aufnr into gs_coupa_to-parent-external_ref_code.
        endif.
        "<lookup> -> <name>.
        gs_coupa_to-lookup-name = 'Plano de Contas AMAGGI - CL/CCC/CC/OC'.

        lcl_utilities=>adicionar_zeros_esquerda( exporting
                                                  iv_input = ls_coas_full-aufnr
                                                 importing
                                                  ev_ouput = ls_coas_full-aufnr ).

        ls_integration_key-id_integr  = gs_coupa_to-external_ref_num && ls_coas_full-aufnr.
        ls_integration_key-ident_proc = 'TO'.

        check me->if_process_lookup_values~go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
                                                                                                = ls_integration_key ) eq abap_false.

        concatenate 'TO' gs_coupa_to-external_ref_num ls_coas_full-aufnr into gs_coupa_to-external_ref_num.
        append gs_coupa_to to gt_coupa_to.
      endloop.

      free: gt_coas_full, gt_npact, ls_npact, ls_integration_key.
    endif.

  endmethod.

  method if_process_lookup_values~build_out_data.

    data: ls_import_data type zcoupa_import_data.

    loop at gt_coupa_to into gs_coupa_to.
      clear: ls_import_data.

      ls_import_data-tp_referencia = gs_coupa_to-external_ref_num(2).
      ls_import_data-id_referencia = gs_coupa_to-external_ref_num+2(16).
      gs_coupa_to-external_ref_num = gs_coupa_to-external_ref_num+2(4).

      call transformation zt_coupa_to
        source lookup_value = gs_coupa_to
        result xml ls_import_data-xml
        options xml_header = 'no'.

      check sy-subrc is initial.

      append ls_import_data to me->if_process_lookup_values~gt_import_data.
    endloop.

    free: ls_import_data.

  endmethod.

  method if_process_lookup_values~execute_process.

    me->if_process_lookup_values~select_data( ).
    me->if_process_lookup_values~build_out_data( ).

    et_import_data[] = me->if_process_lookup_values~gt_import_data[].

  endmethod.

endclass.

class lcl_process_lookup_values_cr implementation. "Importar dados Conta Razão

  method constructor.

    if iv_days is not initial.
      me->gv_amount_days_ago = iv_days.
    else.
      me->gv_amount_days_ago = 1.
    endif.

    me->gv_base_date = lcl_utilities=>calculate_days_by_datum( iv_number_of_days
                                                                 = me->gv_amount_days_ago ).

    create object me->if_process_lookup_values~go_coupa_integration_log.

  endmethod.

  method if_process_lookup_values~select_data.

    data: ls_integration_key type zcoupa_integration_key.

    "Conta do Razão criadas.
    select ska1~saknr, ska1~ktopl, ska1~xloev, "#EC CI_DB_OPERATION_OK[2431747]
           ska1~xspea, ska1~xspeb, ska1~xspep, "#EC CI_DB_OPERATION_OK[2389136]
           skat~txt50, skb1~bukrs
      from ska1 inner join skat
      on  ska1~ktopl = skat~ktopl
      and ska1~saknr = skat~saknr
      inner join skb1                  "#EC CI_DB_OPERATION_OK[2431747]
      on ska1~saknr = skb1~saknr
      into table @data(gt_ska1_created)
      where ska1~ktopl = '0050'
      and   ska1~erdat ge @me->gv_base_date
      and   ska1~ktoks in ( 'YB01', 'YB06' )
      and   skat~spras eq @sy-langu.

    loop at gt_ska1_created into data(ls_ska1_created).
      clear: gs_coupa_cr, ls_integration_key.

      lcl_utilities=>remover_zeros_esquerda( exporting
                                              iv_input = ls_ska1_created-saknr
                                             importing
                                              ev_ouput = ls_ska1_created-saknr ).
      "<active>
      gs_coupa_cr-active = 'true'.
      "<name>
      concatenate ls_ska1_created-saknr '-' ls_ska1_created-txt50 into gs_coupa_cr-name separated by space.
      "<external-ref-num>
      gs_coupa_cr-external_ref_num = ls_ska1_created-saknr.
      "<parent> -> <external-ref-code>
      gs_coupa_cr-parent-external_ref_code = ls_ska1_created-bukrs.
      "<lookup> -> <name>.
      gs_coupa_cr-lookup-name = 'Conta Razão'.

      ls_integration_key-id_integr  = gs_coupa_cr-external_ref_num.
      ls_integration_key-ident_proc = 'CR'.

      check me->if_process_lookup_values~go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
                                                                                              = ls_integration_key ) eq abap_false.
      concatenate 'CR' gs_coupa_cr-external_ref_num into gs_coupa_cr-external_ref_num.

      append gs_coupa_cr to gt_coupa_cr.
    endloop.

    free: gt_ska1_created, ls_ska1_created, ls_integration_key.

    "Conta do Razão modificadas.
    select objectid
      from cdhdr
      into table @data(gt_cdhdr)
      where objectclas = 'SACH'
      and   tcode      = 'FS00'
      and   udate      ge @me->gv_base_date.
    if sy-subrc is initial.
      loop at gt_cdhdr into data(ls_cdhdr).
        append value #( ktopl = ls_cdhdr(04) saknr = ls_cdhdr+4(10) ) to gt_ska1.
      endloop.

      if gt_ska1 is not initial.
        sort gt_ska1 by saknr.
        delete adjacent duplicates from gt_ska1 comparing saknr.
        "Conta do Razão modificadas.
        select ska1~saknr, ska1~ktopl, ska1~xloev, "#EC CI_DB_OPERATION_OK[2389136]
               ska1~xspea, ska1~xspeb, ska1~xspep, "#EC CI_DB_OPERATION_OK[2431747]
               skat~txt50, skb1~bukrs
          from ska1 inner join skat
          on  ska1~ktopl = skat~ktopl
          and ska1~saknr = skat~saknr
          inner join skb1              "#EC CI_DB_OPERATION_OK[2431747]
          on ska1~saknr = skb1~saknr
          into table @data(gt_ska1_modified)
          for all entries in @gt_ska1
          where ska1~saknr = @gt_ska1-saknr
          and   ska1~ktopl = '0050'
          and   ska1~erdat ge @me->gv_base_date
          and   ska1~ktoks in ( 'YB01', 'YB06' )
          and   skat~spras eq @sy-langu.
        if sy-subrc is initial.

          loop at gt_ska1_modified into data(ls_ska1_modified).
            clear: gs_coupa_cr, ls_integration_key.

            lcl_utilities=>remover_zeros_esquerda( exporting
                                                    iv_input = ls_ska1_modified-saknr
                                                   importing
                                                    ev_ouput = ls_ska1_modified-saknr ).
            "<active>
            gs_coupa_cr-active = 'true'.
            if ls_ska1_modified-xloev is not initial or ls_ska1_modified-xspea is not initial
            or ls_ska1_modified-xspeb is not initial or ls_ska1_modified-xspep is not initial.
              gs_coupa_cr-active = 'false'.
            endif.
            "<name>
            concatenate ls_ska1_modified-saknr '-' ls_ska1_modified-txt50 into gs_coupa_cr-name separated by space.
            "<external-ref-num>
            gs_coupa_cr-external_ref_num = ls_ska1_modified-saknr.
            "<parent> -> <external-ref-code>
            gs_coupa_cr-parent-external_ref_code = ls_ska1_modified-bukrs.
            "<lookup> -> <name>.
            gs_coupa_cr-lookup-name = 'Conta Razão'.

            ls_integration_key-id_integr  = gs_coupa_cr-external_ref_num.
            ls_integration_key-ident_proc = 'CR'.
            concatenate 'CR' gs_coupa_cr-external_ref_num into gs_coupa_cr-external_ref_num.

            append gs_coupa_cr to gt_coupa_cr.
          endloop.

          free: gt_ska1_modified, ls_ska1_modified, ls_integration_key.

        endif.
      endif.

    endif.

  endmethod.

  method if_process_lookup_values~build_out_data.

    data: ls_import_data type zcoupa_import_data.

    loop at gt_coupa_cr into gs_coupa_cr.
      clear: ls_import_data.

      ls_import_data-tp_referencia = gs_coupa_cr-external_ref_num(2).
      ls_import_data-id_referencia = gs_coupa_cr-external_ref_num+2.
      gs_coupa_cr-external_ref_num = gs_coupa_cr-external_ref_num+2.

      call transformation zt_coupa_cr
        source lookup_value = gs_coupa_cr
        result xml ls_import_data-xml
        options xml_header = 'no'.

      check sy-subrc is initial.

      append ls_import_data to me->if_process_lookup_values~gt_import_data.
    endloop.

    free: ls_import_data.

  endmethod.

  method if_process_lookup_values~execute_process.

    me->if_process_lookup_values~select_data( ).
    me->if_process_lookup_values~build_out_data( ).

    et_import_data[] = me->if_process_lookup_values~gt_import_data[].

  endmethod.

endclass.

class lcl_alv_return implementation.

  method constructor.

    cl_salv_table=>factory( importing
                              r_salv_table = go_alv
                            changing
                              t_table      = gt_alv_data ).

    me->config_alv_columns( ).
    me->config_alv_functions( ).
    me->config_alv_events( ).

  endmethod.

  method append_new_line.

    data: ls_alv type zcoupa_import_data_alv.

    ls_alv-id_referencia  = iv_import_data-id_referencia.
    ls_alv-tp_referencia  = iv_import_data-tp_referencia.
    ls_alv-xml_icon       = '@0U@'.

    if iv_integration_log-nm_code ne 200 and
       iv_integration_log-nm_code ne 201 and
       iv_integration_log-nm_code ne 202.
      ls_alv-status = '@02@'.
      ls_alv-xml    = iv_integration_log-ds_data_retorno.
    else.
      ls_alv-status = '@01@'.
      ls_alv-xml    = iv_import_data-xml.
    endif.

    append ls_alv to me->gt_alv_data.

  endmethod.

  method config_alv_columns.

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'XML' ).
    go_column->set_visible( abap_false ).

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'XML_ICON' ).
    go_column->set_icon( abap_true ).
    go_column->set_cell_type( exporting
                                value = if_salv_c_cell_type=>hotspot ).
    go_column->set_short_text( 'XML').

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'STATUS' ).
    go_column->set_icon( abap_true ).
    go_column->set_short_text( 'Status').

  endmethod.

  method config_alv_functions.

    go_funct = go_alv->get_functions( ).
    go_funct->set_all( abap_true ).

  endmethod.

  method config_alv_events.

    go_events = go_alv->get_event( ).
    set handler go_alv_return->on_link_click for go_events.

  endmethod.

  method display.

    go_alv->display( ).

  endmethod.

  method on_link_click.

    read table gt_alv_data into data(ls_alv_data) index row.
    if sy-subrc is initial.
      call method cl_abap_browser=>show_xml
        exporting
          xml_string = ls_alv_data-xml.
    endif.

  endmethod.

endclass.
