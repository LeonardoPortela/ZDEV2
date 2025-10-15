*&---------------------------------------------------------------------*
*& Report  ZWRR0011
*&
*&---------------------------------------------------------------------*
*& Chamado: CS2022000268 Integração - Thunders x SAP
*& ABAP: Anderson Oenning
*&---------------------------------------------------------------------*
report zwrr0011.

tables: zfiwrt0021, coep.

types: begin of ty_operacion,
         name  type string,
         value type i,
       end of ty_operacion.

data: t_tp_operacion  type table of ty_operacion,
      vl_message(150) type c,
      vg_service      type /ui2/service_name,
      vg_branch       type zfiwrt0021-branch,
      mes_ref_ini     type i,
      mes_ref_conc    type i,
*      lv_ctravmi      TYPE i,
*      lv_ctravmr      TYPE i,
      mes_ref_fim     type i value 12,
      data_ref_ini    type char10,
      p_data_ini      type sy-datum,
      p_data_fim      type sy-datum,
      data_ref_fim    type char10,
      ano_ref         type char04,
      ano_base        type char04,
      t_zfiwrt0021    type table of zfiwrt0021,
      wa_zfiwrt0021   type zfiwrt0021,
      t_param         type zde_header_field_t.


data: conv_mes type char02.
data: lv_dias(3) type c.


selection-screen: begin of block b1 with frame title text-001.
  select-options: s_ano for zfiwrt0021-ano no intervals no-extension,
                  s_mes for coep-perio no intervals no-extension .

selection-screen: end of block b1.


start-of-selection.

  if sy-batch eq abap_true.
    try .
        zcl_job=>get_ck_program_execucao( exporting i_nome_program = sy-cprog importing e_qtd = data(e_qtd) ).
      catch zcx_job.
        e_qtd = 1.
    endtry.

    if e_qtd gt 1.
      leave program.
    endif.
  endif.

  "Setar tipo de operação.
  append value #( name = 'Venda' value = 2 ) to t_tp_operacion.
  append value #( name = 'Swap' value = 3 ) to t_tp_operacion.

  vl_message = 'Aguarde, importando dados contrato THUNDERS'.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 99
      text       = vl_message.



*&---------------------------------------------------------------------------------------------*
*& Seleção de dados operação - API operations thunders / Contrato de Energia
*&---------------------------------------------------------------------------------------------*

  "Iniciar consulta mes a mes, usando como referencia o ano atual e mes atual.
  clear: mes_ref_ini, ano_ref.

  if t_tp_operacion is not initial.
    loop at t_tp_operacion assigning field-symbol(<ls_tp_operacion>).
      clear: mes_ref_ini, ano_ref.

      if s_ano is initial.
        mes_ref_ini = sy-datum+4(2).
      else.
        mes_ref_ini = s_mes-low.
      endif.


      if s_ano is initial.
        ano_ref = sy-datum(4).
      else.
        ano_ref = s_ano-low.
      endif.


      while mes_ref_ini <= mes_ref_fim.

        vl_message = |Dados { <ls_tp_operacion>-name } mes { mes_ref_ini } dados Operação|.

        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            percentage = 99
            text       = vl_message.


        try.
            "Seta parametros.
            append value #( name = 's'  value = ano_ref ) to t_param.                 "Ano de referencia
            append value #( name = 'y'  value = ano_ref ) to t_param.                 "Ano de referencia
            append value #( name = 'm'  value = mes_ref_ini ) to t_param.             "Mes de referencia
            append value #( name = 'ot' value = <ls_tp_operacion>-value ) to t_param. "Tipo de operação

            "Setar o serviço.
            vg_service = 'THUNDERS'.

            zcl_int_thunders_operations=>zif_int_thunders_operations~get_instance(
            )->set_service( i_service = vg_service
            )->set_query( i_parametro = t_param
            )->get_dados_thunders_operations( importing i_thunders_operations = data(t_operations) ).

          catch zcx_integracao into data(ex_integra).
            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          catch zcx_error into data(ex_error).    "  "
            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        endtry.

*&---------------------------------------------------------------------------------------------*
*& Seleção de dados conciliação - API conciliations thunders / Contrato de Energia
*&---------------------------------------------------------------------------------------------*
        if t_operations is not initial.
          sort t_operations by billingstatus.
          delete t_operations where billingstatus eq 'Não autorizado'. "Deletar todos que não estão autorizado

          sort t_operations by tradetype.
          delete t_operations where tradetype ne 'Venda'. "Deletar todos diferente da operação vendas.

          sort t_operations by code.

          loop at t_operations assigning field-symbol(<ls_operations>).

            wa_zfiwrt0021-contrato = <ls_operations>-code.

            "Tipo de contrato.
            if <ls_operations>-classifications eq 'Longo Prazo'.
              wa_zfiwrt0021-tipo = 'LP'.
            else.
              wa_zfiwrt0021-tipo = 'CP'.
            endif.

            "Dados empresa.
            if <ls_operations>-partycnpj is not initial.
              replace '.' in <ls_operations>-partycnpj with ' '.
              replace '.' in <ls_operations>-partycnpj with ' '.
              replace '.' in <ls_operations>-partycnpj with ' '.
              replace '/' in <ls_operations>-partycnpj   with ' '.
              replace '-' in <ls_operations>-partycnpj   with ' '.

            endif.

            "Dados cliente.
            if <ls_operations>-counterpartycnpj is not initial.
              replace '.' in <ls_operations>-counterpartycnpj with ' '.
              replace '.' in <ls_operations>-counterpartycnpj with ' '.
              replace '.' in <ls_operations>-counterpartycnpj with ' '.
              replace '/' in <ls_operations>-counterpartycnpj   with ' '.
              replace '-' in <ls_operations>-counterpartycnpj   with ' '.

              select single kunnr  from kna1
                  into ( @wa_zfiwrt0021-kunnr )
               where stcd1 eq @<ls_operations>-counterpartycnpj.

            endif.


            "Dados data validade contrato.
            wa_zfiwrt0021-data_inicio = |{ <ls_operations>-startdate(4) }{ <ls_operations>-startdate+5(2) }{ <ls_operations>-startdate+8(2) }|.
            wa_zfiwrt0021-data_final  = |{ <ls_operations>-enddate(4) }{ <ls_operations>-enddate+5(2) }{ <ls_operations>-enddate+8(2) }|.

            "Material.
            wa_zfiwrt0021-matnr = 53. "Energia.
            condense wa_zfiwrt0021-matnr no-gaps.

            "Material.
            wa_zfiwrt0021-texto_nota = 'Cadastro realizado pela integração SAP x Thunders'.

            "Usuario.
            wa_zfiwrt0021-usuario = sy-uname.


            mes_ref_conc = mes_ref_ini.
*            lv_ctravmi = mes_ref_ini. "RJF
*            lv_ctravmr = mes_ref_ini. "RJF
            while ( mes_ref_conc <= 13 ).

              if mes_ref_conc ne 13.
                vl_message = |Dados { <ls_tp_operacion>-name } contrato { <ls_operations>-code } mes { mes_ref_conc } dados conciliação|.

                call function 'SAPGUI_PROGRESS_INDICATOR'
                  exporting
                    percentage = 99
                    text       = vl_message.
              endif.


              clear: p_data_ini, p_data_fim.
              "Ano referencia.
              if mes_ref_conc < 10.
                p_data_ini = |{ ano_ref }0{ mes_ref_conc }01|.
              else.
                p_data_ini = |{ ano_ref }{ mes_ref_conc }01|.
              endif.

              if mes_ref_conc eq 13.
                ano_base  = ano_ref + 1.
                p_data_ini = |{ ano_base }0101|.
              endif.

              "Pegando o ultimo dia do mes de referencia.
              call function 'LAST_DAY_OF_MONTHS'
                exporting
                  day_in            = p_data_ini "Data de referencia.
                importing
                  last_day_of_month = p_data_fim "Ultimo dia do mes de referencia.
                exceptions
                  day_in_no_date    = 1
                  others            = 2.

              clear: data_ref_ini, data_ref_fim, conv_mes.

              if mes_ref_conc < 10.
                conv_mes = |0{ mes_ref_conc }|.
              else.
                if mes_ref_conc eq 13.
                  conv_mes =  '01'.
                else.
                  conv_mes =  mes_ref_conc.
                endif.
              endif.

              data_ref_ini = |{ p_data_ini(4) }-{ conv_mes }-01|.
              data_ref_fim = |{ p_data_fim(4) }-{ p_data_fim+4(2) }-{ p_data_fim+6(2) }|.

              try .
                  "Seta parametros.
                  free: t_param.
                  append value #( name = 'due_start_date' value = data_ref_ini ) to t_param.
                  append value #( name = 'due_end_date'   value = data_ref_fim ) to t_param.

                  "Setar o serviço.
                  vg_service = 'THUNDERS_CONCILIATIONS'.

                  zcl_int_thunders_operations=>zif_int_thunders_operations~get_instance(
                  )->set_service( i_service = vg_service
                  )->set_query( i_parametro = t_param
                  )->get_dados_thunders_operations( importing i_thunders = data(t_conciliations) ).

                  if t_conciliations is not initial.

                    select * from zfiwrt0029
                      into table @data(it_029).
                    if sy-subrc is initial. sort it_029 by bukrs werks cfopt. endif.

                    loop at t_conciliations assigning field-symbol(<ls_conciliations>).
                      loop at <ls_conciliations>-invoices assigning field-symbol(<ls_invoices>).
                        if <ls_invoices>-statusname eq 'Aprovada'.

                          loop at <ls_invoices>-items assigning field-symbol(<ls_items>) where codereference eq <ls_operations>-code.

                            clear: vg_branch.
                            select single branch from zfiwrt0031
                              into @vg_branch
                           where partyprofilecode eq @<ls_operations>-partyprofilecode.

                            if vg_branch is initial. "Check se encontrou a filial parametrizada.
                              continue.
                            endif.

                            select single * from j_1bbranch
                             into @data(ls_j_1bbranch)
                             where branch eq @vg_branch.

                            wa_zfiwrt0021-bukrs   =  ls_j_1bbranch-bukrs.
                            wa_zfiwrt0021-branch  =  ls_j_1bbranch-branch.


*               Dados bancario da conta fornecedor.
                            select single banco agencia_banc conta_banc
                              from zfiwrt0030
                              into ( wa_zfiwrt0021-banco, wa_zfiwrt0021-agencia_banc, wa_zfiwrt0021-conta_banc )
                              where bukrs eq ls_j_1bbranch-bukrs
                               and  werks eq ls_j_1bbranch-branch.

*                            "Vencimento.
*                            CASE <ls_conciliations>-paymentcondition.
*                              WHEN 'MS + 1 (10 du)'.
*                                wa_zfiwrt0021-vencimento  = 10.
*                              WHEN 'MS + 1 (Vencimento 6 d.u)'.
*                                wa_zfiwrt0021-vencimento  = 6.
*                              WHEN OTHERS.
*                            ENDCASE.

* Ini - CS2022000268 - N 02 - Criar tela de parametro para cadastro CFOP e tela de parametro para cadastro condições pagamento - RJF.
                            if <ls_conciliations>-paymentcondition is not initial.
                              data(lv_prefix) = <ls_conciliations>-paymentcondition+3(2).
                              lv_dias   = <ls_conciliations>-paymentcondition(3).
                              if lv_prefix eq 'DU' and lv_dias co '0123456789'.
                                wa_zfiwrt0021-vencimento   = lv_dias.
                              elseif lv_dias co '0123456789'.
                                wa_zfiwrt0021-vencimento_f = lv_dias.
                              endif.
                            endif.
* Fim - CS2022000268 - N 02 - Criar tela de parametro para cadastro CFOP e tela de parametro para cadastro condições pagamento - RJF.

                            "Aguardando  refinamento do keyuser.
                            if <ls_invoices>-cfop is not initial.
* Ini - CS2022000268 - N 02 - Criar tela de parametro para cadastro CFOP e tela de parametro para cadastro condições pagamento - RJF.
                              data: lv_cfop type numc4.
                              lv_cfop = <ls_invoices>-cfop.
                              read table it_029 assigning field-symbol(<fs_029>) with key bukrs = wa_zfiwrt0021-bukrs
                                                                                          werks = wa_zfiwrt0021-branch
                                                                                          cfopt = lv_cfop binary search.
                              if sy-subrc is initial and <fs_029>-cfopz is not initial.
                                wa_zfiwrt0021-operacao  = <fs_029>-cfopz.
                              endif.
* Fim - CS2022000268 - N 02 - Criar tela de parametro para cadastro CFOP e tela de parametro para cadastro condições pagamento - RJF.
                            endif.

                            case mes_ref_conc.
                              when 13.
                                wa_zfiwrt0021-montante12 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa12   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 02.
                                wa_zfiwrt0021-montante01 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa01   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 03.
                                wa_zfiwrt0021-montante02 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa02   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.


                              when 04.
                                wa_zfiwrt0021-montante03 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa03   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 05.
                                wa_zfiwrt0021-montante04 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa04   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 06.
                                wa_zfiwrt0021-montante05 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa05   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 07.
                                wa_zfiwrt0021-montante06 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa06   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 08.
                                wa_zfiwrt0021-montante07 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa07   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 09.
                                wa_zfiwrt0021-montante08 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa08   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 10.
                                wa_zfiwrt0021-montante09 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa09   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 11.
                                wa_zfiwrt0021-montante10 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa10   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

                              when 12.
                                wa_zfiwrt0021-montante11 = <ls_items>-quantity.
                                wa_zfiwrt0021-tarifa11   = <ls_items>-unitprice.
                                wa_zfiwrt0021-ano  = ano_ref.
                                wa_zfiwrt0021-ger_autom = abap_true.
                                wa_zfiwrt0021-fatura_u = '1'.

*                              WHEN 12.
*                                wa_zfiwrt0021-montante12 = <ls_items>-quantity.
*                                wa_zfiwrt0021-tarifa12   = <ls_items>-unitprice.
*                                wa_zfiwrt0021-ano  = ano_ref.

                              when others.
                            endcase.

*                            lv_ctravmr = lv_ctravmr + 1.

                          endloop.
                        endif.
                      endloop.
                    endloop.

                  endif.

                catch zcx_integracao into ex_integra.
                  ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
                catch zcx_error into ex_error.    "  "
                  ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              endtry.

              add 1 to mes_ref_conc.

              if wa_zfiwrt0021-montante01 is not initial
                or wa_zfiwrt0021-montante02 is not initial
                or wa_zfiwrt0021-montante03 is not initial
                or wa_zfiwrt0021-montante04 is not initial
                or wa_zfiwrt0021-montante05 is not initial
                or wa_zfiwrt0021-montante06 is not initial
                or wa_zfiwrt0021-montante07 is not initial
                or wa_zfiwrt0021-montante08 is not initial
                or wa_zfiwrt0021-montante09 is not initial
                or wa_zfiwrt0021-montante10 is not initial
                or wa_zfiwrt0021-montante11 is not initial
                or wa_zfiwrt0021-montante12 is not initial.

                modify zfiwrt0021 from wa_zfiwrt0021.
              endif.
            endwhile.

            clear: ls_j_1bbranch, wa_zfiwrt0021.
          endloop.

        endif.

        add 1 to mes_ref_ini.

        free: t_operations, t_conciliations.
      endwhile.

    endloop.
  endif.
