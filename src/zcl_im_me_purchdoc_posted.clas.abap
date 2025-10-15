CLASS ZCL_IM_ME_PURCHDOC_POSTED DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

*"* public components of class ZCL_IM_ME_PURCHDOC_POSTED
*"* do not include other source files here!!!
  PUBLIC SECTION.

    INTERFACES IF_EX_ME_PURCHDOC_POSTED .
  PROTECTED SECTION.
*"* protected components of class ZCL_IM_ME_PURCHDOC_POSTED
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_IM_ME_PURCHDOC_POSTED
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_ME_PURCHDOC_POSTED IMPLEMENTATION.


  method if_ex_me_purchdoc_posted~posted.

    types:
      begin of ty_ekbe,
        ebeln type ekbe-ebeln,
        ebelp type ekbe-ebelp,
        menge type ekbe-menge,
        shkzg type ekbe-shkzg,
        gjahr type ekbe-gjahr,
        belnr type ekbe-belnr,
        buzei type ekbe-buzei,
      end of ty_ekbe.

    data: tl_ekpo         type table of uekpo,
          tl_eket         type table of ueket,
          tl_vbpa         type table of vbpa,
          tl_ekpa         type table of ekpa,
          tl_zmmt0035     type table of zmmt0035,
          tl_zsdt0094     type table of zsdt0094,
          tl_zsdt0094_2   type table of zsdt0094,
          lt_split_safra  type table of char40,
          wa_zsdt0094_2   type zsdt0094,
          wa_zmmt0035     type zmmt0035,
          wa_ekpa         type ekpa,
          sl_vbpa         type vbpa,
          sl_ekpo         type uekpo,
          sl_eket         type ueket,
          vl_zauart       type zsdt0010-zauart,
          vl_atua         type char01,
          qt_ordem        type vbap-kwmeng,
          vl_kunnr        type kna1-kunnr,
          vl_werks        type uekpo-werks,
          vl_bedat        type ekko-bedat,
          vl_aux          type char01,
          vl_mtorg        type mbew-mtorg,
          wa_uekpa        type uekpa,
          vl_local        type ekpa-lifn2,

          vl_erro(1),
          vl_149_mae(1),
          vl_94(1),
          vl_94_2(1),
          _troca_zterm(1),
          _royaltie(1),
          vl_lin          type i,
          vl_del          type i,
          vg_waers        type ekko-waers,
          vl_total_filho  type zsdt0094-total_proporc.

**************************************************
*** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
**************************************************
    data: obj_tx_curva type ref to zcl_webservice_tx_curva.

    free: obj_tx_curva.
    create object: obj_tx_curva.


    if sy-ucomm = 'MESAVE' or  sy-ucomm = 'OPT1' or sy-ucomm = 'YES' or sy-ucomm = 'ABR' or sy-cprog eq 'RM06BB30'.
      if sy-tcode eq 'ME21N' or
         sy-tcode eq 'ME22N' or
         sy-tcode eq 'ME23N' or
         sy-tcode eq 'ME59N' or
         sy-tcode eq 'ZMM0149'  or
         sy-cprog eq 'RM06BB30' or
         sy-batch eq abap_true.
        field-symbols: <icms>  type any,
                       <desti> type any.
        assign ('(SAPLMEGUI)EKKO_CI-ZICMS') to <icms>.
*        IF <ICMS> IS ASSIGNED.
*          IM_EKKO-ZICMS = <ICMS>.
*        ENDIF.
*        ASSIGN ('(SAPLMEGUI)EKKO_CI-ZDESTI') TO <DESTI>.
*        IF <DESTI> IS ASSIGNED.
*          IM_EKKO-DESTI = <DESTI>.
*        ENDIF.

        "grava treinamento com o pedido
*        CALL FUNCTION 'Z_MM_TREINA_COMPRAS'
*          EXPORTING
*            i_lifnr = im_ekko-lifnr
*            i_ebeln = im_ekko-ebeln
*            i_ebelp = '0010'
*            i_btn   = ' '
*            i_tipo  = 'P'
*          IMPORTING
*            i_erro  = vl_erro.



        if im_ekko-frgsx is initial and
          'PCE_PCEI_PCS_PCSI_PCEF_PSEF_PCC_ZDBP_ZFTE_ZDEF_ZSEM_ZEF_ZNB_ZPI_ZAR' cs im_ekko-bsart and  strlen( im_ekko-bsart ) ge 3.
          "
          field-symbols: <ekpo2>  type ekpo.
          assign ('(SAPLMEPO)ekpo') to <ekpo2>.
          "total de itens gravados
          clear vl_lin.
          select  count(*)
            from ekpo into vl_lin
           where ebeln eq im_ekko-ebeln.

          "total deletados ou bloqueados ja gravados
          clear vl_del.
          select  count(*)
           from ekpo into vl_del
          where ebeln eq im_ekko-ebeln
          and   loekz ne ''.
          "
          if vl_lin ge 1.
            loop at im_ekpo into  <ekpo2>.
              select count(*)
               from ekpo
              where ebeln = @<ekpo2>-ebeln
                and ebelp = @<ekpo2>-ebelp.
              if ( sy-subrc ne 0 ) .
                add 1 to  vl_lin.
              endif.

              select count(*)
                from ekpo
               where ebeln = @<ekpo2>-ebeln
                 and ebelp = @<ekpo2>-ebelp
                 and loekz ne ''.

              if ( sy-subrc = 0 ) .
                continue.
              endif.

              if <ekpo2>-loekz ne ''.
                add 1 to vl_del.
              endif.
            endloop.
          endif.
          if vl_lin ne vl_del or vl_lin = 0.
            message e000(z01) with 'Não foi encontrada estratégia de aprovação '
                                    'entre em contato com o Helpdesk'.
          endif.
        endif.
*        "CS2016000936/2017001583 (ME22N)
        if ( 'PCE_PCEI_PCS_PCSI_PCEF_PSEF' cs im_ekko-bsart ).
          select single bedat
            into vl_bedat
            from ekko
          where ebeln = im_ekko-ebeln.
          if sy-subrc = 0. "Se ja existir o pedido
            if vl_bedat ne im_ekko-bedat.
              message e000(z01) with 'Não alterar a data doc (MODI) '.
            endif.
          else.
            if sy-datum ne im_ekko-bedat.
              message e000(z01) with 'Não alterar a data doc (Novo)'.
            endif.
          endif.
        endif.

      endif.
    endif.

    if sy-tcode is initial.
      sy-tcode = 'ME28'.
    endif.
* comentado para subir  CS2016000936
    if sy-tcode eq 'ME21N' or
       sy-tcode eq 'ME22N' or
       sy-tcode eq 'ME23N' or
       sy-tcode eq 'ME29N' or
       sy-tcode eq 'ZMM0149' or
       sy-tcode eq 'ME28'.

      select single waers from ekko into vg_waers
              where ebeln = im_ekko-ebeln.

      if sy-subrc ne 0.
        vg_waers = im_ekko-waers.
      endif.

      " Verificar se pedido é desmenbrado do original
      clear: vl_149_mae.
      select single *
        from zmmt0035
        into @data(w0035)
        where ebeln = @im_ekko-ebeln.

      if sy-subrc eq 0.
        vl_149_mae = 'X'.
        "Pega pedidos filhos ZSON
        select distinct zmmt0037~ebeln
          from zmmt0037
          inner join ekko on ekko~ebeln = zmmt0037~ebeln
          and ekko~bsart = 'ZSON' " pedido filho
          into table @data(it_zmmt0037)
          where zmmt0037~nro_sol_cp eq @w0035-nro_sol_cp
          and   zmmt0037~ebeln      ne ' '.
        "
        " Checa se foi apenas criação de pedido filho sem alterar o valor global do pedido, neste caso não recalcula nada
        select single *
          from zmmt0037
         into @data(w0037)
         where zmmt0037~nro_sol_cp eq @w0035-nro_sol_cp
         and   zmmt0037~ebeln      eq '9999999999'.
        "
        if sy-subrc = 0. "Não altera HEDGE
          vl_149_mae = 'H'.
        endif.
      endif.

      if 'ZFTE_ZSEM_ZDEF_ZEFI_YFTE_YEFI' cs im_ekko-bsart and ( im_ekko-waers = 'BRL' or im_ekko-waers ne vg_waers ) and vl_149_mae ne 'H' and w0035-ck_hedge is initial.
        field-symbols: <wmwst> type any,
                       <wrbtr> type any,
                       <ekpo>  type ekpo.

        data v_ebeln_ori type ekko-ebeln.
        data t_konv type table of konv.

        types: ty_konv type table of komv.

        field-symbols: <vorga> type any,
                       <cva>   type any,
                       <ekko>  type ekko,
                       <lfa1>  type lfa1,
                       <konv>  type ty_konv.

**************************************************
*** OBJ DA TAXA CAMBIO E CURVA E OS METODOS DA 94
**************************************************
        "DATA: obj_tx_curva TYPE REF TO zcl_webservice_tx_curva.

        "FREE: obj_tx_curva.

*********************
***** CRIA OS OBJ
*********************
        "CREATE OBJECT: obj_tx_curva.


        data: t_ekpo        type table of ekpo,
              w_ekko        type ekko,
              w_ekko2       type ekko,
              w_ekpo2       like line of im_ekpo,
              wg_tot_ant    type ekpo-netwr,
              wg_tot_atu    type ekpo-netwr,
              wg_tot_son    type ekpo-netwr,
              wg_tot_dif    type ekpo-netwr,
              wg_tot_fin    type ekpo-netwr,
              wg_qtd_ant    type ekpo-menge,
              wg_qtd_atu    type ekpo-menge,
              wg_qtd_son    type ekpo-menge,
              wg_qtd_fin    type ekpo-menge,
              wg_qtd_ekb    type ekpo-menge,
              wg_qtd_dif    type ekpo-menge,
              wa_ite        type mepoitem,
              wa_zsdt0094   type zsdt0094,
              wa_zsdt0094_e type zsdt0094,
              wa_zmmt0037   type zmmt0037,
              wa_konv       type konv,
              it_zsdt0094   type table of zsdt0094,
              it_ekbe       type table of ty_ekbe,
              wa_ekbe       type ty_ekbe,
              vg_angnr      type ekko-angnr,
              vg_taxa       type zsdt0094-taxa_cambio,
              vg_taxa_ant   type zsdt0094-taxa_cambio,
              vg_ihran      type ekko-ihran,
              vg_ihran94    type ekko-ihran,
              var_estorno   type num10,
              tabix         type sy-tabix,
              v_udate       type cdhdr-udate,
              v_lindel      type i,
              v_linhas      type i,
              xachou(1),
              vg_tipo(1).


        clear: _royaltie, _troca_zterm.
*        faz  copia
        move-corresponding im_ekko to w_ekko.

        assign ('(SAPLMEPO)ekpo') to <ekpo>.

        refresh t_ekpo.
        select  * from ekpo into table t_ekpo
          where ebeln = im_ekko-ebeln and
                loekz = space.

        if t_ekpo[] is not initial.
          select ebeln ebelp menge shkzg gjahr belnr buzei
            from ekbe
            into table it_ekbe
            for all entries in  t_ekpo
           where ebeln =  t_ekpo-ebeln
           and   ebelp =  t_ekpo-ebelp
           and   bewtp = 'E'.

          sort it_ekbe by ebelp.
        endif.

        if im_ekpo[] is not initial.
          select ebeln ebelp menge shkzg gjahr belnr buzei
            from ekbe
            appending table it_ekbe
            for all entries in  im_ekpo
           where ebeln =  im_ekpo-ebeln
           and   ebelp =  im_ekpo-ebelp
           and   bewtp = 'E'.

        endif.

        sort it_ekbe by ebeln ebelp gjahr belnr buzei.
        delete adjacent duplicates from it_ekbe comparing ebeln ebelp gjahr belnr buzei.

        clear: wg_tot_ant, wg_tot_atu,wg_qtd_atu,wg_qtd_ant,vg_taxa_ant, wg_tot_fin,wg_qtd_fin, wg_tot_son,wg_qtd_son.

        if im_ekko-zterm eq 'I006' and  vl_149_mae = 'X'.
          _royaltie = 'X'.
        endif.
*        valor Antes
        clear: vl_94, vl_94_2.
        refresh it_zsdt0094.
        select *
          from zsdt0094
          into table it_zsdt0094
          where vbeln   eq im_ekko-ebeln
          and   tipo    eq 'PDI'
          and   estorno eq 0
          order by data_registro hora_registro.

        if it_zsdt0094[] is not initial or im_ekko-frgzu is not initial.   " se gravou no HEDGE, ou está está aprovado (neste caso não faz nada)

          if it_zsdt0094[] is not initial.
            select single *
              from ekko
              into w_ekko2
              where ebeln = im_ekko-ebeln.

            if w_ekko2-frgzu is initial. "ja tinha aprovado e reinicializou a estrategia
              vl_94 = 'X'.
            endif.
            vl_94_2 = 'X'.

            loop at it_zsdt0094 into wa_zsdt0094_e.
              add  wa_zsdt0094_e-total_proporc to wg_tot_ant.
              add  wa_zsdt0094_e-cadencia_qte  to wg_qtd_ant.
              vg_taxa_ant = wa_zsdt0094_e-taxa_cambio.
            endloop.

          else. "Primeira vez entra aqui e esse valor sera o atual
            refresh t_ekpo.
            select  * from ekpo into table t_ekpo
              where ebeln = im_ekko-ebeln and
                    loekz = space.
            loop at t_ekpo into  <ekpo>.
              clear wa_ite.
              call function 'MEPO_DOC_ITEM_GET'
                exporting
                  im_ebelp = <ekpo>-ebelp                           "'00010'
                importing
                  ex_item  = wa_ite
                exceptions
                  failure  = 1
                  others   = 2.
              if sy-subrc <> 0.
              endif.

              assign ('(SAPLMEPO)ekko') to <ekko>.
              if sy-tcode eq 'ME28' and <ekko>-ebeln is initial.
                select single * from ekko into <ekko>
                  where ebeln = im_ekko-ebeln.
              endif.
              assign ('(SAPLMEPO)fc_vorga') to <vorga>.
              assign ('(SAPLMEPO)cva_en') to <cva>.

              <vorga> = <cva>.

              perform kond_taxes in program saplmepo using 'D' 'X'.

              assign ('(SAPLMEPO)taxcom-WMWST') to <wmwst>.
              assign ('(SAPLMEPO)taxcom-WRBTR') to <wrbtr>.

              clear wa_konv.
              try.

                  cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
                    exporting it_selection_attribute = value #(
                   ( fieldname = 'KNUMV' value = <ekko>-knumv )
                   ( fieldname = 'KPOSN' value = <ekpo>-ebelp )
                   ( fieldname = 'KSCHL' value = 'RB00' )
                   )
                    importing et_prc_element_classic_format = data(etl381c14r4630) ).
                  wa_konv = etl381c14r4630[ 1 ].
                catch cx_prc_result cx_sy_itab_line_not_found .
                  sy-subrc = 4.
              endtry.

*              IF VL_149_MAE IS INITIAL AND _ROYALTIE IS INITIAL.
              if  _royaltie is initial.
                wg_tot_ant = wg_tot_ant + <wrbtr> + <wmwst>. " + WA_KONV-KBETR.
              else.
                select single *
                from zmmt0037
                into wa_zmmt0037
                where nro_sol_cp = w0035-nro_sol_cp
                and   ebelp      = <ekpo>-ebelp
                and   ebeln      = ' '.
                if sy-subrc = 0.
                  if wa_zmmt0037-bprme ne 'TO'.
                    wg_tot_ant = wg_tot_ant + ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh ). "Somente valor do royalties
                  else.
                    wg_tot_ant = wg_tot_ant + ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh / 1000 ). "Somente valor do royalties
                  endif.
                endif.
              endif.
              wg_qtd_ant = wg_qtd_ant + <ekpo>-menge.
            endloop.
          endif.

*        valor atual
          " Linhas modificadas apenas
          assign ('(SAPLMEPO)ekko') to <ekko>.
          if sy-tcode eq 'ME28' and <ekko>-ebeln is initial.
            select single * from ekko into <ekko>
              where ebeln = im_ekko-ebeln.
          endif.

          move-corresponding w_ekko to <ekko>.
          loop at im_ekpo into  <ekpo>.

            "Verifica se foi modificado status de eliminação.
            select single loekz
              from ekpo into @data(_loekz_ant)
             where ebeln = @<ekpo>-ebeln
               and ebelp = @<ekpo>-ebelp.

            if ( sy-subrc = 0 ) and ( _loekz_ant ne <ekpo>-loekz ).
              data(_change_loekz) = 'X'.
            endif.

            if <ekpo>-loekz ne ''.
              continue.
            endif.
            clear wa_ite.
            call function 'MEPO_DOC_ITEM_GET'
              exporting
                im_ebelp = <ekpo>-ebelp                           "'00010'
              importing
                ex_item  = wa_ite
              exceptions
                failure  = 1
                others   = 2.
            if sy-subrc <> 0.
            endif.

            assign ('(SAPLMEPO)fc_vorga') to <vorga>.
            assign ('(SAPLMEPO)cva_en') to <cva>.

            <vorga> = <cva>.

            perform kond_taxes in program saplmepo using 'D' 'X'.

            assign ('(SAPLMEPO)taxcom-WMWST') to <wmwst>.
            assign ('(SAPLMEPO)taxcom-WRBTR') to <wrbtr>.

            clear wa_konv.
            try.

                cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
                  exporting it_selection_attribute = value #(
                 ( fieldname = 'KNUMV' value = <ekko>-knumv )
                 ( fieldname = 'KPOSN' value = <ekpo>-ebelp )
                 ( fieldname = 'KSCHL' value = 'RB00' )
                 )
                  importing et_prc_element_classic_format = data(etl457c12r6893) ).
                wa_konv = etl457c12r6893[ 1 ].
              catch cx_prc_result cx_sy_itab_line_not_found .
                sy-subrc = 4.
            endtry.

*            IF VL_149_MAE IS INITIAL AND _ROYALTIE IS INITIAL.
            if _royaltie is initial.
              wg_tot_atu = wg_tot_atu + <wrbtr> + <wmwst>. " + WA_KONV-KBETR.
            else.
              select single *
                from zmmt0037
                into wa_zmmt0037
                where nro_sol_cp = w0035-nro_sol_cp
                and   ebelp      = <ekpo>-ebelp
                and   ebeln      = ' '.
              if sy-subrc = 0.
                if wa_zmmt0037-bprme ne 'TO'.
                  wg_tot_atu = wg_tot_atu + ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh ). "Somente valor do royalties
                else.
                  wg_tot_atu = wg_tot_atu + ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh / 1000 ). "Somente valor do royalties
                endif.
              endif.

            endif.
            wg_qtd_atu = wg_qtd_atu + <ekpo>-menge.

            if <ekpo>-elikz = 'X'. "Remessa Final
              wg_qtd_ekb = 0.
              loop at it_ekbe into wa_ekbe where ebelp = <ekpo>-ebelp.
                if wa_ekbe-shkzg = 'S'.
                  add wa_ekbe-menge to wg_qtd_ekb.
                else.
                  subtract  wa_ekbe-menge from wg_qtd_ekb.
                endif.
              endloop.
              wa_ekbe-menge = <ekpo>-menge - wg_qtd_ekb.
              if  wa_ekbe-menge gt 0.
*                IF VL_149_MAE IS INITIAL AND _ROYALTIE IS INITIAL.
                if _royaltie is initial.
                  wg_tot_fin = wg_tot_fin + ( ( <wrbtr> + <wmwst> ) * ( wa_ekbe-menge / <ekpo>-menge ) ).
                else.
                  if wa_zmmt0037-bprme ne 'TO'.
                    wg_tot_fin = wg_tot_fin + ( ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh ) * ( wa_ekbe-menge / <ekpo>-menge ) ).
                  else.
                    wg_tot_fin = wg_tot_fin + ( ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh / 1000 ) * ( wa_ekbe-menge / <ekpo>-menge ) ).
                  endif.
                endif.
                wg_qtd_fin = wg_qtd_fin +  wa_ekbe-menge.
              endif.
            endif.

          endloop.


          "pega valor atual do que não foi modificado
          if vl_94_2 = 'X' . "Já está aprovado
            refresh t_ekpo.
            select  * from ekpo into table t_ekpo
              where ebeln = im_ekko-ebeln and
                    loekz = space.

            loop at t_ekpo into  <ekpo>.
              read table im_ekpo into  w_ekpo2 with key ebelp = <ekpo>-ebelp.
              if sy-subrc = 0.
                continue.
              endif.

              "
              clear wa_ite.
              call function 'MEPO_DOC_ITEM_GET'
                exporting
                  im_ebelp = <ekpo>-ebelp                           "'00010'
                importing
                  ex_item  = wa_ite
                exceptions
                  failure  = 1
                  others   = 2.
              if sy-subrc <> 0.
              endif.

              assign ('(SAPLMEPO)ekko') to <ekko>.
              if sy-tcode eq 'ME28' and <ekko>-ebeln is initial.
                select single * from ekko into <ekko>
                  where ebeln = im_ekko-ebeln.
              endif.
              assign ('(SAPLMEPO)fc_vorga') to <vorga>.
              assign ('(SAPLMEPO)cva_en') to <cva>.

              <vorga> = <cva>.

              perform kond_taxes in program saplmepo using 'D' 'X'.

                                                            "BUG37116
              if  _royaltie is initial.
                assign ('(SAPLMEPO)taxcom-WMWST') to <wmwst>.
                assign ('(SAPLMEPO)taxcom-WRBTR') to <wrbtr>.
                wg_tot_atu = wg_tot_atu + <wrbtr> + <wmwst>.
                wg_qtd_atu = wg_qtd_atu + <ekpo>-menge.
              else.
                select single *
                    from zmmt0037
                    into wa_zmmt0037
                    where nro_sol_cp = w0035-nro_sol_cp
                    and   ebelp      = <ekpo>-ebelp
                    and   ebeln      = ' '.
                if sy-subrc = 0.
                  if wa_zmmt0037-bprme ne 'TO'.
                    wg_tot_atu = wg_tot_atu + ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh ). "Somente valor do royalties
                  else.
                    wg_tot_atu = wg_tot_atu + ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh / 1000 ). "Somente valor do royalties
                  endif.
                  wg_qtd_atu = wg_qtd_atu + wa_zmmt0037-menge .
                endif.
              endif.
              clear wa_konv.
              try.

                  cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
                    exporting it_selection_attribute = value #(
                   ( fieldname = 'KNUMV' value = <ekko>-knumv )
                   ( fieldname = 'KPOSN' value = <ekpo>-ebelp )
                   ( fieldname = 'KSCHL' value = 'RB00' )
                   )
                    importing et_prc_element_classic_format = data(etl574c14r8376) ).
                  wa_konv = etl574c14r8376[ 1 ].
                catch cx_prc_result cx_sy_itab_line_not_found .
                  sy-subrc = 4.
              endtry.

              if <ekpo>-elikz = 'X'. "Remessa Final
                wg_qtd_ekb = 0.
                loop at it_ekbe into wa_ekbe where ebelp = <ekpo>-ebelp.
                  if wa_ekbe-shkzg = 'S'.
                    add wa_ekbe-menge to wg_qtd_ekb.
                  else.
                    subtract  wa_ekbe-menge from wg_qtd_ekb.
                  endif.
                endloop.
                wa_ekbe-menge = <ekpo>-menge - wg_qtd_ekb.
                if  wa_ekbe-menge gt 0.
*                  IF VL_149_MAE IS INITIAL AND _ROYALTIE IS INITIAL.
                  if  _royaltie is initial.
*                    WG_TOT_ATU = WG_TOT_ATU - ( ( <WRBTR> + <WMWST> + WA_KONV-KBETR ) * ( WA_EKBE-MENGE / <EKPO>-MENGE ) ).
                    wg_tot_atu = wg_tot_atu - ( ( <wrbtr> + <wmwst>  ) * ( wa_ekbe-menge / <ekpo>-menge ) ).
                  else.
                    select single *
                       from zmmt0037
                       into wa_zmmt0037
                       where nro_sol_cp = w0035-nro_sol_cp
                       and   ebelp      = <ekpo>-ebelp
                       and   ebeln      = ' '.
                    if sy-subrc = 0.
                      if wa_zmmt0037-bprme ne 'TO'.
                        wg_tot_atu = wg_tot_atu - ( ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh ) * ( wa_ekbe-menge / <ekpo>-menge ) ).
                      else.
                        wg_tot_atu = wg_tot_atu - ( ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh / 1000 ) * ( wa_ekbe-menge / <ekpo>-menge ) ).
                      endif.
                    endif.
                  endif.
                  wg_qtd_atu = wg_qtd_atu -  wa_ekbe-menge.
                endif.
              endif.
            endloop.
          endif.

          clear wa_zsdt0094-taxa_cambio.

          assign ('(SAPLMEPO)ekko') to <ekko>.
          select single * from ekko into w_ekko
               where ebeln = im_ekko-ebeln.
          if sy-subrc = 0.
            vg_ihran = w_ekko-ihran.
            vg_waers = w_ekko-waers.
          else.
            vg_ihran = <ekko>-ihran.
            vg_waers = <ekko>-waers.
          endif.

          v_udate = sy-datum.
          refresh it_zsdt0094.
          select *
            from zsdt0094
            into table it_zsdt0094
            where vbeln   eq im_ekko-ebeln
            and   tipo    eq 'PDI'.

          clear vg_ihran94.
          if  it_zsdt0094[] is not initial.
            sort it_zsdt0094 by data_registro descending hora_registro descending.
            read table it_zsdt0094 into wa_zsdt0094_e index 1.
            vg_ihran94 = wa_zsdt0094_e-data_venc.
          endif.

          clear vg_taxa.
          "
          if vl_149_mae = 'X' and it_zmmt0037[] is not initial.
            "Soma pedido filho
            v_ebeln_ori = im_ekko-ebeln.
            loop at it_zmmt0037 into data(w_0037).
              refresh t_ekpo.
              select  * from ekpo into table t_ekpo
                where ebeln = w_0037-ebeln and
                      loekz = space.


              loop at t_ekpo into  <ekpo>.
                "
                clear wa_ite.
                call function 'MEPO_DOC_ITEM_GET'
                  exporting
                    im_ebelp = <ekpo>-ebelp                           "'00010'
                  importing
                    ex_item  = wa_ite
                  exceptions
                    failure  = 1
                    others   = 2.
                if sy-subrc <> 0.
                endif.

                assign ('(SAPLMEPO)ekpo') to <ekpo>.
                assign ('(SAPLMEPO)ekko') to <ekko>.
                if sy-tcode eq 'ME28' and <ekko>-ebeln is initial.
                  select single * from ekko into <ekko>
                    where ebeln = im_ekko-ebeln.
                endif.
                assign ('(SAPLMEPO)lfa1') to <lfa1>.

                assign ('(SAPLMEPO)fc_vorga') to <vorga>.
                assign ('(SAPLMEPO)cva_en') to <cva>.

                <vorga> = <cva>.

                select  single * from ekko into  <ekko>
                  where ebeln = w_0037-ebeln.

                select single * from lfa1 into <lfa1>
                  where lifnr = <ekko>-lifnr.

                try.

                    cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
                      exporting it_selection_attribute = value #(
                     ( fieldname = 'KNUMV' value = <ekko>-knumv )
                     )
                      importing et_prc_element_classic_format = t_konv ).
                  catch cx_prc_result .
                    sy-subrc = 4.
                endtry.

                assign ('(SAPLMEPO)tkomv[]') to <konv>.
                <konv>[] = t_konv[].

                perform kond_taxes in program saplmepo using 'D' 'X'.


                if  _royaltie is initial.
                  assign ('(SAPLMEPO)taxcom-WMWST') to <wmwst>.
                  assign ('(SAPLMEPO)taxcom-WRBTR') to <wrbtr>.
                  wg_tot_son = wg_tot_son + <wrbtr> + <wmwst>.
                  wg_qtd_son = wg_qtd_son + <ekpo>-menge.
                else.
                  select single *
                      from zmmt0037
                      into wa_zmmt0037
                      where nro_sol_cp = w0035-nro_sol_cp
                      and   ebelp      = <ekpo>-ebelp
                      and   ebeln      = <ekpo>-ebeln.
                  if sy-subrc = 0.
                    if wa_zmmt0037-bprme ne 'TO'.
                      wg_tot_son = wg_tot_son + ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh ). "Somente valor do royalties
                    else.
                      wg_tot_son = wg_tot_son + ( ( wa_zmmt0037-netpr_roya * wa_zmmt0037-menge ) / wa_zmmt0037-peinh / 1000 ). "Somente valor do royalties
                    endif.
                    wg_qtd_son = wg_qtd_son + wa_zmmt0037-menge .
                  endif.
                endif.

              endloop.
            endloop.
            "volta original
            refresh t_ekpo.
            select  * from ekpo into table t_ekpo
              where ebeln = v_ebeln_ori and
                    loekz = space.

            loop at t_ekpo into  <ekpo>.
              "
              clear wa_ite.
              call function 'MEPO_DOC_ITEM_GET'
                exporting
                  im_ebelp = <ekpo>-ebelp                           "'00010'
                importing
                  ex_item  = wa_ite
                exceptions
                  failure  = 1
                  others   = 2.
              if sy-subrc <> 0.
              endif.

              assign ('(SAPLMEPO)ekpo') to <ekpo>.
              assign ('(SAPLMEPO)ekko') to <ekko>.
              if sy-tcode eq 'ME28' and <ekko>-ebeln is initial.
                select single * from ekko into <ekko>
                  where ebeln = im_ekko-ebeln.
              endif.
              assign ('(SAPLMEPO)lfa1') to <lfa1>.

              assign ('(SAPLMEPO)fc_vorga') to <vorga>.
              assign ('(SAPLMEPO)cva_en') to <cva>.

              <vorga> = <cva>.

              select  single * from ekko into  <ekko>
                where ebeln = v_ebeln_ori. "IM_EKKO-EBELN.

              select single * from lfa1 into <lfa1>
                where lifnr = <ekko>-lifnr.

              try.

                  cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
                    exporting it_selection_attribute = value #(
                   ( fieldname = 'KNUMV' value = <ekko>-knumv )
                   )
                    importing et_prc_element_classic_format = t_konv ).
                catch cx_prc_result .
                  sy-subrc = 4.
              endtry.

              assign ('(SAPLMEPO)tkomv[]') to <konv>.
              <konv>[] = t_konv[].

              perform kond_taxes in program saplmepo using 'D' 'X'.

            endloop.
          endif.

          wg_tot_dif = 0.
          wg_qtd_dif = 0.
          if ( wg_tot_atu = 0 ) and ( _change_loekz is initial ). " AND   <EKKO>-IHRAN NE VG_IHRAN. "não houve mudança de valor e status eliminação
            "soma valores dos filhos
            add wg_qtd_son to wg_qtd_ant.
            add wg_tot_son to wg_tot_ant.
            wg_tot_dif = wg_tot_ant. "pega o anterior gravado
            wg_qtd_dif = wg_qtd_ant. "pega o anterior gravado
          else.
*            "soma valores dos filhos
            add wg_qtd_son to wg_qtd_atu.
            add wg_tot_son to wg_tot_atu.
            wg_tot_dif = wg_tot_atu - wg_tot_ant.
            wg_qtd_dif = wg_qtd_atu - wg_qtd_ant.
          endif.

          if ( w_ekko-zterm  is not initial  ) and "Cond. Pgto Antiga
             ( im_ekko-zterm is not initial  ) and "Cond. Pgto Nova
             ( w_ekko-zterm <> im_ekko-zterm ). "Trocou condição de Pgto.

            if ( ( w_ekko-zterm  ne 'I006' ) and ( w_ekko-zterm ne 'I007' ) and ( w_ekko-zterm ne 'C003' ) ) and "Antiga Cond.Pgto dispara HEDGE         "FF #168911 - ins C003
               ( ( im_ekko-zterm eq 'I006' ) or ( im_ekko-zterm eq 'I007' ) or  ( im_ekko-zterm eq 'C003' ) ).    "Novo Cond.Pgto não dispara HEDGE      "FF #168911 - ins C003
              "Marcar somente para estornar lançamento antigo.
              _troca_zterm = 'X'.
            endif.
          endif.
          "
          clear: v_lindel, v_linhas.
          loop at im_ekpo into  <ekpo>.
            if <ekpo>-loekz eq 'L'.
              add 1 to v_lindel.
            endif.
            add 1 to v_linhas.
          endloop.
          "
          clear xachou.
          if v_linhas gt 0 and v_linhas = v_lindel.
            if wg_tot_dif ne 0.
              xachou = 'X'.
            endif.
          endif.
          if  <ekko>-ihran gt vg_ihran.
            xachou = 'X'.
          endif.
          "
          select single * from ekko into w_ekko
              where ebeln = im_ekko-ebeln.

          if  ( sy-tcode ne 'ME28'  and sy-tcode ne 'ME29N' and im_ekko-frgzu is initial and it_zsdt0094[] is not initial and wg_tot_fin eq 0 and <ekko>-waers eq vg_waers and xachou is initial ) or
              (  sy-tcode = 'ZMM0149' and wg_tot_dif gt 0 and wg_tot_atu gt w_ekko-rlwrt  ). "somente altera hedge se for menor, se for maior reinicializa estrategia e hedge na aprovação

          elseif  ( ( wg_tot_dif   ne 0           ) or
                    ( wg_tot_fin   gt 0           ) or
                    ( <ekko>-ihran ne vg_ihran  and xachou = 'X' ) or
                    ( <ekko>-waers ne vg_waers    ) or "CS2017002305
                    ( _troca_zterm is not initial ) ) and ( im_ekko-ihran >= '20170516' ) .

            if ( ( <ekko>-ihran ne vg_ihran ) or ( <ekko>-waers ne vg_waers ) or ( _troca_zterm is not initial ) ). "VL_94 = 'X'. "estorna tudo
              call function 'NUMBER_GET_NEXT'
                exporting
                  nr_range_nr             = '01'
                  object                  = 'ZEST_0094'
                importing
                  number                  = var_estorno
                exceptions
                  interval_not_found      = 1
                  number_range_not_intern = 2
                  object_not_found        = 3
                  quantity_is_0           = 4
                  quantity_is_not_1       = 5
                  interval_overflow       = 6
                  buffer_overflow         = 7
                  others                  = 8.

              refresh it_zsdt0094.
              select *
                from zsdt0094
                into table it_zsdt0094
                where vbeln   eq im_ekko-ebeln
                and   tipo    eq 'PDI'
                and   estorno eq 0.
              if it_zsdt0094[] is not initial.
                loop at it_zsdt0094 into wa_zsdt0094_e.
                  tabix  = sy-tabix.
                  wa_zsdt0094-mandt           = sy-mandt.
                  wa_zsdt0094-data_registro   = sy-datum.
                  wa_zsdt0094-hora_registro   = sy-uzeit.
                  wa_zsdt0094-programa        = sy-cprog.
                  wa_zsdt0094-nro_sol_ov      = wa_zsdt0094_e-nro_sol_ov.
                  wa_zsdt0094-data_venc       = wa_zsdt0094_e-data_venc.

                  if ( vg_ihran94 > vg_ihran  ).
                    vg_ihran = vg_ihran94.
                  endif.
                  if ( vg_ihran > wa_zsdt0094-data_venc  ).
                    wa_zsdt0094-data_venc = vg_ihran.
                  endif.

                  if wa_zsdt0094-data_venc < sy-datum.
                    if ( im_ekko-ihran > wa_zsdt0094-data_venc  ).
                      wa_zsdt0094-data_venc = im_ekko-ihran.
                    endif.
                  endif.

                  if ( wa_zsdt0094-data_venc < sy-datum ).
                    wa_zsdt0094-data_venc = sy-datum + 30.
                  endif.

                  wa_zsdt0094-data_lib        = sy-datum.
                  wa_zsdt0094-cadencia_qte    = wa_zsdt0094_e-cadencia_qte * -1.
                  wa_zsdt0094-zieme           = wa_zsdt0094_e-zieme.
                  wa_zsdt0094-total_proporc   = wa_zsdt0094_e-total_proporc * -1.
                  wa_zsdt0094-taxa_cambio     = wa_zsdt0094_e-taxa_cambio.
                  wa_zsdt0094-tipo            = wa_zsdt0094_e-tipo.
                  if wa_zsdt0094_e-tipo_taxa = 'V'.
                    wa_zsdt0094-tipo_taxa       = 'C'.
                  else.
                    wa_zsdt0094-tipo_taxa       = 'V'.
                  endif.

                  wa_zsdt0094-taxa_curva = obj_tx_curva->buscar_taxa( i_data     = im_ekko-ihran
                                                                      i_data_lib = wa_zsdt0094-data_lib
                                                                      i_tipo     = wa_zsdt0094-tipo_taxa ).

                  wa_zsdt0094-vbeln           = wa_zsdt0094_e-vbeln.
                  wa_zsdt0094-inco1           = wa_zsdt0094_e-inco1.
                  wa_zsdt0094-estorno         = var_estorno.

                  wa_zsdt0094_e-estorno       = var_estorno.
                  modify it_zsdt0094 from wa_zsdt0094_e index tabix transporting estorno.

                  insert into zsdt0094 values wa_zsdt0094.
                  commit work.
                  wait up to 2 seconds.
                endloop.
                modify zsdt0094 from table it_zsdt0094.
                commit work.
                refresh it_zsdt0094.
                if wg_tot_atu = 0. "não houve mudança de valor
                  wg_tot_dif = wg_tot_ant. "pega o anterior gravado
                  wg_qtd_dif = wg_qtd_ant. "pega o anterior gravado
                else.
                  wg_tot_dif = wg_tot_atu.
                  wg_qtd_dif = wg_qtd_atu.
                endif.
                wg_tot_dif = wg_tot_atu.
                wg_qtd_dif = wg_qtd_atu.
                vg_taxa_ant = 0.
                wait up to 2 seconds.
              endif.
            endif.

            if ( ( im_ekko-zterm ne 'I006' ) and ( im_ekko-zterm ne 'I007' )  and ( im_ekko-zterm ne 'C003' ) and <ekko>-waers = 'BRL' ) or _royaltie = 'X'. "FF #168911 - ins C003
              "Busca primeira taxa para o vencimento atualizada na 94
              vg_taxa_ant = 0.
              select *
                from zsdt0094 into table @data(it_0094)
               where vbeln     eq @im_ekko-ebeln
**                 AND DATA_VENC EQ @IM_EKKO-IHRAN
                 and tipo      eq 'PDI'.

              sort it_0094 by data_registro hora_registro.

              loop at it_0094 into wa_zsdt0094_e.
                vg_taxa_ant = wa_zsdt0094_e-taxa_cambio.
                exit.
              endloop.

              select count(*)
                from zsdt0094 into @data(v_count_0094)
               where vbeln  eq @im_ekko-ebeln
                 and tipo   eq 'PDI'.

              if v_count_0094 = 0.
                data(_primeiro_disparo) = 'X'.
              endif.

              if wg_tot_dif ne 0.
                clear wa_zsdt0094.
                if wg_tot_dif gt 0.
                  vg_tipo = 'V'.
                else.
                  vg_tipo = 'C'.
                endif.
                wa_zsdt0094-mandt           = sy-mandt.
                wa_zsdt0094-data_registro   = sy-datum.
                wa_zsdt0094-hora_registro   = sy-uzeit.
                wa_zsdt0094-programa        = sy-cprog.
                wa_zsdt0094-nro_sol_ov      = im_ekko-ebeln.
                wa_zsdt0094-data_venc       = im_ekko-ihran.

                if ( vg_ihran94 > vg_ihran  ).
                  vg_ihran = vg_ihran94.
                endif.

                if ( vg_ihran > wa_zsdt0094-data_venc  ).
                  wa_zsdt0094-data_venc = vg_ihran.
                endif.
                if ( wa_zsdt0094-data_venc < sy-datum ).
                  wa_zsdt0094-data_venc = sy-datum + 30.
                endif.

                if sy-tcode ne 'ME29N' and sy-tcode ne 'ME28' .
                  wa_zsdt0094-data_lib        = v_udate.
                else.
                  wa_zsdt0094-data_lib        = sy-datum. "IM_EKKO-AEDAT.
                endif.


                wa_zsdt0094-cadencia_qte    = wg_qtd_dif.
                wa_zsdt0094-zieme           = <ekpo>-meins.
                wa_zsdt0094-total_proporc   = wg_tot_dif.
                wa_zsdt0094-taxa_curva = obj_tx_curva->buscar_taxa( i_data     = im_ekko-ihran
                                                                    i_data_lib = wa_zsdt0094-data_lib
                                                                    i_tipo     = vg_tipo ).


                if vg_taxa_ant ne 0.
                  wa_zsdt0094-taxa_cambio     = vg_taxa_ant.
                else.
                  wa_zsdt0094-taxa_cambio     = wa_zsdt0094-taxa_curva.
                endif.
                wa_zsdt0094-tipo            = 'PDI'.
                wa_zsdt0094-tipo_taxa       = vg_tipo.
                wa_zsdt0094-vbeln           = im_ekko-ebeln.
                wa_zsdt0094-inco1           = im_ekko-inco1.

                insert into zsdt0094 values wa_zsdt0094.

                commit work.
                wait up to 2 seconds.

                if ( w_ekko-ihran ne im_ekko-ihran ) or ( _primeiro_disparo is not initial ).
                  "atualiza no pedido
                  vg_angnr = wa_zsdt0094-taxa_cambio.
                  translate vg_angnr using '.,'.
                  condense vg_angnr no-gaps.
                  assign ('(SAPLMEPO)ekko') to <ekko>.
                  if sy-tcode eq 'ME28' and <ekko>-ebeln is initial.
                    select single * from ekko into <ekko>
                      where ebeln = im_ekko-ebeln.
                  endif.
                  <ekko>-angnr = vg_angnr.

                  update ekko set angnr = vg_angnr
                   where ebeln = im_ekko-ebeln.
                  loop at it_zmmt0037 into w_0037.
                    update ekko set angnr = vg_angnr
                      where ebeln = w_0037-ebeln.
                  endloop.

                  commit work.
                endif.

              endif.
              if  wg_tot_fin gt 0.
                clear wa_zsdt0094.

                "Inverte sinal
                multiply wg_tot_fin by -1.
                multiply wg_qtd_fin by -1.

                if wg_tot_fin gt 0.
                  vg_tipo = 'V'.
                else.
                  vg_tipo = 'C'.
                endif.
                wa_zsdt0094-mandt           = sy-mandt.
                wa_zsdt0094-data_registro   = sy-datum.
                wa_zsdt0094-hora_registro   = sy-uzeit.
                wa_zsdt0094-programa        = sy-cprog.
                wa_zsdt0094-nro_sol_ov      = im_ekko-ebeln.
                wa_zsdt0094-data_venc       = im_ekko-ihran.

                if ( vg_ihran94 > vg_ihran  ).
                  vg_ihran = vg_ihran94.
                endif.
                "
                if ( vg_ihran > wa_zsdt0094-data_venc  ).
                  wa_zsdt0094-data_venc = vg_ihran.
                endif.
                if ( wa_zsdt0094-data_venc < sy-datum ).
                  wa_zsdt0094-data_venc = sy-datum + 30.
                endif.

                if sy-tcode ne 'ME29N' and sy-tcode ne 'ME28' .
                  wa_zsdt0094-data_lib        = v_udate.
                else.
                  wa_zsdt0094-data_lib        = sy-datum. "IM_EKKO-AEDAT.
                endif.


                wa_zsdt0094-cadencia_qte    = wg_qtd_fin.
                wa_zsdt0094-zieme           = <ekpo>-meins.
                wa_zsdt0094-total_proporc   = wg_tot_fin.
                wa_zsdt0094-taxa_curva = obj_tx_curva->buscar_taxa( i_data     = im_ekko-ihran
                                                                    i_data_lib = wa_zsdt0094-data_lib
                                                                    i_tipo     = vg_tipo ).


                if vg_taxa_ant ne 0.
                  wa_zsdt0094-taxa_cambio     = vg_taxa_ant.
                else.
                  wa_zsdt0094-taxa_cambio     = wa_zsdt0094-taxa_curva.
                endif.
                wa_zsdt0094-tipo            = 'PDI'.
                wa_zsdt0094-tipo_taxa       = vg_tipo.
                wa_zsdt0094-vbeln           = im_ekko-ebeln.
                wa_zsdt0094-inco1           = im_ekko-inco1.

                insert into zsdt0094 values wa_zsdt0094.

                commit work.
                wait up to 2 seconds.

                if ( w_ekko-ihran ne im_ekko-ihran ) or ( _primeiro_disparo is not initial )..
                  "atualiza no pedido
                  vg_angnr = wa_zsdt0094-taxa_cambio.
                  translate vg_angnr using '.,'.
                  condense vg_angnr no-gaps.
                  assign ('(SAPLMEPO)ekko') to <ekko>.
                  if sy-tcode eq 'ME28' and <ekko>-ebeln is initial.
                    select single * from ekko into <ekko>
                      where ebeln = im_ekko-ebeln.
                  endif.
                  <ekko>-angnr = vg_angnr.

                  update ekko set angnr = vg_angnr
                   where ebeln = im_ekko-ebeln.
                  loop at it_zmmt0037 into w_0037.
                    update ekko set angnr = vg_angnr
                      where ebeln = w_0037-ebeln.
                  endloop.

                  commit work.
                endif.

              endif.

            endif. "IF ( IM_EKKO-ZTERM NE 'I006' ) AND ( IM_EKKO-ZTERM NE 'I007' ) AND ( IM_EKKO-ZTERM NE 'C003' ).

          endif.
        endif.
      endif.


      assign ('(SAPLMEPO)ekpo') to <ekpo>.

      "desmarcar remessa final
      if im_ekko-bsart = 'ZSON' and <ekpo>-elikz = ' '. "Remessa Final

        clear: vl_total_filho, wa_zmmt0035.

        select single * from zmmt0035
           into wa_zmmt0035
          where nro_sol_cp eq im_ekko-submi.

        select sum( total_proporc ) from zsdt0094
          into vl_total_filho
         where nro_sol_ov eq im_ekko-ebeln. "pedido_filho.

        if vl_total_filho ne 0.
          "INVERTE VALOR zsdt0094
          if vg_tipo = 'V'.
            vg_tipo = 'C'.
          else.
            vg_tipo = 'V'.
          endif.

          select * from zsdt0094 into table tl_zsdt0094_2
          where nro_sol_ov eq im_ekko-ebeln. "pedido_filho.

          sort tl_zsdt0094_2 by data_registro hora_registro ascending.

          read table  tl_zsdt0094_2 into wa_zsdt0094_2 index 1.
          if sy-subrc eq 0.
            wa_zsdt0094-mandt         = sy-mandt.
            wa_zsdt0094-data_registro = sy-datum.
            wa_zsdt0094-hora_registro = sy-uzeit.
            wa_zsdt0094-programa      = sy-cprog.
            wa_zsdt0094-nro_sol_ov    = wa_zsdt0094_2-nro_sol_ov. "Pedido filho
            wa_zsdt0094-vbeln         = wa_zmmt0035-ebeln. "Pedido mae
            wa_zsdt0094-data_venc     = im_ekko-ihran. "-->vencimento_pedido_mae
            wa_zsdt0094-cadencia_qte  = wa_zsdt0094_2-cadencia_qte * -1.
            wa_zsdt0094-total_proporc = wa_zsdt0094_2-total_proporc * -1.
            wa_zsdt0094-zieme         = <ekpo>-meins.
            wa_zsdt0094-tipo          = 'PDI'.
            wa_zsdt0094-tipo_taxa     = vg_tipo.
            wa_zsdt0094-taxa_curva = obj_tx_curva->buscar_taxa( i_data     = im_ekko-ihran
                                                                i_data_lib = sy-datum
                                                                i_tipo     = vg_tipo ).
            wa_zsdt0094-taxa_cambio = wa_zsdt0094-taxa_curva.
            insert into zsdt0094 values wa_zsdt0094.
            commit work.
            wait up to 2 seconds.
          endif.
        endif.
      endif.


*        -- marcar remessa final
      if im_ekko-bsart = 'ZSON' and <ekpo>-elikz = 'X'. "Remessa Final

        clear:  vl_total_filho, wa_zmmt0035, wa_zsdt0094.

        select sum( total_proporc )
          from zsdt0094
          into vl_total_filho
           where nro_sol_ov eq im_ekko-ebeln. "pedido_filho.

        if vl_total_filho eq 0.

          select single * from zmmt0035
            into wa_zmmt0035
           where nro_sol_cp eq im_ekko-submi.
*
*            solictacao = ekko-submi
*            selecionar zmmt0035 onde nro_sol_cp pegar ebeln (pedido mãe)
*            pedido_mae = zmmt0035-ebeln.
*
          move-corresponding im_ekko to w_ekko.

          "itens do pedido filho zson
          refresh t_ekpo.
          select * from ekpo into table t_ekpo
          where ebeln eq im_ekko-ebeln
            and loekz eq space.

          if t_ekpo[] is not initial.
            " migo entrada/estornada
            select ebeln ebelp menge shkzg gjahr belnr buzei
            from ekbe
            into table it_ekbe
            for all entries in t_ekpo
            where ebeln eq t_ekpo-ebeln
            and   ebelp eq t_ekpo-ebelp
            and   bewtp eq 'E'.

            sort it_ekbe by ebelp.
          endif.

          sort it_ekbe by ebeln ebelp gjahr belnr buzei.
          delete adjacent duplicates from it_ekbe comparing ebeln ebelp gjahr belnr buzei.

          "variaveis para calculo do valor do pedido com impostos
          call function 'MEPO_DOC_ITEM_GET'
            exporting
              im_ebelp = <ekpo>-ebelp
            importing
              ex_item  = wa_ite
            exceptions
              failure  = 1
              others   = 2.

          assign ('(SAPLMEPO)fc_vorga') to <vorga>.
          assign ('(SAPLMEPO)cva_en') to <cva>.
          <vorga> = <cva>.
          perform kond_taxes in program saplmepo using 'D' 'X'.
          assign ('(SAPLMEPO)taxcom-WMWST') to <wmwst>.
          assign ('(SAPLMEPO)taxcom-WRBTR') to <wrbtr>.
          "variaveis para calculo do valor do pedido com imposto

          loop at t_ekpo into <ekpo>.
            wg_qtd_ekb = 0. "Quantidade já baixada no item
            loop at it_ekbe into wa_ekbe where ebelp = <ekpo>-ebelp.
              if wa_ekbe-shkzg = 'S'.
                add wa_ekbe-menge to wg_qtd_ekb.
              else.
                subtract wa_ekbe-menge from wg_qtd_ekb.
              endif.
            endloop.

            wa_ekbe-menge = <ekpo>-menge - wg_qtd_ekb.
            if wa_ekbe-menge gt 0.
              wg_tot_fin = wg_tot_fin + ( ( <wrbtr> + <wmwst> ) * ( wa_ekbe-menge / <ekpo>-menge ) ).
              wg_qtd_fin = wg_qtd_fin + wa_ekbe-menge.
            endif.
          endloop.

          vg_tipo = 'C'.

          if wg_tot_fin > 0.

            wa_zsdt0094-mandt         = sy-mandt.
            wa_zsdt0094-data_registro = sy-datum.
            wa_zsdt0094-hora_registro = sy-uzeit.
            wa_zsdt0094-programa      = sy-cprog.
            wa_zsdt0094-nro_sol_ov    = im_ekko-ebeln.
            wa_zsdt0094-data_venc     = im_ekko-ihran." -->vencimento_pedido_mae
            wa_zsdt0094-cadencia_qte  = wg_qtd_fin * -1.
            wa_zsdt0094-zieme         = <ekpo>-meins.
            wa_zsdt0094-total_proporc = wg_tot_fin * -1.
            wa_zsdt0094-taxa_curva = obj_tx_curva->buscar_taxa( i_data     = im_ekko-ihran
                                                                i_data_lib = sy-datum
                                                                i_tipo     = vg_tipo ).
            if vg_taxa_ant ne 0.
              wa_zsdt0094-taxa_cambio = vg_taxa_ant.
            else.
              wa_zsdt0094-taxa_cambio = wa_zsdt0094-taxa_curva.
            endif.
            wa_zsdt0094-tipo       = 'PDI'.
            wa_zsdt0094-tipo_taxa  = vg_tipo.
            wa_zsdt0094-vbeln      = wa_zmmt0035-ebeln.
            wa_zsdt0094-inco1      = im_ekko-inco1.
            insert into zsdt0094 values wa_zsdt0094.
            commit work.
            wait up to 2 seconds.

          endif.
        endif.
      endif.
    endif.


    if sy-tcode eq 'ME21N' or
       sy-tcode eq 'ME22N' or
       sy-cprog eq 'ZMMR170'.

*      IF IM_EKKO-BSART = 'NB'.
*        MESSAGE E000(Z01) WITH 'Este tipo de Pedido'
*                               'só pode ser usado em processo de LES.'.
*      ENDIF.

      select single zauart
        from zsdt0010
        into vl_zauart
      where  zauart eq im_ekko-bsart.

      check not vl_zauart is initial.

      case sy-tcode.
        when 'ME21N'.
          vl_aux = 'I'.
        when 'ME22N'.
          vl_aux = 'A'.
      endcase.

      tl_ekpo[] = im_ekpo[].
      tl_eket[] = im_eket[].

      if tl_ekpo[] is initial.
        select * into corresponding fields of table tl_ekpo
          from ekpo
         where ebeln eq im_ekko-ebeln.
      endif.

      clear: vl_local, vl_erro.
      loop at tl_ekpo into sl_ekpo.
        if 'YFTE' cs vl_zauart.                             "BUG 172998
          vl_local = sl_ekpo-werks.
        endif.
        if 'ZNB_ZFTE' cs vl_zauart.                         "BUG 172998
          vl_local = sl_ekpo-werks.
          select single mtorg
            from mbew
            into vl_mtorg
           where matnr = sl_ekpo-matnr
           and   bwkey = sl_ekpo-werks.
          if vl_mtorg ne 1 and vl_mtorg ne 6.
            vl_erro = 'X'.
          endif.

        endif.
      endloop.

      check vl_erro is initial.

      select * into corresponding fields of table tl_ekpa
        from ekpa
       where ebeln eq im_ekko-ebeln.

      loop at tl_ekpa into wa_ekpa.
        sl_vbpa-vbeln = wa_ekpa-ebeln.
        sl_vbpa-parvw = wa_ekpa-parvw.
        sl_vbpa-lifnr = wa_ekpa-lifn2.
        append sl_vbpa to tl_vbpa.
      endloop.

      loop at im_ekpa into wa_uekpa.
        sl_vbpa-vbeln = wa_uekpa-ebeln.
        sl_vbpa-parvw = wa_uekpa-parvw.
        sl_vbpa-lifnr = wa_uekpa-lifn2.
        append sl_vbpa to tl_vbpa.
      endloop.

      if vl_local is not initial.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = vl_local
          importing
            output = vl_local.

        sl_vbpa-vbeln = im_ekko-ebeln.
        sl_vbpa-parvw = 'LR'.
        sl_vbpa-kunnr = vl_local.
        append sl_vbpa to tl_vbpa.
      endif.

*---> 05/07/2023 - Migração S4 - DL
      sort tl_ekpo by ebeln ebelp.
*<--- 05/07/2023 - Migração S4 - DL

      loop at tl_ekpo into sl_ekpo.

        if tl_eket[] is initial.
          select * into corresponding fields of table tl_eket
            from eket
           where ebeln eq sl_ekpo-ebeln.
        endif.
*---> 05/07/2023 - Migração S4 - DL
        sort tl_eket by ebeln ebelp.
*<--- 05/07/2023 - Migração S4 - DL

        read table tl_eket into sl_eket
          with key ebeln = sl_ekpo-ebeln
          binary search.

        if sl_ekpo-loekz ne space.
          vl_atua = 'D'.
        else.
          vl_atua = vl_aux.
        endif.

        qt_ordem = sl_ekpo-menge.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = sl_ekpo-werks
          importing
            output = vl_kunnr.

        if im_ekko-bsart = 'ZUB'.
          vl_werks = im_ekko-reswk.
        else.
          vl_werks = sl_ekpo-werks.
        endif.

        data: lva_charg_ret type eket-charg.

        data: lwa_ekko_aux  type ekko.
        data: lwa_ekpo_aux  type ekpo.
        data: lwa_eket_aux  type eket.

        move-corresponding im_ekko to lwa_ekko_aux.
        move-corresponding sl_ekpo to lwa_ekpo_aux.
        move-corresponding sl_eket to lwa_eket_aux.

        call function 'ZMM_GET_SAFRA_PEDIDO_FOR_OPUS'
          exporting
            i_ekko  = lwa_ekko_aux
            i_ekpo  = lwa_ekpo_aux
            i_eket  = lwa_eket_aux
            i_ebeln = lwa_ekko_aux-ebeln
          importing
            e_safra = lva_charg_ret.

        sl_eket-charg = lva_charg_ret.

*        IF sl_eket-charg IS INITIAL AND 'ZNB_ZFTE' CS vl_zauart.
*          sl_eket-charg = im_ekko-unsez.
*        ENDIF.
*
*        "Ajuste Determinação Safra Para Fertilizantes
*        SELECT SINGLE *
*          FROM tvarvc INTO @DATA(lwa_tvarvc)
*         WHERE name = 'MAGGI_GR_FERTILIZANTES'
*           AND LOW  = @sl_ekpo-MATKL.
*
*        IF ( SY-SUBRC EQ 0 ) AND ( IM_EKKO-UNSEZ IS INITIAL ).
*
*          CLEAR: LT_SPLIT_SAFRA[].
*          SPLIT sl_eket-charg AT '/' INTO TABLE LT_SPLIT_SAFRA.
*
*          IF LINES( LT_SPLIT_SAFRA ) EQ 2.
*            READ TABLE LT_SPLIT_SAFRA INTO DATA(LWA_SAFRA) INDEX 2.
*            IF STRLEN( LWA_SAFRA ) EQ 4.
*              sl_eket-charg = LWA_SAFRA.
*            ENDIF.
*          ENDIF.
*        ENDIF.
        "Ajuste Determinação Safra Para Fertilizantes

        if sl_eket-charg is initial and 'ZNB_ZFTE_YFTE' cs vl_zauart.
          continue.
        endif.

                                                            "US163737
        if sl_eket-charg is initial and vl_zauart = 'ZUB' and im_ekko-submi is not initial.
          select single zsdt0053~charg
            from zsdt0051
            inner join zsdt0053 on zsdt0051~nro_sol_ov = zsdt0053~nro_sol_ov
           into sl_eket-charg
           where zsdt0051~nro_sol_ov = im_ekko-submi
           and zsdt0051~auart = 'ZUB'.

        endif.
                                                            "US163737


*--> 24.08.2023 18:28:52 - Migração S4 – ML - Início
*  CALL FUNCTION 'Z_SD_OUTBOUND_ORD_VENDA'
*    IN BACKGROUND TASK DESTINATION 'XI_ORDEM_VENDA'
*    EXPORTING
*      nu_ordem_venda = im_ekko-ebeln
*      tp_ordem_venda = im_ekko-bsart
*      nu_item        = sl_ekpo-ebelp
*      dt_ordem_venda = im_ekko-aedat
*      tp_frete       = sl_ekpo-inco1
*      id_cliente     = vl_kunnr
*      qt_ordem_venda = qt_ordem
*      cd_material    = sl_ekpo-matnr
*      vr_unitario    = sl_ekpo-netpr
*      cd_safra       = sl_eket-charg
*      cd_empresa     = im_ekko-bukrs
*      cd_centro      = vl_werks "im_ekko-reswk
*      cd_moeda       = im_ekko-waers
*      st_atualizacao = vl_atua
*      status         = im_ekko-statu
*      dt_atualizacao = sy-datum
*      hr_atualizacao = sy-uzeit
*      rg_atualizado  = '0'
*      id_interface   = '14'
*    TABLES
*      it_vbpa        = tl_vbpa.

*comentado para subir 172998
*        "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - INICIO
*        ZCL_EUDR_UTILS=>CHECK_OV_PEDIDO_EUDR(
*          EXPORTING
*            I_EBELN = IM_EKKO-EBELN     " Nº do documento de compras
*          RECEIVING
*            R_EUDR  = DATA(V_EUDR)               " Atende critérios Europeu
*        ).
*        "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - FIM
*comentado para subir 172998

        data: lv_rfc type rfcdest.

        constants: c_fm type rs38l_fnam value 'Z_SD_OUTBOUND_ORD_VENDA'.

        call function 'ZFMCPI_UTIL_GET_RFC'
          exporting
            i_fm          = c_fm
          importing
            e_rfc         = lv_rfc
          exceptions
            no_rfc        = 1
            no_rfc_config = 2
            others        = 3.

        if sy-subrc eq 0.

          call function c_fm in background task
            destination lv_rfc
            exporting
              nu_ordem_venda = im_ekko-ebeln
              tp_ordem_venda = im_ekko-bsart
              nu_item        = sl_ekpo-ebelp
              dt_ordem_venda = im_ekko-aedat
              tp_frete       = sl_ekpo-inco1
              id_cliente     = vl_kunnr
              qt_ordem_venda = qt_ordem
              cd_material    = sl_ekpo-matnr
              vr_unitario    = sl_ekpo-netpr
              cd_safra       = sl_eket-charg
              cd_empresa     = im_ekko-bukrs
              cd_centro      = vl_werks "im_ekko-reswk
              cd_moeda       = im_ekko-waers
              st_atualizacao = vl_atua
              status         = im_ekko-statu
              dt_atualizacao = sy-datum
              hr_atualizacao = sy-uzeit
              rg_atualizado  = '0'
              id_interface   = '14'
*comentado para subir 172998
*             TIPO_DOCUMENTO = 'PD'   ""VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
*             EUDR           = V_EUDR ""VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
            tables
              it_vbpa        = tl_vbpa.
        else.
          call function c_fm in background task
            exporting
              nu_ordem_venda = im_ekko-ebeln
              tp_ordem_venda = im_ekko-bsart
              nu_item        = sl_ekpo-ebelp
              dt_ordem_venda = im_ekko-aedat
              tp_frete       = sl_ekpo-inco1
              id_cliente     = vl_kunnr
              qt_ordem_venda = qt_ordem
              cd_material    = sl_ekpo-matnr
              vr_unitario    = sl_ekpo-netpr
              cd_safra       = sl_eket-charg
              cd_empresa     = im_ekko-bukrs
              cd_centro      = vl_werks "im_ekko-reswk
              cd_moeda       = im_ekko-waers
              st_atualizacao = vl_atua
              status         = im_ekko-statu
              dt_atualizacao = sy-datum
              hr_atualizacao = sy-uzeit
              rg_atualizado  = '0'
              id_interface   = '14'
*comentado para subir 172998
*             TIPO_DOCUMENTO = 'PD'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
*             EUDR           = V_EUDR "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
            tables
              it_vbpa        = tl_vbpa.
        endif.

        commit work.
*<-- 24.08.2023 18:28:52 - Migração S4 – ML – Fim

        clear: sl_ekpo,
               sl_eket.

      endloop.

    endif.


  endmethod.
ENDCLASS.
