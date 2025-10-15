*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 13/09/2010                                              &*
*& Descrição: Criação de Nota fiscal. Execução via JOB                &*
*& Transação: ZNFJ0001                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   03.08.2010                            &*
*&--------------------------------------------------------------------&*
report  zwrj0001_pan_08012024.

data: tl_zib  type table of zib_contabil  with header line,
      t_hkont type standard table of  rgsb4 with header line,
      v_vbund type bseg-vbund,
      v_bewar type bseg-bewar,
      wl_cont type sy-tabix.

data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.
data: wl_mode(1).
data:v_docnum type j_1bnfe_active-docnum,
     v_cancel type j_1bnfe_active-cancel,
     v_candat type j_1bnfdoc-candat.



types: begin of ty_obj_key,
         seq_lcto type zfiwrt0008-seq_lcto,
         obj_key  type zib_contabil_chv-obj_key,
       end of ty_obj_key,

       begin of ty_anlc,
         afabe type anlc-afabe,
         kansw type anlc-kansw,
         answl type anlc-answl,
         nafag type anlc-nafag,
         knafa type anlc-knafa,
       end of ty_anlc.

*&---------------------------------------------------------------------*
*&      Variaveis
*&---------------------------------------------------------------------*
constants: c_a type c value 'A',
           c_e type c value 'E',
           c_p type c value 'P'.
*&---------------------------------------------------------------------*
*&      Tabelas internas
*&---------------------------------------------------------------------*
data: tg_0007     type table of zfiwrt0007 with header line,
      tg_0008     type table of zfiwrt0008 with header line,
      tg_0008_aux type table of zfiwrt0008 with header line,
      tg_0009     type table of zfiwrt0009 with header line,
      tg_0011     type table of zfiwrt0011 with header line,
      tg_0012     type table of zfiwrt0012 with header line,
      tg_0020     type table of zfiwrt0020 with header line,
      tg_2000     type table of zfiwrt2000 with header line,
      tg_0010     type table of zfiwrt0010 with header line,
      tg_0015     type table of zfiwrt0015 with header line,
      tg_0013     type table of zfiwrt0013 with header line,
      tg_0001     type table of zfiwrt0001 with header line,
      tg_anlc     type table of ty_anlc    with header line,
      tg_0001_aux type table of zfiwrt0001 with header line,
      tg_1baa     type table of j_1baa     with header line,
      tg_batl1    type table of j_1batl1t   with header line,
      tg_batl2    type table of j_1batl2t   with header line,
      tg_batl4    type table of j_1batl4t  with header line,
      tg_batl5    type table of j_1batl5t   with header line,
      tg_1000     type table of zfiwrt1000 with header line,
      tg_obj_key  type table of ty_obj_key with header line,
      tg_zib_chv  type table of zib_contabil_chv with header line,
      tg_zib_cont type table of zib_contabil with header line,
      tg_zib_err  type table of zib_contabil_err with header line.

data: vg_matnr type char18.

*&---------------------------------------------------------------------*
*&      Variaveis
*&---------------------------------------------------------------------*
data: wg_wheres        type sy-tabix, "contador de registros encontrados.
      wg_tot_wheres    type sy-tabix, "contador de registros processados com exito.
      wg_par_wheres    type sy-tabix, "contador de registros parcialmente processados.
      wg_err_wheres    type sy-tabix, "contador de registros com erro.
      wl_linhas        type sy-tabix,
      wl_erro_ger(1),
      wl_erro(1),
      wg_documento(10),
      wa_j_1bnfdoc_e   type j_1bnfdoc,
      wa_j_1bnfdoc     type j_1bnfdoc,
      wa_zib           type zib_contabil.

data: ti_bdcdata type standard table of bdcdata,   "Guarda o mapeamento
      wa_bdcdata like line of ti_bdcdata.

data: _vbeln type vbeln.
data: vr_kostl type kostl.

*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
start-of-selection.
  data: vg_job      type i.

  select single count( * ) into vg_job
    from tbtco
   where jobname eq 'GESTAO_EMISSAO_NF'
     and status eq 'R'.

  if ( vg_job eq 1 ).
    perform limpa_estruturas.
    perform seleciona_dados.
    perform executa_bapi.
    perform log_execucao.
  endif.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_dados .

  data: vg_seq_lcto type zfiwed006.  "*-BUG 131030-08.01.2024-JT

  select *
    from zfiwrt0008
    into table tg_0008
     where status eq c_a
       and loekz  eq space.

*-BUG 131030-08.01.2024-JT-inicio
*-----------------------------------------------
*-para DEBUG -----------------------------------
*-----------------------------------------------
  if vg_seq_lcto is not initial.
    select *
      from zfiwrt0008
      into table tg_0008
     where seq_lcto eq vg_seq_lcto
       and status   eq c_a
       and loekz    eq space.
  endif.
*-BUG 131030-08.01.2024-JT-fim

  if sy-subrc is initial.

    loop at tg_0008 where obj_key is not initial.
      move: tg_0008-seq_lcto to tg_obj_key-seq_lcto,
            tg_0008-obj_key  to tg_obj_key-obj_key.
*      CONCATENATE 'ZGF' tg_0008-seq_lcto tg_0008-budat(4) INTO tg_obj_key-obj_key.

      append tg_obj_key.
      clear: tg_obj_key.
    endloop.

    if tg_obj_key[] is not initial.
      select *
        from zib_contabil_chv
        into table tg_zib_chv
         for all entries in tg_obj_key
         where obj_key eq tg_obj_key-obj_key.

      select *
        from zib_contabil_err
        into table tg_zib_err
         for all entries in tg_obj_key
         where obj_key eq tg_obj_key-obj_key.

      select *
          from zib_contabil
          into table tg_zib_cont
           for all entries in tg_obj_key
           where obj_key eq tg_obj_key-obj_key.
    endif.

    select *
      from zfiwrt0009
      into table tg_0009
       for all entries in tg_0008
        where seq_lcto eq tg_0008-seq_lcto.

    select *
      from zfiwrt0015
      into table tg_0015
      for all entries in tg_0008
       where seq_lcto eq tg_0008-seq_lcto.

    select *
      from zfiwrt0010
      into table tg_0010
      for all entries in tg_0008
       where seq_lcto eq tg_0008-seq_lcto.

    select *
      from zfiwrt0013
      into table tg_0013
      for all entries in tg_0008
       where seq_lcto eq tg_0008-seq_lcto.

    select *
      from zfiwrt0012
      into table tg_0012
      for all entries in tg_0008
       where seq_lcto eq tg_0008-seq_lcto.


    if sy-subrc is initial.
      select *
        from zfiwrt2000
         into table tg_2000
         for all entries in tg_0012
         where bwart eq tg_0012-bwart.

    endif.

    select *
      from zfiwrt0011
      into table tg_0011
      for all entries in tg_0008
       where seq_lcto eq tg_0008-seq_lcto
         and estorno  eq space.

    delete tg_0011 where dmbtr is initial.

    select *
      from zfiwrt0001
      into table tg_0001
      for all entries in tg_0008
      where operacao eq tg_0008-operacao.

    select  *
      from j_1baa
      into table tg_1baa
      for all entries in tg_0008
       where nftype eq tg_0008-nftype.

    select *
      from j_1batl1t
      into table tg_batl1
       for all entries in tg_0008
        where taxlaw eq tg_0008-taxlw1
          and langu  eq sy-langu.

    select *
      from j_1batl2t
      into table tg_batl2
       for all entries in tg_0008
        where taxlaw eq tg_0008-taxlw2
          and langu  eq sy-langu.

    select *
      from j_1batl4t
      into table tg_batl4
       for all entries in tg_0008
        where taxlaw eq tg_0008-taxlw4
          and langu  eq sy-langu.

    select *
      from j_1batl5t
      into table tg_batl5
       for all entries in tg_0008
        where taxlaw eq tg_0008-taxlw5
          and langu  eq sy-langu.
  endif.
endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  EXECUTA_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form executa_bapi .
  data: wl_obj_key   type zib_contabil_chv-obj_key,
        vl_docnum    type j_1bnfdoc-docnum,
        vl_parid     type j_1bnfdoc-parid,
        vl_refkey    type c length 14,
        vl_create_nf type c,
        vobj_key     type zib_contabil-obj_key,
        lw_setleaf   type setleaf.

  sort: tg_zib_chv  by obj_key,
        tg_zib_cont by obj_key,
        tg_zib_err  by obj_key type,
        tg_0009     by seq_lcto itmnum,
        tg_0011     by seq_lcto,
        tg_0012     by seq_lcto,
        tg_0001     by operacao,
        tg_0015     by seq_lcto parvw.

  data: sl_ship_point type vstel,
        sl_data_rem   type ledat,
        vl_delivery   type bapishpdelivnumb-deliv_numb,
        tl_item       type table of bapidlvreftosalesorder,
        sl_item       type bapidlvreftosalesorder,
        tl_return     type table of bapiret2 with header line,
        wl_vbap       type vbap,
        vl_vbeln(10),
        "
        sl_vbkok_wa   type vbkok,
        tl_vbpok      type table of vbpok,
        tl_prot       type table of prott,
        sl_vbpok      type vbpok,
        sl_prot       type prott,
        r_remessa     type vbeln_vl.

  free tg_0008_aux.

*  LOOP AT TG_0008.
*    CALL FUNCTION 'ENQUEUE_EZFIWRT0008'
*      EXPORTING
*        SEQ_LCTO       = TG_0008-SEQ_LCTO
*      EXCEPTIONS
*        FOREIGN_LOCK   = 1
*        SYSTEM_FAILURE = 2
*        OTHERS         = 3.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      APPEND TG_0008 TO TG_0008_AUX.
*    ENDIF.
*  ENDLOOP.

  loop at tg_0008.

*    IF LINE_EXISTS( TG_0008_AUX[ SEQ_LCTO = TG_0008-SEQ_LCTO ] ).
*      CONTINUE.
*    ENDIF.

*    IF TG_0008-DOCS_ESTORNADOS IS NOT INITIAL.
*      CONTINUE.
*    ENDIF.

    read table tg_0001 with key operacao = tg_0008-operacao into data(wa_0001) binary search.
    if sy-subrc is initial and wa_0001-aviso_rec eq 'S'.
      data(ck_erro_aviso) = abap_false.
      clear: r_remessa.
      perform f_gera_aviso tables tg_1000 using tg_0008 changing ck_erro_aviso tl_return-message r_remessa.

      if ck_erro_aviso eq abap_true.
        tg_0008-status = c_e.
        modify tg_0008.
        modify zfiwrt0008 from tg_0008.
        commit work.

        if tl_return-message is not initial.
          tl_return-type = 'E'.
          append tl_return.
          perform chama_log_bapi tables tl_return using 'VBELN_R'.
        endif.

        commit work.
        continue.

      else.
        loop at tg_0009 assigning field-symbol(<fs_0009>) where seq_lcto eq tg_0008-seq_lcto.
          <fs_0009>-vbeln_r = r_remessa.
          update zfiwrt0009 set vbeln_r = r_remessa "REMESSA
           where seq_lcto = <fs_0009>-seq_lcto
             and itmnum   = <fs_0009>-itmnum.
          commit work.
        endloop.
      endif.
    endif.

    "Gera remessa para Frete
    clear vl_vbeln.

    data(lva_erro_ger_remessa) = abap_false.

    loop at tg_0009 where seq_lcto eq tg_0008-seq_lcto and vbeln_r is initial.
      if tg_0009-vbeln is not initial.
        select single *
          from vbap
          into wl_vbap
          where vbeln = tg_0009-vbeln
          and   posnr = tg_0009-posnr.

        refresh: tl_item, tl_return.
        clear: vl_delivery, sl_item.
        "
        sl_ship_point      = wl_vbap-vstel.
        sl_data_rem        = sy-datum.
        "Item

        sl_item-ref_doc    = wl_vbap-vbeln.
        sl_item-ref_item   = wl_vbap-posnr.
        sl_item-dlv_qty    = tg_0009-menge. "Quantidade da nota
        sl_item-sales_unit = wl_vbap-vrkme.
        append sl_item to tl_item.
        call function 'BAPI_OUTB_DELIVERY_CREATE_SLS' "#EC CI_USAGE_OK[2438131]
          exporting
            ship_point        = sl_ship_point
            due_date          = sl_data_rem
          importing
            delivery          = vl_delivery
          tables
            sales_order_items = tl_item
            return            = tl_return.
        if vl_delivery is initial.

          lva_erro_ger_remessa = abap_true.

          call function 'BAPI_TRANSACTION_ROLLBACK'.
          "
          tg_0008-status = c_e.
          modify tg_0008.
          modify zfiwrt0008 from tg_0008.
          commit work.
          "
          tl_return-type = 'E'.
          vl_vbeln  = tg_0009-vbeln.

          concatenate 'Erro ao gerar REMESSA da ORDEM VENDA->' vl_vbeln into  tl_return-message separated by space.
          append tl_return.
          perform chama_log_bapi tables tl_return
                               using 'VBELN_R'.
          exit.
        else.
          call function 'BAPI_TRANSACTION_COMMIT'
            exporting
              wait = 'X'.

          update zfiwrt0009 set vbeln_r = vl_delivery "REMESSA
          where seq_lcto = tg_0009-seq_lcto
          and   itmnum   = tg_0009-itmnum.
          commit work.

          "PICKING
          refresh: tl_vbpok ,tl_prot, tl_return.
          sl_vbkok_wa-vbeln_vl  = vl_delivery.
          sl_vbkok_wa-vbeln     = vl_delivery.
          sl_vbkok_wa-wabuc     = 'X'.
          sl_vbkok_wa-wadat_ist = sy-datum.
          "
          sl_vbpok-vbeln_vl       = vl_delivery.
          sl_vbpok-posnr_vl       = 10.
          sl_vbpok-vbeln          = vl_delivery.
          sl_vbpok-posnn          = 10.
          sl_vbpok-matnr          = tg_0009-matnr.
          sl_vbpok-pikmg          = tg_0009-menge.
          sl_vbpok-charg          = wl_vbap-charg .
          sl_vbpok-gewei          = wl_vbap-meins.

          sl_vbpok-lgort          = wl_vbap-lgort.
          sl_vbpok-brgew          = tg_0009-menge.
          sl_vbpok-ntgew          = tg_0009-menge.
          append sl_vbpok to tl_vbpok.
          "
          call function 'SD_DELIVERY_UPDATE_PICKING_1'
            exporting
              vbkok_wa                 = sl_vbkok_wa
              synchron                 = 'X'
              if_error_messages_send_1 = 'X'
            tables
              vbpok_tab                = tl_vbpok
              prot                     = tl_prot.
          "
          if tl_prot[] is initial.
            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = 'X'.
          else.

            lva_erro_ger_remessa = abap_true.

            tg_0008-status = c_e.
            modify tg_0008.
            modify zfiwrt0008 from tg_0008.
            commit work.

            clear: tl_return[].

            tl_return-type = 'E'.

            loop at tl_prot into data(lwa_prot) where msgty = 'E'.
              message id lwa_prot-msgid type 'S' number lwa_prot-msgno with lwa_prot-msgv1 lwa_prot-msgv2 lwa_prot-msgv3 lwa_prot-msgv4
                 into tl_return-message.
              exit.
            endloop.

            append tl_return.
            perform chama_log_bapi tables tl_return
                                 using 'VBELN_R'.

            data: zcl_delivery type ref to zcl_delivery.
            create object zcl_delivery.
            zcl_delivery->set_nr_remessa( i_remessa = vl_delivery ).
            data(r_estornou_del) = zcl_delivery->eliminar( ).
            data(r_retorno_del)  = zcl_delivery->get_retorno_eliminar( ).
            free zcl_delivery.

            if r_estornou_del eq abap_true.
              update zfiwrt0009 set vbeln_r = space
               where seq_lcto = tg_0009-seq_lcto
               and   itmnum   = tg_0009-itmnum.
              commit work.
            endif.

            exit.

            "CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          endif.
        endif.
      endif.
    endloop.

    if lva_erro_ger_remessa is not initial. "Erro remessa
      continue.
    endif.

    clear: vl_create_nf.

    read table tg_zib_chv
      with key obj_key = tg_0008-obj_key "wl_obj_key
                 binary search.
    if sy-subrc is initial.
      read table tg_0012
      with key seq_lcto = tg_0008-seq_lcto
                  binary search.
      if sy-subrc is not initial.
        tg_0008-status = c_p.
        modify tg_0008.
        modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra no
      endif.
    endif.

    read table tg_0012
      with key seq_lcto = tg_0008-seq_lcto
                  binary search.
    if  sy-subrc is initial.
      if tg_0008-mblnr is initial.
        perform bapi_movest.
      endif.
      "
      if tg_0008-lm_estoque = 'S'.
        continue.
      endif.
    endif.

    if tg_0008-docnum is initial.
      clear: lw_setleaf, tg_0012.

      read table tg_0012
        with key seq_lcto = tg_0008-seq_lcto
                    binary search.
      if ( sy-subrc is initial and tg_0008-mblnr is not initial ) or sy-subrc is not initial.

        if sy-subrc is initial .
          select single * from setleaf into lw_setleaf where setname = 'ZNFW_CATEGORIA_NOTA'
                                                         and valfrom = tg_0012-bwart.
        endif.

        if tg_0008-mblnr is not initial.
          clear vl_docnum.

          concatenate tg_0008-mblnr tg_0008-mjahr into vl_refkey.
          "ALRS 13/07/2015
          select single docnum
            from j_1bnflin
            into vl_docnum
            where refkey = vl_refkey.

          if sy-subrc = 0.
            tg_0008-docnum = vl_docnum.
            tg_0008-status = c_p.
            modify tg_0008.
            modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
          endif.

          clear: vl_refkey.
        endif.
        if ( tg_0008-docnum is initial ) and ( lw_setleaf is initial ).
          vl_create_nf = 'X'.
          perform cria_fiscal.
        endif.
      endif.
    endif.

    if tg_0008-tp_mv_imob = 'V'.
      if tg_0008-docnum is not initial.
        select single docnum
         from j_1bnfe_active
         into v_docnum
         where docnum     =  tg_0008-docnum
         and   cancel     = ''
         and   docsta     = '1'.
        if sy-subrc = 0.
          "
          select single *
             from zib_contabil
            into wa_zib
            where obj_key = tg_0008-obj_key.
          if sy-subrc ne 0. "Se não gerou contabil ainda
            perform cria_contab_venda.
          endif.
          "Gerar um documento de venda por linha
          clear wl_erro_ger.
          loop at tg_0009  where seq_lcto eq tg_0008-seq_lcto and belnr_imb is initial. "gera as linhas que deram erro ou primeira vez
            perform f_shdbf92.
            if  wl_erro_ger = 'X'.
              exit.
            endif.
          endloop.
          if wl_erro_ger = 'X'.
            tg_0008-status = c_a.
            update zfiwrt0008 set belnr_imb = ''
                                  status    = c_a
            where seq_lcto = tg_0008-seq_lcto.
          else.
            tg_0008-status = c_p.
            update zfiwrt0008 set belnr_imb = tg_0008-belnr_imb
                                  status    = c_p
            where seq_lcto = tg_0008-seq_lcto.
          endif.
          commit work.
          modify tg_0008.
        else.
          "Tratativa BUG 65344 - BG - INICIO
          select single a~cancel b~candat
            from j_1bnfe_active as a
               inner join j_1bnfdoc as b on a~docnum = b~docnum
            into ( v_cancel, v_candat )
            where a~docnum     =  tg_0008-docnum.

          if sy-subrc = 0.
            if v_cancel = 'X' or v_candat is not initial.
              tg_0008-status = c_p.
            else.
              tg_0008-status = c_a.
            endif.
            modify tg_0008.
            modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
            commit work.
          endif.
          "Tratativa BUG 65344 - BG - FIM
        endif.
      endif.
    elseif tg_0008-tp_mv_imob = 'T' or tg_0008-tp_mv_imob = 'C'. "(comodato)
      if tg_0008-docnum is not initial.
        "SAIDA
        clear wa_j_1bnfdoc.
        select single *
         from j_1bnfdoc
         into wa_j_1bnfdoc
         where docnum     =  tg_0008-docnum.

        if ( vl_create_nf is not initial ) and
           ( wa_j_1bnfdoc-direct eq 2 ).
          "No momento da criação da Saída de Transferência,
          "efetua contabilização somente dos impostos.
          concatenate  tg_0008-obj_key 'I' into  vobj_key.
          select single *
           from zib_contabil
          into wa_zib
          where obj_key = vobj_key.
          if sy-subrc ne 0.
            perform cria_contab_trans using 'X'.
          endif.
        endif.

        "ENTRADA
        clear: wa_j_1bnfdoc_e.
        select single *
         from j_1bnfdoc
         into wa_j_1bnfdoc_e
         where direct = '1'
         and   partyp = 'B'
         and   bukrs  = wa_j_1bnfdoc-bukrs
         and   branch = wa_j_1bnfdoc-parid+4(4)
         and   pstdat ge wa_j_1bnfdoc-pstdat
*         and   docnum =  tg_0008-docnum  "US162044 comentado pra subir
         and   nfenum = wa_j_1bnfdoc-nfenum.
        if sy-subrc = 0.
          clear wl_erro_ger.
          loop at tg_0009  where seq_lcto eq tg_0008-seq_lcto and trans_status is initial.
            perform f_bapi_asset.
          endloop.

          "Caso encontre a Entrada lançada,
          "efetua contabilização das demais partidas(Exceto Impostos).
          select single *
           from zib_contabil
          into wa_zib
          where obj_key = tg_0008-obj_key.
          if sy-subrc ne 0.
            perform cria_contab_trans using ''.
          endif.

          if wl_erro_ger = 'X'.
            tg_0008-status = c_a.
          else.
            tg_0008-status = c_p.
          endif.
          modify tg_0008.
          modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
          commit work.
        else.
          "Tratativa BUG 65344 - BG - INICIO
          if tg_0008-tp_mv_imob = 'T'.
            select single a~cancel b~candat
           from j_1bnfe_active as a
              inner join j_1bnfdoc as b on a~docnum = b~docnum
           into ( v_cancel, v_candat )
           where a~docnum     =  tg_0008-docnum .

            if sy-subrc = 0.
              if v_cancel = 'X' or v_candat is not initial.
                tg_0008-status = c_p.
              else.
                tg_0008-status = c_a.
              endif.

              modify tg_0008.
              modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
              commit work.
            endif.
          elseif tg_0008-tp_mv_imob = 'C'.
            tg_0008-status = c_a.
            modify tg_0008.
            modify zfiwrt0008 from       tg_0008.
            commit work.
          endif.
          "Tratativa BUG 65344 - BG - FIM
        endif.
      endif.
    else.
      read table tg_0011
      with key seq_lcto = tg_0008-seq_lcto
                  binary search.
      if sy-subrc is initial.
        if tg_0008-docnum is not initial
        and tg_zib_chv-belnr is initial.
          perform cria_contab.
        endif.
      endif.
      "CS2016001772
      if  tg_0008-status = c_a and tg_0008-docnum is not initial.
        tg_0008-status = c_p.
        modify tg_0008.
        modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
        commit work.
      endif.
    endif.

*   "// Disparo Hedge Inicio
    data(obj_tx)    = new zcl_taxa_curva( ).
    data cx_exception type ref to zcx_webservice.
    data(r_operacao) = obj_tx->get_auart( 'ZNFW0005_HEDGE_OPERACAO' ). "// set de Tipos de Operação para Lançamento do Hedge

    if r_operacao is not initial.
      if tg_0008-operacao in r_operacao.
        if tg_0020 is not initial.

          select distinct ( refkey )
            from j_1bnflin
            into table @data(it_lin)
            for all entries in @tg_0020
            where docnum eq @tg_0020-docnum.

          if sy-subrc is initial.

            loop at it_lin into data(wa_lin).
              _vbeln = wa_lin-refkey.

              select single *
                from vbrk
                into @data(_vbrk)
                where vbeln eq @_vbeln.

              _vbrk-sfakn = _vbrk-vbeln.

*     "// Lança uma reversão do Hedge no momento da anulação das Notas
*     "// Hedge Aquaviario
              try .
                  zcl_webservice_tx_curva=>hedge_aquaviario(
                    _code = 'VF11'
                    _vbrk = _vbrk
                  ).
                catch zcx_webservice into cx_exception.
              endtry.

            endloop.
          endif.
        endif.
      endif.
    endif.

*   "// Disparo Hedge Fim

    clear: tg_0011, tg_0012, tg_zib_chv, wl_obj_key.
  endloop.

  modify zfiwrt0008 from table tg_0008.
  loop at tg_0008.
    delete from zfiwrt1000 where seq_lcto eq tg_0008-seq_lcto.
  endloop.
  modify zfiwrt1000 from table tg_1000.
  commit work and wait.
endform.                    " EXECUTA_BAPI
*&---------------------------------------------------------------------*
*&      Form  CRIA_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cria_fiscal .

  data: sl_header               type bapi_j_1bnfdoc,
        sl_header_add           type bapi_j_1bnfdoc_add,
        tl_partner              type table of bapi_j_1bnfnad,
        tl_item                 type table of bapi_j_1bnflin,
        tl_item_add             type table of bapi_j_1bnflin_add,
        tl_item_tax             type table of bapi_j_1bnfstx,
        tl_vbpa_tmp             type table of vbpa with header line,
        tl_return               type table of bapiret2 with header line,
        tl_return_aux           type table of bapiret2 with header line,
        tl_msg                  type table of bapi_j_1bnfftx,
        tl_dados_adic           type table of logbr_nf_texts_compatibility,
        sl_partner              type bapi_j_1bnfnad,
        sl_item                 type bapi_j_1bnflin,
        sl_item_add             type bapi_j_1bnflin_add,
        sl_item_tax             type bapi_j_1bnfstx,
        sl_msg                  type bapi_j_1bnfftx,
        sl_dados_adic           type logbr_nf_texts_compatibility,
        sl_return               type bapiret2,
        vl_itmnum               type j_1bnflin-itmnum,
        vl_refkey               type j_1bnflin-refkey,
        vl_docnum               type j_1bnfdoc-docnum,
        vl_docnum2(10),
        vl_chave_nfe            type c length 44,
        sl_nfcheck              type bapi_j_1bnfcheck,
        wa_material_text_record type makt,
        wl_cont                 type sy-tabix,
        wl_werks                type werks_d,
        wa_j_1baa               type j_1baa,
        wa_j_1bb2               type j_1bb2,
        wl_field_aux(20),
        wa_lfa1_key             type lfa1,
        wa_zib_nfe              type zib_nfe_forn,
        wa_kna1                 type kna1,
        wl_texto_adic           type logbr_nf_texts_compatibility-text.  "*-BUG 131030-08.01.2024-JT

  data: oo_exceptions type ref to cx_root.

  data: i_doc               type j_1bnfdoc,
        i_acttab            type j_1bnfe_active,
        wl_zib_nfe_dist_ter type zib_nfe_dist_ter,
        tg_xml_cte          type zcte_xml_sefaz_auth,
        vuf(2),
        vcmun(12).

  data: v_candat_null  type j_1bnfdoc-candat,
        lit_active_aux type table of j_1bnfe_active.


*        x_message     TYPE message,
*        x_msgid          LIKE sy-msgid,
*        x_msgno          LIKE sy-msgno,
*        x_msgty          LIKE sy-msgty.
*        x_msgv1          LIKE CLIKE,
*        x_msgv2          LIKE CLIKE,
*        x_msgv3          LIKE CLIKE,
*        x_msgv4          LIKE CLIKE.

  "ALRS Verifica se ja tem nota com a mesma referncia ZW
  select single docnum
    from j_1bnflin
    into vl_docnum
    where reftyp = 'ZW'
    and   refkey = tg_0008-seq_lcto.

  "14/04/2016 ALRS
  if sy-subrc = 0.
    tg_0008-docnum = vl_docnum.
    tg_0008-status = c_p.
    modify tg_0008.
    modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
    exit.
  endif.

  clear: sl_header, sl_header_add, sl_partner, sl_item, sl_item_add, sl_item_tax,
         sl_msg, tg_1baa, wl_cont, wl_werks, wl_field_aux, vl_docnum, wl_zib_nfe_dist_ter.

  refresh: tl_partner, tl_item, tl_item_add, tl_item_tax, tl_return, tl_msg.

  read table tg_1baa
    with key nftype = tg_0008-nftype.

* Preenchimenyto Header
  sl_header-nftype  = tg_0008-nftype.
  sl_header-doctyp  = tg_1baa-doctyp.
  sl_header-direct  = tg_1baa-direct.
  if tg_1baa-entrad is not initial.
    sl_header-entrad = tg_1baa-entrad.
  endif.
  sl_header-docdat  = tg_0008-bldat. "p_budat.
  sl_header-pstdat  = tg_0008-budat.
  sl_header-credat  = tg_0008-bldat.
  sl_header-model   = tg_1baa-model.
  sl_header-docref  = tg_0008-referencia.

                                                            "US162044
  if tg_0008-nftype = 'NS'.
    sl_header-ind_final = '0'.
  endif.
                                                            "US162044

  select single * into wa_j_1baa
    from j_1baa
   where nftype eq tg_0008-nftype.

  if not ( tg_0008-nfenum is initial ) and ( wa_j_1baa-nfe eq 'X' ) and ( wa_j_1baa-direct eq '1' ) and not ( tg_0008-tcode_org eq 'ZNFW0009' and tg_0008-form is not initial ) and
         ( tg_0008-not_check_xml eq abap_false ).

    clear: wa_kna1, wa_lfa1_key.

    if ( tg_1baa-partyp eq 'C' ).
      select single * into wa_kna1
       from kna1
      where kunnr eq tg_0008-parid.

    else.
      select single * into wa_lfa1_key
        from lfa1
      where lifnr eq tg_0008-parid.
    endif.

    if ( sy-subrc eq 0 ).

      data: lva_numero_nf type zib_nfe_dist_ter-numero,
            lva_serie_nf  type zib_nfe_dist_ter-serie,
            lva_cnpj_emi  type zib_nfe_dist_ter-forne_cnpj,
            lva_cpf_emi   type zib_nfe_dist_ter-forne_cpf.

      lva_numero_nf  = tg_0008-nfenum.
      lva_serie_nf   = tg_0008-series.

      if ( wa_kna1 is not initial ).
        lva_cnpj_emi = wa_kna1-stcd1.
        lva_cpf_emi  = wa_kna1-stcd2.
      else.
        lva_cnpj_emi  = wa_lfa1_key-stcd1.
        lva_cpf_emi   = wa_lfa1_key-stcd2.
      endif.

*  US180498
      if lva_cnpj_emi is not initial.
*  US180498
        if tg_1baa-model = '57'.
          select single * into @data(lwa_zib_cte_dist_ter)
               from zib_cte_dist_ter
              where emit_cnpj  eq @lva_cnpj_emi
                and numr_cte   eq @lva_numero_nf
                and numr_serie eq @lva_serie_nf
                and dt_emissao eq @tg_0008-bldat.
        else.
          select single * into @data(lwa_zib_nfe_dist_ter)
            from zib_nfe_dist_ter
           where forne_cnpj eq @lva_cnpj_emi
             and numero     eq @lva_numero_nf
             and serie      eq @lva_serie_nf.
        endif.

      elseif lva_cpf_emi is not initial.
*  US180498
        if tg_1baa-model = '57'.
          select single * into lwa_zib_cte_dist_ter
          from zib_cte_dist_ter
         where emit_cpf   eq lva_cpf_emi
           and numr_cte   eq lva_numero_nf
           and numr_serie eq lva_serie_nf
           and dt_emissao eq tg_0008-bldat.
        else.
          select single * into lwa_zib_nfe_dist_ter
            from zib_nfe_dist_ter
           where forne_cpf  eq lva_cpf_emi
             and numero     eq lva_numero_nf
             and serie      eq lva_serie_nf.
        endif.
      else.
        sy-subrc = 4.
      endif.

      if ( sy-subrc ne 0 ).
        sl_msg-message = 'Arquivo XML da NF-e não cadastrado.'.
        append sl_msg to tl_msg.
        clear: sl_msg.
      else.
*  US180498
        if tg_1baa-model = '57'.
          call function 'Z_DETALHAMENTO_CTE'
            exporting
              i_chave_nfe = lwa_zib_cte_dist_ter-cd_chave_cte
            importing
              e_xml_cte   = tg_xml_cte.

          condense tg_xml_cte-cteproc-cte-infcte-ide-ufini.
          condense tg_xml_cte-cteproc-cte-infcte-ide-cmunini.
          vuf   = tg_xml_cte-cteproc-cte-infcte-ide-ufini.
          vcmun = tg_xml_cte-cteproc-cte-infcte-ide-cmunini.
          concatenate vuf vcmun into sl_header-cte_strt_lct separated by space.
          "
          condense tg_xml_cte-cteproc-cte-infcte-ide-uffim.
          condense tg_xml_cte-cteproc-cte-infcte-ide-cmunfim.
          vuf   = tg_xml_cte-cteproc-cte-infcte-ide-uffim.
          vcmun = tg_xml_cte-cteproc-cte-infcte-ide-cmunfim.
          concatenate vuf vcmun into sl_header-cte_end_lct separated by space.

        else.
          sl_header-access_key = lwa_zib_nfe_dist_ter-chave_nfe.
          sl_header-docstat    = 1.
          sl_header-tpemis     = lwa_zib_nfe_dist_ter-chave_nfe+34(1).
        endif.
      endif.

    endif.
  endif.

  if ( wa_j_1baa-nfe is not initial ) and ( wa_j_1baa-form is initial ) and ( tg_0008-not_check_xml eq abap_true ) and ( tg_0008-access_key is not initial ).
    sl_header-docstat = '1'.
    sl_header-tpemis  = tg_0008-access_key+34(1).
  endif.

  if sy-subrc is initial.
    if wa_j_1baa-form is not initial.
      select single * into wa_j_1bb2
        from j_1bb2
       where bukrs  eq tg_0008-bukrs
         and branch eq tg_0008-branch
         and form   eq wa_j_1baa-form.

      if sy-subrc is initial.
        move wa_j_1bb2-series to sl_header-series.
      endif.
    else.
      sl_header-series = tg_0008-series.
    endif.
  endif.





  sl_header-inco1   = tg_0008-inco1.
  sl_header-inco2   = tg_0008-inco2.
*  sl_header-nfnum   = 1.
*  CONDENSE sl_header-series.
  sl_header-manual  = 'X'.
  sl_header-waerk   = 'BRL'.
  sl_header-bukrs   = tg_0008-bukrs.
  sl_header-branch  = tg_0008-branch.
  sl_header-parvw   = tg_0008-parvw.
  if sl_header-parvw eq 'BR'.
    wl_field_aux = tg_0008-parid.
    shift wl_field_aux left deleting leading '0'.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wl_field_aux
      importing
        output = wl_werks.

    concatenate  tg_0008-bukrs wl_werks into sl_header-parid.
  else.
    sl_header-parid   = tg_0008-parid.
  endif.
  sl_header-nfe     = tg_1baa-nfe.

  if sl_header-nfe is initial.
    if tg_1baa-form  is initial.
      sl_header-nfnum      = tg_0008-nfenum.
      sl_header-series     = tg_0008-series.
    endif.
  else.
    if tg_1baa-form  is initial.
      sl_header-nfenum   = tg_0008-nfenum.
      sl_header-series   = tg_0008-series.
    endif.
  endif.

  if ( tg_1baa-nfe       is not initial ) and
     ( tg_1baa-form      is not initial ) and
     ( tg_0008-tcode_org eq 'ZNFW0009'  ) and
     ( tg_0008-form      is not initial ).

    if tg_0008-access_key is initial.
      tg_0008-status = c_e.
      modify tg_0008.

      clear: tl_return_aux[], tl_return_aux.

      tl_return_aux-type    = 'E'.
      tl_return_aux-message = 'Não foi informado a chave de acesso no documento!'.
      append tl_return_aux.
      perform chama_log_bapi tables tl_return_aux using 'DOCNUM'.
      exit.
    endif.

    select single *
      from zib_nfe_dist_ter into wl_zib_nfe_dist_ter
     where chave_nfe = tg_0008-access_key.

    if ( sy-subrc ne 0 ).
      tg_0008-status = c_e.
      modify tg_0008.

      clear: tl_return_aux[], tl_return_aux.

      tl_return_aux-type    = 'E'.
      tl_return_aux-message = 'Não foi encontrado o registro de distribuição do XML da NF-e!'.
      append tl_return_aux.
      perform chama_log_bapi tables tl_return_aux using 'DOCNUM'.
      exit.
    endif.

    sl_header-access_key = tg_0008-access_key.
    sl_header-tpemis     = sl_header-access_key+34(1).
  endif.

* Preenche Header ADD
*  sl_header_add-nftot = ' '."tg_0008-netwr.
  loop at tg_0009
    where seq_lcto eq tg_0008-seq_lcto.

    add tg_0009-netwr to sl_header_add-nftot.

  endloop.

* Preenche NFCHECK
  sl_nfcheck-chekcon = 'X'.
  clear wl_werks.
  loop at tg_0015
    where seq_lcto eq tg_0008-seq_lcto.
* Preenche Partner
    sl_partner-parvw  = tg_0015-parvw.

    if tg_0015-parvw eq 'AG'
    or tg_0015-parvw eq 'LR'.
      sl_partner-partyp = 'C'.
      sl_partner-parid  = tg_0015-parid.
      append sl_partner to tl_partner.
    elseif tg_0015-parvw eq 'BR'.
      sl_partner-partyp = 'B'.
      shift tg_0015-parid left deleting leading '0'.
      wl_werks  = tg_0015-parid(4).
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wl_werks
        importing
          output = wl_werks.

      concatenate  tg_0008-bukrs wl_werks into sl_partner-parid.
      append sl_partner to tl_partner.
    elseif tg_0015-parvw eq 'LF' or tg_0015-parvw = 'Z1'.
      sl_partner-partyp = 'V'.
      sl_partner-parid  = tg_0015-parid.
      append sl_partner to tl_partner.
    endif.


  endloop.

  loop at tg_0009
    where seq_lcto eq tg_0008-seq_lcto.

    clear: vg_matnr.
    tg_0009-matnr = |{ tg_0009-matnr alpha = out }|.
    vg_matnr = tg_0009-matnr.
    vg_matnr = |{ vg_matnr alpha = in }|.
    tg_0009-matnr = vg_matnr.

* Preenche Item
    add 10 to vl_itmnum.
    call function 'J_1B_MATERIAL_READ'
      exporting
        matnr                = tg_0009-matnr
        val_area             = tg_0009-bwkey
        val_type             = space
        language             = sy-langu
        i_werks              = tg_0008-branch
      importing
        nbm                  = sl_item-nbm
        matuse               = sl_item-matuse
        matorg               = sl_item-matorg
        material_text_record = wa_material_text_record
        e_matkl              = sl_item-matkl
      exceptions
        material_not_found   = 1
        valuation_not_found  = 2
        others               = 3.

    if sy-subrc ne 0.
      message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(wl_message_erro).

      tg_0008-status = c_e.
      modify tg_0008.

      tl_return-type = 'E'.
      tl_return-message = wl_message_erro.
      append tl_return.

      perform chama_log_bapi tables tl_return using 'DOCNUM'.

      return.
    endif.

    if sl_item-matuse is initial.
      sl_item-matuse = '0'.
      sl_item-matorg = '0'.
    endif.


    select single * from zfiwrt0006 into @data(wa_0006)
      where operacao eq @tg_0008-operacao.

    sl_item-maktx   = wa_material_text_record-maktx.

    sl_item-itmnum  = vl_itmnum.

    clear: vg_matnr.
    tg_0009-matnr = |{ tg_0009-matnr alpha = out }|.
    vg_matnr = tg_0009-matnr.
    vg_matnr = |{ vg_matnr alpha = in }|.
    tg_0009-matnr = vg_matnr.

* ---> S4 Migration - 04/07/2023 - FTM - Início
*    sl_item-matnr   = tg_0009-matnr.
    data(v_len) = strlen( tg_0009-matnr ).
    if v_len > 18.
      sl_item-matnr_long = tg_0009-matnr.
    else.
      sl_item-matnr      = tg_0009-matnr.
    endif.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
    sl_item-bwkey   = tg_0009-bwkey.
*  sl_item-taxsit  = sl_lin-taxsit.
*    sl_item-taxsit  = '50'.
*    sl_item-taxsi2  = sl_lin-taxsi2.
    sl_item-refkey  = tg_0008-seq_lcto.
    sl_item-reftyp  = 'ZW'.
    sl_item-menge   = tg_0009-menge.
    sl_item-meins   = tg_0009-meins.
    sl_item-charg   = tg_0009-charg.
    sl_item-itmtyp  = tg_0009-itmtyp.
    sl_item-werks   = tg_0009-bwkey.
    sl_item-cfop_10 = tg_0009-cfop.
    sl_item-netpr   = tg_0009-netpr."s_val-netwrt / s_val-menge.
    sl_item-netwr   = tg_0009-netwr.
*    sl_item-netwr   = tg_0009-netpr * tg_0009-menge.

    if wa_0006-taxlw3 is not initial.
      sl_item-tmiss = 'X'.
      sl_item-taxlw3 = wa_0006-taxlw3.
    else.
      sl_item-taxlw1  = tg_0008-taxlw1.                       "'IC5'.
      sl_item-taxlw2  = tg_0008-taxlw2.                       "'I03'.
    endif.

    sl_item-netdis  =  tg_0009-netdis * -1.
    sl_item-netfre  =  tg_0009-netfre.
    sl_item-netins  =  tg_0009-netins.
    sl_item-netoth  =  tg_0009-netoth.
    sl_item-netwr   = sl_item-netwr - sl_item-netdis - ( sl_item-netfre + sl_item-netins + sl_item-netoth ).

    "
    sl_item-taxlw4  = tg_0008-taxlw4.                       "'C08'.
    sl_item-taxlw5  = tg_0008-taxlw5.                       "'P08'.
    sl_item-incltx  = 'X'.
    append sl_item to tl_item.
    clear: sl_item.
* Preenche Item ADD
    sl_item_add-itmnum = vl_itmnum.
    sl_item_add-direct = sl_header-direct.
    append sl_item_add to tl_item_add.
    clear: sl_item_add.

** Preenche Item TAX
    loop at tg_0010
      where seq_lcto eq tg_0008-seq_lcto
        and itmnum   eq tg_0009-itmnum.

      sl_item_tax-itmnum = vl_itmnum.
      sl_item_tax-taxtyp = tg_0010-taxtyp.
      sl_item_tax-base   = tg_0010-base.
      sl_item_tax-rate   = tg_0010-rate.
      sl_item_tax-taxval = tg_0010-taxval.
      sl_item_tax-excbas = tg_0010-excbas.
      sl_item_tax-othbas = tg_0010-othbas.

*      SUBTRACT tg_0010-rate FROM sl_item-netwr.
      append sl_item_tax to tl_item_tax.
      clear: sl_item_tax.
    endloop.

    if tg_0009-vbeln is not initial.
      clear: tl_vbpa_tmp[].
      select *
        from vbpa into table tl_vbpa_tmp
       where vbeln eq tg_0009-vbeln.

      loop at tl_vbpa_tmp where ( ( parvw eq 'PC' ) or
                                  ( parvw eq 'LR' ) ).

        read table tl_partner into sl_partner with key parvw = tl_vbpa_tmp-parvw.
        if sy-subrc ne 0.
          clear: sl_partner.
          sl_partner-parvw  = tl_vbpa_tmp-parvw.

          case tl_vbpa_tmp-parvw.
            when 'PC'.
              sl_partner-parid  = tl_vbpa_tmp-lifnr.
              sl_partner-partyp = 'V'.
            when 'LR'.
              sl_partner-partyp = 'C'.
              sl_partner-parid  = tl_vbpa_tmp-kunnr.
          endcase.

          append sl_partner to tl_partner.
        endif.
      endloop.
    endif.

  endloop.

** Texto de Taxlaw1
  read table tg_batl1
    with key taxlaw = tg_0008-taxlw1.

  if sy-subrc is initial.
    if tg_batl1-line1 is not initial
    or tg_batl1-line2 is not initial
    or tg_batl1-line3 is not initial
    or tg_batl1-line4 is not initial.
      add 1 to wl_cont.

      if tg_batl1-line1 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl1-line1.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.

      if tg_batl1-line2 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl1-line2.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.

      if tg_batl1-line3 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl1-line3.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.
      if tg_batl1-line4 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl1-line4.

        append sl_msg to tl_msg.
*
      endif.
      clear: sl_msg.
    endif.
  endif.
** Texto de Taxlaw2
  read table tg_batl2
    with key taxlaw = tg_0008-taxlw2.

  if sy-subrc is initial.
    if tg_batl2-line1 is not initial
    or tg_batl2-line2 is not initial
    or tg_batl2-line3 is not initial
    or tg_batl2-line4 is not initial.
      add 1 to wl_cont.

      if tg_batl2-line1 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl2-line1.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.

      if tg_batl2-line2 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl2-line2.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.

      if tg_batl2-line3 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl2-line3.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.
      if tg_batl2-line4 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl2-line4.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.
      endif.
      clear: sl_msg.
    endif.
  endif.

** Texto de Taxlaw4
  read table tg_batl4
    with key taxlaw = tg_0008-taxlw4.

  if sy-subrc is initial.
    if tg_batl4-line1 is not initial
    or tg_batl4-line2 is not initial
    or tg_batl4-line3 is not initial
    or tg_batl4-line4 is not initial.
      add 1 to wl_cont.

      if tg_batl4-line1 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl4-line1.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.

      if tg_batl4-line2 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl4-line2.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.

      if tg_batl4-line3 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl4-line3.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.
      if tg_batl4-line4 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl4-line4.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.
      endif.
      clear: sl_msg.
    endif.
  endif.

** Texto de Taxlaw5
  read table tg_batl5
    with key taxlaw = tg_0008-taxlw5.

  if sy-subrc is initial.
    if tg_batl5-line1 is not initial
    or tg_batl5-line2 is not initial
    or tg_batl5-line3 is not initial
    or tg_batl5-line4 is not initial.
      add 1 to wl_cont.
      if tg_batl5-line1 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl5-line1.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.

      if tg_batl5-line2 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl5-line2.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.

      if tg_batl5-line3 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl5-line3.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.

      endif.
      if tg_batl5-line4 is not initial.
        sl_msg-seqnum  = wl_cont.
        add 1 to sl_msg-linnum.
        sl_msg-message = tg_batl5-line4.

        append sl_msg to tl_msg.
*        CLEAR: sl_msg.
      endif.
      clear: sl_msg.
    endif.
  endif.

*  REFRESH TG_0001_AUX.
*  SELECT *
*  FROM ZFIWRT0001
*  INTO TABLE TG_0001_AUX
*  WHERE OPERACAO EQ TG_0008-OPERACAO.
*
*  DESCRIBE TABLE TG_0001_AUX LINES WL_LINHAS.

*-BUG 131030-08.01.2024-JT-inicio
  free: wl_texto_adic.
  loop at tg_0013 where seq_lcto eq tg_0008-seq_lcto.
    wl_texto_adic = wl_texto_adic && tg_0013-message && '-'.
  endloop.

  sl_dados_adic-counter = '0001'.
  sl_dados_adic-type    = 'C'.
  sl_dados_adic-text    = wl_texto_adic.
  append sl_dados_adic to tl_dados_adic.
  clear: sl_dados_adic.

*  LOOP AT tg_0013
*    WHERE seq_lcto EQ tg_0008-seq_lcto.
*    "LP- ajuste dados adicionais
**
**    sl_msg-seqnum  = tg_0013-seqnum + wl_cont.
**    sl_msg-linnum  = tg_0013-linnum.
**    sl_msg-message = tg_0013-message.
**
**    "IF SY-TABIX > WL_LINHAS.
**    sl_msg-manual = 'M'.
**    "ENDIF.
**
**    APPEND sl_msg TO tl_msg.
**    CLEAR: sl_msg.
*    sl_dados_adic-counter = tg_0013-seqnum + wl_cont.
*    " sl_dados_adic-itmnum = tg_0013-linnum.
*    sl_dados_adic-text = tg_0013-message.
*    sl_dados_adic-type =  'C'.
*
*    APPEND sl_dados_adic TO tl_dados_adic.
*    CLEAR: sl_dados_adic.
*  ENDLOOP.
*-BUG 131030-08.01.2024-JT-fim

  data(_error) = ''.
  perform f_check_duplicidade_nfe changing _error.
  check _error is initial.

* Cria NF
  try.
      call function 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
        exporting
          obj_header     = sl_header
          obj_header_add = sl_header_add
          nfcheck        = sl_nfcheck
        importing
          e_docnum       = vl_docnum
        tables
          obj_partner    = tl_partner
          obj_item       = tl_item
          obj_item_add   = tl_item_add
          obj_item_tax   = tl_item_tax
          obj_header_msg = tl_msg
          obj_texts      = tl_dados_adic
          return         = tl_return.
    cleanup into oo_exceptions.
      data(msg) = oo_exceptions->get_text( ).
      tl_return = value #(
      type           = 'E'
      id             = ''
      number         = ''
      message        = msg ).
  endtry.

  if vl_docnum is not initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

    " CS2018000302 - Corrigir erro ZNFW
    wait up to 2 seconds.
    select single *
      into @data(wdoc)
      from j_1bnfdoc
      where docnum = @vl_docnum.

    if sy-subrc = 0.

      "18.02.2019 - CS2018003078
      if ( ( tg_1baa-nfe       is not initial ) and
           ( tg_1baa-form      is not initial ) and
           ( tg_0008-tcode_org eq 'ZNFW0009'  ) and
           ( tg_0008-form      is not initial ) )
         or
        ( tg_1baa-nfe is not initial and tg_1baa-form is initial and tg_0008-not_check_xml eq abap_true and tg_0008-access_key is not initial ).

        clear: i_doc, i_acttab.

        select single * into i_acttab
          from j_1bnfe_active
         where docnum eq vl_docnum.

        select single * into i_doc
          from j_1bnfdoc
         where docnum eq vl_docnum.

        if ( i_acttab is not initial ) and ( i_doc is not initial ).

          i_doc-nfenum     = tg_0008-access_key+25(9).
          i_doc-series     = tg_0008-access_key+22(3).
          i_doc-docstat    = '1'.

          if wl_zib_nfe_dist_ter-nr_protocolo is not initial.
            i_doc-authcod    = wl_zib_nfe_dist_ter-nr_protocolo.
            i_doc-authdate   = wl_zib_nfe_dist_ter-dt_protocolo.
            i_doc-authtime   = wl_zib_nfe_dist_ter-hr_protocolo.
          else.

            clear: lit_active_aux[].

            select a~* into corresponding fields of table @lit_active_aux
              from j_1bnfe_active as a inner join j_1bnfdoc as b on a~docnum = b~docnum
             where a~regio    eq @tg_0008-access_key(2)
               and a~nfyear   eq @tg_0008-access_key+2(2)
               and a~nfmonth  eq @tg_0008-access_key+4(2)
               and a~stcd1    eq @tg_0008-access_key+6(14)
               and a~model    eq @tg_0008-access_key+20(2)
               and a~serie    eq @tg_0008-access_key+22(3)
               and a~nfnum9   eq @tg_0008-access_key+25(9)
               and a~docnum9  eq @tg_0008-access_key+34(9)
               and a~cdv      eq @tg_0008-access_key+43(1)
               and a~direct   eq '2'
               and a~form     ne ' '
               and b~candat   eq @v_candat_null
               and b~cancel   eq @space
               and b~doctyp   in ('1','2','6').

            if lit_active_aux[] is not initial.
              read table lit_active_aux into data(lwa_active_aux) index 1.
              i_doc-authcod    = lwa_active_aux-authcod.
              i_doc-authdate   = lwa_active_aux-authdate.
              i_doc-authtime   = lwa_active_aux-authtime.
            endif.

          endif.

          i_acttab-nfnum9   = i_doc-nfenum.
          i_acttab-serie    = i_doc-series.
          i_acttab-authcod  = i_doc-authcod.
          i_acttab-authdate = i_doc-authdate.
          i_acttab-authtime = i_doc-authtime.
          i_acttab-docnum9  = tg_0008-access_key+34(9)  .
          i_acttab-docsta   = i_doc-docstat.
          i_acttab-cdv      = tg_0008-access_key+43(1).

          call function 'J_1B_NFE_UPDATE_ACTIVE'
            exporting
              i_acttab  = i_acttab
              i_doc     = i_doc
              i_updmode = 'U'.

          commit work.
        endif.
      endif.

      tg_0008-docnum = vl_docnum.
      read table tg_0012
        with key seq_lcto = tg_0008-seq_lcto.
      if sy-subrc is not initial.
        tg_0008-status = c_p.
      else.
        tg_0008-status = c_a.
      endif.
      read table tg_0011
        with key seq_lcto = tg_0008-seq_lcto.
      if sy-subrc is not initial.
        tg_0008-status = c_p.
      else.
        tg_0008-status = c_a.
      endif.
      modify tg_0008.
      modify zfiwrt0008 from tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota

      "Verificação de Documento Anulado Referênciado
      "CFOP 1206 - Anulação de valor relativo à prestação de serviço de transporte
      "CFOP 2206 - Anulação de valor relativo à prestação de serviço de transporte
      read table tg_0009 with key seq_lcto = tg_0008-seq_lcto.

      if tg_0009-cfop(4) eq '1206' or tg_0009-cfop(4) eq '2206'.
        select * into table tg_0020
          from zfiwrt0020
         where seq_lcto eq tg_0008-seq_lcto.

        loop at tg_0020.
          "Anular Documento.
          call method zcl_cte=>set_anular_cte_saida
            exporting
              i_docnum = tg_0020-docnum.
        endloop.

      endif.
    else.
      tg_0008-status = c_e.
      modify tg_0008.
      tl_return-type = 'E'.
      vl_docnum2  = vl_docnum.
      concatenate 'Erro ao gerar doc.fiscal, não encontrado->' vl_docnum2 into  tl_return-message separated by space.
      append tl_return.
      perform chama_log_bapi tables tl_return
                           using 'DOCNUM'.
    endif.
    " CS2018000302 - Corrigir erro ZNFW
  else.
    tg_0008-status = c_e.
    modify tg_0008.

    perform chama_log_bapi tables tl_return
                           using 'DOCNUM'.
  endif.
endform.                    " CRIA_FISCAL
*&---------------------------------------------------------------------*
*&      Form  BAPI_MOVEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bapi_movest .
  data: wl_header   type bapi2017_gm_head_01,
        wl_code     type bapi2017_gm_code,
        wl_mblnr    type zfiwrt0008-mblnr,
        wl_mjahr    type mjahr,
        vg_seq_lcto type zfiwrt0008-seq_lcto,
        wl_refkey   type j_1bnflin-refkey,
        tl_item     type table of bapi2017_gm_item_create with header line,
        tl_return   type table of bapiret2 with header line,
        tl_0009     type table of zfiwrt0009 with header line,
*** Stefanini - IR237366 - 03/07/2025 - FINC - Início de Alteração
        tl_0010     type table of zfiwrt0010 with header line.
*** Stefanini - IR237366 - 03/07/2025 - FINC - Fim de Alteração
  clear: wl_header, tl_item, wl_code, wl_mblnr, wl_mjahr.
  refresh: tl_item, tl_return, tl_0009.


  tl_0009[] = tg_0009[].
  delete tl_0009 where seq_lcto ne tg_0008-seq_lcto.

*** Stefanini - IR237366 - 03/07/2025 - FINC - Início de Alteração
  tl_0010[] = tg_0010[].
  delete tl_0010 where seq_lcto ne tg_0008-seq_lcto.
*** Stefanini - IR237366 - 03/07/2025 - FINC - Fim de Alteração

  read table tg_0012
        with key seq_lcto = tg_0008-seq_lcto.

  if sy-subrc is initial.
    clear tg_2000.
    read table tg_2000
      with key bwart = tg_0012-bwart.
    "
    if sy-subrc = 0 and tg_2000-function_name is not initial.
      export tg_0008-seq_lcto to memory id 'VMSEQ_LCTO'.
      call function 'ZNFW_BUSCA_PARAMETROS_BAPI'
        exporting
          i_function_name  = tg_2000-function_name
          i_zfiwrt0008     = tg_0008
          i_zfiwrt0012     = tg_0012
        importing
          materialdocument = wl_mblnr
          matdocumentyear  = wl_mjahr
        tables
          it_zfiwrt0009    = tl_0009
*** Stefanini - IR237366 - 03/07/2025 - FINC - Início de Alteração
*          it_zfiwrt0010    = tl_0010
*** Stefanini - IR237366 - 03/07/2025 - FINC - Fim de Alteração
          return           = tl_return.


      if wl_mblnr is not initial.
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = 'X'.
        move: wl_mblnr to tg_0008-mblnr,
              wl_mjahr to tg_0008-mjahr,
              c_p      to tg_0008-status.
        concatenate tg_0008-mblnr tg_0008-mjahr into wl_refkey.
        select single *
          from j_1bnflin
          into @data(wlin)
          where refkey = @wl_refkey
          and   reftyp = 'MD'.
        if sy-subrc = 0.
          select single *
            from j_1bnfdoc
            into @data(wdoc)
            where docnum =  @wlin-docnum.
          tg_0008-docnum = wdoc-docnum.
          tg_0008-nftype = wdoc-nftype.
          tg_0008-form   = wdoc-form.
        endif.
        modify tg_0008.
        modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
        commit work.
      else.
        tg_0008-status = c_e.
        modify tg_0008.
        modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
        commit work.
        perform chama_log_bapi tables tl_return
                               using 'MBLNR'.
      endif.
    else.
      tg_0008-status = c_e.
      modify tg_0008.
      modify zfiwrt0008 from       tg_0008. "ALRS Grava imediatamente para que se o JOB atrasar não gere outra nota
      commit work.
      tl_return-type = 'E'.
      tl_return-message = 'Não encontrada configuração para baixa estoque'.
      append tl_return.
      perform chama_log_bapi tables tl_return
                             using 'MBLNR'.

    endif.
  endif.
  clear vg_seq_lcto.
  export vg_seq_lcto to memory id 'VMSEQ_LCTO'.
endform.                    " BAPI_MOVEST
*&---------------------------------------------------------------------*
*&      Form  LIMPA_ESTRUTURAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form limpa_estruturas .
  clear: tg_0007,
         tg_0008,
         tg_0009,
         tg_0011,
         tg_0012,
         tg_0010,
         tg_0015,
         tg_0013,
         tg_0001,
         tg_0012,
         tg_0011,
         tg_1baa,
         tg_1000,
         tg_batl1,
         tg_batl2,
         tg_batl4,
         tg_batl5.

  refresh: tg_0007,
           tg_0008,
           tg_0009,
           tg_0011,
           tg_0012,
           tg_0010,
           tg_0015,
           tg_0013,
           tg_0001,
           tg_0012,
           tg_0011,
           tg_1baa,
           tg_1000,
           tg_batl1,
           tg_batl2,
           tg_batl4,
           tg_batl5.
endform.                    " LIMPA_ESTRUTURAS
*&---------------------------------------------------------------------*
*&      Form  LOG_EXECUCAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form log_execucao .
  data : tl_0008_aux like table of tg_0008 with header line.

  describe table tg_0008 lines wg_wheres.

** Conta registros com erros.
  tl_0008_aux[] = tg_0008[].
  delete tl_0008_aux where docnum is not initial
                       and mblnr  is not initial
                       and mjahr  is not initial.

  describe table tl_0008_aux lines wg_err_wheres.
** Conta registros parcialmente processados.
  tl_0008_aux[] = tg_0008[].
  delete tl_0008_aux where docnum is initial
                       and mblnr is not initial
                       and mjahr is not initial
                       and status eq c_e.

  describe table tl_0008_aux lines wg_par_wheres.
** Conta registros totalmente processados.
  tl_0008_aux[] = tg_0008[].
  delete tl_0008_aux where docnum is initial
                       and mblnr  is initial
                       and mjahr  is initial.

  describe table tl_0008_aux lines wg_tot_wheres.

  write: 'Registros Totalmente Processados:  ', wg_tot_wheres.
  write: /.
  write: 'Registros Parcialmente Processados:', wg_par_wheres.
  write: /.
  write: 'Total de Registros com Erro:       ', wg_err_wheres.
  write: /.
  write: 'Total de Registros Encontrados :   ', wg_wheres.

  write: /,/,/,
        'Para os registros que tiveram erro ou foram parcialmente processados é possivel ver o log da bapi atraves da tabela de log.'.

endform.                    " LOG_EXECUCAO
*&---------------------------------------------------------------------*
*&      Form  CHAMA_LOG_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_RETURN  text
*----------------------------------------------------------------------*
form chama_log_bapi  tables   tl_return structure bapiret2
                     using    value(p_field).
  data: wl_cont type sy-tabix.

  clear: wl_cont.

  loop at tl_return.
    add 1 to wl_cont.
    move: sy-mandt           to tg_1000-mandt,
          tg_0008-seq_lcto   to tg_1000-seq_lcto,
          p_field            to tg_1000-field,
          wl_cont            to tg_1000-buzei,
          tl_return-type     to tg_1000-type,
          tl_return-message  to tg_1000-mensagem,
          sy-datum           to tg_1000-erdat,
          sy-uzeit           to tg_1000-ertim.


    append tg_1000.
    clear: tg_1000.
  endloop.
endform.                    " CHAMA_LOG_BAPI
*&---------------------------------------------------------------------*
*&      Form  CRIA_CONTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cria_contab .
  data: tl_zib  type table of zib_contabil  with header line,
        t_hkont type standard table of  rgsb4 with header line,
        v_vbund type bseg-vbund,
        v_bewar type bseg-bewar,
        wl_cont type sy-tabix.


* 126778 - PANF - 06.11 - INICIO
  data: lr_katyp type range of cskb-katyp.

  lr_katyp = value #( ( sign = 'I'
                        option = 'EQ'
                        low = '11')
                       ( sign = 'I'
                        option = 'EQ'
                        low = '12')  ).

  if tg_0011 is not initial.

    select single *
      from tka02
      into @data(ls_tka02)
      where bukrs = @tg_0008-bukrs.

    if sy-subrc = 0.

      select kokrs, kstar, katyp
        from cskb
        into table @data(lt_cskb)
        for all entries in @tg_0011
        where kokrs  = @ls_tka02-kokrs
              and kstar   =  @tg_0011-hkont
              and datbi >=  @sy-datum .

    endif.

  endif.

* 126778 - PANF - 06.11 - FIM

  call function 'G_SET_GET_ALL_VALUES'
    exporting
      class         = '0000'
      setnr         = 'CONTAS_EC-CS'
    tables
      set_values    = t_hkont
    exceptions
      set_not_found = 1
      others        = 2.

  read table tg_0001 with key operacao = tg_0008-operacao binary search.
  read table tg_0009 with key seq_lcto = tg_0008-seq_lcto binary search.



  read table tg_zib_cont
    with key obj_key = tg_0008-obj_key
               binary search.

  if sy-subrc is initial.
    if tg_zib_cont-rg_atualizado eq 'S'.
      read table tg_zib_err
        with key obj_key = tg_zib_cont-obj_key
                 type    = 'E'
                   binary search.
      if sy-subrc is initial.
        tg_0008-status = c_e.
        modify tg_0008.
      endif.
    elseif tg_zib_cont-rg_atualizado eq 'N'.

    endif.

  else.

    clear: tl_zib.
    refresh: tl_zib.
    loop at tg_0011
       where seq_lcto eq tg_0008-seq_lcto.

      if sy-subrc is initial.

        concatenate 'GF' tg_0008-seq_lcto into tl_zib-xblnr.
        clear v_vbund.
        if tg_0011-vbund is not initial.
          v_vbund = tg_0011-vbund.
        else.
          read table t_hkont with key from = tg_0011-hkont.
          if sy-subrc = 0.
            select single vbund into v_vbund from kna1 where lifnr = tg_0008-parid.
          endif.
        endif.
        "
        clear v_bewar.
        select single bewar
          into v_bewar
          from zfit0030
         where hkont eq tg_0011-hkont
           and cond  eq ''.


        add 1 to wl_cont.
        if tg_0011-wrbtr eq 0.
          move tg_0011-dmbtr        to tl_zib-wrbtr.
          move 'BRL'                to tl_zib-waers.
        else.
          move tg_0011-waers        to tl_zib-waers.
          move tg_0011-wrbtr        to tl_zib-wrbtr.
          "
          move tg_0011-dmbtr        to tl_zib-dmbtr.
          move tg_0011-waers_i      to tl_zib-waers_i.
        endif.

        if tg_0011-dmbe2 ne 0.
          move tg_0011-curha        to tl_zib-waers_f.
          move tg_0011-dmbe2        to tl_zib-dmbe2.
        endif.

        if tl_zib-dmbe3 ne 0.
          move tg_0011-curin        to tl_zib-waers_g.
          move tg_0011-dmbe3        to tl_zib-dmbe3.
        endif.
        "

        clear: vg_matnr.
        tg_0011-artnr = |{ tg_0011-artnr alpha = out }|.
        vg_matnr = tg_0011-artnr.
        vg_matnr = |{ vg_matnr alpha = in }|.
        tg_0011-artnr = vg_matnr.


        move: wl_cont              to tl_zib-seqitem,
              tg_0008-obj_key      to tl_zib-obj_key,
              tg_0011-bschl        to tl_zib-bschl,
              tg_0008-branch       to tl_zib-gsber,
              tg_0008-bukrs        to tl_zib-bukrs,
              v_vbund              to tl_zib-vbund,
              v_bewar              to tl_zib-bewar,
              '3'                  to tl_zib-interface,
              'Gestão Emissão NF'  to tl_zib-bktxt,
              tg_0008-budat(4)     to tl_zib-gjahr,
              tg_0008-budat+4(2)   to tl_zib-monat,
              'WR'                 to tl_zib-blart,
              tg_0011-hkont        to tl_zib-hkont,

              tg_0011-artnr        to tl_zib-matnr,

              tg_0008-branch       to tl_zib-bupla,

              'N'                  to tl_zib-rg_atualizado,
              tg_0011-newbw        to tl_zib-newbw,
              tg_0011-zlsch        to tl_zib-zlsch,
              tg_0011-kostl        to tl_zib-kostl,
              tg_0011-umskz        to tl_zib-umskz.
*              tg_0011-ZFBDT        to tl_zib-ZFBDT.

        if tg_0001-transf_icms = 'C'.
          if tg_0011-bschl = '40'.
            tl_zib-gsber = tg_0008-parid+6(4).
            tl_zib-bupla = tg_0008-parid+6(4).
          elseif tg_0011-bschl = '50'.
            tl_zib-gsber = tg_0009-bwkey.
            tl_zib-bupla = tg_0009-bwkey.
          endif.
        elseif tg_0001-transf_icms = 'D'.
          if tg_0011-bschl = '50'.
            tl_zib-gsber = tg_0008-parid+6(4).
            tl_zib-bupla = tg_0008-parid+6(4).
          elseif tg_0011-bschl = '40'.
            tl_zib-gsber = tg_0009-bwkey.
            tl_zib-bupla = tg_0009-bwkey.
          endif.
        endif.

* 126778 - PANF - 06.11 - INICIO
        try.
            if lt_cskb[ kokrs  = ls_tka02-kokrs
                        kstar   = tg_0011-hkont ]-katyp  in lr_katyp.

              tl_zib-prctr =  '9900'.
              tl_zib-matnr =  tg_0009-matnr.
            endif.

          catch cx_sy_itab_line_not_found.

        endtry.
* 126778 - PANF - 06.11 - Fim

        if tg_0011-zfbdt is not initial.
          concatenate tg_0011-zfbdt+6(2) tg_0011-zfbdt+4(2) tg_0011-zfbdt(4) into tl_zib-zfbdt separated by '.'.
        endif.
        concatenate tg_0008-bldat+6(2) tg_0008-bldat+4(2) tg_0008-bldat(4) into tl_zib-bldat separated by '.'.
        concatenate tg_0008-budat+6(2) tg_0008-budat+4(2) tg_0008-budat(4) into tl_zib-budat separated by '.'.

        if tl_zib-wrbtr lt 0.
          multiply tl_zib-wrbtr by -1.
        endif.

        if tl_zib-dmbtr lt 0.
          multiply tl_zib-dmbtr by -1.
        endif.

        if tl_zib-dmbe2 lt 0.
          multiply tl_zib-dmbe2 by -1.
        endif.

        if tl_zib-dmbe3 lt 0.
          multiply tl_zib-dmbe3 by -1.
        endif.

        if tg_0011-newbw is initial.
          move: tg_0009-menge  to tl_zib-quantity,
                tg_0009-meins  to tl_zib-base_uom.
          append tl_zib.

        else.
          loop at tg_0009
            where seq_lcto eq tg_0008-seq_lcto.
            move: tg_0009-anln1 to tl_zib-anln1,
                  tg_0009-anln2 to tl_zib-anln2.
            move: tg_0009-menge  to tl_zib-quantity,
                  tg_0009-meins  to tl_zib-base_uom.
            append tl_zib.
          endloop.
        endif.
*        tg_0008-obj_key = tl_zib-obj_key.
        modify tg_0008.
        clear: tl_zib.
      endif.
    endloop.

    modify zib_contabil from table tl_zib.
  endif.
endform.                    " CRIA_CONTAB
*&---------------------------------------------------------------------*
*&      Form  F_SHDBF92
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_shdbf92 .
  refresh ti_bdcdata.
  data: v_msg(50),
        v_data(10),
        wl_qtdn     type i,
        vmenge      type anla-menge,
        vktokd      type kna1-ktokd,
        v_fval      type c,
        v_dynpro(4) type c,
        wl_qtd(10),
        wl_vlr(16).

  read table tg_0001 with key operacao = tg_0008-operacao binary search.
  "READ TABLE TG_0009 WITH KEY SEQ_LCTO = TG_0008-SEQ_LCTO BINARY SEARCH.

  wl_qtdn = tg_0009-menge.
  write wl_qtdn to wl_qtd.

  write tg_0009-netwr to wl_vlr.

  clear vmenge.
  select single menge
    from anla
    into vmenge
    where bukrs = tg_0008-bukrs
    and   anln1 = tg_0009-anln1
    and   anln2 = tg_0009-anln2.

  concatenate 'GENF-'  tg_0008-seq_lcto into v_msg.
  concatenate tg_0008-bldat+6(2) tg_0008-bldat+4(2) tg_0008-bldat(4) into v_data separated by '.'.

  v_dynpro = '0303'.
  v_fval = 'I'.
  "Nos casos de Intercompany, passar o razão especial 'J'
  select single ktokd
    from kna1
    into vktokd
   where kunnr = tg_0008-parid
     and ktokd = 'ZCIC'.

  if sy-subrc = 0.
    v_fval = 'J'.
    v_dynpro = '0304'.
  endif.

  perform f_bdc_data using:
        'SAPMF05A'  '0100'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '/00',
        ''          ''      ''   'BKPF-BLDAT'       v_data,
        ''          ''      ''   'BKPF-BLART'       'DR',
        ''          ''      ''   'BKPF-BUKRS'       tg_0008-bukrs,
        ''          ''      ''   'BKPF-BUDAT'       v_data,
        ''          ''      ''   'BKPF-MONAT'       tg_0008-budat+4(2),
        ''          ''      ''   'BKPF-WAERS'       'BRL',
        ''          ''      ''   'BKPF-BKTXT'       v_msg,
        ''          ''      ''   'RF05A-NEWBS'      '09',
        ''          ''      ''   'RF05A-NEWKO'      tg_0008-parid,
        ''          ''      ''   'RF05A-NEWUM'      v_fval.

  perform f_bdc_data using:
       'SAPMF05A'  v_dynpro 'X'  ''                 ' ',
       ''          ''      ''   'BDC_OKCODE'        '/00',
       ''          ''      ''   'BSEG-WRBTR'       wl_vlr,
       ''          ''      ''   'BSEG-BUPLA'       tg_0009-bwkey,
       ''          ''      ''   'BSEG-GSBER'       tg_0009-bwkey,
       ''          ''      ''   'BSEG-ZFBDT'       v_data,
       ''          ''      ''   'RF05A-NEWBS'      '50',
       ''          ''      ''   'RF05A-NEWKO'      '350000'.


  perform f_bdc_data using:
       'SAPMF05A'  '0300'  'X'  ''                 ' ',
       ''          ''      ''   'BDC_OKCODE'        '/00',
       ''          ''      ''   'BSEG-WRBTR'       wl_vlr,
       ''          ''      ''   'BSEG-BUPLA'       tg_0009-bwkey,
       ''          ''      ''   'RF05A-XAABG'      'X',
       ''          ''      ''   'COBL-GSBER'       tg_0009-bwkey.


  perform f_bdc_data using:
      'SAPLFAA_MF05A'  '0210'  'X'  ''                  ' ',
            ''         ''      ''   'BDC_OKCODE'        '=GOON',
            ''         ''      ''   'GS_RLAMBU-ANLN1'   tg_0009-anln1,
            ''         ''      ''   'GS_RLAMBU-ANLN2'   tg_0009-anln2,
            ''         ''      ''   'GS_RLAMBU-BWASL'   '210',
            ''         ''      ''   'GS_RLAMBU-BZDAT'   v_data,
            ''         ''      ''   'GS_RLAMBU-XVABG'   'X'.
*            ''         ''      ''   'GS_RLAMBU-ANBTR'   wl_vlr.


  perform f_bdc_data using:
       'SAPMF05A'  '0300'  'X'  ''                 ' ',
       ''          ''      ''   'BDC_OKCODE'       '=BU',
       ''          ''      ''   'BSEG-WRBTR'       wl_vlr,
       ''          ''      ''   'BSEG-BUPLA'       tg_0009-bwkey,
       ''          ''      ''   'BSEG-SGTXT'       'Fatura cliente Receita Venda Ativos',
       ''          ''      ''   'COBL-GSBER'       tg_0009-bwkey,
       ''          ''      ''   'COBL-ANLN1'       tg_0009-anln1,
       ''          ''      ''   'COBL-ANLN2'       tg_0009-anln2.


  clear wl_erro.
  perform zf_call_transaction using 'F-92' changing wl_erro.

  wait up to 5 seconds.

  if wl_erro = 'X'.
    rollback work.
    clear tg_0008-belnr_imb.
    wl_erro_ger = 'X'.
  else.
    commit work.
    tg_0008-belnr_imb = wg_documento.
    modify tg_0008.
    tg_0009-belnr_imb = wg_documento.
    modify tg_0009.
    update zfiwrt0009 set  belnr_imb = tg_0009-belnr_imb
        where seq_lcto = tg_0009-seq_lcto
        and   itmnum   = tg_0009-itmnum.
    commit work.
  endif.
endform.                    " F_SHDBF92
*&---------------------------------------------------------------------*
*&      Form  CRIA_CONTAB_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cria_contab_venda .
  data: tl_zib     type table of zib_contabil  with header line,
        wl_cont    type sy-tabix,
        xv_020     type anlc-nafal,
        xv_030     type anlc-nafal,
        xv_050     type anlc-nafal,
        xv_taxval  type zfiwrt0010-taxval,
        wl_danfe   type j_1bnfdoc-nfenum,
        wl_seqitem type zib_contabil-seqitem.

  select single nfenum
    from j_1bnfdoc
    into wl_danfe
    where docnum = tg_0008-docnum.

  read table tg_0001 with key operacao = tg_0008-operacao binary search.
  "READ TABLE TG_0009 WITH KEY SEQ_LCTO = TG_0008-SEQ_LCTO BINARY SEARCH.

  wl_seqitem = 0.
  refresh: tl_zib.
  loop at tg_0009  where seq_lcto eq tg_0008-seq_lcto.
    select afabe kansw answl nafag knafa
      from anlc
      into table tg_anlc
      where bukrs	=	tg_0008-bukrs
      and   anln1	=	tg_0009-anln1
      and   anln2	=	tg_0009-anln2
      and   gjahr	=	tg_0008-budat+0(4)
      and   afabe	in (20,30,50).

    select single taxval
      from zfiwrt0010
      into xv_taxval
      where seq_lcto =  tg_0009-seq_lcto
      and   itmnum   =  tg_0009-itmnum
      and   taxtyp   =  'ICM3'.

    clear:  xv_020, xv_030, xv_050.

    read table tg_anlc with key afabe = '20'.
    if sy-subrc = 0.
      xv_020 = tg_anlc-kansw + tg_anlc-answl + tg_anlc-nafag + tg_anlc-knafa.
    endif.

    read table tg_anlc with key afabe = '30'.
    if sy-subrc = 0.
      xv_030 = tg_anlc-kansw + tg_anlc-answl + tg_anlc-nafag + tg_anlc-knafa..
    endif.

    read table tg_anlc with key afabe = '50'.
    if sy-subrc = 0.
      xv_050 = tg_anlc-kansw + tg_anlc-answl + tg_anlc-nafag + tg_anlc-knafa..
    endif.

    "para cada imobilizado gera 8 linhas
    do 8 times.
      clear: tl_zib.
      tl_zib-obj_key    = tg_0008-obj_key.
      "TL_ZIB-SEQITEM    = SY-INDEX.
      "
      concatenate tg_0008-budat+4(2) '/' tg_0008-budat+0(4) into tl_zib-xblnr.
      concatenate 'DANFE:' wl_danfe into tl_zib-xblnr.

      if sy-index = 1 or sy-index = 3 or sy-index = 5 or sy-index = 7.
        tl_zib-bschl      = '40'.
      elseif sy-index = 2 or sy-index = 4 or sy-index = 6 or sy-index = 8.
        tl_zib-bschl      = '50'.
      endif.

      tl_zib-gsber      = tg_0009-bwkey.
      tl_zib-bukrs      = tg_0008-bukrs.
      tl_zib-interface    = '35'.
      concatenate 'GENF-' tg_0008-seq_lcto into  tl_zib-bktxt.
      concatenate tg_0008-bldat+6(2) tg_0008-bldat+4(2) tg_0008-bldat(4) into tl_zib-bldat separated by '.'.
      concatenate tg_0008-budat+6(2) tg_0008-budat+4(2) tg_0008-budat(4) into tl_zib-budat separated by '.'.
      tl_zib-gjahr      = tg_0008-budat+0(4).
      tl_zib-monat      = tg_0008-budat+4(2).
      tl_zib-blart      = 'AB'.

      if sy-index = 1.
        tl_zib-hkont      = '0000512001'.
      elseif sy-index = 2.
        tl_zib-hkont      = '0000121401'.
      elseif sy-index = 3.
        tl_zib-hkont      = '0000512002'.
      elseif sy-index = 4.
        tl_zib-hkont      = '0000121402'.
      elseif sy-index = 5.
        tl_zib-hkont      = '0000512003'.
      elseif sy-index = 6.
        tl_zib-hkont      = '0000121403'.
      elseif sy-index = 7.
        tl_zib-hkont      = '0000423007'.
      elseif sy-index = 8.
        tl_zib-hkont      = '0000213000'.
      endif.

      if sy-index = 1 or sy-index = 2.
        tl_zib-wrbtr      = xv_050.
        tl_zib-dmbtr      = xv_050.
        concatenate 'VENDA IMOB. AREA : 050' tg_0009-anln1 '-' tg_0009-anln2 into tl_zib-sgtxt.
      elseif sy-index = 3 or sy-index = 4.
        tl_zib-wrbtr      = xv_020.
        tl_zib-dmbtr      = xv_020.
        concatenate 'VENDA IMOB. AREA : 020' tg_0009-anln1 '-' tg_0009-anln2 into tl_zib-sgtxt.
      elseif sy-index = 5 or sy-index = 6.
        tl_zib-wrbtr      = xv_030.
        tl_zib-dmbtr      = xv_030.
        concatenate 'VENDA IMOB. AREA : 030' tg_0009-anln1 '-' tg_0009-anln2 into tl_zib-sgtxt.
      elseif sy-index = 7 or sy-index = 8.
        tl_zib-wrbtr      = xv_taxval.
        tl_zib-dmbtr      = xv_taxval.
        concatenate 'VENDA IMOB. :' tg_0009-anln1 '-' tg_0009-anln2 into tl_zib-sgtxt.
      endif.
      if tl_zib-wrbtr le 0.
        continue.
      endif.
      "numera linha
      add 1 to wl_seqitem.
      tl_zib-seqitem    = wl_seqitem.

      tl_zib-waers      = 'BRL'.
      tl_zib-bupla      =  tg_0009-bwkey.

      tl_zib-waers_i    = 'BRL'.
      tl_zib-rg_atualizado  = 'N'.
      "ALRS
      select single *
        from tka02
        into @data(ls_tka02)
        where bukrs = @tg_0008-bukrs.

      if sy-subrc = 0.
        select count(*)
          from cskb
          where kokrs  = @ls_tka02-kokrs
                and kstar   =  @tl_zib-hkont
                and katyp   in ( '11', '12' )
                and datbi >=  @sy-datum .
        if sy-subrc = 0.
          tl_zib-prctr =  '9900'.
          tl_zib-matnr =  tg_0009-matnr.
        endif.

      endif.
      "ALRS
      append tl_zib.
    enddo.
  endloop.
  "
  modify zib_contabil from table tl_zib.
  commit work and wait.

endform.                    " CRIA_CONTAB_VENDA

*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*----------------------------------------------------------------------*
form zf_call_transaction using p_trans changing p_erro.
  constants: c_msgid like it_msg-msgid value 'F5',
             c_msgnr like it_msg-msgnr value '312',
             c_msgne like it_msg-msgnr value '539'.

  data: tl_return type table of bapiret2 with header line,
        vfield    type zfiwrt1000-field.
  refresh: tl_return, it_msg.
  data opt type ctu_params.

  opt-dismode = 'E'.
  opt-nobinpt = 'X'.

  call transaction p_trans using ti_bdcdata options from opt
          messages into it_msg.

*  wl_mode = 'E'.
*  CALL TRANSACTION p_trans USING ti_bdcdata
*          MODE wl_mode
*          MESSAGES INTO it_msg.

  read table it_msg with key msgtyp = 'A'.
  if sy-subrc = 0.
    p_erro = 'X'.
  else.
    read table it_msg with key msgtyp = 'E'.
    if sy-subrc = 0.
      p_erro = 'X'.
    endif.
  endif.

  clear wg_documento.

  read table it_msg with key msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

  if sy-subrc = 0.
    move it_msg-msgv1 to wg_documento.
    clear p_erro.
  endif.

  if  wg_documento is initial.
    p_erro = 'X'.
    loop at it_msg where msgtyp = 'E'.
      move: it_msg-msgid  to sy-msgid,
            it_msg-msgtyp to sy-msgty,
            it_msg-msgnr  to sy-msgno,
            it_msg-msgv1  to sy-msgv1,
            it_msg-msgv2  to sy-msgv2,
            it_msg-msgv3  to sy-msgv3,
            it_msg-msgv4  to sy-msgv4.

      call function 'MESSAGE_PREPARE'
        exporting
          language = sy-langu
          msg_id   = sy-msgid
          msg_no   = it_msg-msgnr
          msg_var1 = sy-msgv1
          msg_var2 = sy-msgv2
          msg_var3 = sy-msgv3
          msg_var4 = sy-msgv4
        importing
          msg_text = tl_return-message.
      tl_return-type = 'E'.
      append tl_return.
    endloop.
    vfield = tg_0009-itmnum.
    condense vfield no-gaps.
    concatenate 'BELNR_IMB' vfield into vfield.
    perform chama_log_bapi tables tl_return
                            using vfield.
  else.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wg_documento
      importing
        output = wg_documento.
  endif.


endform.                    "ZF_CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
form f_bdc_data  using p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  clear wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  append wa_bdcdata to ti_bdcdata.

endform.                    " F_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  CRIA_CONTAB_TRANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cria_contab_trans using p_contab_imposto .
  data: tl_zib      type table of zib_contabil  with header line,
        wl_cont     type sy-tabix,
        xv_020      type anlc-nafal,
        xv_030      type anlc-nafal,
        xv_050      type anlc-nafal,
        xv_taxval   type zfiwrt0010-taxval,
        vl_seq_item type sy-tabix,
        wl_seqitem  type zib_contabil-seqitem.

  data: lr_katyp type range of cskb-katyp.


  refresh tl_zib.
  read table tg_0001 with key operacao = tg_0008-operacao binary search.
  "READ TABLE TG_0009 WITH KEY SEQ_LCTO = TG_0008-SEQ_LCTO BINARY SEARCH.

  wl_seqitem = 0.

  loop at tg_0009 where seq_lcto = tg_0008-seq_lcto.
    select afabe kansw answl nafag knafa
    from anlc
    into table tg_anlc
    where bukrs	=	tg_0008-bukrs
    and   anln1	=	tg_0009-anln1
    and   anln2	=	tg_0009-anln2
    and   gjahr	=	tg_0008-budat+0(4)
    and   afabe	in (20,30,50).

    select single taxval
      from zfiwrt0010
      into xv_taxval
      where seq_lcto =  tg_0009-seq_lcto
      and   itmnum   =  tg_0009-itmnum
      and   taxtyp   =  'ICM3'.

    clear:  xv_020, xv_030, xv_050.

    read table tg_anlc with key afabe = '20'.
    if sy-subrc = 0.
      xv_020 = tg_anlc-kansw + tg_anlc-answl + tg_anlc-nafag + tg_anlc-knafa.
    endif.

    read table tg_anlc with key afabe = '30'.
    if sy-subrc = 0.
      xv_030 = tg_anlc-kansw + tg_anlc-answl + tg_anlc-nafag + tg_anlc-knafa.
    endif.

    read table tg_anlc with key afabe = '50'.
    if sy-subrc = 0.
      xv_050 = tg_anlc-kansw + tg_anlc-answl + tg_anlc-nafag + tg_anlc-knafa.
    endif.

    do 4 times. "faz somente uma conta 10.03.2015 ALRS

      clear: vl_seq_item.

      case sy-index.
        when 1.
          if p_contab_imposto is not initial.
            continue.
          endif.
          vl_seq_item = 1.
        when 2.
          if p_contab_imposto is not initial.
            continue.
          endif.
          vl_seq_item = 2.
        when 3.
          if p_contab_imposto is initial.
            continue.
          endif.
          vl_seq_item = 1.
        when 4.
          if p_contab_imposto is initial.
            continue.
          endif.
          vl_seq_item = 2.
      endcase.


      clear: tl_zib.
      if p_contab_imposto is not initial..
        concatenate  tg_0008-obj_key 'I' into  tl_zib-obj_key.
      else.
        tl_zib-obj_key  = tg_0008-obj_key.
      endif.

      tl_zib-seqitem    = vl_seq_item.

      concatenate  wa_j_1bnfdoc-nfenum '-'  wa_j_1bnfdoc-series into tl_zib-xblnr.

      if sy-index = 1 or sy-index = 3 or sy-index = 5.
        tl_zib-bschl      = '40'.
        tl_zib-gsber      = tg_0008-move_plant.

        if sy-index = 3.
          tl_zib-gsber = tg_0009-bwkey.
        endif.

      elseif sy-index = 2 or sy-index = 4 or sy-index = 6.
        tl_zib-gsber      = tg_0009-bwkey.
        tl_zib-bschl      = '50'.
      endif.

      tl_zib-bukrs      = tg_0008-bukrs.
      tl_zib-interface    = '35'.
      concatenate 'GENF-' tg_0008-seq_lcto into  tl_zib-bktxt.

      if ( tg_0008-tp_mv_imob     = 'T'          ) and
         ( p_contab_imposto       is initial     ) and
         ( wa_j_1bnfdoc_e         is not initial ) and
         ( wa_j_1bnfdoc_e-direct  = 1            ).

        concatenate wa_j_1bnfdoc_e-docdat+6(2) wa_j_1bnfdoc_e-docdat+4(2) wa_j_1bnfdoc_e-docdat(4) into tl_zib-bldat separated by '.'.
        concatenate wa_j_1bnfdoc_e-pstdat+6(2) wa_j_1bnfdoc_e-pstdat+4(2) wa_j_1bnfdoc_e-pstdat(4) into tl_zib-budat separated by '.'.

        tl_zib-gjahr      = wa_j_1bnfdoc_e-pstdat+0(4).
        tl_zib-monat      = wa_j_1bnfdoc_e-pstdat+4(2).
      else.
        concatenate tg_0008-bldat+6(2) tg_0008-bldat+4(2) tg_0008-bldat(4) into tl_zib-bldat separated by '.'.
        concatenate tg_0008-budat+6(2) tg_0008-budat+4(2) tg_0008-budat(4) into tl_zib-budat separated by '.'.

        tl_zib-gjahr      = tg_0008-budat+0(4).
        tl_zib-monat      = tg_0008-budat+4(2).
      endif.

      tl_zib-blart      = 'AB'.
      if tg_0008-tp_mv_imob ne 'C'.
        if sy-index = 1.
          tl_zib-hkont      = '0000121401'.
        elseif sy-index = 2.
          tl_zib-hkont      = '0000121401'.
        elseif sy-index = 3.              "Referente a Contab. de Imposto
*** Stefanini - IR239682 - 02/06/2025 - LAZAROSR - Início de Alteração
*          tl_zib-hkont      = '0000423007'.
          tl_zib-hkont      = '0000332013'.
*** Stefanini - IR239682 - 02/06/2025 - LAZAROSR - Fim de Alteração
        elseif sy-index = 4.              "Referente a Contab. de Imposto
          tl_zib-hkont      = '0000213000'.
        endif.

        if sy-index = 1 or sy-index = 2.
          tl_zib-wrbtr      = xv_050.
          tl_zib-dmbtr      = xv_050.
          concatenate 'TRANSF. IMOB. AREA : 050' tg_0009-anln1 '-' tg_0009-anln2 into tl_zib-sgtxt.
        elseif sy-index = 3 or sy-index = 4.
          tl_zib-wrbtr      = xv_taxval.
          tl_zib-dmbtr      = xv_taxval.
          concatenate 'TRANSF. IMOB. :' tg_0009-anln1 '-' tg_0009-anln2 into tl_zib-sgtxt.
        endif.
      else.
        if tg_0008-operacao = '1212' or tg_0008-operacao = '1515'. "Remessa/Entrada comodato
          if sy-index = 1.
            tl_zib-hkont      = '0000191252'.
          elseif sy-index = 2.
            tl_zib-hkont      = '0000291252'.
          elseif sy-index = 3.
            tl_zib-hkont      = '0000191202'.
          elseif sy-index = 4.
            tl_zib-hkont      = '0000291202'.
          endif.
        elseif tg_0008-operacao = '1516' or tg_0008-operacao = '1514'. "Retorno/Devolução comodato
          if sy-index = 1.
            tl_zib-hkont      = '0000291252'.
          elseif sy-index = 2.
            tl_zib-hkont      = '0000191252'.
          elseif sy-index = 3.
            tl_zib-hkont      = '0000291202'.
          elseif sy-index = 4.
            tl_zib-hkont      = '0000191202'.
          endif.
        endif.

        tl_zib-wrbtr      = xv_050.
        tl_zib-dmbtr      = xv_050.
        concatenate 'COMODATO. IMOB. AREA : 050' tg_0009-anln1 '-' tg_0009-anln2 into tl_zib-sgtxt.
      endif.

      if tl_zib-wrbtr le 0.
        continue.
      endif.

*      "numera linha
      add 1 to wl_seqitem.
      tl_zib-seqitem    = wl_seqitem.

      tl_zib-waers      = 'BRL'.
      if tl_zib-bschl      = '40'.
        tl_zib-bupla      = tg_0008-move_plant.
      else.
        tl_zib-bupla      =  tg_0009-bwkey.
      endif.
      tl_zib-waers_i    = 'BRL'.
      tl_zib-rg_atualizado  = 'N'.
      "ALRS
      select single *
        from tka02
        into @data(ls_tka02)
        where bukrs = @tg_0008-bukrs.

      if sy-subrc = 0.
        select count(*)
          from cskb
          where kokrs  = @ls_tka02-kokrs
                and kstar   =  @tl_zib-hkont
                and katyp   in ( '11', '12' )
                and datbi >=  @sy-datum .
        if sy-subrc = 0.
          tl_zib-prctr =  '9900'.
          tl_zib-matnr =  tg_0009-matnr.
        endif.

      endif.
      "ALRS
      append tl_zib.
    enddo.
  endloop.
  modify zib_contabil from table tl_zib.
  commit work .

endform.                    " CRIA_CONTAB_TRANS
*&---------------------------------------------------------------------*
*&      Form  F_BAPI_ASSET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_bapi_asset .

  data:ls_tdd    type bapi1022_feglg003,
       ls_tddx   type bapi1022_feglg003x,
       tl_return type table of bapiret2 with header line,
       vfield    type zfiwrt1000-field.

  clear: vr_kostl.
  select single kostl
  from anlz
  into vr_kostl
  where bukrs = tg_0008-bukrs
    and anln1 = tg_0009-anln1
    and anln2 = tg_0009-anln2.

  read table tg_0001 with key operacao = tg_0008-operacao binary search.
*  READ TABLE TG_0009 WITH KEY SEQ_LCTO = TG_0008-SEQ_LCTO BINARY SEARCH.

  update anlz set   gsber = tg_0008-move_plant
    where           anln1 = tg_0009-anln1
    and             anln2 = tg_0009-anln2
    and             bukrs = tg_0008-bukrs.
  commit work.

  ls_tdd-costcenter = tg_0008-kostl.
  ls_tdd-plant      = tg_0008-move_plant.
*  LS_TDD-BUS_AREA   = TG_0008-MOVE_PLANT.
  "
  ls_tddx-costcenter = 'X'.
  ls_tddx-plant      = 'X'.
*  LS_TDDX-BUS_AREA   = 'X'.
  refresh: tl_return.
  call function 'BAPI_FIXEDASSET_CHANGE'
    exporting
      companycode        = tg_0008-bukrs
      asset              = tg_0009-anln1
      subnumber          = tg_0009-anln2
      timedependentdata  = ls_tdd
      timedependentdatax = ls_tddx
    importing
      return             = tl_return.

  read table tl_return with key type = 'E'.
  if sy-subrc ne 0.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
    update zfiwrt0009 set  trans_status = 'X'
       where seq_lcto = tg_0009-seq_lcto
       and   itmnum   = tg_0009-itmnum.
    commit work.
    "
    vfield = tg_0009-itmnum.
    condense vfield no-gaps.
    concatenate 'TRANSF' vfield into vfield.
    perform chama_log_bapi tables tl_return
                           using vfield.

**********************************************************************
* 112114 CS2023000132 Contabilizar as transferências de ativo imobilizado - PSA
**********************************************************************
*    perform cria_contab_trans_dest. comentei pra subir  US162044

  else.
    wl_erro_ger = 'X'.
    vfield = tg_0009-itmnum.
    condense vfield no-gaps.
    concatenate 'TRANSF' vfield into vfield.
    perform chama_log_bapi tables tl_return
                           using vfield.
  endif.
endform.                    " F_BAPI_ASSET

form f_check_duplicidade_nfe changing p_error.

  data: tl_return  type table of bapiret2 with header line.

  data: v_werks  type werks_d,
        v_lifnr  type lifnr,
        v_nftype type j_1bnftype,
        v_parvw  type j_1bparvw,
        v_data   type invdt,
        v_xblnr  type xblnr1.

  data: v_ck_somente_dup type char01.

  clear: p_error, v_werks, v_lifnr, v_nftype, v_parvw, v_data, v_xblnr, tl_return[], v_ck_somente_dup.

  check ( tg_0008 is not initial ) and ( tg_0008-nftype is not initial ).

  select single * from j_1baa into @data(_wl_1baa) where nftype eq @tg_0008-nftype.

  check ( sy-subrc = 0 ) and ( _wl_1baa-form is initial ).

  v_werks    = tg_0008-branch.
  v_lifnr    = tg_0008-parid.
  v_nftype   = tg_0008-nftype.
  v_parvw    = tg_0008-parvw.
  v_data     = tg_0008-bldat.
  concatenate tg_0008-nfenum '-' tg_0008-series into v_xblnr.

  if tg_0008-not_check_xml eq abap_true.
    v_ck_somente_dup = abap_true.
  endif.

  call function 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
    exporting
      p_lifnr          = v_lifnr
      p_parvw          = v_parvw
      p_nftype         = v_nftype
      p_xblnr          = v_xblnr
      p_data           = v_data
      p_werks          = v_werks
      p_tcode          = 'ZNFW0002'
      p_ck_somente_dup = v_ck_somente_dup
    exceptions
      error            = 1
      others           = 2.

  if sy-subrc ne 0.
    p_error = 'X'.
    tg_0008-status = c_e.
    modify tg_0008.

    tl_return-type = 'E'.
    concatenate sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into  tl_return-message separated by space.
    append tl_return.
    perform chama_log_bapi tables tl_return using 'DOCNUM'.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_GERA_AVISO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TG_0008_SEQ_LCTO  text
*      <--P_TL_RETURN_MESSAGE  text
*----------------------------------------------------------------------*
form f_gera_aviso  tables it_1000 structure zfiwrt1000
                   using    wa_zfiwrt0008 type zfiwrt0008
                   changing c_erro type char01
                            c_message type bapi_msg
                            r_remessa type vbeln_vl.

  data: i_item   type zde_bapi_remessa_item,
        i_parid	 type j_1bparid,
        i_xblnr	 type xblnr_v1,
        l_serie  type c length 3,
        lc_aviso type ref to zcl_aviso_recebimento.

  read table tg_0015 with key seq_lcto = wa_zfiwrt0008-seq_lcto parvw = 'SP' into data(wa_parceiro_sp) binary search.
  if sy-subrc is not initial.
    concatenate 'Erro ao gerar Aviso de Recebimento->' 'Agente de Frete deve ser Informado!' into c_message separated by space.
    c_erro = abap_true.
    exit.
  endif.

  read table tg_0015 with key seq_lcto = tg_0008-seq_lcto parvw = 'PC' into data(wa_parceiro_pc) binary search.
  if sy-subrc is not initial.
    concatenate 'Erro ao gerar Aviso de Recebimento->' 'Ponto de Coleta deve ser Informado!' into c_message separated by space.
    c_erro = abap_true.
    exit.
  endif.

  read table tg_0015 with key seq_lcto = tg_0008-seq_lcto parvw = 'LR' into data(wa_parceiro_lr) binary search.
  if sy-subrc is not initial.
    concatenate 'Erro ao gerar Aviso de Recebimento->' 'Local de Entrega deve ser Informado!' into c_message separated by space.
    c_erro = abap_true.
    exit.
  endif.

  create object lc_aviso.

  data: i_lifnr	type lifnr.

  i_lifnr = wa_zfiwrt0008-branch.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = i_lifnr
    importing
      output = i_lifnr.

  lc_aviso->set_fornecedor( i_lifnr = i_lifnr ).
  lc_aviso->set_pedido_compra( i_ebeln = wa_zfiwrt0008-ebeln ).
  lc_aviso->set_data_lancamento( i_bldat = sy-datum ).

  data: lv_netwr type netwr.

  lv_netwr = 0.

  loop at tg_0009 into data(wa_0009) where seq_lcto eq wa_zfiwrt0008-seq_lcto.

    clear: vg_matnr.
    wa_0009-matnr = |{ wa_0009-matnr alpha = out }|.
    vg_matnr = wa_0009-matnr.
    vg_matnr = |{ vg_matnr alpha = in }|.
    wa_0009-matnr = vg_matnr.

    i_item-ebeln        = wa_zfiwrt0008-ebeln.
    i_item-ebelp        = '00010'.
    i_item-vgtyp        = 'V'. "pedido
    i_item-quantidade   = wa_0009-menge.
    i_item-unidade      = wa_0009-meins.
    i_item-material     = wa_0009-matnr.
    i_item-traty        = '0001'.
    i_item-tragr        = '0001'.
    i_item-ladgr        = '0003'.
    i_item-mfrgr        = '00000001'.
    i_item-kzbew        = 'B'.
    i_item-plant        = wa_0009-bwkey.
    i_item-stge_loc     = wa_0009-lgort.
    i_item-move_type    = '101'.
    i_item-batch        = wa_0009-charg.
    i_item-licha        = wa_0009-charg.
    lc_aviso->set_item( i_item = i_item ).
    add wa_0009-netwr to lv_netwr.
  endloop.

  lc_aviso->set_sp_frete_parid( i_parid = wa_parceiro_sp-parid ).
  lc_aviso->set_sp_frete_partyp( i_partyp = 'V' ).

  lc_aviso->set_lc_coleta_parid( i_parid = wa_parceiro_pc-parid ).
  lc_aviso->set_lc_coleta_partyp( i_partyp = 'V' ).

  lc_aviso->set_lc_entrega_parid( i_parid = wa_parceiro_lr-parid ).
  lc_aviso->set_lc_entrega_partyp( i_partyp = 'C' ).


  lc_aviso->set_vfdat( i_vfdat = sy-datum ).
  lc_aviso->set_ship_point( i_ship_point = wa_zfiwrt0008-branch ).
  lc_aviso->set_valor_nota( i_valor_nota = lv_netwr ).

  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = wa_zfiwrt0008-nfenum
    importing
      output = i_xblnr.

  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = wa_zfiwrt0008-series
    importing
      output = l_serie.

  concatenate i_xblnr '-' l_serie into i_xblnr.
  condense i_xblnr no-gaps.
  lc_aviso->set_xblnr( i_xblnr = i_xblnr ).

  lc_aviso->set_ck_route_validar( i_ck_route_validar = abap_true ).

  try.
      data(r_gerou) = lc_aviso->criar_aviso_recebimento( i_particao_lote = abap_false ).
    catch zcx_delivery into data(ex_delivery).
      "ex_delivery->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
      message id ex_delivery->msgid type 'S' number ex_delivery->msgno with ex_delivery->msgv1
                                                                            ex_delivery->msgv2
                                                                            ex_delivery->msgv3
                                                                            ex_delivery->msgv4 into c_message.
      c_erro = abap_true.
      exit.
  endtry.

  if r_gerou eq abap_true.
    r_remessa = lc_aviso->get_nr_remessa( ).
  else.

    data: wl_cont       type sy-tabix,
          wa_zfiwrt1000 type zfiwrt1000.
    clear: wl_cont.
    data(r_retorno) = lc_aviso->get_retorno( ).

    loop at r_retorno into data(wa_retorno).
      clear: wa_zfiwrt1000.
      add 1 to wl_cont.
      move: sy-mandt               to wa_zfiwrt1000-mandt,
            wa_zfiwrt0008-seq_lcto to wa_zfiwrt1000-seq_lcto,
            'VBELN_R'              to wa_zfiwrt1000-field,
            wl_cont                to wa_zfiwrt1000-buzei,
            wa_retorno-type        to wa_zfiwrt1000-type,
            wa_retorno-message     to wa_zfiwrt1000-mensagem,
            sy-datum               to wa_zfiwrt1000-erdat,
            sy-uzeit               to wa_zfiwrt1000-ertim.
      append wa_zfiwrt1000 to it_1000.
      clear: wa_zfiwrt1000.
    endloop.

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  CRIA_CONTAB_TRAN_DEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

**********************************************************************
* 112114 CS2023000132 Contabilizar as transferências de ativo imobilizado - PSA
**********************************************************************

form cria_contab_trans_dest.

  data: xv_050 type zfiwrt0009-netpr.
  data: vgsber type zib_contabil-gsber.

  loop at tg_0009 where seq_lcto = tg_0008-seq_lcto.
    clear: xv_050.
    xv_050 = tg_0009-netpr * tg_0009-menge.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = tg_0008-parid
      importing
        output = vgsber.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = vgsber
      importing
        output = vgsber.
    clear: tl_zib.
    concatenate  tg_0008-obj_key 'T' into  tl_zib-obj_key.
    tl_zib-seqitem       = '001'.
    tl_zib-bschl         = '70'.
    tl_zib-gsber         = vgsber.
    tl_zib-bukrs         = tg_0008-bukrs.
    tl_zib-interface     = '35'.
    tl_zib-rg_atualizado = 'N'.
    concatenate 'TRANS.IMOB-' tg_0008-seq_lcto into  tl_zib-bktxt.
    concatenate tg_0008-bldat+6(2) tg_0008-bldat+4(2) tg_0008-bldat(4) into tl_zib-bldat separated by '.'.
    concatenate tg_0008-budat+6(2) tg_0008-budat+4(2) tg_0008-budat(4) into tl_zib-budat separated by '.'.
    tl_zib-gjahr         = tg_0008-budat+0(4).
    tl_zib-monat         = tg_0008-budat+4(2).
    tl_zib-blart         = 'AB'.
    tl_zib-hkont         = '132006'.
    tl_zib-wrbtr         = xv_050.
    tl_zib-dmbtr         = xv_050.
    tl_zib-waers         = 'BRL'.
    tl_zib-waers_i       = 'BRL'.
    tl_zib-anln1         = tg_0009-anln1.
    tl_zib-anln2         = tg_0009-anln2.
    tl_zib-bewar         = '120'.
    tl_zib-kostl         = vr_kostl. "Centro de custo selecionado no inicio do FORM f_bapi_asset.
    append tl_zib.

    clear: tl_zib.
    concatenate  tg_0008-obj_key 'T' into  tl_zib-obj_key.
    tl_zib-seqitem       = '002'.
    tl_zib-bschl         = '75'.
    tl_zib-gsber         = tg_0008-move_plant.
    tl_zib-bukrs         = tg_0008-bukrs.
    tl_zib-interface     = '35'.
    tl_zib-rg_atualizado = 'N'.
    concatenate 'TRANS.IMOB-' tg_0008-seq_lcto into  tl_zib-bktxt.
    concatenate tg_0008-bldat+6(2) tg_0008-bldat+4(2) tg_0008-bldat(4) into tl_zib-bldat separated by '.'.
    concatenate tg_0008-budat+6(2) tg_0008-budat+4(2) tg_0008-budat(4) into tl_zib-budat separated by '.'.
    tl_zib-gjahr         = tg_0008-budat+0(4).
    tl_zib-monat         = tg_0008-budat+4(2).
    tl_zib-blart         = 'AB'.
    tl_zib-hkont         = '132006'.
    tl_zib-wrbtr         = xv_050.
    tl_zib-dmbtr         = xv_050.
    tl_zib-waers         = 'BRL'.
    tl_zib-waers_i       = 'BRL'.
    tl_zib-anln1         = tg_0009-anln1.
    tl_zib-anln2         = tg_0009-anln2.
    tl_zib-bewar         = '140'.
    tl_zib-kostl         = tg_0008-kostl.
    append tl_zib.

  endloop.

  modify zib_contabil from table tl_zib.
  commit work .


endform.
