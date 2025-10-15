*&---------------------------------------------------------------------*
*& Report  ZFIR077
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report zfir077.
types: begin of ty_lotes,
         lote     type zfit0156-lote,
         vlr_lote type zfit0156-vlr_lote,
         vlr_resi type zfit0156-vlr_resi,
       end of   ty_lotes.

data: it_split        type table of ty_lotes,
      wa_split        type          ty_lotes,
      it_agrupa       type table of zfit0156,
      it_bsik         type table of zfit0156,
      wa_bsik         type zfit0156,
      it_lote         type table of zfit0156,
      vvalor_tot      type bseg-dmbtr,
      vvalor_dif      type bseg-dmbtr,
      w_bkpf          type bkpf,
      lc_ukurs        type ukurs_curr,
      it_zob_mensagem type table of zob_mensagem,
      wa_zob_mensagem type zob_mensagem.

data: wa_ret_document type zfie_ret_document,
      it_ret_document like standard table of wa_ret_document.

data: vg_job      type i.

data: xachou(1),
      xachouc(1),
      xlimite      type i,
      v_belnr_prox type bsik-belnr,
      v_belnr_orig type bsik-belnr.

select single count( * ) into vg_job
  from tbtco
 where jobname eq 'SPLIT_LOTE_SIGAM'
   and status eq 'R'.

if ( vg_job eq 1 ).
  perform processa_dados.
endif.


*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form processa_dados .

  data: vl_error    type c,
        vl_doc_comp type bsad-belnr,
        vl_msg      type string.

  select *
      from zfit0156
      into table @data(it_zfit0156)
      where rg_atualizado = '0'.

  sort it_zfit0156 by bukrs belnr buzei lifnr.
  loop at it_zfit0156 into  data(w_156).
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = w_156-lifnr
      importing
        output = w_156-lifnr.
    modify it_zfit0156 from w_156 index sy-tabix transporting lifnr.
  endloop.
  it_agrupa[] = it_zfit0156[].
  delete adjacent duplicates from it_agrupa comparing bukrs belnr buzei lifnr lote.

  it_lote[] = it_zfit0156[].
  sort it_lote by lote.
  delete adjacent duplicates from it_lote comparing lote.

  loop at it_agrupa into data(wa_agrupa).
    refresh: it_split,
             it_ret_document.

    loop at it_zfit0156 into data(wa_zfit0156) where bukrs = wa_agrupa-bukrs
                                               and   belnr = wa_agrupa-belnr
                                               and   buzei = wa_agrupa-buzei
                                               and   lifnr = wa_agrupa-lifnr
                                               and   lote  = wa_agrupa-lote.
      wa_split-lote = wa_zfit0156-lote.
      wa_split-vlr_lote = wa_zfit0156-vlr_lote.
      wa_split-vlr_resi = wa_zfit0156-vlr_resi.
      append wa_split to it_split.
    endloop.

    perform f_bapi_f51 using     wa_agrupa
                       changing  vl_error.

    if not it_ret_document is initial.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*      call function 'Z_FI_OUTBOUND_RETURN' in background task
*        destination 'XI_SIGAM_RETURN'
*        as separate unit
*        tables
*          OUTRETURN = IT_RET_DOCUMENT.

      data: lv_rfc type rfcdest.

      constants: c_fm type rs38l_fnam value 'Z_FI_OUTBOUND_RETURN'.

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
          as separate unit
          tables
            outreturn = it_ret_document.
      else.
        call function c_fm in background task
          tables
            outreturn = it_ret_document.
      endif.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

    endif.
    commit work.

    refresh it_zob_mensagem.
    loop at it_ret_document into wa_ret_document.
      move-corresponding wa_ret_document to wa_zob_mensagem.

      call function 'NUMBER_GET_NEXT'
        exporting
          nr_range_nr             = '01'
          object                  = 'ZOB_MENSG'
        importing
          number                  = wa_zob_mensagem-seq_registro
        exceptions
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          others                  = 8.

      call function 'OIL_DATE_TO_TIMESTAMP'
        exporting
          i_date   = sy-datum
          i_time   = sy-uzeit
        importing
          e_tstamp = wa_zob_mensagem-timestamp.

      append wa_zob_mensagem to it_zob_mensagem.
    endloop.

    if it_zob_mensagem[] is not initial.
      modify zob_mensagem from table it_zob_mensagem.
      commit work.
    endif.

  endloop.
  "

endform.

form f_bapi_f51 using wa_agrupa type zfit0156
                changing p_erro.


  data: l_auglv   type t041a-auglv   value 'UMBUCHNG', "Posting with Clearing
        l_tcode   type sy-tcode      value 'FB05',     "You get an error with any other value
        l_sgfunct type rfipi-sgfunct value 'C'.        "Post immediately

  data: lt_blntab     type standard table of blntab  with header line,
        lt_ftclear    type standard table of ftclear with header line,
        lt_ftpost     type standard table of ftpost  with header line,
        lt_fttax      type standard table of fttax   with header line,
        lds_return    type bapiret2,
        fg_alterou(1).


  data: vdata(10),
        vdata_venc(10),
        cnum_seq(2),
        wl_vlr(16),
        wl_taxa(16),
        wl_vlrc(16),
        wl_vlrn        type p decimals 2,
        vcampo(15),
        v_kur          type bkpf-kursf,
        vvalor_bax     type bseg-dmbtr,
        vvalor_resi    type bseg-dmbtr,
        vvalor_lote1   type bseg-dmbtr,
        vvalor_lote2   type bseg-dmbtr,
        msg_no         type t100-msgnr,
        msg_text       type string,
        p_mode         like rfpdo-allgazmd,
        vl_dt_mov      type sy-datum,
        count_ft       type ftpost-count,
        w_bsak         type bsak,
        t_bsak         type table of bsak,
        v_xsimu        type char1.

  clear: v_belnr_prox ,v_belnr_orig, fg_alterou.
  refresh: it_bsik.
  "
  select single *
      from bkpf as c
      inner join bsik as d
      on  d~bukrs = c~bukrs
      and d~belnr = c~belnr
      and d~lifnr =  wa_agrupa-lifnr
      into corresponding fields of w_bkpf
      where c~bukrs = wa_agrupa-bukrs
      and   c~belnr = wa_agrupa-belnr.

  if sy-subrc ne 0.
    "Se foi compensado  checa se o docuemnto compensado tem a partida que se quer atribuir
    clear xachou.
    xlimite = 0.
    v_belnr_orig = wa_agrupa-belnr.
    v_belnr_prox = wa_agrupa-belnr.
    while xachou = ''.
      add 1 to xlimite.
      if xlimite gt 10.
        xachou = 'X'.
      endif.

      refresh t_bsak.
      select *
        from bsak
        into table t_bsak
       where bukrs = wa_agrupa-bukrs
       and   belnr = v_belnr_prox
       and   lifnr = wa_agrupa-lifnr
       and   augbl <> v_belnr_prox.

      if t_bsak[] is initial.
        xachou = 'X'.
      else.
        loop at t_bsak into w_bsak.

          v_belnr_prox = w_bsak-augbl.
          vvalor_lote1 = wa_agrupa-vlr_lote - ( 10 / 100 ).
          vvalor_lote2 = wa_agrupa-vlr_lote + ( 10 / 100 ).
          "
          select single *
            from bsik
            into @data(w_bsik_comp)
          where bukrs = @w_bsak-bukrs
          and   belnr = @w_bsak-augbl
          and   gjahr = @w_bsak-augdt+0(4)
          and   wrbtr between @vvalor_lote1 and @vvalor_lote2.

          if sy-subrc = 0.
* ---> S4 Migration - 16/06/2023 - MA
*            select single *
*            from BSEG
*            into @data(W_BSEG_COMP)
*            where BUKRS = @W_BSIK_COMP-BUKRS
*            and   BELNR = @W_BSIK_COMP-BELNR
*            and   BUZEI = @W_BSIK_COMP-BUZEI
*            and   GJAHR = @W_BSIK_COMP-GJAHR.

            data: lt_bseg     type fagl_t_bseg,
                  w_bseg_comp type bseg.

            call function 'FAGL_GET_BSEG'
              exporting
                i_bukrs   = w_bsik_comp-bukrs
                i_belnr   = w_bsik_comp-belnr
                i_gjahr   = w_bsik_comp-gjahr
              importing
                et_bseg   = lt_bseg
              exceptions
                not_found = 1
                others    = 2.

            delete lt_bseg where buzei ne w_bsik_comp-buzei.

            read table lt_bseg into data(ls_bseg) index 1.
            if sy-subrc = 0.
              move-corresponding ls_bseg to w_bseg_comp.
            endif.
*<--- S4 Migration - 16/06/2023 - MA

            if strlen( w_bseg_comp-zuonr ) le 10.
              vvalor_bax = 9999.
              perform f_atribui using     w_bseg_comp wa_agrupa-lote vvalor_bax v_belnr_orig w_bseg_comp-xref1 w_bseg_comp-xref3.
              fg_alterou = 'X'.
              xachou = 'X'.
            elseif w_bseg_comp-zuonr = wa_agrupa-lote. "Caso reenvie um lote já atribuido
              clear wa_ret_document.
              wa_ret_document-obj_key        = wa_agrupa-lote.
              wa_ret_document-interface      = '58'.
              wa_ret_document-dt_atualizacao = sy-datum.
              wa_ret_document-hr_atualizacao = sy-uzeit.
              wa_ret_document-type           = 'S'.
              wa_ret_document-id             = 'Z01'.
              wa_ret_document-num            = '003'.
              wa_ret_document-message        = 'Documento JÁ ESTA atribuído no doc. de compensação'.
              wa_ret_document-message_v1     = wa_agrupa-belnr.
              wa_ret_document-message_v2     = w_bseg_comp-belnr.
              wa_ret_document-message_v3     = ''.
              wa_ret_document-message_v4     = ''.
              condense wa_ret_document-message_v3 no-gaps.
              append wa_ret_document to it_ret_document.
              "
              fg_alterou = 'X'.
              xachou = 'X'.
            endif.
          else.
            select *
            from bsik
            appending corresponding fields of table it_bsik
              where bukrs = w_bsak-bukrs
              and   belnr = w_bsak-augbl
              and   gjahr = w_bsak-augdt+0(4).
          endif.

        endloop.
      endif.
    endwhile.


    if fg_alterou ne 'X' and v_belnr_prox  is not initial.
      if v_belnr_prox ne wa_agrupa-belnr.
        sort it_bsik by bukrs belnr buzei.
        loop at it_bsik into wa_bsik.
          select single *
              from bsik
              into @data(w_bsik2)
              where bukrs = @wa_bsik-bukrs
              and   lifnr = @wa_bsik-lifnr
              and   belnr = @wa_bsik-belnr
              and   buzei = @wa_bsik-buzei.
          if sy-subrc = 0.
            data etl300c12r1218 type table of bseg.
            data rldnr_l300c12r8652 type rldnr.
            call function 'FAGL_GET_LEADING_LEDGER'
              importing
                e_rldnr       = rldnr_l300c12r8652
              exceptions
                not_found     = 1
                more_than_one = 2.
            if sy-subrc = 0.
              call function 'FAGL_GET_GL_DOCUMENT'
                exporting
                  i_rldnr   = rldnr_l300c12r8652
                  i_bukrs   = w_bsik2-bukrs
                  i_belnr   = w_bsik2-belnr
                  i_gjahr   = w_bsik2-gjahr
                  i_buzei   = w_bsik2-buzei
                importing
                  et_bseg   = etl300c12r1218
                exceptions
                  not_found = 1.
            endif.
            if sy-subrc = 0 and lines( etl300c12r1218 ) = 1.
              w_bseg_comp = etl300c12r1218[ 1 ].
              sy-dbcnt = 1.
            else.
              sy-subrc = 4.
              sy-dbcnt = 0.
            endif.


            if strlen( w_bseg_comp-zuonr ) le 10 and
              w_bseg_comp-wrbtr ge wa_split-vlr_lote.
              wa_split-vlr_resi = w_bseg_comp-wrbtr - wa_split-vlr_lote.
              modify it_split from wa_split index 1.
              wa_agrupa-vlr_resi = wa_split-vlr_resi.
              wa_agrupa-belnr    = w_bseg_comp-belnr.
              select single *
                from bkpf
               into w_bkpf
               where bukrs = w_bseg_comp-bukrs
               and   belnr =  w_bseg_comp-belnr
               and   gjahr =  w_bseg_comp-gjahr.
              fg_alterou = 'Y'. "desmenbra
              exit.
            endif.
          endif.
        endloop.
      endif.

    endif.

    if fg_alterou is initial.
      clear wa_ret_document.
      wa_ret_document-obj_key        = wa_agrupa-lote.
      msg_text = 'Documento não existe no SAP ou já compensado'.
      concatenate msg_text '-' wa_agrupa-belnr into msg_text .
      wa_ret_document-interface      = '58'.
      wa_ret_document-dt_atualizacao = sy-datum.
      wa_ret_document-hr_atualizacao = sy-uzeit.
      wa_ret_document-type           = 'E'.
      wa_ret_document-id             = 'Z01'.
      wa_ret_document-num            = '003'.
      wa_ret_document-message        = msg_text.
      wa_ret_document-message_v1     = wa_agrupa-belnr.
      wa_ret_document-message_v2     = ''.
      wa_ret_document-message_v3     = ''.
      wa_ret_document-message_v4     = ''.
      fg_alterou = 'X'.
      append wa_ret_document to it_ret_document.
    endif.

  endif.

  if fg_alterou eq 'Y' or fg_alterou is initial .
    clear: vvalor_resi, vvalor_bax.
    loop at it_split into wa_split.
      add wa_split-vlr_lote to vvalor_bax.
      add wa_split-vlr_resi to vvalor_resi.
    endloop.
*    vvalor_tot = vvalor_bax + vvalor_resi. MARESSA 14.10.2022
    vvalor_tot = vvalor_bax.

* ---> S4 Migration - 15/06/2023 - MA
*Não tem todos campos chave
    select *
      from bseg
      into table  @data(t_bseg)
       where bukrs = @wa_agrupa-bukrs
       and   belnr = @wa_agrupa-belnr
       and   lifnr = @wa_agrupa-lifnr. "#EC CI_DB_OPERATION_OK[2431747]
* <--- S4 Migration - 15/06/2023 - MA
    if sy-subrc ne 0.
      refresh  t_bseg.
    endif.

    clear xachou. "procura se existe alguma partida sem lote e com valot contasbil >=  ao valor enviado pelço SIGAM
    loop at t_bseg into data(w_bseg).
      select single *
        from bsik
        into @data(w_bsik)
        where bukrs = @w_bseg-bukrs
        and   lifnr = @w_bseg-lifnr
        and   belnr = @w_bseg-belnr
        and   buzei = @w_bseg-buzei.
      if sy-subrc = 0.
        if strlen( w_bseg-zuonr ) le 10.
          vvalor_dif = w_bseg-wrbtr - vvalor_tot.
          if vvalor_dif ge 0."ACHA a partida correta  24.04.2024
            xachou = 'X'.
            exit.
          endif.
        endif.
      endif.
    endloop.
    "Incluido em 26.09.2025
    CLEAR xachouc. "Achou só pra carimbar
    if xachou is initial. "se não achou procura se existe alguma partida sem lote e com valot contasbil >=  ao valor enviado pelço SIGAM em no maximo 0.10 para somente atribuir
      loop at t_bseg into w_bseg.
        select single *
          from bsik
          into w_bsik
          where bukrs = w_bseg-bukrs
          and   lifnr = w_bseg-lifnr
          and   belnr = w_bseg-belnr
          and   buzei = w_bseg-buzei.
        if sy-subrc = 0.
          if strlen( w_bseg-zuonr ) le 10.
            vvalor_dif = w_bseg-wrbtr - vvalor_tot.
            if abs( vvalor_dif ) le '0.10'.
              xachouc = 'X'.
              exit.
            endif.
          endif.
        endif.
      endloop.
    endif.
    "Incluido em 26.09.2025

    if t_bseg[] is initial or ( xachou is initial and xachouc is INITIAL ) .
      clear wa_ret_document.
      wa_ret_document-obj_key        = wa_agrupa-lote.
      msg_text = 'Documento já atribuido com lote ou não existe'.
      concatenate msg_text '-' wa_agrupa-belnr into msg_text .
      wa_ret_document-interface      = '58'.
      wa_ret_document-dt_atualizacao = sy-datum.
      wa_ret_document-hr_atualizacao = sy-uzeit.
      wa_ret_document-type           = 'E'.
      wa_ret_document-id             = 'Z01'.
      wa_ret_document-num            = '003'.
      wa_ret_document-message        = msg_text.
      wa_ret_document-message_v1     = wa_agrupa-belnr.
      wa_ret_document-message_v2     = ''.
      wa_ret_document-message_v3     = ''.
      wa_ret_document-message_v4     = ''.
      fg_alterou = 'X'.
      append wa_ret_document to it_ret_document.
    else.
      read table it_lote into data(w_lote) with key lote = wa_agrupa-lote.
      if w_lote-rg_atualizado ne 'N'.
        vvalor_bax = w_bseg-wrbtr - vvalor_bax.
        if ( vvalor_bax gt '0.10' and vvalor_bax ne 0 ) and xachouc is INITIAL..
          wa_split-lote = ' '.
          wa_split-vlr_lote = vvalor_bax.
          append wa_split to it_split.

          " Taxa do documento NOVO
          clear lc_ukurs.
          if w_bseg-dmbe2 ne 0.
            lc_ukurs = w_bseg-wrbtr / w_bseg-dmbe2.
          endif.

          p_mode = 'N'.

          call function 'POSTING_INTERFACE_START'
            exporting
              i_client           = sy-mandt
              i_function         = 'C'
              i_mode             = p_mode
              i_update           = 'S'
              i_user             = sy-uname
            exceptions
              client_incorrect   = 1
              function_invalid   = 2
              group_name_missing = 3
              mode_invalid       = 4
              update_invalid     = 5
              others             = 6.

          if sy-subrc ne 0.
            rollback work.
            message 'Houve ao efetuar a compensação' type 'S'.
            return.
          endif.

          vl_dt_mov = sy-datum.

          concatenate  vl_dt_mov+6(2) vl_dt_mov+4(2) vl_dt_mov(4) into vdata separated by '.'.


          condense wl_taxa no-gaps.

          clear: lt_blntab,   lt_blntab[],
                 lt_ftclear,  lt_ftclear[],
                 lt_ftpost,   lt_ftpost[],
                 lt_fttax,    lt_fttax[],

                 lds_return, p_erro.

          count_ft = 1.

          lt_ftpost-stype = 'K'."Header
          lt_ftpost-count = count_ft.  "number of Dynpro

          lt_ftpost-fnam = 'BKPF-BUKRS'.
          lt_ftpost-fval = w_bkpf-bukrs.
          append lt_ftpost.

          lt_ftpost-fnam = 'BKPF-WAERS'.
          lt_ftpost-fval = w_bkpf-waers.
          append lt_ftpost.

          write: w_bkpf-kursf to wl_taxa.
          condense wl_taxa no-gaps.
          lt_ftpost-fnam = 'BKPF-KURSF'.
          lt_ftpost-fval = wl_taxa.
          append lt_ftpost.


          lt_ftpost-fnam = 'BKPF-BLDAT'.
          lt_ftpost-fval =  vdata.
          append lt_ftpost.

          lt_ftpost-fnam = 'BKPF-BUDAT'.
          lt_ftpost-fval = vdata.
          append lt_ftpost.

          lt_ftpost-fnam = 'BKPF-MONAT'.
          lt_ftpost-fval =  vl_dt_mov+4(2).
          append lt_ftpost.

          lt_ftpost-fnam = 'BKPF-BLART'.
          lt_ftpost-fval = w_bkpf-blart.
          append lt_ftpost.

          lt_ftpost-fnam = 'BKPF-XBLNR'.
          lt_ftpost-fval = w_bkpf-xblnr.
          append lt_ftpost.



          loop at it_split into wa_split.
*---> 09/06/2023 - Migração S4 - JS
*            wl_vlrn = wa_split-vlr_lote.
            wl_vlrn = conv #( wa_split-vlr_lote ).
*<--- 09/06/2023 - Migração S4 - JS

            write: wl_vlrn to wl_vlrc.

            add 1 to count_ft.
            lt_ftpost-stype = 'P'.
            lt_ftpost-count = count_ft .

            lt_ftpost-fnam = 'RF05A-NEWBS'.
            lt_ftpost-fval =  w_bseg-bschl. "'31'.
            append lt_ftpost.

            if w_bseg-bschl = '29'.
              lt_ftpost-fnam = 'RF05A-NEWUM'.
              lt_ftpost-fval = w_bseg-umskz.
              append lt_ftpost.
            endif.

            lt_ftpost-fnam = 'BSEG-HKONT'.
            lt_ftpost-fval =  wa_agrupa-lifnr.
            append lt_ftpost.

            lt_ftpost-fnam = 'BSEG-SGTXT'.
            lt_ftpost-fval = w_bseg-sgtxt. "'Split fatura'.
            append lt_ftpost.

            lt_ftpost-fnam = 'BSEG-ZUONR'.
            lt_ftpost-fval = wa_split-lote.
            append lt_ftpost.

            if w_bseg-bschl = '31'.
              lt_ftpost-fnam = 'BSEG-KIDNO'.
              lt_ftpost-fval = w_bseg-kidno.
              append lt_ftpost.
            endif.

            lt_ftpost-fnam = 'BSEG-GSBER'.
            lt_ftpost-fval = w_bseg-gsber.
            append lt_ftpost.

            concatenate  w_bseg-zfbdt+6(2) w_bseg-zfbdt+4(2) w_bseg-zfbdt(4) into vdata_venc separated by '.'.
            lt_ftpost-fnam = 'BSEG-ZFBDT'.
            lt_ftpost-fval = vdata_venc.
            append lt_ftpost.

            if w_bseg-bschl ne '29'.
              lt_ftpost-fnam = 'BSEG-ZBD1T'.
              lt_ftpost-fval = w_bseg-zbd1t.
              condense lt_ftpost-fval no-gaps.
              append lt_ftpost.
            endif.

            lt_ftpost-fnam = 'BSEG-WRBTR'.
            lt_ftpost-fval =  wl_vlrc.
            append lt_ftpost.

            wl_vlrn = wl_vlrn / lc_ukurs.
            write: wl_vlrn to wl_vlrc.
            lt_ftpost-fnam = 'BSEG-DMBE2'.
            lt_ftpost-fval =  wl_vlrc.
            append lt_ftpost.

          endloop.

          "FATURA ORIGINAL
          lt_ftclear-agkoa  = w_bseg-koart.
          lt_ftclear-agkon  = w_bseg-lifnr.
          lt_ftclear-agums  = w_bseg-umskz.
          lt_ftclear-agbuk  = w_bseg-bukrs.
          lt_ftclear-xnops  = 'X'.
          lt_ftclear-selfd  = 'BELNR'.
          concatenate w_bseg-belnr w_bseg-gjahr w_bseg-buzei into lt_ftclear-selvon.
          append lt_ftclear.

          call function 'POSTING_INTERFACE_CLEARING'
            exporting
              i_auglv                    = l_auglv
              i_tcode                    = l_tcode
              i_sgfunct                  = l_sgfunct
              i_no_auth                  = 'X'
              i_xsimu                    = v_xsimu
            importing
              e_msgid                    = lds_return-id
              e_msgno                    = lds_return-number
              e_msgty                    = lds_return-type
              e_msgv1                    = lds_return-message_v1
              e_msgv2                    = lds_return-message_v2
              e_msgv3                    = lds_return-message_v3
              e_msgv4                    = lds_return-message_v4
            tables
              t_blntab                   = lt_blntab
              t_ftclear                  = lt_ftclear
              t_ftpost                   = lt_ftpost
              t_fttax                    = lt_fttax
            exceptions
              clearing_procedure_invalid = 1
              clearing_procedure_missing = 2
              table_t041a_empty          = 3
              transaction_code_invalid   = 4
              amount_format_error        = 5
              too_many_line_items        = 6
              company_code_invalid       = 7
              screen_not_found           = 8
              no_authorization           = 9
              others                     = 10.


          if lt_blntab[] is initial.
            p_erro = 'X'.
            write lds_return-number to msg_no.
            call function 'MESSAGE_PREPARE'
              exporting
                msg_id                 = lds_return-id
                msg_no                 = msg_no
                msg_var1               = lds_return-message_v1
                msg_var2               = lds_return-message_v2
                msg_var3               = lds_return-message_v3
                msg_var4               = lds_return-message_v4
              importing
                msg_text               = msg_text
              exceptions
                function_not_completed = 1
                message_not_found      = 2
                others                 = 3.
            write: / msg_text.

            clear wa_ret_document.
            wa_ret_document-obj_key        = wa_agrupa-lote.
            wa_ret_document-interface      = '58'.
            wa_ret_document-dt_atualizacao = sy-datum.
            wa_ret_document-hr_atualizacao = sy-uzeit.
            wa_ret_document-type           = lds_return-type.
            wa_ret_document-id             = lds_return-id.
            wa_ret_document-num            = msg_no.
            wa_ret_document-message        = msg_text.
            wa_ret_document-message_v1     = w_bseg-belnr.
            wa_ret_document-message_v2     = lds_return-message_v2.
            wa_ret_document-message_v3     = lds_return-message_v3.
            wa_ret_document-message_v4     = lds_return-message_v4.
            fg_alterou = 'X'.
            append wa_ret_document to it_ret_document.

          else.
            read table lt_blntab index 1.

            clear wa_ret_document.
            wa_ret_document-obj_key        = wa_agrupa-lote.
            msg_text = 'Documento criado com sucesso'.
            concatenate msg_text '-' w_bseg-belnr into msg_text .
            wa_ret_document-interface      = '58'.
            wa_ret_document-dt_atualizacao = sy-datum.
            wa_ret_document-hr_atualizacao = sy-uzeit.
            wa_ret_document-type           = 'S'.
            wa_ret_document-id             = 'Z01'.
            wa_ret_document-num            = '003'.
            wa_ret_document-message        = msg_text.
            wa_ret_document-message_v1     = w_bseg-belnr.
            wa_ret_document-message_v2     = lt_blntab-belnr.
            wa_ret_document-message_v3     = ''.
            wa_ret_document-message_v4     = ''.
            if fg_alterou = 'Y'.
              wa_ret_document-message_v1     = v_belnr_orig.
              wa_agrupa-belnr = v_belnr_orig.
            endif.
            fg_alterou = 'X'.
            append wa_ret_document to it_ret_document.

          endif.

          "fim
          call function 'POSTING_INTERFACE_END'
            exporting
              i_bdcimmed              = 'X'
            exceptions
              session_not_processable = 1
              others                  = 2.

          commit work.
          "
          if lt_blntab-belnr is not initial.
* ---> S4 Migration - 16/06/2023 - MA
*            select *
*               from BSEG
*               into table  @data(T_BSEG_ATRIBUI)
*               where BUKRS = @W_BSEG-BUKRS
*               and   BELNR = @LT_BLNTAB-BELNR
*               and   GJAHR = @SY-DATUM+0(4).

            data: "LT_BSEG        type FAGL_T_BSEG,
                  "W_BSEG_COMP    type BSEG,
                  t_bseg_atribui type table of bseg.

            data: it_bseg  type fagl_t_bseg,
                  lv_belnr type belnr_d,
                  lv_bukrs type bukrs,
                  lv_gjahr type gjahr.

            lv_bukrs = w_bseg-bukrs.
            lv_belnr = conv #( lt_blntab-belnr ).
            lv_gjahr = conv #( sy-datum+0(4) ).


            call function 'FAGL_GET_BSEG'
              exporting
                i_bukrs   = lv_bukrs
                i_belnr   = lv_belnr
                i_gjahr   = lv_gjahr
              importing
                et_bseg   = lt_bseg
              exceptions
                not_found = 1
                others    = 2.

            if sy-subrc = 0 and lines( lt_bseg ) > 0.
              move-corresponding lt_bseg to t_bseg_atribui.
              sy-dbcnt = lines( lt_bseg ).
            else.
              sy-subrc = 4.
              sy-dbcnt = 0.
            endif.
*<--- S4 Migration - 16/06/2023 - MA




            vvalor_bax = 8888.
            loop at t_bseg_atribui into data(w_bseg_atribui).
              perform f_atribui using w_bseg_atribui wa_agrupa-lote vvalor_bax wa_agrupa-belnr w_bseg-xref1 w_bseg-xref3.
            endloop.
          endif.
        else.
          perform f_atribui using     w_bseg wa_agrupa-lote vvalor_bax wa_agrupa-belnr w_bseg-xref1 w_bseg-xref3.
          fg_alterou = 'X'.
        endif.

      else.
        clear wa_ret_document.
        wa_ret_document-obj_key        = wa_agrupa-lote.
        msg_text = 'Existe documento já compensado no lote ou doc inexistente'.
        concatenate msg_text '-' wa_agrupa-belnr into msg_text .
        wa_ret_document-interface      = '58'.
        wa_ret_document-dt_atualizacao = sy-datum.
        wa_ret_document-hr_atualizacao = sy-uzeit.
        wa_ret_document-type           = 'E'.
        wa_ret_document-id             = 'Z01'.
        wa_ret_document-num            = '003'.
        wa_ret_document-message        = msg_text.
        wa_ret_document-message_v1     = wa_agrupa-belnr.
        wa_ret_document-message_v2     = ''.
        wa_ret_document-message_v3     = ''.
        wa_ret_document-message_v4     = ''.
        fg_alterou = 'X'.
        append wa_ret_document to it_ret_document.
      endif.
    endif.
  endif.
  "
  if fg_alterou = 'X'.
    update zfit0156 set rg_atualizado = '1'
      where bukrs = wa_agrupa-bukrs
      and   belnr = wa_agrupa-belnr
      and   buzei = wa_agrupa-buzei
      and   lote  = wa_agrupa-lote.
    commit work.
  endif.

endform.

form f_atribui using p_bseg  type bseg
                     p_lote  type zfit0156-lote
                     p_valor type bseg-dmbtr
                     p_belnr type bseg-belnr
                     p_xref1 type bseg-xref1
                     p_xref3 type bseg-xref3.

  data: lt_bkdf  type table of bkdf,
        lt_bkpf  type table of bkpf,
        wa_bkpf  type bkpf,
        lt_bsec  type table of bsec,
        wa_bseg  type bseg,
        lt_bsed  type table of bsed,
        lt_bseg  type table of bseg,
        lt_bset  type table of bset,
        msg_text type string.

  clear wa_bkpf.
  refresh lt_bkpf.
  select single *
    from bkpf
    into wa_bkpf
  where bukrs = p_bseg-bukrs
  and   belnr = p_bseg-belnr
  and   gjahr = p_bseg-gjahr.

  append wa_bkpf to lt_bkpf.
  clear wa_bseg.
  refresh  lt_bseg.
  move-corresponding p_bseg to wa_bseg.
  if p_valor = 8888.
    wa_bseg-xref1 = p_xref1.
    wa_bseg-xref3 = p_xref3.
  else.
    wa_bseg-zuonr = p_lote.
  endif.
  append wa_bseg to lt_bseg.

  call function 'CHANGE_DOCUMENT'
    tables
      t_bkdf = lt_bkdf
      t_bkpf = lt_bkpf
      t_bsec = lt_bsec
      t_bsed = lt_bsed
      t_bseg = lt_bseg
      t_bset = lt_bset.

  if sy-subrc = 0.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

    if p_valor = 8888.
      exit.
    endif.

    clear wa_ret_document.
    wa_ret_document-obj_key        = p_lote.
    if p_valor = 9999.
      msg_text = 'Documento atribuído no doc. de compensação'.
    elseif p_valor = 0.
      msg_text = 'Documento atribuído sem desmembrar'.
    elseif abs( p_valor ) le '0.10'.
      msg_text = 'Documento atribuído com diferença na tolerância'.
    else.
      msg_text = 'Documento atribuido com diferença maior tolerância'.
    endif.

    wa_ret_document-interface      = '58'.
    wa_ret_document-dt_atualizacao = sy-datum.
    wa_ret_document-hr_atualizacao = sy-uzeit.
    wa_ret_document-type           = 'S'.
    wa_ret_document-id             = 'Z01'.
    wa_ret_document-num            = '003'.
    wa_ret_document-message        = msg_text.
    wa_ret_document-message_v1     = p_bseg-belnr.
    wa_ret_document-message_v2     = 'DIFER'.
    wa_ret_document-message_v3     = p_valor.
    wa_ret_document-message_v4     = ''.
    if p_valor = 0.
      clear wa_ret_document-message_v2.
    elseif p_valor = 9999.
      wa_ret_document-message_v1 = p_belnr.
      wa_ret_document-message_v2 = p_bseg-belnr.
    endif.
    condense wa_ret_document-message_v3 no-gaps.
    append wa_ret_document to it_ret_document.
  else.
    clear wa_ret_document.
    wa_ret_document-obj_key        = p_lote.
    msg_text = 'Documento não atribuido com diferença'.

    wa_ret_document-interface      = '58'.
    wa_ret_document-dt_atualizacao = sy-datum.
    wa_ret_document-hr_atualizacao = sy-uzeit.
    wa_ret_document-type           = 'E'.
    wa_ret_document-id             = 'Z01'.
    wa_ret_document-num            = '003'.
    wa_ret_document-message        = msg_text.
    wa_ret_document-message_v1     = p_bseg-belnr.
    wa_ret_document-message_v2     = 'DIFER'.
    wa_ret_document-message_v3     = p_valor.
    wa_ret_document-message_v4     = ''.
    condense wa_ret_document-message_v3 no-gaps.
    append wa_ret_document to it_ret_document.
  endif.

endform.
