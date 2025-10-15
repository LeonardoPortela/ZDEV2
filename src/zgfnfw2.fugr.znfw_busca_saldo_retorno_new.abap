function znfw_busca_saldo_retorno_new.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  ZFIWRT0008-BUKRS
*"     REFERENCE(I_BRANCH) TYPE  ZFIWRT0008-BRANCH
*"     REFERENCE(I_PARVW) TYPE  ZFIWRT0008-PARVW
*"     REFERENCE(I_PARID) TYPE  ZFIWRT0008-PARID
*"     REFERENCE(I_OPERACAO) TYPE  ZFIWRT0008-OPERACAO OPTIONAL
*"     REFERENCE(I_SEQ_LCTO) TYPE  ZFIWRT0008-SEQ_LCTO OPTIONAL
*"     REFERENCE(I_SEQ_MODIFY) TYPE  ZFIWRT0008-SEQ_LCTO OPTIONAL
*"     REFERENCE(I_SHIPFROM) TYPE  LFA1-REGIO OPTIONAL
*"     REFERENCE(I_SHIPTO) TYPE  LFA1-REGIO OPTIONAL
*"     REFERENCE(I_IMOBILIZADO) TYPE  ZFIWRT0008-IMOBILIZADO OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_HEADER) TYPE  ZFIWRT0008
*"  TABLES
*"      ET_ITENS STRUCTURE  ZFIWRT0009
*"  EXCEPTIONS
*"      SEM_SALDO
*"----------------------------------------------------------------------

  types: begin of ty_0008.
          include structure zfiwrt0008.
  types: docref_ret type zfiwrt0008-docref,
         end of ty_0008.
  types: begin of ty_itens_aux,
          doc_ref type zfiwrt0008-seq_lcto,
          itmnum  type zfiwrt0009-itmnum,
          matnr   type zfiwrt0009-matnr,
          menge   type zfiwrt0009-menge,
         end of ty_itens_aux.

  ranges: rg_seq_lcto    for zfiwrt0008-seq_lcto,
          rg_imobilizado for zfiwrt0008-imobilizado.

  data: tl_0008 type table of ty_0008 with header line,
        tl_0009 type table of zfiwrt0009 with header line,
        tl_0008_ret type table of zfiwrt0008 with header line,
        tl_0009_ret type table of zfiwrt0009 with header line,
        tl_itens_aux type table of ty_itens_aux with header line,
        tl_docest type table of zfiwrs0003 with header line,
        wl_bnfdoc type j_1bnfdoc,
        wl_0006 type zfiwrt0006,
        wl_index type sy-tabix,
        wl_werks_aux type werks_d,
        wl_parid_aux type j_1bparid,
        wl_field_aux(20).

  refresh: rg_seq_lcto,
           rg_imobilizado.

  if i_seq_lcto is not initial
  and i_seq_lcto ne '0000000000'.
    rg_seq_lcto-sign    = 'I'.
    rg_seq_lcto-option  = 'EQ'.
    rg_seq_lcto-low     = i_seq_lcto.

    append rg_seq_lcto.
    clear: rg_seq_lcto.
  endif.

  if i_imobilizado is not initial.
*  and i_seq_lcto ne '0000000000'.
    rg_imobilizado-sign    = 'I'.
    rg_imobilizado-option  = 'EQ'.
    rg_imobilizado-low     = i_imobilizado.

    append rg_imobilizado.
    clear: rg_imobilizado.
  endif.

** Busa documentos criados com o mesma empresa, local de negocios e cliente.
  if i_parvw eq 'BR'.
    clear: wl_field_aux.
    wl_field_aux = i_parid.
    shift wl_field_aux left deleting leading '0'.
    wl_werks_aux = wl_field_aux.
    shift wl_werks_aux left deleting leading '0'.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wl_werks_aux
      importing
        output = wl_werks_aux.

    clear: wl_field_aux.
    wl_field_aux = i_branch.
    shift wl_field_aux left deleting leading '0'.
    wl_parid_aux = wl_field_aux.
    shift wl_parid_aux left deleting leading '0'.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wl_parid_aux
      importing
        output = wl_parid_aux.

    select *
        from zfiwrt0008
        into table tl_0008
         where bukrs    eq i_bukrs
           and branch   eq wl_werks_aux
           and parvw    eq i_parvw
           and parid    eq wl_parid_aux
           and loekz    eq space
           and docs_estornados eq space
           and seq_lcto in rg_seq_lcto
           and imobilizado in rg_imobilizado
           and  ( docref   eq space
            or docref   eq '0000000000' ).
  else.
    select *
      from zfiwrt0008
      into table tl_0008
       where bukrs    eq i_bukrs
         and branch   eq i_branch
         and parvw    eq i_parvw
         and parid    eq i_parid
         and loekz    eq space
         and docs_estornados eq space
         and imobilizado in rg_imobilizado
         and seq_lcto in rg_seq_lcto
         and  ( docref   eq space
          or docref   eq '0000000000' ).

  endif.
  if sy-subrc is initial.
    loop at tl_0008.
      tl_0008-docref_ret = tl_0008-seq_lcto.
      modify tl_0008.
    endloop.

*** Busca itens dos documentos do cliente.
    select *
      from zfiwrt0009
      into table tl_0009
       for all entries in tl_0008
       where seq_lcto eq tl_0008-seq_lcto.

    if sy-subrc is initial.
      refresh: tl_docest.
      loop at tl_0008.
        move: tl_0008-seq_lcto to tl_docest-seq_lcto.

        append tl_docest.
        clear: tl_docest.
      endloop.
*** Busca documentos de estorno
      call function 'ZNFW_ESTORNA_SEQ_LCTO'
        tables
          t_docs = tl_docest.

      sort: tl_docest by seq_lcto.
      loop at tl_docest.
         if tl_docest-docnum_est is not initial
         or tl_docest-belnr_est is not initial
         or tl_docest-mblnr_est is not initial
         or tl_docest-docnum is initial.
           delete tl_0009 where seq_lcto eq tl_docest-seq_lcto.
         endif.

      endloop.
    endif.
*** busca documentos  criados com referencia aos documentos encontrados do cliente
    select *
      from zfiwrt0008
      into table tl_0008_ret
      for all entries in tl_0008
       where docref eq tl_0008-docref_ret
         and loekz  eq space
         and docs_estornados eq space.

    if sy-subrc is initial.
      loop at tl_0008_ret.
        move: tl_0008_ret-seq_lcto to tl_docest-seq_lcto.

        append tl_docest.
        clear: tl_docest.
      endloop.
*** Busca documentos de estorno
      call function 'ZNFW_ESTORNA_SEQ_LCTO'
        tables
          t_docs = tl_docest.

      sort: tl_docest by seq_lcto.
*---> 05/07/2023 - Migração S4 - DL
SORT tl_0008_ret BY seq_lcto.
*<--- 05/07/2023 - Migração S4 - DL
      loop at tl_docest.
        read table tl_0008_ret
          with key seq_lcto = tl_docest-seq_lcto
                      binary search.
        if sy-subrc is initial.
          if ( tl_docest-docnum is not initial
          and tl_docest-docnum_est is not initial ).
            delete tl_0008_ret index sy-tabix.
            continue.
          endif.

          if ( tl_docest-belnr is not initial
          and tl_docest-belnr_est is not initial ).
            delete tl_0008_ret index sy-tabix.
            continue.
          endif.

          if ( tl_docest-mblnr is not initial
          and tl_docest-mblnr_est is not initial ).
            delete tl_0008_ret index sy-tabix.
            continue.
          endif.
        endif.
      endloop.

      if tl_0008_ret[] is not initial.
****  Busa itens dos documentos criados com referencia aos docuemntos dos clientes.
        select *
          from zfiwrt0009
          into table tl_0009_ret
           for all entries in tl_0008_ret
           where seq_lcto eq tl_0008_ret-seq_lcto.

      endif.
    endif.
  endif.

  if i_seq_modify is not initial.
    delete tl_0009_ret where seq_lcto eq i_seq_modify.

  endif.
  sort: tl_0008_ret by seq_lcto.

  loop at tl_0009_ret.
    read table tl_0008_ret
      with key seq_lcto = tl_0009_ret-seq_lcto
                  binary search.

    move: tl_0008_ret-docref to tl_itens_aux-doc_ref,
          tl_0009_ret-itmnum to tl_itens_aux-itmnum,
          tl_0009_ret-matnr  to tl_itens_aux-matnr,
          tl_0009_ret-menge  to tl_itens_aux-menge.

    collect tl_itens_aux.
    clear: tl_0008_ret.
  endloop.

  if i_shipto eq i_shipfrom
  and i_shipto is not initial
  and i_shipfrom is not initial.
    select single *
      from zfiwrt0006
      into wl_0006
       where operacao eq i_operacao
         and indcoper eq 'D'.

  elseif i_shipto ne i_shipfrom
  and i_shipto is not initial
  and i_shipfrom is not initial.
    select single *
      from zfiwrt0006
      into wl_0006
       where operacao eq i_operacao
         and indcoper eq 'F'.
  endif.


  sort: tl_itens_aux by doc_ref itmnum.

  loop at tl_0009.
    read table tl_itens_aux
      with key doc_ref = tl_0009-seq_lcto
               itmnum  = tl_0009-itmnum
                 binary search.

    if sy-subrc is initial.
      subtract tl_itens_aux-menge from tl_0009-menge.
      tl_0009-netwr = tl_0009-menge * tl_0009-netpr.
    endif.

    if i_parvw eq 'BR'.
      tl_0009-bwkey = i_branch.
    endif.
    tl_0009-cfop = wl_0006-cfop.
    append tl_0009 to et_itens.
  endloop.

  if i_seq_lcto is not initial.
    read table tl_0008 index 1.
    move-corresponding: tl_0008  to  e_header.
    select single *
      from j_1bnfdoc
      into wl_bnfdoc
       where docnum eq e_header-docnum.

    if wl_bnfdoc-nfenum is not initial.
      move: wl_bnfdoc-nfenum to e_header-nfenum,
            wl_bnfdoc-series to e_header-series.
    else.
      move: wl_bnfdoc-nfnum  to e_header-nfenum,
            wl_bnfdoc-series to e_header-series.
    endif.
  endif.
  delete et_itens where menge eq 0.

  if et_itens[] is initial.
    raise sem_saldo.

  endif.

endfunction.
