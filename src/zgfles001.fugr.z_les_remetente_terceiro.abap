function z_les_remetente_terceiro.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(CNPJ) TYPE  ZLEST0035-CNPJ
*"  TABLES
*"      TI_NFS STRUCTURE  ZLESE0001
*"  EXCEPTIONS
*"      REMETENTE_PROPRIO
*"      NFS_NOT_FOUND
*"----------------------------------------------------------------------
data: begin of tl_0041 occurs 0.
       include type zlest0041.
data:  lifnr type lfa1-lifnr,
      end of tl_0041.


  data: wl_1bbranch type j_1bbranch,
        wl_lfa1     type lfa1,
*        tl_0041     type table of zlest0041 with header line,
        tl_0035     type table of zlest0035 with header line,
        tl_lfa1     type table of lfa1 with header line.

  refresh: tl_0041, tl_0035, tl_lfa1.
  clear: wl_1bbranch, wl_lfa1.

  select single *
    from j_1bbranch
    into wl_1bbranch
     where stcd1 eq cnpj.

  if sy-subrc is not initial.
    select single *
      from lfa1
      into wl_lfa1
       where stcd1 eq cnpj.

    if sy-subrc is not initial.
      raise lifnr_not_found.
    endif.

    if ti_nfs[] is not initial.
      select *
        from zlest0041
        into table tl_0041
         for all entries in ti_nfs
          where nr_nf       eq ti_nfs-nr_nf_i
            and serie       eq ti_nfs-serie_nf_i
            and cod_cliente eq wl_lfa1-lifnr.


      if sy-subrc is initial.
        loop at tl_0041.
*          shift tl_0041-serie left deleting leading '0'.
         call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = tl_0041-centro_comprador
            importing
              output = tl_0041-lifnr.

         call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = tl_0041-serie
            importing
              output = tl_0041-serie.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = tl_0041-serie_propria
            importing
              output = tl_0041-serie_propria.

          modify tl_0041.
        endloop.

        select *
          from lfa1
          into table tl_lfa1
           for all entries in tl_0041
            where lifnr eq tl_0041-lifnr.

        select *
          from zlest0035
          into table tl_0035
           for all entries in tl_0041
            where nr_nf    eq tl_0041-nr_nf_propria
              and serie_nf eq tl_0041-serie_propria
              and docnum   eq tl_0041-docnum
              and werks    eq tl_0041-centro_comprador.

      else.
        raise nfs_not_found.
      endif.

      sort: tl_0041 by nr_nf serie cod_cliente,
            tl_0035 by nr_nf serie_nf  docnum werks,
            tl_lfa1 by lifnr.
      loop at ti_nfs.
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = ti_nfs-serie_nf_i
            importing
              output = ti_nfs-serie_nf_i.

        read table tl_0041
          with key nr_nf = ti_nfs-nr_nf_i
                   serie = ti_nfs-serie_nf_i
                   cod_cliente = wl_lfa1-lifnr
                         binary search.

        if sy-subrc is initial.
          read table tl_lfa1
            with key lifnr = tl_0041-lifnr
                     binary search.

          read table tl_0035
            with key nr_nf    = tl_0041-nr_nf_propria
                     serie_nf = tl_0041-serie_propria
                     docnum   = tl_0041-docnum
                     werks    = tl_0041-centro_comprador
                        binary search.

          if sy-subrc is initial.

            move: tl_0035-nr_nf    to ti_nfs-nr_nf_e,
                  tl_0035-serie_nf to ti_nfs-serie_nf_e,
                  tl_0035-docnum   to ti_nfs-docnum_e,
                  tl_lfa1-stcd1    to ti_nfs-cnpj_e.

            modify ti_nfs.

          endif.
        endif.
      endloop.
    endif.

  else.
    raise remetente_proprio.
  endif.




endfunction.
