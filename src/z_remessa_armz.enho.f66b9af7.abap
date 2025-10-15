"Name: \PR:SAPLJ1BF\FO:FILL_ADDITIONAL_FIELDS\SE:BEGIN\EI
ENHANCEMENT 0 Z_REMESSA_ARMZ.
*
  field-symbols: <fs_bktxt>    type any.
  data: vrate        type j_1bnfstx-rate,
        vbase        type j_1bnfstx-base,
        vebeln       type ekko-ebeln,
        v_forn_orig  type lfa1-lifnr,
        v_forn_dest  type lfa1-lifnr,
        v_regio_orig type lfa1-regio,
        v_regio_dest type lfa1-regio.

  if wa_nf_doc-direct = '2'.
    assign ('(SAPMM07M)MKPF-BKTXT') to <fs_bktxt>.
    if <fs_bktxt> is assigned.
      if strlen( <fs_bktxt> ) = 10.
        vebeln = <fs_bktxt>.
        if vebeln  co '1234567890.'.
          select single *
          into @data(w_ekko)
          from ekko
          where ebeln = @vebeln.

          if 'ZARM_ZARS' cs w_ekko-bsart.
            call function 'CONVERSION_EXIT_ALPHA_INPUT'
              exporting
                input  = wa_nf_doc-branch
              importing
                output = v_forn_orig.

            select single regio from lfa1 into v_regio_orig where lifnr = v_forn_orig.

            select single regio from lfa1 into v_regio_dest where lifnr = wa_nf_doc-parid.

            if v_regio_orig ne v_regio_dest.
              vrate = 12.
              select single rate
                into vrate
                from j_1btxic1
                where land1 = 'BR'
                and   shipfrom = v_regio_orig
                and   shipto   = v_regio_dest.
              loop at wa_nf_stx assigning field-symbol(<fs_stx>).
                if <fs_stx>-taxtyp = 'ICM3' and <fs_stx>-rate is initial.
                  vbase = <fs_stx>-excbas /  ( ( 100 - vrate ) / 100 ).
                  <fs_stx>-base = vbase.
                  <fs_stx>-rate = vrate.
                  <fs_stx>-taxval = vbase * ( vrate / 100 ).
                  <fs_stx>-excbas = 0.
                else.
                  <fs_stx>-othbas = vbase.
                endif.

              endloop.
            endif.
          endif.
        endif.
      endif.
    endif.
  endif.

ENDENHANCEMENT.
