function z_verifica_miro_paga.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BELNR) TYPE  RE_BELNR
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"  CHANGING
*"     VALUE(AUGDT) TYPE  AUGDT OPTIONAL
*"     VALUE(AUGBL) TYPE  AUGBL OPTIONAL
*"----------------------------------------------------------------------

  data: wa_bkpf  type bkpf,
        vg_awkey type awkey.

  clear: augdt,
         augbl.

  concatenate belnr gjahr into vg_awkey.

  select single * into wa_bkpf
    from bkpf
   where awtyp eq 'RMRP'
     and awkey eq vg_awkey.

  if sy-subrc is initial.
    select single augdt augbl into (augdt, augbl)
      from bsak
     where bukrs eq wa_bkpf-bukrs
       and gjahr eq wa_bkpf-gjahr
       and belnr eq wa_bkpf-belnr.
  endif.

endfunction.
