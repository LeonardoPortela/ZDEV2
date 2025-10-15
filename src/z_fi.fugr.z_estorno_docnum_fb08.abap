function z_estorno_docnum_fb08.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_GJAHR) TYPE  GJAHR
*"     REFERENCE(I_STGRD) TYPE  STGRD
*"     REFERENCE(I_BELNR) TYPE  BELNR_D
*"  EXPORTING
*"     REFERENCE(E_STBLG) TYPE  STBLG
*"----------------------------------------------------------------------

  data: it_dta   type standard table of bdcdata,
        wa_dta   type bdcdata,
        wg_bdc   type bdcdata,
        tg_bdc   type table of bdcdata,
        tg_msg   type table of bdcmsgcoll,
        wg_msg   type bdcmsgcoll,
        opt      type ctu_params,
        vl_stblg type bkpf-stblg.

  free: it_dta.
  define shdb.
    clear wa_dta.
    wa_dta-program   = &1.
    wa_dta-dynpro    = &2.
    wa_dta-dynbegin  = &3.
    wa_dta-fnam      = &4.
    wa_dta-fval      = &5.
    append wa_dta to it_dta.
  end-of-definition.

  shdb:
      'SAPMF05A' '0105' 'X'  ' '           ' ',
      ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
      ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
      ' '        ' '    ' '  'RF05A-BELNS' i_BELNR,
      ' '        ' '    ' '  'BKPF-BUKRS'  i_bukrs,
      ' '        ' '    ' '  'RF05A-GJAHS' i_gjahr,
      ' '        ' '    ' '  'UF05A-STGRD' i_stgrd.

  opt-dismode = 'E'.
  call transaction 'FB08' using it_dta options from opt.

  check sy-subrc is initial.

  select single stblg
    from bkpf into e_STBLG
   where bukrs = i_bukrs
     and belnr = i_BELNR
     and gjahr = i_gjahr.

  check ( sy-subrc = 0 ) and ( e_STBLG is not initial ).



endfunction.
