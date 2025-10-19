function z_les_determina_parceiro_aviso.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      I_XVTTK STRUCTURE  VTTKVB
*"      I_XVTTP STRUCTURE  VTTPVB
*"      I_XVBPA STRUCTURE  VBPAVB
*"----------------------------------------------------------------------

  data: vg_xvttk like line of i_xvttk,
        vg_xvttp like line of i_xvttp,
        vg_xvbpa like line of i_xvbpa,
        wa_lips  type lips,
        wa_likp  type likp,
        wa_lfa1  type lfa1,
        wa_t001w type t001w,
        wa_adrc  type adrc.

* Informações do cabeçalho do documento de transporte.
  read table i_xvttk into vg_xvttk index 1.
  check not vg_xvttk is initial.

  "Transporte de chegada com/sem carga
  check vg_xvttk-abfer = 2 or vg_xvttk-abfer = 4.

* Informações dos items do documento de transporte.
  read table i_xvttp into vg_xvttp index 1.
  check not vg_xvttp is initial.

  read table i_xvbpa into vg_xvbpa with key parvw = 'PC'.
  if not sy-subrc is initial.

    select single *
      into wa_lips
      from lips
     where vbeln eq vg_xvttp-vbeln.

    if ( sy-subrc is initial ) and ( wa_lips-pstyv eq 'ELN' ).

      select single *
        into wa_likp
        from likp
       where vbeln eq wa_lips-vbeln.

      if not wa_likp-lifnr is initial.

* Dados de endereço do fornecedor.
        select single *
          into wa_lfa1
          from lfa1
         where lifnr = wa_likp-lifnr.

        if not wa_lfa1 is initial.

* Dados de endereço do fornecedor.
          select single *
            into wa_adrc
            from adrc
           where addrnumber = wa_lfa1-adrnr.

          if not wa_adrc is initial.
            clear: vg_xvbpa.
            move:
             sy-mandt        to vg_xvbpa-mandt,
             vg_xvttk-tknum  to vg_xvbpa-vbeln,
             '000000'        to vg_xvbpa-posnr,
             'PC'            to vg_xvbpa-parvw,
             wa_likp-lifnr   to vg_xvbpa-lifnr,
             wa_lfa1-adrnr   to vg_xvbpa-adrnr,
             wa_lfa1-land1   to vg_xvbpa-land1,
             'D'             to vg_xvbpa-adrda,
             'I'             to vg_xvbpa-updkz,
             wa_adrc-name1   to vg_xvbpa-name1,
             'LI'            to vg_xvbpa-nrart,
             '08'            to vg_xvbpa-fehgr,
             sy-langu        to vg_xvbpa-spras.
            append vg_xvbpa to i_xvbpa.
          endif.
        endif.
      endif.
    endif.
  endif.

  read table i_xvbpa into vg_xvbpa with key parvw = 'LR'.
  if ( not sy-subrc is initial ) and ( not vg_xvttk-tplst is initial ).

    select single * into wa_t001w
      from t001w
     where werks eq vg_xvttk-tplst.

    if ( sy-subrc is initial ) and ( not wa_t001w-j_1bbranch is initial ).

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_t001w-j_1bbranch
        importing
          output = wa_t001w-kunnr.

      select single * into wa_adrc
        from adrc
       where addrnumber eq wa_t001w-adrnr.

      if not wa_adrc is initial.
        move:
         sy-mandt        to vg_xvbpa-mandt,
         vg_xvttk-tknum  to vg_xvbpa-vbeln,
         '000000'        to vg_xvbpa-posnr,
         'LR'            to vg_xvbpa-parvw,
         wa_t001w-kunnr  to vg_xvbpa-kunnr,
         wa_t001w-adrnr  to vg_xvbpa-adrnr,
         wa_t001w-land1  to vg_xvbpa-land1,
         'D'             to vg_xvbpa-adrda,
         'I'             to vg_xvbpa-updkz,
         wa_t001w-name1   to vg_xvbpa-name1,
         'KU'            to vg_xvbpa-nrart,
         '07'            to vg_xvbpa-fehgr,
         sy-langu        to vg_xvbpa-spras.
        append vg_xvbpa to i_xvbpa.
      endif.
    endif.
  endif.


endfunction.
