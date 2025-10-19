function z_ciot_empresa_parceiro .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_EMPRESA) TYPE  BUKRS
*"     REFERENCE(P_PARTYP) TYPE  J_1BPARTYP DEFAULT 'C'
*"     VALUE(P_PARID) TYPE  J_1BPARID
*"     REFERENCE(P_DT_POSICAO) TYPE  ZDTCIOTPARC
*"     REFERENCE(P_TKNUM) TYPE  TKNUM OPTIONAL
*"     VALUE(P_ROUTE) TYPE  ROUTR OPTIONAL
*"  EXPORTING
*"     REFERENCE(P_EMITE) TYPE  CHAR01
*"----------------------------------------------------------------------

  data: wa_zciot_parceiros type zciot_parceiros.

  data: vg_data_corte type sy-datum,
        it_setleaf  like table of setleaf initial size 0 with header line.

  clear: it_setleaf[], p_emite.

  "Opter Área de contabilidade de custos
  select * into table it_setleaf
    from setleaf
   where setname eq 'Z_TODOS_TRANS_VIAGEM'.

  if sy-subrc is initial.
    read table it_setleaf index 1.
    vg_data_corte = it_setleaf-valfrom.
    if vg_data_corte le p_dt_posicao.
      p_emite = 'X'.
    endif.
  endif.

  if p_emite is initial.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = p_parid
      importing
        output = p_parid.

    if ( p_route is initial ) and ( not p_tknum is initial ).
      call function 'Z_PFE_REMESSA_DESTINO'
        exporting
          p_vttk  = p_tknum
        importing
          p_route = p_route.
    endif.

    select single * into wa_zciot_parceiros from zciot_parceiros
     where bukrs     eq p_empresa
       and parid     eq p_parid
       and partyp    eq p_partyp
       and dt_inicio le p_dt_posicao
       and route     eq p_route.

    if sy-subrc is initial.
      p_emite = 'X'.
    endif.

  endif.

endfunction.
