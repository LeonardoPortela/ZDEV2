function z_pfe_remessa_destino.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_VTTK) TYPE  TKNUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(P_ROUTE) TYPE  ROUTR
*"----------------------------------------------------------------------

  data: wa_vttk type vttk.

  select single route into p_route
    from vttk
   where tknum eq p_vttk.

endfunction.
