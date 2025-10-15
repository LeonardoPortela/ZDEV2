function z_val_volume_lanc_algodao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_MATNR) TYPE  MATNR
*"     REFERENCE(P_VOLUME) TYPE  VOLUM_15
*"  EXCEPTIONS
*"      INF_VOLUME
*"----------------------------------------------------------------------

*  data: vg_matkl    type matkl,
*        it_setleaf  type table of setleaf with header line.
*
*  clear: it_setleaf[], vg_matkl.
*
*  select single matkl into vg_matkl
*    from mara
*   where matnr eq p_matnr.
*
*  if ( sy-subrc is initial ) and ( vg_matkl is not initial ).
*
*    "Opter Área de contabilidade de custos
**    select * into table it_setleaf
**      from setleaf
**     where setname eq 'MAGGI_VA01'.
**
**    read table it_setleaf with key valfrom = vg_matkl.
*
*    if ( sy-subrc is initial ) and ( ( p_volume is initial ) or ( p_volume lt 0 ) ).
*      message e003 raising inf_volume with p_matnr.
*    endif.
*
*  endif.

endfunction.
