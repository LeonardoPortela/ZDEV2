FUNCTION zpost_delivery_head_screen_val.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IM_LIKP) TYPE  LIKPVB
*"----------------------------------------------------------------------

  DATA: ls_ZSDT0406 TYPE zsdt0406.

  ls_ZSDT0406-vbeln = im_likp-vbeln.
  ls_ZSDT0406-zztransporte = gv_transporte.
  ls_ZSDT0406-zzincoterms = gv_incoterms.
  ls_ZSDT0406-zzcuit = gv_cuit.
  ls_ZSDT0406-zztransportista = gv_transportista.
  ls_ZSDT0406-zzdocumento = gv_documento.
  ls_ZSDT0406-zzchasis = gv_chasis.
  ls_ZSDT0406-zzacoplado = gv_acoplado.
  MODIFY zsdt0406
    FROM ls_ZSDT0406.

ENDFUNCTION.
