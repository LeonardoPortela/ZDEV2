FUNCTION zget_delivery_head_screen_val.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  EXPORTING
*"     REFERENCE(EX_LIKP) TYPE  LIKP
*"----------------------------------------------------------------------

*  ex_likp-zztransporte = gv_transporte.
*  ex_likp-zzincoterms = gv_incoterms.
*  ex_likp-zzcuit = gv_cuit.
*  ex_likp-zztransportista = gv_transportista.
*  ex_likp-zzdocumento = gv_documento.
*  ex_likp-zzchasis = gv_chasis.
*  ex_likp-zzacoplado = gv_acoplado.

*  DATA: ls_ZSDT0406 TYPE zsdt0406.
*
*  ls_ZSDT0406-VBELN = ex_likp-vbeln.
*  ls_ZSDT0406-zztransporte = gv_transporte.
*  ls_ZSDT0406-zzincoterms = gv_incoterms.
*  ls_ZSDT0406-zzcuit = gv_cuit.
*  ls_ZSDT0406-zztransportista = gv_transportista.
*  ls_ZSDT0406-zzdocumento = gv_documento.
*  ls_ZSDT0406-zzchasis = gv_chasis.
*  ls_ZSDT0406-zzacoplado = gv_acoplado.
*  MODIFY zsdt0406
*    FROM ls_ZSDT0406.




ENDFUNCTION.
