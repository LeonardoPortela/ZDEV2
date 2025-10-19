FUNCTION zset_delivery_head_screen_val.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IM_LIKP) TYPE  LIKP
*"----------------------------------------------------------------------
   CLEAR: gv_transporte,
           gv_incoterms,
           gv_cuit,
           gv_transportista,
           gv_documento,
           gv_chasis,
           gv_acoplado.

  CHECK im_likp-vbeln is NOT INITIAL.

  SELECT SINGLE zztransporte
                zzincoterms
                zzcuit
                zztransportista
                zzdocumento
                zzchasis
                zzacoplado
                INTO (gv_transporte,
                      gv_incoterms,
                      gv_cuit,
                      gv_transportista,
                      gv_documento,
                      gv_chasis,
                      gv_acoplado)
                FROM zsdt0406
                WHERE vbeln EQ im_likp-vbeln.


*  gv_transporte = im_likp-zztransporte.
*  gv_incoterms = im_likp-zzincoterms.
*  gv_cuit = im_likp-zzcuit.
*  gv_transportista = im_likp-zztransportista.
*  gv_documento = im_likp-zzdocumento.
*  gv_chasis = im_likp-zzchasis.
*  gv_acoplado = im_likp-zzacoplado.

ENDFUNCTION.
