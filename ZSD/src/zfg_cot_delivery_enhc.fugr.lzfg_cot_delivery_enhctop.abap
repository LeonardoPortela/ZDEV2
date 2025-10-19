FUNCTION-POOL zfg_cot_delivery_enhc.        "MESSAGE-ID ..

* INCLUDE LZFG_COT_DELIVERY_ENHCD...         " Local class definition
CONSTANTS c_z_cot_vstel_werks TYPE rvari_vnam VALUE 'Z_COT_VSTEL_WERKS' ##NO_TEXT.
CONSTANTS c_z_cot_lfart TYPE rvari_vnam VALUE 'Z_COT_LFART' ##NO_TEXT.

DATA: gv_transporte    TYPE  zzeltransporte,
      gv_incoterms     TYPE  zzelincoterms,
      gv_cuit          TYPE  zzelcuit,
      gv_transportista TYPE  zzeltransportista,
      gv_documento     TYPE  zzeldocumento,
      gv_chasis        TYPE  zzelchasis,
      gv_acoplado      TYPE  zzelacoplado.
