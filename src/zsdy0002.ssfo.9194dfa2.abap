IF w_cai_num is not INITIAL.

*  CONCATENATE w_cai_venc+6(2) w_cai_venc+4(2) w_cai_venc(4)
*  into w_cai_venc
*  SEPARATED BY '/'.
*
*  CONCATENATE 'C.A.I. N°:' w_cai_num '-'
*              'Fecha de Vto.:' w_cai_venc
*         INTO w_mostrar_cai
*  SEPARATED BY '  '.

ENDIF.
