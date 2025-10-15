IF w_cae_num is not INITIAL.

  CONCATENATE w_cae_venc+6(2) w_cae_venc+4(2) w_cae_venc(4)
  into w_cae_venc
  SEPARATED BY '/'.

  CONCATENATE 'C.A.E. N°:' w_cae_num '-'
              'Fecha de Vto.:' w_cae_venc
         INTO w_mostrar_cae
  SEPARATED BY '  '.

ENDIF.
