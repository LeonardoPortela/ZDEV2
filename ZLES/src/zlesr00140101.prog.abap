*----------------------------------------------------------------------*
***INCLUDE ZLESR00140101 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  Z_IMPOSTOS_RETIDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form z_impostos_retidos tables it_retidos structure zles0043_imp_retidos
                         using vg_frete     type zles0043_imp_retidos
                               vg_visualiza type c.

  call function 'Z_LES_INFORMA_IMP_RETIDOS'
    exporting
      p_bukrs                 = vg_frete-bukrs
      p_lifnr                 = vg_frete-lifnr
      p_visualiza             = vg_visualiza
      v_base_sugerido         = vg_frete-base
    tables
      imp_retidos             = it_retidos
    exceptions
      sem_impostos_retidos    = 1
      sem_impostos_retidos_br = 2
      others                  = 3.

  if not sy-subrc is initial.
    message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " Z_IMPOSTOS_RETIDOS
