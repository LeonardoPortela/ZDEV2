"Name: \PR:SAPLMEPO\FO:MEPO_PROCESS_TOPLINE\SE:BEGIN\EI
ENHANCEMENT 0 Z_PEDIDO_BSART.
*
  data w_ekko type ekko.
  IF im_topline_new-bsart NE im_topline_old-bsart.
    select SINGLE * into w_ekko from ekko where ebeln = im_topline_new-ebeln.
    IF sy-subrc = 0.
        message e398(00) with 'Para pedidos já criados'
                              'Não é permitido alterar o tipo '.
    ENDIF.
  endif.

ENDENHANCEMENT.
