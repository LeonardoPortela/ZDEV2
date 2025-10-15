"Name: \PR:SAPLMEGUI\TY:LCL_DOCUMENT_CMD\IN:IF_COMMAND_MM\ME:EXECUTE\SE:BEGIN\EI
ENHANCEMENT 0 Z_BLOQUEIA_REQ.
*
  IF ( sy-tcode = 'ME52N' or sy-tcode = 'ME53N' ) and im_fcode = 'MECREA'.
     MESSAGE w000(z_mm) WITH 'Utilizar diretamente a ME51N para criar requisição'.
     im_fcode = 'MEBACK'.
  elseIF ( sy-tcode = 'ME22N' or sy-tcode = 'ME23N' ) and im_fcode = 'MECREA'.
     MESSAGE w000(z_mm) WITH 'Utilizar diretamente a ME21N para criar pedido'.
     im_fcode = 'MEBACK'.
  ENDIF.
ENDENHANCEMENT.
