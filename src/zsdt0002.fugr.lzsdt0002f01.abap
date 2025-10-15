*----------------------------------------------------------------------*
***INCLUDE LZSDT0002F01 .
*----------------------------------------------------------------------*

FORM z_preenche_texto.

  CLEAR zsdt0002-vtext.

  SELECT SINGLE vtext
    FROM tvfkt
    INTO zsdt0002-vtext
  WHERE  fkart EQ  zsdt0002-fkart
    AND  spras EQ 'PT'.

ENDFORM.                    "Z_PREENCHE_TEXTO
