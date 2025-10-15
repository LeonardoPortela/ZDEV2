*----------------------------------------------------------------------*
***INCLUDE LZSDT0005F01 .
*----------------------------------------------------------------------*

FORM z_preenche_texto.

  CLEAR zsdt0005-cfotxt.

  CHECK NOT zsdt0005-cfop IS INITIAL.

  SELECT SINGLE cfotxt
    FROM j_1bagnt
    INTO zsdt0005-cfotxt
  WHERE  spras EQ 'PT'
    AND  cfop  EQ zsdt0005-cfop.

ENDFORM.                    "z_preenche_texto
