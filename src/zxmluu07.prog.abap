*&---------------------------------------------------------------------*
*&  Include           ZXMLUU07
*&---------------------------------------------------------------------*
*"*"Lokale Schnittstelle:
*"       CHANGING
*"             VALUE(I_ESUH) LIKE  ESUH STRUCTURE  ESUH
*"       EXCEPTIONS
*"              NOT_ALLOWED
*"----------------------------------------------------------------------

break bmarcelo.
IF sy-tcode(2) =  'VI' and sy-mandt = '060'.
  i_esuh-sumnolim = space.
ENDIF.
