"Name: \PR:SAPMM07I\FO:TRANSAKTIONS_INIT\SE:BEGIN\EI
ENHANCEMENT 0 Z_MI07.
*
 DATA: wl_setleaf TYPE setleaf.
  IF sy-tcode = 'MI07' and sy-calld is INITIAL.
    SELECT SINGLE *
      FROM setleaf
      INTO wl_setleaf
       WHERE setname EQ 'MI20'
         AND valfrom EQ sy-uname.
    IF wl_setleaf-valfrom IS INITIAL.
      MESSAGE e836(sd) DISPLAY LIKE 'E' WITH 'Sem autorização para acessar '
                                             'a MI07 diretamente'.
      EXIT.
    ENDIF.
  ENDIF.


ENDENHANCEMENT.
