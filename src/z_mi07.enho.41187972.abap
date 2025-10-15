"Name: \PR:SAPMM07I\FO:DIFFERENZ_BEARBEITEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_MI07.
*
  DATA: wl_setleaf TYPE setleaf.
  IF yvm07i-difmg NE 0.
    SELECT SINGLE *
      FROM setleaf
      INTO wl_setleaf
       WHERE setname EQ 'MI20'
         AND valfrom EQ sy-uname.
    IF wl_setleaf-valfrom IS INITIAL.
      MESSAGE e836(sd) DISPLAY LIKE 'E' WITH 'Há documentos, que não podem ser processados'
                                             'por este usuário.'.
      EXIT.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
