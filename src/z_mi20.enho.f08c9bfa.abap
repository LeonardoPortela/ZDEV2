"Name: \PR:RM07IDIFH\FO:USER_COMMAND\SE:BEGIN\EI
ENHANCEMENT 0 Z_MI20.
*
  DATA: wl_setleaf TYPE setleaf,
        wl_erro.

  IF r_ucomm = '9DIF'.
    CLEAR: wl_erro, wl_setleaf.

    IF sy-tcode EQ 'MI20'.
      SELECT SINGLE *
        FROM setleaf
        INTO wl_setleaf
         WHERE setname EQ 'MI20'
           AND valfrom EQ sy-uname.

      LOOP AT yiseg WHERE box   EQ 'X'
                        AND difmg NE 0.

        IF wl_setleaf-valfrom IS INITIAL.
          MOVE: space TO yiseg-box.
          MODIFY yiseg.
          wl_erro = 'X'.
        ENDIF.
      ENDLOOP.
      IF wl_erro IS NOT INITIAL.
        MESSAGE i836(sd) DISPLAY LIKE 'E' WITH 'Há documentos, que não podem ser processados'
                                               'por este usuário.'.
        EXIT.
      ENDIF.
    ENDIF.
    "

  ENDIF.
ENDENHANCEMENT.
