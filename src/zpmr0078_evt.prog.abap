*&---------------------------------------------------------------------*
*&  Include           ZPMR0078_EVT
*&---------------------------------------------------------------------*


START-OF-SELECTION.

  IF s_equnr IS INITIAL AND
     s_eqtyp IS INITIAL AND
     s_iwerk IS INITIAL AND
     s_erdat IS INITIAL AND
     s_aedat IS INITIAL AND
     s_tplnr  IS INITIAL AND
     s_fltyp  IS INITIAL AND
     s_iwerk2 IS INITIAL AND
     s_erdat2 IS INITIAL AND
     s_aedat2 IS INITIAL AND
     s_arbpl IS INITIAL AND
     s_werks  IS INITIAL AND
     s_begda  IS INITIAL AND
     s_aedat3 IS INITIAL.

    MESSAGE 'Favor preencher ao menos o campo Centro!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.

  PERFORM f_seleciona_dados.

  PERFORM f_processa_dados.
