"Name: \FU:J_1B_SD_CFOP\SE:END\EI
ENHANCEMENT 0 Z_PEDIDO_ZUB_EXP.
*
  TYPES:
  BEGIN OF ty_ekko,
       werks TYPE ekpo-werks,
       RESWK TYPE ekko-RESWK,
   end of ty_ekko.

  data: W_SETLEAF              TYPE SETLEAF,
        W_SETLINET             TYPE SETLINET,
        vg_setname             TYPE setleaf-setname,
        WS_ekko                TYPE ty_ekko,
        WS_LFA1                TYPE LFA1,
        REGIO_E                TYPE LFA1-regio,
        REGIO_D                TYPE LFA1-regio,
        VWERKS_e               TYPE LFA1-lifnr,
        VWERKS_d               TYPE LFA1-lifnr,
        pos                    type i.

     clear: WS_ekko, WS_LFA1, REGIO_E, REGIO_D, VWERKS_E,VWERKS_D.
     SELECT SINGLE b~werks, a~RESWK
     INTO CORRESPONDING FIELDS OF @WS_ekko
     from ekko as a
     INNER JOIN ekpo as b
     on a~ebeln = b~ebeln
     where a~ebeln = @I_LIPS-VGBEL
     and   a~bsart = 'ZUB'.


   if sy-subrc = 0.
     " BUG 59101 - AOENNING - 31/05/2021.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WS_ekko-RESWK
      IMPORTING
        OUTPUT = VWERKS_e.

     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WS_ekko-werks
      IMPORTING
        OUTPUT = VWERKS_d.

     select SINGLE REGIO FROM LFA1 INTO REGIO_E WHERE LIFNR EQ VWERKS_e.
     select SINGLE REGIO FROM LFA1 INTO REGIO_D WHERE LIFNR EQ VWERKS_d.

     if REGIO_E ne  REGIO_D.
       vg_setname = 'MAGGI_CFOP_PEDTRANSF'.
     else.
       vg_setname = 'MAGGI_CFOP_PEDTRANSF2'.
     endif.
     SELECT SINGLE *
          FROM SETLEAF
          INTO W_SETLEAF
          WHERE SETNAME = vg_setname
          and   valfrom = WS_ekko-WERKS."v_werks.
      if sy-subrc = 0.
         SELECT SINGLE *
              FROM SETLINET
              INTO W_SETLINET
              WHERE SETNAME = vg_setname
              and   LINEID  = W_SETLEAF-LINEID.
           SEARCH W_SETLINET-DESCRIPT FOR 'CFOP:'.
           IF sy-subrc = 0.
              pos = sy-fdpos + 5.
              CFOP = W_SETLINET-DESCRIPT+pos(6).
              wcfop-cfop = CFOP.
              MODIFY wcfop INDEX windex.
          ENDIF.
      endif.
   endif.

ENDENHANCEMENT.
