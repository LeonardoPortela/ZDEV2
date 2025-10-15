"Name: \PR:SAPLCOIH\FO:CAUFVD_BILLABLE\SE:END\EI
ENHANCEMENT 0 Z_LCOIHF0H_PRCTR.

*-US 158036-04-12-2024-#158036-RJF-Inicio
 IF caufvd-equnr IS NOT INITIAL.
*   BREAK rfreitas.
   DATA: ln_kostln(4)  TYPE n,
         ln_kostlm(10) TYPE n.

   SELECT equnr, eqtyp, eqart, kostl
     UP TO 1 ROWS
     FROM itob " *ITOB-EQTYP - Objetos técnicos PM (EQUI, local de instalação)
     INTO @DATA(wa_itob)
     WHERE equnr EQ @caufvd-equnr.
   ENDSELECT.
   IF sy-subrc IS INITIAL.

     ln_kostlm = caufvd-kostl.

     IF  wa_itob-eqtyp EQ '1'
      OR wa_itob-eqtyp EQ '2'
      OR wa_itob-eqtyp EQ '3'
      OR wa_itob-eqtyp EQ '4'
      OR wa_itob-eqtyp EQ 'A'.

       SELECT * FROM zpmt0001
       INTO TABLE @DATA(it_zpmt0001)
       WHERE eqtyp  EQ @wa_itob-eqtyp
         AND eqart  EQ @wa_itob-eqart.

       IF sy-subrc IS INITIAL.
         SORT it_zpmt0001 BY eqtyp eqart.
         LOOP AT it_zpmt0001 INTO DATA(wa_pmt0001) WHERE eqtyp = wa_itob-eqtyp
                                                     AND eqart = wa_itob-eqart.
           ln_kostln  = wa_pmt0001-kostlg.
           IF sy-subrc IS INITIAL AND ln_kostln IS NOT INITIAL.
             FREE caufvd-kostl.
             ln_kostlm = caufvd-bukrs+2(2) && '0' && caufvd-werks+2(2) && ln_kostln.
             caufvd-kostl = ln_kostlm.
             iloa-kostl   = caufvd-kostl.
           ENDIF.
         ENDLOOP.
       ENDIF.
     ENDIF.
   ENDIF.
 ENDIF.
*-US 158036-04-12-2024-#158036-RJF-Fim
ENDENHANCEMENT.
