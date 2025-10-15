"Name: \PR:SAPLEBND\FO:REL_STRATEGIE_WERTE\SE:END\EI
ENHANCEMENT 0 Z_PED_INSUMOS_ESTR_BRL.
*
  DATA:    TCURR-UKURX(8) TYPE P.

  data: w_zmmt0163 type zmmt0163,
        REFE(16)      TYPE P,
        REF1(8)       TYPE P value 100,
        REF2(8)       TYPE P VALUE 100,
        TRATE         LIKE TCURR-UKURS,
        TRATEX        LIKE TCURR-UKURX.

  data v_parametro(10).

  IF SY-XPROG = 'SAPLME02' or SY-XPROG = 'SAPCNVE' or SY-XPROG = 'SAPLME08' or sy-tcode = 'ZMM0149'.
    IF merkmale-atfor EQ 'CURR' AND
       merkmale-atnam EQ 'ZMMVALOR' AND
       fwaers EQ 'BRL' AND
       hwaers EQ 'BRL'.
       if CEKKO-bukrs is not INITIAL and CEKKO-bsart is not INITIAL.
          CONCATENATE CEKKO-bukrs CEKKO-bsart into v_parametro.
          SELECT COUNT( * )
             FROM TVARVC
             WHERE name = 'Z_ME21N_INSUMOS'
             AND low  = v_parametro.
          IF sy-subrc = 0.
             READ TABLE xcomw into xcomw with key atinn = xksml-imerk.
             IF sy-subrc = 0.
                data(tabix) = sy-tabix.
                select SINGLE *
                  from zmmt0163
                  into w_zmmt0163
                 where DT_TAXA = ( select max( DT_TAXA ) from zmmt0163 ).
                if sy-subrc = 0.
                     tratex = w_zmmt0163-WKURS * 1000000.
                     refe =  refefw * 100000000000 * 1 * ref1 / ( tratex * ref2 * 1 ).
*                     refe = refefw * 1 * tratex * ref1 / ( 100000000000 * ref2 * 1 ).

                     refefw = refe.
                     MOVE refefw TO xcomw-atflv.
                     MOVE refefw TO xcomw-slflv.
                     DO tcurx-currdec TIMES.
                       xcomw-atflv = xcomw-atflv / 10.
                       xcomw-slflv = xcomw-slflv / 10.
                     ENDDO.
                     MODIFY xcomw from xcomw INDEX tabix.
                endif.
             ENDIF.
          ENDIF.
       ENDIF.
    EndIf.
  endif.

ENDENHANCEMENT.
