"Name: \FU:CONVERT_TO_LOCAL_CURRENCY\SE:BEGIN\EI
ENHANCEMENT 0 Z_PED_INSUMOS_ESTR.
*
  data v_parametro(10).
  data v_bapi(10).
  IMPORT v_bapi TO v_bapi FROM MEMORY ID 'Z110_BAPI'.

  FIELD-SYMBOLS:   <f_ekko> TYPE CEKKO.
  IF ( SY-XPROG = 'SAPLME02' or SY-XPROG = 'SAPCNVE' or SY-XPROG = 'SAPLME08' or sy-tcode = 'ZMM0149' ) and v_bapi ne 'GOODS'.
     "CHECK foreign_currency NE local_currency.
     ASSign ('(SAPLEBND)CEKKO') to <f_ekko>.
     if <f_ekko> is ASSIGNED and foreign_currency NE local_currency.
       if  <f_ekko>-bukrs is not INITIAL and  <f_ekko>-bsart is not INITIAL.
           CONCATENATE <f_ekko>-bukrs <f_ekko>-bsart into v_parametro.
           SELECT COUNT( * )
              FROM TVARVC
              WHERE name = 'Z_ME21N_INSUMOS'
              AND low  = v_parametro.
            if sy-subrc = 0.
               mandt = client.
               CLEAR g_exact_date.
*             ------- Initialisierung -----------------------------------------------
               exchange_rate  = 100000.
               exchange_ratex = 100000000000.
               foreign_factor = 1.
               local_factor   = 1.
               local_amount = foreign_amount.
               CLEAR: derived_rate_type, fixed_rate, notation.

               exchange_rate  = trate.
               exchange_ratex = tratex.
               foreign_factor = tcurr-ffact.
               local_factor   = tcurr-tfact.
               exit.
            endif.
        endif.
     ENDIF.
  ENDIF.

ENDENHANCEMENT.
