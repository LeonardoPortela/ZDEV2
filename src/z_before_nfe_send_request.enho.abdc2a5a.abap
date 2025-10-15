"Name: \FU:J_1B_NFE_SEND_REQUESTS\SE:BEGIN\EI
ENHANCEMENT 0 Z_BEFORE_NFE_SEND_REQUEST.

  LOOP AT IT_ACTTAB ASSIGNING FIELD-SYMBOL(<FS_ACTV>).
    CASE <FS_ACTV>-MODEL.
      WHEN '57'.

        SELECT SINGLE *
          FROM J_1BNFE_ACTIVE INTO @DATA(WL_ACTIVE)
         WHERE DOCNUM EQ @<FS_ACTV>-DOCNUM.

        CHECK ( SY-SUBRC EQ 0 ) AND ( WL_ACTIVE-NFNUM9 IS NOT INITIAL ).

        CASE WL_ACTIVE-DOCSTA.
          WHEN SPACE. "Documento não autorizado

            SELECT SINGLE *
              FROM ZCTE_CIOT INTO @DATA(WL_CIOT)
             WHERE DOCNUM EQ @<FS_ACTV>-DOCNUM.

            IF SY-SUBRC EQ 0.
              CASE WL_CIOT-ST_CIOT.
                WHEN '0'   OR "Pendente
                     SPACE OR "Pendente
                     '3'   OR "Rejeitado
                     '8'.     "Cancelado

                  <FS_ACTV>-DOCSTA      = '2'.
                  <FS_ACTV>-SCSSTA      = '0'.
                  <FS_ACTV>-ACTION_REQU = '1'.
                  <FS_ACTV>-MSSTAT      = 'A'.
              ENDCASE.
            ENDIF.
        ENDCASE.
    ENDCASE.
  ENDLOOP.


ENDENHANCEMENT.
