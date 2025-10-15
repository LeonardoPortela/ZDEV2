"Name: \PR:SAPLKO71\FO:OBJECT_SELECTION_RUN\SE:END\EI
ENHANCEMENT 0 Z_CO_FECHAMENTO.
*
data: vg_aufnr      type coas-aufnr,
      vg_bukrs      type coas-bukrs,
      wa_objnr      type jsto_pre,
      E_STATUS(1),
      E_MESSA(64),
      VG_LAST_DAY TYPE SY-DATUM,
      VG_LAST_DAY_AUX(8).

IF ct_objnr[] is not INITIAL.
    READ TABLE ct_objnr into wa_objnr INDEX 1.
    vg_aufnr = wa_objnr-OBJNR+2(12).
    select SINGLE bukrs
      into vg_bukrs
      from coas
    where aufnr = vg_aufnr.

    IF sy-subrc = 0.
       if lko74-bzdat is INITIAL.
          CONCATENATE LKO74-gjahr LKO74-perio+1(2) '01' INTO VG_LAST_DAY_AUX.
          VG_LAST_DAY = VG_LAST_DAY_AUX.
       else.
          VG_LAST_DAY = LKO74-BZDAT.
       endif.

        CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
          EXPORTING
            I_DATE = VG_LAST_DAY
          IMPORTING
            E_DATE = VG_LAST_DAY.

        CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          I_BUKRS    = vg_BUKRS
          I_DATA     = VG_LAST_DAY
        IMPORTING
          E_STATUS   = E_STATUS
          E_MESSA    = E_MESSA
        EXCEPTIONS
          ERROR      = 1
          OTHERS     = 2.

        IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        IF  E_STATUS = 'E'.
            MESSAGE e000(z01) WITH e_messa.
        ENDIF.
    ENDIF.

ENDIF.
ENDENHANCEMENT.
