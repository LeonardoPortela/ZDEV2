"Name: \FU:MM_MAINTAIN_PARTNERS\SE:BEGIN\EI
ENHANCEMENT 0 Z_PARTNER_COUPA.
*
IF SUBSCREEN_MODE is INITIAL.
    data(_lines) = lines( X_MMPA ).
    IF _lines = 0.
       if I_LIFRE is NOT INITIAL.
          X_MMPA-EBELN = EBELN.
          X_MMPA-EBELP = 0.
          X_MMPA-EKORG = EKORG.
          X_MMPA-PARVW = 'RS'.
          X_MMPA-PARZA = '001'.
          X_MMPA-ERNAM = sy-uname.
          X_MMPA-ERDAT = sy-datum.
          X_MMPA-LIFN2 = I_LIFRE.
          append X_MMPA.
       ENDIF.
    "
       if I_LLIEF is NOT INITIAL.
         X_MMPA-EBELN = EBELN.
         X_MMPA-EBELP = 0.
         X_MMPA-EKORG = EKORG.
         X_MMPA-PARVW = 'WL'.
         X_MMPA-PARZA = '001'.
         X_MMPA-ERNAM = sy-uname.
         X_MMPA-ERDAT = sy-datum.
         X_MMPA-LIFN2 = I_LLIEF.
         append X_MMPA.
       ENDIF.
    ENDIF.
ENDIF.
ENDENHANCEMENT.
