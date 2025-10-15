"Name: \PR:SAPMM07M\FO:F-SEGMENTE_ERZEUGEN\SE:END\EI
ENHANCEMENT 0 Z_CONTACONSUMO_PM.
*

  DATA: WA_COAS   TYPE COAS,
        WA_MATKL  TYPE MARA-MATKL,
        WA_SAKNR  TYPE ZMMT0039-SAKNR,
        VG_MSG    TYPE STRING,
        tabix     type sy-tabix.

*  IF 'MB1A_MBST_ZPM0003_MIGO_IW41_IW42' cs SY-TCODE  .

    LOOP AT XACCIT .
      tabix = sy-tabix.
*      IF  '261_262' cs XACCIT-BWART  and XACCIT-AUFNR is not INITIAL.
       IF  XACCIT-AUFNR is not INITIAL.
        SELECT SINGLE *
          FROM COAS
          INTO WA_COAS
          WHERE AUFNR = XACCIT-AUFNR.
        CHECK sy-subrc = 0.
*        IF 'ZPM1_ZPM2_ZPM3_ZPM4' CS WA_COAS-AUART. " Alteração solicitada pelo chamado 131104
        IF WA_COAS-AUTYP = '30'.
          SELECT SINGLE MATKL
            FROM MARA
            INTO WA_MATKL
            WHERE MATNR = XACCIT-MATNR.
          IF SY-SUBRC = 0.
            SELECT SINGLE SAKNR
            FROM ZMMT0039
            INTO WA_SAKNR
            WHERE MATKL = WA_MATKL.
            IF SY-SUBRC = 0.
              XACCIT-HKONT = WA_SAKNR.
              modify  XACCIT INDEX tabix TRANSPORTING hkont.
            ELSE.
              CLEAR: VG_MSG.
               DATA(VG_MATNR) = |{ XACCIT-MATNR ALPHA = OUT }|.
*              VG_MSG = |'Grp material { WA_MATKL } do material { XACCIT-MATNR } não possui conta associada|.
              MESSAGE E024(SD) WITH 'Grp material '  WA_MATKL
                                    'do material não possui conta associada'.
"$$
"$$
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

ENDENHANCEMENT.
