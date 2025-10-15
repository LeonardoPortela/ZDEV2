"Name: \PR:SAPLV60A\FO:XVBPAK_IDENTISCH\SE:BEGIN\EI
ENHANCEMENT 0 Z_ZRFL_PARC2.
*
  data Vlines  type  sy-tabix.
  Vlines = lines( XVBUP ).
  "
  if VBRK-FKART = 'ZRFL' or VBRK-FKART = 'ZRDC'.
      READ TABLE XVBPAK WITH key parvw = 'LR'.
      SELECT SINGLE *
        from ZSDT0121
        into @data(W_ZSDT0121)
        WHERE WERKS = @vbap-GSBER
        and   MATNR = @vbap-matnr
        and   KUNNR = @XVBPAK-kunnr.
      "
      if sy-subrc = 0.
        SELECT SINGLE *
         from kna1
         into @DATA(W_KNA1)
         WHERE kunnr = @XVBPAK-kunnr.
        "
        IF sy-subrc = 0.
          SELECT SINGLE *
           from lfa1
           into @DATA(W_lfa1)
           WHERE stcd1 = @W_KNA1-stcd1.
         IF sy-subrc = 0.
             READ TABLE XVBPAK WITH key parvw = 'PC'.
             IF sy-subrc = 0.
                XVBPAK-lifnr = W_lfa1-lifnr.
                XVBPAK-adrnr = W_lfa1-ADRNR.
                modify XVBPAK INDEX sy-tabix.
             ENDIF.
             READ TABLE XVBPA WITH key parvw = 'PC'.
             IF sy-subrc = 0.
                XVBPA-lifnr = W_lfa1-lifnr.
                XVBPA-adrnr = W_lfa1-ADRNR.
                modify XVBPA INDEX sy-tabix.
             ENDIF.
         ENDIF.
        ENDIF.
      endif.
     IF  vlines GT 1.
      SY-SUBRC = 0.
      exit.
     endif.
    "
  ENDIF.


ENDENHANCEMENT.
