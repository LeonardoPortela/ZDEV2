"Name: \PR:SAPLV60A\FO:PREISFINDUNG_GESAMT\SE:END\EI
ENHANCEMENT 0 ZSD_FATURAMENTO_ARGENTINA.
*
*-#130592-10.01.2024-JT- inicio
  LOOP AT tkomp.
    IF tkomp-fxmsg NE space.
      konv_geaendert = 'X'.
    ENDIF.
    READ TABLE xvbrp INDEX tkomp-ix_vbap.
    xvbrp_tabix = tkomp-ix_vbap.

    IF xvbrk-exnum NE space.
      DATA: xxposnr       LIKE vbrp-posnr,
            xxkompl,
            vbtyp_lief(2) VALUE 'JT'.                " auslieferungsartig

      xxposnr = xvbrp-posnr.
      IF ( xvbrp-vgtyp CA vbtyp_lief AND xvbrk-expkz EQ 'X' ) OR
*         the following statement is due to compatibility to Rel. 2.2*
         ( xvbrp-vgtyp IS INITIAL AND xvbrk-fktyp CA 'IL'
                                  AND xvbrk-expkz EQ 'X'    ).
        xxposnr = xvbrp-vgpos.
      ENDIF.
      CALL FUNCTION 'EXPIMP_ITEM_VALUE_MAINTAIN'
        EXPORTING
          exnum          = xvbrk-exnum
          expos          = xxposnr
          grwcu          = t001-waers
          grwrt          = tkomp-grwrt
          i_ft_basis     = '2FA'
        IMPORTING
          complete       = xxkompl
        EXCEPTIONS
          no_export_data = 04.
*       IF XXKOMPL NE 'X' OR SY-SUBRC NE 0.
      IF ( xxkompl NE 'X' OR sy-subrc NE 0 )
         AND tkomp-grwrt NE 0.
        xvbrp-uvall = 'X'.
*           XVBRK-UVALS = 'X'.
      ENDIF.
      IF ( xvbrp-vgtyp CA vbtyp_lief AND xvbrk-expkz CA 'YZ'
                                     AND sy-subrc    EQ 0   ) OR
*         the following statement is due to compatibility to Rel. 2.2*
         ( xvbrp-vgtyp IS INITIAL AND xvbrk-fktyp CA 'IL'
                                  AND xvbrk-expkz CA 'YZ'
                                  AND sy-subrc    EQ 0      ).
* read delivery
        IF xvbrp-vgbel NE likp-vbeln.
          SELECT SINGLE * FROM likp WHERE vbeln EQ xvbrp-vgbel.
        ENDIF.

* transfer statistical value to the export data of the delivery item
        CALL FUNCTION 'EXPIMP_ITEM_VALUE_MAINTAIN'
          EXPORTING
            exnum          = likp-exnum
            expos          = xvbrp-vgpos
            grwcu          = t001-waers
            grwrt          = tkomp-grwrt
            i_ft_basis     = '2FA'
          EXCEPTIONS
            no_export_data = 04.
      ENDIF.
      IF NOT xvbrp-charg IS INITIAL
         AND xvbrp-fkimg IS INITIAL.
*          no incomplete export data for batch split items with
*          billing quantity zero
        xvbrp-uvall = ' '.
*           XVBRK-UVALS = ' '.
      ENDIF.
    ENDIF.
  ENDLOOP.
*-#130592-10.01.2024-JT- fim
*
ENDENHANCEMENT.
