*----------------------------------------------------------------------*
***INCLUDE LZSD_LIBERACAO_OVF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_read_saldo
*&---------------------------------------------------------------------*
FORM f_read_saldo USING uv_vbeln TYPE vbeln_va
                        uv_posnr TYPE posnr_va
               CHANGING cs_saldo TYPE zi_sd_zsdt0082_saldo.

  SELECT SINGLE * FROM zi_sd_zsdt0082_saldo
    INTO @cs_saldo
      WHERE vbeln = @uv_vbeln
       AND posnr = @uv_posnr.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_read_limite
*&---------------------------------------------------------------------*
FORM f_read_limite  USING    uv_vbeln TYPE vbeln_va
                    CHANGING cs_limite TYPE zsd_in_est_saldo_01.

  SELECT SINGLE * FROM zsd_in_est_saldo_01
    INTO @cs_limite
      WHERE vbeln = @uv_vbeln.

ENDFORM.
