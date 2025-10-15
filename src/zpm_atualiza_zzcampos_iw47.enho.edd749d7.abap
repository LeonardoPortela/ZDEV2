"Name: \PR:RIAFRU20\FO:COMPLETE_OBJECT_TAB\SE:BEGIN\EI
ENHANCEMENT 0 ZPM_ATUALIZA_ZZCAMPOS_IW47.
"ins    FF - 10.10.2023
IF join_result_table[] IS NOT INITIAL.

  SELECT rueck, rmzhl, zzqmnum, zzltxa1, zzmncod, zztxtcdma,
         zzlongitude, zzlatitude
  INTO TABLE @DATA(lt_afru)
  FROM afru FOR ALL ENTRIES IN @join_result_table
    WHERE rueck = @join_result_table-rueck
      AND rmzhl = @join_result_table-rmzhl.

  IF sy-subrc = 0.

    LOOP AT join_result_table ASSIGNING FIELD-SYMBOL(<fs_dados>).

      READ TABLE lt_afru WITH KEY rueck = <fs_dados>-rueck
                                  rmzhl = <fs_dados>-rmzhl INTO DATA(wa_afru).
      IF sy-subrc = 0.
        <fs_dados>-zzqmnum      = wa_afru-zzqmnum.
        <fs_dados>-zzltxa1      = wa_afru-zzltxa1.
        <fs_dados>-zzmncod      = wa_afru-zzmncod.
        <fs_dados>-zztxtcdma    = wa_afru-zztxtcdma.
        <fs_dados>-zzlongitude  = wa_afru-zzlongitude.
        <fs_dados>-zzlatitude   = wa_afru-zzlatitude.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDIF.
ENDENHANCEMENT.
