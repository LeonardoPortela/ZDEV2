FUNCTION zsd_get_data_ordem_venda .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_ERDAT_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_AEDAT_INI) TYPE  AEDAT OPTIONAL
*"     VALUE(I_AEDAT_FIM) TYPE  AEDAT OPTIONAL
*"  TABLES
*"      T_VKORG STRUCTURE  ZSDE_VKORG OPTIONAL
*"      T_VBELN STRUCTURE  ZSDE_VBELN OPTIONAL
*"      T_SAIDA_VBAK TYPE  ZSDST0037_TVBAK
*"      T_SAIDA_VBAP TYPE  ZSDST0037_TVBAP
*"      T_SAIDA_VBPA STRUCTURE  VBPA
*"----------------------------------------------------------------------
  DATA:
    lt_range_erdat TYPE RANGE OF vbak-erdat,
    lt_range_aedat TYPE RANGE OF vbak-aedat,
    lra_vbeln      TYPE RANGE OF vbeln,
    lra_vkorg      TYPE RANGE OF vkorg.

  LOOP AT t_vkorg ASSIGNING FIELD-SYMBOL(<fs_vkorg>).
    APPEND VALUE #(  sign = 'I'
                             option = 'EQ'
                             low  = <fs_vkorg>-vkorg
                             ) TO lra_vkorg.
  ENDLOOP.

  LOOP AT t_vbeln ASSIGNING FIELD-SYMBOL(<fs_vbeln>).
    APPEND VALUE #(  sign = 'I'
                             option = 'EQ'
                             low  = <fs_vbeln>-vbeln
                             ) TO lra_vbeln.
  ENDLOOP.

  IF i_erdat_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_erdat_ini
                             high = i_erdat_fim ) TO lt_range_erdat.
  ENDIF.

  IF i_aedat_ini IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I'
                             option = 'BT'
                             low  = i_aedat_ini
                             high = i_aedat_fim ) TO lt_range_aedat.
  ENDIF.

  SELECT *
    FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE t_saida_vbak
    WHERE erdat IN lt_range_erdat
      AND aedat IN lt_range_aedat
      AND vbeln IN lra_vbeln
      AND vkorg IN lra_vkorg.
  IF sy-subrc IS INITIAL.
    SORT t_saida_vbak BY vbeln.
  ENDIF.

  SELECT *
    FROM vbap
    INTO CORRESPONDING FIELDS OF TABLE t_saida_vbap
    WHERE erdat IN lt_range_erdat
      AND aedat IN lt_range_aedat
      AND vbeln IN lra_vbeln.
  IF sy-subrc IS INITIAL.
    SORT t_saida_vbap BY vbeln.
  ENDIF.

  IF t_saida_vbak[] IS NOT INITIAL.

    SELECT *
      FROM vbpa
      INTO TABLE t_saida_vbpa
      FOR ALL ENTRIES IN t_saida_vbak
      WHERE  vbeln = t_saida_vbak-vbeln.

  ENDIF.

  IF t_saida_vbap[] IS NOT INITIAL.

    SELECT *
      FROM vbpa
      APPENDING TABLE t_saida_vbpa
      FOR ALL ENTRIES IN t_saida_vbap
      WHERE  vbeln = t_saida_vbap-vbeln.

  ENDIF.

  SORT t_saida_vbpa BY vbeln posnr parvw.
  DELETE ADJACENT DUPLICATES FROM t_saida_vbpa COMPARING vbeln posnr parvw.


ENDFUNCTION.
