FUNCTION zles_selecao_rom_aqua.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TP_ROMANEIO) TYPE  CHAR08
*"     REFERENCE(I_SERIES) TYPE  J_1BSERIES OPTIONAL
*"     REFERENCE(I_CTE_EMIT_AQUAV) TYPE  CHAR01
*"     REFERENCE(I_CENTRO_EMISSOR) TYPE  WERKS_D
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_TP_CLASS) TYPE  CHAR2
*"     REFERENCE(I_PARID) TYPE  LIFNR
*"     REFERENCE(I_LOCAL_DESCARGA) TYPE  ZDE_LOCAL_DESCARGA_OPUS
*"       OPTIONAL
*"     REFERENCE(I_NR_SAFRA) TYPE  ZNR_SAFRA OPTIONAL
*"     REFERENCE(I_EUDR) TYPE  ZEUDR OPTIONAL
*"  TABLES
*"      E_ZSDT0001 STRUCTURE  ZSDT0001
*"----------------------------------------------------------------------

  RANGES: r_series         FOR zsdt0001-series,
          r_cte_emit_aquav FOR zsdt0001-cte_emit_aquav,
          r_local_entrega  FOR zsdt0001-local_descarga,
          r_tp_transgenia  FOR zsdt0001-tp_transgenia, "LES - US 165779 - Transp. Aquav. Algodao - WPP --->>
          r_eudr           FOR zsdt0001-eudr. "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 --->>>

  CLEAR: r_series,         r_series[],
         r_cte_emit_aquav, r_cte_emit_aquav[],
         r_eudr[], "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
         r_tp_transgenia[], "LES - US 165779 - Transp. Aquav. Algodao - WPP --->>
         e_zsdt0001[],     r_local_entrega[].

  CHECK i_centro_emissor IS NOT INITIAL.

  IF i_series IS NOT INITIAL.
    r_series-sign   = 'I'.
    r_series-option = 'EQ'.
    r_series-low    = i_series.
    APPEND r_series.
  ENDIF.

  "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 --->>>
  IF i_eudr IS NOT INITIAL.

    r_eudr-sign   = 'I'.

    IF i_eudr EQ 'S'.
      r_eudr-option = 'EQ'.
      r_eudr-low    = 'S'.
    else.
      r_eudr-option = 'NE'.
      r_eudr-low    = 'S'.
    ENDIF.

    APPEND r_eudr.
  ENDIF.
  "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 <<<---

  CASE i_cte_emit_aquav.
    WHEN '1'.
      r_cte_emit_aquav-sign   = 'I'.
      r_cte_emit_aquav-option = 'EQ'.
      r_cte_emit_aquav-low    = ''.
      APPEND r_cte_emit_aquav.
    WHEN '2'.
      r_cte_emit_aquav-sign   = 'I'.
      r_cte_emit_aquav-option = 'EQ'.
      r_cte_emit_aquav-low    = 'X'.
      APPEND r_cte_emit_aquav.
    WHEN '3'.
  ENDCASE.

  "Buscar informações para condições de Portochuelo.
  "IF I_LOCAL_DESCARGA IS NOT INITIAL.
  "  SELECT * FROM ZLEST0104 INTO TABLE @DATA(IT_ZLEST0104) WHERE LOCAL_DESCARGA EQ @I_LOCAL_DESCARGA.
  "ELSE.
  "  SELECT * FROM ZLEST0104 INTO TABLE @IT_ZLEST0104 WHERE EMISSOR EQ @I_CENTRO_EMISSOR.
  "ENDIF.
  "CHECK SY-SUBRC = 0.

  IF i_local_descarga IS NOT INITIAL.
    APPEND VALUE #( sign   = 'I' option = 'EQ' low = i_local_descarga ) TO r_local_entrega.
  ENDIF.

  "LES - US 165779 - Transp. Aquav. Algodao - WPP --->>
  CASE i_tp_class.
    WHEN 'CO'.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = 'CO' ) TO r_tp_transgenia.
    WHEN 'R1' OR 'R2'.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = 'P1' ) TO r_tp_transgenia.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = 'T1' ) TO r_tp_transgenia.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = 'D1' ) TO r_tp_transgenia.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = 'P2' ) TO r_tp_transgenia.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = 'T2' ) TO r_tp_transgenia.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = 'D2' ) TO r_tp_transgenia.
    WHEN OTHERS.
  ENDCASE.

  SELECT z~* INTO TABLE @e_zsdt0001
    FROM zsdt0001 AS z
    INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
   WHERE z~tp_movimento   EQ 'E'
     AND z~cte_emit_aquav IN @r_cte_emit_aquav
     AND z~parid          EQ @i_parid
     AND z~matnr          EQ @i_matnr
     AND z~tp_transgenia  IN @r_tp_transgenia
     AND z~series         IN @r_series
     AND l~local_descarga IN @r_local_entrega
     AND z~eudr           IN @r_eudr. "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328

  CASE i_tp_romaneio.
    WHEN 'PROPRIO'.
      DELETE e_zsdt0001 WHERE doc_rem EQ space.
    WHEN 'TERCEIRO'.
      DELETE e_zsdt0001 WHERE doc_rem NE space.
  ENDCASE.

*  "Classificação da Soja
*  CASE i_tp_class.
*
*    WHEN: 'CO'.
*
*      CASE i_tp_romaneio.
*        WHEN: 'PROPRIO'.
*
*          SELECT z~* INTO TABLE @e_zsdt0001
*            FROM zsdt0001 AS z
*            INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
*           WHERE z~tp_movimento   EQ 'E'
*             AND z~cte_emit_aquav IN @r_cte_emit_aquav
*             AND z~parid          EQ @i_parid
*             AND z~matnr          EQ @i_matnr
*             AND z~tp_transgenia  EQ 'CO'
*             "AND Z~DOC_REM        NE @SPACE
*             AND z~series         IN @r_series
*             AND l~local_descarga IN @r_local_entrega
*             AND z~eudr           IN @r_eudr. "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
*
*        WHEN: 'TERCEIRO'.
*
*          SELECT z~* INTO TABLE @e_zsdt0001
*           FROM zsdt0001 AS z
*          INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
*          WHERE z~tp_movimento   EQ 'E'
*            AND z~cte_emit_aquav IN @r_cte_emit_aquav
*            AND z~parid          EQ @i_parid
*            AND z~matnr          EQ @i_matnr
*            AND z~tp_transgenia  EQ 'CO'
*            "AND Z~DOC_REM        EQ @SPACE
*            AND z~series         IN @r_series
*            AND l~local_descarga IN @r_local_entrega
*            AND z~eudr           IN @r_eudr. "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
*
*      ENDCASE.
*
*    WHEN: 'R1' OR 'R2'.
*
*      CASE i_tp_romaneio.
*        WHEN: 'PROPRIO'.
*          SELECT z~* INTO TABLE @e_zsdt0001
*            FROM zsdt0001 AS z
*            INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
*           WHERE z~tp_movimento   EQ 'E'
*             AND z~cte_emit_aquav IN @r_cte_emit_aquav
*             AND z~parid          EQ @i_parid
*             AND z~matnr          EQ @i_matnr
*             AND z~tp_transgenia  IN ('P1','T1','D1','P2','T2','D2')
*             "AND Z~DOC_REM        NE @SPACE
*             AND z~series         IN @r_series
*             AND l~local_descarga IN @r_local_entrega
*             AND z~eudr           IN @r_eudr. "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
*
*        WHEN: 'TERCEIRO'.
*          SELECT z~* INTO TABLE @e_zsdt0001
*            FROM zsdt0001 AS z
*           INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
*           WHERE z~tp_movimento   EQ 'E'
*             AND z~cte_emit_aquav IN @r_cte_emit_aquav
*             AND z~parid          EQ @i_parid
*             AND z~matnr          EQ @i_matnr
*             AND z~tp_transgenia  IN ('P1','T1','D1','P2','T2','D2')
*             "AND Z~DOC_REM        EQ @SPACE
*             AND z~series         IN @r_series
*             AND l~local_descarga IN @r_local_entrega
*             AND z~eudr           IN @r_eudr. "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
*      ENDCASE.
*
*      CASE i_tp_romaneio.
*        WHEN 'PROPRIO'.
*          DELETE e_zsdt0001 WHERE doc_rem EQ space.
*        WHEN 'TERCEIRO'.
*          DELETE e_zsdt0001 WHERE doc_rem NE space.
*      ENDCASE.
  "LES - US 165779 - Transp. Aquav. Algodao - WPP <----

*    WHEN: 'R2'.
*
*      CASE I_TP_ROMANEIO.
*
*        WHEN: 'PROPRIO'.
*          SELECT *
*            FROM ZSDT0001 INTO TABLE E_ZSDT0001
*           WHERE TP_MOVIMENTO   EQ 'E'
*             AND BUKRS          EQ WL_ZLEST0104-BUKRS
*             AND BRANCH         EQ WL_ZLEST0104-BRANCH
*             AND CTE_EMIT_AQUAV IN R_CTE_EMIT_AQUAV
*             AND PARID          EQ I_PARID
*             AND MATNR          EQ I_MATNR
*             AND TP_TRANSGENIA  IN ('P2','T2','D2')
*             AND DOC_REM        NE SPACE
*             AND SERIES         IN R_SERIES
*            AND LOCAL_DESCARGA  EQ WL_ZLEST0104-LOCAL_DESCARGA.
*
*
*        WHEN: 'TERCEIRO'.
*
*          SELECT *
*            FROM ZSDT0001 INTO TABLE E_ZSDT0001
*           WHERE TP_MOVIMENTO   EQ 'E'
*             AND BUKRS          EQ WL_ZLEST0104-BUKRS
*             AND BRANCH         EQ WL_ZLEST0104-BRANCH
*             AND CTE_EMIT_AQUAV IN R_CTE_EMIT_AQUAV
*             AND PARID          EQ I_PARID
*             AND MATNR          EQ I_MATNR
*             AND TP_TRANSGENIA  IN ('P2','T2','D2')
*             AND DOC_REM        EQ SPACE
*             AND SERIES         IN R_SERIES
*             AND LOCAL_DESCARGA  EQ WL_ZLEST0104-LOCAL_DESCARGA.
*
*      ENDCASE.
*  ENDCASE.


  IF i_nr_safra IS NOT INITIAL.
    DELETE e_zsdt0001 WHERE nr_safra NE i_nr_safra.
  ENDIF.

ENDFUNCTION.
