FUNCTION zles_aqua_nf_complemento.
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
*"     REFERENCE(I_EUDR) TYPE  ZEUDR OPTIONAL "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
*"  CHANGING
*"     REFERENCE(IT_ZLEST0205) TYPE  ZDE_ZLEST0205_T
*"----------------------------------------------------------------------


  RANGES: r_series         FOR zsdt0001-series,
          r_cte_emit_aquav FOR zsdt0001-cte_emit_aquav,
          r_eudr           FOR zsdt0001-eudr, "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 --->>>
          r_local_entrega  FOR zsdt0001-local_descarga.

  CLEAR: it_zlest0205.

  CLEAR: r_eudr[]. "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328


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

  IF i_local_descarga IS NOT INITIAL.
    APPEND VALUE #( sign   = 'I' option = 'EQ' low = i_local_descarga ) TO r_local_entrega.
  ENDIF.

  CASE i_tp_class.

    WHEN: 'CO'.

      CASE i_tp_romaneio.
        WHEN: 'PROPRIO'.

          SELECT z~*
            FROM zlest0205 AS z
            INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
           WHERE 1 EQ 1
             "and Z~TP_MOVIMENTO   EQ 'E'
             AND z~cte_emit_aquav IN @r_cte_emit_aquav
             AND z~parid          EQ @i_parid
             AND z~matnr          EQ @i_matnr
             AND z~tp_transgenia  EQ 'CO'
*            AND z~doc_rem        NE @space  "*-US 159346-25.11.2024-JT-inicio
             AND z~series         IN @r_series
             AND l~local_descarga IN @r_local_entrega
             AND z~eudr           IN @r_eudr "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
            INTO TABLE @it_zlest0205.

        WHEN: 'TERCEIRO'.

          SELECT z~*
           FROM zlest0205 AS z
          INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
          WHERE 1 EQ 1
            "and Z~TP_MOVIMENTO   EQ 'E'
            AND z~cte_emit_aquav IN @r_cte_emit_aquav
            AND z~parid          EQ @i_parid
            AND z~matnr          EQ @i_matnr
            AND z~tp_transgenia  EQ 'CO'
            AND z~doc_rem        EQ @space
            AND z~series         IN @r_series
            AND l~local_descarga IN @r_local_entrega
            AND z~eudr           IN @r_eudr "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
            INTO TABLE @it_zlest0205.

      ENDCASE.

    WHEN: 'R1' OR 'R2'.

      CASE i_tp_romaneio.
        WHEN: 'PROPRIO'.
          SELECT z~*
            FROM zlest0205 AS z
            INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
           WHERE 1 EQ 1
             "and Z~TP_MOVIMENTO   EQ 'E'
             AND z~cte_emit_aquav IN @r_cte_emit_aquav
             AND z~parid          EQ @i_parid
             AND z~matnr          EQ @i_matnr
             AND z~tp_transgenia  IN ('P1','T1','D1','P2','T2','D2')
*            AND z~doc_rem        NE @space    *-US 159346-25.11.2024-JT-inicio
             AND z~series         IN @r_series
             AND l~local_descarga IN @r_local_entrega
             AND z~eudr           IN @r_eudr "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
             INTO TABLE @it_zlest0205.

        WHEN: 'TERCEIRO'.
          SELECT z~*
            FROM zlest0205 AS z
           INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
           WHERE 1 EQ 1
             "and Z~TP_MOVIMENTO   EQ 'E'
             AND z~cte_emit_aquav IN @r_cte_emit_aquav
             AND z~parid          EQ @i_parid
             AND z~matnr          EQ @i_matnr
             AND z~tp_transgenia  IN ('P1','T1','D1','P2','T2','D2')
             AND z~doc_rem        EQ @space
             AND z~series         IN @r_series
             AND l~local_descarga IN @r_local_entrega
             AND z~eudr           IN @r_eudr "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
             INTO TABLE @it_zlest0205.
      ENDCASE.

    WHEN ' '.

      CASE i_tp_romaneio.
        WHEN: 'PROPRIO'.
          SELECT z~*
            FROM zlest0205 AS z
            INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
           WHERE 1 EQ 1
             AND z~cte_emit_aquav IN @r_cte_emit_aquav
             AND z~parid          EQ @i_parid
             AND z~matnr          EQ @i_matnr
             AND z~tp_transgenia  EQ @space
*            AND z~doc_rem        NE @space  *-US 159346-25.11.2024-JT-inicio
             AND z~series         IN @r_series
             AND l~local_descarga IN @r_local_entrega
             AND z~eudr           IN @r_eudr "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
            INTO TABLE @it_zlest0205.

        WHEN: 'TERCEIRO'.
          SELECT z~*
            FROM zlest0205 AS z
           INNER JOIN zlest0104 AS l ON l~bukrs EQ z~bukrs AND l~branch EQ z~branch AND l~local_descarga EQ z~local_descarga
           WHERE 1 EQ 1
             AND z~cte_emit_aquav IN @r_cte_emit_aquav
             AND z~parid          EQ @i_parid
             AND z~matnr          EQ @i_matnr
             AND z~tp_transgenia  EQ @space
             AND z~doc_rem        EQ @space
             AND z~series         IN @r_series
             AND l~local_descarga IN @r_local_entrega
             AND z~eudr           IN @r_eudr "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
             INTO TABLE @it_zlest0205.
      ENDCASE.


  ENDCASE.

  IF i_nr_safra IS NOT INITIAL.
    DELETE it_zlest0205 WHERE nr_safra NE i_nr_safra.
  ENDIF.

ENDFUNCTION.
