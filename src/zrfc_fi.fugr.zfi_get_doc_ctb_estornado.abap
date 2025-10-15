FUNCTION zfi_get_doc_ctb_estornado.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_NO_SEND_LEGADOS) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZFIE_DOC_CONTAB_ESTOR
*"----------------------------------------------------------------------

  DATA: lra_data  TYPE RANGE OF zsdt0143-data_atual.
  DATA: lra_bukrs TYPE RANGE OF t001-bukrs.

  APPEND VALUE #( sign = 'I' option = 'BT' low = i_data_ini high = i_data_fim ) TO lra_data.

  IF i_bukrs IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = i_bukrs  ) TO lra_bukrs.
  ENDIF.

  IF i_no_send_legados IS NOT INITIAL.

    SELECT b~bukrs
           b~gjahr
           b~belnr
           b~stblg
           b2~gjahr AS gjahr_estorno
           b~tcode
           b2~tcode AS tcode_estorno
           b~awkey
     INTO CORRESPONDING FIELDS OF TABLE t_saida
      FROM bkpf AS b INNER JOIN
           bkpf AS b2 ON b~stblg  = b2~belnr
                     AND b~bukrs  = b2~bukrs
   WHERE b~cpudt IN lra_data
     AND b~bukrs IN lra_bukrs
     AND NOT EXISTS ( SELECT belnr
                        FROM zfit0193 AS c
                     WHERE c~bukrs = b~bukrs
                       AND c~belnr = b~belnr
                       AND c~gjahr = b~gjahr ).
  ELSE.

    SELECT b~bukrs
           b~gjahr
           b~belnr
           b~stblg
           b2~gjahr AS gjahr_estorno
           b~tcode
           b2~tcode AS tcode_estorno
           b~awkey
       INTO CORRESPONDING FIELDS OF TABLE t_saida
        FROM bkpf AS b INNER JOIN
             bkpf AS b2 ON b~stblg  = b2~belnr
                       AND b~bukrs  = b2~bukrs
     WHERE b~cpudt IN lra_data
       AND b~bukrs IN lra_bukrs.

  ENDIF.

  IF sy-subrc IS INITIAL.

    DATA(lt_saida) = t_saida[].
    SORT lt_saida BY bukrs stblg gjahr.

    SELECT docnum, bukrs, belnr, gjahr
      FROM j_1bnfdoc
      INTO TABLE @DATA(lt_j_1bnfdoc)
      FOR ALL ENTRIES IN @lt_saida
      WHERE bukrs = @lt_saida-bukrs
        AND belnr = @lt_saida-stblg
        AND gjahr = @lt_saida-gjahr.
    IF sy-subrc IS INITIAL.
      SORT lt_saida BY bukrs belnr gjahr.
*---> 05/07/2023 - Migração S4 - DL
      SORT lt_j_1bnfdoc BY bukrs belnr gjahr.
*<--- 05/07/2023 - Migração S4 - DL
      LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

        READ TABLE lt_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_doc>)
        WITH KEY bukrs = <fs_saida>-bukrs
                 belnr = <fs_saida>-stblg
                 gjahr = <fs_saida>-gjahr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_saida>-docnum        = <fs_doc>-docnum.
          <fs_saida>-origem_fiscal = 'S'.
        ELSE.
          <fs_saida>-origem_fiscal = 'N'.
        ENDIF.

      ENDLOOP.

    ELSE.

      LOOP AT t_saida ASSIGNING <fs_saida>.

        <fs_saida>-origem_fiscal = 'N'.

      ENDLOOP.

    ENDIF.

  ENDIF.



ENDFUNCTION.
