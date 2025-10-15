FUNCTION zmm_get_data_doc_material.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BUDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_BUDAT_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_MBLNR STRUCTURE  MBLNR_PRE OPTIONAL
*"      T_SAIDA_MKPF STRUCTURE  MKPF
*"      T_SAIDA_MSEG STRUCTURE  MSEG
*"----------------------------------------------------------------------

  DATA: lra_mblnr TYPE RANGE OF mblnr,
        lra_data  TYPE RANGE OF mkpf-budat.
  IF i_budat_ini IS NOT INITIAL AND i_budat_fim IS NOT INITIAL.

    APPEND VALUE #( sign = 'I' option = 'BT' low = i_budat_ini high = i_budat_fim ) TO lra_data.

  ELSE.

    IF i_budat_ini IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'GE' low = i_budat_ini ) TO lra_data.
    ENDIF.

    IF i_budat_fim IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'LE' low = i_budat_fim ) TO lra_data.
    ENDIF.

  ENDIF.

  LOOP AT t_mblnr ASSIGNING FIELD-SYMBOL(<fs_mblnr>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_mblnr> ) TO lra_mblnr.
  ENDLOOP.

  SELECT *
    FROM mkpf
    INTO TABLE t_saida_mkpf
    WHERE budat IN lra_data
      AND mblnr IN lra_mblnr.

  IF t_saida_mkpf[] IS NOT INITIAL.
    SELECT *
  FROM mseg
  INTO TABLE t_saida_mseg
FOR ALL ENTRIES IN t_saida_mkpf
  WHERE  mblnr = t_saida_mkpf-mblnr.

  ENDIF.

ENDFUNCTION.
