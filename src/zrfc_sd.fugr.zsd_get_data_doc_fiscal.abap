FUNCTION zsd_get_data_doc_fiscal.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CREDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_CREDAT_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_DOCNUM STRUCTURE  ZSD_DOCNUM OPTIONAL
*"      T_SAIDA_J_1BNFDOC STRUCTURE  J_1BNFDOC
*"      T_SAIDA_J_1BNFLIN STRUCTURE  J_1BNFLIN
*"----------------------------------------------------------------------

  DATA: lra_docnum TYPE RANGE OF docnum,
        lra_credat TYPE RANGE OF credat.

  LOOP AT t_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_docnum>-docnum ) TO lra_docnum.
  ENDLOOP.

  IF i_credat_ini IS NOT INITIAL OR i_credat_fim IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'BT' low = i_credat_ini high = i_credat_fim ) TO lra_credat.
  ENDIF.

  SELECT *
    FROM j_1bnfdoc
    INTO TABLE t_saida_j_1bnfdoc
     WHERE credat IN lra_credat
       AND docnum IN lra_docnum.
  IF sy-subrc IS INITIAL.

    SELECT *
      FROM j_1bnflin
      INTO TABLE t_saida_j_1bnflin
      FOR ALL ENTRIES IN t_saida_j_1bnfdoc
     WHERE  docnum = t_saida_j_1bnfdoc-docnum.

  ENDIF.

ENDFUNCTION.
