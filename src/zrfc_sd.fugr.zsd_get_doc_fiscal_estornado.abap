FUNCTION zsd_get_doc_fiscal_estornado.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CREDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_CREDAT_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZSDDE0323
*"----------------------------------------------------------------------


  TYPES: lr_range_te TYPE RANGE OF credat,
         lr_range_ta TYPE RANGE OF credat.
  DATA: it_range_credat TYPE RANGE OF j_1bnfdoc-credat.
  IF  i_credat_ini IS NOT INITIAL.
    it_range_credat = VALUE lr_range_te( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                            option = 'BT' ( low = i_credat_ini high = i_credat_fim )
                                                                          ).
  ENDIF.


  SELECT de~docnum AS docnum_estorno,
         de~credat AS credat_estorno,
         de~docref AS docnum_estornado,
         dr~credat AS credat_doc_estornado,
         dr~cancel AS cancel_doc_estornado
         INTO CORRESPONDING FIELDS OF TABLE @t_saida
  FROM j_1bnfdoc AS de
  INNER JOIN  j_1bnfdoc AS dr
    ON  dr~docnum = de~docref
  WHERE de~credat IN @it_range_credat
  AND DE~doctyp = '5'.




  ENDFUNCTION.
