FUNCTION zles_get_data_spl_0002.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_ERDAT_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_SAIDA TYPE  ZLEST0229
*"----------------------------------------------------------------------
  TYPES: lr_range_te TYPE RANGE OF erdat.
  DATA: it_range_erdat TYPE RANGE OF zlest0096-erdat.

  IF i_erdat_ini IS NOT INITIAL.
    it_range_erdat = VALUE lr_range_te( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                              option = 'BT' ( low = i_erdat_ini high = i_erdat_fim )
                                                                          ).
  ENDIF.

  SELECT a~erdat,
         a~cod_destino,
         a~nm_filial,
         a~nm_transbordo,
         a~nm_destino,
         a~wgbez,
         a~tp_transgenia,
         a~lfimg,
         a~quant_transp,
         b~meta_destino
    INTO CORRESPONDING FIELDS OF TABLE @t_saida
    FROM zlest0096 AS a
    LEFT OUTER JOIN  zlest00100 AS b
      ON a~cod_destino = b~cod_destino
     AND a~matnr       = b~matnr
  WHERE a~erdat IN @it_range_erdat.
    IF sy-subrc IS INITIAL.
     SORT t_saida BY cod_destino nm_filial.
    ENDIF.






ENDFUNCTION.
