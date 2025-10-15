FUNCTION ZMM_GET_DATA_ZMM0170 .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_SPMON TYPE  ZMME0246_SPMON
*"      IT_WERKS TYPE  ZMME0246_WERKS
*"      IT_LGORT TYPE  ZMME0246_LGORT
*"      IT_MATNR TYPE  ZMME0246_MATNR
*"      ET_SAIDA TYPE  ZMME0246
*"----------------------------------------------------------------------

  DATA:
    lt_werks       TYPE RANGE OF zmmt0130-werks,
    lt_range_werks TYPE RANGE OF zmmt0130-werks,
    lt_matnr       TYPE RANGE OF zmmt0130-matnr,
    lt_range_matnr TYPE RANGE OF zmmt0130-matnr,
    lt_lgort       TYPE RANGE OF zmmt0130-lgort,
    lt_range_lgort TYPE RANGE OF zmmt0130-lgort,
    lt_spmon       TYPE RANGE OF zmmt0130-spmon,
    lt_range_spmon TYPE RANGE OF zmmt0130-spmon.

    lt_range_spmon = VALUE #(
      FOR <ls_itab> IN it_spmon
      ( sign = 'I'
        option = 'EQ'
        low = <ls_itab> )
    ).

    lt_range_werks = VALUE #(
      FOR <ls_itab1> IN it_werks
      ( sign = 'I'
        option = 'EQ'
        low = <ls_itab1> )
    ).

    lt_range_lgort = VALUE #(
  FOR <ls_itab2> IN it_lgort
  ( sign = 'I'
    option = 'EQ'
    low = <ls_itab2> )
).

    lt_range_matnr = VALUE #(
  FOR <ls_itab3> IN it_matnr
  ( sign = 'I'
    option = 'EQ'
    low = <ls_itab3> )
).


    SELECT tw~name1
           tl~lgobe
           zmmt~spmon
           zmmt~werks
           zmmt~lgort
           zmmt~matnr
           zmmt~maktx
           zmmt~meins
           zmmt~lbkum
           zmmt~salk3
           zmmt~estmdavl
           zmmt~vlrmdest
           zmmt~mgvbr
           zmmt~wgvbr
           zmmt~rtestavl
           zmmt~cbestavl
           zmmt~cbvleavl
           zmmt~cbmdeavl
           zmmt~tmpcober
           zmmt~usnam
           zmmt~data_atual
           zmmt~hora_atual
      INTO CORRESPONDING FIELDS OF TABLE et_saida
  FROM zmmt0130 AS zmmt LEFT OUTER JOIN t001w AS tw
      ON zmmt~werks = tw~werks
  LEFT JOIN t001l AS tl
      ON zmmt~werks = tl~werks
    AND zmmt~lgort = tl~lgort
    WHERE spmon IN lt_range_spmon
      AND zmmt~werks IN lt_range_werks
      AND zmmt~lgort IN lt_range_lgort
      AND matnr IN lt_range_matnr.



ENDFUNCTION.
