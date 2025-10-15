FUNCTION ZFI_GET_DOC_COMPENSACAO_ZIB.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CPUDT_INI) TYPE  CPUDT OPTIONAL
*"     VALUE(I_CPUDT_FIM) TYPE  CPUDT OPTIONAL
*"  TABLES
*"      T_BELNR STRUCTURE  COBELNR
*"      T_BUKRS STRUCTURE  BSPL_BUKRS
*"      T_SAIDA STRUCTURE  ZFIDE0002
*"----------------------------------------------------------------------

  TYPES: lr_range_te TYPE RANGE OF erdat,
         lr_range_ta TYPE RANGE OF erdat.
  DATA: it_range_CPUDT   TYPE RANGE OF CPUDT,
        lt_range_belnr   TYPE RANGE OF BKPF-belnr,
        lt_range_burks   TYPE RANGE OF buKRS.

  IF  I_CPUDT_INI IS NOT INITIAL.
    it_range_CPUDT = VALUE lr_range_te( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                            option = 'BT' ( low = I_CPUDT_INI high = i_CPUDT_fim )
                                                                          ).
  ENDIF.


  lt_range_belnr = VALUE #(
FOR <ls_itab2> IN t_belnr
( sign = 'I'
  option = 'EQ'
  low = <ls_itab2> )
).

  lt_range_burks = VALUE #(
FOR <ls_itab3> IN t_bukRs
( sign = 'I'
 option = 'EQ'
 low = <ls_itab3> )
).


SELECT     B~BUKRS
           B~BELNR
           B~AWKEY
           K~AUGBL
           K~WRBTR
           K~WSKTO
           b~TCODE
           C~bktxt
           C~CPUDT
           C~CPUTM
INTO CORRESPONDING FIELDS OF TABLE t_saida
FROM BKPF as B
     inner join BSAK  as K
     on B~BELNR = K~BELNR AND B~BUKRS = K~BUKRS
     inner join BKPF  as C
    on C~BELNR = K~AUGBL AND C~BUKRS = K~BUKRS
WHERE b~TCODE = 'FB05'
      AND B~STBLG = ' '
      AND C~CPUDT in it_range_CPUDT
      and B~BELNR in lt_range_belnr
      and b~bukrs in lt_range_burks
      AND EXISTS ( SELECT *
                  FROM BSAK as bs
                  WHERE BS~BELNR = K~BELNR
                        AND BS~BUKRS = K~BUKRS
                        AND BS~GJAHR = K~GJAHR
                        AND BS~SHKZG = 'H' )
      AND EXISTS ( SELECT *
                   FROM zib_contabil
                   WHERE obj_key = b~awkey ).



ENDFUNCTION.
