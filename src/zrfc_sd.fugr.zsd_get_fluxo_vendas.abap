FUNCTION zsd_get_fluxo_vendas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ERDAT_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_ERDAT_FIM) TYPE  ERDAT OPTIONAL
*"  TABLES
*"      T_VBELV STRUCTURE  ZSDDE0324 OPTIONAL
*"      T_POSNV STRUCTURE  ZSDDE0324_1 OPTIONAL
*"      T_VBELN STRUCTURE  ZSDDE0324_2 OPTIONAL
*"      T_POSNN STRUCTURE  ZSDDE0324_3 OPTIONAL
*"      T_VBTYP_N STRUCTURE  ZSDDE0324_4 OPTIONAL
*"      T_VBTYP_V STRUCTURE  ZSDDE0324_5 OPTIONAL
*"      T_SAIDA STRUCTURE  ZSDDE0324_6
*"----------------------------------------------------------------------

  TYPES: lr_range_te TYPE RANGE OF erdat,
         lr_range_ta TYPE RANGE OF erdat.
  DATA: it_range_erdat   TYPE RANGE OF j_1bnfdoc-credat,
        lt_range_vbelv   TYPE RANGE OF vbfa-vbelv,
        lt_range_posnv   TYPE RANGE OF vbfa-posnv,
        lt_range_vbeln   TYPE RANGE OF vbfa-vbeln,
        lt_range_posnn   TYPE RANGE OF vbfa-posnn,
        lt_range_vbtyp_n TYPE RANGE OF vbfa-vbtyp_n,
        lt_range_vbtyp_v TYPE RANGE OF vbfa-vbtyp_v.

  IF  i_erdat_ini IS NOT INITIAL.
    it_range_erdat = VALUE lr_range_te( LET s = 'I' o = 'EQ' IN sign = 'I'
                                                            option = 'BT' ( low = i_erdat_ini high = i_erdat_fim )
                                                                          ).
  ENDIF.


  lt_range_vbelv   = VALUE #(
  FOR <ls_itab> IN t_vbelv
  ( sign = 'I'
    option = 'EQ'
    low = <ls_itab> )
).

  lt_range_posnv = VALUE #(
FOR <ls_itab1> IN t_posnv
( sign = 'I'
  option = 'EQ'
  low = <ls_itab1> )
).

  lt_range_vbeln = VALUE #(
FOR <ls_itab2> IN t_vbeln
( sign = 'I'
  option = 'EQ'
  low = <ls_itab2> )
).

  lt_range_posnn = VALUE #(
FOR <ls_itab3> IN t_posnn
( sign = 'I'
 option = 'EQ'
 low = <ls_itab3> )
).

  lt_range_vbtyp_n = VALUE #(
FOR <ls_itab4> IN t_vbtyp_n
( sign = 'I'
 option = 'EQ'
 low = <ls_itab4> )
).

  lt_range_vbtyp_v = VALUE #(
FOR <ls_itab5> IN t_vbtyp_v
( sign = 'I'
option = 'EQ'
low = <ls_itab5> )
).

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE t_saida
      FROM vbfa
      WHERE erdat IN it_range_erdat
        AND vbelv   IN lt_range_vbelv
        AND posnv   IN lt_range_posnv
        AND vbeln   IN lt_range_vbeln
        AND posnn   IN lt_range_posnn
        AND vbtyp_n IN lt_range_vbtyp_n
        AND vbtyp_v IN lt_range_vbtyp_v.

if t_saida[] is NOT INITIAL.
       loop at t_saida[] ASSIGNING FIELD-SYMBOL(<wa_saida>).
      if <wa_saida>-vbtyp_n eq 'O' and <wa_saida>-vbtyp_v eq 'H'.

        select single doc~cancel
          from j_1bnflin as lin
          inner join j_1bnfdoc as doc on lin~docnum = doc~docnum
          into @data(v_cancelada)
          where lin~refkey eq @<wa_saida>-vbeln and
                lin~refitm eq @<wa_saida>-posnn and
                doc~cancel eq 'X'.

          if sy-subrc is INITIAL.
            <wa_saida>-nf_cancel = 'X'.
            endif.
        endif.
      ENDLOOP.

      endif.

ENDFUNCTION.
