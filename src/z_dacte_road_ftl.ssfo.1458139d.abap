
concatenate 'LOGO_DACTE_' I_DADOS_CTE-CHAVE+6(8)
       INTO V_LOGO.

select single tdname
  from stxbitmaps into v_graphic
 where tdobject eq 'GRAPHICS'
   and tdname    eq v_logo
   and tdid      eq 'BMAP'
   and tdbtype   eq 'BCOL'.

IF sy-subrc ne 0.

  select SINGLE *
    from setleaf INTO @DATA(lwa_setleaf)
   WHERE setname eq 'DOC_AUX_IMP_LOGO_AMAGGI'
     AND valfrom eq @I_DADOS_CTE-CHAVE+6(8).

  IF sy-subrc eq 0.
    v_graphic = 'LOGO_DACTE_77294254'.
    v_logo    = 'LOGO_DACTE_77294254'.
  endif.

ENDIF.















