
concatenate 'LOGO_DACTE_' I_DADOS_CTE-CHAVE+6(8)
       INTO V_LOGO.

select single tdname
  from stxbitmaps into v_graphic
 where tdobject eq 'GRAPHICS'
   and tdname    eq v_logo
   and tdid      eq 'BMAP'
   and tdbtype   eq 'BCOL'.

IF sy-subrc ne 0.

  CASE I_DADOS_CTE-CHAVE+6(8).
   	WHEN '00315457' or
   	     '85509792' or
         '85509792' or
         '04786144' or
         '16587133' or
         '33019357' or
         '15366495' or
         '09137338' or
         '11338257' or
         '23771214' or
         '08936794' or
         '08918031' or
         '08936816' or
         '03908754' or
         '10431501' or
         '84590892'.

      v_graphic = 'LOGO_DACTE_77294254'.
      v_logo    = 'LOGO_DACTE_77294254'.

   ENDCASE.

ENDIF.


















