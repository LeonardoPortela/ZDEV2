clear: v_logo,
       v_graphic.

concatenate 'DANFE_' NOTA_FISCAL-ISSUER-CGC into v_logo.

select single tdname
  into v_graphic
  from stxbitmaps
 where tdobject eq 'GRAPHICS'
   and tdname   eq v_logo
   and tdid     eq 'BMAP'
   and tdbtype  eq 'BMON'.






















