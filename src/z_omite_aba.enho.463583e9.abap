"Name: \PR:SAPLMGD1\FO:T130F_LESEN_KOMPLETT\SE:END\EI
ENHANCEMENT 0 Z_OMITE_ABA.


data: it_zmmt0212 type table of zmmt0212.

if sy-tcode eq 'MM02'.
  select *
    from zmmt0212
    into table @it_zmmt0212
    where user_sap = @sy-uname
    and   aba      in ( 'SP01', 'SP02' ).

  if not sy-subrc is initial.
    loop at screen.
      if screen-name eq 'MAKT-MAKTX'
      or screen-name eq 'SKTEXT-MAKTX'
      or screen-name eq 'SKTEXT-SPRAS'.
        screen-input = 0.
        modify screen.
      endif.
    endloop.
  endif.

endif.
ENDENHANCEMENT.
