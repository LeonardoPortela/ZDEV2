"Name: \PR:SAPLAIST\FO:KOSTL_PRUEFEN\SE:BEGIN\EI
ENHANCEMENT 0 ZZ_CHECKCADASTRO_ATIVO.

* # 93772 - Check Cadastro Ativo - RJF - 2022.12.29 - Ini
* CS2022000874 Criar um check na transação AS01 Para deixar cadastrar o mesmo número de ativo em outra empresa.
IF sy-tcode eq 'AS01'.
if anla-ANLN1 is not INITIAL and anla-bukrs is not INITIAL.

  SELECT *
    up to 1 rows
    into @DATA(wa_bukrs)
    from anla
    where ANLN1 eq @anla-ANLN1
      and anln2 eq @anla-ANLN2.
  ENDSELECT.
    if wa_bukrs is not INITIAL. "NE anla-bukrs.
        IF NOT ( anla-bukrs EQ '0035' OR wa_bukrs-bukrs EQ '0035' OR wa_bukrs-bukrs EQ '0038' OR anla-bukrs EQ '0038' ). "IR150244 - RIM - 2023.09.06

            MESSAGE e599(aa) WITH 'O número de imobilizado informado já '
                                  'existe cadastro para Empresa:'
                                  anla-bukrs.
        ENDIF.
    endif.
endif.

ENDIF.
* # 93772 - Check Cadastro Ativo - RJF - 2022.12.29 - Fim
*
ENDENHANCEMENT.
