
LOOP AT it_sintetica INTO DATA(ls_sint).
  ADD ls_sint-vlr_brl TO gv_sint_tot.
  ADD ls_sint-vlr_usd TO gv_sint_usd.
ENDLOOP.


LOOP AT it_sintetica2 INTO DATA(ls_sint2).
  ADD ls_sint2-vlr_brl TO gv_sint2_tot.
  ADD ls_sint2-vlr_usd TO gv_sint2_usd.
ENDLOOP.

WRITE: gv_sint_tot  TO gv_brl_tot,
       gv_sint_usd  TO gv_usd_tot,
       gv_sint2_tot TO gv_brl2_tot,
       gv_sint2_usd TO gv_usd2_tot.

CONDENSE gv_brl_tot NO-GAPS.
CONDENSE gv_usd_tot NO-GAPS.
CONDENSE gv_brl2_tot NO-GAPS.
CONDENSE gv_usd2_tot NO-GAPS.

LOOP AT it_analitica INTO wa_analitica.


WRITE:
  wa_analitica-id       TO wa_analitica2-id,
  wa_analitica-bukrs    TO wa_analitica2-bukrs,
  wa_analitica-ano      TO wa_analitica2-ano,
  wa_analitica-mes      TO wa_analitica2-mes,
  wa_analitica-abertura TO wa_analitica2-abertura,
  wa_analitica-belnr    TO wa_analitica2-belnr,
  wa_analitica-hkont    TO wa_analitica2-hkont,
  wa_analitica-clifor   TO wa_analitica2-clifor,
  wa_analitica-partidas TO wa_analitica2-partidas,
  wa_analitica-name1    TO wa_analitica2-name1,
  wa_analitica-ov       TO wa_analitica2-ov,
  wa_analitica-vlr_brl  TO wa_analitica2-vlr_brl,
  wa_analitica-vlr_usd  TO wa_analitica2-vlr_usd,
  wa_analitica-blart    TO wa_analitica2-blart DD/MM/YY,
  wa_analitica-budat    TO wa_analitica2-budat DD/MM/YY,
  wa_analitica-zfbdt    TO wa_analitica2-zfbdt DD/MM/YY,
  wa_analitica-histo    TO wa_analitica2-histo,
  wa_analitica-class2   TO wa_analitica2-class2.


  APPEND wa_analitica2 TO it_analitica2.
  CLEAR wa_analitica2.


ENDLOOP.
