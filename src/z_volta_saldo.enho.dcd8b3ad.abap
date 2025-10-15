"Name: \PR:SAPMV56A\FO:MELDUNG_SICHERUNG\SE:END\EI
ENHANCEMENT 0 Z_VOLTA_SALDO.
*Reverte o saldo da DACT na tabela ZLEST0035
  types: begin of tyl_0045.
          include type zlest0045.
  types:  nr_nf type zlest0035-nr_nf,
          serie_nf type zlest0035-serie_nf,
          cnpj     type zlest0035-cnpj,
         end of tyl_0045.

  data: tl_0044 type table of zlest0044 with header line,
        tl_0045 type table of tyl_0045 with header line,
        tl_0035 type table of zlest0035 with header line.
*        tl_vtfa type table of vtfa with header line.

  refresh: tl_0044, tl_0045, tl_0035.


if sy-tcode eq 'VT02N'
or sy-tcode eq 'VT07'
or sy-tcode eq 'VT01N'
or sy-tcode eq 'VT04'.
  IF FLAG-DELETE is not INITIAL.
  SELECT *
    FROM ZLEST0044
    INTO TABLE TL_0044
     FOR ALL ENTRIES IN YVTTK
      WHERE NR_TRANS EQ YVTTK-TKNUM.

    IF SY-SUBRC IS INITIAL.
    SELECT *
      FROM ZLEST0045
      INTO TABLE TL_0045
       FOR ALL ENTRIES IN TL_0044
        where chave_cte eq tl_0044-chave_cte.

     if sy-subrc is initial.
       loop at tl_0045.
         move: tl_0045-CHAVE+25(9) to tl_0045-nr_nf,
               tl_0045-CHAVE+22(3) to tl_0045-serie_nf,
               tl_0045-chave+6(14) to tl_0045-cnpj.

         modify tl_0045.
       endloop.
       select *
         from zlest0035
         into table tl_0035
          for all entries in tl_0045
          where nr_nf    eq tl_0045-nr_nf
            and serie_nf eq tl_0045-SERIE_NF
            and cnpj     eq tl_0045-CNPJ.
     endif.
    ENDIF.

    clear: tl_0045, tl_0044, tl_0035.
    loop at tl_0035.
     read table tl_0045
       with key nr_nf    = tl_0035-nr_nf
                serie_nf = tl_0035-serie_nf
                cnpj     = tl_0035-cnpj.
     if sy-subrc is initial.
       add tl_0045-peso_rateado to tl_0035-saldo.
     modify tl_0035.
     endif.
     clear: tl_0045, tl_0035.
    endloop.

    loop at tl_0044.
     move: space to tl_0044-nr_trans,
           space to tl_0044-nr_frete.

     modify tl_0044.
    endloop.


    modify zlest0044 from table tl_0044.
    modify zlest0035 from table tl_0035.
endif.
endif.
ENDENHANCEMENT.
