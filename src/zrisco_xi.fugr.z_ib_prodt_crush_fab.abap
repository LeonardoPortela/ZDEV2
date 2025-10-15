function z_ib_prodt_crush_fab.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_PROD STRUCTURE  ZIB_PROD_CRUSHF
*"----------------------------------------------------------------------


  data: wa_crushf type zib_prod_crushf,
        it_crushf type table of zib_prod_crushf.

  clear: it_crushf.

  loop at it_prod into wa_crushf.
    wa_crushf-mandt = sy-mandt.
    append wa_crushf to it_crushf.
  endloop.

  if it_crushf is not initial.
    delete from zib_prod_crushf.
    modify zib_prod_crushf from table it_crushf.
    commit work.
  endif.

endfunction.
