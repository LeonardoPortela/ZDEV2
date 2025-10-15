"Name: \PR:SAPLMGMM\FO:AUFBAUEN_BILDTAB\SE:END\EI
ENHANCEMENT 0 Z_VALIDA_ABAS.
*Verifica se o usuario tem acesso a as visões.

*  Ajuste solicitado na US #153371
  if sy-tcode eq 'MM02'.
    data: it_zmmt0212 type table of zmmt0212.

    select *
      from zmmt0212
      into table @it_zmmt0212
      where user_sap = @sy-uname.

    loop at bildtab assigning field-symbol(<fs_bildtab>).
      if <fs_bildtab>-guifu eq 'SP03' or
         <fs_bildtab>-guifu eq 'SP11' or
         <fs_bildtab>-guifu eq 'SP21' or
         <fs_bildtab>-guifu eq 'SP22'.
        continue.
      endif.
      read table it_zmmt0212 transporting no fields with key aba = <fs_bildtab>-guifu.
      if sy-subrc <> 0.
        delete bildtab where guifu eq <fs_bildtab>-guifu. "INDEX sy-tabix.
      endif.
    endloop.
  endif.

*data: tl_setleaf type table of setleaf with header line.
*cs2017000572 inverter
*if sy-tcode eq 'MM02'.
*select *
*  from setleaf
*  into table tl_setleaf
*   where setname eq 'MM02_USR'
*     and valfrom eq sy-uname.
*
*if not sy-subrc is initial.
*  select single *
*     from zmmt0078
*     into  @data(wa_zmmt0078)
*     where usnam eq @sy-uname
*     and   mtart eq 'ZEPI'
*     and   werks ne ''
*     and   werks ne '9999'.
*  if sy-subrc = 0.
*    delete bildtab where guifu ne 'SP03'.
*  else.
*
*
* ------> US #153371 - MMSILVA - 29.04.2025 - Fim <------
*
*  endif.
*endif.
*endif.
ENDENHANCEMENT.
