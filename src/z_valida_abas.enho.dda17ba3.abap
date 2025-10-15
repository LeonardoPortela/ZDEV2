"Name: \PR:SAPLMGMM\FO:INIT_TABSTRIP\SE:END\EI
ENHANCEMENT 0 Z_VALIDA_ABAS.
*Omiti abas para algums usuarios na transacao mm02
*data: tl_setleaf type table of setleaf with header line.

*Ajuste solicitado na US #153371
if sy-tcode eq 'MM02'.
  data: it_zmmt0212 type table of zmmt0212.

  select *
    from zmmt0212
    into table @it_zmmt0212
    where user_sap = @sy-uname.

  loop at bildtab assigning field-symbol(<fs_bildtab>).
    read table it_zmmt0212 transporting no fields with key aba = <fs_bildtab>-guifu.
    if sy-subrc <> 0.
      <fs_bildtab>-kzsel = ''.
    endif.
    if <fs_bildtab>-guifu eq 'SP03' or
       <fs_bildtab>-guifu eq 'SP11' or
       <fs_bildtab>-guifu eq 'SP21' or
       <fs_bildtab>-guifu eq 'SP22'.
      <fs_bildtab>-kzsel = 'X'.
    endif.
  endloop.

endif.
*
**"CS2017000572 inverter
*if sy-tcode eq 'MM02'.
*  loop at bildtab assigning field-symbol(<fs_bildtab>).
*    <fs_bildtab>-kzsel = ''.
**    modify bildtab from <fs_bildtab> .
*  endloop.
*
*  select *
*    from setleaf
*    into table tl_setleaf
*     where setname eq 'MM02_USR'
*       and valfrom eq sy-uname.
*
*  if  sy-subrc is initial.
*    loop at bildtab.
*      bildtab-kzsel = 'X'.
*      modify bildtab.
*    endloop.
*
*    loop at screen.
*      if screen-group1 eq 'TAB'.
*        screen-invisible = 0.
*        screen-input     = 1.
*        modify screen.
*      endif.
*    endloop.
**    "SSO
**    select single *
**       from zmmt0078
**       into  @data(wa_zmmt0078)
**       where usnam eq @sy-uname
**       and   mtart eq 'ZEPI'
**       and   werks ne ''
**       and   werks ne '9999'.
**
**    if sy-subrc = 0.
**      loop at bildtab.
**        if bildtab-guifu eq 'SP03'.
**          bildtab-kzsel = 'X'.
**        else.
**          bildtab-kzsel = space.
**        endif.
**        modify bildtab.
**      endloop.
**    endif.
*  endif.
*
*endif.

ENDENHANCEMENT.
