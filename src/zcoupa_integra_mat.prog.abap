*&---------------------------------------------------------------------*
*&  Include  ZCOUPA_INTEGRA
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*

data: zmmt0184 type zmmt0184.
data: l_zmmt0184 type zmmt0184.

"BUG SOLTO 171015
select single *
      from tvarvc
      into @data(tvarvc_)
     where name = 'COUPA_ENVIA_MATERIAL'
     and   low  = @sy-tcode.
"BUG SOLTO 171015
if sy-subrc ne 0.
  " Data e hora da execução
  if ( 'ZHIB_ERSA_ZEPI_ZLAG_ZREC' cs wmara-mtart ).
    select single *
    into zmmt0184
    from zmmt0184
        where matnr          =  wmara-matnr
        and   werks          =  wmarc-werks
        and   fg_processado  = 'N'.
    if sy-subrc = 0.
      exit.
    endif.
    get time stamp field data(l_timestamp_after).
    zmmt0184-time_stamp     = l_timestamp_after.
    zmmt0184-matnr          =  wmara-matnr.
    zmmt0184-werks          =  wmarc-werks.
    zmmt0184-fg_processado  = 'N'. "N"
    zmmt0184-data_processo  = sy-datum.
    zmmt0184-hora_processo  = sy-timlo.

    modify zmmt0184 from zmmt0184.

  endif.
endif.
