"Name: \TY:CL_IDENTITY\IN:IF_IDENTITY\ME:ACTION_UNLOCK\SE:BEGIN\EI
ENHANCEMENT 0 ZCHECK_UNLOCK_BLOC_RH.
*

  if ms_segment_header-bname is not initial.
    select SINGLE * INTO @data(wa_zHCMT0007)
      from ZHCMT0007
     where BNAME eq @ms_segment_header-bname
       and BLOQUEIO_RH     eq @abap_true
       and BLOQUEIO_RH_EFE eq @abap_true.

    if sy-subrc is INITIAL.
      MESSAGE I001(ZUSER) WITH ms_segment_header-bname DISPLAY LIKE 'E'.
      RETURN.
    endif.
  endif.

ENDENHANCEMENT.
